import sys
import numpy as np

from rocketcea.biprop_utils.mr_t_limits import MR_Temperature_Limits

from rocketcea.Goal import Goal
from rocketcea.biprop_utils.run_cea_w_error_corr import run_cea_ode, run_cea_odf

from rocketcea.biprop_utils.goldSearch import search_max as gold_search_max
from rocketcea.biprop_utils.InterpProp_scipy import InterpProp
"""

-----------------------
biprop_utils
Copyright (C) 2015  Charlie Taylor

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-----------------------
__author__    = 'Charlie Taylor'
__copyright__ = 'Copyright (c) 2015 Charlie Taylor'
__license__   = 'GPL-3'
__email__     = "cet@appliedpython.com"

"""

def frac_ox( MR ):
    """return the fraction of ox (fox) for given MR"""
    return MR / (MR + 1.0)


MR_MAX = 1000.0 
def mr_of_fracox( fox ):
    """return the MR for given ox fraction (fox)"""
    if fox < 1.0:
        mr = fox / (1.0 - fox)
        return min( mr, MR_MAX )
    else:
        return MR_MAX
    

class MR_Peak_At_EpsPc( object ):
    
    def __str__(self):
        return '''<mrPeak=%g, ispPeak=%g, mrLeftOfPeak=%g, mrRightOfPeak=%g, pc=%g, eps=%g>'''%\
            (self.mrPeak, self.ispPeak, self.mrLeftOfPeak, self.mrRightOfPeak, self.pc, self.eps)
        
    def __init__(self, mrLimitsObj, pc=100., eps=10., ispType='CEAODE', # ispType can be CEAODE, CEAFROZEN
                 NterpSize=100): # size of interpolation array
        
        self.mrLimitsObj = mrLimitsObj
        self.pc = pc
        self.eps = eps
        self.ispType = ispType
        
        self.mrL = [] # build lists of non-zero isp both above and below stoich_MR
        self.isL = []
        
        stoich_MR = self.mrLimitsObj.Stoich_MR
        #print( 'stoich_MR =',stoich_MR)
        stoich_fox = frac_ox( stoich_MR )
        #print( 'stoich_fox =',stoich_fox)
        self.stoich_fox = stoich_fox
        
        stoich_Isp = self.isp_at_mr( mr=stoich_MR )
        
        # build mr and isp lists from the stoich point to the left and right.
        # Only bother doing this because CEA frozen calc will often fail with condensed species.
        
        # build list from stoich towards MR=0
        mr_leftL = [stoich_MR]
        is_leftL = [ stoich_Isp ]
        df = 1.0 / float(NterpSize)
        fox = stoich_fox - df
        while fox > 0.0:
            mr = mr_of_fracox( fox )
            isp = self.isp_at_mr( mr=mr )
            if isp > 1.0:
                mr_leftL.append( mr )
                is_leftL.append( isp )
                fox = fox - df
                
                if isp < stoich_Isp*0.9:
                    break
            else:
                break
        
        #print 'mr_leftL =',mr_leftL
        #print 'is_leftL =',is_leftL
        
        # build list from stoich towards ox fraction = 1.0
        mr_rightL = []
        is_rightL = []
        
        fox = stoich_fox + df
        while fox < 1.0:
            mr = mr_of_fracox( fox )
            isp = self.isp_at_mr( mr=mr )
            if isp > 1.0:
                mr_rightL.append( mr )
                is_rightL.append( isp )
                fox = fox + df
                
                if isp < stoich_Isp*0.9:
                    break
            else:
                break
        #print 'mr_rightL =',mr_rightL
        #print 'is_rightL =',is_rightL
        # combine both lists
        mr_leftL.reverse()
        is_leftL.reverse()
        
        self.mrL = mr_leftL + mr_rightL
        self.isL = is_leftL + is_rightL
        
        if len( self.mrL ) < 3:
            print('ERROR... len(mrL) < 3')

        #print 'Optimizing pc=%g, eps=%g'%(pc, eps)
        self.mr_isp_terp = InterpProp(self.mrL, self.isL)
        self.mrPeak, iterp_peak = gold_search_max(self.mr_isp_terp, self.mrL[0], self.mrL[-1], tol=1.0e-5)
            
        self.ispPeak = self.isp_at_mr( mr=self.mrPeak )
        
        self.isp_min = min( self.isL )
        self.isp_max = max( self.isL )
        
        if self.ispPeak < 0.01 or self.mrPeak < 0.0:
            imin = 0
            for i in range( len(self.mrL) ):
                if self.isL[i] < self.isL[imin]:
                    imin = i
                    
            imax = min(len(self.mrL)-1, imin+2)
            imin = max(0, imin-2)
            self.mrPeak, iterp_peak = gold_search_max(self.mr_isp_terp, self.mrL[imin], self.mrL[imax], tol=1.0e-5)
            self.ispPeak = self.isp_at_mr( mr=self.mrPeak )
        
        if self.ispPeak < 0.01 or self.mrPeak < 0.0:
            print(( '_______________BAD OPTIMUM at pc=%g and eps=%g   mrPeak=%g, ispPeak=%g'%(pc, eps, self.ispPeak, self.mrPeak)))
        
        # place holders for left and right of peak Isp
        self._mrLeftOfPeak = None
        self._mrRightOfPeak = None

    
    @property
    def mrLeftOfPeak(self): # 1st point
        if self._mrLeftOfPeak is None:
            self.calc_mrLow_minus_NPcentIsp()
        return self._mrLeftOfPeak
    
    @property
    def mrRightOfPeak(self): # 1st point
        if self._mrRightOfPeak is None:
            self.calc_mrHigh_minus_NPcentIsp()
        return self._mrRightOfPeak

            
        
    def isp_at_mr(self, mr):
        '''Return Isp from either CEA or TDK program.  Can be ODE or Frozen.
           Return value is determined by ispType flag (CEAODE, CEAFROZEN)
        '''
        
        if self.ispType=='CEAODE':
            return run_cea_ode(self.mrLimitsObj.ispODEObj, Pc=self.pc, MR=mr, eps=self.eps )
            
        elif self.ispType=='CEAFROZEN':
            ispFroz = run_cea_odf(self.mrLimitsObj.ispODEObj, Pc=self.pc, MR=mr, eps=self.eps )
            return ispFroz
            
        else:
            print('ERROR... illegal value for ispType',self.ispType,'(should be CEAODE, CEAFROZEN)')
            sys.exit()

    def calc_mrLow_minus_NPcentIsp(self, NPcent=3.0): # N % lower than Peak Isp
        
        ispTarget = self.ispPeak * (100.0-NPcent)/100.0
                
        G = Goal(goalVal=ispTarget, minX=self.mrL[0], maxX=self.mrPeak, 
            funcOfX=self.mr_isp_terp, tolerance=1.0E-4, maxLoops=40, failValue=None)
        self._mrLeftOfPeak, self.ierrorLowIsp = G()
            
        if self.ierrorLowIsp:
            print('WARNING... mrLeftOfPeak NOT found.  Setting equal to mrPeak')
            self._mrLeftOfPeak = self.mrPeak
            
        if self._mrLeftOfPeak < 0.01:
            print('WARNING... mrLeftOfPeak is Too Low.  Setting equal to mrPeak')
            self._mrLeftOfPeak = 0.01
            
        return self._mrLeftOfPeak


    def calc_mrHigh_minus_NPcentIsp(self, NPcent=3.0): # N % lower than Peak Isp
        
        ispTarget = self.ispPeak * (100.0-NPcent)/100.0
        
        G = Goal(goalVal=ispTarget, minX=self.mrPeak, maxX=self.mrL[-1], 
            funcOfX=self.mr_isp_terp, tolerance=1.0E-4, maxLoops=40, failValue=None)
        self._mrRightOfPeak, self.ierrorHighIsp = G()
            
        if self.ierrorHighIsp:
            print('WARNING... mrRightOfPeak NOT found.  Setting equal to mrPeak')
            self._mrRightOfPeak = self.mrPeak
            print(self)
        
        #if self._mrRightOfPeak>5.0:
        #    print self
        
        return self._mrRightOfPeak

    def get_peak_mr_vs_isp_lists(self, Npts=20):
        mrL = list( np.linspace(self.mrLeftOfPeak, self.mrRightOfPeak, Npts) )
        ispL = [self.isp_at_mr( mr=mr) for mr in mrL]
        return mrL, ispL
    
if __name__=="__main__":
    from pylab import *
    
    mc = MR_Temperature_Limits(oxName='F2', fuelName='H2', oxPcentL=None, fuelPcentL=None,
        TC_LIMIT=1400.0, PcNominal=200.0, epsNominal=3.0,
        MR_MIN=0.0, MR_MAX=1000.0)
    print('Stoich MR =',mc.Stoich_MR,'for %s/%s'%(mc.cea_oxName, mc.cea_fuelName))
    print()
    print('Min MR = %g'%mc.min_MR, '  Tc at Min MR =',mc.Tc_at_min_MR)
    print('Max MR = %g'%mc.max_MR, '  Tc at Max MR =',mc.Tc_at_max_MR)
    
    mrcurve = MR_Peak_At_EpsPc( mc, pc=1000., eps=10., ispType='CEAFROZEN')#'CEAFROZEN' )
    print('Peak IspODE=%g sec at MR ='%mrcurve.ispPeak,mrcurve.mrPeak)
    print() 
    print('MR at 97% Isp (on low  side) =', mrcurve.mrLeftOfPeak)
    print('MR at 97% Isp (on high side) =', mrcurve.mrRightOfPeak)
    
    mrlo = 1.01 * mrcurve.mrL[0]
    mrhi = 0.99 * mrcurve.mrL[-1]
    
    mrL = np.linspace(mrlo, mrhi, 100)
    foxL = [frac_ox(mr) for mr in mrL]
    ispL = [mrcurve.isp_at_mr( mr=mr) for mr in mrL]
    
    title( '%s/%s'%(mc.cea_oxName, mc.cea_fuelName) + ' ' + mrcurve.ispType )
    
    #plot(mrL, ispL)
    plot(foxL, ispL)
    
    foxL = [frac_ox(mr) for mr in mrcurve.mrL]
    plot(foxL, [i for i in mrcurve.isL], 's')
    #plot(mrL, ispL, 'o')
    
    mrL = [mrcurve.mrLeftOfPeak, mrcurve.mrPeak, mrcurve.mrRightOfPeak]
    foxL = [frac_ox(mr) for mr in mrL]
    ispL = [mrcurve.isp_at_mr( mr=mr) for mr in mrL]
    plot(foxL, ispL, 'o')
    
    axvline( mrcurve.stoich_fox )
    #mrL = [i*.04 for i in range(150)]
    #ispL = [mrcurve.mr_isp_terp(mr) for mr in mrL]
    #plot(mrL, ispL, '.')
    #print('mrcurve.mrL =',mrcurve.mrL)
    #print('mrcurve.isL =',mrcurve.isL)
    
    show()
    
    