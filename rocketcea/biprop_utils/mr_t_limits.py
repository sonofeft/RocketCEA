import sys
from rocketcea.cea_obj import CEA_Obj
from rocketcea.blends import get_propellant_name

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
class MR_Temperature_Limits( object ):

    def __init__(self, oxName='N2O4', fuelName='MMH', oxPcentL=None, fuelPcentL=None,
        TC_LIMIT=1000.0, PcNominal=1000.0, epsNominal=10.0,
        MR_MIN=0.0, MR_MAX=1000.0): # degR, psia
        
        self.oxName = oxName
        self.oxPcentL = oxPcentL
        self.fuelName = fuelName
        self.fuelPcentL = fuelPcentL
        
        self.cea_fuelName = get_propellant_name( Name=fuelName, PcentL=fuelPcentL)
        self.cea_oxName = get_propellant_name( Name=oxName, PcentL=oxPcentL)
        self.TC_LIMIT = TC_LIMIT
        self.PcNominal = PcNominal
        self.epsNominal = epsNominal
        self.MR_MIN = MR_MIN
        self.MR_MAX = MR_MAX

        self.ispODEObj = CEA_Obj(fuelName=self.cea_fuelName, oxName=self.cea_oxName, 
                                 useFastLookup=0, fac_CR=None)

        self.Stoich_MR = self.ispODEObj.getMRforER( ERphi=1.0 )
        #print( 'Stoich MR =',self.Stoich_MR,'for %s/%s'%(self.cea_oxName, self.cea_fuelName) )
        
        self.find_min_mr()
        self.find_max_mr()
        
        
    def find_min_mr(self):
        '''Find min mr where CEA Isp is Non-Zero AND Tc >= TC_LIMIT'''
        
        isp = self.ispODEObj.get_Isp( Pc=self.PcNominal, MR=0.0, eps=self.epsNominal)
        if isp>0.0:
            self.min_MR = 0.0
            isp,cstr,self.Tc_at_min_MR = self.ispODEObj.get_IvacCstrTc( Pc=self.PcNominal, MR=self.min_MR, eps=self.epsNominal)
            if isp>0.1 and self.Tc_at_min_MR > self.TC_LIMIT:
                return
        
            
        mr_top = self.Stoich_MR
        mr_bot = 0.0
        for i in range(40):
            mr = (mr_top + mr_bot) / 2.0
            
            isp,cstr,tc = self.ispODEObj.get_IvacCstrTc( Pc=self.PcNominal, MR=mr, eps=self.epsNominal)
            if isp>0.0 and tc>self.TC_LIMIT:
                mr_top = mr
            else:
                mr_bot = mr
                
        #print( 'Calculated Min MR = %g'%mr_top, '  Isp at Min MR =',self.ispODEObj.get_Isp( Pc=self.PcNominal, MR=mr_top, eps=eps) )
        self.min_MR = mr_top
        isp,cstr,self.Tc_at_min_MR = self.ispODEObj.get_IvacCstrTc( Pc=self.PcNominal, MR=mr_top, eps=self.epsNominal)
            
        
    def find_max_mr(self):
        '''Find max mr where CEA Isp is Non-Zero AND Tc >= TC_LIMIT'''
        isp = self.ispODEObj.get_Isp( Pc=self.PcNominal, MR=self.MR_MAX, eps=self.epsNominal)
        if isp>0.0:
            self.max_MR = self.MR_MAX
            isp,cstr,self.Tc_at_max_MR = self.ispODEObj.get_IvacCstrTc( Pc=self.PcNominal, MR=self.max_MR, eps=self.epsNominal)
            if isp>0.0 and self.Tc_at_max_MR>self.TC_LIMIT:
                return
        
            
        mr_top = self.MR_MAX
        mr_bot = self.Stoich_MR
        for i in range(40):
            mr = (mr_top + mr_bot) / 2.0
            
            isp,cstr,tc = self.ispODEObj.get_IvacCstrTc( Pc=self.PcNominal, MR=mr, eps=self.epsNominal)
            
            if isp>0.0 and tc>self.TC_LIMIT:
                mr_bot = mr
            else:
                mr_top = mr
                
        #print( 'Calculated Max MR = %g'%mr_bot, '  Isp at Max MR =',self.ispODEObj.get_Isp( Pc=self.PcNominal, MR=mr_bot, eps=eps))
        self.max_MR = mr_bot
        isp,cstr,self.Tc_at_max_MR = self.ispODEObj.get_IvacCstrTc( Pc=self.PcNominal, MR=mr_bot, eps=self.epsNominal)
        
    def __str__(self):
        
        return '''<%s/%s, Stoich_MR=%g, Min MR=%g, Max MR=%g, Tc Left=%g R, Tc Right=%g R>'''%\
            (self.cea_oxName, self.cea_fuelName, self.Stoich_MR, self.min_MR, self.max_MR, self.Tc_at_min_MR, self.Tc_at_max_MR)

if __name__=="__main__":
    
    mc = MR_Temperature_Limits()
    
    print( 'Stoich MR =',mc.Stoich_MR,'for %s/%s'%(mc.cea_oxName, mc.cea_fuelName))
    print('')
    print( 'Min MR = %g'%mc.min_MR, '  Tc at Min MR =',mc.Tc_at_min_MR )
    print( 'Max MR = %g'%mc.max_MR, '  Tc at Max MR =',mc.Tc_at_max_MR )
    
    