
'''If CEA gives back Isp==0, then try to estimate it.'''
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
def make_cea_ode_est(cea_ispObj, Pc=100.0, MR=1.0, eps=40.0):
    
    # Got here if simple call did not work 
    # Try averaging a high and low Pc
    pc_hi = Pc * 1.05
    pc_lo = Pc * 0.95

    ispODE_hi = cea_ispObj.get_Isp( Pc=pc_hi, MR=MR, eps=eps)
    ispODE_lo = cea_ispObj.get_Isp( Pc=pc_lo, MR=MR, eps=eps)
    
    # If both look good, take the average
    if ispODE_hi>0.1 and ispODE_lo>0.1:
        ispODE = (ispODE_hi+ispODE_lo)/2.0
    else:
        # hope there's a good one (Is hope a plan?)
        ispODE = max(ispODE_hi, ispODE_lo)
        print( 'WARNING... make_cea_ode_est is returning max of hi and low =',ispODE )
        
    print( 'Estimated CEA_Isp at Pc=%g, MR=%g, eps=%g'%(Pc, MR, eps),'ispODE%g'%ispODE )
    return ispODE

def make_cea_odf_est(cea_ispObj, Pc=100.0, MR=1.0, eps=40.0):
                
    # Got here if simple call did not work 
    # Try averaging a high and low Pc
    pc_hi = Pc * 1.05
    pc_lo = Pc * 0.95

    ispFroz_hi,cstrODE,tcODE = cea_ispObj.getFrozen_IvacCstrTc( Pc=pc_hi, MR=MR, eps=eps, frozenAtThroat=0)
    ispFroz_lo,cstrODE,tcODE = cea_ispObj.getFrozen_IvacCstrTc( Pc=pc_lo, MR=MR, eps=eps, frozenAtThroat=0)
    
    # If both look good, take the average
    if ispFroz_hi>0.1 and ispFroz_lo>0.1:
        ispFroz = (ispFroz_hi+ispFroz_lo)/2.0
    else:
        # hope there's a good one (Is hope a plan?)
        ispFroz = max(ispFroz_hi, ispFroz_lo)
        if ispFroz <= 0.1:
            print( "Searching...", end='')
            for i_hope in range(18):
                if ispFroz <= 0.1:
                    print( ".", end='')
                    pc_hi *= 1.1
                    pc_lo *= 0.9

                    ispFroz_hi,cstrODE,tcODE = cea_ispObj.getFrozen_IvacCstrTc( Pc=pc_hi, MR=MR, eps=eps, frozenAtThroat=0)
                    ispFroz_lo,cstrODE,tcODE = cea_ispObj.getFrozen_IvacCstrTc( Pc=pc_lo, MR=MR, eps=eps, frozenAtThroat=0)
                    ispFroz = max(ispFroz_hi, ispFroz_lo)
            if ispFroz <= 0.1:
                print( "_NOT_FOUND_")
                print( 'Estimated CEA_Isp Frozen at Pc=%g, MR=%g, eps=%g'%(Pc, MR, eps),'ispFroz=%g'%ispFroz)
            else:
                print('')
            
    #print( 'Estimated CEA_Isp Frozen at Pc=%g, MR=%g, eps=%g'%(Pc, MR, eps),'ispFroz=%g'%ispFroz)
    return ispFroz


def run_cea_ode(cea_ispObj, Pc=100.0, MR=1.0, eps=40.0):
        
    ispODE = cea_ispObj.get_Isp( Pc=Pc, MR=MR, eps=eps)  

    if ispODE > 0.1: # i.e. it's probably OK
        return ispODE
    else:
        return make_cea_ode_est(cea_ispObj, Pc=Pc, MR=MR, eps=eps)

def run_cea_odf(cea_ispObj, Pc=100.0, MR=1.0, eps=40.0):
        
    ispFroz,cstrODE,tcODE = cea_ispObj.getFrozen_IvacCstrTc( Pc=Pc, MR=MR, eps=eps, frozenAtThroat=0)

    if ispFroz > 0.1: # i.e. it's probably OK
        return ispFroz
    else:
        return make_cea_odf_est(cea_ispObj, Pc=Pc, MR=MR, eps=eps)

def run_all_cea(cea_ispObj, Pc=100.0, MR=1.0, eps=40.0):
        
    ispFroz,cstrODE,tcODE = cea_ispObj.getFrozen_IvacCstrTc( Pc=Pc, MR=MR, eps=eps, frozenAtThroat=0)
    if ispFroz < 0.1: # try a guess
        ispFroz = make_cea_odf_est(cea_ispObj, Pc=Pc, MR=MR, eps=eps)

    ispODE = run_cea_ode(cea_ispObj, Pc=Pc, MR=MR, eps=eps)
    
    return ispODE, ispFroz, cstrODE, tcODE

if __name__=="__main__":
    
    from rocketcea.cea_obj import CEA_Obj
    
    cea_ispObj = CEA_Obj(oxName='LOX', fuelName='LH2')
    
    for pc,mr,eps in [(100.,1.0,40),(100.,2.0,40),(400.,1.2453509339,32.),(400.,2.5,32.)]:
    
        isp_ode = run_cea_ode(cea_ispObj, Pc=pc, MR=mr, eps=eps)
        isp_odf = run_cea_odf(cea_ispObj, Pc=pc, MR=mr, eps=eps)
        isp_est = make_cea_ode_est(cea_ispObj, Pc=pc, MR=mr, eps=eps)
        isp_froz_est = make_cea_odf_est(cea_ispObj, Pc=pc, MR=mr, eps=eps)
        print( '     isp_ode =',isp_ode)
        print( '     isp_odf =',isp_odf)
        print( '     isp_est =',isp_est)
        print( 'isp_froz_est =',isp_froz_est)
        print('')
    
    
    ispODE, ispFroz, cstrODE, tcODE = run_all_cea(cea_ispObj, Pc=100.0, MR=6.0, eps=40.0)
    print('Run All:', ispODE, ispFroz, cstrODE, tcODE)
    
