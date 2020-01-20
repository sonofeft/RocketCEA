"""
In order to use makeCardForNewTemperature, the propellant definition card can
only have one component.  A propellant such as Peroxide90, for example has
the definition of both H2O2 and WATER.

        "Peroxide90":[" oxid H2O2(L) H 2 O 2  wt%=90.00 ",
                  " h,cal=-44880.0     t(k)=298.15  rho.g/cc=1.407 ",
                   " oxid = WATER H 2.0 O 1.0 wt%= 10.0        ",
                   " h,cal=-68317. t(k)=298.15 rho.g/cc=1.0   "],

This should be reduced to a single definition such as:

        "Peroxide_90h2o2" : ["oxid Peroxide_90h2o2 H 2 O 1.82659  wt%=100.0  ",
                             "h,cal=-48944.2  t(k)=298.15 rho,g/cc = 1.395"],  
]

"""
from rocketcea.cea_obj import CEA_Obj, add_new_fuel, add_new_oxidizer, add_new_propellant
from rocketcea.blends import makeCardForNewTemperature
from pylab import *

# ======== make a single component card for 90% Peroxide ===========
Mh2o2 = 34.014 # g/mole
Mh2o  = 18.015 # g/mole

mass_frac_h2o2 = 0.9

moles_h2o2 = mass_frac_h2o2 / Mh2o2
moles_h2o  = (1.0 - mass_frac_h2o2) / Mh2o

moles_total = moles_h2o2 + moles_h2o

mole_frac_h2o2 = moles_h2o2 / moles_total
mole_frac_h2o  = moles_h2o  / moles_total

frac_ox = 2.0 * mole_frac_h2o2 + mole_frac_h2o
frac_h  = 2.0 * (mole_frac_h2o2 + mole_frac_h2o)

MolWtMixture = frac_ox*15.999 + frac_h*1.008

print( 'frac_ox:%g  frac_h:%g   MolWtMixture:%g'%(frac_ox, frac_h, MolWtMixture) )

cal_per_mole_h2o2 = -44880.0
cal_per_mole_h2o  = -68317.

cal_per_mole = cal_per_mole_h2o2 * mole_frac_h2o2 + cal_per_mole_h2o * mole_frac_h2o
print( 'cal_per_mole:%g'%cal_per_mole )

card_str = """
oxid Peroxide_90h2o2 H %g O %g  wt%%=100.0 
h,cal=%g  t(k)=298.15 rho,g/cc = 1.395  
"""%(frac_h, frac_ox, cal_per_mole)

print( card_str )

# ============== Add the new card to RocketCEA ==============
add_new_oxidizer( 'Peroxide_90h2o2', card_str )

# ============= Make a sample run at nominal conditions ==========
#            (gives exact same results as library Peroxide90) 
C = CEA_Obj(propName="Peroxide_90h2o2")
s = C.get_full_cea_output( Pc = 1000.0,eps=2.0,short_output = 1)
print( s )

# =============== make plot ================
TvalL = []
IspL  = []
for TdegR in range(500, 660, 10):
    
    new_card = makeCardForNewTemperature(ceaName='Peroxide_90h2o2', newTdegR=TdegR, CpAve=0.66, MolWt=MolWtMixture)
    ispObj = CEA_Obj( propName=new_card )
    
    TvalL.append( TdegR )
    IspL.append( ispObj(Pc=1000.0, eps=2.0 ) )
    
plot(TvalL, IspL, label='90% Peroxide', linewidth=2)

legend(loc='best')
grid(True)
title( '90% Peroxide Ideal Performance' )
xlabel( 'Temperature (R)' )
ylabel( 'IspVac (sec)' )

show()

