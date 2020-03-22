
from rocketcea.cea_obj import CEA_Obj
from pylab import *

crL = [1.4+i*0.2 for i in range(19) ]

propL = [('N2O4','MMH',2.0), ('CLF5','N2H4',2.5), 
         ('LOX','CH4',3.0), ('LOX','LH2',6.0)]

Pc = 500.0

est_pratL = [1.0 + 0.54 / cr**2.2 for cr in crL]
plot(crL, est_pratL, '.', label='Est Eqn = 1.0 + 0.54 / CR**2.2', markersize=16)    

def make_pratL( oxName, fuelName, MR ):
    C = CEA_Obj( oxName=oxName, fuelName=fuelName)
    pratL = [C.get_Pinj_over_Pcomb( Pc=Pc, MR=MR, fac_CR=cr) for cr in crL]
    
    plot(crL, pratL, label='%s/%s'%(oxName,fuelName), linewidth=2)

for (oxName, fuelName, MR) in propL:
    make_pratL( oxName, fuelName, MR )
    

legend(loc='best')
grid(True)
title( 'Rayleigh Line Loss' )
xlabel( 'Contraction Ratio' )
ylabel( 'Pinj_face / Pplenum' )
savefig('compare_rayleigh.png', dpi=120)

show()
    