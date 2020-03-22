

from rocketcea.cea_obj import CEA_Obj
from pylab import *

Pc = 100.0
MR = 6.0
oxName = 'LOX'
fuelName = 'MMH'

ispObj = CEA_Obj(propName='', oxName=oxName, fuelName=fuelName)

cr_min = 1.4
cr_max = 5.1
d_cr = 0.2

crL = []
cea_pratL = []
est_pratL = []
fac_CR = cr_min

while fac_CR < cr_max:
    crL.append( fac_CR )
    cea_pratL.append( ispObj.get_Pinj_over_Pcomb( Pc=Pc, MR=MR, fac_CR=fac_CR) )
    est_pratL.append( 1.0 + 0.54 / fac_CR**2.2 )
    
    fac_CR += d_cr
plot(crL, cea_pratL, label='CEA %s/%s'%(oxName,fuelName), linewidth=2)
plot(crL, est_pratL, label='Est Eqn = 1.0 + 0.54 / CR**2.2', linewidth=2)

legend(loc='best')
grid(True)
title( 'Rayleigh Pressure Ratio for %s at Pc=%g'%(ispObj.desc, Pc) )
xlabel( 'Contraction Ratio' )
ylabel( 'Pinjector / Pchamber' )
savefig('pinj_over_pcomb.png', dpi=120)

show()
