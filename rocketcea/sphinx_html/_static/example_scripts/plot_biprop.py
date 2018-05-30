

from rocketcea.cea_obj import CEA_Obj
from pylab import *

Pc = 1000.0
eps = 100.0
mrMin = 3.0
mrStep = 0.05
mrMax = 8.0

oxName = 'LOX'
fuelName = 'LH2'

ispObj = CEA_Obj(propName='', oxName=oxName, fuelName=fuelName)

ispArr = []
MR = mrMin
mrArr = []
while MR < mrMax:
    ispArr.append( ispObj(Pc, MR, eps ))
    mrArr.append(MR)
    MR += mrStep
plot(mrArr, ispArr, label='%s/%s'%(oxName,fuelName), linewidth=2)

legend(loc='best')
grid(True)
title( '%s Performance at Eps=%g, Pc=%g psia'%(ispObj.desc, eps, Pc) )
xlabel( 'Mixture Ratio' )
ylabel( 'Isp ODE (sec)' )
savefig('biprop_isp.png', dpi=120)

show()
