

from rocketcea.cea_obj import CEA_Obj
from pylab import *

Pc = 500.0
eps = 25.0
mrMin = 2.0
mrStep = 0.05
mrMax = 4.0

# ('OF2','NH3'),
for oxName,fuelName in [('LOX','CH4'),('GOX','GCH4')]:

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
title( 'Propellant Performance Comparison at Eps=%g, Pc=%g psia'%(eps,Pc) )
xlabel( 'Mixture Ratio' )
ylabel( 'Isp ODE (sec)' )
savefig('lox_gox_compare.png', dpi=120)

show()
