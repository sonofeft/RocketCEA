
from rocketcea.cea_obj import CEA_Obj
from pylab import *

Pc = 500.0
eps = 25.0
mrMin = 1.0
mrStep = 0.05
mrMax = 2.75

for oxName,fuelName in [('N2O4','N2H4'),('N2O4','M20'),('N2O4','MMH')]:

    ispObj = CEA_Obj( oxName=oxName, fuelName=fuelName )

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
savefig('cea_compare3.png', dpi=120)

show()
