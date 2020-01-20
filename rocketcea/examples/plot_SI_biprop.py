

from rocketcea.cea_obj_w_units import CEA_Obj
from pylab import *

Pc = 1000.0 / 145.037738 # convert to MPa
eps = 100.0
mrMin = 3.0
mrStep = 0.05
mrMax = 8.0

oxName = 'LOX'
fuelName = 'LH2'

ispObj = CEA_Obj(propName='', oxName=oxName, fuelName=fuelName,
                 pressure_units='MPa', isp_units='km/s')

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
title( '%s Performance at Eps=%g, Pc=%g %s'%(ispObj.desc, eps, Pc, ispObj.isp_units) )
xlabel( 'Mixture Ratio' )
ylabel( 'Isp ODE (%s)'%ispObj.isp_units )
savefig('biprop_SI_isp.png', dpi=120)

show()
