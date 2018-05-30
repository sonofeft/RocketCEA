
from rocketcea.cea_obj import CEA_Obj
from pylab import *

Pc = 500.0

ispIRFNA = CEA_Obj(propName='', oxName='IRFNA', fuelName="MHF3")
for e in [50.0,20.0,10.0]:
    ispArr = []
    MR = 1.1
    mrArr = []
    while MR < 3.5:
        ispArr.append( ispIRFNA(Pc, MR, e ))
        mrArr.append(MR)
        MR += 0.05
    plot(mrArr, ispArr, label='AreaRatio %g'%e)

legend(loc='best')
grid(True)
title( ispIRFNA.desc )
xlabel( 'Mixture Ratio' )
ylabel( 'Isp ODE (sec)' )
savefig('cea_plot.png', dpi=120)

show()
