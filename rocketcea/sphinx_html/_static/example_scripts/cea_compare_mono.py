

from rocketcea.cea_obj import CEA_Obj
from pylab import *

Pc = 500.0
epsMin = 5.0
epsMax = 41.0
epsStep = 1.0

propL = ['HPB24','HAN315','HAN269','HYD40']
propL = ['HYD10','HYD20','HYD30','HYD40','HYD50']

sortedL = []
for propName in propL:
    ispObj = CEA_Obj(propName=propName)
    ispMax = ispObj( Pc=Pc, eps=epsMax )
    sortedL.append((ispMax, propName))
sortedL.sort()
sortedL.reverse()

for ispMax,propName in sortedL:

    ispObj = CEA_Obj(propName=propName)

    ispArr = []
    eps = epsMin
    epsArr = []
    while eps < epsMax:
        ispArr.append( ispObj(Pc=Pc, eps=eps ))
        epsArr.append(eps)
        eps += epsStep
    plot(epsArr, ispArr, label='%s'%(propName,), linewidth=2)

legend(loc='best')
grid(True)
title( 'Monoprop Ideal Performance Comparison' )
xlabel( 'Area Ratio' )
ylabel( 'IspVac (sec)' )
savefig('cea_compare_mono.png', dpi=120)

show()
