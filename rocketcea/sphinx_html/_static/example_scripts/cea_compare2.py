

from rocketcea.cea_obj import CEA_Obj
from pylab import *

Pc = 500.0
eps = 25.0
mrMin = 1.0
mrStep = 0.05
mrMax = 4.0

mrL = [mrMin + i*mrStep for i in range( int((mrMax-mrMin)/mrStep))]
ispLL = [] # a list of lists of Isp data

for oxName,fuelName in [('N2O4','N2H4'),('N2O4','MMH'),('N2O4','M20'),
    ('CLF5','N2H4'),('CLF5','NH3')]:

    ispObj = CEA_Obj( oxName=oxName, fuelName=fuelName )
    
    ispL = [ispObj.get_Isp(Pc=Pc, MR=MR, eps=eps) for MR in mrL]
    ispLL.append( [max(ispL), '%s/%s'%(oxName,fuelName), ispL] )

ispLL.sort(reverse=True) # sort in-place from high to low

for maxIsp, name, ispL in ispLL:
    plot(mrL, ispL, label=name, linewidth=2)

legend(loc='best')
grid(True)
title( 'Propellant Performance Comparison at Eps=%g, Pc=%g psia'%(eps,Pc) )
xlabel( 'Mixture Ratio' )
ylabel( 'Isp ODE (sec)' )
savefig('cea_compare2.png', dpi=120)

show()
