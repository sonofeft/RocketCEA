
from rocketcea.cea_obj import CEA_Obj
from pylab import *

Pc = 500.0
eps = 25.0
mrMin = 1.0
mrStep = 0.05
mrMax = 10.0


mrL = [mrMin + i*mrStep for i in range( int((mrMax-mrMin)/mrStep))]
rhoIspLL = [] # a list of lists of Isp data

# Specific Gravity of Propellants
sgD = {'OF2':1.521, 'B2H6':0.435, 'C3H8':0.583, 'CH4':0.444, 
      'C2H6_167':0.8343,'N2H4':1.0037, 'CLF5':1.776, 'MMH':0.8702, 
      'N2O4':1.433, 'NH3':0.405 }

for oxName,fuelName in [('OF2','B2H6'),('OF2','C3H8'),('OF2','CH4'),\
    ('OF2','C2H6_167'),('OF2','N2H4'),\
    ('CLF5','N2H4'),('N2O4','MMH')]:

    ispObj = CEA_Obj( oxName=oxName, fuelName=fuelName )
    
    ispL = [ispObj.get_Isp(Pc=Pc, MR=MR, eps=eps) for MR in mrL]
    
    sgOx = sgD[oxName]
    sgFuel = sgD[fuelName]
    
    rhoIspL = []
    for MR, Isp in zip( mrL, ispL ):
        sgBulk = (MR+1.0) / (MR/sgOx + 1./sgFuel)
        rhoIspL.append( Isp*sgBulk )
        
    rhoIspLL.append( [max(rhoIspL), '%s/%s'%(oxName,fuelName), rhoIspL] )

rhoIspLL.sort(reverse=True) # sort in-place from high to low

for maxIsp, name, rhoIspL in rhoIspLL:
    plot(mrL, rhoIspL, label=name, linewidth=2)
        

legend(loc='best')
grid(True)
title( 'rho*Isp Performance Comparison at Eps=%g, Pc=%g psia'%(eps,Pc) )
xlabel( 'Mixture Ratio' )
ylabel( 'SG x IspVac (sec)' )
savefig('cea_compareRhoIsp.png', dpi=120)
show()
