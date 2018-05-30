from rocketcea.cea_obj import CEA_Obj
from pylab import *

eps = 20.0
MR = 2.0

ispAmbArr = []
ispVacArr = []
pcArr = []

C = CEA_Obj(propName='', oxName='N2O4', fuelName="MMH")
for Pc in range(2000, 50, -50):
    
    IspAmb, mode = C.estimate_Ambient_Isp(Pc=Pc, MR=MR, eps=eps, Pamb=14.7)
    
    ispAmbArr.append( IspAmb )
    pcArr.append( Pc )
    
    IspVac = C.get_Isp( Pc=Pc, MR=MR, eps=eps)
    ispVacArr.append( IspVac )
        
plot(pcArr, ispVacArr, label='Isp Vacuum' )
plot(pcArr, ispAmbArr, label='Isp Sea Level' )

legend(loc='best')
grid(True)
title( C.desc + ' Sea Level Isp vs Pc\nMixture Ratio=%g, Area Ratio=%g'%(MR,eps) )
xlabel( 'Chamber Pressure (psia)' )
ylabel( 'Isp (sec)' )
savefig('IspAmb_plot.png', dpi=120)

show()
