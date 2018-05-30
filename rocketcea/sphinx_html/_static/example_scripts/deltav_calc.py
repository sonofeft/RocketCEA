from math import log
from rocketcea.cea_obj import CEA_Obj

C = CEA_Obj( oxName='LOX', fuelName='LH2')

Wpayload = 8500.0 # lbm
Wstg = 5106.0 # lbm
Wpropellant = 45920.0 # lbm

Winit = Wstg + Wpropellant + Wpayload
Wfinal = Winit - Wpropellant

def show_deltaV( Pc=475.0, eps=84.0, MR=5.88 ):
    IspVac = C.get_Isp(Pc=Pc, MR=MR, eps=eps)
    IspDel = 0.969 * IspVac
    deltaV = 32.174 * IspDel * log( Winit / Wfinal ) # ft/sec
    print( '%8.1f %8.1f    %8.2f    %8.1f     %8.1f        %8.1f     '%(Pc, eps, MR, IspVac, IspDel, deltaV))

print(' Pc(psia) AreaRatio  MixtureRatio   IspVac(sec)  IspDel(sec)   deltaV(ft/sec)')
show_deltaV( Pc=475.0, eps=84.0 )

for eps in range(100, 251, 50):
    show_deltaV( Pc=475.0, eps=eps )

show_deltaV( Pc=475.0, eps=280.0 )