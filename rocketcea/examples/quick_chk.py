import sys
from rocketcea.cea_obj import CEA_Obj, __version__

def main():
    
    print('Python Version:',sys.version,'  RocketCEA Version:', __version__)
    
    C = CEA_Obj( oxName='N2O4', fuelName='MMH')
    Pc=100.0
    eps=10.0
    MR=1.0

    print(' Pc(psia) AreaRatio  MR  IspVac(sec)  Cstar(fps) Tc(degR)  MolWt    gamma')    
    IspVac, Cstar, Tc, MW, gamma = C.get_IvacCstrTc_ChmMwGam(Pc=Pc, MR=MR, eps=eps)
    print( '%8.1f %8.1f   %3.1f  %8.1f      %8.1f    %5.1f %8.2f  %8.4f '%\
         (Pc, eps, MR, IspVac, Cstar, Tc, MW, gamma))

if __name__=="__main__":
    
    main()
