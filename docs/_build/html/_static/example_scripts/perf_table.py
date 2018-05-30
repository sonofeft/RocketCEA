from rocketcea.cea_obj import CEA_Obj

C = CEA_Obj( oxName='N2O4', fuelName='MMH')

def show_perf( Pc=100.0, eps=10.0, MR=1.0 ):
    
    IspVac, Cstar, Tc, MW, gamma = C.get_IvacCstrTc_ChmMwGam(Pc=Pc, MR=MR, eps=eps)
    
    print( '%8.1f %8.1f   %8.1f       %8.1f      %8.1f    %8.1f %8.2f  %8.4f '%\
         (Pc, eps, MR, IspVac, Cstar, Tc, MW, gamma))

print(' Pc(psia) AreaRatio  MixtureRatio   IspVac(sec)  Cstar(ft/sec) Tc(degR)  MolWt    gamma')

Pc = 250.0
eps = 50.0
for MR in [1.0 + i*0.1 for i in range(20)]:
    show_perf( Pc=Pc, eps=eps, MR=MR )

