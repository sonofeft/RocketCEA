from rocketcea.cea_obj import CEA_Obj

C = CEA_Obj(oxName="LOX", fuelName="H2",  useFastLookup=0)
Pc=500.
MR=6.0
for eps in [2., 5., 7., 10., 20., 50.]:
    
    IspVac = C.get_Isp( Pc=Pc, MR=MR, eps=eps)
    IspAmb, mode = C.estimate_Ambient_Isp(Pc=Pc, MR=MR, eps=eps, Pamb=14.7)
    
    print('Pc=%4i  eps=%3i  IspAmb/IspVac= %6.2f/%6.2f  Mode= %s'%(int(Pc),int(eps), IspAmb, IspVac, mode))
