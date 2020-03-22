
from rocketcea.cea_obj import CEA_Obj

ispObj = CEA_Obj( oxName='LOX', fuelName='LH2', fac_CR=2.5)
s = ispObj.get_full_cea_output( Pc=1000.0, MR=6.0, eps=40.0)

print( s )