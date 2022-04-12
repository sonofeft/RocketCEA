
from rocketcea.cea_obj import CEA_Obj, ROCKETCEA_DATA_DIR
print( 'ROCKETCEA_DATA_DIR =', ROCKETCEA_DATA_DIR )
C = CEA_Obj( oxName='LOX', fuelName='LH2', makeOutput=1, make_debug_prints=True)
Isp = C.get_Isp(Pc=100.0, MR=1.0, eps=40.0)
print( Isp )
