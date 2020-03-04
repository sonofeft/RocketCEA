
from rocketcea.cea_obj import CEA_Obj

ispObj = CEA_Obj( oxName='LOX', fuelName='LH2')
s = ispObj.get_full_cea_output( Pc=1000.0, MR=6.0, eps=40.0, frozen=1, frozenAtThroat=0,
                                short_output=1)

print( s )
