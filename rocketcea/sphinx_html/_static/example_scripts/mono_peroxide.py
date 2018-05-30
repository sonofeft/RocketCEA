
from rocketcea.cea_obj import CEA_Obj

ispObj = CEA_Obj( propName='Peroxide98')
s = ispObj.get_full_cea_output( Pc=1000.0, eps=10.0)

print( s )