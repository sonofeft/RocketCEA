
from rocketcea.cea_obj import CEA_Obj

Pc = 40.0
eps = 5.0

ispObj = CEA_Obj( oxName='GOX', fuelName='GH2')
s = ispObj.get_full_cea_output( Pc=Pc, MR=5.0, eps=eps, frozen=0, frozenAtThroat=0,
                                short_output=1, show_transport=1)

print( s )

print( """==================================""" )

print( ispObj.get_HeatCapacities( Pc=Pc, MR=.25, eps=eps ) )
print( ispObj.get_HeatCapacities( Pc=Pc, MR=5.0, eps=eps ) )
