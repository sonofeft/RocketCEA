
from rocketcea.cea_obj import CEA_Obj

ispObj = CEA_Obj( oxName='LOX', fuelName='LH2')
s = ispObj.get_full_cea_output( Pc=1000.0, MR=6.0, eps=40.0, frozen=0, frozenAtThroat=0,
                                short_output=1, show_transport=1)

print( s )

print( """==================================
   call get_HeatCapacities( Pc=1000.0, MR=6.0, eps=40.0 )
   call get_HeatCapacities( Pc=1000.0, MR=6.0, eps=40.0, frozen=1 )
   """ )

print( ispObj.get_HeatCapacities( Pc=1000.0, MR=6.0, eps=40.0 ) )
print( ispObj.get_HeatCapacities( Pc=1000.0, MR=6.0, eps=40.0, frozen=1 ) )
