from rocketcea.cea_obj_w_units import CEA_Obj, CEA_Obj_default
from rocketcea.cea_obj import get_rocketcea_data_dir #, set_rocketcea_data_dir

test_dir = r'D:\temp\cea w spaces'
print( 'ROCKETCEA_DATA_DIR =', get_rocketcea_data_dir() )

# set_rocketcea_data_dir( test_dir )

print( 'ROCKETCEA_DATA_DIR =', get_rocketcea_data_dir() )

# compare 200 psia (13.7895 bar) results

C = CEA_Obj(oxName='LOX', fuelName='LH2', pressure_units='bar', cstar_units='m/s', temperature_units='K', isp_units='N-s/kg')
IspVac, Cstar, Tcomb = C.get_IvacCstrTc( Pc=13.7895, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0)
print( 'IspVac=%g, Cstar=%g, Tcomb=%g'%(IspVac, Cstar, Tcomb) )
print( '  .... converted to English ....')
print( 'IspVac=%g, Cstar=%g, Tcomb=%g'%(IspVac*0.101972, Cstar*3.28084, Tcomb*1.8) )

Cd = CEA_Obj_default(oxName='LOX', fuelName='LH2')
IspVac, Cstar, Tcomb = Cd.get_IvacCstrTc( Pc=200.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0)
print( 'IspVac=%g, Cstar=%g, Tcomb=%g'%(IspVac, Cstar, Tcomb) )

print()
print( '... try getting transport properties ...')
print( C.get_Chamber_Transport( Pc=200.0, MR=1.0, eps=40.0, frozen=0) )
print( C.cea_obj.get_Chamber_Transport( Pc=200.0, MR=1.0, eps=40.0, frozen=0) )
print()
print( '... should get same chamber Cp regardless of frozen flag ...' )
print( 'frozen=0 C.get_Chamber_Cp =', C.get_Chamber_Cp( Pc=100.0, MR=1.0, eps=40.0, frozen=0) )
print( 'frozen=1 C.get_Chamber_Cp =', C.get_Chamber_Cp( Pc=100.0, MR=1.0, eps=40.0, frozen=1) )
