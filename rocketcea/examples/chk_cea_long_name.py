from rocketcea.cea_obj import CEA_Obj, get_rocketcea_data_dir #, set_rocketcea_data_dir

test_dir = r'D:\temp\cea w spaces'
print( 'ROCKETCEA_DATA_DIR =', get_rocketcea_data_dir() )

# set_rocketcea_data_dir( test_dir )

print( 'ROCKETCEA_DATA_DIR =', get_rocketcea_data_dir() )

C = CEA_Obj( oxName='LOX', fuelName='LH2', make_debug_prints=True)
Isp = C.get_Isp(Pc=100.0, MR=1.0, eps=40.0)
print( Isp )

