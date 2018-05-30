from rocketcea.cea_obj import CEA_Obj, add_new_fuel, add_new_oxidizer, add_new_propellant

# ==========
card_str = """
name H2O2(L) H 2 O 2  wt%=100.00
h,cal=-44880.0     t(k)=298.15  rho.g/cc=1.407
"""
add_new_propellant( 'MyProp', card_str )
C = CEA_Obj(propName="MyProp")

s = C.get_full_cea_output( Pc=250.0, eps=40.0, short_output=1)

print( s )