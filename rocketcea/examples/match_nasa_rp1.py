from rocketcea.cea_obj import CEA_Obj, add_new_fuel, add_new_oxidizer, add_new_propellant

card_str = """
fuel NASA_RP-1  C 1.0   H 1.95   wt%=100.00
h,cal=-5907.672     t(k)=298.15   rho=.773
"""
add_new_fuel( 'NASA_RP_1', card_str )

C = CEA_Obj(oxName="GOX", fuelName="NASA_RP_1")

s = C.get_full_cea_output( Pc=1000.0, MR=3.0, eps=40.0, short_output=1)

print( s )