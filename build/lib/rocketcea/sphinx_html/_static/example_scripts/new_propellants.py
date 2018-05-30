from rocketcea.cea_obj import CEA_Obj, add_new_fuel, add_new_oxidizer, add_new_propellant

card_str = """
oxid N2O4(L)   N 2 O 4   wt%=96.5
h,cal=-4676.0     t(k)=298.15
oxid SiO2  Si 1.0 O 2.0    wt%=3.5
h,cal=-216000.0     t(k)=298.15  rho=1.48
"""
add_new_oxidizer( 'GelN2O4', card_str )

# ==========
card_str = """
fuel CH6N2(L)  C 1.0   H 6.0   N 2.0     wt%=60.00
h,cal=12900.0     t(k)=298.15   rho=.874
fuel   AL AL 1.0   wt%=40.00
h,cal=0.0     t(k)=298.15   rho=0.1
"""
add_new_fuel( 'MMH_AL', card_str )

C = CEA_Obj(oxName="GelN2O4", fuelName="MMH_AL")

s = C.get_full_cea_output( Pc=1850.0, MR=0.7, eps=40.0, short_output=1)

print( s )