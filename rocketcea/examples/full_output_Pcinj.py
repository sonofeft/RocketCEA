"""
    figure out Pcinj_face to get desired Pcomb_end (100 atm in example)
"""
from rocketcea.cea_obj import CEA_Obj

cr = 2.5 # contraction ratio
ispObj = CEA_Obj( oxName='LOX', fuelName='LH2', fac_CR=cr)

# Use 100 atm to make output easy to read
Pc = 100.0 * 14.6959

# use correlation to make 1st estimate of Pcinj_face / Pcomb_end
PinjOverPcomb = 1.0 + 0.54 / cr**2.2 

# use RocketCEA to refine initial estimate
PinjOverPcomb = ispObj.get_Pinj_over_Pcomb( Pc=Pc * PinjOverPcomb, MR=6.0 )

# print results (noting that "COMB END" == 100.00 atm)
s = ispObj.get_full_cea_output( Pc=Pc * PinjOverPcomb, MR=6.0, eps=40.0)
print( s )

