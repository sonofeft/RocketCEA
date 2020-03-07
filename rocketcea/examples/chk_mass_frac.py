from rocketcea.cea_obj import CEA_Obj

C = CEA_Obj( oxName='GOX', fuelName='GH2')
Pc=40.0
eps=5.0
MR=5.0

molWtD, massFracD = C.get_SpeciesMassFractions( Pc=Pc, MR=MR, eps=eps )
    
print('   ROCKETCEA MASS FRACTIONS')
for species, mfL in massFracD.items():
    
    s = '  '.join( [ '%7.5f'%mf for mf in mfL ] )
    print( ' %-15s '%species, s, '  MW=%g'%molWtD[species] )
print("""   CEA OUTPUT: MASS FRACTIONS
 *H               0.00637  0.00531  0.00081
 HO2              0.00002  0.00001  0.00000
 *H2              0.06108  0.06063  0.06109
 H2O              0.85269  0.87387  0.93445
 *O               0.00703  0.00463  0.00005
 *OH              0.06416  0.04962  0.00352
 *O2              0.00865  0.00593  0.00008
""")

# ================== frozen ==================
molWtD, massFracD = C.get_SpeciesMassFractions( Pc=Pc, MR=MR, eps=eps, 
                                                frozen=1, frozenAtThroat=1 )
print('   FROZEN ROCKETCEA MASS FRACTIONS')
for species, mfL in massFracD.items():
    
    s = '  '.join( [ '%7.5f'%mf for mf in mfL ] )
    print( ' %-15s '%species, s, '  MW=%g'%molWtD[species] )

