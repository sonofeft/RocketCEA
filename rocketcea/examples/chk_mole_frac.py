from rocketcea.cea_obj import CEA_Obj

C = CEA_Obj( oxName='GOX', fuelName='GH2')
Pc=40.0
eps=5.0
MR=5.0

molWtD, moleFracD = C.get_SpeciesMoleFractions( Pc=Pc, MR=MR, eps=eps )
    
print('   ROCKETCEA MOLE FRACTIONS')
for species, mfL in moleFracD.items():
    
    s = '  '.join( [ '%7.5f'%mf for mf in mfL ] )
    print( ' %-15s '%species, s, '  MW=%g'%molWtD[species] )
    
print("""   CEA OUTPUT MOLE FRACTIONS
 *H               0.07147  0.06044  0.00962
 HO2              0.00001  0.00000  0.00000
 *H2              0.34261  0.34471  0.36428
 H2O              0.53522  0.55597  0.62354
 *O               0.00497  0.00332  0.00004
 *OH              0.04266  0.03344  0.00249
 *O2              0.00306  0.00212  0.00003
""")
