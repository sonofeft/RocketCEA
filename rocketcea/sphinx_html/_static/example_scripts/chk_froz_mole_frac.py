from rocketcea.cea_obj import CEA_Obj

ispObj = CEA_Obj( oxName='LOX', fuelName='LH2')

for frozen, frozenAtThroat in [(0,0), (1,0), (1,1)]:
    molWtD, moleFracD = ispObj.get_SpeciesMoleFractions( Pc=1000.0, MR=6.0, eps=40.0, 
                                            frozen=frozen, frozenAtThroat=frozenAtThroat )
                                            
    print('   ROCKETCEA MOLE FRACTIONS (frozen=%i, frozenAtThroat=%i)'%(frozen, frozenAtThroat)  )
    for species, mfL in moleFracD.items():
        
        s = '  '.join( [ '%7.5f'%mf for mf in mfL ] )
        print( ' %-15s '%species, s, '  MW=%g'%molWtD[species] )
    print('='*55)
