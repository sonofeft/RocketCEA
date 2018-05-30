

prop_sgD = {} # propellant specific gravity dictionary

def bulkDensity(ox, fuel, MR):
    return (MR+1.0) / (MR/prop_sgD[ox] + 1.0/prop_sgD[fuel])

def add_prop_sg(name, sg):
    prop_sgD[name] = sg

def got_sg(name):
    return name in prop_sgD

def get_sg(name):
    return prop_sgD.get(name, None)

# AD370870= AFRPL reference from 1966, "Propellant Handbook"

prop_sgD['A50']     =  0.899596952885  # AD370870=0.899 (using PRISM)
prop_sgD['C2H6']    =  0.543829885939  # ...PRISM prolib
prop_sgD['C3H8']    =  0.58293882547   # ...PRISM prolib
prop_sgD['CH4']     =  0.446753686756  # ...PRISM prolib
prop_sgD['CLF3']    =  1.8517          # AD370870=1.8806 (using PRISM)
prop_sgD['CLF5']    =  1.77621958359   # AD370870=1.793 (using PRISM)
prop_sgD['Ethanol'] =  0.788877327915  # ...PRISM prolib
prop_sgD['F2']      =  1.4601150543    # AD370870=1.505 (using PRISM)
prop_sgD['H2']      =  0.0689229665441 # AD370870=0.071 (using PRISM)
prop_sgD['LH2']      =  0.0689229665441# AD370870=0.071 (using PRISM)
prop_sgD['H2O2']    =  1.4425          # AD370870=1.4425
prop_sgD['IRFNA']   =  1.55007474959   # AD370870=1.564 (using PRISM)
prop_sgD['M20']     =  0.974230004661  # ...PRISM prolib
prop_sgD['MMH']     =  0.871917046643  # AD370870=0.8743 (using PRISM)
prop_sgD['MON25']   =  1.35631540589   # ...PRISM prolib
prop_sgD['MHF3']    =  0.889079        # ...PRISM prolib
prop_sgD['N2F4']    =  1.65            # AD370870=1.65
prop_sgD['N2H4']    =  1.00367340036   # AD370870=1.008 (using PRISM)
prop_sgD['HYD40']    =  1.00367340036  # AD370870=1.008 (using PRISM)
prop_sgD['HAN315']    =  1.4651        # ...PRISM prolib
prop_sgD['HAN269']    =  1.417         # ...PRISM prolib
prop_sgD['N2O']     =  0.78029655698   # ...PRISM prolib
prop_sgD['N2O4']    =  1.43298874618   # AD370870=1.45 (using PRISM)
prop_sgD['NH3']     =  0.692           # AD370870=0.692
prop_sgD['O2']      =  1.14594811844   # AD370870=1.14 (using PRISM)
prop_sgD['LOX']      =  1.14594811844  # AD370870=1.14 (using PRISM)
prop_sgD['OF2']     =  1.546           # AD370870=1.546
prop_sgD['Propylene'] =  0.610059597606# ...PRISM prolib
prop_sgD['RP1']     =  0.788877327915  # AD370870=0.801 (using PRISM)
prop_sgD['UDMH']    =  0.788877327915  # AD370870=0.7861 (using PRISM)

def sg_mix( name1, name2, pcent1 ):
    f1 = pcent1/100.0
    f2 = 1.0 - f1
    invRho = f1/prop_sgD[name1] + f2/prop_sgD[name2]
    return 1.0/invRho

prop_sgD['FLOX70'] = sg_mix( 'F2', 'O2', 70.0 )
prop_sgD['FLOX80'] = sg_mix( 'F2', 'O2', 80.0 )

            
if __name__=="__main__":
    

    print( '--------------')
    keyL = prop_sgD.keys()
    keyL.sort()
    for name in keyL:
        print( '%10s, %s'%(name, prop_sgD[name]) )
    print()
    def pBulk(ox, fuel, MR):
        rhob = bulkDensity(ox, fuel, MR)
        print( '%s/%s at MR=%g has bulkDensity=%g g/ml = %g lbm/cuin'%(ox, fuel, MR, rhob, rhob*0.0361273))
    pBulk('N2O4','N2H4',1.0)

    pBulk('N2O4','A50',1.83)
    pBulk('LOX','RP1',2.27)
    pBulk('LOX','LH2',5.5)

