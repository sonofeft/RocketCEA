from rocketcea.cea_obj import CEA_Obj, py_cea

C = CEA_Obj( oxName='GOX', fuelName='GH2')
Pc=40.0
eps=5.0
MR=5.0

molWtD, massFracD = C.get_SpeciesMassFractions( Pc=Pc, MR=MR, eps=eps )

# check to see that setting zero values on common blocks doesn't cause problems
for k,p in enumerate(py_cea.cdata.prod):
    p = p.decode("utf-8").strip()
    print('%i)'%k,p)
    for i in range(3):
        py_cea.comp.en[k-1,i] = 0.0
        py_cea.therm.mw[k-1] = 0.0
    if k>50:
        break

C = CEA_Obj( oxName='GOX', fuelName='GH2')
LmolWtD, LmassFracD = C.get_SpeciesMassFractions( Pc=Pc, MR=MR, eps=eps )

for k,mw in molWtD.items():
    if k in LmolWtD:
        print( k, molWtD[k], LmolWtD[k] )
        print( '    ', massFracD[k] )
        print( '    ', LmassFracD[k] )
    else:
        print('MISSING:', k)

