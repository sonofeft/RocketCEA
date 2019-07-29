from engcoolprop.ec_fluid import EC_Fluid  # <== pip install EngCoolProp 
from rocketcea.blends import makeCardForNewTemperature
from rocketcea.cea_obj import CEA_Obj, fuelCards

# ======= use EngCoolProp to calculate dT and dH ========
fl = EC_Fluid(symbol="CH4" )
fl.setProps(P=14.7, Q=0) # Set T and all liquid
fl.printTPD() # Print state point at given T,P

ox = EC_Fluid(symbol="O2" )
ox.setProps(P=20.0, Q=0) # Set T and all liquid
ox.printTPD() # Print state point at given T,P

fl2 = EC_Fluid(symbol="CH4" )
fl2.setProps(T=ox.T, Q=0) # Set T and all liquid
fl2.printTPD() # Print state point at given T,P

dT = fl2.T-fl.T
dH = fl2.H-fl.H

print( '\nCH4 dT=%g degR, dH=%g BTU/lbm'%(dT, dH) )

# ======== Build a new adjusted CEA card for the subcooled CH4 ==========
CpAve = abs(dH / dT)
card_name = makeCardForNewTemperature( ceaName='CH4', newTdegR=fl2.T, CpAve=CpAve, MolWt=16.04 )
print('')
print( 'New Name = ' + card_name )
print( '\n'.join(fuelCards[card_name]) )
print('')
print( 'Standard CH4')
print( '\n'.join(fuelCards['CH4']) )

C = CEA_Obj( oxName='LOX', fuelName='CH4')
C2 = CEA_Obj( oxName='LOX', fuelName=card_name)

IspVac,  Cstar,  Tc  =  C.get_IvacCstrTc(Pc=3600, MR=3.8, eps=200)
IspVac2, Cstar2, Tc2 = C2.get_IvacCstrTc(Pc=3600, MR=3.8, eps=200)
print('')
print('       Both NBP    Common Temp')
print('IspVac  %6.1f     %6.1f sec'%(IspVac, IspVac2))
print('Cstar   %6.1f     %6.1f ft/sec'%(Cstar, Cstar2))
print('Tcomb   %6.1f     %6.1f degR'%(Tc, Tc2))