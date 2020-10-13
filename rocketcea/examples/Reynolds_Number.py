from rocketcea.cea_obj import CEA_Obj

# Calculate Reynolds number in Chamber and at Throat.
ispObj = CEA_Obj( oxName='LOX', fuelName='MMH')
Pc = 100
MR = 1.0
eps= 40
Rt = 1.0 # inches
fac_CR = 2.5
frozen = 0

sonicL = ispObj.get_SonicVelocities( Pc=Pc, MR=MR, eps=eps, frozen=frozen)
Mchm = ispObj.get_Chamber_MachNumber( Pc=Pc, MR=MR, fac_CR=fac_CR )

velChm = Mchm * sonicL[0]
velTht = sonicL[1]
print( 'velChm=%g ft/sec(%g m/s),  velTht=%g ft/sec(%g m/s)'%(velChm, velChm*0.3048, velTht, velTht*0.3048 ) )

densityL = ispObj.get_Densities( Pc=Pc, MR=MR, eps=eps, frozen=frozen)
densChm = densityL[0]
densTht = densityL[1]
print( 'densChm=%g lbm/cuft,  densTht=%g lbm/cuft'%(densChm,  densTht ) )


Dt = 2.0*Rt
Dchm = Dt * fac_CR**0.5
print( 'Dchm=%g in(%g cm),  Dt=%g in(%g cm)'%(Dchm, Dchm*2.54, Dt, Dt*2.54 ) )

Cpchm, viscchm, condchm, Prandtlchm = ispObj.get_Chamber_Transport( Pc=Pc, MR=MR, eps=eps, frozen=frozen)
Cpt, visct, condt, Prandtlt = ispObj.get_Throat_Transport( Pc=Pc, MR=MR, eps=eps, frozen=frozen)

print('Cpchm=%g, viscchm=%g, condchm=%g, Prandtlchm=%g'%(Cpchm, viscchm, condchm, Prandtlchm) )
print('  Cpt=%g,   visct=%g,   condt=%g,   Prandtlt=%g'%(Cpt, visct, condt, Prandtlt) )

print('================')
viscchm *= 0.067196898 / 1000 # convert from millipoise to lbm/s-ft
visct   *= 0.067196898 / 1000

ReyChm = densChm * velChm * Dchm / viscchm / 12.0
ReyTht = densTht * velTht * Dt / visct     / 12.0

print( 'ReyChm=%g ,  ReyTht=%g '%(ReyChm,  ReyTht ) )




