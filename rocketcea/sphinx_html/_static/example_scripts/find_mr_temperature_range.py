from rocketcea.biprop_utils.mr_t_limits import MR_Temperature_Limits

mc = MR_Temperature_Limits( oxName='N2O4', fuelName='NH3', oxPcentL=None, fuelPcentL=None,
        TC_LIMIT=1000.0, PcNominal=1000.0, epsNominal=10.0,
        MR_MIN=0.0, MR_MAX=10.0)
    
print( 'Stoich MR =',mc.Stoich_MR,'for %s/%s'%(mc.cea_oxName, mc.cea_fuelName))
print( 'Min MR = %g'%mc.min_MR, '  Tc at Min MR =',mc.Tc_at_min_MR)
print( 'Max MR = %g'%mc.max_MR, '  Tc at Max MR =',mc.Tc_at_max_MR)
print( str(mc) )
print('')

mc = MR_Temperature_Limits( oxName='MON12', fuelName='M10', oxPcentL=None, fuelPcentL=None,
        TC_LIMIT=1000.0, PcNominal=1000.0, epsNominal=10.0,
        MR_MIN=0.0, MR_MAX=1000.0)
    
print( 'Stoich MR =',mc.Stoich_MR,'for %s/%s'%(mc.cea_oxName, mc.cea_fuelName))
print( 'Min MR = %g'%mc.min_MR, '  Tc at Min MR =',mc.Tc_at_min_MR)
print( 'Max MR = %g'%mc.max_MR, '  Tc at Max MR =',mc.Tc_at_max_MR)
print('')


mc = MR_Temperature_Limits( oxName=['F2','O2'], fuelName=["N2H4","NH3"], oxPcentL=[65,35], fuelPcentL=[90,10],
        TC_LIMIT=1000.0, PcNominal=1000.0, epsNominal=10.0,
        MR_MIN=0.0, MR_MAX=1000.0)
    
print( 'Stoich MR =',mc.Stoich_MR,'for %s/%s'%(mc.cea_oxName, mc.cea_fuelName))
print( 'Min MR = %g'%mc.min_MR, '  Tc at Min MR =',mc.Tc_at_min_MR)
print( 'Max MR = %g'%mc.max_MR, '  Tc at Max MR =',mc.Tc_at_max_MR)
print('')
