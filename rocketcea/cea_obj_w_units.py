from __future__ import print_function
#!/usr/bin/env python
# -*- coding: ascii -*-


from rocketcea.cea_obj import CEA_Obj as CEA_Obj_default
from rocketcea.units import get_units_obj

class CEA_Obj( object ):
    """
    RocketCEA wraps the NASA FORTRAN CEA code to calculate Isp, cstar, and Tcomb
    
    This object wraps the English unit version of CEA_Obj to enable desired user units.    
    """

    def __init__(self, propName='', oxName='', fuelName='', 
        useFastLookup=0, makeOutput=0, 
        isp_units='sec', cstar_units='ft/sec', 
        pressure_units='psia', temperature_units='degR', 
        sonic_velocity_units='ft/sec', enthalpy_units='BTU/lbm', 
        density_units='lbm/cuft', specific_heat_units='BTU/lbm degR',
        viscosity_units='millipoise', thermal_cond_units='mcal/cm-K-s', 
        fac_CR=None, make_debug_prints=False):
        """::
        
        #: RocketCEA wraps the NASA FORTRAN CEA code to calculate Isp, cstar, and Tcomb
        #: This object wraps the English unit version of CEA_Obj to enable desired user units.
        #: Same as CEA_Obj with standard units except, input and output units can be specified.
        #:  parameter             default             options
        #: isp_units            = 'sec',         # N-s/kg, m/s, km/s
        #: cstar_units          = 'ft/sec',      # m/s
        #: pressure_units       = 'psia',        # MPa, KPa, Pa, Bar, Atm, Torr
        #: temperature_units    = 'degR',        # K, C, F
        #: sonic_velocity_units = 'ft/sec',      # m/s
        #: enthalpy_units       = 'BTU/lbm',     # J/g, kJ/kg, J/kg, kcal/kg, cal/g
        #: density_units        = 'lbm/cuft',    # g/cc, sg, kg/m^3
        #: specific_heat_units  = 'BTU/lbm degR' # kJ/kg-K, cal/g-C, J/kg-K (# note: cal/g K == BTU/lbm degR)
        #: viscosity_units      = 'millipoise'   # lbf-sec/sqin, lbf-sec/sqft, lbm/ft-sec, poise, centipoise
        #: thermal_cond_units   = 'mcal/cm-K-s'  # millical/cm-degK-sec, BTU/hr-ft-degF, BTU/s-in-degF, cal/s-cm-degC, W/cm-degC
        #: fac_CR, Contraction Ratio of finite area combustor (None=infinite)
        #: if make_debug_prints is True, print debugging info to terminal.
        """

        self.isp_units            = isp_units
        self.cstar_units          = cstar_units
        self.pressure_units       = pressure_units
        self.temperature_units    = temperature_units
        self.sonic_velocity_units = sonic_velocity_units
        self.enthalpy_units       = enthalpy_units
        self.density_units        = density_units
        self.specific_heat_units  = specific_heat_units
        self.viscosity_units      = viscosity_units
        self.thermal_cond_units   = thermal_cond_units
        self.fac_CR               = fac_CR
        
        # Units objects for input/output (e.g. Pc and Pamb)
        self.Pc_U       = get_units_obj('psia', pressure_units )
        
        # units of output quantities
        self.isp_U            = get_units_obj('sec', isp_units)
        self.cstar_U          = get_units_obj('ft/sec', cstar_units)
        self.temperature_U    = get_units_obj('degR', temperature_units)
        self.sonic_velocity_U = get_units_obj('ft/sec', sonic_velocity_units)
        self.enthalpy_U       = get_units_obj('BTU/lbm', enthalpy_units)
        self.density_U        = get_units_obj('lbm/cuft', density_units)
        self.specific_heat_U  = get_units_obj('BTU/lbm degR', specific_heat_units)
        self.viscosity_U      = get_units_obj('millipoise', viscosity_units)
        self.thermal_cond_U   = get_units_obj('mcal/cm-K-s', thermal_cond_units)
        
        self.cea_obj = CEA_Obj_default(propName=propName, oxName=oxName, fuelName=fuelName, 
                               useFastLookup=useFastLookup, makeOutput=makeOutput,
                               fac_CR=fac_CR, make_debug_prints=make_debug_prints)
        self.desc = self.cea_obj.desc
    
    def get_IvacCstrTc(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        IspVac, Cstar, Tcomb = self.cea_obj.get_IvacCstrTc( Pc=Pc, MR=MR, eps=eps, 
                                    frozen=frozen, frozenAtThroat=frozenAtThroat )
        
        IspVac = self.isp_U.dval_to_uval( IspVac )
        Cstar  = self.cstar_U.dval_to_uval( Cstar )
        Tcomb = self.temperature_U.dval_to_uval( Tcomb )
        
        return IspVac, Cstar, Tcomb
    
    def getFrozen_IvacCstrTc(self, Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        IspFrozen, Cstar, Tcomb = self.cea_obj.getFrozen_IvacCstrTc( Pc=Pc, MR=MR, eps=eps, 
                                                             frozenAtThroat=frozenAtThroat)
        IspFrozen = self.isp_U.dval_to_uval( IspFrozen )
        Cstar  = self.cstar_U.dval_to_uval( Cstar )
        Tcomb = self.temperature_U.dval_to_uval( Tcomb )
        
        return IspFrozen, Cstar, Tcomb
        
    def get_IvacCstrTc_exitMwGam(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        IspVac, Cstar, Tcomb, mw, gam = self.cea_obj.get_IvacCstrTc_exitMwGam( Pc=Pc, MR=MR, eps=eps,
                                                        frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        IspVac = self.isp_U.dval_to_uval( IspVac )
        Cstar  = self.cstar_U.dval_to_uval( Cstar )
        Tcomb = self.temperature_U.dval_to_uval( Tcomb )
        
        return IspVac, Cstar, Tcomb, mw, gam
        
    def get_IvacCstrTc_ChmMwGam(self, Pc=100.0, MR=1.0, eps=40.0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        IspVac, Cstar, Tcomb, mw, gam = self.cea_obj.get_IvacCstrTc_ChmMwGam( Pc=Pc, MR=MR, eps=eps )
        
        IspVac = self.isp_U.dval_to_uval( IspVac )
        Cstar  = self.cstar_U.dval_to_uval( Cstar )
        Tcomb = self.temperature_U.dval_to_uval( Tcomb )
        
        return IspVac, Cstar, Tcomb, mw, gam
        
    def get_IvacCstrTc_ThtMwGam(self, Pc=100.0, MR=1.0, eps=40.0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        IspVac, Cstar, Tcomb, mw, gam = self.cea_obj.get_IvacCstrTc_ThtMwGam( Pc=Pc, MR=MR, eps=eps )
        
        IspVac = self.isp_U.dval_to_uval( IspVac )
        Cstar  = self.cstar_U.dval_to_uval( Cstar )
        Tcomb = self.temperature_U.dval_to_uval( Tcomb )
        
        return IspVac, Cstar, Tcomb, mw, gam


    def __call__(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        return self.get_Isp(Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)

    def get_Isp(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        IspVac = self.cea_obj.get_Isp( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat )
        IspVac = self.isp_U.dval_to_uval( IspVac )
        
        return IspVac
        
    def get_Cstar(self, Pc=100.0, MR=1.0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Cstar = self.cea_obj.get_Cstar( Pc=Pc, MR=MR )
        Cstar  = self.cstar_U.dval_to_uval( Cstar )
        return Cstar
        
    def get_Tcomb(self, Pc=100.0, MR=1.0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Tcomb = self.cea_obj.get_Tcomb( Pc=Pc, MR=MR )
        Tcomb  = self.temperature_U.dval_to_uval( Tcomb )
        return Tcomb
        
    def get_PcOvPe(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_PcOvPe( Pc=Pc, MR=MR, eps=eps, 
                                        frozen=frozen, frozenAtThroat=frozenAtThroat )
        
    def get_eps_at_PcOvPe(self, Pc=100.0, MR=1.0, PcOvPe=1000.0, frozen=0, frozenAtThroat=0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_eps_at_PcOvPe( Pc=Pc, MR=MR, PcOvPe=PcOvPe, 
                                               frozen=frozen, frozenAtThroat=frozenAtThroat )
        
    def get_Throat_PcOvPe(self, Pc=100.0, MR=1.0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_Throat_PcOvPe( Pc=Pc, MR=MR )
    
    def get_Pinj_over_Pcomb(self, Pc=100.0, MR=1.0, fac_CR=None):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_Pinj_over_Pcomb(Pc=Pc, MR=MR, fac_CR=fac_CR)
        
    def get_MachNumber(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_MachNumber( Pc=Pc, MR=MR, eps=eps, 
                                            frozen=frozen, frozenAtThroat=frozenAtThroat )
        
    def get_Temperatures(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        tempList = self.cea_obj.get_Temperatures( Pc=Pc, MR=MR, eps=eps,
                                                  frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        for i,T in enumerate( tempList ):
            tempList[i] = self.temperature_U.dval_to_uval( T )
        return tempList # Tc, Tthroat, Texit
        
    def get_SonicVelocities(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        sonicList = self.cea_obj.get_SonicVelocities( Pc=Pc, MR=MR, eps=eps, 
                                                      frozen=frozen, frozenAtThroat=frozenAtThroat )
        
        for i,S in enumerate( sonicList ):
            sonicList[i] = self.sonic_velocity_U.dval_to_uval( S )
        return sonicList # Chamber, Throat, Exit
        
    def get_Chamber_SonicVel(self, Pc=100.0, MR=1.0, eps=40.0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        sonicVel = self.cea_obj.get_Chamber_SonicVel( Pc=Pc, MR=MR, eps=eps )
        
        sonicVel = self.sonic_velocity_U.dval_to_uval( sonicVel )
        return sonicVel
        
    def get_Enthalpies(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        
        hList = self.cea_obj.get_Enthalpies( Pc=Pc, MR=MR, eps=eps, 
                                             frozen=frozen, frozenAtThroat=frozenAtThroat )
        for i,H in enumerate( hList ):
            hList[i] = self.enthalpy_U.dval_to_uval( H )
        
        return hList
    def get_SpeciesMassFractions(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0,
                                 min_fraction=0.000005):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        
        molWtD, massFracD = self.cea_obj.get_SpeciesMassFractions(Pc=Pc, MR=MR, eps=eps, 
                                        frozenAtThroat=frozenAtThroat,min_fraction=min_fraction)
        return molWtD, massFracD

    def get_SpeciesMoleFractions(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0,
                                 min_fraction=0.000005):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        
        molWtD, moleFracD = self.cea_obj.get_SpeciesMoleFractions(Pc=Pc, MR=MR, eps=eps, 
                                        frozenAtThroat=frozenAtThroat, min_fraction=min_fraction)
        return molWtD, moleFracD
        
    def get_Chamber_H(self, Pc=100.0, MR=1.0, eps=40.0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        H = self.cea_obj.get_Chamber_H( Pc=Pc, MR=MR, eps=eps )
        return self.enthalpy_U.dval_to_uval( H )
        
    def get_Densities(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        dList = self.cea_obj.get_Densities( Pc=Pc, MR=MR, eps=eps, 
                                            frozen=frozen, frozenAtThroat=frozenAtThroat )
        
        for i,d in enumerate( dList ):
            dList[i] = self.density_U.dval_to_uval( d )
        
        return dList
        
    def get_Chamber_Density(self, Pc=100.0, MR=1.0, eps=40.0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        H = self.cea_obj.get_Chamber_Density( Pc=Pc, MR=MR, eps=eps )
        return self.density_U.dval_to_uval( H )
        
    def get_HeatCapacities(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        cpList = self.cea_obj.get_HeatCapacities( Pc=Pc, MR=MR, eps=eps, 
                                                  frozen=frozen, frozenAtThroat=frozenAtThroat )
        
        for i,cp in enumerate( cpList ):
            cpList[i] = self.specific_heat_U.dval_to_uval( cp )
        
        return cpList
        
    def get_Chamber_Cp(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """
        NOTE: should return same value regardless of frozen flag or eps.
        """
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Cp = self.cea_obj.get_Chamber_Cp( Pc=Pc, MR=MR, eps=eps, frozen=frozen )
        return self.specific_heat_U.dval_to_uval( Cp )
        
    def get_Throat_Isp(self, Pc=100.0, MR=1.0, frozen=0):
        """
        NOTE: should return same value regardless of eps.
        """
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Isp = self.cea_obj.get_Throat_Isp( Pc=Pc, MR=MR, frozen=frozen)
        Isp = self.isp_U.dval_to_uval( Isp )
        
        return Isp
        
    def get_Chamber_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_Chamber_MolWt_gamma( Pc=Pc, MR=MR, eps=eps )
        
    def get_Throat_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_Throat_MolWt_gamma( Pc=Pc, MR=MR, eps=eps, frozen=frozen )
        
    def get_exit_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0, 
                             frozen=0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_exit_MolWt_gamma( Pc=Pc, MR=MR, eps=eps, 
                                                  frozen=frozen, 
                                                  frozenAtThroat=frozenAtThroat)
        
    def get_eqratio(self, Pc=100.0, MR=1.0, eps=40.0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        return self.cea_obj.get_eqratio( Pc=Pc, MR=MR, eps=eps )
        
    def getMRforER(self, ERphi=None, ERr=None):
        return self.cea_obj.getMRforER( ERphi=ERphi, ERr=ERr )
        
    def get_description(self):
        return self.cea_obj.get_description()
        
    def estimate_Ambient_Isp(self, Pc=100.0, MR=1.0, eps=40.0, Pamb=14.7, 
                             frozen=0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Pamb = self.Pc_U.uval_to_dval( Pamb ) # convert user units to psia
        IspAmb, mode = self.cea_obj.estimate_Ambient_Isp(Pc=Pc, MR=MR, eps=eps, Pamb=Pamb,
                                                         frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        IspAmb = self.isp_U.dval_to_uval( IspAmb )
        
        return IspAmb, mode


    def get_PambCf(self, Pamb=14.7, Pc=100.0, MR=1.0, eps=40.0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Pamb = self.Pc_U.uval_to_dval( Pamb ) # convert user units to psia
        
        CFcea, CF, mode = self.cea_obj.get_PambCf( Pamb=Pamb, Pc=Pc, MR=MR, eps=eps)
        
        return CFcea, CF, mode

    def getFrozen_PambCf(self, Pamb=0.0, Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0):
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Pamb = self.Pc_U.uval_to_dval( Pamb ) # convert user units to psia
        
        CFcea,CFfrozen, mode = self.cea_obj.getFrozen_PambCf( Pamb=Pamb, Pc=Pc, MR=MR, eps=eps,
                                                              frozenAtThroat=frozenAtThroat)
        
        return CFcea,CFfrozen, mode



    def get_Chamber_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Cp, visc, cond, Prandtl = self.cea_obj.get_Chamber_Transport( Pc=Pc, MR=MR, eps=eps, frozen=frozen)
        
        #Cp = Cp * 8314.51 / 4184.0  # convert into BTU/lbm degR
        Cp = self.specific_heat_U.dval_to_uval( Cp )
        visc = self.viscosity_U.dval_to_uval( visc )
        cond = self.thermal_cond_U.dval_to_uval( cond )
        
        return Cp, visc, cond, Prandtl
        
    def get_Throat_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Cp, visc, cond, Prandtl = self.cea_obj.get_Throat_Transport( Pc=Pc, MR=MR, eps=eps, frozen=frozen)
        
        #Cp = Cp * 8314.51 / 4184.0  # convert into BTU/lbm degR
        Cp = self.specific_heat_U.dval_to_uval( Cp )
        visc = self.viscosity_U.dval_to_uval( visc )
        cond = self.thermal_cond_U.dval_to_uval( cond )
        
        return Cp, visc, cond, Prandtl
        
    def get_Exit_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        
        Pc = self.Pc_U.uval_to_dval( Pc ) # convert user units to psia
        Cp, visc, cond, Prandtl = self.cea_obj.get_Exit_Transport( Pc=Pc, MR=MR, eps=eps, 
                                                                  frozen=frozen, 
                                                                  frozenAtThroat=frozenAtThroat)
        
        # Cp = Cp * 8314.51 / 4184.0  # convert into BTU/lbm degR
        Cp = self.specific_heat_U.dval_to_uval( Cp )
        visc = self.viscosity_U.dval_to_uval( visc )
        cond = self.thermal_cond_U.dval_to_uval( cond )
        
        return Cp, visc, cond, Prandtl

if __name__ == "__main__":
    
    C = CEA_Obj(oxName='N2O4', fuelName="MMH",
        isp_units='sec', cstar_units='ft/sec',
        pressure_units='psia', temperature_units='degR',
        sonic_velocity_units='ft/sec', enthalpy_units='BTU/lbm',
        density_units='lbm/cuft', specific_heat_units='BTU/lbm degR',
        viscosity_units='millipoise', thermal_cond_units='mcal/cm-K-s')
    
    Cmpa = CEA_Obj(oxName='N2O4', fuelName="MMH",
        isp_units='sec', cstar_units='ft/sec',
        pressure_units='MPa', temperature_units='degR',
        sonic_velocity_units='ft/sec', enthalpy_units='BTU/lbm',
        density_units='lbm/cuft', specific_heat_units='BTU/lbm degR',
        viscosity_units='millipoise', thermal_cond_units='mcal/cm-K-s')
    
    # Csi=CEA_Obj( oxName='N2O4', fuelName="MMH", 
    #     isp_units='N sec/kg', cstar_units='m/sec', 
    #     pressure_units='MPa', temperature_units='C', 
    #     sonic_velocity_units='m/sec', enthalpy_units='cal/g', 
    #     density_units='sg', specific_heat_units='cal/g C',
    #     viscosity_units='poise', thermal_cond_units='cal/s-cm-degC')
    
    def make_a_list( result ):
        if type(result) == type("string"):
            rL = [ result ]
        else:
            try:
                rL = list( result )
            except:
                rL = list( (result,) )
        
        for i,v in enumerate(rL):
            try:
                # rL[i] = '%g'%v 
                rL[i] = "{:.5}".format(v) # 5 significant figures
            except:
                rL[i] = '%s'%v 
        return rL
    
    def chk_method( mname, max_chks=99,  **D ):
        # get English unit results
        r1 = getattr(C, mname)( **D )
        # print( '.......... got r1 for:', mname)
        
        if 'Pc' in D:
            D['Pc'] = D['Pc'] * 0.00689476 # change to MPa
        if 'Pamb' in D:
            D['Pamb'] = D['Pamb'] * 0.00689476 # change to MPa
        # get Mpa results where Pc is equal to English run.
        r2 = getattr(Cmpa, mname)( **D )
        # print( '.......... got r2 for:', mname)
        Ngood = 0
        Nbad = 0
        sL = []
        
        # make lists of output values from english and Mpa runs
        r1L = make_a_list( r1 )
        r2L = make_a_list( r2 )
        
        # check that both give same results
        for i,v in enumerate( r1L ):
            if i>=max_chks:
                break
            sL.append(v)
            if r1L[i] == r2L[i]:
                Ngood += 1
            else:
                Nbad += 1
                
            
        if Nbad > 0:
            print( 'Chk:',mname, sL, 'Ngood=%i'%Ngood, ' NBAD=%i'%Nbad )
            print( '   r1L',r1L )
            print( '   r2L',r2L )
        else:
            print( 'Chk:',mname, 'ALL GOOD:%i -->'%Ngood, sL )
        
    chk_method('get_IvacCstrTc', **{'Pc':100.0})
    chk_method('get_IvacCstrTc', **{'Pc':100.0, 'frozen':1})
    chk_method('getFrozen_IvacCstrTc', **{'Pc':100.0})
    
    chk_method('get_IvacCstrTc_exitMwGam', **{'Pc':100.0})
    chk_method('get_IvacCstrTc_exitMwGam', **{'Pc':100.0, 'frozen':1})
    chk_method('get_IvacCstrTc_ChmMwGam', **{'Pc':100.0})
    chk_method('get_IvacCstrTc_ThtMwGam', **{'Pc':100.0})
    chk_method('get_Isp', **{'Pc':100.0, 'frozen':1})
    chk_method('get_Cstar',**{'Pc':100.0})
    chk_method('get_Tcomb',**{'Pc':100.0})
    chk_method('get_PcOvPe',**{'Pc':100.0})
    chk_method('get_eps_at_PcOvPe',**{'Pc':100.0, 'PcOvPe':554.5})
    chk_method('get_eps_at_PcOvPe',**{'Pc':100.0, 'PcOvPe':554.5, 'frozen':1})
    
    chk_method('get_Throat_PcOvPe',**{'Pc':100.0})
    chk_method('get_MachNumber',**{'Pc':100.0})
    chk_method('get_MachNumber',**{'Pc':100.0, 'frozen':1})
    chk_method('get_Temperatures',**{'Pc':100.0})
    chk_method('get_SonicVelocities',**{'Pc':100.0})
    chk_method('get_SonicVelocities',**{'Pc':100.0, 'frozen':1})
    chk_method('get_Chamber_SonicVel',**{'Pc':100.0})
    chk_method('get_Enthalpies',**{'Pc':100.0})
    chk_method('get_Enthalpies',**{'Pc':100.0, 'frozen':1})
    chk_method('get_Chamber_H',**{'Pc':100.0})
    chk_method('get_Densities',**{'Pc':100.0})
    chk_method('get_Densities',**{'Pc':100.0, 'frozen':1})
    chk_method('get_Chamber_Density',**{'Pc':100.0})
    chk_method('get_HeatCapacities',**{'Pc':100.0})
    chk_method('get_HeatCapacities',**{'Pc':100.0, 'frozen':1})
    chk_method('get_Chamber_Cp',**{'Pc':100.0})
    chk_method('get_Throat_Isp',**{'Pc':100.0})
    chk_method('get_Throat_Isp',**{'Pc':100.0, 'frozen':1})
    chk_method('get_Chamber_MolWt_gamma',**{'Pc':100.0})
    chk_method('get_Throat_MolWt_gamma',**{'Pc':100.0})
    chk_method('get_Throat_MolWt_gamma',**{'Pc':100.0, 'frozen':1})
    chk_method('get_exit_MolWt_gamma',**{'Pc':100.0})
    chk_method('get_eqratio',**{'Pc':100.0})
    chk_method('getMRforER',**{'ERphi':1.0})
    chk_method('get_description')
    chk_method('estimate_Ambient_Isp', max_chks=1,**{'Pc':100.0, 'Pamb':14.7})

    chk_method('get_Chamber_Transport',**{'Pc':100.0, 'MR':1.0, 'eps':40.0,  'frozen':0})
    chk_method('get_Chamber_Transport',**{'Pc':100.0, 'MR':1.0, 'eps':40.0,  'frozen':1})
    
    chk_method('get_Throat_Transport',**{'Pc':100.0, 'MR':1.0, 'eps':40.0, 'frozen':0})
    chk_method('get_Throat_Transport',**{'Pc':100.0, 'MR':1.0, 'eps':40.0,  'frozen':1})
    chk_method('get_Exit_Transport',**{'Pc':100.0, 'MR':1.0, 'eps':40.0,  'frozen':0})
    chk_method('get_Exit_Transport',**{'Pc':100.0, 'MR':1.0, 'eps':40.0,  'frozen':1})
        
    chk_method('get_PambCf', max_chks=2, **{'Pc':100.0, 'Pamb':14.7})
    chk_method('getFrozen_PambCf', max_chks=2, **{'Pc':100.0, 'Pamb':14.7, 'frozenAtThroat':1})

    chk_method('get_SpeciesMassFractions', max_chks=1,**{'Pc':100.0, 'frozen':1, 'frozenAtThroat':0})
    chk_method('get_SpeciesMoleFractions', max_chks=1,**{'Pc':100.0, 'frozen':0, 'frozenAtThroat':0})

    

    print( '======================= Checking get_Chamber_Cp calls ==========================')
    print( 'frozen=0 C.get_Chamber_Cp =', C.get_Chamber_Cp( Pc=100.0, MR=1.0, eps=40.0, frozen=0) )
    print( 'frozen=1 C.get_Chamber_Cp =', C.get_Chamber_Cp( Pc=100.0, MR=1.0, eps=40.0, frozen=1) )
    print( 'NOTE: CEA version < 1.1.34 and Windows 11 with python 3.7 and 3.10 has FORTRAN error re-reading temp.dat with get_???_Transport')
    print( '    : However, Unit tests continue to pass.')
    print( C.get_Chamber_Transport( Pc=100.0, MR=1.0, eps=40.0, frozen=0) )
    print( C.get_Chamber_Transport( Pc=100.0, MR=1.0, eps=40.0, frozen=1) )
    print( C.get_Throat_Transport( Pc=100.0, MR=1.0, eps=40.0, frozen=0) )
    print( C.get_Throat_Transport( Pc=100.0, MR=1.0, eps=40.0, frozen=1) )
    print( C.get_Exit_Transport( Pc=100.0, MR=1.0, eps=40.0, frozen=0) )
    print( C.get_Exit_Transport( Pc=100.0, MR=1.0, eps=40.0, frozen=1) )
    