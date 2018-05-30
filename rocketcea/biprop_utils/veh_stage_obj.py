from math import log, exp, log10
from rocketcea.Goal import Goal

gc = 32.174


def getRhoVeh( volCuIn ):
    """Generic equation for scaling stage mass by volume"""
    if volCuIn <=1.0E-8:
        print( 'ERROR in volCuIn in getRhoVeh; volCuIn=',volCuIn )
        volCuIn = 1.0E-8 # for volume<=0 reset volume tiny so calculated rhoveh is terrible.
        
    expon = (log10(volCuIn) + 1.)/(-3.)
    rhoveh =  10.0**expon
    return rhoveh



class ReferenceStage:
    '''Scale reference stage to new propellant volume using getRhoVeh correlation'''
        
    def calc_rhoVeh(self, vol):
        return self.corrFactor * getRhoVeh( vol )
        
    @property
    def MassFrac(self):
        try:
            mf = self.WtPropellant / (self.WtPropellant + self.WtInert)
        except:
            mf = 0.0
        return mf
               
    def __init__(self, WtPayload=1600000.0, volCuInRef=75500.0*1728, WtInertRef=300000.0, 
                 Name='Saturn V 1st stg'):
        
        self.stage_desc = "Wpayload=%g lbm, Mass Scaled from: %s"%(WtPayload, Name)
        self.Name = Name
        
        rhoVeh = WtInertRef / volCuInRef
        corrRhoStg = getRhoVeh( volCuInRef )
        self.corrFactor = rhoVeh / corrRhoStg
        
        self.WtPayload = WtPayload
        
        self.WtInert = None
        self.WtPropellant = None
        self.VolPropellant = None # cuin
        self.DeltaV = None
        self.GLOW = None # includes payload mass
    
    def setGLOW(self, GLOW=5000.0, sg=1.0, Isp=300.0):
        
        self.GLOW = GLOW
        rho = sg * 0.0361273
        
        def calc_glow_from_vol( V ):
            rhoStg = self.calc_rhoVeh(V)
            return V*(rhoStg  + rho) + self.WtPayload
        
        G = Goal(goalVal=GLOW, minX=0.01, maxX=(GLOW-self.WtPayload)/rho, verbose=False,
            funcOfX=calc_glow_from_vol, tolerance=1.0E-10, maxLoops=80, failValue=None)
        self.VolPropellant, ierror = G()
        
        if ierror:
            self.WtInert = None
            self.WtPropellant = None
            self.VolPropellant = None # cuin
            self.DeltaV = None
            self.GLOW = None # includes payload mass
        else:        
            self.WtPropellant = rho * self.VolPropellant
            self.WtInert = self.GLOW - self.WtPropellant - self.WtPayload
            Wbo =  self.WtPayload + self.WtInert
            self.DeltaV = gc * Isp * log( self.GLOW/Wbo )

        

    def setWtInert(self, WtInert=1000.0, sg=1.0, Isp=300.0):
        
        self.WtInert = WtInert
        rho = sg * 0.0361273
        
        def calc_winert_from_vol( V ):
            rhoStg = self.calc_rhoVeh(V)
            return V * rhoStg
        
        G = Goal(goalVal=WtInert, minX=0.01, maxX=99.99*WtInert/rho,  verbose=False,
            funcOfX=calc_winert_from_vol, tolerance=1.0E-10, maxLoops=80, failValue=None)
        self.VolPropellant, ierror = G()
        if ierror:
            self.WtInert = None
            self.WtPropellant = None
            self.VolPropellant = None # cuin
            self.DeltaV = None
            self.GLOW = None # includes payload mass
        else:        
        
            self.WtPropellant = rho * self.VolPropellant
            Wbo =  self.WtPayload + self.WtInert
            self.GLOW = Wbo + self.WtPropellant
            self.DeltaV = gc * Isp * log( self.GLOW/Wbo )
    
        
    def setWtPropellant(self, WtPropellant=1000.0, sg=1.0, Isp=300.0):
        
        self.WtPropellant = WtPropellant
        rho = sg * 0.0361273
        self.VolPropellant = WtPropellant / rho
        
        rhoVeh = self.calc_rhoVeh( self.VolPropellant )
        self.WtInert = rhoVeh * self.VolPropellant
        
        Wbo =  self.WtPayload + self.WtInert
        self.GLOW = Wbo + self.WtPropellant
        self.DeltaV = gc * Isp * log( self.GLOW/Wbo )


    def setVolPropellant(self, VolPropellant=10000.0, sg=1.0, Isp=300.0):
        
        self.VolPropellant = VolPropellant
        rho = sg * 0.0361273
        self.WtPropellant = rho * VolPropellant
        
        rhoVeh = self.calc_rhoVeh( self.VolPropellant )
        self.WtInert = rhoVeh * self.VolPropellant
        
        Wbo =  self.WtPayload + self.WtInert
        self.GLOW = Wbo + self.WtPropellant
        self.DeltaV = gc * Isp * log( self.GLOW/Wbo )

    def setDeltaV(self, DeltaV=2000.0, sg=1.0, Isp=300.0):
        
        self.DeltaV = DeltaV
        rho = sg * 0.0361273
        
        def calc_dv_from_vol( V ):
            rhoStg = self.calc_rhoVeh(V)
            glow = V*(rhoStg  + rho) + self.WtPayload
            wbo = glow - V*rho
            return gc * Isp * log( glow/wbo )
        
        gam = DeltaV / Isp / gc
        glow = self.WtPayload * 1000.0 * exp(gam)

        G = Goal(goalVal=DeltaV, minX=0.01, maxX=glow/rho,  verbose=False,
            funcOfX=calc_dv_from_vol, tolerance=1.0E-10, maxLoops=80, failValue=None)
        self.VolPropellant, ierror = G()
        if ierror:
            self.WtInert = None
            self.WtPropellant = None
            self.VolPropellant = None # cuin
            self.DeltaV = None
            self.GLOW = None # includes payload mass
        else:        
        
            self.WtPropellant = rho * self.VolPropellant
            
            rhoVeh = self.calc_rhoVeh( self.VolPropellant )
            self.WtInert = rhoVeh * self.VolPropellant
            
            Wbo =  self.WtPayload + self.WtInert
            self.GLOW = Wbo + self.WtPropellant


    def summ_print(self):
        
        try:
            print('Wp=%g, Vp=%g, Winert=%g, dv=%g, GLOW=%g, mu=%g'%\
                (self.WtPropellant, self.VolPropellant, self.WtInert, self.DeltaV, self.GLOW, self.MassFrac))
        except:
            print('Wp=%s, Vp=%s, Winert=%s, dv=%s, GLOW=%s, mu=%s'%\
                (self.WtPropellant, self.VolPropellant, self.WtInert, self.DeltaV, self.GLOW, self.MassFrac))


class ReferenceStagePair( ReferenceStage ):
    '''Scale reference stage pair to new propellant volume using getRhoVeh correlation'''
    def calc_rhoVeh(self, vol):
        
        vrat = (log10(vol)-log10(self.volCuIn2))/ (log10(self.volCuIn1)-log10(self.volCuIn2))
        logRV = log10(self.rhoVeh2) + vrat*(log10(self.rhoVeh1)-log10(self.rhoVeh2))
        rhoVeh = 10.0**logRV
        
        return rhoVeh
       
    def __init__(self,  Name='N2H4 DACS', WtPayload=30.0,
        volCuIn1=463.58, WtInert1=18.1563,
        volCuIn2=192.23, WtInert2=15.28557):
        
        self.stage_desc = "Wpayload=%g lbm, Stage Pair from: %s"%(WtPayload, Name)
        self.WtPayload = WtPayload
            
        self.volCuIn1 = volCuIn1
        self.WtInert1 = WtInert1
        self.volCuIn2 = volCuIn2
        self.WtInert2 = WtInert2
        self.Name = Name
        
        self.rhoVeh1 = WtInert1 / volCuIn1
        self.corrRhoStg1 = getRhoVeh( volCuIn1 )

        self.rhoVeh2 = WtInert2 / volCuIn2
        self.corrRhoStg2 = getRhoVeh( volCuIn2 )

        self.corrFactor1 = self.rhoVeh1 / self.corrRhoStg1
        self.corrFactor2 = self.rhoVeh2 / self.corrRhoStg2

        
        self.WtInert = None
        self.WtPropellant = None
        self.VolPropellant = None # cuin
        self.DeltaV = None
        self.GLOW = None # includes payload mass


class ConstMassFracStage:
    """Assumes all propellants can achieve the same stage propellant mass fraction (A BAD ASSUMPTION)"""

    def __init__(self, mass_frac=0.8, WtPayload=1000.0):
        
        self.stage_desc = "Wpayload=%g lbm, Mass Fraction=%g (A BAD ASSUMPTION)"%(WtPayload, mass_frac)
        
        self.mass_frac = mass_frac
        self.WtPayload = WtPayload
        
        self.WtInert = None
        self.WtPropellant = None
        self.VolPropellant = None # cuin
        self.DeltaV = None
        self.GLOW = None # includes payload mass
        
    @property
    def MassFrac(self):
        try:
            mf = self.WtPropellant / (self.WtPropellant + self.WtInert)
        except:
            mf = 0.0
        return mf
    
    def setGLOW(self, GLOW=5000.0, sg=1.0, Isp=300.0):
        
        self.GLOW = GLOW
        WstgIgn = self.GLOW - self.WtPayload
        
        self.WtPropellant = WstgIgn * self.mass_frac
        self.WtInert = WstgIgn - self.WtPropellant
        rho = sg * 0.0361273
        self.VolPropellant = self.WtPropellant / rho

        Wbo =  self.WtPayload + self.WtInert
        self.DeltaV = gc * Isp * log( self.GLOW/Wbo )

    def setWtInert(self, WtInert=1000.0, sg=1.0, Isp=300.0):
        
        self.WtInert = WtInert
        self.WtPropellant = self.mass_frac * WtInert / (1.0-self.mass_frac)
        
        rho = sg * 0.0361273
        self.VolPropellant = self.WtPropellant / rho

        Wbo =  self.WtPayload + self.WtInert
        self.GLOW = Wbo + self.WtPropellant
        self.DeltaV = gc * Isp * log( self.GLOW/Wbo )
    
        
    def setWtPropellant(self, WtPropellant=1000.0, sg=1.0, Isp=300.0):
        
        self.WtPropellant = WtPropellant
        self.WtInert = WtPropellant * (1.0-self.mass_frac) / self.mass_frac
    
        rho = sg * 0.0361273
        self.VolPropellant = self.WtPropellant / rho

        Wbo =  self.WtPayload + self.WtInert
        self.GLOW = Wbo + self.WtPropellant
        self.DeltaV = gc * Isp * log( self.GLOW/Wbo )


    def setVolPropellant(self, VolPropellant=10000.0, sg=1.0, Isp=300.0):
        
        self.VolPropellant = VolPropellant
        
        rho = sg * 0.0361273
        self.WtPropellant = VolPropellant * rho
        self.WtInert = self.WtPropellant * (1.0-self.mass_frac) / self.mass_frac

        Wbo =  self.WtPayload + self.WtInert
        self.GLOW = Wbo + self.WtPropellant
        self.DeltaV = gc * Isp * log( self.GLOW/Wbo )

    def setDeltaV(self, DeltaV=2000.0, sg=1.0, Isp=300.0):
        
        self.DeltaV = DeltaV
        rho = sg * 0.0361273
        
        gam = DeltaV / Isp / gc
        mu = self.mass_frac
        
        self.GLOW = self.WtPayload * mu / ( exp(-gam) - (1.0-mu) )
        WstgIgn = self.GLOW - self.WtPayload
        
        self.WtPropellant = WstgIgn * mu
        self.WtInert = WstgIgn - self.WtPropellant
        self.VolPropellant = self.WtPropellant / rho

    def summ_print(self):
        
        try:
            print('mf=%g, Wp=%g, Vp=%g, Winert=%g, dv=%g, GLOW=%g, mu=%g'%\
                (self.mass_frac, self.WtPropellant, self.VolPropellant, self.WtInert, self.DeltaV, self.GLOW, self.MassFrac))
        except:
            print('mf=%g, Wp=%s, Vp=%s, Winert=%s, dv=%s, GLOW=%s, mu=%s'%\
                (self.mass_frac, self.WtPropellant, self.VolPropellant, self.WtInert, self.DeltaV, self.GLOW, self.MassFrac))




if __name__=="__main__":
    
    if 1:
        #cmf = ConstMassFracStage()
        cmf = ReferenceStage()
        cmf.summ_print()
        cmf.setWtInert( WtInert=1000.0 )
    else:
        cmf = ReferenceStagePair()
        cmf.summ_print()
        cmf.setWtInert( WtInert=15.28557 )
    
    cmf.summ_print()
    wp = cmf.WtPropellant
    vol = cmf.VolPropellant
    dv = cmf.DeltaV
    glow = cmf.GLOW
        
    cmf.setWtPropellant( WtPropellant=wp )
    cmf.summ_print()
    
    cmf.setVolPropellant( VolPropellant=vol )
    cmf.summ_print()
    
    cmf.setDeltaV( DeltaV=dv)
    cmf.summ_print()
    
    cmf.setGLOW( GLOW=glow)
    cmf.summ_print()
    