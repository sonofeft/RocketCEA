import matplotlib
matplotlib.use('Agg') # a fix for Travis CI

import unittest
# import unittest2 as unittest # for versions of python < 2.7

"""
        Method                            Checks that
self.assertEqual(a, b)                      a == b   
self.assertNotEqual(a, b)                   a != b   
self.assertTrue(x)                          bool(x) is True  
self.assertFalse(x)                         bool(x) is False     
self.assertIs(a, b)                         a is b
self.assertIsNot(a, b)                      a is not b
self.assertIsNone(x)                        x is None 
self.assertIsNotNone(x)                     x is not None 
self.assertIn(a, b)                         a in b
self.assertNotIn(a, b)                      a not in b
self.assertIsInstance(a, b)                 isinstance(a, b)  
self.assertNotIsInstance(a, b)              not isinstance(a, b)  
self.assertAlmostEqual(a, b, places=5)      a within 5 decimal places of b
self.assertNotAlmostEqual(a, b, delta=0.1)  a is not within 0.1 of b
self.assertGreater(a, b)                    a is > b
self.assertGreaterEqual(a, b)               a is >= b
self.assertLess(a, b)                       a is < b
self.assertLessEqual(a, b)                  a is <= b

for expected exceptions, use:

with self.assertRaises(Exception):
    blah...blah...blah

with self.assertRaises(KeyError):
    blah...blah...blah

Test if __name__ == "__main__":
    def test__main__(self):
        # loads and runs the bottom section: if __name__ == "__main__"
        runpy = imp.load_source('__main__', os.path.join(up_one, 'filename.py') )
        
See:
      https://docs.python.org/2/library/unittest.html
         or
      https://docs.python.org/dev/library/unittest.html
for more assert options
"""

import sys, os
import imp

here = os.path.abspath(os.path.dirname(__file__)) # Needed for py.test
up_one = os.path.split( here )[0]  # Needed to find rocketcea development version
if here not in sys.path[:2]:
    sys.path.insert(0, here)
if up_one not in sys.path[:2]:
    sys.path.insert(0, up_one)

from rocketcea.cea_obj import CEA_Obj, getCacheDict
from rocketcea.blends import newFuelBlend
from rocketcea.input_cards import oxCards, fuelCards, propCards
from rocketcea.cea_obj import add_new_fuel, add_new_oxidizer, add_new_propellant

class MyTest(unittest.TestCase):


    def test_should_always_pass_cleanly(self):
        """Should always pass cleanly."""
        pass

    def test_ceaobj_existence(self):
        """Check that ceaobj exists"""
        C = CEA_Obj(oxName="LOX", fuelName='MMH')
        # See if the CEA_Obj object exists
        self.assertTrue(C)
        
        del C

    def test_lox_mmh_existence(self):
        """Check that LOX/MMH gives correct result"""
        C = CEA_Obj(oxName="LOX", fuelName='MMH')
        
        Pc,MR,eps = 1000.0, 5.88687, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertAlmostEqual(i, 259.131110638, places=3)
        self.assertAlmostEqual(c, 4396.62540955, places=3)
        self.assertAlmostEqual(t, 4674.34960735, places=3)
        
        del C

    def test_M10(self):
        """Check that M10 gives correct result"""
        C = CEA_Obj(oxName="LOX", fuelName='M10')
        
        Pc,MR,eps = 1000.0, 1.0, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertAlmostEqual(i, 383.380155446, places=3)
        
        del C

    def test_badM1000(self):
        """Check that M1000 gives Exception"""
        with self.assertRaises(Exception):
            C = CEA_Obj(oxName="LOX", fuelName='M1000')

    def test_Peroxide83(self):
        """Check that Peroxide83 gives correct result"""
        C = CEA_Obj(oxName='Peroxide83', fuelName="CH4")
        
        Pc,MR,eps = 1000.0, 1.0, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertAlmostEqual(i, 226.278531988, places=3)
        
        del C

    def test_BadPeroxide(self):
        """Check that Peroxidexx gives Exception"""
        with self.assertRaises(Exception):
            C = CEA_Obj(oxName='Peroxidexx', fuelName="CH4")
        

    def test_BadPeroxide1000(self):
        """Check that Peroxide1000 gives Exception"""
        with self.assertRaises(Exception):
            C = CEA_Obj(oxName='Peroxide1000', fuelName="CH4")

    def test_HYD30(self):
        """Check that HYD30 gives correct result"""
        C = CEA_Obj(propName='HYD30')
        
        Pc,MR,eps = 200.0, 1.0, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        #s = C.get_full_cea_output( Pc=Pc, MR=MR, eps=eps)
        #print( s )
        
        self.assertAlmostEqual(i, 254.73410271134156, places=3)
        
        del C

    def test_isp_cache(self):
        """Check that Cache is working"""
        cacheD = getCacheDict()
        len_1 = len( cacheD )
        self.assertGreater(len(cacheD), 0)
        
        C = CEA_Obj(oxName="LOX", fuelName='M19') # new propellant so increases cache.
        Pc,MR,eps = 1000.0, 4.0, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertGreater(len(cacheD), len_1) # cache got bigger
        
        del C


    def test_badMON1300(self):
        """Check that MON1300 gives Exception"""
        with self.assertRaises(Exception):
            C = CEA_Obj(oxName="MON1300", fuelName='MMH')

    def test_badMONxyz(self):
        """Check that MONxyz gives Exception"""
        with self.assertRaises(Exception):
            C = CEA_Obj(oxName="MONxyz", fuelName='MMH')

    def test_MON13(self):
        """Check that MON13 gives correct result"""
        C = CEA_Obj(oxName="MON13", fuelName='MMH')
        
        Pc,MR,eps = 1000.0, 1.0, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertAlmostEqual(i, 315.3551574752479, places=3)
        
        del C

    def test_badFLOX888(self):
        """Check that FLOX888 gives Exception"""
        with self.assertRaises(Exception):
            C = CEA_Obj(oxName="FLOX888", fuelName='MMH')

    def test_FLOX88(self):
        """Check that FLOX88 gives correct result"""
        C = CEA_Obj(oxName="FLOX88", fuelName='MMH')
        
        Pc,MR,eps = 1000.0, 1.0, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertAlmostEqual(i, 378.90026661507926, places=3)
        
        del C

    def test_getFrozen_IvacCstrTc(self):
        """ test call to getFrozen_IvacCstrTc( Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        i,c,t = C.getFrozen_IvacCstrTc( Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0)
        
        self.assertAlmostEqual(i, 335.79399488107725, places=3)
        
        del C

    def test_get_IvacCstrTc_exitMwGam(self):
        """ test call to get_IvacCstrTc_exitMwGam( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        IspODE, Cstar, Tcomb, mw, gam = C.get_IvacCstrTc_exitMwGam( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(IspODE, 351.738807671, places=3)
        self.assertAlmostEqual(gam, 1.24657352829, places=3)
        
        del C

    def test_get_IvacCstrTc_ChmMwGam(self):
        """ test call to get_IvacCstrTc_ChmMwGam( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        IspODE, Cstar, Tcomb, mw, gam = C.get_IvacCstrTc_ChmMwGam( Pc=100.0, MR=1.0, eps=40.0)
        #print( IspODE, Cstar, Tcomb, mw, gam )
        
        self.assertAlmostEqual(IspODE, 351.7388076713265, places=3)
        self.assertAlmostEqual(gam, 1.157284929843286, places=3)
        
        del C

    def test_get_IvacCstrTc_ThtMwGam(self):
        """ test call to get_IvacCstrTc_ThtMwGam( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        IspODE, Cstar, Tcomb, mw, gam = C.get_IvacCstrTc_ThtMwGam( Pc=100.0, MR=1.0, eps=40.0)
        #print( IspODE, Cstar, Tcomb, mw, gam )
        
        self.assertAlmostEqual(IspODE, 351.7388076713265, places=3)
        self.assertAlmostEqual(gam, 1.1660556259815762, places=3)
        
        del C

    def test_get_Isp(self):
        """ test call to get_Isp( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        IspODE = C.get_Isp( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(IspODE, 351.7388076713265, places=3)
        
        del C

    def test_get_Cstar(self):
        """ test call to get_Cstar( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        cstar = C.get_Cstar( Pc=100.0, MR=1.0)
        print( cstar )
        
        self.assertAlmostEqual(cstar, 6073.157251190658, places=3)
        
        # get it again just to test cache 
        cstar = C.get_Cstar( Pc=100.0, MR=1.0)
        
        del C

    def test_get_Tcomb(self):
        """ test call to get_Tcomb( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        Tc = C.get_Tcomb( Pc=100.0, MR=1.0)
        
        self.assertAlmostEqual(Tc, 5464.953232850104, places=3)
        
        del C

    def test_get_PcOvPe(self):
        """ test call to get_PcOvPe( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        PcOvPe = C.get_PcOvPe( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(PcOvPe, 550.3111877518063, places=3)
        
        del C

    def test_get_eps_at_PcOvPe(self):
        """ test call to get_eps_at_PcOvPe( Pc=100.0, MR=1.0, PcOvPe=1000.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        eps = C.get_eps_at_PcOvPe( Pc=100.0, MR=1.0, PcOvPe=1000.0)
        
        self.assertAlmostEqual(eps, 63.23132705083242, places=3)
        
        del C

    def test_get_Throat_PcOvPe(self):
        """ test call to get_Throat_PcOvPe( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        PcOvPe = C.get_Throat_PcOvPe( Pc=100.0, MR=1.0)
        
        self.assertAlmostEqual(PcOvPe, 1.7514818804078667, places=3)
        
        del C

    def test_get_MachNumber(self):
        """ test call to get_MachNumber( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        M = C.get_MachNumber( Pc=100.0, MR=1.0,eps=40.0)
        
        self.assertAlmostEqual(M, 4.44559589277226, places=3)
        
        del C

    def test_get_SonicVelocities(self):
        """ test call to get_SonicVelocities( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        sonicList = C.get_SonicVelocities( Pc=100.0, MR=1.0,eps=40.0)
        print( sonicList )
        
        self.assertEqual( len(sonicList), 3)
        self.assertAlmostEqual(sonicList[0], 4187.8341406, places=3)
        self.assertAlmostEqual(sonicList[2], 2446.32946625, places=3)
        
        del C

    def test_get_Chamber_SonicVel(self):
        """ test call to get_Chamber_SonicVel( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        v = C.get_Chamber_SonicVel( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(v, 4187.834140603644, places=3)
        
        del C

    def test_get_Enthalpies(self):
        """ test call to get_Enthalpies( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        hList = C.get_Enthalpies( Pc=100.0, MR=1.0,eps=40.0)
        print( hList )
        
        self.assertEqual( len(hList), 3 )
        self.assertAlmostEqual(hList[0], 164.75127481, places=3)
        self.assertAlmostEqual(hList[2], -2198.84467212, places=3)
        
        del C

    def test_get_Chamber_H(self):
        """ test call to get_Chamber_H( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        h = C.get_Chamber_H( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(h, 164.75127480959486, places=3)
        
        del C

    def test_get_Densities(self):
        """ test call to get_Densities( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        dList = C.get_Densities( Pc=100.0, MR=1.0,eps=40.0)
        
        self.assertEqual( len(dList), 3 )
        self.assertAlmostEqual(dList[0], 0.0305721, places=3)
        self.assertAlmostEqual(dList[2], 0.00017537, places=3)
        
        del C

    def test_get_Chamber_Density(self):
        """ test call to get_Chamber_Density( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        d = C.get_Chamber_Density( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(d, 0.030572097913422087, places=3)
        
        del C

    def test_get_HeatCapacities(self):
        """ test call to get_HeatCapacities( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        cpL = C.get_HeatCapacities( Pc=100.0, MR=1.0,eps=40.0)
        
        self.assertEqual( len(cpL), 3)
        self.assertAlmostEqual(cpL[0], 1.21681146, places=3)
        self.assertAlmostEqual(cpL[2], 0.54516549, places=3)
        
        del C

    def test_get_Chamber_Cp(self):
        """ test call to get_Chamber_Cp( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        cpList = C.get_Chamber_Cp( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(cpList, 1.2168114560892265, places=3)
        
        del C

    def test_get_Throat_Isp(self):
        """ test call to get_Throat_Isp( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        i = C.get_Throat_Isp( Pc=100.0, MR=1.0)
        
        self.assertAlmostEqual(i, 233.43895092017615, places=3)
        
        del C

    def test_get_Chamber_MolWt_gamma(self):
        """ test call to get_Chamber_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        mw,gam = C.get_Chamber_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(mw, 17.92996373798247, places=3)
        self.assertAlmostEqual(gam, 1.157284929843286, places=3)
        
        del C

    def test_get_Throat_MolWt_gamma(self):
        """ test call to get_Throat_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        mw,gam = C.get_Throat_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(mw, 18.103782908970977, places=3)
        self.assertAlmostEqual(gam, 1.1660556259815762, places=3)
        
        del C

    def test_get_exit_MolWt_gamma(self):
        """ test call to get_exit_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        mw,gam = C.get_exit_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(mw, 18.428712322579944, places=3)
        self.assertAlmostEqual(gam, 1.2465735282902142, places=3)
        
        del C

    def test_get_eqratio(self):
        """ test call to get_eqratio( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        eq = C.get_eqratio( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertEqual( len(eq), 2)
        self.assertAlmostEqual(eq[0], 1.7363572550114232, places=3)
        
        del C

    def test_getMRforER(self):
        """ test call to getMRforER( ERphi=None, ERr=None) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        mr = C.getMRforER( ERphi=1.0, ERr=None)
        self.assertAlmostEqual(mr, 1.7363572550114232, places=3)

        mr = C.getMRforER( ERphi=None, ERr=1.0)
        self.assertAlmostEqual(mr, 1.7363572550114232, places=3)

        del C

    def test_get_description(self):
        """ test call to get_description(self) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH")
        ans = C.get_description()
        
        self.assertEqual(ans, 'LOX / MMH')
        
        del C


    def test_ambient_Isp(self):
        C = CEA_Obj(oxName="LOX", fuelName="LH2")
        
        IspAmb, mode = C.estimate_Ambient_Isp(Pc=100.0, MR=6.0, eps=20.0, Pamb=14.7)
        self.assertAlmostEqual(IspAmb, 235.9980548, places=3)
        
        IspAmb, mode = C.estimate_Ambient_Isp(Pc=1000.0, MR=6.0, eps=20.0, Pamb=14.7)
        self.assertAlmostEqual(IspAmb, 366.709649, places=3)
        
        IspAmb, mode = C.estimate_Ambient_Isp(Pc=5000.0, MR=6.0, eps=20.0, Pamb=14.7)
        self.assertAlmostEqual(IspAmb, 423.712640620, places=3)
        
        del C
    
    def test_full_cea_output(self):
        C = CEA_Obj(oxName="LOX", fuelName="LH2")
        s = C.get_full_cea_output( Pc=1000.0, MR=6.0, eps=40.0)
        #print( s )
        
        ipos = s.find('Isp, LB-SEC/LB              154.4    431.2')
        self.assertGreater( ipos, 4000 )
        
        del C
    
    def test_new_propellants(self):
        card_str = """
        oxid N2O4(L)   N 2 O 4   wt%=96.5
        h,cal=-4676.0     t(k)=298.15
        oxid SIO2  SI 1 O 2    wt%=3,5
        h,cal=-216000.0     t(k)=298.15  rho.g/cc=1.48
        """
        add_new_oxidizer( 'GelN2O4', card_str )
        
        # ==========
        card_str = """
        fuel CH6N2(L)  C 1     H 6     N 2     wt%=60.00
        h,cal=12900.0     t(k)=298.15   rho=.874
        fuel   AL 1   wt%=40.00
        h,cal=0.0     t(k)=298.15   rho=.1
        """
        add_new_fuel( 'MMH_AL', card_str )
        
        C = CEA_Obj(oxName="GelN2O4", fuelName="MMH_AL")
        IspODE = C.get_Isp( Pc=1850.0, MR=0.7, eps=40.0)
        self.assertAlmostEqual(IspODE, 380.83236183365057, places=3)

        
        # ==========
        card_str = """
        name H2O2(L) H 2 O 2  wt%=100.00
        h,cal=-44880.0     t(k)=298.15  rho.g/cc=1.407
        """
        add_new_propellant( 'MyProp', card_str )
        C = CEA_Obj(propName="MyProp")
        IspODE = C.get_Isp( Pc=1850.0, eps=40.0)
        self.assertAlmostEqual(IspODE, 189.9709005711723, places=3)

    def test__main__(self):
        old_sys_argv = list(sys.argv)
        sys.argv = list(sys.argv)
        sys.argv.append('suppress_show')
        
        try:
            runpy = imp.load_source('__main__', os.path.join(up_one, 'cea_obj.py') )
        except:
            raise Exception('ERROR... failed in __main__ routine')
        finally:
            sys.argv = old_sys_argv
    
    def test_get_ambient_Cf(self):
        """ test call to get_eqratio( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="LH2")
        CFcea, CF,mode = C.get_PambCf( Pc=1000.0, MR=6.0, eps=40.0, Pamb=14.7*0.14823)
        
        self.assertAlmostEqual(CF, 1.8351, places=3)
        self.assertAlmostEqual(CFcea, 1.8351, places=3)
        
        # check frozen at throat
        CFcea, CFfrozen, mode = C.getFrozen_PambCf(Pc=1000.0, MR=6.0, eps=40.0, Pamb=14.7*0.12612,
                                                   frozenAtThroat=1)
        self.assertAlmostEqual(CFcea, 1.7843, places=3)
        self.assertAlmostEqual(CFfrozen, 1.7843, places=3)
        
        # check frozen at chamber
        CFcea, CFfrozen, mode = C.getFrozen_PambCf(Pc=1000.0, MR=6.0, eps=40.0, Pamb=14.7*0.12376)
        self.assertAlmostEqual(CFcea, 1.7919, places=3)
        self.assertAlmostEqual(CFfrozen, 1.7919, places=3)
        
        del C

if __name__ == '__main__':
    # Can test just this file from command prompt
    #  or it can be part of test discovery from nose, unittest, pytest, etc.
    unittest.main()

