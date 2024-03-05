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
        # Change bottom of source file to call "dev_tests"
        
         def dev_tests():
            pass

         if __name__ == "__main__":
            dev_tests()
            
        # then test by calling <name>.dev_tests()
        
See:
      https://docs.python.org/2/library/unittest.html
         or
      https://docs.python.org/dev/library/unittest.html
for more assert options
"""

import sys, os


here = os.path.abspath(os.path.dirname(__file__)) # Needed for py.test
up_one = os.path.split( here )[0]  # Needed to find rocketcea development version
if here not in sys.path[:2]:
    sys.path.insert(0, here)
if up_one not in sys.path[:2]:
    sys.path.insert(0, up_one)

from rocketcea.cea_obj import CEA_Obj
from rocketcea.blends import newFuelBlend
from rocketcea.input_cards import oxCards, fuelCards, propCards
from rocketcea.cea_obj import add_new_fuel, add_new_oxidizer, add_new_propellant

class MyTest(unittest.TestCase):


    def test_lox_mmh_existence(self):
        """Check that LOX/MMH gives correct result"""
        C = CEA_Obj(oxName="LOX", fuelName='MMH', fac_CR=3.0)
        
        Pc,MR,eps = 1000.0, 5.88687, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertAlmostEqual(i, 259.1301512870862, places=3)
        self.assertAlmostEqual(c, 4396.52781695842, places=3)
        self.assertAlmostEqual(t, 4656.635824779046, places=3)
        
        del C

    def test_M10(self):
        """Check that M10 gives correct result"""
        C = CEA_Obj(oxName="LOX", fuelName='M10', fac_CR=3.0)
        
        Pc,MR,eps = 1000.0, 1.0, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertAlmostEqual(i, 383.3610263935366, places=3)
        
        del C


    def test_MON13(self):
        """Check that MON13 gives correct result"""
        C = CEA_Obj(oxName="MON13", fuelName='MMH', fac_CR=3)
        
        Pc,MR,eps = 1000.0, 1.0, 100.0
        i,c,t = C.get_IvacCstrTc(Pc,MR,eps)
        
        self.assertAlmostEqual(i, 315.325683085419, places=3)
        
        del C


    def test_getFrozen_IvacCstrTc(self):
        """ test call to getFrozen_IvacCstrTc( Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        i,c,t = C.getFrozen_IvacCstrTc( Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0)
        
        self.assertAlmostEqual(i, 335.7936755290008, places=3)
        
        del C

    def test_get_IvacCstrTc_exitMwGam(self):
        """ test call to get_IvacCstrTc_exitMwGam( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        IspODE, Cstar, Tcomb, mw, gam = C.get_IvacCstrTc_exitMwGam( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(IspODE, 351.7235908873327, places=3)
        self.assertAlmostEqual(gam, 1.24657352829, places=3)
        
        del C

    def test_get_IvacCstrTc_ChmMwGam(self):
        """ test call to get_IvacCstrTc_ChmMwGam( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2)
        
        IspODE, Cstar, Tcomb, mw, gam = C.get_IvacCstrTc_ChmMwGam( Pc=100.0, MR=1.0, eps=40.0)
        #print( IspODE, Cstar, Tcomb, mw, gam )
        
        self.assertAlmostEqual(IspODE, 351.71498221320405, places=3)
        self.assertAlmostEqual(gam, 1.157284929843286, places=3)
        
        del C

    def test_get_IvacCstrTc_ThtMwGam(self):
        """ test call to get_IvacCstrTc_ThtMwGam( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=100)
        
        IspODE, Cstar, Tcomb, mw, gam = C.get_IvacCstrTc_ThtMwGam( Pc=100.0, MR=1.0, eps=40.0)
        #print( IspODE, Cstar, Tcomb, mw, gam )
        
        self.assertAlmostEqual(IspODE, 351.7388076713265, places=3)
        self.assertAlmostEqual(gam, 1.1660556259815762, places=3)
        
        del C

    def test_get_Isp(self):
        """ test call to get_Isp( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        IspODE = C.get_Isp( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(IspODE, 351.7235908873327, places=3)
        
        del C

    def test_get_Cstar(self):
        """ test call to get_Cstar( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        cstar = C.get_Cstar( Pc=100.0, MR=1.0)
        print( cstar )
        
        self.assertAlmostEqual(cstar, 6071.98790105685, places=3)
        
        # get it again just to test cache 
        cstar = C.get_Cstar( Pc=100.0, MR=1.0)
        
        del C

    def test_get_Tcomb(self):
        """ test call to get_Tcomb( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        Tc = C.get_Tcomb( Pc=100.0, MR=1.0)
        
        self.assertAlmostEqual(Tc, 5438.017608723179, places=3)
        
        del C

    def test_get_PcOvPe(self):
        """ test call to get_PcOvPe( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        PcOvPe = C.get_PcOvPe( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(PcOvPe, 568.3567593758571, places=3)
        
        del C

    def test_get_eps_at_PcOvPe(self):
        """ test call to get_eps_at_PcOvPe( Pc=100.0, MR=1.0, PcOvPe=1000.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        eps = C.get_eps_at_PcOvPe( Pc=100.0, MR=1.0, PcOvPe=1000.0)
        
        self.assertAlmostEqual(eps, 61.6764341406682, places=3)
        
        del C

    def test_get_Throat_PcOvPe(self):
        """ test call to get_Throat_PcOvPe( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        PcOvPe = C.get_Throat_PcOvPe( Pc=100.0, MR=1.0)
        
        self.assertAlmostEqual(PcOvPe, 1.809632258591452, places=3)
        
        del C

    def test_get_MachNumber(self):
        """ test call to get_MachNumber( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        M = C.get_MachNumber( Pc=100.0, MR=1.0,eps=40.0)
        
        self.assertAlmostEqual(M, 4.444667300017029, places=3)
        
        del C

    def test_get_SonicVelocities(self):
        """ test call to get_SonicVelocities( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        sonicList = C.get_SonicVelocities( Pc=100.0, MR=1.0,eps=40.0)
        print( sonicList )
        #print( (1.0/3.28083)*sonicList )
        print( [s*(1.0/3.28083) for s in sonicList] )
        
        self.assertEqual( len(sonicList), 3)
        self.assertAlmostEqual(sonicList[0], 4176.70139407, places=3)
        self.assertAlmostEqual(sonicList[2], 2446.69593317, places=3)
        
        del C

    def test_get_Chamber_SonicVel(self):
        """ test call to get_Chamber_SonicVel( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        v = C.get_Chamber_SonicVel( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(v, 4176.70139407, places=3)
        
        del C

    def test_get_Enthalpies(self):
        """ test call to get_Enthalpies( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        hList = C.get_Enthalpies( Pc=100.0, MR=1.0,eps=40.0)
        print( hList )
        
        self.assertEqual( len(hList), 3 )
        self.assertAlmostEqual(hList[0], 143.61895142033558, places=3)
        self.assertAlmostEqual(hList[2], -2198.5652665486314, places=3)
        
        del C

    def test_get_Chamber_H(self):
        """ test call to get_Chamber_H( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        h = C.get_Chamber_H( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(h, 143.61895142033558, places=3)
        
        del C

    def test_get_Densities(self):
        """ test call to get_Densities( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        dList = C.get_Densities( Pc=100.0, MR=1.0,eps=40.0)
        print('  dList', dList)
        #print('rhoList', (1.0 / (62.42796 * 100.0)) * dList)
        print('rhoList', [d*(1.0 / (62.42796 * 100.0)) for d in dList])
        
        self.assertEqual( len(dList), 3 )
        self.assertAlmostEqual(dList[0], 0.028722, places=7)
        self.assertAlmostEqual(dList[2], 0.00016975, places=7)
        
        del C

    def test_get_Chamber_Density(self):
        """ test call to get_Chamber_Density( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        d = C.get_Chamber_Density( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(d, 0.028722, places=7)
        
        del C

    def test_get_HeatCapacities(self):
        """ test call to get_HeatCapacities( Pc=100.0, MR=1.0,eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        cpL = C.get_HeatCapacities( Pc=100.0, MR=1.0,eps=40.0)
        
        self.assertEqual( len(cpL), 3)
        self.assertAlmostEqual(cpL[0], 1.2114385955651787, places=5)
        self.assertAlmostEqual(cpL[2], 0.5451393382768416, places=5)
        
        del C

    def test_get_Chamber_Cp(self):
        """ test call to get_Chamber_Cp( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        cpList = C.get_Chamber_Cp( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(cpList, 1.2114385955651787, places=7)
        
        del C

    def test_get_Throat_Isp(self):
        """ test call to get_Throat_Isp( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        i = C.get_Throat_Isp( Pc=100.0, MR=1.0)
        
        self.assertAlmostEqual(i, 233.38383866700576, places=3)
        
        del C

    def test_get_Chamber_MolWt_gamma(self):
        """ test call to get_Chamber_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        mw,gam = C.get_Chamber_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(mw, 17.937744474163555, places=5)
        self.assertAlmostEqual(gam, 1.1573440099999723, places=5)
        
        del C

    def test_get_Throat_MolWt_gamma(self):
        """ test call to get_Throat_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        mw,gam = C.get_Throat_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(mw, 18.100532022975145, places=5)
        self.assertAlmostEqual(gam, 1.1656503646343368, places=5)
        
        del C

    def test_get_exit_MolWt_gamma(self):
        """ test call to get_exit_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        mw,gam = C.get_exit_MolWt_gamma( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertAlmostEqual(mw, 18.42871163394873, places=7)
        self.assertAlmostEqual(gam, 1.2465880148815687, places=5)
        
        del C

    def test_get_eqratio(self):
        """ test call to get_eqratio( Pc=100.0, MR=1.0, eps=40.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        eq = C.get_eqratio( Pc=100.0, MR=1.0, eps=40.0)
        
        self.assertEqual( len(eq), 2)
        self.assertAlmostEqual(eq[0], 1.7363572550114232, places=7)
        
        del C

    def test_getMRforER(self):
        """ test call to getMRforER( ERphi=None, ERr=None) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        mr = C.getMRforER( ERphi=1.0, ERr=None)
        self.assertAlmostEqual(mr, 1.7363572550114232, places=7)

        mr = C.getMRforER( ERphi=None, ERr=1.0)
        self.assertAlmostEqual(mr, 1.7363572550114232, places=7)

        del C

    def test_get_Pinj_over_Pcomb(self):
        """ test call to get_Pinj_over_Pcomb( Pc=100.0, MR=1.0) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=2.5)
        
        prat = C.get_Pinj_over_Pcomb( Pc=100.0, MR=1.0 )
        self.assertAlmostEqual(prat, 1.0701504515007396, places=7)

        
        prat = C.get_Pinj_over_Pcomb( Pc=100.0, MR=1.0, fac_CR=3.0 )
        self.assertAlmostEqual(prat, 1.0477109324175184, places=7)
        
        prat = C.get_Pinj_over_Pcomb( Pc=100.0, MR=1.0 )
        self.assertAlmostEqual(prat, 1.0701504515007396, places=7)

        del C

    def test_get_description(self):
        """ test call to get_description(self) """
        C = CEA_Obj(oxName="LOX", fuelName="MMH", fac_CR=3.0)
        ans = C.get_description()
        
        self.assertEqual(ans, 'LOX / MMH CR=3')
        
        del C


    def test_ambient_Isp(self):
        C = CEA_Obj(oxName="LOX", fuelName="LH2", fac_CR=2.5)
        
        IspAmb, mode = C.estimate_Ambient_Isp(Pc=100.0, MR=6.0, eps=20.0, Pamb=14.7)
        self.assertAlmostEqual(IspAmb, 235.89898324081676, places=5)
        
        IspAmb, mode = C.estimate_Ambient_Isp(Pc=1000.0, MR=6.0, eps=20.0, Pamb=14.7)
        self.assertAlmostEqual(IspAmb, 366.68084284535496, places=5)
        
        IspAmb, mode = C.estimate_Ambient_Isp(Pc=5000.0, MR=6.0, eps=20.0, Pamb=14.7)
        self.assertAlmostEqual(IspAmb, 423.68494652662264, places=3)
        
        del C
    
    def test_full_cea_output(self):
        C = CEA_Obj(oxName="LOX", fuelName="LH2", fac_CR=2.5)
        s = C.get_full_cea_output( Pc=1000.0, MR=6.0, eps=40.0)
        #print( s )
        
        ipos = s.find('Isp, LB-SEC/LB               39.2    154.4    431.1')
        self.assertGreater( ipos, 4000 )
        
        del C

if __name__ == '__main__':
    # Can test just this file from command prompt
    #  or it can be part of test discovery from nose, unittest, pytest, etc.
    unittest.main()

