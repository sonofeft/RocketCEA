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
import rocketcea.blends as blends
from rocketcea.input_cards import oxCards, fuelCards, propCards

class BlendsTest(unittest.TestCase):

    def test_should_always_pass_cleanly(self):
        """Should always pass cleanly."""
        pass


    def test_newOxBlend(self):
        """ test call to     def test_newOxBlend(self): """
        ans = blends.newOxBlend( ['F2','O2'], oxPcentL=[75.0, 25.0])
        
        self.assertEqual(ans, 'F2_75_O2_25')

        with self.assertRaises(Exception):
            ans = blends.newOxBlend( ['MON15','O2'], oxPcentL=[75.0, 25.0])


    def test_newFuelBlend(self):
        """ test call to     def test_newFuelBlend(self): """
        mmhPcent = 11.1
        ans = blends.newFuelBlend( fuelL=["MMH","N2H4"], fuelPcentL=[mmhPcent,100.0-mmhPcent])
        
        self.assertEqual(ans, 'MMH_11.1_N2H4_88.9')

    def test_newPropWithNewState(self):
        """ test call to     def test_newPropWithNewState(self): """
        propName = "ARC448"
        CPREF=0.7351
        WTMOL=32.048
        
        dTdegR = 20.0
        newTrefDegR = 298.15 * 1.8 + dTdegR
        deltaH = dTdegR * CPREF
        delCALperMOLE = deltaH * WTMOL / 1.8
        
        newHfCalPerMole = blends.getPropHfCalPerMole( propName ) + delCALperMOLE
        
        ans = blends.newPropWithNewState( propCards, propName, newHfCalPerMole, newTrefDegR)
        
        self.assertEqual(ans, 'ARC448_m36605.3_556.67')

    def test_newFuelWithNewState(self):
        """ test call to     def test_newFuelWithNewState(self): """
        
        fuelName = 'NH3'
        CPREF=1.06307 # (BTU/DEG R/LBM)
        WTMOL=17.0303
        dTdegR = 20.0
        newTrefDegR = 298.15 * 1.8 + dTdegR
        deltaH = dTdegR * CPREF
        delCALperMOLE = deltaH * WTMOL / 1.8
        
        newHfCalPerMole = blends.getFuelHfCalPerMole( fuelName ) + delCALperMOLE
        
        ans = blends.newFuelWithNewState( fuelName, newHfCalPerMole, newTrefDegR)
        
        self.assertEqual(ans, 'NH3_m16888.8_556.67')

    def test_newOxWithNewState(self):
        """ test call to     def test_newOxWithNewState(self): """
        
        oxName="IRFNA"
        CPREF=0.41
        WTMOL=59.7
        
        dTdegR = 20.0
        newTrefDegR = 298.15 * 1.8 + dTdegR
        deltaH = dTdegR * CPREF
        delCALperMOLE = deltaH * WTMOL / 1.8
        
        newHfCalPerMole = blends.getOxHfCalPerMole( oxName ) + delCALperMOLE
        
        ans = blends.newOxWithNewState( oxName, newHfCalPerMole, newTrefDegR)
        
        self.assertEqual(ans, 'IRFNA_m64588_556.67')

    def test_turnCardsIntoTokenL(self):
        """ test call to     def test_turnCardsIntoTokenL(self): """
        cardL = fuelCards[ 'A50' ]
        ans = blends.turnCardsIntoTokenL( cardL )
        
        self.assertEqual( len(ans), 26)

    def test_getFloatTokenFromCards(self):
        """ test call to     def test_getFloatTokenFromCards(self): """
        cardL = oxCards[ 'N2O4' ]
        ans = blends.getFloatTokenFromCards( cardL, token='t(k)' )
        
        self.assertEqual(ans, 298.15)

    def test_getFuelRefTempDegK(self):
        """ test call to     def test_getFuelRefTempDegK(self): """
        ans = blends.getFuelRefTempDegK( 'M20' )
        print( ans )
        
        ans = blends.getFuelRefTempDegK( 'N2H4' )
        
        self.assertEqual(ans, 298.15)

    def test_getOxRefTempDegK(self):
        """ test call to     def test_getOxRefTempDegK(self): """
        ans = blends.getOxRefTempDegK( 'MON15' )
        self.assertEqual(ans, 298.15)
        
        ans = blends.getOxRefTempDegK( 'CLF5' )
        self.assertEqual(ans, 298.15)
        

    def test_getFuelHfCalPerMole(self):
        """ test call to     def test_getFuelHfCalPerMole(self): """
        ans = blends.getFuelHfCalPerMole( 'MMH' )
        self.assertEqual(ans, 12900.0)
        
        ans = blends.getFuelHfCalPerMole( 'M20' )
        self.assertEqual(ans, 12900.0) # returns MMH value in mixture
        

    def test_getOxHfCalPerMole(self):
        """ test call to     def test_getOxHfCalPerMole(self): """
        ans = blends.getOxHfCalPerMole( 'CLF3' )
        self.assertEqual(ans, -45680.0)
        
        ans = blends.getOxHfCalPerMole( 'Peroxide90' )
        self.assertEqual(ans, -44880.0) # value of Peroxide, NOT blend of Peroxide90
        

    def test_makeCardForNewTemperature(self):
        """ test call to     def test_makeCardForNewTemperature(self): """
        ans = blends.makeCardForNewTemperature( ceaName='CH4', newTdegR=536.0 )
        print( ans )
        
        self.assertEqual(ans, 'CH4_m19028.6_536')

    def test_renamePropIfNewHfOrTrefInName(self):
        """ test call to     def test_renamePropIfNewHfOrTrefInName(self): """
        ans = blends.renamePropIfNewHfOrTrefInName( propCards, 'N2H4' )
        self.assertEqual(ans, 'N2H4')
        
        ans = blends.renamePropIfNewHfOrTrefInName( fuelCards, "LH2 h,cal=-2155.0  t(k)=21.0" )
        self.assertEqual(ans, 'LH2_m2155_21')
        
        with self.assertRaises(Exception):
            ans = blends.renamePropIfNewHfOrTrefInName( fuelCards, "LH2 h,cal=-2155.0 " )
        
        with self.assertRaises(Exception):
            ans = blends.renamePropIfNewHfOrTrefInName( fuelCards, "LH2 h,cal=-x.0  t(k)=21.0" )
    
    def test__main__(self):
        runpy = imp.load_source('__main__', os.path.join(up_one, 'blends.py') )

if __name__ == '__main__':
    # Can test just this file from command prompt
    #  or it can be part of test discovery from nose, unittest, pytest, etc.
    unittest.main()
