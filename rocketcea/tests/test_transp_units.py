
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

from rocketcea.cea_obj_w_units import CEA_Obj as CEA_Obj_w_units
from rocketcea.cea_obj import CEA_Obj

class MyTest(unittest.TestCase):


    def test_should_always_pass_cleanly(self):
        """Should always pass cleanly."""
        pass


    def test_use_Mpa_as_input_Pc(self):
        """test use Mpa as input Pc"""
        
        C = CEA_Obj( oxName='LOX', fuelName='LH2')
        CwU = CEA_Obj_w_units( oxName='LOX', fuelName='LH2', pressure_units='MPa')
        
        a = C.get_Isp(Pc=1000.0, MR=6.0, eps=40.0)
        b = CwU.get_Isp(Pc=1000.0/145.037738, MR=6.0, eps=40.0)
        self.assertAlmostEqual(a, b, delta=0.01)

    def test_chamber_transport_props(self):
        """test chamber transport props"""
        
        C = CEA_Obj( oxName='LOX', fuelName='LH2')
        CwU = CEA_Obj_w_units( oxName='LOX', fuelName='LH2', pressure_units='MPa',
              specific_heat_units='kJ/kg-K',  # note: cal/g K == BTU/lbm degR
              viscosity_units='poise', thermal_cond_units='BTU/s-in-degF')
        
        Cp1, v1, con1, P1 = C.get_Chamber_Transport(Pc=1000.0, MR=6.0, eps=40.0, frozen=0)
        Cp2, v2, con2, P2 = CwU.get_Chamber_Transport(Pc=1000.0/145.037738, MR=6.0, eps=40.0, frozen=0)
        
        Cp3 = C.get_Chamber_Cp(Pc=1000.0, MR=6.0, eps=40.0, frozen=0)
        
        # look at heat capacity
        self.assertAlmostEqual(Cp1, 2.0951, delta=0.001)
        self.assertAlmostEqual(Cp2, 8.771346, delta=0.001)
        self.assertAlmostEqual(Cp1, Cp3, delta=0.001)
        
        # look at viscosity
        self.assertAlmostEqual(v1, 1.0588, delta=0.001)
        self.assertAlmostEqual(v2, 0.0010588, delta=0.000001)
        
        # look at conductivity
        self.assertAlmostEqual(con1, 4.1444, delta=0.0001)
        self.assertAlmostEqual(con2, 2.3192E-5, delta=1.0E-9)
        

if __name__ == '__main__':
    # Can test just this file from command prompt
    #  or it can be part of test discovery from nose, unittest, pytest, etc.
    unittest.main()

