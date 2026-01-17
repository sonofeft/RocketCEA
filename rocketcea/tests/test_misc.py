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
import tempfile

here = os.path.abspath(os.path.dirname(__file__)) # Needed for py.test
up_one = os.path.split( here )[0]  # Needed to find rocketcea development version
if here not in sys.path[:2]:
    sys.path.insert(0, here)
if up_one not in sys.path[:2]:
    sys.path.insert(0, up_one)

import rocketcea.Isp
import rocketcea.Goal
import rocketcea.separated_Cf
from rocketcea.biprop_utils.mr_t_limits import MR_Temperature_Limits
import rocketcea.biprop_utils.mr_t_limits
from rocketcea.biprop_utils import run_cea_w_error_corr
from rocketcea.cea_obj import CEA_Obj #, get_rocketcea_data_dir, set_rocketcea_data_dir
from rocketcea import cea_obj_w_units

class MyTest(unittest.TestCase):


    def test_should_always_pass_cleanly(self):
        """Should always pass cleanly."""
        pass

    def test_ambientCf_at_vac(self):
    
        Cf, CfOverCfvac, mode = rocketcea.separated_Cf.ambientCf(gam=1.25, epsTot=20.0, Pc=200.0, Pamb=0.0)
        
        self.assertEqual(mode, 'UnderExpanded Pe=0.858599')
    
    def test_bad_goal_func(self):
        
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=0.0, maxX=57.123, 
            funcOfX=None, tolerance=1.0E-3, maxLoops=40, failValue=None)
        
        with self.assertRaises(Exception):
            X, ierror = G()
    
    def test_goal_func_err(self):
        
        def FofX(x):
            return x / 0.0
        
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=0.0, maxX=57.123, 
            funcOfX=FofX, tolerance=1.0E-3, maxLoops=40, failValue=None)
        
        X, ierror = G()
        self.assertEqual(ierror, 1)
        
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=0.0, maxX=57.123, 
            funcOfX=FofX, tolerance=1.0E-3, maxLoops=40, failValue=0.0)
        
        X, ierror = G()
        self.assertEqual(ierror, 1)
    
    def test_goal_func_early_terminate(self):
        
        def FofX(x):
            return x**3 - x**2 + x - 11.875
        
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=0.0, maxX=57.123, 
            funcOfX=FofX, tolerance=1.0E-3, maxLoops=2, failValue=None)
        
        X, ierror = G()
        self.assertEqual(ierror, 2)

        # -----------
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=0.0, maxX=57.123, 
            funcOfX=FofX, tolerance=100.0, maxLoops=40, failValue=None)
        
        X, ierror = G()
        self.assertEqual(ierror, 0)
        
        # -----------
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=2.5, maxX=2.511111, 
            funcOfX=FofX, tolerance=1.0E-3, maxLoops=40, failValue=None)
        X, ierror = G()
        self.assertAlmostEqual(X, 2.5, places=3)

        
        # -----------
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=2.4, maxX=2.5, 
            funcOfX=FofX, tolerance=1.0E-3, maxLoops=40, failValue=None)
        X, ierror = G()
        self.assertAlmostEqual(X, 2.5, places=3)
        
        # -----------
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=2.4, maxX=2.6, 
            funcOfX=FofX, tolerance=1.0E-3, maxLoops=40, failValue=None)
        X, ierror = G()
        self.assertAlmostEqual(X, 2.5, places=3)
        
    
    def test_goal_func_bad_range(self):
        
        def FofX(x):
            return x**3 - x**2 + x - 11.875
        
        G = rocketcea.Goal.Goal(goalVal=0.0, minX=2.6, maxX=2.9, 
            funcOfX=FofX, tolerance=1.0E-3, maxLoops=2, failValue=None)
        
        X, ierror = G()
        self.assertEqual(ierror, 1)



    def test_Goal__main__(self):
        old_sys_argv = list(sys.argv)
        sys.argv = list(sys.argv)
        sys.argv.append('suppress_show')
        
        try:
            rocketcea.Goal.dev_tests()
        except:
            raise Exception('ERROR... failed in __main__ routine')
        finally:
            sys.argv = old_sys_argv

    
    def test_Isp__main__(self):
        old_sys_argv = list(sys.argv)
        sys.argv = list(sys.argv)
        sys.argv.append('suppress_show')
        
        try:
            rocketcea.Isp.dev_tests()
        except:
            raise Exception('ERROR... failed in __main__ routine')
        finally:
            sys.argv = old_sys_argv

    
    def test_separated_Cf__main__(self):
        old_sys_argv = list(sys.argv)
        sys.argv = list(sys.argv)
        #sys.argv.append('suppress_show')
        
        try:
            rocketcea.separated_Cf.dev_tests()
        except:
            raise Exception('ERROR... failed in __main__ routine')
        finally:
            sys.argv = old_sys_argv
    
    def test_mr_temperature_limits_1(self):
        mc = MR_Temperature_Limits( oxName='N2O4', fuelName='NH3', oxPcentL=None, fuelPcentL=None,
            TC_LIMIT=1000.0, PcNominal=1000.0, epsNominal=10.0,
            MR_MIN=0.0, MR_MAX=1000.0)
        
        self.assertAlmostEqual(mc.max_MR, 35.3561, places=3)
    
    def test_mr_temperature_limits_1b(self):
        mc = MR_Temperature_Limits( oxName='N2O4', fuelName='NH3', oxPcentL=None, fuelPcentL=None,
            TC_LIMIT=1000.0, PcNominal=1000.0, epsNominal=10.0,
            MR_MIN=0.0, MR_MAX=10.0)
        
        self.assertAlmostEqual(mc.max_MR, 10, places=3)
        
        s = str(mc)
        self.assertEqual(s,'<N2O4/NH3, Stoich_MR=2.02602, Min MR=0.332277, Max MR=10, Tc Left=1000 R, Tc Right=2506.12 R>')
        
    def test_mr_temperature_limits_2(self):
        mc = MR_Temperature_Limits( oxName='MON12', fuelName='M10', oxPcentL=None, fuelPcentL=None,
            TC_LIMIT=1000.0, PcNominal=1000.0, epsNominal=10.0,
            MR_MIN=0.0, MR_MAX=1000.0)
        self.assertAlmostEqual(mc.max_MR, 85.131, places=3)

    def test_mr_temperature_limits_3(self):
        mc = MR_Temperature_Limits( oxName=['F2','O2'], fuelName=["N2H4","NH3"], oxPcentL=[65,35], fuelPcentL=[90,10],
            TC_LIMIT=1000.0, PcNominal=1000.0, epsNominal=10.0,
            MR_MIN=0.0, MR_MAX=1000.0)
        self.assertAlmostEqual(mc.max_MR, 79.5403, places=3)

    
    def test_mr_t_limits__main__(self):
        old_sys_argv = list(sys.argv)
        sys.argv = list(sys.argv)
        #sys.argv.append('suppress_show')
        
        try:
            rocketcea.biprop_utils.mr_t_limits.dev_tests()
        except:
            raise Exception('ERROR... failed in __main__ routine')
        finally:
            sys.argv = old_sys_argv
        
    
    def test_run_cea_w_error_corr__main__(self):
        old_sys_argv = list(sys.argv)
        sys.argv = list(sys.argv)
        #sys.argv.append('suppress_show')
        
        try:
            run_cea_w_error_corr.dev_tests()
        except:
            raise Exception('ERROR... failed in __main__ routine')
        finally:
            sys.argv = old_sys_argv

    
    def test_cea_w_units__main__(self):
        old_sys_argv = list(sys.argv)
        sys.argv = list(sys.argv)
        sys.argv.append('suppress_show')
        
        try:
            cea_obj_w_units.dev_tests()
        except:
            raise Exception('ERROR... failed in __main__ routine')
        finally:
            sys.argv = old_sys_argv

    # def test_win32_rocketcea_data_dir_spaces(self):
    #     """On Windows, use short path names when spaces in ROCKETCEA_DATA_DIR"""
    #     if sys.platform == 'win32':
    #         save_ROCKETCEA_DATA_DIR = get_rocketcea_data_dir()

    #         try:
    #             with tempfile.TemporaryDirectory() as tmpdirname:
    #                 print('created temporary directory', tmpdirname)
                    
    #                 subdir = os.path.join( tmpdirname, 'cea w spaces' )
    #                 os.mkdir( subdir )
    #                 print( 'subdir =', subdir )
    #                 print( 'os.path.exists(subdir) =', os.path.exists(subdir) )
    #                 print()

    #                 set_rocketcea_data_dir( subdir )

    #                 C = CEA_Obj( oxName='LOX', fuelName='LH2', make_debug_prints=True)
    #                 Isp = C.get_Isp(Pc=100.0, MR=1.0, eps=40.0)
    #                 print( Isp )
    #         finally:
    #             # restore 
    #             set_rocketcea_data_dir( save_ROCKETCEA_DATA_DIR )


if __name__ == '__main__':
    # Can test just this file from command prompt
    #  or it can be part of test discovery from nose, unittest, pytest, etc.
    unittest.main()

