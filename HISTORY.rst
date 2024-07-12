.. 2019-03-26 sonofeft 4d178660acefdffe2cdbe4829d6f2d0d917428cc
   Maintain spacing of "History" and "GitHub Log" titles

History
=======

GitHub Log
----------

* July 12, 2024
    - (by: Charlie)
        - version 1.2.1 fixed MW when condensibles are present.

* Mar 23, 2024
    - (by: Charlie)
        - version 1.2.0 with meson build 
        - updated docs

* Mar 16, 2024
    - (by: Charlie)
        - Try to solve ReadTheDocs problem
        - some fixes and doc updates for numpy.distutils deprecation

* Mar 5, 2024
    - (by: Charlie)
        - removed "import imp" from tests for "__main__" testing
        - remove "future" from requirements

* Feb 25, 2024
    - (by: Charlie)
        - rebuilt docs

* Feb 24, 2024
    - (by: Charlie)
        - Rebase to edited v1.1.34 to v1.1.35

* Feb 16, 2024
    - (by: Charlie)
        - Removed __future__ as a requirement

* Dec 29, 2023
    - (by: Charlie)
        - Merge branch 'master' of https://github.com/sonofeft/RocketCEA
        - added version tags

* Nov 14, 2023
    - (by: Eligh)
        - Fixed Pinj_over_Pcomb by adding unit converasion
        - implimented get_Pinj_over_Pcomb in cea_obj_w_units

* Jul 4, 2023
    - (by: Charlie)
        - doc rebuild
        - fixed transport property FORTRAN error (makeOutput flag)

* Jun 28, 2023
    - (by: Charlie)
        - fix add_dll_directory
        - docs updata
        - rebuild docs
        - static build problem on Ubuntu
        - lament readthedocs for now
        - try to fix readthedocs
        - try to fix readthedocs automodule
        - rebuilt docs
        - version update
        - some build cleanup
        - set language flag to english in conf.py
        - added frozen flags to some functions

* May 3, 2023
    - (by: Charlie and Alex)
        - Added missing arguments to member functions of cea_obj_w_units
        - Fixed the conversion from default imperial units to user units in cea_obj_w_units get_Exit_Transport()

* Jun 2, 2022
    - (by: Charlie)
        - version 1.1.29, force data re-read when calculating transport properties 

* Apr 12, 2022
    - (by: Charlie)
        - updated doc files, added some examples
        - added function doc strings
        - Added logic and test function to correct for spaces in path name on Windows
        - set version number to 1.1.28 

* Apr 11, 2022
    - (by: Charlie)
        - Added space detection for user home dir and using 8.3, short format for Windows 

* Apr 8, 2022
    - (by: Charlie)
        - update docs for v1.1.27
        - empty cache when redefining propellant
        - doc update 

* Jan 11, 2022
    - (by: Charlie)
        - ran unit tests
        - Added Anaconda on Windows install directions 

* Oct 5, 2021
    - (by: Charlie)
        - Made changes to show support for python 3.10 

* Sep 16, 2021
    - (by: Charlie)
        - Added python 3.8 test results
        - doc spelling correction
        - version 1.1.25 and Windows install doc updates 

* Jan 1, 2021
    - (by: sonofeft)
        - Update .travis.yml
        - solve travisci numpy, scipy problem
        - update to docs 

* Dec 1, 2020
    - (by: sonofeft)
        - Merge branch 'master' of https://github.com/sonofeft/RocketCEA
        - updated sphinx pages after NASA links changed
        - NASA links changed 

* Oct 28, 2020
    - (by: sonofeft)
        - moved work files to user HOME directory (fixes Colab)
        - version 1.1.24

* Oct 13, 2020
    - (by: sonofeft)
        - doc updates for python 3.8 and 3.9
        - updated docs for python 3.8 and 3.9 using pipwin

* Oct 8, 2020
    - (by: sonofeft)
        - version 1.1.19, added frozen flags to many methods
        - removed deprecated useFastLookup logic
        - added tests for new frozen flags
    
* July 30, 2020
    - (by: sonofeft)
        - version 1.1.18, trying some python 3.8 DLL import fixes

* Apr 26, 2020
    - (by: sonofeft)
        - version 1.1.17, changed some prints to debug prints

* Apr 21, 2020
    - (by: sonofeft)
        - fixed xlChart, added mach and entropy methods
        
* Apr 5, 2020
    - (by: Franky333)
        - Added Pascal (Pa) to units
    - (by: sonofeft)
        - Cleaned up docs for Pa units

* Apr 3, 2020
    - (by: sonofeft)
        - Added CEA vs TDK vs RPA documentation
        - small update to unit tests

* Apr 1, 2020
    - (by: sonofeft)
        - Fixed properties problem with Cp, H, density and sonic vel
        - updated temperature adjust docs

* Mar 31, 2020
    - (by: sonofeft)
        - documentation updates

* Mar 22, 2020
    - (by: sonofeft)
        - version 1.1.12 changed mole/mass fractions to injector face, chamber, throat and exit 
        - version 1.1.11 added finite area combustor system-wide

* Mar 19, 2020
    - (by: sonofeft)
        - version 1.1.10 Added finite area combustor to full cea output
        - version 1.1.9 fixes combustion species error, added xxx_NASA propellants
        
* Mar 16, 2020
    - (by: sonofeft)
        - Bug fix for species fractions and small fix to CEA FORTRAN output file

* Mar 10, 2020
    - (by: sonofeft)
        - put units labels in function calls
        - version 1.1.7 with some minor changes
        
* Mar 8, 2020
    - (by: sonofeft)
        - updated unit testing

* Mar 6, 2020
    - (by: sonofeft)
        - update history
        - Added combustion species for version 1.1.6
        - examine species mass/mole fraction output
        
* Mar 4, 2020
    - (by: sonofeft)
        - version 1.1.4 adds thrust coefficient

* Feb 25, 2020
    - (by: sonofeft)
        - some doc updates

* Feb 21, 2020
    - (by: sonofeft)
        - Added error tolerance to multi-input of get_full_cea_output

* Feb 20, 2020
    - (by: sonofeft)
        - Added multiple column, traditional CEA output and documentation
        - updates to docs

* Feb 11, 2020
    - (by: sonofeft)
        - updated git history
        - Finished up (hopefully) transport units and unit testing
        - A few doc tweaks for the transport property additions.
        - Started Adding Transport Property Methods
        
* Feb 7, 2020
    - (by: sonofeft)
        - uploaded version 1.1.0
        - Added Frozen option to Nozzle Exit Temperature 

* Jan 21, 2020
    - (by: sonofeft)
        - added Pc units to full_output method 


* Jan 20, 2020
    - (by: sonofeft) 
        - Version 1.08
        - Added docs for SI units
        - created SI example "plot_SI_biprop.py"
        - Some folder cleanup and starting  SI Units Addition

* Aug 9, 2019
    - (by: sonofeft) 
        - Version 1.06
        - Incorporated suggestion of `mahoep <https://github.com/mahoep>`_ for transport properties
        - Added get_Temperatures function to return (Tc, Tthroat, Texit)

* July 31, 2019
    - (by: sonofeft) 
        - update quickstart docs

* July 29, 2019
    - (by: sonofeft) 
        - Major Overhaul of f2py usage

* July 6, 2019
    - (by: sonofeft and stepbot)
        - Begin to support MacOS (Darwin)

* Mar 26, 2019
    - (by: sonofeft) 
        - Added Windows install fix info to docs
* Oct 22, 2018
    - (by: sonofeft) 
        - added help for bad MinGW path error
* Oct 21, 2018
    - (by: sonofeft) 
        - full instructions to recompile with gfortran
* Oct 20, 2018
    - (by: sonofeft) 
        - Identified MinGW library issue in docs
        - Updated quickstart to show warning about recent python requiring user to run as administrator
* Aug 12, 2018
    - (by: sonofeft) 
        - modify .gitattributes to make project show as python
        - added .gitattributes
* May 30, 2018
    - (by: sonofeft) 
        - Reinstated Travis CI
        - matplotlib fix for Travis CI
            added: matplotlib.use('Agg')
        - try apt packages on travis ci
        - try conda on travis ci
        - try some pip fixes to travis ci
        - try apt-get in travis build
        - Removed Travis-CI
            Need to solve missing libgfortran.so.3 import error on Travis CI
        - Developed code first commit
        - Revert "Initial commit"
            This reverts commit e06031f0a5c0244d944fc3b1f4a3ed987579a2a7.
    - (by: Charlie Taylor) 
        - Initial commit

* May 29, 2018
    - (by: Charlie Taylor)
        - Verified operation on multiple platforms.

* May 13, 2018
    - (by: Charlie Taylor)
        - First Created RocketCEA with PyHatch        

* 2005 - 2015
    - (by: Charlie Taylor)
        - Added enhancements to CEA interface.

* 2005
    - (by: Charlie Taylor)
        - Modified **CEA2.f** into **py_cea.f** so that 
          `f2py <https://docs.scipy.org/doc/numpy/f2py/python-usage.html>`_ could build a python module
        