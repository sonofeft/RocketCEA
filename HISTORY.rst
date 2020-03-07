.. 2019-03-26 sonofeft 4d178660acefdffe2cdbe4829d6f2d0d917428cc
   Maintain spacing of "History" and "GitHub Log" titles

History
=======

GitHub Log
----------
* Mar 6, 2020
    - (by: sonofeft)
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
        