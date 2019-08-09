.. 2019-03-26 sonofeft 4d178660acefdffe2cdbe4829d6f2d0d917428cc
   Maintain spacing of "History" and "GitHub Log" titles

History
=======

GitHub Log
----------

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
        