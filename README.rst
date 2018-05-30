

.. image:: https://travis-ci.org/sonofeft/RocketCEA.svg?branch=master
    :target: https://travis-ci.org/sonofeft/RocketCEA

.. image:: https://img.shields.io/pypi/v/RocketCEA.svg
    :target: https://pypi.python.org/pypi/rocketcea
        
.. image:: https://img.shields.io/pypi/pyversions/RocketCEA.svg
    :target: https://wiki.python.org/moin/Python2orPython3

.. image:: https://img.shields.io/pypi/l/RocketCEA.svg
    :target: https://pypi.python.org/pypi/rocketcea


RocketCEA Wraps The NASA Fortran CEA Code And Provides Some Useful Tools.
=========================================================================


See the Code at: `<https://github.com/sonofeft/RocketCEA>`_

See the Docs at: `<http://rocketcea.readthedocs.org/en/latest/>`_

See PyPI page at:`<https://pypi.python.org/pypi/rocketcea>`_

See NASA CEA HomePage at: `<https://www.grc.nasa.gov/WWW/CEAWeb/ceaHome.htm>`_


RocketCEA makes direct calls to the FORTRAN CEA code in "rocket" mode to calculate
Isp, Cstar, Tcham etc. and provides tools to help determine useful
mixture ratio range, optimum MR and more.

RocketCEA does not use the FORTRAN **CEA2.f** file directly.

Many modifications have been made in order to wrap CEA with 
`f2py <https://docs.scipy.org/doc/numpy/f2py/python-usage.html>`_ to build a python module.

Additional changes to **CEA2.f** have been made in order to properly handle hydrazine monopropellant's
ammonia dissociation.
