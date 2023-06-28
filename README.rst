
.. image:: https://img.shields.io/pypi/v/RocketCEA.svg
        
.. image:: https://img.shields.io/badge/python-2.7|3.5|3.6|3.7|3.8|3.9|3.10-blue

.. image:: https://img.shields.io/pypi/l/RocketCEA.svg

RocketCEA Wraps The NASA FORTRAN CEA Code And Provides Some Useful Tools
========================================================================

See the Code at: `<https://github.com/sonofeft/RocketCEA>`_

See the Docs at: `<http://rocketcea.readthedocs.org/en/latest/>`_

See PyPI page at:`<https://pypi.python.org/pypi/rocketcea>`_

See NASA CEA HomePage at: `<https://www1.grc.nasa.gov/research-and-engineering/ceaweb/>`_

See NASA CEA On-Line at: `<https://cearun.grc.nasa.gov/>`_

RocketCEA makes direct calls to the NASA FORTRAN CEA code in "rocket" mode to calculate
Isp, Cstar, Tcham etc. and provides tools to help determine useful
mixture ratio range, optimum MR and more.

RocketCEA does not use the FORTRAN **CEA2.f** file directly.

Many modifications have been made in order to wrap CEA with 
`f2py <https://docs.scipy.org/doc/numpy/f2py/python-usage.html>`_ to build a python module.

Additional changes to **CEA2.f** have been made in order to properly handle hydrazine monopropellant's
ammonia dissociation.

Although the default units in RocketCEA are English units, SI units may be used for both
input and output as well. (See **Simple Examples > Transport Properties** in Docs)
