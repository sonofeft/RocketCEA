"""A numpy.distutils based setup module for RocketCEA

See:
http://rocketcea.readthedocs.org/en/latest/
https://github.com/sonofeft/RocketCEA

If installing from source, then
the best way to install RocketCEA is to use pip after navigating to the source directory::

    cd <path to where setup.py is located>
    pip install -e .

This will execute the setup.py file and insure that its pip-specific commands are run.

"""


# Use numpy to build the f2py fortran extension
# --------------------------------    
from numpy.distutils.core import Extension, setup


ext_py_cea = Extension(name = 'rocketcea.py_cea',
                       sources = ['rocketcea/py_cea.f'])

setup(ext_modules = [ext_py_cea],)

print('\n\nFor a quick test of RocketCEA execute the command:\n\n' +\
      '''python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"''')
