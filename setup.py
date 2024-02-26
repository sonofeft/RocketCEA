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
import setuptools
from numpy.distutils.core import Extension, setup


# Always prefer setuptools over distutils
from setuptools import  find_packages

# To use a consistent encoding
from codecs import open
from os import path

here = path.abspath(path.dirname(__file__))

# Get the long description from the relevant file
with open(path.join(here, 'README.rst'), encoding='utf-8') as f:
    long_description = f.read()
    
    # twine currently fails long description with "Unexpected section title or transition"
    # (seems to be related to line endings)
    long_description = long_description.replace('\n','\r')

# Place install_requires into the text file "requirements.txt"
with open(path.join(here, 'requirements.txt'), encoding='utf-8') as f2:
    requires = f2.read().strip().splitlines()

target_file = path.join( here, 'rocketcea','_version.py')
exec( open( target_file ).read() )  # creates local __version__ variable


#ext_py_cea = Extension(name = 'rocketcea.py_cea',
#                      sources = ['rocketcea/py_cea.f'],
#                      extra_link_args=["-static"])

ext_py_cea = Extension(name = 'rocketcea.py_cea',
                       sources = ['rocketcea/py_cea.f'])


setup(
    name='rocketcea',
    version = __version__,  # METADATA_RESET:    version = '<<version>>',
    ext_modules = [ext_py_cea],

    description = 'RocketCEA wraps the FORTRAN CEA code and provides some useful tools.',
    long_description = long_description,
    long_description_content_type='text/x-rst',

    # The project's main homepage.
    url='http://rocketcea.readthedocs.org/en/latest/',
    download_url='https://github.com/sonofeft/RocketCEA',

    # Author details
    author = 'Charlie Taylor',
    author_email = 'cet@appliedpython.com',

    # license
    license = 'GPL-3',

    classifiers = [
        # Common status values are: "3 - Alpha", "4 - Beta", "5 - Production/Stable"
        #        less common        1 - Planning, 2 - Pre-Alpha, 6 - Mature, 7 - Inactive
        'Development Status :: 4 - Beta',

        "Operating System :: OS Independent",
        'Intended Audience :: Developers',
        "Intended Audience :: End Users/Desktop",
        'Topic :: Software Development :: Build Tools',

        # This license should match "license" above
        'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',

        # Specify the Python versions you support here. In particular, ensure
        # that you indicate whether you support Python 2, Python 3 or both.
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
    ],

    platforms = 'any',

    # What does your project relate to?
    keywords = 'rocketcea setuptools development',

    packages = find_packages(exclude=['.tox', '.hg', 'docs']),
    package_data = {'rocketcea':['examples/*.*', 'f.inp', 'f.out', 'thermo.*', 'temp.dat',
                    'trans.*', 'py_cea.f', 'py_cea.inc']},
    include_package_data=True,

    # List run-time dependencies here.  These will be installed by pip when
    # your project is installed.
    install_requires = requires,  # read from requirements.txt

    tests_require = ['nose','coverage'], # ...OR... ['pytest','pytest-cov']
    test_suite='rocketcea.tests', # allows "setup.py test" to work

    # List additional groups of dependencies here (e.g. development
    # dependencies). You can install these using the following syntax,
    # for example:
    # $ pip install -e .[dev,test]
    extras_require = {
        'dev': ['check-manifest'],
        'test': ['coverage'],
    },

    zip_safe= False,

    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # pip to create the appropriate form of executable for the target platform.

    entry_points = {
        'console_scripts': [
            'rocketcea=rocketcea.show_html_help:main',
        ],
    },
)

print('\n\nFor a quick test of RocketCEA execute the command:\n\n' +\
      '''python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"''')
