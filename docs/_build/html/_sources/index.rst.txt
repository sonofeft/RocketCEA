
.. RocketCEA documentation master file
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. include:: ../README.rst


See NASA CEA Documentation:
`CEA User Manual (NASA RP-1311).pdf <./_static/CEA_User_Manual_(NASA_RP-1311).pdf>`_
and
`CEA Methodology (NASA RP-1311).pdf <./_static/CEA_Methodology_(NASA_RP-1311).pdf>`_


.. note::

   For those interested in using **RocketCEA** to design rocket thrusters,
   there are two companion projects
   `RocketIsp <https://rocketisp.readthedocs.io/en/latest/>`_ and 
   `RocketProps <https://rocketprops.readthedocs.io/en/latest/>`_.
   
   `RocketIsp <https://rocketisp.readthedocs.io/en/latest/>`_ uses a simplified JANNAF 
   approach to calculate delivered specific impulse (Isp) for liquid rocket thrust chambers.
   
   `RocketProps <https://rocketprops.readthedocs.io/en/latest/>`_
   calculates the various liquid propellant properties required to analyse 
   a liquid propellant thrust chamber.


.. note::

   On Windows, 32 bit python 3.8 and above are not supported.

   As of this writing (10/5/2021) 64 bit python 3.10 requires Unofficial Binaries
   to install numpy, scipy and matplotlib. 

RocketCEA
=========

Contents:

.. toctree::
   :maxdepth: 2

   quickstart
   installgfortran
   std_examples
   traditional_example
   simple_examples
   finite_area_comb
   comb_species
   engine_mr
   propellant_select
   propellants
   new_propellants
   plot_examples
   ambient_isp
   hydrazine_mono
   temperature_adjust
   cea_tdk_rpa
   parasol_example
   functions
   copyright
   authors
   history


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`


