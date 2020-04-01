.. hydrazine_mono

.. _`hydrazine_mono_link`:

Hydrazine Monopropellant
========================

Chemical Reactions
------------------

When decomposing Hydrazine (N2H4) as a monopropellant, there are two successive reactions to consider::

    3 N2H4 --> 4 NH3 +   N2 (highly exothermic)
    4 NH3  --> 2 N2  + 6 H2 (endothermic)

The first reaction is highly exothermic and goes to completion.
It creates ammonia (NH3) and nitrogen gas (N2).

The second reaction (ammonia dissociation), however, can be controlled by the design of a catalyst bed. 
The more ammonia dissociation there is, the cooler the resulting combustion products and the lower the 
Isp of the monopropellant engine.

CEA Modification
----------------

In RocketCEA the CEA FORTRAN code has been modified to include "Undissociated Ammonia (UA)" as an exhaust product.
When decomposing hydrazine with only partial ammonia dissociation,
the CEA input includes an "omit NH3" statement to prevent normal equilibrium NH3 calculations.

Partial ammonia dissociation is implemented in RocketCEA with the **propName** "HYDnn", where "nn" is the mass percent
of ammonia that dissociates.  For example, 30, 40 and 50 percent dissociation would be "HYD30", "HYD40" and "HYD50".

This can be demonstrated with the following RocketCEA script that shows the difference between **HYD40** (40% ammonia dissociation)
and **N2H4** (equilibrium CEA logic)

.. literalinclude:: ./_static/example_scripts/show_X_mono.py

::

           Isp     Cstar     Tc
          (sec)   (ft/sec)  (degR)
    40%    238.7   4364.4   2404.1
    100%   222.0   3995.1   1581.8

Note that high levels of ammonia dissociation can be beneficial in some applications like gas generators where lower
combustion temperature is desirable. Typical ranges of ammonia dissociation for hydrazine monopropellant applications
are::

    %Dissociation          Application
      30% - 50%          High Performance Thrusters
      50% - 70%          ACS Thrusters
      60% - 80%          Gas Generators

Performance Plot
----------------

A plot of Monopropellant Hydrazine Performance can be created with the following script.


.. literalinclude:: ./_static/example_scripts/amm_dissociation.py

    
.. image:: ./_static/example_scripts/amm_dissociation.png
    :width: 80%
    
`Click Image to View Fill Size`