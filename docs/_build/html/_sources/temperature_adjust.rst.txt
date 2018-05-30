.. temperature_adjust

.. _`temperature_adjust_link`:

Temperature Adjust
==================

There are any number of good reasons to analyze propellants at other than their standard condition.
RocketCEA provides some aid in creating adjusted input decks.

Standard T and H
------------------

Propellants in RocketCEA are assumed to be at standard temperature and enthalpy.
For storable and gaseous propellants, room temperature is standard.
For cryogenic propellants, standard condition is normal boiling point (NBP).

Calling the propellants "LOX" and "LH2" by name in a CEA_obj statement such as this::

    ispObj = CEA_Obj( oxName='LOX', fuelName='LH2')

will result in LOX and LH2 at standard conditions.

Changing T and H
----------------

As an example, I'll use a LOX/CH4 engine, where, instead of running the engine on propellants
that are each at their normal boiling point (NBP), this engine will store both propellants at a 
common temperature. The LOX will be at NBP and the CH4 will be subcooled. 

In some designs a common propellant temperature is desirable in order to have common dome tankage.
The ox and fuel are separated by an uninsulated dome without the worry of heat transfer between them.
Additionally, common dome tankage makes a shorter stage.

I'll assume that only the CH4 CEA card will need to be adjusted, but the LOX CEA card can be used as is.
In order to calculate the common storage temperature, I'll assume that the bleed valve on the LOX tank is 
set at 5 psig so that the LOX is stored at 20 psia and therefore 168 degR.

Using the fluid properties code `CoolProp <http://www.coolprop.org/dev/index.html>`_ as wrapped by the
engineering units code `EngCoolProp <http://engcoolprop.readthedocs.org/en/latest/>`_, the following script
will calculate the delta T and delta H for CH4, create a new CEA card for subcooled CH4,
run the performance of both all-NBP and common-T engine designs and output the comparison between  all-NBP and common-T.

.. literalinclude:: ./_static/example_scripts/adjust_ch4_t.py

The output from the script shows that the CH4 will change by, dT=-33.2 degR and dH=-27.2 BTU/lbm.

The new name created for the subcooled CH4 incorporates the new H and T.
Both CEA cards are shown in the output for comparison.::

           degR      psia   lbm/cuft   BTU/#    BTU/#  BTU/#R   ---
    CH4 T= 201.0 P=  14.7 D=26.3666 E= -0.10 H=  0.01 S=0.000 Q=0.00
    O2  T= 167.8 P=  20.0 D=70.2858 E=-55.14 H=-55.09 S=0.716 Q=0.00
    CH4 T= 167.8 P=   2.3 D=27.9723 E=-27.20 H=-27.18 S=-0.147 Q=0.00

    CH4 dT=-33.171 degR, dH=-27.1848 BTU/lbm

    New Name = CH4_m21632.1_167.836
     fuel CH4(L) C 1 H 4 wt%=100. 
     h,cal=-21632.1 t(k)=93.24 rho=0.4239 

    Standard CH4
     fuel CH4(L) C 1 H 4     wt%=100.  
     h,cal=-21390.     t(k)=111.66   rho=0.4239  

           Both NBP    Common Temp
    IspVac   396.9      396.7 sec
    Cstar   6002.4     5999.7 ft/sec
    Tcomb   6732.5     6728.7 degR

The final answer is that the performance only drops by about 0.2 seconds, almost certainly better than
the added weight and complexity of interpropellant insulation, or separate-dome tankage.
  
