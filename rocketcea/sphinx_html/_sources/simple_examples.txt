.. simple_examples

Simple Examples
===============

RocketCEA always begins with an import statement and an instance of a CEA_obj::

    from rocketcea.cea_obj import CEA_Obj
    C = CEA_Obj( oxName='LOX', fuelName='LH2')

If the above is done at a command prompt, we can query the CEA_Obj as shown below::

    >>> from rocketcea.cea_obj import CEA_Obj
    >>> C = CEA_Obj( oxName='LOX', fuelName='LH2')
    >>> C.get_Isp(Pc=100.0, MR=1.0, eps=40.0)
    374.3036176557629
    >>> C.get_Isp(Pc=100.0, MR=6.0, eps=40.0)
    448.190232998362

Note that the number of significant figures in the Isp above are much higher than in the standard CEA output.

While there is likely no physical significance to this, it can sometimes be useful numerically in 
computations that take derivatives of Isp with respect to a design variable. (for example optimizers.)


N2O4/MMH Performance
--------------------

Successive queries of the CEA_Obj can be made to create tables of information.
The script below will make a table of N2O4/MMH performance data.


.. literalinclude:: ./_static/example_scripts/perf_table.py

The resulting table is shown below::

 Pc(psia) AreaRatio  MixtureRatio   IspVac(sec)  Cstar(ft/sec) Tc(degR)  MolWt    gamma
   250.0     50.0        1.0          306.1        5378.5      4243.4    16.73    1.2539 
   250.0     50.0        1.1          311.6        5476.1      4532.8    17.39    1.2377 
   250.0     50.0        1.2          316.7        5554.9      4791.1    18.03    1.2216 
   250.0     50.0        1.3          321.4        5617.1      5018.0    18.63    1.2062 
   250.0     50.0        1.4          325.6        5664.5      5214.0    19.21    1.1918 
   250.0     50.0        1.5          329.3        5698.4      5379.7    19.75    1.1786 
   250.0     50.0        1.6          332.4        5719.6      5515.7    20.26    1.1668 
   250.0     50.0        1.7          335.1        5728.7      5623.4    20.74    1.1569 
   250.0     50.0        1.8          337.4        5726.4      5704.9    21.19    1.1489 
   250.0     50.0        1.9          339.3        5713.9      5763.2    21.60    1.1428 
   250.0     50.0        2.0          340.8        5692.9      5801.9    21.99    1.1384 
   250.0     50.0        2.1          341.9        5665.4      5824.6    22.34    1.1353 
   250.0     50.0        2.2          342.7        5633.5      5834.8    22.67    1.1333 
   250.0     50.0        2.3          343.0        5598.6      5835.2    22.98    1.1319 
   250.0     50.0        2.4          342.8        5562.1      5828.0    23.27    1.1311 
   250.0     50.0        2.5          341.6        5524.8      5814.9    23.54    1.1307 
   250.0     50.0        2.6          338.7        5487.2      5797.4    23.79    1.1305 
   250.0     50.0        2.7          335.5        5449.7      5776.3    24.03    1.1306 
   250.0     50.0        2.8          332.4        5412.5      5752.5    24.25    1.1308 
   250.0     50.0        2.9          329.3        5375.8      5726.4    24.47    1.1311 


LOX/LH2 Delta V
---------------

Conducting an analysis with RocketCEA is much easier than the standard approach to running CEA and 
reviewing the pages of CEA output (as we did in the LOX/LH2 example from :ref:`Standard Examples <std_examples_link>`)

We can query the CEA_Obj instance repeatedly for specific information, as opposed to simply printing a page of CEA output.

If we wanted to run some deltaV calculations on a LOX/LH2 stage to see what impact changing the engine's area ratio
would have, we might do the following.

.. literalinclude:: ./_static/example_scripts/deltav_calc.py

The script above calls RocketCEA for a number of area ratio values to get ideal vacuum Isp.
An efficiency is applied to that ideal Isp to arrive at a delivered Isp.
The delivered Isp is then used to calculate a stage deltaV.

The script gives the following output::

     Pc(psia) AreaRatio  MixtureRatio   IspVac(sec)  IspDel(sec)   deltaV(ft/sec)
       475.0     84.0        5.88       464.9        450.5         21392.5     
       475.0    100.0        5.88       467.7        453.2         21518.6     
       475.0    150.0        5.88       473.5        458.8         21785.7     
       475.0    200.0        5.88       477.1        462.3         21954.9     
       475.0    250.0        5.88       479.8        464.9         22075.5     
       475.0    280.0        5.88       481.0        466.1         22133.4     
   
   