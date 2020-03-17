.. comb_species

Species Mole Fractions
======================

RocketCEA version 1.1.6 and greater provide access to the combustion species 
mass fractions and mole fractions.

.. warning::

   Due to bug-fix please use >= version 1.1.8 for Species Fractions.


Notice at the bottom of the sample run at 
:ref:`Standard Examples LOX/LH2 Performance <std_examples_mole_frac_link>`
that the combustion species mole fractions are displayed.

CEA prints those values as::


     MOLE FRACTIONS

     *H               0.03417  0.02810  0.00001
     HO2              0.00003  0.00002  0.00000
     *H2              0.24832  0.24538  0.24401
     H2O              0.66590  0.68751  0.75598
     H2O2             0.00001  0.00001  0.00000
     *O               0.00334  0.00217  0.00000
     *OH              0.04478  0.03446  0.00000
     *O2              0.00345  0.00236  0.00000

RocketCEA provides the function `get_SpeciesMoleFractions` that returns two dictionaries.
One with with species molecular weights and one with the combustion species mole fractions.
The user can specify equilibrium or frozen values.

A Display of the contents of those two dictionaries is shown below::

       ROCKETCEA MOLE FRACTIONS (frozen=0, frozenAtThroat=0)
     *H               0.03417  0.02810  0.00001   MW=1.00794
     HO2              0.00003  0.00002  0.00000   MW=33.0067
     *H2              0.24832  0.24538  0.24401   MW=2.01588
     H2O              0.66590  0.68751  0.75598   MW=18.0153
     H2O2             0.00001  0.00001  0.00000   MW=34.0147
     *O               0.00334  0.00217  0.00000   MW=15.9994
     *OH              0.04478  0.03446  0.00000   MW=17.0073
     *O2              0.00345  0.00236  0.00000   MW=31.9988
    =======================================================
       ROCKETCEA MOLE FRACTIONS (frozen=1, frozenAtThroat=0)
     *H               0.03417  0.03417  0.03417   MW=1.00794
     HO2              0.00003  0.00003  0.00003   MW=33.0067
     *H2              0.24832  0.24832  0.24832   MW=2.01588
     H2O              0.66590  0.66590  0.66590   MW=18.0153
     H2O2             0.00001  0.00001  0.00001   MW=34.0147
     *O               0.00334  0.00334  0.00334   MW=15.9994
     *OH              0.04478  0.04478  0.04478   MW=17.0073
     *O2              0.00345  0.00345  0.00345   MW=31.9988
    =======================================================
       ROCKETCEA MOLE FRACTIONS (frozen=1, frozenAtThroat=1)
     *H               0.03417  0.02810  0.02810   MW=1.00794
     HO2              0.00003  0.00002  0.00002   MW=33.0067
     *H2              0.24832  0.24538  0.24538   MW=2.01588
     H2O              0.66590  0.68751  0.68751   MW=18.0153
     H2O2             0.00001  0.00001  0.00001   MW=34.0147
     *O               0.00334  0.00217  0.00217   MW=15.9994
     *OH              0.04478  0.03446  0.03446   MW=17.0073
     *O2              0.00345  0.00236  0.00236   MW=31.9988
    =======================================================

The script that produces the above output is:


.. literalinclude:: ./_static/example_scripts/chk_froz_mole_frac.py


Species Mass Fractions
----------------------

If mass fractions are preferred, that can be obtained by calling the method
`get_SpeciesMassFractions`. 

Modifying the above mole fraction script to 
return mass fractions is shown below.

.. literalinclude:: ./_static/example_scripts/chk_froz_mass_frac.py

This script produces the following::

       ROCKETCEA MASS FRACTIONS (frozen=0, frozenAtThroat=0)
     *H               0.00256  0.00208  0.00000   MW=1.00794
     HO2              0.00008  0.00004  0.00000   MW=33.0067
     *H2              0.03720  0.03636  0.03486   MW=2.01588
     H2O              0.89139  0.91033  0.96514   MW=18.0153
     H2O2             0.00003  0.00001  0.00000   MW=34.0147
     *O               0.00397  0.00255  0.00000   MW=15.9994
     *OH              0.05658  0.04308  0.00000   MW=17.0073
     *O2              0.00820  0.00555  0.00000   MW=31.9988
    =======================================================
       ROCKETCEA MASS FRACTIONS (frozen=1, frozenAtThroat=0)
     *H               0.00256  0.00256  0.00256   MW=1.00794
     HO2              0.00008  0.00008  0.00008   MW=33.0067
     *H2              0.03720  0.03720  0.03720   MW=2.01588
     H2O              0.89139  0.89139  0.89139   MW=18.0153
     H2O2             0.00003  0.00003  0.00003   MW=34.0147
     *O               0.00397  0.00397  0.00397   MW=15.9994
     *OH              0.05658  0.05658  0.05658   MW=17.0073
     *O2              0.00820  0.00820  0.00820   MW=31.9988
    =======================================================
       ROCKETCEA MASS FRACTIONS (frozen=1, frozenAtThroat=1)
     *H               0.00256  0.00208  0.00208   MW=1.00794
     HO2              0.00008  0.00004  0.00004   MW=33.0067
     *H2              0.03720  0.03636  0.03636   MW=2.01588
     H2O              0.89139  0.91033  0.91033   MW=18.0153
     H2O2             0.00003  0.00001  0.00001   MW=34.0147
     *O               0.00397  0.00255  0.00255   MW=15.9994
     *OH              0.05658  0.04308  0.04308   MW=17.0073
     *O2              0.00820  0.00555  0.00555   MW=31.9988
    =======================================================


