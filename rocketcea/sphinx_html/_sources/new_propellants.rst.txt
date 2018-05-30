.. new_propellants


.. _`new_propellants_link`:


New Propellants
===============

New propellants can be added and existing propellants modified.

Create a python string variable that contains the new card.  It can be multiline such as::

    card_str = """
    oxid N2O4(L)   N 2 O 4   wt%=96.5
    h,cal=-4676.0     t(k)=298.15
    oxid SiO2  Si 1.0 O 2.0    wt%=3.5
    h,cal=-216000.0     t(k)=298.15  rho=1.48
    """

Add the new propellant card to RocketCEA and give it a name.
It can be added as oxidizer, fuel or propellant.::

    add_new_oxidizer( 'GelN2O4', card_str )
    add_new_fuel( 'MMH_AL', card_str )
    add_new_propellant( 'MyProp', card_str )

New Ox and Fuel
---------------

The example below defines some gel propellants, adding SiO2 to the oxidizer and Al to the fuel.

.. literalinclude:: ./_static/example_scripts/new_propellants.py

Outputs::

    *******************************************************************************

             NASA-GLENN CHEMICAL EQUILIBRIUM PROGRAM CEA, OCTOBER 18, 2002
                       BY  BONNIE MCBRIDE AND SANFORD GORDON
          REFS: NASA RP-1311, PART I, 1994 AND NASA RP-1311, PART II, 1996

     *******************************************************************************



     reac
      fuel CH6N2(L)  C 1.0   H 6.0   N 2.0     wt%=60.00
      h,cal=12900.0     t(k)=298.15   rho=.874
      fuel   AL AL 1.0   wt%=40.00
      h,cal=0.0     t(k)=298.15   rho=0.1
      oxid N2O4(L)   N 2 O 4   wt%=96.5
      h,cal=-4676.0     t(k)=298.15
      oxid SiO2  Si 1.0 O 2.0    wt%=3.5
      h,cal=-216000.0     t(k)=298.15  rho=1.48
      
     prob case=GelN2O4_/_MMH_AL
      rocket equilibrium  p,psia=1850.000000, supar=40.000000,
      o/f=0.700000
      
     outp   calories short
      
     end





                  THEORETICAL ROCKET PERFORMANCE ASSUMING EQUILIBRIUM

               COMPOSITION DURING EXPANSION FROM INFINITE AREA COMBUSTOR

     Pinj =  1850.0 PSIA
     CASE = GelN2O4_/_MMH_A

                 REACTANT                    WT FRACTION      ENERGY      TEMP
                                              (SEE NOTE)      CAL/MOL       K  
     FUEL        CH6N2(L)                     0.6000000     12900.000    298.150
     FUEL        AL                           0.4000000         0.000    298.150
     OXIDANT     N2O4(L)                      0.9650000     -4676.000    298.150
     OXIDANT     SiO2                         0.0350000   -216000.000    298.150

     REACTANT DENSITY= 42.2857 G/CC

     O/F=    0.70000  %FUEL= 58.823529  R,EQ.RATIO= 2.921279  PHI,EQ.RATIO= 2.974634

                     CHAMBER   THROAT     EXIT
     Pinf/P            1.0000   1.7251   363.79
     P, ATM            125.88   72.973  0.34604
     T, K             3082.43  2905.55  1698.41
     RHO, G/CC       1.1177-2 6.9501-3 6.0631-5
     H, CAL/G          26.820  -116.52 -1150.32
     U, CAL/G         -245.95  -370.79 -1288.54
     G, CAL/G        -7686.58 -7387.30 -5400.38
     S, CAL/(G)(K)     2.5024   2.5024   2.5024

     M, (1/n)          22.457   22.708   24.419
     MW, MOL WT        21.109   21.154   21.314
     (dLV/dLP)t      -1.01858 -1.08537 -1.07583
     (dLV/dLT)p        1.2354   2.3383   2.8119
     Cp, CAL/(G)(K)    0.8362   2.4115   4.0182
     GAMMAs            1.1668   1.1275   1.0921
     SON VEL,M/SEC     1153.9   1095.2    794.7
     MACH NUMBER        0.000    1.000    3.949

     PERFORMANCE PARAMETERS

     Ae/At                      1.0000   40.000
     CSTAR, FT/SEC              5497.8   5497.8
     CF                         0.6536   1.8729
     Ivac,LB-SEC/LB              210.7    338.8
     Isp, LB-SEC/LB              111.7    320.0


     MOLE FRACTIONS

     *AL              0.00600  0.00518  0.00000
     ALH              0.00437  0.00353  0.00000
     ALH2             0.00006  0.00003  0.00000
     ALH3             0.00002  0.00001  0.00000
     ALN              0.00001  0.00001  0.00000
     *ALO             0.00007  0.00003  0.00000
     ALOH             0.00979  0.00662  0.00000
     AL(OH)2          0.00001  0.00000  0.00000
     AL2              0.00005  0.00003  0.00000
     AL2O             0.02172  0.01895  0.00000
     AL2O2            0.00006  0.00003  0.00000
     CH3              0.00004  0.00004  0.00000
     CH4              0.00011  0.00012  0.00011
     *CN              0.00001  0.00001  0.00000
     *CO              0.15623  0.15626  0.14630
     *CO2             0.00007  0.00004  0.00000
     C2H2,acetylene   0.00009  0.00012  0.00004
     *H               0.01187  0.00904  0.00016
     HCN              0.00423  0.00459  0.00142
     HCO              0.00002  0.00001  0.00000
     HNC              0.00079  0.00074  0.00004
     *H2              0.46728  0.47240  0.48876
     H2O              0.00153  0.00083  0.00001
     NH2              0.00002  0.00001  0.00000
     NH3              0.00014  0.00009  0.00000
     *N2              0.25026  0.24773  0.23571
     *OH              0.00001  0.00001  0.00000
     *Si              0.00003  0.00003  0.00000
     SiH              0.00004  0.00004  0.00000
     SiH2             0.00001  0.00001  0.00000
     SiO              0.00497  0.00499  0.00028
     ALN(cr)          0.00000  0.00000  0.03294
     ALN(L)           0.00000  0.00589  0.00000
     AL2O3(a)         0.00000  0.00000  0.07646
     AL2O3(L)         0.06004  0.06257  0.00000
     C(gr)            0.00000  0.00000  0.01534
     Si2N2O(cr)       0.00000  0.00000  0.00242

      * THERMODYNAMIC PROPERTIES FITTED TO 20000.K

     NOTE. WEIGHT FRACTION OF FUEL IN TOTAL FUELS AND OF OXIDANT IN TOTAL OXIDANTS


New Monoprop
------------

.. literalinclude:: ./_static/example_scripts/new_propellants_2.py


Outputs::

    *******************************************************************************

             NASA-GLENN CHEMICAL EQUILIBRIUM PROGRAM CEA, OCTOBER 18, 2002
                       BY  BONNIE MCBRIDE AND SANFORD GORDON
          REFS: NASA RP-1311, PART I, 1994 AND NASA RP-1311, PART II, 1996

     *******************************************************************************



     reac
      name H2O2(L) H 2 O 2  wt%=100.00
      h,cal=-44880.0     t(k)=298.15  rho.g/cc=1.407
      
     prob case= MyProp
      rocket equilibrium  p,psia=250.000000, supar=40.000000,
      
      
     outp   calories short
      
     end





                  THEORETICAL ROCKET PERFORMANCE ASSUMING EQUILIBRIUM

               COMPOSITION DURING EXPANSION FROM INFINITE AREA COMBUSTOR

     Pinj =   250.0 PSIA
     CASE = MyProp         

                 REACTANT                    WT FRACTION      ENERGY      TEMP
                                              (SEE NOTE)      CAL/MOL       K  
     NAME        H2O2(L)                      1.0000000    -44880.000    298.150

     REACTANT DENSITY=  1.4070 G/CC

     O/F=    0.00000  %FUEL=  0.000000  R,EQ.RATIO= 0.500000  PHI,EQ.RATIO= 0.000000

                     CHAMBER   THROAT     EXIT
     Pinf/P            1.0000   1.8080   718.01
     P, ATM            17.011   9.4092  0.02369
     T, K             1274.61  1130.86   287.09
     RHO, G/CC       3.6882-3 2.2993-3 2.3038-5
     H, CAL/G        -1319.43 -1381.79 -1707.68
     U, CAL/G        -1431.13 -1480.90 -1732.58
     G, CAL/G        -4379.64 -4096.87 -2396.94
     S, CAL/(G)(K)     2.4009   2.4009   2.4009

     M, (1/n)          22.676   22.676   22.907
     MW, MOL WT        22.676   22.676   22.676
     (dLV/dLP)t      -1.00000 -1.00000 -2.96976
     (dLV/dLT)p        1.0001   1.0000  37.6942
     Cp, CAL/(G)(K)    0.4410   0.4265  59.6427
     GAMMAs            1.2480   1.2586   1.1073
     SON VEL,M/SEC      763.7    722.4    339.7
     MACH NUMBER        0.000    1.000    5.306

     PERFORMANCE PARAMETERS

     Ae/At                      1.0000   40.000
     CSTAR, FT/SEC              3404.6   3404.6
     CF                         0.6961   1.7369
     Ivac,LB-SEC/LB              132.2    189.7
     Isp, LB-SEC/LB               73.7    183.8


     MOLE FRACTIONS

     H2O              0.66666  0.66667  0.65659
     *OH              0.00001  0.00000  0.00000
     *O2              0.33333  0.33333  0.33333
     H2O(L)           0.00000  0.00000  0.01008

      * THERMODYNAMIC PROPERTIES FITTED TO 20000.K

     NOTE. WEIGHT FRACTION OF FUEL IN TOTAL FUELS AND OF OXIDANT IN TOTAL OXIDANTS

