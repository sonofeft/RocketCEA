.. std_examples

.. _`std_examples_link`:

Standard Examples
=================

For those with `NASA CEA <https://www.grc.nasa.gov/WWW/CEAWeb/ceaHome.htm>`_ experience,
running CEA and reviewing the printed output is standard practice.

The following examples show how to run a case and review the standard `CEA <https://www.grc.nasa.gov/WWW/CEAWeb/ceaHome.htm>`_ output.


RocketCEA always begins with an import statement and an instance of a CEA_obj::

    from rocketcea.cea_obj import CEA_Obj
    ispObj = CEA_Obj( oxName='LOX', fuelName='LH2')

Instead of manually assembling a run deck for CEA, the above is all that is needed to run a CEA **rocket** case with standard
definitions of all the most common rocket propellants.

.. _`example_1_link`:

LOX/LH2 Performance
-------------------

The script below is typical to calculate predicted equilibrium performance at a specific
chamber pressure, mixture ratio and expansion ratio.

The script uses the **short** output option. (long-form output is default)

.. literalinclude:: ./_static/example_scripts/loxlh2_full_output.py

Notice that RocketCEA generates the propellant cards and input parameters for the **rocket equilibrium** problem.
(as opposed to a **shock** or **detonation** problem)

The propellant temperature and enthalpy are *standard*, which usually means room temperature for storable or gas
propellants and normal boiling point for cryogenic propellants.

See the :ref:`Temperature Adjust <temperature_adjust_link>` page for modifying the temperature and enthalpy of the reactants.

Notice also that the CEA documentation (shown below) allows for different pressure units to be used for Pc.
The default units in **RocketCEA** are psia, however, bar, atm and mmh are also options.

.. image:: ./_static/full_output_p_units.jpg

Pc units can be specified with the input parameter **pc_units** as shown in the following lines::

    s = ispObj.get_full_cea_output( Pc=1000.0, MR=6.0, eps=40.0, short_output=1, pc_units='psia')
    s = ispObj.get_full_cea_output( Pc=68.948, MR=6.0, eps=40.0, short_output=1, pc_units='bar')
    s = ispObj.get_full_cea_output( Pc=68.046, MR=6.0, eps=40.0, short_output=1, pc_units='atm')
    s = ispObj.get_full_cea_output( Pc=51715., MR=6.0, eps=40.0, short_output=1, pc_units='mmh')

The above script gives the standard output that a typical CEA run from the command prompt would give::

     *******************************************************************************

             NASA-GLENN CHEMICAL EQUILIBRIUM PROGRAM CEA, OCTOBER 18, 2002
                       BY  BONNIE MCBRIDE AND SANFORD GORDON
          REFS: NASA RP-1311, PART I, 1994 AND NASA RP-1311, PART II, 1996

     *******************************************************************************



     reac
      fuel H2(L)  H 2
      h,cal=-2154.0      t(k)=20.27       wt%=100.
      oxid O2(L)  O 2
      h,cal=-3102.      t(k)=90.18       wt%=100.
      
     prob case=LOX_/_LH2
      rocket equilibrium  p,psia=1000.000000, supar=40.000000,
      o/f=6.000000
      
     outp   calories short
      
     end





                  THEORETICAL ROCKET PERFORMANCE ASSUMING EQUILIBRIUM

               COMPOSITION DURING EXPANSION FROM INFINITE AREA COMBUSTOR

     Pinj =  1000.0 PSIA
     CASE = LOX_/_LH2      

                 REACTANT                    WT FRACTION      ENERGY      TEMP
                                              (SEE NOTE)      CAL/MOL       K  
     FUEL        H2(L)                        1.0000000     -2154.000     20.270
     OXIDANT     O2(L)                        1.0000000     -3102.000     90.180

     O/F=    6.00000  %FUEL= 14.285714  R,EQ.RATIO= 1.322780  PHI,EQ.RATIO= 1.322780

                     CHAMBER   THROAT     EXIT
     Pinf/P            1.0000   1.7351   459.06
     P, ATM            68.046   39.216  0.14823
     T, K             3483.35  3291.03  1440.95
     RHO, G/CC       3.2038-3 1.9758-3 1.7690-5
     H, CAL/G         -235.74  -509.81 -2372.54
     U, CAL/G         -750.09  -990.49 -2575.47
     G, CAL/G        -15090.3 -14544.2 -8517.40
     S, CAL/(G)(K)     4.2644   4.2644   4.2644

     M, (1/n)          13.458   13.606   14.111
     (dLV/dLP)t      -1.02525 -1.01954 -1.00000
     (dLV/dLT)p        1.4496   1.3682   1.0001
     Cp, CAL/(G)(K)    2.0951   1.9171   0.7308
     GAMMAs            1.1401   1.1403   1.2388
     SON VEL,M/SEC     1566.3   1514.4   1025.6
     MACH NUMBER        0.000    1.000    4.123

     PERFORMANCE PARAMETERS

     Ae/At                     1.00000   40.000
     CSTAR, FT/SEC              7560.0   7560.0
     CF                         0.6572   1.8351
     Ivac,LB-SEC/LB              289.8    451.7
     Isp, LB-SEC/LB              154.4    431.2


     MOLE FRACTIONS

     *H               0.03417  0.02810  0.00001
     HO2              0.00003  0.00002  0.00000
     *H2              0.24832  0.24538  0.24401
     H2O              0.66590  0.68751  0.75598
     H2O2             0.00001  0.00001  0.00000
     *O               0.00334  0.00217  0.00000
     *OH              0.04478  0.03446  0.00000
     *O2              0.00345  0.00236  0.00000

      * THERMODYNAMIC PROPERTIES FITTED TO 20000.K

     NOTE. WEIGHT FRACTION OF FUEL IN TOTAL FUELS AND OF OXIDANT IN TOTAL OXIDANTS


Transport Properties
--------------------

New in RocketCEA version 1.06,
to include transport properties, set the **show_transport** flag.

.. literalinclude:: ./_static/example_scripts/loxlh2_transport_output.py

which adds the following lines to the output::


     TRANSPORT PROPERTIES (GASES ONLY)
       CONDUCTIVITY IN UNITS OF MILLICALORIES/(CM)(K)(SEC)

     VISC,MILLIPOISE   1.0588   1.0153  0.43328

      WITH FROZEN REACTIONS

     Cp, CAL/(G)(K)    0.9029   0.8950   0.6859
     CONDUCTIVITY      1.3519   1.2760   0.4369
     PRANDTL NUMBER    0.7071   0.7121   0.6803

To access transport properties, each of the following calls return a tuple of
(Heat Capacity, Viscosity, Thermal Conductivity and Prandtl Number)::

    Cp, visc, cond, Pr = ispObj.get_Chamber_Transport(Pc=1000.0, MR=6.0)
    Cp, visc, cond, Pr = ispObj.get_Chamber_Transport(Pc=1000.0, MR=6.0, frozen=1)

    Cp, visc, cond, Pr = ispObj.get_Throat_Transport(Pc=1000.0, MR=6.0)
    Cp, visc, cond, Pr = ispObj.get_Throat_Transport(Pc=1000.0, MR=6.0, frozen=1)

    Cp, visc, cond, Pr = ispObj.get_Exit_Transport(Pc=1000.0, MR=6.0, eps=40.0)
    Cp, visc, cond, Pr = ispObj.get_Exit_Transport(Pc=1000.0, MR=6.0, eps=40.0, frozen=1)

where the **frozen** flag determines equilibrium or frozen output.

The standard units will be the same as the printout, namely::

    Cp   = CAL/(G)(K)
    visc = MILLIPOISE
    cond = MILLICALORIES/(CM)(K)(SEC)
    Pr   = dimensionless

If different units are desired, use the **cea_obj_w_units** wrapper, for example::

    from rocketcea.cea_obj_w_units import CEA_Obj
    C = CEA_Obj( oxName='LOX', fuelName='LH2', 
                 specific_heat_units='kJ/kg-K',
                 viscosity_units='poise', 
                 thermal_cond_units='BTU/s-in-degF')
    

Frozen Performance
------------------

To run the same LOX/LH2 case above, but with frozen composition during expansion, the following script is used.

Notice here that the flag to freeze the composition at the throat is set. 
Otherwise, the chamber composition is used.

Also notice that without the short output flag, the long form of output results.

.. literalinclude:: ./_static/example_scripts/frozen_full_output.py

The result are shown below::


     *******************************************************************************

             NASA-GLENN CHEMICAL EQUILIBRIUM PROGRAM CEA, OCTOBER 18, 2002
                       BY  BONNIE MCBRIDE AND SANFORD GORDON
          REFS: NASA RP-1311, PART I, 1994 AND NASA RP-1311, PART II, 1996

     *******************************************************************************



     reac
      fuel H2(L)  H 2
      h,cal=-2154.0      t(k)=20.27       wt%=100.
      oxid O2(L)  O 2
      h,cal=-3102.      t(k)=90.18       wt%=100.
      
     prob case=LOX_/_LH2
      rocket frozen nfz=2   p,psia=1000.000000, supar=40.000000,
      o/f=6.000000
      
     outp   calories
      
     end

     OPTIONS: TP=F  HP=F  SP=F  TV=F  UV=F  SV=F  DETN=F  SHOCK=F  REFL=F  INCD=F
     RKT=T  FROZ=T  EQL=F  IONS=F  SIUNIT=F  DEBUGF=F  SHKDBG=F  DETDBG=F  TRNSPT=F

     TRACE= 0.00E+00  S/R= 0.000000E+00  H/R= 0.000000E+00  U/R= 0.000000E+00

     Pc,BAR =    68.947304

     Pc/P =

     SUBSONIC AREA RATIOS =

     SUPERSONIC AREA RATIOS =    40.0000

     NFZ=  2  Mdot/Ac= 0.000000E+00  Ac/At= 0.000000E+00

        REACTANT          WT.FRAC   (ENERGY/R),K   TEMP,K  DENSITY
            EXPLODED FORMULA
     F: H2(L)            1.000000  -0.108393E+04    20.27  0.0000
              H  2.00000
     O: O2(L)            1.000000  -0.156098E+04    90.18  0.0000
              O  2.00000

      SPECIES BEING CONSIDERED IN THIS SYSTEM
     (CONDENSED PHASE MAY HAVE NAME LISTED SEVERAL TIMES)
      LAST thermo.inp UPDATE:    9/09/04

      g 6/97  *H               g 4/02  HO2              tpis78  *H2            
      g 8/89  H2O              g 6/99  H2O2             g 5/97  *O             
      g 4/02  *OH              tpis89  *O2              g 8/01  O3             
      g11/99  H2O(cr)          g 8/01  H2O(L)           g 8/01  H2O(L)         

     O/F =   6.000000

                           EFFECTIVE FUEL     EFFECTIVE OXIDANT        MIXTURE
     ENTHALPY                  h(2)/R              h(1)/R               h0/R
     (KG-MOL)(K)/KG       -0.53769505E+03     -0.48782395E+02     -0.11862706E+03

     KG-FORM.WT./KG             bi(2)               bi(1)               b0i
      *H                   0.99212255E+00      0.00000000E+00      0.14173179E+00
      *O                   0.00000000E+00      0.62502344E-01      0.53573438E-01

     POINT ITN      T            H           O 
       1    9    3483.350      -9.273     -16.160
     Pinf/Pt = 1.734909
       2    4    3291.075      -9.450     -16.510
     Pinf/Pt = 1.735136
       2    1    3291.030      -9.450     -16.510





               THEORETICAL ROCKET PERFORMANCE ASSUMING FROZEN COMPOSITION
                                     AFTER POINT 2

     Pinj =  1000.0 PSIA
     CASE = LOX_/_LH2      

                 REACTANT                    WT FRACTION      ENERGY      TEMP
                                              (SEE NOTE)      CAL/MOL       K  
     FUEL        H2(L)                        1.0000000     -2154.000     20.270
     OXIDANT     O2(L)                        1.0000000     -3102.000     90.180

     O/F=    6.00000  %FUEL= 14.285714  R,EQ.RATIO= 1.322780  PHI,EQ.RATIO= 1.322780

                     CHAMBER   THROAT     EXIT
     Pinf/P            1.0000   1.7351   539.53
     P, ATM            68.046   39.216  0.12612
     T, K             3483.35  3291.03  1149.39
     RHO, G/CC       3.2038-3 1.9758-3 1.8194-5
     H, CAL/G         -235.74  -509.81 -2255.83
     U, CAL/G         -750.09  -990.49 -2423.71
     G, CAL/G        -15090.3 -14544.2 -7157.33
     S, CAL/(G)(K)     4.2644   4.2644   4.2644

     M, (1/n)          13.458   13.606   13.606
     Cp, CAL/(G)(K)    2.0951   1.9171   0.6859
     GAMMAs            1.1401   1.1403   1.2705
     SON VEL,M/SEC     1566.3   1514.4    944.7
     MACH NUMBER        0.000    1.000    4.352

     PERFORMANCE PARAMETERS

     Ae/At                     1.00000   40.000
     CSTAR, FT/SEC              7560.0   7560.0
     CF                         0.6572   1.7843
     Ivac,LB-SEC/LB              289.8    436.7
     Isp, LB-SEC/LB              154.4    419.3

     MOLE FRACTIONS

     *H              0.02810   HO2             0.00002   *H2             0.24538
     H2O             0.68751   H2O2            0.00001   *O              0.00217
     *OH             0.03446   *O2             0.00236

      * THERMODYNAMIC PROPERTIES FITTED TO 20000.K

        PRODUCTS WHICH WERE CONSIDERED BUT WHOSE 

