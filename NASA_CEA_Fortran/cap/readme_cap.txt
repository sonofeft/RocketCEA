 		           README FOR CAP
        
The Coefficiients and Properties (CAP) computer code is described 
in NASA/TP--2001-210595 available from the CEA website. See "Codes
related to thermodynamic properties of individual species..." 
          
           http://www.grc.nasa.gov/WWW/CEAWeb/
          
The report provides a detailed description of input preparation and 
examples of input and output for many species.  
          
The program permits the generation of tabulated thermodynamic functions 
from NASA least-squares coefficients.  CAP provides considerable 
flexibility in the output format, the temperature schedule, and the 
energy units of the calculated properties. ThermoBuild at the following
website is similar except for the fact that it uses the NASA thermo
database and does not allow for as much flexibility in energy units.
          
           http://cea.grc.nasa.gov/
          
The Fortran source code and associated data are available at the 
following internet URL:
          
           ftp://ftp.grc.nasa.gov/users/xxminna/CEA+Fortran/cap/
          
GENERAL INFORMATION TO GET STARTED:
          
1)  Compile the fortran program cap.f.  Note that compilers 
    are freely available from the CEAWeb.
2)  When executing CAP, the file cap.elms must be available
    if heats of formation and log K values are desired.
3)  Input and output files as assumed to be standard input
    (I/O unit 5) and standard output (I/O/ unit 6).  OPEN
    statements may be added to the source code.
4)  Run the executable with the provided example "cap.in",e.g.:
          
            cap.x < cap.in > cap.out
          
Generally, the input consists of 2 records followed by any number of 
sets of coefficients in the format of 9 coefficients for Cp--see 
ThermoBuild (details given in "About ThermoBuild") or the CAP manual.
The first record contains options in the form of keywords.  The
second record indicates the desired T schedule.  These records are
also descrided in source code comments.
          
CAP will generate output tables for each species whoses coefficients
are provided.  An unlimited number of species may be run. For species 
with several phases, the phases must be supplied in increasing order 
by temperature.  If the temperature schedule in record 2 exceeds the
range of the coefficients, CAP will use the coefficients for up to 
20 percent beyond the endpoint.
          
The file "cap.elms" contains coefficients for the reference elements.
These data may be revised or expanded.

If you have any comments or questions, please contact Russell.W.Claus@nasa.gov.     
