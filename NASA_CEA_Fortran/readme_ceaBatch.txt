                        
			README FOR CEA2 BATCH JOBS

CEA2 is slightly different that the NASA Glenn program described in NASA RP-1311, 
Parts I and II. (Manuals are available from this website). The program now uses an 
INCLUDE file named cea.inc which consolidates PARAMETER assignments and 
COMMON variable statements in one place.  Previously these statements were at 
the beginning of each subroutine.  The INCLUDE file is useful for revising maximum
dimensions and input/output unit assignments. See RP-1311, part II, p39.

The FORTRAN source and data are ASCII files in our anonymous FTP server available 
at the following internet URL:   ftp://ftp.grc.nasa.gov/users/xxminna/CEA+Fortran/ .
        
The files you should have downloaded are:
	readme.cea
	cea2.f	
	cea.inc
	thermo.inp
	trans.ing
	cea2.inp
	cea2.out

Since this program is identical to the one available with the CEAgui download, 
you may use that executable called FCEA2.exe if the platform is appropriate. 
Or you may compile cea2.f  with cea.inc available in the same directory.  
Note that compilers are freely available from the internet.

File names have an extension (suffix) defined as follows: 
	Fortran source programs - .f
	Input files - .inp
	Output files - .out
	Files of output parameters for plotting - .plt
	Include file - .inc
	Unformatted thermodynamic or transport property databases - .lib

The root names of these files are referred to as prefixes. To run CEA2, simply type 
in the name of the executable and CEA2 will interactively ask for the prefix of the input file.

The thermodynamic and transport data files must be preprocessed to form “.lib” files 
before input problem files can be run. The “.lib” files are obtained by running the 
thermo.inp and trans.inp as input files. The user types thermo or trans when CEA2 
asks for an input prefix and the required files are created.  These files do not need to be 
processed again unless the data are revised.  The thermo.out file lists each species name, 
the reference/date code, the heat of formation in Joules/mole at 298.15K , 
and some reference information.

You are now ready to run the sample problems cea2.inp and check with the sample output 
cea2.out

To get thermodynamic properties of individual species for the coefficients or to download subsets of 
the 
thermodynamic data, go to the following URL and see “About ThermoBuild”  for details.

	http://cea.grc.nasa.gov/

If you have any comments or questions, please contact Russell.W.Claus@nasa.gov

 

