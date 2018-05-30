                                              README FOR PAC

We are currently using a version of PAC called PAC99.  Some input features will be 
a little different than those described in the manual, NASA RP-1271, 1992, available 
from the website http://www.grc.nasa.gov/WWW/CEAWeb/.  Also atomic weights, 
fundamental constants, and some of the examples have been updated.  
Please let us know if you have any difficulties.

The FORTRAN source and data are ASCII files in our anonymous FTP server available 
at the following URL:

         ftp://ftp.grc.nasa.gov/users/xxminna/CEA+Fortran/pac/
         
The files you should have downloaded are:

	readme.pac
	pac99.f
	new.groups
	EFdata.i97
	exp.i97
	exp.o97
	exp.c97
	exp.e97

The root names of these files are referred to as prefixes. File names have extensions 
(suffixes) defined as follows: 

	Fortran source programs - .f
	Input files - .i97
	General output listing with tables - .o97
	Coefficients in latest thermo data format - .c97
	Element data used in calculating the heat of Formation and LogK - .e97

The “.e97” file will be used only for elements and then only if the option is called for in 
the “i97” file. Coefficients output “.c97” is also an option.

General information to get started:

* Compile the fortran program pac99.f.  Note that compilers are freely 
      available from the internet.
* Type in the name of the executable.  PAC99 will ask for the name of the input prefix.
* The first input file should the EFdata.i97.  PAC99 will make a binary file unf.EFdata 
      to be used for the heat of Formation and LogK calculations in the tables.
* The file new.groups must be available for the group additivity option.  It is not a binary file.
* Run the sample problems exp.i97 and check the output files.

To get thermodynamic properties of individual species for the NASA Glenn coefficients database 
or to download subsets of this data, go to the following URL and see “About ThermoBuild”
 for details.      
         http://cea.grc.nasa.gov/

If you have any comments or questions, please contact Russell.W.Claus@nasa.gov
         

