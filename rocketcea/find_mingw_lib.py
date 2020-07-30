import os
import sys

"""
For users that compile FORTRAN with MinGW, python 3.8 and above need to 
add the import path of the DLL libraries.
"""

def add_mingw_lib():
    # python >=3.8 needs to be given permission to import DLL files.
    if hasattr(os, 'add_dll_directory'):
        is_64_bit = sys.maxsize > 2**32 

        path_str = os.environ.get('PATH')

        pathL = path_str.split(';')
        for path in pathL:
            slower = path.lower()
            if slower.endswith('bin'):
                if is_64_bit:
                    if slower.find('mingw64') >= 0:
                        #print( '-->Adding: "%s"'%path,'to os.add_dll_directory' )
                        os.add_dll_directory( path )
                else:
                    if slower.find('mingw32') >= 0:
                        #print( '-->Adding: "%s"'%path,'to os.add_dll_directory' )
                        os.add_dll_directory( path )
    
    
    