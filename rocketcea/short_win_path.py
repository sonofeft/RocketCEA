import sys
import os
import pathlib

# try to solve spaces in Windows Path names by making 8.3, short path name.
_CAN_RUN_CTYPES = False
if sys.platform == 'win32':
    try:
        import ctypes
        from ctypes import wintypes
        _GetShortPathNameW = ctypes.windll.kernel32.GetShortPathNameW
        _GetShortPathNameW.argtypes = [wintypes.LPCWSTR, wintypes.LPWSTR, wintypes.DWORD]
        _GetShortPathNameW.restype = wintypes.DWORD
        _CAN_RUN_CTYPES = True
    except:
        print('ERROR getting _GetShortPathNameW')


def get_usable_path( inp_path ):
    pobj = pathlib.Path( inp_path )
    partsL = list( pobj.parts )
    # print( 'partsL =', partsL )
    
    for i in range( len(partsL) ):
        path = pathlib.Path( os.path.sep.join( partsL[0:i+1] ) )
        # print( 'path =', path,  '   path.exists() =', path.exists() )  
        
        if partsL[i].find(' ') >= 0:
            if path.exists():
                # print( '   Exists AND has spaces:', partsL[i])
                # print( '       (need 8.3 format)' )
                short_path = get_short_path_name( str(path) )
                # print( '        short_path =', short_path )
                pshort = pathlib.Path( short_path )
                partsL[i] = pshort.parts[i]
            else:
                # print( '   Does NOT exist AND has spaces:', partsL[i] )
                # print( '       (need spaces replaced with underscore)' )
                partsL[i] = partsL[i].replace(' ', '_')
    # print( '-'*22 )

    path = pathlib.Path( os.path.sep.join( partsL ) )
    # print( 'Final Path =', str(path) )
    return str(path)

def get_short_path_name(long_name):
    """
    Gets the short path name of a given long path.
    http://stackoverflow.com/a/23598461/200291
    
    GetShortPathName is used by first calling it without a destination buffer. 
    It will return the number of characters you need to make the destination buffer. 
    You then call it again with a buffer of that size. 
    If, due to a TOCTTOU problem, the return value is still larger, 
    keep trying until you've got it right.:
    """
    print( 'long_name =', long_name)
    import pathlib
    pobj = pathlib.Path( long_name )
    # print('    pobj =', pobj,  '   pobj.exists() =', pobj.exists() )
    
    # if not os.path.isdir( long_name ):
    #     # Windows Short names only work for existing directories
    #     long_name = long_name.replace(' ', '_')
    #     print('New directory w/o any spaces in name')
    #     print('    ', long_name)
    #     return long_name
    
    #if sys.platform != 'win32':
    if not _CAN_RUN_CTYPES:
        print( 'Failed to get short name in get_short_path_name' )
        print( '   for long_name:', long_name )
        return long_name
    
    output_buf_size = 0
    counter = 0
    COUNTER_LIMIT = 20 # prevent infinite loop
    while counter < COUNTER_LIMIT:
        counter += 1
        output_buf = ctypes.create_unicode_buffer(output_buf_size)
        needed = _GetShortPathNameW(long_name, output_buf, output_buf_size)
        if output_buf_size >= needed:
            print( 'Using short form of Windows path:' )
            
            if len(output_buf.value) > 0 and output_buf.value.find(' ') < 0:
                print( '   "%s"'%output_buf.value )
                return output_buf.value
        else:
            output_buf_size = needed
            
    print( 'Failed to get short name in get_short_path_name' )
    print( '   for long_name:', long_name )
    print( '   Trying with underscores in place of spaces')
    return long_name.replace(' ', '_')
