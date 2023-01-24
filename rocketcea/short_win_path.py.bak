import sys

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
        pass


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
            print( '   ', output_buf.value )
            return output_buf.value
        else:
            output_buf_size = needed
            
    print( 'Failed to get short name in get_short_path_name' )
    print( '   for long_name:', long_name )
    return long_name
