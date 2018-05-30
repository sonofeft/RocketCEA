#!/usr/bin/env python
# sphinxy: commandline continuous integration sphinx documentation
#
# Copyright (C) 2015 Charlie Taylor <ctatsourceforge@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

"""
sphinxy - Script to monitor changes to *.rst files and rerun sphinx-build

Since sphinx-build checks for "out-of-date" rst files,
first "touch" all *.rst files to put them "out-of-date".
Then run "sphinx-build" command

sphinxy tries to launch a web browser with the index.html file from local disk.
This is not guaranteed to work on all systems.

After each rebuild, hit F5 (screen refresh) to see the new HTML doc files.

This is heavily adapted from nosy by Mike Steder at:
https://gist.github.com/steder/1220683
"""


__version__ = 1.0

import os
import subprocess
import stat
import sys
import time
import glob
import webbrowser
from keyboard_hit import KBHit

kb = KBHit()

TEST_RUNNER =  "sphinx-build -b html -d _build/doctrees  . _build/html"
SPELL_RUNNER = "sphinx-build -b spelling -d _build/doctrees  . _build/html"

PATTERNS = ["*.rst",]
TARGET_DIR = os.curdir

INDEX_PAGE = os.path.join( TARGET_DIR, '_build', 'html', 'index.html' )

webbrowser.open(INDEX_PAGE)

def print_instructions():
    print('')
    print('='*55)
    print('      Hit F5 (Refresh) in Browser')
    print('='*55)
    print('  hit ESC or <ctrl>C to exit')
    print('  hit "b" to launch webbrowser')
    print('  hit "s" to spell check (if sphinxcontrib-spelling is installed)')
    print('  hit any other key to rebuild HTML')
    

def checksum_directory(directory, touch_first=False):
    """
    Walk directory structure and return simple checksum based on
    file size and modified time.
    """
    file_checksums = []
    fileL = glob.glob( os.path.join(directory,'*.rst') )
    
    for source_path in fileL:
    
        if touch_first: # 
            os.utime(source_path, None)
            
        try:
            stats = os.stat(source_path)
        except OSError:
            # ignore temp files and files we don't
            # have perms to access
            continue
        file_checksums.append(
            stats[stat.ST_SIZE] + stats[stat.ST_MTIME])
    return sum(file_checksums)


def main():
    args = " ".join(sys.argv[1:])
    command = "%s %s"%(TEST_RUNNER, args)

    latest_checksum = checksum_directory(TARGET_DIR)
    print( "Sphinxy starting with: %s"%(command) )
    print( command )
    subprocess.call(command.split())
    print_instructions()
    
    try:
        while (True):
            
            checksum = checksum_directory(TARGET_DIR, touch_first=False)
            if checksum != latest_checksum:
                print( "Sphinxy detected a change and is rerunning tests with: %s"%(command) )
                latest_checksum = checksum_directory(TARGET_DIR, touch_first=True)
                subprocess.call(command.split())
                
                print_instructions()
                
            time.sleep(1)
            command = "%s %s"%(TEST_RUNNER, args)
            if kb.kbhit():
                c = kb.getch()
                if ord(c) == 27: # ESC
                    sys.exit()
                elif ord(c) == ord('b'): # launch browser
                    webbrowser.open(INDEX_PAGE)
                elif ord(c) == ord('s'): # spell check
                    command = "%s %s"%(SPELL_RUNNER, args)
                    latest_checksum = -1
                else:
                    latest_checksum = -1
                    
                    
    except KeyboardInterrupt:
        print( "Exiting Sphinxy..." )


if __name__=="__main__":
    main()