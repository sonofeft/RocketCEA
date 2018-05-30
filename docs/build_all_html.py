"""
Build all of the sphinx HTML files.

First "touch" all *.rst files to put them "out-of-date".
then run "sphinx-build" command

"""

import sys, os, glob
import subprocess

fileL = glob.glob( os.path.join(os.curdir,'*.rst') )

for fname in fileL:
    os.utime(fname, None)
    
command = "sphinx-build -b html -d _build/doctrees  . _build/html"

print command
subprocess.call(command.split())
    
