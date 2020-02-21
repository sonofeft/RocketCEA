
from subprocess import run, Popen, PIPE, STDOUT
import os, sys
import time

here = os.path.abspath(os.path.dirname(__file__))

cmd = '''cmd.exe /c start /d"%s" /b git --no-pager log  --since=2000.weeks'''%here
p = run(cmd, bufsize=0, shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)#, close_fds=True)
output = p.stdout.decode('utf-8')

commitL = output.split('\n')


class Commit( object ):
    def __init__(self, istart):
        
        self.author = ''
        self.date = ''
        
        i = istart + 1
        while not commitL[i].startswith('Author: '):
            i += 1
            if i >= len( commitL ):
                return
        self.author = commitL[i].split()[1]
        
        while not commitL[i].startswith('Date: '):
            i += 1
            if i >= len( commitL ):
                return
        
        sL = commitL[i].split()
        try:
            self.date = '%s %s, %s'%( sL[2], sL[3], sL[5] )
        except:
            self.date = str(sL)
        
        sL = []
        i += 1
        while not commitL[i].startswith('commit '):
            s = commitL[i].strip()
            if s:
                sL.append( '        - ' + s)
            i += 1
            if i >= len( commitL ):
                break
            
        self.desc = '\n'.join(sL)
            
    def __str__(self):
        s = """* %s\n    - (by: %s)\n%s"""%(self.date, self.author, self.desc)
        return s

commit_objL = []

if commitL:
    startL = []
    for i,line in enumerate(commitL):
        if line.startswith('commit '):
            startL.append( i )
    
    for i in startL:
        C = Commit( i )
        if commit_objL:
            if (C.date==commit_objL[-1].date) and (C.author==commit_objL[-1].author):
                commit_objL[-1].desc = commit_objL[-1].desc + '\n' + C.desc
            else:
                commit_objL.append( C )
        else:
            commit_objL.append( C )
        
for C in commit_objL:
    print( C, '\n' )