""" Visit https://developer.github.com/v3/repos/statistics/
    for API tips.

    This code builds the HISTORY.rst file using the GitHub API

"""

#    I like extra spaces inside parens and sometimes camelCase
# pylint: disable=C0326, C0325
# pylint: disable=C0103

import requests
import os
import datetime
import getpass

here = os.path.abspath(os.path.dirname(__file__))

fInp = open(os.path.join(here, 'HISTORY.rst'), 'r')
old_lineL = fInp.read().splitlines() # strip \n from end of lines
fInp.close()
for line in old_lineL:
    print(line)

try:
    lastCommitSig = old_lineL[0][3:].strip()
except:
    lastCommitSig = 'commit signature, "date_str author_str sha_str"'
    
print('lastCommitSig =',lastCommitSig)

GITHUB_USER = 'sonofeft'


print( '='*55 )
print( '     Building HISTORY.rst file' )
print( '     at: ' + here )
print( '     Need GitHub user password for ' + GITHUB_USER)
print( '='*55 )
PASSWORD = getpass.getpass(prompt='Enter Password: ')

github_url = "https://api.github.com/repos/sonofeft/RocketCEA/commits"
headers = {'content-type': 'application/json'}

t = requests.get(github_url, auth=(GITHUB_USER,PASSWORD))
commitL = t.json()
    
print( 'len(commitL) =' + '%s'%len(commitL) )

newestCommitSig = None
newAdditionsL = [] # tuples of (date_str, author_str, sha_str, msgL)
for D in commitL:
    date_str = D['commit']['author']['date'][:10]
    author_str = D['commit']['author']['name']
    sha_str =  D['sha']
    msgL = D['commit']['message'].split('\n')

    commitSig = '%s %s %s'%(date_str, author_str, sha_str)
    if not newestCommitSig:
        newestCommitSig = commitSig
        
    print( date_str, author_str, sha_str ) #, msgL
    if commitSig == lastCommitSig:
        print('^^^^^^^^^^^^^^^^^ Matches last commit signature')
        break # only use the new commits, bail out when hitting the last
        
    newAdditionsL.append( (date_str, author_str, sha_str, msgL) )
    

fOut = open(os.path.join(here, 'HISTORY.rst'), 'w')

# save the newest commit signature at the top of HISTORY.rst in REST comment
fOut.write(""".. %s
   Maintain spacing of "History" and "GitHub Log" titles

History
=======

GitHub Log
----------

"""%newestCommitSig)


last_date_str = ''
last_author_str = ''
for date_str, author_str, sha_str, msgL in newAdditionsL:

    if date_str != last_date_str:
        com_date = datetime.datetime.strptime(date_str, '%Y-%m-%d').date()
        fOut.write( '* ' + com_date.strftime('%b %d, %Y') + '\n' )

    if (date_str != last_date_str) or (author_str != last_author_str):
        fOut.write( '    '  +'- (by: %s) '%author_str + '\n' )

    pad = '        - '
    for msg in msgL:
        if msg:
            fOut.write(pad + '%s'%msg + '\n' )
        else:
            pad = '            '



    print( date_str + ' ' + repr(msgL) )

    last_date_str = date_str
    last_author_str = author_str

fOut.write( '\n'.join(old_lineL[8:]) )


fOut.close()
