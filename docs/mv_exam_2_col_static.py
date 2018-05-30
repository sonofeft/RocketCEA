"""
Move everything from the examples subdirectory to the ./_static/example_scripts
subdirectory.

Move *.py files both as *.py and as colorized html.

Build a file "examples.rst" with links and references to all the files and images.
"""
import sys, os
import shutil
import glob

here = os.path.abspath(os.path.dirname(__file__)) # Needed to find fulltoc
up_one = os.path.split( here )[0]  # Needed to find rocketcea development version
src_dir = os.path.join( up_one, 'RocketCEA', 'examples' )
target_dir = os.path.join( here, '_static', 'example_scripts' )

css_path = os.path.join( here, '_static', 'colorized_default.css')
css_src = open(css_path,'r').read()

from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

def make_colorized_file( py_fname, htm_fname ):
    
    name = os.path.split( py_fname )[-1][:-3]
    
    # make a colorized copy of *.py file         
    pysrc_code = open(py_fname, 'r').read()    
    colorized_py_html = highlight(pysrc_code, lexer, formatter)
    col_fname = os.path.join( target_dir, name + '.html' )
    COL_out = open(col_fname, 'w')
    
    COL_out.write( "<html><head><title>%s.py</title></head><body>\n"%name )
    COL_out.write( css_src )
    COL_out.write( "\n<h2>%s.py</h2>\n"%name )
    COL_out.write( colorized_py_html )
    COL_out.write( "</body></html>" )
    COL_out.close()
    
    
    # Create RST lines regardless of other writes.
    fRST_out.write( '\n%s\n%s\n\n'%(name.upper(), '-'*len(name) ) )
    
    fRST_out.write("""
.. literalinclude:: ./_static/example_scripts/%s.py

"""%name)
    
    
    rst_line = """Source File:`%s.py <./_static/example_scripts/%s.html>`_"""%( name, name)
    print rst_line
    fRST_out.write('%s\n\n'%rst_line)
    

def make_py_rst_entry( py_name ):
    
    fRST_out.write("""
.. literalinclude:: ./_static/example_scripts/%s

"""%os.path.split( py_name )[-1])
    
    

def make_img_rst_entry( img_name ):
    src = """
    
.. image:: ./_static/example_scripts/%s
    :width: 45%%
    
    """%os.path.split( img_name )[-1]
    
    fRST_out.write( src )
    
def make_htm_rst_entry( fname ):

    fRST_out.write("""
.. literalinclude:: ./_static/example_scripts/%s.py

"""%os.path.split( fname )[-1])

# ====================================
fRST_name = os.path.join( target_dir, 'examples_dir.txt' )
fRST_out = open(fRST_name, 'w')
fRST_out.write(""".. examples_dir

Examples Directory
==================

A number of examples are included below as starting point for your own investigations.


""")


lexer = get_lexer_by_name("python", stripall=False)
formatter = HtmlFormatter(linenos=True, cssclass="default")

srcL = glob.glob( os.path.join(src_dir, '*') )
for fname in srcL:
    ftarget = os.path.join( target_dir, os.path.split( fname )[-1])
    
    if os.path.isfile( fname ):
        print ' FILE: ', os.path.split(fname)[-1]
        shutil.copyfile(fname, ftarget)
        
        if fname.endswith('.py'): # need to make colorized file 
            htm_fname = os.path.join( target_dir, fname[:-2]+'html' )
            print htm_fname
            #make_colorized_file( fname, htm_fname )
            make_py_rst_entry( fname )
            
            print
        elif fname.endswith('.png') or fname.endswith('.jpg') or fname.endswith('.jpeg'):
            make_img_rst_entry( fname )
            
        elif fname.endswith('htm'): # a Parasol or PRISM file ???
            make_htm_rst_entry( fname )
            
            # HTML needs some processing to correct img href
            htm_src = open(fname, 'r').read()
            sL = htm_src.split('img src=".')
            if len(sL)>1:
                snewL = [ sL[0] ]
                for s in sL[1:]:
                    snewL.append( '/_static/example_scripts/' + s )
                htm_src = 'img src=".'.join(snewL)
            htmlOut = open( ftarget + 'l', 'w')
            htmlOut.write( htm_src )
            htmlOut.close()
            
            
        
    elif os.path.isdir( fname ):
        print  ' DIRECTORY: ',os.path.split(fname)[-1]
        if os.path.exists(ftarget):
            shutil.rmtree(ftarget)
        shutil.copytree(fname, ftarget)

