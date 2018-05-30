#!/usr/bin/env python
import os, sys
import webbrowser

here = os.path.abspath(os.path.dirname(__file__))

INDEX_PAGE = os.path.join( here, 'sphinx_html', 'index.html' )

def main():
    webbrowser.open(INDEX_PAGE)

if __name__ == '__main__':
    main()
