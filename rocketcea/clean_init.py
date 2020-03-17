#!/usr/bin/env python
# -*- coding: ascii -*-

"""
Before each CEA run, initialize all COMMON BLOCK variables to their initial values
"""

import numpy as np

INIT_D = {} # index=(commblock, varname): value=numpy array

def clean_list( L ):
	'''Return a list w/o all the __xxxx__ entries'''
	return [val  for val in L  if not val.startswith('__')]

def gather_initial_vals( py_cea ): # py_cea is the imported FORTRAN module
	'''Collect all the FORTRAN COMMON BLOCK arrays'''
	# iterate over all the py_cea interface and find the commblock arrays
	for cbName in dir(py_cea):
		if not cbName.startswith('__'):
			obj = getattr(py_cea, cbName )
			cL = clean_list( dir(obj))
			if cL:
				for vname in cL:
					var = getattr( obj, vname )
					# make a copy of the numpy array
					INIT_D[ (cbName, vname) ] = np.copy(var)#, 'F')
	
	
def set_py_cea_initial_vals( py_cea ):
	'''Use the numpy arrays in INIT_D to reinitialize py_cea COMMON BLOCKs'''

	for cbName in dir(py_cea):
		if not cbName.startswith('__'):
			obj = getattr(py_cea, cbName )
			cL = clean_list( dir(obj))
			if cL:
				for vname in cL:
					var = getattr( obj, vname )
					#print('        ', var)
					saved_var = INIT_D[ (cbName, vname) ]
				
					# reset COMMON block vals
					if len(var.shape)==0:
						var = saved_var
					elif len(var.shape)==1:
						var[:] = saved_var
					elif len(var.shape)==2:
						var[:,:] = saved_var
					elif len(var.shape)==3:
						var[:,:,:] = saved_var



