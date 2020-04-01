#!/usr/bin/env python
# -*- coding: ascii -*-
# This Python Source Code Generated by XYmath

def curve_fit_func( x ):
    '''
    Curve Fit Results from XYmath 03/21/2020
    Can be called with x=float or x=numpy array

    y = 1 + A/x**c
        A = 0.5415348635596349
        c = 2.1947827271417966
        x = 5.000000000000002]
        y = 1.0170129489294337]
        Correlation Coefficient = 0.9997414525145157
        Standard Deviation = 0.0011470085872208342
        Percent Standard Deviation = 0.10420537921305567%
    y = 1 + 0.5415348635596349/x**2.1947827271417966

     (x,y) Data Pairs from 03/21/2020 Used in Curve Fit 
     (x,y) = (1.6,1.19554),(1.8,1.14797),(2,1.11647),(2.2,1.09433),
        (2.4,1.07809),(2.6,1.0658),(2.8,1.05624),(3,1.04865),(3.2,1.04252),
        (3.4,1.03749),(3.6,1.03331),(3.8,1.0298),(4,1.02682),(4.2,1.02427),
        (4.4,1.02207),(4.6,1.02016),(4.8,1.01849)

    '''
    if x<1.5999999999999999 or x>4.800000000000002:
        print( 'WARNING... x is outside range in curve_fit_func' )
        print( '  x =',x,' x range = (1.5999999999999999 to 4.800000000000002)'  )
    
    return 1 + 0.5415348635596349/x**2.1947827271417966

if __name__=='__main__':
    print( '='*44 )
    y_test = curve_fit_func( 4.800000000000002 )
    print( 'y_test  =',y_test,'for x_test =',4.800000000000002 )
    print( 'y_xymath=',1.0173160934362933 )
    print( )
    print( 'y_test should equal y_xymath above.' )
