from math import atan, pi
import copy

colorL = ['r','g','b','darkcyan','deepskyblue','darkorange','brown','deeppink',
    'maroon','crimson','seagreen','fuchsia','darkviolet' ]


def within(p, q, r):
    "Return true iff q is between p and r (inclusive)."
    return p <= q <= r or r <= q <= p

def getLocalAngle( rhob, rhobL, ispL, rhobRange, ispRange, figsize ):
    '''assume an aspect ratio of w,h = figsize
       assume that rhobL has been sorted, low to high'''
    if len(rhobL)<2:
        return 0
    else:
        for i in range( len(rhobL)-1 ):
            if within(rhobL[i], rhob, rhobL[i+1]):
                break
        dr = rhobL[i+1] - rhobL[i]
        di = ispL[i+1] - ispL[i]
        
        w,h = figsize
        fdr = w * dr / rhobRange
        fdi = h * di / ispRange
        
        if fdr==0.0:
            if fdi>0.0:
                return 90
            else:
                return -90
        else:
            try:
                ang = int( atan( fdi / fdr ) * 180.0 / pi )
                return ang
            except:
                return 0
            
def label_frac_ipos( xL, yL, xrange, yrange, label_frac=0.3 ):
    
    dL = [0.0]
    for i in range(1, len(xL) ):
        d = abs(xL[i]-xL[i-1])/xrange + abs(yL[i]-yL[i-1])/yrange
        dL.append( d + dL[-1] )
        
    d_target = label_frac * dL[-1]
    
    dbest = 1.0E24
    ipos = 0
    for i in range( len(dL) ):
        d = abs( d_target - dL[i] )
        if d<dbest:
            dbest = d
            ipos = i
            
    return ipos
    
    
