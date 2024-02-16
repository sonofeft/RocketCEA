# from __future__ import print_function
from math import log10, floor

def calcMinMaxRange(vmin, vmax, num_ticks=10):
    try:
        if vmin>vmax:
            vmin,vmax = vmax,vmin
            diff = vmax - vmin                    
        elif vmin==vmax:
            vmin = 0.0
            diff = vmax
        else:
            diff = vmax - vmin   
            
        ldiff = log10( diff )
        n = int( str(ldiff)[0] ) # first digit of log10 of diff
        
        tenN = 10.0**n
        msd = int(diff / tenN) # most significant decimal digit of diff
        r = (msd+1)*10.0**n # slightly larger than diff, but even number of msd values
        
        t = 10.0**( int(log10(r)) ) # diff in form 1.0Enn
        
        rmin = int(vmin/t) * t
        rmax = int(vmax/t) * t 
        while rmax < vmax:
            rmax += t
        while rmin > vmin:
            rmin -= t
        
        # Now calc a reasonable step size
        tempStep = (rmax-rmin) / num_ticks
        mag = floor( log10(tempStep) )
        magPow = 10.0**mag
        magMSD = int( 0.5 + tempStep/magPow )
        step = magMSD * magPow
        
        return rmin, rmax, step
    except:
        return vmin, vmax, (vmax-vmin)/10.0

if __name__ == "__main__":
    def test( vmin, vmax):
        print( 'for vmin, vmax=',vmin, vmax, end='')
        rmin,rmax,step = calcMinMaxRange(vmin, vmax)
        print( ' (rmin,rmax)=(%g,%g)'%(rmin, rmax),'    step=%g'%step )
    
    test(4233.0, 4610.0)
    test(4610.0, 4233.0)
    
    test( 255.0, 255.0)
    test( -255.0, 255.0)

