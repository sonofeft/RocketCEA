# Written by Charlie Taylor <cet@appliedpython.com>
# Oct,21 2005
'''Interpolated Properties'''

from scipy import array, float64, interpolate

class InterpProp:
    '''Interpolate tables of properties
       will automatically sort input data...
       (x array must be monotonically increasing)'''
       
        
    def __call__(self, xval=0.0):
        return self.getValue( xval )

    def __name__(self):
        return 'InterpProp_scipy'
       
    def __init__(self, xInp=[0,1], yInp=[10,110], extrapOK=1, linear=0,
        minY=None, maxY=None):
          
        # Sort Data to Make sure that x array is monotonically increasing
        a = map(float,xInp)  
        b = map(float,yInp)
        c = [(x,y) for x,y in zip(a,b)]
        c.sort()
        a = []
        b = []
        for aa,bb in c:
            a.append(aa)
            b.append(bb)
        
        # now turn lists into Numpy/SciPy arrays
        self.x = array(a,float64)
        self.y = array(b,float64)
        
        self.extrapOK = extrapOK
        self.linear = linear
        if len(self.x) <= 1:
            self.extrapOK = 0
        if len(self.x) <= 2:
            self.linear = 1
            
        if linear:
            self.Nterp = 1
        else:
            self.Nterp = 2
        
        if len(self.x) > 1:
            #self.interpFunc = interpolate.interp1d(self.x, self.y, 
            #    kind=self.Nterp, bounds_error=False)
                
            self.interpFunc = interpolate.UnivariateSpline(self.x, self.y,  k=self.Nterp, s=0)
        else:
            self.interpFunc = lambda x : self.y[0]
        
        try:
            self.minY = float(minY)
        except:
            self.minY = None
            
        try:
            self.maxY = float(maxY)
        except:
            self.maxY = None
        
        self.delX = max(self.x) - min(self.x)
        
    def getValue(self, xval=0.0):
        xval = float(xval)
        
        if not self.extrapOK:
            if xval<self.x[0]: return self.y[0]
            if xval>self.x[-1]: return self.y[-1]
            #print '1) self.interpFunc( %s ) ='%xval,self.interpFunc( xval )
            return float(self.interpFunc( xval ))
        
        if xval<self.x[0]: 
            yval = self.y[0]+(xval-self.x[0])*(self.y[1]-self.y[0])/(self.x[1]-self.x[0])
        elif xval>self.x[-1]: 
            yval = self.y[-1]+(xval-self.x[-1])*(self.y[-1]-self.y[-2])/(self.x[-1]-self.x[-2])
        else:
            #print '2) self.interpFunc( %s ) ='%xval,self.interpFunc( xval )
            yval = float(self.interpFunc( xval ))
                        
        if self.minY != None: yval = max(yval, self.minY)
        if self.maxY != None: yval = min(yval, self.maxY)
        return yval
        
    def deriv(self, xval=0.0, stepFrac=0.0001):
        dx = self.delX * stepFrac
        return (self(xval+dx) - self(xval-dx)) / 2.0 / dx
          
if __name__ == "__main__": #Self Test
    from pylab import *

    x = [1,2,6,4,5]
    y = [10,40,360,160,250]
    i = InterpProp(x,y)
    print( 'with extrapOK and no min/max' )
    print( 'interpolated x=2.5 =',i.getValue(2.5) )
    print( 'extrapolated x=0.0 =',i.getValue(0.0) )
    print( 'extrapolated x=7.0 =',i.getValue(7) )
    print('')
    i = InterpProp(x,y,extrapOK=0)
    print( 'with extrapOK=0 and no min/max')
    print( 'interpolated x=2.5 =',i.getValue(2.5))
    print( 'extrapolated x=0.0 =',i.getValue(0.0))
    print( 'extrapolated x=7.0 =',i.getValue(7))
    print('')
    i = InterpProp(x,y,minY=0.0, maxY=300)
    print( 'with extrapOK and minY=0, maxY=300')
    print( 'interpolated x=2.5 =',i.getValue(2.5))
    print( 'extrapolated x=0.0 =',i.getValue(0.0))
    print( 'extrapolated x=7.0 =',i.getValue(7))

    print('')
    x = [6, 1]
    y = [360, 10]
    i = InterpProp(x,y,linear=1)
    print( 'Two point linear with extrapOK and no min/max')
    print( 'interpolated x=2.5 =',i.getValue(2.5))
    print( 'extrapolated x=0.0 =',i.getValue(0.0))
    print( 'extrapolated x=7.0 =',i.getValue(7))
    
    print('')
    x = [6]
    y = [360]
    i = InterpProp(x,y)
    print( 'Single point with extrapOK and no min/max')
    print( 'interpolated x=2.5 =',i.getValue(2.5))
    print( 'extrapolated x=0.0 =',i.getValue(0.0))
    print( 'extrapolated x=7.0 =',i.getValue(7))

    print( '='    *55)
    # do rapid laser example from PPT file
    x = [2.,4.25,5.25,7.81,9.2,10.6]
    y = [7.2,7.1,6.,5.,3.5,5.]
    
    q = InterpProp( x, y)
    

    xL = []
    yL = []
    Npts = 100
    for i in range(Npts+1):
        xL.append( 12.*i/Npts )
        yL.append( q(xL[-1]) )
    plt.plot(xL, yL, '-')
    plt.plot( q.x, q.y, 'o-' )
    plt.grid()
    plt.show()
