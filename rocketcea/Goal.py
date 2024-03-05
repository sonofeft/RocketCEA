from math import *

# This CLASS version of goal finder should allow nested Goal loops

# The world's best root finder by Jack Crenshaw
# ... turned into world's best "goal-value" finder by Charlie Taylor
class Goal( object ):
    
        
    def __call__(self, xval=0.0):
        
        ierror = 1 # needs to be reset by cren_goal call to avoid failValue logic
        try:
            X, ierror = self.cren_goal(self.goalVal, self.minX, self.maxX, 
                self.funcOfX, self.tolerance, self.maxLoops)
        except:
            if self.verbose:
                print(  "ERROR unrecoverable in Goal.py")
            X = self.maxX
            
        if ierror:
            if (self.failValue==None):
                X = self.maxX
                if self.verbose:
                    print(  "returned an ERROR=%s from cren_goal in goalFind.py"%str(ierror))
                    print(  "goalVal, minX, maxX, funcOfX",self.goalVal, self.minX, self.maxX, self.funcOfX.__name__)
                    print(  "using X value=",X)
            else:
                X = self.failValue
            
        return X, ierror
        

       
    def __init__(self, goalVal=0.0, minX=0.0, maxX=1000.0, verbose=True,
        funcOfX=None, tolerance=1.0E-4, maxLoops=40, failValue=None):

      # initialize properties
        self.goalVal = goalVal
        self.minX = minX
        self.maxX = maxX
        self.funcOfX = funcOfX
        self.tolerance = tolerance
        self.maxLoops = maxLoops
        self.failValue = failValue
        self.verbose = verbose
    
    
    def cren_goal(self, goal, x0, x2, f, eps, imax):
        # ierror parameters
        SUCCESS = 0
        MyBAD_DATA = 1
        NO_CONVERGENCE = 2
          
        xmlast = x0
        
        y0 = f(x0) - goal
        if (y0 == 0.0):
            return x0, SUCCESS
      
        y2 = f(x2) - goal
        if (y2 == 0.0):
            return x2, SUCCESS
    
        if(y2 * y0 > 0.0):
            return x0, MyBAD_DATA
        
        for i in range(imax):
            #print( 'I=',i,' of ',imax)
          
            x1 = 0.5 * (x2 + x0)
            y1 = f(x1) - goal
            if (y1 ==  0.0) :
                return x1, SUCCESS
            
            if (abs (x1 - x0) < eps) :
                return x1, SUCCESS
            
            if (y1 * y0 > 0.0):
                x0,x2,y0,y2 = (x2,x0,y2,y0) # switch points 0 and 2
            
            y10 = y1 - y0
            y21 = y2 - y1
            y20 = y2 - y0
            if (y2 * y20 < 2.0 * y1 * y10):
                x2,y2 = (x1,y1)
            else:
                b = (x1 - x0) / y10   
                c = (y10 -y21) / (y21 * y20) 
                xm = x0 - b * y0 * (1.0-c * y1)
                ym = f(xm) - goal
                if (ym ==  0.0)  :
                    return xm, SUCCESS
                
                if (abs (xm - xmlast) < eps) :
                    return xm, SUCCESS
                
                xmlast = xm
                if (ym * y0 < 0.0):
                    x2,y2 = (xm,ym)
                else:
                    x0,y0,x2,y2 = (xm,ym,x1,y1)
                
        return x1, NO_CONVERGENCE
            

def dev_tests():  #self test
    
    def FofX(x):
        #print(  'calling FofX with x=',x)
        return x**3 - x**2 + x - 11.875
        
    print(  '======================================================')
    print(  '----->Answer should be = 2.5')
    print(  'with tolerance=1.0E-3  =')
    G = Goal(goalVal=0.0, minX=0.0, maxX=57.123, 
        funcOfX=FofX, tolerance=1.0E-3, maxLoops=40, failValue=None)
    print(  G())
    
    print(  'with tolerance=1.0E-20 =')
    G2 = Goal(goalVal=0.0, minX=0.0, maxX=57.123, 
        funcOfX=FofX, tolerance=1.0E-20, maxLoops=40, failValue=None)
    print(  G2())

if __name__ == "__main__":
    dev_tests()

    