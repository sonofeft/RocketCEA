# The following is needed to find rocketcea when self-testing 
import os, sys
here = os.path.abspath(os.path.dirname(__file__))
up_one = os.path.split( here )[0]
up_two = os.path.split( up_one )[0]

if here not in sys.path[:3]:
    sys.path.insert(0, here)
if up_one not in sys.path[:3]:
    sys.path.insert(0, up_one)
if up_two not in sys.path[:3]:
    sys.path.insert(0, up_two)



import numpy as np
from rocketcea.biprop_utils.contour_supt import within

def evalPoly(a,xData,x):
    """
    Evaluate Newton's polynomial p at x.
    p = evalPoly(a,xData,x)
    """
    n = len(xData) - 1  # Degree of polynomial
    p = a[n]
    for k in range(1,n+1):
        p = a[n-k] + (x -xData[n-k])*p
    return p

def coeffts(xData,yData):
    """    
    Compute the coefficients of Newton's polynomial.
    a = coeffts(xData,yData)
    """
    m = len(xData)  # Number of data points
    a = yData.copy()
    for k in range(1,m):
        a[k:m] = (a[k:m] - a[k-1])/(xData[k:m] - xData[k-1])
    return a


def find_first_terp( x_target, xArr, yArr ):
    """
    Step through xArr to see if x_target is between two elements of xArr.
    If found, use Newton's polynomial Interpolation to find value of y.
    If len(xArr) < 4 then return linear interpolation.
    
    Return None if not found.
    
    Newton's polynomial
    a = coeffts(xArr, yArr)
    y = evalPoly(a, xArr, x)
    """
    
    len_limit = len(xArr) / 2
    
    if 1:
        xL = []
        yL = []
        got_it = False
        for i in range( len(xArr)-1 ):
            if xArr[i] > 0.0 and xArr[i+1] > 0.0:
                if within(xArr[i], x_target, xArr[i+1]):
                    got_it = True
                xL.append(xArr[i])
                yL.append(yArr[i])
                
            elif got_it:
                break
            else:
                xL = []
                yL = []
                
        if len(xL)>3:
            xArr = np.array( xL )
            yArr = np.array( yL )
        else:
            return None
    else:
        if not np.all( xArr>0 ):
            return None
    
    i_target = -1
    for i in range(1, len(xArr)-1 ):        
        if within(xArr[i], x_target, xArr[i+1]):
            i_target = i + 1
            break
            
    if i_target < 0:
        return None
        
    if len(xArr)<4: # return linear interp
        # xArr[i_target+1] will be valid here
        frac = (x_target-xArr[i_target]) / (xArr[i_target+1]-xArr[i_target])
        return yArr[i_target] + frac*( yArr[i_target+1] - yArr[i_target] )
    
    i = min(i_target, len(xArr)-4)
    #print 'i_target=%i,  i=%i'%(i_target, i)
    
    xData = xArr[i:i+4]
    yData = yArr[i:i+4]
    #print 'xData =',xData
    #print 'yData =',yData
    a = coeffts( xData, yData )
    if np.isnan(a).any():
        return None
        
    ans = evalPoly(a, xData, x_target)
    return ans


def dev_tests():
    
    sgArr = np.array([0.13433717, 0.69979866, 1.26526015])
    isArr = np.array([290.04456811, 340.42993479, 390.81530146, 441.20066813])
    print( 'sgArr =',sgArr)
    print( 'isArr =',isArr)

    
    glow= np.array([[1317.75945868, 1263.61828587, 1225.21325698, 1196.56151539],
                    [1317.75945868, 1263.61828587, 1225.21325698, 1196.56151539],
                    [1317.75945868, 1263.61828587, 1225.21325698, 1196.56151539]])
    print( 'glow=',glow)
    
    
    is_ans = find_first_terp( 1240.0, glow[0], isArr )
    print( 'is_ans =',is_ans)
    sg_ans = sgArr[0]
    print( 'sg_ans =',sg_ans )

if __name__ == "__main__":
    dev_tests()

    