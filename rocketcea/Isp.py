from math import sqrt
from rocketcea.Goal import Goal

def CalcPCoPE(gam,eps):
    
    Pc = 100.0  #! just for reference pick a value for Pc
    Pthrt = Pc * (2./(gam+1.))**(gam/(gam-1.))
    PeMax = Pthrt
    PeMin = 1.0E-20

    g1=((gam+1.0)/2.0) ** (1.0/(gam-1.0))
    g2=(gam+1.0)/(gam-1.0)
    g3=(1.0/gam)
    g4=((gam-1.0)/gam)

    for _I in range(54):  # in FORTRAN  use 25 for single precision

        Pe = (PeMax + PeMin) / 2.0
        PCoPE = Pc / Pe
        PEoPC = Pe / Pc
        #print(  'trying Pe=',Pe)

        p1=PEoPC**g3
        p2=1.0-PEoPC**g4

        epsCalc=1.0/(g1*p1*(g2*p2)**0.5)

        if epsCalc <= eps:
            PeMax = Pe
        else:
            PeMin = Pe

        Pe = (PeMax + PeMin) / 2.0
        PCoPE = Pc / Pe
    if abs((epsCalc-eps)/eps) > 0.0001 :
        print('=================  WARNING  ==========================')
        print(  ' Bad Solution in CalcPCoPE')
        print(  ' (desired Area Ratio=',eps,'  actual=',epsCalc,')')
        print(  ' input gam=',gam,' eps=',eps)
        print('=================  WARNING  ==========================')
      
    return PCoPE

def  CalcEps(gam,PCoPE):

    #PEoPC=1.0/PCoPE

    if(PCoPE > 1.0):
        g1=((gam+1.0)/2.0) ** (1.0/(gam-1.0))
        p1=PCoPE**(-1.0/gam)
        g2=(gam+1.0)/(gam-1.0)
        p2=1.0-PCoPE**(-(gam-1.0)/gam)

        eps=1.0/(g1*p1*(g2*p2)**0.5)
    else:
        eps=1.0
      
    return eps
                
def solvePCoPE(gam, eps):
    
    def getEpsForPEoPC( _PEoPC ):
        return CalcEps(gam,1./_PEoPC)-eps
        
    PToPC = (2./(gam+1.))**(gam/(gam-1.))
    G = Goal(goalVal=0.0, minX=1.0E-12, maxX=PToPC, 
        funcOfX=getEpsForPEoPC, tolerance=1.0E-12, maxLoops=40, failValue=None)
    PEoPC, ierror = G()
    return 1./PEoPC

def  CalcCFvac(GamInp,PCoPE):

    #c calculate separation pressure for nozzle
    if (GamInp < 1.0001):
        gam = 1.0001
    else:
        gam = GamInp
      
    eps = CalcEps(gam,PCoPE)

    g1=2*gam**2/(gam-1.0)
    g2=(2.0/(gam+1.0))**((gam+1.0)/(gam-1.0))

    g3=1.0-(1.0/PCoPE)**((gam-1.0)/gam)
    CFvac=0.0
    if(g3 > 0.0):CFvac=sqrt(g1*g2*g3) + eps/PCoPE
    if(g3 <= 0.0 or CFvac<0.001):CFvac=0.001
        
    return CFvac

def  CalcCstar(Gam,Temp,WtMol):

    bot=Gam*((2.0/(Gam+1.0))**((Gam+1.0)/(Gam-1.0)))**0.5
    top= ( Gam * 1545.0 * 32.174 * Temp / WtMol ) ** 0.5
    cstar=top/bot
    
    return cstar

def  CalcIdealIsVac(Gam,Temp,WtMol,eps):

    cstar = CalcCstar(Gam,Temp,WtMol)
    pcope = CalcPCoPE(Gam,eps)
    Cfvac = CalcCFvac(Gam,pcope)
    
    IspVac=cstar * Cfvac / 32.174
      
    return IspVac


def dev_tests():
    
    def testme():
        gam=1.25
        eps=2.0
        Temp=5000.0
        WtMol=20.0
        Pc=250.0
        Pamb=0.0
        print(  'for gam=%g and eps=%g'%(gam,eps))
        print( )
        pcope = solvePCoPE(gam, eps)
        print(  'solvePCoPE(%g,%g)='%(gam,eps),pcope)
        print(  'CalcEps(gam,PCoPE)=',CalcEps(gam,pcope))
        print(  )
        pcope = CalcPCoPE(gam,eps)
        
        print(  'CalcPCoPE(%g,%g)='%(gam,eps),pcope,'    Pe/Pc=',1./pcope)
        print(  'CalcEps(gam,PCoPE)=',CalcEps(gam,pcope))
        print(  'CalcCFvac(gam,PCoPE)',CalcCFvac(gam,pcope))
        
        print(  'for T=%g degR and WtMol=%g lbm/lbmole'%(Temp, WtMol))
        print(  'CalcCstar(Gam,Temp,WtMol)',CalcCstar(gam,Temp,WtMol))
        
        print(  'for Pc=%g  and Pamb=%g'%(Pc,Pamb))
        print(  'CalcIdealIsVac(Gam,Temp,WtMol,eps)',CalcIdealIsVac(gam,Temp,WtMol,eps))
        
        print( )
        
        grayLambda = 0.985 #cos(10.*pi/180.)
        print(  'CalcCFvac(gam,PCoPE)',CalcCFvac(gam,pcope),'  in gray book=',grayLambda*CalcCFvac(gam,pcope))
        print(  'CalcCstar(Gam,Temp,WtMol)',CalcCstar(gam,Temp,WtMol))
    testme()

if __name__ == "__main__":
    dev_tests()

