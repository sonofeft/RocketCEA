
from math import *
from parasol import *
from rocketcea.cea_obj import CEA_Obj

# create system object (make sure author is correct... it's used for report)
S = ParametricSoln(subtaskName="Nozzle Area Ratio", 
    author="Charlie Taylor", taskName="System Analysis", constraintTolerance=0.001)

fuelName = 'LH2'
oxName = 'LOX'
pcRef=1488.0 
epsRef = 21.5
MR=6.0

ispODEObj = CEA_Obj(fuelName=fuelName, oxName=oxName, useFastLookup=0)

ispODEref,cstrODEref,tcODEref = ispODEObj.get_IvacCstrTc( Pc=pcRef, MR=MR, eps=epsRef)

# add design variables to the system (these variables may be used to
# optimize the system or to create plots)
# design vars have: 
#     name, value, minVal, maxVal, NSteps,  units,  description
S.addDesVars(
    ['Pc',1500,1400,3000,60,'psia','Chamber Pressure'],
    ['eps',20,10,60,60,'','Nozzle Area Ratio'],
    )


# now add any Result Variables That might be plotted
# "sysMass" is required
# result variables have: 
#    name,      units,  description
S.addResultVars(
    ['IspVac','sec','Vacuum Isp'],
    ['Pexit','psia','Nozzle Exit Pressure'],
    ['IspSL','sec','Sea Level Isp'],
    )


# the following control routine ties together the system components
#  with the system design variables
def myControlRoutine(S):
    # get current values of design variables    
    Pc,eps = S.getDesVars("Pc","eps")
    
    ispODE,cstrODE,tcODE = ispODEObj.get_IvacCstrTc( Pc=Pc, MR=MR, eps=eps)
    PcOvPe = ispODEObj.get_PcOvPe( Pc=Pc, MR=MR, eps=eps)

    IspVac = ispODE
    Pexit = Pc / PcOvPe
    
    IspSL, mode = ispODEObj.estimate_Ambient_Isp(Pc=Pc, MR=MR, eps=eps, Pamb=14.7)

    S["IspVac"] = IspVac
    S["IspSL"] = IspSL
    S["Pexit"] = Pexit
    
    if IspVac < IspSL:
        mode = mode + '<<ERROR>>'
        print('Pc=%4i  eps=%3i  IspAmb=%10.2f IspVac=%10.2f  Mode=%s'%(int(Pc),int(eps), IspSL, IspVac, mode))

# need to tell system the name of the control routine
S.setControlRoutine(myControlRoutine)

make2DPlot(S, sysParam=["IspVac","IspSL"], desVar="eps", dpi=100,
           titleStr="Vacuum and Sea Level Isp for %s"%ispODEObj.desc)

make2DParametricPlot(S, sysParam="IspVac", desVar="Pc", dpi=100,
    paramVar=["eps",10., 15., 20., 30., 40.],  linewidth=2,
    titleStr="Vacuum Isp for %s"%ispODEObj.desc)


make2DParametricPlot(S, sysParam="IspSL", desVar="Pc", dpi=100,
    paramVar=["eps",10., 15., 20., 30., 40.]  ,linewidth=2,
    titleStr="Sea Level Isp for %s"%ispODEObj.desc)


make2DParametricPlot(S, sysParam="Pexit", desVar="Pc", dpi=100,
    paramVar=["eps",10., 15., 20., 30., 40.]  ,linewidth=2)


makeCarpetPlot(S, sysParam="IspVac", 
    desVarL=[["eps",10., 15., 20., 30., 40.],["Pc",1000.,2000.,3000.]], 
    xResultVar="Pexit",
    haLabel='center', vaLabel='center', dpi=100,
    titleStr="Vacuum Isp and Nozzle Exit Pressure for %s"%ispODEObj.desc)


makeCarpetPlot(S, sysParam="IspSL", 
    desVarL=[["eps",10., 15., 20., 30., 40.],["Pc",1500.,2000.,3000.]], 
    xResultVar="Pexit",
    haLabel='center', vaLabel='center', dpi=100,
    titleStr="Sea Level Isp and Nozzle Exit Pressure for %s"%ispODEObj.desc)


makeCarpetPlot(S, sysParam="IspSL", 
    desVarL=[["eps",10., 15., 20., 30., 40.],["Pc",1500.,2000.,3000.]], 
    xResultVar="IspVac",
    haLabel='center', vaLabel='center', dpi=100,
    titleStr="Sea Level and Vacuum Isp for %s"%ispODEObj.desc)


# now save summary of system
S.saveFullSummary()

# Be sure to wrap-up any files
S.close()
