from rocketcea import separated_Cf
from rocketcea import xlChart
from rocketcea.cea_obj import CEA_Obj

pc=108.0
eps=5.0
oxName= 'N2O4'
fuelName='MMH'
mr = 1.65
Nsteps = 20

ispObj = CEA_Obj(oxName=oxName, fuelName=fuelName)

IspODE, Cstar, Tcomb, mw, gam = ispObj.get_IvacCstrTc_ChmMwGam( Pc=pc, MR=mr, eps=eps)

pcArr = [100.0, 150.0, 200.0]

rs = [ [ 'Pamb','Cf/Cfvac', 'Pc','mode'] ]
for pc in pcArr:
    for i in range( Nsteps+1):
        Pamb = 14.7 * i / Nsteps
        Cf, CfOverCfvac, mode = separated_Cf.ambientCf(gam=gam, epsTot=eps, Pc=pc, Pamb=Pamb)
            
        rs.append( [ Pamb, CfOverCfvac, pc, mode] )
    rs.append(['','','',''])

xl = xlChart.xlChart()
xl.xlApp.DisplayAlerts = 0  # Allow Quick Close without Save Message

myTitle = "%s/%s Ambient Performance at Area Ratio=%.1f\n"%(oxName, fuelName, eps) +\
    "Pc Range = %g - %g psia"%(min(pcArr), max(pcArr))
xl.makeChart(rs,  
            title=myTitle,nCurves = 1,
            chartName="Performance",
            sheetName="FillData",yLabel="Cfamb/Cfvac", xLabel="Ambient Pressure (psia)")
xl.setYrange( 0.6, 1.0)