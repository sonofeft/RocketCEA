import rocketcea.Isp as Isp
#print( 'imported',Isp.__file__ )

def ambientCf(gam=1.25, epsTot=20.0, Pc=200.0, Pamb=14.7):
    
    PcOvPe = Isp.CalcPCoPE(gam, epsTot)
    
    if Pamb > 0.0:
        CfOvCfvacAtEsep, CfOvCfvac, Cfsep, CfiVac, CfiAmbSimple, CfVac, epsSep, Psep = \
            sepNozzleCf(gam, epsTot, Pc, Pamb)
    else:
        CfVac = Isp.CalcCFvac(gam, PcOvPe)
        Psep = 0.0
    
    
    Pexit = Pc / PcOvPe
    
    if Pexit > Psep:
        #print(  'Not Separated, Cfsep =',Cfsep)
        #print(  'CfVac - Pamb*epsTot/Pc = ',CfVac - Pamb*epsTot/Pc)
        Cf = CfVac - Pamb*epsTot/Pc
        
        if Pexit > Pamb:
            mode = 'UnderExpanded Pe=%g'%Pexit
        else:
            mode = 'OverExpanded Pe=%g'%Pexit
    else:
        #print(  'separated, Cfsep =',Cfsep)
        #print(  'Simplified Cfsep =',CfiAmbSimple)
        Cf = Cfsep
        mode = 'Separated Psep=%g, epsSep=%.1f'%(Psep, epsSep)
    CfOverCfvac = Cf / CfVac
    
    return Cf, CfOverCfvac, mode

def sepNozzleCf(gam=1.25, epsTot=20.0, Pc=200.0, Pamb=14.7):
    
    '''Uses approach of Sherwin Kalt and David Badal in:
       "Conical Rocket Nozzle Performance under Flow Separated Conditions"
       
       This routine calculates an efficiency factor to apply to the 
       Ispvac or Cfvac of the nozzle
       
       IspAmb = CfOvCfvac * IspVac
       
       or to the vacuum performance of the nozzle at the separation point
       IspAmb = CfOvCfvacAtEsep * IspVac(at epsSep)
       '''

    PcOvPe = Isp.CalcPCoPE(gam,epsTot)
    CfVac = Isp.CalcCFvac(gam, PcOvPe)
    

    # this is Kalt&Badal correlation for Pi (incipient separation pressure)
    PiOvPa = (2./3.)*(Pc/Pamb)**(-0.2)
    Pi = PiOvPa * Pamb
    Psep = Pi

    # area ratio and Cfvac that correspond to the point of Pi
    epsSep = Isp.CalcEps(gam,Pc/Pi)
    epsSep = epsSep
    
    PcOvPesep = Isp.CalcPCoPE(gam,epsSep)
    CfiVac = Isp.CalcCFvac(gam, PcOvPesep)
    
    CfiAmbSimple = CfiVac - Pamb*epsSep/Pc

    # eps95 is the area ratio at which the internal pressure = 0.95 * Pamb
    # c = result of simultaneous solution of eps95 = epsSep + (epsSep-1)/2.4 = epsSep + (epsTot-epsSep)/1.45
    c = 2.4/1.45 
    if epsSep <= epsTot*c/(1.+c) + 1./(1.+c):
        eps95 = epsSep + (epsSep-1)/2.4
    else:
        eps95 = epsSep + (epsTot-epsSep)

    P95 = 0.95 * Pamb

    cf_integral_iTo95 = 0.55 *(Pi+P95)*(eps95-epsSep)/Pc
    cf_integral_95ToExit = (0.975*eps95 + 0.025*epsTot)*Pamb/Pc

    Cf = CfiVac + cf_integral_iTo95 - cf_integral_95ToExit
    
    CfOvCfvac = Cf / CfVac
    CfOvCfvacAtEsep = Cf / CfiVac
    return CfOvCfvacAtEsep, CfOvCfvac, Cf, CfiVac, CfiAmbSimple, CfVac, epsSep, Psep
    
def dev_tests():
    import csv

    csvfilename = 'separated_Noz.csv'
    print(  "saving data to CSV file",csvfilename)
    csvWriter = csv.writer(open(csvfilename, "w"),  dialect='excel')
    csvWriter.writerow(  ['gam','EpsTot','Pc','Pamb','IspVac_AtEpsTot','IspAmb_Separated','IspAmb_Simple',
        'Cf/Cfvac_AtEpsTot','Cf/Cfvac_AtEpsSep','Cf','CfiVac','CfiAmbSimple','CfVac','EpsSep']  )
    
    gam,epsTot,Pc,Pamb = 1.25, 25.0, 200., 14.7
    IspVac = 316.0
    
    CfOvCfvacAtEsep, CfOvCfvac, Cfsep, CfiVac, CfiAmbSimple,CfVac, epsSep, Psep = sepNozzleCf(gam=gam, epsTot=epsTot, Pc=Pc, Pamb=Pamb)
    IspAmb = IspVac * CfOvCfvac
    IspSimpleAtEsep = IspVac * CfiAmbSimple/CfVac
    
    csvWriter.writerow(  [gam,epsTot,Pc,Pamb, IspVac, IspAmb, IspSimpleAtEsep,
        CfOvCfvac,CfOvCfvacAtEsep,Cfsep,CfiVac,CfiAmbSimple,CfVac,epsSep]  )
    print(  'gam=%g, epsTot=%g, Pc=%g, Pamb=%g'%(gam,epsTot,Pc,Pamb))
    print(  'CfOvCfvacAtEsep, CfOvCfvac',CfOvCfvacAtEsep, CfOvCfvac)
    print(  'Cfsep, CfiAmbSimple', Cfsep, CfiAmbSimple)
    print(  'CfiVac CfVac',  CfiVac, CfVac)
    print(  'epsSep', epsSep)
    print( )

    Pc=250.0
    epsTot=20.45
    IspVac=321.3
    CfOvCfvacAtEsep, CfOvCfvac, Cfsep, CfiVac, CfiAmbSimple,CfVac, epsSep, Psep = sepNozzleCf(gam=gam, epsTot=epsTot, Pc=Pc, Pamb=Pamb)
    IspAmb = IspVac * CfOvCfvac
    IspSimpleAtEsep = IspVac * CfiAmbSimple/CfVac
    
    csvWriter.writerow(  [gam,epsTot,Pc,Pamb, IspVac, IspAmb, IspSimpleAtEsep,
        CfOvCfvac,CfOvCfvacAtEsep,Cfsep,CfiVac,CfiAmbSimple,CfVac,epsSep]  )
    print(  'gam=%g, epsTot=%g, Pc=%g, Pamb=%g'%(gam,epsTot,Pc,Pamb))
    print(  'CfOvCfvacAtEsep, CfOvCfvac',CfOvCfvacAtEsep, CfOvCfvac)
    print(  'Cfsep, CfiAmbSimple', Cfsep, CfiAmbSimple)
    print(  'CfiVac CfVac',  CfiVac, CfVac)
    print(  'epsSep', epsSep)
    print( )

    ambientCf(gam=1.25, epsTot=20.0, Pc=200.0, Pamb=14.7)

if __name__ == "__main__":
    dev_tests()

