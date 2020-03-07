import sys
from rocketcea.cea_obj import CEA_Obj, __version__, py_cea

def main():
    
    print('Python Version:',sys.version,'  RocketCEA Version:', __version__)
    
    C = CEA_Obj( oxName='GOX', fuelName='GH2')
    Pc=40.0
    eps=5.0
    MR=5.0

    print(' Pc(psia) AreaRatio  MR  IspVac(sec)  Cstar(fps) Tc(degR)  MolWt    gamma')    
    IspVac, Cstar, Tc, MW, gamma = C.get_IvacCstrTc_ChmMwGam(Pc=Pc, MR=MR, eps=eps)
    print( '%8.1f %8.1f   %3.1f  %8.1f      %8.1f    %5.1f %8.2f  %8.4f '%\
         (Pc, eps, MR, IspVac, Cstar, Tc, MW, gamma))
    
    
    print( 'py_cea.indx.npt =',py_cea.indx.npt )
    #print( py_cea.comp.en )
    #print( py_cea.cdata.prod )
    
    massFracD = {} # index=species: value=[massfrac1, massfrac2, ...]
    molWtD    = {} # index=species: value=molecular weight
    print('   ROCKETCEA MASS FRACTIONS')
    for k,p in enumerate(py_cea.cdata.prod):
        p = p.decode("utf-8").strip()
        if p:
                
            sL = []
            mfL = []
            gt_zero = False
            for i in range(3):
                en = py_cea.comp.en[k-1,i]
                mw = py_cea.therm.mw[k-1]
                
                sL.append( '%7.5f'%(en*mw,)   )
                mfL.append( en*mw )
                if mfL[-1] >= 0.000005:
                    gt_zero = True
            
            
            if gt_zero:
                print( '%-17s'%p, '  '.join(sL), '  MW=%7.4f'%mw )
                massFracD[p] = mfL
                molWtD[p] = mw

    print("""   CEA OUTPUT: MASS FRACTIONS
 *H               0.00637  0.00531  0.00081
 HO2              0.00002  0.00001  0.00000
 *H2              0.06108  0.06063  0.06109
 H2O              0.85269  0.87387  0.93445
 *O               0.00703  0.00463  0.00005
 *OH              0.06416  0.04962  0.00352
 *O2              0.00865  0.00593  0.00008
""")
    print(molWtD)
    print(massFracD)

    moleFracD = {} # index=species: value=[molefrac1, molefrac2, ...]
    molWtD    = {} # index=species: value=molecular weight
    print('   ROCKETCEA MOLE FRACTIONS')
    for k,p in enumerate(py_cea.cdata.prod):
        p = p.decode("utf-8").strip()
        if p:
                
            sL = []
            mfL = []
            gt_zero = False
            for i in range(3):
                en = py_cea.comp.en[k-1,i]
                totn = py_cea.prtout.totn[i]
                sL.append( '%7.5f'%(en/totn,)   )
                mfL.append( en/totn )
                if mfL[-1] >= 0.000005:
                    gt_zero = True
            
            if gt_zero:
                print( '%-17s'%p, '  '.join(sL) )
                moleFracD[p] = mfL
                molWtD[p] = mw

    print("""   CEA OUTPUT MOLE FRACTIONS
 *H               0.07147  0.06044  0.00962
 HO2              0.00001  0.00000  0.00000
 *H2              0.34261  0.34471  0.36428
 H2O              0.53522  0.55597  0.62354
 *O               0.00497  0.00332  0.00004
 *OH              0.04266  0.03344  0.00249
 *O2              0.00306  0.00212  0.00003
""")
    print(molWtD)
    print(moleFracD)

if __name__=="__main__":
    
    main()
