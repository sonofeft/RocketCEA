from rocketcea.cea_obj import CEA_Obj

C40 =  CEA_Obj( propName='HYD40')
C100 = CEA_Obj( propName='N2H4')

I40,  C40,  T40 =   C40.get_IvacCstrTc(Pc=200.0, eps=20.0)
I100, C100, T100 = C100.get_IvacCstrTc(Pc=200.0, eps=20.0)

print('       Isp     Cstar     Tc')
print('      (sec)   (ft/sec)  (degR)')
print('40%%    %5.1f   %6.1f   %6.1f'%(I40, C40, T40) )
print('100%%   %5.1f   %6.1f   %6.1f'%(I100, C100, T100) )
