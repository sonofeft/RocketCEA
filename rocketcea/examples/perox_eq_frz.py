
from rocketcea.cea_obj_w_units import CEA_Obj
from pylab import *

Pc = 500.0

P90 =  CEA_Obj( propName='Peroxide90', pressure_units='psia', temperature_units='degK', )

epsL = []
TceqL = []
TcfrzL = []
for ieps in range(5,21):

    TcEq = P90.get_Temperatures( Pc=Pc, eps=float(ieps) )[2]

    TcFrz = P90.get_Temperatures( Pc=Pc, eps=float(ieps), frozen=1, frozenAtThroat=0 )[2]
    
    epsL.append( float(ieps) )
    TceqL.append( TcEq )
    TcfrzL.append( TcFrz )

plot( epsL, TceqL, label='Equilibrium')
plot( epsL, TcfrzL, label='Frozen')

legend(loc='best')
grid(True)
title( 'Peroxide90 Equilibrium/Frozen Tc Comparison' )
xlabel( 'Area Ratio' )
ylabel( 'Tc (%s)'%P90.temperature_units )
savefig('perox_eq_frz.png', dpi=120)

show()
