from rocketcea.cea_obj import CEA_Obj
import matplotlib.pyplot as plt

Pc = 200.0
eps = 20.0

xL = []    # save data to lists
ispL = []
cstarL = []
tcL = []

for x in range(10, 100, 5): # look at amm_dissociation from 5% to 95%
    propName = 'HYD%g'%x
    ispObj = CEA_Obj(propName=propName)
    
    xL.append( x ) # save percent amm_dissociation
    
    IspVac, Cstar, Tcomb = ispObj.get_IvacCstrTc( Pc=Pc, eps=eps)
    ispL.append( IspVac ) # save IspVac
    cstarL.append( Cstar )# save Cstar
    tcL.append( Tcomb )   # save Tcomb

fig, ax1 = plt.subplots()
ax1.plot(xL, ispL, 'b-', label='IspVac', linewidth=4)

plt.grid(True)
plt.title( 'Hydrazine Ideal Performance vs. Ammonia Dissociation\nPc=%g psia, Area Ratio=%g'%(Pc, eps) )
ax1.set_xlabel( '% Ammonia Dissociation' )
ax1.set_ylabel( 'IspVac (sec)' )

ax2 = ax1.twinx()
ax2.set_ylabel('Cstar (ft/sec) and Tc (degR)')
ax2.plot(xL, cstarL, 'g-', label='Cstar')
ax2.plot(xL, tcL,    'r-', label='Tcham')

ax1.legend(loc='center left')
ax2.legend(loc='center right')

plt.savefig('amm_dissociation.png', dpi=120)
plt.show()