from rocketcea.cea_obj_w_units import CEA_Obj
from rocketcea.cea_obj import add_new_propellant
import rocketcea.cea_obj_w_units
print( rocketcea.cea_obj_w_units.__file__ )

for p in range(50, 91):
    k = 5
    for k in range(5, 46):
        if p+k > 95:
            break
        j = round(100-p-k, 1)
        p1 = round((p/20)*19, 1)
        p2 = round((p / 20), 1)
        WT0 = j
        WT1 = k
        WT2 = p1
        WT3 = p2
        
        # same name runs affoul of memoized cache.
        prop_name = "Propellant"# "prop_{0}_{1}_{2}_{3}".format(WT0, WT1, WT2, WT3)
        
        card_str = """
        name pbt C 37.33 H 66.73 N 24.89 O 8.48 wt%={0}
        h,cal= 36728.486 t(k)=298.15 rho=1.3
        name a3 C 2.4 H 4.1 O 3.1 N 1.3 wt%={1}
        h,cal= -46358.545 t(k)=298.15 rho=1.38
        name an N 1.0 H 4.0 N 1.0 O 3.0 wt%={2}
        h,cal= -87332.284 t(k)=298.15 rho=1.725
        name kn K 1.0 N 1.0 O 3.0 wt%={3}
        h,cal= -118167.824 t(k)=298.15 rho=2.109
        """.format(WT0, WT1, WT2, WT3)
        add_new_propellant(prop_name, card_str)
        
        C = CEA_Obj(propName=prop_name, pressure_units='bar', cstar_units='m/s', temperature_units='K', isp_units='N-s/kg')
        tem = C.get_Tcomb(Pc=200)
        
        print( prop_name, tem )
        