from __future__ import print_function
#!/usr/bin/env python
# -*- coding: ascii -*-

"""
This file wraps default RockeCEA units with desired user units.
"""
import sys
from future.utils import raise_

# build a dictionary of conversion factors
unitsConvFactD = {} # index=default units: value=default dictionary of conversion factor data.
#      [default dictionaries index=new units: value=(multiplier, offset)]

# ----------- set up master dictionary for conversion factors --------------
def add_default_unit( name='psia', akaL=None ):
    """Initialize unitsConvFactD for various default units."""
    unitsConvFactD[ name ] = {}
    unitsConvFactD[ name ][ name ] = (1.0, 0)
    # allow user to call them out with all upper or lower case
    unitsConvFactD[ name ][ name.lower() ] = (1.0, 0)
    unitsConvFactD[ name ][ name.upper() ] = (1.0, 0)
    
    # include alternate designations for default units.
    if akaL is not None:
        for aka in akaL:
            unitsConvFactD[ name ][ aka ] = (1.0, 0)
            # allow user to call them out with all upper or lower case
            unitsConvFactD[ name ][ aka.lower() ] = (1.0, 0)
            unitsConvFactD[ name ][ aka.upper() ] = (1.0, 0)

# the following are the default units for i/o of RockeCEA
add_default_unit( 'psia' )
add_default_unit( 'sec', ['lbf sec/lbm', 'lbf-sec/lbm'] ) # refers to Isp seconds.
add_default_unit( 'degR', ['R'] )
add_default_unit( 'ft/sec', ['fps'] )
add_default_unit( 'BTU/lbm' )
add_default_unit( 'lbm/cuft' )
add_default_unit( 'BTU/lbm degR', ['BTU/lbm-degR','BTU/lbm R','BTU/lbm-R'] )
add_default_unit( 'millipoise' )
add_default_unit( 'mcal/cm-K-s', ['mcal/cm-degK-sec', 'mcal/cm-degC-s', 'millical/cm-degK-sec'] )

# ----------- function to get new units conversion factor --------------
def get_conv_factor( default_units, user_units ):
    """Get multiplier and offset for user_units."""
    try:
        D = unitsConvFactD[ default_units ]
    except:
        traceback = sys.exc_info()[2]
        raise_( ValueError, 'default units "%s" not recognized'%default_units, traceback )
        
    
    (uod, offset) = D.get(user_units, (None,None))
    if uod is None:
        traceback = sys.exc_info()[2]
        raise_( ValueError, 'units "%s" not associated with default units="%s"'%(user_units,default_units), traceback )
            
    return uod, offset # multiplier is (user_units / default_units)

# ----------- function to add new units to master dictionary --------------
def add_user_units( default_units, user_units, multiplier, offset=0 ):
    """Add new units to dictionary of default_units."""
    # get dictionary for default_units
    D = unitsConvFactD[ default_units ]
    D[ user_units ] = (multiplier, offset)
    
    # allow user to call them out with all upper or lower case
    D[ user_units.lower() ] = (multiplier, offset)
    D[ user_units.upper() ] = (multiplier, offset)
    
# ----------- add units for Pressure --------------
# ...Pressure
add_user_units('psia', 'MPa', 0.00689475729) # multiplier = user units / default units
add_user_units('psia', 'kPa', 6.89475729)
add_user_units('psia', 'Pa', 6894.75729)
add_user_units('psia', 'Bar', 0.0689475729)
add_user_units('psia', 'Atm', 0.068046)
add_user_units('psia', 'Torr', 51.7149)

# ...Temperature
add_user_units('degR', 'degK', 5.0/9.0)
add_user_units('degR', 'K', 5.0/9.0)
add_user_units('degR', 'degC', 5.0/9.0 , -273.15)
add_user_units('degR', 'C', 5.0/9.0 , -273.15)
add_user_units('degR', 'degF', 1.0 , -459.67)
add_user_units('degR', 'F', 1.0 , -459.67)

# ...Isp
add_user_units('sec', 'N sec/kg', 9.80665)
add_user_units('sec', 'N s/kg', 9.80665)
add_user_units('sec', 'N-sec/kg', 9.80665)
add_user_units('sec', 'N-s/kg', 9.80665)
add_user_units('sec', 'm/sec', 9.80665)
add_user_units('sec', 'm/s', 9.80665)
add_user_units('sec', 'km/s', 9.80665/1000.0)
add_user_units('sec', 'km/sec', 9.80665/1000.0)

# ...velocity and Cstar
add_user_units('ft/sec', 'm/sec', 0.3048)
add_user_units('ft/sec', 'm/s', 0.3048)

# ...enthalpy 
add_user_units('BTU/lbm','J/g',2.3244462314030762)
add_user_units('BTU/lbm','kJ/kg',2.3244462314030762)
add_user_units('BTU/lbm','J/kg',2324.446231403076)
add_user_units('BTU/lbm','kcal/kg', 0.5555565908154452)
add_user_units('BTU/lbm','cal/g', 0.5555565908154452)
add_user_units('BTU/lbm','ft lbf/lbm',777.7783085697629)
add_user_units('BTU/lbm','ft-lbf/lbm',777.7783085697629)

# ...heat capacity
add_user_units('BTU/lbm degR','kJ/kg degK', 4.1868)
add_user_units('BTU/lbm degR','kJ/kg K', 4.1868)
add_user_units('BTU/lbm degR','kJ/kg degC', 4.1868)
add_user_units('BTU/lbm degR','kJ/kg C', 4.1868)

add_user_units('BTU/lbm degR','J/kg degK', 4186.8)
add_user_units('BTU/lbm degR','J/kg K', 4186.8)
add_user_units('BTU/lbm degR','J/kg degC', 4186.8)
add_user_units('BTU/lbm degR','J/kg C', 4186.8)

add_user_units('BTU/lbm degR','cal/g degK', 1.0)
add_user_units('BTU/lbm degR','cal/g K', 1.0)
add_user_units('BTU/lbm degR','cal/g degC', 1.0)
add_user_units('BTU/lbm degR','cal/g C', 1.0)

add_user_units('BTU/lbm degR','kJ/kg-degK', 4.1868)
add_user_units('BTU/lbm degR','kJ/kg-K', 4.1868)
add_user_units('BTU/lbm degR','kJ/kg-degC', 4.1868)
add_user_units('BTU/lbm degR','kJ/kg-C', 4.1868)

add_user_units('BTU/lbm degR','J/kg-degK', 4186.8)
add_user_units('BTU/lbm degR','J/kg-K', 4186.8)
add_user_units('BTU/lbm degR','J/kg-degC', 4186.8)
add_user_units('BTU/lbm degR','J/kg-C', 4186.8)

add_user_units('BTU/lbm degR','cal/g-degK', 1.0)
add_user_units('BTU/lbm degR','cal/g-K', 1.0)
add_user_units('BTU/lbm degR','cal/g-degC', 1.0)
add_user_units('BTU/lbm degR','cal/g-C', 1.0)

# ... density
add_user_units('lbm/cuft', 'g/cc', 0.016018463)
add_user_units('lbm/cuft', 'sg', 0.016018463)
add_user_units('lbm/cuft', 'kg/m^3', 16.018463374)

# ... dynamic viscosity
add_user_units('millipoise', 'centipoise', 0.1)
add_user_units('millipoise', 'poise', 0.001)
add_user_units('millipoise', 'lbf-sec/sqin', 1.4503773779686e-8)
add_user_units('millipoise', 'lbf-sec/sqft', 0.0000020885434224573)
add_user_units('millipoise', 'lbm/ft-sec', 0.0000671968994813)
add_user_units('millipoise', 'lbm/in-sec', 0.0000671968994813 / 12.0)

# ... thermal conductivity
add_user_units('mcal/cm-K-s', 'BTU/hr-ft-degF', 241.747 / 1000.0 )
add_user_units('mcal/cm-K-s', 'BTU/s-in-degF', 0.005596 / 1000.0 )
add_user_units('mcal/cm-K-s', 'cal/s-cm-degC', 1.0 / 1000.0 )
add_user_units('mcal/cm-K-s', 'cal/s-m-degC', 100.0 / 1000.0 )
add_user_units('mcal/cm-K-s', 'W/cm-degC', 4.184 / 1000.0 )

class Units( object ):
    
    def __init__(self, default_units='psia', user_units='MPa', 
                 user_over_default=0.0068947572, user_offset=0):
        """
        default = string defining default units in RockeCEA
        user    = string defining desired user units
        user_over_default = conversion factor to user units from default units.
        user_offset       = additive offset from simple conversion (Temperature only)
        """
        
        self.default_units     = default_units
        self.user_units        = user_units
        self.user_over_default = user_over_default
        
        if user_offset is None:
            user_offset = 0
        self.user_offset       = user_offset # only used for temperature and psig

    def __str__(self):
        if self.user_offset:
            if self.user_offset > 0:
                s = '+ %g'%self.user_offset
            else:
                s = '- %g'%abs(self.user_offset)
            return 'User units:"%s" = %g * "%s" %s'%(self.user_units, 
                   self.user_over_default, self.default_units, s )
        else:
            return 'User units:"%s" = %g * "%s"'%(self.user_units, 
                   self.user_over_default, self.default_units)

    def __call__(self, user_value): # same as: uval_to_dval
        """Given a value in user units, return RockeCEA default value"""
        return self.uval_to_dval( user_value )

    def uval_to_dval(self, user_value):
        """Given a value in user units, return a value in RockeCEA default units."""
        return (user_value-self.user_offset) / self.user_over_default

    def dval_to_uval(self, def_value):
        """Given a value in RockeCEA default units, return value in user units."""
        return def_value * self.user_over_default + self.user_offset

    def show_dval_to_uval(self, def_value):
        """Given a value in RockeCEA default units, print conversion user units."""
        user_value = self.dval_to_uval( def_value )
        print( '%g %s = %g %s'%(def_value, self.default_units, user_value, self.user_units) )

    def show_uval_to_dval(self, user_value):
        """Given a value in user units, print conversion to RockeCEA default units."""
        def_value = self( user_value )
        print( '%g %s = %g %s'%( user_value, self.user_units, def_value, self.default_units) )

# ======================= Factory function for Units objects ==============
def get_units_obj( default_units, user_units ):
    
        uod, offset = get_conv_factor( default_units, user_units )
                
        return Units( default_units=default_units, user_units=user_units, 
                      user_over_default=uod, user_offset=offset )
    

if __name__ == "__main__":
    
    def chk_obj( U, user_val=1.0 ):
        print('*'*44)
        dv = U.uval_to_dval( user_val )
        uvcalc = U.dval_to_uval( dv )
        print( U, '\n%g %s = %g %s'%(user_val,U.user_units, dv, U.default_units),
                  '==> %g %s = %g %s'%(dv, U.default_units, uvcalc, U.user_units) )
        
    
    P = Units(default_units='psia', user_units='MPa', user_over_default=0.0068947572)
    chk_obj( P )
    
    chk_obj( Units(default_units='degR', user_units='degC', user_over_default=5.0/9.0, 
             user_offset=-273.15) )
        
    chk_obj( get_units_obj('psia', 'MPa') )
    chk_obj( get_units_obj('psia', 'Pa') )
    chk_obj( get_units_obj('psia', 'bar') )
    chk_obj( get_units_obj('psia', 'ATM') )
    
    chk_obj( get_units_obj('degR', 'K') )
    chk_obj( get_units_obj('degR', 'c') )
    chk_obj( get_units_obj('degR', 'degf') )
    
    chk_obj( get_units_obj('sec', 'N sec/kg') )
    chk_obj( get_units_obj('ft/sec', 'm/sec') )
    
    chk_obj( get_units_obj('BTU/lbm','kcal/kg') )
        
    chk_obj( get_units_obj('BTU/lbm degR','kJ/kg C') )
    chk_obj( get_units_obj('lbm/cuft', 'g/cc') )
    chk_obj( get_units_obj('lbm/cuft', 'kg/m^3') )
    chk_obj( get_units_obj('BTU/lbm','cal/g') )
    chk_obj( get_units_obj('sec', 'km/sec') )
    
    chk_obj( get_units_obj('millipoise', 'lbm/ft-sec') )
    chk_obj( get_units_obj('millipoise', 'lbm/in-sec') )
    
    chk_obj( get_units_obj('mcal/cm-K-s', 'BTU/s-in-degF') )
    chk_obj( get_units_obj('mcal/cm-K-s', 'cal/s-cm-degC') )
 
    print('='*55)
    for k,vD in unitsConvFactD.items():
        print(k, [uk for uk in vD.keys()])
        print()