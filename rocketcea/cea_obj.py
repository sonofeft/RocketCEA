#!/usr/bin/env python
# -*- coding: ascii -*-

r"""
RocketCEA wraps the NASA FORTRAN CEA code and provides some useful tools.

RocketCEA makes direct calls to the FORTRAN CEA code in "rocket" mode to calculate
Isp, Cstar, Tcham etc. and provides tools to help determine useful
mixture ratio range, optimum MR and more.

See the NASA CEA code at: `<https://www1.grc.nasa.gov/research-and-engineering/ceaweb/>`_

RocketCEA
Copyright (C) 2005-2018  Applied Python

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-----------------------

"""

import os, sys
here = os.path.abspath(os.path.dirname(__file__))
import shutil

USER_HOME_DIR = os.path.dirname( os.path.expanduser('~/') )
ROCKETCEA_DATA_DIR = os.path.join( USER_HOME_DIR, 'RocketCEA' )

from rocketcea.short_win_path import get_short_path_name
# try to solve spaces in Windows Path names by making 8.3, short path names.
if ROCKETCEA_DATA_DIR.find(' ') >= 0:
    ROCKETCEA_DATA_DIR = get_short_path_name( ROCKETCEA_DATA_DIR )

# # python >=3.8 needs to be given permission to import DLL files.
from rocketcea.find_mingw_lib import add_mingw_lib
add_mingw_lib()

# for multi-file projects see LICENSE file for authorship info
# for single file projects, insert following information
__author__ = 'Charlie Taylor'
__copyright__ = 'Copyright (c) 2018 Charlie Taylor'
__license__ = 'GPL-3'
exec( open(os.path.join( here,'_version.py' )).read() )  # creates local __version__ variable
__email__ = "cet@appliedpython.com"
__status__ = "4 - Beta" # "3 - Alpha", "4 - Beta", "5 - Production/Stable"

from rocketcea.input_cards import oxCards, fuelCards, propCards
from rocketcea.blends import newFuelBlend, newOxBlend, renamePropIfNewHfOrTrefInName
from rocketcea.blends import isAPeroxide_Blend, isAnMMH_N2H4_Blend, isMON_Ox_Blend, isFLOX_Ox_Blend, is_HYD_Ammonia_Blend
from rocketcea.blends import addPeroxideBlend, addMON_Blend, addFLOX_Blend, addMMH_N2H4_Blend, addHYD_AmmoniaBlend

try:
    import rocketcea.py_cea as py_cea
except:
    # automodule doc builds fail on ReadTheDocs w/o some error handling.
    if os.environ.get('READTHEDOCS',False):
        print('Allowing py_cea import to fail on READTHEDOCS.')
        py_cea = None
    else:
        # go ahead and raise the error.
        import rocketcea.py_cea as py_cea
    

from rocketcea.separated_Cf import ambientCf, sepNozzleCf
#
# gravitational conversion factor
GC = 32.174

_last_called = None # remember the last object to read the datafile
_NLines_Max_ever = 0 # make sure to overwrite any lines from previous calls

# hold CEA_Cache objects by propellant name
_CacheObjDict = {}
def getCacheDict():
    """Returns internal cache of previously called calculations."""
    return _CacheObjDict

def set_py_cea_line(N, line):
    '''make sure that trailing blanks are on added lines'''
    ln =  line + " "
    py_cea.setinpline(N, ln)
    #print( '"'+ln[:77]+'"' )


def add_new_card( name, card_str, propD ):
    """::

    #: Add or Replace a propellant.
    #: name = string name (e.g. oxName, fuelName or propName)
    #: card_str = a single multiline string containing CEA input card for new propellant
    #: propD = dictionary to receive new propellant (e.g. oxCards, fuelCards or propCards)
    """
    
    # if name is already in propD, empty out the memoized run cache
    # (i.e. if propellant is redefined, then old runs are not accurate)
    if name in propD:
        _CacheObjDict.clear()

    sL = card_str.split('\n')
    cardL = []
    for s in sL:
        s = s.strip()
        if s:
            cardL.append( ' ' + s + ' ' ) # make sure there are spaces around each line entry
    propD[ name ] = cardL


def add_new_fuel( name, card_str ):
    """Add a new Fuel Card"""
    add_new_card( name, card_str, fuelCards )

def add_new_oxidizer( name, card_str ):
    """Add a new Oxidizer Card"""
    add_new_card( name, card_str, oxCards )

def add_new_propellant( name, card_str ):
    """Add a new Propellant Card"""
    add_new_card( name, card_str, propCards )


_PrintCountDict = {}


class CEA_Cache(object):
    def __init__(self, maxCache=10000, propName=None):
        """Create the cache object that saves previous calculations in RAM to speed repetitive calls."""

        self.maxCache = maxCache
        self.propName = propName
        self.ispDict = {}
        self.cstarDict = {}
        self.tcDict = {}

        # keep track of size, assume faster than a len( xxDict ) call
        self.Nisp = 0
        self.Ncstar = 0
        self.Ntc = 0

    def setIsp(self, desc='', isp=0.0):
        # do not check for existence, assume usage logic handles that
        if self.Nisp < self.maxCache:
            self.Nisp += 1
            #print( 'in setIsp, desc=',desc,' isp=',isp )
            self.ispDict[desc] = isp

    def setCstar(self, desc='', cstar=0.0):
        # do not check for existence, assume usage logic handles that
        if self.Ncstar < self.maxCache:
            self.Ncstar += 1
            self.cstarDict[desc] = cstar

    def setTcK(self, desc='', tc=0.0):
        # do not check for existence, assume usage logic handles that
        if self.Ntc < self.maxCache:
            self.Ntc += 1
            self.tcDict[desc] = tc

    def getIsp(self, desc=''):
        try:
            return self.ispDict[desc]
        except:
            return None

    def getCstar(self, desc=''):
        try:
            return self.cstarDict[desc]
        except:
            return None

    def getTcK(self, desc=''):
        try:
            return self.tcDict[desc]
        except:
            return None

class CEA_Obj(object):
    """
    RocketCEA wraps the NASA FORTRAN CEA code to calculate Isp, cstar, and Tcomb
    """

    def __init__(self, propName='', oxName='', fuelName='', fac_CR=None,
        useFastLookup=0, # depricated
        makeOutput=0, make_debug_prints=False):
        """
        #: Create the base CEA wrapper object.
        #: Fast Lookup is depricated.
        #: fac_CR = Contraction Ratio of finite area combustor (None=infinite)
        #: if make_debug_prints is True, print debugging info to terminal.
        """

        self.makeOutput = makeOutput # makes "f.out"
        self.make_debug_prints = make_debug_prints
        
        if fac_CR is not None:
            self.fac_CR = float( fac_CR )
            #print('Creating CEA_Obj with finite area combustor CR = ', self.fac_CR)
        else:
            self.fac_CR = None

        oxName = renamePropIfNewHfOrTrefInName( oxCards, oxName )
        fuelName = renamePropIfNewHfOrTrefInName( fuelCards, fuelName )
        propName = renamePropIfNewHfOrTrefInName( propCards, propName )


        oxName = oxName.replace('(g)', '(G)')
        fuelName = fuelName.replace('(g)', '(G)')
        propName = propName.replace('(g)', '(G)')

        # do NOT allow "-" or "+" as part of the name
        oxName = oxName.replace('-', '_')
        fuelName = fuelName.replace('-', '_')
        propName = propName.replace('-', '_')

        oxName = oxName.replace('+', '_')
        fuelName = fuelName.replace('+', '_')
        propName = propName.replace('+', '_')

        if oxName[-3:]=='(G)':
            oxName = 'G' + oxName[:-3]
            if self.make_debug_prints:
                print('Ox name changed to',oxName)
        if fuelName[-3:]=='(G)':
            fuelName = 'G' + fuelName[:-3]
            if self.make_debug_prints:
                print('Fuel name changed to',fuelName)
        if propName[-3:]=='(G)':
            propName = 'G' + propName[:-3]
            if self.make_debug_prints:
                print('Propellant name changed to',propName)

        # may want to interpolate tables of CEA runs for speed
        if useFastLookup:
            print('WARNING... useFastLookup is no longer supported.')
        self.useFastLookup = 0

        self.readDatafileOnce = 0

        #check for propellant (mono or solid) vs. fuel and ox
        self.cea_deck = ["reac"]
        self.desc = ''
        self.useMR = 1
        self.propName = propName
        if len(propName)>0: # can be in propCards, fuelCards, or oxCards
            if propName in propCards:
                self.cea_deck.append( propCards[ propName ] )
                self.desc += ' ' + propName

            elif is_HYD_Ammonia_Blend( propName ):  #HYD40 will be caught above
                addHYD_AmmoniaBlend( propName, self.cea_deck ) # e.g. HYD30, HYD25.5
                self.desc +=  propName

            elif propName in fuelCards:
                tempList = fuelCards[ propName ]
                if type(tempList) == type(''):
                    tempList = [tempList]

                propList = []
                for p in tempList:
                    propList.append( p.replace(' fuel ',' name ' ) )

                self.cea_deck.append( propList )
                self.desc += ' ' + propName
                if self.make_debug_prints:
                    print("fuel Cards converted into prop Cards")
                    for card in self.cea_deck:
                        if type(card) == type(''):
                            print(card)
                        else:
                            for c in card:
                                print(c)
            elif propName in oxCards:
                tempList = oxCards[ propName ]
                if type(tempList) == type(''):
                    tempList = [tempList]

                propList = []
                for p in tempList:
                    propList.append( p.replace(' oxid ',' name ' ) )

                self.cea_deck.append( propList )
                self.desc += ' ' + propName
                if self.make_debug_prints:
                    print("ox Cards converted into prop Cards")
                    for card in self.cea_deck:
                        if type(card) == type(''):
                            print(card)
                        else:
                            for c in card:
                                print(c)
            else:
                print('ERROR... bad propellant name (%s) in cea_obj.py'%propName)

            self.useMR = 0

        #check for fuel
        self.fuelName = fuelName
        if len(fuelName)>0:
            if fuelName in fuelCards:
                self.cea_deck.append( fuelCards[ fuelName ] )
                self.desc +=  fuelName

            elif isAnMMH_N2H4_Blend( fuelName ):  #M20 will be caught above
                addMMH_N2H4_Blend( fuelName, self.cea_deck ) # e.g. M10, M15, M23.789
                self.desc +=  fuelName

            else:
                print('ERROR... bad fuel name (%s) in cea_obj.py'%fuelName)
                raise Exception('ERROR... bad fuel name (%s) in cea_obj.py'%fuelName)

        #check for oxidizer
        self.oxName = oxName
        if len(oxName)>0:
            if oxName in oxCards:
                self.cea_deck.append( oxCards[ oxName ] )
                self.desc = oxName + ' / ' + self.desc

            elif isAPeroxide_Blend( oxName ):
                addPeroxideBlend( oxName, self.cea_deck )
                self.desc = oxName + ' / ' + self.desc

            elif isMON_Ox_Blend( oxName ):
                addMON_Blend(oxName, self.cea_deck)
                self.desc = oxName + ' / ' + self.desc

            elif isFLOX_Ox_Blend( oxName ):
                addFLOX_Blend( oxName, self.cea_deck )
                self.desc = oxName + ' / ' + self.desc

            else:
                print('ERROR... bad oxidizer name (%s) in cea_obj.py'%oxName)
                raise Exception('ERROR... bad oxidizer name (%s) in cea_obj.py'%oxName)

        if self.fac_CR is not None:
            self.desc += ' CR=%g'%self.fac_CR

        #thisPath = py_cea.__file__
        #print('py_cea.__file__ =' + py_cea.__file__)
        #print('here            =' + here)
        #dataPath = os.path.dirname( thisPath )
        #sp = dataPath.split('\\')
        #dataPath = '/'.join( sp ) + '/ ' # be sure to leave extra space
        #self.pathPrefix = here + os.sep + ' ' # be sure to leave extra space  # dataPath
        
        self.pathPrefix = ROCKETCEA_DATA_DIR + os.sep + ' ' # be sure to leave os.sep + extra space  # dataPath
        
        if not os.path.exists( ROCKETCEA_DATA_DIR ):
            os.mkdir( ROCKETCEA_DATA_DIR, 0o777 )
            
        def maybe_copy( fname ):
            if not os.path.exists( os.path.join(ROCKETCEA_DATA_DIR, fname) ):
                if os.path.exists( os.path.join(here, fname) ):
                    shutil.copyfile( os.path.join(here, fname), os.path.join(ROCKETCEA_DATA_DIR, fname) )
        
        maybe_copy( 'f.inp' )
        maybe_copy( 'f.out' )
        maybe_copy( 'temp.dat' )
        maybe_copy( 'thermo.lib' )
        maybe_copy( 'trans.lib' )
        
        #shutil.copyfile( os.path.join(here, 'f.inp'), os.path.join(ROCKETCEA_DATA_DIR, 'f.inp') )
        #shutil.copyfile( os.path.join(here, 'f.out'), os.path.join(ROCKETCEA_DATA_DIR, 'f.out') )
        #shutil.copyfile( os.path.join(here, 'temp.dat'), os.path.join(ROCKETCEA_DATA_DIR, 'temp.dat') )
        #shutil.copyfile( os.path.join(here, 'thermo.lib'), os.path.join(ROCKETCEA_DATA_DIR, 'thermo.lib') )
        #shutil.copyfile( os.path.join(here, 'trans.lib'), os.path.join(ROCKETCEA_DATA_DIR, 'trans.lib') )
        
        #self.pathPrefix = self.pathPrefix.replace('\\','/')
        #print( "self.pathPrefix -->",self.pathPrefix )

        # make a cache object for this propellant combo if it does not already exist
        try:
            cacheObj = _CacheObjDict[ self.desc ]
        except:
            _CacheObjDict[ self.desc ] = CEA_Cache(maxCache=10000, propName=self.desc )



    def setupCards(self, Pc=100.0, MR=1.0, eps=40.0, subar=None, PcOvPe=None, frozen=0,
                   ERphi=None, ERr=None, frozenAtThroat=0, short_output=0,
                   show_transport=0, pc_units='psia', output='calories',
                   show_mass_frac=False):
        '''
        Set up card deck and call CEA FORTRAN code.::

        #: Pc = combustion end pressure (psia)
        #: eps = Nozzle Expansion Area Ratio
        #: if PcOvPe has a value, use it instead of eps to run case
        #: ERphi = Equivalence ratios in terms of fuel-to-oxidant weight ratios.
        #: ERr = Chemical equivalence ratios in terms of valences.
        #: pc_units = 'psia', 'bar', 'atm', 'mmh'(mm of mercury)
        #: frozen flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat flag, 0=frozen in chamber, 1=frozen at throat
        '''

        global _last_called, _NLines_Max_ever
        
        N = 1
        for line in self.cea_deck:
            if type(line) == type("str"):
                set_py_cea_line(N, line)
                N += 1
            else: # might be a list of strings
                for ln in line:
                    set_py_cea_line(N, ln)
                    N += 1

        set_py_cea_line(N,"   ")
        N += 1

        #if self.desc==self.oxName + ' / ' + self.fuelName:
        #    temp_prop_case = self.oxName + '_/_' + self.fuelName
        #else:
        #    temp_prop_case = self.desc
        temp_prop_case='RocketCEA'

        set_py_cea_line(N,"prob case="+temp_prop_case+",  ")
        #set_py_cea_line(N,"prob case="+self.desc+"  ")


        N += 1
        #print( "prob case="+self.desc+"  " )

        if frozen:
            if frozenAtThroat:
                if self.fac_CR is not None:
                    eqfrStr = 'frozen nfz=3 ' # nfz=3  for fac run
                else:
                    eqfrStr = 'frozen nfz=2 ' # nfz=2 is throat, nfz=1 is chamber
            else:
                eqfrStr = 'frozen nfz=1 ' # nfz=1 is chamber
        else:
            eqfrStr = 'equilibrium'

        # ======================== make input strings for single or multiple values ==================
        # i.e. get_full_cea_output may input lists for eps, PcOvPe, Pc, MR, or subar
        
        def make_inp_str( param=' supar=', val=1.0 ):
            """allow number, list or None. 
            Return empty string for error or None."""
            if val is None:
                return ''
            try:
                rtn_str = param + '%f, '%val  #e.g. " supar=%f,  "%eps
                return rtn_str
            except:
                pass
            try:
                # e.g. " supar=%s,  "% ','.join( ['%g'%v for v in eps] )
                rtn_str = param + '%s, '%','.join( ['%g'%v for v in val] )
            except:
                rtn_str = ''
                
            return rtn_str
        
        # ------------ Eps String ------------
        eps_str = make_inp_str( " supar=", eps )
        
        # ------------ Pc String -------------
        pc_str = make_inp_str( " p,%s="%pc_units, Pc )
        
        # ------------ Subsonic Area Ration String ----------
        subar_str = make_inp_str( " subar=", subar )
        
        # ------------ Pc/Pe String -------------
        pcope_str = make_inp_str( " pi/p=", PcOvPe )
        

        # ------------------------------------------------
        # set up rocket line with Pc, PC/Pe and Epsilon
        set_py_cea_line(N," rocket %s  %s"%(eqfrStr,pc_str) + subar_str + pcope_str + eps_str )

        
        # ------------ Finite Area Combustor String ------
        
        #print('In setupCards, fac_CR =', self.fac_CR)
        if self.fac_CR is not None:
            N += 1
            fac_str = ' fac ' + make_inp_str(" ac/at=", self.fac_CR )
                    
            set_py_cea_line(N, fac_str )
            
            self.i_injface = 0
            self.i_chm = 1
            self.i_thrt = 2
            self.i_exit = 3
        else:
            self.i_injface = 0
            self.i_chm = 0
            self.i_thrt = 1
            self.i_exit = 2
        

        N += 1

        if self.useMR :
            if ERphi != None:
                # use ER,phi as an input instead of MR
                # phi = Equivalence ratios in terms of fuel-to-oxidant weight ratios
                set_py_cea_line(N,"phi,eq.ratio=%f"%ERphi+"  ")
                N += 1
            elif ERr != None:
                # use ER,r as an input instead of MR
                # r = Chemical equivalence ratios in terms of valences
                set_py_cea_line(N,"r,eq.ratio=%f"%ERr+"  ")
                N += 1
            else:
                # use MR as input
                
                # ------------ Mixture Ratio String -------------
                mr_str = make_inp_str(" o/f=", MR)
                
                set_py_cea_line(N, mr_str)
                N += 1
        else:
            set_py_cea_line(N,"    ")
            N += 1

        for i_line,line in enumerate(["   ","    ","   ","end "]):
            if i_line == 2:
                line = 'output %s '%output
                if short_output:
                    line += " short "
                if show_transport:
                    line += " transport "
                if show_mass_frac:
                    line += " massf "
                    
            set_py_cea_line(N,line)
            N += 1

        # make sure to overwrite any lines from previous calls
        if N>_NLines_Max_ever:
            _NLines_Max_ever=N
        if _NLines_Max_ever>N:
            while N<_NLines_Max_ever:
                set_py_cea_line(N,"    ")
                N += 1

        # now call CEA
        myfile = "f.inp "  # be sure to leave extra space at end

        readData = 1
        if (self is _last_called): readData = 0

        try:
            if self.readDatafileOnce and (self.desc==_last_called.desc):
                readData = 0
        except:
            print("ERROR reading data file for",self.desc)

        if readData:
            _last_called = self
            self.readDatafileOnce = 1
            if self.desc in _PrintCountDict:
                _PrintCountDict[self.desc] = _PrintCountDict[self.desc] + 1
                if _PrintCountDict[self.desc] % 100 == 0:
                    print("reading cea isp data files for",self.desc,_PrintCountDict[self.desc],'times')
            else:
                #print("reading cea isp data files for",self.desc)
                _PrintCountDict[self.desc] = 1


        if self.make_debug_prints:
            if self.makeOutput:
                print('NOTICE... making an output file: f.out')
                print("          in directory:", ROCKETCEA_DATA_DIR )

        # Before calling CEA, init values to zero so bad run can be detected
        py_cea.rockt.vaci[ self.i_thrt ] =  0.0 # Vacuum Isp at throat
        py_cea.rockt.vaci[ self.i_exit ] =  0.0 # Vacuum Isp at exit
        py_cea.rockt.cstr = 0.0     # cstar
        py_cea.prtout.ttt[ self.i_chm ] = 0.0  # chamber temperature
        py_cea.rockt.app[ self.i_thrt ] = 0.0   # Pc/Pt
        py_cea.rockt.app[ self.i_exit ] = 0.0   # Pc/Pe
        py_cea.rockt.aeat[ self.i_exit ] = 0.0  # exit area/throat area
        py_cea.rockt.vmoc[ self.i_exit ]  = 0.0
        py_cea.miscr.eqrat = 0.0    # equivalence ratio

        for i in range(3):
            py_cea.rockt.sonvel[i] = 0.0
            py_cea.prtout.hsum[i] = 0.0
            py_cea.prtout.wm[i] = 0.0
            py_cea.prtout.gammas[i] = 0.0
            py_cea.prtout.vlm[i] = 0.0
            py_cea.prtout.cpr[i] = 0.0
            
            py_cea.trpts.vis[i] = 0.0    # viscosity
            py_cea.trpts.cpeql[i] = 0.0  # equilibrium specific heat
            py_cea.trpts.coneql[i] = 0.0 # equilibrium thermal conductivity
            py_cea.trpts.preql[i] = 0.0  # equilibrium prandtl number
            py_cea.trpts.cpfro[i] = 0.0  # frozen specific heat
            py_cea.trpts.confro[i] = 0.0 # frozen thermal conductivity
            py_cea.trpts.prfro[i] = 0.0  # frozen prandtl number
        
        # set species concentrations to 0.0 prior to calcs
        #for k,p in enumerate(py_cea.cdata.prod):
        #    for i in range(3):
        #        py_cea.comp.en[k-1,i] = 0.0
        #        py_cea.therm.mw[k-1] = 0.0
        #    if k>=50:
        #        break
            
            
        #print( 'calling py_cea with pathPrefix and myfile=' )
        #print( '"'+self.pathPrefix+'"',' and ', '"'+myfile+'"' )
        py_cea.py_cea(self.pathPrefix, myfile, self.makeOutput, readData)

    def get_full_cea_output(self, Pc=100.0, MR=1.0, eps=40.0, subar=None, PcOvPe=None,
                            frozen=0, frozenAtThroat=0, short_output=0, show_transport=1,
                            pc_units='psia', output='calories', show_mass_frac=False,
                            fac_CR=None):
        """
        Get the full output file created by CEA. Return as a string.::
        
        #: Pc = combustion end pressure
        #: eps = Nozzle Expansion Area Ratio
        #: pc_units = 'psia', 'bar', 'atm', 'mmh'(mm of mercury)
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        #: fac_CR = Contraction Ratio of finite area combustor, (None=infinite)
        """

        # regardless of how run was set up, change makeOutput flag True
        save_flag = self.makeOutput
        self.makeOutput = True
        
        # Allow user to override fac_CR from CEA_Obj __init__
        save_fac_CR = self.fac_CR
        if fac_CR is not None:
            self.fac_CR = fac_CR

        self.setupCards( Pc=Pc, MR=MR, eps=eps, subar=subar, PcOvPe=PcOvPe,
                         frozen=frozen, frozenAtThroat=frozenAtThroat, 
                         short_output=short_output,
                         show_transport=show_transport, pc_units=pc_units,
                         output=output, show_mass_frac=show_mass_frac)

        self.makeOutput = save_flag # restore makeOutput
        self.fac_CR = save_fac_CR   # restore fac_CR

        return open( os.path.join(ROCKETCEA_DATA_DIR,'f.out'),'r').read()


    def get_Pinj_over_Pcomb(self, Pc=100.0, MR=1.0, fac_CR=None):
        """
        Get the pressure ratio of Pinjector / Pchamber.::
        
        #: Pc = combustion end pressure (psia)
        #: fac_CR = Contraction Ratio of finite area combustor, (None=infinite)
        """
        
        # Allow user to override fac_CR from CEA_Obj __init__
        save_fac_CR = self.fac_CR
        if fac_CR is not None:
            self.fac_CR = fac_CR
            
        if self.fac_CR is None:
            print('ERROR in get_Pinj_over_Pcomb... Need value for fac_CR')
            raise Exception('ERROR in get_Pinj_over_Pcomb... Need value for fac_CR')

        self.setupCards( Pc=Pc, MR=MR )

        self.fac_CR = save_fac_CR   # restore fac_CR
        
        Pinj_over_Pcomb = py_cea.prtout.ppp[0] / py_cea.prtout.ppp[1]

        return Pinj_over_Pcomb


    def __call__(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        """Returns IspVac(sec) if CEA_Obj is simply called like a function."""
        return self.get_Isp(Pc=Pc, MR=MR, eps=eps, 
                            frozen=frozen, frozenAtThroat=frozenAtThroat)

    def get_IvacCstrTc(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: Return the tuple (IspVac, Cstar, Tcomb)in(sec, ft/sec, degR)
        #: Pc = combustion end pressure (psia)
        #: eps = Nozzle Expansion Area Ratio
        #: MR is only used for ox/fuel combos.
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """

        cacheDesc1 = (Pc,MR,eps, frozen, frozenAtThroat)
        try:
            IspVac = _CacheObjDict[ self.desc ].getIsp( cacheDesc1 )
        except:
            IspVac = None

        # don't bother looking at Cstar and Tc if there's no Isp
        cacheDesc2 = (Pc,MR, frozen, frozenAtThroat)
        if IspVac:
            try:
                Cstar = _CacheObjDict[ self.desc ].getCstar( cacheDesc2 )
                TcK = _CacheObjDict[ self.desc ].getTcK( cacheDesc2 )
            except:
                Cstar = None
                TcK = None
            if Cstar and TcK:
                Tcomb = TcK * 1.8 # convert from Kelvin to Rankine
                return IspVac, Cstar, Tcomb

        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)

        #print( "py_cea.rockt.vaci",py_cea.rockt.vaci )
        IspVac = py_cea.rockt.vaci[ self.i_exit ]
        #print('py_cea.rockt.cstr',py_cea.rockt.cstr)
        #print( 'py_cea.rockt.app', py_cea.rockt.app )
        Cstar = float(py_cea.rockt.cstr)
        
        #print( "py_cea.prtout.ttt",py_cea.prtout.ttt )
        TcK = py_cea.prtout.ttt[ self.i_chm ]
        Tcomb = TcK * 1.8  # convert from Kelvin to Rankine
        _CacheObjDict[ self.desc ].setIsp( cacheDesc1, IspVac )
        _CacheObjDict[ self.desc ].setCstar( cacheDesc2, Cstar )
        _CacheObjDict[ self.desc ].setTcK( cacheDesc2, TcK )
        return IspVac, Cstar, Tcomb

    def getFrozen_IvacCstrTc(self, Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0):
        """::

        #: Return the tuple (IspFrozen, Cstar, Tcomb)in(sec, ft/sec, degR)
        #: Pc = combustion end pressure (psia)
        #: eps = Nozzle Expansion Area Ratio
        #: MR is only used for ox/fuel combos.
        #: frozenAtThroat flag, 0=frozen in chamber, 1=frozen at throat
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=1, frozenAtThroat=frozenAtThroat)
        IspFrozen = py_cea.rockt.vaci[ self.i_exit ]
        Cstar = float(py_cea.rockt.cstr)
        TcK = py_cea.prtout.ttt[ self.i_chm ]
        Tcomb = TcK * 1.8  # convert from Kelvin to Rankine
        return IspFrozen, Cstar, Tcomb

    def get_IvacCstrTc_exitMwGam(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: return the tuple (IspVac, Cstar, Tcomb, mw, gam)in(sec, ft/sec, degR, lbm/lbmole, -)
        #: Pc = combustion end pressure (psia)
        #: eps = Nozzle Expansion Area Ratio
        #: mw and gam apply to nozzle exit.
        #: MR is only used for ox/fuel combos.
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)

        #print( "py_cea.rockt.vaci",py_cea.rockt.vaci )
        IspVac = py_cea.rockt.vaci[ self.i_exit ]
        Cstar = float(py_cea.rockt.cstr)
        Tcomb = py_cea.prtout.ttt[ self.i_chm ] * 1.8  # convert from Kelvin to Rankine
        mw,gam = py_cea.prtout.wm[ self.i_exit ], py_cea.prtout.gammas[ self.i_exit ]

        return IspVac, Cstar, Tcomb, mw, gam


    def get_IvacCstrTc_ChmMwGam(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (IspVac, Cstar, Tcomb, mw, gam)in(sec, ft/sec, degR, lbm/lbmole, -)
        #: Pc = combustion end pressure (psia)
        #: eps = Nozzle Expansion Area Ratio
        #: mw and gam apply to chamber.
        #: MR is only used for ox/fuel combos.
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        #print( "py_cea.rockt.vaci",py_cea.rockt.vaci )
        IspVac = py_cea.rockt.vaci[ self.i_exit ]
        Cstar = float(py_cea.rockt.cstr)
        Tcomb = py_cea.prtout.ttt[ self.i_chm ] * 1.8  # convert from Kelvin to Rankine
        mw,gam = py_cea.prtout.wm[ self.i_chm ], py_cea.prtout.gammas[ self.i_chm ]

        return IspVac, Cstar, Tcomb, mw, gam

    def get_IvacCstrTc_ThtMwGam(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (IspVac, Cstar, Tcomb, mw, gam)in(sec, ft/sec, degR, lbm/lbmole, -)
        #: Pc = combustion end pressure (psia)
        #: eps = Nozzle Expansion Area Ratio
        #: mw and gam apply to throat.
        #: MR is only used for ox/fuel combos.
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        #print( "py_cea.rockt.vaci",py_cea.rockt.vaci )
        IspVac = py_cea.rockt.vaci[ self.i_exit ]
        Cstar = float(py_cea.rockt.cstr)
        Tcomb = py_cea.prtout.ttt[ self.i_chm ] * 1.8  # convert from Kelvin to Rankine
        mw,gam = py_cea.prtout.wm[ self.i_thrt ], py_cea.prtout.gammas[ self.i_thrt ]

        return IspVac, Cstar, Tcomb, mw, gam

    def get_Isp(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: return IspVac (sec)
        #: Pc = combustion end pressure (psia)
        #: eps = Nozzle Expansion Area Ratio
        #: MR is only used for ox/fuel combos.
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        cacheDesc1 = (Pc,MR,eps, frozen, frozenAtThroat)
        try:
            IspVac = _CacheObjDict[ self.desc ].getIsp( cacheDesc1 )
        except:
            IspVac = None
        if IspVac:
            return IspVac

        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)

        IspVac = py_cea.rockt.vaci[ self.i_exit ]
        _CacheObjDict[ self.desc ].setIsp( cacheDesc1, IspVac )
        #print( 'py_cea.rockt.vaci',py_cea.rockt.vaci )
        #print( 'py_cea.rockt.cstr',py_cea.rockt.cstr )
        return IspVac

    def get_Cstar(self, Pc=100.0, MR=1.0):
        """::

        #: return Cstar (ft/sec)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        """
        cacheDesc2 = (Pc,MR,0,0) # set frozen flags to zero in cache description
        try:
            Cstar = _CacheObjDict[ self.desc ].getCstar( cacheDesc2 )
        except:
            Cstar = None
        if Cstar:
            return Cstar


        self.setupCards( Pc=Pc, MR=MR, eps=2.0)
        Cstar = float(py_cea.rockt.cstr)
        _CacheObjDict[ self.desc ].setCstar( cacheDesc2, Cstar )
        return Cstar

    def get_Tcomb(self, Pc=100.0, MR=1.0):
        """::

        #: return Tcomb (degR)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        """
        cacheDesc2 = (Pc,MR,0,0) # set frozen flags to zero in cache description
        try:
            TcK = _CacheObjDict[ self.desc ].getTcK( cacheDesc2 )
            Tcomb = TcK * 1.8  # convert from Kelvin to Rankine
        except:
            TcK = None
            Tcomb = None
        if Tcomb:
            return Tcomb

        self.setupCards( Pc=Pc, MR=MR, eps=2.0)
        TcK = py_cea.prtout.ttt[ self.i_chm ]
        Tcomb = TcK * 1.8  # convert from Kelvin to Rankine
        _CacheObjDict[ self.desc ].setTcK( cacheDesc2, TcK )
        return Tcomb

    def get_PcOvPe(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: return Pc / Pexit.
        #: Pc = combustion end pressure (psia)
        #: eps = Nozzle Expansion Area Ratio
        #: MR is only used for ox/fuel combos.
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        PcOvPe = py_cea.rockt.app[ self.i_exit ]
        if self.fac_CR is not None:
            PcOvPe = PcOvPe * py_cea.rockt.app[ self.i_chm ]
        
        #print( 'py_cea.rockt.app',py_cea.rockt.app )
        #Pexit = py_cea.prtout.ppp[ self.i_exit ]*14.7/1.01325
        #return Pc/Pexit
        return PcOvPe

    def get_eps_at_PcOvPe(self, Pc=100.0, MR=1.0, PcOvPe=1000.0, frozen=0, frozenAtThroat=0):
        """::

        #: Given a Pc/Pexit, return the Area Ratio that applies.
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        self.setupCards( Pc=Pc, MR=MR, PcOvPe=PcOvPe, frozen=frozen, frozenAtThroat=frozenAtThroat)
        eps = py_cea.rockt.aeat[ self.i_exit ]
        #print( 'py_cea.rockt.aeat',py_cea.rockt.aeat )
        return eps

    def get_Throat_PcOvPe(self, Pc=100.0, MR=1.0):
        """::

        #: return Pc/Pexit at throat.
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        """
        self.setupCards( Pc=Pc, MR=MR, eps=2.0)
        PcOvPe = py_cea.rockt.app[ self.i_thrt ]
        if self.fac_CR is not None:
            PcOvPe = PcOvPe * py_cea.rockt.app[ self.i_chm ]
        return PcOvPe

    def get_MachNumber(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: return nozzle exit mach number.
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        M = py_cea.rockt.vmoc[ self.i_exit ]
        return M

    def get_Chamber_MachNumber(self, Pc=100.0, MR=1.0, fac_CR=None):
        """::

        #: Return  mach numbers at the chamber
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: fac_CR = Contraction Ratio of finite area combustor, (None=infinite)
        """
        # Allow user to override fac_CR from CEA_Obj __init__
        save_fac_CR = self.fac_CR
        if fac_CR is not None:
            self.fac_CR = fac_CR
            
        if self.fac_CR is None:
            print('ERROR in get_Chamber_MachNumber... Need value for fac_CR')
            raise Exception('ERROR in get_Chamber_MachNumber... Need value for fac_CR')
            
        self.setupCards( Pc=Pc, MR=MR )

        self.fac_CR = save_fac_CR   # restore fac_CR
        
        M = py_cea.rockt.vmoc[ 1 ]
        return M

    def get_Temperatures(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: Return a list of temperatures at the chamber, throat and exit (degR)
        #: (Note frozen flag determins whether Texit is equilibrium or Frozen temperature)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat flag, 0=frozen in chamber, 1=frozen at throat
        """
        #self.setupCards( Pc=Pc, MR=MR, eps=eps)
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        # convert from Kelvin to Rankine
        #tempList = 1.8 * py_cea.prtout.ttt[:3]
        if self.fac_CR is not None:
            tempList = list( py_cea.prtout.ttt[1:4] )
        else:
            tempList = list( py_cea.prtout.ttt[:3] )
            
        tempList = [1.8*T for T in tempList]
        return tempList # Tc, Tthroat, Texit


    def get_SonicVelocities(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: Return a list of sonic velocities at the chamber, throat and exit (ft/sec)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        # convert from m/sec into ft/sec
        if self.fac_CR is not None:
            sonicList = list(py_cea.rockt.sonvel[1:4])
        else:
            sonicList = list(py_cea.rockt.sonvel[:3])
            
        sonicList = [v * 3.28083 for v in sonicList]
        return sonicList

    def get_Chamber_SonicVel(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: Return the sonic velocity in the chamber (ft/sec)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        """
        sonicList = self.get_SonicVelocities( Pc=Pc, MR=MR, eps=eps)
        return sonicList[ 0 ] # 0 == self.i_chm here


    def get_Entropies(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: Return a list of entropies at the chamber, throat and exit CAL/(G)(K)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        if self.fac_CR is not None:
            sList =  list(py_cea.prtout.ssum[1:4])
        else:
            sList =  list(py_cea.prtout.ssum[:3])
        
        for i,s in enumerate( sList ):
            sList[i] = s * 8314.51 / 4184.0  # convert into CAL/(G)(K)
        return sList

    def get_Enthalpies(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: Return a list of enthalpies at the chamber, throat and exit (BTU/lbm)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        if self.fac_CR is not None:
            hList =  list(py_cea.prtout.hsum[1:4])
        else:
            hList =  list(py_cea.prtout.hsum[:3])
        
        for i,h in enumerate( hList ):
            hList[i] = h * 1.8 * 8314.51 / 4184.0  # convert into BTU/lbm
        return hList

    def get_SpeciesMassFractions(self, Pc=100.0, MR=1.0,eps=40.0, 
                                 frozen=0, frozenAtThroat=0, min_fraction=0.000005):
        """::

        #: Returns species mass fractions at the injector face, chamber, throat and exit.
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat flag, 0=frozen in chamber, 1=frozen at throat
        #: Returns 2 dictionaries
        #: molWtD dictionary: index=species: value=molecular weight
        #: massFracD dictionary: index=species: value=[massfrac_injface, massfrac_chm, massfrac_tht, massfrac_exit]
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        massFracD = {} # index=species: value=[massfrac_chm, massfrac_tht, massfrac_exit]
        molWtD    = {} # index=species: value=molecular weight
        
        for k in range( py_cea.indx.ngc ):
            p = py_cea.cdata.prod[k]
            p = p.decode("utf-8").strip()
            if p:
                sL = []
                mfL = []
                gt_zero = False
                #for i in range(3):
                for i in [self.i_injface, self.i_chm, self.i_thrt, self.i_exit]:
                    en = py_cea.comp.en[k-1,i]
                    mw = py_cea.therm.mw[k-1]
                    
                    mfL.append( en*mw )
                    if mfL[-1] >= min_fraction: # default = 0.000005
                        gt_zero = True
                
                if gt_zero:
                    if frozen:
                        if frozenAtThroat:
                            mfL = [ mfL[0], mfL[1], mfL[2], mfL[2] ]
                        else:
                            mfL = [ mfL[0] for _ in mfL ]
                            
                    massFracD[p] = mfL
                    molWtD[p] = mw
                    
        return molWtD, massFracD

    def get_SpeciesMoleFractions(self, Pc=100.0, MR=1.0,eps=40.0, 
                                 frozen=0, frozenAtThroat=0, min_fraction=0.000005):
        """::

        #: Returns species mole fractions at the injector face, chamber, throat and exit.
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat flag, 0=frozen in chamber, 1=frozen at throat
        #: Returns 2 dictionaries
        #: molWtD dictionary: index=species: value=molecular weight
        #: moleFracD dictionary: index=species: value=[molefrac_injface, molefrac_chm, molefrac_tht, molefrac_exit]
        """
        
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        moleFracD = {} # index=species: value=[molefrac_chm, molefrac_tht, molefrac_exit]
        molWtD    = {} # index=species: value=molecular weight
        
        for k in range( py_cea.indx.ngc ):
            p = py_cea.cdata.prod[k]
            p = p.decode("utf-8").strip()
            if p:
                sL = []
                mfL = []
                gt_zero = False
                #for i in range(3):
                for i in [self.i_injface, self.i_chm, self.i_thrt, self.i_exit]:
                    en = py_cea.comp.en[k-1,i]
                    totn = py_cea.prtout.totn[i]
                    mw = py_cea.therm.mw[k-1]
                    
                    mfL.append( en/totn )
                    if mfL[-1] >= min_fraction: # default = 0.000005
                        gt_zero = True
                
                if gt_zero:
                    if frozen:
                        if frozenAtThroat:
                            mfL = [ mfL[0], mfL[1], mfL[2], mfL[2] ]
                        else:
                            mfL = [ mfL[0] for _ in mfL ]
                    
                    moleFracD[p] = mfL
                    molWtD[p] = mw
        return molWtD, moleFracD

    def get_Chamber_H(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: Return the enthalpy in the chamber (BTU/lbm)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        """
        hList = self.get_Enthalpies( Pc=Pc, MR=MR, eps=eps)
        return hList[ 0 ] # BTU/lbm  # 0 == self.i_chm here


    def get_Densities(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: Return a list of densities at the chamber, throat and exit(lbm/cuft)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        if self.fac_CR is not None:
            dList =  list(py_cea.prtout.vlm[1:4])
        else:
            dList =  list(py_cea.prtout.vlm[:3])
        
        for i,v in enumerate( dList ):
            dList[i] = 62.42796 * 100.0 / v # convert into lbm/cuft
        return dList

    def get_Chamber_Density(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: Return the density in the chamber(lbm/cuft)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        """
        dList = self.get_Densities( Pc=Pc, MR=MR, eps=eps)
        return dList[ 0 ] # lbm/cuft  # 0 == self.i_chm here


    def get_HeatCapacities(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: Return a list of heat capacities at the chamber, throat and exit(BTU/lbm degR)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        # convert from m/sec into ft/sec
        if self.fac_CR is not None:
            cpList =  list(py_cea.prtout.cpr[1:4])
        else:
            cpList =  list(py_cea.prtout.cpr[:3])
        
        for i,cp in enumerate( cpList ):
            cpList[i] = cp * 8314.51 / 4184.0  # convert into BTU/lbm degR (aka cal/gm K)
        return cpList

    def get_Chamber_Cp(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: Return the heat capacity in the chamber(BTU/lbm degR)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen flag (0=equilibrium, 1=frozen)
        """
        cpList = self.get_HeatCapacities( Pc=Pc, MR=MR, eps=eps, frozen=frozen)
        return cpList[ 0 ] # BTU/lbm degR  # 0 == self.i_chm here

    def get_Throat_Isp(self, Pc=100.0, MR=1.0, frozen=0):
        """::

        #: Return the IspVac for the throat(sec).
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: frozen = flag (0=equilibrium, 1=frozen)
        """
        eps=1.0
        cacheDesc1 = (Pc,MR,eps,frozen,0) # set frozen flag to zero in cache description
        try:
            IspVac = _CacheObjDict[ self.desc ].getIsp( cacheDesc1 )
        except:
            IspVac = None
        if IspVac:
            return IspVac

        self.setupCards( Pc=Pc, MR=MR, eps=2.0, frozen=frozen)

        IspVac = py_cea.rockt.vaci[ self.i_thrt ]
        _CacheObjDict[ self.desc ].setIsp( cacheDesc1, IspVac )
        #print( 'py_cea.rockt.vaci',py_cea.rockt.vaci )
        #print(  'py_cea.rockt.cstr',py_cea.rockt.cstr )
        return IspVac


    def get_Chamber_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (mw, gam) for the chamber (lbm/lbmole, -)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        """
        #common /prtout/ cpr,dlvpt,dlvtp,gammas,hsum,ppp,ssum,totn,ttt,vlm,wm,pltout
        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        mw,gam = py_cea.prtout.wm[ self.i_chm ], py_cea.prtout.gammas[ self.i_chm ]
        return mw,gam

    def get_Throat_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: return the tuple (mw, gam) for the throat (lbm/lbmole, -)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen = flag (0=equilibrium, 1=frozen)
        """
        #common /prtout/ cpr,dlvpt,dlvtp,gammas,hsum,ppp,ssum,totn,ttt,vlm,wm,pltout
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen)

        mw,gam = py_cea.prtout.wm[ self.i_thrt ], py_cea.prtout.gammas[ self.i_thrt ]
        return mw,gam

    def get_exit_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: return the tuple (mw, gam) for the nozzle exit (lbm/lbmole, -)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen = flag (0=equilibrium, 1=frozen)
        #: frozenAtThroat = flag 0=frozen in chamber, 1=frozen at throat
        """
        #common /prtout/ cpr,dlvpt,dlvtp,gammas,hsum,ppp,ssum,totn,ttt,vlm,wm,pltout
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)

        mw,gam = py_cea.prtout.wm[ self.i_exit ], py_cea.prtout.gammas[ self.i_exit ]
        return mw,gam


    def get_eqratio(self, Pc=100.0, MR=1.0, eps=40.0):
        '''Returns BOTH ERr and ERphi (valence basis and mass basis respectively)'''
        #common /miscr/ a,atwt,avgdr,boltz,b0,eqrat,...
        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        ERr = py_cea.miscr.eqrat
        try:  # the logic below is lifted directly from the CEA FORTRAN source code
            tem = (py_cea.inpt.vpls[ self.i_chm ]+py_cea.inpt.vmin[ self.i_chm ])*py_cea.miscr.oxfl
            ERphi = -(py_cea.inpt.vmin[ self.i_thrt ]+py_cea.inpt.vpls[ self.i_thrt ])/tem
        except:
            ERphi = 0.0

        return float(ERr), float(ERphi)

    def getMRforER(self, ERphi=None, ERr=None):
        """::

        #: return the value of mixture ratio that applies to the input equivalence ratio.
        #: Can be ERr or ERphi (valence basis and mass basis respectively)
        """
        #common /miscr/ a,atwt,avgdr,boltz,b0,eqrat,...

        if ERphi != None:
            self.setupCards( Pc=100.0, ERphi=ERphi, eps=40.0 )
            MR = py_cea.miscr.oxfl
        elif ERr != None:
            self.setupCards( Pc=100.0, ERr=ERr, eps=40.0 )
            MR = py_cea.miscr.oxfl
        else:
            print('WARNING... ERROR in call to getMRforER.  No ER value input')
            MR = 0.0 #ERROR

        #self.setupCards( Pc=100.0, MR=1.0, eps=40.0 ) # fix any mismatches in system from ER call
        return float(MR)

    def get_description(self):
        """Return a string description of the propellant(s).  e.g. 'LOX / MMH'"""
        return str(self.desc)

    def estimate_Ambient_Isp(self, Pc=100.0, MR=1.0, eps=40.0, Pamb=14.7, 
                             frozen=0, frozenAtThroat=0):
        """::

        #: return the tuple (IspAmb, mode)
        #: Use throat gam to run ideal separation calculations.
        #: mode is a string containing, UnderExpanded, OverExpanded, or Separated
        #: Pc = combustion end pressure (psia)
        #: Pamb ambient pressure (e.g. sea level=14.7 psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen flag, 0=equilibrium, 1=frozen 
        #: frozenAtThroat flag, 0=frozen in chamber, 1=frozen at throat
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)

        IspVac = py_cea.rockt.vaci[ self.i_exit ]
        mw,gam = py_cea.prtout.wm[ self.i_thrt ], py_cea.prtout.gammas[ self.i_thrt ] # throat gamma
        #PcOvPe = py_cea.rockt.app[ self.i_exit ]
        Pexit = py_cea.prtout.ppp[ self.i_exit ]*14.7/1.01325
        #print( "=============== Pexit ", Pexit, py_cea.prtout.ppp[ self.i_exit ]/1.01325, py_cea.prtout.ppp[ self.i_exit ] )
        Cstar = float(py_cea.rockt.cstr)
        # ==================================
        
        CfOvCfvacAtEsep, CfOvCfvac, Cfsep, CfiVac, CfiAmbSimple, CfVac, epsSep, Psep = \
             sepNozzleCf(gam, eps, Pc, Pamb)
        
        if Pexit > Psep:
            # if not separated, use theoretical equation for back-pressure correction
            IspAmb = IspVac - Cstar*Pamb*eps/Pc/32.174
            CfAmb = IspAmb * 32.174 / Cstar
        else:
            # if separated, use Kalt and Badal estimate of ambient thrust coefficient
            # NOTE: there are better, more modern methods available
            IspODEepsSep,CstarODE,Tc = \
                self.get_IvacCstrTc(Pc=Pc, MR=MR, eps=epsSep)
                
            CfvacAtEsep = CfVac * IspODEepsSep / IspVac
            
            CfAmb = CfvacAtEsep * CfOvCfvacAtEsep
            IspAmb = CfAmb * Cstar / 32.174
        
        # figure out mode of nozzle operation
        if Pexit > Psep:
            if Pexit > Pamb + 0.05:
                mode = 'UnderExpanded (Pe=%g)'%Pexit
            elif Pexit < Pamb - 0.05:
                mode = 'OverExpanded (Pe=%g)'%Pexit
            else:
                mode = 'Pexit = %g'%Pexit
        else:
            mode = 'Separated (Psep=%g, epsSep=%g)'%(Psep,epsSep)

        return IspAmb, mode


    def get_PambCf(self, Pamb=14.7, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: Return the Thrust Coefficient (CF) for equilibrium chemistry and ambient pressure
        #: Pc = combustion end pressure (psia)
        #: Pamb ambient pressure (e.g. sea level=14.7 psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        """

        IspAmb, mode = self.estimate_Ambient_Isp( Pc=Pc, MR=MR, eps=eps, Pamb=Pamb)
        Cstar = float(py_cea.rockt.cstr)
        
        CFamb = GC * IspAmb / Cstar
        CFcea = GC * float(py_cea.rockt.spim[ self.i_exit ]) / Cstar
        
        return CFcea, CFamb, mode

    def getFrozen_PambCf(self, Pamb=0.0, Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0):
        """::

        #: Return the Thrust Coefficient (CF) for frozen chemistry and ambient pressure
        #: Pc = combustion end pressure (psia)
        #: Pamb ambient pressure (e.g. sea level=14.7 psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozenAtThroat flag, 0=frozen in chamber, 1=frozen at throat
        """

        IspAmb, mode = self.estimate_Ambient_Isp( Pc=Pc, MR=MR, eps=eps, Pamb=Pamb,
                                                  frozen=1, frozenAtThroat=frozenAtThroat)
        Cstar = float(py_cea.rockt.cstr)
        
        CFfrozen = GC * IspAmb / Cstar
        CFcea = GC * float(py_cea.rockt.spim[ self.i_exit ]) / Cstar
        
        return CFcea,CFfrozen, mode

    def get_Chamber_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: Return a list of heat capacity, viscosity, thermal conductivity and Prandtl number
        #: in the chamber. (units are default printout units)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio... has no effect on chamber properties
        #: frozen flag (0=equilibrium, 1=frozen)
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, show_transport=1)
        
        if frozen:
            Cp = py_cea.trpts.cpfro[ self.i_chm ]
            visc = py_cea.trpts.vis[ self.i_chm ]
            cond = py_cea.trpts.confro[ self.i_chm ]
            Prandtl = py_cea.trpts.prfro[ self.i_chm ]
        else:
            Cp = py_cea.trpts.cpeql[ self.i_chm ]
            visc = py_cea.trpts.vis[ self.i_chm ]
            cond = py_cea.trpts.coneql[ self.i_chm ]
            Prandtl = py_cea.trpts.preql[ self.i_chm ]
        
        return Cp, visc, cond, Prandtl

    def get_Throat_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: Return a list of heat capacity, viscosity, thermal conductivity and Prandtl number
        #: in the throat. (units are default printout units)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio... has no effect on throat properties
        #: frozen flag (0=equilibrium, 1=frozen)
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, show_transport=1)
        
        if frozen:
            Cp = py_cea.trpts.cpfro[ self.i_thrt ]
            visc = py_cea.trpts.vis[ self.i_thrt ]
            cond = py_cea.trpts.confro[ self.i_thrt ]
            Prandtl = py_cea.trpts.prfro[ self.i_thrt ]
        else:
            Cp = py_cea.trpts.cpeql[ self.i_thrt ]
            visc = py_cea.trpts.vis[ self.i_thrt ]
            cond = py_cea.trpts.coneql[ self.i_thrt ]
            Prandtl = py_cea.trpts.preql[ self.i_thrt ]
        
        return Cp, visc, cond, Prandtl

    def get_Exit_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: Return a list of heat capacity, viscosity, thermal conductivity and Prandtl number
        #: at the exit. (units are default printout units)
        #: Pc = combustion end pressure (psia)
        #: MR is only used for ox/fuel combos.
        #: eps = Nozzle Expansion Area Ratio
        #: frozen flag (0=equilibrium, 1=frozen)
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, show_transport=1)
        
        if frozen:
            Cp = py_cea.trpts.cpfro[ self.i_exit ]
            visc = py_cea.trpts.vis[ self.i_exit ]
            cond = py_cea.trpts.confro[ self.i_exit ]
            Prandtl = py_cea.trpts.prfro[ self.i_exit ]
        else:
            Cp = py_cea.trpts.cpeql[ self.i_exit ]
            visc = py_cea.trpts.vis[ self.i_exit ]
            cond = py_cea.trpts.coneql[ self.i_exit ]
            Prandtl = py_cea.trpts.preql[ self.i_exit ]
        
        return Cp, visc, cond, Prandtl


# ----------------------- DEBUG OUTPUT -----------------------------
def print_py_cea_vars():
    """
    Print all the interface variables to the FORTRAN pyd file.
    Normally used for debugging or verifying FORTRAN internal values.
    """
    commonL = dir(py_cea)
    for common in commonL:
        if common[:1] != '_':
            print(common)
            vL = dir( getattr(py_cea,common) )
            print(vL)
            for v in vL:
                var = getattr( getattr(py_cea,common), v)
                print(v,var)
            print()


if __name__ == '__main__':

    from pylab import *


    def showOutput( ispObj ):
        print()
        print(ispObj.desc,'   at Pc=%.1f, MR=%.3f, eps=%.2f...'%(Pc,MR,eps))
        i,c,t = ispObj.get_IvacCstrTc(Pc,MR,eps)
        print('Isp = ',i)
        print('Cstar = ',c)
        print('Tcomb = ',t)
        print('  at eps    =',eps)
        PcOvPe = ispObj.get_PcOvPe(Pc=Pc, MR=MR, eps=eps)
        print('PcOvPe = ',PcOvPe)
        epsAtPcOvPe = ispObj.get_eps_at_PcOvPe(Pc=Pc, MR=MR, PcOvPe=PcOvPe)
        print('epsAtPcOvPe=',epsAtPcOvPe)
        print('Mach Number=',ispObj.get_MachNumber(Pc=Pc, MR=MR, eps=eps))
        print()
        print('Chamber Sonic Vel =',ispObj.get_Chamber_SonicVel( Pc=Pc, MR=MR,eps=eps))
        print('Enthalpies =',ispObj.get_Enthalpies( Pc=Pc, MR=MR,eps=eps))
        print('Densities =',ispObj.get_Densities( Pc=Pc, MR=MR,eps=eps))
        print('Cp        =',ispObj.get_HeatCapacities( Pc=Pc, MR=MR,eps=eps))
        print('=======================================')

    Pc,MR,eps = 1000.0, 1.0, 30.0
    ispNew = CEA_Obj(fuelName="MMH", oxName="N2O4")
    showOutput( ispNew )
    
    #Cp, visc, cond, Pr = ispNew.get_Exit_Transport(Pc=1000.0, MR=6.0, eps=40.0, frozen=1)
    #print('Cp=%g, visc=%g, cond=%g, Pr=%g'%(Cp, visc, cond, Pr) )
    #sys.exit()

    ispNew = CEA_Obj(propName="ARC311")
    showOutput( ispNew )

    ispNew = CEA_Obj(propName="N2O")
    showOutput( ispNew )

    ispNew = CEA_Obj(oxName="LOX", fuelName="H2")
    showOutput( ispNew )

    ispNew = CEA_Obj(oxName="LOX", fuelName="GH2_160")
    showOutput( ispNew )

    print()
    print('amb_'*14)
    C = CEA_Obj(oxName="LOX", fuelName="H2")
    MR = 6.0
    for Pc in [100., 500., 1500., 5000.]:
        for eps in [2.0, 5., 10., 20., 50., 100.]:

            IspVac = C.get_Isp( Pc=Pc, MR=MR, eps=eps)
            IspAmb, mode = C.estimate_Ambient_Isp(Pc=Pc, MR=MR, eps=eps, Pamb=14.7)

            print('Pc=%4i  eps=%3i  IspAmb=%10.2f IspVac=%10.2f  Mode=%s'%(int(Pc),int(eps), IspAmb, IspVac, mode))
        print('  ---')
