#!/usr/bin/env python
# -*- coding: ascii -*-

r"""
RocketCEA wraps the NASA FORTRAN CEA code and provides some useful tools.

RocketCEA makes direct calls to the FORTRAN CEA code in "rocket" mode to calculate
Isp, Cstar, Tcham etc. and provides tools to help determine useful
mixture ratio range, optimum MR and more.

See the NASA CEA code at: `<https://www.grc.nasa.gov/WWW/CEAWeb/>`_

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

import os, sys, platform
here = os.path.abspath(os.path.dirname(__file__))


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
    

from rocketcea.separated_Cf import ambientCf
#

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

    def __init__(self, propName='', oxName='', fuelName='', useFastLookup=0,
        makeOutput=0):
        """
        #: Create the base CEA wrapper object.
        #: Fast Lookup is being reviewed.
        """

        self.makeOutput = makeOutput # makes "f.out"

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
            print('Ox name changed to',oxName)
        if fuelName[-3:]=='(G)':
            fuelName = 'G' + fuelName[:-3]
            print('Fuel name changed to',fuelName)
        if propName[-3:]=='(G)':
            propName = 'G' + propName[:-3]
            print('Propellant name changed to',propName)

        # may want to interpolate tables of CEA runs for speed
        self.useFastLookup = useFastLookup

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

        #thisPath = py_cea.__file__
        #print('py_cea.__file__ =' + py_cea.__file__)
        #print('here            =' + here)
        #dataPath = os.path.dirname( thisPath )
        #sp = dataPath.split('\\')
        #dataPath = '/'.join( sp ) + '/ ' # be sure to leave extra space
        self.pathPrefix = here + os.sep + ' ' # be sure to leave extra space  # dataPath
        #self.pathPrefix = self.pathPrefix.replace('\\','/')
        #print( "self.pathPrefix",self.pathPrefix )

        # make a cache object for this propellant combo if it does not already exist
        try:
            cacheObj = _CacheObjDict[ self.desc ]
        except:
            _CacheObjDict[ self.desc ] = CEA_Cache(maxCache=10000, propName=self.desc )

        #print( self.cea_deck )
        if self.useFastLookup:
            if self.useMR :
                self.fastModuleName = "cea_fit_" + self.oxName.replace('-','_') + '_' + self.fuelName.replace('-','_')
            else:
                self.fastModuleName = "cea_fit_" + self.propName.replace('-','_')
            fp = None
            try:
                pathList = [os.path.dirname( os.path.abspath(sys.argv[0])[:] ),
                    self.pathPrefix[:-1]]
                #print( 'pathList',pathList )
                fp, pathname, description = imp.find_module(self.fastModuleName, pathList)
                self.fastModule = imp.load_module(self.fastModuleName, fp, pathname, description)
            except:
                # Since we may exit via an exception, close fp explicitly.
                self.useFastLookup = 0
                print("WARNING... Fast Module",self.fastModuleName,"failed to load")
                print("   Will call CEA code instead (slower but more accurate)")
                print(traceback.print_exc())
                if fp:
                    fp.close()


    def setupCards(self, Pc=100.0, MR=1.0, eps=40.0, subar=None, PcOvPe=None, frozen=0,
                   ERphi=None, ERr=None, frozenAtThroat=0, short_output=0,
                   show_transport=0, pc_units='psia', output='calories'):
        '''
        Set up card deck and call CEA FORTRAN code.::

        #: if PcOvPe has a value, use it instead of eps to run case
        #: ERphi = Equivalence ratios in terms of fuel-to-oxidant weight ratios.
        #: ERr = Chemical equivalence ratios in terms of valences.
        #: pc_units = 'psia', 'bar', 'atm', 'mmh'(mm of mercury)
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

        if self.desc==self.oxName + ' / ' + self.fuelName:
            temp_prop_case = self.oxName + '_/_' + self.fuelName
        else:
            temp_prop_case = self.desc

        set_py_cea_line(N,"prob case="+temp_prop_case+"  ")
        #set_py_cea_line(N,"prob case="+self.desc+"  ")


        N += 1
        #print( "prob case="+self.desc+"  " )

        if frozen:
            if frozenAtThroat:
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


        if self.makeOutput:
            print("NOTICE... making an output file")

        # Before calling CEA, init values to zero so bad run can be detected
        py_cea.rockt.vaci[1] =  0.0 # Vacuum Isp at throat
        py_cea.rockt.vaci[2] =  0.0 # Vacuum Isp at exit
        py_cea.rockt.cstr = 0.0     # cstar
        py_cea.prtout.ttt[0] = 0.0  # chamber temperature
        py_cea.rockt.app[1] = 0.0   # Pc/Pt
        py_cea.rockt.app[2] = 0.0   # Pc/Pe
        py_cea.rockt.aeat[2] = 0.0  # exit area/throat area
        py_cea.rockt.vmoc[2]  = 0.0
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
        #print( 'calling py_cea with pathPrefix and myfile=' )
        #print( '"'+self.pathPrefix+'"',' and ', '"'+myfile+'"' )
        py_cea.py_cea(self.pathPrefix, myfile, self.makeOutput, readData)

    def get_full_cea_output(self, Pc=100.0, MR=1.0, eps=40.0, subar=None, PcOvPe=None,
                            frozen=0, frozenAtThroat=0, short_output=0, show_transport=1,
                            pc_units='psia', output='calories'):
        """
        Get the full output file created by CEA. Return as a string.
        #: pc_units = 'psia', 'bar', 'atm', 'mmh'(mm of mercury)
        """

        # regardless of how run was set up, change makeOutput flag True
        save_flag = self.makeOutput
        self.makeOutput = True

        self.setupCards( Pc=Pc, MR=MR, eps=eps, subar=subar, PcOvPe=PcOvPe,
                         frozen=frozen, frozenAtThroat=frozenAtThroat, 
                         short_output=short_output,
                         show_transport=show_transport, pc_units=pc_units,
                         output=output)

        self.makeOutput = save_flag # restore makeOutput

        return open( os.path.join(here,'f.out'),'r').read()


    def __call__(self, Pc=100.0, MR=1.0, eps=40.0):
        """Returns IspVac if CEA_Obj is simply called like a function."""
        if self.useFastLookup:
            isp =  self.fastModule.get_isp(Pc, eps, MR)
            return isp
        else:
            return self.get_Isp(Pc=Pc, MR=MR, eps=eps)

    def get_IvacCstrTc(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: Return the tuple (IspVac, Cstar, Tcomb).
        #: MR is only used for ox/fuel combos.
        """
        if self.useFastLookup:
            isp,cstr,tc = self.fastModule.get_ivaccstrtc(Pc, eps, MR)
            return isp,cstr,tc
        else:

            #cacheDesc1 = '%g|%g|%g'%(Pc,MR,eps)  # %g only shows 6 sig digits
            cacheDesc1 = (Pc,MR,eps)
            try:
                IspVac = _CacheObjDict[ self.desc ].getIsp( cacheDesc1 )
            except:
                IspVac = None

            # don't bother looking at Cstar and Tc if there's no Isp
            #cacheDesc2 = '%g|%g'%(Pc,MR)  # %g only shows 6 sig digits
            cacheDesc2 = (Pc,MR)
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

            self.setupCards( Pc=Pc, MR=MR, eps=eps)

            #print( "py_cea.rockt.vaci",py_cea.rockt.vaci )
            IspVac = py_cea.rockt.vaci[2]
            Cstar = float(py_cea.rockt.cstr)
            TcK = py_cea.prtout.ttt[0]
            Tcomb = TcK * 1.8  # convert from Kelvin to Rankine
            _CacheObjDict[ self.desc ].setIsp( cacheDesc1, IspVac )
            _CacheObjDict[ self.desc ].setCstar( cacheDesc2, Cstar )
            _CacheObjDict[ self.desc ].setTcK( cacheDesc2, TcK )
            return IspVac, Cstar, Tcomb

    def getFrozen_IvacCstrTc(self, Pc=100.0, MR=1.0, eps=40.0, frozenAtThroat=0):
        """::

        #: Return the tuple (IspFrozen, Cstar, Tcomb).
        #: MR is only used for ox/fuel combos.
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=1, frozenAtThroat=frozenAtThroat)
        IspFrozen = py_cea.rockt.vaci[2]
        Cstar = float(py_cea.rockt.cstr)
        TcK = py_cea.prtout.ttt[0]
        Tcomb = TcK * 1.8  # convert from Kelvin to Rankine
        return IspFrozen, Cstar, Tcomb

    def get_IvacCstrTc_exitMwGam(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (IspVac, Cstar, Tcomb, mw, gam)
        #: mw and gam apply to nozzle exit.
        #: MR is only used for ox/fuel combos.
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        #print( "py_cea.rockt.vaci",py_cea.rockt.vaci )
        IspVac = py_cea.rockt.vaci[2]
        Cstar = float(py_cea.rockt.cstr)
        Tcomb = py_cea.prtout.ttt[0] * 1.8  # convert from Kelvin to Rankine
        mw,gam = py_cea.prtout.wm[2], py_cea.prtout.gammas[2]

        return IspVac, Cstar, Tcomb, mw, gam


    def get_IvacCstrTc_ChmMwGam(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (IspVac, Cstar, Tcomb, mw, gam)
        #: mw and gam apply to chamber.
        #: MR is only used for ox/fuel combos.
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        #print( "py_cea.rockt.vaci",py_cea.rockt.vaci )
        IspVac = py_cea.rockt.vaci[2]
        Cstar = float(py_cea.rockt.cstr)
        Tcomb = py_cea.prtout.ttt[0] * 1.8  # convert from Kelvin to Rankine
        mw,gam = py_cea.prtout.wm[0], py_cea.prtout.gammas[0]

        return IspVac, Cstar, Tcomb, mw, gam

    def get_IvacCstrTc_ThtMwGam(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (IspVac, Cstar, Tcomb, mw, gam)
        #: mw and gam apply to throat.
        #: MR is only used for ox/fuel combos.
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        #print( "py_cea.rockt.vaci",py_cea.rockt.vaci )
        IspVac = py_cea.rockt.vaci[2]
        Cstar = float(py_cea.rockt.cstr)
        Tcomb = py_cea.prtout.ttt[0] * 1.8  # convert from Kelvin to Rankine
        mw,gam = py_cea.prtout.wm[1], py_cea.prtout.gammas[1]

        return IspVac, Cstar, Tcomb, mw, gam

    def get_Isp(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return IspVac.
        #: MR is only used for ox/fuel combos.
        """
        if self.useFastLookup:
            isp =  self.fastModule.get_isp(Pc, eps, MR)
            return isp
        else:
            #cacheDesc1 = '%g|%g|%g'%(Pc,MR,eps) # %g only shows 6 sig digits
            cacheDesc1 = (Pc,MR,eps)
            try:
                IspVac = _CacheObjDict[ self.desc ].getIsp( cacheDesc1 )
            except:
                IspVac = None
            if IspVac:
                return IspVac

            self.setupCards( Pc=Pc, MR=MR, eps=eps)

            IspVac = py_cea.rockt.vaci[2]
            _CacheObjDict[ self.desc ].setIsp( cacheDesc1, IspVac )
            #print( 'py_cea.rockt.vaci',py_cea.rockt.vaci )
            #print( 'py_cea.rockt.cstr',py_cea.rockt.cstr )
            return IspVac

    def get_Cstar(self, Pc=100.0, MR=1.0):
        """::

        #: return Cstar.
        #: MR is only used for ox/fuel combos.
        """
        if self.useFastLookup:
            Cstar =  self.fastModule.get_cstar(Pc, MR)
            return Cstar
        else:
            #cacheDesc2 = '%g|%g'%(Pc,MR) # %g only shows 6 sig digits
            cacheDesc2 = (Pc,MR)
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

        #: return Tcomb.
        #: MR is only used for ox/fuel combos.
        """
        if self.useFastLookup:
            tc =  self.fastModule.get_tcomb(Pc, MR)
            return tc
        else:
            #cacheDesc2 = '%g|%g'%(Pc,MR) # %g only shows 6 sig digits
            cacheDesc2 = (Pc,MR)
            try:
                TcK = _CacheObjDict[ self.desc ].getTcK( cacheDesc2 )
                Tcomb = TcK * 1.8  # convert from Kelvin to Rankine
            except:
                TcK = None
                Tcomb = None
            if Tcomb:
                return Tcomb

            self.setupCards( Pc=Pc, MR=MR, eps=2.0)
            TcK = py_cea.prtout.ttt[0]
            Tcomb = TcK * 1.8  # convert from Kelvin to Rankine
            _CacheObjDict[ self.desc ].setTcK( cacheDesc2, TcK )
            return Tcomb

    def get_PcOvPe(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return Pc / Pexit.
        #: MR is only used for ox/fuel combos.
        """
        if self.useFastLookup:
            PcOvPe =  self.fastModule.get_pcovpe(Pc, eps, MR)
            return PcOvPe
        else:
            self.setupCards( Pc=Pc, MR=MR, eps=eps)
            PcOvPe = py_cea.rockt.app[2]
            #print( 'py_cea.rockt.app',py_cea.rockt.app )
            return PcOvPe

    def get_eps_at_PcOvPe(self, Pc=100.0, MR=1.0, PcOvPe=1000.0):
        """::

        #: Given a Pc/Pexit, return the Area Ratio that applies.
        #: MR is only used for ox/fuel combos.
        """
        self.setupCards( Pc=Pc, MR=MR, PcOvPe=PcOvPe)
        eps = py_cea.rockt.aeat[2]
        #print( 'py_cea.rockt.aeat',py_cea.rockt.aeat )
        return eps

    def get_Throat_PcOvPe(self, Pc=100.0, MR=1.0):
        """::

        #: return Pc/Pexit at throat.
        #: MR is only used for ox/fuel combos.
        """
        self.setupCards( Pc=Pc, MR=MR, eps=2.0)
        PcOvPe = py_cea.rockt.app[1]
        return PcOvPe

    def get_MachNumber(self, Pc=100.0, MR=1.0,eps=40.0):
        """::

        #: return nozzle exit mach number.
        #: MR is only used for ox/fuel combos.
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps)
        M = py_cea.rockt.vmoc[2]
        return M

    def get_Temperatures(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0, frozenAtThroat=0):
        """::

        #: Return a list of temperatures at the chamber, throat and exit.
        #: (Note frozen flag determins whether Texit is equilibrium or Frozen temperature)
        #: MR is only used for ox/fuel combos.
        """
        #self.setupCards( Pc=Pc, MR=MR, eps=eps)
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen, frozenAtThroat=frozenAtThroat)
        
        # convert from Kelvin to Rankine
        tempList = 1.8 * py_cea.prtout.ttt[:3]
        return tempList # Tc, Tthroat, Texit


    def get_SonicVelocities(self, Pc=100.0, MR=1.0,eps=40.0):
        """::

        #: Return a list of sonic velocities at the chamber, throat and exit.
        #: MR is only used for ox/fuel combos.
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps)
        # convert from m/sec into ft/sec
        sonicList = 3.28083 * py_cea.rockt.sonvel[:3]
        return sonicList

    def get_Chamber_SonicVel(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: Return the sonic velocity in the chamber.
        #: MR is only used for ox/fuel combos.
        """
        sonicList = self.get_SonicVelocities( Pc=Pc, MR=MR, eps=eps)
        return sonicList[0]


    def get_Enthalpies(self, Pc=100.0, MR=1.0,eps=40.0):
        """::

        #: Return a list of enthalpies at the chamber, throat and exit.
        #: MR is only used for ox/fuel combos.
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps)
        # convert from m/sec into ft/sec
        hList =  py_cea.prtout.hsum[:3]
        for i,h in enumerate( hList ):
            hList[i] = h * 1.8 * 8314.51 / 4184.0  # convert into BTU/lbm
        return hList

    def get_Chamber_H(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: Return the enthalpy in the chamber.
        #: MR is only used for ox/fuel combos.
        """
        hList = self.get_Enthalpies( Pc=Pc, MR=MR, eps=eps)
        return hList[0] # BTU/lbm


    def get_Densities(self, Pc=100.0, MR=1.0,eps=40.0):
        """::

        #: Return a list of densities at the chamber, throat and exit.
        #: MR is only used for ox/fuel combos.
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps)
        # convert from m/sec into ft/sec
        dList =  py_cea.prtout.vlm[:3]
        for i,v in enumerate( dList ):
            dList[i] = 62.42796 * 100.0 / v # convert into lbm/cuft
        return dList

    def get_Chamber_Density(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: Return the density in the chamber.
        #: MR is only used for ox/fuel combos.
        """
        dList = self.get_Densities( Pc=Pc, MR=MR, eps=eps)
        return dList[0] # lbm/cuft


    def get_HeatCapacities(self, Pc=100.0, MR=1.0,eps=40.0, frozen=0):
        """::

        #: Return a list of heat capacities at the chamber, throat and exit.
        #: MR is only used for ox/fuel combos.
        #: frozen flag (0=equilibrium, 1=frozen)
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, frozen=frozen)
        # convert from m/sec into ft/sec
        cpList =  py_cea.prtout.cpr[:3]
        for i,cp in enumerate( cpList ):
            cpList[i] = cp * 8314.51 / 4184.0  # convert into BTU/lbm degR (aka cal/gm K)
        return cpList

    def get_Chamber_Cp(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: Return the heat capacity in the chamber.
        #: MR is only used for ox/fuel combos.
        """
        cpList = self.get_HeatCapacities( Pc=Pc, MR=MR, eps=eps, frozen=frozen)
        return cpList[0] # BTU/lbm degR

    def get_Throat_Isp(self, Pc=100.0, MR=1.0):
        """::

        #: Return the IspVac for the throat.
        #: MR is only used for ox/fuel combos.
        """
        eps=1.0
        cacheDesc1 = (Pc,MR,eps)
        try:
            IspVac = _CacheObjDict[ self.desc ].getIsp( cacheDesc1 )
        except:
            IspVac = None
        if IspVac:
            return IspVac

        self.setupCards( Pc=Pc, MR=MR, eps=2.0)

        IspVac = py_cea.rockt.vaci[1]
        _CacheObjDict[ self.desc ].setIsp( cacheDesc1, IspVac )
        #print( 'py_cea.rockt.vaci',py_cea.rockt.vaci )
        #print(  'py_cea.rockt.cstr',py_cea.rockt.cstr )
        return IspVac


    def get_Chamber_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (mw, gam) for the chamber
        #: MR is only used for ox/fuel combos.
        """
        #common /prtout/ cpr,dlvpt,dlvtp,gammas,hsum,ppp,ssum,totn,ttt,vlm,wm,pltout
        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        mw,gam = py_cea.prtout.wm[0], py_cea.prtout.gammas[0]
        return mw,gam

    def get_Throat_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (mw, gam) for the throat
        #: MR is only used for ox/fuel combos.
        """
        #common /prtout/ cpr,dlvpt,dlvtp,gammas,hsum,ppp,ssum,totn,ttt,vlm,wm,pltout
        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        mw,gam = py_cea.prtout.wm[1], py_cea.prtout.gammas[1]
        return mw,gam

    def get_exit_MolWt_gamma(self, Pc=100.0, MR=1.0, eps=40.0):
        """::

        #: return the tuple (mw, gam) for the nozzle exit
        #: MR is only used for ox/fuel combos.
        """
        #common /prtout/ cpr,dlvpt,dlvtp,gammas,hsum,ppp,ssum,totn,ttt,vlm,wm,pltout
        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        mw,gam = py_cea.prtout.wm[2], py_cea.prtout.gammas[2]
        return mw,gam


    def get_eqratio(self, Pc=100.0, MR=1.0, eps=40.0):
        '''Returns BOTH ERr and ERphi (valence basis and mass basis respectively)'''
        #common /miscr/ a,atwt,avgdr,boltz,b0,eqrat,...
        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        ERr = py_cea.miscr.eqrat
        try:  # the logic below is lifted directly from the CEA FORTRAN source code
            tem = (py_cea.inpt.vpls[0]+py_cea.inpt.vmin[0])*py_cea.miscr.oxfl
            ERphi = -(py_cea.inpt.vmin[1]+py_cea.inpt.vpls[1])/tem
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

    def estimate_Ambient_Isp(self, Pc=100.0, MR=1.0, eps=40.0, Pamb=14.7):
        """::

        #: return the tuple (IspAmb, mode)
        #: Use throat gam to run ideal separation calculations.
        #: mode is a string containing, UnderExpanded, OverExpanded, or Separated
        #: MR is only used for ox/fuel combos.
        """

        self.setupCards( Pc=Pc, MR=MR, eps=eps)

        IspVac = py_cea.rockt.vaci[2]
        mw,gam = py_cea.prtout.wm[1], py_cea.prtout.gammas[1] # throat gamma

        Cf, CfOverCfvac, mode = ambientCf(gam=gam, epsTot=eps, Pc=Pc, Pamb=Pamb)

        IspAmb = IspVac * CfOverCfvac

        return IspAmb, mode


    def get_Chamber_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: Return a list of heat capacity, viscosity, thermal conductivity and Prandtl number
        #: in the chamber. (units are default printout units)
        #: MR is only used for ox/fuel combos.
        #: eps has no effect on chamber properties
        #: frozen flag (0=equilibrium, 1=frozen)
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, show_transport=1)
        
        if frozen:
            Cp = py_cea.trpts.cpfro[0]
            visc = py_cea.trpts.vis[0]
            cond = py_cea.trpts.confro[0]
            Prandtl = py_cea.trpts.prfro[0]
        else:
            Cp = py_cea.trpts.cpeql[0]
            visc = py_cea.trpts.vis[0]
            cond = py_cea.trpts.coneql[0]
            Prandtl = py_cea.trpts.preql[0]
        
        return Cp, visc, cond, Prandtl

    def get_Throat_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: Return a list of heat capacity, viscosity, thermal conductivity and Prandtl number
        #: in the throat. (units are default printout units)
        #: MR is only used for ox/fuel combos.
        #: eps has no effect on throat properties
        #: frozen flag (0=equilibrium, 1=frozen)
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, show_transport=1)
        
        if frozen:
            Cp = py_cea.trpts.cpfro[1]
            visc = py_cea.trpts.vis[1]
            cond = py_cea.trpts.confro[1]
            Prandtl = py_cea.trpts.prfro[1]
        else:
            Cp = py_cea.trpts.cpeql[1]
            visc = py_cea.trpts.vis[1]
            cond = py_cea.trpts.coneql[1]
            Prandtl = py_cea.trpts.preql[1]
        
        return Cp, visc, cond, Prandtl

    def get_Exit_Transport(self, Pc=100.0, MR=1.0, eps=40.0, frozen=0):
        """::

        #: Return a list of heat capacity, viscosity, thermal conductivity and Prandtl number
        #: at the exit. (units are default printout units)
        #: MR is only used for ox/fuel combos.
        #: eps is nozzle area ratio
        #: frozen flag (0=equilibrium, 1=frozen)
        """
        self.setupCards( Pc=Pc, MR=MR, eps=eps, show_transport=1)
        
        if frozen:
            Cp = py_cea.trpts.cpfro[2]
            visc = py_cea.trpts.vis[2]
            cond = py_cea.trpts.confro[2]
            Prandtl = py_cea.trpts.prfro[2]
        else:
            Cp = py_cea.trpts.cpeql[2]
            visc = py_cea.trpts.vis[2]
            cond = py_cea.trpts.coneql[2]
            Prandtl = py_cea.trpts.preql[2]
        
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

    if 0:
        #ispNew = CEA_Obj(oxName="O2(g)", fuelName="Ethanol",  useFastLookup=1)
        ispNew = CEA_Obj(oxName="GOX", fuelName="GCH4",  useFastLookup=0)
        Pc = 1000.0
        mr = 1.05
        i,c,t = ispNew.get_IvacCstrTc(Pc,mr,25.0)
        print('=========================================')
        print('for ',ispNew.desc)
        print('MR = ',mr)
        print('Isp = ',i)
        print('Cstar = ',c)
        print('Tcomb = ',t)
        print('=========================================')

        for e in [ 50., 30.0, 20.0, 10.0]:
            ispArr = []
            MR = 0.5
            mrArr = []
            while MR < 6.0:
                ispArr.append( ispNew(Pc, MR, e ))
                mrArr.append(MR)
                MR += 0.1
            plot(mrArr, ispArr, label='eps %.1f'%e, linewidth=4)

        legend(loc='best')
        grid(True)
        title( ispNew.desc )
        xlabel( 'Mixture Ratio' )
        ylabel( 'Isp (sec)' )
        if not sys.argv[-1]=='suppress_show':
            show()

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

    ispNew = CEA_Obj(propName="ARC311")
    showOutput( ispNew )

    ispNew = CEA_Obj(propName="N2O")
    showOutput( ispNew )

    ispNew = CEA_Obj(oxName="LOX", fuelName="H2",  useFastLookup=0)
    showOutput( ispNew )

    ispNew = CEA_Obj(oxName="LOX", fuelName="GH2_160",  useFastLookup=0)
    showOutput( ispNew )

    print()
    print('amb_'*14)
    C = CEA_Obj(oxName="LOX", fuelName="H2",  useFastLookup=0)
    MR = 6.0
    for Pc in [100., 500., 1500., 5000.]:
        for eps in [2.0, 5., 10., 20., 50., 100.]:

            IspVac = C.get_Isp( Pc=Pc, MR=MR, eps=eps)
            IspAmb, mode = C.estimate_Ambient_Isp(Pc=Pc, MR=MR, eps=eps, Pamb=14.7)

            print('Pc=%4i  eps=%3i  IspAmb=%10.2f IspVac=%10.2f  Mode=%s'%(int(Pc),int(eps), IspAmb, IspVac, mode))
        print('  ---')
