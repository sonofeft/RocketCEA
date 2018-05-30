#!/usr/bin/env python
# -*- coding: ascii -*-

"""
Handle propellant blends.  Make new input cards for various ox and fuel blends.
"""
from rocketcea.input_cards import oxCards, fuelCards, propCards

def all_in_dict( nameL, D ):
    """If all members of nameL are keys in D, then return True, otherwise False."""
    for name in nameL:
        if name not in D:
            return False
    return True

def get_propellant_name( Name=None, PcentL=None):
    '''::
    
    #: Return the name of the blend defined by "Name". (string or list of strings)
    #: Might be a defined blend such as MON25 or FLOX80.
    #: Might be in the library such as "MMH" or "CLF5"
    #: Might need to create a long name from percentages Name=["N2H4","NH3"], PcentL=[90,10]
    #:   (if new name, add card to oxCards, fuelCards or propCards)
    '''
    
    if type(Name)==list:
        
        if all_in_dict( Name, oxCards ):
            blend_name = newOxBlend(oxL=Name, oxPcentL=PcentL)
            return blend_name
            
        if all_in_dict( Name, fuelCards ):
            blend_name = newFuelBlend( fuelL=Name, fuelPcentL=PcentL)
            return blend_name
        
        raise Exception('Blend Components NOT Recognized:  Name=%s, PcentL=%s'%(str(Name), str(PcentL)))
        
        return None
    else:
        if (Name in oxCards) or (Name in fuelCards) or (Name in propCards):
            return Name
        if is_HYD_Ammonia_Blend( Name ):
            return Name
        if isAPeroxide_Blend( Name ):
            return Name
        if isAnMMH_N2H4_Blend( Name ):
            return Name
        if isMON_Ox_Blend( Name ):
            return Name
        if isFLOX_Ox_Blend( Name ):
            return Name
    
    print('WARNING... Blend Name NOT Recognized "%s"'%str(Name))
    return Name


def is_HYD_Ammonia_Blend( name ): 
    """
    check if name is for a blend of N2H4 and undissociated Ammonia (UA)
    HYD40 is in standard deck, however can make HYD30, HYD50 etc. "on-the-fly"
    """
    try:
        dissPcent = float( name[3:] ) 
    except:
        return 0 # fails to have an Frac in legal range
        
    if name.upper()[:3] == 'HYD':
        if dissPcent>=0.0 and dissPcent<=100.0:
            return 1
    return 0

def addHYD_AmmoniaBlend( name, cea_deck ):
    """
    Add new N2H4 + undissociated Ammonia (UA) Blend to the CEA_Obj.cea_deck.
    Use name like "HYD30" to represent hydrazine with 30% dissociated ammonia.
    """
    try:
        pcent = float( name[3:] )
        x = pcent / 100.0
        xUA = (4.0 * (1.0-x)) / 3.0
        xN  = (2.0 * (2.0*x + 1.0)) / 3.0
        xH  = (2.0 * 6.0 * x) / 3.0
    except:
        raise Exception('N2H4 un-dissociated Ammonia Blend NOT Recognized:  %s'%name)



    if (pcent>=0.0) and pcent<=100.0:
        cardL = [   " name = diss UA %g H %g N %g wt%%= 99.5  "%(xUA, xH, xN),
                    " h,cal=12094. t(k)=298.15 rho,g/cc = 1.004     ",
                    "  ",
                    " name = water H 2.0 O 1.0  wt%=0.5   ",
                    " h,cal=-68308.  t(k)=298.15 rho,g/cc = 0.9998   ",
                    " !	THIS IS %g%% AMMONIA DISSOCIATION "%(100.0*x,),
                    " omit NH3 "]
                    
        if pcent >= 100.0:
            cardL[0] = " name = diss H 4 N 2 wt%%= 99.5  "
            cardL.pop()
            cardL.pop()
    else:
        raise Exception('N2H4 un-dissociated Ammonia Blend NOT Recognized:  %s'%name)
        
    # put cardL into propCards dictionary
    propCards[name] = cardL
    #print(cardL)
        
    # name will be in propCards
    cea_deck.append( propCards[ name ] )
# --------------------------------------------------------------------------    
        
def isAPeroxide_Blend( name ): 
    """
    check if name is for a blend of peroxide and water.
    Peroxide98 and Peroxide90 are in standard deck, however can make Peroxide95 etc. "on-the-fly"
    """
    if name.lower()[:8]=='peroxide':
        return 1
    else:
        return 0

def addPeroxideBlend( oxName, cea_deck ):
    """
    Add new Peroxide Blend to the CEA_Obj.cea_deck.
    Use name like Peroxide95 to represent 95% Peroxide and 5% water.
    """
    try:
        h2o2Pcent =  float( oxName[8:] )
        waterPcent = 100.0 - h2o2Pcent
    except:
        raise Exception('Peroxide/Water Ox Blend NOT Recognized:  %s'%oxName)
        
    if (waterPcent>0.0) and waterPcent<100.0:
        blendName = newOxBlend(oxL=["H2O2","H2O"], oxPcentL=[h2o2Pcent, waterPcent])
    else:
        raise Exception('Peroxide/Water Ox Blend NOT Recognized:  %s'%oxName)
        
    # need to cover both naming conventions for the blend
    cardL = oxCards[ blendName ]
    oxCards[oxName] = cardL
        
        
    # blendName will be in oxCards
    cea_deck.append( oxCards[ blendName ] )
# --------------------------------------------------------------------------    

def isAnMMH_N2H4_Blend( name ): # M20 is in the standard deck
    # check for an MMH + N2H4 blend
    """
    check if name is for a blend of MMH + N2H4
    M20 is in standard deck, however can make M10, M30 etc. "on-the-fly"
    """
    
    try:
        mmhPcent = float( name[1:] ) 
    except:
        return 0 # fails to have an mmhFrac in legal range
        
    if name[0] in ['M','m']:
        if mmhPcent>0.0 and mmhPcent<100.0:
            return 1
    return 0
    

def addMMH_N2H4_Blend( fuelName, cea_deck ):
    """
    Add new MMH + N2H4 Blend to the CEA_Obj.cea_deck
    Use name like M10 to represent 10% MMH and 90% N2H4
    """
    
    mmhPcent = float( fuelName[1:] )  # e.g. M10, M15, M23.789
        
    if mmhPcent>0.0 and mmhPcent<100.0:
        blendName = newFuelBlend( fuelL=["MMH","N2H4"], fuelPcentL=[mmhPcent,100.0-mmhPcent])
    else:
        raise Exception('MMH + N2H4 Blend NOT Recognized:  %s'%fuelName)
        
    # need to cover both naming conventions for the blend
    cardL = fuelCards[ blendName ]
    fuelCards[fuelName] = cardL
        
        
    # blendName will be in fuelCards
    cea_deck.append( fuelCards[ blendName ] )
    
# --------------------------------------------------------------------------    

def isMON_Ox_Blend( name ):
    """
    check for MON oxidizer (MON1 to MON40)  48% is theoretical max
    MON15 and MON25 are in standard deck, however can make MON20, MON30 etc. "on-the-fly"
    """
    
    try:
        noPcent = float( name[3:] ) 
    except:
        return 0 # fails to have an Frac in legal range
        
    if name[:3] in ['MON','mon']:
        if noPcent>0.0 and noPcent<40.0:  # 48% is theoritical max
            return 1
    return 0

def addMON_Blend( oxName, cea_deck ):
    """
    Add new MON oxidizer Blend to the CEA_Obj.cea_deck
    MON15 and MON25 are in standard deck, however can make MON20, MON30 etc. "on-the-fly"
    """
    mwN2O3 = 76.01
    mwNO = 30.01
    
    try:
        massNO = float( oxName[3:] ) 
            
        molesNO = massNO / mwNO
        molesN2O3 = molesNO 
        massN2O3 = molesN2O3 * mwN2O3
        massN2O4 = 100.0 - massN2O3
        
    except:
        raise Exception('MON Ox Blend NOT Recognized:  %s'%oxName)
        
    if (massN2O4>0.0) and (massN2O4<100.0):
        blendName = newOxBlend(oxL=['N2O4','N2O3'], oxPcentL=[massN2O4, massN2O3])
    else:
        raise Exception('MON Ox Blend NOT Recognized:  %s'%oxName)
        
    # need to cover both naming conventions for the blend
    cardL = oxCards[ blendName ]
    oxCards[oxName] = cardL
        
        
    # blendName will be in oxCards
    cea_deck.append( oxCards[ blendName ] )
# --------------------------------------------------------------------------    
    

def isFLOX_Ox_Blend( name ): 
    """
    check for FLOX oxidizer (e.g. FLOX70 or FLOX82.5)
    No FLOX blends are in standard deck, however can make FLOX70, FLOX82.5 etc. "on-the-fly"
    """
    
    try:
        f2Pcent = float( name[4:] ) 
    except:
        return 0 # fails to have an Frac in legal range
        
    if name[:4] in ['FLOX','flox']:
        if f2Pcent>0.0 and f2Pcent<100.0:
            return 1
    return 0

def addFLOX_Blend( oxName, cea_deck ):
    # add FLOX oxidizer (e.g. FLOX70 or FLOX82.5)
    """
    Add new add FLOX oxidizer to the CEA_Obj.cea_deck
    FLOX70, for example, represents 70% F2 and 30% LOX
    """
    
    try:
        f2Pcent = float( oxName[4:] )
        o2Pcent = 100.0 - f2Pcent
        
    except:
        raise Exception('FLOX Ox Blend NOT Recognized:  %s'%oxName)
        
    if f2Pcent>0.0:
        blendName = newOxBlend(oxL=['F2','O2'], oxPcentL=[f2Pcent, o2Pcent])
    else:
        blendName = 'O2'
        
    # need to cover both naming conventions for the blend
    cardL = oxCards[ blendName ]
    oxCards[oxName] = cardL
        
        
    # blendName will be in oxCards
    cea_deck.append( oxCards[ blendName ] )
        
# --------------------------------------------------------------------------    

def tightenUpEquals( card ):
    """make sure there's no spaces around equal signs."""
    c = None
    while 1:
        c = card.replace(' =','=')
        c = c.replace('= ','=')
        if c==card:
            break
        card = c
    return c
    
    
def giveCardNewHfAndTref( card, newHfCalPerMole, newTrefDegR ):
    """Change the values of "h,cal" and "t(k)" on propellant cards.  """
    c = tightenUpEquals( card )
    cNew = ''
    spL = c.split()
    TrefDegK = newTrefDegR / 1.8
    for sp in spL:
        speL = sp.split('=')
        if len(speL)==2:
            if speL[0]=='h,cal':
                speL[1]='%.1f'%newHfCalPerMole
            if speL[0]=='t(k)':
                speL[1]='%.2f'%TrefDegK
            s = '='.join(speL)
        else:
            s = sp
        cNew += ' ' + s 
        
    return cNew + ' '

def giveCardMassPercent( card, fuelPcent ):
    """set the value of mass percentage (wt%) on propellant card."""
    c = tightenUpEquals( card )
    cNew = ''
    spL = c.split()
    for sp in spL:
        speL = sp.split('=')
        if len(speL)==2:
            if speL[0]=='wt%':
                speL[1]='%g'%fuelPcent
            s = '='.join(speL)
        else:
            s = sp
        cNew += ' ' + s 
        
    #print( 'cNew =',cNew )
    return cNew + ' '
    

def newOxBlend( oxL=None, oxPcentL=None):
    '''
    create ox blends such as MON25 given the oxidizer names and weight percentages.
    e.g. oxL=["N2O4","N2O3"], oxPcentL=[36.67,63.33]
    '''
    if 1:#try:
        newName = ''
        newCardL = []
        for i,name in enumerate(oxL):
            newName += name + '_%g'%oxPcentL[i]
            if i<len(oxL)-1:
                newName += '_'
                
            if len(oxCards[ name ])>2:
                raise Exception('ERROR... can NOT specify ox Blend for multi propellant card= ' + name)
                
            cardL = oxCards[ name ]
                
            for card in cardL:
                if float( oxPcentL[i] ) > 0.0:
                    newCardL.append( giveCardMassPercent( card, oxPcentL[i] ) )
            
            #newCardL.extend( cardL )
            
        oxCards[newName] = newCardL
        
        #print( 'new cards for newName=',newName )
        #for card in newCardL:
        #    print( card )
        #print()
            
        return newName
    else:#except:
        raise Exception('ERROR... in newOxBlend')

def newFuelBlend( fuelL=None, fuelPcentL=None):
    '''
    create fuel blends such as M20  given the fuel names and weight percentages.
    e.g. fuelL=["MMH","N2H4"], fuelPcentL=[20,80]
    '''
    if 1:#try:
        newName = ''
        newCardL = []
        for i,name in enumerate(fuelL):
            newName += name + '_%g'%fuelPcentL[i]
            if i<len(fuelL)-1:
                newName += '_'
                
            if len(fuelCards[ name ])>2:
                raise Exception('ERROR... can NOT specify Fuel Blend for multi propellant card= '+ name)
                
            cardL = fuelCards[ name ]
                
            for card in cardL:
                if float( fuelPcentL[i] ) > 0.0:
                    newCardL.append( giveCardMassPercent( card, fuelPcentL[i] ) )
            
            #newCardL.extend( cardL )
            
        fuelCards[newName] = newCardL
        
        #print( 'new cards for newName=',newName )
        #for card in newCardL:
        #    print( card )
        #print()
            
        return newName
    else:#except:
        raise Exception('ERROR... in newFuelBlend')

def newPropWithNewState( cardDict, name, newHfCalPerMole, newTrefDegR):
    '''
    Take name as it exists in fuelCards or oxCards and change Hf and Tref to be
    the input values newHfCalPerMole and newTrefDegR
    '''
    try:
        suffix =  '_%g_%g'%(newHfCalPerMole, newTrefDegR)
        suffix = suffix.replace('-','m')
        newName = name + suffix
        
        if newName not in cardDict:
            cardL = cardDict[ name ]
            if len(cardL)>2:
                raise Exception('ERROR... can NOT specify new Hf, Tref for multi propellant card: '+ name)
                #return name
            newCardL = []
            for card in cardL:
                newCardL.append( giveCardNewHfAndTref( card, newHfCalPerMole, newTrefDegR ) )
            
            cardDict[newName] = newCardL
            
            
        return newName
    except:
        
        raise Exception('ERROR... in newPropWithNewState')

def newFuelWithNewState( name, newHfCalPerMole, newTrefDegR):
    '''
    Take name as it exists in fuelCards and change Hf and Tref to be
    the input values newHfCalPerMole and newTrefDegR
    '''
    return newPropWithNewState( fuelCards, name, newHfCalPerMole, newTrefDegR)
    
def newOxWithNewState( name, newHfCalPerMole, newTrefDegR):
    '''
    Take name as it exists in oxCards and change Hf and Tref to be
    the input values newHfCalPerMole and newTrefDegR
    '''
    return newPropWithNewState( oxCards, name, newHfCalPerMole, newTrefDegR)

def turnCardsIntoTokenL( cardL ):
    """turn the card list into one long list of tokens"""
    tokenL = []
    for card in cardL:
        c = card.replace('=',' ')
        spL = c.split()
        for s in spL:
            if s:
                tokenL.append(s)
    return tokenL

def getFloatTokenFromCards( cardL, token='t(k)' ):
    """Get the float value of the desired token. (e.g. 't(k)' )"""
    tokenL = turnCardsIntoTokenL( cardL )
    #print( tokenL )
    for i,tok in enumerate(tokenL):
        if tok.lower()==token:
            try:
                return float( tokenL[i+1] )
            except:
                return None
    return None
    
def getFuelRefTempDegK( name ):
    """Get the fuel float value of the reference temperature 't(k)'"""
    try:
        cardL = fuelCards[ name ]
        return getFloatTokenFromCards( cardL, 't(k)' )
    except:
        return None
    
def getPropRefTempDegK( name ):
    """Get the propellant float value of the reference temperature 't(k)'"""
    try:
        cardL = propCards[ name ]
        return getFloatTokenFromCards( cardL, 't(k)' )
    except:
        return None
    
def getOxRefTempDegK( name ):
    """Get the oxidizer float value of the reference temperature 't(k)'"""
    try:
        cardL = oxCards[ name ]
        return getFloatTokenFromCards( cardL, 't(k)' )
    except:
        return None
    
def getFuelHfCalPerMole( name ):
    """Get the fuel float value of the reference temperature 'h,cal'"""
    try:
        cardL = fuelCards[ name ]
        return getFloatTokenFromCards( cardL, 'h,cal' )
    except:
        return None
    
def getOxHfCalPerMole( name ):
    """Get the ox float value of the reference temperature 'h,cal'"""
    try:
        cardL = oxCards[ name ]
        return getFloatTokenFromCards( cardL, 'h,cal' )
    except:
        raise Exception('Could NOT find "h,cal" in getOxHfCalPerMole for %s'%name)
    
def getPropHfCalPerMole( name ):
    """Get the propellant float value of the reference temperature 'h,cal'"""
    try:
        cardL = propCards[ name ]
        return getFloatTokenFromCards( cardL, 'h,cal' )
    except:
        return None


def makeCardForNewTemperature( ceaName='CH4', newTdegR=536.0, CpAve=0.791, MolWt=16.04 ):
    """
    Create a new propellant card that reflects a change in temperature of the propellant
    from the original reference temperature on the original card to the new input value
    of temperature, newTdegR.
    
    CpAve = BTU/lbm degR
    """
    
    if ceaName in oxCards:
        cardD = oxCards
        prop_type = 'ox'
    elif ceaName in fuelCards:
        cardD = fuelCards
        prop_type = 'fuel'
    elif ceaName in propCards:
        cardD = propCards
        prop_type = 'prop'
    else:
        raise Exception('Could NOT find ceaName=%s in makeCardForNewTemperature'%ceaName)
    
    
    # find Hf and Tref on card
    cardL = cardD[ceaName]
    #print( cardL )
    
    # make one big line and look for Hf and Tref
    line = ' '.join(cardL)
    line = line.replace('=',' ')
    sL = line.split()
    Hf, TrefR = None, None
    for i,s in enumerate(sL):
        token = s.strip()
        if token=='h,cal':
            Hf = float( sL[i+1] )
        if token=='t(k)':
            TrefR = float( sL[i+1] ) * 1.8
    
    if Hf==None or TrefR==None:
        raise Exception('Did NOT find Heat of Formation and/or Reference Temperature in makeCardForNewTemperature')
    
    #print( 'Hf=',Hf,'  TrefR=',TrefR )
    
    HrefCalPerMole = Hf
    #print( 'HrefCalPerMole=',HrefCalPerMole )
    
    dTdegR = newTdegR - TrefR
    newTrefDegR = newTdegR
    deltaH = dTdegR * CpAve
    delCALperMOLE = deltaH * MolWt / 1.8
    
    #print( 'deltaH=',deltaH,'  delCALperMOLE=',delCALperMOLE )
    
    HfNew = Hf + delCALperMOLE
    #print( 'HfNew=',HfNew )
    
    if prop_type=='fuel':
        newName = newFuelWithNewState(ceaName, HfNew, newTdegR)
    elif prop_type=='ox':
        newName = newOxWithNewState(ceaName, HfNew, newTdegR)
    elif prop_type=='prop':
        newName = newPropWithNewState(ceaName, HfNew, newTdegR)
    else: # should never get here
        raise Exception('Could NOT find prop_type of ceaName=%s in makeCardForNewTemperature'%ceaName)
        
    return newName
    

def renamePropIfNewHfOrTrefInName( cardDict, name ):
    '''Look for "h,cal OR "t(k)"" in name.
       If present, then create new modified name and create new
       card in cardDict if necessary
       
       for example to tweak LH2 run might look like:
       "LH2 h,cal=-2155.0  t(k)=21.0"   
    '''
    
    if name in cardDict: # if name in dictionary already, simply return it
        return name
    
    # w/o any equals sign, assume that the name is unchanged
    if name.find("=") == -1:
        return name
        
    spL = name.split()
    #check for extra entries
    if len(spL)>1:
        dictName = spL[0]
        if dictName in cardDict:
            n = tightenUpEquals( name )
            #print('tightenUpEquals( name )',n)
            nL = n.split()
            if len(nL) != 3:
                s = 'ERROR in adjusted propellant format:\n    "%s"\n'%name +\
                    '    Should be:  "LH2 h,cal=-2155.0  t(k)=21.0"'
                raise Exception(s)
                #return name
            
            try:
                valD = {}
                for s in nL[1:]:
                    eL = s.split('=')
                    valD[ eL[0].lower() ] = float( eL[1] )
                    
                newHfCalPerMole, newTrefDegR = valD['h,cal'], valD['t(k)']
                newName = newPropWithNewState( cardDict, dictName, newHfCalPerMole, newTrefDegR)
                return newName
            except:
                s = 'ERROR in adjusted propellant format:\n    "%s"\n'%name +\
                    '    Should be:  "LH2 h,cal=-2155.0  t(k)=21.0"'
                raise Exception(s)
                #return name
        
    # if all else fails, simply return input name
    return name

if __name__ == '__main__':
    
    for name in ['FLOX80', 'MON15', 'M13', 'Peroxide88']:
        L = []

        if isAPeroxide_Blend( name ):
            addPeroxideBlend( name, L )
            
        elif isMON_Ox_Blend( name ):
            addMON_Blend(name, L)
        
        elif isFLOX_Ox_Blend( name ):
            addFLOX_Blend( name, L )
            
        elif isAnMMH_N2H4_Blend( name ):
            addMMH_N2H4_Blend( name, L )

        print( name )
        print( L )
        print( '-'*55 )



    