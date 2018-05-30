import string

def getNcolFromLetter( colLet ):
    if len(colLet)==2:
        Ncol = 26*(ord( string.upper(colLet[0]) ) - 64) + ord( string.upper(colLet[1]) ) - 64
    elif len(colLet) ==1:
        Ncol = ord( string.upper(colLet) ) - 64
    else:
        Ncol = 1
    return Ncol

def getNcolNrow( cell="$a$1"):
    sp = string.split( cell, '$')
    Ncol=1
    Nrow=1
    try:
        Ncol = getNcolFromLetter( sp[1] )
        Nrow = string.atoi( sp[2] )
    except:
        pass
    return Ncol, Nrow
    
def getNcolumnsNrowsFromRange( crange="$A$1:$D$8" ):
    sp = string.split( crange, ':' )
    Ncolumns=0
    Nrows=0
    try:
        if len(sp[1])>0:
            Nc1, Nr1 = getNcolNrow( sp[0] )
            Nc2, Nr2 = getNcolNrow( sp[1] )
            Ncolumns = Nc2 - Nc1 + 1
            Nrows = Nr2 - Nr1 + 1
    except:
        pass
    return Ncolumns, Nrows  

class xlChFormula:
    '''excel spreadsheet chart formulas for XY Series'''

    def setLabel(self, col=1, row=1, Sheet=''):
        self.labelLoc = self.makeColLocation( NColumn=col, NRow=row)
        if len(Sheet)>0: self.labelSht = Sheet

    def setXColumn(self, col=1, topRow=2, botRow=100, Sheet=''):
        self.xcolRng = self.makeColRange( NColumn=col, fromRow=topRow, toRow=botRow)
        if len(Sheet)>0: self.xcolSht = Sheet

    def setValueColumn(self, col=1, topRow=2, botRow=100, Sheet=''):
        self.valRng = self.makeColRange( NColumn=col, fromRow=topRow, toRow=botRow)
        if len(Sheet)>0: self.valSht = Sheet
    
    def splitIntoParts(self):
        '''from self.formula, split into individual parts'''
        sp = string.split( self.formula, ',')
        self.labelSht, self.labelLoc = string.split( sp[0], '!')
        self.labelSht = string.split( self.labelSht, '(')[1]
        
        self.xcolSht, self.xcolRng = string.split( sp[1], '!')
        self.valSht, self.valRng = string.split( sp[2], '!')
        self.seriesNum = string.split( sp[3], ')')[0]

    def excelColLetter(self, NColumn=1):
        '''return the letter representation of excel columns'''
        if NColumn>26:
            r = NColumn % 26 - 1
            q = NColumn / 26 - 1
            return string.uppercase[q] + string.uppercase[r]
        else:
            return string.uppercase[NColumn-1]

    def makeColLocation(self, NColumn=1, NRow=1):
        L = self.excelColLetter( NColumn )
        return '$'+L+'$'+str(NRow) 

    def makeColRange(self, NColumn=1, fromRow=1, toRow=10):
        L = self.excelColLetter( NColumn )
        return '$'+L+'$'+str(fromRow) +':'+ '$'+L+'$'+str(toRow)

    def makeRange(self, fromColumn=1, fromRow=1, toColumn=2, toRow=10):
        L1 = self.excelColLetter( fromColumn )
        L2 = self.excelColLetter( toColumn )
        return '$'+L1+'$'+str(fromRow) +':'+ '$'+L2+'$'+str(toRow)

    def getFormula(self):
        return '=SERIES(' + self.labelSht + '!' + self.labelLoc + \
               ',' + self.xcolSht  + '!' + self.xcolRng +\
               ',' + self.valSht + '!' + self.valRng + \
               ',' + self.seriesNum + ')'

    def setFormula(self, strForm='=SERIES(Sheet1!$C$1,Sheet1!$A$2:$A$102,Sheet1!$C$2:$C$102,1)'):
        self.formula = strForm
        self.splitIntoParts()
        
    def __init__(self, strForm='=SERIES(Sheet1!$C$1,Sheet1!$A$2:$A$102,Sheet1!$C$2:$C$102,1)'):
        self.formula = strForm
        self.splitIntoParts()