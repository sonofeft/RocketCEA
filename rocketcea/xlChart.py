import sys, os
from win32com.client import Dispatch
import string
from rocketcea.excel_const import constants
import rocketcea.xlChFormula as xlChFormula


msoFalse                      =0x0        # from enum MsoTriState
msoTrue                       =-1         # from enum MsoTriState

def squareUpRS( rs, pad='' ):
    '''make the input data list of lists such that each row is the same length
       add "pad" to short rows'''
    
    maxL = len(rs[0])
    minL = maxL
    for row in rs:
        L = len(row)
        maxL = max(maxL, L)
        minL = min(minL, L)
    
    #print 'in squareUpRS, maxL=',maxL,'  minL=',minL
    
    if minL<maxL:
        rs = list(rs)
        for i,row in enumerate(rs):
            #print i,rs[i]
            #padL = [pad]*(maxL-len(row))
            #print '   padL=',padL
            rs[i] = list(row) + [pad]*(maxL-len(row))
            #print i,rs[i]
    return rs
    
    
def addColumnToRS( rs, colName, columnL ):    
    '''add column of data to rs (list of row lists)
       if rs==None, create an empty rs'''
    
    if rs==None:
        rs = [[colName]]  # empty list with 1st row of labels also empty
        for val in columnL:
            rs.append( [val] )
        return rs
    
    rs = squareUpRS( rs ) # make sure all rows are of equal length
    rowLength = len( rs[0] ) # length of rows before any data is added
    
    rs[0].append(colName) # add label of new column
    
    Lrs = len(rs) - 1 # don't count label row 
    Lcol = len(columnL)
    
    if Lcol <= Lrs:
        for i,val in enumerate(columnL):
            rs[i+1].append( val )
        
        rs = squareUpRS( rs ) # make sure all rows are of equal length
    else:
        # add data to existing rows
        for i,row in enumerate(rs[1:]):
            row.append( columnL[i] )
        
        # make new rows with rowLength blanks 
        for i in range(Lrs,Lcol):
            row = ['']*rowLength
            row.append( columnL[i] )
            rs.append( row )
            
        
    return rs
    
    
def combineRS( rs1, rs2 ):    
    '''combine two rs lists into one rs list'''
    
    if rs1==None:
        return rs2
    if rs2==None:
        return rs1
        
    def getrowN( n, rsn ):
        try:
            row = rsn[n]
        except:
            row = ['']*len( rsn[0] )
        return row
    
    rs = []
    lenrs = max( len(rs1), len(rs2) )
    
    for i in range( lenrs ):
        row1 = getrowN( i, rs1 )
        row2 = getrowN( i, rs2 )
        
        rs.append( row1 + row2 )
        
    return rs
    
    

class xlChart:
    numWkBooks = 0
    def __init__(self,xlsFile="", Visible=1):
        self.xlApp = Dispatch("Excel.Application")
        self.xlApp.Visible = Visible
        xlChart.numWkBooks = xlChart.numWkBooks + 1
            
        # I don't think the sheet and column lists should be mapped
        # (unfortunately, they are right now)
        self.sheetList = []
        self.chartList = []
        self.chartNColumns = []  
        self.chartNRows = []
        self.leaveOpen = 1
        self.nRows = 0
        self.nColumns = 0
        self.formula = xlChFormula.xlChFormula() # a scratch variable
        
        if len(xlsFile)>0: # opening an existing XLS file
            xlsFile = os.path.abspath(xlsFile) # Excel likes absolute path names
            self.xlApp.Workbooks.Open(xlsFile)
            self.initFromXLSFile()
            self.xlBook = None
        else:              # making a new XLS file
            self.xlBook = self.xlApp.Workbooks.Add()            
            self.xlSheet = self.xlApp.Sheets(1)
            self.sheetList.append( self.xlSheet )
    
    def closeQuietly(self):
        self.xlApp.DisplayAlerts = 0  # Allow Quick Close without Save Message
    
    def closeQuestion(self):
        self.xlApp.DisplayAlerts = 1  # Get Save Question When Closing


    def __del__(self):
        if (__name__ != "__main__") and (not self.leaveOpen):
            # i.e. only close if this is not a Self Test
            if self.xlApp != None: self.close()
            
    def getSheetNumberFromName(self, shtName='Sheet1'):
        '''get workbook sheet number from name'''
        ns=1
        isheet = 1 # 1-based index
        for i in range(1, self.xlApp.Sheets.Count+1):
            if self.xlApp.Sheets(i).Type == constants.xlWorksheet:
                if str(shtName)==str(self.xlApp.Sheets(i).Name): ns = i   #sheet
                #print 'sheet #%i = %s'%(isheet,self.xlApp.Sheets(i).Name)
                isheet += 1
                
        #print 'selected sheet =',ns
        return ns # for 1-based index
            
    def getChartSheetNumberUsingSheetData(self, datashtName='Sheet1'):
        '''get chart sheet number which uses data from sheet name'''
        ns = 0
        for i in range(1, self.xlApp.Sheets.Count+1):  #sheets use 1-based index
            try:  # check all sheets even if previous sheets raise an exception
                if self.xlApp.Sheets(i).Type != constants.xlWorksheet:
                    # must be a chart if not a worksheet
                    f = xlChFormula.xlChFormula( self.xlApp.Sheets(i).SeriesCollection(1).Formula)
                    # returns last chart which uses data from sheet
                    if datashtName==f.labelSht: ns = i
            except:
                pass
        #print datashtName, ns
        return ns
            
    def initFromXLSFile(self):
        '''initialize from existing XLS file.  
           (file has already been opened and assigned to self.xlApp)'''

        #print "now in initFromXLSFile"
        self.xlSheet = None
        self.chart = None
        
        for ns in range(1, self.xlApp.Sheets.Count+1):  #sheets use 1-based index
            try:
                #print "starting loop",ns
                if self.xlApp.Sheets(ns).Type == constants.xlWorksheet:
                    Ncolumns, Nrows = xlChFormula.getNcolumnsNrowsFromRange(\
                        self.xlApp.Sheets(ns).UsedRange.AddressLocal)
                        
                    # check that sheet has info in it and should be added to sheet list
                    if Ncolumns>0 and Nrows>0:
                        self.xlSheet = self.xlApp.Sheets(ns)
                        self.sheetList.append( self.xlSheet )
                        self.nRows = Nrows
                        self.nColumns = Ncolumns
                        
                        # chart and sheet lists are mapped
                        self.chartNColumns.append( self.nColumns )
                        self.chartNRows.append( self.nRows )
                        
                        #print "calling getChartSheetNumberUsingSheetData"
                        nc = self.getChartSheetNumberUsingSheetData(self.xlApp.Sheets(ns).Name)
                        #print "found chart for sheet",ns,"chart=",nc
                        #print "chart type =",self.xlApp.Sheets(nc).ChartType," =? ",constants.xlXYScatterLines
                        if (nc>0): ###/// and (self.xlApp.Sheets(nc).ChartType==constants.xlXYScatterLines):
                            #print "assigning chart for sheet",ns,"chart=",nc
                            self.chart = self.xlApp.Sheets(nc)
                            self.chartList.append( self.chart )
                        #else:
                        #    self.chartList.append( None )
                        
            except:
                pass

    def getRangeValueFromRowCol(self, ULrow=1, ULcol=1, LRrow=9, LRcol=9):
        cUL = self.formula.makeColLocation( NColumn=ULcol, NRow=ULrow)
        cLR = self.formula.makeColLocation( NColumn=LRcol, NRow=LRrow)
        return self.getRangeValue( cUL, cLR )

    def getRangeValue(self, ULcell='$A$1', LRcell='$D$5'):
        try:
            r = self.xlSheet.Range(ULcell, LRcell)
            return  r.Value 
        except:
            return ''

    def getRangeFormulas(self, ULcell='$A$1', LRcell='$D$5'):
        try:
            r = self.xlSheet.Range(ULcell, LRcell)
            return  r.Formula 
        except:
            return ''

    def getCellValue(self, cell='$A$1'):
        try:
            r = self.xlSheet.Range(cell, cell)
            return str( r.Value )
        except:
            return ''

    def getRowColCellValue(self, NColumn=1, NRow=1):
        c = self.formula.makeColLocation( NColumn=NColumn, NRow=NRow)
        return self.getCellValue(c)    

    def getColumnName(self, NColumn=1):        
        if NColumn<=self.nColumns:
            #///cL = '$'+ self.formula.excelColLetter( NColumn )+ '$1'
            cL = self.formula.makeColLocation( NColumn=NColumn, NRow=1)
            r = self.xlSheet.Range(cL, cL)
            return str( r.Value )
        else:
            return 'None'

    def getAllColumnNames(self):
        cL = self.formula.makeColLocation( NColumn=self.nColumns, NRow=1)
        r = self.xlSheet.Range("$a$1", cL)
        return r.Value[0]
        
    def addNewSeries(self, NChart=None, NSheet=None, xColumn=1, yColumn=2):
        # use 1-based indeces for NChart and NSheet

        if NChart == None:
            ch = self.chartList[-1]
        else:
            ch = self.chartList[NChart-1]
        
        if NSheet == None:
            sh = self.sheetList[-1]
        else:
            sh = self.sheetList[NSheet-1]
            
        series = ch.SeriesCollection().NewSeries()
        Ncolumns, Nrows = xlChFormula.getNcolumnsNrowsFromRange( sh.UsedRange.AddressLocal )
        L = self.formula.makeColRange(NColumn=xColumn, fromRow=2, toRow=Nrows)
        rx = sh.Range(L)
        series.XValues = rx
        
        L = self.formula.makeColRange(NColumn=yColumn, fromRow=2, toRow=Nrows)
        ry = sh.Range(L)
        series.Values = ry
        
        cL = self.formula.makeColLocation( NColumn=yColumn, NRow=1)
        r = sh.Range(cL)
        series.Name = r
        
    def addNewSeriesToCurrentSheetChart(self, xColumn=1, yColumn=2):

        sh = self.xlSheet
        ch = self.chart
        series = ch.SeriesCollection().NewSeries()
        Ncolumns, Nrows = xlChFormula.getNcolumnsNrowsFromRange( sh.UsedRange.AddressLocal )
        L = self.formula.makeColRange(NColumn=xColumn, fromRow=2, toRow=Nrows)
        rx = sh.Range(L)
        series.XValues = rx
        
        L = self.formula.makeColRange(NColumn=yColumn, fromRow=2, toRow=Nrows)
        ry = sh.Range(L)
        series.Values = ry
        
        cL = self.formula.makeColLocation( NColumn=yColumn, NRow=1)
        r = sh.Range(cL)
        series.Name = r


    def changeSeriesOnChart(self, NChart=1, NSeries=1, NSheet=1, xColumn=1, yColumn=2):
        # use 1-based indeces for NChart and NSheet
        sh = self.sheetList[NSheet-1]
        ch = self.chartList[NChart-1]
        Ncolumns, Nrows = xlChFormula.getNcolumnsNrowsFromRange( sh.UsedRange.AddressLocal )
        L = self.formula.makeColRange(NColumn=xColumn, fromRow=2, toRow=Nrows)
        rx = sh.Range(L)
        ch.XYGroups(1).SeriesCollection(NSeries).XValues = rx
        
        L = self.formula.makeColRange(NColumn=yColumn, fromRow=2, toRow=Nrows)
        ry = sh.Range(L)
        ch.XYGroups(1).SeriesCollection(NSeries).Values = ry
        
        cL = self.formula.makeColLocation( NColumn=yColumn, NRow=1)
        r = sh.Range(cL)
        ch.SeriesCollection(NSeries).Name = r

    def changeSeriesXValuesColumn(self, NColumn=2, NSeries=1):
        if NColumn<=self.nColumns:
            L = self.formula.makeColRange(NColumn=NColumn, fromRow=2, toRow=self.nRows)
            r = self.xlSheet.Range(L)
            self.chart.XYGroups(1).SeriesCollection(NSeries).XValues = r
            self.labelXAxis( self.getAllXNames() )
            
            #///////////// Formula approach seems buggy            
            #self.formula.setFormula(self.chart.XYGroups(1).SeriesCollection(NSeries).Formula )
            #self.formula.setXColumn( col=NColumn, topRow=2, botRow=self.nRows, Sheet='')
            #self.chart.XYGroups(1).SeriesCollection(NSeries).Formula = self.formula.getFormula()

    def setAllXValueColumns(self, NColumn=1, ZeroBased=0):
        try:
            ns = 1
            c = NColumn
            if ZeroBased:c = c + 1
            while 1:
                L = self.formula.makeColRange(NColumn=c, fromRow=2, toRow=self.nRows)
                r = self.xlSheet.Range(L)
                self.chart.XYGroups(1).SeriesCollection(ns).XValues = r
                ns = ns + 1
        except:
            pass
        self.labelXAxis( self.getRowColCellValue( NColumn=c, NRow=1) )

    def changePlottedColumn(self, NColumn=2, NSeries=1):
        try:
            if NColumn<=self.nColumns:
                L = self.formula.makeColRange(NColumn=NColumn, fromRow=2, toRow=self.nRows)
                r = self.xlSheet.Range(L)
                self.chart.XYGroups(1).SeriesCollection(NSeries).Values = r

                cL = self.formula.makeColLocation( NColumn=NColumn, NRow=1)
                r = self.xlSheet.Range(cL)
                self.chart.SeriesCollection(NSeries).Name = r
                #////////// Formula approach seems buggy
                #self.formula.setFormula(self.chart.XYGroups(1).SeriesCollection(NSeries).Formula )
                #self.formula.setValColumn( col=NColumn, topRow=2, botRow=self.nRows, Sheet='')
                #self.formula.setLabel( col=NColumn, row=1, Sheet='')
                #self.chart.XYGroups(1).SeriesCollection(NSeries).Formula = self.formula.getFormula()
        except: pass
        
    def formatFocusedChart(self):

        self.chart.ChartType = constants.xlXYScatterLines
        self.chart.SizeWithWindow = 1

        self.chart.PlotArea.Interior.ColorIndex = constants.xlNone

        self.chart.HasTitle = 1  # Need to enable title before setting text
        self.chart.Axes( constants.xlCategory, constants.xlPrimary).HasTitle = 1
        
        self.chart.Axes( constants.xlValue, constants.xlPrimary).HasTitle = 1

        self.chart.Axes(1).HasMajorGridlines = 1  # turn on grid for x axis
        self.chart.Axes(1).MajorGridlines.Border.LineStyle = constants.xlDot  # make grid style dots
        self.chart.Axes(2).MajorGridlines.Border.LineStyle = constants.xlDot  # make grid style dots
    

    def makeNewChartOfPlottedColumns(self, cols=(2,), ZeroBased=0, chartName='' ):
        '''use cols tuple to set all plotted columns
        if Number of Columns is wrong, correct'''        
        
        self.chart = self.xlApp.Charts.Add()
        self.chartList.append( self.chart )
        self.chartNColumns.append( self.nColumns )
        self.chartNRows.append( self.nRows )
        
        self.formatFocusedChart()

        self.setAllPlottedColumns( cols=cols, ZeroBased=ZeroBased ,chartName=chartName)
        

    def setAllPlottedColumns(self, cols=(2,), ZeroBased=0 , chartName=''):
        '''use cols tuple to set all plotted columns
        if Number of Columns is wrong, correct'''
        if len(cols)>0: self.setNumberOfPlotCurves( len(cols) )
        ns = 1
        name = ''
        for c in cols:
            if ZeroBased:c = c + 1
            self.changePlottedColumn(NColumn=c, NSeries=ns)
            ns = ns + 1
            if len(name)>0: name = name + ", "
            name = name + str( self.getColumnName(c) )
        self.labelPrimaryYAxis(yLabel=name)
        self.labelXAxis( self.getColumnName(1) )
        if len(chartName)>0: self.chart.Location(Where=1, Name=chartName)

    def focusSheet(self, N):
        '''use 1-based index to Sheets'''
        print 'focusing on sheet #',N
        self.xlSheet = self.sheetList[N-1]
        #self.chart = self.chartList[N-1]
        #self.nColumns = self.chartNColumns[N-1]
        #self.nRows = self.chartNRows[N-1]
        self.xlSheet.Activate()
        
    def focusSheetByName(self, shtName='Sheet1'):
        N = self.getSheetNumberFromName( shtName=shtName)
        self.xlApp.Sheets(N).Activate()
        #self.focusSheet(N)

    def focusChart(self, N):
        '''use 1-based index to Charts'''
        self.xlSheet = self.sheetList[N-1]
        self.chart = self.chartList[N-1]
        self.nColumns = self.chartNColumns[N-1]
        self.nRows = self.chartNRows[N-1]
        self.chart.Activate()

    def labelPrimaryYAxis(self, yLabel="Vertical Axis"):        
        self.chart.Axes( constants.xlValue, constants.xlPrimary).HasTitle = 1
        self.chart.Axes( constants.xlValue, constants.xlPrimary).AxisTitle.Characters.Text = yLabel

    def getAllXNames(self):
        xLabels = {}
        allXNames = ''
        try:
            ns = 1
            while 1:
                self.formula.setFormula(self.chart.XYGroups(1).SeriesCollection(ns).Formula)
                ns = ns + 1
                xLabels[self.getCellValue( cell=self.formula.labelLoc )] = 1
        except:
            pass
        for k in xLabels.keys():
            if len(allXNames)>0:allXNames = allXNames + ', '
            allXNames = allXNames + k
        return allXNames


    def labelXAxis(self, xLabel="Horizontal Axis"):
        self.chart.Axes( constants.xlCategory, constants.xlPrimary).HasTitle = 1
        self.chart.Axes( constants.xlCategory, constants.xlPrimary).AxisTitle.Characters.Text = xLabel

    def changePlotTitle(self, title="Data from Python Script"):
        self.chart.HasTitle = 1  # Need to enable title before setting text
        self.chart.ChartTitle.Characters.Text = title

    def setPlotTitleSize(self, pointSize=20, bold=0):
        self.chart.HasTitle = 1  # Need to enable title before setting text
        self.chart.ChartTitle.Characters.Font.Size = pointSize
        self.chart.ChartTitle.Characters.Font.Bold = bold
            
        
    def putSeriesOnSecondary(self, J, y2Label=""):
        '''use 1-based index to Sheets'''
        try:
            self.chart.SeriesCollection(J).AxisGroup = constants.xlSecondary
            if len(y2Label)>0:
                self.chart.Axes( constants.xlValue, constants.xlSecondary).HasTitle = 1
                self.chart.Axes( constants.xlValue, constants.xlSecondary).AxisTitle.Characters.Text = y2Label
        except:
            pass

    def putSeriesOnPrimary(self, J):
        '''use 1-based index to Sheets'''
        try:
            self.chart.SeriesCollection(J).AxisGroup = constants.xlPrimary
        except:
            pass

    def close(self):
        if self.xlBook != None:
            self.xlApp.DisplayAlerts=0 # Allow Quick Close without Save Message
            self.xlBook.Close()
            self.xlBook = None
            self.xlApp.DisplayAlerts=1
        xlChart.numWkBooks = xlChart.numWkBooks - 1
        if xlChart.numWkBooks==0: self.xlApp.Quit()
        self.xlApp = None

    def setLineStyle(self, NSeries=1, style=1):
        if style==0: s =  constants.xlLineStyleNone
        elif style==1: s = constants.xlContinuous
        elif style==2: s = constants.xlDash 
        elif style==3: s = constants.xlDashDot  
        elif style==4: s = constants.xlDashDotDot
        elif style==5: s = constants.xlDot  
        elif style==6: s = constants.xlDouble
        elif style==7: s = constants.xlSlantDashDot
        else:s = constants.xlContinuous

        try:
            self.chart.SeriesCollection(NSeries).Border.LineStyle = s
            return 1
        except:
            return 0

    def setLineStyles(self, style=1):
        N = 1
        while self.setLineStyle(NSeries=N, style=style):
            N = N + 1

    def setLineThickness(self, NSeries=1, thickness=1):
        t=constants.xlMedium
        if thickness<=0:
            return self.setLineStyle( NSeries=NSeries, style=0)
        elif thickness==1: t=constants.xlHairline
        elif thickness==2: t=constants.xlThin
        elif thickness==3: t=constants.xlMedium
        else:              t=constants.xlThick
        try:
            self.setLineStyle( NSeries=NSeries, style=1)
            self.chart.SeriesCollection(NSeries).Border.Weight = t
            return 1
        except:
            return 0

    def setLineThicknesses(self, thickness=1):
        N = 1
        while self.setLineThickness(NSeries=N, thickness=thickness):
            N = N + 1

    def setSeriesColorIndex(self, NSeries=1, colorIndex=1):
        try:
            self.chart.SeriesCollection(NSeries).Border.ColorIndex = colorIndex
            if self.chart.SeriesCollection(NSeries).MarkerStyle != constants.xlMarkerStyleNone:
                self.chart.SeriesCollection(NSeries).MarkerBackgroundColorIndex = colorIndex
                self.chart.SeriesCollection(NSeries).MarkerForegroundColorIndex = colorIndex
            return 1
        except:
            return 0

    def setSeriesColor(self, NSeries=1, red=255, green=0, blue=0):
        try:
            RevRGB= int(red) + int(green * 256) + int(blue * 65536)

            self.chart.SeriesCollection(NSeries).Border.Color = RevRGB
            if self.chart.SeriesCollection(NSeries).MarkerStyle != constants.xlMarkerStyleNone:
                self.chart.SeriesCollection(NSeries).MarkerBackgroundColor = RevRGB
                self.chart.SeriesCollection(NSeries).MarkerForegroundColor = RevRGB
            #print 'RevRGB',RevRGB
            return 1
        except:
            return 0


    def setMarkerSize(self, NSeries=1, size=4):
        try:
            self.chart.SeriesCollection(NSeries).MarkerSize = size
            return 1
        except:
            return 0

    def setMarkerSizes(self, size=4):
        N = 1
        while self.setMarkerSize(NSeries=N, size=size):
            N = N + 1


    def turnMarkerOnOff(self, NSeries=1, showPoints=0):
        try:
            if showPoints:
                self.chart.SeriesCollection(NSeries).MarkerStyle = constants.xlMarkerStyleAutomatic
            else:
                self.chart.SeriesCollection(NSeries).MarkerStyle = constants.xlMarkerStyleNone
            return 1
        except:
            return 0

    def turnMarkersOnOff(self, showPoints=0):
        N = 1
        while self.turnMarkerOnOff(NSeries=N, showPoints=showPoints):
            N = N + 1
    
    def setXScaleType(self, log=1):
        if log:
            self.chart.Axes(1).ScaleType = constants.xlLogarithmic
            #self.chart.Axes(1).AxisBetweenCategories = 1
            self.chart.Axes(1).MinorTickMark = constants.xlTickMarkCross
            self.chart.Axes(1).HasMinorGridlines = 1  # turn on grid for x axis
            self.chart.Axes(1).MinorGridlines.Border.LineStyle = constants.xlDot  # make grid style dots
            self.chart.Axes(1).MajorGridlines.Border.LineStyle = constants.xlContinuous
        else:
            self.chart.Axes(1).ScaleType = constants.xlScaleLinear
    
    def setYScaleType(self, log=1):
        if log:
            self.chart.Axes(2).ScaleType = constants.xlLogarithmic
            #self.chart.Axes(2).AxisBetweenCategories = 1
            self.chart.Axes(2).MinorTickMark = constants.xlTickMarkCross
            self.chart.Axes(2).HasMinorGridlines = 1  # turn on grid for x axis
            self.chart.Axes(2).MinorGridlines.Border.LineStyle = constants.xlDot  # make grid style dots
            self.chart.Axes(2).MajorGridlines.Border.LineStyle = constants.xlContinuous
        else:
            self.chart.Axes(2).ScaleType = constants.xlScaleLinear
    
    def setXrange(self, xmin=1.0, xmax=10.0):
        self.chart.Axes(1).MinimumScale=xmin
        self.chart.Axes(1).MaximumScale=xmax
    
    def setYrange(self, ymin=1.0, ymax=10.0):
        self.chart.Axes(2).MinimumScale=ymin
        self.chart.Axes(2).MaximumScale=ymax

        
    def setYrangeOnSecondary(self, ymin=1.0, ymax=10.0):
        self.chart.Axes(constants.xlValue, constants.xlSecondary).MinimumScale=ymin
        self.chart.Axes(constants.xlValue, constants.xlSecondary).MaximumScale=ymax


    def addTextBox(self,text='comment',left=100,top=100, width=50, height=50, 
        border=3, auto=1, transparency=0.3, isBold=0, fontSize=0):
        tb = self.chart.TextBoxes().Add(left,top,width,height)
        tb.Text=text[:255] # try limiting length to prevent errors
        tb.Border.Weight=border
        if isBold:
            tb.Font.IsBold = 1
        if fontSize:
            tb.Font.Size = fontSize
        if auto:
            tb.AutoSize=True
            tb.Interior.Color = constants.xlColorIndexAutomatic
        
            w = self.chart.PlotArea.Width
            h = self.chart.PlotArea.Height
        
            tb.Left = int( w/4)
            tb.Top = int( h/4 )
            
        tb.ShapeRange.Fill.Transparency = transparency
        
        
    def setNumberOfPlotCurves(self, NCurves=1):            
        pRange = self.xlSheet.Range('$A$1', self.xlSheet.Cells(self.nRows, NCurves+1))
        self.chart.SetSourceData( pRange, constants.xlColumns )

    def makeChart(self, rs, title="Data From Python", nCurves = 1,
                  sheetName="",chartName="",  showPoints=1, showLegend=1,
                  yLabel="Vertical Axis", xLabel="Horizontal Axis",
                  always_add_new_sheets=False):
        '''make Excel Chart from rs
        rs is a tuple of tuples, or a list of lists
        Top row of titles, then rows of data'''
        if (len(self.chartList)>0) or always_add_new_sheets:
            self.xlSheet = self.xlApp.Sheets.Add()
            self.sheetList.append( self.xlSheet )
        if len(sheetName)>0:self.xlSheet.Name = sheetName
    
        rs = squareUpRS( rs, pad='' )

        self.nRows = len(rs)
        self.nColumns = len(rs[0])
        cRange = self.xlSheet.Range('$A$1', self.xlSheet.Cells(self.nRows, self.nColumns))
        cRange.Value = rs
        
        self.chart = self.xlApp.Charts.Add()
        self.chartList.append( self.chart )
        self.chartNColumns.append( self.nColumns )
        self.chartNRows.append( self.nRows )
        if len(chartName)>0: self.chart.Location(Where=1, Name=chartName)

        self.chart.ChartType = constants.xlXYScatterLines
        self.chart.SizeWithWindow = 1

        if self.nColumns>nCurves+1:  # if number of curves is set, use it.
            pRange = self.xlSheet.Range('$A$1', self.xlSheet.Cells(self.nRows, nCurves+1))
        else:                     # otherwise plot everything
            pRange = cRange
        self.chart.SetSourceData( pRange, constants.xlColumns )
        self.chart.PlotArea.Interior.ColorIndex = constants.xlNone

        self.chart.HasTitle = 1  # Need to enable title before setting text
        self.chart.ChartTitle.Characters.Text = title
        
        if len(xLabel)>0:
            self.chart.Axes( constants.xlCategory, constants.xlPrimary).HasTitle = 1
            self.chart.Axes( constants.xlCategory, constants.xlPrimary).AxisTitle.Characters.Text = xLabel
        
        if len(yLabel)>0:
            self.chart.Axes( constants.xlValue, constants.xlPrimary).HasTitle = 1
            self.chart.Axes( constants.xlValue, constants.xlPrimary).AxisTitle.Characters.Text = yLabel

        self.chart.Axes(1).HasMajorGridlines = 1  # turn on grid for x axis
        self.chart.Axes(1).MajorGridlines.Border.LineStyle = constants.xlDot  # make grid style dots
        self.chart.Axes(2).MajorGridlines.Border.LineStyle = constants.xlDot  # make grid style dots
    
        if not showPoints: self.turnMarkersOnOff(showPoints=0)
        if not showLegend: self.chart.HasLegend = 0  # set Legend On or Off

    def makeDataSheet(self, rs, sheetName="DataSheet", autoFit=1, rowFormatL=None,
        textFont='', textFontSize=None, colFormatL=None):
        '''make Excel Data Sheet from rs
        rs is a tuple of tuples, or a list of lists
        Top row of titles, then rows of data'''
        self.xlSheet = self.xlApp.Sheets.Add()
        self.sheetList.append( self.xlSheet )
        if len(sheetName)>0:self.xlSheet.Name = sheetName
            
        rs = squareUpRS( rs, pad='' )
            
        
        self.nRows = len(rs)
        self.nColumns = len(rs[0])
        
        # get rid of any equals signs as 1st letter of string
        for i in range( self.nRows ):
            for j in range( self.nColumns ):
                try:
                    if rs[i][j][0]=='=':
                        rs[i][j] = "'" + str(rs[i][j])
                except:
                    pass
                        
        
        cRange = self.xlSheet.Range('$A$1', self.xlSheet.Cells(self.nRows, self.nColumns))
        cRange.Value = rs
        
        if textFont:
            cRange.Font.Name = textFont
            
        if textFontSize:
            cRange.Font.Size = textFontSize
        
        if autoFit:
            cRange.Columns.AutoFit()
            
        #cRange.NumberFormat = "0.00"
        if rowFormatL:
            for i,rowFormat in enumerate( rowFormatL ):
                if rowFormat:
                    N = i+1
                    cLR = self.formula.makeColLocation( NColumn=self.nColumns, NRow=N)
                    rowRange = self.xlSheet.Range('$A$%i'%N, cLR)
                    rowRange.NumberFormat = rowFormat
                    #print '$A$%i'%N, rowFormat
        
        if colFormatL:
            for i,colFormat in enumerate( colFormatL ):
                if colFormat:
                    NColumn = i+1
                    if NColumn>26:
                        r = NColumn % 26 - 1
                        q = NColumn / 26 - 1
                        colStr = string.uppercase[q] + string.uppercase[r]
                    else:
                        colStr = string.uppercase[NColumn-1]
                    colRange = self.xlSheet.Range('$%s$1'%colStr, '$%s$%i'%(colStr,self.nRows))
                    colRange.NumberFormat = colFormat
                    #print colRange[0]

    def setRangeOfDataSheet(self, rs, sheetName="DataSheet", upperLeft='$A$1',
        bgColor=None):
        '''add to Excel Data Sheet from rs
        rs is a tuple of tuples, or a list of lists
        Top row of titles, then rows of data'''
        self.focusSheetByName(shtName=sheetName)
        
        rs = squareUpRS( rs, pad='' )
        
        ucol, urow = xlChFormula.getNcolNrow( cell=upperLeft)
        lcol = len(rs[0]) + ucol - 1
        lrow = len(rs) + urow - 1

        cLR = self.formula.makeColLocation( NColumn=lcol, NRow=lrow)
        
        #self.nRows = len(rs)
        #self.nColumns = len(rs[0])
        cRange = self.xlSheet.Range(upperLeft, cLR)
        cRange.Value = rs

        if bgColor:
            cRange.Interior.ColorIndex = bgColor
            

    def setCellOfDataSheet(self, val=123, sheetName="DataSheet", irow=1, icol=1,
        bgColor=None):
        
        self.focusSheetByName(shtName=sheetName)
        cell = self.formula.makeColLocation( NColumn=icol, NRow=irow)
        
        self.setRangeOfDataSheet( [[val]], sheetName=sheetName, upperLeft=cell,
            bgColor=bgColor)

    def setRangeOnCurrentSheet(self, rs, upperLeft='$A$1', bgColor=None):
        '''add to Excel Data Sheet from rs
        rs is a tuple of tuples, or a list of lists
        Top row of titles, then rows of data'''
        
        rs = squareUpRS( rs, pad='' )
        
        ucol, urow = xlChFormula.getNcolNrow( cell=upperLeft)
        lcol = len(rs[0]) + ucol - 1
        lrow = len(rs) + urow - 1

        cLR = self.formula.makeColLocation( NColumn=lcol, NRow=lrow)
        
        #self.nRows = len(rs)
        #self.nColumns = len(rs[0])
        cRange = self.xlSheet.Range(upperLeft, cLR)
        cRange.Value = rs

        if bgColor:
            cRange.Interior.ColorIndex = bgColor
        

    def pageSetupForSheet(self, landscape=0, fitWidth=0, fitHeight=0, marginInches=0.0):
        '''use active sheet'''
        #self.xlSheet = self.sheetList[N-1]
        #self.xlSheet.Activate()
        if fitWidth:
            self.xlSheet.PageSetup.FitToPagesWide = fitWidth
        if fitHeight:
            self.xlSheet.PageSetup.FitToPagesTall = fitHeight
        
        if landscape:
            self.xlSheet.PageSetup.Orientation = constants.xlLandscape
        else:
            self.xlSheet.PageSetup.Orientation = constants.xlPortrait
        self.xlSheet.PageSetup.Zoom = False
        
        if marginInches > 0.0:
            self.xlSheet.PageSetup.LeftMargin = self.xlApp.InchesToPoints(marginInches)  
            self.xlSheet.PageSetup.RightMargin = self.xlApp.InchesToPoints(marginInches)  
            self.xlSheet.PageSetup.TopMargin = self.xlApp.InchesToPoints(marginInches)  
            self.xlSheet.PageSetup.BottomMargin = self.xlApp.InchesToPoints(marginInches)  


    def autoFitSheet(self, ULeft='$A$1',  nRows=100, nColumns=100):
        '''autofit range of current sheet from A1 to nRows, nColumns'''                        
        
        cRange = self.xlSheet.Range(ULeft, self.xlSheet.Cells(nRows, nColumns))
        cRange.Columns.AutoFit()

    def formatCellsInRange(self, fmtString="0.00", ULeft='$A$1',  nRows=100, nColumns=100):
        CRange = self.xlSheet.Range(ULeft, self.xlSheet.Cells(nRows, nColumns))
        CRange.NumberFormat = fmtString
        

    def AddPictureToDataSheet(self, imgAbsPath='', sheetName="DataSheet", left=100, top=100, width=100, height=100):
        '''add Image to Excel Data Sheet 
           imgAbsPath is the absolute path to image
           top and left are points to upper left of image on page
           width and height are points as well
        '''
        
        imgAbsPath = os.path.abspath(imgAbsPath) # make sure it's an absolute path name
        
        if sheetName:
            self.focusSheetByName(shtName=sheetName)
        self.xlSheet.Shapes.AddPicture( imgAbsPath, msoFalse, msoTrue, left, top, width, height)

if __name__ == "__main__": #Self Test
    xl = xlChart()
    rs = ( ("x", "y", "Zee","Queue"), (1,1,1,1,1,1), (2,2,3,4), (3,3,5,8) )
    rs = squareUpRS( rs, pad='xxx' )
    
    xl.makeChart(rs, title="Now is the time for First Things",nCurves = 3,
                 chartName="First Chart",
                 sheetName="First Dataset",
                 yLabel="Vertical Axis", xLabel="Horizontal Axis")
    rs = ( ("A", "B", "Cee", "Dee"), (11,33,33,33), (21,21,25,30), (31,11,21,25) )
    xl.makeChart(rs, title="This is a Second Plot",nCurves = 3,
                 chartName="Second Chart", showPoints=0, showLegend=0,
                 sheetName="Second Dataset",
                 yLabel="Vertical Axis", xLabel="Horizontal Axis")
    xl.focusChart(1)
    xl.putSeriesOnSecondary(1, y2Label="Second Y Axis")
    xl.putSeriesOnSecondary(2)
    xl.putSeriesOnPrimary(1)
    xl.labelXAxis("Whole New X Axis Label")
    xl.labelPrimaryYAxis("Whole New Y Axis Label")
    xl.setLineThickness(NSeries=1, thickness=1)
    xl.setLineThickness(NSeries=2, thickness=2)
    xl.setLineThickness(NSeries=3, thickness=3)
    xl.setMarkerSize( NSeries=1, size=4)
    xl.setMarkerSize( NSeries=2, size=6)
    xl.setMarkerSize( NSeries=3, size=8)
    xl.xlApp.DisplayAlerts = 0  # Allow Quick Close without Save Message
    xl.setLineStyle(NSeries=1, style=0)
    
    xl.setSeriesColorIndex(NSeries=1, colorIndex=1)
    #xl.setSeriesColorIndex(NSeries=2, colorIndex=22)
    xl.setSeriesColor( NSeries=2, red=0, green=255, blue=0)
    xl.setSeriesColor( NSeries=3, red=255, green=0, blue=0)
    text='''abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy
abcdefghijklmnopqrstuvwxy'''
    xl.addTextBox(text=text)
    xl.setXrange( 1.0, 3.0)
    xl.setYrange( 1.0, 5.0)
    
    here = os.path.abspath(os.path.dirname(__file__))
    
    #xl.AddPictureToDataSheet( imgAbsPath=os.path.join(here,'r_daneel_olivaw.jpg'), sheetName="First Dataset", left=100, top=100, width=100, height=100)    
    for i in range(1,11):
        xl.setRangeOfDataSheet( [['SPOT']], sheetName="Sheet1", upperLeft='$A$%i'%i,
            bgColor=i)