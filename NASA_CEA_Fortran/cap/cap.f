      PROGRAM CAP
C   1/9/01
C  CAP stands for Coefficients and Properties.  It calculates
C     thermodynamic functions, including delta H and log K from
C     coeficients in the new form as produced by the PAC91 program.
C     In a way CAP is the reverse of PAC.
C
C  INPUT FILE: Two header records followed by any number of sets of
C     coefficients in the most recent format (See NASA TP-1311).
C
C  Record one contains 6 possible character variables in single quotes
C     with space and/or commas between them.  They may be either all 
C     upper or all lower case.  They contain the options as follows:
C
C     'logk'   Include delta H and log K values with rounded thermo
C              functions.
C     'mfig'   Do tables of thermo functions with many figures and no
C              delta H and log K values.
C     'joules' Energy units for the output will be in joules or
C              kilojoules.  (JOULES is the default option)
C     'cal'    Energy units for the output will be in calories or Kcal.
C     'engr'   Engineering units with temperature in degR.
C     'nodim'  Thermodynamic functions for a many-figured table will
C              be dimensionless.  (i.e. NODIM is only valid when used
C              with MFIG)
C     'plot'   Dump T, Cp/R, H-H0/RT, S/R, -(G-H0)/RT, H/RT,and -G/RT in 
C              columns.
C
C  Record two gives the temperature schedule in the form of temperatures
C     and temperature intervals.  The even-numbered values are 
C     intervals to be used between the odd-numbered temperature values.
C     The schedule will be used for the entire computer run.
C     For each set of coefficients, CAP automatically
C     limits the schedule according to the temperatures ranges given
C     with the coefficients.  Endpoints, 298.15, and transition points
C     are automatically added if they are within the requested range.
C     For transition points, values will be given for each phase at
C     the common point.  Up to 490 values may be given.  They are
C     read with a list-directed format.  That is, values are given
C     with either space or commas between them.
C     Even numbered interval values must be included even
C     if they are zero.  Two adjacent commas will indicate a zero value.
C     For example, to get the temperature set: 100,298.15,500,1000,1500,
C     2000,and 3000, the record might be as follows.
C       100,0,500,500. 2000,,3000./
C
C     The following is a sample set for the two header records:
C 
C        'mfig' 'LOGK' 'joules' 'cal' /
C    200,100,500,500,3000,1000,6000. /
C     
C      This should give both many-figured tables and rounded tables
C      with delta H and log K for both sets of enery units, joules and
C      calories.  The temperature schedule should be 200 K every 100 K
C      to 500 K, every 500 K to 3000 K, and every 1000 K to 
C      6000 K with 298.15 inserted between 200 and 300 K.  (i.e. 200,
C      298.15,300,400,500,1000,1500,2000,2500,3000,4000,5000,6000.)
C
      LOGICAL mfig,calclk,rec3,plot
      DOUBLE PRECISION EXL,TEX,TT,temps(500),htf,hplot,hh,gh,molwt

C     added 10/26/99 to initialize tin() array:
      DOUBLE PRECISION tin(500)/500*0.0d+00/

      dimension sl(5)
C 
      CHARACTER*4 rec1(4)
      CHARACTER*4 fmla(2,5)
      character*8 output(8),code
      character*64 record
C 
      CHARACTER*4 BLANK,DATE,EFAZ,FAZ,FORMLA
      CHARACTER*4 NAME,SYMBOL
      COMMON /CHAR4/ BLANK,DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),NAME(4,9),SYMBOL(2,100)
C 
      DOUBLE PRECISION asindt,COEF,CPR,GHRT,htform,enth29,
     1 HHRT,R,RR,tconv,Reng,SR,T,tcoef,term,z,wt,WORD,TTRANS
      COMMON /REAL8/ asindt,COEF(10,20),CPR(500),
     1 GHRT(500),htform,enth29,HHRT(500),R,RR,tconv,Reng,
     2 SR(500),T(500),tcoef(2,20),term,z,wt,WORD(4),TTRANS(6)
C 
      COMMON /REAL4/ EX(8,20),sel(5),sels(5)
C 
      COMMON /INTGR/ IC80,itr,JF(5),
     1 numexp(20),NEL,NKIND,NKINDS,
     2 NMAT(100),NMLA(100),NOATMS,NT,NTMP,NFZ,INA,NNA
C 
      LOGICAL hasind,refel,TOUT,mfig,eng
      COMMON /LOGCL/ hasind,refel,TOUT(3),mfig,eng
C 
C  INITIALIZE ONCE. 
C 
      OPEN (18,FILE='plotout',STATUS='unknown',FORM='formatted')
      WRITE(6,2)
2     FORMAT(/,' ****************************************************',
     & '*********************************')
      WRITE(6,3)
3     FORMAT(/,20x,'CAP (COEFFICIENTS AND PROPERTIES) PROGRAM',/ 28x,
     & 'NASA GLENN RESEARCH CENTER',//,4x,'CALCULATES THERMODYNAMIC ',
     & 'FUNCTIONS FROM COEFFICIENTS IN THE NASA GLENN FORMAT')
      WRITE(6,2)
      RR = 8.314510D0
      R=RR/4.184D0
      Reng = RR*453.59237d0/(1.8d0*1055.056d0)
      DO 10 I=1,500
      CPR(I)=0.   
      temps(i)=0.d0
      DO 5 i=1.8
        output(i)=blank
5     CONTINUE
10    T(I)=0.    
      NT=0      
      do 13 i=1,5
        formla(1,i)=blank
        sel(i) = 0.
13    continue
      DO 16 I=1,3
16      TOUT(I)=.FALSE.
      plot = .FALSE.
      mfig=.FALSE.  
      calclk=.false.
      eng=.false.
      tconv=1.d0
C 
C  READ AND WRITE CONTENTS OF INPUT RECORD with output information.
C 
      READ (5,*,err=248,END=250) output
      WRITE (6,20) output
20    FORMAT (//,'  KEYWORDS:  ',8(a4,3x))

C
C     PROCESS OUTPUT RECORD
C 
      DO 25  I=1,8
      IF (output(i).EQ.'joules' .or. output(i).eq.'JOULES') THEN 
        tout(2) =.TRUE.
      ELSEIF (output(i).EQ.'cal' .or. output(i).eq.'CAL') THEN 
        TOUT(3) =.TRUE. 
      ELSEIF (output(i).EQ.'nodim' .or. output(i).eq.'NODIM') THEN
        TOUT(1) =.TRUE.
      ELSEIF (output(i).EQ.'logk' .or. output(i).eq.'LOGK') THEN
        OPEN (13,FILE='cap.elms',STATUS='old',FORM='formatted')
        calclk=.TRUE.
      ELSEIF (output(i).EQ.'mfig' .or. output(i).eq.'MFIG') THEN
        mfig=.TRUE.
      ELSEIF (output(i).EQ.'engr' .or. output(i).eq.'ENGR') THEN
        eng=.TRUE.
        tconv=1.8d0
      ELSEIF (output(i).EQ.'plot' .or. output(i).eq.'PLOT') THEN
        WRITE (*,22)
22      FORMAT (/,'  DATA FOR PLOTTING IN FILE plotout')
        PLOT=.TRUE.
      elseif (output(i).NE.' ') then
        write (6,23) output(i)
23      format (//,' *** ',a4,' NOT RECOGNIZED. KEYWORD SHOULD',
     &  ' BE ALL UPPER OR ALL LOWER CASE WITH NO BLANKS')
      ENDIF
25    CONTINUE
      if (.not.tout(2).and..not.tout(3)) then
        if (.not.tout(1)) then
          tout(2) = .true.
          if (.not.calclk ) mfig = .true.
        endif
      else
        if (.not.calclk )mfig =.true.
        if (eng) WRITE (6,26)
26      FORMAT(/'  NOTE TO USER:',/'  KEYWORDS ''JOULES'' AND ',
     *    '''CAL'' ARE IGNORED WHEN ''ENGR'' HAS BEEN SPECIFIED')          
      endif
      if (eng) then
        tout(2) = .true.
        WRITE(6,27)
27      format(/'  KEYWORD ''ENGR'' IMPLIES TEMPERATURES IN SCHEDULE',
     &  ' ARE IN DEGREES RANKINE')
        tout(3) = .false.
        tout(2) = .true.
      endif
        
c
c  Read temperature schedule.
c
      nts=0
      READ (5,*,err=248,end=250) tin
      do 35 i=1,500,2
      if (tin(i).lt. 0.001) go to 40
      i1=i+1
      nts=nts+1
      temps(nts)=tin(i)
      if (tin(i1).eq.0.)go to 35
30    tt=temps(nts) + tin(i1)
      if(tt.lt.tin(i+2)-.0001d0)then
         nts=nts+1
         temps(nts) = tt
         if(nts.le.500) go to 30
      endif
35    continue
40    do 45 i=1,nts
      temps(i) = temps(i)
      if(temps(i).gt.298.15d0*tconv-.005) go to 60
      if (temps(i+1).lt.298.15d0*tconv+.005) go to 45
      it=i+1
      go to 50
45    continue
      go to 60
50    nts=nts+1
      do 55 i=nts,it+1,-1
      temps(i)=temps(i-1)
55    continue
      temps(it)=298.15d0*tconv 
60    WRITE(6,65) (TEMPS(I),I=1,NTS) 
65    FORMAT(//'  TEMPERATURE SCHEDULE',//(6f12.3))
      WRITE (6,70)
70    FORMAT (//35x,   '***** NOTE *****',//3x,'The temperature range of
     * the coefficients read in the standard input (I/O unit 5)',/,3x,
     * 'will be extended by 20 %.  E.g. a range of 300 to 5000 K will be
     * extended to 240'/,3x,'to 6000 K. The extrapolated data may have '
     *,'large errors. The element data cap.elms'/,3x,'(I/O unit 13) ',
     * 'will NOT be extrapolated when calculating delta H and log K.'///
     * /)
      rec3=.false.
c
c   End Initialization.
c
80    nt=nts
      WRITE (6,82) 
82    FORMAT(//22x,'COEFFICIENTS FOR FITTED THERMODYNAMIC FUNCTIONS')
      do 85 i=1,nt
      t(i)=temps(i)/tconv
85    continue
c
      DO 90 i=1,20
      DO 90 J=1,8 
      COEF(J,I)=0.
      EX(J,I)=0. 
90    CONTINUE
      IX=1
      ITR=0    
      INA=1
      NNA=0
      if (rec3) go to 130
95    READ(5,100,end=260) (rec1(i),i=1,4),record
      if (rec1(1).eq.' ') goto 95
100   FORMAT(4a4,a64)
      READ(5,105,ERR=900,END=1000) nit,code,(fmla(1,I),fmla(2,I),
     * SL(I),I=1,5),ifz,molwt,htf
105   FORMAT (i2,a8,5(2a1,f6.2),i2,f13.5,f15.3)
      if (formla(1,1).eq.blank) go to 130
      if (ifaz.eq.0.or.ifz.le.ifaz) go to 120
      do 110 i=1,5
      if (fmla(1,i).ne.formla(1,i)) go to 120
      if (fmla(2,i).ne.formla(2,i)) go to 120
      if (sel(i).ne.sl(i)) go to 120
110   continue
      go to 130
120   rec3=.true.
      go to 260
130   do 140 i=1,5
      formla(1,i)=fmla(1,i)
      formla(2,i)=fmla(2,i)
      sel(i)=sl(i)
140   continue
      rec3=.false.
      nna=nna+1
      do 145 i=1,4
      name(i,nna)=rec1(i)
145   continue
      numint=nit
      ifaz=ifz
      htform=htf
      wt=molwt
c
      WRITE(6,150) (name(I,nna),I=1,4),record
150   FORMAT(//,6x,4a4,a64)
      WRITE(6,155) NUMINT,code,(FORMLA(1,i),FORMLA(2,i),sel(i),
     * I=1,5),ifaz,molwt,htform
155   FORMAT (6x,i2,a8,5(2a1,f6.2),i2,f13.5,f15.3)
      do 180 i=1,numint
      itr=itr+1
      READ(5,160,err=900,end=1000) tcoef(1,itr),tcoef(2,itr),numexp(itr) 
     * ,(ex(j,itr),j=1,8),enth29
160   FORMAT(2f11.3,i1,8f5.1,f17.3)
      WRITE(6,165) TCOEF(1,ITR),TCOEF(2,ITR),NUMEXP(ITR) 
     * ,(ex(j,itr),j=1,8),enth29
165   FORMAT(7x,2f10.3,i2,8f5.1,f17.3)
      READ (5,170,end=1000) (coef(j,itr),j=1,10)
170   FORMAT(5d16.9)
      WRITE (6,175) (coef(j,itr),j=1,10)
175   FORMAT(6x,1p,5d16.9)
180   continue
      ttrans(nna)=tcoef(2,itr)
      do 200 i=1,5
      if (sel(i).eq.0.) go to 95
      nkind=i
200   continue
      go to 95
C 
248   WRITE (6,249)
249   FORMAT (/' ****ERROR IN KEYWORDS OR TEMPERATURE SCHEDULE RECORDS.'
     & ,/,10x, 'DID YOU END RECORDS WITH A SLASH (/)?')
      GOTO 1000
250   WRITE (6,255)   
255   FORMAT (//' ****ERROR IN INPUT, GO TO NEXT SPECIES, (MAIN PGM)')
      GOTO 1000    
C 
C  INSERT TRANSITION POINTS IN CTEM SCHEDULE & EXTEND T RANGE.
C
260   tcoef(1,1)=.8d0*tcoef(1,1)
      tcoef(2,itr)=1.2d0*tcoef(2,itr)
      if(nna.le.1) go to 315
      it=1
      do 310 n=1,nna-1
      tt=ttrans(n)
c     IF ( tt.lt.temps(1) .or. tt.gt.temps(nts)) goto 310
      IF ( tt.lt.t(1) .or. tt.gt.t(nts)) goto 310
      do 270 i=it,nt
      if(tt.gt.t(i)) go to 270
      ii=i
      ix=2
      if(tt.eq.t(i))ix=1
      go to 280
270   CONTINUE
      go to 310
280   it=ii
      NT=NT+IX
      do 300 i=nt,it,-1
300   t(i)=t(i-ix)
      t(it)=tt
      t(it+1)=tt
310   continue
315   IT=0 
      hasind = .false.
C
      if(nt.le.0) go to 250
      hplot = (htform - enth29)/8.3145d0
320   IT=IT+1
322   TT=T(IT)
      IF (TT.GT.Tcoef(2,itr)) THEN
        nt = it - 1
        goto 360
      ENDIF
C
      DO 330 i=1,itr
        L=i
323     IF ( tt.LT.tcoef(1,L)) THEN
          nt=nt-1
          if(nt.le.it) goto 360
          do 324 j=it,nt
324       t(j)=t(j+1)
          tt= t(it)
          IF( tt.gt.tcoef(2,L)) goto 330
          goto 323
        ENDIF
        IF (TT.GE.Tcoef(1,L).AND.TT.LT.Tcoef(2,L)) GOTO 340
        IF (TT.EQ.Tcoef(2,L)) THEN
          IF (it.EQ.1) GOTO 340
          IF (TT.NE.T(it-1)) GOTO 340
        ENDIF
330   CONTINUE
      GOTO 360
340   CPR(IT)=0.
      HHRT(IT)=0.
      SR(IT)=0.
      GHRT(IT)=0.
      DO 350 J=1,numexp(L)
      EXL=EX(J,L)
      TEX=COEF(J,L)
      IF (EXL.NE.0.) TEX=TEX*TT**EXL  
      IF (EXL.EQ.-1.) HHRT(IT)=HHRT(IT)+COEF(J,L)*DLOG(TT)/TT
      IF (EXL.NE.-1.) HHRT(IT)=HHRT(IT)+TEX/(EXL+1.D0)
      CPR(IT)=CPR(IT)+TEX                           
      IF (EXL.EQ.0.) SR(IT)=SR(IT)+COEF(J,L)*DLOG(TT)
      IF (EXL.NE.0.) SR(IT)=SR(IT)+TEX/EXL     
350   CONTINUE                                  
      HHRT(IT)=HHRT(IT)+(COEF(9,L))/TT    
      SR(IT)=SR(IT)+COEF(10,L)                    
      GHRT(IT)=SR(IT)-HHRT(IT)                     
      IF (plot) THEN
        hh = hhrt(it) - hplot/TT
        gh = ghrt(it) + hplot/TT
        WRITE (18,355) TT,cpr(IT),hh,sr(it),gh,hhrt(it),ghrt(it)
355     FORMAT (1p,7e12.4)
      ENDIF
      IF (IT.LT.NT) GO TO 320
c
c  Store element numbers from BLOCK DATA in JF array.  If
c    species is a reference element, set relel = .true.
c
360   refel=.false.
      do 400 k=1,nkind
      jf(k)=0
      do 380 i=1,100
      do 370 j=1,2
      if (SYMBOL(j,i).ne.formla(j,k)) go to 380
370   continue
      jf(k)=i
      if(nkind.gt.1 .or. htform.ne.0.) go to 400
      n=sel(1)+.001
      if (n.ne.nmla(i)) go to 400
      if (efaz(i).eq.'G' .and. ifaz.ne.0) go to 400
      if (efaz(i).ne.'G' .and. ifaz.eq.0) go to 400
      refel=.true.
      go to 400
380   continue
      WRITE (6,390) formla(1,k),formla(2,k)
390   FORMAT('0*** Element ',2A1,' not found in BLOCK DATA')
      calclk = .false.
      go to 405
400   continue
c
405   if(refel.or.htform.ne.0.d0) go to 430
      if(t(1).gt.298.151 .or. t(nt).lt.298.149) go to 430
      do 410 i=1,nt
      if(dabs(t(i)-298.15d0).gt..0001)go to 410
      htform = hhrt(i)*rr*298.15d0
      go to 430
410   continue
C
430   term=(htform-enth29)/rr
      asindt=0.
      if (htform.ne.0. .or. refel) then
           hasind=.true.
           if(enth29.eq.0.) asindt=298.15d0
      else
           hasind=.false.
      endif
      IF (mfig.or.tout(1)) CALL TABLES
      IF (calclk) CALL LOGK
      if(rec3) go to 80
      goto 1000
C 
900   write (6,910)
910   FORMAT (//' ***ERROR IN FORMAT OF THE NEXT COEFFICIENT DATA SET -'
     & ,' MAYBE OLD FORMAT ')
1000  CLOSE (18)
      CLOSE (13)
      stop
      END
c
      SUBROUTINE LOGK                                                   
C
C  CALCULATE ROUNDED TABLES WITH DELTAH AND LOGK CALCULATIONS.
C
      DIMENSION PT(30),MARK(30),VFM2(15),idone(5)
C 
      DOUBLE PRECISION HZERO,RT,RUSE(2),hconv
      logical kdone
      DOUBLE PRECISION ALK(500),DH0,CP,S,H,HH,GH,DH(500),H0
      DOUBLE PRECISION HH0,DNMLA,EXL,TEX,TT,H298,dmla,TI
C
      CHARACTER*4 CHH,CHDH,BZ,VFM2,FSTAR,F2B,ZERO,FI6,ename(2,30)
      CHARACTER*4 FA4,F3X,F8X,F9,F12,FP2,FP3,FP4,fm(2),fms(2),f312,f224
C
      CHARACTER*4 BLANK,DATE,EFAZ,FAZ,FORMLA
      CHARACTER*4 NAME,SYMBOL
      COMMON /CHAR4/ BLANK,DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),NAME(4,9),SYMBOL(2,100)
C 
      DOUBLE PRECISION asindt,COEF,CPR,GHRT,htform,enth29,
     1 HHRT,R,RR,tconv,Reng,SR,T,tcoef,term,z,wt,WORD,TTRANS
      COMMON /REAL8/ asindt,COEF(10,20),CPR(500),
     1 GHRT(500),htform,enth29,HHRT(500),R,RR,tconv,Reng,
     2 SR(500),T(500),tcoef(2,20),term,z,wt,WORD(4),TTRANS(6)
C 
      COMMON /REAL4/ EX(8,20),sel(5),sels(5)
C 
      COMMON /INTGR/ IC80,itr,JF(5),
     1 numexp(20),NEL,NKIND,NKINDS,
     2 NMAT(100),NMLA(100),NOATMS,NT,NTMP,NFZ,INA,NNA
C 
      LOGICAL hasind,refel,TOUT,mfig,eng
      COMMON /LOGCL/ hasind,refel,TOUT(3),mfig,eng
C
c     EQUIVALENCE (IT,TI)
C 
      DATA  FSTAR/'2H *'/,F9/',F9'/,FP2/'.2'/, FI6/',I6'/,F3X/',3X'/
     1 ,FA4/',A4'/,FP4/'.4'/,FP3/'.3'/, ZERO/'0'/
     2 , F12/',F12'/, F8X/',8X'/, F2B/'2H  '/,f312/'3f12'/,f224/'2f24'/
C 
      DATA VFM2/'(', '2H  ', ',F9', '.2', ',F9', '.3,', 'F12', '.3',
     1   ',F12', '.3', ',F12', '.3',  ',F12', '.4', ')'/
C
      REWIND 13
      CHH=FSTAR
      CHDH=FSTAR
      RUSE(1)=RR
      RUSE(2)=R
      z=1.d0
      hconv = 1.d0/1000.d0
      if (eng) then
        z= 1.d0/wt
        RUSE(1) = Reng*z
        hconv = 1.8d0
        VFM2(6) = '.4,'
      endif
      IK=1     
      fms(1)=blank
      iend=0
      INIT=1
      NTX=nt
      if (hasind) then
         vfm2(7)=f312
      else
         vfm2(7)=f224
      endif
      VFM2( 9)=F12
      VFM2(10)=FP3
      DO 10 I=1,NT
      DH(I)=0.D0 
10    ALK(I)=0.D0
      DH0=0.D0
C
C  CHDH = BLANK OR ZERO - USE CHARACTER FOR DELTA H AND LOGK COLUMNS
C  CHH = BLANK - USE BLANK FOR H COLUMN
C
C  SEARCH I/O 13 FOR ELEMENT DATA FOR THIS MOLECULE.
C
      DO 20 I=1,NKIND
      idone(i)=-1
20    continue     
30    READ (13,40,END=210) nint,fm,fel
40    FORMAT(/,i2,8x,2a1,f6.2)
      do 50 i=1,nint
      READ (13,45,end=1000) tcoef(1,i),tcoef(2,i),numexp(i),
     *  (ex(j,i),j=1,8),hzero,(coef(j,i),j=1,10)
45    FORMAT(2f11.3,i1,8f5.1,f17.3,/(5d16.9))
50    continue
      DO 60 Ij=1,NKIND
      J=JF(Ij)       
      IF (fm(1).NE.FORMLA(1,Ij)) GO TO 60 
      IF (fm(2).NE.FORMLA(2,Ij)) GO TO 60
      ii=ij
      GO TO 70
60    CONTINUE
      GO TO 30
70    LL=1 
      hzero=-hzero/rr
      IF (refel) GO TO 210  
      DMLA=SEL(II)
      DNMLA=NMLA(J)
      if (idone(ii).lt.0) then
          idone(ii)=0
          DH0=DH0-HZERO*DMLA/DNMLA
          istart=0
      endif
      if (fm(1).ne.fms(1) .or. fm(2).ne.fms(2)) then
         if(fms(1).ne.blank) ntx=min(ntx,iend)
      endif
C 
      i1=idone(ii)+1
      L1=1
      DO 180 I=I1,NTX
      TT=t(i)
      HH=0.d0
      S=0.d0
      GH=0.d0
C 
C   FIND INTERVAL FOR T
C 
      do 100 LL=L1,nint
      if(TT.lt.tcoef(1,LL)-.0001 .or. TT.gt.tcoef(2,LL)+.0001)
     * go to 100
      if (istart.eq.0) then
         istart=i
         INIT=max(INIT,i)
      endif
      L=LL
      go to 110
100   continue
      go to 180
c
c           CALCULATE ELEMENT H AND G PROPERTIES
c
110   DO 120 J=1,numexp(L)
      EXL=EX(J,L)
      TEX=COEF(J,L)
      IF (EXL.NE.0.) TEX=TEX*TT**EXL
      IF (EXL.EQ.-1.) HH=HH+COEF(J,L)*DLOG(TT)/TT
      IF (EXL.NE.-1.) HH=HH+TEX/(EXL+1.D0)
      IF (EXL.EQ.0.) S=S+COEF(J,L)*DLOG(TT)
      IF (EXL.NE.0.) S=S+TEX/EXL
120   CONTINUE
      HH=HH+(COEF(9,L))/TT
      S=S+COEF(10,L)
      GH=S-HH
      iend=i
      idone(ii)=i
C 
C  CALCULATE DELTA H AND DELTA F 
C 
      DH(I)=DH(I)-HH*DMLA/DNMLA               
      ALK(I)=ALK(I)-GH*DMLA/DNMLA            
C 
C  SET T INDEX IN MARK. IF THIS END NOT THE END OF THE DATA, IT
C     IS A TRANSITION POINT FOR THIS ELEMENT.
C  IK IS THE INDEX FOR THE TRANSITION POINTS.
C 
      if(fm(1).eq.fms(1) .and. fm(2).eq.fms(2)) then
         MARK(IK)=I         
         PT(IK)=Tcoef(1,L)*tconv
         ename(1,ik)=fm(1)
         ename(2,ik)=fm(2)
         IF (IK.LT.30) IK=IK+1
         fms(1)=blank
      endif
180   CONTINUE                                 
         fms(1)=fm(1)
         fms(2)=fm(2)
      go to 30
C
210   DH0=DH0*hconv
      IK=IK-1
      IF (hasind.and.asindt.eq.0.)DH0= DH0 + term*hconv
      DO 220 I=1,NKIND                    
      IF (idone(I).lt.0) CHDH=BLANK             
220   CONTINUE                                 
      IF (refel) CHDH=ZERO               
C 
C  PRINT TABLES
C
      ntx = min(ntx,iend)
      kdone=.false.
      DO 530 NTABLE=1,2
      IF (.NOT.TOUT(NTABLE+1)) GO TO 530
      DH0=DH0*ruse(ntable)
c
      WRITE (6,230) ((NAME(I,J),I=1,3),J=1,nna)
230   FORMAT (//// 3x,'THERMODYNAMIC FUNCTIONS CALCULATED FROM COEFFICIE
     *NTS FOR ',9A4,/57X,2(3X,6A4))
      if (.not.eng) WRITE (6,240)
240   FORMAT (//'      T         Cp        H-H298        S      -(G-H298
     1)/T      H        delta Hf     log K')
      if (eng) WRITE (6,242)
242   FORMAT (//'      T         Cp        H-H537        S      -(G-H537
     1)/T      H       delta Hf     log K')
      IF (NTABLE.EQ.1) then
        if (.not.eng) WRITE(6,250)
        if (eng) WRITE(6,252)
      ELSEIF (NTABLE.EQ.2) then
        WRITE(6,255)
      ENDIF
250   FORMAT ('    deg-K    J/mol-K      kJ/mol     J/mol-K     J/mol-K
     *     kJ/mol      kJ/mol'/)
252   FORMAT('   Rankine  BTU/lb-deg    BTU/lb    BTU/lb-deg  BTU/lb-deg
     *   BTU/lb      BTU/lb'/)
255   FORMAT ('    deg-K   cal/mol-K    kcal/mol   cal/mol-K   cal/mol-K
     *    kcal/mol    kcal/mol'/)
      IF (.NOT.hasind.or.asindt.ne.0.0) GO TO 290
      H0=(term*RUSE(NTABLE))*hconv
      HH0=-enth29*hconv
      if (eng) hh0=hh0*RUSE(NTABLE)/rr
      if(ntable.eq.2)hh0=hh0/4.184D0
      IF (CHDH.EQ.BLANK .OR. CHDH.EQ.ZERO) GO TO 280
      WRITE (6,275) HH0,H0,DH0
275   FORMAT (6X,'0',8X,'0.',F15.3,7X,'0.',8X,'INFINITE',F11.3,F12.3
     * ,4X,'INFINITE')
      GO TO 290    
280   WRITE (6,285) HH0,H0,CHDH
285   FORMAT (6X,'0',8X,'0.',F15.3,7X,'0.',8X,'INFINITE',F11.3,
     * 8X,A4,4X,'INFINITE')
290   H=0.
      H298=htform
      if (eng) H298=H298 * RUSE(1)/rr
      if(ntable.eq.2)H298=H298/4.184d0
      IF (hasind) GO TO 298
      CHH=BLANK  
      CHDH=BLANK  
C
298   DO 420 I=1,NT
      TI=T(I) *tconv 
      IT= TI + 1.d-7
      VFM2(3)=FI6  
      VFM2(4)=F3X   
      if (dabs(dfloat(it)-ti).lt.1.d-6) go to 300
c     IF (DMOD(TI,1.D0).EQ.0.) GO TO 300
      VFM2(3)=F9  
      VFM2(4)=FP2
300   RT=RUSE(NTABLE)*T(I)
      H=HHRT(I)*RT*hconv
      CP=CPR(I)*RUSE(NTABLE)
      S=SR(I)*RUSE(NTABLE) 
      HH=(HHRT(I)*RT-H298)*hconv
      GH=(GHRT(I)*RUSE(NTABLE)+H298/T(I))
C
      VFM2(2)=F2B
       IF (IK.EQ.0) GO TO 320    
       DO 310 IX=1,IK
       IF (MARK(IX).EQ.I) VFM2(2)=FSTAR
310   CONTINUE                                       
320   BZ=ZERO                                         
      IF (CHDH.EQ.ZERO)GO TO 350                       
      BZ=BLANK                                          
      IF (CHDH.EQ.BLANK)GO TO 350                       
      IF ((I.GT.NTX.AND.NTX.NE.0).OR.I.LT.INIT) GO TO 350 
      if (kdone) then
          DH(I)=DH(I)/4.184D0               
      else
          DH(I)=(DH(I)+HHRT(I))*RT*hconv
          ALK(I)=(GHRT(I)+ALK(I))/2.3025851D0 
      endif
      VFM2(11)=F12
      if ( hasind) then
         VFM2(12)=FP3
         VFM2(13)=F12 
         VFM2(14)=FP4  
         IF(VFM2(3).EQ.F9)WRITE (6,VFM2)TI,CP,HH,S,GH,H,DH(I),ALK(I)
         IF(VFM2(3).EQ.FI6)WRITE (6,VFM2)IT,CP,HH,S,GH,H,DH(I),ALK(I)
      else
         VFM2( 9)=F12
         VFM2(10)=FP3
         VFM2(12)=FP4
         IF(VFM2(3).EQ.F9)WRITE (6,VFM2)TI,CP,S,H,DH(I),ALK(I)
         IF(VFM2(3).EQ.FI6)WRITE (6,VFM2)IT,CP,S,H,DH(I),ALK(I)
      endif
      GO TO 400      
350   VFM2(11)=F8X
      VFM2(12)=FA4
      if (hasind) then
         VFM2(13)=F8X
         VFM2(14)=FA4
         IF(VFM2(3).EQ.F9)WRITE (6,VFM2)TI,CP,HH,S,GH,H,BZ,BZ
         IF(VFM2(3).EQ.FI6)WRITE (6,VFM2)IT,CP,HH,S,GH,H,BZ,BZ
      else
         VFM2( 9)=F8X
         VFM2(10)=FA4
         IF(VFM2(3).EQ.F9)WRITE (6,VFM2)TI,CP,S,H,BZ,BZ
         IF(VFM2(3).EQ.FI6)WRITE (6,VFM2)IT,CP,S,H,BZ,BZ
      endif
C 
400   IF (DMOD(T(I),500.D0).NE.0.0) GO TO 420
      IF (T(I).GT.10000.D0.AND.DMOD(T(I),2500.D0).NE.0.) GO TO 420
      WRITE (6,410) 
410   FORMAT ( )
420   CONTINUE
C 
C     WRITE FOOTNOTE 
C 
      IF (IK.EQ.0) GO TO 510
      if (.not.eng) WRITE (6,440) (PT(I),I=1,IK)
440   FORMAT (//'   *Assigned reference phase change at ',               
     *  f8.2,' K',6(',',f8.2,' K'))
      if (eng) WRITE (6,450) (PT(I),I=1,IK)
450   FORMAT (//'   *Assigned reference phase change at ',               
     *  f8.2,' degR',6(',',f8.2,' degR'))
      LINES=LINES+4                      
510   WRITE (6,520)
520   FORMAT (/////)
      kdone = .true.
530   continue
      REWIND 13                        
1000  RETURN                          
      END                           
      SUBROUTINE TABLES
C 
C  WRITE OUTPUT FILE FOR THE FIRST 3 TABLES OF THERMODYNAMIC FUNCTIONS. 
C 
      REAL*8 RUSE(3),TI,HH29,GH29,HH,AR,ART,S,CP,GH,G,H,HR,h298,AHT
C
      CHARACTER*7 VFM(7)
C
      CHARACTER*4 BLANK,DATE,EFAZ,FAZ,FORMLA,NAME,SYMBOL
      COMMON /CHAR4/ BLANK,DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),NAME(4,9),SYMBOL(2,100)
C 
      DOUBLE PRECISION asindt,COEF,CPR,GHRT,htform,enth29,
     1 HHRT,R,RR,tconv,Reng,SR,T,tcoef,term,z,wt,WORD,TTRANS
      COMMON /REAL8/ asindt,COEF(10,20),CPR(500),
     1 GHRT(500),htform,enth29,HHRT(500),R,RR,tconv,Reng,
     2 SR(500),T(500),tcoef(2,20),term,z,wt,WORD(4),TTRANS(6)
C 
      COMMON /REAL4/ EX(8,20),sel(5),sels(5)
C 
      COMMON /INTGR/ IC80,itr,JF(5),
     1 numexp(20),NEL,NKIND,NKINDS,
     2 NMAT(100),NMLA(100),NOATMS,NT,NTMP,NFZ,INA,NNA
C 
      LOGICAL hasind,refel,TOUT,mfig,eng
      COMMON /LOGCL/ hasind,refel,TOUT(3),mfig,eng
C 
      RUSE(1)=R
      RUSE(2)=RR
      RUSE(3)=R
      H298=htform/rr
      if (eng) RUSE(2)=Reng
C 
C  FORMAT STATEMENT STORED IN VFM ARRAY
C 
      VFM(1) = '(F11.2,'
      VFM(2) = 'F12.5,'
C 
C  PRINT MFIG TABLES
C 
      DO 190 NTABLE=1,3
      AHT=asindt*tconv
      HR=term*RUSE(NTABLE) 
      IF (.NOT.TOUT(NTABLE)) GO TO 190
      WRITE (6, 65) ((NAME(I,J),I=1,3),J=1,nna)
65    FORMAT (//// 3x,'THERMODYNAMIC FUNCTIONS CALCULATED FROM COEFFICIE
     *NTS FOR ',9A4,/58X,2(3X,6A4))
C
      if (ntable.eq.1) then
          if(hasind) then
            if (.not.eng) WRITE(6,80)AHT,term  
80        FORMAT(///'        Assigned H(T)/R at',f7.2,' K =',F12.3,' K')
            if (eng) WRITE(6,81)AHT,term*tconv
81        FORMAT(///'        Assigned H(T)/R at',f7.2,' Rankine =',F12.3,
     1      ' Rankine')
          endif
          if (asindt .eq. 0.) then
            WRITE (6,85)   
85          FORMAT (//,7X,'T',9X,'Cp/R',6X,'(H-H0)/RT',7X,
     1      'S/R',6X,'-(G-H0)/RT',7X,'H/RT',9X,'-G/RT'/)
          else
            if(.not.eng) WRITE (6,86)
86          FORMAT (//,7X,'T',9X,'Cp/R',5X,'(H-H298)/RT',6X,
     1      'S/R',5X,'-(G-H298)/RT',6X,'H/RT',9X,'-G/RT'/)
            if(eng) WRITE (6,87)
87          FORMAT (//,7X,'T',9X,'Cp/R',5X,'(H-H537)/RT',6X,
     1      'S/R',5X,'-(G-H537)/RT',6X,'H/RT',9X,'-G/RT'/)
          endif
        do 66 i=3,5
66      VFM (i) = 'F13.7,'
        VFM (6) = 'F14.7,'
        VFM (7) = 'F14.7)'
      else
        VFM(3) = 'F13.3,'
        VFM(4) = 'F13.6,'
        VFM(5) = 'F13.3,'
        VFM(6) = 'F14.3,'
        VFM(7) = 'F14.3)'
        if (eng) then
          VFM(3) = 'F13.4,'
          VFM(5) = 'F13.4,'
          VFM(6) = 'F14.4,'
          VFM(7) = 'F14.4)'
        endif
      endif
C
      IF (.not.hasind) then
        WRITE (6,70)                
70        FORMAT (//'           NO HZERO VALUE IS AVAILABLE')
          VFM(3) = '13x,'
          VFM(5) = '13x,'
      ENDIF
C
C
        IF(NTABLE.EQ.2) then
          if(hasind) then
            if(.not.eng) WRITE(6,90) AHT,HR
90          FORMAT(///'     Assigned H(T) at',f8.2,' K =',F12.3,
     *      ' J/mol')
            if(eng) WRITE(6,91) AHT,HR*tconv/wt
91          FORMAT(///'     Assigned H(T) at',f8.2,' Rankine =',F12.3,
     *      ' BTU/lb')
          endif
            if (asindt .eq. 0.) then
            WRITE (6,100) 
100         FORMAT (/,7X,'T',11X,'Cp',10X,'H-H0',9X,'S',9X
     1      ,'-(G-H0)',10X,'H',12X,'-G')
          else 
            if (.not.eng) WRITE (6,101) 
101         FORMAT (/,7X,'T',11X,'Cp',9X,'H-H298',8X,'S',8X
     1      ,'-(G-H298)',9X,'H',12X,'-G')
            if (eng) WRITE (6,102) 
102         FORMAT (/,7X,'T',11X,'Cp',9X,'H-H537',8X,'S',8X
     1      ,'-(G-H537)',9X,'H',12X,'-G')
          endif
          if (.not.eng) WRITE (6,115)
115       FORMAT(6X,'deg K',5X,'J/mol-K',7X,'J/mol',6X,
     *    'J/mol-K',7X,'J/mol',9X,'J/mol',9X,'J/mol'/)
          if (eng) WRITE (6,116)
116       FORMAT(4X,'Rankine',3X,'BTU/lb-deg',6X,'BTU/lb',4X,
     *    'BTU/lb-deg',5X,'BTU/lb',8X,'BTU/lb',8X,'BTU/lb'/)
C
        ELSEIF(NTABLE.EQ.3) then
          if(hasind) WRITE(6,120) AHT,HR 
120       FORMAT(///'    Assigned H(T) at',f8.2,' K =',F12.3,' cal/mol')
          if (asindt .eq. 0.) then
            WRITE (6,100) 
          else
            WRITE (6,101)
          endif
          WRITE(6,130)
130    FORMAT(6X,'deg-K',4X,'cal/mol-K',5X,'cal/mol',4X,
     * 'cal/mol-K',6X,'cal/mol',6X,'cal/mol',7X,'cal/mol'/)
        ENDIF
C
      DO 180 I=1,NT
      TI=T(I)*tconv
      IF(DABS(TI-298.15D0*tconv).LT..02)TI=298.15D0*tconv
      CP=CPR(I)                      
      H=HHRT(I)                     
      S=SR(I)                      
      G=GHRT(I)                   
      IF (NTABLE.NE.1)GO TO 140  
      AR=1./T(I)
      ART=1.                   
      GO TO 150               
140   AR=RUSE(NTABLE)
      ART=AR*TI
      CP=CP*AR                    
      H=H*ART                  
      S=S*AR                    
      G=G*ART                
      AR=AR*tconv
C
150   HH=H-term*AR 
      GH=G+term*AR 
      HH29= H-H298*AR 
      GH29= G+H298*AR 
      z=1.d0
      if (eng .AND. NTABLE.EQ.2) z=1.d0/wt
      if (hasind) then
         IF (asindt.ne.0.) then
            WRITE (6,VFM) TI,CP*z,hh29*z,s*z,gh29*z,h*z,g*z
         else
            WRITE (6,VFM) TI,CP*z,hh*z,s*z,gh*z,h*z,g*z
         endif
      else
         WRITE (6,vfm) TI,cp*z,s*z,h*z,g*z
      endif
180   CONTINUE                         
      WRITE (6,185)
185   FORMAT (/////)
      if ( .not.mfig ) goto 200
190   CONTINUE
200   RETURN
      END
c
      BLOCK DATA                                                        
C 
      CHARACTER*4 BLANK,DATE,EFAZ,FAZ,FORMLA
      CHARACTER*4 NAME,SYMBOL
      COMMON /CHAR4/ BLANK,DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),NAME(4,9),SYMBOL(2,100)
C 
      DOUBLE PRECISION asindt,COEF,CPR,GHRT,htform,enth29,
     1 HHRT,R,RR,tconv,Reng,SR,T,tcoef,term,z,wt,WORD,TTRANS
      COMMON /REAL8/ asindt,COEF(10,20),CPR(500),
     1 GHRT(500),htform,enth29,HHRT(500),R,RR,tconv,Reng,
     2 SR(500),T(500),tcoef(2,20),term,z,wt,WORD(4),TTRANS(6)
C 
      COMMON /REAL4/ EX(8,20),sel(5),sels(5)
C 
      COMMON /INTGR/ IC80,itr,JF(5),
     1 numexp(20),NEL,NKIND,NKINDS,
     2 NMAT(100),NMLA(100),NOATMS,NT,NTMP,NFZ,INA,NNA
C 
      LOGICAL hasind,refel,TOUT,mfig,eng
      COMMON /LOGCL/ hasind,refel,TOUT(3),mfig,eng
C 
C    CONSTANTS FROM THE 1986 CODATA RECOMMENDED VALUES OF THE FUND.
C    PHYSICAL CONSTANTS; J OF RES NAT BUR STDS; COHEN AND TAYLOR;
C    VOL 92, P 85, 1987.
C
      DATA SYMBOL/  'E',' ','H',' ','D',' ','H','E','L','I','B','E','B',
     1  ' ','C',' ','N',' ','O',' ','F',' ','N','E','N','A','M','G','A',
     2  'L','S','I','P',' ','S',' ','C','L','A','R','K',' ','C','A','S',
     3  'C','T','I','V',' ','C','R','M','N','F','E','C','O','N','I','C',
     4  'U','Z','N','G','A','G','E','A','S','S','E','B','R','K','R','R',
     5  'B','S','R','Y',' ','Z','R','N','B','M','O','T','C','R','U','R',
     6  'H','P','D','A','G','C','D','I','N','S','N','S','B','T','E','I',
     7  ' ','X','E','C','S','B','A','L','A','C','E','P','R','N','D','P',
     8  'M','S','M','E','U','G','D','T','B','D','Y','H','O','E','R','T',
     9  'M','Y','B','L','U','H','F','T','A','W',' ','R','E','O','S','I',
     $  'R','P','T','A','U','H','G','T','L','P','B','B','I','P','O','A',
     $  'T','R','N','F','R','R','A','A','C','T','H','P','A','U',' ','N',
     $  'P','P','U','A','M','C','M','B','K','C','F'/                    
C 
      DATA EFAZ/4*'G',4*'S',4*'G',6*'S',2*'G',12*'S','L',3*'S','S','G', 
     1 17*'S','G',25*'S','L',5*'S','G',12*'S'/, BLANK/'    '/           
C 
      DATA NMAT/0,1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20, 
     1 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,     
     2 41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,     
     3 61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,     
     4 81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98/           
C 
      DATA NMLA/ 1,2,2,5*1,3*2,7*1,2,17*1,2,17*1,2,45*1/                
      END
