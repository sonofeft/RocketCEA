C***********************************************************************
C                      P R O G R A M      P A C 9 9
C
C           PROPERTIES AND COEFFICIENTS FOR INDIVIDUAL SPECIES
C***********************************************************************
C
C   Nov.22,1994 - EFTAPE - Remove restriction that "EFDA" with no data
C                 must end set of EFdata input.  
C   Mar.10,1995 - Change dimensions in WCOMMN to accomodate x's and y's
C                 for 9 nus rather than 6.
C   Mar.23,1995 - Put correction in EFTAPE.  We were getting multiple
C                 sets of data for MG which is in the sample problems.
C   Sep.20,1996 - Error in plot dump stuff.
C   Oct.4,1996  - Error corrected for nal>0 in ATOM.
C   Oct.30.1996 - Add CPONLY fitting option. This option is similar to
C                 the NOH and NOS combination but the original data are
C                 not replaced with data calculated using the Cp-fit
C                 coefficients.
C   Nov.4, 1996 - ATOM. Remove EQUIVALENCE statement and increase
C                 dimensions of Q, TDQDT, XTDQDT. There was a problem
C                 exceeding dimensions with the nal FILL option.
C   Nov.15,1996 - LOGK. To correct delta h and log K calculations
C                 when 'joules' tables are not called for.
C   Dec.19,1996 - main. Correct statement 560 for CTAB option.
C   Dec.19,1996 - RECO. Statement setting NF moved because it was not
C                 always being set.
C   Dec.27,1996 - POLY. Disallow theta corrections with IAIBIC input.
C                 Variable calcth added.
C   Jan. 6,1997 - LOGK. Logk and Deltah were wrong for some 
C                 combinations of CAL and JOULES in OUTP record.
C   Feb 11,1997 - WILHOI. Add 'TSTA' to METH record.
C   May  7,1997 - Corrected comman alignment. Errors were discovered
C                 when PAC97 was compiled on SGI.
C   Jul 25,1997 - ATOM. Increased dim of levels from 600 to 800.
C   Aug 20,1997 - ATOM. Add BETHE method.
C   Oct  3,1997 - POLY. Add additional check for number of vibs.
C   Oct  8,1998 - Change number of temperatures for property tables
C                 from 202 to 502.
C   Dec 24,1998 - Main. Add 'H0' and 'HF0' label options on formula line
C   Jan  4,1999 - EFTAPE. Add third decimal place for T in EFdata.
C   May 25,1999 - BLOCKDATA. New atomic weights.
C   June 4,1999 - PUNCH. Use f13.7 instead of f13.5 for molecular wt.
C   July29,1999 - Disable test for completed LS before *.plt dump.
C   Feb 1, 2000 - IROTOR. Made IxB double precision, eg 2.7992774d0.
C   Sep 5, 2000 - RECO. Correct H298-H0 when changing energy units.
C   Dec19, 2000 - COMMON. CPONLY(8) --> CPONLY(9).
C   Dec20, 2000 - COMMON. T(502) --> T(0:502).
C   Feb27, 2001 - LINK1. Change max test on QLNTOT from 88.d0 to 700.d0
C   Oct12, 2001 - ATOM. Limit no. of atomic levels to 900.
C   Jan31, 2002 - IROTOR and HMAT.  Tried to change maximum number of potential
C                 terms for hindered rotors from 6 to 8. Also number of B's
C                 from 4 to 6.  HMAT is general and has the most changes.
C   Jun27, 2002 - RECO. The K in KCAL and KJOULE were being ignored on METH 
C                 record when enthalpies were absolute (follow H).
C   Jul02, 2002 - HMAT. Add HINT to DOUBLE PRECISION list as required by
C                 single precision machines.
C   Sep06, 2002 - Minor changes including I/O variable names throughout.
C   Oct18, 2002 - BLOCKDATA. New atomic wts with N=14.0067. Most data in
C                 were corrected to these wts.  A few species were not.
C
      PROGRAM PAC99
C
C  PROPERTIES AND COEFFICIENTS CODE BY BONNIE MCBRIDE, NASA LEWIS
C
C     TEST(1)  LEAST SQUARES COEFFICIENTS HAVE BEEN CALCULATED.
C     TEST(2)  MOLECUTE IS LINEAR
C     TEST(3)  SPECIES IS AN ION.
C     TEST(4)  SPECIES IS A GAS.
C     TEST(5)  SPECIES IS A LIQUID.
C     TEST(6)  SPECIES IS A SOLID.
C     TEST(7)  METHOD RECORD HAS BEEN READ
C     TEST(8)  AN ASSIGNED H IS AVAILABLE
C     TEST(9)  CP/R, H-H0/RT, AND S/R ARE READY TO BE OUTPUTTED
C     TEST(10) SPECIES TO BE REFERENCE ELEMENT FOR HEAT-OF-FORMATION
C              AND LOGK CALCULATIONS.  REVISE unf.EFdata file.
C     TEST(12) LOG K CALLED FOR
C     TEST(13) DATA ARE IN THE FORM, H-H298 AND -(F-H298)
C     TEST(14) INTERMEDIATE OUTPUT CALLED FOR
C     TEST(15) LEAST SQUARES CALLED FOR
C     TEST(16) ERROR IN INPUT.  GO TO NEXT SPECIES
C     TEST(17) WRITE OUT READ-IN COEFFICIENTS  (I/O UNITS 12 & 16)
C     TEST(18) ENTHALPY IS ABSOLUTE
C     TEST(19) SPECH IS SET
C     TEST(20) TEMPERATURE SCHEDULE HAS BEEN STORED
C
      SAVE
      PARAMETER (IOCFT=10,IOELM=11,IOEFT=13,IOSCH=14,IOPLT=16)
      PARAMETER (IOUSCH=17,IOGRP=19)
      LOGICAL CFOUT,TAB8,exs,plot
      DOUBLE PRECISION CONV,EXL,TEX,TT,TCWD(32),cpsave,difcp,hsave
      DOUBLE PRECISION ssave
C
      CHARACTER*4 TCLB(32)
      CHARACTER*4 ORIG,COFS
C
      DIMENSION ORIG(2),COFS(4)
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      CHARACTER*200 prefix*196,infile,ofile,pfile,cfile,efile
      DATA COFS/'COEF','FICI','ENTS',' '/, ORIG/'ORIG','INAL'/
      WRITE (*,5)
 5    FORMAT (//' ENTER PREFIX ONLY FOR INPUT FILE, SUFFIX SHOULD BE', 
     &' .i97'/' OUTPUT FILES WILL HAVE THE SAME PREFIX WITH SUFFIXES:',
     & //,5x,'.o97  FOR STANDARD OUTPUT (I/O UNIT 6)',
     & /,5x,'.c97  FOR COEFFICIENTS IN FORMAT REQUIRED FOR CEA',
     & /,5x,'.p97  FOR DATA DUMPED FOR PLOTTING',
     & /,5x,'.e97  FOR ELEMENT DATA USED IN EFDATA FILE'//)
      READ (*,10) prefix
 10   FORMAT (a)
      ln = INDEX(prefix,' ') - 1
      infile = prefix(1:ln)//'.i97'
      ofile = prefix(1:ln)//'.o97'
      pfile = prefix(1:ln)//'.p97'
      cfile = prefix(1:ln)//'.c97'
      efile = prefix(1:ln)//'.e97'
      INQUIRE (FILE=infile,EXIST=exs)
      IF ( .NOT.exs ) THEN
        PRINT *, ' NONEXISTENT FILE ',infile
        STOP 
      ENDIF
      OPEN (5,FILE=infile,STATUS='old',FORM='formatted')
      OPEN (6,FILE=ofile,STATUS='unknown',FORM='formatted')
      OPEN (IOSCH,STATUS='scratch',FORM='formatted')
      OPEN (IOUSCH,STATUS='scratch',FORM='unformatted')
      OPEN (IOGRP,FILE='new.groups',STATUS='old',FORM='formatted')
      OPEN (IOEFT,FILE='unf.EFdata',FORM='unformatted')
      OPEN (IOCFT,FILE=cfile,FORM='formatted')
      WRITE (6,99001)
      WRITE (6,99002)
      WRITE (6,99001)
C
C  INITIALIZE ONCE.
C
      NEL=0
      R=RR/4.184D0
      REWIND IOSCH
      LINES=16
      GO TO 25
C
C  INITIALIZATION FOR EACH SPECIES.  FOLLOWS FINISH RECORD.
C
20    LINES=6
      mtab=-1
      idone=0
25    DO 30 I=1,4
30    SNM(I)=' '
      DO 40 I=1,502
      CPR(I)=0.
40    T(I)=0.
      DATE(1)=' '
      DATE(2)=' '
      TLST=0.
      NT=0
      INA=1
      NNA=0
      DO 60 I=1,19
      DO 55 K=1,6
55    NAME(I,K)=' '
60    TEST(I)=.FALSE.
      TEST(20)=.FALSE.
      DO 65 I=1,3
65    TOUT(I)=.FALSE.
      TAB8=.FALSE.
      plot=.FALSE.
      HEAT=' '
      TINTVL=0.0
      NIT=1
      NTMP=1
      NNN=1
      ASINDT=0.0
      ASINDH=0.0D0
      SPECH=0.0
      H298HR=0
      H298H0=0
      intvls=0
      ivl1=0
      NFZ=0
      PI=0.
      PTMELT=0.0
      CNST=.TRUE.
      CFOUT=.FALSE.
      NTCF=0
      TCONST=0.
c
      DO 70 I=1,9
      TRANGE(I)=0.
      NF(I)=0
      NOCP(I)=.FALSE.
      NOH(I)=.FALSE.
      NOS(I)=.FALSE.
      CPONLY(i) = .FALSE.
      DO 70 J=1,10
      COEF(J,I)=0.
70    EX(J,I)=0.
c
      OLD=.FALSE.
      IX=1
      ITR=0
      NTRANG=0
      STD=' '
      BAR=.TRUE.
      SCONST=SBAR
C
C  CALL INPUT TO READ AND WRITE CONTENTS OF ONE INPUT RECORD.
C
80    CALL INPUT
C
C     CHECK RECORD IDS.
C
90    IF (CODE(1).EQ.'FINI') GO TO 490
      IF (CODE(1).EQ.'LIST') GO TO 170
      IF (CODE(1).EQ.'ATM' ) GO TO 155
      IF (CODE(1).EQ.'OUTP') GO TO 210
      IF (CODE(1).EQ.'PHAS') GO TO 230
      IF (CODE(1).EQ.'INTE') GO TO 280
      IF (CODE(1).EQ.'DATE') GO TO 260
      IF (CODE(1).EQ.'TEMP') GO TO 300
      IF (CODE(1).EQ.'PINT') GO TO 330
      IF (CODE(1).EQ.'METH') GO TO 340
      IF (CODE(1).EQ.'EFTA') GO TO 290
      IF (CODE(1).EQ.'LOGK') GO TO 310
      IF (CODE(1).EQ.'REFN'.OR.CODE(1).EQ.' '.OR.CODE(1).EQ.'NAME') GOTO
     * 80
      IF (CODE(1).EQ.'NOLE') GO TO 160
      IF (CODE(1).EQ.'LSTS') GO TO 670
      IF (CODE(1).EQ.'EFDA') GO TO 200
      IF (CODE(1).EQ.'CTEM') GO TO 860
      IF (CODE(1).EQ.'STOP') GOTO 900
C
C  IF COLS 1-4 CONTAIN NO RECOGNIZABLE CODE, ASSUME formula RECORD.
C  CALL IDENT TO ANALYZE FORMULA
C
      CALL IDENT
      IF (TEST(16)) GO TO 420
C
C  STORE HEAT OF REACTION AND ASSIGNED T FROM FORMULA RECORD
C
      HEAT = ' '
      DO 120 I=2,4
      IF (ABEL(1,I).EQ.'T') THEN
        ASINDT = WORD(I)
      ELSEIF (ABEL(1,I).EQ.'DELT'.OR.ABEL(1,I).EQ.'HF0'.OR.
     &  ABEL(1,I).EQ.'Hf0') THEN
        HEAT = 'DELT'
        ASINDH = WORD(I)
      ELSEIF (ABEL(1,I).EQ.'ASIN'.OR.ABEL(1,I).EQ.'H0'.OR.
     &  ABEL(1,I).EQ.'HF29'.OR.ABEL(1,I).EQ.'Hf29') THEN 
        HEAT = 'ASIN'
        ASINDH = WORD(I)
        IF (ABEL(1,I).EQ.'HF29'.OR.ABEL(1,I).EQ.'Hf29') 
     &    ASINDT = 298.15D0
      ENDIF
120   CONTINUE
      IF (HEAT.NE.' '.AND.ASINDT.EQ.0.) TEST(19)=.TRUE.
      IF (HEAT.EQ.'ASIN'.AND.ASINDT.EQ.0.) TEST(8)=.TRUE.
C
C  CONVERT HEAT OF REACTION TO PROPER UNITS IF NECESSARY.
C
      CONV=1.D0
      DO 140 I=2,4
      IF (ABEL(1,I).EQ.'KCAL'.OR.ABEL(1,I).EQ.'KJOU') CONV=1000.D0
      IF (ABEL(1,I).EQ.'EV') CONV=23060.541D0
      IF (R.GT.8.D0.AND.(ABEL(1,I).EQ.'CAL'.OR.CONV.NE.1.)) CONV=
     1  CONV*4.184D0
      IF (ABEL(1,I).EQ.'INVC') CONV=R*HCK
      IF (R.GT.2.D0) GO TO 130
      IF (ABEL(1,I).EQ.'JOUL'.OR.ABEL(1,I).EQ.'KJOU')
     1  CONV=CONV/4.184D0
130   IF (CONV.NE.1.D0) GO TO 150
140   CONTINUE
      IF (R.GT.8.D0) CONV=CONV*4.184D0
150   ASINDH=ASINDH*CONV/R
      GO TO 80
C
155   IF (.NOT.TEST(4)) GO TO 80
      BAR=.FALSE.
      STD='ATM'
      SCONST=SATM
      GO TO 80
160   TEST(15)=.FALSE.
      GO TO 80
170   CALL EFLIST
      GO TO 80
 200  OPEN (IOELM,FILE=efile,FORM='formatted')
      CALL EFTAPE
      GO TO 80
C
C     PROCESS 'OUTP' RECORD
C
210   DO 220 I=1,4
        IF (ABEL (1,I).EQ.'JOUL') THEN
          TOUT (2)=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'CAL') THEN
          TOUT (3)=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'DMLE') THEN
          TOUT (1)=.TRUE.
          TAB8=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'CTAB') THEN
          CFOUT=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'LOGK') THEN
          TEST(12)=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'INTE') THEN
          TEST (14)=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'ATM'.AND.TEST(4)) THEN
          BAR=.FALSE.
          STD='ATM'
          SCONST=SATM
        ELSEIF (ABEL (1,I).EQ.'MFIG') THEN
          TAB8=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'EFTA') THEN
          TEST (10)=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'LSQS'.OR.ABEL (1,I).EQ.'LSTS') THEN
          TEST (15)=.TRUE.
        ELSEIF (ABEL (1,I).EQ.'PLOT') THEN
          plot = .TRUE.
          OPEN (IOPLT,FILE=pfile,FORM='formatted')
        ENDIF
220   CONTINUE
      GO TO 80
C
C  CONDENSED PHASE THAT CANNOT FIT ON FORMULA RECORD.  PUT L FOR LIQ OR
C     ANY CHARACTER FOR SOLID.  USE COLUMN 7,25,43,OR 61.
C
230   TEST(4)=.FALSE.
      DO 240 I=1,4
      IF (ABEL(1,I).EQ.' ') GO TO 240
      FAZ=ABEL(1,I)
      IF (FAZ.EQ.'L') TEST(5)=.TRUE.
      IF (FAZ.NE.'L') TEST(6)=.TRUE.
240   CONTINUE
      GO TO 80
C
C  STORE OPTIONS.
C
260   DO 270 IJ=1,4
      IF (ABEL(1,IJ).EQ.' ') GO TO 270
      DATE(1)=ABEL(1,IJ)
      DATE(2)=ABEL(2,IJ)
270   CONTINUE
      GO TO 80
280   TEST(14)=.TRUE.
      GO TO 80
290   TEST(10)=.TRUE.
      GO TO 80
300   CALL TEMPER
      TEST(20)=.TRUE.
      GO TO 80
310   TEST(12)=.TRUE.
      GO TO 80
C
330   mtab=1
      DO 335 I=1,4
        IF (ABEL(1,I).EQ.'JOUL') mtab=2
        IF (ABEL(1,I).EQ.'CAL') mtab=3
335   CONTINUE
      GO TO 80
C
C  METHOD RECORD HAS BEEN READ.
C
340   IF(BAR.AND.TEST(4)) STD='BAR'
      DO 350 I=1,4
      IF (ABEL(1,I).EQ.' ') GO TO 350
      IF (ABEL(1,I).EQ.'READ'.OR.ABEL(1,I).EQ.'COEF') CODE(1)=ABEL(1,I)
      IF (ABEL(1,I).EQ.'ADD'.OR.ABEL(1,I).EQ.'WILH') CODE(1)=ABEL(1,I)
      IF (ABEL(1,I).EQ.'NOAT') NOATMS=WORD(I)
      IF (ABEL(1,I).EQ.'IP') PI=WORD(I)
350   CONTINUE
      IF (CODE(1).NE.'WILH') GO TO 355
      IF (TEST (9)) THEN
      CALL WILHOI
      ELSE
      WRITE (6,*) 'NO DATA FOR WILHOIT EXTRAPOLATION'
      ENDIF
      GO TO 80
355   IF (TEST(15).OR.TEST(17)) IX=IX+1
      TEST(7)=.TRUE.
      IF (NT.NE.0 .OR. CODE(1).EQ.'READ') GO TO 370
C
C  STORE STANDARD T SCHEDULE IF NO TEMP RECORDS HAVE BEEN READ.
C
      IF (TEST(3)) GO TO 362
      T(1)=100.0
      T(2)=200.0
      T(3)=298.15D0
      T(4)=300.0
      DO 360 NT=5,61
360   T(NT)=T(NT-1)+100.0
      NT=61
      GO TO 370
C
362   T(1)=298.15D0
      T(2)=300.
      NT=59
      DO 364 I=3,59
364   T(I)=T(I-1)+100.D0
C
C      CALL RECO FOR READIN OR COEF METHODS
C      CALL ATOM FOR MONATOMIC GASES
C      CALL POLY FOR DIATOMIC OR POLYATOMIC GASES
C      CALL GROUP FOR GROUP ADDITIVITY PROGRAM
C
370   IF (CODE(1).NE.'ADD') GO TO 380
      STD=' '
C
C      ENTROPY DIFFERENCE FOR BAR AND ATMOSPHERE IGNORED
C
      CALL GROUP
      GO TO 470
380   IF (CODE(1).NE.'READ'.AND.CODE(1).NE.'COEF') GO TO 390
      CALL RECO
      GO TO 480
390   IF ((HCK.EQ.0.).OR.WEIGHT.EQ.0.) GO TO 400
      IF (NOATMS.EQ.1) GO TO 450
      IF (NOATMS.GE.2) GO TO 460
C
400   WRITE (6,410)
410   FORMAT (/,' ERROR IN INPUT, GO TO NEXT SPECIES, (MAIN PGM)')
C
420   IF (CODE(1).EQ.'FINI') GO TO 440
      READ (5,430) CODE(1)
430   FORMAT (A4)
      GO TO 420
440   TEST(16)=.FALSE.
      LINES=LINES+2
      GO TO 20
450   CALL ATOM
      GO TO 470
460   CALL POLY
470   NIT=NT+1
480   IF (TEST(16)) GO TO 400
      TEST(7)=.FALSE.
      GO TO 90
C
490   IF (TEST(9)) GO TO 510
      WRITE (6,500)
500   FORMAT(/' CP/R,(H-H0)/RT,AND S/R ARE NOT READY FOR OUTPUT (MAIN)')
      GO TO 20
510   NLAST=NT
C
C  CALL DELH TO CALCULATE H0 IF NECESSARY.  DELH WILL CALL LEAST FOR
C      LEAST SQUARES FIT IF OPTION HAS BEEN REQUESTED.
C
      IF (NNN.LT.NLAST) CALL DELH
      SNM(1)=ORIG(1)
      SNM(2)=ORIG(2)
      SNM(3)=' '
      SNM(4)=' '
C
C  CALL TABLES TO PRINT FIRST TWO TABLES OF FUNCTIONS.
C
      CALL PAGEID
      if (.not.tout(1).and..not.tout(3)) tout(2)=.true.
      IF (TAB8) CALL TABLES
C
C  FOR EFTAPE OPTION, CALL EFTAPE TO PUNCH EF DATA AND PUT DATA ON TAPE.
C
      IF (TEST(10)) then
        OPEN (IOELM,FILE=efile,FORM='formatted')
        CALL EFTAPE
      ENDIF
C
C     IF LOGK OPTION, CALL LOGK TO CALCULATE DELTAH AND LOG K AND
C      PRINT TWO TABLES OF PROPERTIES.
C
      IF (TEST(12)) CALL LOGK
      IF (.NOT.CFOUT.AND..NOT.plot) GO TO 20
C  Following statement disabled 7/29/99
C     if (test(15) .and. .not.TEST(1)) go to 20
      DO 550 I=1,4
      SNM(I)=COFS(I)
550   ABEL(1,I)=' '
      IF ( plot ) GO TO 615
560   IF ( NTCF.EQ.0 ) GOTO 615
      IJ=0
      NT=0
      DO 600 J=1,NTCF
      IJ=IJ+1
      WORD(IJ)=TCWD(J)
      ABEL(1,IJ)=TCLB(J)
      IF (J.LT.NTCF) GO TO 580
      DO 570 IJ=IJ+1,4
570   ABEL(1,IJ)=' '
      GO TO 590
580   IF (IJ.LT.4) GO TO 600
590   CALL TEMPER
      IJ=0
600   CONTINUE
C
C  INSERT TRANSITION POINTS IN CTEM SCHEDULE
C
      if(nna.le.1) go to 615
      it=1
      do 610 n=1,nna-1
        tt=ttrans(n)
        do 604 i=it,nt-1
          IF( tt.le.t(i) ) THEN
            ii=i
            ix=2
            if(tt.eq.t(i))ix=1
          ENDIF
604     continue
        it=ii
        NT=NT+IX
        do 608 i=nt,it,-1
          t(i)=t(i-ix)
608     continue
        t(it)=tt
        t(it+1)=tt
610   continue
C
615   IT=0
      L=1
620   IT=IT+1
622   TT=T(IT)
      if (tt.ge.tc(L)) go to 630
      nt=nt-1
      if(nt.le.it) go to 665
      do 624 i=it,nt
      cpr(i)=cpr(i+1)
624   t(i)=t(i+1)
      go to 622
630   IF (TT.LT.TC(L+1).OR.L.EQ.intvls) GO TO 640
      if(tt.eq.tc(l+1) .and. tt.ne.t(it-1)) go to 640
      L=L+1
      IF (L.LE.intvls) GO TO 630
      nt=it-1
      GO TO 665
640   cpsave = CPR(IT)
      hsave = HHRT(IT)
      ssave = SR(IT)
      CPR(IT)=0.
      HHRT(IT)=0.
      SR(IT)=0.
      GHRT(IT)=0.
      NFL=NF(L)
      DO 650 J=1,NFL
      EXL=EX(J,L)
      TEX=COEF(J,L)
      IF (EXL.NE.0.) TEX=TEX*TT**EXL
      IF (EXL.EQ.-1.) HHRT(IT)=HHRT(IT)+COEF(J,L)*DLOG(TT)/TT
      IF (EXL.NE.-1.) HHRT(IT)=HHRT(IT)+TEX/(EXL+1.D0)
      CPR(IT)=CPR(IT)+TEX
      IF (EXL.EQ.0.) SR(IT)=SR(IT)+COEF(J,L)*DLOG(TT)
      IF (EXL.NE.0.) SR(IT)=SR(IT)+TEX/EXL
650   CONTINUE
      HHRT(IT)=HHRT(IT)+(COEF(9,L)-ASINDH)/TT
      SR(IT)=SR(IT)+COEF(10,L)
      GHRT(IT)=SR(IT)-HHRT(IT)
      IF ( plot ) THEN
        difcp = cpsave - CPR(IT)
        WRITE ( IOPLT,99010 ) T(IT),cpsave,CPR(IT),hsave,HHRT(IT),
     &           ssave,SR(IT)
      ENDIF
      IF (IT.LT.NT) GO TO 620
665   IF ( plot.AND.cfout ) THEN
        plot = .FALSE.
        GOTO 560
      ENDIF
      IF ( CFOUT ) THEN
        IF (TAB8) CALL TABLES
        IF (TEST(12)) CALL LOGK
      ENDIF
      GO TO 20
C
C  STORE DATA FROM LSTSQS RECORD.
C
670   IX=IC80
      if (ix.eq.0) ix=9
      DO 680 I=1,4
      IF (ABEL(1,I).EQ.'EXP') GO TO 690
      IF (ABEL(1,I).EQ.'NOCP'.OR.ABEL(1,I).EQ.'NOH'.OR.
     1 ABEL(1,I).EQ.'NOS') GOTO 690
680   CONTINUE
      GO TO 730
690   IF (IX.LE.9) GO TO 730
      WRITE (6,720)
720   FORMAT(/' ONLY 8 TEMPERATURE INTERVALS ALLOWED FOR LEAST SQUARES')
      GO TO 80
730   DO 850 I=1,4
      IF (ABEL(1,I).EQ.'T') GO TO 760
      IF (ABEL(1,I).EQ.'TCON'.OR.ABEL(1,I).EQ.'TPRO') GO TO 750
      IF (ABEL(1,I).EQ.'EXP') GO TO 780
      IF (ABEL(1,I).EQ.' ') GO TO 850
      IF (ABEL(1,I).EQ.'NOCN') GO TO 790
      IF (ABEL(1,I).EQ.'NOCP') GO TO 800
      IF (ABEL(1,I).EQ.'NOH') GO TO 810
      IF (ABEL(1,I).EQ.'OLD') go to 815
      IF (ABEL(1,I).EQ.'NOS') GO TO 820
      IF (ABEL(1,I).EQ.'CPON') GO TO 818
      WRITE (6,740) ABEL(1,I),WORD(I)
740   FORMAT (/,A4,' IS AN INCORRECT LABEL FOR THE NUMBER--',E12.4,
     1  '.  VALUE IGNORED (MAIN PGM)')
      GO TO 850
750   TCONST=WORD(I)
      GO TO 850
760   NTRANG=NTRANG+1
      IF (NTRANG.GT.9) GO TO 850
      TRANGE(NTRANG)=WORD(I)
      GO TO 850
780   NF(IX)=NF(IX)+1
      N=NF(IX)
      EX(N,IX)=WORD(I)
      if (ic80.eq.0) lscode(1) = -1
      GO TO 850
790   CNST=.FALSE.
      GO TO 850
800   NOCP(IX)=.TRUE.
      GO TO 830
810   NOH(IX)=.TRUE.
      GO TO 830
815   OLD=.TRUE.
      go to 830
818   CPONLY(IX) = .TRUE.
      NOH(IX) = .TRUE.
820   NOS(IX)=.TRUE.
830   IF (IC80.NE.0) GO TO 850
      DO 840 J=1,8
      CPONLY(J) = CPONLY(ix)
      NOCP(J)=NOCP(ix)
      NOS(J)=NOS(ix)
840   NOH(J)=NOH(ix)
850   CONTINUE
      GO TO 80
860   DO 890 I=1,4
      IF (ABEL(1,I).EQ.'T'.OR.ABEL(1,I).EQ.'I') GO TO 870
      IF (ABEL(1,I).EQ.' ') GO TO 890
      WRITE (6,740) ABEL(1,I),WORD(I)
      GO TO 890
870   NTCF=NTCF+1
      TCWD(NTCF)=WORD(I)
      TCLB(NTCF)=ABEL(1,I)
890   CONTINUE
      GO TO 80
900   CLOSE (5)
      CLOSE (6)
      CLOSE (IOSCH)
      CLOSE (IOUSCH)
      CLOSE (IOGRP)
      CLOSE (IOEFT)
      CLOSE (IOELM)
      CLOSE (IOPLT)
      STOP
99001 FORMAT (/' ***************************************************',
     & '****************************',/)
99002 FORMAT (11x,'NASA-LEWIS PROPERTIES AND COEFFICIENTS PROGRAM,',
     & '  P A C 99',/ 9x,'BY  BONNIE MCBRIDE AND',
     & ' SANFORD GORDON  REF: NASA RP-1271,1992 & TP-2002-211556')
c99010 FORMAT (1x,1p,8E12.5)
99010 FORMAT (1x,1p,E12.4,2E13.6,5E12.4)
      END
      SUBROUTINE ATOM 
      SAVE 
C 
C  ROUTINE CALCULATES PROPERTIES OF ATOMIC GASES FROM ENERGY LEVELS.  
C 
C  BUILT-IN FILL PARAMETERS WERE TAKEN FROM : J.R.DOWNEY,JR., 
C    "CALCULATION OF THERMODYNAMIC PROPERTIES OF IDEAL GASES AT HIGH 
C    TEMPERATURES," THE DOW CHEMICAL CO.,MIDLAND,MI,CONTRACT NOS 
C    F44620-75-C-0048 AND AFOSR-TR-78-0960, MARCH 9,1978.  
C    PARAMETERS ARE NOT BUILT IN FOR ATOMIC NOS 58-71 AND NOS LARGER 
C    THAN 86.  FOR THESE ATOMS AND IONIC SPECIES VALUES MAY BE READ IN 
C    ON THE METH RECORD WITH LABELS SUMG AND B.  SUMG REPRESENTS THE 
C    TOTAL QUANTUM WEIGHT OF THE GROUND STATE.  FOR POSITIVE B VALUES 
C    THE TOTAL QUANTUM WEIGHT OF AN EXCITED STATE IS BN**2. FOR NEGATIVE 
C    B VALUES THE TOTAL QUANTUM WEIGHT IS THE ABSOLUTE VALUE OF B.  
C
      CHARACTER*4 CUTOFF
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      DIMENSION AJ(900),G(900),NN(900),nnlast(400)
      REAL*8 ALNWT,QSUM,TEMPY,TMP,TDQDTS,X,XTDQDS,gfill(400),anum1
      REAL*8 ANU(900),Q(1500),TDQDT(1500),XTDQDT(1500),anui(400),anuk
C
      LOGICAL TSTFIL,GLABEL,limitL
C
C  INITIALIZE TO NO CUT-OFF AND NO FILL
C
      NATOM=100
      CUTOFF=' '
      TSTFIL=.FALSE.
      GLABEL=.FALSE.
      limitL=.false.
      B=0
      FORM=0
      NFIXED=10000

C
C  CHECK FOR FILL AND CUTOFF ON METHOD RECORD.
C
      DO 20 I=1,4
      IF (ABEL(1,I).EQ.'FILL') then
         TSTFIL=.TRUE.
         nal=word(i)
      ELSEIF (ABEL(1,I).EQ.'GLAB') then
         GLABEL=.TRUE.
      ELSEIF (ABEL(1,I).EQ.'TEMP') then
         CUTOFF='TEMP'
      ELSEIF (ABEL(1,I).eq.'FIXE') then
         NFIXED=WORD(I)
         CUTOFF='FIXE'
      ELSEIF (ABEL(1,I).eq.'BETH') then
         NFIXED=WORD(I)
         CUTOFF='BETH'
         one6th=1.d0/6.d0
      ELSEIF (ABEL(1,I).EQ.'B') then
         B=WORD(I)
      ELSEIF (ABEL(1,I).EQ.'SUMG') then
         FORM=WORD(I)
      ENDIF
20    CONTINUE
      K=0
      ALNWT=DLOG(WEIGHT)*1.5 D0
      NFIRST=0
      WRITE (6,30) WEIGHT
30    FORMAT (/,' ATOMIC WEIGHT =',F12.7)
      LINES=LINES+2
      IF(.NOT.TEST(14)) GO TO 40
      DO 36 I=1,100
      WRITE (6,33) SYMBOL(1,I),SYMBOL(2,I),ATMWT(I),NMAT(I),NMLA(I),
     1 GG(I),AG(I)
33    FORMAT (5X,2A1,F15.5,2I6,2F8.0)
36    CONTINUE
C
C  CALL INPUT TO READ AND LIST A DATA RECORD.
C
40    CALL INPUT
      IF (NFIRST.NE.0) GO TO 50
      SUB=CODE(1)
      NFIRST=1
50    IF (CODE(1).NE.SUB) GO TO 120
      If (limitL) goto 40
      DO 110 I=1,4
      IF (ABEL(1,I).NE.'IP') GO TO 60
      PI=WORD(I)
      GO TO 110
60    IF (FWORD(I).LT.0.) GO TO 110
      IF (K.EQ.900) GO TO 90
      K=K+1
      NN(K)=IC80
      IF (NN(K).EQ.0.AND.CUTOFF.NE.' ') GO TO 70
          if (GLABEL) then
              g(k)=fword(i)
                  if (g(k).le.0.) then
                  WRITE (6,65)
65                FORMAT (/,' **** g <= 0 ')
                  RETURN
                  endif
              AJ(K)=(g(K)-1.)/2.
          else
              AJ(K)=FWORD(I)
              G(K)=2.*AJ(K)+1.
          endif
      ANU(K)=WORD(I)
      IF ((WORD(I).EQ.0.).AND.(K.GT.1)) ANU(K)=ANU(K-1)
      GO TO 110
70    WRITE (6,80) ABEL(1,I),NN(K)
80    FORMAT (/,' ERROR IN DATA--J =',A6,6X,7HLEVEL =,I3)
      LINES=LINES+2
      GO TO 110
90    WRITE (6,100)
100   FORMAT (/,' PROGRAM CAN ACCOMMODATE ONLY 900 LEVELS')
      limitL = .true.
      goto 680
110   CONTINUE
      GO TO 40
120   KLAST=K
C
C  SORT ENERGY LEVELS IN INCREASING NUMERICAL ORDER.
C
      J=1
130   M=J
      DO 150 I=J,KLAST
      IF (ANU(M)-ANU(I)) 150,150,140
140   M=I
150   CONTINUE
      IF (M-J) 160,170,160
160   TEMPY=ANU(M)
      ANU(M)=ANU(J)
      ANU(J)=TEMPY
      TEMPY=G(M)
      G(M)=G(J)
      G(J)=TEMPY
      KTEMPY=NN(M)
      NN(M)=NN(J)
      NN(J)=KTEMPY
      TEMPY=AJ(M)
      AJ(M)=AJ(J)
      AJ(J)=TEMPY
170   J=J+1
      IF (KLAST-J) 180,180,130
180   CONTINUE
      NN(KLAST+1)=0
      AJ(KLAST+1)=0.0
      G(KLAST+1)=0.0
      ANU(KLAST+1)=0.0
      IF (.NOT.TEST(14)) GO TO 220
      WRITE (6,190)
190   FORMAT (/,4X,1HN,6X,1HJ,7X,1HG,5X,13HENERGY LEVEL ,12X,1HN,6X,
     1'J',7X,1HG,5X,13HENERGY LEVEL )
      LINES=LINES+2
      DO 210 I=1,KLAST,2
      INN=I+1
      WRITE (6,200) (NN(IN),AJ(IN),G(IN),ANU(IN),IN=I,INN)
200   FORMAT (2(I6,F8.1,F8.1,F14.3,10X))
      LINES=LINES+1
210   continue
220   IF (CUTOFF.NE.'FIXE') GO TO 240
      IF (NFIXED.GE.NN(1)) GO TO 240
      WRITE (6,230) NN(1)
230   FORMAT (/' SINCE FIXEDN IS LESS THAN FIRST N, FIXEDN IS SET EQUAL'
     1  ,I3, '(ATOM)')
      NFIXED=NN(1)
      LINES=LINES+2
C
C  FILL OPTION
C
240   IF (.NOT.TSTFIL) GO TO 390
      INDX=JF(1)
      IF (NKIND.EQ.1) GO TO 260
C
C  ION WITH FILL
C
      IF(MLA(2).GT.0) GO TO 260
      INDX=INDX+MLA(2)
      IF (INDX.GT.3) GO TO 260
      IF (INDX.LT.3) GO TO 270
      INDX=2
C
260   IF (B.EQ.0.) B=AG(INDX)
      IF (FORM.EQ.0.) FORM=GG(INDX)
270   IF ((B.EQ.0.).OR.FORM.EQ.0.) GO TO 370
      WRITE (6,275) nal
275   FORMAT (/,' NUMBER OF ADDITIONAL LEVELS =',i4)
      WRITE (6,280)
280   FORMAT ('  B/-C*',8X,1HN,2X,'PRED.SUM(2J+1)',2X,'ACT.SUM(2J+1)'
     1 ,2X,'DIFF',6X,'MAX LEVEL',5X,'2J+1  T CUTOFF')
      KX=1
      NN1=NN(1)
290   KUREN=NN(KX)
      SUM=0.0
      L=1
      K=KX
      DO 360 J=K,KLAST
      IF (NN(J).LT.0) GO TO 320
      IF (NN(J)-KUREN) 295,300,310
295   if (KUREN.NE.NN1) go to 310
300   SUM=SUM+G(J)
      M=J
      NN(J)=-NN(J)
      IF (J.NE.KLAST) GO TO 360
      GO TO 330
310   IF (L.NE.1) GO TO 320
      L=0
      KX=J
320   IF (J.NE.KLAST) GO TO 360
330   IF (KUREN.EQ.NN1) GO TO 340
      IF (B.GT.0.) GO TO 335
      FORM=ABS(B)
      GO TO 340
335   TEMPY=KUREN*KUREN
      FORM=B*TEMPY
340   DIFF=FORM-SUM
      IF (KUREN.LT.NN1) DIFF=0.0
      NNM=MAX(-NN(M),NN1)
      IF (DIFF.GT.0.0) then
         if (nal.gt.0) then
            gfill(nnm)=diff/nal
            nnlast(nnm)=m
            anui(nnm)= (PI-ANU(M))/(nal+1)
         else
            g(m)=g(m)+diff
            nnlast(nnm)=0
         endif
      ENDIF
      tcut = (pi - ANU(M))*HCK
      WRITE (6,350) B,NNM,FORM,SUM,DIFF,ANU(M),G(M),tcut
350   FORMAT (/,F9.1,I7,F12.1,F15.1,F11.1,F14.3,F9.1,F10.1)
      IF (L.NE.1) GO TO 290
      GO TO 390
360   CONTINUE
370   WRITE (6,380)
380   FORMAT (/,' NO FILL--B OR SUMG MISSING ON METHOD RECORD, (ATOM)')
C
C  END FILL OPTION
C
390   IF (ASINDT.NE.0.0) GO TO 400
      NT1=NT
      GO TO 410
400   NT1=NT+1
      T(NT1)=ASINDT
C
C  M = INDEX FOR T.   K = INDEX FOR ELECTRONIC LEVELS.
410   DO 650 M=NIT,NT1
      I=0
      JJ=1
      mic = 2.461D0*T(M)**one6th
      IF(CUTOFF.eq.'BETH') NFIXED=MAX(iabs(NN(1)),mic)
C
C  CALCULATE THE PARTITION FUNCTION AND DERIVATIVES FOR EACH ELECTRONIC
C      LEVEL AND TEMPERATURE.
      anum1=anu(1)
      nout=-nn(1)
      DO 555 K=1,KLAST
      infill=0
      IF (nal.gt.0) then
         nnk=max(-NN(k),NN1)
         if (k.eq.nnlast(nnk))then
            infill=nal
         endif
      ENDIF
      gk=g(k)
      anuk=ANU(K)
      do 551 in=0,infill
         if (in.gt.0) then
            gk=gfill(nnk)
            anuk=anuk+ anui(nnk)
         endif
      X=(HCK/T(M))*ANUK
      IF (X-85.) 440,420,420
420   IF (.NOT.TEST(14)) GO TO 580
      WRITE (6,430)
430   FORMAT (/,' NOT ALL LEVELS WERE USED.  X IS GREATER THAN 85.')
      GO TO 580
440   IF (CUTOFF.NE.'TEMP') GO TO 490
      IF (PI.NE.0.) GO TO 460
      WRITE (6,450)
450   FORMAT (/,' IONIZATION POTENTIAL IS MISSING')
      GO TO 680
460   THERM=PI-T(M)/HCK
      IF (ANUK-THERM) 500,500,470
470   IF (TEST(14)) then
         WRITE (6,480) THERM,anum1,nout
480      FORMAT (/,' ALL LEVELS USED TO THE THERMAL BINDING ENERGY'
     1     ,F12.3,/'   LAST LEVEL USED =',F12.3,';    N =',I4)
      ELSEIF (in.eq.0) then
         go to 580
      ENDIF
      go to 555
490   IF (IABS(NN(K)).gt.NFIXED) THEN
        JJ=0
        GO TO 550
      ENDIF
500   I=I+1
      if (I.GT.1500) then
        limitL = .true.
        WRITE (6,100)
        goto 680
      endif
      Q(I)=GK*DEXP(-1.D0*X)
      TDQDT(I)=Q(I)*X
      XTDQDT(I)=TDQDT(I)*X
      IF (Q(I)-0.1E-9) 510,510,550
510   IF (X-2.0) 550,550,520
520   IF (XTDQDT(I)-0.1E-9) 530,530,550
530   IF (.NOT.TEST(14)) GO TO 580
      WRITE (6,540) anum1,nout
540   FORMAT (/' NOT ALL ASSIGNED LEVELS USED, Q AND DERIVS ARE TOO SMALL
     *;'/'  LAST LEVEL USED =',F12.3,' ;  N =',I4)
      GO TO 580
550   anum1=anuk
      nout=nnk
551   continue
555   CONTINUE
      IF (.NOT.TEST(14)) GO TO 580
      IF (JJ.EQ.0) WRITE (6,560) NFIXED
560   FORMAT (/,' ALL LEVELS USED THROUGH N =',I5)
      IF (JJ.NE.0) WRITE (6,570)
570   FORMAT (/,' ALL ASSIGNED LEVELS HAVE BEEN USED')
C  CALCULATE TOTAL Q, DERIVATIVES, AND THERMODYNAMIC FUNCTIONS FOR T.
580   QSUM=0.0
      TDQDTS=0.0
      XTDQDS=0.0
      J=I
      DO 590 II=1,J
      QSUM=QSUM+Q(I)
      TDQDTS=TDQDTS+TDQDT(I)
      XTDQDS=XTDQDS+XTDQDT(I)
590   I=I-1
      IF (TDQDTS-0.1D-9) 600,610,610
600   TMP=0.0
      GO TO 620
610   TMP=TDQDTS/QSUM
620   HHRT(M)=TMP+2.5
      IF (M.GT.NT) GO TO 660
      CPR(M)=2.5+XTDQDS/QSUM-TMP*TMP
      GHRT(M)=DLOG(QSUM)+2.5*DLOG(T(M))+ALNWT+SCONST
      SR(M)=GHRT(M)+HHRT(M)
      IF (.NOT.TEST(14)) GO TO 650
      WRITE (6,630) T(M),CPR(M),HHRT(M),GHRT(M)
630   FORMAT (1X,'T',F11.2,6X,'CP/R',F12.6,6X,'H-H0/RT',F11.6,6X,
     1'G-H0/RT',F12.6)
      x=anum1*hck/t(m)
      WRITE (6,640) X,QSUM,TDQDTS,XTDQDS
640   FORMAT (1X,'X',F12.7,6X,'QSUM',F12.7,6X,'T(DQ/DT)S',F13.7,6X,
     1 'XT(DQ/DT)S',F13.7)
650   CONTINUE
      GO TO 670
660   SPECH=HHRT(NT1)*ASINDT
      IF (DABS(ASINDT-298.15D0).LT..05) SPECH=HHRT(NT1)*298.15D0
C
C  TEST(19)-- ENTHALPY HAS BEEN CALCULATED FOR T  ON FORMULA RECORD.
C  TEST(9)--FUNCTIONS HAVE BEEN CALCULATED.
C
      TEST(19)=.TRUE.
670   TEST(9)=.TRUE.
680   if (limitL) TEST(9)=.false.
      RETURN
      END
      SUBROUTINE DELH
      SAVE
C
C  CALCULATES THE ASSIGNED H VALUE.  CALLS LEAST AND PUNCH.
C
      PARAMETER (IOEFT=13)
      DIMENSION ELMT(2)
      DOUBLE PRECISION TP(3,502),HZERO,AMP,TNO,HA,TI
C
      CHARACTER*4 DELTAH,ASH,ELMT,PHAZ
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      EQUIVALENCE  (TP,TAPE)
      DATA DELTAH/'DELT'/, ASH/'ASIN'/
C
      IF (TEST(18)) GO TO 30
      IF (H298HR.EQ.0. .AND. .NOT.TEST(13)) GO TO 30
15    IF (HEAT.EQ.BLANK) GO TO 120
      IF (TEST(13).AND.ASINDT.EQ.298.15D0) TEST(8)=.TRUE.
      IF (TEST(8)) GO TO 55
      IF (TEST(19)) GO TO 110
      WRITE (6,20)
20    FORMAT (/,' INSUFFICIENT DATA FOR AN H0 VALUE, (DELH)')
      GO TO 310
30    DO 40 I=1,NT
      IF (DABS(T(I)-298.15D0).GT.5.D-02) GO TO 40
      IF(TEST(18))GO TO 44
      IF(H298HR.EQ.0.)H298HR=298.15D0*HHRT(I)
      GO TO 15
40    CONTINUE
      IF(TEST(18))GO TO 70
      GO TO 15
44    IF(TEST(8))GO TO 47
      IF(H298HR.EQ.0.)GO TO 45
      ASINDH=HHRT(I)*298.15D0-H298HR
      ASINDT=0.
      GO TO 46
45    ASINDT=298.15D0
      ASINDH=HHRT(I)*298.15D0
      H298HR=ASINDH
46    HEAT=ASH
      TEST(8)=.TRUE.
47    DO 48 I=1,NT
      TI=T(I)
      IF(DABS(TI-298.15).LT..05)TI=298.15D0
      GHRT(I)=GHRT(I)+ASINDH/TI
48    HHRT(I)=HHRT(I)-ASINDH/TI
      IF(ASINDT.EQ.298.15D0)TEST(13)=.TRUE.
      TEST(18)=.FALSE.
      DO 50 IA=ivl1,intvls
      IF (COEF(1,IA).EQ.0.D0 .AND. COEF(2,IA).EQ.0.D0) GO TO 50
      COEF(9,IA)=COEF(9,IA)-asindh
50    CONTINUE
55    IF (.NOT.TEST(17)) GO TO 60
      if (ivl1.le.idone) go to 310
      CALL PUNCH
      TEST(17)=.FALSE.
      GO TO 66
60    if(.not.TEST(15)) go to 65
      CALL LEAST
65    if(.not.TEST(1) .or. ivl1.le.idone) go to 310
      CALL PUNCH
66    TTRANS(INA)=TC(INTVLS+1)
      INA=MIN0(NNA,INA+1)
C     idone=intvls
      GO TO 310
C
70    IF(.NOT.TEST(8))GO TO 15
      IF(ASINDT.EQ.0.)GO TO 90
      IF(ASINDT.NE.298.15D0)GO TO 15
      TEST(13)=.TRUE.
90    TEST(18)=.FALSE.
      DO 100 I=1,NT
      HHRT(I)=HHRT(I)-ASINDH/(T(I))
100   GHRT(I)=GHRT(I)+ASINDH/(T(I))
      GO TO 15
C
110   IF (HEAT.EQ.ASH) GO TO 140
      IF (HEAT.EQ.DELTAH) GO TO 150
120   WRITE (6,130)
130   FORMAT (/' EITHER ASINDH,DELTAH,OR HF298, WAS NOT FOUND ON THE',
     1' FORMULA RECORD, (DELH)')
      LINES=LINES+2
      TEST(8)=.FALSE.
      GO TO 310
140   ASINDH=ASINDH-SPECH
      IF (DABS(ASINDT-298.15D0).LT..05) H298HR=SPECH
      TEST(8)=.TRUE.
      GO TO 30
C
150   IF (HEAT.EQ.DELTAH) ASINDH=ASINDH-SPECH
      REWIND IOEFT
      ICT=0
160   READ (IOEFT,END=250) ELMT,NELM,PHAZ,CODE,HZERO,AMP,TNO,((TP(J,K),
     * J=1,3),K=1,101)
      IF (ELMT(1).EQ.BLANK) GO TO 250
      IF (HEAT.EQ.DELTAH) GO TO 170
      IF (NELM.GT.1) GO TO 160
      IF (PHAZ.NE.'G') GO TO 160
170   DO 200 I=1,NKIND
      J=JF(I)
      IF (PHAZ.EQ.EFAZ(J)) GO TO 180
      IF (PHAZ.EQ.'G'.OR.EFAZ(J).EQ.'G') GO TO 200
180   IF (NELM.NE.NMLA(J)) GO TO 200
      IF (ELMT(1).NE.FORMLA(1,I)) GO TO 200
      IF (ELMT(2).NE.FORMLA(2,I)) GO TO 200
      ICT=ICT+1
      GO TO 210
200   CONTINUE
      GO TO 160
210   IF (ASINDT.EQ.0.) GO TO 240
      N=TNO+.1
      DO 220 K=1,N
      IF (DABS(TP(1,K)-ASINDT).LT..000001) GO TO 270
220   CONTINUE
      WRITE (6,230)
230   FORMAT (/' HHRT FOR ASINDT WAS NOT FOUND ON EF TAPE, (DELH)')
      GO TO 300
240   HA=HZERO
      GO TO 280
250   WRITE (6,260) FORMLA(1,I),FORMLA(2,I)
260   FORMAT (/,1x,2A1,'DATA WERE NOT FOUND ON EF TAPE, (DELH)')
      GO TO 300
270   HA=TP(2,K)*(TP(1,K)+HZERO)
280   IF (HEAT.EQ.DELTAH) ASINDH=ASINDH+FLOAT(MLA(I))*HA/FLOAT(NMLA(J))
      IF (ICT.GE.NKIND) GO TO 290
      GO TO 160
290   TEST(8)=.TRUE.
300   REWIND IOEFT
      IF (TEST(8)) GO TO 30
310   RETURN
      END
      SUBROUTINE DERIV (I1,I2,I3,I4,I5,J1,J2,J3,J4,J5,J6)
      SAVE
C
C  FIND Q DERIVATIVES.
C
      double precision QCORT
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      real*8 v,x,y,alfa,alfb,alfc,g,wx,beta,a,b,c,dard,dd,wf,w,sym,stwt,
     1 t00,theta,rh,r,s,ql,q,qln,dq,ddq,qtot,qlntot,dqtot,ddqtot,aij,
     2 aiii,ai
      logical testw
      COMMON /WCOMMN/ V(300),X(9,9),Y(9,9,9),ALFA(9),ALFB(9),ALFC(9),
     1 G(9),WX(9),BETA(4),A,B,C,DARD,DD,WF,W,SYM,STWT,T00,THETA(3),
     2 RH(6),R(300,3),S(300,3),QL(3),Q,QLN,DQ,DDQ,QTOT,QLNTOT,DQTOT,
     3 DDQTOT,AIJ(9,9),AIII,AI(9),DN(300),ND(300),CORT,NNU,TESTW(6)
C
      DIMENSION I(5), J(6)
C
      I(1)=I1
      I(2)=I2
      I(3)=I3
      I(4)=I4
      I(5)=I5
      J(1)=J1
      J(2)=J2
      J(3)=J3
      J(4)=J4
      J(5)=J5
      J(6)=J6
      QL(2)=0.
      QL(3)=0.
      DO 20 IR=1,5
      K=I(IR)
      IF (K.EQ.0) GO TO 30
      QL(1)=QL(1)*R(K,1)
      QL(2)=QL(2)+R(K,2)
20    QL(3)=QL(3)+R(K,3)
30    DO 40 IS=1,6
      K=J(IS)
      IF (K.EQ.0) GO TO 50
      QL(1)=QL(1)*S(K,1)
      QL(2)=QL(2)+S(K,2)
40    QL(3)=QL(3)+S(K,3)
50    QLN=QLN+QL(1)
      QCORT=QL(2)-CORT
      DQ=QCORT*QL(1)+DQ
      DDQ=QL(1)*(QL(3)+QCORT**2-QCORT)+DDQ
      RETURN
      END
      SUBROUTINE EFTAPE
      SAVE
C
C  PROCESS EF DATA.   DATA ARE STORED ON IO UNIT IOEFT FOR SUBSEQUENT
C     USE IN DELTA H AND LOG K CALCULATIONS.
C  USES IO UNIT IOUSCH AS SCRATCH TAPE FOR UNFORMATTED DATA.
C
      PARAMETER (IOELM=11,IOEFT=13,IOUSCH=17)
      DIMENSION ELMT(2),DD(2)
      DOUBLE PRECISION HOVERR,MELTPT,TNUM,FILL(3)
      CHARACTER*4 DD,PM,ANT,H0R,ELMT,PHAZ
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      EQUIVALENCE (WORD(2),FILL(1))
C
      DATA PM/'MP'/,ANT/'NT'/,H0R/'H0/R'/
C
C  TEST 10--WRITE EF DATA CALCULATED IN CURRENT RUN ON IO UNIT IOELM IN
C       FORMATTED FORM.  ALSO ADD TO UNFORMATTED DATA ON IO UNIT IOEFT.
C
      MELTPT=PTMELT
      HOVERR=ASINDH
      IF (.NOT.TEST(10)) GO TO 160
      TN=NT
      TNUM=TN
      WRITE (IOELM,20) 'EFDA',FORMLA(1,1),FORMLA(2,1),MLA(1),FAZ,DATE,
     1 H0R,HOVERR,PM,PTMELT,ANT,TN
20    FORMAT (A4,2X,2A1,I1,A1,3X,2A4,3X,3(A4,2X,F12.4),I2)
      WRITE (6,30) ' EFDA',FORMLA(1,1),FORMLA(2,1),MLA(1),FAZ,DATE,H0R,
     1 HOVERR,PM,PTMELT,ANT,TN
30    FORMAT (/,1x,A4,2X,2A1,I1,A1,3X,2A4,3X,3(A4,2X,F12.4),I2)
      WRITE (IOELM,60) (T(I),HHRT(I),GHRT(I),I=1,NT)
60    FORMAT (2(F12.3,2F14.6))
      WRITE (6,70) (T(I),HHRT(I),GHRT(I),I=1,NT)
70    FORMAT (1X,F12.3,2F14.6,F12.3,2F14.6)
C
C  SET POSITION ON TAPE IOEFT
C
      REWIND IOEFT
      REWIND IOUSCH
      ND=1
      DO 80 IX=1,220
      II=IX
      READ (IOEFT,END=115) ELMT,NLM,PHAZ,DD,FILL,TAPE
      IF (ELMT(1).EQ.BLANK) GO TO 110
      IF (ELMT(1).NE.FORMLA(1,1)) GO TO 80
      IF (ELMT(2).NE.FORMLA(2,1)) GO TO 80
      IF (NLM.NE.MLA(1).OR.PHAZ.NE.FAZ) GO TO 80
      ND=0
      GO TO 90
80    CONTINUE
C
90    DO 100 ix=II,220
      READ (IOEFT,END=115) ELMT,NLM,PHAZ,DD,FILL,TAPE
      WRITE (IOUSCH) ELMT,NLM,PHAZ,DD,FILL,TAPE
      ndfile = ix
      IF (ELMT(1).EQ.BLANK) GO TO 110
100   CONTINUE
c110   II=II-1
110   continue
115   REWIND IOUSCH
      REWIND IOEFT
      ii=ii-1
      IF (II.LT.1) GO TO 130
      DO 120 I=1,II
      READ (IOEFT,END=130) ELMT,NLM,PHAZ,DD,FILL,TAPE
120   CONTINUE
C
130   DO 140 I=1,502
140   TAPE(I)=T(I)
      WRITE (IOEFT) FORMLA(1,1),FORMLA(2,1),MLA(1),FAZ,DATE,HOVERR,
     1 MELTPT,TNUM,(TAPE(I),HHRT(I),GHRT(I),I=1,502)
      IF (ND.EQ.1) GO TO 260
      II=II+1
      DO 150 KI=II,NDFILE
      READ (IOUSCH) ELMT,NLM,PHAZ,DD,FILL,TAPE
      WRITE (IOEFT) ELMT,NLM,PHAZ,DD,FILL,TAPE
150   CONTINUE
c     GO TO 400
      GO TO 500
C
C   WRITE FORMATTED EF DATA READ FROM IO UNIT 5 TO IO UNIT IOEFT IN
C       UNFORMATTED FORM.
C
160   IF (FORMLA(1,1).EQ.BLANK) GO TO 260
      DO 170 J=1,100
      IF (SYMBOL(1,J).NE.FORMLA(1,1)) GO TO 170
      IF (SYMBOL(2,J).NE.FORMLA(2,1)) GO TO 170
      GO TO 190
170   CONTINUE
      WRITE (6,180) FORMLA(1,1),FORMLA(2,1)
180   FORMAT (/,1x,2A1,' NOT IN SYMBOL LIST (EFTAPE)')
190   N=WORD(4)*3.D0+.1
      READ (5,60) (TAPE(I),I=1,N)
C
      N1=N+1
      IF (N1.GT.1506)GO TO 255
      DO 250 J=N1,1506
250   TAPE(J)=0
255   WRITE (IOEFT) FORMLA(1,1),FORMLA(2,1),MLA(1),FAZ,DATE,FILL,TAPE
      GO TO 500
260   WRITE (IOEFT) BLANK,BLANK,NLM,FAZ,DATE,FILL,TAPE
      GO TO 500
C
C  LIST ENTHALPY AND FREE ENERGY DATA FOR THE ELEMENTS.
C
      ENTRY EFLIST
C
      WRITE (6,290)
290   FORMAT (//,1x,' EFDATA - DIMENSIONLESS ENTHALPY AND GIBBS '
     1 ,'FUNCTION FOR THE ELEMENTS',//)
      REWIND IOEFT
300   READ (IOEFT,END=500) ELMT,NLM,PHAZ,DD,FILL,TAPE
      N=FILL(3)*3.D0+.1
c     IF (ELMT(1).EQ.BLANK) GO TO 400
      IF (ELMT(1).EQ.BLANK) GO TO 500
      WRITE (6, 310) ELMT,NLM,PHAZ,DD,FILL
310   FORMAT (/' EFDATA',5X,2A1,I1,'(',A1,')',5X,2A4,5X,'H0/R',F11.4,
     1 5X,'MP',F10.4,5X,'NT',F5.0,/)
      WRITE (6,60) (TAPE(J),J=1,N)
      GO TO 300
C
c400   WRITE (6,410)
c410   FORMAT ('1   ')
500   RETURN
      END
C
      SUBROUTINE EIGEN (N) 
      SAVE 
C 
      DOUBLE PRECISION MU,NUF,NUI,A,OMEGA,SINT,COST,BIP,SINT2,COST2
      DOUBLE PRECISION SINCOS,APQ,BPP,BQQ,BPQ,BQP,SWAP
C
      COMMON /BLK3/A(188,188)
C
      NUI=0.D0
      FN=N
      DO 10 JX=1,N
      DO 10 JY=JX,N
      IF (JX.EQ.JY) GO TO 10
      NUI=NUI+A(JX,JY)**2
10    CONTINUE
      NUI=DSQRT(NUI+NUI)
      NUF=1.0D-8*NUI
20    NUI=NUI/FN
30    IQ=2
40    IP=1
50    IF (DABS(A(IP,IQ)).GE.NUI) GO TO 100
      IF (IP.NE.IQ-1) GO TO 90
60    IF (IQ.EQ.N) GO TO 70
      IQ=IQ+1
      GO TO 40
70    IF (IND.NE.1 .OR. NUI.EQ.0.) GO TO 80
      IND=0
      GO TO 30
80    IF (NUI.LE.NUF) GO TO 120
      GO TO 20
90    IP=IP+1
      GO TO 50
100   IND=1
      MU=0.5*(A(IP,IP)-A(IQ,IQ))
      OMEGA=-A(IP,IQ)/DSQRT(A(IP,IQ)**2+MU**2)
      IF (MU.LT.0.) OMEGA=-OMEGA
      SINT=OMEGA/DSQRT(2.*(1.+DSQRT(1.-OMEGA**2)))
      COST=DSQRT(1.-SINT**2)
      DO 110 I=1,N
      IF (I.EQ.IP.OR.I.EQ.IQ) GO TO 110
      BIP=A(I,IP)*COST-A(I,IQ)*SINT
      A(I,IQ)=A(I,IP)*SINT+A(I,IQ)*COST
      A(I,IP)=BIP
      A(IP,I)=A(I,IP)
      A(IQ,I)=A(I,IQ)
110   CONTINUE
      SINT2=SINT*SINT
      COST2=COST*COST
      SINCOS=SINT*COST
      APQ=2.*A(IP,IQ)*SINCOS
      BPP=A(IP,IP)*COST2+A(IQ,IQ)*SINT2-APQ
      BQQ=A(IP,IP)*SINT2+A(IQ,IQ)*COST2+APQ
      BPQ=(A(IP,IP)-A(IQ,IQ))*SINCOS+A(IP,IQ)*(COST2-SINT2)
      BQP=BPQ
      A(IP,IP)=BPP
      A(IQ,IQ)=BQQ
      A(IP,IQ)=BPQ
      A(IQ,IP)=BQP
      IF (IP.EQ.IQ-1) GO TO 60
      GO TO 90
120   NM1=N-1
      DO 130 I=1,NM1
      DO 130 J=I,N
      IF (A(I,I).LT.A(J,J)) GO TO 130
      SWAP=A(I,I)
      A(I,I)=A(J,J)
      A(J,J)=SWAP
130   CONTINUE
      RETURN
      END
      SUBROUTINE GROUP
      SAVE
      PARAMETER (IOGRP=19)
C
C  PROGRAM MODIFIED FOR INTERACTIVE USE BY S.E.STEIN ON 5/2/84
C  THERE ARE FOUR TYPES OF C-ATOMS USED IN GROUP DEFINITION
C    (1) BENSON-TYPE GROUPS'  CB (BENZENE LIKE); CD (DOUBLY BONDED);
C        CF (FUSED RING); C2ACE (ACENAPHTHYLENE RING-GROUP);
C      CT( TRIPLY BONDED); CBY (BENZYNE TYPE)
C   **** FOR ACTUAL GROUPS IN USE SEE FILE GROUP ****
C    (2) BOND STRENGTH CORRECTIONS- TO GENERATE RADICAL R. FROM
C        PRECURSOR MOLECULE R-H, SUBTRACT ONE OF THESE GROUPS FROM R-H.
C        THE GROUPS ARE' HVIN (VINYL-H); HVINS (STABILIZED VINYL-H);
C          HPHEN (PHENYL-H); HC2H (C2H-H)
C    (3) MISCELLANEOUS CORRECTIONS' NSY (SYMMETRY |); NIS (ISOMER |);
C          H298 (ENTHALPY CORRECTION AT 298K, IN KCAL);
C          ES298 (ENTROPY CORRECTION AT 298K, IN E.U.)
C    (4) ENTIRE MOLECULES' H2; H; C2H2; C4H2
C
      CHARACTER*4 TITLE,GRP,GFORM,SYMNO,STATWT,SRCO,HRCO,EL,SOMN
C
      DIMENSION TITLE(2),GRP(2,60)
      DIMENSION GRPN(60),SEL(5),GFORM(2,5),GMLA(5),EL(2,5),fmla(5)
      LOGICAL SUMOK
      DOUBLE PRECISION FWT,TFWT,AN(10,2),TLN,TT,TMIN,TMAX,TBE,TEN,htform
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      DATA SYMNO/'SYMN'/, STATWT/'STAT'/, SRCO/'SRCO'/, HRCO/'HRCO'/
      DATA SOMN /'NISO'/
C
C  INITIALIZE FOR EACH SET OF METHOD AND DATA RECORDS.
C
      DO 10 I=NIT,NT
      CPR(I)=0.0
      HHRT(I)=0.0
10    GHRT(I)=0.0
      HHRT(NT+1)=0.0
      GHRT(NT+1)=0.0
      ASINDT=298.15D0
      SUMOK=.TRUE.
      TFWT=0.
      TMIN=100.
      TMAX=6000.
      DO 20 I=1,5
      GFORM(1,I)=BLANK
      GFORM(2,I)=BLANK
      fmla(i)=mla(i)
20    GMLA(I)=0
      DO 40 K=1,2
      DO 40 J=1,10
40    COEF(J,K)=0.D0
      SYM=1.0
      NG=0
C
      WRITE (6,60) WEIGHT
60    FORMAT (/,' MOLECULAR WT.=',F12.7)
      NFIRST=0
      LINES=LINES+4
C
65    READ (5,70) CODE,(ABEL(1,I),ABEL(2,I),WORD(I),I=1,4)
70    FORMAT (A4,A2,4(A4,A2,F12.9))
      IF (NFIRST.NE.0) GO TO 95
C
C  INITIALIZE FOR FIRST RECORD ONLY.
      NFIRST=1
      SUB=CODE(1)
C
C  IF COLS 1-4 OR 79-80 ARE DIFFERENT FROM PREVIOUS RECORD, GO TO 323.
C
95    IF (CODE(1).NE.SUB) GO TO 323
      WRITE (6,75) CODE,(ABEL(1,I),ABEL(2,I),WORD(I),I=1,4)
75    FORMAT (/,1x,A4,A2,4(A4,A2,F12.6))
C
C  IN DO LOOP THRU 320 CHECK EACH LABEL ON DATA RECORD AND STORE DATA.
C
      DO 320 ID=1,4
      IF (ABEL(1,ID).EQ.BLANK) GO TO 320
      IF (ABEL(1,ID).EQ.SYMNO) GO TO 130
      IF (ABEL(1,ID).EQ.SOMN .OR. ABEL(1,ID).EQ.STATWT) GO TO 135
      IF (ABEL(1,ID).EQ.SRCO) GO TO 140
      IF (ABEL(1,ID).EQ.HRCO) GO TO 145
      IF (NG.LE.60) GO TO 120
      WRITE(6,110) ABEL(1,ID),ABEL(2,ID)
110   FORMAT(/,1x,2A4,'IS BEYOND GROUP NUMBER LIMIT OF 60')
      GO TO 320
120   NG=NG+1
      GRP(1,NG)=ABEL(1,ID)
      GRP(2,NG)=ABEL(2,ID)
      GRPN(NG)=WORD(ID)
      GO TO 320
130   COEF(10,1)=COEF(10,1)-DLOG(WORD(ID))
      GO TO 320
135   COEF(10,1)=COEF(10,1)+DLOG(WORD(ID))
      GO TO 320
140   COEF(10,1)=COEF(10,1)+WORD(ID)
      GO TO 320
145   COEF(9,1)=COEF(9,1)+WORD(ID)
320   CONTINUE
      GO TO 65
C
323   COEF(10,2)=COEF(10,1)
      COEF(9,2)=COEF(9,1)
      NGG=0
325   READ(IOGRP,330,END=388) TITLE,numint,(EL(1,I),EL(2,I),
     *  SEL(I),I=1,5),fwt,htform
330   FORMAT (2A4,/i2,8x,5(2a1,f6.2),2x,f13.5,f15.3)
      ij=1
      do 335 i=1,numint
      read(IOGRP,333) t1,ten,numexp
333   format(1x,2f10.3,i2)
      read (IOGRP,334) (an(j,ij),j=1,10)
334   format(5d16.8)
      if(ij.eq.2)go to 335
      ij=2
      tbe=t1
335   continue
      if(numexp.gt.5 .or. numint .gt. 2) go to 380
      DO 340 IG=1,NG
      IF (TITLE(1).NE.GRP(1,IG)) GO TO 340
      IF (TITLE(2).NE.GRP(2,IG)) GO TO 340
      GO TO 350
340   CONTINUE
      GO TO 325
C
350   TFWT=TFWT+FWT*GRPN(IG)
      NGG=NGG+1
      DO 370 I=1,5
      IF (EL(1,I).EQ.BLANK.OR.SEL(I).EQ.0.) GO TO 370
      DO 360 L=1,NKIND
      IF (FORMLA(1,L).NE.EL(1,I)) GO TO 360
      IF (FORMLA(2,L).NE.EL(2,I)) GO TO 360
      GFORM(1,L)=EL(1,I)
      GFORM(2,L)=EL(2,I)
      GMLA(L)=GMLA(L)+SEL(I)*GRPN(IG)
      GO TO 370
360   CONTINUE
370   CONTINUE
      go to 382
380   WRITE (6,381) TITLE
381   FORMAT(/,2x,2A4,' HAS ILLEGAL FORMAT FOR GROUPS')
      go to 325
C
382   ivl1=1
      intvls=2
      DO 386 K=1,2
      nf(k)=numexp
      do 383 kk=1,numexp
383   ex(kk,k)=kk-1
      DO 385 J=1,10
385   COEF(J,K)=COEF(J,K)+GRPN(IG)*AN(j,k)
386   CONTINUE
      TMIN=DMAX1(TMIN,TBE)
      TMAX=DMIN1(TMAX,TEN)
      DO 387 I=1,5
      IF (FMLA(I).NE.GMLA(I)) GO TO 325
387   CONTINUE
C
388   DO 390 I=1,5
      IF (FMLA(I).NE.GMLA(I)) GO TO 400
      IF (GFORM(1,I).NE.FORMLA(1,I)) GO TO 400
      IF (GFORM(2,I).NE.FORMLA(2,I)) GO TO 400
390   CONTINUE
      IF (NGG.EQ.NG .AND. DABS(WEIGHT-TFWT).LT..01) GO TO 420
400   WRITE (6,410) FORMLA,FMLA,WEIGHT,GFORM,GMLA,TFWT
410   FORMAT (/' MISMATCH OF FORMULA WITH GROUPS (GROUP)'/,(6X,5(2A1,2X)
     *  ,5F5.2,2X,F13.5))
      TEST(16)=.TRUE.
      GO TO 575
C
420   IF (TMIN.GE.298. .AND. TMIN.LE.300.) TMIN=298.15
      if (test(15)) go to 422
      tc(1)=tmin
      tc(2)=1000.
      tc(3)=tmax
422   do 470 i=nit,nt
      TT=T(I)
      IF (TT.GE.TMIN) GO TO 430
      WRITE(6,425) TT
425   FORMAT(/' >>> T =',F10.2,' OUT OF RANGE OF GROUP DATA. PROPERTIES
     *MAY HAVE LARGE ERRORS')
      LINES=LINES+1
430   TLN=DLOG(TT)
      K=1
      IF (TT.GT.1000.) K=2
      HHRT(I)=((((COEF(5,K)/5.)*TT+COEF(4,K)/4.)*TT+COEF(3,K)/3.)*TT+
     * COEF(2,K)/2.)*TT+COEF(1,K)+COEF(9,K)/TT
      SR(I)=((((COEF(5,K)/4.)*TT+COEF(4,K)/3.)*TT+COEF(3,K)/2.)*TT+
     * COEF(2,K))*TT+COEF(1,K)*TLN+COEF(10,K)
      CPR(I)=(((COEF(5,K)*TT+COEF(4,K))*TT+COEF(3,K))*TT+COEF(2,K))*TT+
     *  COEF(1,K)
      GHRT(I)=SR(I)-HHRT(I)
      IF (DABS(TT-298.15D0).GT..01) GO TO 440
      ASINDH=HHRT(I)*TT
      test(8)=.true.
C
440   IF (TT.GE.TMAX) GO TO 480
470   continue
      go to 490
C
480   NT=I
490   NLAST=NT
      NT1=NT+1
      TEST(1)=.TRUE.
      TEST(9)=.TRUE.
      TEST(18)=.TRUE.
575   REWIND IOGRP
      BACKSPACE 5
      CALL INPUT
      RETURN
      END
C
      SUBROUTINE HMAT (N1,N2,ns,nb,EL)
C     Extended Jan.2002.
      SAVE
C
      LOGICAL cosine
      DOUBLE PRECISION EL(N2),hs
C
      double precision sn,bn,st,hint
      COMMON /BLK1/ SN(9),BN(9),ST
      COMMON /BLK3/ HINT(188,188)
C
      Do 10 i=1,N2
      Do 10 j=i,N2
10    hint(i,j) = 0.
      if (N1.eq.1) then
        cosine = .false.
        nn = 0
      elseif (N1.eq.2) then
        cosine = .true.
        nn = 1
        hint(1,1) = ST
      endif
C
      DO 70 i=1+nn,N2
        if (cosine.and.i.le.ns+1)hint(1,i) = hint(1,i)-.707107d0*SN(i-1)
        k=i-1
        do 60 j=1,ns
        hs = SN(j)*.5d0
        jj = i + j
        if (jj.gt.N2) goto 65
        hint(i,jj) = hint(i,jj) - hs
        IF (j.ge.2*(i-nn)) then
          k=k+1
          if (cosine) hs=-hs
          hint(i,k) = hint(i,k) + hs
        ENDIF
60      continue
65    hint(i,i) = hint(i,i) + (i-nn)**2 + ST
70    CONTINUE
C
      Do 80 I = 1,nb
        noffd = n2 - i
        do 75 j=1,noffd
          jpi = j+i
75      hint(j+nn,jpi+nn) = hint(j+nn,jpi+nn) + DFLOAT(j*jpi)*.5d0*bn(i)
80    Continue
C
      do 114 m=1,nb
      do 114 mp=m,nb
        I = m+mp
        if (I.gt.nb) goto 114
        j = m+nn
        k = mp+nn
        mmp = DFLOAT(m*mp)
        if (cosine) mmp = -mmp
        HINT(j,k) = HINT(j,k) + .5d0*mmp*BN(I)
114   continue
C
118   DO 120 I=1,N2
      DO 120 J=I,N2
120   HINT(J,I)=HINT(I,J)
C
      CALL EIGEN (N2)
      DO 130 I=1,N2
130   EL(I)=HINT(I,I)
      RETURN
      END
      SUBROUTINE IDENT
      SAVE
      PARAMETER (IOSCH=14)
C
C  FROM FORMULA, DETERMINE--
C      1)  PHASE OF SPECIES,
C      2)  NUMBER OF ATOMS IN SPECIES,
C      3)  MOLECULAR WEIGHT,
C      4)  IF ION, NUMBER OF ELECTRONS ADDED OR SUBTRACTED FROM NEUTRAL
C              SPECIES.
      DIMENSION A(24),NO(20),NUM(20),FM(10)
C
      CHARACTER*4 A,PLUS,AMINUS,PARNSL,FM
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      DATA PLUS/'+'/, AMINUS/'-'/, PARNSL/'('/
      DATA FM/'1','2','3','4','5','6','7','8','9','0'/
C
      DO 10 I=1,20
      NO(I)=0
10    NUM(I)=0
      READ (IOSCH,20) (A(I),I=1,24)
20    FORMAT (24A1)
      BACKSPACE IOSCH
C
C WHICH CHARACTERS ARE NUMBERS AND WHAT ARE THEY
C
C     INO=NUMBER OF NUMBERS
C     NUM(I)= LOCATION OF NUMBERS IN IA ARRAY
C     NO(I)= NUMBERS IN THESE LOCATIONS
C
      WEIGHT=0.0
      INO=0
      IX=0
      IONNUM=0
      DO 70 N=2,24
      DO 40 I=1,10
      IF (A(N).EQ.FM(I)) GO TO 50
40    CONTINUE
      IF (A(N).EQ.BLANK) GO TO 80
      IF (A(N).EQ.PLUS) IONNUM=IONNUM-1
      IF (A(N).EQ.AMINUS) IONNUM=IONNUM+1
      GO TO 70
50    IF (INO.NE.0.AND.N.GT.NUM(INO)+3) GO TO 90
      IF (I.EQ.10) I=0
      IF (IONNUM.EQ.0) GO TO 60
      IF (IONNUM.LT.0) I=-I
      IF (IX.NE.0) IX=10*IONNUM+I
      IF (IX.EQ.0) IX=I
      IONNUM=IX
      GO TO 70
60    INO=INO+1
      NO(INO)=I
      NUM(INO)=N
70    CONTINUE
C
C  IF NO NUMBERS (INO=0) PROBABLY NOT A FORMULA RECORD.  RETURN TO PAC1.
C
80    IF (INO.NE.0) GO TO 110
90    WRITE (6,100)
100   FORMAT (/' ERROR IN ABOVE RECORD, IGNORE CONTENTS, (IDENT)')
      RETURN
110   IF (IONNUM.EQ.0) GO TO 120
C
C  IONIC SPECIES,  CALCULATE CORRECTION TO MOLECULAR WEIGHT.
C
      TEST(3)=.TRUE.
      FIONNO=IONNUM
      WEIGHT=FIONNO*ATMWT(1)
      GO TO 150
120   NEXT=NUM(INO)+1
C
C  DETERMINE PHASE OF SPECIES.
C
      IF (A(NEXT).EQ.BLANK.OR.A(NEXT+1).EQ.'G') GO TO 150
      IF (A(NEXT).EQ.PARNSL) GO TO 140
      WRITE (6,130)
C
130   FORMAT (' ERROR IN NAME, GO TO NEXT SPECIES, (IDENT)')
      TEST(16)=.TRUE.
      RETURN
140   IF (A(NEXT+1).EQ.'L') TEST(5)=.TRUE.
      IF (A(NEXT+1).NE.'L') TEST(6)=.TRUE.
      NPLUS=NEXT+1
      FAZ=A(NPLUS)
      GO TO 160
150   TEST(4)=.TRUE.
      FAZ='G'
C
160   I=1
      J=1
      K=0
      DO 170 LMN=1,5
      FORMLA(1,LMN)=BLANK
      FORMLA(2,LMN)=BLANK
170   MLA(LMN)=0
      NOATMS=0
C
C  STORE EACH ATOMIC SYMBOL IN FORMLA(J).  NUMBER OF ATOMS IN MLA(J).
C
180   FORMLA(1,J)=A(K+1)
      IF (NUM(I).EQ.(K+3)) FORMLA(2,J)=A(K+2)
      IF ((NUM(I)+1).EQ.NUM(I+1)) GO TO 190
      MLA(J)=NO(I)
      GO TO 200
190   MLA(J)=10*NO(I)+NO(I+1)
      I=I+1
C
C  NOATMS = TOTAL NUMBER OF ATOMS IN MOLECULE.
200   NOATMS=NOATMS+MLA(J)
C
C FIND ATOM FORMULA IN SYMBOL TABLE
C
      DO 220 L=1,100
      DO 210 II=1,2
      IF (FORMLA(II,J).NE.SYMBOL(II,L)) GO TO 220
210   CONTINUE
      GO TO 250
220   CONTINUE
230   WRITE (6,240)
240   FORMAT (/' ATOM RECORD MISSING OR FORMULA INCORRECT, (IDENT)' )
      WEIGHT=0
      GO TO 260
C
C  CALCULATE MOLECULAR WEIGHT.
C  STORE POSITION OF ELEMENT DATA IN JF.
C
250   JF(J)=L
      IF (ATMWT(L).EQ.0.0) GO TO 230
      WEIGHT=WEIGHT+ATMWT(L)*FLOAT(MLA(J))
260   IF (INO.LE.I) GO TO 270
      K=NUM(I)
      I=I+1
      J=J+1
      GO TO 180
270   IF (.NOT.TEST(3)) GO TO 280
      J=J+1
      JF(J)=1
      FORMLA(1,J)=SYMBOL(1,1)
      MLA(J)=IONNUM
C
C  NKIND = NUMBER OF ELEMENTS IN FORMULA.
C
280   NKIND=J
C
C  ADJUST FORMULA TO COMBINE LIKE ELEMENTS.
C
      NKINDS=NKIND
      IF (NKIND.EQ.1) GO TO 350
      DO 340 J=2,NKINDS
      JK=J-1
      DO 310 JJ=1,JK
      DO 300 II=1,2
      IF (FORMLA(II,JJ).NE.FORMLA(II,J)) GO TO 310
300   CONTINUE
      GO TO 320
310   CONTINUE
      GO TO 340
320   MLA(JJ)=MLA(JJ)+MLA(J)
      NK=NKIND
      NKIND=NKIND-1
      DO 330 K=J,NKIND
      FORMLA(1,K)=FORMLA(1,K+1)
      FORMLA(2,K)=FORMLA(2,K+1)
      MLA(K)=MLA(K+1)
330   CONTINUE
      FORMLA(1,NK)=BLANK
      FORMLA(2,NK)=BLANK
      MLA(NK)=0
      IF(J+1.GE.NK)GO TO 350
340   CONTINUE
350   RETURN
      END
C
C   READ AND WRITE INPUT RECORDS, USES SCRATCH TAPE 4
C
      SUBROUTINE INPUT
      SAVE
      PARAMETER (IOSCH=14)
C
C  READ AND WRITE INPUT RECORDS.
C  USES SCRACH TAPE IOSCH.
C
      CHARACTER*4 A,B,VFM,AN,AWD,REFN,EFDAT,ANAME
      CHARACTER*4 XREAD
      LOGICAL VD
      DIMENSION A(4),VFM(17),AN(11),AWD(18,4),XREAD(2,4)
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      DATA A/',A1',',A2',',A3',',A4'/,EFDAT/'EFDA'/,ANAME/'NAME'/
      DATA AN/'0','1','2','3','4','5','6','7','8','9','.'/
      DATA REFN/'REFN'/
      DATA VFM/'(6X', ',A4', ',A2', ',D12', '.0', ',A4', ',A2', ',D12',
     1 '.0', ',A4', ',A2', ',D12', '.0', ',A4', ',A2', ',D12', '.0)'/
C
      READ (5,20,END=260) CODE,(AWD(I,1),I=1,18)
20    FORMAT (20A4)
      WRITE (IOSCH,20) CODE,(AWD(I,1),I=1,18)
      WRITE (6,30) CODE,(AWD(I,1),I=1,18)
30    FORMAT (/,1x,20A4)
      IF (CODE(1).EQ.REFN) GO TO 270
      BACKSPACE IOSCH
      IF (CODE(1).EQ.ANAME) GO TO 50
C
C  EFDATA RECORD
C
      IF (CODE(1).NE.EFDAT) GO TO 60
      READ (IOSCH,40) FORMLA(1,1),FORMLA(2,1),MLA(1),FAZ,DATE,
     1 (ABEL(1,I),WORD(I),I=2,4),IC80
40    FORMAT (6X,2A1,I1,A1,3X,2A4,3X,3(A4,2X,D12.0),I2)
      GO TO 270
C
C  NAME RECORD
C
50    NNA=MIN0(NNA+1,6)
      READ(IOSCH,55)(NAME(I,NNA),I=1,19)
55    FORMAT(6X,18A4,A2)
      GO TO 270
C
60    IF (CODE(1).EQ.'METH'.OR.CODE(1).EQ.'DATE') TEST(7)=.FALSE.
      IF (TEST(7)) GO TO 80
      READ (IOSCH,70) (ABEL(1,I),ABEL(2,I),WORD(I),I=1,4),IC80
70    FORMAT (6X,4(A4,A2,D12.0),I2)
      GO TO 250
C
C  DATA RECORDS FOLLOWING METH RECORD
C
C  SOME DEFINITIONS --
C    FWORD - FIRST NUMBER
C    D(I) - DEGENERACY OR SECOND NUMBER, I.E. D(I)=3 IN V12(3)
C    LIT - NUMBER OF INITIAL LITERALS
C    NDEC - IF DECIMAL APPEARS IN LABEL, NDEC < 0
C    VD - IF TRUE, LABEL IS V WITH A DEGENERACY, SEE D(I)
C
80    READ (IOSCH,85) CODE,AWD,IC80
85    FORMAT (A4,A2,72A1,I2)
      BACKSPACE IOSCH
      DO 200 I=1,4
      D(I)=0.
      FWORD(I)=-1.
      LIT=0
      NDEC=0
      ABEL(1,I)=BLANK
      ABEL(2,I)=BLANK
      VD=.FALSE.
      DO 180 J=1,6
      B=AWD(J,I)
      DO 90 N=1,11
      IF (B.EQ.AN(N)) GO TO 130
90    CONTINUE
      IF (VD) GO TO 190
      IF (B.NE.BLANK) GO TO 110
      IF (LIT.EQ.0) GO TO 180
      GO TO 190
110   IF(FWORD(I).EQ.-1.) LIT=LIT+1
      IF(FWORD(I).GT.0.) VD=.TRUE.
      GO TO 180
130   IF(N.EQ.11) GO TO 140
      IF(VD) GO TO 160
      IF(NDEC.LT.0) GO TO 150
      IF(FWORD(I).GT.0.) FWORD(I)=FWORD(I)*10.+FLOAT(N-1)
      IF(FWORD(I).EQ.-1.) FWORD(I)=N-1
      GO TO 180
C
140   NDEC=-1
      FWORD(I)=MAX(FWORD(I),0.)
      GO TO 180
150   FWORD(I)=FWORD(I)+FLOAT(N-1)*10.**NDEC
      NDEC=NDEC-1
      GO TO 180
160   IF (D(I).NE.0.) D(I)=10.*D(I)+FLOAT(N-1)
      IF (D(I).EQ.0.) D(I)=N-1
180   CONTINUE
C
190   IVFM=4*I-2
      IVFM2=IVFM+1
      I1=4
      I2=2
      IF(LIT.EQ.0) GO TO 195
      IF(LIT.GT.1) GO TO 192
      ABEL(1,I)=AWD(1,I)
      GO TO 195
192   IF (LIT.LT.4) GO TO 193
      IF (LIT.EQ.5) ABEL(2,I)=AWD(5,I)
      GO TO 195
193   I1=LIT
      I2=6-I1
C
195   VFM(IVFM)=A(I1)
      VFM(IVFM2)=A(I2)
200   CONTINUE
C
      READ (IOSCH,VFM) (XREAD(1,I),XREAD(2,I),WORD(I),I=1,4)
      DO 220 I=1,4
      IF(ABEL(1,I).EQ.BLANK) ABEL(1,I)=XREAD(1,I)
      IF(ABEL(2,I).EQ.BLANK) ABEL(2,I)=XREAD(2,I)
220   CONTINUE
C
250   BACKSPACE IOSCH
      GO TO 270
260   CODE(1)='STOP'
270   RETURN
      END
      SUBROUTINE INTROT (T,HCK,DQ,DDQ,QLN,L,LT)
      SAVE
C   CALLED FROM LINK1
C     CALCULATES CONTRIBUTION OF INTERNAL ROTATION
C     ELS ARE THE (-) ENERGY LEVELS OF THE SIN FUNCTION
C     ELC ARE THE (+) ENERGY LEVELS OF THE COS FUNCTION
C     ETA IS A VECTOR OF THE COMBINED ENERGY LEVELS,THE FIRST NEL
C     MEMBERS ARE THE POSTIVE COS LEVELS FOLLOWED BY THE SIN LEVELS
C
      DOUBLE PRECISION ETA,ELS,ELC,T,HCK
      DOUBLE PRECISION Q,TSD,TSDD,ETAS,ARX,TP1
      DOUBLE PRECISION DQ,DDQ,QLN,TQ
      DIMENSION ETA(375)
C 
      COMMON /BLK2/ ELC(188,4),ELS(187,4),LL(4),RSYM(4),LR(4)
C
      NEL=LL(L)
      LT=LR(L)
C
C  FREE ROTOR
C
      IF (NEL.NE.0) GO TO 15
      Q=DSQRT(ELC(1,L)*T)
      DQ=.5D0
      DDQ=-.5D0
      GO TO 40
C
15    NEL2=2*NEL
      I=NEL
      J=1
      DO 20 K=1,NEL
      ETA(J)=ELC(I,L)
      J=J+1
      ETA(J)=ELS(I,L)
      J=J+1
      I=I-1
20    CONTINUE
      Q=0.
      TSD=0.
      TSDD=0.
C
      DO 30 I=1,NEL2
      ETAS=ETA(I)*HCK
      ARX=-ETAS/T
      IF (ARX.LT.-100.d0) GO TO 30
      TP1=DEXP(ARX)
      Q=TP1+Q
      TSD=TP1*ETAS+TSD
      TSDD=TP1*ETAS*ETAS+TSDD
30    CONTINUE
      TQ=T*Q
      DQ=TSD/TQ
      DDQ=((TSDD-TSD*TSD/Q)/T-2.*TSD)/TQ
40    IF (Q .GT. 1.D-25) QLN=DLOG(Q/RSYM(L))
      RETURN
      END
      SUBROUTINE IROTOR (IROT,NIR)
      SAVE
C
C  CONTRIBUTIONS FOR INTERNAL ROTORS - CALLED FROM SUBROUTINE POLY.
C
C  MAXIMUM NUMBER OF UNIQUE ROTORS IS 8.
C  NIR IS THE TOTAL NUMBER OF ROTORS.
C  LIR IS THE NUMBER OF ROTORS WITH PARTICULAR CHARACTERISTICS.
C  THE MAXIMUM NUMBER OF ENERGY LEVELS IS 187.
C  I2PI IS THE NUMBER OF ANGLES IN 2-PI RADIANS.
C  7/25/88 - POTENTIAL CONSTANTS V IN INPUT IN INVCM RATHER THAN CAL.
C
      LOGICAL IROT,FREE
      DOUBLE PRECISION POT,BROT(10),PHI,V,VN,V0,VMIN
      DIMENSION PHI(361),POT(361),IEL0(10),IEL1(10),V(9),VN(9)
      DOUBLE PRECISION PIM,TB0,FJ
C
      CHARACTER*4 RB,RIB,RNEL,RNOUT,RV,ROTORS,ROSYM,ANGLES
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      double precision sn,bn,st,elc,els,ELOW
      COMMON /BLK1/ SN(9),BN(9),ST
      COMMON /BLK2/ ELC(188,4),ELS(187,4),LL(4),RSYM(4),LR(4)
C
      DATA RB/'BROT'/, RIB/'IB'/, RNEL/'NEL'/, RNOUT/'NOUT'/, RV/ 'V'/
      DATA ROTORS/'NROT'/,  ROSYM/'ROSY'/, ANGLES/'ANGL'/
C
      KIR=0
      L=0
      NIQUE=1000
10    CALL INPUT
      IF (NIQUE.NE.1000) GO TO 50
C
20    V0=0.
      NIQUE=IC80
      L=L+1
      FREE=.TRUE.
      DO 30 I=1,9
      V(I)=0.
      VN(I)=0.
30    CONTINUE
      LIR=1
      IROT=.FALSE.
      I2PI=201
      RSYM(L)=1.
      NEL=100
      LL(L)=NEL
      NOUT=0
      DO 40 I=1,10
40    BROT(I)=0.D0
C
50    IF (SUB.NE.CODE(1).OR.IC80.NE.NIQUE) GO TO 180
      DO 170 ID=1,4
      IF (ABEL(1,ID).EQ.BLANK) GO TO 170
      I=FWORD(ID)
      IF (I.LT.0) I=0
      IF (ABEL(1,ID).EQ.RB) GO TO 70
      IF (ABEL(1,ID).EQ.RIB) GO TO 90
      IF (ABEL(1,ID).EQ.RNEL) GO TO 100
      IF (ABEL(1,ID).EQ.RNOUT) GO TO 110
      IF (ABEL(1,ID).EQ.ROTORS) GO TO 120
      IF (ABEL(1,ID).EQ.RV) GO TO 130
      IF (ABEL(1,ID).EQ.ROSYM) GO TO 150
      IF (ABEL(1,ID).EQ.ANGLES) GO TO 160
      WRITE (6,60) ABEL(1,ID),WORD(ID)
60    FORMAT (/,A4,' IS AN INCORRECT LABEL FOR --',E16.8,'. THUS THE V
     1ALUE WAS IGNORED (IROTOR)')
      GO TO 400
C
70    IF (I.GT.1) GO TO 80
      BROT(1)=WORD(ID)
      GO TO 170
80    BROT(I)=WORD(ID)
      GO TO 170
90    BROT(1)=2.7992774d0/WORD(ID)
      GO TO 170
100   NEL=WORD(ID)
      LL(L)=NEL
      GO TO 170
110   NOUT=WORD(ID)
      GO TO 170
120   LIR=WORD(ID)
      LR(L)=LIR
      KIR=KIR+LIR
      GO TO 170
130   if(i.eq.0) v0=word(id)
      if(i.gt.0) VN(I)=WORD(ID)
      GO TO 170
150   RSYM(L)=WORD(ID)
      GO TO 170
160   I2PI=WORD(ID)
170   CONTINUE
      GO TO 10
C
180   DO 190 I=1,9
      IF (VN(I).EQ.0.) GO TO 190
      V(I)=VN(I)/0.3497551D0
      FREE=.FALSE.
190   CONTINUE
      IF (BROT(2).NE.0.) FREE=.FALSE.
      IF (FREE) GO TO 285
C
      VMIN=1.E5
      PIM=I2PI-1
      DO 230 I=1,I2PI
      PHI(I)= (2*I-2)/PIM
      POT(I)=V0
      DO 220 J=1,9
      IF (V(J).EQ.0.0) GO TO 220
      FJ=J
      POT(I)=POT(I)+0.5*VN(J)*(1.0-DCOS(FJ*PHI(I)*3.14159265D0))
220   CONTINUE
      IF (POT(I).LT.VMIN) VMIN=POT(I)
230   CONTINUE
      WRITE (6,310) VMIN
      VMIN=ABS(VMIN)
      DO 240 I=1,10
      IEL0(I)=I-1
240   IEL1(I)=I
      IF (.NOT.TEST(14)) GO TO 260
      WRITE (6,320)
      DO 250 J=1,I2PI,10
      K=J+9
      IF (K.GT.I2PI) K=I2PI
      WRITE (6,330) (PHI(I),I=J,K)
250   WRITE (6,340) (POT(I),I=J,K)
260   CONTINUE
      ns=0
      nb=0
      do 265 i=9,1,-1
        if (ns.eq.0.and.vn(i).ne.0) ns=i
        if (nb.eq.0.and.brot(i).ne.0) nb=i
265   continue
      WRITE (6,350) (V(i),i=1,ns)
      WRITE (6,351) (VN(i),i=1,ns)
      WRITE (6,352) (BROT(i),i=1,nb)
      IF (NEL.GT.187) GO TO 300
      DO 268 I=2,nb
268   BN(I-1)=BROT(I)/BROT(1)
      nb = nb-1
      TB0=2.0*BROT(1)
      ST=0.0
      DO 270 I=1,ns
      SN(I)=VN(I)/TB0
270   ST=ST+SN(I)
      ST=ST+VMIN/BROT(1)
      NELP1=NEL+1
      CALL HMAT (1,NEL,ns,nb,ELS(1,L))
      CALL HMAT (2,NELP1,ns,nb,ELC(1,L))
      DO 275 I=1,NEL
      ELS(I,L)=BROT(1)*ELS(I,L)
275   ELC(I,L)=BROT(1)*ELC(I,L)
      IF (NOUT.LE.0) GO TO 277
      WRITE (6,360) (IEL0(I),I=1,10)
      CALL PRINT (NOUT,L)
277   ELOW=ELC(1,L)
      IF (ELS(1,L).LT.ELOW) ELOW=ELS(1,L)
      DO 280 I=1,NEL
      ELS(I,L)=ELS(I,L)-ELOW
280   ELC(I,L)=ELC(I,L)-ELOW
      IF (NOUT.LE.0) GO TO 295
      WRITE (6,370) (IEL0(I),I=1,10)
      CALL PRINT (NOUT,L)
      GO TO 295
C
C  FREE ROTOR - SET LEVELS TO ZERO.
C
285   LL(L)=0
      ELC(1,L)=3.14159265D0/(BROT(1)*HCK)
      WRITE (6,290) NIQUE
290   FORMAT (/,'  V=0 FOR ROTOR',I3,'. USE CLASSICAL PARTITION FUNCTION
     1 FOR FREE ROTOR (IROTOR).')
C
295   IF (KIR.GT.0) GO TO 298
      LR(1)=1
      KIR=1
      WRITE (6,296)
296   FORMAT(/,' NROT ASSUMED TO BE 1 (IROTOR)')
298   IF (SUB.NE.CODE(1)) GO TO 390
      IF (KIR.LT.NIR.AND.L.LT.4) GO TO 20
      GO TO 390
300   WRITE (6,380)
      GO TO 400
C
C         SOME FORMAT STATEMENTS
C
310   FORMAT (/,'  MINIMUM OF POTENTIAL FUNCTION IS',F9.3,' CM**(-1)')
320   FORMAT ('-',2X,'VALUES OF POTENTIAL FUNCTION IN CM**(-1) AT VARIOU
     1S ANGLES IN PI RADIAN')
330   FORMAT (11X,'PHI',7X,10F11.5)
340   FORMAT (11X,'POT',5X,10F11.3)
350   FORMAT (2X,'BARRIER POTENTIAL CONSTANTS IN CAL/MOL:'/5x,9F11.3)
351   FORMAT (2X,'BARRIER POTENTIAL CONSTANTS IN CM**(-1):'/5x,9F11.3/)
352   FORMAT (2X,'ROTATION CONSTANTS IN CM**(-1):',6X,(6F11.5))
360   FORMAT ('-',2X,'ENERGY LEVELS',10X,'EVEN----COS----ELC(+) IN CM**(
     1-1)',5X,'ODD----SIN----ELS(-) IN CM**(-1)',//,16X,10I11)
370   FORMAT ('-',2X,'ENERGY DIFFERENCE BETWEEN THE SPECIFIC LEVEL AND
     1THE LOWEST LEVEL IN CM**(-1)',5X,'TO BE USED FOR THERMODYNAMIC FUN
     2CTIONS',//16X,10I11)
380   FORMAT ('-',2X,'THE NUMBER OF ENERGY LEVELS IS TOO LARGE.')
390   IROT=.TRUE.
400   RETURN
      END
C
      real*8 function figs(xin)
C
C     Retain 9 rounded figures 
C
      real*8 xin,xout
      xout=dabs(xin)
      if (xout.gt.0.d0) then
         iex=-anint(dlog10(xout))+9
         xout=anint(xout*10.d0**iex)*10.d0**(-iex)
      endif
      figs=sign(xout,xin)
      END
      SUBROUTINE LEAST
      SAVE
      real*8 figs
      DOUBLE PRECISION A(15,16),TP, ANSTPY(15),F(4),FC(4),ERR(4),TFIX,
     1TOTERR(4),TOTREL(4),TOTSQ(4),TOTSQR(4),AVERR(4),RELERR(4),DH,
     2AVREL(4),AVSQ(4),AVSQR(4),MAXERR(4),MAXREL(4),TMAX(4),TMAXRL(4)
C
      DOUBLE PRECISION CPRFIX,EXI,EXJ,EXIJ,HHRTFX,HRTC,
     1 SRFIX,TEMPY,DLT,RPT,CP,HH,ICP,IS,IH,S,SFXS,TFXS,CFXS,HFXS
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      NCF=10
      its=0
      NCF1=NCF-1
      DO 20 I=1,8
20    INTV(I)=.FALSE.
      IF (NTRANG.EQ.0) GO TO 130
C
C  SORT IN INCREASING ORDER TEMPERATURES SPECIFYING INTERVALS
C
60    J=1
70    M=J
80    DO 100 I=J,NTRANG
      IF (TRANGE(I).GE.TRANGE(M)) GO TO 100
      M=I
100   CONTINUE
      IF (M.EQ.J) GO TO 120
      TEMPY=TRANGE(M)
      TRANGE(M)=TRANGE(J)
      TRANGE(J)=TEMPY
      GO TO 80
120   J=J+1
      IF (NTRANG.GT.J) GO TO 70
      go to 140
C
C  Set default TRANGE
C
130   TRANGE(1)=200.D0
      IF(TEST(3)) TRANGE(1)=298.15D0
      TRANGE(2)=1000.D0
      NTRANG=3
      if (.not.OLD.and..not.TEST(4)) ntrang=2
      TRANGE(NTRANG)=6000.0D0
140   TEST(1)=.false.
      if (t(nlast).le.trange(1).or.t(nnn).ge.trange(ntrang)) go to 2000
c
c  if transition point, insert initial t in trange schedule.
c
      if(nnn.eq.1.or.t(nnn).ne.t(nnn-1))go to 142
      do 141 i=1,ntrang
      if(dabs(t(nnn)-trange(i)).lt..001) go to 142
141   continue
      ntrang=ntrang+1
      trange(ntrang)=t(nnn)
      go to 60
142   TEST(1)=.true.
      CALL PAGEID
      WRITE (6,143)
143   FORMAT (/,' LEAST SQUARES' /)
      LINES=9
      IF (T(NNN).NE.TLST .OR. T(NNN).NE.T(NNN-1)) GO TO 153
      TCONST=TLST
      DH=HHRT(NNN)-HHRT(NNN-1)
      HHRTFX=HLST+DH
      SRFIX=SLST+DH
      GO TO 155
153   TLST=0.D0
      IF (TCONST.NE.0.) GO TO 155
      TCONST=DMAX1(TRANGE(1),298.15D0)
155   TFIX=TCONST
      IF (TFIX.GT.T(NLAST)) TFIX=T(NLAST)
      IF (NNN.EQ.1) GO TO 160
      if (tconst.ge.t(nnn) .and. tconst .le.t(nlast)) go to 160
      IF (T(NNN-1).EQ.T(NNN).OR.TFIX.LT.T(NNN)) TFIX=T(NNN)
C
160   NM1=NTRANG-1
      LT=NM1
      IT=1
      IF ((TRANGE(IT).GE.T(NNN)-.00001D0).AND.(TRANGE(NTRANG).LE.
     1 T(NLAST)+.00001D0)) GO TO 220
      LINES=LINES+2
      IT1=IT
      DO 170 I=IT1,NM1
      IF (TRANGE(I).EQ.TRANGE(I+1))GO TO 165
      IF (TRANGE(I).GE.T(NNN)-.00001D0) GO TO 190
      IF (T(NNN).LT.TRANGE(I+1)-.00001D0) GO TO 190
165   INTV(IT)=.TRUE.
      IT=IT+1
170   CONTINUE
190   DO 200 I=IT,NM1
      IF (TRANGE(LT).LE.T(NLAST)-.00001D0) GO TO 220
      IF (T(NLAST).GT.TRANGE(LT)+.00001D0) GO TO 220
      INTV(LT)=.TRUE.
      LT=LT-1
200   CONTINUE
C
C  LOCATE TEMPERATURE CONSTRAINTS
220   IT1=IT
      DO 230 I=IT1,LT
      IT=I
      IF (TRANGE(I+1).GT.TFIX-.00001D0) GO TO 240
230   CONTINUE
C
C  ADJUST TEMPERATURE INTERVALS, IF NECESSARY
C
240   DO 270 I=NNN,NLAST
      ICON=I
      IF (DABS(T(I)-TFIX).LT.0.00001D0) GO TO 280
270   CONTINUE
280   CPRFIX=CPR(ICON)
      IF (TLST.NE.0.) GO TO 285
      HHRTFX=HHRT(ICON)
      SRFIX=SR(ICON)
285   TLST=0.D0
      izz=3
      IF (OLD) izz=1
      dnf=izz+4
      do 290 i=1,it
      if(.not.intv(i)) go to 292
290   continue
292   iv=ivl1
      do 295 i=1,lt
      if(intv(i)) go to 295
      iv=iv+1
      if(i.ge.it) go to 296
295   continue
296   ivl1=ivl1+1
C
C  IF INTERVAL IS NOT SET, USE DEFAULT EXPONENT SET
C
300   IF (NF(IV).NE.0) GO TO 320
      if (nf(9).eq.0) then
         DO 310 I=1,dnf
310      EX(I,IV)=I-izz
         nf(IV)=dnf
         iz=izz
         go to 330
      else
         nf(iv)=nf(9)
         do 315 i=1,nf(9)
315      ex(i,iv) = ex(i,9)
      endif
C
C  ORDER EXPONENTS
C
320   IF(NF(IV).EQ.1) GO TO 326
      NFD=NF(IV)-1
      DO 324 I=1,NFD
      J1=I+1
      NFE=NF(IV)
      DO 324 J=J1,NFE
      IF (EX(J,IV).GT.EX(I,IV)) GO TO 324
      EXI=EX(I,IV)
      EX(I,IV)=EX(J,IV)
      EX(J,IV)=EXI
324   CONTINUE
C
C  set iz to exponent that is zero
C
326   do 327 i=1,nf(IV)
      if(ex(i,iv).eq.0.) goto 328
327   continue
328   iz=i
330   nfp=nf(IV)
      IF (IV.GT.8) GO TO 1180
      DO 336 I=1,10
336   COEF(I,IV)=0.D0
      DO 338 I=NNN,NLAST
      IF((T(I)+.00001D0).GT.TRANGE(IT)) GO TO 350
338   CONTINUE
      WRITE (6,340)
340   FORMAT (/' LEAST SQUARES NOT COMPLETED. INTERVAL TEMPERATURES NOT
     1FOUND IN TEMPERATURE SCHEDULE, (LEAST)')
      TEST(1)=.false.
      GO TO 1160
350   NBEGIN=I
C
      TC(IV)=T(I)
      DO 360 I=NBEGIN,NLAST
      IF (T(I)-.00001D0.GT.TRANGE(IT+1)) GO TO 370
      NEND=I
360   CONTINUE
370   TC(IV+1)=T(NEND)
      NPTS=NEND-NBEGIN+1
      IF (NPTS.GE.NF(IV)) GO TO 380
374   if (npts .ge. 2*iz-1) go to 378
      iz=iz-1
      do 376 i=iz,10
376   ex(i,iv)=ex(i+1,iv)
      go to 374
378   NFP=NPTS
      do 379 i=npts+1,10
379   ex(i,IV)=0
      NF(IV)=NPTS
380   ICP=1
      IH=1
      IS=1
      IF(NOCP(IV))ICP=0
      IF(NOH(IV))IH=0
      IF(NOS(IV))IS=0
      NF1=NFP+IH
      NF2=NF1+IS
      NF5=NF2
      IF(.NOT.CNST)GO TO 385
      NF3=NF2+ICP
      NF4=NF3+IH
      NF5=NF4+IS
385   NF6=NF5+1
C
C CLEAR MATRIX REGION
C
      DO 400 I=1,NF5
      DO 400 J=1,NF6
400   A(I,J)=0.0
C
C SET UP MATRIX ELEMENTS FOR DIAGONAL AND ABOVE FOR FIRST NF ROWS
C
      DO 722 L=NBEGIN,NEND
      DLT=DLOG(T(L))
      RPT=1.D0/T(L)
      S=SR(L)*IS
      CP=CPR(L)*ICP
      HH=HHRT(L)*IH
      DO 700 I=1,NFP
      EXI=EX(I,IV)
      IF (EXI.NE.(-1.D0)) GO TO 420
      IF(.NOT.NOH(IV))A(I,NF1)=A(I,NF1)+DLT*RPT*RPT
      IF(.NOT.NOS(IV))A(I,NF2)=A(I,NF2)-RPT
      A(I,NF6)=A(I,NF6)+(CP+HH*DLT-S)*RPT
      GO TO 460
C
420   IF (EX(I,IV).NE.0.0) GO TO 440
      IF(.NOT.NOH(IV))A(I,NF1)=A(I,NF1)+RPT
      IF(.NOT.NOS(IV))A(I,NF2)=A(I,NF2)+DLT
      A(I,NF6)=A(I,NF6)+CP+HH+S*DLT
      GO TO 460
C
440   IF(.NOT.NOH(IV))A(I,NF1)=A(I,NF1)+T(L)**(EXI-1.D0)/(EXI+1.D0)
      IF(.NOT.NOS(IV))A(I,NF2)=A(I,NF2)+(T(L)**EXI)/EXI
      A(I,NF6)=A(I,NF6)+(CP+HH/(EXI+1.D0)+S/EXI)*(T(L)**EXI)
C
460   DO 700 J=I,NFP
      EXJ=EX(J,IV)
      IF (EX(I,IV).GT.0..OR.EX(J,IV).LT.-1.) GO TO 690
      EXIJ=EXJ
      IF (EXI.EQ.-1.) GO TO 480
      IF (EXI.EQ.0.) GOTO 470
      EXIJ=EXI
      IF (EXJ.EQ.-1.) GO TO 550
      IF (EXJ.EQ.0.) GO TO 650
      GO TO 690
470   IF (EXJ.EQ.0.) GO TO 620
      GO TO 650
480   IF (EXJ.EQ.0.) GO TO 520
      IF (EXJ.NE.-1.) GO TO 550
      A(I,J)=A(I,J)+(ICP+IS+IH*DLT*DLT)*RPT**2
      GO TO 700
520   A(I,J)=A(I,J)+RPT*(ICP+(IH-IS)*DLT)
      GO TO 700
550   A(I,J)=A(I,J)+(ICP-IS/EXIJ+DLT*IH/(EXIJ+1.D0))*T(L)**(EXIJ-1.D0)
      GO TO 700
620   A(I,J)=A(I,J)+ICP+IH+IS*DLT**2
      GO TO 700
650   A(I,J)=A(I,J)+(ICP+IH/(EXIJ+1.D0)+IS*DLT/EXIJ)*T(L)**EXIJ
      GO TO 700
690   A(I,J)=A(I,J)+(ICP+IH/((EXI+1.D0)*(EXJ+1.D0))+IS/(EXI*EXJ))
     1 *T(L)**(EXI+EXJ)
700   CONTINUE
C
C SET UP MATRIX FOR DIAGONAL AND ABOVE FOR REMAINING ROWS
C
      IF(NOH(IV)) GO TO 702
      A(NF1,NF1)=A(NF1,NF1)+RPT**2
      A(NF1,NF6)=A(NF1,NF6)+HH*RPT
702   IF(NOS(IV)) GO TO 722
      A(NF2,NF2)=A(NF2,NF2)+1.D0
      A(NF2,NF6)=A(NF2,NF6)+S
722   CONTINUE
C
      IF (.NOT.CNST)GO TO 728
      IF(.NOT.NOH(IV))A(NF1,NF4)=1.D0/TFIX
      IF(.NOT.NOS(IV))A(NF2,NF5)=1.D0
      IF(.NOT.NOCP(IV))A(NF3,NF6)=CPRFIX
      IF(.NOT.NOH(IV))A(NF4,NF6)=HHRTFX
      IF(.NOT.NOS(IV))A(NF5,NF6)=SRFIX
      DO 726 I=1,NFP
      EXI=EX(I,IV)
      IF(EXI.NE.(-1.D0))GO TO 723
      IF(.NOT.NOCP(IV))A(I,NF3)=1.D0/TFIX
      IF(.NOT.NOH(IV))A(I,NF4)=DLOG(TFIX)/TFIX
      IF(.NOT.NOS(IV))A(I,NF5)=-1.D0/TFIX
      GO TO 726
723   IF(EXI.NE.0.)GO TO 724
      IF(.NOT.NOCP(IV))A(I,NF3)=1.D0
      IF(.NOT.NOH(IV))A(I,NF4)=1.D0
      IF(.NOT.NOS(IV))A(I,NF5)=DLOG(TFIX)
      GO TO 726
724   TEMPY=TFIX**EXI
      IF(.NOT.NOCP(IV))A(I,NF3)=TEMPY
      IF(.NOT.NOH(IV))A(I,NF4)=TEMPY/(EXI+1.D0)
      IF(.NOT.NOS(IV))A(I,NF5)=TEMPY/EXI
726   CONTINUE
C
C COMPLETE THE MATRIX BY REFLECTING SYMMETRICAL ELEMENTS ABOVE DIAGONAL
C
728   K=2
      NF5=NF6-1
      NF4=NF5-1
      DO 740 I=1,NF4
      DO 730 J=K,NF5
730   A(J,I)=A(I,J)
740   K=K+1
C
C SOLVE THE MATRIX.
C
      N=NF5
      DO 770 I=1,N
770   ANSTPY(I)=0.0
      DO 800 I=1,N
      DO 780 J=I,N
      A(I,J+1)=A(I,J+1)/A(I,I)
      IF (I.EQ.N) GO TO 810
780   CONTINUE
      K=I+1
      DO 790 II=K,N
      DO 790 JJ=I,N
790   A(II,JJ+1)=-A(II,I)*A(I,JJ+1)+A(II,JJ+1)
800   CONTINUE
810   if (n.ne.nf1) then
         ANSTPY(N)=figs(A(I,J+1))
      else
	 ANSTPY(NF1)=A(I,J+1)
      endif
      IF (N.EQ.1) GO TO 845
      J=N-1
      II=J
      DO 840 I=1,II
      K=J+1
      DO 830 MM=1,I
      ANSTPY(J)=ANSTPY(J)+ANSTPY(K)*A(J,K)
830   K=K+1
      if (j.ne.nf1) then
         ANSTPY(J)=figs(A(J,K)-ANSTPY(J))
      else
         ANSTPY(J)=A(J,K)-ANSTPY(J)
      endif
840   J=J-1
C
845   IF (.NOT.NOH(IV) .AND. .NOT.NOS(IV)) GO TO 860
      NF1=NFP+1
      NF2=NFP+2
      IF (NOH(IV)) ANSTPY(NF2)=figs(ANSTPY(NF1))
      FC(2)=0.
      FC(3)=0.
      DLT=DLOG(TFIX)
      DO 857 I=1,NFP
      EXI=EX(I,IV)
      TP=TFIX**EXI
      IF(EXI.NE.(-1.D0)) GO TO 853
      FC(2)=FC(2)+ANSTPY(I)*DLT/TFIX
      GO TO 855
853   FC(2)=FC(2)+ANSTPY(I)*TP/(EXI+1.D0)
      IF (EXI.NE.0.0)GO TO 855
      FC(3)=FC(3)+ANSTPY(I)*DLT
      GO TO 857
855   FC(3)=FC(3)+ANSTPY(I)*TP/EXI
857   CONTINUE
      IF(NOH(IV))ANSTPY(NF1)=(HHRTFX-FC(2))*TFIX
      IF(NOS(IV))ANSTPY(NF2)=figs(SRFIX-FC(3))
860   intvls=max0(iv,intvls)
      DO 865 I=1,NFP
865   COEF(I,iv)=ANSTPY(I)
      COEF(NCF1,iv)=ANSTPY(NF1)
      COEF(NCF,iv)=ANSTPY(NF2)
C
C CALCULATE FROM THE LEAST SQUARES COEFFICIENTS VALUES OF CP/R,H-H0/RT,
C   S/R,F-H0/RT, AND THE ERRORS AND RELATIVE ERRORS IN THESE AT EACH
C   TEMPERATURE. ALSO THE AVERAGE ERROR, AVERAGE RELATIVE ERROR,
C   LARGEST ERROR AND LARGEST RELATIVE ERROR.
C
      WRITE (6,870)
870   FORMAT (/,7X,1HT,6X,10HCP/R INPUT,4X,9HCP/R CALC,5X,11HHH/RT INPUT
     1,3X,10HHH/RT CALC,6X,9HS/R INPUT,5X,8HS/R CALC,6X,12H-GH/RT INPUT
     2,4X,11H-GH/RT CALC)
      WRITE (6,880)
880   FORMAT (1H ,14X,10HINPUT-CALC,5X,8HFRACTION,6X,10HINPUT-CALC,4X,8H
     1FRACTION,7X,10HINPUT-CALC,5X,8HFRACTION,7X,10HINPUT-CALC,4X,8HFRAC
     2TION)
C
      LINES=LINES+3
      DO 890 I=1,4
      TOTERR(I)=0.
      TOTREL(I)=0.
      TOTSQR(I)=0.
      TOTSQ(I)=0.
      MAXERR(I)=0.
      MAXREL(I)=0.
      TMAX(I)=0.
890   TMAXRL(I)=0.
C
      DO 1030 L=NBEGIN,NEND
      DLT=DLOG(T(L))
      RPT=1.D0/T(L)
      F(1)=CPR(L)
      F(2)=HHRT(L)
      F(3)=SR(L)
      F(4)=GHRT(L)
      FC(1)=0.
      FC(2)=ANSTPY(NF1)*RPT
      FC(3)=ANSTPY(NF2)
C
      DO 920 I=1,NFP
      EXI=EX(I,IV)
      TP=T(L)**EXI
      IF (EXI.NE.(-1.D0)) GO TO 900
      FC(2)=FC(2)+ANSTPY(I)*DLT*RPT
      GO TO 910
900   FC(2)=FC(2)+ANSTPY(I)*TP/(EXI+1.D0)
      IF (EXI.NE.0.0) GO TO 910
      FC(3)=FC(3)+ANSTPY(I)*DLT
      GO TO 920
910   FC(3)=FC(3)+ANSTPY(I)*TP/EXI
920   FC(1)=FC(1)+ANSTPY(I)*TP
      FC(4)=FC(3)-FC(2)
C
      IF (.NOT.NOCP(IV)) GO TO 922
      CPR(L)=FC(1)
      F(1)=FC(1)
922   IF (.NOT. NOH(IV).OR.CPONLY(IV)) GO TO 924
      HHRT(L)=FC(2)
      F(2)=FC(2)
924   IF (.NOT.NOS(IV).OR.CPONLY(IV)) GO TO 926
      F(3)=FC(3)
      SR(L)=FC(3)
926   GHRT(L)=SR(L)-HHRT(L)
      F(4)=GHRT(L)
      IF (L.NE.NBEGIN.OR.IT.EQ.1) GO TO 950
      IF (INTV(IT-1)) GO TO 950
      ITS=IT-1
      CFXS=FC(1)
      HFXS=FC(2)
      SFXS=FC(3)
      TFXS=T(L)
950   DO 990 I=1,4
      ERR(I)=F(I)-FC(I)
      ABSERR=DABS(ERR(I))
      TOTERR(I)=TOTERR(I)+ABSERR
      TOTSQ(I)=TOTSQ(I)+ABSERR*ABSERR
      IF (F(I).NE.0.) GO TO 970
      F(I)=1.
      IF (I.EQ.2.AND.DABS(T(L)-298.15).LT.1.) GO TO 970
      WRITE (6,960)
960   FORMAT (/' ERROR IN DATA.  LEAST SQUARES NOT COMPLETED, (LEAST)')
      TEST(1)=.false.
      GO TO 1160
970   IF (F(I).GT.1.D-06) GO TO 974
      IF (ERR(I).GT.1.D-06) GO TO 974
      RELERR(I)=ERR(I)
      GO TO 976
974   RELERR(I)=ERR(I)/F(I)
976   ABSREL=DABS(RELERR(I))
      TOTREL(I)=TOTREL(I)+ABSREL
      TOTSQR(I)=TOTSQR(I)+ABSREL*ABSREL
      IF (ABSERR.LT.MAXERR(I)) GO TO 980
      MAXERR(I)=ABSERR
      TMAX(I)=T(L)
980   IF (ABSREL.LT.MAXREL(I)) GO TO 990
      MAXREL(I)=ABSREL
      TMAXRL(I)=T(L)
990   CONTINUE
      IF (L.LT.NEND) GO TO 1000
      IF (T(L).LT.TLST) GO TO 1000
      IF (T(L).EQ.TFIX) GO TO 1000
      TLST=T(L)
      HLST=FC(2)
      SLST=FC(3)
1000  CONTINUE
C
      WRITE (6,1010) T(L),CPR(L),FC(1),HHRT(L),FC(2),F(3),FC(3),F(4),
     1 FC(4)
1010  FORMAT (F12.2,2F13.7,2X,2F13.7,2X,2F14.7,2X,2F14.7)
      WRITE (6,1020) (ERR(I),RELERR(I),I=1,4)
1020  FORMAT (12X,2F13.7,2X,2F13.7,2X,2F14.7,2X,2F14.7)
1030  CONTINUE
C
      POINTS=NEND-NBEGIN+1
      DO 1040 I=1,4
      AVERR(I)=TOTERR(I)/POINTS
      AVREL(I)=TOTREL(I)/POINTS
      AVSQ(I)=DSQRT(TOTSQ(I)/POINTS)
1040  AVSQR(I)=DSQRT(TOTSQR(I)/POINTS)
C
C
      IF (NOCP(IV)) GO TO 1055
      WRITE (6,1050) MAXREL(1),TMAXRL(1),AVREL(1),AVSQR(1)
1050  FORMAT (3X,19HMAX REL ERR CP/R  =,F10.6,4X,6HTEMP =,F7.0,6X,20HAVE
     1R REL ERR CP/R  =,F10.6,6X,22HREL LST SQ ERR CP/R  =,F10.6)
      LINES=LINES+1
1055  IF (NOH(IV).AND..NOT.CPONLY(IV)) GO TO 1065
      WRITE (6,1060) MAXREL(2),TMAXRL(2),AVREL(2),AVSQR(2)
1060  FORMAT (3X,19HMAX REL ERR HH/RT =,F10.6,4X,6HTEMP =,F7.0,6X,20HAVE
     1R REL ERR HH/RT =,F10.6,6X,22HREL LST SQ ERR HH/RT =,F10.6)
1065  IF (NOS(IV).AND..NOT.CPONLY(IV)) GO TO 1075
      WRITE (6,1070) MAXREL(3),TMAXRL(3),AVREL(3),AVSQR(3)
1070  FORMAT (3X,19HMAX REL ERR S/R   =,F10.6,4X,6HTEMP =,F7.0,6X,20HAVE
     1R REL ERR S/R   =,F10.6,6X,22HREL LST SQ ERR S/R   =,F10.6)
      LINES=LINES+1
1075  IF (NOH(IV).AND.NOS(IV).AND..NOT.CPONLY(IV)) GO TO 1085
      WRITE (6,1080) MAXREL(4),TMAXRL(4),AVREL(4),AVSQR(4)
1080  FORMAT (3X,19HMAX REL ERR GH/RT =,F10.6,4X,6HTEMP =,F7.0,6X,20HAVE
     1R REL ERR GH/RT =,F10.6,6X,22HREL LST SQ ERR GH/RT =,F10.6)
1085  IF (NOCP(IV)) GO TO 1095
      WRITE (6,1090) MAXERR(1),TMAX(1),AVERR(1),AVSQ(1)
1090  FORMAT (7X,15HMAX ERR CP/R  =,F10.6,4X,6HTEMP =,F7.0,10X,16HAVER E
     1RR CP/R  =,F10.6,10X,18HLST SQ ERR CP/R  =,F10.6)
1095  IF (NOH(IV).AND..NOT.CPONLY(IV)) GO TO 1105
      WRITE (6,1100) MAXERR(2),TMAX(2),AVERR(2),AVSQ(2)
1100  FORMAT (7X,15HMAX ERR HH/RT =,F10.6,4X,6HTEMP =,F7.0,10X,16HAVER E
     1RR HH/RT =,F10.6,10X,18HLST SQ ERR HH/RT =,F10.6)
1105  IF (NOS(IV).AND..NOT.CPONLY(IV)) GO TO 1115
      WRITE (6,1110) MAXERR(3),TMAX(3),AVERR(3),AVSQ(3)
1110  FORMAT (7X,15HMAX ERR S/R   =,F10.6,4X,6HTEMP =,F7.0,10X,16HAVER E
     1RR S/R   =,F10.6,10X,18HLST SQ ERR S/R   =,F10.6)
1115  IF (NOH(IV).AND.NOS(IV).AND..NOT.CPONLY(IV)) GO TO 1125
      WRITE (6,1120) MAXERR(4),TMAX(4),AVERR(4),AVSQ(4)
1120  FORMAT (7X,15HMAX ERR GH/RT =,F10.6,4X,6HTEMP =,F7.0,10X,16HAVER E
     1RR GH/RT =,F10.6,10X,18HLST SQ ERR GH/RT =,F10.6)
1125  WRITE (6,1130) (ANSTPY(I),EX(I,IV),I=1,NFP)
1130  FORMAT(' CP/R =',5(1Pd17.9,3HT**,0PF4.1)/8X,5(1Pd17.9,3HT**,0PF4.1
     1))
      HRTC=figs(ANSTPY(NF1)+ASINDH)
      WRITE (6,1140) ANSTPY(NF1),HRTC,ANSTPY(NF2)
1140  FORMAT (' (H-H0)/R CONSTANT = ',1pd19.11,', H/R CONSTANT =',d19.11
     1 ,', S/R CONSTANT =',d19.11)
      INTV(IT)=.TRUE.
      intvls=max(intvls,iv)
      IF (INTV(lt)) GO TO 1144
      CPRFIX=FC(1)
      HHRTFX=FC(2)
      SRFIX=FC(3)
      TFIX=T(NEND)
      IT=IT +1
      iv=iv+1
      GO TO 300
C
1144  if (its.eq.0) go to 1160
      DO 1147 I=lt,1,-1
      IF (.NOT.INTV(I)) GO TO 1148
1147  CONTINUE
      GO TO 1160
1148  IT=ITS
      CPRFIX=CFXS
      HHRTFX=HFXS
      SRFIX =SFXS
      TFIX=TFXS
      its=0
            iv=it
      IF(IT.GT.0) GO TO 300
C
1160  IF (TC(intvls).GE.TRANGE(NTRANG)) TEST(15)=.FALSE.
      GO TO 2000
C
1180  WRITE(6,1190) iv
1190  FORMAT(/' NUMBER OF TEMPERATURE INTERVALS =',I3,
     1  '--ONLY 8 ALLOWED')
C
2000  RETURN
      END
      SUBROUTINE LINK1 (IROT,NIR)
      SAVE
C
C  CALCULATE Q
C     TESTW(1)  MOLECULE IS NON-LINEAR
C     TESTW(2)  RIGID ROTATOR-HARMONIC OSCILLATOR APPROXIMATION
C     TESTW(3)  SECOND ORDER CORRECTIONS ARE CALLED FOR
C     TESTW(4)  PENNINGTON AND KOBE APPROXIMATION
C     TESTW(5)  JANAF METHOD FOR DIATOMIC MOLECULES
C     TESTW(6)  SPECIES HAS EXCITED ELECTRONIC STATES
C
      LOGICAL IROT
C
      CHARACTER*4 LEL,LHO,LRR,LXIJ,LRHO,LTHETA,LYIJK,LZ
      CHARACTER*4 LALPHA,LAX,LG,LIR,LX2,LXY,LG2,LAX2,LFERMI,LDARD
c
      real*8 dqs,ddqs,qlns,SQ(3,502),EDQ,EQLN,EDDQ,CT,U
      real*8 RS,RF,AL,CON,CTT
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      real*8 v,x,y,alfa,alfb,alfc,g,wx,beta,a,b,c,dard,dd,wf,w,sym,stwt,
     1 t00,theta,rh,ri,s,ql,q,qln,dq,ddq,qtot,qlntot,dqtot,ddqtot,aij,
     2 aiii,ai
      logical testw
      COMMON /WCOMMN/ V(300),X(9,9),Y(9,9,9),ALFA(9),ALFB(9),ALFC(9),
     1 G(9),WX(9),BETA(4),A,B,C,DARD,DD,WF,W,SYM,STWT,T00,THETA(3),
     2 RH(6),RI(300,3),S(300,3),QL(3),Q,QLN,DQ,DDQ,QTOT,QLNTOT,DQTOT,
     3 DDQTOT,AIJ(9,9),AIII,AI(9),DN(300),ND(300),CORT,NNU,TESTW(6)
C
      EQUIVALENCE (TAPE,SQ)
      DATA     LEL/'ELEC' /, LHO/'H.O.'/, LRR/'R.R.'/, LXIJ/'XIJ'/,
     1 LRHO/'RHO '/, LTHETA/'THTA' /, LYIJK/'YIJK'/, LALPHA/'ALFA' /,
     2 LZ  /'WEZE'/, LAX/'AXIJ'/, LG/'G+AG'/, LIR/'IROT'/, LX2 /'XIJ2'/,
     3 LXY /'XY  '/, LG2/'G2GX'/, LAX2 /'AX2 '/, LFERMI/'FERM'/,
     4 LDARD/'DARD'/
C
      KD(IX,JX)=MIN0(IX,JX)/MAX0(IX,JX)
C
C  TEST(14)--INTERM RECORD HAS BEEN READ CALLING FOR INTERMEDIATE OUTPUT
C
      IF (.NOT.TEST(14)) GO TO 30
      DO 10 I=1,NNU
      NND=DN(I)
10    WRITE (6,20) I,V(I),NND
20    FORMAT (/,' V(',I3,') =',F9.4,'(',I1,')')
30    IF (ASINDT.NE.0.0) GO TO 40
      NT1=NT
      GO TO 50
40    NT1=NT+1
      T(NT1)=ASINDT
C
C  DO LOOP CALCULATES Q & DERIVATIVES FOR ELECTRONIC LEVEL. IT=T INDEX.
C
50    DO 430 IT=NIT,NT1
      QTOT=1.0
      QLNTOT=0.0
      DQTOT=0.0
      DDQTOT=0.0
      Q=1.0
      IF (TEST(14)) WRITE (6,60) T(IT)
60    FORMAT (/,' T =',F9.3)
      CT=HCK/T(IT)
      IF (B.NE.0.) GO TO 70
      QLNTOT=SQ(1,IT)
      DQTOT=SQ(2,IT)
      DDQTOT=SQ(3,IT)
      GO TO 120
70    DO 100 I=1,NNU
      IF (V(I).EQ.0.) GO TO 100
      RI(I,1)=0.0
      U=CT*V(I)
      IF (U.GE.30.) GO TO 80
C
C  RI(I,1) = RI.   S(I,1) = SI.  A 2 OR 3 IN THE SECOND SUBSCRIPT
C     INDICATES FIRST OR  SECOND DERIVATIVE RESPECTIVELY OF RI AND SI.
C  THESE DERIVATIVES ARE USED TO OBTAIN THE DERIVATIVES OF THE Q
C     CONTRIBUTIONS IN SUBROUTINE DERIV.
C
      RI(I,1)=dEXP(-U)
80    S(I,1)=1./(1.-RI(I,1))
      RI(I,2)=U
      RI(I,3)=-U
      S(I,2)=RI(I,1)*S(I,1)*U
      S(I,3)=S(I,2)*(S(I,2)+U-1.)
      IF (TEST(14)) WRITE (6,90) U,RI(I,1),S(I,1),I
90    FORMAT (/'   U =',E14.7,'   R =',E14.7,'   S =',E14.7,3X,'I =',I3)
100   CONTINUE
      IF (TEST(14)) WRITE (6,110)
110   FORMAT (/' CONTRIBUTION',13X,1HQ,15X,4HLN Q,11X,8H H-H0/RT,13X,
     1 'CP/R')
C
C  QLN = LN Q.      DQ = TDLNQ/DT.      DDQ = T2D2(LN Q)/DT2.
C  SUBROUTINE QSUM ACCUMULATES CONTRIBUTIONS OF LN Q AND DERIVATIVES.
C
C  ELECTRONIC PARTITION FUNCTION--FORMULA 1.
C
120   DQ=CT*T00
      QLN=DLOG(STWT/SYM)-DQ
      DDQ=-2.0*DQ
      LABEL=LEL
      EDQ=DQ
      EQLN=QLN
      EDDQ=DDQ
      CALL QSUM (TEST(14))
      IF (V(1).EQ.0.) GO TO 390
C
C  HARMONIC OSCILLATOR PARTITION FUNCTION--FORMULA 2.
C
      DO 130 I=1,NNU
      IF (V(I).EQ.0.) GO TO 130
      QLN=QLN+DN(I)*DLOG(S(I,1))
      DQ=DQ+DN(I)*S(I,2)
      DDQ=DDQ+DN(I)*S(I,3)
130   CONTINUE
      DDQ=DDQ-DQ
      CORT=0.
      LABEL=LHO
      CALL QSUM (TEST(14))
C
C  RIGID ROTATOR PARTITION FUNCTION--FORMULAS 3 AND 4.
C
      LABEL=LRR
      IF (TESTW(1)) GO TO 140
      Q=1.0d0/(CT*B)
      DQ=1.0d0
      DDQ=-1.0d0
      GO TO 150
140   Q= dsqrt(3.1415927d0/(A*B*C*CT**3))
      DQ=1.5d0
      DDQ=-1.5d0
150   QLN=DLOG(Q)
      CALL QSUM (TEST(14))
C
C  END RRHO CALCULATIONS.  GO TO 900(C480) TO ACCUMULATE Q FOR LEVEL.
C
      IF (TESTW(2)) GO TO 340
C
C  ROTATIONAL STRETCHING--FORMULA 5.
C  HIGHER ORDER CORRECTION TERMS ADDED OCTOBER, 1986 FOR LINEAR AND
C    SPHERICAL-TOP (A=B=C) MOLECULES.  R.S.MCDOWELL, JCP V39,NO3,P526.
C
      LABEL=LRHO
      IF(RH(1).LT.2.) GO TO 167
      RS=0.
      IF(A.EQ.B.AND.B.EQ.C) RS=DD*HCK/(16.*T(IT))
      RF=RH(6)*T(IT)
      QLN=(((RH(4)*RF+RH(3))*RF+RH(2))*RF+RH(1))*RF+RH(5)+RS
      DQ=(((4.*RH(4)*RF+RH(3)*3.)*RF+2.*RH(2))*RF+RH(1))*RF-RS
      DDQ=((12.*RH(4)*RF+RH(3)*6.)*RF+2.*RH(2))*RF*RF+2.*RS
      GO TO 168
C
167   QLN=RH(1)*T(IT)
      DQ=QLN
      DDQ=0.0
      IF(RH(2).EQ.0) GO TO 168
C
C  CORRECTION FACTOR ADDED JULY, 1986.
C
      RF=(2*RH(2)-RH(1)**2)*T(IT)**2
      QLN=QLN+.5*RF
      DQ=DQ+RF
      DDQ=RF
168   CALL QSUM (TEST(14))
C
C     LOW TEMPERATURE ROTATION -- FORMULA 6.
C
      LABEL=LTHETA
      Q=1.+((THETA(3)/T(IT)+THETA(2))/T(IT)+THETA(1))/T(IT)
      QLN=DLOG(Q)
      DQ=-((3.*THETA(3)/T(IT)+2.*THETA(2))/T(IT)+THETA(1))/T(IT)/Q
      DDQ=((2.*THETA(3)/T(IT)+THETA(2))*3./T(IT)+THETA(1))*2./T(IT)/Q-DQ
     1**2
      CALL QSUM (TEST(14))
C
C  VIBRATIONAL-ROTATION INTERACTION USING ALPHA CONSTANTS--FORMULAS 8-10
C
      LABEL=LALPHA
      IF ( NNU.LE.9 ) THEN
      DO 180 I=1,NNU
      QL(1)=AI(I)*DN(I)
      CALL DERIV (I,0,0,0,0,I,0,0,0,0,0)
      IF (TESTW(4)) GO TO 180
      QL(1)=.5*DN(I)*AI(I)**2
      CALL DERIV (I,0,0,0,0,I,I,0,0,0,0)
      QL(1)=DN(I)/6.*AI(I)**3
      CALL DERIV (I,0,0,0,0,I,I,I,0,0,0)
      QL(1)=DN(I)/6.*AI(I)**3
      CALL DERIV (I,I,0,0,0,I,I,I,0,0,0)
      IF (TESTW(1)) GO TO 180
      QL(1)=AIJ(I,I)*DN(I)
      CALL DERIV (I,0,0,0,0,I,I,0,0,0,0)
      QL(1)=AIJ(I,I)*DN(I)*AI(I)
      CALL DERIV (I,0,0,0,0,I,I,I,0,0,0)
      QL(1)=AIJ(I,I)*DN(I)*AI(I)
      CALL DERIV (I,I,0,0,0,I,I,I,0,0,0)
      DO 170 J=1,NNU
      QL(1)=AIJ(I,J)*DN(I)*DN(J)
      IF (I.GT.J) GO TO 160
      CALL DERIV (I,J,0,0,0,I,J,0,0,0,0)
160   QL(1)=AI(I)*AIJ(I,J)*DN(I)*DN(J)
      IF (I.EQ.J) QL(1)=QL(1)*2.
170   CALL DERIV (I,J,0,0,0,I,I,J,0,0,0)
      IF (NOATMS.GT.2) GO TO 180
C
C  FORMULA 11.
C
      QL(1)=AIII
      CALL DERIV (I,0,0,0,0,I,I,I,0,0,0)
      QL(1)=4.*AIII
      CALL DERIV (I,I,0,0,0,I,I,I,0,0,0)
      QL(1)=AIII
      CALL DERIV (I,I,I,0,0,I,I,I,0,0,0)
180   CONTINUE
C
      IF (TEST(14)) WRITE (6,190)
190   FORMAT (/,' FIRST ORDER CORRECTIONS' )
      CALL QSUM (TEST(14))
      CORT=1.0
C
C  FIRST ORDER XIJ--FORMULA 12.
C
      LABEL=LXIJ
      DO 200 I=1,NNU
      DO 200 J=I,NNU
      CON=DN(I)*DN(J)
      IF (I.EQ.J) CON=CON+DN(I)
      QL(1)=CON*(-CT)*X(I,J)
200   CALL DERIV (I,J,0,0,0,I,J,0,0,0,0)
      CALL QSUM (TEST(14))
C
C  END CALCULATIONS FOR PANDK AND JANAF.
C
C  FIRST ORDER YIJK--FORMULA13.
C
      IF (TESTW(4)) GO TO 340
      LABEL=LYIJK
      DO 210 I=1,NNU
      DO 210 J=I,NNU
      DO 210 K=J,NNU
      CON=ND(I)*(ND(J)+KD(I,J))*(ND(K)+KD(I,K)+KD(J,K))
      QL(1)=CON*(-CT)*Y(I,J,K)
210   CALL DERIV (I,J,K,0,0,I,J,K,0,0,0)
      CALL QSUM (TEST(14))
C
C  FIRST ORDER ALPHA-XIJ INTERACTION--FORMULA 17.
C
      LABEL=LAX
      DO 220 I=1,NNU
      AL=AI(I)*(-CT)
      DO 220 J=1,NNU
      IF (I.EQ.J) QL(1)=AL*X(I,I)*2.*DN(I)*(DN(I)+1.)
      IF (I.NE.J) QL(1)=AL*X(I,J)*DN(I)*DN(J)
220   CALL DERIV (I,J,0,0,0,I,I,J,0,0,0)
      CALL QSUM (TEST(14))
C
      DO 230 I=1,NNU
      IF (G(I).EQ.0.) GO TO 230
C
C  GII CORRECTION-- FORMULA 16.
C
      LABEL=LG
      QL(1)=G(I)*(-CT)*2.d0
      CALL DERIV (I,0,0,0,0,I,I,0,0,0,0)
      QL(1)=4.*G(I)*AI(I)*CT
      CALL DERIV (I,I,0,0,0,I,I,I,0,0,0)
230   CONTINUE
      IF (LABEL.EQ.LG) CALL QSUM (TEST(14))
      IF (NOATMS.GT.2) GO TO 240
C
C  WEZE FOR DIATOMIC MOLECULES--FORMULA 15.
C
      LABEL=LZ
      QL(1)=24.*WX(3)*(-CT)
      CALL DERIV (1,1,1,1,0,1,1,1,1,0,0)
      CALL QSUM (TEST(14))
240   CTT=CT**2/2.
      ENDIF
      IF (WF.EQ.0.0 .AND. DARD.EQ.0.) GO TO 250
C
C  FERMI AND DARLING DENNISON RESONANCE--FORMULA 7.
C    5/3/89 - FERMI CONTRIBUTION CHANGED TO
C    EQUIVALENT FORMULATION.  RW AND SW CHANGED TO RI(9,I) & S(9,I).
C    ADDED DARLING DENNISON - GURVICH (1978) FORMULA, PAGE 61.
C
      CORT=2.
      IF (WF.NE.0.) THEN
        LABEL=LFERMI
        U=CT*2.*V(2)
        CON=WF**2
      ELSE
        LABEL=LDARD
        U=CT*(V(1)+V(2))*.5D0
        CON=2.D0*DARD**2
      ENDIF
      RI(9,1)=dEXP(-U)
      S(9,1)=1./(1.-RI(9,1))
      RI(9,2)=U
      RI(9,3)=-U
      S(9,2)=RI(9,1)*S(9,1)*U
      S(9,3)=S(9,2)*(S(9,2)+U-1.)
      CON=CON*CTT
      QL(1)=CON
      IF (WF.NE.0.) THEN
        CALL DERIV (9,0,0,0,0,2,2,9,0,0,0)
      ELSE
        CALL DERIV (1,2,0,0,0,9,9,9,9,0,0)
      ENDIF
      CALL QSUM (TEST(14))
C
C  END CALCULATIONS FOR NRRAO1.
C
250   IF (.NOT.TESTW(3)) GO TO 340
      IF (TEST(14)) WRITE (6,260)
260   FORMAT (/,25H SECOND ORDER CORRECTIONS)
C
C  XIJ - XIJ INTERACTION--FORMULAS 18 AND 19.
C
      IF ( NNU.LE.9 ) THEN 
        LABEL=LX2
        CORT=2.0
        DO 280 I=1,NNU
        DO 270 J=I,NNU
        CON=DN(I)*DN(J)
        IF (I.EQ.J) CON=2.*DN(I)*(DN(I)+1.)
        QL(1)=CON*X(I,J)**2*CTT
270     CALL DERIV (I,J,0,0,0,I,I,J,J,0,0)
        DO 280 J=1,NNU
        DO 280 K=J,NNU
        CON=(2-KD(J,K))*(1+KD(I,J))*(1+KD(I,K))*ND(I)*(ND(J)+KD(I,J))*
     &  (ND(K)+KD(I,K))
        QL(1)=CON*X(I,J)*X(I,K)*CTT
280     CALL DERIV (I,J,K,0,0,I,I,J,K,0,0)
        CALL QSUM (TEST(14))
C
C  XIJ - YIJK INTERACTION--FORMULAS 20 AND 21.
C
        LABEL=LXY
        DO 290 I=1,NNU 
        DO 290 J=I,NNU
        DO 290 K=1,NNU
        CON=2*(1+KD(I,J))*(1+KD(I,K)+KD(J,K))*(ND(I)+KD(I,J))*ND(J)*
     &    (ND(K)+KD(J,K)+KD(I,K))
        QL(1)=CON*CTT*X(I,J)*Y(I,J,K)
290     CALL DERIV (I,J,K,0,0,I,I,J,J,K,0)
        DO 300 I=1,NNU
        DO 300 J=1,NNU
        DO 300 K=1,NNU
        DO 300 L=K,NNU
        CON=(1+KD(I,J))*(1+KD(I,K)+KD(I,L))*ND(I)*(ND(J)+KD(I,J))*
     &      (ND(K)+KD(I,K))*(ND(L)+KD(I,L)+KD(K,L))*2
        QL(1)=CON*CTT*X(I,J)*Y(I,K,L)
300     CALL DERIV (I,J,K,L,0,I,I,J,K,L,0)
        CALL QSUM (TEST(14))
        DO 320 I=1,NNU
        IF (G(I).EQ.0.) GO TO 320
C
C  GII - GII AND GII - XIJ INTERACTIONS--FORMULAS 22 AND 23.
C
        LABEL=LG2
        QL(1)=2.*G(I)**2*CTT
        CALL DERIV (I,0,0,0,0,I,I,I,I,0,0)
        DO 310 J=1,NNU
        CON=4.*G(I)*X(I,J)*CTT
        IF (I.EQ.J) CON=16.*G(I)*(G(I)+2.*X(I,I))*CTT
        QL(1)=CON
        CALL DERIV (I,J,0,0,0,I,I,I,J,0,0)
        QL(1)=CON
        IF (I.EQ.J) QL(1)=CTT*2.*(G(I)+12.*X(I,I))*G(I)
310     CALL DERIV (I,I,J,0,0,I,I,I,J,0,0)
320     CONTINUE
        IF (LABEL.EQ.LG2) CALL QSUM (TEST(14))
C
C  ALPHA - XIJ - XIJ INTERACTION--FORMULAS 24 THRU 27.
C
        LABEL=LAX2
        DO 330 I=1,NNU
        AL=AI(I)*CTT
        QL(1)=4.*AL*(X(I,I)*DN(I)*(DN(I)+1.))**2
        CALL DERIV (I,I,I,I,I,I,I,I,I,I,0)
        DO 330 J=1,NNU
        IF (I.EQ.J) CON=4.*DN(I)*(DN(I)+1.)
        IF (I.NE.J) CON=DN(I)*DN(J)
        QL(1)=CON*X(I,J)**2*AL
        CALL DERIV (I,J,0,0,0,I,I,I,J,J,0)
        DO 330 K=1,NNU
        CON=(1+KD(I,J))*(1+KD(I,K))*ND(I)*(ND(J)+KD(I,J))*(ND(K)+
     &     KD(I,K))
        QL(1)=CON*AL*X(I,J)*X(I,K)
        CALL DERIV (I,J,K,0,0,I,I,I,J,K,0)
        QL(1)=CON*AL*X(I,J)*X(I,K)
        CALL DERIV (I,J,K,I,0,I,I,I,J,K,0)
        CON=(1+KD(I,J))*(1+KD(J,K))*(2-KD(I,K))*ND(I)*(ND(J)+KD(I,J))*
     &  ((1+KD(I,K))*ND(K)+KD(I,K)+KD(J,K)+KD(I,J)*KD(J,K))
        QL(1)=CON*AL*X(I,J)*X(J,K)
        CALL DERIV (I,J,K,0,0,I,I,J,J,K,0)
330     CONTINUE
        CALL QSUM (TEST(14))
      ENDIF
C
C  INTERNAL ROTATION
C
340   IF (.NOT.IROT) GO TO 370
      LABEL=LIR
      K=0
      L=1
350   CALL INTROT (T(IT),HCK,DQS,DDQS,QLNS,L,LT)
      DO 360 J=1,LT
      K=K+1
      DQ=DQS
      DDQ=DDQS
      QLN=QLNS
      CALL QSUM (TEST(14))
360   CONTINUE
      L=L+1
      IF (K.LT.NIR) GO TO 350
370   IF (TESTW(6)) GO TO 380
C
C   CALCULATIONS FOR SPECIES WITH ONE ELECTRONIC STATE
C
      GHRT(IT)=QLNTOT
      HHRT(IT)=DQTOT
      CPR(IT)=DDQTOT+2.d0*DQTOT
      GO TO 430
C
C   CALCULATIONS FOR SPECIES WITH EXCITED ELECTRONIC STATES
C
380   SQ(1,IT)=QLNTOT-EQLN
      SQ(2,IT)=DQTOT-EDQ
      SQ(3,IT)=DDQTOT-EDDQ
390   IF (QLNTOT.LE.700.d0) GO TO 420
      WRITE (6,400)
400   FORMAT (/,' Q TOO LARGE TO INCLUDE EXCITED STATES, (LINK1)')
410   IF (CODE(1).NE.SUB) RETURN
      CALL INPUT
      GO TO 410
420   QTOT=0.
      IF (QLNTOT.GT.-20.) QTOT=dEXP(QLNTOT)
      GHRT(IT)=QTOT+GHRT(IT)
      HHRT(IT)=DQTOT*QTOT+HHRT(IT)
      CPR(IT)=(DDQTOT+DQTOT**2)*QTOT+CPR(IT)
430   CONTINUE
      RETURN
      END
      SUBROUTINE LOGK
      SAVE
      PARAMETER (IOEFT=13)
C
C  CALCULATE ROUNDED TABLES WITH DELTAH AND LOGK CALCULATIONS.
C
C   TEST(4) - SPECIES IS A GAS.
C   TEST(8) - AN ASSIGNED H IS AVAILABLE.
C   TEST(13) - DATA ARE IN THE FORM, H-H298 AND -(G-H298)
C   TEST(18) - ENTHALPY IS ABSOLUTE
C
      DIMENSION ENT(2,20),PT(20),NQ(5),MARK(20),TK(3),VFM2(15),itn(20)
C
      DOUBLE PRECISION TP(3,502),HZERO,AMP,TNO,H00RT,RT,RUSE(2)
      DOUBLE PRECISION ALK(502),DH0,FRT,CP,S,H,HH,GH,DH(502),H0
      DOUBLE PRECISION HH0,DMLA,DNMLA,H298
C
      CHARACTER*4 CHH,CHDH,BZ,VFM2,FSTAR,F2B,ZERO,FI6,ENT
      CHARACTER*4 FA4,F3X,F8X,F9,F12,FP2,FP3,FP4
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      EQUIVALENCE (IT,TI), (TAPE,TP)
C
      DATA  FSTAR/'2H *'/,F9/',F9'/,FP2/'.2'/, FI6/',I6'/,F3X/',3X'/
     1 ,FA4/',A4'/,FP4/'.4'/,FP3/'.3'/, ZERO/'0'/
     2 , F12/',F12'/, F8X/',8X'/, F2B/'2H  '/
C
      DATA VFM2/'(', '2H  ', ',F9', '.2', ',F9', '.3,3', 'F12', '.4',
     1   ',F12', '.4', ',F12', '.4',  ',F12', '.4', ')'/
C
      REWIND IOEFT
      CHH=FSTAR
      CHDH=FSTAR
      RUSE(1)=R
      IF (R.LT.8.D0)RUSE(1)=R*4.184D0
      RUSE(2)=RUSE(1)/4.184D0
      H298=H298HR
      IF (TEST(13))H298=0.
      IF (TEST(18))H298=ASINDH
      IK=0
      DO 10 I=1,NT
      DH(I)=0.D0
10    ALK(I)=0.D0
      INIT=1
      NTX=0
      DH0=0.D0
C
C  NQ = 0 - EFDATA FOR ELEMENT NOT AVAILABLE
C  NQ = 1 - EFDATA FOR ELEMENT IS AVAILABLE
C  NQ = 2 - MOLECULE IS AN ELEMENT ITSELF
C  CHDH = BLANK OR ZERO - USE CHARACTER FOR DELTA H AND LOGK COLUMNS
C  CHH = BLANK - USE BLANK FOR H COLUMN
C
C  SEARCH IOEFT FOR EFDATA FOR THIS MOLECULE.
C
      DO 20 I=1,NKIND
      J=JF(I)
20    NQ(I)=0
      IF (NKIND.GT.1) GO TO 30
      J=JF(1)
      IF (MLA(1).EQ.NMLA(J).AND.FAZ.EQ.EFAZ(J)) NQ(1)=2
30    READ (IOEFT,END=210) CODE,NA,SUB,DATE,HZERO,AMP,TNO,((TP(J,K),
     * J=1,3),K=1,502)
      IF (CODE(1).EQ.BLANK) GO TO 210
      DO 40 II=1,NKIND
      J=JF(II)
      IF (CODE(1).NE.FORMLA(1,II)) GO TO 40
      IF (CODE(2).NE.FORMLA(2,II)) GO TO 40
      GO TO 50
40    CONTINUE
      GO TO 30
50    LL=1
      IF (SUB.NE.EFAZ(J)) GO TO 30
      IF (NA.NE.NMLA(J)) GO TO 30
      IF (NQ(II).EQ.2) GO TO 190
      NQ(II)=1
      MX=TNO+.1
      DMLA=MLA(II)
      DNMLA=NMLA(J)
      DH0=DH0-HZERO*DMLA*RUSE(1)/DNMLA
C
C  FIND TRANSITION POINTS FOR REACTANT
C
      k1=1
      ntn=1
      DO 80 K=1,MX-1
      IF (TP(1,K).NE.TP(1,K+1)) GO TO 80
      itn(ntn)=k
      ntn=ntn+1
80    CONTINUE
      itn(ntn)=MX
      M=itn(1)
      DO 180 I=1,NT
C
C   FIND T IN EFDATA
C
      if (t(i).ge.tp(1,k1).and.t(i).le.tp(1,m))go to 130
      if (tp(1,1).gt.t(i)) go to 110
      if (t(i).gt.tp(1,mx)) go to 130
      do 100 j=2,ntn
      jx=j-1
      jj=itn(j)
      if(t(i).le.tp(1,jj)) go to 104
100   continue
104   k1=itn(jx)+1
      M=jj
      go to 120
110   INIT=INIT+1
      GO TO 180
C
C  MP OF REACTANT, PUT * IN MARK FOR FOOTNOTE
C
120   IF (IK.LT.20) IK=IK+1
      MARK(IK)=I
      PT(IK)=tp(1,k1)
      ENT(1,IK)=CODE(1)
      ENT(2,IK)=CODE(2)
130   DO 160 K=K1,M
      TK(2)=TP(2,K)
      TK(3)=TP(3,K)
      IF (DABS(TP(1,K)-T(I)).LT.0.01) GO TO 170
      IF (TP(1,K).LT.T(I)) GO TO 160
      IF ((M-K1).EQ.0) GO TO 160
C
C  INTERPOLATION OF EF DATA
C
      N=4
      IF ((M-K1).LT.3) N=M-K1+1
      K2=K-2
      IF (K.EQ.K1) K2=K
      IF (K.EQ.(K1+1)) K2=K-1
      NK=K2+N-1
      DO 150 L=2,3
      TK(L)=0.0
      DO 150 JJ=K2,NK
      TK(1)=1.0
      DO 140 JM=1,N
      IM=K2+JM-1
      IF (TP(1,JJ).EQ.TP(1,IM)) GO TO 140
      TK(1)=TK(1)*(T(I)-TP(1,IM))/(TP(1,JJ)-TP(1,IM))
140   CONTINUE
150   TK(L)=TK(L)+TK(1)*TP(L,JJ)
      GO TO 170
160   CONTINUE
      IF ((NTX.EQ.0).OR.((I-1).LT.NTX)) NTX=I-1
      GO TO 190
C
C  CALCULATE DELTA H AND DELTA F
C
170   DH(I)=DH(I)-TK(2)*DMLA/DNMLA
      ALK(I)=ALK(I)-TK(3)*DMLA/DNMLA
180   CONTINUE
190   DO 200 I=1,NKIND
      IF (NQ(I).EQ.0) GO TO 30
200   CONTINUE
210   DH0=DH0/1000.
      IF (TEST(18).AND.ASINDT.NE.0.)GO TO 215
      IF (TEST(8))DH0= DH0 + (RUSE(1)*ASINDH)/1000.D0
215   DO 220 I=1,NKIND
      IF (NQ(I).EQ.1) GO TO 220
      IF (NQ(I).EQ.0) CHDH=BLANK
      IF (NQ(I).EQ.2) CHDH=ZERO
220   CONTINUE
C
C  PRINT TABLES
C
      DO 510 NTABLE=1,2
      IF (.NOT.TOUT(NTABLE+1)) GO TO 510
      WRITE (6,230) (SNM(I),I=1,4),((NAME(I,J),I=1,4),J=1,nna)
230   FORMAT (1H ,4A4,6(3X,4A4)///)
      IF (TEST(18).AND.ASINDT.EQ.0.)GO TO 233
      IF (TEST(13).OR.H298.NE.0.)GO TO 238
233   WRITE (6,235)
235   FORMAT (/'     T         CP         H-H0         S       -(G-H0)/
     1T      H        DELTA H      LOG K')
      GO TO 245
238   WRITE (6,240)
240   FORMAT (/'     T         CP        H-H298        S      -(G-H298)/
     1T      H        DELTA H      LOG K')
245   IF (NTABLE.EQ.1)WRITE(6,250)
      IF (NTABLE.EQ.2)WRITE(6,255)
250   FORMAT ('    DEG-K    J/MOL-K      KJ/MOL     J/MOL-K     J/MOL-K
     *     KJ/MOL      KJ/MOL'/)
255   FORMAT ('    DEG-K   CAL/MOL-K    KCAL/MOL   CAL/MOL-K   CAL/MOL-K
     *    KCAL/MOL    KCAL/MOL'/)
      IF (.NOT.TEST(8)) GO TO 290
      IF ((TEST(13).OR.TEST(18)).AND.ASINDT.NE.0.)GO TO 290
      H0=(ASINDH*RUSE(NTABLE))/1000.
      HH0=-H298HR*RUSE(NTABLE)/1000.
      IF (CHDH.EQ.BLANK .OR. CHDH.EQ.ZERO) GO TO 280
      WRITE (6,275) HH0,H0,DH0
275   FORMAT (6X,'0',6X,'-------',F12.4,5X,'-------',5X,'-------',2F12.4
     * ,5X,'-------')
      GO TO 290
280   WRITE (6,285) HH0,H0,CHDH
285   FORMAT (6X,'0',6X,'-------',F12.4,5X,'-------',5X,'-------',F12.4,
     * 8X,A4,5X,'-------')
290   LINES=10
      H=0.
      IF (TEST(8).OR.TEST(18)) GO TO 298
      CHH=BLANK
      CHDH=BLANK
C
298   DO 420 I=1,NT
      IT=T(I)
      VFM2(3)=FI6
      VFM2(4)=F3X
      IF (DMOD(T(I),1.D0).EQ.0.) GO TO 300
      TI=T(I)
      VFM2(3)=F9
      VFM2(4)=FP2
300   RT=RUSE(NTABLE)*T(I)
      H00RT=ASINDH/T(I)
      IF (.NOT.TEST(18)) H=H00RT
      H=(H+HHRT(I))*RT/1000.D0
      FRT=GHRT(I)-H00RT
      CP=CPR(I)*RUSE(NTABLE)
      S=SR(I)*RUSE(NTABLE)
      HH=(HHRT(I)-H298/T(I))*RT/1000.D0
      GH=(GHRT(I)+H298/T(I))*RUSE(NTABLE)
C
C
      VFM2(2)=F2B
      IF (IK.EQ.0) GO TO 320
      DO 310 IX=1,IK
      IF (MARK(IX).EQ.I) VFM2(2)=FSTAR
310   CONTINUE
320   BZ=ZERO
      IF (CHDH.EQ.ZERO)GO TO 350
      BZ=BLANK
      IF (CHDH.EQ.BLANK) GO TO 350
      IF ((I.GT.NTX.AND.NTX.NE.0).OR.I.LT.INIT) GO TO 350
      IF ( ntable.EQ.1.OR..NOT.Tout(2) ) THEN
        DH(I)=(DH(I)+HHRT(I))*RT/1000.D0+DH0
        ALK(I)=(GHRT(I)+ALK(I)-DH0*1000.D0/RT)/2.3025851D0
      ELSE
        DH(I)=DH(I)/4.184D0
      ENDIF
      VFM2(11)=F12
      VFM2(12)=FP4
      VFM2(13)=F12
      VFM2(14)=FP4
      IF(VFM2(3).EQ.F9)WRITE (6,VFM2)TI,CP,HH,S,GH,H,DH(I),ALK(I)
      IF(VFM2(3).EQ.FI6)WRITE (6,VFM2)IT,CP,HH,S,GH,H,DH(I),ALK(I)
      GO TO 400
350   VFM2(11)=F8X
      VFM2(12)=FA4
      VFM2(13)=F8X
      VFM2(14)=FA4
      IF (CHH.EQ.BLANK) GO TO 360
      IF(VFM2(3).EQ.F9)WRITE (6,VFM2)TI,CP,HH,S,GH,H,BZ,BZ
      IF(VFM2(3).EQ.FI6)WRITE (6,VFM2)IT,CP,HH,S,GH,H,BZ,BZ
      GO TO 400
360   IF(VFM2(3).EQ.F9)WRITE (6,VFM2)TI,CP,HH,S,GH
      IF(VFM2(3).EQ.FI6)WRITE (6,VFM2)IT,CP,HH,S,GH
C
400   LINES=LINES+1
      IF (DMOD(T(I),500.D0).NE.0.0) GO TO 420
      IF (T(I).GT.10000.D0.AND.DMOD(T(I),2500.D0).NE.0.) GO TO 420
      WRITE (6,410)
410   FORMAT (1H )
420   continue     
      IF (IK.EQ.0) GO TO 460
C
C     WRITE FOOTNOTE
C
      WRITE (6,440)
440   FORMAT (/,' *A CHANGE IN PHASE OF AN ASSIGNED REFERENCE ELEMENT'
     1,' HAS OCCURRED BETWEEN THIS TEMPERATURE AND THE PRECEDING ONE,')
      WRITE (6,450) (ENT(1,I),ENT(2,I),PT(I),I=1,IK)
450   FORMAT ('  ',2A1,'-- ',F8.3///)
460   CALL PAGEID
510   DH0=DH0/4.184D0
      REWIND IOEFT
      RETURN
      END
      SUBROUTINE PAGEID
      SAVE
C
C  PRINTS CHEMICAL FORMULA AT BOTTOM OF PAGE.
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      WRITE (6,20) (SNM(I),I=1,4),STD,((NAME(I,J),I=1,4),J=1,NNA)
20    FORMAT (/,15X,4A4,2X,A3,2X,6(2X,4A4)///)
      RETURN
      END
      SUBROUTINE POLY
      SAVE
C
C  CALCULATE PROPERTIES FOR DIATOMIC AND POLYATOMIC MOLECULES.
C
C               IF TEST IS TRUE--
C     TESTW(1)  MOLECULE IS NON-LINEAR
C     TESTW(2)  RIGID ROTATOR-HARMONIC OSCILLATOR APPROXIMATION
C     TESTW(3)  SECOND ORDER CORRECTIONS ARE CALLED FOR
C     TESTW(4)  PENNINGTON AND KOBE APPROXIMATION
C     TESTW(5)  JANAF METHOD FOR DIATOMIC MOLECULES
C     TESTW(6)  SPECIES HAS EXCITED ELECTRONIC STATES
C
      CHARACTER*4 AJANAF,ALF,ALPHA,ANRRA,A1,A2,BE,BET,B1,B2,C1,C2,DE,DJ
      CHARACTER*4 DJK,DK,D1,D2,D222,ENA,ENB,ENC,EIAIB,G2,O1,O2,PANDK
      CHARACTER*4 RHO,RRHO,STATWT,SYMNO,TAAA,TAAB,TAAC,TABA,TACA
      CHARACTER*4 TBBB,TBBC,TBCB,TCCC,TROT,T1,T2,VV,WE,WEXE,WEYE,WEZE
      CHARACTER*4 WXX,W1,W2,XX,YY,DRD
C
      LOGICAL IROT, CALRHO, calcth
      DOUBLE PRECISION DJKN,DKN,TAU(9),BEJ,ASQ,BSQ,CSQ,CY,BCONV,
     & IAIBIC,DJN,TAB,TBC,TAC,Z,E1,F1,G1,H1
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      real*8 v,x,y,alfa,alfb,alfc,g,wx,beta,a,b,c,dard,dd,wf,w,sym,stwt,
     1 t00,theta,rh,ri,s,ql,q,qln,dq,ddq,qtot,qlntot,dqtot,ddqtot,aij,
     2 aiii,ai
      logical testw
      COMMON /WCOMMN/ V(300),X(9,9),Y(9,9,9),ALFA(9),ALFB(9),ALFC(9),
     1 G(9),WX(9),BETA(4),A,B,C,DARD,DD,WF,W,SYM,STWT,T00,THETA(3),
     2 RH(6),RI(300,3),S(300,3),QL(3),Q,QLN,DQ,DDQ,QTOT,QLNTOT,DQTOT,
     3 DDQTOT,AIJ(9,9),AIII,AI(9),DN(300),ND(300),CORT,NNU,TESTW(6)
C
      DATA RRHO/'RRHO'/, AJANAF/'JANA'/, ANRRA/'NRRA'/, O2/'O2'/
      DATA PANDK/'PAND'/, BCONV/2.7992774D0/, O1/'O1'/
C
      DATA             SYMNO /'SYMN'/, STATWT/'STAT'/,  A1   /'A   '/
     1,  B1  /'B'   /,   C1  /'C'   /,   ENA /'IA'  /,  ENB  /'IB'  /
     2,  RHO /'RHO '/, ALPHA /'ALPH'/,   WE  /'WE  '/, WEXE  /'WEXE'/
     3, WEYE /'WEYE'/,  WEZE /'WEZE'/,   DE  /'DE  '/,  ALF  /'ALFA'/
     4,  W1  /'W   '/,   T2  /'TO  '/,   A2  /'AO  '/,  B2   /'BO  '/
     5,  C2  /'CO  '/,   D1  /'D   '/,   DRD /'DARD'/,  D2   /'DO  '/
     6, D222 /'DOOO'/,   WXX /'WX  '/,   BET /'BETA'/,  W2   /'WO  '/
     7,  BE  /'BE  '/,   ENC /'IC  '/,   T1  /'T   '/,  VV   /'V   '/
     8,  XX  /'X   '/,   YY  /'Y   '/,   G2  /'G   '/,  EIAIB/'IAIB'/
      DATA TROT/'INTR'/ ,DJ/'DJ'/,DJK/'DJK'/,DK/'DK'/,TAAA/'TAAA'/,
     1 TBBB/'TBBB'/,TCCC/'TCCC'/,TAAC/'TAAC'/,TAAB/'TAAB'/,
     2 TBBC/'TBBC'/,TABA/'TABA'/,TACA/'TACA'/,TBCB/'TBCB'/
C
C  INITIALIZE FOR EACH SET OF METHOD AND DATA RECORDS.
C
      DO 10 I=NIT,NT
      CPR(I)=0.0
      HHRT(I)=0.0
10    GHRT(I)=0.0
      HHRT(NT+1)=0.0
      GHRT(NT+1)=0.0
      SYM=1.0
      STWT=1.0
      DO 15 I=2,6
15    TESTW(I)=.FALSE.
C
C  CHECK METHOD
C
      DO 25 I=1,4
      IF (ABEL(1,I).EQ.RRHO) GO TO 35
      IF (ABEL(1,I).EQ.ANRRA) GO TO 20
      IF (ABEL(1,I).EQ.PANDK) GO TO 45
      IF (ABEL(1,I).EQ.AJANAF) GO TO 50
      GO TO 25
20    IF (ABEL(2,I).EQ.O1) GO TO 55
      IF (ABEL(2,I).EQ.O2) GO TO 40
25    CONTINUE
      WRITE (6,30)
30    FORMAT (/,' METHOD CODE WAS NOT RECOGNIZED, USED NRRAO1, (POLY)')
      GO TO 55
35    TESTW(2)=.TRUE.
      GO TO 55
40    TESTW(3)=.TRUE.
      GO TO 55
45    TESTW(4)=.TRUE.
      GO TO 55
50    TESTW(5)=.TRUE.
      IF (NOATMS.GT.2) TESTW(2)=.TRUE.
55    WRITE (6,60) WEIGHT
60    FORMAT (/,' MOLECULAR WT.=',F12.7)
      NFIRST=0
      LINES=LINES+4
C
C  CALL INPUT TO READ AND PRINT CONTENTS OF INPUT RECORD.
C
65    CALL INPUT
      IF (NFIRST.NE.0) GO TO 95
C
C  INITIALIZE FOR FIRST RECORD ONLY.
      NFIRST=1
      SUB=CODE(1)
C
C  INITIALIZE FOR EACH ELECTRONIC LEVEL.
C
70    IROT=.FALSE.
      T00=0.0
      A=0.0
      B=0.0
      C=0.0
      DJN=0
      DJKN=0
      DKN=0
      DO 75 I=1,9
75    TAU(I)=0
      CALRHO=.FALSE.
      calcth = .true.
      DD=0.0
      WF=0.0
      DARD=0.0
      W=0.0
      THETA(3)=0.0
      AIII=0.0
      DO 80 I=1,9
      ALFA(I)=0.0
      ALFB(I)=0.0
      ALFC(I)=0.0
      G(I)=0.0
      WX(I)=0.0
      if (i.le.6) RH(I)=0.0
      DO 80 J=1,9
      X(I,J)=0.0
      AIJ(I,J)=0.0
80    CONTINUE
      DO 85 I=1,300
      V(I)=0.0
      ND(I)=1
85    DN(I)=1.0d0
      DO 87 I=1,4
      BETA(I)=0.0
87    CONTINUE
      DO 90 I=1,9
      DO 90 J=1,9
      DO 90 K=1,9
90    Y(I,J,K)=0.0
      LELC=IC80
C
C  ASSUME LINEAR MOLECULE WITH 3N-5 FREQS.  IF THERE IS AN A OR IA
C      IN THE INPUT, CHANGE TO 3N-6.
C
      NV=3*NOATMS-5
      TESTW(1)=.FALSE.
      GO TO 100
C
C  IF COLS 1-4 OR 79-80 ARE DIFFERENT FROM PREVIOUS RECORD, GO TO 510.
C
95    IF (CODE(1).NE.SUB.OR.LELC.NE.IC80) GO TO 325
C
C  IN DO LOOP THRU 500 CHECK EACH LABEL ON DATA RECORD AND STORE DATA.
C
100   DO 320 ID=1,4
      IF (ABEL(1,ID).EQ.BLANK) GO TO 320
      I=FWORD(ID)
      IF (I.LT.0) I=0
      IF (ABEL(1,ID).EQ.T1.OR.ABEL(1,ID).EQ.T2) GO TO 115
      IF (ABEL(1,ID).EQ.STATWT) GO TO 125
      IF (ABEL(1,ID).EQ.SYMNO) GO TO 130
      IF (ABEL(1,ID).EQ.B1.OR.ABEL(1,ID).EQ.B2.OR.ABEL(1,ID).EQ.BE)
     1  GO TO 135
      IF (ABEL(1,ID).EQ.ENB) GO TO 140
      IF (ABEL(1,ID).EQ.ALPHA.OR.ABEL(1,ID).EQ.ALF) GO TO 260
      IF (ABEL(1,ID).EQ.D1) GO TO 150
      IF (ABEL(1,ID).EQ.D2.OR.ABEL(1,ID).EQ.D222) GO TO 150
C
      IF (NOATMS.EQ.2) GO TO 280
      IF (ABEL(1,ID).EQ.RHO) GO TO 145
      IF (ABEL(1,ID).EQ.ENA) GO TO 235
      IF (ABEL(1,ID).EQ.ENC) GO TO 220
      IF (ABEL(1,ID).EQ.EIAIB) GO TO 218
      IF (ABEL(1,ID).EQ.A1.OR.ABEL(1,ID).EQ.A2) GO TO 245
      IF (ABEL(1,ID).EQ.C1.OR.ABEL(1,ID).EQ.C2) GO TO 225
      IF (ABEL(1,ID).EQ.W1.OR.ABEL(1,ID).EQ.W2) GO TO 230
      IF (ABEL(1,ID).EQ.DRD) GO TO 232
      IF (ABEL(1,ID).EQ.VV) GO TO 265
      IF (ABEL(1,ID).EQ.XX) GO TO 270
      IF (ABEL(1,ID).EQ.YY) GO TO 275
      IF (ABEL(1,ID).EQ.G2) GO TO 255
      IF (ABEL(1,ID).EQ.TROT) GO TO 120
      IF (ABEL(1,ID).EQ.DJ) GO TO 155
      IF (ABEL(1,ID).EQ.DJK) GO TO 160
      IF (ABEL(1,ID).EQ.DK) GO TO 165
      IF (ABEL(1,ID).EQ.TAAA) GO TO 170
      IF (ABEL(1,ID).EQ.TBBB) GO TO 175
      IF (ABEL(1,ID).EQ.TCCC) GO TO 180
      IF (ABEL(1,ID).EQ.TAAC) GO TO 185
      IF (ABEL(1,ID).EQ.TAAB) GO TO 190
      IF (ABEL(1,ID).EQ.TBBC) GO TO 195
      IF (ABEL(1,ID).EQ.TABA) GO TO 200
      IF (ABEL(1,ID).EQ.TACA) GO TO 205
      IF (ABEL(1,ID).EQ.TBCB) GO TO 210
105   WRITE (6,110) ABEL(1,ID),WORD(ID)
110   FORMAT (/,A6,' IS AN INCORRECT LABEL FOR THE NUMBER--',E16.8,'.
     1 THUS THE VALUE WAS IGNORED (POLY)')
      GO TO 320
C
C
115   T00=WORD(ID)
      GO TO 320
120   IROT=.TRUE.
      NIR=WORD(ID)+.01
      CALL IROTOR (IROT,NIR)
      GO TO 95
125   STWT=WORD(ID)
      GO TO 320
130   SYM=WORD(ID)
      GO TO 320
135   B=WORD(ID)
      GO TO 320
140   B=BCONV/WORD(ID)
      GO TO 320
145   IF (I.GE.1)RH(I)=WORD(ID)
      IF (I.EQ.0)RH(1)=WORD(ID)
      GO TO 320
150   DD=WORD(ID)
      GO TO 320
155   DJN=WORD(ID)
      GO TO 215
160   DJKN=WORD(ID)
      GO TO 215
165   DKN=WORD(ID)
      GO TO 215
170   TAU(1)=WORD(ID)
      GO TO 215
175   TAU(2)=WORD(ID)
      GO TO 215
180   TAU(3)=WORD(ID)
      GO TO 215
185   TAU(4)=WORD(ID)
      GO TO 215
190   TAU(5)=WORD(ID)
      GO TO 215
195   TAU(6)=WORD(ID)
      GO TO 215
200   TAU(7)=WORD(ID)
      GO TO 215
205   TAU(8)=WORD(ID)
      GO TO 215
210   TAU(9)=WORD(ID)
215   CALRHO=.TRUE.
      GO TO 320
218   A=BCONV
      B=BCONV/WORD(ID)
      C=BCONV
      calcth = .false.
      GO TO 240
220   C=BCONV/WORD(ID)
      GO TO 320
225   C=WORD(ID)
      GO TO 320
230   WF=WORD(ID)
      GO TO 320
232   DARD=WORD(ID)
      GO TO 320
C
C  IF IA OR A LABEL, NON-LINEAR MOLECULE IS ASSUMED.
C
235   A=BCONV/WORD(ID)
240   TESTW(1)=.TRUE.
      NV=3*NOATMS-6
      GO TO 320
245   IF (I.EQ.0) GO TO 250
      IX=I/10
      JX=I-10*IX
      AIJ(IX,JX)=WORD(ID)
      GO TO 320
250   A=WORD(ID)
      GO TO 240
255   IF (I.GT.10) I=I/10
      G(I)=WORD(ID)
      GO TO 320
260   IF (I.EQ.0) I=1
      IF (ABEL(1,ID).NE.ALPHA.AND.ABEL(2,ID).NE.B1) GO TO 262
      ALFB(I)=WORD(ID)
      GO TO 320
262   IF (ABEL(2,ID).NE.A1) GO TO 264
      ALFA(I)=WORD(ID)
      GO TO 320
264   IF (ABEL(2,ID).NE.C1) GO TO 105
      ALFC(I)=WORD(ID)
      GO TO 320
C
C  STORE FREQUENCY AND DEGENERACY ACCORDING TO LABEL.
C
265   V(I)=WORD(ID)
      IF (D(ID).EQ.0.) D(ID)=1.
      DN(I)=D(ID)
      ND(I)=D(ID)
      GO TO 320
C
C  STORE XIJ ACCORDING TO LABEL
270   IX=I/10
      JX=I-10*IX
      X(IX,JX)=WORD(ID)
      GO TO 320
C
C  STORE YIJK ACCORDING TO LABEL.
275   IY=I/100
      I=I-100*IY
      JY=I/10
      KY=I-10*JY
      Y(IY,JY,KY)=WORD(ID)
      GO TO 320
C
C  SOME INPUT FOR DIATOMIC MOLECULES.
C
280   IF (ABEL(1,ID).EQ.WE) GO TO 285
      IF (ABEL(1,ID).EQ.WEXE) GO TO 290
      IF (ABEL(1,ID).EQ.WEYE) GO TO 295
      IF (ABEL(1,ID).EQ.WEZE) GO TO 300
      IF (ABEL(1,ID).EQ.WXX) GO TO 305
      IF (ABEL(1,ID).EQ.BET) GO TO 310
      IF (ABEL(1,ID).EQ.DE) GO TO 315
      GO TO 105
285   W=WORD(ID)
      GO TO 320
290   WX(1)=WORD(ID)
      GO TO 320
295   WX(2)=WORD(ID)
      GO TO 320
300   WX(3)=WORD(ID)
      GO TO 320
305   WX(I)=WORD(ID)
      GO TO 320
310   BETA(I)=WORD(ID)
      GO TO 320
315   DD=WORD(ID)
320   CONTINUE
C
C  DATA FOR RECORD HAS BEEN STORED.  READ THE NEXT RECORD.
C
      GO TO 65
C
C
C  DATA FOR ELECTRONIC LEVEL HAS BEEN STORED--CALCULATE SOME VARIABLES
C     REQUIRED IN EQUATIONS.
C
325   NNU=0
      IRT=0
      IF (CODE(1).EQ.SUB) TESTW(6)=.TRUE.
      I=0
      IF (NOATMS.NE.2) GO TO 335
C
C  DIATOMIC MOLECULES--
C
      IF (TESTW(6).AND.W.EQ.0.) GO TO 510
      IF (W.LE.0.) GO TO 540
      V(1)=W-2.0*WX(1)+3.25*WX(2)+5.0*WX(3)+7.5625*WX(4)
      X(1,1)=-WX(1)+4.5*WX(2)+14.5*WX(3)
      Y(1,1,1)=WX(2)+8.*WX(3)
      AI(1)=ALFB(1)-ALFB(2)-.75*ALFB(3)
      AIJ(1,1)=-ALFB(2)-1.5*ALFB(3)
C
C  CALCULATE AND CHECK NUMBER OF FREQS. (NNU).  MAXIMUM 9 FOR NON-RRHO.
C
335   NNU=NNU+1
      IF (TESTW(6).AND.B.EQ.0.) GO TO 510
      IF (V(NNU).NE.0.0) GO TO 338
      IF (.NOT.IROT) GO TO 540
      IRT=IRT+1
      IF (IRT.GT.NIR) GO TO 540
338   I=I+ND(NNU)
      IF (I.LT.NV) GO TO 335
      IF (NNU.LT.300.AND.V(NNU+1).NE.0.) GO TO 540
C  Added next statement 10/3/97 to check for number of vibs (NV)
      if (I.GT.NV) goto 540
      IF (B.EQ.0.0) GO TO 550
      IF (NNU.GT.9) THEN
        IF ( .NOT.TESTW(2) ) WRITE (6,339)
339    FORMAT (/' ANHARMONICITIES NOT INCLUDED WHEN NUMBER OF NuS > 9')
      ENDIF
      IF (NOATMS.EQ.2) GO TO 340
      IF (TESTW(2)) GO TO 480
      IF (TESTW(1)) GO TO 390
      GO TO 350
C
C  DIATOMIC MOLECULES.
C
340   DD=((BETA(3)*0.5+BETA(2))*0.5-BETA(1))*0.5+DD
      IF (DD.EQ.0.0) DD=(4.0*B**3)/W**2
      BEJ=B
      B=((ALFB(3)*0.5+ALFB(2))*0.5-ALFB(1))*0.5+B
      IF (.NOT.TESTW(5)) GO TO 345
C
C  JANAF CORRECTIONS
C
      AI(1)=AI(1)/BEJ
      X(1,1)=X(1,1)*V(1)/W
      RH(1)=4.*dSQRT(DD/BEJ)/(HCK*V(1))
      GO TO 365
C
C  IF RRHO, SKIP TO 850.
C
345   IF (TESTW(2)) GO TO 510
C
C  DIATOMICS--NOT JANAF.
C
      IF (TESTW(4)) AI(1)=ALFB(1)-2.*ALFB(2)-3.25*ALFB(3)
      AI(1)=AI(1)/B
      AIJ(1,1)=AIJ(1,1)/B
      AIII=-ALFB(3)/B
      IF (TESTW(4)) AI(1)=(AI(1)+1.)*AI(1)
      GO TO 365
C
C  LINEAR POLYATOMIC MOLECULES--
C
350   IF (NNU.LE.9 ) THEN
        DO 360 I=1,NNU
        AI(I)=ALFB(I)/B
        DO 355 J=1,NNU
        CON=DN(J)/2.d0
        IF (I.EQ.J) CON=DN(I)
        AIJ(I,J)=-AIJ(I,J)/B
        IF (J.LT.I) AIJ(I,J)=AIJ(J,I)
355     AI(I)=AI(I)+CON*AIJ(I,J)
        IF (TESTW(4)) AI(I)=(AI(I)+1.)*AI(I)
360     CONTINUE
      ENDIF
C
C  LINEAR AND DIATOMIC MOLECULES--CALULATE RHO TERMS.
C    HIGHER ORDER TERMS ADDED OCT 1986 FROM R.S.MCDOWELL, JCP V39,
C    NO3, AUG 1,1963, P526.
C
365   IF (RH(1).NE.0.0) GO TO 368
      RH(1)=2.
      RH(2)=10.
      RH(3)=98.6666667
      RH(5)=-.66666667*DD/B
      RH(6)=DD/(B**2*HCK)
C
C  LINEAR AND DIATOMIC MOLECULES--CALCULTE THETAS.
C
368   THETA(1)=(HCK*B)/3.0
      THETA(2)=THETA(1)**2*0.6
      THETA(3)=(THETA(1)*THETA(2)*4.0)/7.0
      IF (.NOT.TEST(14)) GO TO 405
      WRITE (6,370) B,DD
370   FORMAT (/,' B0 =',F10.6,5X,'D0 =',E13.6)
      IF ( NNU.LE.9 ) THEN
        DO 380 I=1,NNU
        WRITE (6,375) I,AI(I)
375     FORMAT (/,' AI(',I1,') =',F10.7)
380     WRITE (6,385) (I,J,AIJ(I,J),J=1,NNU)
385     FORMAT (/,6(' A(',I1,',',I1,') =',F10.7,5X))
        GO TO 405
      ENDIF
C
C  NON-LINEAR MOLECULES--
C
390   IF (C.EQ.0.0) GO TO 560
      IF (A.NE.B.OR.B.NE.C) GO TO 393
      RH(1)=3.75
      RH(2)=22.5
      RH(3)=258.75
      RH(4)=4218.75
      RH(5)=-.75*DD/B
      RH(6)=DD/(B**2*HCK)
393   IF ( NNU.LE.9 ) THEN
        DO 400 I=1,NNU
        AI(I)=(ALFA(I)/A+ALFB(I)/B+ALFC(I)/C)/2.
        IF (TESTW(4)) AI(I)=(.5*AI(I)+1.)*AI(I)+((ALFA(I)/A)**2+(ALFB(I)
     &   /B)**2+(ALFC(I)/C)**2)/4.D0
        IF (TEST(14)) WRITE (6,395) AI(I),ALFA(I),ALFB(I),ALFC(I),I
395     FORMAT (/,' AI =',F10.7,4X,'ALPHA A =',F10.7,4X,'ALPHA B =',
     &   F10.7,4X,'ALPHA C =',F10.7,4X,'I =',I1)
400     CONTINUE
      ENDIF
      IF ( calcth ) then
      ASQ=A**2
      BSQ=B**2
      CSQ=C**2
      THETA(1)=(2.0*(A+B+C)-A*B/C-A*C/B-B*C/A)*(HCK/12.0)
      THETA(2)=(10.0*(ASQ+BSQ+CSQ)+12.0*(A*B+B*C+A*C)-12.0*(ASQ*B/C+A*BS
     1Q/C+BSQ*C/A+B*CSQ/A+ASQ*C/B+A*CSQ/B)+7.0*(ASQ*BSQ/CSQ+ASQ*CSQ/BSQ+
     2BSQ*CSQ/ASQ))*HCK**2/480.d0
      THETA(3)=0.0
      else
        theta(1) = 0.
        theta(2) = 0.
      endif
405   IF (TEST(14)) WRITE (6,410) (I,THETA(I),I=1,3)
410   FORMAT (/,3(' THETA(',I1,') =',F12.6,4X)/)
      IF (NOATMS.EQ.2) GO TO 480
      IF (.NOT.CALRHO) GO TO 420
C
C  CALCULATE ROTATIONAL STRETCHING TERMS WHEN DATA ARE AVAILABLE.
C     GLUSHKO,GURVICH,ET.AL.,VOL 1,PART 1,1978, PAGE 59.
C
      IF (DJN.EQ.0.) GO TO 415
      Z=B/A
      E1=DJN*(DJN+2.D0*(DJKN+DKN))+(DJKN+DKN)**2
      F1=DJN*(2.D0*(DJN+DKN)+3.D0*DJKN)+DJKN*(DJKN+DKN)
      G1=2.D0*DJN*(3.D0*(DJN+DJKN)+DKN)+DJKN**2
      H1=DJN*(2.D0*DJN+DJKN)
      RH(2)=((((35.D0*E1*Z+20.D0*F1)*Z+8.D0*G1)*Z+32.D0*H1)*Z+128.D0*
     1 DJN**2)*3.D0/(32.D0*(BSQ*HCK)**2)
      RH(1)=((3.D0*Z*(DJN+DJKN+DKN)+2.D0*DJKN+4.D0*DJN)*Z+8.D0*DJN)/
     1 (HCK*4.D0*BSQ)
      GO TO 420
415   TAB=TAU(5)+2.D0*TAU(7)
      TBC=TAU(6)+2.D0*TAU(9)
      TAC=TAU(4)+2.D0*TAU(8)
      RH(1)=3.D0*(TAU(1)/ASQ+TAU(2)/BSQ+TAU(3)/CSQ)+2.D0*(TAB/(A*B)+TBC/
     1 (B*C)+TAC/(A*C))
      RH(1)=-RH(1)/(HCK*16.D0)
      RH(2)=0
C
420   IF (.NOT.TESTW(1)) GO TO 428
      IAIBIC=BCONV**3/(A*B*C)
425   FORMAT (/,' A0=',F10.6,5X,'B0=',F10.6,5X,'C0=',F10.6,/'0IAIBIC=',
     1  F15.6,'E-117 (G*CM**2)**3')
      WRITE (6,425) A,B,C,IAIBIC
      LINES=LINES+3
428   IF(.NOT.CALRHO) GO TO 432
      WRITE (6,430) RH(1),RH(2)
430   FORMAT (/,' RHO 1=',E15.8,5X,'RHO 2=',E15.8)
C
C  POLYATOMIC MOLECULES.  MAKE X AND Y MATRICES SYMMETRIC.
C
432   IF ( NNU.LE.9 ) THEN
      DO 435 I=1,NNU
      DO 435 J=I,NNU
      X(J,I)=X(I,J)
435   CONTINUE
      DO 455 I=1,NNU
      DO 455 J=I,NNU
      DO 455 L=J,NNU
      IF (I.NE.J) GO TO 440
      IF (J-L) 445,455,445
440   IF (J-L) 450,445,450
445   Y(J,L,I)=Y(I,J,L)
      Y(L,I,J)=Y(I,J,L)
      GO TO 455
450   Y(I,L,J)=Y(I,J,L)
      Y(J,I,L)=Y(I,J,L)
      Y(J,L,I)=Y(I,J,L)
      Y(L,I,J)=Y(I,J,L)
      Y(L,J,I)=Y(I,J,L)
455   CONTINUE
      IF (TEST(14)) WRITE (6,460) (((I,J,L,Y(I,J,L),L=J,NNU),J=I,NNU),I=
     11,NNU)
460   FORMAT (5(' Y(',I1,',',I1,',',I1,') =',F7.3,3X))
C
C  APPLY X CORRECTIONS FOR NRRAO1 AND 2.
      DO 470 I=1,NNU
      DO 470 J=1,NNU
      CY=0.0
      DO 465 K=1,NNU
      IF ((K.NE.I).AND.(K.NE.J)) CY=CY+DN(K)*Y(I,J,K)/2.
465   CONTINUE
      IF (I.EQ.J) X(I,I)=X(I,I)+Y(I,I,I)*(1.5*DN(I)+3.)+CY
      IF(I.NE.J)X(I,J)=X(I,J)+(DN(I)+1.)*Y(I,I,J)+(DN(J)+1.)*Y(I,J,J)+CY
470   CONTINUE
C
C  GII CORRECTIONS
      DO 475 I=1,NNU
      IF (G(I).EQ.0.) GO TO 475
      G(I)=G(I)+B
      IF (TESTW(4)) X(I,I)=X(I,I)+G(I)/3.
      IF (.NOT.TESTW(4)) V(I)=V(I)-G(I)
475   CONTINUE
      ENDIF
C
C  INTERMEDIATE OUTPUT--XIJS AND ELECTRONIC LEVEL.
C
480   IF (.NOT.TEST(14)) GO TO 505
      IF (TESTW(2)) GO TO 498
      WRITE (6,485)
485   FORMAT (/' X(I,J),G(I,I)' )
      DO 490 I=1,NNU
490   WRITE (6,495) (X(I,J),J=1,NNU),G(I)
495   FORMAT (1H ,10F10.4)
498   IF (TESTW(6)) WRITE (6,500) LELC
500   FORMAT (/,'1ELECTRONIC LEVEL =',I2)
505   IF (TESTW(5)) TESTW(4)=.TRUE.
C
C  CALL LINK1 TO CALCULATE PARTITION FUNCTION AND DERIVATIVES FOR LEVEL.
510   CALL LINK1 (IROT,NIR)
C
C IF COLS 1-4 = COLS 1-4 OF PREVIOUS RECORD, ASSUME THERE IS ANOTHER
C      ELECTRONIC LEVEL AND GO TO 140.
C  OTHERWISE CALCULATE FUNCTIONS FROM Q AND DERIVATIVES (VALUES FOR
C      MULTIPLE ELECTRONIC STATES HAVE BEEN SUMMED).
      IF (CODE(1).EQ.SUB) GO TO 70
      NT1=NT
      IF (ASINDT.NE.0..AND..NOT.TEST(19)) NT1=NT+1
      DO 520 I=NIT,NT1
      IF (.NOT.TESTW(6)) GO TO 515
      Q=GHRT(I)
      GHRT(I)=DLOG(Q)
      DQ=HHRT(I)/Q
      HHRT(I)=DQ
      CPR(I)=CPR(I)/Q+(2.-DQ)*DQ
515   GHRT(I)=GHRT(I)+1.5*DLOG(WEIGHT)+2.5*DLOG(T(I))+SCONST
      HHRT(I)=HHRT(I)+2.5
      SR(I)=GHRT(I)+HHRT(I)
520   CPR(I)=CPR(I)+2.5
C
      IF (ASINDT.EQ.0..OR.TEST(19)) GO TO 535
C
C  CALCULATE ENTHALPY FOR ASSIGNED T ON FORMULA RECORD.
C
      IF (DABS(ASINDT-298.15D0).GT..05) GO TO 525
      SPECH=HHRT(NT1)*298.15D0
      H298HR=SPECH
      GO TO 530
525   SPECH=HHRT(NT1)*ASINDT
530   TEST(19)=.TRUE.
535   TEST(9)=.TRUE.
      GO TO 575
540   WRITE (6,545)
545   FORMAT (/,' WRONG NUMBER OF NU-S(V-S) (POLY)')
      GO TO 570
550   WRITE (6,555)
555   FORMAT (/,' THE VALUE OF B IS MISSING (POLY)')
      GO TO 570
560   WRITE (6,565)
565   FORMAT (/,' THE VALUE OF C IS MISSING (POLY)')
570   TEST(16)=.TRUE.
575   RETURN
      END
      SUBROUTINE PRINT (N,L)
      SAVE
C
      DOUBLE PRECISION ECOS,ESIN
C
      COMMON/BLK2/ECOS(188,4),ESIN(187,4),LL(4),RSYM(4),LR(4)
C
      DO 30 J=1,N,10
      JM1=J-1
      K=JM1+10
      IF (K.GT.N) K=N
      WRITE (6,10) JM1
10    FORMAT (5X,I3,' +')
      WRITE (6,20) (ECOS(I,L),I=J,K)
20    FORMAT (10X,'LEVEL(+)',10F11.3)
30    WRITE (6,40) (ESIN(I,L),I=J,K)
40    FORMAT (10X,'LEVEL(-)',10F11.3)
      RETURN
      END
      SUBROUTINE PUNCH
      SAVE
      PARAMETER (IOCFT=10)
C
C     TEST(4)  SPECIES IS A GAS.
C     TEST(6)  SPECIES IS A SOLID.
C     TEST(13) DATA ARE IN THE FORM, H-H298 AND -(F-H298)
C
      DIMENSION FMLA(6)
      DOUBLE PRECISION HF298,HH0
      real*8 figs
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      HF298=ASINDH
      NL=intvls-ivl1+1
      DO 30 IA=ivl1,intvls
      IF (COEF(1,IA).EQ.0.D0 .AND. COEF(3,IA).EQ.0.D0 .AND.
     & COEF(4,IA).EQ.0.D0) GO TO 20
      COEF(9,IA)=figs(COEF(9,IA)+HF298)
      GO TO 30
20    NL=NL-1
30    CONTINUE
      J=3*NL+5
      WRITE (6,32)
32    FORMAT (/,' THERMODYNAMIC DATA COEFFICIENTS, RECORD IMAGES -')
      IF (TEST(13)) then
      HH0=0.
      else
      HF298=H298HR+HF298
      HH0=H298HR*RR
      endif
      HF298=HF298*RR
C
C  COEFFICIENTS - GENERAL FORM
C
      WRITE (6,310) (NAME(I,INA),I=1,19)
310   FORMAT (/,1x,18A4,A2)
      WRITE (IOCFT,315) (NAME(I,INA),I=1,19)
315   FORMAT (18A4,A2)
C
      DO 332 I=1,5
332   FMLA(I)=MLA(I)
      IF (.NOT.TEST(4)) NFZ=NFZ+1
      WRITE (6,338) NL,DATE,(FORMLA(1,I),FORMLA(2,I),FMLA(I),I=1,5),
     1 NFZ,WEIGHT,HF298
338   FORMAT (2X,I1,1X,A4,A2,1X,5(2A1,F6.2),I2,F13.7,F15.3)
      WRITE (IOCFT,340) NL,DATE,(FORMLA(1,I),FORMLA(2,I),
     1 FMLA(I),I=1,5),NFZ,WEIGHT,HF298
340   FORMAT (1X,I1,1X,A4,A2,1X,5(2A1,F6.2),I2,F13.7,F15.3)
      DO 500 L=ivl1,intvls
c  Following statement inserted 10/31/91
      IF (COEF(1,L).EQ.0.D0 .AND. COEF(2,L).EQ.0.D0) GO TO 500
      WRITE (6,368) TC(L),TC(L+1),NF(L),(EX(I,L),I=1,8),HH0,
     * (COEF(J,L),J=1,10)
368   FORMAT (2X,2F10.3,I2,8F5.1,2X,F15.3,1P/1X,5D16.9/1X,5D16.9)
      WRITE (IOCFT,370) TC(L),TC(L+1),NF(L),(EX(I,L),I=1,8),HH0,
     * (COEF(J,L),J=1,10)
370   FORMAT (1X,2F10.3,I2,8F5.1,2X,F15.3,1P/5D16.9/5D16.9)
500   CONTINUE
      ivl1=intvls
C  Following statement moved here from bottom of DELH 2/14/91
      idone=intvls
      BACK = .false.
      RETURN
      END
      SUBROUTINE QSUM (TEST)
      SAVE
C
C  ACCUMULATE VALUES OF Q AND ITS DERIVATIVES.
C
      double precision CPROUT
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      real*8 v,x,y,alfa,alfb,alfc,g,wx,beta,a,b,c,dard,dd,wf,w,sym,stwt,
     1 t00,theta,rh,r,s,ql,q,qln,dq,ddq,qtot,qlntot,dqtot,ddqtot,aij,
     2 aiii,ai
      logical testw
      COMMON /WCOMMN/ V(300),X(9,9),Y(9,9,9),ALFA(9),ALFB(9),ALFC(9),
     1 G(9),WX(9),BETA(4),A,B,C,DARD,DD,WF,W,SYM,STWT,T00,THETA(3),
     2 RH(6),R(300,3),S(300,3),QL(3),Q,QLN,DQ,DDQ,QTOT,QLNTOT,DQTOT,
     3 DDQTOT,AIJ(9,9),AIII,AI(9),DN(300),ND(300),CORT,NNU,TESTW(6)
C
      LOGICAL TEST
C
      IF (.NOT.TEST) GO TO 20
      Q=0.
      IF (DABS(QLN).LE.88.) Q=DEXP(QLN)
      CPROUT=DDQ+2.d0*DQ
      WRITE (6,10) LABEL,Q,QLN,DQ,CPROUT
10    FORMAT (4X,A4,E21.4,3F18.8)
20    QLNTOT=QLNTOT+QLN
      DQTOT=DQTOT+DQ
      DDQTOT=DDQTOT+DDQ
      QLN=0.0
      DQ=0.0
      DDQ=0.0
      RETURN
      END
      SUBROUTINE RECO
      SAVE
C
C  READ IN THERMO PROPERTIES DIRECTLY OR READ IN COEFFICIENTS.
C
      CHARACTER*4 AC,ACPR,SRCR,AE
      CHARACTER*4 AS,A0,A2,CAL,CH,CHHO,CP,CS,DELT,GH,GHO,BARS,ATMS
      CHARACTER*4 GMH,H,HH,HHD1,HHO,HH2C,HR,HRT,H29,PMLT,DATAR
      CHARACTER*4 PNCH,REDUC,S,SLT,SLT0,SLT2,SRC,SSR,TCOE,TI,RCODE
C
      DOUBLE PRECISION TT,RT,HCOEF,SCOEF,DOR,HCOE,SCOE,SS,TEX,HHRTT
      DOUBLE PRECISION TJ1,TJ2,RSAVE,SRCOR,TX(4),TZ,TY
      LOGICAL TSTRED,TSTCO,TSTK ,scset,hcset
      DIMENSION REX(10)
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      DATA SLT0/'0/T'/,SLT2/'2/T'/,  TI  /'T   '/,  PNCH /'TPUN'/
     2, CHHO /'CHHO'/, AE  /'E'   /,   HH2C /'CHH '/,  HR  /'CH/R'/
     3, SRC /'CS/R'/,  CAL /'CAL '/,   REDUC/'REDU'/,  HHD1 /'CH-H'/
     4, HH  /'H-H' /,  GMH /'-G-H'/,   S  /'S   '/,    SLT  /'/T  '/
     5, GH  /'-GH'/,   ACPR /'CP/R'/,  HRT /'H/RT'/,   SSR  /'S/R '/
     6, H29 /'H298'/,  DELT /'DELT'/,  AC /'C   '/,    AS  /'AS  '/
     7, PMLT  /'MELT'/, HHO  /'H-HO'/,  GHO  /'-GHO'/
     8, CS  /'CS  '/,  CH  /'CH  '/,   CP  /'CP  '/,   H  /'H   '/
     9, TCOE /'TCOE'/, A0  /'0   '/,   A2  /'2'/,      BARS/'BAR'/
     $, ATMS/'ATM'/,   SRCR/'SRCO'/,   DATAR/'R'/
C
C  INITIALIZE.  NIT IS INDEX FOR NEXT T AND CORRESPONDING FUNCTIONS.
C
      LST=0
      scset=.false.
      hcset=.false.
      NTT=NIT
      lsave=intvls
      isave=ivl1
      BACK=.TRUE.
      RSAVE=R
      SRCOR=0
      HHRT(NTT)=0.
      GHRT(NTT)=-1.0
      SR(NTT)=0.
      if (nt.gt.0) THEN
        TT=T(NIT-1)
        TJ1=T(NIT)
        TJ2=T(NT)
      endif
      TSTCO=.FALSE.
      TSTRED=.FALSE.
      TSTK=.FALSE.
      DOR=-1.D0
C
C  STORE INFORMATION FROM METHOD RECORD.
C
      DO 150 I=1,4
      IF (ABEL(1,I).EQ.'READ') GO TO 150
      IF (ABEL(1,I).EQ.H29) GO TO 30
      IF (ABEL(1,I).EQ.DELT) GO TO 120
      IF (ABEL(1,I).EQ.'COEF') GO TO 40
      IF (ABEL(1,I).EQ.'KCAL') GO TO 50
      IF (ABEL(1,I).EQ.'KJOU') GO TO 60
      IF (ABEL(1,I).EQ.CAL) GO TO 70
      IF (ABEL(1,I).EQ.'JOUL') GO TO 80
      IF (ABEL(1,I).EQ.datar) GO TO 65
C
C  IF LABEL = 'DMLE', COEFFICIENTS ARE FOR CP/R.
C
      IF (ABEL(1,I).EQ.REDUC) GO TO 90
      IF (ABEL(1,I).EQ.'DMLE') GO TO 90
      IF (ABEL(1,I).EQ.SRCR) GO TO 95
      IF (ABEL(1,I).EQ.BARS) GO TO 100
      IF (ABEL(1,I).EQ.ATMS) GO TO 105
      IF (ABEL(1,I).EQ.PMLT) GO TO 110
      IF (ABEL(1,I).EQ.PMLT) GO TO 110
      IF (ABEL(1,I).EQ.BLANK) GO TO 150
C
      WRITE (6,20) ABEL(1,I)
20    FORMAT (/,' ERROR IN CODE--',A4,6X,' (RECO)')
      GO TO 150
30    H298H0=WORD(I)
      H298HR=0.
      GO TO 150
40    TSTCO=.TRUE.
      GO TO 150
50    TSTK=.TRUE.
      IF (R.GT.8.) GO TO 70
      GO TO 150
60    TSTK=.TRUE.
      IF (R.LT.8.) GO TO 80
      GO TO 150
65    r=word(i)
      GO TO 150
70    IF(R.GT.8.D0)R=R/4.184D0
      GO TO 150
80    IF(R.LT.8.D0)R=R*4.184D0
      GO TO 150
90    TSTRED=.TRUE.
      GO TO 150
C
C  TO CORRECT S/R FROM TRC TABLES TO ATM, SUBTRACT LN 1.01325 OR .013163
C
 95   SRCOR=SRCOR+WORD(I)
      GO TO 150
100   IF (.NOT.BAR.AND.TEST(4)) SRCOR=-.013163D0 +SRCOR
      GO TO 150
105   IF (BAR.AND.TEST(4)) SRCOR= .013163+SRCOR
      GO TO 150
110   PTMELT=WORD(I)
      GO TO 150
C
C  DELTAH OR DELTAS FOR HEAT OF TRANSITION.  CALCULATE H-H0/RT AND
C      -(G-H0)/RT FOR NEW PHASE.
C
120   IF (ABEL(2,I).EQ.AS) WORD(I)=WORD(I)*TT
      IF (NIT.GT.1) GO TO 140
      TEST(16)=.TRUE.
      WRITE (6,130)
130   FORMAT (/' T FOR TRANSITION UNKNOWN. GO TO NEXT SPECIES. (RECO)')
      GO TO 880
140   DOR=WORD(I)
150   CONTINUE
C
      IF(.NOT.TSTK) GO TO 153
      IF(DOR.NE.-1.D0)  DOR=DOR*1000.D0
      IF(T(NIT-1).GT.298.15)GO TO 153
         H298H0=H298H0*1000.D0
         H298HR=H298HR*1000.D0
153   IF (DOR .LT. 0.D0) GO TO 155
      HHRT(NTT)=HHRT(NIT-1)+DOR/(R*TT)
      GHRT(NTT)=GHRT(NIT-1)
      SR(NTT)=GHRT(NTT)+HHRT(NTT)
155   IF (H298HR.EQ.0.) H298HR=H298H0/R
      IF (H298H0.EQ.0.) H298H0=H298HR*R
      IF (HHRT(NTT).EQ.0.) GO TO 160
      NLAST=NIT-1
C
C  IF THERE HAS BEEN A HEAT OF TRANSITION, CALL DELH TO CHECK FOR
C      LSTSQS OR PUNCHED COEFFICIENTS FOR PREVIOUS PHASE.
C
      CALL DELH
      if (BACK) then
           ivl1=isave
           intvls=lsave
      else
           isave=ivl1
           lsave=intvls
           BACK=.true.
      endif
      NNN=NIT
      NLAST=NT
C
160   DO 180 J=ivl1+1,8
      DO 180 I=1,10
180   COEF(I,J)=0.0
      TEST(17)=.false.
      NEX=0
      NDT=0
      JT=0
      NFIRST=0
      if(dabs(asindt-298.15).gt..001.or.h298h0.eq.0.) go to 190
      spech=h298h0/r
      TEST(19)=.true.
190   CALL INPUT
      NWORD=1
      IF (NFIRST.NE.0) GO TO 200
C
C  INITIALIZE FOR FIRST RECORD.
C
      SUB=CODE(1)
      HCOEF=0.0
      SCOEF=0.0
C
C  IF COLS 1-4 NOT = TO COLS 1-4 PREVIOUS RECORD, GO TO 210 (C240).
C
200   IF (CODE(1).NE.SUB) GO TO 690
      if(.not.tstco) go to 205
      if(nfirst.ne.0) go to 490
      if(nit.gt.nnn) then
	  ivl1=ivl1+1
      endif
205   DO 210 I=1,4
      IF (ABEL(2,I).EQ.A0.OR.ABEL(2,I).EQ.A2) ABEL(2,I)=BLANK
      IF (ABEL(2,I).EQ.SLT0.OR.ABEL(2,I).EQ.SLT2) ABEL(2,I)=SLT
210   CONTINUE
      NFIRST=NFIRST+1
      RCODE=BLANK
      DO 220 I=1,4
      IF (ABEL(1,I).EQ.TI) GO TO 240
220   CONTINUE
      RCODE=TI
      WRITE (6,230) RCODE
230   FORMAT (/' DATA RECORD WAS SKIPPED BECAUSE ',A4,
     & ' VALUE WAS MISSING',' (RECO)')
      LINES=LINES+2
      GO TO 190
C
C  IF TWO CONSECUTIVE T ABEL(1,I)S, ASSUME COEFFICIENTS
C
240   IF (ABEL(1,I+1).EQ.TI) GO TO 490
      TT=WORD(I)
      IF (DABS(TT-298.15D0).LT.5.D-02) TT=298.15D0
      RT=R*TT
      IF (TT.EQ.0.0) GO TO 280
C
C  CHECK FOR CP.
      DO 250 I=1,4
      IF (ABEL(1,I).EQ.CP) GO TO 260
      IF (ABEL(1,I).EQ.ACPR) GO TO 270
250   CONTINUE
      GO TO 280
260   CPR(NTT)=WORD(I)/R
      GO TO 280
270   CPR(NTT)=WORD(I)
C
C  CHECK FOR ENTHALPY.  SKIP IF CALCULATED FROM DELTAH.
C
280   IF ((HHRT(NTT).NE.0.).AND.NFIRST.EQ.1) GO TO 440
      DO 300 I=1,4
      IF (ABEL(1,I).EQ.HH.OR.ABEL(1,I).EQ.HHO) GO TO 310
      IF (ABEL(1,I).NE.H.AND.ABEL(1,I).NE.HRT) GO TO 300
      HHRT(NTT)=WORD(I)
      IF (ABEL(1,I).EQ.H) then
        HHRT(NTT)=WORD(I)/RT
        if (TSTK) HHRT(NTT) = HHRT(NTT)*1000.d0
      ENDIF
      IF (.NOT.TEST(8)) GO TO 290
      IF (ASINDT.NE.0..AND.ASINDT.NE.298.15D0) GO TO 290
      HHRT(NTT)=HHRT(NTT)-ASINDH/TT
      IF (ASINDT.EQ.298.15D0) TEST(13)=.TRUE.
      GO TO 330
290   TEST(18)=.TRUE.
      GO TO 330
300   CONTINUE
      RCODE=HHO
      GO TO 350
C
310   IF (TSTK) WORD(I)=WORD(I)*1000.D0
      HHRT(NTT)=WORD(I)
      IF (ABEL(2,I).EQ.SLT) HHRT(NTT)=WORD(I)/R
      IF (ABEL(2,I).EQ.BLANK.OR.ABEL(2,I).EQ.A2) HHRT(NTT)=WORD(I)/RT
      IF (FWORD(I).NE.2.) GO TO 330
      IF (TT.NE.0.) GO TO 320
      IF (H298H0.EQ.0.) H298H0=-WORD(I)
      GO TO 190
320   IF (H298H0.EQ.0.0) TEST(13)=.TRUE.
      HHRT(NTT)=HHRT(NTT)+H298H0/RT
C
C  CHECK FOR T= ASINDT ON FORMULA RECORD.
C
330   IF (DABS(TT-ASINDT).GT.0.005) GO TO 350
      SPECH=HHRT(NTT)*TT
      TEST(19)=.TRUE.
      IF (.NOT.TEST(18)) GO TO 350
      IF (.NOT.TSTCO.OR.DABS(SPECH-ASINDH).GT..1) GO TO 350
      WRITE (6,340)
340   FORMAT (/' H ALREADY SET, ASSUME H(T)-H298 = 0')
      SPECH=ASINDH
      HHRT(NTT)=H298H0/RT
      IF (H298H0.EQ.0.) TEST(13)=.TRUE.
      TEST(18)=.FALSE.
C
C  CHECK FOR FREE ENERGY FUNCTIONS.
C
350   GHRTT=-1.D0
      SS=-1.D0
      DO 390 I=1,4
      IF (ABEL(1,I).EQ.GHO) ABEL(1,I)=GH
      IF (ABEL(1,I).NE.GMH) GO TO 360
      GHRTT=WORD(I)/RT
      GO TO 370
360   IF (ABEL(1,I).NE.GH) GO TO 380
      GHRTT=WORD(I)
      IF (ABEL(2,I).EQ.SLT) GHRTT=WORD(I)/R
370   IF (FWORD(I).NE.2.) GO TO 400
      GHRTT=GHRTT-H298H0/RT
      IF (H298H0.EQ.0.) TEST(13)=.TRUE.
      GO TO 400
C
C  CHECK FOR ENTROPY FUNCTIONS.
C
380   IF (ABEL(1,I).EQ.S) SS=WORD(I)/R
      IF (ABEL(1,I).EQ.SSR) SS=WORD(I)
390   CONTINUE
400   IF (SS.NE.-1.D0) SR(NTT)=SS
      IF (GHRTT.NE.-1.D0) GHRT(NTT)=GHRTT
      IF (RCODE.NE.HHO) GO TO 410
      IF (SS.NE.-1.D0.AND.GHRTT.NE.-1.D0) HHRT(NTT)=SS-GHRTT
      GO TO 430
410   IF (GHRTT.EQ.-1.D0) GO TO 420
      SR(NTT)=GHRTT+HHRT(NTT)
      GO TO 430
420   IF (SS.NE.-1.D0) GHRT(NTT)=SS-HHRT(NTT)
430   SR(NTT)=SR(NTT)+SRCOR
      GHRT(NTT)=GHRT(NTT)+SRCOR
      IF (DABS(TT-ASINDT).GT.0.005) GO TO 440
      SPECH=HHRT(NTT)*TT
C
C  TEST(19)--THERE IS AN ENTHALPY FOR THE ASINDT ON THE FORMULA RECORD
C      STORED IN SPECH.
C
      TEST(19)=.TRUE.
C
C  TEST(9)--THERE ARE THERMODYNAMIC FUNCTIONS FOR AT LEAST ONE T.
C
440   TEST(9)=.TRUE.
      IF (NIT.EQ.NNN.or.tstco) GO TO 460
      IF (T(NIT-1).LT.PTMELT.AND.TT.GT.PTMELT) GO TO 450
      IF (TT.NE.T(NIT-1)) GO TO 460
450   NLAST=NIT-1
      CALL DELH
      if (BACK) then
           ivl1=isave
           intvls=lsave
      else
           isave=ivl1
           lsave=intvls
           BACK=.true.
      endif
      NNN=NIT
      NLAST=NT
460   IF (.NOT.TSTCO) GO TO 480
      NEX=0
      DO 470 J=ivl1+1,8
      DO 470 I=1,10
470   COEF(I,J)=0.
      GO TO 190
480   NT=NT+1
      T(NT)=TT
      NTT=NT+1
      NIT=NTT
      GO TO 190
C
C            PROCESS COEFFICIENTS
C
C  STORE CONTENTS OF DATA RECORD.
C  IF GHRT(NTT) IS NOT =  -1 (NTT=0,) CALCULATE INTEGRATION CONSTANTS
C     FROM THE ENTHALPY AND FREE ENERGY (OR S) WHICH HAVE JUST BEEN READ
C
490   IF (GHRT(NTT).NE.(-1.0)) GO TO 495
      NTT=MAX0(NTT-1,0)
495   ITX=0
      DO 670 ID=NWORD,4
      IF (ABEL(1,ID).EQ.BLANK) GO TO 670
      IF (ABEL(1,ID).EQ.TI) GO TO 560
      NDT=1
      IF (ABEL(1,ID).EQ.PNCH.OR.ABEL(1,ID).EQ.TCOE) GO TO 520
      IF (ABEL(1,ID).EQ.HHD1) GO TO 640
      IF (ABEL(1,ID).EQ.CHHO.OR.ABEL(1,ID).EQ.HH2C) GO TO 630
      IF (ABEL(1,ID).EQ.CS) GO TO 650
      IF (ABEL(1,ID).EQ.SRC) GO TO 660
      IF (ABEL(1,ID).EQ.CH) GO TO 600
      IF (ABEL(1,ID).EQ.HR) GO TO 610
      IF (ABEL(1,ID).EQ.AE) GO TO 580
      IF (ABEL(1,ID).EQ.AC) GO TO 590
      WRITE (6,500) ABEL(1,ID),WORD(ID)
C
500   FORMAT (/,A6,' IS AN INCORRECT LABEL FOR THE NUMBER --',E16.8,'.
     1  THUS THE VALUE WAS IGNORED.')
      GO TO 670
C
C  TEST(17)--WRITE COEFFICIENTS OUT ON I/O UNITS 12 AND 16
C    IF NO VALUE FOLLOWS THE TCOE CODE, USE LEAST SQUARES T RANGES.
C    TPUN ALSO TURNS OFF THE LEAST SQUARES (TEST(15)=.FALSE) FOR THE
C    REMAINDER OF THE PROBLEM UNLESS ANOTHER LSTS IS READ.
C
520   TEST(17)=.TRUE.
      if(word(id).eq.0.) go to 530
      itx=itx+1
      tx(itx)=word(id)
      GO TO 670
530   itx=2
      tx(2)=tj2
      tx(1)=tj1
      go to 670
C
C  TJ1 TO TJ2 = TEMPERATURE RANGE FOR WHICH COEFFICIENTS ARE GOOD.
C
560   IF (NDT.EQ.1) GO TO 570
      IF (JT.NE.1) TJ1=WORD(ID)
      IF (JT.EQ.1) TJ2=WORD(ID)
      JT=1
      GO TO 670
570   IDD=ID
      GO TO 680
580   NN=FWORD(ID)
      REX(NN)=WORD(ID)
      NEX=NEX+1
      GO TO 670
C
C  DIVIDE COEFFICIENTS BY R IF NO 'DMLE' LABEL ON METHOD RECORD
C  (TSTRED=F).
C
590   IF (.NOT.TSTRED) WORD(ID)=WORD(ID)/R
C
C  TEST(18)--ABSOLUTE VALUES FOR ENTHALPY.
C
      NN=FWORD(ID)
      COEF(NN,ivl1+1)=WORD(ID)
      GO TO 670
600   IF (.NOT.TSTRED) WORD(ID)=WORD(ID)/R
610   IF(NIT.EQ.1.OR..NOT.TEST(19)) GO TO 615
      if(.not.tstred) word(id)=word(id)/r
      HCOEF=WORD(ID)-SPECH
      hcset=.true.
      GO TO 670
615   TEST(18)=.TRUE.
      IF (ASINDT.NE.0.) WRITE (6,620)
620   FORMAT (/,' ENTHALPY IS ABSOLUTE--ASINDT SHOULD = 0.')
      LINES=LINES+2
630   if(.not.tstred) word(id)=word(id)/r
      HCOEF=WORD(ID)
      hcset=.true.
      GO TO 670
640   IF (.NOT.TSTRED) WORD(ID)=WORD(ID)/R
      GO TO 630
650   IF (.NOT.TSTRED) WORD(ID)=WORD(ID)/R
660   SCOEF=WORD(ID)
      scset=.true.
670   CONTINUE
      if (itx.eq.0) go to 190
      if (.not.TEST(15).or. TEST(17)) go to 677
      TZ= min(tx(1),tx(2),tx(3),tx(4))
      TY= max(tx(1),tx(2),tx(3),tx(4))
      if (ntrang.gt.0) go to 672
      if (TZ.le.200.) go to 675
      ntrang=2
      trange(1)=200.
      trange(2)=TZ
c     go to 675
c672   do 673 i=1,ntrang
c      n=i
c      if (trange(i).gt.TZ) go to 674
c673   continue
c      go to 674
c674   ntrang=n
c      trange(n)=TZ
c      if(n.le.1) go to 676
672   n=ntrang
      do 674 i=1,n
      if(abs(trange(i)-TZ).gt..01) go to 673
      ntrang=ntrang+1
      trange(ntrang)=tz
      go to 674
673   if(abs(trange(i)-ty).gt..01) go to 674
      ntrang=ntrang+1
      trange(ntrang)=TY
674   continue
                        go to 677
675   CALL DELH
c676   TEST(15)=.FALSE.
677   do 678 i=1,itx-1
c     intvls=intvls+1
c  intvls changed to ivl1 in next 2 statements 10/31/91
      tc(ivl1+i)=tx(i)
678   tc(ivl1+i+1)=tx(i+1)
      if (test(17).and.itx.gt.1) intvls=intvls+itx-1
      go to 190
C
680   NWORD=IDD
690   IF (.NOT.TSTCO) GO TO 880
      NF2=10
      NF1=NF2-1
      IVL1=IVL1+1
      intvls = max0(intvls,ivl1)
      NT1=NT
      IF ((ASINDT.EQ.0.).OR.TEST(19)) GO TO 710
      IF (ASINDT.GE.TJ1.AND.ASINDT.LE.TJ2) GO TO 700
      IF (DABS(ASINDT-298.15D0).GT.(.01).OR.H298HR.EQ.0.) GO TO 710
      SPECH=H298HR
      TEST(19)=.TRUE.
      GO TO 710
700   NT1=NT+1
      T(NT1)=ASINDT
710   I=NIT
720   IF (T(I).LT.TJ1.OR.T(I).GT.TJ2) GO TO 800
730   CPR(I)=0.0
      HHRTT=0.0
      SS=0.0
C
C  CALCULATE FUNCTIONS FROM EQUATION
C
      DO 740 J=1,NEX
      TEX=1.0
C
C  NTT = INDEX FOR THE ENTHALPY AND ENTROPY VALUES TO BE USED TO
C     CALCULATE THE INTEGRATION CONSTANTS.  IF THERE ARE NO SUCH VALUES,
C     NTT=0.
C
      IF (NTT.EQ.0) TT=T(I)
      IF (REX(J).NE.0.0) TEX=TT**REX(J)
      IF (REX(J).EQ.(-1.)) HHRTT=HHRTT+COEF(J,ivl1)*DLOG(TT)/TT
      IF (REX(J).NE.(-1.)) HHRTT=HHRTT+COEF(J,ivl1)/(REX(J)+1.0)*TEX
      IF (I.GT.NT) GO TO 740
      CPR(I)=CPR(I)+COEF(J,ivl1)*TEX
      IF (TEX.EQ.1.0) SS=COEF(J,ivl1)*DLOG(TT)+SS
      IF (TEX.NE.1.0) SS=SS+COEF(J,ivl1)/REX(J)*TEX
740   CONTINUE
      IF (NTT.EQ.0) GO TO 770
      if(.not.hcset)HCOEF=(HHRT(NTT)-HHRTT)*TT
      if(.not.scset)SCOEF=SR(NTT)-SS
      hcset=.false.
      scset=.false.
      IF (DABS(TT-TLST).GT..001.or.lst.eq.1) GO TO 760
c
c  The following correction was made 6/5/91 for intervals with no
c      transition enthalpy. One statement new and one changed.
c
      dor=0.
      if(dabs(t(ntt)-t(ntt-1)).lt..001)DOR=HHRT(NTT)-HHRT(NTT-1)
      HCOE=(HLST-HHRTT)*TLST+dor*tlst
      SCOE=SLST+DOR-SS
      WRITE (6,750) TLST
750   FORMAT (/' COEFFICIENTS ADJUSTED TO FIT UPPER PHASE AT',F8.2)
      LST=1
760   NTT=0
      GO TO 730
770   IF (I.GT.NT) GO TO 790
      HHRT(I)=HHRTT+HCOEF/T(I)
      SR(I)=SS+SCOEF+srcor
      GHRT(I)=SR(I)-HHRT(I)
      IF (I.LE.NT) NIT=I+1
      IF (I.EQ.NT1) GO TO 800
      I=I+1
      GO TO 720
790   SPECH=HHRTT*ASINDT+HCOEF
      TEST(19)=.TRUE.
C
C  ivl1 = INDEX FOR TEMPERATURE INTERVALS
C  INTVLS = TOTAL NO. OF INTERVALS
C
800   NTT=NIT-1
      IF (LST.NE.1) GO TO 810
c  The following 3 statements were added 3/20/91
      tlst=t(ntt)
      if(tlst.gt.0)hlst=hhrt(ntt)+(hcoe/tlst-hcoef/tlst)
      slst=sr(ntt)+(scoe-scoef)
      HCOEF=HCOE
      SCOEF=SCOE
      LST=0
810   DO 820 I=1,nex
820   EX(I,ivl1)=REX(I)
      NF2=10
      NF1=9
C  The following statements was moved from below 'if' statement 12/19/96
      NF(ivl1)=NEX
      if (.not.TEST(17)) go to 860
C     NF(ivl1)=NEX
      COEF(NF1,ivl1)=HCOEF
      COEF(NF2,ivl1)=SCOEF
      if(intvls.eq.ivl1) go to 860
      do 850 k=ivl1+1,intvls
      NF(k)=NF(k-1)
      DO 840 NN=1,10
      COEF(NN,k)=COEF(NN,k-1)
      EX(NN,k)=REX(NN)
840   CONTINUE
850   CONTINUE
860   TC(ivl1)=MAX(tc(ivl1),tj1)
      TC(intvls)=MIN(tc(intvls),tj2)
      IF (CODE(1).NE.SUB) GO TO 870
      NDT=0
      JT=0
      NEX=0
      GO TO 490
870   TEST(9)=.TRUE.
880   R=RSAVE
      if (TEST(17)) then
         ivl1=isave+1
      else
         intvls = lsave
         ivl1= isave
      endif
      RETURN
      END
       
      SUBROUTINE TABLES
      SAVE
C
C  WRITE OUTPUT FILE FOR THE FIRST 3 TABLES OF THERMODYNAMIC FUNCTIONS.
C
      LOGICAL HASIND,HH298,HABS,HHZERO
      DOUBLE PRECISION RUSE(3),TI,HH29,GH29,HH,AR,ART,S,CP,GH,G,H,H298
      DOUBLE PRECISION HR,HSG(7)
      DIMENSION VFM(20)
C
      CHARACTER*4 VFM,XX,F14,F16,P3,P6,P7,ht,hmh,hsubt,dml,hdml,rt,slhr
      CHARACTER*4 zero,two,href
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      EQUIVALENCE (TEST(8),HASIND),(TEST(13),HH298),(TEST(18),HABS)
      EQUIVALENCE (HH,HSG(1)),(HSG(2),HH29),(HSG(3),S),(HSG(4),GH),
     1  (HSG(5),GH29),(HSG(6),H),(HSG(7),G)
C
      DATA XX/',14X'/, F16/',F16'/, P7/'.7'/, P3/'.3'/
      DATA P6/'.6'/,F14/',F14'/,VFM(1)/'(1H '/, VFM(2)/',F12'/
      DATA VFM(3)/'.2'/, VFM(4)/',F12'/, VFM(5)/'.5'/, VFM(20)/')'/
      DATA HSUBT/'H'/,HMH/'H-H'/,SLHR/'/R'/,RT/'RT'/,zero/'0'/,two/'2'/
C
      RUSE(1)=R
      RUSE(2)=R
      IF(R.LT.8.)RUSE(2)=R*4.184D0
      RUSE(3)=RUSE(2)/4.184D0
      H298=H298HR
      HHZERO=.TRUE.
      IF(HH298.AND.(H298HR.EQ.0..OR.H298HR.EQ.ASINDH))HHZERO=.FALSE.
      IF(.NOT.HHZERO)H298=0.
      IF(HABS.AND.ASINDT.NE.0.)HHZERO=.FALSE.
C
C  INITIALIZE FORMAT FOR THE FIRST TABLE
C
      DO 30 I=6,14,2
      VFM(I)=F14
      VFM(I+1)=P7
30    CONTINUE
      VFM(16)=F16
      VFM(17)=P7
      NHSG=7
      IF (HHZERO) GO TO 40
      VFM(6)=BLANK
      VFM(7)=XX
      VFM(12)=BLANK
      VFM(13)=XX
      IF(ASINDT.EQ.298.15D0) GO TO 55
40    IF (H298HR.NE.0. .OR. HH298) GO TO 50
C
      VFM(8)=BLANK
      VFM(9)=XX
      VFM(14)=BLANK
      VFM(15)=XX
50    IF (HASIND.OR.HABS) GO TO 55
      NHSG=NHSG-2
      VFM(16)=BLANK
      VFM(17)=BLANK
55    if (mtab.eq.-1) go to 60
      ht=hmh
      if(habs)ht=hsubt
      dml=blank
      if(mtab.eq.1) dml=slhr
      hdml=blank
      if(mtab.eq.1)hdml=rt
      href=zero
      if(hh298) href=two
      if(habs) href=blank
C
C  PRINT MFIG TABLES
C
60    DO 190 NTABLE=1,3
      LINES=LINES+4
      IF (.NOT.TOUT(NTABLE)) GO TO 190
      WRITE(6,65) (SNM(I),I=1,4),((NAME(I,J),I=1,4),J=1,NNA)
65    FORMAT (/,1x,4A4,6(3X,4A4))
C
      if (ntable.ne.1) then
        IF (VFM(7).EQ.P7) VFM(7)=P3
        IF (VFM(9).EQ.P7) VFM(9)=P3
        VFM(11)=P6
        IF (VFM(13).EQ.P7) VFM(13)=P3
        IF (VFM(15).EQ.P7) VFM(15)=P3
        VFM(16)=F16
        VFM(17)=P3
      endif
C
      IF (.not.HASIND) then
        WRITE (6,70)
70      FORMAT (/,' NO HZERO VALUE IS AVAILABLE')
      else
        LINES=LINES+6
        HR=ASINDH*RUSE(NTABLE)
        IHT=0
        IF(.NOT.HHZERO)IHT=298
C
        IF(NTABLE.eq.1) then
          WRITE(6,80)IHT,ASINDH
80        FORMAT(/,' ASSIGNED H/R AT',I4,' K =',F12.3,' K'/)
          WRITE (6,85)
85        FORMAT (9X,'T',8X,'CP/R',8X,'(H-H0)/RT    (H-H298)/RT',6X,
     1    'S/R',8X,'-(G-H0)/RT   -(G-H298)/RT',8X,'H/RT',12X,'-G/RT'//)
C
        ELSEIF(NTABLE.EQ.2) then
          WRITE(6,90) IHT,HR
90        FORMAT(/,' ASSIGNED H AT',I4,' K =',F12.3,' J/MOLE')
          WRITE (6,100)
100       FORMAT (/,8X,'T',10X,'CP',11X,'H-H0',10X,'H-H298',8X,'S',11X
     1    ,'-(G-H0)',6X,'-(G-H298)',11X,'H',14X,'-G')
          WRITE (6,115)
115       FORMAT(7X,'DEG-K',5X,'J/MOL-K',8X,'J/MOL',10X,'J/MOL',6X,
     *    'J/MOL-K',9X,'J/MOL',9X,'J/MOL',11X,'J/MOL',11X,'J/MOL'/)
C
        ELSEIF(NTABLE.EQ.3) then
          WRITE(6,120) IHT,HR
120       FORMAT(/,' ASSIGNED H AT',I4,' K =',F12.3,' CAL/MOLE')
          WRITE (6,100)
          WRITE(6,130)
130    FORMAT(7X,'DEG-K',4X,'CAL/MOL-K',7X,'CAL/MOL',7X,'CAL/MOL',4X,
     * 'CAL/MOL-K',7X,'CAL/MOL',7X,'CAL/MOL',9X,'CAL/MOL',9X,'CAL/MOL'/)
        endif
C
      endif
      VFM(18)=VFM(16)
      VFM(19)=VFM(17)
C
      DO 180 I=1,NT
      TI=T(I)
      IF(DABS(T(I)-298.15D0).LT..02)TI=298.15D0
      CP=CPR(I)
      HH=HHRT(I)
      S=SR(I)
      GH=GHRT(I)
      IF (NTABLE.NE.1)GO TO 140
      AR=1./TI
      ART=1.
      GO TO 150
140   AR=RUSE(NTABLE)
      ART=AR*TI
      CP=CP*AR
      HH=HH*ART
      S=S*AR
      GH=GH*ART
C
150   IF (.NOT.HASIND.AND..NOT.HABS) GO TO 160
      H=HHRT(I)*ART
      G=GHRT(I)*ART
      IF(.NOT.HABS)GO TO 155
      IF(ASINDT.NE.0.)GO TO 160
      HH=HH-ASINDH*AR
      GH=GH+ASINDH*AR
      GO TO 160
155   H=H+ASINDH*AR
      G=G-ASINDH*AR
160   IF (H298.NE.0.) GO TO 165
      HH29=HH
      GH29=GH
      GO TO 170
165   HH29=(HHRT(I)-H298/TI)*ART
      GH29=(GHRT(I)+H298/TI)*ART
170   IF (H298.NE.0.D0.AND.HHZERO) GO TO 175
      WRITE (6,VFM) T(I),CP,HH,S,(HSG(J),J=5,NHSG)
      GO TO 178
175   WRITE (6,VFM) T(I),CP,(HSG(J),J=1,NHSG)
C
178   CONTINUE
C     IF(MTAB.EQ.NTABLE) WRITE (IOCFT,179)T(I),DML,CP,HT,HREF,HDML,HH,DML,S
C79   FORMAT(6X,'T',5X,F10.4,'  CP',a2,2X,F10.5,2x,A3,A1,A2,1P,D12.5,'S'
C    *,0P,A2,3X, F12.6)
180   CONTINUE
      CALL PAGEID
190   CONTINUE
      RETURN
      END

      SUBROUTINE TEMPER
      SAVE
C
C  STORES T SCHEDULE IN T ARRAY FROM DATA ON TEMP RECORDS.
C  NT = NUMBER OF TEMPERATURES
C  TINTVL = I VALUE ON TEMP RECORD.  PRESERVED IF LAST VALUE ON RECORD
C      SO IT WILL BE AVAILABLE FOR USE WITH DATA ON NEXT TEMP RECORD.
C
      CHARACTER*4 AT
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      DATA AT/'T'/
C
10    DO 160 J=1,4
      IF (ABEL(1,J).EQ.BLANK) GO TO 160
      IF (ABEL(1,J).EQ.AT) GO TO 60
      IF (ABEL(1,J).EQ.'I') GO TO 40
20    WRITE (6,30)
30    FORMAT (/,' ERROR IN LABELS ON TEMP RECORD, (TEMPER)')
      GO TO 190
40    IF (NT.GT.0) GO TO 50
      GO TO 20
50    TINTVL=WORD(J)
      GO TO 160
60    IF (NT.EQ.0) GO TO 120
      IF (TINTVL.EQ.0.0) GO TO 130
70    IF (T(NT).GE.(298.15D0-.0001)) GO TO 80
      IF ((T(NT)+TINTVL).GT.(298.15D0+.0001)) GO TO 110
80    NT=NT+1
      IF (NT.GT.502) GO TO 170
      T(NT)=T(NT-1)+TINTVL
90    IF (T(NT).GE.(WORD(J)-.0001)) GO TO 100
      GO TO 70
100   TINTVL=0.0
      GO TO 160
110   NT=NT+1
      T(NT)=298.15D0
      NT=NT+1
      T(NT)=T(NT-2)+TINTVL
      GO TO 90
120   NT=1
      T(NT)=WORD(J)
      GO TO 160
130   IF (T(NT).GE.(298.15D0-.0001)) GO TO 140
      IF (WORD(J).GT.(298.15D0+.0001D0)) GO TO 150
140   NT=NT+1
      IF (NT.GT.502) GO TO 170
      T(NT)=WORD(J)
      GO TO 160
150   NT=NT+1
      T(NT)=298.15D0
      GO TO 140
160   CONTINUE
      goto 200
C
170   NT=502
      WRITE (6,180)
180   FORMAT (/,' NUMBER OF TEMPERATURES EXCEEDS 502, (TEMPER)')
      RETURN
C
C   TEMP RECORD IS BLANK--USE STANDARD TEMPERATURE RANGE
C
190   ABEL(1,1)=AT
      WORD(1)=100.0 D0
      ABEL(1,2)='I'
      WORD(2)=100.0 D0
      ABEL(1,3)=AT
      WORD(3)=6000.0 D0
      GO TO 10
200   RETURN
      END
C
      SUBROUTINE WCALC(A,Y,T,CPO,CPI,SUM,C,H,G)
      SAVE
C
      DOUBLE PRECISION A(4),Y,T,SUM,C,H,G,SIGF(4),SIGC,SIGY,SIGS
C
      DO 200 J=1,4
      SIGF(J)=FLOAT(J+1)*A(J)
      DO 200 N=J,4
200   SIGF(J)=SIGF(J)+A(N)
      SIGC=(((A(4)*Y+A(3))*Y+A(2))*Y+A(1))*Y
      SIGS=(((A(4)*Y/5.+A(3)/4.)*Y+A(2)/3.)*Y+A(1)/2.)*Y
      SIGY=(((SIGF(4)*Y/3.+SIGF(3)/2.)*Y/5.+SIGF(2)/6.)*Y+SIGF(1)/3.)*
     1 Y/2.
      C=CPO+(CPI-CPO)*Y*(Y+(Y-1.)*SIGC)
      H=(CPO-(CPI-CPO)*((2.+SUM)*(Y/2.-1.+(1./Y-1.)*DLOG(T/Y))+Y*SIGY))
      G=CPI*DLOG(T)-(CPI-CPO)*(DLOG(Y)+(1.+SIGS)*Y)
      RETURN
      END
      SUBROUTINE WILHOI
      SAVE
C
C  A PROGRAM TO EXTRAPOLATE THERMO FOR GASES WITH WILHOIT POLYNOMIALS.
C
C     REFERENCE - WILHOIT,R.C.,"THERMODYNAMICS RESEARCH CENTER CURRENT
C          NEWS," VOL.3, TEXAS A&M UNIVERSITY (1975).
C
C       CPO=7/2*R FOR LINEAR MOLECULE. CPO=4*R FOR NON-LINEAR MOLECULE.
C       CPI=(3*N-3/2)*R FOR LINEAR MOLECULE. CPI=(3*N-2)*R FOR NONLINEAR
C
      DIMENSION M(15),L(15)
      DOUBLE PRECISION  HX,SX,AS(4),SUMS,CCAL(502),HCAL(502),GCAL(502),
     1 C(502),SCAL,HDIF1,SDIF1,HDIF(502),SDIF(502),SHPT(8),SPT(8),A(8),
     2 Y(502),TS(8,8),SUM,HCONS,SCONS,SD,SDMIN,HERR,SERR,THERR,TSERR,
     3 tstart
C
      CHARACTER*4 PREFIX,PXNL
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
      DATA PXNL/'NON-'/
C
      IF(NOATMS.GT.1) GO TO 8
      WRITE (6,6)
6     FORMAT (/,' WILHOIT EXTRAPOLATION NOT AVAILABLE FOR MONATOMICS')
      GO TO 330
8     tstart = 0.d0
      DO 10 I=1,4
      IF (ABEL(1,I).EQ.' ') GO TO 10
      IF (ABEL(1,I).EQ.'LINE') TEST(2)=.TRUE.
      IF (ABEL(1,I).EQ.'TSTA') tstart = WORD(I)
10    CONTINUE
      IF (TEST(2)) GO TO 20
      CPO=4.0
      CPI=(3*NOATMS-2)
      PREFIX=PXNL
      GO TO 30
20    PREFIX=BLANK
      CPO=3.5
      CPI=FLOAT(3*NOATMS)-1.5
30    B=0.0
      SDMIN=1.E+10
      IBEGIN = 1
      DO 35 I=1,NT
      IF (CPR(I).NE.0..AND.T(I).GE.tstart) GO TO 36
      IF (CPR(I).LE.0.) THEN
        WRITE(6,34)
34    FORMAT(/' >>> WILHOIT NOT USED TO EXTRAPOLATE TO LOWER T VALUES')
        LINES =LINES + 2
      ENDIF
      IBEGIN=I+1
35    continue
36    DO 310 IS=1,80
      B=B+50.0
      SD=0.
      DO 50 K=1,7
      SPT(K)=0.0
      DO 40 I=IBEGIN,NT
      IF (CPR(I).EQ.0.) GO TO 40
      Y(I)=(T(I)/(T(I)+B))
      SPT(K)=SPT(K)+(Y(I)*10.)**(K-1)
40    CONTINUE
50    CONTINUE
C
      DO 60 KR=1,4
      DO 60 KC=1,4
      K=KR+KC-1
60    TS(KC,KR)=SPT(K)
      DO 70 KR=1,4
      SHPT(KR)=0.0
      DO 70 I=IBEGIN,NT
      IF (CPR(I).EQ.0.) GO TO 70
      C(I)=(CPR(I)-CPO-((Y(I)**2)*(CPI-CPO)))/((CPI-CPO)*(Y(I)**2)*
     1 (Y(I)-1.))
      SHPT(KR)=SHPT(KR)+C(I)*(Y(I)*10.)**(KR-1)
70    CONTINUE
      DO 80 J=1,4
      L(J)=J
80    M(J)=J
      DO 160 J=1,4
      BIGEST=0.0
      DO 100 IL=J,4
      DO 90 IM=J,4
      KL=L(IL)
      KM=M(IM)
      IF (BIGEST.GE.DABS(TS(KL,KM))) GO TO 90
      BIGEST=DABS(TS(KL,KM))
      NL=IL
      NM=IM
90    CONTINUE
100   CONTINUE
      IF (BIGEST.NE.0.0) GO TO 120
      WRITE (6,110)
110   FORMAT ('1***ERROR IN WILHOIT EXTRAPOLATION***')
      GO TO 310
120   IT=L(J)
      L(J)=L(NL)
      L(NL)=IT
      IT=M(J)
      M(J)=M(NM)
      M(NM)=IT
      KL=L(J)
      KM=M(J)
      SHPT(KL)=SHPT(KL)/TS(KL,KM)
      DO 130 IA=1,4
      KLA=L(IA)
      IF (KLA.EQ.KL) GO TO 130
      SHPT(KLA)=SHPT(KLA)-SHPT(KL)*TS(KLA,KM)
130   CONTINUE
      IF (J.EQ.4) GO TO 170
      JP=J+1
      DO 140 IB=JP,4
      KMA=M(IB)
140   TS(KL,KMA)=TS(KL,KMA)/TS(KL,KM)
      DO 160 IA=1,4
      KLA=L(IA)
      IF (KLA.EQ.KL) GO TO 160
      DO 150 IB=JP,4
      KMA=M(IB)
150   TS(KLA,KMA)=TS(KLA,KMA)-TS(KL,KMA)*TS(KLA,KM)
160   CONTINUE
170   DO 180 I=1,4
      KLA=L(I)
      KMA=M(I)
180   A(KMA)=SHPT(KLA)
      SUM=0.
      DO 190 J=1,4
      A(J)=A(J)*10.0**(J-1)
190   SUM=SUM+A(J)
      HERR=0.
      SERR=0.
      NTS=0
      DO 210 I=IBEGIN,NT
      IF (CPR(I).EQ.0.) GO TO 210
      NTS=NTS+1
      CALL WCALC (A,Y(I),T(I),CPO,CPI,SUM,CCAL(I),HCAL(I),SCAL)
      HDIF(I)=(HCAL(I)-HHRT(I))*T(I)
      SDIF(I)=SCAL-SR(I)
      SD=SD+(CPR(I)-CCAL(I))**2
      GCAL(I)=SCAL-HCAL(I)
      IF(NTS.NE.1)GO TO 195
      HDIF1=HDIF(I)
      SDIF1=SDIF(I)
195   IT=I
      HX=DABS(HDIF(I)-HDIF1)
      IF (HX.LE.HERR) GO TO 200
      HERR=HX
      THERR=T(I)
200   SX=DABS(SDIF(I)-SDIF1)
      IF (SX.LE.SERR) GO TO 210
      SERR=SX
      TSERR=T(I)
210   CONTINUE
      SD=DSQRT(SD/NTS)
      IF (SD.GT.SDMIN) GO TO 230
      SDMIN=SD
      BMIN=B
      SCONS=-SDIF(IT)
      HCONS=-HDIF(IT)
      SUMS=SUM
      DO 220 J=1,4
220   AS(J)=A(J)
C
230   IF (.NOT.TEST(14)) GO TO 310
      WRITE (6,240)
240   FORMAT (/,3X,'WILHOIT COEFFICIENTS')
      WRITE (6,250) (A(J),J=1,3),A(4)
250   FORMAT (/,3X,'A(0) = ',E16.9,5X,'A(1) = ',E16.9,5X,'A(2)
     1 = ',E16.9,5X,'A(3) = ',E16.9)
      WRITE (6,260) B,CPO,CPI,PREFIX,NOATMS
260   FORMAT (/,' B =',F7.1,5X,'CPO/R =',F9.4,5X,'CPI/R =',F9.4,5X,A4,'
     1LINEAR',5X,'NO. ATOMS =',I4/)
      WRITE (6,270) SD,HERR,THERR,SERR,TSERR
270   FORMAT (1H ,'STD DEV =',F9.4,5X,'MAXERR H/R =',F9.4,2X,'T =',F9.2,
     15X,'MAXERR S/R =',F10.5,2X,'T =',F9.2)
      WRITE (6,280)
280   FORMAT (//8X,1HT,6X,'CP/R CALC',2X,'CP/R INPUT',6X,'H/RT CALC',10X
     1,'H/RT INPUT',8X,'H/R(CAL-INP)',4X,'-G/RT CAL',4X,'-G/RT INP',3X,'
     2S/R(CAL-INP)'/)
      DO 290 I=IBEGIN,NT
      IF (CPR(I).EQ.0.) GO TO 290
      WRITE (6,300) T(I),CCAL(I),CPR(I),HCAL(I),HHRT(I),HDIF(I),GCAL(I),
     1GHRT(I),SDIF(I)
290   CONTINUE
300   FORMAT (1X,3F11.3,3E19.7,3F13.5)
310   CONTINUE
      IF (SDMIN.GE.1.E+10) GO TO 330
C
C  EXTRAPOLATION
C
      DO 320 I=IBEGIN,NT
      IF (CPR(I).NE.0.) GO TO 320
      Y(I)=T(I)/(T(I)+BMIN)
      CALL WCALC (AS,Y(I),T(I),CPO,CPI,SUMS,CPR(I),HHRT(I),SR(I))
      SR(I)=SR(I)+SCONS
      HHRT(I)=HHRT(I)+HCONS/T(I)
      GHRT(I)=SR(I)-HHRT(I)
      IF (TEST(19).OR.DABS(T(I)-ASINDT).GT..05)GO TO 320
      SPECH=HHRT(I)*298.15D0
      IF (DABS(ASINDT-298.15D0).GT..05) SPECH=HHRT(I)*ASINDT
      TEST(19)=.TRUE.
320   CONTINUE
C
      if (lines.ge.53) call pageid
      WRITE (6,240)
      WRITE (6,250) (AS(J),J=1,3),AS(4)
      WRITE (6,325) HCONS,SCONS
325   FORMAT(/' INTEGRATION CONSTANTS:    H/R =',E16.9,5X,'S/R =',E16.9)
      WRITE (6,260) BMIN,CPO,CPI,PREFIX,NOATMS
      LINES=LINES+7
      if (lines.ge.55) call pageid
C
330   RETURN
      END
      BLOCKDATA
C
      CHARACTER*4 ABEL,BLANK,CODE,DATE,EFAZ,FAZ,FORMLA,HEAT,LABEL
      CHARACTER*4 NAME,SNM,STD,SUB,SYMBOL
      COMMON /CHAR4/ ABEL(2,4),BLANK,CODE(2),DATE(2),EFAZ(100),FAZ,
     1 FORMLA(2,5),HEAT,LABEL,NAME(19,6),SNM(4),STD,SUB,SYMBOL(2,100)
C
      DOUBLE PRECISION ATMWT,ASINDH,ASINDT,COEF,CPR,GHRT,H298HR,H298H0,
     1 HCK,HHRT,HLST,PI,PTMELT,R,RR,SATM,SBAR,SCONST,SLST,SPECH,SR,T,
     2 TAPE,TC,TCONST,TLST,TRANGE,WEIGHT,WORD,TTRANS
      COMMON /REAL8/ ATMWT(100),ASINDH,ASINDT,COEF(10,9),CPR(502),
     1 GHRT(502),H298HR,H298H0,HCK,HHRT(502),HLST,PI,PTMELT,R,RR,SATM,
     2 SBAR,SCONST,SLST,SPECH,SR(502),T(0:502),TAPE(1506),TC(9),TCONST,
     3 TLST,TRANGE(9),WEIGHT,WORD(4),TTRANS(6)
C
      COMMON /REAL4/ AG(100),D(4),EX(10,9),FWORD(4),GG(100)
C
      COMMON /INTGR/ IC80,intvls,IVL1,ITR,JF(5),LINES,LSCODE(8),MLA(5),
     1 MLAS(5),MTAB,NATOM,NDFILE,NEL,NF(9),NF1,NF2,NFP,NIT,NKIND,NKINDS,
     2 NLAST,NMAT(100),NMLA(100),NNN,NOATMS,NT,NTMP,NTRANG,NFZ,INA,NNA
     3 ,IDONE
C
      LOGICAL BACK,BAR,CNST,CPONLY,INTV,NOCP,NOH,NOS,OLD,TEST,TOUT
      COMMON /LOGCL/ BACK,BAR,CNST,CPONLY(9),INTV(8),NOCP(9),NOH(9),
     1 NOS(9),OLD,TEST(20),TOUT(3)
C
C    Physical Constants:  Cohen,R.E.; and Taylor, B.N.:
C    THE 1986 CODATA RECOMMENDED VALUES OF THE FUNDAMENTAL PHYSICAL 
C    CONSTANTS. J.Phys.Chem.Ref.Data, Vol.17, No.4, 1988, pp 1795-1803.
C
      DATA RR/8.314510D0/, HCK/1.438769D0/, SATM /-3.664856D0/
      DATA SBAR/-3.651693D0/
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
C  ATOMIC WEIGHTS - Coplen,T.B., Atomic Weights of the Elements 1999,
C    2001, J.Phys.Chem.Ref.Data, vol.20, no.3, pp.701-712.
C
      DATA ATMWT/5.48579911D-04,1.00794D0,2.014102D0,4.002602D0,
     1 6.941D0,9.012182D0,10.811D0,12.0107D0,14.0067D0,15.9994D0,
     2 18.9984032D0,20.1797D0,22.989770D0,24.305D0,26.981538D0,
     3 28.0855D0,30.973761D0,32.065D0,35.453D0,39.948D0,39.0983D0,
     4 40.078D0,44.95591D0,47.867D0, 50.9415D0,51.9961D0,54.938049D0,
     5 55.845D0,58.933200D0,58.6934D0,63.546D0,65.39D0,69.723D0,72.64D0,
     6 74.92160D0,78.96D0,79.904D0,83.80D0,85.4678D0,87.62D0,88.90585D0,
     7 91.224D0,92.90638D0,95.94D0,97.9072D0,101.07D0,102.9055D0,
     $ 106.42D0,
     8 107.8682D0,112.411D0,114.818D0,118.710D0, 121.760D0,127.6D0,
     9 126.90447D0,131.293D0,132.90545D0,137.327D0,138.9055D0,140.116D0,
     $ 140.90765D0,144.9127D0,145.D0,150.36D0,151.964D0,157.25D0,
     $ 158.92534D0,
     $ 162.50D0,164.93032D0,167.259D0,168.93421D0,173.04D0,174.967D0,
     $ 178.49D0,180.9479D0,183.84D0,186.207D0,190.23D0,192.217D0,
     $ 195.078D0,196.96655D0,200.59D0,204.3833D0,207.2D0,208.98038D0,
     $ 208.9824D0, 209.9871D0,
     $ 222.0176D0,223.0197D0,226.0254D0,227.0278D0,232.0381D0,
     $ 231.03588D0,238.02891D0,237.0482D0,244.0642D0,243.0614D0,
     $ 247.0703D0,247.0703D0,251.0587D0/
C
      DATA AG/2.,2.,2.,4., 2., 4., 2.,12., 30., 40., 30., 12., 2.,4.,2.,
     1  12.,30.,40.,30.,12.,2.,4.,-1170.,-3780.,-8100.,-12096.,-12852.,
     2 -9720.,-5130.,-1800.,2.,4.,2.,12.,30.,40.,30.,12.,2.,4.,-1170.,
     3 -3780.,-8100.,-12096.,-12852.,-9720.,-5130.,-1800.,2.,4.,2.,12.,
     4 30.,40.,30.,12.,2.,4.,-1170.,14*0.,-3780.,-8100.,-12096.,-12852.,
     5 -9720.,-5130.,-1800.,2.,4.,2.,12.,30.,40.,30.,12.,12*0. /
C
      DATA GG/2.,2.,2.,1.,8.,13.,6.,15.,20.,15.,6.,1.,18.,33.,16.,75.,
     1  170.,215.,156.,61.,42.,426.,1260.,3855.,7992.,11676.,12216.,
     2 9135.,4780.,1666.,362.,61.,30.,159.,380.,495.,366.,145.,74.,670.,
     3 1260.,3855.,7992.,11676.,12216.,9135.,4780.,1666.,394.,125.,92.,
     4 351.,860.,1135.,846.,337.,124.,1138.,2200.,14*0.,3855.,7992.,
     5 11676.,12216. ,9135.,4780.,1666.,434.,205.,132.,591.,1460.,1935.,
     6 1446.,577.,12*0/
C
      DATA EFAZ/4*'G',4*'S',4*'G',6*'S',2*'G',12*'S','S',3*'S','S','G',
     1 17*'S','G',25*'S','S',5*'S','G',12*'S'/, BLANK/'    '/
C
      DATA NMAT/0,1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     1 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     2 41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
     3 61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
     4 81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98/
C
      DATA NMLA/ 1,2,2,5*1,3*2,7*1,2,17*1,2,17*1,2,45*1/
      END

