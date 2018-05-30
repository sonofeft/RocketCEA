      PROGRAM MAKELIB
C***********************************************************************
C READ THERMO DATA FROM I/O UNIT 5 IN ASCII FORMAT AND WRITE
C UNFORMATTED ON I/O UNIT 6.  DATA ARE REORDERED GASES FIRST. A
C LIST OF NAMES AS PROCESSED WILL BE LISTED ON I/O UNIT 8 (names.lst).
C INPUT DATA FORMAT IS DESCRIBED ON PAGE 43 OF NASA/TP-2002-211556.
c These data may be obtained interactively from NASA Thermobuild at
c   the CEA website: http://www.grc.nasa.gov/WWW/CEAWeb/
c
c Makelib makes an unformatted library of data requires by NASA
c computer programs CEA, CEA2, and CECALL.  2/14/2003 (B.McBride)
c Added CHARACTER*10 Thdate, replaced lerc.nasa by grc.nasa 11/17/2005
C
c Input in order:
c   1) Optional initial comment records ('!' or '#' in column 1).
c   2) Record should contain the word 'thermo' starting in column 1.
c   3) Record with 4 temperatures indicating standard intervals for
c      gases followed by date of last update.
c   4) Coefficient data sets as described in NASA/TP-2002-211556
c      by McBride, Zehe, and Gordon.
c   ...
c These data may be obtained interactively from NASA Thermobuild at
c   the CEA website: http://www.grc.nasa.gov/WWW/CEAWeb/
C
C I/O UNITS:
C UNIT 13 - scratch
C UNIT 8 - list of names, date codes, Hf298, and notes from input.
C
C GASEOUS SPECIES:
C THE STANDARD TEMPERATURE RANGES TGL ARE GIVEN ON THE FIRST
C RECORD, FOLLOWED BY THE DATE OF THE LAST DATA CHANGE THDATE.
C
C WHEN COEFFICIENTS ARE NOT GIVEN FOR THE THIRD TEMPERATURE
C INTERVAL, A STRAIGHT LINE FOR CP/R IS USED.  FOR HIGH TEMPS,
C THE EXTRAPOLATION GOES BETWEEN THE LAST POINT GIVEN AND THE
C FOLLOWING VALUES AA AT TINF=1.D06 K:
C      MONATOMICS  2.5
C      DIATOMICS   4.5
C      POLYATOMICS 3*N-1.75  (AVERAGE 1.5 AND 2)
C
C THE FOLLOWING EXTRAPOLATION IS NOT CURRENTLY PROGRAMED (12/9/98):
C   FOR LOW TEMPS, THE EXTRAPOLATION GOES BETWEEN THE FIRST VALUE
C   DOWN TO THE FOLLOWING VALUES AA AT 0 K:
C      MONATOMICS  2.5
C      DIATOMICS   3.5
C      POLYATOMICS 3.75 (AVERAGE 3.5 AND 4.0)
C
C IF DATA ARE AVAILABLE FOR THE THIRD T INTERVAL, IFAZ (SEE
C DEFINITION) IS SET TO -1 AND THE NAME IS ALTERED TO START WITH *.
C
C CONDENSED SPECIES:
C NO EXTRAPOLATIONS ARE DONE.  TEMP INTERVALS VARY.
C
C SOME DEFINITIONS:
C TGL(I)  - TEMPERATURE INTERVALS FOR GASES (I.E. 200,1000,6000,20000).
C FILL(I) - IF TRUE, DATA MISSING FOR INTERVAL.  CURRENTLY ONLY 3RD
C           INTERVAL CHECKED.
C NGL     - NUMBER OF GASEOUS PRODUCTS.
C NS      - NGL + NUMBER OF CONDENSED PRODUCT PHASES.
C NALL    - NS + NUMBER OF REACTANT SPECIES.
C IFAZ    - PHASE INDICATOR. GASES ARE 0, CONDENSED PHASES ARE NUMBERED
C           STARTING WITH 1 FOR THE LOWEST T RANGE, 2 FOR THE NEXT
C           CONTIGUOUS PHASE, ETC.
C NTL     - NUMBER OF T INTERVALS FOR A SPECIES SET.
C***********************************************************************
      CHARACTER*15 name
      CHARACTER*16 namee,record*80,ans*1
      CHARACTER*65 notes
      CHARACTER*2 sym(5)
      CHARACTER*6 date
	CHARACTER*10 Thdate
      INTEGER i,ifaz,ifzm1,inew,int,j,k,kk,l,nall,ncoef,ngl,ns,ntl
      INTEGER INDEX
      LOGICAL fill(3)
      REAL*8 aa,atms,cpfix,dlt,expn(8),fno(5),hform,hh,mwt,templ(9),tex,
     &       tgl(4),thermo(9,3),tinf,tl(2),ttl,tx
      REAL*8 DBLE,DLOG
C
      OPEN (13,STATUS='scratch',FORM='unformatted')
      OPEN (8,FILE='names.lst',STATUS='unknown',FORM='formatted')
C
c  Read and skip comments and 'thermo' records
c
 10   READ (5,15,err=400) record
 15   FORMAT (a80)
      IF ( record(:6).NE.'thermo' ) GOTO 10
C
      ngl = 0
      ns = 0
      nall = 0
      ifzm1 = 0
      inew = 0
      tinf = 1.D06
      READ (5,99001) tgl,Thdate
 100  DO i = 1,3
        fill(i) = .TRUE.
        DO j = 1,9
          thermo(j,i) = 0.
        ENDDO
      ENDDO
      hform = 0.
      tl(1) = 0.
      tl(2) = 0.
      READ (5,99002,END=300,ERR=400) name,notes
      IF ( name(:3).EQ.'END'.OR.name(:3).EQ.'end' ) THEN
        IF ( INDEX(name,'ROD').EQ.0.AND.INDEX(name,'rod').EQ.0 )
     &       GOTO 300
        ns = nall
        GOTO 100
      ENDIF
      READ (5,99003,ERR=400) ntl,date,(sym(j),fno(j),j=1,5),ifaz,mwt,
     &                       hform
      WRITE (8,99004) name,date,hform,notes
C IF NTL=0, REACTANT WITHOUT COEFFICIENTS
      IF ( ntl.EQ.0 ) THEN
        IF ( ns.EQ.0 ) GOTO 300
        nall = nall + 1
        READ (5,99005,ERR=400) tl,ncoef,expn,hh
        thermo(1,1) = hform
        WRITE (13) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                thermo
        GOTO 100
      ELSEIF ( name.EQ.'Air' ) THEN
        sym(1) = 'N'
        fno(1) = 1.56168D0
        sym(2) = 'O'
        fno(2) = .419590D0
        sym(3) = 'AR'
        fno(3) = .009365D0
        sym(4) = 'C'
        fno(4) = .000319D0
      ELSEIF ( name.EQ.'e-' ) THEN
        mwt = 5.48579903D-04
      ENDIF
      DO i = 1,ntl
        READ (5,99005,ERR=400) tl,ncoef,expn,hh
        READ (5,99006,ERR=400) templ
        IF ( ifaz.EQ.0.AND.i.GT.3 ) GOTO 400
        IF ( ifaz.LE.0 ) THEN
          IF ( tl(2).GT.tgl(4)-.01D0 ) THEN
            ifaz = -1
            namee = '*'//name
            name = namee(:15)
          ENDIF
          IF ( tl(1).GE.tgl(i+1) ) GOTO 200
          int = i
          fill(i) = .FALSE.
        ELSE
          int = 1
          IF ( i.GT.1 ) THEN
            DO k = 1,7
              thermo(k,1) = 0.D0
            ENDDO
          ENDIF
        ENDIF
        DO l = 1,ncoef
          DO k = 1,7
            IF ( expn(l).EQ.DBLE(k-3) ) THEN
              thermo(k,int) = templ(l)
              GOTO 150
            ENDIF
          ENDDO
 150    ENDDO
        thermo(8,int) = templ(8)
        thermo(9,int) = templ(9)
        IF ( ifaz.GT.0 ) THEN
          nall = nall + 1
          IF ( ifaz.GT.ifzm1 ) THEN
            inew = inew + 1
          ELSE
            inew = i
          ENDIF
          WRITE (13) name,ntl,date,(sym(j),fno(j),j=1,5),inew,tl,mwt,
     &                  thermo
        ENDIF
 200  ENDDO
      ifzm1 = ifaz
      IF ( ifaz.LE.0 ) THEN
        inew = 0
        nall = nall + 1
        IF ( ifaz.LE.0.AND.ns.EQ.0 ) THEN
          ngl = ngl + 1
          IF ( fill(3) ) THEN
            atms = 0.
            DO i = 1,5
              IF ( sym(i).EQ.' '.OR.sym(i).EQ.'E' ) GOTO 210
              atms = atms + fno(i)
            ENDDO
C FOR GASES WITH NO COEFFICIENTS FOR TGL(3)-TGL(4) INTERVAL,
C CALCULATE ESTIMATED COEFFICIENTS. (STRAIGHT LINE FOR CP/R)
 210        aa = 2.5D0
            IF ( atms.GT.1.9 ) aa = 4.5D0
            IF ( atms.GT.2.1 ) aa = 3.*atms - 1.75D0
            ttl = tl(2)
            tx = ttl - tinf
            cpfix = 0
            templ(8) = 0.
            templ(9) = 0.
            dlt = DLOG(ttl)
            DO k = 7,1, - 1
              kk = k - 3
              IF ( kk.EQ.0 ) THEN
                cpfix = cpfix + thermo(k,2)
                templ(8) = templ(8) + thermo(k,2)
                templ(9) = templ(9) + thermo(k,2)*dlt
              ELSE
                tex = ttl**kk
                cpfix = cpfix + thermo(k,2)*tex
                templ(9) = templ(9) + thermo(k,2)*tex/kk
                IF ( kk.EQ.-1 ) THEN
                  templ(8) = templ(8) + thermo(k,2)*dlt/ttl
                ELSE
                  templ(8) = templ(8) + thermo(k,2)*tex/(kk+1)
                ENDIF
              ENDIF
            ENDDO
            templ(2) = (cpfix-aa)/tx
            thermo(4,3) = templ(2)
            templ(1) = cpfix - ttl*templ(2)
            thermo(3,3) = templ(1)
            thermo(8,3) = thermo(8,2)
     &                    + ttl*(templ(8)-templ(1)-.5*templ(2)*ttl)
            thermo(9,3) = -templ(1)*dlt + thermo(9,2) + templ(9)
     &                    - templ(2)*ttl
          ENDIF
        ENDIF
C WRITE COEFFICIENTS ON SCRATCH I/O UNIT 13
        WRITE (13) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                thermo
      ENDIF
      GOTO 100
C END OF DATA. COPY CONDENSED & REACTANT DATA FROM 13 & ADD TO 6.
 300  REWIND 13
      IF ( ns.EQ.0 ) ns = nall
      WRITE (6) tgl,ngl,ns,nall,Thdate
C WRITE GASEOUS PRODUCTS ON 6
      IF ( ngl.NE.0 ) THEN
        DO i = 1,ns
          READ (13) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                 thermo
          IF ( ifaz.LE.0 ) WRITE (6) name,ntl,date,
     &                            (sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                            thermo
        ENDDO
      ENDIF
      IF ( ngl.NE.nall ) THEN
C WRITE CONDENSED PRODUCTS AND REACTANTS ON 6
        REWIND 13
        DO i = 1,nall
          READ (13) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                 thermo
          IF ( i.GT.ns ) THEN
            WRITE (6) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,
     &                    mwt,thermo(1,1)
            IF ( ntl.GT.0 ) WRITE (6) thermo
          ELSEIF ( ifaz.GT.0 ) THEN
            WRITE (6) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,
     &                    mwt,(thermo(k,1),k=1,9)
          ENDIF
        ENDDO
      ENDIF
      GOTO 1000
 400  WRITE (8,99007) name
 1000 STOP
99001 FORMAT (4F10.3,a10)
99002 FORMAT (a15,a65)
99003 FORMAT (i2,1x,a6,1x,5(a2,f6.2),i2,f13.5,f15.3)
99004 FORMAT (' ',a15,2x,a6,e15.6,2x,a65)
99005 FORMAT (2F11.3,i1,8F5.1,2x,f15.3)
99006 FORMAT (5D16.8/2D16.8,16x,2D16.8)
99007 FORMAT (/' ERROR IN PROCESSING thermo.inp AT OR NEAR ',A15,
     &        ' (UTHERM)')
      END
