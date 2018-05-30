

      PROGRAM b1b2b3
C 
C												February 12, 2004
C  1. Find atomic symbols for reactant species.
C  2. Find species ID lists for B1, B2, and B3.
C
C  Input consists of one record for each reactant species ID 
C    plus a ENDR record followed by one record of atomic
C      symbols. - 12/18/2001 Using atomTemp a72
C    The reactant species ID's must start in column 2.
C    The symbols must be:
C       1) All capital letters
C       2) Start in column 2
C       3) In one line (record)
C       4) Left-adjusted in field-widths of 3 characters 
C          eg.  " C  H  V  MG "  (quotes not part of input)
C  When trying to find reactant species ID's, the first record is
C    blank and is followed by the atomic symbol list.
C  When trying to find omit and only lists, any species ID's from
C    the reactants are listed followed by a blank.  Then if there
C    are any atoms from user defined reactants, those atomic symbols
C    are listed in the final record. The final record will be blank
C    if there are no user defined reactants.
C  Output lists appear in files B1, B2, and B3 for gases, condensed
C    and reactant-only species respectively.
C    
      PARAMETER (IOTHM=14,iob1=10,iob2=11,iob3=12)
C
      DOUBLE PRECISION b(5),t1,t2,mw,cft(9,3),Tg(4)
C
      CHARACTER el(5)*2,date*6,sub*15,Thdate*10,sub0*15,atoms(24)*3
      CHARACTER Rids(24)*15, atomTemp*72
C
      OPEN (5,FILE='input.txt',STATUS='old',FORM='formatted')
      OPEN (IOTHM,FILE='thermo.lib',STATUS='old',FORM='unformatted')
      OPEN (iob1,FILE='B1.txt',STATUS='unknown',FORM='formatted')
      OPEN (iob2,FILE='B2.txt',STATUS='unknown',FORM='formatted')
      OPEN (iob3,FILE='B3.txt',STATUS='unknown',FORM='formatted')
C
      WRITE (iob1,20) ' '
      WRITE (iob2,20) ' '
      WRITE (iob3,20) ' '
      REWIND iob1
      REWIND iob2
      REWIND iob3
      do 12 i=1,24
      Atoms(i)=' '
      Rids(i) = ' '
12    continue
      Natom = 0
      Nrid = 0
C
C  Read reactant names (Rids)--blank if none.
C


	DO 25 i=1,24
      READ (5,15) Rids(i)
15    FORMAT (1X, a )
	print *, Rids(i)
        IF (Rids(i)(1:4).EQ.'ENDR') THEN
          READ (5,20) atomTemp
20        FORMAT (1X, a72 )
          print *, atomTemp
		GOTO 30
	  ENDIF
        Nrid = Nrid + 1
	
25    CONTINUE

30	CONTINUE
      n= 1

      DO 40 K=1,24

        Atoms(K) = atomTemp(n:n+2)
	  Natom = Natom + 1
	  if ( Atoms(K) .eq. '   ' ) GOTO 60
        n=n+3
      
40	CONTINUE  
C
C  Read atomic symbols (Atoms)--3 blank Char '   ' if none.
C


60	continue
      
C
C  SEARCH thermo.lib SPECIES NAMES (sub)
C
C   SOME DEFINITIONS:
C     ntgas = NUMBER OF GASEOUS SPECIES IN thermo.lib.
C     ntot =  NTGAS PLUS NUMBER OF TEMPERATURE INTERVALS FOR CONDENSED.
C     nall =  NTOT PLUS THE NUMBER OF REACTANT SPECIES IN THERMO.LIB.
C     Ng =    NUMBER OF GASES WITH STORED COEFFICIENTS.
C     Nc =    NUMBER OF CONDENSED INTERVALS WITH STORED COEFFICIENTS.
C     Ngc =    Ng + Nc
C     Thdate = DATE OF LAST thermo.lib UPDATE.
C
100   IF (Nrid .EQ. 0)  goto 210
C
C  Find atomic symbols for reactant species in thermo.lib
C
110   REWIND IOTHM
      READ (IOTHM) Tg,ntgas,ntot,nall,Thdate
      sub0 = ' '
C   BEGIN LOOP FOR READING SPECIES DATA FROM THERMO.LIB.
      Iid = 0
      DO 200 itot = 1,nall
      IF (itot.le.ntot) THEN
        icf=3
        io = iob1
        if ( itot.GT.ntgas ) then
          icf=1
          io = iob2
        endif
          READ (IOTHM) sub,nint,date,(el(j),b(j),j=1,5),Ifaz,t1,t2,
     &                 mw,((cft(i,j),i=1,9),j=1,icf)
      ELSE
        io = iob3
        READ (IOTHM) sub,nint,date,(el(j),b(j),j=1,5),ifaz,t1,t2,mw,eform
        IF (nint.GT.0) READ (IOTHM) ((cft(i,j),i=1,9),j=1,nint)
      ENDIF
C
      IF ( sub.EQ.sub0 ) go to 200
      sub0 = sub
      IF (sub(1:1).EQ.'*') sub = sub(2:)
      DO 114 i=1,Nrid
        if ( Rids(i).EQ.sub) goto 116
114   CONTINUE
      goto 200
116   Iid = Iid + 1
      print *, sub
          DO 140 k = 1,5
            if ( b(k).EQ.0. ) GOTO 140
            IF ( Natom.EQ.0 ) THEN
              NAtom = Natom +1
              Atoms(Natom) = el(k)
              goto 140
            ELSE
              Nd = Natom
              DO 120 i = 1,Nd
                if ( Atoms(i).EQ.el(k) ) GOTO 140
                Natom = Natom + 1
                Atoms(Natom) = el(k)
                goto 140
120           CONTINUE
            ENDIF
140       CONTINUE
          IF ( Iid.GE.Nrid ) goto 210
200   CONTINUE
C
C  Find species ID's for B1, B2, and B3 for atomic symbols (Atoms).
C
210   REWIND IOTHM
      READ (IOTHM) Tg,ntgas,ntot,nall,Thdate
      sub0 = ' '
C   BEGIN LOOP FOR READING SPECIES DATA FROM THERMO.LIB.
      DO 400 itot = 1,nall
      IF (itot.le.ntot) THEN
        icf=3
        io = iob1
        if ( itot.GT.ntgas ) then
          icf=1
          io = iob2
        endif
          READ (IOTHM) sub,nint,date,(el(j),b(j),j=1,5),Ifaz,t1,t2,
     &                 mw,((cft(i,j),i=1,9),j=1,icf)
      ELSE
        io = iob3
        READ (IOTHM) sub,nint,date,(el(j),b(j),j=1,5),ifaz,t1,t2,mw,eform
        IF (nint.GT.0) READ (IOTHM) ((cft(i,j),i=1,9),j=1,nint)
      ENDIF
C
      IF ( sub.EQ.sub0 ) go to 400
      sub0 = sub
      IF (sub(1:1).EQ.'*') sub = sub(2:)
          DO 300 k = 1,5
            DO 260 i = 1,Natom
            if ( b(k).EQ.0. ) GOTO 350
            if ( Atoms(i).EQ.el(k) ) GOTO 300
260         CONTINUE
            GOTO 400
300       CONTINUE
C
350   WRITE (io,355) sub
355   FORMAT ( a15 )
400   CONTINUE
C
500   CLOSE (IOTHM)
      CLOSE ( 5)
      CLOSE (iob1)
      CLOSE (iob2)
      CLOSE (iob3)
      STOP
      END