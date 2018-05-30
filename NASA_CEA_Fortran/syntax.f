C*********************************************************************** 
C           syntax.f for 'cleaning' CEA input for GUI February 12, 2004
C*********************************************************************** 
C     program syntax for CEA GUI
C	Nov. 12, 2002 created by Bonnie McBride
C				  inFile=GUIfile.txt , outFile=GUIfile2.txt
C	Dec. 03, 2002 Max char per a line (MAXNCHAR=120) ; LOGICAL  Moles
C		
      PARAMETER (MAXNGC=600)
      PARAMETER (MAXTR=30)
	PARAMETER (MAXNCHAR=120)
C DUMMY ARGUMENTS
      LOGICAL Caseok,Readok,Moles
      CHARACTER*15 Ensert(20)
C LOCAL VARIABLES
      CHARACTER*15 cin(MAXNGC),cx15
      CHARACTER*4 Code
      CHARACTER*1 cx1
      CHARACTER*2 cx2
      CHARACTER*3 cx3
      CHARACTER*4 cx4
      CHARACTER*5 cx5
      CHARACTER*6 cx6
      CHARACTER*26 lc,uc
      CHARACTER*120 rec,tmp
      LOGICAL pltdat
      INTEGER Lcin(MAXNGC),Mcin(MAXNGC)
      REAL*8 dpin(MAXNGC)
C
      DATA uc/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA lc/'abcdefghijklmnopqrstuvwxyz'/ 
C
      OPEN (5,FILE='GUIfile.txt',STATUS='old',FORM='formatted')
	OPEN (6,FILE='GUIfile2.txt',STATUS='unknown',FORM='formatted')
      Caseok = .TRUE.
      Readok = .TRUE.
C CALL INFREE TO READ DATASET
 100  CALL INFREE(Readok,Cin,Ncin,Lcin,Mcin)
      IF ( .NOT.Readok ) GOTO 400
      Code = cin(1)
      IF ( Code.EQ.'    ' ) goto 100
c
c THE FOLLOWING 4 LINES MAY BE ACTIVATED FOR DEBUG PURPOSES
c     write (6,*)'           i     Cin(i)             Lcin(i)   Mcin(i)'
c     do i=1,Ncin
c       write (6,*) i,'     ',Cin(i),Lcin(i),Mcin(i)
c     enddo
c
	write (6, * ) '     '
      write (6,110) cin(1)
 110  FORMAT ( '     ',a15)
      if ( Code.EQ.'end' ) goto 400
      if ( Code.EQ.'only'.OR.Code.EQ.'inse'.OR.Code.EQ.'omit' ) then
        nn = MIN(MAXNGC,Ncin)
C	  Insert a blank line before each code
	  write (6, * ) '     '
        write (6,105) (cin(i),i=2,nn)
 105    format ( 5a16 )
        go to 100
      endif
      nrec = 1
      rec = ' '
c
C PROCESS 'OUTP' DATASET.
c
      IF ( Code.EQ.'outp' ) THEN
        pltdat = .FALSE.
        DO 120 i = 2,Ncin
          if ( Lcin(i).EQ.0 ) go to 120
          cx15 = cin(i)
          cx1 = cx15(:1)
          cx2 = cx15(:2)
          cx3 = cx15(:3)
          cx4 = cx15(:4)
          cx5 = cx15(:5)
          IF ( Lcin(i).LT.0 ) THEN
            if ( cx3.eq.'cal'.OR.cx5.eq.'short'.OR.cx5.eq.'massf'.OR.
     &           cx2.eq.'si' .OR.cx4.eq.'mole' ) then
              n = -Lcin(i) + 1
              if (nrec+n.ge.MAXNCHAR) call recout(nrec,rec)
              rec(nrec:nrec+n) = cx15(:n-1)//' '   
              nrec = nrec + n  
              pltdat = .FALSE.
              goto 120
            elseif ( cx4.EQ.'tran'.OR.cx3.EQ.'trn' ) THEN
              if (nrec+7.ge.MAXNCHAR) call recout(nrec,rec)
              rec(nrec:nrec+7) = 'transp '
              nrec = nrec + 7
              pltdat = .FALSE.
              goto 120
            elseif ( cx4.EQ.'trac' ) THEN
              n = -Lcin(i) + 1
              m = Mcin(i+1) + 1
              if (nrec+n+m.ge.MAXNCHAR) call recout(nrec,rec)
              rec(nrec:nrec+n+m) = cx15(:n-1)//'='//Cin(i+1)(:m-1)//' ' 
              nrec = nrec + n + m
              Lcin(i+1) = 0
              pltdat = .FALSE.
              goto 120
            elseif (cx4.eq.'plot') then
              pltdat = .true.
              n = 5
              if (nrec+n.ge.MAXNCHAR) call recout(nrec,rec)
              rec(nrec:nrec+n) = 'plot '
              nrec = nrec + n
              goto 120
            elseif ( cx3.EQ.'deb'.OR.cx3.EQ.'dbg' ) THEN
              if (nrec+6.ge.MAXNCHAR) call recout(nrec,rec)
              rec(nrec:nrec+6) = 'debug='
              nrec = nrec + 6
                DO j = i+1,i+9
                  IF ( Lcin(j).ne.i ) GOTO 120
                  n = Mcin(j) + 1
                  if (nrec+n.ge.MAXNCHAR) call recout(nrec,rec)
                  rec(nrec:nrec+n-1) = Cin(j)(:n-1)   
                  nrec = nrec +n
                    if ( Lcin(j+1).eq.i ) then
                      rec(nrec-1:nrec) = ',' 
                    else
                      rec(nrec-1:nrec) = ' ' 
                    endif
                  lcin(j) = 0
                ENDDO
              pltdat = .false.
              goto 120
            elseif ( pltdat ) then
              n = -Lcin(i) + 1
              if (nrec+n.ge.MAXNCHAR) call recout(nrec,rec)
              rec(nrec:nrec+n) = cx15(:n-1)//' '
              nrec = nrec + n
              go to 120
            else
              WRITE (8,99002) cin(i)
            endif
          ELSEIF ( Lcin(i).GT.0 ) THEN
            WRITE (8,99002) cin(i)
          ENDIF
 120    ENDDO
          goto 200
      ENDIF
C SORT AND STORE DATA FROM 'REAC' DATASET.
      IF ( Code.EQ.'reac' ) THEN
        Moles = .FALSE.
        Nreac = 0
        i = 1
 140    i = i + 1
        cx15 = cin(i)
        cx1 = cx15(:1)
        cx2 = cx15(:2)
        cx3 = cx15(:3)
        cx4 = cx15(:4)
        cx5 = cx15(:5)
        IF ( i.gt.Ncin.or.Lcin(i).eq.0 ) goto 200
        IF ( lcin(i).GT.0 ) THEN
		WRITE (6, * ) '     '
          WRITE (6,99003) cin(i)
          GOTO 140
        ENDIF
        IF ( cx2.EQ.'na'.OR.cx2.EQ.'ox'.OR.cx2.EQ.'fu' ) THEN
          nreac = nreac +1
          if (nrec.gt.1) call recout(nrec,rec)
          if (Lcin(i+1).gt.0) then
            Lcin(i+1) = -Lcin(i+1)
            write (6,99001)
            caseok = .false.
          endif
          n = -Lcin(i) + 1
          m = -Lcin(i+1) + 1
          if (nrec+n+m.gt.MAXNCHAR)call recout(nrec,rec)
          rec(nrec:nrec+n+m) = cx15(:n-1)//'='//Cin(i+1)(:m-1)//' '
          nrec = nrec + m + n
          Lcin(i)  = 0
          i = i + 1
          go to 140
C LOOK FOR PERCENTS
        ELSEIF ( cx1.EQ.'m'.OR.cx1.EQ.'w' ) THEN
          n = -Lcin(i) + 1
          m = Mcin(i+1) + 1
          if ( nrec+n+m.ge.MAXNCHAR ) call recout(nrec,rec)
          rec(nrec:nrec+n+m) = cx15(:n-1)//'='//Cin(i+1)(:m-1)//' '
          nrec = nrec + m + n
          Lcin(i)  = 0
          i = i + 1
          IF ( cx1.EQ.'m'.AND.Nreac.EQ.1 ) Moles = .TRUE.
          IF ( cx1.EQ.'m'.AND..NOT.Moles.OR.cx1.EQ.'w'.AND. Moles ) THEN
            Caseok = .FALSE.
            WRITE (6,99005)
          ENDIF
          GOTO 140
C LOOK FOR TEMPERATURE
        ELSEIF ( cx1.EQ.'t' ) THEN
          IF ( lcin(i+1).GT.0 ) THEN
            IF ( lcin(i).LT.1 ) THEN
              Lcin(i) = 0
              i = i + 1
              if ( nrec+5+mcin(i).gt.MAXNCHAR ) call recout(nrec,rec)
C			call recout(nrec,rec)
              rec(nrec:nrec+4) = 't,k='
              IF ( INDEX(cx15,'r').GT.0 ) rec(nrec:nrec+4) = 't,r='
              IF ( INDEX(cx15,'c').GT.0 ) rec(nrec:nrec+4) = 't,c='
              IF ( INDEX(cx15,'f').GT.0 ) rec(nrec:nrec+4) = 't,f='
              nrec = nrec + 4 
              rec(nrec:nrec+mcin(i)+1) = Cin(i)(:mcin(i))//' '
              nrec = nrec + mcin(i) + 1
            ENDIF
          ELSE
            WRITE (6,99006)
            Caseok = .FALSE.
          ENDIF
          GOTO 140
C LOOK FOR ENTHALPY
        ELSEIF ( cx1.EQ.'h'.OR.cx1.EQ.'u' ) THEN
          IF ( lcin(i+1).GT.0 ) THEN
            IF ( nrec+12+mcin(i+1).gt.MAXNCHAR) call recout(nrec,rec)
              rec(nrec:nrec+2) = cx1//','
              nrec = nrec + 2
            if ( INDEX(cin(i),'k').GT.0 ) then
              rec(nrec:nrec+1) = 'k'
              nrec = nrec + 1
            endif
            if ( INDEX(cin(i),'c').GT.0 ) then
              rec(nrec:nrec+8) = 'cal/mol='
              nrec = nrec + 8
            else
              rec(nrec:nrec+6) = 'j/mol='
              nrec = nrec + 6
            endif
            Lcin(i) = 0
            i = i + 1
            rec(nrec:nrec+mcin(i)+1) = Cin(i)(:mcin(i))//' '
            nrec = nrec + mcin(i) + 1
          ENDIF
          GOTO 140
C LOOK FOR DENSITY
        ELSEIF ( cx3.EQ.'rho'.OR.cx3.EQ.'den' ) THEN
          IF ( lcin(i+1).GT.0 ) THEN
            i = i + 1
C            if ( nrec+12+mcin(i).gt.MAXNCHAR ) call recout(nrec,rec)
		call recout(nrec,rec)
            IF ( INDEX(cx15,'kg').GT.0 ) THEN
              rec(nrec:nrec+12) = 'rho,kg/m**3='
              nrec = nrec + 12
            ELSE
              rec(nrec:nrec+9) = 'rho,g/cc='
              nrec = nrec + 9
            ENDIF
            n = mcin(i) + 1
            rec(nrec:nrec+n) = Cin(i)(:n-1)//' '
            nrec = nrec + n
          ENDIF
          GOTO 140
C CHECK FOR CHEMICAL SYMBOLS IN EXPLODED FORMULA
        ELSEIF ( (lcin(i).EQ.-1.OR.lcin(i).EQ.-2).AND.INDEX(uc,cx1)
     &               .GT.0 ) THEN
          IF ( lcin(i).EQ.-2 ) THEN
            ix = INDEX(lc,cx2(2:2))
            IF ( ix.GT.0 ) cx2(2:2) = uc(ix:ix)
          ENDIF
          n=  -Lcin(i) + 1
          m=  mcin(i+1) + 1
          if (nrec+n+m.gt.MAXNCHAR) call recout(nrec,rec)
          IF ( lcin(i+1).NE.i ) THEN
            rec(nrec:nrec+n+2) = cx2(:-Lcin(i))//' 1 '
            nrec = nrec + n + 2
          ELSE
            rec(nrec:nrec+n+m) = cx15(:n-1)//' '//Cin(i+1)(:m-1)//' '
            nrec = nrec + n + m
            i = i + 1
          ENDIF
          GOTO 140
        ENDIF
        WRITE (6,99007) cin(i)
      ENDIF
C SORT AND STORE INPUT FROM 'PROB' DATASET
      IF ( Code.EQ.'prob' ) THEN
C PROCESS LITERAL VARIABLES IN 'PROB' DATASET 
        i = 1
 160    i = i + 1
        cx15 = cin(i)
        cx1 = cx15(:1)
        cx2 = cx15(:2)
        cx3 = cx15(:3)
        cx4 = cx15(:4)
        cx5 = cx15(:5)
        cx6 = ' '
        if ( i.gt.Ncin ) goto 200
        IF ( Lcin(i).LT.0.AND.(i.EQ.Ncin.or.Lcin(i+1).NE.i) ) THEN
          IF ( cx4.EQ.'case' ) THEN
            i = i + 1
            n = -Lcin(i) + 6
            if ( nrec+n.gt.MAXNCHAR) call recout(nrec,rec)
            rec(nrec:nrec+n) = 'case='//Cin(i)(:n-6)//' '
            nrec = nrec + n
            Lcin(i) = 0
            goto 160
          ENDIF
          if ( cx2.EQ.'pt' ) cx2 = 'tp'
          if ( cx2.EQ.'ph' ) cx2 = 'hp'
          if ( cx2.EQ.'ps' ) cx2 = 'sp'
          if ( cx2.EQ.'vs' ) cx2 = 'sv'
          if ( cx2.EQ.'vu' ) cx2 = 'uv'
          if ( cx2.EQ.'vt' ) cx2 = 'tv'
          IF ( cx2.eq.'tp'.OR.cx2.eq.'hp'.OR.cx2.eq.'sp'.OR.
     &         cx2.eq.'tv'.OR.cx2.eq.'uv'.OR.cx2.eq.'sv' ) THEN
C		newline per each Problem Type
          call recout(nrec,rec)
C            if ( nrec+3.gt.MAXNCHAR ) CALL RECOUT(nrec,rec)
            rec(nrec:nrec+3) = cx2//' '
            nrec = nrec + 3
            goto 160
          ENDIF
          IF ( cx3.eq.'fac'.OR.cx3.eq.'det'.OR.cx3.eq.'ion'.OR.
     &         cx2.eq.'eq' .OR.cx2.eq.'sh' .OR.cx3.eq.'inc'.OR.
     &         cx3.eq.'ref' ) THEN
            n = -Lcin(i)+1
            if (nrec+n.gt.MAXNCHAR) CALL RECOUT(nrec,rec)
            rec(nrec:nrec+n) = Cin(i)(:n-1)//' '
            nrec = nrec  + n
            goto 160
          ENDIF
          if ( cx3.eq.'dbg'.OR.cx3.eq.'deb' ) cx6 = 'debug'
          if ( cx2.eq.'ro'.OR.cx3.eq.'rkt' ) cx6 = 'rocket'
          if ( cx2.eq.'fr'.OR.cx2.eq.'fz' ) cx6 = 'frozen'
          IF ( cx6.ne.' ' ) THEN
            n = 7
            if (cx6.eq.'debug') n = 6
            if (nrec+n.gt.MAXNCHAR) CALL RECOUT(nrec,rec)
            rec(nrec:nrec+n) = cx6(:n-1)//' '
            nrec = nrec + n
            cx6 = ' '
            goto 160
          ENDIF
          WRITE (6,99002) cx15
          lcin(i) = 0
          goto 160
      ENDIF
C  PROCESS LITERALS WITH EQUAL SIGNS
      IF ( Lcin(i).LT.0.AND.Lcin(i+1).EQ.i ) THEN
        IF ( cx1.eq.'t'.AND.cx4.ne.'tces'.AND.cx4.ne.'trac' ) THEN
C            if ( nrec+4.gt.MAXNCHAR ) call recout(nrec,rec)
C		New line
		call recout(nrec,rec)	

            rec(nrec:nrec+4) = 't,k='
            if ( INDEX(cx15,'r').GT.0 ) rec(nrec:nrec+4) = 't,r='
            if ( INDEX(cx15,'c').GT.0 ) rec(nrec:nrec+4) = 't,c='
            if ( INDEX(cx15,'f').GT.0 ) rec(nrec:nrec+4) = 't,f='
            nrec = nrec + 4
          goto 160
          ENDIF
          IF ( (cx2.EQ.'pc'.OR.cx2.EQ.'pi').AND.INDEX(cx15(3:15),'p')
     &       .GT.0.AND.INDEX(cx15,'psi').EQ.0 ) THEN
C           if ( nrec+6.gt.MAXNCHAR ) CALL RECOUT(nrec,rec)
C		newline for pi/pe
		CALL RECOUT(nrec,rec)
           rec(nrec:nrec+6) = 'pi/pe='
           nrec = nrec + 6
           goto 160
          ENDIF
          IF ( cx1.EQ.'p'.AND.cx3.NE.'phi' ) THEN
C            if ( nrec+7.gt.MAXNCHAR ) call recout(nrec,rec)
		call recout(nrec,rec)
            cx15 = 'p,bar='
            if ( INDEX(Cin(i),'psi').NE.0 ) cx15='p,psia='
            if ( INDEX(Cin(i),'mmh').NE.0 ) cx15='p,mmhg='
            if ( INDEX(Cin(i),'atm').NE.0 ) cx15='p,atm='
            n = 7
            if ( cx15.eq.'p,atm='.OR.cx15.eq.'p,bar=' ) n= 6
            rec(nrec:nrec+n) = cx15(:n)
            nrec = nrec + n
            cx6 = ' '
            goto 160
          ENDIF
          IF ( cx3.EQ.'rho' ) THEN
C            if ( nrec+12.gt.MAXNCHAR ) call recout(nrec,rec)
		call recout(nrec,rec)
            IF ( INDEX(cx15,'kg').NE.0 ) THEN
              rec(nrec:nrec+12 ) = 'rho,kg/m**3='
              nrec = nrec + 12
            ELSE
              rec(nrec:nrec+9) = 'rho,g/cc='
              nrec = nrec + 9
            ENDIF
            goto 160
          ENDIF
          IF ( cx1.EQ.'v' ) THEN
C           if ( nrec+10.gt.MAXNCHAR ) call recout(nrec,rec)
		call recout(nrec,rec)
            IF ( INDEX(cx15,'kg').NE.0 ) THEN
              rec(nrec:nrec+10) = 'v,m**3/kg='
              nrec = nrec + 10
            ELSE
              rec(nrec:nrec+7) = 'v,cc/g='
              nrec = nrec + 7
            ENDIF
            goto 160
          ENDIF
          IF ( cx3.EQ.'nfz'.OR.cx3.EQ.'nfr' ) THEN
            if ( nrec+4.gt.MAXNCHAR ) call recout(nrec,rec)
            rec(nrec: ) = 'nfz='
            nrec = nrec + 4
            goto 160
          ENDIF
          IF ( cx3.eq.'u/r'.OR.cx2.eq.'ur'.OR.cx3.eq.'s/r'.OR.
     &         cx2.eq.'sr' .OR.cx3.eq.'h/r'.OR.cx2.eq.'hr' ) THEN
            if ( cx2.eq.'hr' ) cx3 = 'h/r'
            if ( cx2.eq.'ur' ) cx3 = 'u/r'
            if ( cx2.eq.'sr' ) cx3 = 's/r'
            if ( nrec+4.gt.MAXNCHAR ) call recout(nrec,rec)
            rec(nrec:nrec+4) = cx3//'='
            nrec = nrec + 4
            go to 160
          ENDIF
          IF ( cx2.eq.'u1' .OR.cx4.eq.'mach'.OR.cx3.eq.'sub' .OR.
     &         cx3.eq.'sup'.OR.cx3.eq.'phi' .OR.cx4.eq.'case'.OR.
     &         cx3.eq.'o/f'.OR.cx3.eq.'f/a' .OR.cx3.eq.'f/o' .OR.
     &         cx2.eq.'%f' .OR.cx1.eq.'r'   .OR.cx2.eq.'nf'  .OR.
     &         cx2.eq.'ac' .OR.cx4.eq.'tces' ) THEN
            n = -Lcin(i)+1
C		Newline
		if ( cx3.eq. 'sub' .OR. cx3 .eq. 'sup' ) call recout(nrec,rec)
            if ( nrec+n.gt.MAXNCHAR ) call recout(nrec,rec)
            rec(nrec:nrec+n) = cx15(:n-1)//'='
            nrec = nrec + n                                        
            go to 160
          ENDIF
          IF ( cx4.EQ.'mdot'.OR.cx2.eq.'ma' ) THEN
            if ( nrec+8.gt.MAXNCHAR ) call recout(nrec,rec)
            rec(nrec:nrec+8) = 'mdot/ac='
            nrec = nrec + 8                                        
            go to 160
          ENDIF
        ENDIF
C  ADD NUMBERS AS LITERALS 
        IF ( Lcin(i).GT.0 ) THEN
          n = Mcin(i) + 1
          if ( nrec+n.gt.MAXNCHAR ) call recout(nrec,rec)
          IF ( Lcin(i).eq.Lcin(i+1) ) THEN
            rec(nrec:nrec+n) = cx15(:n-1)//','
          ELSE
            rec(nrec: ) = cx15(:n-1)//' '
          ENDIF
          nrec = nrec + n
          GOTO 160
        ENDIF
        WRITE (6,99002) cx15
      ENDIF
 200  CALL RECOUT(nrec,rec)
      GOTO 100
 400  STOP
99001 FORMAT (' FUEL NAMES MUST NOT START WITH A NUMERIC' ) 
99002 FORMAT (' WARNING!!  DID NOT RECOGNIZE ',A15) 
99003 FORMAT (' WARNING!!  LITERAL EXPECTED FOR ',A15) 
99004 FORMAT (' REACTANT AMOUNT MISSING (INPUT)') 
99005 FORMAT (' MOLES AND WEIGHT PERCENTS SHOULD NOT BE MIXED') 
99006 FORMAT (' REACTANT TEMPERATURE MISSING') 
99007 FORMAT (' WARNING!! ',A15,' NOT RECOGNIZED')
      END
      SUBROUTINE INFREE(Readok,Cin,Ncin,Lcin,Mcin) 
C*********************************************************************** 
C FREE-FORM READ FOR CEA.  READS AND DECIPHERS DATA FOR ONE DATASET.
C
C DEFINITIONS:
C   CH1  - INDIVIDUAL CHARACTERS IN RECORD, MAXIMUM 132.
C   NCH1 - COLUMN NUMBER FOR THE LAST NON-BLANK CHARACTER IN RECORD.
C   NCIN - NUMBER OF VARIABLES IN DATASET.
C   CIN  - CHARACTER STRINGS IN DATASET. MAXIMUM 15 CHARACTERS.
C   LCIN - NEG. LENGTH OF LITERALS.  FOR NUMERICS, INDEX OF PREVIOUS 
C          LITERAL.  ZERO FOR UNACCEPTIBLE VARIABLES.  VARIABLE 
C          FOLLOWING "CASE" IS ALWAYS ASSUMED TO BE LITERAL.
C   NB   - NUMBER OF DELIMITERS IN STRING.
C   NX   - NUMBER OF CHARACTERS IN STRING.
C   Mcin - Number of characters in numeric string.
C   CNUM - CHARACTER STRING REPRESENTING DATASET NUMBERS. MAXIMUM 24 
C          CHARACTERS.
C***********************************************************************
      PARAMETER (MAXNGC=600)
C DUMMY ARGUMENTS
      CHARACTER*15 Cin(MAXNGC)
      INTEGER Ncin
      INTEGER Lcin(MAXNGC),Mcin(MAXNGC)
      LOGICAL Readok
C LOCAL VARIABLES
      CHARACTER*1 ch1(132),cx,nums(13)
      CHARACTER*24 cnum
      CHARACTER*3 fmtl(3)
      CHARACTER*4 w1
C
      DATA nums/'+','-','0','1','2','3','4','5','6','7','8','9','.'/
      Ncin = 1
      Lcin(1) = 0
      kcin = 0
      Mcin(1) = 0
 100  nb = 1
      nx = 0
      cnum = ' '
      Cin(Ncin) = ' '
      ch1(1) = ' '
      nch1 = 1
C READ CHARACTERS, ONE AT A TIME
      READ (5,99001,END=500,ERR=500) ch1 
C FIND FIRST AND LAST NON-BLANK CHARACTER
      DO i = 132,1, - 1
        nch1 = i
        IF ( ch1(i).NE.' '.AND.ch1(i).NE.'	' ) GOTO 200
      ENDDO
 200  DO i = 1,nch1
        ich1 = i
        IF ( ch1(i).NE.' '.AND.ch1(i).NE.'	' ) GOTO 300
      ENDDO
 300  IF ( nch1.EQ.1.OR.ch1(ich1).EQ.'#'.OR.ch1(ich1).EQ.'!' ) GOTO 100
      w1 = ch1(ich1)//ch1(ich1+1)//ch1(ich1+2)//ch1(ich1+3) 
C IS STRING A KEYWORD SIGNALLING START OR END OF DATASET?
      IF ( w1.EQ.'ther'.OR.w1.EQ.'tran'.OR.w1.EQ.'prob'.OR.
     &     w1.EQ.'reac'.OR.w1.EQ.'outp'.OR.w1.EQ.'omit'.OR.
     &     w1.EQ.'only'.OR.w1.EQ.'inse'.OR.w1(1:3).EQ.'end' ) THEN
        IF ( Ncin.EQ.1 ) THEN
          Cin(Ncin) = w1
          IF ( w1(1:3).EQ.'end'.OR.w1.EQ.'ther'.OR.w1.EQ.'tran' ) THEN 
c           WRITE (6,99002) (ch1(i),i=1,nch1)
            RETURN
          ENDIF
          ich1 = ich1 + 4
          nx = 4
          Lcin(1) = -4
        ELSE
C KEYWORD READ FOR NEXT DATASET. END PROCESSING
          BACKSPACE 5
          IF ( nx.EQ.0 ) Ncin = Ncin - 1
          RETURN
        ENDIF
      ELSEIF ( Ncin.EQ.1 ) THEN
        WRITE (6,99003)
        GOTO 500
      ENDIF
c     WRITE (8,99002) (ch1(i),i=1,nch1)
      DO i = ich1,nch1
        cx = ch1(i)
C LOOK FOR DELIMITER STRINGS
        IF ( cx.EQ.','.AND.(Lcin(Ncin).GT.0.OR.nx.EQ.0) ) cx = ' '
        IF ( cx.EQ.'='.AND.(Lcin(Ncin).LT.0.OR.nx.EQ.0) ) cx = ' '
        IF ( cx.NE.' '.AND.cx.NE.'	' ) THEN 
C LOOK FOR CHARACTER STRINGS
          nx = nx + 1
          IF ( Ncin.GT.1 ) THEN
            cnum(nx:nx) = cx
            IF ( nx.LE.15 ) Cin(Ncin) = cnum
            IF ( nx.EQ.1 ) THEN
C IS THIS A NUMERIC?
              DO j = 1,13
                IF ( ch1(i).EQ.nums(j) ) THEN
                  Lcin(Ncin) = kcin
                  Mcin(Ncin) = 1
                  GOTO 310
                ENDIF
              ENDDO
              Lcin(Ncin) = -1
              kcin = Ncin
            ELSEIF ( Lcin(Ncin).LT.0 ) THEN
              Lcin(Ncin) = -nx
            ENDIF
 310        nb = 1
          ENDIF
          IF ( i.LT.nch1.OR.Lcin(Ncin).LT.0 ) GOTO 400
        ENDIF
        IF ( nb.EQ.1..AND.nx.GT.0 ) THEN
          IF ( Ncin.GT.0.AND.Lcin(Ncin).GT.0 ) THEN 
C  NUMERIC CHARACTER STRINGS 
          Mcin(Ncin) = nx
          ENDIF
 340      Ncin = Ncin + 1
          Cin(Ncin) = ' '
          Lcin(Ncin) = 0
          Mcin(Ncin) = 0
          nx = 0
          cnum = ' '
        ENDIF
        nb = nb + 1
 400  ENDDO
      IF ( nx.GT.0 ) THEN
        Ncin = Ncin + 1
        Lcin(Ncin) = 0
        Mcin(Ncin) = 0
      ENDIF
      GOTO 100
 500  Readok = .FALSE.
      RETURN
99001 FORMAT (132A1)
99002 FORMAT (1x,120A1)
99003 FORMAT (/' FATAL ERROR IN INPUT FORMAT (INFREE)') 
99004 FORMAT (/' WARNING!!  UNACCEPTABLE NUMBER ',A15,' (INFREE)')
      END
      SUBROUTINE RECOUT(nrec,rec)
c*********************************************************************** 
c  Routine to write a record.
c***********************************************************************
      CHARACTER*120 rec
      write (6,10) rec
 10   FORMAT ( A120 )
      nrec = 1
      rec = ' '
      RETURN
      END
