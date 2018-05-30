C***********************************************************************
C                     P R O G R A M      C E A 2
C
C             CHEMICAL EQULIBRIUM WITH APPLICATIONS         5/21/04
C***********************************************************************
C 11/01/94 - CORRECTION IN EQLBRM  TEST FOR CONVERGENCE ON CONDENSED
C            CHANGED FROM: DABS(DELN(J))<.5D-05
C                      TO: DABS(DELN(J)/TOTN(NPT))<.5D-05
C 06/03/96 - INFREE CHANGED TO BE MORE COMPATIABLE WITH VARIOUS FORTRAN
C            COMPILERS AND TO ACCEPT NUMERICS WITH UP TO 24 CHARACTERS.
C 07/05/96 - CORRECTIONS MADE FOR COMPATIBILITY WITH OTHER COMPILERS.
C 08/29/96 - CORRECTION IN EQLBRM TO CORRECT FOR CONDENSED SPECIES
C            TEST AT TEMPERATURE INTERVAL OVERLAP POINTS.
C 09/17/96 - ADD 2 MORE VARIABLES FOR PLOTING, THE VOLUME PARTIAL
C            DERIVATIVES: (DLNV/DLNT)P AND (DLNV/DLNP)T AS DEFINED
C            IN EQUATIONS (2.50) AND (2.51) IN RP-1311,PT1. THE PLOT
C            LIST VARIABLES IN DATASET 'OUTPUT' HAVE EMBEDDED STRINGS
C            'DLNT' AND 'DLNP' RESPECTIVELY (E.G. 'DLNV/DLNT' OR
C            '(DLNV/DLNP)T'
C 10/18/96 - CORRECT INFREE SO THAT NUMERICAL INPUT ARRAYS MAY BE SPLIT
C            FROM RECORD TO RECORD.
C 12/11/96 - INPUT: ELIMINATE 'PSI' POSSIBILITY IN PCP CHECK.
C 12/12/96 - INPUT: WRITE MESSAGE WHEN VALUES OF P OR T ARE MISSING.
C 01/31/97 - MAIN: REVERSE ORDER FOR INSERTING CONDENSED SO THAT HIGHEST
C            T INTERVAL IS INSERTED.
C 01/31/97 - EQLBRM: CHECK ON INITIAL T INTERVALS FOR NON-TP PROBLEMS
C            WHEN A T ESTIMATE IS GIVEN. IF NO T EST., USE 3800K.
C 01/31/97 - THERMP: REMOVE INITIAL ESTIMATE OF 3800K WHEN NO T IS GIVEN
C 03/18/97 - INPUT: INSURE THAT INPUT VARIABLES P,T,MACH,U1,V,RHO,SUBAR,
C            SUPAR,O/F,AND PCP DO NOT EXCEED STORAGE.  WRITE MESSAGE IF
C            TOO MANY VALUES ARE LISTED IN THE INPUT FILE.
C 03/18/97 - SEARCH: IFZ(NC+1) SET TO 0. CAUSED PROBLEMS WITH CONDENSED.
C 03/20/97 - MAIN: CHECK FOR 100%FUEL WHEN OXIDANT IS GIVEN AND OMITTING
C            THE OXIDANT CHANGES THE CHEMICAL SYSTEM. FATAL ERROR MESSAGE.
C 03/21/97 - MAIN: FOR NON-TP CASES AND FIRST POINT AND NO T ESTIMATE IS
C            GIVEN, AND CONDENSED SPECIES ARE INSERTED, ESTIMATE T TO BE
C            THE LOWEST T MAXIMUM OF THE INSERTED SPECIES MINUS 10K..
C 04/22/97 - HCALC AND DETON: CEA WAS ALLOWING ALLOWING EXECUTION OF
C            DETON PROBLEMS WITH CONDENSED REACTANTS.
C 04/28/97 - EQLBRM: CORRECT PHASES OF CONDENSED PRODUCTS WERE NOT BEING
C            CHECKED AT THE BEGINING OF THE ROUTINE FOR TP PROBLEMS.
C 08/11/97 - SEARCH: PRINT MESSAGE FOR EXCEEDING MAXNGC,MAXNG, OR MAXNC.
C 09/04/97 - MAIN,SEARCH: CHECK TO SEE IF PRODUCTS CONTAIN ALL ELEMENTS.
C 10/09/97 - REACT,HCALC: CHANGE ERROR MESSAGE FOR REACTANT DATA NOT
C            FOUND IN THERMO.LIB.
C 12/22/97 - INTRODUCE VARIABLE ARRAY SIZES FOR INPUT VARIABLES P,T,V,
C            (CORRECTED AND REACTANT MIXTURES. THESE MAXIMUM VALUES ARE
C             IN 1/29/98)  PARAMETER STATEMENTS
C             MAXPV - MAX NUMBER OF PRESSURES OR DENSITIES (DEFAULT 26)
C             MAXT - MAX NUMBER OF TEMPERATURES (DEFAULT 51)
C             MAXMIX - MAX NUMBER OF MIXTURE VALUES (DEFAULT 52)
C 02/12/98 - UTHERM: CHECK FOR NUMBER OF T INTERVALS (NT) BEING > 3 FOR
C            GASES (IFAZ=0).  IF SO, EXIT WITH MESSAGE.
C 03/02/98 - INCREASE DIMENSIONS OF VARIABLES FOR PLOT DUMP - PLTVAR,
C            PLTOUT,MAX NPLT, AND FORMAT STATEMENT IN MAIN.
C 03/10/98 - CORRECTED 3/2/98 CHANGE.
C 05/20/98 - UTHERM & HCALC. CORRECTED 'AIR' CALCULATIONS ABOVE 6000K
C            IN HCALC BY USING COEFS FOR 1000-6000. CORRECT MOLAR AMTS
C            IN UTHERM.
C 12/01/98 - INFREE & INPUT. CHANGED ICH ARRAY.
C 02/12/99 - UTHERM & SEARCH. ADDED DATE TO THERMO DATA.
C            VARFMT. ELIMINATE "," BEFORE ")" IN FORMAT FMT.
C 02/16/99 - INFREE. USE "CTRL I" FOR TAB.
C 04/16/99 - NOTE: THIS VERSION CONTAINS SOME CLEANUP CHANGES NOT
C            INCLUDED IN PREVIOUS VERSIONS.
C 04/16/99 - UTHERM: WHEN SETTING 'AIR' COMPOSITION, REMOVE CHECK ON C
C            ALSO INSERT WEIGHT WITH MORE FIGURES FOR E-.
C 04/16/99 - THERMO.INP CHANGED SO THAT ALL COEFFICIENTS LISTED IN ORDER:
C             -2  -1 0 1 2 3 4 WHETHER ALL COEFFICIENTS EXIST OR NOT.
C 05/26/99 - NEW ATOMIC WEIGHTS IN BLOCKDATA. CARBON NOW 12.0107.
C 06/11/99 - EQLBRM: MANY SMALL CHANGES THAT DO NOT AFFECT CALCULATIONS.
C            (CORRECTED 7/16/99)
C 07/01/99 - COMMON INPT AND NEWOF: BRATIO IS REPLACED BY BCHECK WHERE
C            BCHECK = (BIGGEST B0I)*.000001 AND USED IN EQN.(3.6A),RP-1311.
C            BRATIO AS DESCRIBED IN SEC.3.2 IS USED IN NEWOF ONLY.
C 08/30/99 - CHANGE I/O UNITS FOR INPUT AND OUTPUT FROM 5 & 6 TO 7 & 8.
C 11/05/99 - REWORD '99000 FORM...' IN SEARCH & '99003 FORM...' IN REACT.
C 06/13/00 - INPUT. BYPASS TCEST VARIABLE IN T(I) LOOP. CEA WAS CONVERTING
C            VALUE FROM CENTIGRADE TO KELVIN AND ASSIGNING TEMPERATURE.
C 09/15/00 - MAIN. PRINT MESSAGE IF INSERT SPECIES IS NOT FOUND.
C 10/17/00 - CHANGE MAX NUMBER OF ELEMENTS IN A REACTANT FROM 6 TO 12.
C 01/16/01 - EQLBRM: PRINT MESSAGE IF TT DRIVEN DOWN TO < .2*TG(1).
C 01/16/01 - INPUT: HP IN PRINTOUT CORRECTED TO F WHEN UV = T.
C 04/23/01 - TEST FOR THERMO PROPERTY EXTRAPOLATION FOR PURE SPECIES:
C            ( TT.GE.TG(1)*.8D0.AND.TT.LE.TG(4)*1.1D0) WHERE TG(1)= 200K.
C 08/06/01 - UTHERM: CORRECT LABELLING FOR CONDENSED REACTANTS.
C 05/30/02 - ROCKET: In "IF ( ipp.LT.Npp ) ipp = ipp-1", change LT to LE
C 06/21/02 - UPDATED FORTRAN
C 08/19/02 - INPUT: DISCONTINUE RUN FOR ILLEGAL EQUIVALENCE RATIO.
C 09/20/02 - REACT: FOR REACTANT TEMPS OUT OF RANGE, PRINT T RANGE.
C 10/18/02 - BLOCKDATA: New atomic weights inc. N=14.0067, S=32.065,...
C  1/17/03 - cea.inc: Change MAXTR from 30 to 40 for large systems.
C  3/31/03 - Declare I/O units 7 & 8 to be IOINP & IOOUT in cea.inc  
C  4/03/03 - ROCKET: Added check for assigned ae/at < 1.
C 10/22/03 - OUT2: Plot check on p,s,and g needs check on first 2 letters.
C 11/14/03 - OUT: Add cond...fz and pran...fz to .plt for all problems.
C 11/18/03 - INPUT: If exploded formula, Energy = ' ',not 'lib'.
C 11/25/03 - DETON: Extra line for .plt data with multiple DETON calls.
C 01/30/04 - ROCKET: To correct points printed after fac, change 
C            IF(ipp.LE.Npp)ipp=ipp-1 to IF(ipp.LT.Npp.OR.Npp.EQ.4) ...
C 02/05/04 - Chg. numbered ENDDO's to CONTINUE's for Watson compiler.
C 05/21/04 - Added labels to columns in .plt file
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*15 ensert(20)
      CHARACTER*200 infile,ofile
      CHARACTER*196 prefix
      LOGICAL caseok,ex,readok
      INTEGER i,inc,iof,j,ln,n
      INTEGER INDEX
      REAL*8 xi,xln
      REAL*8 DLOG
      SAVE caseok,ensert,ex,i,inc,infile,iof,j,ln,n,ofile,prefix,readok,
     &  xi,xln
C
      WRITE (*,99001)
      READ (*,99002) prefix
      ln = INDEX(prefix,' ') - 1
      infile = prefix(1:ln)//'.inp'
      ofile = prefix(1:ln)//'.out'
      Pfile = prefix(1:ln)//'.plt'
      INQUIRE (FILE=infile,EXIST=ex)
      IF ( .NOT.ex ) THEN
        PRINT *,infile,' DOES NOT EXIST'
        GOTO 400
      ENDIF
      OPEN (IOINP,FILE=infile,STATUS='old',FORM='formatted')
      OPEN (IOOUT,FILE=ofile,STATUS='unknown',FORM='formatted')
      OPEN (IOSCH,STATUS='scratch',FORM='unformatted')
      OPEN (IOTHM,FILE='thermo.lib',FORM='unformatted')
      OPEN (IOTRN,FILE='trans.lib',FORM='unformatted')
      WRITE (IOOUT,99006)
      WRITE (IOOUT,99007)
      WRITE (IOOUT,99006)
      readok = .TRUE.
      Newr = .FALSE.
 100  Iplt = 0
      Nplt = 0
      CALL INPUT(readok,caseok,ensert)
      IF ( caseok.AND.readok ) THEN
        DO iof = 1,Nof
          IF ( Oxf(iof).EQ.0..AND.B0p(1,1).NE.0. ) THEN
            DO i = 1,Nlm
              IF ( B0p(i,1).EQ.0..OR.B0p(i,2).EQ.0. ) THEN
                WRITE (IOOUT,99008)
                GOTO 200
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        IF ( Ions ) THEN
          IF ( Elmt(Nlm).NE.'E' ) THEN
            Nlm = Nlm + 1
            Elmt(Nlm) = 'E'
            B0p(Nlm,1) = 0.
            B0p(Nlm,2) = 0.
          ENDIF
        ELSEIF ( Elmt(Nlm).EQ.'E' ) THEN
          Nlm = Nlm - 1
        ENDIF
        DO n = 1,Nreac
          Jray(n) = 0
        ENDDO
        CALL SEARCH
        IF ( Ngc.EQ.0 ) GOTO 300
        Newr = .FALSE.
        IF ( Trnspt ) CALL READTR
C INITIAL ESTIMATES
        Npr = 0
        Gonly = .TRUE.
        Enn = .1D0
        Ennl = -2.3025851
        Sumn = Enn
        xi = Ng
        IF ( xi.EQ.0. ) xi = 1.
        xi = Enn/xi
        xln = DLOG(xi)
        DO inc = 1,Nc
          j = Ng + inc
          En(j,1) = 0.D0
          Enln(j) = 0.D0
        ENDDO
        DO j = 1,Ng
          En(j,1) = xi
          Enln(j) = xln
        ENDDO
        IF ( Nc.NE.0.AND.Nsert.NE.0 ) THEN
          DO 120 i = 1,Nsert
            DO j = Ngc,Ngp1, - 1
              IF ( Prod(j).EQ.ensert(i) ) THEN
                Npr = Npr + 1
                Jcond(Npr) = j
                IF ( .NOT.Short ) WRITE (IOOUT,99003) Prod(j)
                GOTO 120
              ENDIF
            ENDDO
            WRITE (IOOUT,99004) ensert(i)
 120      CONTINUE
        ENDIF
        IF ( Rkt ) THEN
          CALL ROCKET
        ELSEIF ( Tp.OR.Hp.OR.Sp ) THEN
          CALL THERMP
        ELSEIF ( Detn ) THEN
          CALL DETON
        ELSEIF ( Shock ) THEN
          CALL SHCK
        ENDIF
        IF ( Nplt.GT.0 ) THEN
          OPEN (IOPLT,FILE=Pfile,FORM='formatted')
          WRITE (IOPLT,99009) (Pltvar(j),j=1,Nplt)
          DO i = 1,Iplt
            WRITE (IOPLT,99005) (Pltout(i,j),j=1,Nplt)
          ENDDO
          WRITE (IOPLT,99009) (Pltvar(j),j=1,Nplt)
        ENDIF
      ENDIF
 200  IF ( readok ) GOTO 100
 300  CLOSE (IOINP)
      CLOSE (IOOUT)
      CLOSE (IOSCH)
      CLOSE (IOTHM)
      CLOSE (IOTRN)
      CLOSE (IOPLT)
 400  STOP
99001 FORMAT (//' ENTER INPUT FILE NAME WITHOUT .inp EXTENSION.'/ 
     &        '   THE OUTPUT FILES FOR LISTING AND PLOTTING WILL HAVE',/
     &       ' THE SAME NAME WITH EXTENSIONS .out AND .plt RESPECTIVELY'
     &       //)
99002 FORMAT (a)
99003 FORMAT (1X,A16,'INSERTED')
99004 FORMAT (/' WARNING!!!',A16,'NOT FOUND FOR INSERTION')
99005 FORMAT (1x,1p,20E12.4)
99006 FORMAT (/' ***************************************************',
     &        '****************************')
99007 FORMAT (/,9x,'NASA-GLENN CHEMICAL EQUILIBRIUM PROGRAM CEA2,',
     &        ' MAY 21, 2004',/19x,'BY  BONNIE MCBRIDE', 
     &        ' AND SANFORD GORDON',/5x,
     &        ' REFS: NASA RP-1311, PART I, 1994',
     &        ' AND NASA RP-1311, PART II, 1996')
99008 FORMAT (/,'OXIDANT NOT PERMITTED WHEN SPECIFYING 100% FUEL(main)')
99009 FORMAT ('#',2x,20A12)
      END
      BLOCKDATA 
C***********************************************************************
C FUNDAMENTAL CONSTANTS FROM:  COHEN,E.RICHARD & TAYLOR,BARRY N.,
C THE 1986 CODATA RECOMMENDED VALUES OF THE FUNDAMENTAL PHYSICAL
C CONSTANTS, J.PHYS.CHEM.REF.DATA, VOL.17, NO.4, 1988, PP 1795-1803.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C
      DATA Rr/8314.51D0/,Pi/3.14159265D0/,Avgdr/6.0221367D0/,
     &     Boltz/1.380658D0/
C ATOMIC SYMBOLS
      DATA Symbol/'H ','D ','HE','LI','BE','B ','C ','N ','O ','F ',
     &     'NE','NA','MG','AL','SI','P ','S ','CL','AR','K ','CA','SC',
     &     'TI','V ','CR','MN','FE','CO','NI','CU','ZN','GA','GE','AS',
     &     'SE','BR','KR','RB','SR','Y ','ZR','NB','MO','TC','RU','RH',
     &     'PD','AG','CD','IN','SN','SB','TE','I ','XE','CS','BA','LA',
     &     'CE','PR','ND','PM','SM','EU','GD','TB','DY','HO','ER','TM',
     &     'YB','LU','HF','TA','W ','RE','OS','IR','PT','AU','HG','TL',
     &     'PB','BI','PO','AT','RN','FR','RA','AC','TH','PA','U ','NP',
     &     'PU','AM','CM','BK','CF','ES'/
C
C  ATOMIC WEIGHTS - Coplen,T.B., Atomic Weights of the Elements 1999. 
C     J.Phys.Chem.Ref.Data, vol.30, no.3, 2001, pp.701-712.
C
      DATA atmwt/               1.00794D0,2.014102D0,4.002602D0,6.941D0,
     1 9.012182D0,10.811D0,12.0107D0,14.0067D0,15.9994D0,18.9984032D0,
     2 20.1797D0,22.989770D0,24.305D0,26.981538D0,28.0855D0,30.973761D0,
     3 32.065D0,35.453D0,39.948D0,39.0983D0,40.078D0,44.95591D0,
     4 47.867D0, 50.9415D0,51.9961D0,54.938049D0,
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
     $ 247.0703D0,247.0703D0,251.0587D0,252.083D0/
C ATOMIC VALENCES
      DATA Valnce/1.,1.,0.,1.,2.,3.,4.,0., - 2., - 1.,0.,1.,2.,3.,4.,5.,
     &     4., - 1.,0.,1.,2.,3.,4.,5.,3.,2.,3.,2.,2.,2.,2.,3.,4.,3.,4.,
     &     - 1.,0.,1.,2.,3.,4.,5.,6.,7.,3.,3.,2.,1.,2.,3.,4.,3.,4.,
     &     - 1.,0.,1.,2.,3.,3.,3.,3.,3.,3.,3.,3.,3.,3.,3.,3.,3.,3.,3.,
     &     4.,5.,6.,7.,4.,4.,4.,3.,2.,1.,2.,3.,2., - 1.,0.,1.,2.,3.,4.,
     &     5.,6.,5.,4.,3.,3.,3.,3.,3./
C INFORMATION USED IN VARIABLE OUTPUT FORMAT
      DATA Fmt/'(1X',',A15',',','F9.','0,','F9.','0,','F9.','0,','F9.',
     &     '0,','F9.','0,','F9.','0,','F9.','0,','F9.','0,','F9.','0,',
     &     'F9.','0,','F9.','0,','F9.','0,','F9.','0',')'/
      END
      SUBROUTINE CPHS
C***********************************************************************
C CALCULATES THERMODYNAMIC PROPERTIES FOR INDIVIDUAL SPECIES
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      REAL*8 cx(7),hcx(7),scx(7)
      INTEGER i,ij,j,jj,k
      SAVE i,ij,j,jj,k,scx
C
      DATA cx/2*0.,1.D0,.5D0,.6666666666666667D0,.75D0,.8D0/
      DATA hcx(3)/1.D0/
      k = 1
      IF ( Tt.GT.Tg(2) ) k = 2
      IF ( Tt.GT.Tg(3) ) k = 3
      cx(2) = 1.D0/Tt
      cx(1) = cx(2)**2
      scx(3) = Tln
      scx(2) = -cx(2)
      hcx(2) = Tln*cx(2)
      hcx(1) = -cx(1)
      scx(1) = hcx(1)*.5D0
      DO i = 4,7
        hcx(i) = cx(i)*Tt
        scx(i) = cx(i-1)*Tt
      ENDDO
      DO j = 1,Ng
        H0(j) = 0.D0
        S(j) = 0.D0
      ENDDO
      DO i = 7,4, - 1
        DO j = 1,Ng
          S(j) = (S(j)+Coef(j,i,k))*scx(i)
          H0(j) = (H0(j)+Coef(j,i,k))*hcx(i)
        ENDDO
      ENDDO
      DO i = 1,3
        DO j = 1,Ng
          S(j) = S(j) + Coef(j,i,k)*scx(i)
          H0(j) = H0(j) + Coef(j,i,k)*hcx(i)
        ENDDO
      ENDDO
      DO j = 1,Ng
        S(j) = S(j) + Coef(j,9,k)
        H0(j) = H0(j) + Coef(j,8,k)*cx(2)
      ENDDO
      IF ( .NOT.Tp.OR.Convg ) THEN
        DO j = 1,Ng
          Cp(j) = 0.D0
        ENDDO
        DO i = 7,4, - 1
          DO j = 1,Ng
            Cp(j) = (Cp(j)+Coef(j,i,k))*Tt
          ENDDO
        ENDDO
        DO i = 1,3
          DO j = 1,Ng
            Cp(j) = Cp(j) + Coef(j,i,k)*cx(i)
          ENDDO
        ENDDO
      ENDIF
      IF ( Npr.NE.0.AND.k.NE.3.AND.Ng.NE.Ngc ) THEN
        DO ij = 1,Npr
          j = Jcond(ij)
          jj = Jcond(ij) - Ng
          Cp(j) = 0.D0
          H0(j) = 0.D0
          S(j) = 0.D0
          DO i = 7,4, - 1
            S(j) = (S(j)+Cft(jj,i))*scx(i)
            H0(j) = (H0(j)+Cft(jj,i))*hcx(i)
            Cp(j) = (Cp(j)+Cft(jj,i))*Tt
          ENDDO
          DO i = 1,3
            S(j) = S(j) + Cft(jj,i)*scx(i)
            H0(j) = H0(j) + Cft(jj,i)*hcx(i)
            Cp(j) = Cp(j) + Cft(jj,i)*cx(i)
          ENDDO
          S(j) = S(j) + Cft(jj,9)
          H0(j) = H0(j) + Cft(jj,8)*cx(2)
        ENDDO
      ENDIF
      GOTO 99999
      ENTRY ALLCON
      DO jj = 1,Nc
        j = jj + Ng
        Cp(j) = 0.D0
        H0(j) = 0.D0
        S(j) = 0.D0
        DO i = 7,4, - 1
          S(j) = (S(j)+Cft(jj,i))*scx(i)
          H0(j) = (H0(j)+Cft(jj,i))*hcx(i)
          Cp(j) = (Cp(j)+Cft(jj,i))*Tt
        ENDDO
        DO i = 1,3
          S(j) = S(j) + Cft(jj,i)*scx(i)
          H0(j) = H0(j) + Cft(jj,i)*hcx(i)
          Cp(j) = Cp(j) + Cft(jj,i)*cx(i)
        ENDDO
        S(j) = S(j) + Cft(jj,9)
        H0(j) = H0(j) + Cft(jj,8)*cx(2)
      ENDDO
99999 END
      SUBROUTINE DETON
C***********************************************************************
C CHAPMAN-JOUGUET DETONATIONS.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*15 fdv,fg1,fh1,fhs1,fm1,fmm1,fpp1,frr1,ft1,ftt1
      CHARACTER*3 unit
      INTEGER i,ii,iof,itr,j,mdv,mgam,mh,mmach,mp,mson,mt,mxx(8)
      INTEGER INDEX
      REAL*8 a11,a12,a21,a22,alam,alfa,amm,b1,b2,cpl(NCOL),d,gam,
     &       gm1(NCOL),h1(NCOL),p1,pp1,pub(NCOL),rk,rr1,rrho(NCOL),t1,
     &       tem,tt1,tub(NCOL),ud,x1,x2
      SAVE a11,a12,a21,a22,alam,alfa,amm,b1,b2,cpl,d,gam,gm1,h1,i,ii,
     &  iof,itr,j,mdv,mgam,mh,mmach,mp,mson,mt,mxx,p1,pp1,pub,rk,rr1,
     &  rrho,t1,tem,tt1,tub,ud,unit,x1,x2
C
      EQUIVALENCE (mxx(1),mp)
      EQUIVALENCE (mxx(2),mt)
      EQUIVALENCE (mxx(3),mgam)
      EQUIVALENCE (mxx(4),mh)
      EQUIVALENCE (mxx(5),mdv)
      EQUIVALENCE (mxx(6),mson)
      EQUIVALENCE (mxx(7),mmach)
      DATA ft1/'T1, K'/,fh1/'H1, CAL/G'/,fhs1/'H1, KJ/KG'/,
     &     fm1/'M1, (1/n) '/,fg1/'GAMMA1'/,fpp1/'P/P1'/,ftt1/'T/T1'/,
     &     fmm1/'M/M1'/,frr1/'RHO/RHO1'/,fdv/'DET VEL,M/SEC'/
      iof = 0
      Eql = .TRUE.
      IF ( T(1).EQ.0. ) THEN
        T(1) = Rtemp(1)
        Nt = 1
      ENDIF
 100  Tt = T(1)
      iof = iof + 1
      Oxfl = Oxf(iof)
      CALL NEWOF
C BEGIN T LOOP.
      DO It = 1,Nt
        t1 = T(It)
C BEGIN P LOOP.
        DO Ip = 1,Np
          p1 = P(Ip)
          Tt = t1
          Pp = p1
          CALL HCALC
          IF ( Tt.EQ.0. ) RETURN
          IF ( Detdbg ) CALL OUT1
          h1(Npt) = Hsub0*R
          tub(Npt) = t1
          pub(Npt) = p1
          cpl(Npt) = Cpmix*R
          itr = 0
          Tt = 3800.
          pp1 = 15.
          Pp = pp1*p1
C CALCULATE ENTHALPY FOR INITIAL ESTIMATE OF T2(TT AFTER EQLBRM)
          Hsub0 = h1(Npt)/R + .75*t1*pp1/Wmix
          Tp = .FALSE.
          Hp = .TRUE.
          CALL EQLBRM
          Hsub0 = h1(Npt)/R
          Hp = .FALSE.
          IF ( Tt.NE.0. ) THEN
            gam = Gammas(Npt)
            tt1 = Tt/t1
            ii = 0
            tem = tt1 - .75*pp1/(Cpr(Npt)*Wmix)
            amm = Wm(Npt)/Wmix
            IF ( Detdbg ) WRITE (IOOUT,99001) Tt
C LOOP FOR IMPROVING T2/T1 AND P2/P1 INITIAL ESTIMATE.
            DO ii = 1,3
              alfa = amm/tt1
              pp1 = (1.+gam)*(1.+(1.-4.*gam*alfa/(1.+gam)**2)**.5)
     &              /(2.*gam*alfa)
              rk = pp1*alfa
              tt1 = tem + .5*pp1*gam*(rk*rk-1.)/(Wmix*Cpr(Npt)*rk)
              IF ( Detdbg ) WRITE (IOOUT,99002) ii,pp1,tt1
            ENDDO
            Tp = .TRUE.
            Tt = t1*tt1
            rr1 = pp1*amm/tt1
C BEGIN MAIN ITERATION LOOP.
 110        itr = itr + 1
            Pp = p1*pp1
            CALL EQLBRM
            IF ( Npt.EQ.0 ) GOTO 200
            IF ( Tt.NE.0. ) THEN
              gam = Gammas(Npt)
              amm = Wm(Npt)/Wmix
              rr1 = pp1*amm/tt1
              a11 = 1./pp1 + gam*rr1*Dlvpt(Npt)
              a12 = gam*rr1*Dlvtp(Npt)
              a21 = .5*gam*(rr1**2-1.-Dlvpt(Npt)*(1.+rr1**2))
     &              + Dlvtp(Npt) - 1.
              a22 = -.5*gam*Dlvtp(Npt)*(rr1**2+1.) - Wm(Npt)*Cpr(Npt)
              b1 = 1./pp1 - 1. + gam*(rr1-1.)
              b2 = Wm(Npt)*(Hsum(Npt)-h1(Npt)/R)
     &             /Tt - .5*gam*(rr1*rr1-1.)
              d = a11*a22 - a12*a21
              x1 = (a22*b1-a12*b2)/d
              x2 = (a11*b2-a21*b1)/d
              alam = 1.
              tem = x1
              IF ( tem.LT.0. ) tem = -tem
              IF ( x2.GT.tem ) tem = x2
              IF ( -x2.GT.tem ) tem = -x2
              IF ( tem.GT.0.4054652 ) alam = .4054652/tem
              pp1 = pp1*EXP(x1*alam)
              tt1 = tt1*EXP(x2*alam)
              Tt = t1*tt1
              ud = rr1*(Rr*gam*Tt/Wm(Npt))**.5
              IF ( Detdbg ) WRITE (IOOUT,99003) itr,pp1,tt1,rr1,x1,x2
C CONVERGENCE TEST
              IF ( itr.LT.8.AND.tem.GT.0.5E-04 ) GOTO 110
              IF ( itr.LT.8 ) THEN
                rrho(Npt) = rr1
                IF ( cpl(Npt).EQ.0. ) THEN
                  gm1(Npt) = 0.
                  Vmoc(Npt) = 0.
                ELSE
                  gm1(Npt) = cpl(Npt)/(cpl(Npt)-R/Wmix)
                  Vmoc(Npt) = ud/(Rr*gm1(Npt)*t1/Wmix)**.5
                ENDIF
              ELSE
                WRITE (IOOUT,99004)
                Npt = Npt - 1
                Tt = 0.
              ENDIF
              IF ( Trnspt ) CALL TRANP
              Isv = 0
              IF ( Ip.NE.Np.OR.It.NE.Nt.AND.Tt.NE.0. ) THEN
                Isv = Npt
                IF ( Npt.NE.NCOL ) GOTO 120
              ENDIF
            ENDIF
C OUTPUT
            WRITE (IOOUT,99005)
            CALL OUT1
C SET MXX ARRAY FOR PLOTTING PARAMETERS
            DO i = 1,8
              mxx(i) = 0
            ENDDO
            DO i = 1,Nplt
              IF ( INDEX(Pltvar(i)(2:),'1').NE.0 ) THEN
                IF ( Pltvar(i)(:3).EQ.'son' ) THEN
                  mson = i
                ELSEIF ( Pltvar(i)(:3).EQ.'gam' ) THEN
                  mgam = i
                ELSEIF ( Pltvar(i)(:1).EQ.'h' ) THEN
                  mh = i
                ELSEIF ( Pltvar(i)(:1).EQ.'t' ) THEN
                  mt = i
                ELSEIF ( Pltvar(i)(:1).EQ.'p' ) THEN
                  mp = i
                ENDIF
              ELSEIF ( INDEX(Pltvar(i),'vel').NE.0 ) THEN
                mdv = i
              ELSEIF ( INDEX(Pltvar(i),'mach').NE.0 ) THEN
                mmach = i
              ENDIF
            ENDDO
            WRITE (IOOUT,99006)
            Fmt(4) = '13'
            Fmt(5) = ' '
            Fmt(7) = '4,'
            DO i = 1,Npt
              IF ( Siunit ) THEN
                V(i) = pub(i)
                unit = 'BAR'
              ELSE
                V(i) = pub(i)/1.01325D0
                unit = 'ATM'
              ENDIF
              IF ( mp.GT.0 ) Pltout(i+Iplt,mp) = V(i)
            ENDDO
            WRITE (IOOUT,Fmt) 'P1, '//unit//'        ',(V(j),j=1,Npt)
            Fmt(7) = '2,'
            WRITE (IOOUT,Fmt) ft1,(tub(j),j=1,Npt)
            IF ( .NOT.Siunit ) WRITE (IOOUT,Fmt) fh1,(h1(j),j=1,Npt)
            IF ( Siunit ) WRITE (IOOUT,Fmt) fhs1,(h1(j),j=1,Npt)
            DO i = 1,Npt
              V(i) = Wmix
              Sonvel(i) = (Rr*gm1(i)*tub(i)/Wmix)**.5
            ENDDO
            Fmt(7) = '3,'
            WRITE (IOOUT,Fmt) fm1,(V(j),j=1,Npt)
            Fmt(7) = '4,'
            WRITE (IOOUT,Fmt) fg1,(gm1(j),j=1,Npt)
            Fmt(7) = '1,'
            WRITE (IOOUT,Fmt) 'SON VEL1,M/SEC ',(Sonvel(j),j=1,Npt)
            IF ( Nplt.GT.0 ) THEN
              DO i = 1,Npt
                IF ( mt.GT.0 ) Pltout(i+Iplt,mt) = tub(i)
                IF ( mgam.GT.0 ) Pltout(i+Iplt,mgam) = gm1(i)
                IF ( mh.GT.0 ) Pltout(i+Iplt,mh) = h1(i)
                IF ( mson.GT.0 ) Pltout(i+Iplt,mson) = Sonvel(i)
              ENDDO
            ENDIF
            WRITE (IOOUT,99007)
            Fmt(4) = Fmt(6)
            CALL OUT2
            IF ( Trnspt ) CALL OUT4
            WRITE (IOOUT,99008)
            Fmt(7) = '3,'
            DO i = 1,Npt
              V(i) = Ppp(i)/pub(i)
              Pcp(i) = Ttt(i)/tub(i)
              Sonvel(i) = Sonvel(i)*rrho(i)
              IF ( mmach.GT.0 ) Pltout(i+Iplt,mmach) = Vmoc(i)
              IF ( mdv.GT.0 ) Pltout(i+Iplt,mdv) = Sonvel(i)
            ENDDO
            WRITE (IOOUT,Fmt) fpp1,(V(j),j=1,Npt)
            WRITE (IOOUT,Fmt) ftt1,(Pcp(j),j=1,Npt)
            DO i = 1,Npt
              V(i) = Wm(i)/Wmix
            ENDDO
            Fmt(7) = '4,'
            WRITE (IOOUT,Fmt) fmm1,(V(j),j=1,Npt)
            WRITE (IOOUT,Fmt) frr1,(rrho(j),j=1,Npt)
            WRITE (IOOUT,Fmt) 'DET MACH NUMBER',(Vmoc(j),j=1,Npt)
            Fmt(7) = '1,'
            WRITE (IOOUT,Fmt) fdv,(Sonvel(j),j=1,Npt)
            Eql = .TRUE.
            CALL OUT3
            Iplt = MIN(Iplt+Npt,500)
            IF ( Isv.EQ.0.AND.iof.EQ.Nof ) GOTO 200
            IF ( Np.EQ.1.AND.Nt.EQ.1 ) GOTO 100
            WRITE (IOOUT,99009)
            Npt = 0
 120        Npt = Npt + 1
            IF ( Isv.EQ.1 ) Isv = -1
            CALL SETEN
          ENDIF
        ENDDO
      ENDDO
c     Iplt = MIN(Iplt+Npt,500)
      Iplt = MIN(Iplt+Npt-1,500)
      IF ( iof.LT.Nof ) GOTO 100
 200  Tp = .FALSE.
      RETURN
99001 FORMAT (/' T EST.=',F8.2/11X,'P/P1',17X,'T/T1')
99002 FORMAT (I5,2E20.8)
99003 FORMAT (/' ITER =',I2,5X,'P/P1 =',E15.8,/7X,'T/T1 =',E15.8,5X,
     &        'RHO/RHO1 =',E15.8,/7X,'DEL LN P/P1 =',E15.8,5X,
     &        'DEL LN T/T1 =',E15.8)
99004 FORMAT (/
     &        ' CONSERVATION EQNS NOT SATISFIED IN 8 ITERATIONS (DETON)'
     &        )
99005 FORMAT (//,21X,'DETONATION PROPERTIES OF AN IDEAL REACTING GAS')
99006 FORMAT (/' UNBURNED GAS'/)
99007 FORMAT (/' BURNED GAS'/)
99008 FORMAT (/' DETONATION PARAMETERS'/)
99009 FORMAT (///)
      END
      SUBROUTINE EFMT(Fone,Aa,Vx)
C***********************************************************************
C WRITE OUTPUT RECORD WITH NUMERICAL VALUES IN SPECIAL EXPONENT FORM.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C DUMMY ARGUMENTS
      CHARACTER*15 Aa
      CHARACTER*4 Fone
      REAL*8 Vx(MAXMAT)
C LOCAL VARIABLES
      CHARACTER*4 fmix(5),frmt(8)
      INTEGER i,j,j1,ne(NCOL)
      INTEGER IABS
      REAL*8 ee,fe,w(NCOL)
      REAL*8 DABS,DLOG10
      SAVE ee,fe,i,j,j1,ne,w
C
      DATA frmt/'(1H ',',A15',',','9X,','13(F','6.4,','I2,','1X))'/
      DATA fmix/'I3,','6.4,','I2,','9X,','5.3,'/
      frmt(6) = fmix(2)
      frmt(7) = fmix(3)
      j1 = 1
      frmt(4) = '1x,'
      IF ( Fone.EQ.'9X,' ) THEN
        j1 = 2
        frmt(4) = fmix(4)
      ENDIF
      DO i = j1,Npt
        IF ( Vx(i).NE.0. ) THEN
          ee = DLOG10(DABS(Vx(i)))
          ne(i) = ee
          fe = ne(i)
          IF ( ee.LT.-.2181E-05.AND.fe.NE.ee ) ne(i) = ne(i) - 1
          IF ( IABS(ne(i)).GE.10 ) THEN
            frmt(6) = fmix(5)
            frmt(7) = fmix(1)
          ENDIF
          w(i) = Vx(i)/10.**ne(i)
        ELSE
          w(i) = 0.
          ne(i) = 0.
        ENDIF
      ENDDO
      WRITE (IOOUT,frmt) Aa,(w(j),ne(j),j=j1,Npt)
      END
      SUBROUTINE EQLBRM
C***********************************************************************
C CALCULATE EQUILIBRIUM COMPOSITION AND PROPERTIES.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*12 ae,cmp(MAXEL)
      CHARACTER*16 amb
      LOGICAL cpcalc,i2many,newcom,reduce
      INTEGER i,il,ilamb,ilamb1,inc,ipr,iq2,iter,ix,ixsing,iz,j,ja,jb,
     &        jbx,jc,jcondi,jcons,jdelg,jex,jj,jkg,jneg,jsw,k,kc,kg,kk,
     &        kmat,kneg,l,lc,lcs(MAXEL),le,lelim,lk,ll,lncvg,ls,lsing,
     &        lz,maxitn,ncvg,njc,nn,numb
      INTEGER IABS
      REAL*8 aa,ambda,ambda1,bigen,bigneg,delg,dlnt,dpie,ensol,esize,
     &       gap,gasfrc,pie,pisave(MAXMAT-2),siz9,sizeg,smalno,smnol,
     &       sum,sum1,szgj,tem,tmelt,tsize,ween,xi,xln,xsize,xx(MAXMAT)
      REAL*8 DABS,DEXP,DLOG,DMAX1
      SAVE aa,ae,amb,ambda,ambda1,bigen,bigneg,cmp,cpcalc,delg,dlnt,
     &  dpie,ensol,esize,gap,gasfrc,i,i2many,il,ilamb,ilamb1,inc,ipr,
     &  iq2,iter,ix,ixsing,iz,j,ja,jb,jbx,jc,jcondi,jcons,jdelg,jex,jj,
     &  jkg,jneg,jsw,k,kc,kg,kk,kmat,kneg,l,lc,lcs,le,lelim,lk,ll,lncvg,
     &  ls,lsing,lz,maxitn,ncvg,newcom,njc,nn,numb,pie,pisave,reduce,
     &  siz9,sizeg,sum,sum1,szgj,tem,tmelt,tsize,ween,xi,xln,xsize,xx
C
      DATA smalno/1.E-6/,smnol/ - 13.815511/
      ixsing = 0
      lsing = 0
      jsw = 0
      jdelg = 0
      maxitn = 50
      ncvg = 0
      lncvg = 3*Nlm
      reduce = .FALSE.
      siz9 = Size - 9.2103404D0
      tsize = Size
      xsize = Size + 6.90775528D0
      IF ( Trace.NE.0. ) THEN
        maxitn = maxitn + Ngc/2
        xsize = -DLOG(Trace)
        IF ( xsize.LT.Size ) xsize = Size + .1
      ENDIF
      IF ( xsize.GT.80. ) xsize = 80.D0
      esize = MIN(80.D0,xsize+6.90775528D0)
      jcons = 0
      pie = 0.
      i2many = .FALSE.
      Pderiv = .FALSE.
      Convg = .FALSE.
      numb = 0
      cpcalc = .TRUE.
      IF ( Tp ) cpcalc = .FALSE.
      IF ( Tt.NE.0.D0 ) THEN
        IF ( Npr.EQ.0.OR.(Tt.NE.T(1).AND..NOT.Tp) ) GOTO 400
        k = 1
      ELSE
        Tt = 3800.D0
        IF ( Npr.EQ.0 ) GOTO 400
        k = 1
      ENDIF
 100  j = Jcond(k)
      jc = j - Ng
      kg = -Ifz(jc)
      DO i = 1,9
        kg = kg + 1
        kc = jc + kg
        IF ( Tt.LE.Temp(2,kc) ) THEN
          IF ( kg.NE.0 ) THEN
            Jcond(k) = j + kg
            En(j+kg,Npt) = En(j,Npt)
            En(j,Npt) = 0.
            IF ( Prod(j).NE.Prod(j+kg).AND..NOT.Short ) 
     &           WRITE (IOOUT,99023) Prod(j),Prod(j+kg)
          ENDIF
          GOTO 300
        ELSEIF ( kc.GE.Nc.OR.Ifz(kc+1).LE.Ifz(kc) ) THEN
          GOTO 200
        ENDIF
      ENDDO
 200  IF ( .NOT.Tp ) THEN
        Tt = Temp(2,kc) - 10.D0
        k = 1
        GOTO 100
      ENDIF
      WRITE (IOOUT,99028) Prod(j)
      En(j,Npt) = 0.D0
      Enln(j) = 0.D0
      Deln(j) = 0.D0
      DO i = k,Npr
        Jcond(i) = Jcond(i+1)
      ENDDO
      Npr = Npr - 1
 300  k = k + 1
      IF ( k.LE.Npr ) GOTO 100
 400  Tln = DLOG(Tt)
      IF ( Vol ) Pp = Rr*Enn*Tt/Vv
      CALL CPHS
      Tm = DLOG(Pp/Enn)
      le = Nlm
      IF ( Lsave.NE.0.AND.Nlm.NE.Lsave ) THEN
        tem = EXP(-tsize)
        DO i = Lsave + 1,Nlm
          DO j = 1,Ng
            IF ( A(i,j).NE.0. ) THEN
              En(j,Npt) = tem
              Enln(j) = -tsize
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      ls = Nlm
      lelim = 0
      lz = ls
      IF ( Ions ) lz = ls - 1
      IF ( Npt.EQ.1.AND..NOT.Shock.AND..NOT.Short ) WRITE (IOOUT,99001)
     &     (Elmt(i),i=1,Nlm)
      IF ( Debug(Npt) ) THEN
        DO i = 1,Nlm
          cmp(i) = Elmt(i)
        ENDDO
      ENDIF
C BEGIN ITERATION
 500  IF ( cpcalc ) THEN
        Cpsum = 0.D0
        DO j = 1,Ng
          Cpsum = Cpsum + En(j,Npt)*Cp(j)
        ENDDO
        IF ( Npr.NE.0 ) THEN
          DO k = 1,Npr
            j = Jcond(k)
            Cpsum = Cpsum + En(j,Npt)*Cp(j)
          ENDDO
          cpcalc = .FALSE.
        ENDIF
      ENDIF
      numb = numb + 1
      CALL MATRIX
      iq2 = Iq1 + 1
      IF ( Convg ) Imat = Imat - 1
      IF ( Debug(Npt) ) THEN
        IF ( .NOT.Convg ) THEN
          WRITE (IOOUT,99004) numb
        ELSE
          IF ( .NOT.Pderiv ) WRITE (IOOUT,99002)
          IF ( Pderiv ) WRITE (IOOUT,99003)
        ENDIF
        kmat = Imat + 1
        DO i = 1,Imat
          WRITE (IOOUT,99006) (G(i,k),k=1,kmat)
        ENDDO
      ENDIF
      Msing = 0
      CALL GAUSS
      IF ( Msing.EQ.0 ) THEN
        IF ( Debug(Npt) ) THEN
          WRITE (IOOUT,99005) (cmp(k),k=1,le)
          WRITE (IOOUT,99006) (X(i),i=1,Imat)
        ENDIF
        IF ( .NOT.Convg ) THEN
C OBTAIN CORRECTIONS TO THE ESTIMATES
          IF ( Vol ) X(iq2) = X(Iq1)
          IF ( Tp ) X(iq2) = 0.
          dlnt = X(iq2)
          sum = X(Iq1)
          IF ( Vol ) THEN
            X(Iq1) = 0.
            sum = -dlnt
          ENDIF
          DO 520 j = 1,Ng
            IF ( lelim.NE.0 ) THEN
              Deln(j) = 0.
              DO i = lelim,ls
                IF ( A(i,j).NE.0. ) GOTO 520
              ENDDO
            ENDIF
            Deln(j) = -Mu(j) + H0(j)*dlnt + sum
            DO k = 1,Nlm
              Deln(j) = Deln(j) + A(k,j)*X(k)
            ENDDO
            IF ( pie.NE.0. ) Deln(j) = Deln(j) + A(ls,j)*pie
 520      CONTINUE
          IF ( Npr.NE.0 ) THEN
            DO k = 1,Npr
              j = Jcond(k)
              kk = Nlm + k
              Deln(j) = X(kk)
            ENDDO
          ENDIF
C CALCULATE CONTROL FACTOR,AMBDA
          ambda = 1.D0
          ambda1 = 1.D0
          ilamb = 0
          ilamb1 = 0
          sum = DMAX1(DABS(X(Iq1)),DABS(dlnt))
          sum = sum*5.
          DO j = 1,Ng
            IF ( Deln(j).GT.0. ) THEN
              IF ( (Enln(j)-Ennl+Size).LE.0. ) THEN
                sum1 = DABS(Deln(j)-X(Iq1))
                IF ( sum1.GE.siz9 ) THEN
                  sum1 = DABS(-9.2103404D0-Enln(j)+Ennl)/sum1
                  IF ( sum1.LT.ambda1 ) THEN
                    ambda1 = sum1
                    ilamb1 = j
                  ENDIF
                ENDIF
              ELSEIF ( Deln(j).GT.sum ) THEN
                sum = Deln(j)
                ilamb = j
              ENDIF
            ENDIF
          ENDDO
          IF ( sum.GT.2.D0 ) ambda = 2.D0/sum
          IF ( ambda1.LE.ambda ) THEN
            ambda = ambda1
            ilamb = ilamb1
          ENDIF
          IF ( Debug(Npt) ) THEN
C INTERMEDIATE OUTPUT
            WRITE (IOOUT,99011) Tt,Enn,Ennl,Pp,Tm,ambda
            IF ( ambda.NE.1.D0 ) THEN
              amb = 'ENN'
              IF ( DABS(X(iq2)).GT.DABS(X(Iq1)) ) amb = 'TEMP'
              IF ( ilamb.NE.0 ) amb = Prod(ilamb)
              WRITE (IOOUT,99012) amb
            ENDIF
            IF ( Vol ) WRITE (IOOUT,99013) Vv*.001D0
            WRITE (IOOUT,99014)
            DO j = 1,Ngc
              WRITE (IOOUT,99015) Prod(j),En(j,Npt),Enln(j),Deln(j),
     &                       H0(j),S(j),H0(j) - S(j),Mu(j)
            ENDDO
          ENDIF
C APPLY CORRECTIONS TO ESTIMATES
          Totn(Npt) = 0.D0
          DO j = 1,Ng
            Enln(j) = Enln(j) + ambda*Deln(j)
          ENDDO
          DO 540 j = 1,Ng
            En(j,Npt) = 0.
            IF ( lelim.NE.0 ) THEN
              DO i = lelim,ls
                IF ( A(i,j).NE.0. ) GOTO 540
              ENDDO
            ENDIF
            IF ( (Enln(j)-Ennl+tsize).GT.0. ) THEN
              En(j,Npt) = DEXP(Enln(j))
              Totn(Npt) = Totn(Npt) + En(j,Npt)
            ENDIF
 540      CONTINUE
          IF ( Ions.AND.Elmt(Nlm).EQ.'E' ) THEN
            DO j = 1,Ng
              IF ( A(ls,j).NE.0..AND.En(j,Npt).EQ.0. ) THEN
                IF ( (Enln(j)-Ennl+esize).GT.0. ) THEN
                  En(j,Npt) = DEXP(Enln(j))
                  Totn(Npt) = Totn(Npt) + En(j,Npt)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
          Sumn = Totn(Npt)
          IF ( Npr.NE.0 ) THEN
            DO k = 1,Npr
              j = Jcond(k)
              En(j,Npt) = En(j,Npt) + ambda*Deln(j)
              Totn(Npt) = Totn(Npt) + En(j,Npt)
            ENDDO
          ENDIF
          IF ( .NOT.Tp ) THEN
            Tln = Tln + ambda*dlnt
            Tt = DEXP(Tln)
            cpcalc = .TRUE.
            CALL CPHS
          ENDIF
          IF ( Vol ) THEN
            Enn = Sumn
            Ennl = DLOG(Enn)
            IF ( Vol ) Pp = Rr*Tt*Enn/Vv
          ELSE
            Ennl = Ennl + ambda*X(Iq1)
            Enn = DEXP(Ennl)
          ENDIF
          Tm = DLOG(Pp/Enn)
          IF ( Elmt(Nlm).EQ.'E' ) THEN
C CHECK ON REMOVING IONS
            DO j = 1,Ngc
              IF ( A(Nlm,j).NE.0. ) THEN
                IF ( En(j,Npt).GT.0. ) GOTO 560
              ENDIF
            ENDDO
            pie = X(Nlm)
            lelim = Nlm
            Nlm = Nlm - 1
            GOTO 500
          ENDIF
C TEST FOR CONVERGENCE
 560      IF ( numb.GT.maxitn ) THEN
            WRITE (IOOUT,99019) maxitn,Npt
            IF ( Nc.EQ.0.OR.i2many ) GOTO 1500
            i2many = .TRUE.
            IF ( .NOT.Hp.OR.Npt.NE.1.OR.Tt.GT.100. ) THEN
              IF ( Npr.NE.1.OR.Enn.GT.1.E-4 ) GOTO 1500
C HIGH TEMPERATURE, INCLUDED CONDENSED CONDITION
              WRITE (IOOUT,99020)
              Enn = .1
              Ennl = -2.3025851
              Sumn = Enn
              xi = Ng
              xi = Enn/xi
              xln = DLOG(xi)
              DO j = 1,Ng
                En(j,Npt) = xi
                Enln(j) = xln
              ENDDO
              j = Jcond(1)
              k = 1
              GOTO 1000
            ELSE
              WRITE (IOOUT,99008)
              GOTO 1500
            ENDIF
          ELSE
            sum = (X(Iq1)*Enn/Totn(Npt))
            IF ( DABS(sum).GT.0.5E-5 ) GOTO 500
            DO j = 1,Ng
              IF ( DABS(Deln(j))*En(j,Npt)/Totn(Npt).GT.0.5D-5 )
     &             GOTO 500
            ENDDO
            IF ( DABS(dlnt).GT.1.D-04 ) GOTO 500
            IF ( Npr.NE.0 ) THEN
              DO k = 1,Npr
                j = Jcond(k)
                IF ( DABS(Deln(j)/Totn(Npt)).GT.0.5D-5 ) GOTO 500
                IF ( En(j,Npt).LT.0. ) GOTO 700
              ENDDO
            ENDIF
            le = Nlm
            DO i = 1,Nlm
              IF ( DABS(B0(i)).GE.1.D-06 ) THEN
                sum = 0.
                DO j = 1,Ngc
                  sum = sum + En(j,Npt)*A(i,j)
                ENDDO
                IF ( DABS(B0(i)-sum).GT.Bcheck ) GOTO 500
              ENDIF
            ENDDO
            IF ( Trace.NE.0. ) THEN
              tsize = xsize
              tem = 1.
              IF ( numb.NE.1 ) THEN
                lk = lz
                IF ( Nlm.LT.lz ) lk = Nlm
                DO i = 1,lk
                  IF ( i.NE.lsing ) THEN
                    tem = 0.
                    IF ( X(i).NE.0. ) THEN
                      tem = DABS((pisave(i)-X(i))/X(i))
                      IF ( tem.GT..001 ) GOTO 565
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
 565          DO i = 1,Nlm
                pisave(i) = X(i)
              ENDDO
              IF ( tem.GT..001 ) GOTO 500
              IF ( Ions ) THEN
C CHECK ON ELECTRON BALANCE
                iter = 1
                IF ( pie.NE.0. ) THEN
                  le = Nlm + 1
                  X(le) = pie
                ENDIF
 566            sum1 = 0.
                sum = 0.
                pie = X(le)
                DO j = 1,Ng
                  IF ( A(ls,j).NE.0. ) THEN
                    En(j,Npt) = 0.
                    tem = 0.
                    IF ( Enln(j).GT.-87. ) tem = DEXP(Enln(j))
                    IF ( (Enln(j)-Ennl+tsize).GT.0..AND.Elmt(Nlm)
     &                   .EQ.'E' ) THEN
                      pie = 0.
                      En(j,Npt) = tem
                    ENDIF
                    aa = A(ls,j)*tem
                    sum = sum + aa
                    sum1 = sum1 + aa*A(ls,j)
                  ENDIF
                ENDDO
                IF ( sum1.NE.0. ) THEN
                  dpie = -sum/sum1
                  DO j = 1,Ng
                    IF ( A(ls,j).NE.0. ) Enln(j) = Enln(j) + A(ls,j)
     &                   *dpie
                  ENDDO
                  IF ( Debug(Npt) ) WRITE (IOOUT,99016) iter,dpie
                  IF ( DABS(dpie).GT..0001 ) THEN
                    X(le) = X(le) + dpie
                    iter = iter + 1
                    IF ( iter.LE.80 ) GOTO 566
                    WRITE (IOOUT,99017)
                    GOTO 1500
                  ELSEIF ( Elmt(Nlm).EQ.'E'.AND.pie.NE.0. ) THEN
                    Nlm = Nlm - 1
                    newcom = .TRUE.
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSEIF ( .NOT.Pderiv ) THEN
C TEMPERATURE DERIVATIVES--CONVG=T, PDERIV=F
          Dlvtp(Npt) = 1. - X(Iq1)
          Cpr(Npt) = G(iq2,iq2)
          DO j = 1,Iq1
            Cpr(Npt) = Cpr(Npt) - G(iq2,j)*X(j)
          ENDDO
C PRESSURE DERIVATIVE--CONVG=T, PDERIV=T
          Pderiv = .TRUE.
          GOTO 500
        ELSE
          Dlvpt(Npt) = -1. + X(Iq1)
          IF ( Jliq.EQ.0 ) THEN
            Gammas(Npt) = -1./(Dlvpt(Npt)+(Dlvtp(Npt)**2)*Enn/Cpr(Npt))
          ELSE
            En(Jsol,Npt) = ensol
            Hsum(Npt) = Hsum(Npt) + En(Jliq,Npt)*(H0(Jliq)-H0(Jsol))
            Gammas(Npt) = -1./Dlvpt(Npt)
            Npr = Npr + 1
            Jcond(Npr) = Jliq
          ENDIF
          GOTO 1400
        ENDIF
C SINGULAR MATRIX
      ELSE
        IF ( Convg ) THEN
          WRITE (IOOUT,99007)
          Dlvpt(Npt) = -1.
          Dlvtp(Npt) = 1.
          Cpr(Npt) = Cpsum
          Gammas(Npt) = -1./(Dlvpt(Npt)+(Dlvtp(Npt)**2)*Enn/Cpr(Npt))
          GOTO 1400
        ELSE
          WRITE (IOOUT,99009) numb,Msing
          lsing = Msing
          ixsing = ixsing + 1
          IF ( ixsing.LE.8 ) THEN
            xsize = 80.
            tsize = xsize
            IF ( Msing.GT.Nlm.AND.numb.LT.1.AND.Npr.GT.1.AND.
     &           jdelg.GT.0 ) THEN
              ween = 1000.
              j = 0
              DO 570 i = 1,Npr
                jcondi = Jcond(i)
                IF ( jcondi.NE.jdelg ) THEN
                  DO ll = 1,Nlm
                    IF ( A(ll,jdelg).NE.0.AND.A(ll,jcondi).NE.0. ) THEN
                      IF ( En(jcondi,Npt).LE.ween ) THEN
                        ween = En(jcondi,Npt)
                        j = jcondi
                        k = i
                      ENDIF
                      GOTO 570
                    ENDIF
                  ENDDO
                ENDIF
 570          CONTINUE
              IF ( j.GT.0 ) THEN
                WRITE (IOOUT,99020)
                GOTO 1000
              ENDIF
            ELSEIF ( .NOT.Hp.OR.Npt.NE.1.OR.Nc.EQ.0.OR.Tt.GT.100. ) THEN
              IF ( ixsing.GE.3 ) THEN
                IF ( Msing.LT.Iq1 ) THEN
                  IF ( reduce.AND.Msing.LE.Nlm ) THEN
                    IF ( Nlm.LT.lelim ) GOTO 1500
                    WRITE (IOOUT,99010) Npt,Elmt(Nlm)
                    Nlm = Nlm - 1
                    GOTO 500
                  ELSEIF ( Msing.LE.Nlm ) THEN
C FIND NEW COMPONENTS
                    IF ( .NOT.Ions ) GOTO 1100
                    IF ( Elmt(Nlm).NE.'E' ) GOTO 1100
                    DO j = 1,Ng
                      IF ( A(Nlm,j).NE.0. ) En(j,Npt) = 0.D0
                    ENDDO
                    pie = X(Nlm)
                    Nlm = Nlm - 1
                    IF ( Msing.GT.Nlm ) GOTO 500
                    GOTO 1100
                  ELSE
C REMOVE CONDENSED SPECIES TO CORRECT SINGULARITY
                    k = Msing - Nlm
                    j = Jcond(k)
                    IF ( j.NE.jcons ) THEN
                      jcons = j
                      GOTO 1000
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
              DO 575 jj = 1,Ng
                IF ( Ions ) THEN
                  IF ( Elmt(Nlm).NE.'E' ) THEN
                    IF ( A(ls,jj).NE.0. ) GOTO 575
                  ENDIF
                ENDIF
                IF ( En(jj,Npt).EQ.0. ) THEN
                  En(jj,Npt) = smalno
                  Enln(jj) = smnol
                ENDIF
 575          CONTINUE
              GOTO 500
            ELSE
              WRITE (IOOUT,99008)
            ENDIF
          ENDIF
        ENDIF
        GOTO 1500
      ENDIF
C CALCULATE ENTROPY, CHECK ON DELTA S FOR SP PROBLEMS
 600  Ssum(Npt) = 0.
      DO j = 1,Ng
        Ssum(Npt) = Ssum(Npt) + En(j,Npt)*(S(j)-Enln(j)-Tm)
      ENDDO
      IF ( Npr.GT.0 ) THEN
        DO k = 1,Npr
          j = Jcond(k)
          Ssum(Npt) = Ssum(Npt) + En(j,Npt)*S(j)
        ENDDO
      ENDIF
      IF ( .NOT.Sp ) THEN
        Convg = .TRUE.
      ELSE
        tem = Ssum(Npt) - S0
        IF ( DABS(tem).GT..0005 ) GOTO 500
        IF ( Debug(Npt) ) WRITE (IOOUT,99018) tem
        Convg = .TRUE.
      ENDIF
C CONVERGENCE TESTS ARE SATISFIED, TEST CONDENSED SPECIES.
 700  ncvg = ncvg + 1
      IF ( ncvg.GT.lncvg ) THEN
C ERROR, SET TT=0
        WRITE (IOOUT,99034) lncvg
        GOTO 1500
      ELSE
        IF ( .NOT.Shock ) THEN
          DO il = 1,le
            xx(il) = X(il)
          ENDDO
          IF ( .NOT.Short ) THEN
            IF ( newcom ) WRITE (IOOUT,99021) (cmp(k),k=1,le)
            WRITE (IOOUT,99022) Npt,numb,Tt,(xx(il),il=1,le)
          ENDIF
          IF ( .NOT.Tp.AND.Npr.EQ.0.AND.Tt.LE.Tg(1)*.2D0 ) THEN
            WRITE (IOOUT,99008)
            GOTO 1500
          ENDIF
          newcom = .FALSE.
        ENDIF
        IF ( Npr.NE.0 ) THEN
          bigneg = 0.
          jneg = 0
          DO k = 1,Npr
            j = Jcond(k)
            IF ( En(j,Npt)*Cp(j).LE.bigneg ) THEN
              bigneg = En(j,Npt)*Cp(j)
              jneg = j
              kneg = k
            ENDIF
          ENDDO
          IF ( jneg.NE.0 ) THEN
            j = jneg
            k = kneg
            IF ( j.EQ.Jsol.OR.j.EQ.Jliq ) THEN
              Jsol = 0
              Jliq = 0
            ENDIF
            GOTO 1000
          ENDIF
        ENDIF
        IF ( Ngc.NE.Ng.OR.Tp ) THEN
          Ng = Ngc
          CALL CPHS
          Ng = Ngp1 - 1
          cpcalc = .TRUE.
          IF ( Ngc.EQ.Ng ) GOTO 750
          CALL ALLCON
          IF ( Npr.NE.0.AND..NOT.Tp ) THEN
            gap = 50.
            DO 710 ipr = 1,Npr
              j = Jcond(ipr)
              IF ( j.NE.Jsol.AND.j.NE.Jliq ) THEN
                inc = j - Ng
                kg = -Ifz(inc)
                DO iz = 1,20
                  kg = kg + 1
                  kc = inc + kg
                  IF ( Tt.LE.Temp(2,kc) ) THEN
                    IF ( kg.NE.0 ) THEN
                      jkg = j + kg
                      IF ( IABS(kg).GT.1.OR.Prod(j).EQ.Prod(jkg) )
     &                     GOTO 740
                      IF ( jkg.EQ.jsw ) GOTO 720
                      IF ( Tt.LT.Temp(1,inc)-gap.OR.Tt.GT.Temp(2,inc)
     &                     +gap ) GOTO 740
                      GOTO 720
                    ENDIF
                    GOTO 710
                  ELSEIF ( Ifz(kc+1).LE.Ifz(kc) ) THEN
                    GOTO 710
                  ENDIF
                ENDDO
                IF ( Tt.GT.Temp(2,kc)*1.2D0 ) GOTO 1000
              ENDIF
 710        CONTINUE
          ENDIF
          sizeg = 0.
          szgj = 0.
          DO inc = 1,Nc
            j = inc + Ng
            IF ( Debug(Npt) ) WRITE (IOOUT,99024) Prod(j),Temp(1,inc),
     &                               Temp(2,inc),En(j,Npt)
            IF ( En(j,Npt).LE.0. ) THEN
              IF ( Tt.GT.Temp(1,inc).OR.Temp(1,inc).EQ.Tg(1) ) THEN
                IF ( Tt.LE.Temp(2,inc) ) THEN
                  sum = 0.
                  DO i = 1,Nlm
                    sum = sum + A(i,j)*X(i)
                  ENDDO
                  delg = (H0(j)-S(j)-sum)/Mw(j)
                  IF ( delg.LT.sizeg.AND.delg.LT.0. ) THEN
                    IF ( j.NE.jcons ) THEN
                      sizeg = delg
                      jdelg = j
                    ELSE
                      szgj = delg
                    ENDIF
                    ipr = ipr - 1
                  ENDIF
                  IF ( Debug(Npt) ) WRITE (IOOUT,99025) delg,sizeg
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          IF ( sizeg.EQ.0..AND.szgj.EQ.0. ) GOTO 750
          IF ( sizeg.NE.0. ) THEN
            j = jdelg
            GOTO 800
          ELSE
            WRITE (IOOUT,99026) Prod(jcons)
            GOTO 1500
          ENDIF
 720      kk = MAX(0,kg)
          tmelt = Temp(kk+1,inc)
          Tt = tmelt
          Tln = DLOG(Tt)
          Jsol = MIN(j,jkg)
          Jliq = Jsol + 1
          En(jkg,Npt) = .5D0*En(j,Npt)
          En(j,Npt) = En(jkg,Npt)
          j = jkg
          GOTO 800
C WRONG PHASE INCLUDED FOR T INTERVAL, SWITCH EN
 740      En(jkg,Npt) = En(j,Npt)
          Jcond(ipr) = jkg
          En(j,Npt) = 0.
          jsw = j
          IF ( Prod(j).NE.Prod(jkg).AND..NOT.Short ) WRITE (IOOUT,99023)
     &         Prod(j),Prod(jkg)
          j = jkg
          GOTO 900
        ENDIF
C CONVERGED WITH NO CONDENSED CHANGES.  IF BOTH SOLID & LIQ PRESENT,
C TEMPORARILY REMOVE LIQ TO PREVENT SINGULAR DERIVATIVE MATRIX.
 750    Sumn = Enn
        IF ( Jsol.NE.0 ) THEN
          ensol = En(Jsol,Npt)
          En(Jsol,Npt) = En(Jsol,Npt) + En(Jliq,Npt)
          Dlvtp(Npt) = 0.
          Cpr(Npt) = 0.
          Gammas(Npt) = 0.
          Pderiv = .TRUE.
          DO k = 1,Npr
            IF ( Jcond(k).EQ.Jliq ) GOTO 760
          ENDDO
 760      DO i = k,Npr
            Jcond(i) = Jcond(i+1)
          ENDDO
          Npr = Npr - 1
        ENDIF
        GOTO 500
      ENDIF
C ADD CONDENSED SPECIES
 800  Npr = Npr + 1
      i = Npr
      DO ix = 2,Npr
        Jcond(i) = Jcond(i-1)
        i = i - 1
      ENDDO
      Jcond(1) = j
      IF ( .NOT.Short ) WRITE (IOOUT,99027) Prod(j)
 900  inc = j - Ng
      Convg = .FALSE.
      IF ( Tp ) cpcalc = .FALSE.
      numb = -1
      GOTO 500
C REMOVE CONDENSED SPECIES
 1000 En(j,Npt) = 0.D0
      Deln(j) = 0.D0
      Enln(j) = 0.D0
      DO i = k,Npr
        Jcond(i) = Jcond(i+1)
      ENDDO
      IF ( .NOT.Short ) WRITE (IOOUT,99028) Prod(j)
      Npr = Npr - 1
      DO i = 1,Nlm
        IF ( cmp(i).EQ.Prod(j) ) THEN
          numb = -1
          Convg = .FALSE.
          IF ( Tp ) cpcalc = .FALSE.
          GOTO 1100
        ENDIF
      ENDDO
      GOTO 900
 1100 newcom = .FALSE.
      nn = Nlm
      IF ( Elmt(Nlm).EQ.'E' ) nn = Nlm - 1
C FIND ORDER OF SPECIES FOR COMPONENTS - BIGGEST TO SMALLEST
      njc = 0
      DO lc = 1,nn
        lcs(lc) = 0
      ENDDO
 1200 bigen = -1.D-35
      DO j = 1,Ng
        IF ( En(j,Npt).GT.bigen ) THEN
          IF ( .NOT.Ions.OR.A(ls,j).EQ.0. ) THEN
            bigen = En(j,Npt)
            jbx = j
          ENDIF
        ENDIF
      ENDDO
      IF ( bigen.GT.0. ) THEN
        DO 1250 lc = 1,nn
          IF ( jbx.EQ.0 ) jbx = Jx(lc)
          IF ( A(lc,jbx).GT.smalno ) THEN
            IF ( njc.NE.0 ) THEN
              DO 1205 i = 1,njc
                l = lcs(i)
                IF ( l.EQ.lc ) GOTO 1250
                IF ( l.EQ.0 ) GOTO 1210
                j = Jcm(l)
                DO l = 1,nn
                  IF ( A(l,jbx).NE.A(l,j) ) GOTO 1205
                ENDDO
                GOTO 1250
 1205         CONTINUE
            ENDIF
 1210       DO i = 1,nn
              IF ( i.NE.lc ) THEN
                jex = Jx(i)
                IF ( DABS(A(lc,jbx)*A(i,jex)-A(lc,jex)*A(i,jbx))
     &               .LE.smalno ) GOTO 1250
              ENDIF
            ENDDO
            njc = njc + 1
            IF ( jbx.NE.Jcm(lc) ) newcom = .TRUE.
            Jcm(lc) = jbx
            lcs(njc) = lc
            GOTO 1300
          ENDIF
 1250   CONTINUE
 1300   En(jbx,Npt) = -En(jbx,Npt)
        IF ( njc.LT.nn ) GOTO 1200
      ENDIF
      DO j = 1,Ng
        En(j,Npt) = DABS(En(j,Npt))
      ENDDO
      IF ( newcom ) THEN
C SWITCH COMPONENTS
        DO lc = 1,nn
          jb = Jcm(lc)
          IF ( A(lc,jb).EQ.0. ) THEN
            jb = Jx(lc)
            Jcm(lc) = jb
          ENDIF
          tem = A(lc,jb)
          IF ( tem.NE.0. ) THEN
            pisave(lc) = H0(jb) - S(jb)
            IF ( jb.LE.Ng ) pisave(lc) = pisave(lc) + Enln(jb) + Tm
            cmp(lc) = Prod(jb)
C CALCULATE NEW COEFFICIENTS
            IF ( tem.NE.1. ) THEN
              B0(lc) = B0(lc)/tem
              B0p(lc,1) = B0p(lc,1)/tem
              B0p(lc,2) = B0p(lc,2)/tem
              DO j = 1,Nspx
                A(lc,j) = A(lc,j)/tem
              ENDDO
            ENDIF
            DO i = 1,nn
              IF ( A(i,jb).NE.0..AND.i.NE.lc ) THEN
                tem = A(i,jb)
                DO j = 1,Nspx
                  A(i,j) = A(i,j) - A(lc,j)*tem
                  IF ( DABS(A(i,j)).LT.1.E-5 ) A(i,j) = 0.
                ENDDO
                B0(i) = B0(i) - B0(lc)*tem
                B0p(i,1) = B0p(i,1) - B0p(lc,1)*tem
                B0p(i,2) = B0p(i,2) - B0p(lc,2)*tem
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        IF ( Debug(Npt) ) THEN
          WRITE (IOOUT,99029)
          WRITE (IOOUT,99030) (cmp(k),k=1,nn)
        ENDIF
      ENDIF
      IF ( Msing.NE.0 ) THEN
C SWITCH ORDER OF MSING AND NLM COMPONENTS
        reduce = .TRUE.
        lelim = Nlm
        lsing = Nlm
        IF ( Msing.NE.Nlm ) THEN
          DO j = 1,Nspx
            aa = A(Msing,j)
            A(Msing,j) = A(Nlm,j)
            A(Nlm,j) = aa
          ENDDO
          ja = Jcm(Msing)
          Jcm(Msing) = Jcm(Nlm)
          Jcm(Nlm) = ja
          ae = cmp(Msing)
          cmp(Msing) = cmp(Nlm)
          cmp(Nlm) = ae
          ae = Elmt(Msing)
          Elmt(Msing) = Elmt(Nlm)
          Elmt(Nlm) = ae
          ja = Jx(Msing)
          Jx(Msing) = Jx(Nlm)
          Jx(Nlm) = ja
          aa = Atwt(Msing)
          Atwt(Msing) = Atwt(Nlm)
          Atwt(Nlm) = aa
          aa = B0(Msing)
          B0(Msing) = B0(Nlm)
          B0(Nlm) = aa
          aa = pisave(Msing)
          pisave(Msing) = pisave(Nlm)
          pisave(Nlm) = aa
          DO i = 1,2
            aa = B0p(Msing,i)
            B0p(Msing,i) = B0p(Nlm,i)
            B0p(Nlm,i) = aa
          ENDDO
        ENDIF
      ELSEIF ( .NOT.newcom.AND.Trace.EQ.0. ) THEN
        GOTO 600
      ENDIF
      Msing = 0
      tsize = xsize
      GOTO 500
 1400 Ttt(Npt) = Tt
      Ppp(Npt) = Pp
      Vlm(Npt) = Rr*Enn*Tt/Pp
      Hsum(Npt) = Hsum(Npt)*Tt
      Wm(Npt) = 1./Enn
      gasfrc = Enn/Totn(Npt)
      IF ( gasfrc.LT..0001 ) WRITE (IOOUT,99031) Npt,gasfrc
      IF ( Trace.NE.0. ) THEN
        DO 1450 j = 1,Ng
          IF ( lelim.NE.0 ) THEN
            DO i = lelim,ls
              IF ( A(i,j).NE.0. ) GOTO 1450
            ENDDO
          ENDIF
          IF ( Enln(j).GT.-87. ) En(j,Npt) = DEXP(Enln(j))
 1450   CONTINUE
      ENDIF
      IF ( Debug(Npt) ) WRITE (IOOUT,99032) Npt,Pp,Tt,Hsum(Npt),
     &                      Ssum(Npt),Wm(Npt),Cpr(Npt),Dlvpt(Npt),
     &                            Dlvtp(Npt),Gammas(Npt),Vlm(Npt)
      IF ( Tt.GE.Tg(1).AND.Tt.LE.Tg(4) ) GOTO 1600
      IF ( Shock ) GOTO 1600
      WRITE (IOOUT,99033) Tt,Npt
      IF ( Tt.GE.Tg(1)*.8D0.AND.Tt.LE.Tg(4)*1.1D0 ) GOTO 1600
      Npt = Npt + 1
 1500 Tt = 0.
      Npt = Npt - 1
      WRITE (IOOUT,99035) Npt
 1600 Lsave = Nlm
      Nlm = ls
      IF ( Npr.GT.0 ) Gonly = .FALSE.
      RETURN
99001 FORMAT (/' POINT ITN',6X,'T',10X,4(A4,8X)/(18X,5(A4,8X)))
99002 FORMAT (/' T DERIV MATRIX')
99003 FORMAT (/' P DERIV MATRIX')
99004 FORMAT (/' ITERATION',I3,6X,'MATRIX ')
99005 FORMAT (/' SOLUTION VECTOR',/,6x,5A15/8X,5A15)
99006 FORMAT (3X,5E15.6)
99007 FORMAT (/' DERIVATIVE MATRIX SINGULAR (EQLBRM)')
99008 FORMAT (/' LOW TEMPERATURE IMPLIES A CONDENSED SPECIES SHOULD HA',
     &        'VE BEEN INSERTED,',
     &        /' RESTART WITH insert DATASET (EQLBRM)')
99009 FORMAT (/' SINGULAR MATRIX, ITERATION',I3,'  VARIABLE',I3,
     &        '(EQLBRM)')
99010 FORMAT (/' WARNING!! POINT',I3,
     &        ' USES A REDUCED SET OF COMPONENTS',/
     &       ' SPECIES CONTAINING THE ELIMINATED COMPONENT ARE OMITTED.'
     &       ,/
     &   ' IT MAY BE NECESSARY TO RERUN WITH INSERTED CONDENSED SPECIES'
     &   ,/' CONTAINING COMPONENT ',A8,'(EQLBRM)')
99011 FORMAT (/' T=',E15.8,' ENN=',E15.8,' ENNL=',E15.8,' PP=',E15.8,
     &        /' LN P/N=',E15.8,' AMBDA=',E15.8)
99012 FORMAT (/' AMBDA SET BY ',A16)
99013 FORMAT (' VOLUME=',E15.8,'CC/G')
99014 FORMAT (/24X,'Nj',12X,'LN Nj',8X,'DEL LN Nj',6X,'H0j/RT',/,41X,
     &        'S0j/R',10X,' G0j/RT',8X,' Gj/RT')
99015 FORMAT (1X,A16,4E15.6,/35x,3E15.6)
99016 FORMAT (/' ELECTRON BALANCE ITER NO. =',I4,'  DELTA PI =',E14.7)
99017 FORMAT (/' DID NOT CONVERGE ON ELECTRON BALANCE (EQLBRM)')
99018 FORMAT (/' DELTA S/R =',E15.8)
99019 FORMAT (/,I4,' ITERATIONS DID NOT SATISFY CONVERGENCE',/,15x,
     &        ' REQUIREMENTS FOR THE POINT',I5,' (EQLBRM)')
99020 FORMAT (/' TRY REMOVING CONDENSED SPECIES (EQLBRM)')
99021 FORMAT (/' POINT ITN',6X,'T',10X,4A12/(18X,5A12))
99022 FORMAT (I4,I5,5F12.3,/(12X,5F12.3))
99023 FORMAT (' PHASE CHANGE, REPLACE ',A16,'WITH ',A16)
99024 FORMAT (/1X,A15,2F10.3,3X,E15.7)
99025 FORMAT (' [G0j-SUM(Aij*PIi)]/Mj =',E15.7,9X,'MAX NEG DELTA G =',
     &        E15.7)
99026 FORMAT (/' REINSERTION OF ',A16,' LIKELY TO CAUSE SINGULARITY,',
     &        '(EQLBRM)')
99027 FORMAT (' ADD ',A16)
99028 FORMAT (' REMOVE ',A16)
99029 FORMAT (/' NEW COMPONENTS')
99030 FORMAT (/2x,6A12)
99031 FORMAT (/' WARNING!  RESULTS MAY BE WRONG FOR POINT',I3,' DUE TO',
     &        /' LOW MOLE FRACTION OF GASES (',E15.8,') (EQLBRM)')
99032 FORMAT (/' POINT=',I3,3X,'P=',E13.6,3X,'T=',E13.6,/3X,'H/R=',
     &        E13.6,3X,'S/R=',E13.6,/3X,'M=',E13.6,3X,'CP/R=',E13.6,3X,
     &        'DLVPT=',E13.6,/3X,'DLVTP=',E13.6,3X,'GAMMA(S)=',E13.6,3X,
     &        'V=',E13.6)
99033 FORMAT (' THE TEMPERATURE=',E12.4,' IS OUT OF RANGE FOR POINT',I5,
     &        '(EQLBRM)')
99034 FORMAT (/,I3,' CONVERGENCES FAILED TO ESTABLISH SET OF CONDENSED',
     &        ' SPECIES (EQLBRM)')
99035 FORMAT (/' CALCULATIONS STOPPED AFTER POINT',I3,'(EQLBRM)')
      END
      SUBROUTINE FROZEN
C***********************************************************************
C CALCULATE PROPERTIES WITH FROZEN COMPOSITION AT ASSIGNED ENTROPY
C AND PRESSURE.  CALLED FROM ROCKET.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER i,inc,iter,j,k,nnn
      REAL*8 DABS,DEXP,DLOG
      REAL*8 dlnt,dlpm
      SAVE dlnt,dlpm,i,inc,iter,j,k,nnn
C
      Convg = .FALSE.
      Tln = DLOG(Tt)
      dlpm = DLOG(Pp*Wm(Nfz))
      nnn = Npt
      Npt = Nfz
      DO j = 1,Ng
        IF ( En(j,Nfz).NE.0.D0 ) Deln(j) = -(DLOG(En(j,Nfz))+dlpm)
      ENDDO
      DO iter = 1,8
        Ssum(nnn) = 0.D0
        Cpsum = 0.D0
        CALL CPHS
        DO j = 1,Ng
          Cpsum = Cpsum + En(j,Nfz)*Cp(j)
          Ssum(nnn) = Ssum(nnn) + En(j,Nfz)*(S(j)+Deln(j))
        ENDDO
        IF ( Npr.NE.0 ) THEN
          DO k = 1,Npr
            j = Jcond(k)
            Cpsum = Cpsum + En(j,Nfz)*Cp(j)
            Ssum(nnn) = Ssum(nnn) + En(j,Nfz)*S(j)
          ENDDO
        ENDIF
        IF ( Convg ) THEN
          Npt = nnn
          Hsum(Npt) = 0.D0
          DO j = 1,Ngc
            Hsum(Npt) = Hsum(Npt) + En(j,Nfz)*H0(j)
          ENDDO
          Hsum(Npt) = Hsum(Npt)*Tt
          Ttt(Npt) = Tt
          Gammas(Npt) = Cpsum/(Cpsum-1./Wm(Nfz))
          Vlm(Npt) = Rr*Tt/(Wm(Nfz)*Pp)
          Wm(Npt) = Wm(Nfz)
          Dlvpt(Npt) = -1.
          Dlvtp(Npt) = 1.
          Totn(Npt) = Totn(Nfz)
          Ppp(Npt) = Pp
          Cpr(Npt) = Cpsum
          IF ( Tt.GE.(Tg(1)*.8D0) ) THEN
            DO i = Ngp1,Ngc
              IF ( En(i,Nfz).NE.0. ) THEN
                inc = i - Ng
                IF ( Tt.LT.(Temp(1,inc)-50.).OR.Tt.GT.(Temp(2,inc)+50.)
     &               ) GOTO 100
              ENDIF
            ENDDO
            GOTO 200
          ENDIF
          GOTO 100
        ELSE
          dlnt = (Ssum(Nfz)-Ssum(nnn))/Cpsum
          Tln = Tln + dlnt
          IF ( DABS(dlnt).LT.0.5D-4 ) Convg = .TRUE.
          Tt = DEXP(Tln)
        ENDIF
      ENDDO
      WRITE (IOOUT,99001)
 100  Tt = 0.
      Npt = Npt - 1
 200  RETURN
99001 FORMAT (/' FROZEN DID NOT CONVERGE IN 8 ITERATIONS (FROZEN)')
      END
      SUBROUTINE GAUSS
C***********************************************************************
C SOLVE ANY LINEAR SET OF UP TO MAXMAT EQUATIONS
C NUMBER OF EQUATIONS = IMAT
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER i,imatp1,j,k,nn,nnp1
      REAL*8 bigno,coefx(50),tmp
      REAL*8 DABS,DMAX1
      SAVE coefx,i,imatp1,j,k,nn,nnp1,tmp
C
      DATA bigno/1.E+25/
C BEGIN ELIMINATION OF NNTH VARIABLE
      imatp1 = Imat + 1
      DO nn = 1,Imat
        IF ( nn.NE.Imat ) THEN
C SEARCH FOR MAXIMUM COEFFICIENT IN EACH ROW
          nnp1 = nn + 1
          DO i = nn,Imat
            coefx(i) = bigno
            IF ( G(i,nn).NE.0. ) THEN
              coefx(i) = 0.
              DO j = nnp1,imatp1
                coefx(i) = DMAX1(coefx(i),DABS(G(i,j)))
              ENDDO
              tmp = DABS(G(i,nn))
              IF ( bigno*tmp.GT.coefx(i) ) THEN
                coefx(i) = coefx(i)/tmp
              ELSE
                coefx(i) = bigno
              ENDIF
            ENDIF
          ENDDO
C LOCATE ROW WITH SMALLEST MAXIMUM COEFFICIENT
          tmp = bigno
          i = 0
          DO j = nn,Imat
            IF ( coefx(j).LT.tmp ) THEN
              tmp = coefx(j)
              i = j
            ENDIF
          ENDDO
          IF ( i.EQ.0 ) THEN
            Msing = nn
            GOTO 99999
C INDEX I LOCATES EQUATION TO BE USED FOR ELIMINATING THE NTH
C VARIABLE FROM THE REMAINING EQUATIONS
C INTERCHANGE EQUATIONS I AND NN
          ELSEIF ( nn.NE.i ) THEN
            DO j = nn,imatp1
              tmp = G(i,j)
              G(i,j) = G(nn,j)
              G(nn,j) = tmp
            ENDDO
          ENDIF
        ELSEIF ( G(nn,nn).EQ.0 ) THEN
          Msing = nn
          GOTO 99999
        ENDIF
C DIVIDE NTH ROW BY NTH DIAGONAL ELEMENT AND ELIMINATE THE NTH
C VARIABLE FROM THE REMAINING EQUATIONS
        k = nn + 1
        tmp = G(nn,nn)
        IF ( tmp.EQ.0. ) THEN
          Msing = nn
          GOTO 99999
        ELSE
          DO j = k,imatp1
            G(nn,j) = G(nn,j)/tmp
          ENDDO
          IF ( k.NE.imatp1 ) THEN
            DO i = k,Imat
CDIR$ IVDEP
              DO j = k,imatp1
                G(i,j) = G(i,j) - G(i,nn)*G(nn,j)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C BACKSOLVE FOR THE VARIABLES
      k = Imat
 100  j = k + 1
      X(k) = 0.0D0
      tmp = 0.0
      IF ( Imat.GE.j ) THEN
        DO i = j,Imat
          tmp = tmp + G(k,i)*X(i)
        ENDDO
      ENDIF
      X(k) = G(k,imatp1) - tmp
      k = k - 1
      IF ( k.NE.0 ) GOTO 100
99999 END
      SUBROUTINE HCALC
C***********************************************************************
C CALCULATE PROPERTIES FOR TOTAL REACTANT USING THERMO DATA FOR
C ONE OR MORE REACTANTS. USED ONLY FOR SHOCK AND DETON PROBLEMS.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*6 date(MAXNGC)
      CHARACTER*2 el(5)
      CHARACTER*15 sub
      INTEGER i,icf,ifaz,itot,j,k,l,m,n,nall,nint,ntgas,ntot
      REAL*8 bb(5),enj,er,sj,t1,t2,tem,thermo(9,3),tsave
      REAL*8 DLOG
      SAVE bb,date,el,enj,er,i,icf,ifaz,itot,j,k,l,m,n,nall,nint,ntgas,
     &  ntot,sj,sub,t1,t2,tem,thermo,tsave
C
      tsave = Tt
      Tm = 0.
      IF ( Pp.GT.0. ) Tm = DLOG(Pp*Wmix)
      Ssum(Npt) = 0.
      Hpp(1) = 0.
      Hpp(2) = 0.
      Hsub0 = 0.
      Cpmix = 0.
      tem = (1.+Oxfl)
C LOOP ON REACTANTS.
C IF OXIDANT, K=1
C IF FUEL, K=2
      Nspr = Nspx
      DO n = 1,Nreac
        k = 2
        IF ( Fox(n)(:1).EQ.'O'.OR.Fox(n)(:1).EQ.'o' ) k = 1
        IF ( Tt.EQ.0. ) Tt = Rtemp(n)
        j = Jray(n)
        IF ( j.EQ.0 ) THEN
C SEARCH FOR REACTANT IN STORED THERMO SPECIES. STORE INDEX IN JRAY(N).
          ifaz = 0
          DO j = 1,Ngc
            IF ( Rname(n).EQ.Prod(j).OR.'*'//Rname(n).EQ.Prod(j) ) THEN
              Jray(n) = j
              IF ( j.GT.Ng ) THEN
                WRITE (IOOUT,99001)
                GOTO 20
              ENDIF
              GOTO 50
            ENDIF
          ENDDO
C SEARCH THERMO.LIB FOR SPECIES.
          REWIND IOTHM
          READ (IOTHM) Tg,ntgas,ntot,nall
          Nspr = Nspr + 1
          DO itot = 1,nall
            IF ( itot.LE.ntot ) THEN
              icf = 3
              IF ( itot.GT.ntgas ) icf = 1
              READ (IOTHM) sub,nint,date(Nspr),(el(j),bb(j),j=1,5),ifaz,
     &                     t1,t2,Mw(Nspr),((thermo(l,m),l=1,9),m=1,icf)
            ELSE
              READ (IOTHM) sub,nint,date(Nspr),(el(j),bb(j),j=1,5),ifaz,
     &                     t1,t2,Mw(Nspr),er
              IF ( nint.NE.0 ) THEN
                READ (IOTHM) ((thermo(i,j),i=1,9),j=1,nint)
                icf = nint
              ENDIF
            ENDIF
            IF ( sub.EQ.Rname(n).OR.sub.EQ.'*'//Rname(n) ) THEN
              IF ( ifaz.LE.0.AND.nint.GT.0 ) THEN
                DO j = 1,5
                  IF ( bb(j).EQ.0. ) GOTO 2
                  Nfla(n) = j
                  Ratom(n,j) = el(j)
                  Rnum(n,j) = bb(j)
                ENDDO
 2              Jray(n) = Nspr
                j = Nspr
                DO l = 1,icf
                  DO m = 1,9
                    Coef(j,m,l) = thermo(m,l)
                  ENDDO
                ENDDO
                GOTO 50
              ELSE
                IF ( ifaz.GT.0 ) WRITE (IOOUT,99001)
                IF ( nint.EQ.0 ) WRITE (IOOUT,99002) Rname(n)
                GOTO 20
              ENDIF
            ENDIF
          ENDDO
          Nspr = Nspr - 1
          WRITE (IOOUT,99003) Rname(n)
          Energy(n) = ' '
 20       Tt = 0.
          Cpmix = 0.
          GOTO 100
        ENDIF
C CALCULATE EN FOR REACTANT AND CALCULATE PROPERTIES.
 50     IF ( Moles ) enj = Pecwt(n)/Wp(k)
        IF ( .NOT.Moles ) enj = Pecwt(n)/Rmw(n)
        enj = enj/tem
        IF ( k.EQ.1 ) enj = enj*Oxfl
        Tln = DLOG(Tt)
        En(j,Npt) = enj
        l = 1
        IF ( ifaz.LE.0 ) THEN
          IF ( Tt.GT.Tg(2) ) l = 2
          IF ( Tt.GT.Tg(3).AND.ifaz.LT.0 ) l = 3
        ENDIF
        S(j) = ((((Coef(j,7,l)/4.)*Tt+Coef(j,6,l)/3.)*Tt+Coef(j,5,l)/2.)
     &         *Tt+Coef(j,4,l))*Tt - (Coef(j,1,l)*.5D0/Tt+Coef(j,2,l))
     &         /Tt + Coef(j,3,l)*Tln + Coef(j,9,l)
        H0(j) = ((((Coef(j,7,l)/5.)*Tt+Coef(j,6,l)/4.)*Tt+Coef(j,5,l)/3.
     &          )*Tt+Coef(j,4,l)/2.)
     &          *Tt - (Coef(j,1,l)/Tt-Coef(j,2,l)*Tln-Coef(j,8,l))
     &          /Tt + Coef(j,3,l)
        Cp(j) = (((Coef(j,7,l)*Tt+Coef(j,6,l))*Tt+Coef(j,5,l))
     &          *Tt+Coef(j,4,l))*Tt + (Coef(j,1,l)/Tt+Coef(j,2,l))
     &          /Tt + Coef(j,3,l)
        IF ( H0(j).GT.-.01.AND.H0(j).LT..01 ) H0(j) = 0.
C ADD CONTRIBUTION TO CP, H, AND S OF TOTAL REACTANT.
        Cpmix = Cpmix + Cp(j)*enj
C FOR CONDENSED SPECIES:  SJ = S(J)
        sj = S(j) - DLOG(enj) - Tm
        Ssum(Npt) = Ssum(Npt) + enj*sj
        er = H0(j)*enj*Tt
        Hsub0 = Hsub0 + er
        Hpp(k) = Hpp(k) + er
      ENDDO
      IF ( tsave.NE.0. ) Tt = tsave
 100  RETURN
99001 FORMAT (/' REACTANTS MUST BE GASEOUS FOR THIS PROBLEM (HCALC)')
99002 FORMAT (/' COEFFICIENTS FOR ',A15,' ARE NOT AVAILABLE (HCALC)')
99003 FORMAT (/' ERROR IN DATA FOR ',A15,' CHECK NAME AND TEMPERATURE',
     &        ' RANGE IN',/,' thermo.inp (HCALC)')
      END
      SUBROUTINE INFREE(Readok,Cin,Ncin,Lcin,Dpin)
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
C   DPIN - NUMERICS AS DOUBLE PRECISION VARIABLE.
C   CNUM - CHARACTER STRING REPRESENTING DATASET NUMBERS. MAXIMUM 24
C          CHARACTERS.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C DUMMY ARGUMENTS
      CHARACTER*15 Cin(MAXNGC)
      INTEGER Ncin
      INTEGER Lcin(MAXNGC)
      LOGICAL Readok
      REAL*8 Dpin(MAXNGC)
C LOCAL VARIABLES
      CHARACTER*1 ch1(132),cx,nums(13)
      CHARACTER*24 cnum
      CHARACTER*3 fmtl(3)
      CHARACTER*2 numg(24)
      CHARACTER*4 w1
      INTEGER i,ich1,j,kcin,nb,nch1,nx
C
      DATA fmtl/'(g','16','.0)'/
      DATA nums/'+','-','0','1','2','3','4','5','6','7','8','9','.'/
      DATA numg/'1','2','3','4','5','6','7','8','9','10','11','12','13',
     &     '14','15','16','17','18','19','20','21','22','23','24'/
      Ncin = 1
      Lcin(1) = 0
      kcin = 0
      Dpin(1) = 0.D0
 100  nb = 1
      nx = 0
      cnum = ' '
      Cin(Ncin) = ' '
      ch1(1) = ' '
      nch1 = 1
C READ CHARACTERS, ONE AT A TIME
      READ (IOINP,99001,END=500,ERR=500) ch1
C FIND FIRST AND LAST NON-BLANK CHARACTER
      DO i = 132,1, - 1
        nch1 = i
        IF ( ch1(i).NE.' '.AND.ch1(i).NE.'	' ) GOTO 200
      ENDDO
 200  DO i = 1,nch1
        ich1 = i
        IF ( ch1(i).NE.' '.AND.ch1(i).NE.'	' ) GOTO 300
      ENDDO
 300  IF ( nch1.EQ.1.OR.ch1(ich1).EQ.'#'.OR.ch1(ich1).EQ.'!' ) THEN
        WRITE (IOOUT,99002) (ch1(i),i=1,nch1)
        GOTO 100
      ENDIF
      w1 = ch1(ich1)//ch1(ich1+1)//ch1(ich1+2)//ch1(ich1+3)
C IS STRING A KEYWORD SIGNALLING START OR END OF DATASET?
      IF ( w1.EQ.'ther'.OR.w1.EQ.'tran'.OR.w1.EQ.'prob'.OR.
     &     w1.EQ.'reac'.OR.w1.EQ.'outp'.OR.w1.EQ.'omit'.OR.
     &     w1.EQ.'only'.OR.w1.EQ.'inse'.OR.w1(1:3).EQ.'end' ) THEN
        IF ( Ncin.EQ.1 ) THEN
          Cin(Ncin) = w1
          IF ( w1(1:3).EQ.'end'.OR.w1.EQ.'ther'.OR.w1.EQ.'tran' ) THEN
            WRITE (IOOUT,99002) (ch1(i),i=1,nch1)
            RETURN
          ENDIF
          ich1 = ich1 + 4
          nx = 4
          Lcin(1) = -4
        ELSE
C KEYWORD READ FOR NEXT DATASET. END PROCESSING
          BACKSPACE IOINP
          IF ( nx.EQ.0 ) Ncin = Ncin - 1
          RETURN
        ENDIF
      ELSEIF ( Ncin.EQ.1 ) THEN
        WRITE (IOOUT,99003)
        GOTO 500
      ENDIF
      WRITE (IOOUT,99002) (ch1(i),i=1,nch1)
      DO 400 i = ich1,nch1
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
C CONVERT NUMERIC CHARACTER STRINGS TO REAL*8 VARIABLES (DPIN)
            fmtl(2) = numg(MIN(24,nx))
C INTERNAL READ TO CONVERT TO NUMERIC
            READ (cnum,fmtl,ERR=320) Dpin(Ncin)
          ENDIF
          GOTO 340
 320      IF ( Cin(Ncin-1)(:4).NE.'case' ) WRITE (IOOUT,99004) Cin(i)
          Lcin(Ncin) = 0
 340      Ncin = Ncin + 1
          Cin(Ncin) = ' '
          Lcin(Ncin) = 0
          Dpin(Ncin) = 0.D0
          nx = 0
          cnum = ' '
        ENDIF
        nb = nb + 1
 400  CONTINUE
      IF ( nx.GT.0 ) THEN
        Ncin = Ncin + 1
        Lcin(Ncin) = 0
        Dpin(Ncin) = 0.D0
      ENDIF
      GOTO 100
 500  Readok = .FALSE.
      RETURN
99001 FORMAT (132A1)
99002 FORMAT (1x,80A1)
99003 FORMAT (/' FATAL ERROR IN INPUT FORMAT (INFREE)')
99004 FORMAT (/' WARNING!!  UNACCEPTABLE NUMBER ',A15,' (INFREE)')
      END
      SUBROUTINE INPUT(Readok,Caseok,Ensert)
C***********************************************************************
C DECIPHER KEYWORDS, LITERAL VARIABLES, & NUMERICAL VARIABLES IN INPUT.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C DUMMY ARGUMENTS
      LOGICAL Caseok,Readok
      CHARACTER*15 Ensert(20)
C LOCAL VARIABLES
      CHARACTER*15 cin(MAXNGC),cx15
      CHARACTER*4 code,cx4
      CHARACTER*1 cx1
      CHARACTER*2 cx2
      CHARACTER*3 cx3
      CHARACTER*26 lc,uc
      LOGICAL eqrats,incd,phi,pltdat,reacts,refl
      INTEGER i,ifrmla,ii,in,iv,ix,j,jj,k,lcin(MAXNGC),ncin,nmix
      INTEGER INDEX
      REAL*8 denmtr,dpin(MAXNGC),eratio,hr,mix(MAXNGC),ur,xyz
      REAL*8 DABS,DMIN1,DSQRT
      SAVE cin,code,cx1,cx15,cx2,cx3,cx4,denmtr,dpin,eqrats,eratio,hr,i,
     &  ifrmla,ii,in,incd,iv,ix,j,jj,k,lcin,mix,ncin,nmix,phi,pltdat,
     &  reacts,refl,ur,xyz
C
      DATA uc/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA lc/'abcdefghijklmnopqrstuvwxyz'/
      WRITE (IOOUT,99001)
      Caseok = .TRUE.
      Nonly = 0
      Nomit = 0
      Nsert = 0
      reacts = .FALSE.
      Trace = 0
      Short = .FALSE.
      Massf = .FALSE.
      DO i = 1,NCOL
        Debug(i) = .FALSE.
      ENDDO
      Nplt = 0
      Siunit = .TRUE.
      pltdat = .FALSE.
C CALL INFREE TO READ DATASET
 100  CALL INFREE(Readok,cin,ncin,lcin,dpin)
      IF ( .NOT.Readok ) GOTO 400
      code = cin(1)
      IF ( code.NE.'    ' ) THEN
C STORE PRODUCT NAMES FROM 'ONLY' DATASET
        IF ( code.EQ.'only' ) THEN
          Nonly = MIN(MAXNGC,ncin-1)
          DO i = 1,Nonly
            Prod(i) = cin(i+1)
          ENDDO
C STORE CONDENSED PRODUCT NAMES FROM 'INSERT' DATASET
        ELSEIF ( code.EQ.'inse' ) THEN
          Nsert = MIN(20,ncin-1)
          DO i = 1,Nsert
            Ensert(i) = cin(i+1)
          ENDDO
C STORE PRODUCT NAMES FROM 'OMIT' DATASET
        ELSEIF ( code.EQ.'omit' ) THEN
C CHECK OMIT DATASET
          Nomit = MIN(MAXNGC,ncin-1)
          DO i = 1,Nomit
            Omit(i) = cin(i+1)
          ENDDO
C KEYWORD 'THER' READ
C CALL UTHERM TO CONVERT FORMATTED THERMODYNAMIC DATA
        ELSEIF ( code.EQ.'ther' ) THEN
          Newr = .TRUE.
          REWIND IOTHM
          CALL UTHERM(Readok)
          IF ( .NOT.Readok ) THEN
            WRITE (IOOUT,99025)
            GOTO 400
          ENDIF
C KEYWORD 'TRAN' READ
C CALL UTRAN TO CONVERT FORMATTED TRANSPORT PROPERTIES
        ELSEIF ( code.EQ.'tran' ) THEN
          CALL UTRAN(Readok)
          IF ( .NOT.Readok ) THEN
            WRITE (IOOUT,99025)
            GOTO 400
          ENDIF
C PROCESS 'OUTP' DATASET.
        ELSEIF ( code.EQ.'outp' ) THEN
          DO 120 i = 2,ncin
            IF ( lcin(i).LT.0 ) THEN
              cx2 = cin(i)(1:2)
              cx3 = cin(i)(1:3)
              cx4 = cin(i)(1:4)
              IF ( cx3.EQ.'cal' ) THEN
                Siunit = .FALSE.
              ELSEIF ( cx4.EQ.'tran'.OR.cx3.EQ.'trn' ) THEN
                Trnspt = .TRUE.
              ELSEIF ( cx4.EQ.'trac' ) THEN
                Trace = dpin(i+1)
              ELSEIF ( cin(i)(:5).EQ.'short' ) THEN
                Short = .TRUE.
              ELSEIF ( cin(i)(:5).EQ.'massf' ) THEN
                Massf = .TRUE.
              ELSEIF ( cx3.EQ.'deb'.OR.cx3.EQ.'dbg' ) THEN
                DO j = i + 1,ncin
                  IF ( lcin(j).NE.i ) GOTO 120
                  k = dpin(j)
                  IF ( k.LE.NCOL ) Debug(k) = .TRUE.
                  lcin(j) = 0
                ENDDO
              ELSEIF ( cx2.EQ.'si' ) THEN
                Siunit = .TRUE.
              ELSEIF ( pltdat.AND.Nplt.LT.20 ) THEN
                Nplt = Nplt + 1
                Pltvar(Nplt) = cin(i)
              ELSEIF ( cx2.EQ.'pl' ) THEN
                pltdat = .TRUE.
              ELSE
                WRITE (IOOUT,99002) cin(i)
              ENDIF
            ENDIF
 120      CONTINUE
C SORT AND STORE DATA FROM 'REAC' DATASET.
        ELSEIF ( code.EQ.'reac' ) THEN
          reacts = .TRUE.
          Moles = .FALSE.
          Nreac = 0
          DO i = 1,MAXR
            Pecwt(i) = -1.
          ENDDO
          i = 1
 140      i = i + 1
          IF ( i.LE.ncin ) THEN
            IF ( lcin(i).NE.0 ) THEN
              IF ( lcin(i).GT.0 ) THEN
                WRITE (IOOUT,99003) cin(i)
                GOTO 140
              ENDIF
              cx15 = cin(i)
              cx1 = cx15(:1)
              cx2 = cx15(:2)
              cx3 = cx15(:3)
              cx4 = cx15(:4)
C NEW REACTANT
              IF ( cx2.NE.'na'.AND.cx2.NE.'ox'.AND.cx2.NE.'fu' ) THEN
C LOOK FOR PERCENTS
                IF ( cx1.EQ.'m'.OR.cx1.EQ.'w' ) THEN
                  IF ( lcin(i+1).GT.0 ) THEN
                    i = i + 1
                    Pecwt(Nreac) = dpin(i)
                  ELSE
                    Caseok = .FALSE.
                    WRITE (IOOUT,99004)
                  ENDIF
                  IF ( cx1.EQ.'m'.AND.Nreac.EQ.1 ) Moles = .TRUE.
                  IF ( cx1.EQ.'m'.AND..NOT.Moles.OR.cx1.EQ.'w'.AND.
     &                 Moles ) THEN
                    Caseok = .FALSE.
                    WRITE (IOOUT,99005)
                  ENDIF
                  GOTO 140
                ENDIF
C LOOK FOR TEMPERATURES
                IF ( cx1.EQ.'t' ) THEN
                  IF ( lcin(i+1).GT.0 ) THEN
                    i = i + 1
                    Rtemp(Nreac) = dpin(i)
                    IF ( lcin(i-1).LT.1 ) THEN
                      IF ( INDEX(cx15,'r').GT.0 ) Rtemp(Nreac)
     &                     = Rtemp(Nreac)/1.8D0
                      IF ( INDEX(cx15,'c').GT.0 ) Rtemp(Nreac)
     &                     = Rtemp(Nreac) + 273.15D0
                      IF ( INDEX(cx15,'f').GT.0 ) Rtemp(Nreac)
     &                     = (Rtemp(Nreac)-32.D0)/1.8D0 + 273.15D0
                    ENDIF
                  ELSE
                    WRITE (IOOUT,99006)
                    Caseok = .FALSE.
                  ENDIF
                  GOTO 140
                ENDIF
C LOOK FOR ENTHALPY
                IF ( cx1.EQ.'h'.OR.cx1.EQ.'u' ) THEN
                  Energy(Nreac) = cx15
                  IF ( lcin(i+1).GT.0 ) THEN
                    i = i + 1
                    Enth(Nreac) = dpin(i)*1000.D0/Rr
                    IF ( INDEX(cin(i-1),'c').GT.0 ) Enth(Nreac)
     &                   = Enth(Nreac)*4.184D0
                    IF ( INDEX(cin(i-1),'k').GT.0 ) Enth(Nreac)
     &                   = Enth(Nreac)*1000.D0
                  ENDIF
                  GOTO 140
                ENDIF
C LOOK FOR DENSITY
                IF ( cx3.EQ.'rho'.OR.cx3.EQ.'den' ) THEN
                  IF ( lcin(i+1).GT.0 ) THEN
                    i = i + 1
                    Dens(Nreac) = dpin(i)
                    IF ( INDEX(cx15,'kg').GT.0 ) Dens(Nreac)
     &                   = Dens(Nreac)/1000.D0
                  ENDIF
                  GOTO 140
                ENDIF
C CHECK FOR CHEMICAL SYMBOLS IN EXPLODED FORMULA
                IF ( (lcin(i).EQ.-1.OR.lcin(i).EQ.-2).AND.INDEX(uc,cx1)
     &               .GT.0 ) THEN
                  Energy(Nreac) = ' '
                  ifrmla = ifrmla + 1
                  Nfla(Nreac) = ifrmla
                  IF ( lcin(i).EQ.-2 ) THEN
                    ix = INDEX(lc,cx2(2:2))
                    IF ( ix.GT.0 ) cx2(2:2) = uc(ix:ix)
                  ENDIF
                  Ratom(Nreac,ifrmla) = cx2
                  IF ( lcin(i+1).EQ.i ) THEN
                    Rnum(Nreac,ifrmla) = dpin(i+1)
                  ELSE
                    Rnum(Nreac,ifrmla) = 1.
                  ENDIF
                  i = i + 1
                  GOTO 140
                ENDIF
                WRITE (IOOUT,99007) cin(i)
              ELSE
                Nreac = MIN(Nreac+1,MAXR)
                Fox(Nreac) = cx15
                i = i + 1
                IF ( lcin(i).LT.0 ) Rname(Nreac) = cin(i)
                ifrmla = 0
                Nfla(Nreac) = 0
                Energy(Nreac) = 'lib'
                Enth(Nreac) = 0.
                Jray(Nreac) = 0
                Pecwt(Nreac) = -1.
                Rnum(Nreac,1) = 0.
                Rmw(Nreac) = 0.
                Rtemp(Nreac) = 0.
              ENDIF
            ENDIF
            GOTO 140
          ENDIF
C SORT AND STORE INPUT FROM 'PROB' DATASET
        ELSEIF ( code.EQ.'prob' ) THEN
          Case = ' '
          DO i = 1,MAXPV
            P(i) = 0.
            V(i) = 0.
          ENDDO
          DO i = 1,MAXT
            T(i) = 0.
          ENDDO
          P(1) = 1.
          Trace = 0.
          Lsave = 0
          R = Rr/4184.D0
          S0 = 0.
          hr = 1.D30
          ur = 1.D30
          Tp = .FALSE.
          Hp = .FALSE.
          Sp = .FALSE.
          Rkt = .FALSE.
          Shock = .FALSE.
          Detn = .FALSE.
          Vol = .FALSE.
          Ions = .FALSE.
          Eql = .FALSE.
          Froz = .FALSE.
          Fac = .FALSE.
          Debugf = .FALSE.
          Acat = 0.
          Ma = 0.
          Nfz = 1
          Nsub = 0
          Nsup = 0
          Npp = 0
          Tcest = 3800.
          DO i = 1,NCOL
            Pcp(i) = 0.
            Pcp(i+NCOL) = 0.
            Supar(i) = 0.
            Subar(i) = 0.
            Mach1(i) = 0.
            U1(i) = 0.
          ENDDO
          Gamma1 = 0.
          phi = .FALSE.
          eqrats = .FALSE.
          incd = .FALSE.
          refl = .FALSE.
          Shkdbg = .FALSE.
          Incdeq = .FALSE.
          Incdfz = .FALSE.
          Refleq = .FALSE.
          Reflfz = .FALSE.
          Np = 0
          Nt = 1
          Trnspt = .FALSE.
C PROCESS LITERAL VARIABLES IN 'PROB' DATASET THAT DO NOT HAVE
C ASSOCIATED NUMERICAL DATA.
          DO 160 i = 2,ncin
            IF ( lcin(i).LT.0 ) THEN
              DO j = i + 1,ncin
                IF ( lcin(j).EQ.i ) GOTO 160
              ENDDO
              cx15 = cin(i)
              cx2 = cx15(:2)
              cx3 = cx15(:3)
              cx4 = cx15(:4)
              IF ( cx4.EQ.'case' ) THEN
                Case = cin(i+1)
                lcin(i+1) = 0
              ELSEIF ( cx2.EQ.'tp'.OR.cx2.EQ.'pt' ) THEN
                Tp = .TRUE.
              ELSEIF ( cx2.EQ.'hp'.OR.cx2.EQ.'ph' ) THEN
                Hp = .TRUE.
              ELSEIF ( cx2.EQ.'sp'.OR.cx2.EQ.'ps' ) THEN
                Sp = .TRUE.
              ELSEIF ( cx2.EQ.'sv'.OR.cx2.EQ.'vs' ) THEN
                Sp = .TRUE.
                Vol = .TRUE.
              ELSEIF ( cx2.EQ.'uv'.OR.cx2.EQ.'vu' ) THEN
                Hp = .TRUE.
                Vol = .TRUE.
              ELSEIF ( cx2.EQ.'tv'.OR.cx2.EQ.'vt' ) THEN
                Tp = .TRUE.
                Vol = .TRUE.
              ELSEIF ( cx2.EQ.'ro'.OR.cx3.EQ.'rkt' ) THEN
                Rkt = .TRUE.
              ELSEIF ( cx3.EQ.'dbg'.OR.cx3.EQ.'deb' ) THEN
                Debugf = .TRUE.
                Shkdbg = .TRUE.
                Detdbg = .TRUE.
              ELSEIF ( cx3.EQ.'fac' ) THEN
                Rkt = .TRUE.
                Eql = .TRUE.
                Fac = .TRUE.
                Froz = .FALSE.
              ELSEIF ( cx2.EQ.'eq' ) THEN
                Eql = .TRUE.
              ELSEIF ( cx2.EQ.'fr'.OR.cx2.EQ.'fz' ) THEN
                Froz = .TRUE.
              ELSEIF ( cx2.EQ.'sh' ) THEN
                Shock = .TRUE.
              ELSEIF ( cx3.EQ.'inc' ) THEN
                Shock = .TRUE.
                incd = .TRUE.
                IF ( INDEX(cx15,'eq').GT.0 ) Eql = .TRUE.
                IF ( INDEX(cx15,'fr').GT.0 ) Froz = .TRUE.
                IF ( INDEX(cx15,'fz').GT.0 ) Froz = .TRUE.
              ELSEIF ( cx3.EQ.'ref' ) THEN
                Shock = .TRUE.
                refl = .TRUE.
                IF ( INDEX(cx15,'eq').GT.0 ) Eql = .TRUE.
                IF ( INDEX(cx15,'fr').GT.0 ) Froz = .TRUE.
                IF ( INDEX(cx15,'fz').GT.0 ) Froz = .TRUE.
              ELSEIF ( cx3.EQ.'det' ) THEN
                Detn = .TRUE.
              ELSEIF ( cx4.EQ.'ions' ) THEN
                Ions = .TRUE.
              ELSE
                WRITE (IOOUT,99002) cx15
              ENDIF
              lcin(i) = 0
            ENDIF
 160      CONTINUE
          iv = 2
          Nof = 0
          GOTO 200
        ELSEIF ( code(1:3).EQ.'end' ) THEN
          IF ( Shock ) THEN
            IF ( incd.AND.Froz ) Incdfz = .TRUE.
            IF ( incd.AND.Eql ) Incdeq = .TRUE.
            IF ( refl.AND.Froz ) Reflfz = .TRUE.
            IF ( refl.AND.Eql ) Refleq = .TRUE.
          ENDIF
          Hsub0 = DMIN1(hr,ur)
          Size = 0.
          IF ( hr.GT..9D30 ) hr = 0.D0
          IF ( ur.GT..9D30 ) ur = 0.D0
          IF ( Trnspt ) Viscns = .3125*DSQRT(1.E5*Boltz/(Pi*Avgdr))
          IF ( Siunit ) R = Rr/1000.
          IF ( Detn.OR.Shock ) Newr = .TRUE.
          IF ( .NOT.Short ) THEN
            WRITE (IOOUT,99008) Tp,(Hp.AND..NOT.Vol),Sp,(Tp.AND.Vol),
     &                      (Hp.AND.Vol),(Sp.AND.Vol),Detn,Shock,refl,
     &                      incd,Rkt,Froz,Eql,Ions,Siunit,Debugf,Shkdbg,
     &                      Detdbg,Trnspt
            IF ( T(1).GT.0. ) WRITE (IOOUT,99009) (T(jj),jj=1,Nt)
            WRITE (IOOUT,99010) Trace,S0,hr,ur
            IF ( Np.GT.0.AND.Vol ) WRITE (IOOUT,99011)
     &           (V(jj)*1.D-05,jj=1,Np)
          ENDIF
          IF ( Rkt ) THEN
            IF ( Nt.EQ.0 ) Hp = .TRUE.
            IF ( .NOT.Short ) THEN
              WRITE (IOOUT,99012) (P(jj),jj=1,Np)
              WRITE (IOOUT,99013) (Pcp(jj),jj=1,Npp)
              WRITE (IOOUT,99014) (Subar(i),i=1,Nsub)
              WRITE (IOOUT,99015) (Supar(i),i=1,Nsup)
              WRITE (IOOUT,99016) Nfz,Ma,Acat
            ENDIF
          ELSE
            IF ( .NOT.Vol.AND..NOT.Short ) WRITE (IOOUT,99017)
     &           (P(jj),jj=1,Np)
          ENDIF
          IF ( reacts ) CALL REACT
          IF ( Nreac.EQ.0.OR.Nlm.LE.0 ) THEN
            WRITE (IOOUT,99018)
            Caseok = .FALSE.
            WRITE (IOOUT,99025)
            GOTO 400
          ENDIF
          IF ( Nof.EQ.0 ) THEN
            Nof = 1
            Oxf(1) = 0.
            IF ( Wp(2).GT.0. ) THEN
              Oxf(1) = Wp(1)/Wp(2)
            ELSE
              Caseok = .FALSE.
              WRITE (IOOUT,99004)
              WRITE (IOOUT,99025)
              GOTO 400
            ENDIF
          ELSEIF ( phi.OR.eqrats ) THEN
            DO i = 1,Nof
              eratio = Oxf(i)
              IF ( eqrats ) THEN
                xyz = -eratio*Vmin(2) - Vpls(2)
                denmtr = eratio*Vmin(1) + Vpls(1)
              ELSE
                xyz = -Vmin(2) - Vpls(2)
                denmtr = eratio*(Vmin(1)+Vpls(1))
              ENDIF
              IF ( DABS(denmtr).LT.1.D-30 ) THEN
                Caseok = .FALSE.
                WRITE (IOOUT,99019) eratio
                WRITE (IOOUT,99025)
                GOTO 400
              ENDIF
              Oxf(i) = xyz/denmtr
            ENDDO
          ENDIF
          IF ( .NOT.Sp.AND..NOT.Tp.AND..NOT.Hp.AND..NOT.Rkt.AND.
     &         .NOT.Detn.AND..NOT.Shock ) THEN
            Caseok = .FALSE.
            WRITE (IOOUT,99020)
          ELSEIF ( Tp.AND.T(1).LE.0. ) THEN
            Caseok = .FALSE.
            WRITE (IOOUT,99021)
          ELSEIF ( Np.LE.0 ) THEN
            Caseok = .FALSE.
            WRITE (IOOUT,99022)
          ENDIF
          IF ( .NOT.(Caseok.AND.Nlm.GT.0) ) WRITE (IOOUT,99025)
          GOTO 400
        ELSE
          WRITE (IOOUT,99023)
        ENDIF
      ENDIF
      GOTO 100
C PROCESS NUMERICAL DATA FOLLOWING 'PROB' LITERALS
 200  in = 0
      nmix = 0
      ii = iv
      DO i = ii,ncin
        iv = i
        IF ( lcin(i).NE.0 ) THEN
          IF ( lcin(i).LT.0 ) THEN
            IF ( in.GT.0 ) GOTO 300
            in = i
          ELSE
            IF ( lcin(i).NE.in ) GOTO 300
            nmix = nmix + 1
            mix(nmix) = dpin(i)
            lcin(i) = 0
          ENDIF
        ENDIF
      ENDDO
 300  IF ( nmix.LE.0 ) THEN
        IF ( iv.LT.ncin ) GOTO 200
        GOTO 100
      ENDIF
      cx15 = cin(in)
      cx1 = cx15(:1)
      cx2 = cx15(:2)
      cx3 = cx15(:3)
      cx4 = cx15(:4)
      IF ( cx1.EQ.'t' ) THEN
        Nt = nmix
        IF ( nmix.GT.MAXMIX ) THEN
          Nt = MAXMIX
          WRITE (IOOUT,99024) 't',Nt
        ENDIF
        DO i = 1,Nt
          IF ( cx4.NE.'tces' ) THEN
            T(i) = mix(i)
            IF ( lcin(in).LT.-1 ) THEN
              IF ( INDEX(cx15,'r').GT.0 ) T(i) = T(i)/1.8D0
              IF ( INDEX(cx15,'c').GT.0 ) T(i) = T(i) + 273.15D0
              IF ( INDEX(cx15,'f').GT.0 ) T(i) = (T(i)-32.D0)
     &             /1.8D0 + 273.15D0
            ENDIF
          ENDIF
        ENDDO
      ELSEIF ( (cx2.EQ.'pc'.OR.cx2.EQ.'pi').AND.INDEX(cx15(3:15),'p')
     &         .GT.0.AND.INDEX(cx15,'psi').EQ.0 ) THEN
        Npp = nmix
        IF ( nmix.GT.2*NCOL ) THEN
          Npp = 2*NCOL
          WRITE (IOOUT,99024) 'pcp',Npp
        ENDIF
        DO i = 1,Npp
          Pcp(i) = mix(i)
        ENDDO
      ELSEIF ( cx1.EQ.'p'.AND.cx3.NE.'phi' ) THEN
        Np = nmix
        IF ( nmix.GT.MAXPV ) THEN
          Np = MAXPV
          WRITE (IOOUT,99024) 'p',Np
        ENDIF
        DO 350 i = 1,Np
          P(i) = mix(i)
          IF ( INDEX(cx15,'psi').NE.0 ) THEN
            P(i) = P(i)/14.696006D0
          ELSEIF ( INDEX(cx15,'mmh').NE.0 ) THEN
            P(i) = P(i)/760.D0
          ELSEIF ( INDEX(cx15,'atm').EQ.0 ) THEN
            GOTO 350
          ENDIF
          P(i) = P(i)*1.01325D0
 350    CONTINUE
      ELSEIF ( cx3.EQ.'rho' ) THEN
        xyz = 1.D02
        IF ( INDEX(cx15,'kg').NE.0 ) xyz = 1.D05
        Np = nmix
        IF ( nmix.GT.MAXPV ) THEN
          Np = MAXPV
          WRITE (IOOUT,99024) 'rho',Np
        ENDIF
        DO i = 1,Np
          V(i) = xyz/mix(i)
        ENDDO
      ELSEIF ( cx1.EQ.'v' ) THEN
        xyz = 1.D02
        IF ( INDEX(cx15,'kg').NE.0 ) xyz = 1.D05
        Np = nmix
        IF ( nmix.GT.MAXPV ) THEN
          Np = MAXPV
          WRITE (IOOUT,99024) 'v',Np
        ENDIF
        DO i = 1,Np
          V(i) = mix(i)*xyz
        ENDDO
      ELSEIF ( cx3.EQ.'nfz'.OR.cx3.EQ.'nfr' ) THEN
        Nfz = mix(1)
        Froz = .TRUE.
      ELSEIF ( cx4.EQ.'tces' ) THEN
        Tcest = mix(1)
      ELSEIF ( cx4.EQ.'trac' ) THEN
        Trace = mix(1)
      ELSEIF ( cx3.EQ.'s/r' ) THEN
        S0 = mix(1)
      ELSEIF ( cx3.EQ.'u/r'.OR.cx2.EQ.'ur' ) THEN
        ur = mix(1)
      ELSEIF ( cx3.EQ.'h/r'.OR.cx2.EQ.'hr' ) THEN
        hr = mix(1)
      ELSEIF ( cx2.EQ.'u1' ) THEN
        Nsk = nmix
        IF ( nmix.GT.NCOL ) THEN
          Nsk = NCOL
          WRITE (IOOUT,99024) 'u1',Nsk
        ENDIF
        DO i = 1,Nsk
          U1(i) = mix(i)
        ENDDO
      ELSEIF ( cx4.EQ.'mach' ) THEN
        Nsk = nmix
        IF ( nmix.GT.NCOL ) THEN
          Nsk = NCOL
          WRITE (IOOUT,99024) 'mach1',Nsk
        ENDIF
        DO i = 1,Nsk
          Mach1(i) = mix(i)
        ENDDO
      ELSEIF ( cx3.EQ.'sub' ) THEN
        Nsub = nmix
        IF ( nmix.GT.13 ) THEN
          Nsub = 13
          WRITE (IOOUT,99024) 'subar',Nsub
        ENDIF
        DO i = 1,Nsub
          Subar(i) = mix(i)
        ENDDO
      ELSEIF ( cx3.EQ.'sup' ) THEN
        Nsup = nmix
        IF ( nmix.GT.13 ) THEN
          Nsup = 13
          WRITE (IOOUT,99024) 'supar',Nsup
        ENDIF
        DO i = 1,Nsup
          Supar(i) = mix(i)
        ENDDO
      ELSEIF ( cx2.EQ.'ac' ) THEN
        Acat = mix(1)
      ELSEIF ( cx4.EQ.'mdot'.OR.cx2.EQ.'ma' ) THEN
        Ma = mix(1)
      ELSEIF ( cx4.EQ.'case' ) THEN
        Case = cin(in+1)
        lcin(in+1) = 0
      ELSEIF ( Nof.EQ.0.AND.
     &         (cx3.EQ.'phi'.OR.cx3.EQ.'o/f'.OR.cx3.EQ.'f/a'.OR.
     &         cx2.EQ.'%f'.OR.cx1.EQ.'r') ) THEN
        Nof = nmix
        IF ( nmix.GT.MAXMIX ) THEN
          Nof = MAXMIX
          WRITE (IOOUT,99024) 'o/f',Nof
        ENDIF
        DO k = 1,Nof
          Oxf(k) = mix(k)
        ENDDO
        IF ( cx3.EQ.'phi' ) THEN
          phi = .TRUE.
        ELSEIF ( cx1.EQ.'r' ) THEN
          eqrats = .TRUE.
        ELSEIF ( cx3.EQ.'f/a' ) THEN
          DO k = 1,Nof
            IF ( Oxf(k).GT.0. ) Oxf(k) = 1./Oxf(k)
          ENDDO
        ELSEIF ( cx4.EQ.'%fue' ) THEN
          DO k = 1,Nof
            IF ( Oxf(k).GT.0. ) Oxf(k) = (100.-Oxf(k))/Oxf(k)
          ENDDO
        ENDIF
      ELSE
        WRITE (IOOUT,99002) cx15
      ENDIF
      IF ( iv.GE.ncin ) GOTO 100
      GOTO 200
 400  RETURN
99001 FORMAT (/,/)
99002 FORMAT ('  WARNING!!  DID NOT RECOGNIZE ',A15,' (INPUT)'/)
99003 FORMAT (/' WARNING!!  LITERAL EXPECTED FOR ',A15,'(INPUT)')
99004 FORMAT (/' REACTANT AMOUNT MISSING (INPUT)')
99005 FORMAT (/' MOLES AND WEIGHT PERCENTS SHOULD NOT BE MIXED (INPUT)')
99006 FORMAT (/' REACTANT TEMPERATURE MISSING (INPUT) ')
99007 FORMAT (/' WARNING!! ',A15,' NOT RECOGNIZED (INPUT)')
99008 FORMAT (/' OPTIONS: TP=',L1,'  HP=',L1,'  SP=',L1,'  TV=',L1,
     &        '  UV=',L1,'  SV=',L1,'  DETN=',L1,'  SHOCK=',L1,
     &        '  REFL=',L1,'  INCD=',L1,/' RKT=',L1,'  FROZ=',L1,
     &        '  EQL=',L1,'  IONS=',L1,'  SIUNIT=',L1,'  DEBUGF=',L1,
     &        '  SHKDBG=',L1,'  DETDBG=',L1,'  TRNSPT=',L1)
99009 FORMAT (/' T,K =',7F11.4)
99010 FORMAT (/1p,' TRACE=',E9.2,'  S/R=',E13.6,'  H/R=',E13.6,'  U/R=',
     &        E13.6)
99011 FORMAT (/' SPECIFIC VOLUME,M**3/KG =',1p,(4E14.7))
99012 FORMAT (/' Pc,BAR =',7F13.6)
99013 FORMAT (/' Pc/P =',9F11.4)
99014 FORMAT (/' SUBSONIC AREA RATIOS =',(5F11.4))
99015 FORMAT (/' SUPERSONIC AREA RATIOS =',(5F11.4))
99016 FORMAT (/' NFZ=',i3,1p,'  Mdot/Ac=',e13.6,'  Ac/At=',e13.6)
99017 FORMAT (/' P,BAR =',7F13.6)
99018 FORMAT (/' ERROR IN REACTANTS DATASET (INPUT)')
99019 FORMAT (/' UNABLE TO PROCESS EQUIVALENCE RATIO =',E11.4,'(INPUT)')
99020 FORMAT (/' TYPE OF PROBLEM NOT SPECIFIED (INPUT)')
99021 FORMAT (/' ASSIGNED VALUES OF TEMPERATURE ARE MISSING IN prob',
     &        ' DATASET (INPUT)')
99022 FORMAT (/' ASSIGNED PRESSURE (OR DENSITY) MISSING IN prob',
     &        ' DATASET (INPUT)')
99023 FORMAT (/' WARNING!!  A KEYWORD IS MISSING (INPUT)')
99024 FORMAT (/' NOTE!! MAXIMUM NUMBER OF ASSIGNED ',A5,' VALUES IS',I3,
     &        ' (INPUT)',/)
99025 FORMAT (/' FATAL ERROR IN DATASET (INPUT)')
      END
      SUBROUTINE MATRIX
C***********************************************************************
C SET UP ITERATION OR DERIVATIVE MATRIX.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER i,iq,iq2,iq3,isym,j,k,kk,kmat
      REAL*8 energyl,f,h,ss,sss,term,term1
      SAVE energyl,f,h,i,iq,iq2,iq3,isym,j,k,kk,kmat,ss,sss,term,term1
C
      iq = Nlm + Npr
      Iq1 = iq + 1
      iq2 = Iq1 + 1
      iq3 = iq2 + 1
      kmat = iq3
      IF ( .NOT.Convg.AND.Tp ) kmat = iq2
      Imat = kmat - 1
C CLEAR MATRIX STORAGES TO ZERO
      DO i = 1,Imat
        DO k = 1,kmat
          G(i,k) = 0.0D0
        ENDDO
      ENDDO
      G(iq2,Iq1) = 0.D0
      sss = 0.D0
      Hsum(Npt) = 0.D0
C BEGIN SET-UP OF ITERATION OR DERIVATIVE MATRIX
      DO j = 1,Ng
        Mu(j) = H0(j) - S(j) + Enln(j) + Tm
        IF ( En(j,Npt).NE.0.D0 ) THEN
          h = H0(j)*En(j,Npt)
          f = Mu(j)*En(j,Npt)
          ss = h - f
          term1 = h
          IF ( kmat.EQ.iq2 ) term1 = f
          DO i = 1,Nlm
            IF ( A(i,j).NE.0. ) THEN
              term = A(i,j)*En(j,Npt)
              DO k = i,Nlm
                G(i,k) = G(i,k) + A(k,j)*term
              ENDDO
              G(i,Iq1) = G(i,Iq1) + term
              G(i,iq2) = G(i,iq2) + A(i,j)*term1
              IF ( .NOT.(Convg.OR.Tp) ) THEN
                G(i,iq3) = G(i,iq3) + A(i,j)*f
                IF ( Sp ) G(iq2,i) = G(iq2,i) + A(i,j)*ss
              ENDIF
            ENDIF
          ENDDO
          IF ( kmat.NE.iq2 ) THEN
            IF ( Convg.OR.Hp ) THEN
              G(iq2,iq2) = G(iq2,iq2) + H0(j)*h
              IF ( .NOT.Convg ) THEN
                G(iq2,iq3) = G(iq2,iq3) + H0(j)*f
                G(Iq1,iq3) = G(Iq1,iq3) + f
              ENDIF
            ELSE
              G(iq2,Iq1) = G(iq2,Iq1) + ss
              G(iq2,iq2) = G(iq2,iq2) + H0(j)*ss
              G(iq2,iq3) = G(iq2,iq3) + Mu(j)*ss
              G(Iq1,iq3) = G(Iq1,iq3) + f
            ENDIF
          ENDIF
          G(Iq1,iq2) = G(Iq1,iq2) + term1
        ENDIF
      ENDDO
C CONDENSED SPECIES
      IF ( Npr.NE.0 ) THEN
        DO k = 1,Npr
          j = Jcond(k)
          kk = Nlm + k
          Mu(j) = H0(j) - S(j)
          DO i = 1,Nlm
            G(i,kk) = A(i,j)
            G(i,kmat) = G(i,kmat) - A(i,j)*En(j,Npt)
          ENDDO
          G(kk,iq2) = H0(j)
          G(kk,kmat) = Mu(j)
          Hsum(Npt) = Hsum(Npt) + H0(j)*En(j,Npt)
          IF ( Sp ) THEN
            sss = sss + S(j)*En(j,Npt)
            G(iq2,kk) = S(j)
          ENDIF
        ENDDO
      ENDIF
      sss = sss + G(iq2,Iq1)
      Hsum(Npt) = Hsum(Npt) + G(Iq1,iq2)
      G(Iq1,Iq1) = Sumn - Enn
C REFLECT SYMMETRIC PORTIONS OF THE MATRIX
      isym = Iq1
      IF ( Hp.OR.Convg ) isym = iq2
      DO i = 1,isym
CDIR$ IVDEP
        DO j = i,isym
          G(j,i) = G(i,j)
        ENDDO
      ENDDO
C COMPLETE THE RIGHT HAND SIDE
      IF ( .NOT.Convg ) THEN
        DO i = 1,Nlm
          G(i,kmat) = G(i,kmat) + B0(i) - G(i,Iq1)
        ENDDO
        G(Iq1,kmat) = G(Iq1,kmat) + Enn - Sumn
C COMPLETE ENERGY ROW AND TEMPERATURE COLUMN
        IF ( kmat.NE.iq2 ) THEN
          IF ( Sp ) energyl = S0 + Enn - Sumn - sss
          IF ( Hp ) energyl = Hsub0/Tt - Hsum(Npt)
          G(iq2,iq3) = G(iq2,iq3) + energyl
          G(iq2,iq2) = G(iq2,iq2) + Cpsum
        ENDIF
      ELSE
        IF ( Pderiv ) THEN
C PDERIV = .TRUE.-- SET UP MATRIX TO SOLVE FOR DLVPT
          G(Iq1,iq2) = Enn
          DO i = 1,iq
            G(i,iq2) = G(i,Iq1)
          ENDDO
        ENDIF
        G(iq2,iq2) = G(iq2,iq2) + Cpsum
      ENDIF
      IF ( Vol.AND..NOT.Convg ) THEN
C CONSTANT VOLUME MATRIX
        IF ( kmat.EQ.iq2 ) THEN
          DO i = 1,iq
            G(i,Iq1) = G(i,iq2)
          ENDDO
        ELSE
CDIR$ IVDEP
          DO i = 1,iq
            G(Iq1,i) = G(iq2,i) - G(Iq1,i)
            G(i,Iq1) = G(i,iq2) - G(i,Iq1)
            G(i,iq2) = G(i,iq3)
          ENDDO
          G(Iq1,Iq1) = G(iq2,iq2) - G(Iq1,iq2) - G(iq2,Iq1)
          G(Iq1,iq2) = G(iq2,iq3) - G(Iq1,iq3)
          IF ( Hp ) G(Iq1,iq2) = G(Iq1,iq2) + Enn
        ENDIF
        kmat = Imat
        Imat = Imat - 1
      ENDIF
      END
      SUBROUTINE NEWOF
C***********************************************************************
C CALCULATE NEW VALUES OF B0 AND HSUB0 FOR NEW OF RATIO
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER i,j
      REAL*8 assval,bigb,bratio,dbi,smalb,tem,v1,v2
      REAL*8 DABS,DLOG
      SAVE assval,bigb,bratio,dbi,i,j,smalb,tem,v1,v2
C
      IF ( .NOT.Short ) WRITE (IOOUT,99001) Oxfl
      Eqrat = 0.
      tem = Oxfl + 1.
      v2 = (Oxfl*Vmin(1)+Vmin(2))/tem
      v1 = (Oxfl*Vpls(1)+Vpls(2))/tem
      IF ( v2.NE.0. ) Eqrat = DABS(v1/v2)
      DO i = 1,Nlm
        B0(i) = (Oxfl*B0p(i,1)+B0p(i,2))/tem
        dbi = DABS(B0(i))
        IF ( i.EQ.1 ) THEN
          bigb = dbi
          smalb = dbi
        ELSEIF ( dbi.NE.0. ) THEN
          IF ( dbi.LT.smalb ) smalb = dbi
          IF ( dbi.GT.bigb ) bigb = dbi
        ENDIF
      ENDDO
      Bcheck = bigb*.000001D0
C CALCUALTE MOLECULAR WEIGHT OF TOTAL REACTANT, WMIX.
      IF ( Am(1).NE.0.0.AND.Am(2).NE.0.0 ) THEN
        Wmix = (Oxfl+1.)*Am(1)*Am(2)/(Am(1)+Oxfl*Am(2))
      ELSE
        Wmix = Am(2)
        IF ( Am(2).EQ.0.0 ) Wmix = Am(1)
      ENDIF
      Npt = 1
C IF ASSIGNED U OR H NOT GIVEN IN PROB DATA, INITIAL HSUB0 = 1.D30
      IF ( Size.EQ.0. ) assval = Hsub0
      IF ( assval.GE.1.D30 ) Hsub0 = (Oxfl*Hpp(1)+Hpp(2))/tem
C NOTE THAT "BRATIO" IS "BRATIO" IN SEC 3.2 IN RP-1311.
      bratio = smalb/bigb
      Size = 18.420681D0
      IF ( bratio.LT.1.D-5 ) Size = DLOG(1000.D0/bratio)
      Jsol = 0
      Jliq = 0
      IF ( .NOT.Short ) THEN
        WRITE (IOOUT,99002)
        IF ( Vol ) WRITE (IOOUT,99003)
        IF ( .NOT.Vol ) WRITE (IOOUT,99004)
        WRITE (IOOUT,99005) Hpp(2),Hpp(1),Hsub0
        WRITE (IOOUT,99006)
      ENDIF
      DO i = 1,Nlm
        j = Jcm(i)
        IF ( .NOT.Short ) WRITE (IOOUT,99007) Prod(j),B0p(i,2),B0p(i,1),
     &                           B0(i)
      ENDDO
      RETURN
99001 FORMAT (/' O/F = ',F10.6)
99002 FORMAT (/,23X,'EFFECTIVE FUEL',5X,'EFFECTIVE OXIDANT',8X,
     &        'MIXTURE')
99003 FORMAT (' INTERNAL ENERGY',11X,'u(2)/R',14X,'u(1)/R',14X,'u0/R')
99004 FORMAT (' ENTHALPY',18X,'h(2)/R',14X,'h(1)/R',15X,'h0/R')
99005 FORMAT (' (KG-MOL)(K)/KG',4X,E18.8,2E20.8)
99006 FORMAT (/' KG-FORM.WT./KG',13X,'bi(2)',15X,'bi(1)',15X,'b0i')
99007 FORMAT (1X,A16,3E20.8)
      END
      SUBROUTINE OUT1
C***********************************************************************
C OUT1 WRITES REACTANT AND FUEL-OXIDANT RATIO INFORMATION.
C ENTRY OUT2 WRITES THERMODYNAMIC PROPERTIES.
C ENTRY OUT3 WRITES MOLE FRACTIONS.
C ENTRY OUT4 WRITES TRANSPORT PROPERTIES.
C
C NOTE - ROCKET, SHOCK, AND DETON PROBLEMS HAVE ADDITIONAL OUTPUT.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*15 fc,fgi,fh,fp,frh,fs,fu
      CHARACTER*4 mamo
      INTEGER i,im,ione,j,k,kin,m,mcond,mcondf,mcp,mdvp,mdvt,meq,mfa,
     &        mg,mgam,mh,mie,mm,mmw,mof,mp,mpf,mph,mpn,mpnf,mrho,ms,
     &        mson,mt,mvis,mxx(24),n,notuse
      INTEGER INDEX
      LOGICAL kok
      REAL*8 pfactor,pfuel,phi,rho,tem,tra,vnum
      SAVE fc,fgi,fh,fp,frh,fs,fu,i,im,ione,j,k,kin,kok,m,
     &  mamo,mcond,mcondf,mcp,mdvp,mdvt,meq,mfa,mg,mgam,mh,mie,mm,mmw,
     &  mof,mp,mpf,mph,mpn,mpnf,mrho,ms,mson,mt,mvis,mxx,n,notuse,
     &  pfactor,pfuel,phi,rho,tem,tra,vnum
C
      EQUIVALENCE (mxx(1),mp)
      EQUIVALENCE (mxx(2),mt)
      EQUIVALENCE (mxx(3),mrho)
      EQUIVALENCE (mxx(4),mh)
      EQUIVALENCE (mxx(5),mie)
      EQUIVALENCE (mxx(6),mg)
      EQUIVALENCE (mxx(7),ms)
      EQUIVALENCE (mxx(8),mm)
      EQUIVALENCE (mxx(9),mcp)
      EQUIVALENCE (mxx(10),mgam)
      EQUIVALENCE (mxx(11),mson)
      EQUIVALENCE (mxx(12),mcond)
      EQUIVALENCE (mxx(13),mvis)
      EQUIVALENCE (mxx(14),mpn)
      EQUIVALENCE (mxx(15),mpf)
      EQUIVALENCE (mxx(16),mof)
      EQUIVALENCE (mxx(17),mph)
      EQUIVALENCE (mxx(18),meq)
      EQUIVALENCE (mxx(19),mfa)
      EQUIVALENCE (mxx(20),mmw)
      EQUIVALENCE (mxx(21),mdvt)
      EQUIVALENCE (mxx(22),mdvp)
      EQUIVALENCE (mxx(23),mcondf)
      EQUIVALENCE (mxx(24),mpnf)
      WRITE (IOOUT,99001) Case
      IF ( Moles ) THEN
        WRITE (IOOUT,99002) '   MOLES   '
        IF ( .NOT.Siunit ) WRITE (IOOUT,99003)
        IF ( Siunit ) WRITE (IOOUT,99004)
      ELSE
        WRITE (IOOUT,99002) 'WT FRACTION'
        IF ( .NOT.Siunit ) WRITE (IOOUT,99005)
        IF ( Siunit ) WRITE (IOOUT,99006)
      ENDIF
      DO n = 1,Nreac
        WRITE (IOOUT,99007) Fox(n),Rname(n),Pecwt(n),Enth(n)*R,Rtemp(n)
      ENDDO
      phi = 0.
      tem = (Vpls(1)+Vmin(1))*Oxfl
      IF ( ABS(tem).GE.1.D-3 ) phi = -(Vmin(2)+Vpls(2))/tem
      IF ( Fox(1).EQ.'NAME' ) THEN
        pfuel = 0.
      ELSE
        pfuel = 100.D0/(1.D0+Oxfl)
      ENDIF
      IF ( Rh(1).NE.0..OR.Rh(2).NE.0. ) THEN
        IF ( Rh(1).EQ.0..OR.Rh(2).EQ.0. ) THEN
          rho = MAX(Rh(1),Rh(2))
        ELSE
          rho = (Oxfl+1.)*Rh(1)*Rh(2)/(Rh(1)+Oxfl*Rh(2))
        ENDIF
        IF ( Siunit ) THEN
          rho = rho*1000.D0
          WRITE (IOOUT,99021) rho
        ELSE
          WRITE (IOOUT,99022) rho
        ENDIF
      ENDIF
      WRITE (IOOUT,99008) Oxfl,pfuel,Eqrat,phi
      RETURN
C***********************************************************************
      ENTRY OUT2
      ione = 0
      IF ( Rkt.AND..NOT.Page1 ) THEN
        ione = 2
        IF ( Iopt.NE.0 ) ione = 3
      ENDIF
C SET MXX ARRAY FOR PLOTTING PARAMETERS
      DO i = 1,24
        mxx(i) = 0
      ENDDO
      DO 100 i = 1,Nplt
        IF ( INDEX(Pltvar(i)(2:),'1').EQ.0 ) THEN
          IF ( INDEX(Pltvar(i)(1:),'dlnt').NE.0 ) THEN
            mdvt = i
          ELSEIF ( INDEX(Pltvar(i)(1:),'dlnp').NE.0 ) THEN
            mdvp = i
          ELSEIF ( Pltvar(i)(:4).EQ.'pran' ) THEN
            IF ( INDEX(Pltvar(i)(3:),'fz').NE.0 .OR.
     &           INDEX(Pltvar(i)(3:),'fr').NE.0 ) THEN
              mpnf = i
            ELSE
              mpn = i
            ENDIF
          ELSEIF ( Pltvar(i)(:4).EQ.'cond' ) THEN
            IF ( INDEX(Pltvar(i)(3:),'fz').NE.0 .OR.
     &           INDEX(Pltvar(i)(3:),'fr').NE.0 ) THEN
              mcondf = i
            ELSE
              mcond = i
            ENDIF
          ELSEIF ( Pltvar(i)(:3).EQ.'phi' ) THEN
            mph = i
          ELSEIF ( Pltvar(i)(:2).EQ.'p ' ) THEN
            mp = i
          ELSEIF ( Pltvar(i)(:1).EQ.'t' ) THEN
            mt = i
          ELSEIF ( Pltvar(i)(:3).EQ.'rho' ) THEN
            mrho = i
          ELSEIF ( Pltvar(i)(:1).EQ.'h' ) THEN
            mh = i
          ELSEIF ( Pltvar(i)(:1).EQ.'u' ) THEN
            mie = i
          ELSEIF ( Pltvar(i)(:3).EQ.'gam' ) THEN
            mgam = i
          ELSEIF ( Pltvar(i)(:3).EQ.'son' ) THEN
            mson = i
          ELSEIF ( Pltvar(i)(:2).EQ.'g ' ) THEN
            mg = i
          ELSEIF ( Pltvar(i)(:2).EQ.'s ' ) THEN
            ms = i
          ELSEIF ( Pltvar(i)(:1).EQ.'m'.AND.Pltvar(i)(:2).NE.'ma' ) THEN
            IF ( .NOT.Gonly.AND.Pltvar(i)(:2).EQ.'mw' ) THEN
              mmw = i
            ELSE
              mm = i
            ENDIF
          ELSEIF ( Pltvar(i)(:2).EQ.'cp' ) THEN
            mcp = i
          ELSEIF ( Pltvar(i)(:3).EQ.'vis' ) THEN
            mvis = i
          ELSEIF ( Pltvar(i)(:3).EQ.'o/f' ) THEN
            mof = i
          ELSEIF ( Pltvar(i)(:2).EQ.'%f' ) THEN
            mpf = i
          ELSEIF ( Pltvar(i)(:3).EQ.'f/a' ) THEN
            mfa = i
          ELSEIF ( Pltvar(i)(:1).EQ.'r' ) THEN
            meq = i
          ENDIF
        ENDIF
 100  CONTINUE
      DO i = Iplt + 1,Iplt + Npt
        IF ( mof.GT.0 ) Pltout(i,mof) = Oxfl
        IF ( mpf.GT.0 ) Pltout(i,mpf) = pfuel
        IF ( mph.GT.0 ) Pltout(i,mph) = phi
        IF ( mfa.GT.0 ) Pltout(i,mfa) = 1.D0/Oxfl
        IF ( meq.GT.0 ) Pltout(i,meq) = Eqrat
      ENDDO
      IF ( Siunit ) THEN
        pfactor = 1.D0
        fp = 'P, BAR'
        vnum = 1.D05
        frh = 'RHO, KG/CU M'
        fh = 'H, KJ/KG'
        fu = 'U, KJ/KG'
        fgi = 'G, KJ/KG'
        fs = 'S, KJ/(KG)(K)'
        fc = 'Cp, KJ/(KG)(K)'
      ELSE
        pfactor = 1.D0/1.01325D0
        fp = 'P, ATM'
        vnum = 100.D0
        frh = 'RHO, G/CC'
        fh = 'H, CAL/G'
        fu = 'U, CAL/G'
        fgi = 'G, CAL/G'
        fs = 'S, CAL/(G)(K)'
        fc = 'Cp, CAL/(G)(K)'
      ENDIF
      Fmt(4) = Fmt(6)
C PRESSURE
      CALL VARFMT(Ppp)
      DO i = 1,Npt
        X(i) = Ppp(i)*pfactor
        IF ( Nplt.NE.0.AND.i.GT.ione ) THEN
          IF ( mp.GT.0 ) Pltout(i+Iplt-ione,mp) = X(i)
          IF ( mt.GT.0 ) Pltout(i+Iplt-ione,mt) = Ttt(i)
        ENDIF
      ENDDO
      WRITE (IOOUT,Fmt) fp,(X(j),j=1,Npt)
C TEMPERATURE
      Fmt(4) = '13'
      Fmt(5) = ' '
      Fmt(7) = '2,'
      WRITE (IOOUT,Fmt) 'T, K            ',(Ttt(j),j=1,Npt)
C DENSITY
      DO i = 1,Npt
        IF ( Vlm(i).NE.0. ) X(i) = vnum/Vlm(i)
        IF ( Nplt.NE.0.AND.i.GT.ione.AND.mrho.GT.0 )
     &       Pltout(i+Iplt-ione,mrho) = X(i)
      ENDDO
      CALL EFMT(Fmt(4),frh,X)
C ENTHALPY
      DO i = 1,Npt
        X(i) = Hsum(i)*R
        IF ( Nplt.NE.0.AND.i.GT.ione.AND.mh.GT.0 )
     &       Pltout(i+Iplt-ione,mh) = X(i)
      ENDDO
      Fmt(4) = Fmt(6)
      CALL VARFMT(X)
      WRITE (IOOUT,Fmt) fh,(X(j),j=1,Npt)
C INTERNAL ENERGY
      DO i = 1,Npt
        X(i) = (Hsum(i)-Ppp(i)*Vlm(i)/Rr)*R
        IF ( Nplt.NE.0.AND.i.GT.ione.AND.mie.GT.0 )
     &       Pltout(i+Iplt-ione,mie) = X(i)
      ENDDO
      CALL VARFMT(X)
      WRITE (IOOUT,Fmt) fu,(X(j),j=1,Npt)
C GIBBS ENERGY
      DO i = 1,Npt
        X(i) = (Hsum(i)-Ttt(i)*Ssum(i))*R
        IF ( Nplt.NE.0.AND.i.GT.ione ) THEN
          IF ( mg.GT.0 ) Pltout(i+Iplt-ione,mg) = X(i)
          IF ( mm.GT.0 ) Pltout(i+Iplt-ione,mm) = Wm(i)
          IF ( mmw.GT.0 ) Pltout(i+Iplt-ione,mmw) = 1.D0/Totn(i)
          IF ( ms.GT.0 ) Pltout(i+Iplt-ione,ms) = Ssum(i)*R
          IF ( mcp.GT.0 ) Pltout(i+Iplt-ione,mcp) = Cpr(i)*R
          IF ( mgam.GT.0 ) Pltout(i+Iplt-ione,mgam) = Gammas(i)
          IF ( mdvt.GT.0 ) Pltout(i+Iplt-ione,mdvt) = Dlvtp(i)
          IF ( mdvp.GT.0 ) Pltout(i+Iplt-ione,mdvp) = Dlvpt(i)
        ENDIF
      ENDDO
      CALL VARFMT(X)
      WRITE (IOOUT,Fmt) fgi,(X(j),j=1,Npt)
C ENTROPY
      Fmt(4) = '13'
      Fmt(5) = ' '
      Fmt(7) = '4,'
      WRITE (IOOUT,Fmt) fs,(Ssum(j)*R,j=1,Npt)
      WRITE (IOOUT,99009)
C MOLECULAR WEIGHT
      Fmt(7) = '3,'
      WRITE (IOOUT,Fmt) 'M, (1/n)        ',(Wm(j),j=1,Npt)
      IF ( .NOT.Gonly ) WRITE (IOOUT,Fmt) 'MW, MOL WT      ',
     &                                (1.D0/Totn(j),j=1,Npt)
C (DLV/DLP)T
      Fmt(7) = '5,'
      IF ( Eql ) WRITE (IOOUT,Fmt) '(dLV/dLP)t      ',(Dlvpt(j),j=1,Npt)
C (DLV/DLT)P
      Fmt(7) = '4,'
      IF ( Eql ) WRITE (IOOUT,Fmt) '(dLV/dLT)p      ',(Dlvtp(j),j=1,Npt)
C HEAT CAPACITY
      WRITE (IOOUT,Fmt) fc,(Cpr(j)*R,j=1,Npt)
C GAMMA(S)
      Fmt(7) = '4,'
      WRITE (IOOUT,Fmt) 'GAMMAs          ',(Gammas(j),j=1,Npt)
C SONIC VELOCITY
      Fmt(7) = '1,'
      DO i = 1,Npt
        Sonvel(i) = (Rr*Gammas(i)*Ttt(i)/Wm(i))**.5
        IF ( Nplt.NE.0.AND.i.GT.ione.AND.mson.GT.0 )
     &       Pltout(i+Iplt-ione,mson) = Sonvel(i)
      ENDDO
      WRITE (IOOUT,Fmt) 'SON VEL,M/SEC   ',(Sonvel(j),j=1,Npt)
      RETURN
C***********************************************************************
      ENTRY OUT3
      tra = 5.D-6
      IF ( Trace.NE.0. ) tra = Trace
C MASS OR MOLE FRACTIONS 
      IF ( Massf ) THEN
        mamo = 'MASS'
      ELSE
        mamo = 'MOLE'
      ENDIF
      IF ( Eql ) THEN
        WRITE (IOOUT,99010) mamo
        notuse = 0
        DO k = 1,Ngc
          kok = .TRUE.
          IF ( k.GT.Ng.AND.k.LT.Ngc.AND.Prod(k).EQ.Prod(k+1) ) THEN
            kok = .FALSE.
            im = 0
            GOTO 120
          ENDIF
          DO m = 1,Nplt
            im = 0
            IF ( Pltvar(m).EQ.Prod(k).OR.'*'//Pltvar(m).EQ.Prod(k) )
     &           THEN
              im = m
              GOTO 120
            ENDIF
          ENDDO
 120      kin = 0
          DO i = 1,Npt
            IF ( Massf ) THEN
              tem = Mw(k)
            ELSE
              tem = 1.D0/Totn(i)
            ENDIF
            IF ( k.LE.Ng ) THEN
              X(i) = En(k,i)*tem
            ELSE
              IF ( Prod(k).NE.Prod(k-1) ) X(i) = 0.D0
              IF ( En(k,i).GT.0.D0 ) X(i) = En(k,i)*tem
            ENDIF
            IF ( Nplt.NE.0.AND.i.GT.ione.AND.im.GT.0 )
     &           Pltout(i+Iplt-ione,im) = X(i)
            IF ( kok.AND.X(i).GE.tra ) kin = 1
          ENDDO
          IF ( kin.EQ.1 ) THEN
            IF ( Trace.EQ.0. ) THEN
              WRITE (IOOUT,99011) Prod(k),(X(i),i=1,Npt)
            ELSE
              CALL EFMT(Fmt(4),Prod(k),X)
            ENDIF
            IF ( Prod(k).EQ.Omit(notuse) ) notuse = notuse - 1
          ELSEIF ( Prod(k).NE.Prod(k-1) ) THEN
            notuse = notuse + 1
            Omit(notuse) = Prod(k)
          ENDIF
        ENDDO
      ENDIF
      WRITE (IOOUT,99012) Tg(4)
      IF ( .NOT.Short ) THEN
        WRITE (IOOUT,99013) mamo,tra
        WRITE (IOOUT,99014) (Omit(i),i=1,notuse)
      ENDIF
      IF ( .NOT.Moles ) WRITE (IOOUT,99015)
      GOTO 200
C***********************************************************************
      ENTRY OUT4
      WRITE (IOOUT,99009)
      WRITE (IOOUT,99016)
      IF ( Siunit ) THEN
        WRITE (IOOUT,99018)
      ELSE
        WRITE (IOOUT,99017)
      ENDIF
C TRANSPORT PROPERTIES
      Fmt(4) = Fmt(6)
      IF ( Nplt.GT.0 ) THEN
        DO i = 1,Npt
          IF ( i.GT.ione ) THEN
            IF ( mvis.GT.0 ) Pltout(i+Iplt-ione,mvis) = Vis(i)
            IF ( mcond.GT.0 ) Pltout(i+Iplt-ione,mcond) = Coneql(i)
            IF ( mpn.GT.0 ) Pltout(i+Iplt-ione,mpn) = Preql(i)
            IF ( mcondf.GT.0 ) Pltout(i+Iplt-ione,mcondf) = Confro(i)
            IF ( mpnf.GT.0 ) Pltout(i+Iplt-ione,mpnf) = Prfro(i)
          ENDIF
        ENDDO
      ENDIF
      CALL VARFMT(Vis)
      WRITE (IOOUT,Fmt) 'VISC,MILLIPOISE',(Vis(j),j=1,Npt)
      Fmt(4) = '13'
      Fmt(5) = ' '
      Fmt(7) = '4,'
      IF ( Eql ) THEN
        WRITE (IOOUT,99019)
C SPECIFIC HEAT
        WRITE (IOOUT,Fmt) fc,(Cpeql(j),j=1,Npt)
C CONDUCTIVITY
        WRITE (IOOUT,Fmt) 'CONDUCTIVITY    ',(Coneql(j),j=1,Npt)
C PRANDTL NUMBER
        WRITE (IOOUT,Fmt) 'PRANDTL NUMBER  ',(Preql(j),j=1,Npt)
      ENDIF
      WRITE (IOOUT,99020)
C SPECIFIC HEAT
      WRITE (IOOUT,Fmt) fc,(Cpfro(j),j=1,Npt)
C CONDUCTIVITY
      WRITE (IOOUT,Fmt) 'CONDUCTIVITY    ',(Confro(j),j=1,Npt)
C PRANDTL NUMBER
      WRITE (IOOUT,Fmt) 'PRANDTL NUMBER  ',(Prfro(j),j=1,Npt)
 200  RETURN
99001 FORMAT (' CASE = ',a15)
99002 FORMAT (/13X,'REACTANT',20x,a11,'      ENERGY',6x,'TEMP')
99003 FORMAT (57X,' CAL/MOL ',6x,'K')
99004 FORMAT (57X,'KJ/KG-MOL',6x,'K')
99005 FORMAT (42X,'(SEE NOTE)      CAL/MOL       K  ')
99006 FORMAT (42X,'(SEE NOTE)     KJ/KG-MOL      K  ')
99007 FORMAT (1x,a8,4x,a15,11x,f12.7,f14.3,f11.3)
99008 FORMAT (/' O/F=',F11.5,2X,'%FUEL=',F10.6,2X,'R,EQ.RATIO=',F9.6,2X,
     &        'PHI,EQ.RATIO=',F9.6)
99009 FORMAT ()
99010 FORMAT (/1x,A4,' FRACTIONS'/)
99011 FORMAT (1x,A15,F9.5,12F9.5)
99012 FORMAT (/'  * THERMODYNAMIC PROPERTIES FITTED TO',F7.0,'K')
99013 FORMAT (/'    PRODUCTS WHICH WERE CONSIDERED BUT WHOSE ',A4,
     &        ' FRACTIONS',/'    WERE LESS THAN',1PE13.6,
     &        ' FOR ALL ASSIGNED CONDITIONS'/)
99014 FORMAT (5(1x,A15))
99015 FORMAT (/' NOTE. WEIGHT FRACTION OF FUEL IN TOTAL FUELS AND OF',
     &        ' OXIDANT IN TOTAL OXIDANTS')
99016 FORMAT (' TRANSPORT PROPERTIES (GASES ONLY)')
99017 FORMAT ('   CONDUCTIVITY IN UNITS OF MILLICALORIES/(CM)(K)(SEC)'/)
99018 FORMAT ('   CONDUCTIVITY IN UNITS OF MILLIWATTS/(CM)(K)'/)
99019 FORMAT (/'  WITH EQUILIBRIUM REACTIONS'/)
99020 FORMAT (/'  WITH FROZEN REACTIONS'/)
99021 FORMAT (/' REACTANT DENSITY=',F8.2,' KG/CU M')
99022 FORMAT (/' REACTANT DENSITY=',F8.4,' G/CC')
      END
      SUBROUTINE REACT
C***********************************************************************
C READ AND PROCESS REACTANT RECORDS.  CALLED FROM SUBROUTINE INPUT.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*6 date
      CHARACTER*2 el(5)
      CHARACTER*15 sub
      INTEGER i,icf,ifaz,ifrmla,itot,j,jj,k,kk,kr,l,n,nall,nint,nj,
     &        ntgas,ntot
      LOGICAL fuel,rcoefs,wdone(2),hok
      REAL*8 bb(5),dat(35),dift,eform,pcwt,rcf(9,3),rm,t1,t2
      REAL*8 DABS,DLOG,t1save,t2save
      SAVE bb,dat,date,dift,eform,el,fuel,i,icf,ifaz,ifrmla,itot,j,jj,k,
     &  kk,kr,l,n,nall,nint,nj,ntgas,ntot,pcwt,rcf,rcoefs,rm,sub,t1,t2,
     &  wdone
C
      DO k = 1,2
        wdone(k) = .FALSE.
        Wp(k) = 0.
        Hpp(k) = 0.
        Vpls(k) = 0.
        Vmin(k) = 0.
        Am(k) = 0.
        Rh(k) = 0.
        DO j = 1,MAXEL
          Elmt(j) = ' '
          B0p(j,k) = 0.
        ENDDO
      ENDDO
      DO i = 1,MAXEL
        dat(i) = 0.
      ENDDO
C IF OXIDANT, KR = 1
C IF FUEL, KR = 2
      DO n = 1,Nreac
        hok = .false.
        t1save = 20000.d0
        t2save = 0.d0
        rcoefs = .TRUE.
        IF ( Energy(n).EQ.'lib'.OR.Rnum(n,1).EQ.0. ) THEN
          Tt = Rtemp(n)
          REWIND IOTHM
          READ (IOTHM) Tg,ntgas,ntot,nall
          DO 20 itot = 1,nall
            IF ( itot.LE.ntot ) THEN
              icf = 3
              IF ( itot.GT.ntgas ) icf = 1
              READ (IOTHM) sub,nint,date,(el(j),bb(j),j=1,5),ifaz,t1,t2,
     &                     rm,((rcf(i,j),i=1,9),j=1,icf)
            ELSE
              READ (IOTHM) sub,nint,date,(el(j),bb(j),j=1,5),ifaz,t1,t2,
     &                     rm,eform
              IF ( nint.GT.0 ) READ (IOTHM) ((rcf(i,j),i=1,9),j=1,nint)
            ENDIF
            IF ( sub.EQ.Rname(n).OR.sub.EQ.'*'//Rname(n) ) THEN
              IF ( nint.EQ.0 ) THEN
                rcoefs = .FALSE.
                hok = .true.
                Enth(n) = eform*1000.D0/Rr
                IF ( Tt.EQ.0 ) THEN
                  Tt = t1
                  Rtemp(n) = t1
                ELSE
                  dift = DABS(Tt-t1)
                  IF ( dift.GT.01d0 ) THEN
                    IF ( dift.GT.10.d0 ) THEN
                      WRITE (IOOUT,99001) Rname(n),t1,Tt
                      Nlm = 0
                      hok = .false.
                      GOTO 200
                    ELSE
                      WRITE (IOOUT,99002) Rname(n),t1,Tt
                      Tt = t1
                      Rtemp(n) = t1
                    ENDIF
                  ENDIF
                ENDIF
              ELSE
                if (ifaz.LE.0 ) then
                  t1save = min(t1save,.8d0*tg(1))
                  t2save = max(t2save,1.2d0*t2)
                else  
                  t1save = min(t1save,t1-.001d0)
                  t2save = max(t2save,t2+.001d0)
                endif
                if ( t1save .lt.Tt .and. t2save.gt.Tt ) hok = .true.
              ENDIF
              DO j = 1,5
                IF ( bb(j).EQ.0. ) GOTO 5
                Nfla(n) = j
                Ratom(n,j) = el(j)
                Rnum(n,j) = bb(j)
              ENDDO
 5            IF ( Tt.EQ.0. ) THEN
                IF ( .NOT.Hp ) GOTO 50
                WRITE (IOOUT,99004) n
                Nlm = 0
                GOTO 200
              ENDIF
              IF ( rcoefs.and.hok ) THEN
                Tln = DLOG(Tt)
                l = 1
                IF ( ifaz.LE.0 ) THEN
                  IF ( Tt.GT.Tg(2) ) l = 2
                  IF ( Tt.GT.Tg(3) ) l = 3
                ENDIF
                Enth(n) = (((((rcf(7,l)/5.D0)*Tt+rcf(6,l)/4.D0)*Tt+rcf(5
     &                    ,l)/3.D0)*Tt+rcf(4,l)/2.D0)*Tt+rcf(3,l))
     &                    *Tt - rcf(1,l)/Tt + rcf(2,l)*Tln + rcf(8,l)
                IF ( Vol.AND.ifaz.LE.0 ) Enth(n) = Enth(n) - Tt
              ENDIF
              if (hok) GOTO 50
            ENDIF
 20       CONTINUE
          if (.not.hok) then
            WRITE (IOOUT,99010) Tt,Rname(n),t1save,t2save
            Energy(n) = ' '
            Nlm = 0
            goto 200
          endif
        ENDIF
 50     ifrmla = Nfla(n)
        IF ( Fox(n)(:1).EQ.'f' ) THEN
          fuel = .TRUE.
          kr = 2
          Fox(n) = 'FUEL'
        ELSEIF ( Fox(n)(:4).EQ.'name' ) THEN
          fuel = .TRUE.
          kr = 2
          Fox(n) = 'NAME'
        ELSE
          kr = 1
          Fox(n) = 'OXIDANT'
        ENDIF
        DO j = 1,MAXEL
          dat(j) = 0.
        ENDDO
C STORE ATOMIC SYMBOLS IN ELMT ARRAY.
C CALCULATE MOLECULAR WEIGHT.
C TEMPORARILY STORE ATOMIC VALENCE IN X.
        rm = 0.D0
        DO 100 jj = 1,ifrmla
          DO j = 1,MAXEL
            nj = j
            IF ( Elmt(j).EQ.' ' ) GOTO 60
            IF ( Ratom(n,jj).EQ.Elmt(j) ) GOTO 80
          ENDDO
 60       Nlm = nj
          Elmt(j) = Ratom(n,jj)
 80       DO kk = 1,100
            IF ( Symbol(kk).EQ.Ratom(n,jj) ) THEN
              rm = rm + Rnum(n,jj)*Atmwt(kk)
              Atwt(j) = Atmwt(kk)
              X(j) = Valnce(kk)
              dat(j) = dat(j) + Rnum(n,jj)
              GOTO 100
            ENDIF
          ENDDO
          WRITE (IOOUT,99005) Ratom(n,jj)
          Nlm = 0
          GOTO 200
 100    CONTINUE
        IF ( Pecwt(n).LT.0. ) THEN
          Pecwt(n) = 0.
          IF ( .NOT.Moles.AND..NOT.wdone(kr) ) THEN
            wdone(kr) = .TRUE.
            Pecwt(n) = 100.
            WRITE (IOOUT,99006) n
          ELSE
            WRITE (IOOUT,99007) n
            Nlm = 0
            GOTO 200
          ENDIF
        ENDIF
C ADD CONTRIBUTIONS TO WP(K), HPP(K), AM(K), AND B0P(I,K)
        IF ( Pecwt(n).GT.0. ) wdone(kr) = .TRUE.
        pcwt = Pecwt(n)
        IF ( Moles ) pcwt = pcwt*rm
        Wp(kr) = Wp(kr) + pcwt
        IF ( rm.LE.0.D0 ) THEN
          Nlm = 0
          GOTO 200
        ELSE
          Hpp(kr) = Hpp(kr) + Enth(n)*pcwt/rm
          Am(kr) = Am(kr) + pcwt/rm
          IF ( Dens(n).NE.0. ) THEN
            Rh(kr) = Rh(kr) + pcwt/Dens(n)
          ELSE
            Rh(1) = 0.
            Rh(2) = 0.
          ENDIF
          DO j = 1,Nlm
            B0p(j,kr) = dat(j)*pcwt/rm + B0p(j,kr)
          ENDDO
          Rmw(n) = rm
        ENDIF
      ENDDO
      IF ( .NOT.fuel ) THEN
C 100 PERCENT OXIDANT, SWITCH INDICES
        DO n = 1,Nreac
          Fox(n) = ' '
        ENDDO
        Wp(2) = Wp(1)
        Wp(1) = 0.
        Hpp(2) = Hpp(1)
        Am(2) = Am(1)
        Am(1) = 0.
        DO j = 1,Nlm
          B0p(j,2) = B0p(j,1)
        ENDDO
      ENDIF
      IF ( Nlm.NE.0 ) THEN
C NORMALIZE HPP(KKR),AM(KR),B0P(I,KR), AND PECWT(N).
C CALCULATE V+(KR), AND V-(KR)
        DO kr = 1,2
          IF ( Wp(kr).NE.0. ) THEN
            Hpp(kr) = Hpp(kr)/Wp(kr)
            Am(kr) = Wp(kr)/Am(kr)
            IF ( Rh(kr).NE.0. ) Rh(kr) = Wp(kr)/Rh(kr)
            DO j = 1,Nlm
              B0p(j,kr) = B0p(j,kr)/Wp(kr)
              IF ( X(j).LT.0. ) Vmin(kr) = Vmin(kr) + B0p(j,kr)*X(j)
              IF ( X(j).GT.0. ) Vpls(kr) = Vpls(kr) + B0p(j,kr)*X(j)
            ENDDO
            IF ( .NOT.Moles ) THEN
              DO n = 1,Nreac
                IF ( Fox(n)(:1).NE.'O'.OR.kr.NE.2 ) THEN
                  IF ( Fox(n)(:1).EQ.'O'.OR.kr.NE.1 ) Pecwt(n)
     &                 = Pecwt(n)/Wp(kr)
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
        IF ( .NOT.Short ) THEN
          IF ( Moles ) THEN
            WRITE (IOOUT,99008) ' MOLES '
          ELSE
            WRITE (IOOUT,99008) 'WT.FRAC'
          ENDIF
          DO n = 1,Nreac
            WRITE (IOOUT,99009) Fox(n),Rname(n),Pecwt(n),Enth(n),
     &           Rtemp(n),Dens(n),(Ratom(n,i),Rnum(n,i),i=1,Nfla(n)) 
          ENDDO
        ENDIF
      ENDIF
 200  RETURN
99001 FORMAT (/' REACTANT ',A15,'HAS BEEN DEFINED FOR THE TEMPERATURE', 
     &  F8.2,'K ONLY.'/' YOUR TEMPERATURE ASSIGNMENT',F8.2,
     &  ' IS MORE THAN 10 K FROM THIS VALUE. (REACT)')
99002 FORMAT (/' NOTE! REACTANT ',A15,'HAS BEEN DEFINED FOR ',
     &  'TEMPERATURE',F8.2,'K ONLY.'/' YOUR TEMPERATURE ASSIGNMENT',
     &  F8.2,' IS NOT = BUT <10 K FROM THIS VALUE. (REACT)')
99003 FORMAT (/' NOTE: ',A15,' IS EITHER NOT IN thermo.lib OR THE',
     &        ' TEMPERATURE ',/,
     &        ' IS OUT OF RANGE FOR THIS SPECIES (REACT)')
99004 FORMAT (/' TEMPERATURE MISSING FOR REACTANT NO.',I2,'(REACT)')
99005 FORMAT (/1x,a2,' NOT FOUND IN BLOCKDATA (REACT)')
99006 FORMAT (/' WARNING!!  AMOUNT MISSING FOR REACTANT',I3,'.',
     &        /' PROGRAM SETS WEIGHT PERCENT = 100. (REACT)')
99007 FORMAT (/' AMOUNT MISSING FOR REACTANT NO.',I2,'(REACT)')
99008 FORMAT (/4x,'REACTANT',10x,A7,3X,'(ENERGY/R),K',3X,
     &        'TEMP,K  DENSITY'/,8x,'EXPLODED FORMULA')
99009 FORMAT (1x,a1,': ',a15,f10.6,e15.6,f9.2,f8.4,/8x,5(2x,a2,f8.5))
99010 FORMAT (/' YOUR ASSIGNED TEMPERATURE',F8.2,'K FOR ',A15,/,
     & 'IS OUTSIDE ITS TEMPERATURE RANGE',F8.2,' TO',F9.2,'K (REACT)')
      END
      SUBROUTINE RKTOUT
C***********************************************************************
C SPECIAL OUTPUT FOR ROCKET PROBLEMS.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*4 exit(11)
      CHARACTER*15 fi,fiv,fr,z(4)
      INTEGER i,i23,i46,i57,i68,i79,ione,ixfr,ixfz,j,k,line,ln,mae,mcf,
     &        misp,mivac,mmach,mppf,mppj,mxx(8),nex
      INTEGER INDEX
      REAL*8 agv,aw,gc,tem,tra,vaci(NCOL),ww
      SAVE agv,aw,fi,fiv,fr,gc,i,i23,i46,i57,i68,i79,ione,ixfr,ixfz,j,k,
     &  line,ln,mae,mcf,misp,mivac,mmach,mppf,mppj,mxx,nex,tem,tra,vaci,
     &  ww,z
C
      EQUIVALENCE (mxx(1),mppf)
      EQUIVALENCE (mxx(2),mppj)
      EQUIVALENCE (mxx(3),mmach)
      EQUIVALENCE (mxx(4),mae)
      EQUIVALENCE (mxx(5),mcf)
      EQUIVALENCE (mxx(6),mivac)
      EQUIVALENCE (mxx(7),misp)
      DATA exit/11*'EXIT'/
      IF ( .NOT.Eql ) THEN
        WRITE (IOOUT,99004)
        IF ( Nfz.GT.1 ) WRITE (IOOUT,99005) Nfz
      ELSE
        WRITE (IOOUT,99001)
        IF ( Iopt.NE.0 ) WRITE (IOOUT,99002)
        IF ( Iopt.EQ.0 ) WRITE (IOOUT,99003)
      ENDIF
      IF ( Ttt(1).EQ.T(It) ) WRITE (IOOUT,99006)
      tem = Ppp(1)*14.696006D0/1.01325D0
      WRITE (IOOUT,99009) 'Pin',tem
      i23 = 2
      IF ( Iopt.GT.0 ) THEN
        IF ( Iopt.EQ.1 ) WRITE (IOOUT,99007) Subar(1),App(2)
        IF ( Iopt.EQ.2 ) WRITE (IOOUT,99008) Ma,App(2)
        i23 = 3
      ENDIF
      CALL OUT1
      Fmt(4) = Fmt(6)
      nex = Npt - 2
      IF ( Page1 ) THEN
        ione = 0
        i46 = 4
        i57 = 5
        i68 = 6
        i79 = 7
      ELSE
        ione = i23
      ENDIF
C PRESSURE RATIOS
      IF ( Iopt.EQ.0 ) THEN
        WRITE (IOOUT,99011) (exit(i),i=1,nex)
        CALL VARFMT(App)
        WRITE (IOOUT,Fmt) 'Pinf/P         ',(App(j),j=1,Npt)
      ELSE
        nex = nex - 1
        WRITE (IOOUT,99010) (exit(i),i=1,nex)
        X(1) = 1.D0
        DO i = 2,Npt
          X(i) = Ppp(1)/Ppp(i)
        ENDDO
        CALL VARFMT(X)
        WRITE (IOOUT,Fmt) 'Pinj/P         ',(X(i),i=1,Npt)
      ENDIF
      CALL OUT2
      DO i = 1,8
        mxx(i) = 0
      ENDDO
      DO 100 i = 1,Nplt
        ixfz = INDEX(Pltvar(i)(2:),'fz')
        ixfr = INDEX(Pltvar(i)(2:),'fr')
        IF ( ixfz.NE.0.OR.ixfr.NE.0 ) THEN
          IF ( Eql ) GOTO 100
        ELSEIF ( .NOT.Eql ) THEN
          GOTO 100
        ENDIF
        IF ( Pltvar(i)(:4).EQ.'pi/p'.OR.Pltvar(i)(:3).EQ.'pip' ) THEN
          IF ( Iopt.EQ.0 ) mppf = i
          IF ( Iopt.NE.0 ) mppj = i
        ELSEIF ( Pltvar(i)(:4).EQ.'mach' ) THEN
          mmach = i
        ELSEIF ( Pltvar(i)(:2).EQ.'ae' ) THEN
          mae = i
        ELSEIF ( Pltvar(i)(:2).EQ.'cf' ) THEN
          mcf = i
        ELSEIF ( Pltvar(i)(:4).EQ.'ivac' ) THEN
          mivac = i
        ELSEIF ( Pltvar(i)(:3).EQ.'isp' ) THEN
          misp = i
        ENDIF
 100  CONTINUE
      IF ( Siunit ) THEN
        agv = 1.
        gc = 1.
        fr = 'CSTAR, M/SEC'
        fiv = 'Ivac, M/SEC'
        fi = 'Isp, M/SEC'
      ELSE
        gc = 32.174
        agv = 9.80665
        fr = 'CSTAR, FT/SEC'
        fiv = 'Ivac,LB-SEC/LB'
        fi = 'Isp, LB-SEC/LB'
      ENDIF
      DO k = 2,Npt
        Spim(k) = (2.*Rr*(Hsum(1)-Hsum(k)))**.5/agv
C AW IS THE LEFT SIDE OF EQ.(6.12) IN RP-1311,PT I.
        aw = Rr*Ttt(k)/(Ppp(k)*Wm(k)*Spim(k)*agv**2)
        IF ( k.EQ.i23 ) THEN
          IF ( Iopt.EQ.0 ) Cstr = gc*Ppp(1)*aw
          IF ( Iopt.NE.0 ) Cstr = gc*Ppp(1)/App(2)*aw
        ENDIF
        vaci(k) = Spim(k) + Ppp(k)*aw
        Vmoc(k) = 0.
        IF ( Sonvel(k).NE.0. ) Vmoc(k) = Spim(k)*agv/Sonvel(k)
      ENDDO
C MACH NUMBER
      Vmoc(1) = 0.
      IF ( Gammas(i23).EQ.0. ) Vmoc(i23) = 0.
      Fmt(7) = '3,'
      WRITE (IOOUT,Fmt) 'MACH NUMBER    ',(Vmoc(j),j=1,Npt)
      IF ( Trnspt ) CALL OUT4
      WRITE (IOOUT,99013)
C AREA RATIO
      Fmt(4) = '9x,'
      Fmt(i46) = '9x,'
      CALL VARFMT(Aeat)
      Fmt(5) = ' '
      Fmt(i57) = ' '
      WRITE (IOOUT,Fmt) 'Ae/At          ',(Aeat(j),j=2,Npt)
C C*
      Fmt(i57) = '13'
      Fmt(i68) = Fmt(i68+2)
      Fmt(i79) = '1,'
      WRITE (IOOUT,Fmt) fr,(Cstr,j=2,Npt)
C CF - THRUST COEFICIENT
      Fmt(i79) = '4,'
      DO i = 2,Npt
        X(i) = gc*Spim(i)/Cstr
      ENDDO
      WRITE (IOOUT,Fmt) 'CF             ',(X(j),j=2,Npt)
C VACUUM IMPULSE
      Fmt(i57) = '13'
      Fmt(i79) = '1,'
      WRITE (IOOUT,Fmt) fiv,(vaci(j),j=2,Npt)
C SPECIFIC IMPULSE
      WRITE (IOOUT,Fmt) fi,(Spim(j),j=2,Npt)
      IF ( Nplt.GT.0 ) THEN
        Spim(1) = 0
        Aeat(1) = 0
        Vmoc(1) = 0
        vaci(1) = 0
        X(1) = 0
        Spim(1) = 0
        DO i = ione + 1,Npt
          IF ( mppj.GT.0 ) Pltout(i+Iplt-ione,mppj) = Ppp(1)/Ppp(i)
          IF ( mppf.GT.0 ) Pltout(i+Iplt-ione,mppf) = App(i)
          IF ( mmach.GT.0 ) Pltout(i+Iplt-ione,mmach) = Vmoc(i)
          IF ( mae.GT.0 ) Pltout(i+Iplt-ione,mae) = Aeat(i)
          IF ( mcf.GT.0 ) Pltout(i+Iplt-ione,mcf) = X(i)
          IF ( mivac.GT.0 ) Pltout(i+Iplt-ione,mivac) = vaci(i)
          IF ( misp.GT.0 ) Pltout(i+Iplt-ione,misp) = Spim(i)
        ENDDO
      ENDIF
      WRITE (IOOUT,99012)
      Fmt(4) = ' '
      Fmt(5) = '13'
      Fmt(7) = '5,'
      IF ( Iopt.NE.0 ) THEN
        Fmt(i46) = Fmt(8)
        Fmt(i57) = Fmt(9)
      ENDIF
      IF ( .NOT.Eql ) THEN
        IF ( Massf ) THEN
          WRITE (IOOUT,99014) 'MASS'
        ELSE
          WRITE (IOOUT,99014) 'MOLE'
          ww = 1.D0/Totn(Nfz)
        ENDIF
C MOLE (OR MASS) FRACTIONS - FROZEN
        tra = 5.E-6
        IF ( Trace.NE.0. ) tra = Trace
        line = 0
        DO k = 1,Ngc
          IF ( Massf ) ww = Mw(k)
          X(line+1) = En(k,Nfz)*ww
          IF ( X(line+1).GE.tra ) THEN
            line = line + 1
            z(line) = Prod(k)
          ENDIF
          IF ( line.EQ.3.OR.k.EQ.Ngc ) THEN
            IF ( line.EQ.0 ) GOTO 200
            WRITE (IOOUT,99015) (z(ln),X(ln),ln=1,line)
            line = 0
          ENDIF
        ENDDO
      ENDIF
 200  CALL OUT3
      RETURN
99001 FORMAT (/////13x,' THEORETICAL ROCKET PERFORMANCE ASSUMING',
     &        ' EQUILIBRIUM')
99002 FORMAT (/11x,' COMPOSITION DURING EXPANSION FROM FINITE AREA',
     &        ' COMBUSTOR')
99003 FORMAT (/10x,' COMPOSITION DURING EXPANSION FROM INFINITE AREA',
     &        ' COMBUSTOR')
99004 FORMAT (/////10x,' THEORETICAL ROCKET PERFORMANCE ASSUMING FROZEN'
     &        ,' COMPOSITION')
99005 FORMAT (33X,'AFTER POINT',I2)
99006 FORMAT (25X,'AT AN ASSIGNED TEMPERATURE  ')
99007 FORMAT (' Ac/At =',F8.4,6x,'Pinj/Pinf =',F10.6)
99008 FORMAT (' MDOT/Ac =',F10.3,' (KG/S)/M**2',6x,'Pinj/Pinf =',F10.6)
99009 FORMAT (/1x,A3,' =',F8.1,' PSIA')
99010 FORMAT (/,17X,'INJECTOR  COMB END  THROAT',10(5X,A4))
99011 FORMAT (/17X,'CHAMBER   THROAT',11(5X,A4))
99012 FORMAT ()
99013 FORMAT (/' PERFORMANCE PARAMETERS'/)
99014 FORMAT (1x,A4,' FRACTIONS'/)
99015 FORMAT (1X,3(A15,F8.5,3X))
      END
      SUBROUTINE ROCKET
C***********************************************************************
C EXECUTIVE ROUTINE FOR ROCKET PROBLEMS.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER i,i01,i12,iof,iplt1,iplte,ipp,isub,isup1,isupsv,itnum,
     &        itrot,nar,nipp,niter,nn,npr1,nptth
      LOGICAL done,seql,thi
      REAL*8 a1l,acatsv,aeatl,appl,aratio,asq,b1,c1,check,cprf,dd,dh,
     &       dlnp,dlnpe,dlt,dp,eln,mat,msq,p1,pa,pcpa,pcplt,pinf,pinj,
     &       pinjas,pjrat,ppa,pr,pracat,prat,pratsv,pvg,test,tmelt,usq
      REAL*8 DABS,DLOG,DMAX1,DSQRT
      SAVE acatsv,aeatl,appl,aratio,asq,check,cprf,dd,dh,dlnp,dlnpe,dlt,
     &  done,dp,eln,i,i01,i12,iof,iplt1,iplte,ipp,isub,isup1,isupsv,
     &  itnum,itrot,mat,msq,nar,nipp,niter,nn,npr1,nptth,p1,pcpa,pcplt,
     &  pinf,pinj,pinjas,pjrat,ppa,pr,pracat,prat,pratsv,pvg,seql,test,
     &  thi,tmelt,usq
C
      DATA a1l/ - 1.26505/,b1/1.0257/,c1/ - 1.2318/,pa/1.E05/
      iplte = Iplt
      isup1 = 1
      App(1) = 1.
      Iopt = 0
      Npp = Npp + 2
      nn = Npp
      i01 = 0
      i12 = 1
      nipp = 1
      nptth = 2
      IF ( Fac ) THEN
        Eql = .TRUE.
        Npp = Npp + 1
        IF ( Acat.NE.0. ) THEN
          Iopt = 1
        ELSEIF ( Ma.NE.0. ) THEN
          Iopt = 2
        ELSE
          WRITE (IOOUT,99001)
          Tt = 0.
          GOTO 1400
        ENDIF
        i01 = 1
        i12 = 2
        nipp = 2
        nptth = 3
        DO i = Nsub,1, - 1
          Subar(i+1) = Subar(i)
        ENDDO
        Nsub = Nsub + 1
        IF ( Iopt.NE.1 ) THEN
          IF ( Acat.EQ.0. ) Acat = 2.
        ENDIF
        Subar(1) = Acat
      ELSEIF ( .NOT.Eql.AND.Nfz.GT.1.AND.Nsub.GT.0 ) THEN
        Nsub = 0
        WRITE (IOOUT,99023)
      ENDIF
      nn = nn + Nsub + Nsup
      IF ( Nfz.GT.2.AND.nn.GT.NCOL-2 ) THEN
        WRITE (IOOUT,99002) NCOL - 2
        Nfz = 1
        Froz = .FALSE.
      ENDIF
      seql = Eql
      iof = 0
      Tt = Tcest
      Pp = P(1)
      App(i12) = 1.
C LOOP FOR EACH O/F
 100  It = 1
      iof = iof + 1
      Oxfl = Oxf(iof)
      IF ( T(1).NE.0. ) THEN
        Tp = .TRUE.
      ELSE
        Hp = .TRUE.
      ENDIF
      Sp = .FALSE.
      CALL NEWOF
      IF ( T(1).NE.0. ) Tt = T(1)
C LOOP FOR CHAMBER PRESSURES
 200  DO Ip = 1,Np
        itnum = 0
        Area = .FALSE.
        IF ( T(1).EQ.0. ) Hp = .TRUE.
        IF ( T(1).NE.0. ) Tp = .TRUE.
        Sp = .FALSE.
        Eql = .TRUE.
        isub = 1
        Isup = 1
        Pp = P(Ip)
        pinf = Pp
        ipp = 1
        itrot = 3
        isupsv = 1
        niter = 1
        Page1 = .TRUE.
        iplt1 = iplte
        Iplt = iplte
        done = .FALSE.
C LOOP FOR OUTPUT COLUMNS
 250    nar = Npt
        IF ( Eql ) THEN
          CALL EQLBRM
          IF ( Npt.EQ.Nfz ) cprf = Cpsum
        ELSE
          CALL FROZEN
        ENDIF
C TT = 0 IF NO CONVERGENCE
        IF ( Tt.NE.0. ) THEN
C TEST FOR FINITE AREA COMBUSTOR
          IF ( .NOT.Fac ) GOTO 400
          pinjas = P(Ip)*pa
          pinj = pinjas
          IF ( Npt.LE.2 ) THEN
            IF ( Npt.EQ.1.AND.Trnspt ) CALL TRANP
            IF ( Npt.EQ.2 ) pinf = Ppp(2)
          ENDIF
          IF ( Npt.NE.1 ) GOTO 400
C INITIAL ESTIMATE FOR PC (AND ACAT IF NOT ASSIGNED)
          DO i = 1,4
            prat = (b1+c1*Acat)/(1.+a1l*Acat)
            ppa = pinj*prat
            IF ( Iopt.EQ.1 ) GOTO 260
            Acat = ppa/(Ma*2350.)
            IF ( Acat.GE.1. ) THEN
              pratsv = prat
              IF ( Debugf ) THEN
                IF ( i.LE.1 ) WRITE (IOOUT,99004)
                WRITE (IOOUT,99005) i,ppa,Acat
              ENDIF
            ELSE
              WRITE (IOOUT,99003) Ma
              Tt = 0.
              GOTO 1400
            ENDIF
          ENDDO
          Subar(1) = Acat
 260      Pp = ppa/pa
          App(1) = Pp/Ppp(1)
          GOTO 1100
        ELSE
          IF ( Npt.LT.1 ) GOTO 1400
          IF ( .NOT.Area ) GOTO 600
          Npt = nar - 1
          Isup = Nsup + 2
          Isv = 0
          itnum = 0
          GOTO 950
        ENDIF
 300    Hp = .TRUE.
        Sp = .FALSE.
        niter = niter + 1
        Isv = 0
        Npt = 2
        ipp = 2
        CALL SETEN
        GOTO 250
 350    done = .TRUE.
        App(1) = Ppp(2)/Ppp(1)
        Area = .FALSE.
        IF ( Nsub.GT.1 ) isub = 2
        Isv = 4
        Npt = 2
        ipp = MIN(4,Npp)
        CALL SETEN
        Cpr(2) = Cpr(4)
        Dlvpt(2) = Dlvpt(4)
        Dlvtp(2) = Dlvtp(4)
        Gammas(2) = Gammas(4)
        Hsum(2) = Hsum(4)
        Ppp(2) = Ppp(4)
        App(2) = Ppp(1)/pinf
        Ssum(2) = Ssum(4)
        Totn(2) = Totn(4)
        Ttt(2) = Ttt(4)
        Vlm(2) = Vlm(4)
        Wm(2) = Wm(4)
        IF ( .NOT.Short ) WRITE (IOOUT,99009)
        GOTO 600
C INITIALIZE FOR THROAT
 400    IF ( ipp.GT.nipp ) THEN
          usq = 2.*(Hsum(1)-Hsum(Npt))*Rr
          IF ( ipp.GT.nptth ) GOTO 600
C THROAT
          IF ( .NOT.thi ) THEN
            Vv = Vlm(nptth)
            pvg = Pp*Vv*Gammas(nptth)
            IF ( pvg.EQ.0. ) THEN
              WRITE (IOOUT,99010)
              GOTO 550
            ELSE
              msq = usq/pvg
              IF ( Debug(1).OR.Debug(2) ) WRITE (IOOUT,99011) usq,pvg
              dh = DABS(msq-1.D0)
              IF ( dh.LE.0.4D-4 ) GOTO 550
              IF ( itrot.GT.0 ) THEN
                p1 = Pp
                IF ( Jsol.NE.0 ) THEN
                  tmelt = Tt
                  Pp = Pp*(1.D0+msq*Gammas(nptth))/(Gammas(nptth)+1.D0)
                ELSEIF ( tmelt.EQ.0. ) THEN
                  Pp = Pp*(1.D0+msq*Gammas(nptth))/(Gammas(nptth)+1.D0)
                ELSE
                  WRITE (IOOUT,99012)
                  dlt = DLOG(tmelt/Tt)
                  dd = dlt*Cpr(nptth)/(Enn*Dlvtp(nptth))
                  Pp = Pp*EXP(dd)
                  App(nptth) = P(Ip)/Pp
                  IF ( Fac ) App(nptth) = pinf/Pp
                  IF ( Eql.AND..NOT.Short ) WRITE (IOOUT,99013) 
     &                            App(nptth)
                  thi = .TRUE.
                  GOTO 250
                ENDIF
                GOTO 500
              ELSEIF ( itrot.LT.0 ) THEN
                IF ( itrot.LT.-19 ) THEN
                  WRITE (IOOUT,99010)
                  GOTO 550
                ELSE
                  IF ( Npr.NE.npr1 ) GOTO 550
                  Pp = Pp - dp
                  GOTO 500
                ENDIF
              ELSEIF ( Npr.EQ.npr1 ) THEN
                WRITE (IOOUT,99010)
                GOTO 550
              ELSE
                dp = DABS(Pp-p1)/20.
                Pp = DMAX1(Pp,p1)
                WRITE (IOOUT,99012)
                Pp = Pp - dp
                GOTO 500
              ENDIF
            ENDIF
          ELSE
            Gammas(nptth) = 0.
            GOTO 550
          ENDIF
        ELSE
          IF ( .NOT.Fac.AND.Trnspt ) CALL TRANP
          IF ( Npt.EQ.Nfz ) Eql = seql
          Tp = .FALSE.
          Hp = .FALSE.
          Sp = .TRUE.
          S0 = Ssum(i12)
        ENDIF
 450    tmelt = 0.
        itrot = 3
        thi = .FALSE.
        App(nptth) = ((Gammas(i12)+1.)/2.)
     &               **(Gammas(i12)/(Gammas(i12)-1.))
        IF ( Eql.AND..NOT.Short ) WRITE (IOOUT,99013) App(nptth)
        Pp = pinf/App(nptth)
        Isv = -i12
        GOTO 1200
 500    npr1 = Npr
        App(nptth) = P(Ip)/Pp
        IF ( Fac ) App(nptth) = pinf/Pp
        IF ( Eql.AND..NOT.Short ) WRITE (IOOUT,99013) App(nptth)
        itrot = itrot - 1
        GOTO 250
 550    Awt = Enn*Tt/(Pp*usq**.5)
        pcplt = DLOG(App(nptth))
 600    Isv = 0
        Aeat(Npt) = Enn*Ttt(Npt)/(Pp*usq**.5*Awt)
        IF ( Tt.EQ.0. ) GOTO 1150
        IF ( Area ) GOTO 750
        IF ( Trnspt.AND.(.NOT.Fac.OR.done.OR.Npt.GT.2) ) CALL TRANP
        IF ( Npt.EQ.Nfz ) Eql = seql
        IF ( Fac ) THEN
          IF ( Npt.EQ.nptth ) THEN
            Area = .TRUE.
            GOTO 750
          ELSEIF ( Npt.EQ.2.AND.done ) THEN
            Npt = 3
C  The following statement was corrected 1/30/2004.  Only fac parameters 
C    after combustion were affected--generally extra or missing points.
C  (remove) IF ( ipp.LE.Npp ) ipp = ipp - 1
            IF ( ipp.LT.Npp.OR.npp.EQ.4 ) ipp = ipp - 1
          ENDIF
        ENDIF
 650    IF ( ipp.LT.Npp ) GOTO 1100
 700    IF ( Nsub.EQ.i01.AND.Nsup.EQ.0 ) GOTO 1150
        Area = .TRUE.
C PCP ESTIMATES FOR AREA RATIOS
 750    IF ( itnum.EQ.0 ) THEN
          dlnp = 1.
          itnum = 1
          aratio = Subar(isub)
          IF ( (.NOT.Fac.OR.done).AND.Nsub.LE.i01 ) aratio = Supar(Isup)
          IF ( .NOT.Eql.AND.Nfz.GE.3 ) THEN
            IF ( aratio.LE.Aeat(Nfz) ) THEN
              WRITE (IOOUT,99014) Nfz
              GOTO 1050
            ENDIF
          ENDIF
          IF (aratio .LT. 1.d0 ) THEN
            WRITE (IOOUT,99025) 
            GOTO 1050
          ENDIF
          eln = DLOG(aratio)
          IF ( Fac ) THEN
            IF ( .NOT.done ) GOTO 800
          ENDIF
          IF ( Nsub.LE.i01 ) THEN
            IF ( Nfz.EQ.ipp ) isupsv = Isup
            IF ( Supar(Isup).LT.2. ) THEN
              appl = DSQRT(eln*(1.535d0+3.294d0*eln)) + pcplt
              GOTO 1100
            ELSE
              IF ( Isup.GT.isup1.AND.Supar(Isup-1).GE.2. ) GOTO 850
              appl = Gammas(nptth) + eln*1.4
              GOTO 1100
            ENDIF
          ENDIF
C TEST FOR CONVERGENCE ON AREA RATIO.
        ELSEIF ( Gammas(Npt).GT.0. ) THEN
          check = .00004
          IF ( Debug(Npt) ) WRITE (IOOUT,99016) itnum,aratio,Aeat(Npt),
     &                             App(Npt),dlnp
          IF ( DABS(Aeat(Npt)-aratio)/aratio.LE.check ) GOTO 900
          IF ( ABS(dlnp).LT..00004 ) GOTO 900
          aeatl = DLOG(Aeat(Npt))
          itnum = itnum + 1
          IF ( itnum.GT.10 ) THEN
            WRITE (IOOUT,99017) aratio
            GOTO 900
          ELSE
C IMPROVED PCP ESTIMATES.
            asq = Gammas(Npt)*Enn*Rr*Tt
            dlnpe = Gammas(Npt)*usq/(usq-asq)
            GOTO 850
          ENDIF
        ELSE
          WRITE (IOOUT,99015)
          Npt = Npt - 1
          IF ( Nsub.LE.0 ) isup1 = 100
          IF ( Nsub.LT.0. ) Nsup = Isup - 1
          IF ( Nsub.GT.0 ) Nsub = isub - 1
          GOTO 1000
        ENDIF
 800    appl = pcplt/(Subar(isub)+(10.587*eln**2+9.454)*eln)
        IF ( aratio.LT.1.09 ) appl = .9*appl
        IF ( aratio.GT.10. ) appl = appl/aratio
        IF ( isub.GT.1.OR.Npt.EQ.NCOL ) GOTO 1100
        GOTO 1200
 850    dlnp = dlnpe*eln - dlnpe*aeatl
        appl = appl + dlnp
        IF ( itnum.EQ.1 ) GOTO 1100
        IF ( appl.LT.0. ) appl = .000001
        App(Npt) = EXP(appl)
        Pp = pinf/App(Npt)
        GOTO 250
C CONVERGENCE HAS BEEN REACHED FOR ASSIGNED AREA RATIO
 900    Aeat(Npt) = aratio
        IF ( Fac ) THEN
          IF ( .NOT.done ) THEN
            IF ( Iopt.EQ.1 ) THEN
C OPTION 1 FOR FINITE AREA COMBUSTOR. INPUT IS ASSIGNED INJECTOR
C PRESSURE AND CONTRACTION RATIO. IMPROVED ESTIMATE FOR PC
              Area = .FALSE.
              itnum = 0
              ppa = Ppp(Npt)*pa
              pinj = ppa + 1.D05*usq/Vlm(Npt)
              test = (pinj-pinjas)/pinjas
              pcpa = pinf*pa
              IF ( Debugf ) THEN
                WRITE (IOOUT,99006)
                WRITE (IOOUT,99007) niter,test,pinjas,pinj,pcpa,ppa,
     &                          acatsv,Acat
              ENDIF
              IF ( ABS(test).LT.0.00002 ) GOTO 350
              prat = pinjas/pinj
              Pp = pinf*prat
              GOTO 300
            ELSEIF ( Iopt.EQ.2 ) THEN
C OPTION 2 FOR FINITE AREA COMBUSTOR. INPUT IS ASSIGNED INJECTOR
C PRESSURE AND MASS FLOW PER UNIT AREA. IMPROVED ESTIMATE FOR PC
C AND ACAT
              acatsv = Acat
              pratsv = prat
              Area = .FALSE.
              itnum = 0
              ppa = Ppp(4)*pa
              pinj = ppa + 1.D05*usq/Vlm(4)
              mat = pa/(Awt*Rr)
              Acat = mat/Ma
              prat = (b1+c1*Acat)/(1.+a1l*Acat)
              test = (pinj-pinjas)/pinjas
              pcpa = pinf*pa
              IF ( Debugf ) THEN
                WRITE (IOOUT,99006)
                WRITE (IOOUT,99007) niter,test,pinjas,pinj,pcpa,ppa,
     &                          acatsv,Acat
              ENDIF
              IF ( ABS(test).LT.0.00002 ) GOTO 350
              pjrat = pinj/pinjas
              Pp = pinf
              DO i = 1,2
                pracat = pratsv/prat
                pr = pjrat*pracat
                Pp = Pp/pr
                pcpa = Pp*pa
                Acat = Acat/pr
                Subar(1) = Acat
                pratsv = prat
                pjrat = 1.
                prat = (b1+c1*Acat)/(1.+a1l*Acat)
                IF ( Debugf ) WRITE (IOOUT,99008) pcpa,Acat,pjrat,pracat
              ENDDO
              GOTO 300
            ENDIF
          ENDIF
        ENDIF
 950    IF ( Trnspt ) CALL TRANP
        IF ( Npt.EQ.Nfz ) Eql = seql
 1000   itnum = 0
        IF ( Nsub.GT.i01 ) THEN
          isub = isub + 1
          IF ( isub.LE.Nsub ) GOTO 750
          isub = 1
          Nsub = -Nsub
          IF ( Isup.LE.Nsup ) GOTO 750
          Area = .FALSE.
          GOTO 1150
        ENDIF
 1050   Isup = Isup + 1
        itnum = 0
        IF ( Isup.LE.Nsup ) GOTO 750
        Isup = isupsv
        Area = .FALSE.
        GOTO 1150
C TEST FOR OUTPUT -- SCHEDULES COMPLETE OR NPT=NCOL
 1100   Isv = Npt
        IF ( Npt.NE.NCOL ) GOTO 1200
 1150   IF ( .NOT.Eql ) THEN
          IF ( Nfz.LE.1 ) THEN
            Cpr(Nfz) = cprf
            Gammas(Nfz) = cprf/(cprf-1./Wm(Nfz))
          ENDIF
        ENDIF
        CALL RKTOUT
        Iplt = Iplt + Npt
        IF ( .NOT.Page1 ) THEN
          Iplt = Iplt - 2
          IF ( Iopt.NE.0 ) Iplt = Iplt - 1
          Iplt = MIN(Iplt,500)
        ELSE
          Page1 = .FALSE.
        ENDIF
        iplte = MAX(iplte,Iplt)
        dlnp = 1.
        IF ( Tt.EQ.0. ) Area = .FALSE.
        IF ( .NOT.Eql.AND.Tt.EQ.0. ) WRITE (IOOUT,99018)
        IF ( Isv.EQ.0 ) THEN
C PCP, SUBAR, AND SUPAR SCHEDULES COMPLETED
          IF ( Nsub.LT.0 ) Nsub = -Nsub
          IF ( .NOT.Froz.OR..NOT.Eql ) GOTO 1300
C SET UP FOR FROZEN.
          IF ( Eql ) Iplt = iplt1
          Eql = .FALSE.
          Page1 = .TRUE.
          CALL SETEN
          Tt = Ttt(Nfz)
          ipp = Nfz
          IF ( Nfz.EQ.Npt ) GOTO 1150
          Npt = Nfz
          Enn = 1./Wm(Nfz)
          IF ( Nfz.EQ.1 ) GOTO 450
          IF ( Nsub.GT.0 ) THEN
            Nsub = -Nsub
            WRITE (IOOUT,99023)
          ENDIF
          IF ( App(Nfz).LT.App(nptth) ) THEN
            WRITE (IOOUT,99024)
          ELSE
            IF ( Nfz.LT.Npp ) GOTO 1200
            GOTO 700
          ENDIF
          GOTO 1300
        ELSE
          IF ( Eql ) WRITE (IOOUT,99019)
          Npt = nptth
        ENDIF
C SET INDICES AND ESTIMATES FOR NEXT POINT.
 1200   Npt = Npt + 1
        IF ( Eql.OR.(Isv.EQ.-i12.AND..NOT.seql) ) THEN
C THE FOLLOWING STATEMENT WAS ADDED TO TAKE CARE OF A SITUATION
C WHERE EQLBRM WENT SINGULAR WHEN STARTING FROM ESTIMATES WHERE
C BOTH SOLID AND LIQUID WERE INCLUDED.  JULY 27, 1990.
          IF ( Jliq.NE.0.AND.Isv.GT.0 ) Isv = 0
          CALL SETEN
        ENDIF
 1250   ipp = ipp + 1
        IF ( Npt.GT.nptth ) THEN
          IF ( Area ) THEN
            App(Npt) = EXP(appl)
          ELSE
            App(Npt) = Pcp(ipp-nptth)
            IF ( Fac ) App(Npt) = App(Npt)*pinf/Ppp(1)
            IF ( .NOT.Eql.AND.App(Npt).LT.App(Nfz) ) THEN
              WRITE (IOOUT,99020) Nfz
              GOTO 1250
            ENDIF
          ENDIF
          Pp = pinf/App(Npt)
          IF ( Fac ) THEN
            IF ( Area ) THEN
              IF ( isub.LE.Nsub.AND.isub.GT.i01.AND.aratio.GE.Aeat(2) )
     &             THEN
                WRITE (IOOUT,99021) aratio,Aeat(2)
                Npt = Npt - 1
                GOTO 1000
              ENDIF
            ELSEIF ( Npt.GT.nptth.AND.Pcp(ipp-3).LT.Ppp(1)/Ppp(2) ) THEN
              WRITE (IOOUT,99022) Pcp(ipp-3),Ppp(1)/Ppp(2)
              Npt = Npt - 1
              GOTO 650
            ENDIF
          ENDIF
        ENDIF
        GOTO 250
 1300   Npt = 1
C CHECK FOR COMPLETED SCHEDULES -
C 1) CHAMBER PRESSURES(IP = NP)
C 2) CHAMBER TEMPERATURES(IT = NT)
C 3) O/F VALUES(IOF = NOF)
        IF ( Ip.EQ.Np.AND.It.EQ.Nt.AND.iof.EQ.Nof ) GOTO 1400
        WRITE (IOOUT,99019)
        CALL SETEN
        Tt = Ttt(i12)
      ENDDO
      IF ( It.LT.Nt ) THEN
        It = It + 1
        Tt = T(It)
        GOTO 200
      ELSEIF ( iof.LT.Nof ) THEN
        GOTO 100
      ENDIF
 1400 Iplt = MAX(Iplt,iplte)
      RETURN
99001 FORMAT (/' FATAL ERROR!! EITHER mdot OR ac/at MISSING ',
     &        'FOR fac PROBLEM (ROCKET)')
99002 FORMAT (/' WARNING!!  nfz NOT ALLOWED TO BE > 2 IF THE TOTAL',/,
     &        ' NUMBER OF POINTS IS >',i3,' (ROCKET)')
99003 FORMAT (/' INPUT VALUE OF mdot/a =',F12.3,' IS TOO LARGE.'/
     &        ' GIVES CONTRACTION RATIO ESTIMATE LESS THAN 1 (ROCKET)')
99004 FORMAT (/'  ITERATION',9X,'PC',7X,'CONTRACTION RATIO')
99005 FORMAT (5X,I2,7X,F12.2,3X,F12.6)
99006 FORMAT (' ITER',3X,'TEST',3X,'ASSIGNED PINJ',1x,'CALC PINJ',5X,
     &        'PC',7X,'P AT ACAT',3X,'PREV ACAT',2X,'ACAT')
99007 FORMAT (I3,F10.6,1x,4F12.2,2F9.5)
99008 FORMAT (' NEW PC = ',F10.2,2X,'NEW ACAT = ',F9.6,2X,'PJRAT =',
     &        F10.7,' PRACAT =',F10.7)
99009 FORMAT (' END OF CHAMBER ITERATIONS')
99010 FORMAT (/' WARNING!!  DIFFICULTY IN LOCATING THROAT (ROCKET)')
99011 FORMAT (/' USQ=',E15.8,5X,'PVG=',E15.8)
99012 FORMAT (/' WARNING!!  DISCONTINUITY AT THE THROAT (ROCKET)')
99013 FORMAT (' Pinf/Pt =',F9.6)
99014 FORMAT (/,' WARNING!! FOR FROZEN PERFORMANCE, POINTS WERE OMITTED'
     &        ,' WHERE THE ASSIGNED',/,' SUPERSONIC AREA RATIOS WERE ',
     &        'LESS THAN THE VALUE AT POINT nfz =',I3,' (ROCKET)')
99015 FORMAT (/' WARNING!!  AREA RATIO CALCULATION CANNOT BE DONE ',
     &        'BECAUSE GAMMAs',/,' CALCULATION IMPOSSIBLE. (ROCKET)')
99016 FORMAT (/' ITER=',I2,2X,'ASSIGNED AE/AT=',F14.7,3X,'AE/AT=',F14.7,
     &        /,2X,'PC/P=',F14.7,2X,'DELTA LN PCP=',F14.7)
99017 FORMAT (/' WARNING!!  DID NOT CONVERGE FOR AREA RATIO =',F10.5,
     &        ' (ROCKET)')
99018 FORMAT (/' WARNING!!  CALCULATIONS WERE STOPPED BECAUSE NEXT ',
     &        'POINT IS MORE',/,' THAN 50 K BELOW THE TEMPERATURE',
     &        ' RANGE OF A CONDENSED SPECIES (ROCKET)')
99019 FORMAT (////)
99020 FORMAT (/,' WARNING!! FOR FROZEN PERFORMANCE, POINTS WERE OMITTED'
     &        ,' WHERE THE ASSIGNED',/,
     &        ' PRESSURE RATIOS WERE LESS THAN ',
     &        'THE VALUE AT POINT nfz =',I3,' (ROCKET)')
99021 FORMAT (/' WARNING!!  ASSIGNED subae/at =',f10.5,' IS NOT ',
     &        'PERMITTED TO BE GREATER'/' THAN ac/at =',f9.5,
     &        '.  POINT OMITTED (ROCKET)')
99022 FORMAT (/' WARNING!!  ASSIGNED pip =',F10.5,
     &        ' IS NOT PERMITTED'/' TO BE LESS THAN  Pinj/Pc =',f9.5,
     &        '. POINT OMITTED',' (ROCKET)')
99023 FORMAT (/' WARNING!!  FOR FROZEN PERFORMANCE, SUBSONIC AREA ',/,
     &       ' RATIOS WERE OMITTED SINCE nfz IS GREATER THAN 1 (ROCKET)'
     &       )
99024 FORMAT (/' WARNING!!  FREEZING IS NOT ALLOWED AT A SUBSONIC ',
     &        'PRESSURE RATIO FOR nfz GREATER'/' THAN 1. FROZEN ',
     &        'PERFORMANCE CALCULATIONS WERE OMITTED (ROCKET)')
99025 FORMAT (/' AN ASSIGNED AREA RATIO IS < 1 (ROCKET)' )
      END
      SUBROUTINE SEARCH
C***********************************************************************
C SEARCH THERMO.LIB FOR THERMO DATA FOR SPECIES TO BE CONSIDERED.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*16 bin(2,40),pure(6),spece(2)
      CHARACTER*6 date(MAXNGC)
      CHARACTER*2 el(5)
      CHARACTER*15 sub
      INTEGER i,i5,ifaz,ii,ir,itot,j,jj(2),jk,k,lineb,nall,ne,nint,
     &        npure,nrec,ntgas,ntot
      REAL*8 b(5),t1,t2,thermo(9,3),trdata(36)
      SAVE b,bin,date,el,i,i5,ifaz,ii,ir,itot,j,jj,jk,k,lineb,nall,ne,
     &  nint,npure,nrec,ntgas,ntot,pure,spece,sub,t1,t2,thermo,trdata
C
      Nc = 0
      ne = 0
      DO i = 1,Nlm
        Jx(i) = 0
      ENDDO
      DO j = 1,MAXNGC
        S(j) = 0.
        H0(j) = 0.
        Deln(j) = 0.
        DO i = 1,Nlm
          A(i,j) = 0.
        ENDDO
      ENDDO
C READ TEMPERATURE RANGES FOR COEFFICIENTS OF GASEOUS SPECIES.
C SOME DEFINITIONS:
C   NTGAS = NUMBER OF GASEOUS SPECIES IN THERMO.LIB.
C   NTOT =  NTGAS PLUS NUMBER OF TEMPERATURE INTERVALS FOR CONDENSED.
C   NALL =  NTOT PLUS THE NUMBER OF REACTANT SPECIES IN THERMO.LIB.
C   NG =    NUMBER OF GASES WITH STORED COEFFICIENTS.
C   NC =    NUMBER OF CONDENSED INTERVALS WITH STORED COEFFICIENTS.
C   NGC =    NG + NC
C   THDATE = DATE READ FROM THERMO.INP FILE
      REWIND IOTHM
      READ (IOTHM) Tg,ntgas,ntot,nall,Thdate
      Ngc = 1
      Nc = 1
C BEGIN LOOP FOR READING SPECIES DATA FROM THERMO.LIB.
      DO 200 itot = 1,ntot
        IF ( itot.GT.ntgas ) THEN
          READ (IOTHM) sub,nint,date(Ngc),(el(j),b(j),j=1,5),Ifz(Nc),
     &                 Temp(1,Nc),Temp(2,Nc),Mw(Ngc),(Cft(Nc,k),k=1,9)
        ELSE
          READ (IOTHM) sub,nint,date(Ngc),(el(j),b(j),j=1,5),ifaz,t1,t2,
     &                 Mw(Ngc),thermo
        ENDIF
        IF ( Nonly.NE.0 ) THEN
          i = 1
 20       IF ( Prod(i).NE.sub.AND.'*'//Prod(i).NE.sub ) THEN
            i = i + 1
            IF ( i.LE.Nonly ) GOTO 20
            GOTO 200
          ELSE
            IF ( sub.EQ.Prod(Ngc-1) ) THEN
              Nonly = Nonly + 1
              DO k = Nonly,i + 1, - 1
                Prod(k) = Prod(k-1)
              ENDDO
            ELSE
              Prod(i) = Prod(Ngc)
            ENDIF
            Prod(Ngc) = sub
          ENDIF
        ELSEIF ( Nomit.NE.0 ) THEN
          DO i = 1,Nomit
            IF ( Omit(i).EQ.sub.OR.'*'//Omit(i).EQ.sub ) GOTO 200
          ENDDO
        ENDIF
        DO 50 k = 1,5
          IF ( b(k).EQ.0. ) GOTO 100
          DO i = 1,Nlm
            IF ( Elmt(i).EQ.el(k) ) THEN
              A(i,Ngc) = b(k)
              GOTO 50
            ENDIF
          ENDDO
          DO j = 1,Nlm
            A(j,Ngc) = 0.
          ENDDO
          GOTO 200
 50     CONTINUE
 100    Prod(Ngc) = sub
        IF ( itot.GT.ntgas ) THEN
          Nc = Nc + 1
          IF ( Nc.GT.MAXNC ) GOTO 400
        ELSE
          Ng = Ngc
          IF ( Ng.GT.MAXNG ) GOTO 400
          DO i = 1,3
            DO j = 1,9
              Coef(Ng,j,i) = thermo(j,i)
            ENDDO
          ENDDO
C IF SPECIES IS AN ATOMIC GAS, STORE INDEX IN JX
          IF ( b(2).EQ.0..AND.b(1).EQ.1. ) THEN
            DO i = 1,Nlm
              IF ( Elmt(i).EQ.el(1) ) THEN
                ne = ne + 1
                Jx(i) = Ngc
                Jcm(i) = Ngc
                GOTO 150
              ENDIF
            ENDDO
          ENDIF
        ENDIF
 150    Ngc = Ngc + 1
        IF ( Ngc.GT.MAXNGC ) GOTO 400
 200  CONTINUE
C FINISHED READING THERMO DATA FROM I/O UNIT IOTHM.
      Ifz(Nc) = 0
      Nc = Nc - 1
      Ngc = Ngc - 1
      Ngp1 = Ng + 1
      IF ( Ngc.LT.Nonly ) THEN
        DO k = Ngc + 1,Nonly
          WRITE (IOOUT,99001) Prod(k)
        ENDDO
      ENDIF
C FIND MISSING ELEMENTS (IF ANY) FOR COMPONENTS
      Nspx = Ngc
      IF ( ne.LT.Nlm ) THEN
        DO i = 1,Nlm
          IF ( Nspx.GT.MAXNGC ) GOTO 400
          IF ( Jx(i).EQ.0 ) THEN
            Nspx = Nspx + 1
            DO k = 1,Nlm
              A(k,Nspx) = 0.
            ENDDO
            A(i,Nspx) = 1.
            Prod(Nspx) = Elmt(i)
            DO k = 1,100
              IF ( Elmt(i).EQ.Symbol(k) ) THEN
                Mw(Nspx) = Atmwt(k)
                Atwt(i) = Atmwt(k)
                Cp(Nspx) = 2.5D0
                GOTO 210
              ENDIF
            ENDDO
 210        Jx(i) = Nspx
            Jcm(i) = Nspx
          ENDIF
        ENDDO
      ENDIF
C ARE ALL ELEMENTS IN PRODUCT SPECIES?
      DO 300 i = 1,Nlm
        DO j = 1,Ngc
          IF ( A(i,j).NE.0. ) GOTO 300
          ii = i
        ENDDO
        WRITE (IOOUT,99002) Elmt(ii)
        Ngc = 0
        GOTO 600
 300  CONTINUE
C WRITE POSSIBLE PRODUCT LIST
      IF ( .NOT.Short ) THEN
        WRITE (IOOUT,99003) Thdate
        DO i = 1,Ngc,3
          i5 = i + 2
          IF ( Ngc.LT.i5 ) i5 = Ngc
          WRITE (IOOUT,99004) (date(j),Prod(j),j=i,i5)
        ENDDO
      ENDIF
      GOTO 600
 400  WRITE (IOOUT,99005)
      Ngc = 0
      GOTO 600
C SEARCH FOR TRANSPORT PROPERTIES FOR THIS CHEMICAL SYSTEM
      ENTRY READTR
      REWIND IOTRN
      REWIND IOSCH
      Ntape = 0
      npure = 0
      lineb = 1
      IF ( .NOT.Short ) WRITE (IOOUT,99006)
      READ (IOTRN) nrec
      DO ir = 1,nrec
        READ (IOTRN) spece,trdata
        k = 1
 450    DO j = 1,Ng
          IF ( spece(k).EQ.Prod(j).OR.'*'//spece(k).EQ.Prod(j) ) THEN
            jj(k) = j
            IF ( k.EQ.2 ) THEN
C STORE NAMES FOR BINARIES IN BIN ARRAY.
              DO k = 1,2
                bin(k,lineb) = spece(k)
              ENDDO
              lineb = lineb + 1
              GOTO 500
            ELSE
              jj(2) = j
              IF ( spece(2).EQ.' ' ) THEN
C WRITE NAMES FOR PURE SPECIES.
                npure = npure + 1
                pure(npure) = spece(1)
                GOTO 500
              ELSE
                k = 2
                GOTO 450
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        GOTO 550
 500    WRITE (IOSCH) jj,trdata
        Ntape = Ntape + 1
 550    IF ( npure.NE.0.AND.(npure.GE.6.OR.ir.GE.nrec) ) THEN
          IF ( .NOT.Short ) WRITE (IOOUT,99007) (pure(jk),jk=1,npure)
          npure = 0
        ENDIF
      ENDDO
      lineb = lineb - 1
      IF ( .NOT.Short ) THEN
        WRITE (IOOUT,99008)
        DO j = 1,lineb
          WRITE (IOOUT,99009) (bin(i,j),i=1,2)
        ENDDO
      ENDIF
      WRITE (IOOUT,99010)
 600  RETURN
99001 FORMAT (/' WARNING!!  ',A15,' NOT A PRODUCT IN thermo.lib FILE ',
     &        '(SEARCH)')
99002 FORMAT (/' PRODUCT SPECIES CONTAINING THE ELEMENT',A3,' MISSING',
     &        //,13x,'FATAL ERROR (SEARCH)')
99003 FORMAT (/2x,'SPECIES BEING CONSIDERED IN THIS SYSTEM',
     &        /' (CONDENSED PHASE MAY HAVE NAME LISTED SEVERAL TIMES)',
     &        /'  LAST thermo.inp UPDATE: ',A10,/)
99004 FORMAT (3(2X,A6,2X,A15))
99005 FORMAT (/' INSUFFICIENT STORAGE FOR PRODUCTS-SEE RP-1311,',
     &        /'   PART 2, PAGE 39. (SEARCH)')
99006 FORMAT (/' SPECIES WITH TRANSPORT PROPERTIES'//8X,'PURE SPECIES'/)
99007 FORMAT (4(2x,A16))
99008 FORMAT (/'     BINARY INTERACTIONS'/)
99009 FORMAT (5X,2A16)
99010 FORMAT ()
      END
      SUBROUTINE SETEN
C***********************************************************************
C USE COMPOSITIONS FROM PREVIOUS POINT AS INITIAL ESTIMATES FOR
C CURRENT POINT NPT.  IF -
C  ISV>0  USE COMPOSITIONS FROM POINT ISV.
C  ISV<0  SAVE COMPOSITIONS FROM POINT -ISV FOR POSSIBLE LATER USE.
C         ALSO USE COMPOSITIONS FROM POINT -ISV FOR NPT.
C  ISV=0  USE COMPOSITIONS SAVED WHEN ISV<0.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER j,lsav
      REAL*8 tsave
      REAL*8 DEXP
      SAVE j,lsav,tsave
C
      IF ( Isv.LT.0 ) THEN
C FIRST T--SAVE COMPOSITIONS FOR FUTURE POINTS WITH THIS T
        Isv = -Isv
        tsave = Ttt(Isv)
        Ensave = Enn
        Enlsav = Ennl
        lsav = Lsave
        DO j = 1,Ng
          Sln(j) = Enln(j)
        ENDDO
        DO j = 1,Ng
          En(j,Npt) = En(j,Isv)
        ENDDO
        Npr = 0
        DO j = Ngp1,Ngc
          Sln(j) = En(j,Isv)
          En(j,Npt) = Sln(j)
          IF ( Jliq.EQ.j ) THEN
            En(Jsol,Npt) = En(Jsol,Isv) + En(Jliq,Isv)
            En(Jliq,Npt) = 0.
            Jsol = 0
            Jliq = 0
            tsave = tsave - 5.
            Tt = tsave
            Sln(j) = 0.
          ELSEIF ( En(j,Npt).GT.0. ) THEN
            Npr = Npr + 1
            Jcond(Npr) = j
          ENDIF
        ENDDO
      ELSEIF ( Isv.EQ.0 ) THEN
C NEXT POINT FIRST T IN SCHEDULE, USE PREVIOUS COMPOSITIONS FOR THIS T
        Jsol = 0
        Jliq = 0
        Enn = Ensave
        Ennl = Enlsav
        Lsave = lsav
        Npr = 0
        DO j = Ngp1,Ngc
          En(j,Npt) = Sln(j)
          IF ( En(j,Npt).GT.0.D0 ) THEN
            Npr = Npr + 1
            Jcond(Npr) = j
          ENDIF
        ENDDO
        DO j = 1,Ng
          En(j,Npt) = 0.
          Enln(j) = Sln(j)
          IF ( Sln(j).NE.0. ) THEN
            IF ( (Enln(j)-Ennl+18.5).GT.0. ) En(j,Npt) = DEXP(Enln(j))
          ENDIF
        ENDDO
        IF ( .NOT.Tp ) Tt = tsave
        Sumn = Enn
      ELSEIF ( Isv.GT.0 ) THEN
C USE COMPOSITIONS FROM PREVIOUS POINT
        DO j = 1,Ngc
          En(j,Npt) = En(j,Isv)
        ENDDO
      ENDIF
      END
      SUBROUTINE SHCK
C***********************************************************************
C PRIMARY ROUTINE FOR SHOCK PROBLEMS.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      CHARACTER*1 cr12,cr52
      INTEGER i,iof,it1,it2,itr,j,n
      LOGICAL refl,seql,srefl
      REAL*8 ax,axx,b2,cormax,gg,hs,m2m1(NCOL),mis(13),mu12rt,p1,p21,
     &       p21l,p2p1(NCOL),pmn,rho12,rho52,rrho(NCOL),sg(78),t1,t21,
     &       t21l,t2t1(NCOL),ttmax,u1u2(NCOL),uis(13),utwo(NCOL),uu,wmx,
     &       ww
      REAL*8 DABS,DEXP,DLOG,DMIN1
      SAVE ax,axx,b2,cormax,cr12,cr52,gg,hs,i,iof,it1,it2,itr,j,m2m1,
     &  mis,mu12rt,n,p1,p21,p21l,p2p1,pmn,refl,rho12,rho52,rrho,seql,sg,
     &  srefl,t1,t21,t21l,t2t1,ttmax,u1u2,uis,utwo,uu,wmx,ww
C
      IF ( Trace.EQ.0. ) Trace = 5.E-9
      Tp = .TRUE.
      Cpmix = 0.
      srefl = .FALSE.
      IF ( .NOT.Short ) THEN
        WRITE (IOOUT,99001)
        WRITE (IOOUT,99002) Incdeq,Refleq,Incdfz,Reflfz
      ENDIF
      IF ( Refleq.OR.Reflfz ) srefl = .TRUE.
      seql = Incdeq
      IF ( T(1).EQ.0. ) T(1) = Rtemp(1)
      DO i = 1,Nsk
        uis(i) = U1(i)
        mis(i) = Mach1(i)
        IF ( Mach1(i).EQ.0.0.AND.U1(i).EQ.0.0 ) GOTO 100
      ENDDO
 100  IF ( Nsk.GT.NCOL ) THEN
        WRITE (IOOUT,99003) NCOL
        Nsk = NCOL
      ENDIF
      IF ( .NOT.Short ) THEN
        WRITE (IOOUT,99004) (U1(i),i=1,Nsk)
        WRITE (IOOUT,99005) (Mach1(i),i=1,Nsk)
      ENDIF
      iof = 0
 200  iof = iof + 1
      Oxfl = Oxf(iof)
      CALL NEWOF
      Incdeq = seql
 300  refl = .FALSE.
      it2 = 2
      it1 = 1
      Pp = P(1)
      Tt = T(1)
      IF ( .NOT.Incdeq ) THEN
C FROZEN
        DO n = 1,Nsk
          Dlvtp(n) = 1.
          Dlvpt(n) = -1.
        ENDDO
      ENDIF
      DO Npt = 1,Nsk
        Ppp(Npt) = P(Npt)
        Ttt(Npt) = T(Npt)
        IF ( Npt.GT.1 ) THEN
          IF ( Ppp(Npt).EQ.0. ) Ppp(Npt) = Ppp(Npt-1)
          IF ( Ttt(Npt).EQ.0. ) Ttt(Npt) = Ttt(Npt-1)
          Ssum(Npt) = Ssum(Npt-1)
          Hsum(Npt) = Hsum(Npt-1)
          IF ( Ttt(Npt).EQ.Tt.AND.Ppp(Npt).EQ.Pp ) GOTO 350
        ENDIF
        Pp = Ppp(Npt)
        Tt = Ttt(Npt)
        IF ( Tt.GE.Tg(1)*.8D0 ) THEN
          CALL HCALC
          Hsum(Npt) = Hsub0
        ELSE
          WRITE (IOOUT,99016) Tt,Npt
          GOTO 1000
        ENDIF
 350    IF ( Cpmix.NE.0. ) Gamma1 = Cpmix/(Cpmix-1./Wmix)
        A1 = (Rr*Gamma1*Tt/Wmix)**.5
        IF ( U1(Npt).EQ.0. ) U1(Npt) = A1*Mach1(Npt)
        IF ( Mach1(Npt).EQ.0. ) Mach1(Npt) = U1(Npt)/A1
        Wm(Npt) = Wmix
        Cpr(Npt) = Cpmix
        Gammas(Npt) = Gamma1
        Vlm(Npt) = Rr*Tt/(Wmix*Pp)
      ENDDO
      Npt = Nsk
C OUTPUT--1ST CONDITION
      WRITE (IOOUT,99006)
      IF ( .NOT.Incdeq ) THEN
        WRITE (IOOUT,99008)
      ELSE
        WRITE (IOOUT,99007)
      ENDIF
      Eql = .FALSE.
      CALL OUT1
      WRITE (IOOUT,99009)
      Fmt(4) = '13'
      Fmt(5) = ' '
      Fmt(7) = '4,'
      WRITE (IOOUT,Fmt) 'MACH NUMBER1   ',(Mach1(j),j=1,Npt)
      Fmt(7) = '2,'
      WRITE (IOOUT,Fmt) 'U1, M/SEC      ',(U1(j),j=1,Npt)
      CALL OUT2
C BEGIN CALCULATIONS FOR 2ND CONDITION
      IF ( Incdeq ) Eql = .TRUE.
      Npt = 1
 400  Gamma1 = Gammas(Npt)
      uu = U1(Npt)
      wmx = Wm(Npt)
      p1 = Ppp(Npt)
      t1 = Ttt(Npt)
      hs = Hsum(Npt)
      IF ( refl ) uu = u1u2(Npt)
      mu12rt = wmx*uu**2/(Rr*t1)
      IF ( refl ) THEN
C REFLECTED--SUBSCRIPTS 2=1, 5=2, P52=P21
        t21 = 2.
        b2 = (-1.-mu12rt-t21)/2.
        p21 = -b2 + SQRT(b2**2-t21)
      ELSE
        p21 = (2.*Gamma1*Mach1(Npt)**2-Gamma1+1.)/(Gamma1+1.)
C THE FOLLOWING IMPROVED FORMULATION FOR THE INITIAL ESTIMATE FOR THE
C 2ND CONDITION WAS MADE AND TESTED BY S. GORDON 7/10/89.
        IF ( .NOT.Eql ) THEN
          t21 = p21*(2./Mach1(Npt)**2+Gamma1-1.)/(Gamma1+1.)
        ELSE
          Pp = p21*p1
          Tp = .FALSE.
          Hp = .TRUE.
          Hsub0 = hs + uu**2/(2.*Rr)
          CALL EQLBRM
          t21 = Ttt(Npt)/t1
          Hp = .FALSE.
          Tp = .TRUE.
        ENDIF
      ENDIF
      p21l = DLOG(p21)
      ttmax = 1.05*Tg(4)/t1
      t21 = DMIN1(t21,ttmax)
      t21l = DLOG(t21)
      itr = 1
 500  IF ( Shkdbg ) WRITE (IOOUT,99010) itr,it2,it1,p21,it2,it1,t21,
     &     rho52
      Tt = t21*t1
      Pp = p21*p1
      IF ( .NOT.Eql ) THEN
C FROZEN
        Tln = DLOG(Tt)
        IF ( .NOT.Incdeq ) THEN
          CALL HCALC
          IF ( Tt.EQ.0. ) GOTO 600
          Hsum(Npt) = Hsub0
          Cpr(Npt) = Cpmix
        ELSE
          CALL CPHS
          Cpr(Npt) = Cpsum
          Hsum(Npt) = 0.
          DO j = 1,Ng
            Hsum(Npt) = Hsum(Npt) + H0(j)*En(j,Npt)
          ENDDO
          Hsum(Npt) = Hsum(Npt)*Tt
        ENDIF
      ELSE
        CALL EQLBRM
        IF ( Tt.EQ.0. ) GOTO 800
      ENDIF
      rho12 = wmx*t21/(Wm(Npt)*p21)
      gg = rho12*mu12rt
      rho52 = 1./rho12
      IF ( refl ) gg = -mu12rt*rho52/(rho52-1.)**2
      G(1,1) = -gg*Dlvpt(Npt) - p21
      G(1,2) = -gg*Dlvtp(Npt)
      G(1,3) = p21 - 1. + gg - mu12rt
      IF ( refl ) G(1,3) = p21 - 1. + gg*(rho52-1.)
      gg = gg*t1/wmx
      IF ( .NOT.refl ) gg = gg*rho12
      G(2,1) = -gg*Dlvpt(Npt) + Tt*(Dlvtp(Npt)-1.)/Wm(Npt)
      G(2,2) = -gg*Dlvtp(Npt) - Tt*Cpr(Npt)
      gg = 1. - rho12**2
      IF ( refl ) gg = (rho52+1.)/(rho52-1.)
      G(2,3) = Hsum(Npt) - hs - uu**2*gg/(2.*Rr)
      X(3) = G(1,1)*G(2,2) - G(1,2)*G(2,1)
      X(1) = (G(1,3)*G(2,2)-G(2,3)*G(1,2))/X(3)
      X(2) = (G(1,1)*G(2,3)-G(2,1)*G(1,3))/X(3)
      IF ( Shkdbg ) THEN
        WRITE (IOOUT,99011) G(1,1),G(1,2),G(1,3)
        WRITE (IOOUT,99011) G(2,1),G(2,2),G(2,3)
        WRITE (IOOUT,99012) X(1),X(2)
        WRITE (IOOUT,99013) Hsum(Npt),hs,uu,uu*rho12
      ENDIF
      ax = DABS(X(1))
      axx = DABS(X(2))
      IF ( axx.GT.ax ) ax = axx
      IF ( ax.GE..00005 ) THEN
        cormax = .40546511
        IF ( itr.GT.4 ) cormax = .22314355
        IF ( itr.GT.12 ) cormax = .09531018
        IF ( itr.GT.20 ) cormax = .04879016
        ax = ax/cormax
        IF ( ax.GT.1. ) THEN
          X(1) = X(1)/ax
          X(2) = X(2)/ax
        ENDIF
        p21l = p21l + X(1)
        t21l = t21l + X(2)
        p21 = DEXP(p21l)
        t21 = DEXP(t21l)
        IF ( Shkdbg ) WRITE (IOOUT,99014) cormax,X(1),X(2)
        IF ( itr.NE.1.OR.t21.LT.ttmax ) THEN
          itr = itr + 1
          IF ( itr.LT.61 ) GOTO 500
          WRITE (IOOUT,99015) U1(Npt)
        ELSE
          Tt = 0.
          Npt = Npt - 1
          GOTO 700
        ENDIF
      ENDIF
C CONVERGED OR TOOK 60 ITERATIONS WITHOUT CONVERGING.
C STORE RESULTS.
 600  rrho(Npt) = rho52
      m2m1(Npt) = Wm(Npt)/wmx
      p2p1(Npt) = p21
      t2t1(Npt) = t21
      utwo(Npt) = uu*rho12
      u1u2(Npt) = uu - utwo(Npt)
      IF ( Tt.GE.Tg(1)*.8D0.AND.Tt.LE.Tg(4)*1.1D0 ) THEN
        IF ( .NOT.Eql ) THEN
C FROZEN
          Ppp(Npt) = Pp
          Ttt(Npt) = Tt
          Gammas(Npt) = Cpr(Npt)/(Cpr(Npt)-1./wmx)
          Vlm(Npt) = Rr*Tt/(wmx*Pp)
          IF ( Incdeq ) THEN
            Ssum(Npt) = 0.
            DO j = 1,Ngc
              pmn = Pp*wmx*En(j,Npt)
              IF ( En(j,Npt).GT.0. ) Ssum(Npt) = Ssum(Npt) + En(j,Npt)
     &             *(S(j)-DLOG(pmn))
            ENDDO
          ENDIF
        ENDIF
        GOTO 900
      ENDIF
 700  WRITE (IOOUT,99016) Tt,Npt
      Tt = 0.
 800  IF ( Npt.LT.1 ) GOTO 1000
      Nsk = Npt
 900  IF ( Trnspt ) CALL TRANP
      Isv = 0
      IF ( Npt.LT.Nsk ) Isv = Npt
      IF ( Npt.EQ.1 ) Isv = -1
      Npt = Npt + 1
      IF ( Eql ) CALL SETEN
      IF ( Npt.LE.Nsk ) GOTO 400
      Npt = Nsk
      IF ( refl ) THEN
        IF ( .NOT.Eql ) WRITE (IOOUT,99020)
        IF ( Eql ) WRITE (IOOUT,99021)
        cr12 = '2'
        cr52 = '5'
      ELSE
        IF ( .NOT.Eql ) WRITE (IOOUT,99018)
        IF ( Eql ) WRITE (IOOUT,99019)
        cr12 = '1'
        cr52 = '2'
      ENDIF
      Fmt(7) = '2,'
      WRITE (IOOUT,Fmt) 'U'//cr52//', M/SEC      ',(utwo(j),j=1,Npt)
      CALL OUT2
      IF ( Trnspt ) CALL OUT4
      WRITE (IOOUT,99017)
      Fmt(7) = '3,'
      WRITE (IOOUT,Fmt) 'P'//cr52//'/P'//cr12//'           ',
     &              (p2p1(j),j=1,Npt)
      WRITE (IOOUT,Fmt) 'T'//cr52//'/T'//cr12//'           ',
     &              (t2t1(j),j=1,Npt)
      Fmt(7) = '4,'
      WRITE (IOOUT,Fmt) 'M'//cr52//'/M'//cr12//'           ',
     &              (m2m1(j),j=1,Npt)
      WRITE (IOOUT,Fmt) 'RHO'//cr52//'/RHO'//cr12//'       ',
     &              (rrho(j),j=1,Npt)
      Fmt(7) = '2,'
      IF ( .NOT.refl ) WRITE (IOOUT,Fmt) 'V2, M/SEC      ',(u1u2(j),
     &               j=1,Npt)
      IF ( refl ) WRITE (IOOUT,Fmt) 'U5+V2,M/SEC    ',(u1u2(j),j=1,Npt)
      IF ( .NOT.Eql ) THEN
C WRITE FROZEN MOLE (OR MASS) FRACTIONS
        Fmt(7) = '5,'
        IF ( .NOT.Incdeq ) THEN
          IF ( Massf ) THEN
            WRITE (IOOUT,99022) 'MASS'
          ELSE
            WRITE (IOOUT,99022) 'MOLE'
            ww = wmx
          ENDIF
          DO n = 1,Nreac
            j = Jray(n)
            IF ( Massf ) ww = Mw(j)
            WRITE (IOOUT,99023) Prod(j),(En(j,i)*ww,i=1,Npt)
          ENDDO
        ELSE
          Eql = .TRUE.
          CALL OUT3
          Eql = .FALSE.
        ENDIF
      ELSE
        CALL OUT3
      ENDIF
      Iplt = MIN(Iplt+Npt,500)
      IF ( srefl ) THEN
        IF ( .NOT.refl ) THEN
          refl = .TRUE.
          it2 = 5
          it1 = 2
          Eql = .TRUE.
          IF ( Reflfz ) THEN
            Eql = .FALSE.
            IF ( Refleq ) THEN
              j = 0
              DO i = 1,Npt
                j = j + 1
                sg(j) = u1u2(i)
                j = j + 1
                sg(j) = Wm(i)
                j = j + 1
                sg(j) = Ppp(i)
                j = j + 1
                sg(j) = Ttt(i)
                j = j + 1
                sg(j) = Hsum(i)
                j = j + 1
                sg(j) = Gammas(i)
              ENDDO
            ENDIF
          ENDIF
          Npt = 1
          GOTO 400
        ELSEIF ( .NOT.Eql.AND.Refleq ) THEN
          j = 1
          DO i = 1,Npt
            u1u2(i) = sg(j)
            Wm(i) = sg(j+1)
            Ppp(i) = sg(j+2)
            Ttt(i) = sg(j+3)
            Hsum(i) = sg(j+4)
            Gammas(i) = sg(j+5)
            j = j + 6
          ENDDO
          Eql = .TRUE.
          Npt = 1
          GOTO 400
        ENDIF
      ENDIF
      IF ( Incdeq.AND.Incdfz ) THEN
        Incdeq = .FALSE.
        Eql = .FALSE.
        GOTO 300
      ELSEIF ( iof.GE.Nof ) THEN
        Tp = .FALSE.
        DO n = 1,Nreac
          Rtemp(n) = T(1)
        ENDDO
      ELSE
        DO i = 1,Nsk
          U1(i) = uis(i)
          Mach1(i) = mis(i)
        ENDDO
        GOTO 200
      ENDIF
 1000 RETURN
99001 FORMAT (/'   *** INPUT FOR SHOCK PROBLEMS ***')
99002 FORMAT (/' INCDEQ =',L2,'   REFLEQ =',L2,'   INCDFZ =',L2,
     &        '    REFLFZ =',L2)
99003 FORMAT (/' WARNING!!  ONLY ',I2,' u1 OR mach1 VALUES ALLOWED ',
     &        '(SHCK)')
99004 FORMAT (/1p,' U1 =   ',5E13.6,/(8X,5E13.6))
99005 FORMAT (/1p,' MACH1 =',5E13.6,/(8X,5E13.6))
99006 FORMAT (////25X,'SHOCK WAVE PARAMETERS ASSUMING')
99007 FORMAT (/,16X,' EQUILIBRIUM COMPOSITION FOR INCIDENT SHOCKED',
     &        ' CONDITIONS'//)
99008 FORMAT (/,17X,' FROZEN COMPOSITION FOR INCIDENT SHOCKED',
     &        ' CONDITI1ONS'//)
99009 FORMAT (/' INITIAL GAS (1)')
99010 FORMAT (/' ITR NO.=',I3,3X,'P',I1,'/P',I1,' =',F9.4,3X,'T',I1,
     &        '/T',I1,' =',F9.4,'   RHO2/RHO1 =',F9.6)
99011 FORMAT (/' G(I,J)  ',3E15.8)
99012 FORMAT (/' X       ',2E15.8)
99013 FORMAT (/' HSUM HS UU U2 ',4E15.8)
99014 FORMAT (/' MAX.COR.=',e13.6,' X(1)=',e13.6,' X(2)=',e13.6)
99015 FORMAT (/6x,' WARNING!!  NO CONVERGENCE FOR u1=',F8.1,
     &        /'  ANSWERS NOT RELIABLE, SOLUTION MAY NOT EXIST (SHCK)')
99016 FORMAT (/' TEMPERATURE=',E12.4,' IS OUT OF EXTENDED RANGE ',
     &        'FOR POINT',I5,' (SHCK)')
99017 FORMAT ()
99018 FORMAT (/' SHOCKED GAS (2)--INCIDENT--FROZEN')
99019 FORMAT (/' SHOCKED GAS (2)--INCIDENT--EQUILIBRIUM')
99020 FORMAT (/' SHOCKED GAS (5)--REFLECTED--FROZEN')
99021 FORMAT (/' SHOCKED GAS (5)--REFLECTED--EQUILIBRIUM')
99022 FORMAT (/1x,A4,' FRACTIONS'/)
99023 FORMAT (' ',A16,F8.5,12F9.5)
      END
      SUBROUTINE THERMP
C***********************************************************************
C ASSIGNED THERMODYNAMIC STATES.  HP,SP,TP,UV,SV, AND TV PROBLEMS.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER iof
      LOGICAL uv,tv,sv
      SAVE iof
C
      EQUIVALENCE (Hp,Uv)
      EQUIVALENCE (Tp,Tv)
      EQUIVALENCE (Sp,Sv)
      Eql = .TRUE.
      DO 100 iof = 1,Nof
        Oxfl = Oxf(iof)
        CALL NEWOF
C SET ASSIGNED P OR VOLUME
        DO Ip = 1,Np
          Pp = P(Ip)
C SET ASSIGNED T
          DO It = 1,Nt
            Vv = V(Ip)
            Tt = T(It)
            CALL EQLBRM
            IF ( Npt.EQ.0 ) GOTO 200
            IF ( Trnspt.AND.Tt.NE.0. ) CALL TRANP
            Isv = 0
            IF ( Ip.NE.Np.OR.It.NE.Nt.AND.Tt.NE.0. ) THEN
              Isv = Npt
              IF ( Npt.NE.NCOL ) GOTO 10
            ENDIF
            IF ( .NOT.Hp ) WRITE (IOOUT,99001)
            IF ( Hp ) WRITE (IOOUT,99002)
            IF ( .NOT.Vol ) THEN
              IF ( Hp ) WRITE (IOOUT,99006)
              IF ( Tp ) WRITE (IOOUT,99007)
              IF ( Sp ) WRITE (IOOUT,99008)
            ELSE
              IF ( Uv ) WRITE (IOOUT,99003)
              IF ( Tv ) WRITE (IOOUT,99004)
              IF ( Sv ) WRITE (IOOUT,99005)
            ENDIF
            CALL OUT1
            WRITE (IOOUT,99009)
            CALL OUT2
            IF ( Trnspt ) CALL OUT4
            CALL OUT3
            Iplt = MIN(Iplt+Npt,500)
            IF ( Isv.EQ.0.AND.iof.EQ.Nof ) GOTO 200
            WRITE (IOOUT,99010)
            Npt = 0
 10         Npt = Npt + 1
            IF ( .NOT.Tp.AND.Tt.NE.0. ) T(1) = Tt
            IF ( Nt.EQ.1.AND.Np.EQ.1 ) GOTO 100
            IF ( Ip.EQ.1.AND.It.EQ.1 ) Isv = -Isv
            IF ( Nt.NE.1 ) THEN
              IF ( It.EQ.Nt.OR.Tt.EQ.0. ) Isv = 0
            ENDIF
            CALL SETEN
          ENDDO
        ENDDO
 100  CONTINUE
 200  RETURN
99001 FORMAT (////15X,'THERMODYNAMIC EQUILIBRIUM PROPERTIES AT ASSIGNED'
     &        )
99002 FORMAT (////9X,
     &     'THERMODYNAMIC EQUILIBRIUM COMBUSTION PROPERTIES AT ASSIGNED'
     &     )
99003 FORMAT (/36X,' VOLUME'/)
99004 FORMAT (/28X,'TEMPERATURE AND VOLUME'/)
99005 FORMAT (/30X,'ENTROPY AND VOLUME'/)
99006 FORMAT (/34X,' PRESSURES'/)
99007 FORMAT (/27X,'TEMPERATURE AND PRESSURE'/)
99008 FORMAT (/29X,'ENTROPY AND PRESSURE'/)
99009 FORMAT (/' THERMODYNAMIC PROPERTIES'/)
99010 FORMAT (////)
      END
      SUBROUTINE TRANIN
C***********************************************************************
C BRINGS IN AND SORTS OUT INPUT FOR TRANSPORT CALCULATIONS
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER i,ii,inds(MAXTR),ir,j,jtape(2),k,k1,k2,kt,kvc,l,loop,m,nms
      LOGICAL change,elc1,elc2,ion1,ion2,setx
      REAL*8 coeff,debye,ekt,enel,enmin,ionic,lamda,omega,prop,qc,ratio,
     &       stcf(MAXTR,MAXTR),stcoef(MAXTR),te,testen,testot,total,
     &       trc(6,3,2),wmols(MAXTR),wmred,xsel,xss(MAXTR)
      REAL*8 DABS,DEXP,DLOG,DSQRT
      SAVE change,coeff,debye,ekt,elc1,elc2,enel,enmin,i,ii,inds,ion1,
     &  ion2,ionic,ir,j,jtape,k,k1,k2,kt,kvc,l,lamda,loop,m,nms,omega,
     &  prop,qc,ratio,setx,stcf,stcoef,te,testen,testot,total,trc,wmols,
     &  wmred,xsel,xss
C
      IF ( .NOT.Eql ) THEN
        IF ( .NOT.Shock ) THEN
          IF ( .NOT.setx ) THEN
            setx = .TRUE.
            Nm = nms
            DO i = 1,Nm
              Xs(i) = xss(i)
              Wmol(i) = wmols(i)
              Ind(i) = inds(i)
            ENDDO
          ENDIF
          GOTO 300
        ELSEIF ( .NOT.Incdeq ) THEN
          IF ( Npt.LE.1 ) THEN
            Nm = Nreac
            DO i = 1,Nm
              j = Jray(i)
              Ind(i) = j
              Wmol(i) = Mw(j)
              Xs(i) = En(j,1)*Wm(1)
            ENDDO
          ENDIF
          GOTO 300
        ENDIF
      ENDIF
C PICK OUT IMPORTANT SPECIES
      Nm = 0
      total = 0.D0
      enmin = 1.0D-11/Wm(Npt)
      testot = 0.999999999D0/Wm(Npt)
      DO i = 1,Lsave
        j = Jcm(i)
        IF ( En(j,Npt).LE.0.D0.AND.j.LE.Ngc ) THEN
          IF ( (Enln(j)-Ennl+25.328436D0).GT.0.D0 ) En(j,Npt)
     &         = DEXP(Enln(j))
        ENDIF
        Nm = Nm + 1
        Ind(Nm) = j
        total = total + En(j,Npt)
        IF ( Mw(j).LT.1.0D0 ) enel = En(j,Npt)
        En(j,Npt) = -En(j,Npt)
      ENDDO
      testen = 1.D0/(Ng*Wm(Npt))
      loop = 0
 100  IF ( total.LE.testot.AND.loop.LE.Ng ) THEN
        loop = loop + 1
        testen = testen/10.
        DO j = 1,Ng
          IF ( En(j,Npt).GE.testen ) THEN
            IF ( Nm.GE.MAXTR ) THEN
              WRITE (IOOUT,99001) Nm,Npt
              GOTO 200
            ELSE
              total = total + En(j,Npt)
              Nm = Nm + 1
              Ind(Nm) = j
              En(j,Npt) = -En(j,Npt)
            ENDIF
          ENDIF
        ENDDO
        IF ( testen.GT.enmin ) GOTO 100
      ENDIF
C CALCULATE MOLE FRACTIONS FROM THE EN(J,NPT)
 200  DO j = 1,Ng
        En(j,Npt) = DABS(En(j,Npt))
      ENDDO
      DO i = 1,Nm
        j = Ind(i)
        Wmol(i) = Mw(j)
        Xs(i) = En(j,Npt)/total
      ENDDO
      IF ( Npt.EQ.Nfz ) THEN
        nms = Nm
        DO i = 1,Nm
          xss(i) = Xs(i)
          wmols(i) = Wmol(i)
          inds(i) = Ind(i)
        ENDDO
        setx = .FALSE.
      ENDIF
C REWRITE REACTIONS TO ELIMINATE TRACE ELEMENTS
      Nr = Nm - Lsave
      IF ( Nr.NE.0 ) THEN
        DO k = 1,MAXTR
          DO m = 1,MAXTR
            Stc(k,m) = 0.0D0
          ENDDO
        ENDDO
        k = 1
        DO i = Lsave + 1,Nm
          Stc(k,i) = -1.0D0
          j = Ind(i)
          DO m = 1,Lsave
            Stc(k,m) = A(m,j)
          ENDDO
          k = k + 1
        ENDDO
        DO i = 1,Nm
          IF ( Xs(i).LT.1.0D-10 ) THEN
            m = 1
            change = .FALSE.
            DO 210 j = 1,Nr
              coeff = Stc(j,i)
              IF ( ABS(coeff).GT.1.0D-05 ) THEN
                IF ( .NOT.change ) THEN
                  change = .TRUE.
                  DO k = 1,Nm
                    stcoef(k) = Stc(j,k)/coeff
                  ENDDO
                  GOTO 210
                ELSE
                  DO k = 1,Nm
                    Stc(j,k) = (Stc(j,k)/coeff) - stcoef(k)
                  ENDDO
                ENDIF
              ENDIF
              DO k = 1,Nm
                stcf(m,k) = Stc(j,k)
              ENDDO
              m = m + 1
 210        CONTINUE
            DO ii = 1,Nm
              DO j = 1,Nr
                Stc(j,ii) = stcf(j,ii)
              ENDDO
            ENDDO
            Nr = m - 1
          ENDIF
        ENDDO
      ENDIF
C FIND TRANSPORT DATA FOR IMPORTANT INTERACTIONS
 300  DO i = 1,Nm
        Con(i) = 0.0
        DO j = 1,Nm
          Eta(i,j) = 0.0
        ENDDO
      ENDDO
      REWIND IOSCH
      DO 400 ir = 1,Ntape
        READ (IOSCH) jtape,trc
        DO 350 k = 1,2
          DO i = 1,Nm
            j = Ind(i)
            IF ( j.EQ.jtape(k) ) THEN
              l = i
              IF ( k.EQ.2 ) THEN
                kvc = 1
 302            kt = 1
                IF ( trc(2,1,kvc).NE.0.E0 ) THEN
                  IF ( trc(2,2,kvc).NE.0.E0 ) THEN
                    IF ( Tt.GT.trc(2,1,kvc) ) kt = 2
                    IF ( trc(2,3,kvc).NE.0. ) THEN
                      IF ( Tt.GT.trc(2,2,kvc) ) kt = 3
                    ENDIF
                  ENDIF
                  prop = EXP(trc(6,kt,kvc)
     &                   +(trc(5,kt,kvc)/Tt+trc(4,kt,kvc))
     &                   /Tt+trc(3,kt,kvc)*Tln)
                  IF ( kvc.EQ.2 ) THEN
                    Con(l) = prop
                    GOTO 400
                  ELSE
                    Eta(l,m) = prop
                    IF ( l.NE.m ) Eta(m,l) = Eta(l,m)
                  ENDIF
                ELSEIF ( kvc.EQ.2 ) THEN
                  GOTO 400
                ENDIF
                kvc = 2
                GOTO 302
              ELSE
                m = i
                GOTO 350
              ENDIF
            ENDIF
          ENDDO
          GOTO 400
 350    CONTINUE
 400  CONTINUE
C MAKE ESTIMATES FOR MISSING DATA
C
C INCLUDES ION CROSS SECTION ESTIMATES
C ESTIMATES FOR  E-ION, ION-ION, E-NEUTRAL, ION-NEUTRAL
C DEBYE SHIELDING WITH IONIC CUTOFF DISTANCE
      IF ( Ions ) THEN
        te = Tt/1000.D0
        ekt = 4.8032D0**2/(Boltz*te)
        qc = 100.D0*(ekt**2)
        xsel = enel/total
        IF ( xsel.LT.1.0D-12 ) xsel = 1.0D-12
        debye = ((22.5D0/Pi)*(Rr/Avgdr*100.D0)*(te/xsel))/ekt**3
        ionic = ((810.D0/(4.0D0*Pi))*(Rr/Avgdr*100D0)*(te/xsel))
     &          **(2.0/3.0)/ekt**2
        lamda = DSQRT(debye+ionic)
        lamda = MAX(lamda,2.71828183D0)
      ENDIF
      DO i = 1,Nm
        k = Ind(i)
        Cprr(i) = Cp(k)
        IF ( .NOT.(Ions.AND.(DABS(A(Nlm,k)).EQ.1.D0).AND.
     &       (Eta(i,i).EQ.0.D0)) ) THEN
          IF ( Eta(i,i).EQ.0.D0 ) THEN
            omega = DLOG(50.D0*Wmol(i)**4.6/Tt**1.4)
            omega = MAX(omega,1.D0)
            Eta(i,i) = Viscns*DSQRT(Wmol(i)*Tt)/omega
          ENDIF
          IF ( Con(i).EQ.0.D0 ) Con(i) = Eta(i,i)
     &         *Rr*(.00375D0+.00132D0*(Cprr(i)-2.5D0))/Wmol(i)
        ENDIF
      ENDDO
      DO i = 1,Nm
        DO 450 j = i,Nm
          ion1 = .FALSE.
          ion2 = .FALSE.
          elc1 = .FALSE.
          elc2 = .FALSE.
          omega = 0.0
          IF ( Eta(i,j).EQ.0. ) Eta(i,j) = Eta(j,i)
          IF ( Eta(j,i).EQ.0. ) Eta(j,i) = Eta(i,j)
          IF ( Eta(i,j).EQ.0. ) THEN
            IF ( Ions ) THEN
C ESTIMATE FOR IONS
              k1 = Ind(i)
              k2 = Ind(j)
              IF ( ABS(A(Nlm,k1)).EQ.1.0 ) ion1 = .TRUE.
              IF ( ABS(A(Nlm,k2)).EQ.1.0 ) ion2 = .TRUE.
              IF ( Wmol(i).LT.1.0 ) elc1 = .TRUE.
              IF ( Wmol(j).LT.1.0 ) elc2 = .TRUE.
              IF ( ion1.AND.ion2 ) omega = 1.36D0*qc*DLOG(lamda)
              IF ( (ion1.AND.elc2).OR.(ion2.AND.elc1) )
     &             omega = 1.29D0*qc*DLOG(lamda)
              IF ( (ion1.AND..NOT.ion2).OR.(ion2.AND..NOT.ion1) )
     &             omega = EXP(6.776-0.4*Tln)
              IF ( omega.NE.0. ) THEN
                wmred = DSQRT(2.0*Tt*Wmol(i)*Wmol(j)/(Wmol(i)+Wmol(j)))
                Eta(i,j) = Viscns*wmred*Pi/omega
                Eta(j,i) = Eta(i,j)
                IF ( i.EQ.j ) THEN
                  Cprr(i) = Cp(k1)
                  Con(i) = Eta(i,i)
     &                     *Rr*(.00375D0+.00132D0*(Cprr(i)-2.5D0))
     &                     /Wmol(i)
                ENDIF
                GOTO 450
              ENDIF
            ENDIF
C ESTIMATE FOR UNLIKE INTERACTIONS FROM RIGID SPHERE ANALOGY
            ratio = DSQRT(Wmol(j)/Wmol(i))
            Eta(i,j) = 5.656854D0*Eta(i,i)
     &                 *SQRT(Wmol(j)/(Wmol(i)+Wmol(j)))
            Eta(i,j) = Eta(i,j)/(1.D0+DSQRT(ratio*Eta(i,i)/Eta(j,j)))**2
            Eta(j,i) = Eta(i,j)
          ENDIF
 450    CONTINUE
      ENDDO
      RETURN
99001 FORMAT (/' WARNING!!  MAXIMUM ALLOWED NO. OF SPECIES',I3,
     &        ' WAS USED IN ',
     &        /' TRANSPORT PROPERTY CALCULATIONS FOR POINT',I3,
     &        '(TRANIN))')
      END
      SUBROUTINE TRANP
C***********************************************************************
C CALCULATES GAS TRANSPORT PROPERTIES
C
C   NUMBER OF GASEOUS SPECIES = NM   (MAXIMUM MAXTR)
C   NUMBER OF CHEMICAL REACTIONS = NR (NM - NLM)
C   ARRAY OF STOICHIOMETRIC COEFFICIENTS = STC
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C LOCAL VARIABLES
      INTEGER i,i1,j,jj,k,m,mm,nlmm,nmm
      REAL*8 cpreac,delh(MAXTR),gmat(MAXMAT,MAXMAT+1),phi(MAXTR,MAXTR),
     &       psi(MAXTR,MAXTR),reacon,rtpd(MAXTR,MAXTR),stx(MAXTR),
     &       stxij(MAXTR,MAXTR),sumc,sumv,wtmol,xskm(MAXTR,MAXTR)
      REAL*8 DABS
      SAVE cpreac,delh,gmat,i,i1,j,jj,k,m,mm,nlmm,nmm,phi,psi,reacon,
     &  rtpd,stx,stxij,sumc,sumv,wtmol,xskm
C
      CALL TRANIN
C CALCULATE VISCOSITY AND FROZEN THERMAL CONDUCTIVITY
      nmm = Nm - 1
      DO i = 1,Nm
        rtpd(i,i) = 0.D0
        phi(i,i) = 1.D0
        psi(i,i) = 1.D0
      ENDDO
      Confro(Npt) = 0.D0
      Vis(Npt) = 0.D0
      DO i = 1,nmm
        i1 = i + 1
CDIR$ IVDEP
        DO j = i1,Nm
          sumc = 2.D0/(Eta(i,j)*(Wmol(i)+Wmol(j)))
          phi(i,j) = sumc*Wmol(j)*Eta(i,i)
          phi(j,i) = sumc*Wmol(i)*Eta(j,j)
          sumc = (Wmol(i)+Wmol(j))**2
          psi(i,j) = phi(i,j)
     &               *(1.D0+2.41D0*(Wmol(i)-Wmol(j))*(Wmol(i)-.142D0*
     &               Wmol(j))/sumc)
          psi(j,i) = phi(j,i)
     &               *(1.D0+2.41D0*(Wmol(j)-Wmol(i))*(Wmol(j)-.142D0*
     &               Wmol(i))/sumc)
        ENDDO
      ENDDO
      DO i = 1,Nm
        sumc = 0.D0
        sumv = 0.D0
        DO j = 1,Nm
          sumc = sumc + psi(i,j)*Xs(j)
          sumv = sumv + phi(i,j)*Xs(j)
        ENDDO
        Vis(Npt) = Vis(Npt) + Eta(i,i)*Xs(i)/sumv
        Confro(Npt) = Confro(Npt) + Con(i)*Xs(i)/sumc
      ENDDO
      IF ( Eql.AND.Nr.GT.0 ) THEN
C CALCULATE REACTION HEAT CAPACITY AND THERMAL CONDUCTIVITY
        m = Nr + 1
        DO i = 1,Nr
          delh(i) = 0.0D0
          DO k = 1,Lsave
            j = Jcm(k)
            delh(i) = Stc(i,k)*H0(j) + delh(i)
          ENDDO
          nlmm = Lsave + 1
          DO k = nlmm,Nm
            j = Ind(k)
            delh(i) = Stc(i,k)*H0(j) + delh(i)
          ENDDO
          G(i,m) = delh(i)
        ENDDO
        DO i = 1,MAXTR
          DO j = 1,MAXTR
            IF ( DABS(Stc(i,j)).LT.1.0D-6 ) Stc(i,j) = 0.0D0
          ENDDO
        ENDDO
        jj = Nm - 1
        DO k = 1,jj
          mm = k + 1
          DO m = mm,Nm
            rtpd(k,m) = Wmol(k)*Wmol(m)/(1.1*Eta(k,m)*(Wmol(k)+Wmol(m)))
            xskm(k,m) = Xs(k)*Xs(m)
            xskm(m,k) = xskm(k,m)
            rtpd(m,k) = rtpd(k,m)
          ENDDO
        ENDDO
        DO i = 1,Nr
          DO j = i,Nr
            G(i,j) = 0.0D0
            gmat(i,j) = 0.0D0
          ENDDO
        ENDDO
        DO k = 1,jj
          mm = k + 1
          DO m = mm,Nm
            IF ( Xs(k).GE.1.0D-10.AND.Xs(m).GE.1.0D-10 ) THEN
              DO j = 1,Nr
                IF ( (Stc(j,k).EQ.0.D0).AND.(Stc(j,m).EQ.0.D0) ) stx(j)
     &               = 0.D0
                IF ( (Stc(j,k).NE.0.D0).OR.(Stc(j,m).NE.0.D0) ) stx(j)
     &               = Xs(m)*Stc(j,k) - Xs(k)*Stc(j,m)
              ENDDO
              DO i = 1,Nr
                DO j = i,Nr
                  stxij(i,j) = stx(i)*stx(j)/xskm(k,m)
                  G(i,j) = G(i,j) + stxij(i,j)
                  gmat(i,j) = gmat(i,j) + stxij(i,j)*rtpd(k,m)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ENDDO
        m = 1 + Nr
        DO i = 1,Nr
CDIR$ IVDEP
          DO j = i,Nr
            G(j,i) = G(i,j)
          ENDDO
          G(i,m) = delh(i)
        ENDDO
        Imat = Nr
        CALL GAUSS
        cpreac = 0.D0
        DO i = 1,Nr
          G(i,m) = delh(i)
          cpreac = cpreac + R*delh(i)*X(i)
CDIR$ IVDEP
          DO j = i,Nr
            G(i,j) = gmat(i,j)
            G(j,i) = G(i,j)
          ENDDO
        ENDDO
        CALL GAUSS
        reacon = 0.D0
        DO i = 1,Nr
          reacon = reacon + R*delh(i)*X(i)
        ENDDO
        reacon = .6D0*reacon
      ELSE
        cpreac = 0.D0
        reacon = 0.D0
      ENDIF
C CALCULATE OTHER ANSWERS
      Cpfro(Npt) = 0.D0
      wtmol = 0.D0
      DO i = 1,Nm
        Cpfro(Npt) = Cpfro(Npt) + Xs(i)*Cprr(i)
        wtmol = wtmol + Xs(i)*Wmol(i)
      ENDDO
      Cpfro(Npt) = Cpfro(Npt)*R/wtmol
      Confro(Npt) = Confro(Npt)/1000.D0
      IF ( .NOT.Siunit ) Confro(Npt) = Confro(Npt)/4.184D0
      Vis(Npt) = Vis(Npt)/1000.D0
      Prfro(Npt) = Vis(Npt)*Cpfro(Npt)/Confro(Npt)
      IF ( Eql ) THEN
        cpreac = cpreac/wtmol
        reacon = reacon/1000.D0
        Cpeql(Npt) = cpreac + Cpfro(Npt)
        Coneql(Npt) = Confro(Npt) + reacon
        Preql(Npt) = Vis(Npt)*Cpeql(Npt)/Coneql(Npt)
      ENDIF
      END
      SUBROUTINE UTHERM(Readok)
C***********************************************************************
C READ THERMO DATA FROM I/O UNIT 7 IN RECORD FORMAT AND WRITE
C UNFORMATTED ON I/O UNIT IOTHM.  DATA ARE REORDERED GASES FIRST.
C
C USES SCRATCH I/O UNIT IOSCH.
C
C UTHERM IS CALLED FROM SUBROUTINE INPUT.
C
C NOTE:  THIS ROUTINE MAY BE CALLED DIRECTLY AND USED BY ITSELF TO
C PROCESS THE THERMO DATA.
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
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C DUMMY ARGUMENTS
      LOGICAL Readok
C LOCAL VARIABLES
      CHARACTER*15 name
      CHARACTER*16 namee
      CHARACTER*65 notes
      CHARACTER*2 sym(5)
      CHARACTER*6 date
      INTEGER i,ifaz,ifzm1,inew,int,j,k,kk,l,nall,ncoef,ngl,ns,ntl
      INTEGER INDEX
      LOGICAL fill(3)
      REAL*8 aa,atms,cpfix,dlt,expn(8),fno(5),hform,hh,mwt,templ(9),tex,
     &       tgl(4),thermo(9,3),tinf,tl(2),ttl,tx
      REAL*8 DBLE,DLOG
C
      ngl = 0
      ns = 0
      nall = 0
      ifzm1 = 0
      inew = 0
      tinf = 1.D06
      REWIND IOSCH
      READ (IOINP,99001) tgl,Thdate
 100  DO i = 1,3
        fill(i) = .TRUE.
        DO j = 1,9
          thermo(j,i) = 0.
        ENDDO
      ENDDO
      hform = 0.
      tl(1) = 0.
      tl(2) = 0.
      READ (IOINP,99002,END=300,ERR=400) name,notes
      IF ( name(:3).EQ.'END'.OR.name(:3).EQ.'end' ) THEN
        IF ( INDEX(name,'ROD').EQ.0.AND.INDEX(name,'rod').EQ.0 )
     &       GOTO 300
        ns = nall
        GOTO 100
      ENDIF
      READ (IOINP,99003,ERR=400) ntl,date,(sym(j),fno(j),j=1,5),
     &                 ifaz,mwt,hform
      WRITE (IOOUT,99004) name,date,hform,notes
C IF NTL=0, REACTANT WITHOUT COEFFICIENTS
      IF ( ntl.EQ.0 ) THEN
        IF ( ns.EQ.0 ) GOTO 300
        nall = nall + 1
        READ (IOINP,99005,ERR=400) tl,ncoef,expn,hh
        thermo(1,1) = hform
        WRITE (IOSCH) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,mwt,
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
      DO 200 i = 1,ntl
        READ (IOINP,99005,ERR=400) tl,ncoef,expn,hh
        READ (IOINP,99006,ERR=400) templ
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
        DO 150 l = 1,ncoef
          DO k = 1,7
            IF ( expn(l).EQ.DBLE(k-3) ) THEN
              thermo(k,int) = templ(l)
              GOTO 150
            ENDIF
          ENDDO
 150    CONTINUE
        thermo(8,int) = templ(8)
        thermo(9,int) = templ(9)
        IF ( ifaz.GT.0 ) THEN
          nall = nall + 1
          IF ( ifaz.GT.ifzm1 ) THEN
            inew = inew + 1
          ELSE
            inew = i
          ENDIF
          WRITE (IOSCH) name,ntl,date,(sym(j),fno(j),j=1,5),inew,tl,mwt,
     &                  thermo
        ENDIF
 200  CONTINUE
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
C WRITE COEFFICIENTS ON SCRATCH I/O UNIT IOSCH
        WRITE (IOSCH) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                thermo
      ENDIF
      GOTO 100
C END OF DATA. COPY CONDENSED & REACTANT DATA FROM IOSCH & ADD TO IOTHM.
 300  REWIND IOSCH
      IF ( ns.EQ.0 ) ns = nall
      WRITE (IOTHM) tgl,ngl,ns,nall,Thdate
C WRITE GASEOUS PRODUCTS ON IOTHM
      IF ( ngl.NE.0 ) THEN
        DO i = 1,ns
          READ (IOSCH) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                 thermo
          IF ( ifaz.LE.0 ) WRITE (IOTHM) name,ntl,date,
     &                            (sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                            thermo
        ENDDO
      ENDIF
      IF ( ngl.NE.nall ) THEN
C WRITE CONDENSED PRODUCTS AND REACTANTS ON IOTHM
        REWIND IOSCH
        DO i = 1,nall
          READ (IOSCH) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,mwt,
     &                 thermo
          IF ( i.GT.ns ) THEN
            WRITE (IOTHM) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,
     &                    mwt,thermo(1,1)
            IF ( ntl.GT.0 ) WRITE (IOTHM) thermo
          ELSEIF ( ifaz.GT.0 ) THEN
            WRITE (IOTHM) name,ntl,date,(sym(j),fno(j),j=1,5),ifaz,tl,
     &                    mwt,(thermo(k,1),k=1,9)
          ENDIF
        ENDDO
      ENDIF
      RETURN
 400  WRITE (IOOUT,99007) name
      Readok = .FALSE.
      RETURN
99001 FORMAT (4F10.3,a10)
99002 FORMAT (a15,a65)
99003 FORMAT (i2,1x,a6,1x,5(a2,f6.2),i2,f13.5,f15.3)
99004 FORMAT (' ',a15,2x,a6,e15.6,2x,a65)
99005 FORMAT (2F11.3,i1,8F5.1,2x,f15.3)
99006 FORMAT (5D16.8/2D16.8,16x,2D16.8)
99007 FORMAT (/' ERROR IN PROCESSING thermo.inp AT OR NEAR ',A15,
     &        ' (UTHERM)')
      END
      SUBROUTINE UTRAN(Readok)
C***********************************************************************
C READ TRANSPORT PROPERTIES FORM I/O UNIT 7 IN RECORD FORMAT AND WRITE
C UNFORMATTED ON I/O UNIT IOTRN.  USES SCRATCH I/O UNIT IOSCH.
C
C UTRAN IS CALLED FROM SUBROUTINE INPUT AFTER A RECORD WITH 'tran'
C IN COLUMNS 1-4 HAS BEEN READ.
C
C NOTE:  THIS ROUTINE MAY BE CALLED DIRECTLY  AND USED BY ITSELF TO
C PROCESS THE TRANSPORT PROPERTY DATA.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C DUMMY ARGUMENTS
      LOGICAL Readok
C LOCAL VARIABLES
      CHARACTER*16 tname(2)
      CHARACTER*1 vorc
      INTEGER i,ic,in,iv,j,k,ncc,nn,ns,nv
      REAL*8 cc,tc(36),tcin(6),trcoef(6,3,2),vvl
C
      EQUIVALENCE (tc(1),trcoef(1,1,1))
      ns = 0
      REWIND IOSCH
 100  DO i = 1,36
        tc(i) = 0.
      ENDDO
      READ (IOINP,99001) tname,vvl,nv,cc,ncc
      IF ( tname(1).EQ.'end'.OR.tname(1).EQ.'LAST' ) THEN
        WRITE (IOTRN) ns
        REWIND IOSCH
        DO i = 1,ns
          READ (IOSCH,ERR=200) tname,trcoef
          WRITE (IOTRN) tname,trcoef
        ENDDO
        GOTO 300
      ELSE
        ic = 0
        iv = 0
        nn = nv + ncc
        IF ( nv.LE.3.AND.ncc.LE.3 ) THEN
          DO in = 1,nn
            READ (IOINP,99002) vorc,tcin
            IF ( vorc.EQ.'C' ) THEN
              k = 2
              ic = ic + 1
              j = ic
            ELSE
              k = 1
              iv = iv + 1
              j = iv
            ENDIF
            IF ( j.GT.3 ) GOTO 200
            DO i = 1,6
              trcoef(i,j,k) = tcin(i)
            ENDDO
          ENDDO
          ns = ns + 1
          WRITE (IOSCH) tname,trcoef
          GOTO 100
        ENDIF
      ENDIF
 200  WRITE (IOOUT,99003) tname
      Readok = .FALSE.
 300  RETURN
99001 FORMAT (2A16,2X,A1,I1,A1,I1)
99002 FORMAT (1X,A1,2F9.2,4E15.8)
99003 FORMAT (/' ERROR IN PROCESSING trans.inp AT OR NEAR (UTRAN)',/1X,
     &        2A16)
      END
      SUBROUTINE VARFMT(Vx)
C***********************************************************************
C SET DECIMAL PLACES ACCORDING TO NUMBER SIZE FOR F-FORMAT IN
C VARIABLE FORMAT FMT.
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'cea.inc'
C DUMMY ARGUMENTS
      REAL*8 Vx(NCOL)
C LOCAL VARIABLES
      INTEGER i,k
      REAL*8 vi
      REAL*8 DABS
      SAVE i,k,vi
C
      DO i = 1,Npt
        vi = DABS(Vx(i))
        k = 2*i + 3
        Fmt(k) = '5,'
        IF ( vi.GE.1. ) Fmt(k) = '4,'
        IF ( vi.GE.10. ) Fmt(k) = '3,'
        IF ( vi.GE.100. ) Fmt(k) = '2,'
        IF ( vi.GE.10000. ) Fmt(k) = '1,'
        IF ( vi.GE.1000000. ) Fmt(k) = '0,'
      ENDDO
      Fmt(29)(2:) = ' '
      END
