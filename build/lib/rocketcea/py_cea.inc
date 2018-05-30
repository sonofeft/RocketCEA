C***********************************************************************
C                            cea.include  
C***********************************************************************
C
C  The following parameters set the maximum dimensions for many variables
C    They are defined in part 2, page 39 of the manuals, NASA RP-1311.
C    The variable NCOL set the number of columns in the output.  It may
C    be increased for wider paper or smaller fonts.
C
      INTEGER MAXNGC,MAXNC,NCOL,MAXMAT,MAXTR,MAXR,MAXEL,MAXNG,MAXMIX,
     &        MAXT,MAXPV,NLINES
      PARAMETER (MAXNGC=600)
      PARAMETER (MAXNC=300)
      PARAMETER (NCOL=8)
      PARAMETER (MAXMAT=50)
      PARAMETER (MAXTR=30)
      PARAMETER (MAXR=24)
      PARAMETER (MAXEL=20)
      PARAMETER (MAXNG=500)
      PARAMETER (MAXMIX=52)
      PARAMETER (MAXT=51)
      PARAMETER (MAXPV=26)
C
C  The following parameters set the input/output unit numbers.  These
C    numbers are also defined in the manual, part2 p39, and may be 
C    adjusted as desired.
C
      INTEGER IOSCH,IOTHM,IOPLT,IOTRN
      PARAMETER (IOSCH=13)
      PARAMETER (IOTHM=14)
      PARAMETER (IOPLT=15)
      PARAMETER (IOTRN=18)
      PARAMETER (NLINES=50)
C***********************************************************************
      integer read_d
      integer NLinesSet, NLine, makeio
      common /cet_comm/makeio, read_d, NLinesSet, NLine
      
      CHARACTER*132 InpLines(NLINES)
      common /cet_inp/InpLines
      
      REAL*8 Enn,Ennl,Enlsav,Ensave,Sumn
      REAL*8 Deln(MAXNGC),Enln(MAXNGC),Sln(MAXNGC)
      REAL*8 En(MAXNGC,NCOL)
      COMMON /COMP  / Deln,En,Enln,Enn,Ennl,Enlsav,Ensave,Sln,Sumn
C
      INTEGER Ip,Iplt,It,Nc,Ng,Ngp1,Nlm,Nplt,Nof,Nomit,Nonly,Np,Npr,Npt,
     &        Ngc,Nsert,Nspr,Nspx,Nt
      INTEGER Jcond(45),Jx(MAXEL),Nfla(MAXR),Ifz(MAXNC)
      COMMON /INDX  / Ip,Iplt,It,Jcond,Jx,Nc,Ng,Ngp1,Nlm,Nplt,Nof,Nomit,
     &                Nonly,Np,Npr,Npt,Ngc,Nsert,Nspr,Nspx,Nt,Nfla,Ifz
C
      REAL*8 Cpmix,Wmix,Bcheck
      REAL*8 Am(2),Hpp(2),Vmin(2),Vpls(2),Wp(2),Atmwt(101),Oxf(MAXMIX),
     &       P(MAXPV),Rh(2),T(MAXT),V(MAXPV),Valnce(101)
      REAL*8 B0p(MAXEL,2)
      COMMON /INPT  / Am,B0p,Cpmix,Hpp,Vmin,Vpls,Wmix,Wp,Atmwt,Bcheck,
     &                Oxf,P,Rh,T,V,Valnce
C
      INTEGER Imat,Iq1,Isv,Jliq,Jsol,Lsave,Msing
      COMMON /MISCI / Imat,Iq1,Isv,Jliq,Jsol,Lsave,Msing
C
      LOGICAL Convg,Debug(NCOL),Detdbg,Detn,Eql,Gonly,Hp,Ions,Massf,
     &        Moles,Newr,Pderiv,Shock,Short_bn,Siunit,Sp,Tp,Trnspt,Vol
      COMMON /MISCL / Convg,Debug,Detdbg,Detn,Eql,Gonly,Hp,Ions,Massf,
     &                Moles,Newr,Pderiv,Shock,Short_bn,
     &                Siunit,Sp,Tp,Trnspt,
     &                Vol
C
      REAL*8 Avgdr,Boltz,Eqrat,Hsub0,Oxfl,Pi,Pp,R,Rr,Size_bn,S0,Tln,Tm,
     &       Trace,Tt,Viscns,Vv
      REAL*8 Atwt(MAXEL),B0(MAXEL),X(MAXMAT)
      REAL*8 A(MAXEL,MAXNGC),G(MAXMAT,MAXMAT+1)
      COMMON /MISCR / A,Atwt,Avgdr,Boltz,B0,Eqrat,G,Hsub0,Oxfl,Pi,Pp,R,
     &                Rr,Size_bn,S0,Tln,Tm,Trace,Tt,Viscns,Vv,X
C
      CHARACTER*2 Elmt(MAXEL),Ratom(MAXR,12),Symbol(101)
      CHARACTER*4 Fmt(30)
      CHARACTER*8 Fox(MAXR)
      CHARACTER*10 Thdate
      CHARACTER*15 Case_bn,Energy(MAXR),Omit(0:MAXNGC),Pltvar(20),
     &             Prod(0:MAXNGC),Rname(MAXR)
      CHARACTER*200 Pfile
      COMMON /CDATA / Case_bn,Elmt,Energy,Fmt,Fox,Omit,Pltvar,Prod,
     &                Ratom,
     &                Rname,Symbol,Thdate,Pfile
C
      REAL*8 Cpr(NCOL),Dlvpt(NCOL),Dlvtp(NCOL),Gammas(NCOL),Hsum(NCOL),
     &       Ppp(NCOL),Ssum(NCOL),Totn(NCOL),Ttt(NCOL),Vlm(NCOL),
     &       Wm(NCOL)
      REAL*8 Pltout(500,20)
      COMMON /PRTOUT/ Cpr,Dlvpt,Dlvtp,Gammas,Hsum,Ppp,Ssum,Totn,Ttt,Vlm,
     &                Wm,Pltout
C
      INTEGER Nreac
      INTEGER Jray(MAXR)
      REAL*8 Dens(MAXR),Enth(MAXR),Pecwt(MAXR),Rmw(MAXR),Rtemp(MAXR)
      REAL*8 Rnum(MAXR,12)
      COMMON /REACTN/ Dens,Enth,Pecwt,Rmw,Rnum,Rtemp,Jray,Nreac
C
      REAL*8 Cpsum
      REAL*8 Cft(MAXNC,9),Coef(MAXNG,9,3),Temp(2,MAXNC)
      REAL*8 Cp(MAXNGC),H0(MAXNGC),Mu(MAXNGC),Mw(MAXNGC),S(MAXNGC),Tg(4)
      COMMON /THERM / Cft,Coef,Cp,Cpsum,H0,Mu,Mw,S,Temp,Tg
C
      INTEGER Iopt,Isup,Nfz,Npp,Nsub,Nsup
      LOGICAL Area,Debugf,Fac,Froz,Page1,Rkt
      REAL*8 Acat,Awt,Cstr,Tcest,Ma
      REAL*8 Aeat(NCOL),App(NCOL),Pcp(2*NCOL),Sonvel(NCOL),Spim(NCOL),
     &       Subar(13),Supar(NCOL),Vmoc(NCOL),vaci(NCOL)
      COMMON /ROCKT / Acat,Aeat,App,Awt,Cstr,Ma,Pcp,Sonvel,Spim,Subar,
     &                Supar,Vmoc, Vaci,Tcest,Area,Iopt,
     &                Debugf,Fac,Froz,Isup,
     &                Nfz,Npp,Nsub,Nsup,Page1,Rkt
C
      INTEGER Nsk
      LOGICAL Incdeq,Incdfz,Refleq,Reflfz,Shkdbg
      REAL*8 U1(NCOL),Mach1(NCOL),A1,Gamma1
      COMMON /SHOCKS/ U1,Mach1,A1,Gamma1,Incdeq,Incdfz,Refleq,Reflfz,
     &                Shkdbg,Nsk
C
      INTEGER Nm,Nr,Ntape
      INTEGER Ind(MAXTR),Jcm(MAXEL)
      REAL*8 Cprr(MAXTR),Con(MAXTR),Wmol(MAXTR),Xs(MAXTR)
      REAL*8 Eta(MAXTR,MAXTR),Stc(MAXTR,MAXTR)
      COMMON /TRNP  / Cprr,Con,Eta,Wmol,Xs,Stc,Ind,Jcm,Nm,Nr,Ntape
C
      REAL*8 Coneql(NCOL),Confro(NCOL),Cpeql(NCOL),Cpfro(NCOL),
     &       Preql(NCOL),Prfro(NCOL),Vis(NCOL)
      COMMON /TRPTS / Coneql,Confro,Cpeql,Cpfro,Preql,Prfro,Vis
