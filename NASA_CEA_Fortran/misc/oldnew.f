      PROGRAM OLDNEW
C   Program converts a file of multiple old form NASA coefficients
C   to new file of multiple new form NASA coefficients.  All gases
C   are converted to a two-range version of new form (numint=2)
C   with 1000 as the common temperature (Tcom).  Condensed species
C   in sequence produce a series (phase=1,2,3..) of new form
C   coefficients in order of increasing temperature.
C  
C   The output goes to a file organized as:
C         Rec 1
C             col 1-16        name
C             col 17-24       blank
C             col 25-80       comments (not available from old form)
C         Rec 2
C             col 1           blank
C             col 2           number of temperature intervals (numint)
C             col 3           blank
C             col 4-7         (code)
C             col 8-9         (date)
C             col 10          blank
C             col 11-50       formula ( 5(A2,F6.2)  )
C                              (el1,n1,el2,n2,el3,n3,el4,n4,el5,n5)
C             col 51          blank
C             col 52          (phase) See below for code.
C             col 53-65       molecular weight (molwgt).
C             col 66-80       heat of formation in J/mol (htform)
C         Rec 3
C             col 1           blank
C             col 2-11        low temp of first interval (Tlow)
C             col 12-21       high temp of first interval (Tcom)
C             col 22          blank
C             col 23          Number of Coefficients for Cp
C             col 24-63       T exponents  (ex = 0,1,2,3,4,5,0,0)
C             col 66-80       H-HO at 298.15 J/mole  (rh)
C                             (not available on old form)
C         Rec 4
C             col 1-80        first 5 coefficients 
C                             (alow, blow, clow, dlow, elow)
C         Rec 5
C             col 1-48        last 3 coefficients (xlow, ylow, zlow)
C                             (not available on old form)
C             col 49-80       Integration constants (flow, glow)
C         Rec 6
C             col 1           blank
C             col 2-11        low temp of second interval (Tcom)
C             col 12-21       high temp of second interval (Thi)
C             col 22          blank
C             col 23          Number of Coefficients for Cp
C             col 24-63       T exponents (ex = 0,1,2,3,4,5,0,0)
C             col 66-80       H-HO at 298.15 J/mole  (rh)
C                             (not available on old form)
C         Rec 7
C             col 1-80        first 5 coefficients 
C                             (ahi, bhi, chi, dhi, ehi)
C         Rec 8
C             col 1-48        last 3 coefficients (xhi, yhi, zhi)
C                             (not available on old form)
C             col 49-80       Integration constants (fhi, ghi)
C
C         Data from records beyond 8 are not used in the old form.
C
C   The input comes from a file organized as:
C         Rec 1
C             col 1-16        (name)
C             col 17-18       blank
C             col 19-22       (code)
C             col 23-24       (date)
C             col 25-44       formula  (  4(A2,F3.0)  )
C                                 (el1,n1,el2,n2,el3,n3,el4,n4)
C             col 45          (phase)  See code below.
C             col 46-55       low temp of low range (Tlow)
C             col 56-65       high temp of high range (Thi)
C             col 66-78       molecular weight (molwgt)
C             col 79          blank
C             col 80          blank (might hold integer '1')
C         Rec 2
C             col 1-15        coefficient  (ahi)
C             col 16-30       coefficient  (bhi)
C             col 31-45       coefficient  (chi)
C             col 45-60       coefficient  (dhi)
C             col 61-75       coefficient  (ehi)
C             col 76-79       blank
C             col 80          blank (might hold integer '2')
C         Rec 3
C             col 1-15        coefficient  (fhi)
C             col 16-30       coefficient  (ghi)
C             col 31-45       coefficient  (alow)
C             col 46-60       coefficient  (blow)
C             col 61-75       coefficient  (clow)
C             col 76-79       blank
C             col 80          blank (might hold integer '3')
C         Rec 4
C             col 1-15        coefficient  (dlow)
C             col 16-30       coefficient  (elow)
C             col 31-45       coefficient  (flow)
C             col 46-60       coefficient  (glow)
C             col 61-75       heat of formation/R in K (htform)
C             col 76-79       blank
C             col 80          blank (might hold integer '4')

       character*1 phase, phaseo
       character*2 date,el1,el2,el3,el4,el5
       character*2      el1sav,el2sav,el3sav,el4sav
       character*4 code
       character*18 name
       integer numint
       real    n1,n2,n3,n4,n5
       real    n1sav,n2sav,n3sav,n4sav,ex(8)
       real*8  molwgt,htform,Tlow,Tcom,Thi, Tprvhi
       real*8  alow,blow,clow,dlow,elow,flow,glow,xlow
       real*8  ahi,bhi,chi,dhi,ehi,fhi,ghi,xhi
       data ex/0,1,2,3,4,5,0,0/

C****************************************************************
C   Input data from four old form records.

5      read(5,50,end=100) name,code,date,el1,n1,el2,n2,el3,n3,el4,n4,
     +             phase,Tlow,Thi,molwgt 
50     format( A18,A4,A2,4(A2,F3.0),A1,2F10.3,F13.5,1X,1X)
C   Note: An isotope code might be in col 17-18 of old form.  
C         This item is not used in new form so it is overlooked.

       read(5,60) ahi,bhi,chi,dhi,ehi
60     format( 5(E15.8),4X,1X )

       read(5,60) fhi,ghi,alow,blow,clow
       read(5,60) dlow,elow,flow,glow,htform

C*****************************************************************
C   In the new form, the phase is coded  0 => gas  (G)
C                                        1 => lowest T condensed phase
C                                        2 => next T condensed phase
C                                        3 => next T condensed phase
C                                        etc.
C  In the old form, the phase is coded G => gas
C                                      C => condensed
C  The following transforms this code
       if (phase .eq. 'G') then
           phase = '0'
       else
           if (  el1 .EQ. el1sav    .AND.
     +           el2 .EQ. el2sav    .AND.
     +           el3 .EQ. el3sav    .AND.
     +           el4 .EQ. el4sav    .AND.
     +           n1  .EQ. n1sav     .AND.
     +           n2  .EQ. n2sav     .AND.
     +           n3  .EQ. n3sav     .AND.
     +           n4  .EQ. n4sav     .AND.
     +           Tlow .EQ. Tprvhi          )   then
              if (phaseo .EQ. '6')  stop       
              if (phaseo .EQ. '5')  phase = '6'
              if (phaseo .EQ. '4')  phase = '5'
              if (phaseo .EQ. '3')  phase = '4'
              if (phaseo .EQ. '2')  phase = '3'
              if (phaseo .EQ. '1')  phase = '2'
           else
              phase = '1'
           endif
C          Save formula, phase, and Thi for next cycle
           el1sav = el1
           el2sav = el2
           el3sav = el3
           el4sav = el4
           n1sav = n1
           n2sav = n2
           n3sav = n3
           n4sav = n4
           phaseo = phase
           Tprvhi = Thi
       endif
C*****  end of phase conversion **********************************

       if (Thi .LE. 1000) then
          numint = 1
          Tcom = Thi
       endif
       if (Tlow .GE. 1000) then
          numint = 1
          Tcom = Thi
          alow = ahi
          blow = bhi
          clow = chi
          dlow = dhi
          elow = ehi
          flow = fhi
          glow = ghi  
       endif
       if ((Tlow .LT. 1000) .AND. (Thi .GT. 1000)) then
          numint = 2
          Tcom = 1000
       endif         

C  The values of xlow and xhi never appear in the old form.
C  They are here set to zero for the new form.
       xlow = 0.0
       xhi = 0.0

C  The values of el5 and n5 do not appear in the old form.
C  They are set to blank and zero for the new form.
       el5 = '  '
       n5 = 0

C  Output data to eight (or five) new form records.

       write(6,10) name
10     format( A18)
       htform = htform * 8.31451d0

       write(6,20) numint,code,date,el1,n1,el2,n2,el3,n3,el4,n4,el5,n5,
     +            phase,molwgt,htform
20     format( 1X, I1,1X,A4,A2,1X,5(A2,F6.2 ),1X,A1,F13.5,f15.3 )

       write(6,30) Tlow,Tcom,ex,alow,blow,clow,dlow,elow,flow,glow
30     format( 1X,2F10.3,' 5',8f5.1,1p/5d16.8/48x,2d16.8)

       if (numint .EQ. 2) then
          write(6,30) Tcom,Thi,ex,ahi,bhi,chi,dhi,ehi,fhi,ghi
       endif

       goto 5

100    stop
       end
