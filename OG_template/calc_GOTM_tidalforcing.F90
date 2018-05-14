!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
MODULE calc_gnode

 implicit none

 CONTAINS
!******************************************************************************
!******************************************************************************
!*******************************************************************************
!*******************************************************************************
      SUBROUTINE UVSET(IMN,IHS,IDAY,IMNTH,IYR,SIGO,FTC,U,V,INCON,MCON,NW)
! ==================================================================
!
!     Derive tidal parameters for start time of model run
!
! ==================================================================
!
!     I - IHS,IDAY,IMNTH,IYR   start time
!     O - SIG constituent speeds
!     O - FTC nodal factors F
!     O - U   nodal factors U (in degrees)
!     O - V   phase relative to equilibrium tide (in degrees)
!     I - INCON number of constituents required
!     I - MCON list of constituents required, numbered 1-17 as defined
!     I - NW print out unit number
!
      INTEGER, PARAMETER :: NC=17
      INTEGER :: YEAR,VD,IHS,IMN,IDAY,IMNTH,IYR,NW,INCON,IVDY,NDC
      INTEGER :: I,K,KK
      REAL(8) :: U(NC),V(NC),SIG(NC),SIGR(NC),FTC(NC)
      CHARACTER(8) :: CNAME(NC)
      REAL(8) :: SIGO(NC),F(NC),VPLU(NC),MCON(NC)
      REAL(8) :: RTD,SS,H,P,EN,P1
      DATA CNAME/ 'Q1', 'O1', 'P1', 'S1', 'K1', &
                '2N2','MU2', 'N2','NU2', 'M2', 'L2', 'T2', 'S2', 'K2', &
                'M4','Mm','MSf'/
      DATA SIGR/.2338507481,.2433518789,.2610826055,.2617993878, &
             .2625161701, &
             .4868657873,.4881373225,.4963669182,.4976384533, &
             .5058680490,.5153691799,.5228820265,.5235987756, &
             .5250323419, &
            1.011736098,0.0095016,0.017731/
!
!     IHS             -  Start time GMT on ...
!     IDAY/IMNTH/IYR  -  date   e.g.   12/10/87
!
      CALL VDAY(IDAY,IMNTH,IYR,IVDY)
      VD=IVDY
!
!RP   NOTE CHANGE OF YEAR NUMBER FOR D. BLACKMAN SHPEN
!RP   IF(IYR.GE.1000) YEAR=IYR-1900
!RP   IF(IYR.LT.1000) YEAR=IYR
      YEAR = IYR

!.....YEAR  =  YEAR OF REQUIRED DATA
!.....VD    =  DAY OF REQUIRED DATA..SET UP FOR 0000GMT DAY YEAR

      NDC = NC
!.....NDC   =  Number of constituents allowed

      RTD = 360.0/6.2831852
      DO I = 1,NDC
        SIG(I) = SIGR(I)*RTD
        SIGO(I)= SIG(I)
      END DO

      IF (YEAR) 12,16,12
   12 CALL SHPEN (YEAR,VD,SS,H,P,EN,P1)
      CALL UFSET (P,EN,U,F)
      CALL VSET (SS,H,P,EN,P1,V)

      WRITE (NW,599)
      WRITE (NW,600)
  599 FORMAT(//' Tidal data ',/)
  600 FORMAT(3X,'Name         speed         F        U        V      VPLU ')

      DO 81 KK=1,INCON
      K=MCON(KK)
      VPLU(K)=V(K)+U(K)
      VPLU(K)=VPLU(K)+(FLOAT(IHS)+IMN/60.0)*SIG(K)
   89 IF(VPLU(K)) 90,91,91
   90 VPLU(K)=VPLU(K)+360.
      GOTO 89
   91 IF(VPLU(K)-360.) 92,93,93
   93 VPLU(K)=VPLU(K)-360.
      GOTO 91
   92 CONTINUE
      WRITE(NW,700)K,CNAME(K),SIG(K),F(K),U(K),V(K),VPLU(K)
  700 FORMAT (I4,1X,A8,F12.7,F8.4,3F9.3)
   81 CONTINUE
16    RETURN
      END SUBROUTINE UVSET
!*******************************************************************************
!*******************************************************************************
      SUBROUTINE SHPEN (YEAR,VD,S,H,P,EN,P1)
! =================================================================
!
!     Calculate astronomical arguments for tides
!     This version from D. Blackman 30 Nove 1990
!
! =================================================================
      INTEGER,PARAMETER :: NW=6
      INTEGER :: YEAR,YR,VD
      INTEGER :: ILC,ICENT,IT,IDAY,ILD,IPOS,NN,IYD
      REAL(8) :: T,TD,DELTA,DELTAT,S,H,P,EN,P1
      REAL(8) :: DELT(84)
      REAL(8) :: FULLCYCLE
      DATA DELT /-5.04,-3.90,-2.87,-0.58,0.71,1.80, &
         3.08, 4.63, 5.86, 7.21, 8.58,10.50,12.10, &
        12.49,14.41,15.59,15.81,17.52,19.01,18.39, &
        19.55,20.36,21.01,21.81,21.76,22.35,22.68, &
        22.94,22.93,22.69,22.94,23.20,23.31,23.63, &
        23.47,23.68,23.62,23.53,23.59,23.99,23.80, &
        24.20,24.99,24.97,25.72,26.21,26.37,26.89, &
        27.68,28.13,28.94,29.42,29.66,30.29,30.96, &
        31.09,31.59,31.52,31.92,32.45,32.91,33.39, &
        33.80,34.23,34.73,35.40,36.14,36.99,37.87, &
        38.75,39.70,40.70,41.68,42.82,43.96,45.00, &
        45.98,47.00,48.03,49.10,50.10,50.97,51.81, &
        52.57/

      FULLCYCLE = 360.0
      ILC = 0
      ICENT = YEAR/100
      YR = YEAR - ICENT*100
      T = ICENT - 20

!     FOR THE FOLLOWING EQUATIONS
!     TIME ORIGIN IS FIXED AT 00 HR OF JAN 1ST,2000.
!     SEE NOTES BY CARTWRIGHT
!
      IT = ICENT - 20
      IF (IT) 1,2,2
    1 IDAY = IT/4 -IT
      GO TO 3
    2 IDAY = (IT+3)/4 - IT

!     T IS IN JULIAN CENTURY
!     CORRECTION IN GEGORIAN CALANDER WHERE ONLY CENTURY YEAR DIVISIBLE
!     BY 4 IS LEAP YEAR.

    3 CONTINUE

      TD = 0.0

      IF (YR) 4,5,4

    4 IYD = 365*YR
      ILD = (YR-1)/4
      IF((ICENT - (ICENT/4)*4) .EQ. 0) ILC = 1
      TD = IYD + ILD + ILC

    5 TD = TD + IDAY + VD -1.0 - 0.5
      T = T + (TD/36525.0)

      IPOS=YEAR-1899
      IF (IPOS .LT. 0) GO TO 7
      IF (IPOS .GT. 83) GO TO 6

      DELTA = (DELT(IPOS+1)+DELT(IPOS))/2.0
      GO TO 7

    6 DELTA= (65.0-50.5)/20.0*(YEAR-1980)+50.5

    7 DELTAT = DELTA * 1.0E-6

      S   = 218.3165 + 481267.8813*T - 0.0016*T*T + 152.0*DELTAT
      H   = 280.4661 + 36000.7698*T + 0.0003*T*T + 11.0*DELTAT
      P   =  83.3535 + 4069.0139*T - 0.0103*T*T + DELTAT
      EN  = 234.9555 + 1934.1363*T - 0.0021*T*T + DELTAT
      P1  = 282.9384 + 1.7195*T + 0.0005*T*T

      NN = S/FULLCYCLE
      S = S - NN*FULLCYCLE  ! modulus as NN is an integer, not a real
      IF ( S .LT. 0.0) S = S+FULLCYCLE

      NN = H/FULLCYCLE
      H = H-FULLCYCLE*NN
      IF (H .LT. 0.0) H = H+FULLCYCLE

      NN = P/FULLCYCLE
      P = P- FULLCYCLE*NN
      IF (P .LT. 0.0) P = P+FULLCYCLE

      NN = EN/FULLCYCLE
      EN = EN-FULLCYCLE*NN
      IF(EN .LT. 0.0) EN = EN + FULLCYCLE
      EN = FULLCYCLE - EN

      NN = P1/FULLCYCLE
      P1 = P1 - NN*FULLCYCLE

      WRITE (NW,200) YEAR,VD
  200 FORMAT ('   Year  ',I4,/,'   Day   ',I4)
      WRITE (NW,300)
  300 FORMAT(//,' Astronomical arguments ',/, &
     '        S              H              P             EN      P1')
      WRITE (NW,400) S,H,P,EN,P1
  400 FORMAT (5(5X,F10.5) )

      RETURN

      END SUBROUTINE SHPEN
!*******************************************************************************
!*******************************************************************************
      SUBROUTINE VSET(S,H,P,EN,P1,V)
! =================================================================
!
!     Calculate tidal phases for 0000GMT on start day of run
!
! =================================================================
      INTEGER, PARAMETER :: NC=17
      INTEGER NDC,K
      REAL(8) :: V(NC)
      REAL(8) :: S,H,P,EN,P1

      NDC = NC
!   V 'S  ARE COMPUTED HERE.
      V(1) =-3*S +H +P +270
!   Q1
      V(2) =-2*S +H +270.0
!   O1
      V(3) =-H +270
!   P1
      V(4) =180
!   S1
      V(5) =H +90.0
!   K1
      V(6) =-4*S +2*H +2*P
!   2N2
      V(7) =-4*(S-H)
!   MU2
      V(8) =-3*S +2*H +P
!   N2
      V(9) =-3*S +4*H -P
!   MU2
      V(10) =-2*S +2*H
!   M2
      V(11) =-S +2*H -P +180
!   L2
      V(12) =-H +P1
!   T2
      V(13) =0
!   S2
      V(14) =H+H
!   K2
      V(15) =2*V(10)
!   M4
      V(16) = S - P
!   Mm
      V(17) = 2*S - 2*H
!
      DO 72 K = 1, NDC
69    IF (V(K) ) 70,71,71
70      V(K)=V(K)+360.0
      GO TO 69
71    IF ( V(K) - 360.0) 72,73,73
73      V(K)=V(K)-360.0
      GO TO 71
72    CONTINUE
      RETURN
      END SUBROUTINE VSET
!*******************************************************************************
!*******************************************************************************
      SUBROUTINE UFSET (P,CN,B,A)
! =================================================================
!
!     Calculate nodal parameters for the tides
!
! =================================================================
      INTEGER, PARAMETER :: NC=17
      REAL(8) :: W1,W2,W3,W4,W5,W6,W7,W8,NW,PW,RAD,P,CN
      REAL(8) :: A(NC),B(NC)
      INTEGER :: K,NDC

      NDC=NC

!    A=F       ,  B =U
!    T IS  ZERO AS COMPARED TO TIFA.
      RAD = 6.2831852D0/360.0
      PW = P*RAD
      NW = CN*RAD
      W1 = COS(NW)
      W2 = COS(2*NW)
      W3 = COS(3*NW)
      W4 = SIN(NW)
      W5 = SIN(2*NW)
      W6 = SIN(3*NW)
      W7 = 1 -0.2505*COS(2*PW) -0.1102*COS(2*PW-NW)-0.156*COS(2*PW-2*NW) -0.037*COS(NW)
      W8 = -0.2505*SIN(2*PW) -0.1102*SIN(2*PW-NW)-0.0156*SIN(2*PW-2*NW) -0.037*SIN(NW)

      A(1) = 1.0089+0.1871*W1-0.0147*W2+0.0014*W3
      B(1) = 0.1885*W4 - 0.0234*W5+.0033*W6
!   Q1
      A(2) = A(1)
      B(2) = B(1)
!   O1
      A(3) = 1.0
      B(3) = 0.0
!   P1
      A(4) = 1.0
      B(4) = 0.0
!   S1
      A(5) = 1.0060+0.1150*W1- 0.0088*W2 +0.0006*W3
      B(5) = -0.1546*W4 + 0.0119*W5 -0.0012*W6
!   K1
      A(6) =1.0004 -0.0373*W1+ 0.0002*W2
      B(6) = -0.0374*W4
!  2N2
      A(7) = A(6)
      B(7) = B(6)
!  MU2
      A(8) = A(6)
      B(8) = B(6)
!   N2
      A(9) = A(6)
      B(9) = B(6)
!  NU2
      A(10) = A(6)
      B(10) = B(6)
!   M2
      A(11) = SQRT(W7*W7+W8*W8)
      B(11) = ATAN(W8/W7)
      IF(W7.LT.0) B(11) = B(11) + 3.141992
!   L2
      A(12) = 1.0
      B(12) = 0.0
!   T2
      A(13)= A(12)
      B(13)= B(12)
!   S2
      A(14) = 1.0241+0.2863*W1+0.0083*W2 -0.0015*W3
      B(14) = -0.3096*W4 + 0.0119*W5 - 0.0007*W6
!   K2
      A(15) = A(6)*A(6)
      B(15) = 2*B(6)
!   M4
      A(16) = 0.0
      B(16) = 0.0
!   Mm
      A(17) = 0.0
      B(17) = 0.0
!   MSf

      DO 40 K = 1,NDC
        B(K) = B(K)/RAD
32      IF (B(K)) 34,35,35
34        B(K) = B(K) + 360.0
        GO TO 32
35      IF (B(K)-360.0) 40,37,37
37        B(K) = B(K)-360.0
        GO TO 35
40    CONTINUE
      RETURN
      END SUBROUTINE UFSET
!*******************************************************************************
!*******************************************************************************
      SUBROUTINE VDAY(IDAY,IMNTH,IY,IVDY)
! =================================================================
!
!     Calculate day number in year from day/month/year
!
! =================================================================

       INTEGER :: IDAY,IMNTH,IY,IVDY,IYR

       IF(IMNTH.EQ.1) IVDY=IDAY
       IF(IMNTH.EQ.2) IVDY=IDAY+31
       IF(IMNTH.EQ.3) IVDY=IDAY+59
       IF(IMNTH.EQ.4) IVDY=IDAY+90
       IF(IMNTH.EQ.5) IVDY=IDAY+120
       IF(IMNTH.EQ.6) IVDY=IDAY+151
       IF(IMNTH.EQ.7) IVDY=IDAY+181
       IF(IMNTH.EQ.8) IVDY=IDAY+212
       IF(IMNTH.EQ.9) IVDY=IDAY+243
       IF(IMNTH.EQ.10) IVDY=IDAY+273
       IF(IMNTH.EQ.11) IVDY=IDAY+304
       IF(IMNTH.EQ.12) IVDY=IDAY+334
      IYR=IY
       IF(MOD(IYR,4).EQ.0.AND.IMNTH.GT.2) IVDY=IVDY+1
       IF(MOD(IYR,100).EQ.0.AND.IMNTH.GT.2) IVDY=IVDY-1
       IF(MOD(IYR,400).EQ.0.AND.IMNTH.GT.2) IVDY=IVDY+1
      RETURN
      END SUBROUTINE VDAY
!*******************************************************************************
!*******************************************************************************
END MODULE calc_gnode
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
MODULE fortran_numstr

 implicit none

 CONTAINS

!******************************************************************************
!******************************************************************************
 function NUM2STR(a)
 ! creates a string from a real number, but should use TRIM() in final use to
 ! get rid of extra spaces

 implicit none

 real(8), intent(in) :: a
 integer :: i
 character(20) :: num2str
 character(20) :: string

      write(string,*) a
      num2str=ADJUSTL(string)

 return
 end function NUM2STR
!******************************************************************************
!******************************************************************************
 function INT2STR(a)
 ! creates a string from a real number, but should use TRIM() in final use to
 ! get rid of extra spaces

 implicit none

 integer, intent(in) :: a
 integer :: i
 character(20) :: int2str
 character(20) :: string


      write(string,*) a
      int2str=ADJUSTL(string)
      IF (a .LT. 10) THEN
        int2str='0'//int2str
      END IF

 return
 end function INT2STR
!******************************************************************************
!******************************************************************************
 function STR2NUM(instring)

 implicit none

 real(8) :: str2num
 character(LEN=*),intent(in) :: instring
 character(20) :: string2

     string2=ADJUSTL(instring)
     read(string2,*) str2num

return
end function STR2NUM
!******************************************************************************
!******************************************************************************
 function STR2INT(instring)

 implicit none

 integer :: str2int
 character(LEN=*),intent(in) :: instring
 character(20) :: string2

     string2=ADJUSTL(instring)
     read(string2,*) str2int

return
end function STR2INT
!*****************************************************************************
!*****************************************************************************
END MODULE fortran_numstr
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
MODULE fortran_time

 USE fortran_numstr
 implicit none

 CONTAINS
!*****************************************************************************
!*****************************************************************************
 SUBROUTINE DATESTR2VEC(timestring,timevec)
 !---------------------------------------------------------------
 ! transform a date string into a date vector (y,m,d,HH,MM,SS)
 !---------------------------------------------------------------

 USE fortran_numstr
 implicit none
 CHARACTER(19), INTENT(IN) :: timestring
 INTEGER, INTENT(OUT) :: timevec(6)

   timevec(1)=STR2INT(timestring(1:4))
   timevec(2)=STR2INT(timestring(6:7))
   timevec(3)=STR2INT(timestring(9:10))
   timevec(4)=STR2INT(timestring(12:13))
   timevec(5)=STR2INT(timestring(15:16))
   timevec(6)=STR2INT(timestring(18:19))

 END SUBROUTINE DATESTR2VEC
!*****************************************************************************
!*****************************************************************************
function DATENUM(timestring)
 !---------------------------------------------------------------
 ! transform date string into a numerical time
 !---------------------------------------------------------------

 implicit none

 real(8) :: datenum,daypart
 integer :: timevec(6)
 character(19), intent(in) :: timestring
 integer :: y, mo, d, HH, MM, SS, numdays
 integer, dimension(12) :: daypermonth, monthdays


 y=STR2INT(timestring(1:4))
 mo=STR2INT(timestring(6:7))
 d=STR2INT(timestring(9:10))
 HH=STR2INT(timestring(12:13))
 MM=STR2INT(timestring(15:16))
 SS=STR2INT(timestring(18:19))

 monthdays=(/31,28,31,30,31,30,31,31,30,31,30,31/)

 daypermonth(1) = 0
 daypermonth(2) = daypermonth(1)+31
 daypermonth(3) = daypermonth(2)+28
 daypermonth(4) = daypermonth(3)+31
 daypermonth(5) = daypermonth(4)+30
 daypermonth(6) = daypermonth(5)+31
 daypermonth(7) = daypermonth(6)+30
 daypermonth(8) = daypermonth(7)+31
 daypermonth(9) = daypermonth(8)+31
 daypermonth(10) = daypermonth(9)+30
 daypermonth(11) = daypermonth(10)+31
 daypermonth(12) = daypermonth(11)+30

 ! warning in case of impossible date
 IF (d .GT. monthdays(mo) ) THEN
    IF (mo.EQ.2 .AND. d.EQ.29) THEN ! check if real leap year
      IF ( (mod(y,4).eq.0 .and. mod(y,100).ne.0) .or. mod(y,400).eq.0) THEN
      ELSE
        print *,'Error: year ',y,' is not a leap year!'
        STOP
      END IF
    ELSE
      print *,'Error: not possible to have ',d,' days in month ',mo
      STOP
    END IF
 END IF
 IF (mo .GT. 12) THEN
    print *,'Error: month ',mo,' does not exist!'
    STOP
 END IF

 numdays = y*365 &
         + ceiling(y/4.0)-ceiling(y/100.0)+ceiling(y/400.0) &
         + daypermonth(mo) + d

 ! if current year is leap year
 if( ((mo>2) .and. ((mod(y,4).eq.0.and.mod(y,100).ne.0).or.mod(y,400).eq.0)) ) then
    numdays = numdays + 1
 endif

 ! add hours, minutes and seconds
 daypart=(HH*3600.0+MM*60.0+SS)/(24.0*3600.0)
 datenum=FLOAT(numdays)+daypart

return
end function DATENUM
!*****************************************************************************
!*****************************************************************************
function DATESTR(timenum)
 !---------------------------------------------------------------
 ! transform a numerical date into a date string
 !---------------------------------------------------------------
 implicit none

 character(19) :: datestr
 real(8), intent(in) :: timenum
 real(8) :: y, mo, d, H, M, S
 real(8) :: t1, t2, remainder,t_est
 integer :: i
 logical :: leapyear
 character(2) :: mmo, dd, HH, MM, SS
 character(4) :: yyyy,num2str
 integer, dimension(12) :: monthdays


 monthdays=(/31,28,31,30,31,30,31,31,30,31,30,31/)

 ! get years, first get overestimate due to neglect of leap years
 y=floor(timenum/365)

 t_est=y*365+ceiling(y/4.)-ceiling(y/100.)+ceiling(y/400.)+1.0 ! beginning of this year, +1=1st day
 DO WHILE (t_est .GT. timenum)
   y=y-1
   t_est=y*365+ceiling(y/4.)-ceiling(y/100.)+ceiling(y/400.)+1.0
 END DO

 remainder=timenum-t_est+1.0 ! put day 1 back in remainder, substracted by t_est

 IF ( (mod(y,4.0).EQ.0.0 .AND. mod(y,100.0).NE.0.0) .OR. (mod(y,400.0).EQ.0.0) ) THEN
    leapyear=.TRUE.  ! day already substrcated by t1, so beware if day is before 28 feb
 ENDIF

 t1=remainder
 ! get months and days
 DO i=1,12
   IF (t1 .GE. 1.0) THEN
     mo=i
     d=t1
     t1=t1-monthdays(i)
     IF (leapyear .AND. i.EQ.1) t1=t1+1.0  ! January, replace leap year day
     IF (leapyear .AND. i.EQ.2) t1=t1-1.0  ! After February, so take day off again as feb=28
   END IF
 END DO
 remainder=d
 d=floor(d)

 remainder=remainder-d

 ! get hours, minutes and seconds
 remainder=remainder*24.0*3600.0
 H=floor(remainder/3600.0)
 remainder=mod(remainder,3600.0)
 M=floor(remainder/60.0)
 remainder=mod(remainder,60.0)
 S=NINT(remainder)  ! round to nearest integer because of round off errors

 IF (S .EQ. 60) THEN
   ! full minute so adjust time and day
   S=0
   M=M+1
   IF (M .EQ. 60) THEN
     M=0
     H=H+1
     IF (H .EQ. 24) THEN
       H=0
       d=d+1
       IF (d .GT. monthdays(mo)) THEN
         d=1
	 mo=mo+1
	 IF (mo .GT. 12) THEN
	   mo=1
	   y=y+1
	 END IF
       END IF
     END IF
   END IF
 END IF

 yyyy=INT2STR(INT(y))
 mmo=INT2STR(INT(mo))
 dd=INT2STR(INT(d))
 HH=INT2STR(INT(H))
 MM=INT2STR(INT(M))
 SS=INT2STR(INT(S))

 ! rconstruct time string
 datestr(1:4)=yyyy
 datestr(5:5)='-'
 datestr(6:7)=mmo
 datestr(8:8)='-'
 datestr(9:10)=dd
 datestr(11:11)=' '
 datestr(12:13)=HH
 datestr(14:14)=':'
 datestr(15:16)=MM
 datestr(17:17)=':'
 datestr(18:19)=SS

return
end function DATESTR
!*****************************************************************************
!*****************************************************************************
END MODULE fortran_time
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
MODULE about_sites

 implicit none

 CONTAINS
!*******************************************************************************
!*******************************************************************************
 SUBROUTINE GET_SITE(INCON,siteid,ampz,ampu,ampv,phasez,phaseu,phasev,site_longname)

 implicit none

 INTEGER, INTENT(IN) :: INCON
 CHARACTER(2), INTENT(IN) :: siteid
 CHARACTER(30), INTENT(OUT) :: site_longname
 REAL(8), INTENT(OUT) :: ampz(INCON),ampu(INCON),ampv(INCON)
 REAL(8), INTENT(OUT) :: phasez(INCON),phaseu(INCON),phasev(INCON)


   SELECT CASE (siteid)
      CASE ('lb')  ! Liverpool Bay
          ampz=(/3.52,0.75/)  ! amplitudes
          ampu=(/0.527,0.178/)
          ampv=(/0.045,0.019/)
          phasez=(/310.00,5.00/)   ! phases  (degrees)
          phaseu=(/235.92,280.26/)
          phasev=(/29.69,78.41/)
          site_longname='Liverpool Bay'
      CASE ('wg')  ! West Gabbard
          ampz=(/0.92,0.27/)  ! amplitudes
          ampu=(/0.349,0.107/)
          ampv=(/0.802,0.245/)
          phasez=(/335.00,15.00/)     ! phases  (degrees)
          phaseu=(/53.066,101.670/)
          phasev=(/63.910,110.61/)
          site_longname='West Gabbard'
      CASE ('oe')  ! Oyster Grounds East   1866Ai135BS03
          ampz=(/0.0,0.0/)         ! amplitudes
          ampu=(/0.218,0.060/)
          ampv=(/0.020,0.010/)
          phasez=(/0.0,0.0/)    ! phases  (degrees)
          phaseu=(/225.83,289.66/)
          phasev=(/341.12,308.08/)
          site_longname='Oyster Grounds East'  ! [54.95 N, 4.92 E]
      CASE ('og')  ! Oyster Grounds
          ampz=(/0.4,0.1314/)         ! amplitudes
          ampu=(/0.223,0.065/)
          ampv=(/0.038,0.025/)
          phasez=(/190.0,237.8/)    ! phases  (degrees)
          phaseu=(/202.28,258.67/)
          phasev=(/306.87,337.21/)
          site_longname='Oyster Grounds '  ! [54.41 N, 4.04 E]
      CASE ('sb')  ! Sean Gas Field, is Southern Bight location in MECS
          ampz=(/0.68,0.256/)         ! amplitudes
          ampu=(/0.2267,0.07481/)
          ampv=(/0.43613,0.14392/)
          phasez=(/198.0,250.0/)     ! phases  (degrees)
          phaseu=(/178.758,148.758/)
          phasev=(/64.871,34.871/)
          site_longname='Sean Gas Field' ! [53.17 N, 2.81 E]
      CASE ('nd')  ! North Dogger
          ampz=(/0.5,0.16/)        ! amplitudes
          ampu=(/0.10348,0.03415/)
          ampv=(/0.12454,0.0411/)
          phasez=(/105.0,143.6/)     ! phases  (degrees)
          phaseu=(/194.848,164.848/)
          phasev=(/235.55,205.55/)
          site_longname='North Dogger'  ! [55.68 N, 2.28 E]
   END SELECT


 END SUBROUTINE GET_SITE
!*******************************************************************************
!*******************************************************************************
END MODULE about_sites
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
PROGRAM calc_GOTM_tidalforcing

      USE calc_gnode
      USE fortran_time
      USE fortran_numstr
      USE about_sites
      implicit none

      INTEGER, PARAMETER :: NC=17
      INTEGER, PARAMETER :: INCON=2
      REAL(8) :: MCON(NC),FTC(NC),U(NC),V(NC),SIG(NC)
      REAL(8) :: period(INCON),om(INCON)
      REAL(8) :: gnode(INCON),fnode(INCON)
      REAL(8) :: ampz(INCON),ampu(INCON),ampv(INCON)
      REAL(8) :: phasez(INCON),phaseu(INCON),phasev(INCON)
      REAL(8), ALLOCATABLE :: tax(:),z_ts(:),u_ts(:),v_ts(:),time(:)
      REAL(8) :: dt,PI,t_start,t_end
      INTEGER :: start_vec(6),end_vec(6)
      INTEGER :: Ntime,NW,K,KK,I,IHS,IMN,IDAY,IMNTH,IYR,i2,file_unit
      CHARACTER(8) :: CNAME(NC)  ! CNAME is also defined in the subroutine UVset
      CHARACTER(2) :: siteid
      CHARACTER(8) :: start_t,end_t
      CHARACTER(19) :: start_time,end_time,tmpstr
      CHARACTER(LEN=30) :: ofz,ofuv,site_longname
      DATA CNAME/ 'Q1', 'O1', 'P1', 'S1', 'K1', &
                '2N2','MU2', 'N2','NU2', 'M2', 'L2', 'T2', 'S2', 'K2', &
                'M4','Mm','MSf'/

      !!!!!!!!!!!!!!!  USER DEFINED INPUT !!!!!!!!!!!!!!!!!!!!!!!
      call getenv('start',start_time)  ! get start and stop time from run script
      call getenv('stop',end_time)     ! --> environment varibale
      call getenv('site_id',siteid)
      !siteid='wg';  ! site name, choose from wg,og,sb,nd,lb
      !start_time='2002-11-01 00:00:00'  ! time span to create tidal file for
      !end_time='2008-01-01 00:00:00'
      dt=3600.0;               !output time step in seconds
      !!!!!!!!!!!!!  END OF USER DEFINED INPUT !!!!!!!!!!!!!!!!!!

      start_t=start_time(1:4)//start_time(6:7)//start_time(9:10)
      end_t=end_time(1:4)//end_time(6:7)//end_time(9:10)
      !ofz='z_'//start_t//'_to_'//end_t//'_wg.dat' ! z output file
      !ofuv='uv_'//start_t//'_to_'//end_t//'_wg.dat' ! u,v output file
      ofz='zeta.dat'
      ofuv='pressure.dat'

      call DATESTR2VEC(start_time,start_vec)
      call DATESTR2VEC(end_time,end_vec)

      period(1)=44714.0      ! tidal periods (secs), do  not change, M2
      period(2)=43200.0      ! tidal periods (secs), do  not change, S2

      call GET_SITE(INCON,siteid,ampz,ampu,ampv,phasez,phaseu,phasev,site_longname)
      print *,'Site is ',site_longname

      ! nodal factors=correction for 18 y cyclus --> if period is of same order
      ! than do not correct at all and use [1.0 1.0] (M2, S2 correction), as
      ! it is a constant correction
      fnode(1)=0.97      !fnode=[1.0000 1.0000] for long time series
      fnode(2)=1.0000

!--------------------------------------------------------------------------------
!     now use NODAL_WIN programme to get V values for M2 and S2

!     program to calculate nodal factors f, u, v for tidal constituents
!     up to 17 constituents allowed
!     set of tidal constituents ordered as follows
!     Q1  O1  P1  S1  K1 2N2 MU2  N2 NU2  M2  L2  T2  S2  K2  M4  Mm  MSf
!      1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
      NW=6
      MCON(1)=10  ! Read in the number of constituents required and which ones
      MCON(2)=13  ! For example, if M2 and S2 required, INCON=2, MCON=10,13
      IHS=start_vec(5)   !     Read in start time
      IMN=start_vec(4)
      IDAY=start_vec(3)
      IMNTH=start_vec(2)
      IYR=start_vec(1)
      CALL UVSET(IMN,IHS,IDAY,IMNTH,IYR,SIG,FTC,U,V,INCON,MCON,NW)
!--------------------------------------------------------------------------------
! continue with fortran code of Johans matlab routine, copy V vector into gnode

      DO KK=1,INCON
        K=MCON(KK)
        gnode(KK)=V(K)
      END DO

      ! time step
      t_start=DATENUM(start_time)
      t_end=DATENUM(end_time)

      ! calculate number of time steps and create arrays
      Ntime=NINT( (t_end-t_start)/(dt/(24.0*3600.0)) )+1 ! include start point
      allocate (tax(Ntime),z_ts(Ntime),u_ts(Ntime),v_ts(Ntime),time(Ntime))
      tax=0.0   ! initialise output vectors
      time=0.0
      z_ts=0.0
      u_ts=0.0
      v_ts=0.0

      DO i=1,Ntime
         time(i)=t_start+(i-1)*dt/(24.0*3600.0)  ! dt in s, datenum in days
	 tax(i)=(i-1)*dt
      END DO

      ! calculate tidal vectors
      PI=2.0*ASIN(1.0)

      DO i=1,INCON
         om(i)=2.0*PI/period(i)
      END DO

      DO i=1,Ntime
        DO i2=1,INCON
	   z_ts(i)=z_ts(i)+fnode(i2)*ampz(i2)*COS(om(i2)*tax(i)+(gnode(i2)-phasez(i2))*PI/180.0)
	   u_ts(i)=u_ts(i)+fnode(i2)*ampu(i2)*COS(om(i2)*tax(i)+(gnode(i2)-phaseu(i2))*PI/180.0)
	   v_ts(i)=v_ts(i)+fnode(i2)*ampv(i2)*COS(om(i2)*tax(i)+(gnode(i2)-phasev(i2))*PI/180.0)
	END DO
      END DO

      ! store results
      OPEN(unit=10,file=ofz,status='unknown',action='write')
      OPEN(unit=11,file=ofuv,status='unknown',action='write')
        DO i=1,Ntime
	   tmpstr=DATESTR(time(i))
	   WRITE(10,900) tmpstr,z_ts(i)
	   WRITE(11,901) tmpstr,0.0,u_ts(i),v_ts(i)
        END DO
      CLOSE(10)
      CLOSE(11)

      900 format(A19,1X,ES13.5)
      901 format(A19,3(1X,ES13.5))

      deallocate (tax,z_ts,u_ts,v_ts,time)


END PROGRAM calc_GOTM_tidalforcing
