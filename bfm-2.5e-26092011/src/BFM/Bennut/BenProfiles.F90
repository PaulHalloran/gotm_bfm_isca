#INCLUDE "DEBUG.h"
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL
!	   BFM - Biogeochemical Flux Model version 2.3
!
! FUNCTION
!   BenProfiles.f90
!
! FILE
!   LimitChange.f90
!
! DESCRIPTION
!   
!	function BenProfiles
!	function to calculate the limit  the shift
!	input: 
!  
! !INTERFACE
        subroutine BenProfiles
!
! !AUTHORS
!   Piet Ruardij   
!
! !USES:
        use global_mem,      ONLY:RLEN
#ifdef INCLUDE_BENPROFILES
        use mem, ONLY:BoxNumberZ, NO_BOXES_Z, BoxNumberX, NO_BOXES_X, &
           BoxNumberY,NO_BOXES_Y,BoxNumber,BoxNumberXY,seddepth,ETW_Ben,&
           PrQ1c,PrM1p,PrM3n,PrM4n,PrM5s,PrM6r,KQ1,KPO4,KNO3,KNH4,KSiO3,&
           KRED, Pr2M1p,KPO4_2
        
        use constants, ONLY: INTEGRAL,STANDARD
        use bennut_interface, ONLY:CalculateFromSet
        use mem_BenSilica, ONLY: p_chM5s, p_cvM5s,p_q10

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!       The following global functions are used:eTq
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        use global_interface,   ONLY: eTq

!
! CHANGE_LOG
!   
!
! COPYING
!   
!   Copyright (C) 2004 P. Ruardij, the mfstep group, the ERSEM team 
!   (rua@nioz.nl, vichi@bo.ingv.it)
!
!   This program is free software; you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation;
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTEABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!
!

        IMPLICIT  NONE
        REAL(RLEN)    ::r,s,chM5s
        logical,static      :: start=.true.
        logical,static      ::  llM1p,ll2M1p,llM3n,llM4n,llM5s,llM6r,llQ1c
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
! user defined external functions
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        integer, external  :: D3toD1
        integer, external  :: D2toD1
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


        if ( start) then
           start=.false.
           call check_if_in_output('PrM1p',llM1p)
           call check_if_in_output('Pr2M1p',ll2M1p)
           call check_if_in_output('PrM3n',llM3n)
           call check_if_in_output('PrM4n',llM4n)
           call check_if_in_output('PrM5s',llM5s)
           call check_if_in_output('PrM6r',llM6r)
           call check_if_in_output('PrQ1c',llQ1c)
        endif

        s=0.0;
        DO BoxNumberZ= 1,NO_BOXES_Z
        DO BoxNumberY=1,NO_BOXES_Y
            DO BoxNumberX=1,NO_BOXES_X
             BoxNumber=D3toD1(BoxNumberX,BoxNumberY,BoxNumberZ)
             BoxNumberXY=D2toD1(BoxNumberX,BoxNumberY)
  
             r=abs(seddepth(BoxNumber))
             if (llM1p) PrM1p(BoxNumber)= &
                CalculateFromSet(KPO4(BoxNumberXY),INTEGRAL,STANDARD,s,r)/(r-s)
             if (ll2M1p) Pr2M1p(BoxNumber)= &
                CalculateFromSet(KPO4_2(BoxNumberXY),INTEGRAL,STANDARD,s,r)/(r-s)
             if (llM3n) PrM3n(BoxNumber)= &
                CalculateFromSet(KNO3(BoxNumberXY),INTEGRAL,STANDARD,s,r)/(r-s)
             if (llM4n) PrM4n(BoxNumber)= &
                CalculateFromSet(KNH4(BoxNumberXY),INTEGRAL,STANDARD,s,r)/(r-s)
             if (llM5s) then
               chM5s = p_chM5s+ p_cvM5s*( eTq( ETW_Ben(BoxNumberXY), p_q10)- 1.0D+00)
               PrM5s(BoxNumber)= &
                chM5s- CalculateFromSet(KSiO3(BoxNumberXY),INTEGRAL,STANDARD,s,r)/(r-s)
             endif
             if (llM6r) PrM6r(BoxNumber)= &
                CalculateFromSet(KRED(BoxNumberXY),INTEGRAL,STANDARD,s,r)/(r-s)
             if (llQ1c) PrQ1c(BoxNumber)= &
                CalculateFromSet(KQ1(BoxNumberXY),INTEGRAL,STANDARD,s,r)/(r-s)
             s=r;
            ENDDO
          ENDDO
       ENDDO
      
        return
#endif
      end

