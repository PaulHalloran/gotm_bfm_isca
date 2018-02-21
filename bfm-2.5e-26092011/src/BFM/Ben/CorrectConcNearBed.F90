#INCLUDE "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: CorrectConcNearBed
!
! DESCRIPTION
!   Description of the anoxic diagenitic processes in the sediment
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
      subroutine CorrectConcNearBed_vector(depthlayer, sedi, fto, &
                      p_p_max,n, correction)

!


! !USES:
     use constants, only: RLEN, SEC_PER_DAY
     use turbulence,  ONLY: kappa
     use mem,         ONLY: ETAUB,NO_BOXES_XY


     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     !    Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      IMPLICIT NONE
! !INPUT PARAMETERS:
     integer,intent(IN)                    :: n
     REAL(RLEN), intent(IN),dimension(n)   ::DepthLayer
     REAL(RLEN), intent(IN),dimension(n)   ::Sedi
     REAL(RLEN), intent(IN)                ::fto
     REAL(RLEN), intent(IN)                ::p_p_max
!OUTPUT PARAMETERS:
     REAL(RLEN),dimension(n),intent(OUT)   ::correction

!  
!
! !AUTHORS
!   Original version by  P. Ruardij
!
!
! COPYING
!   
!   Copyright (C) 2006 P. Ruardij and M. Vichi
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
!EOP
!-------------------------------------------------------------------------!
!BOC
!
!

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Local Variables
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      REAL(RLEN),dimension(NO_BOXES_XY)                ::b
      REAL(RLEN),dimension(NO_BOXES_XY)                ::f
      REAL(RLEN),dimension(NO_BOXES_XY)                ::r
      REAL(RLEN),dimension(NO_BOXES_XY)                ::s
      REAL(RLEN),dimension(NO_BOXES_XY)                ::d
      REAL(RLEN),parameter                             ::p_small=1.0D-10
    
      s=0.0;d=0.0;
      where ( Sedi.gt.0.0) 
        f=  min(fto,DepthLayer)
        b = min(100.0D+00,Sedi/(p_small+SEC_PER_DAY*kappa*ETAUB(1))) 
        s=max(1.0D+00,(f/max(1.D-10,DepthLayer-f))**(-b))        
        correction=s;
     elsewhere
        correction=1.0;
     endwhere
     where (correction *f/DepthLayer .gt.p_p_max) 
        correction=p_p_max *DepthLayer/f
     endwhere
     return
     end

!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: CorrectConcNearBed
!
! DESCRIPTION
!   Description of the anoxic diagenitic processes in the sediment
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
      subroutine CorrectConcNearBed(depthlayer, sedi, fto,p_p_max,correction)

!


! !USES:

     use global_mem,only:LOGUNIT
     use constants, only: RLEN, SEC_PER_DAY
     use turbulence,  ONLY: kappa
     use mem,         ONLY: ETAUB,NO_BOXES_XY


     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     !    Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      IMPLICIT NONE
! !INPUT PARAMETERS:
     REAL(RLEN), intent(IN)   ::DepthLayer
     REAL(RLEN), intent(IN)   ::Sedi
     REAL(RLEN), intent(IN)   ::fto
     REAL(RLEN), intent(IN)  ::p_p_max
!OUTPUT PARAMETERS:
     REAL(RLEN),intent(OUT)   ::correction

!  
!
! !AUTHORS
!   Original version by  P. Ruardij
!
!
! COPYING
!   
!   Copyright (C) 2006 P. Ruardij and M. Vichi
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
!EOP
!-------------------------------------------------------------------------!
!BOC
!
!

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Local Variables
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      REAL(RLEN):: b
      REAL(RLEN):: f
      REAL(RLEN):: r
      REAL(RLEN):: s
      REAL(RLEN):: d
      REAL(RLEN),parameter:: p_small=1.0D-10
    
      s=0.0;d=0.0;
      if ( Sedi.gt.0.0) then 
        f=  min(fto,DepthLayer)
        b = min(100.0D+00,Sedi/(p_small+SEC_PER_DAY*kappa*ETAUB(1))) 
        s=max(1.0D+00,(f/max(1.D-10,DepthLayer-f))**(-b))        
        correction=s;
!       if ( s.lt.1.0 ) then
!         write(LOGUNIT,*)'s=',s
!         write(LOGUNIT,*)'b=',b
!         write(LOGUNIT,*)'Depthlayer,f=',Depthlayer,f
!         write(LOGUNIT,*)'ETAUB=',ETAUB(1)
!       endif
     else
        correction=1.0;
     endif
     if (correction *f/DepthLayer .gt.p_p_max) then
        correction=p_p_max *DepthLayer/f
     endif
     return
     end

!EOC


