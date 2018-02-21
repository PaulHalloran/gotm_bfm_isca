!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: CalcRiverConentration.f90
!
! DESCRIPTION
!   !
    ! Calculation of Column values....
    ! This routine is meant to use in 3D-model to calculate the river concentration.

!
! !INTERFACE
  SUBROUTINE CalcRiverConcentration(mode,kmax,numc,ETW,cc_river)

!
! !USES:
  ! The following global scalar vars are used: &
  ! BoxNumberZ, NO_BOXES_Z, BoxNumberY, NO_BOXES_Y, BoxNumberX, NO_BOXES_X, &
  ! BoxNumber, BoxNumberXY

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
  use mem, ONLY: ppO3c,ppO3h,ppN4n,ppN3n,ppR6c,ppR6p,ppR6n,ppN1p
  use CO2System, ONLY:HplusBASIS 
  use mem_PelCO2, ONLY:p_qhK4K3n,p_qhATo

!  
!
! !AUTHORS
!   P. Ruardij
!
! !REVISION_HISTORY
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
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE
  integer,intent(IN)         :: mode
  integer,intent(IN)         :: kmax
  integer,intent(IN)         :: numc
  real(RLEN),intent(IN)      :: ETW(1:kmax)
  real(RLEN),intent(INOUT)   :: cc_river(1:numc,1:kmax)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! external functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   real(RLEN)               :: ERHO(1:kmax)
   real(RLEN)               :: ESW(1:kmax)

#ifdef INCLUDE_PELCO2

   if ( mode.eq.0) return
   select case (mode)
     case ( ppO3h)
!      cc_river(mode,:)=HplusBASIS - cc_river(ppN3n,:)*p_qhK4K3n
     case ( ppO3c)
!      cc_river(ppO3h,:)=HplusBASIS - cc_river(ppN3n,:)*p_qhK4K3n
       ERHO=1027.0;
       ESW=0.3;
       call CalcCO2SatInField(kmax,numc,ERHO,ETW,ESW,cc_river)
     case ( ppR6c)
       cc_river(mode,:)=(cc_river(ppN1p,:))*(1.0/0.70-1.0) /0.001564
     case ( ppR6n)
       cc_river(mode,:)=(cc_river(ppN3n,:)+cc_river(ppN4n,:))*(1.0/0.93-1.0)
     case ( ppR6p)
       cc_river(mode,:)=cc_river(ppN1p,:)*(1.0/0.70-1.0)
   end select
#endif

  end

!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
