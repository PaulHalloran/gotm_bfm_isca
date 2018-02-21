#INCLUDE "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: CalcBndyConentration.f90
!
! DESCRIPTION
!   !
    ! Calculation of Column values....
    ! This routine is meant to use in 3D-model to calculate the river concentration.

!
! !INTERFACE
  SUBROUTINE CalcBndyConcentration(numc,kmax,mode,rho,salt,temp,cc_inout,error)

!
! !USES:
  ! The following global scalar vars are used: &
  ! BoxNumberZ, NO_BOXES_Z, BoxNumberY, NO_BOXES_Y, BoxNumberX, NO_BOXES_X, &
  ! BoxNumber, BoxNumberXY

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem, ONLY: ppO3h,ppO3c

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
  integer,intent(IN)                     :: numc,kmax,mode
  real(RLEN),intent(IN)                  :: temp(0:kmax)
  real(RLEN),intent(IN)                  :: salt(0:kmax)
  real(RLEN),intent(IN)                  :: rho(0:kmax)
  real(RLEN),intent(INOUT)               :: cc_inout(1:numc,0:kmax)
  integer,intent(OUT)                    :: error
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! local variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN)                             :: salt_corr(0:kmax)
  real(RLEN)                             :: temp_corr(0:kmax)
  real(RLEN)                             :: old_DIC(0:kmax)
  

  integer                                ::j

  error=0
  if ( mode.eq.0) return
  select case (mode)
#ifdef INCLUDE_PELCO2
    case (ppO3h)
       ! Calculated according Lee et al. GeoPhysical Research Letters, Vol. 33 L19605
       ! Equation is only valid for range salt range 31-37 and temp-range 0-20
       salt_corr=min(max(31.0,salt(:)),37.0)
       temp_corr=min(max(0.0,temp(:)),20.0)
       cc_inout(ppO3h,:)= 2305.0+53.97*(salt_corr(:)-35.0)+2.74*(salt_corr(:)-35.0)**2 &
                                + 1.16*(temp_corr(:)-20.0)+0.04*(temp_corr(:)-20.0)**2 
    case (ppO3c)
       ! the limit_O3c defined in the bdy_3d_bfm.nc is such that first the bounday is set as
       ! a reflecting boundary ( boundary value copied from adjactent point)
       ! Subsequentially the reflecting value is used  to calculate the average with 
       ! eqilibirum value of DIC . In the way there is no an self-enforcing input of DIC
       ! over the boundary
       salt_corr=min(max(31.0,salt(:)),37.0)
       temp_corr=min(max(0.0,temp(:)),20.0)
       cc_inout(ppO3h,:)= 2305.0+53.97*(salt_corr(:)-35.0)+2.74*(salt_corr(:)-35.0)**2 &
                                + 1.16*(temp_corr(:)-20.0)+0.04*(temp_corr(:)-20.0)**2 
       old_DIC(1:kmax)=cc_inout(ppO3c,1:kmax)
       call CalcCO2SatInField(kmax,numc,rho(1:kmax),temp(1:kmax),salt(1:kmax),cc_inout(1:numc,1:kmax))
       if (maxval(abs(old_DIC(1:kmax)/cc_inout(ppO3c,1:kmax)-1.0))> 0.05) error=1;
       if (error.eq.1) then
         forall (j=1:kmax,abs(old_DIC(j)/cc_inout(ppO3c,j)-1.0)> 0.05) &
         cc_inout(ppO3c,j)= max(0.95* cc_inout(ppO3c,j), &
              min(1.05*cc_inout(ppO3c,j),0.5*(cc_inout(ppO3c,j)+old_DIC(j))))
         write(LOGUNIT,*)  'Boundary for DIC reset to equilibrium value'
!        write(LOGUNIT,*) old_DIC(1:kmax)
!        write(LOGUNIT,*) cc_inout(ppO3c,1:kmax);
       endif
#endif
  end select

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
