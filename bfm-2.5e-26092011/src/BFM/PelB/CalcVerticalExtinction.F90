#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: CalcVerticalExtinction
!
! DESCRIPTION
!   Calculates the vertical extinction.
!     
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  SUBROUTINE CalcVerticalExtinction(mode)
!
! !USES:
  ! The following Pelagic-states are used (NOT in fluxes): R6c
  ! The following box states are used (NOT in fluxes): PhytoPlankton
  ! The following Pelagic 1-d global boxvars are modified : xEPS
  ! The following Pelagic 1-d global boxvars  are used: ABIO_eps, ESS
  ! The following groupmember vars  are used: iiPhytoPlankton
  ! The following constituent constants  are used: iiC, iiL
  ! The following 0-d global parameters are used: p_eps0, &
  ! p_epsR6, p_epsESS, ChlLightFlag, p_epsChla
  ! The following 1-d global parameter vars are used: p_qchlc
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
#IFDEF NOPOINTERS
  use mem,  ONLY: D3STATE
#ELSE
  use mem,  ONLY: R6c, PhytoPlankton
#ENDIF
  use mem, ONLY: xEPS, xEPS_0, xEPS_ESS, xEPS_Chl, ABIO_eps, ESS,  &
      iiPhytoPlankton, iiC, iiL, NO_BOXES
  use mem_Param, ONLY: p_eps0, p_epsR6, p_epsESS, ChlLightFlag, p_epsChla
  use mem_Phyto,ONLY:p_qchlc 

! !INPUT PARAMETERS:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE
  integer,intent(IN)   :: mode


!  
!
! !AUTHORS
!   ERSEM-team
!
!
! !REVISION_HISTORY
!   !
!
! COPYING
!   
!   Copyright (C) 2006 P. Ruardij, the mfstep group, the ERSEM team 
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
  integer  :: i,iout
  real(RLEN), dimension(:), pointer  ::lcl_PhytoPlankton

  select case (mode)
    case(0)
        xEPS_0(:)= 0.0
        xEPS_ESS(:)= 0.0
        xEPS_Chl(:)= p_epsR6* R6c(:)
    case (1)
      select case ( p_eps0== 0.0D+00)
        case( .TRUE. )
          xEPS_ESS(:)  =   ABIO_eps(:)
          xEPS_Chl(:)  =   p_epsR6* R6c(:)
        case( .FALSE. )
          xEPS_0(:)    =   p_eps0 
          xEPS_ESS(:)  =   p_epsESS* ESS(:)
          xEPS_Chl(:)  =   p_epsR6* R6c(:)
      end select
    case(2)
        xEPS_0(:)= 0.0
        xEPS_ESS(:)  =   p_epsESS* ESS(:)
        xEPS_Chl(:)= p_epsR6* R6c(:)
  end select

!       call findnan( xEPS_Chl,NO_BOXES,iout )
!       if (iout.gt.0) then
!         write(LOGUNIT,*) '1 x_eps_chl in Nan in',iout
!         write(logunit,*) 'R6c=',R6c(iout)
!       endif

  select case ( ChlLightFlag)
    case ( 1 )
      do i = 1 , ( iiPhytoPlankton)
        lcl_PhytoPlankton =>    PhytoPlankton(i,iiC)
        xEPS_Chl(:)  =   xEPS_Chl(:)+ p_epsChla * p_qchlc(i)* lcl_PhytoPlankton
      end do

    case ( 2 )
      do i = 1 , ( iiPhytoPlankton)
        lcl_PhytoPlankton =>    PhytoPlankton(i,iiL)
        xEPS_Chl(:)  =   xEPS_Chl(:)+ p_epsChla * lcl_PhytoPlankton
!       call findnan( xEPS_Chl,NO_BOXES,iout )
!       if (iout.gt.0) then
!         write(LOGUNIT,*) '1 x_eps_chl in Nan in',iout
!         write(LOGUNIT,*) '1 x_eps_chl in Nan for',i
!         write(logunit,*) 'phyto=',i,lcl_PhytoPlankton(iout)
!       endif
      end do
  end select
 
  xEPS= xEPS_0 + xEPS_ESS +xEPS_Chl

! call findnan( xEPS,NO_BOXES,i )
! if (i.gt.0) then
!   write(LOGUNIT,*) '1 x_eps in Nan in',i
!   write(logunit,*) 'xEPS_CHL=',xEPS_CHL(i)
!   write(logunit,*) 'xeps=',xeps(i)
!   write(logunit,*) 'EIR=',EIR(iout)
!   write(logunit,*) 'R6c=',R6c(iout)
! endif

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
