#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: Sedimentation
!
! DESCRIPTION
!   Define all fluxes of material which arrive on the sediment:
!	a, fluxes of detritus (slow degradable and labile organic detritus)
!       b. Changes in distributions states which describes exponential distribution.
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine SedimentationDynamics
!
! !USES:

  ! For the following Benthic-states fluxes are defined: Q6c, Q6n, Q6p, Q6s, &
  ! Q1c, Q1n, Q1p, D6m, D7m, D8m, D9m
  ! The following Benthic 1-d global boxvars are used: jbotR6c, jbotR6n, &
  ! jbotR6p, jbotR6s, jbotR1c, jbotR1n, jbotR1p
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
#IFDEF NOPOINTERS
  use mem,  ONLY: D2STATE
#ELSE
  use mem,  ONLY: Q6c, Q6n, Q6p, Q6s, D6m, D7m, D8m, D9m,D1m

#ENDIF
  use mem, ONLY: ppQ6c, ppQ6n, ppQ6p, ppQ6s, ppQ1c, ppQ1n, ppQ1p, &
    ppD6m, ppD7m, ppD8m, ppD9m, jbotR6c, jbotR6n, jbotR6p, jbotR6s, jbotR1c, &
    jbotR1n, jbotR1p, iiBen, flux,BoxNumberX, NO_BOXES_X, &
    BoxNumberY, NO_BOXES_Y, BoxNumberXY,NO_BOXES_XY

!  
!
! !AUTHORS
!   Piet Ruardij
!
!
!
! !REVISION_HISTORY
!   Created at Mon Nov 21 09:11:50 CET 2005
!
!
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
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  REAL(RLEN)      ::newDm(NO_BOXES_XY)
  REAL(RLEN)      ::LocalDelta
  real(RLEN), external  :: GetDelta
  integer         :: i

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! user defined external functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer, external  :: D2toD1
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  DO BoxNumberY=1,NO_BOXES_Y
    DO BoxNumberX=1,NO_BOXES_X
      BoxNumberXY=D2toD1(BoxNumberX,BoxNumberY)

      ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculated Fluxes from Pelagic to Benthic
      ! These fluxes are the sum sedimentation flux + the flux of
      ! material excreted by filterfeeders and originating from the pelagic.
      ! !!!!!!! ALL DETRITUS FLUXES FORM THE PELAGIC TO THE SEDIMENT ARE DIRECTED VIA R6 TO Q6 !!!!!!!!
      ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      call flux(BoxNumberXY, iiBen, ppQ6c,ppQ6c, -jbotR6c(BoxNumberXY) )
      call flux(BoxNumberXY, iiBen, ppQ6n,ppQ6n, -jbotR6n(BoxNumberXY) )
      call flux(BoxNumberXY, iiBen, ppQ6p,ppQ6p, -jbotR6p(BoxNumberXY) )
      call flux(BoxNumberXY, iiBen, ppQ6s,ppQ6s, -jbotR6s(BoxNumberXY) )

      call flux(BoxNumberXY, iiBen, ppQ1c,ppQ1c, -jbotR1c(BoxNumberXY) )
      call flux(BoxNumberXY, iiBen, ppQ1n,ppQ1n, -jbotR1n(BoxNumberXY) )
      call flux(BoxNumberXY, iiBen, ppQ1p,ppQ1p, -jbotR1p(BoxNumberXY) )

      ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculation of changes due to sedimentation of detritus in
      ! distribution state variables (Dx.m is a undetermined source).
      ! !!!!!!! ALL DETRITUS FLUXES FORM THE PELAGIC TO THE SEDIMENT ARE DIRECTED VIA R6 TO Q6 !!!!!!!!
      ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


       LocalDelta=GetDelta( )
       i=BoxNumberXY
       call RecalcPenetrationDepth( D1m(i), D6m(i), -jbotR6c(i)*LocalDelta, Q6c(i),newDm(i) )
       call flux(BoxNumberXY,iiBen, ppD6m,ppD6m,(newDm(i)- D6m(i))/LocalDelta)
       call RecalcPenetrationDepth( D1m(i), D7m(i), -jbotR6n(i)*LocalDelta, Q6n(i),newDm(i) )
       call flux(BoxNumberXY,iiBen, ppD7m,ppD7m,(newDm(i)- D7m(i))/LocalDelta)
       call RecalcPenetrationDepth( D1m(i), D8m(i), -jbotR6p(i)*LocalDelta, Q6p(i),newDm(i) )
       call flux(BoxNumberXY,iiBen, ppD8m,ppD8m,(newDm(i)- D8m(i))/LocalDelta)
       call RecalcPenetrationDepth( D1m(i), D9m(i), -jbotR6s(i)*LocalDelta, Q6s(i),newDm(i) )
       call flux(BoxNumberXY,iiBen, ppD9m,ppD9m,(newDm(i)- D9m(i))/LocalDelta)
    end DO
  end DO

  end



!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
