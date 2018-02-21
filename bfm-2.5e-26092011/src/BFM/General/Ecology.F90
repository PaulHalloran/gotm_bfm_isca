#INCLUDE "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: Ecology
!
! DESCRIPTION
!   !	This submodel calls all other submodels
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine EcologyDynamics
!
! !USES:
  ! The following 0-d global parameters are used: CalcPelagicFlag, &
  ! CalcBenthicFlag
  ! The following global constants are used: RLEN
  ! The following constants are used: BENTHIC_RETURN, BENTHIC_BIO, BENTHIC_FULL

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem,  ONLY: iiBen, iiPel, iiReset,flux,ETW,OCDepth,Depth,NO_BOXES,R1c
  use constants,  ONLY: BENTHIC_RETURN, BENTHIC_BIO, BENTHIC_FULL,SEC_PER_DAY
  use mem_Param,  ONLY: CalcPelagicFlag, CalcBenthicFlag
  use Track, ONLY:calc_all_track_rates,check_track_states
  use mem,ONLY: Output2d_1,Output2d_2,Output2d_3, Output2d_4
  use mem_globalfun,   ONLY: insw
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following global functions are used:ResetTotMassVar
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use global_interface,   ONLY: ResetTotMassVar
  use mem,ONLY:Source_D2_vector,Source_D3_vector,D3State,ppY3c,Y3c,NO_BOXES_XY,P6c &
               ,Pcc,ppN1p,ppP6p,ppR1c,ppPcc,ppZ2c,ppP6c,ppP2c,P2c,D3SOURCE,D3SINK

!  
!
! !AUTHORS
!   ERSEM team	
!
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
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE
  real(RLEN),external  ::GetDelta;
  real(RLEN),dimension(NO_BOXES)   :: r
  integer                          :: iout
  REAL(RLEN),dimension(NO_BOXES_XY):: r1
  
  
#ifdef DEBUG
  call  flux(1,iiReset,1,1,0.00D+00)
#endif
!D3SOURCE(:,:,:)=0.0
!D3SINK(:,:,:)=0.0


  call check_track_states(1,'at start of the BFM calculations')

  call  ResetTotMassVar( )

  call findnan(R1c,NO_BOXES,iout)
  if ( iout>0) write(logunit,*) 'Ecology at start:NAn in R1c layer',iout

  if ( CalcPelagicFlag) &
    call PelagicSystemDynamics

    call FindNaNInRates(iiPel,ppR1c,'Ecology:after PelagicSystems')

  call findnan(R1c,NO_BOXES,iout)
  if ( iout>0) write(logunit,*) 'After pelagic:NAn in rate R1c layer',iout
    

  if ( CalcBenthicFlag > 0 ) then

       call SettlingDynamics
  
       select case ( CalcBenthicFlag)

         case ( BENTHIC_RETURN )  ! Simple benthic return
           call BenthicReturn1Dynamics

         case ( BENTHIC_BIO )  ! Intermediate benthic return
           call PelForcingForBenDynamics
           call BenthicSystemDynamics
           call BenthicNutrient2Dynamics

         case ( BENTHIC_FULL )  ! Full benthic nutrients
           call PelForcingForBenDynamics
           call BenthicSystemDynamics
           call BenthicNutrient3Dynamics
           call Y3Z2CoupDynamics
       end select
       call FindNaNInRates(iiPel,ppR1c,'Ecology:after Benthic')

       call ControlBenPartNutrientBuffersDynamics

       call BentoPelCoupDynamics
       call findnan(R1c,NO_BOXES,iout)
       if ( iout>0) write(logunit,*) 'After bentocoup:NAn in R1c layer',iout
       call FindNaNInRates(iiPel,ppR1c,'Ecology:after BentoPelDynamics')


       
       call SedimentationDynamics


  endif

  call CheckMassConservationNPSDynamics
  call CheckMassConservationCDynamics
 

  call calc_all_track_rates(1)

  call CalculateThermoVars

       call FindNaNInRates(iiPel,ppR1c,'Ecology:at End')
       call findnan(R1c,NO_BOXES,iout)
       if ( iout>0) write(logunit,*) 'AtEnd:NaN in rate R1c layer',iout
  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
