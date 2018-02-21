#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: PelagicSystem
!
! DESCRIPTION
!   This is the Pelagic Submodel. 
!   All the pelagic biogeochemical modules are called in sequence
!   according to the logical switches
!        
!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine PelagicSystemDynamics
!
! !USES:
  ! The following Pelagic 1-d global boxvars  are used: Depth
  ! The following Benthic 1-d global boxvars are modified : totpeln, totpels
  ! The following Benthic 1-d global boxvars  are used: totpelp
  ! The following groupmember vars are used: iiP1, iiP2, iiP3, iiP4, iiZ3, iiZ4, &
  ! iiZ5, iiZ6, iiMesoZooPlankton
  ! The following 0-d global parameters are used: &
  ! ChlLightFlag
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem, ONLY: ppO3c,iiZ2,iiY3, iiP2,iiP6, iiC, iiN,iiP,iiS,iiL,&
      ppPhytoPlankton,iiPhytoPlankton, ppMesoZooPlankton,iiMesoZooPlankton, &
      ppMicroZooPlankton,iiMicroZooPlankton,sunPI, NO_BOXES,R1c

     
  use mem_Param, ONLY: ChlLightFlag, CalcPhytoPlankton,CalcMicroZooPlankton, &
    CalcMesoZooPlankton, CalcBacteria, CalcPelChemistry,CalcYy3,CalcBenOrganisms
  use mem_Phaeo,ONLY:NEW_COLONIES

  use mem,  ONLY: Output2d_1,Output2d_2,Output2d_3, Output2d_4
  use mem,ONLY:ppP1c,iiPel,ppN1p,ppR1c,ppR6c,ppR1c,ppZ2c,ppP6c,ppP2c,P2c,Depth,Source_D3_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following global functions are used:CalcChlorophylla, &
  ! CalcOxygenSaturation
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! use global_interface,   ONLY: CalcChlorophylla, CalcOxygenSaturation

 
  !-=-=-=-= -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following group processes are &
  ! used: PhotoAvailableRadiationDynamics, PhytoDynamics, &
  ! LightAdaptationDynamics, MesoZooDynamics, MicroZooDynamics
  use global_interface, ONLY: PhotoAvailableRadiationDynamics, &
    PhytoDynamics, LightAdaptationDynamics, MesoZooDynamics, MicroZooDynamics
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


!  
!
! !AUTHORS
!   ERSEM team
!
! !REVISION_HISTORY

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
  integer           ::i,ic,ip,in,il,is   ,iout
  real(RLEN)        ::rzero=0.0D+00
  real(RLEN),dimension(NO_BOXES)    :: r,s
  logical,dimension(iiMesoZooPlankton)   ::llMeso


  call  CalcChlorophylla( )


  call PelGlobalDynamics

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Compute oxygen variables: cxoO2 eO2mO2
  ! calculate oxygen OxygenReaeration
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call  CalcOxygenSaturation( )


  call OxygenReaerationDynamics

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! CO2 ppO3c > 0 : all routines and variables are present for CO2processes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  if ( ppO3c > 0 )  call PelCO2Dynamics( )

  call LimitNutrientUptake


  if ( ChlLightFlag== 1) then
    ! Because when P6 is very low the rate R3->R2 is not calculated
    ! we have to clear all the rates from and to R3
    ! otherwise an "old rate" is used for another gridpoint/next time step.
    ! (the arrays ini which the rates are collected are not fully reinitiliazed
    ! for reasons of efficiency.......)
    do i=1,iiPhytoPlankton
      if ( CalcPhytoPlankton(i)) then
        ic=ppPhytoPlankton(i,iiC)
        in=ppPhytoPlankton(i,iiN)
        ip=ppPhytoPlankton(i,iiP)
        is=ppPhytoPlankton(i,iiS)
        il=ppPhytoPlankton(i,iiL)
        call PhotoAvailableRadiationDynamics( i, ic, in, ip, is, il)
      end if
    enddo
  endif

  do i=1,iiPhytoPlankton
    if ( CalcPhytoPlankton(i)) then
      ic=ppPhytoPlankton(i,iiC)
      in=ppPhytoPlankton(i,iiN)
      ip=ppPhytoPlankton(i,iiP)
      is=ppPhytoPlankton(i,iiS)
      il=ppPhytoPlankton(i,iiL)
      call PhytoDynamics( i, ic, in, ip, is, il)
    end if
enddo

  if ( ChlLightFlag== 1) then
    do i=1,iiPhytoPlankton
      if ( CalcPhytoPlankton(i)) then
        ic=ppPhytoPlankton(i,iiC)
        in=ppPhytoPlankton(i,iiN)
        ip=ppPhytoPlankton(i,iiP)
        is=ppPhytoPlankton(i,iiS)
        il=ppPhytoPlankton(i,iiL)
        call LightAdaptationDynamics( i, ic, in, ip, is, il)
      end if
    enddo
  end if

  do i=1,iiMicroZooPlankton
    if ( CalcMicroZooPlankton(i)) then
      ic=ppMicroZooPlankton(i,iiC)
      in=ppMicroZooPlankton(i,iiN)
      ip=ppMicroZooPlankton(i,iiP)
      call MicroZooDynamics( i, ic, in, ip)
    end if
  enddo

  llMeso(:)=CalcMesoZooPlankton(:)
  llMeso(iiZ2)=llMeso(iiZ2).and.CalcYy3 .and.CalcBenOrganisms(iiY3)
  do i=1,iiMesoZooPlankton
    if ( llMeso(i)) then
      ic=ppMesoZooPlankton(i,iiC)
      in=ppMesoZooPlankton(i,iiN)
      ip=ppMesoZooPlankton(i,iiP)
      call MesoZooDynamics( i, ic, in, ip)
    end if
  enddo

  if ( CalcBacteria) &
    call PelBacDynamics

  if ( CalcPelChemistry) &
    call PelChemDynamics

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate the biomass  of singelflagellates which are trasnferred into colonies
  ! For the calculation the total net nutrient uptake is needed
  ! A large nutrient uptake induces singel flagellates to make vcolonies.
  ! r is a dummy variable
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  if ( CalcPhytoPlankton(iiP6) .and. CalcPhytoPlankton(iiP2)) &
      call PhaeocystisCalc(NEW_COLONIES,iiP2,r,sunPI(iiP2,:),rzero)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! CO2 ppO3c > 0 : all routines and variables are present for CO2processes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  if ( ppO3c > 0 )  call AlkalinityDynamics( )



  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

