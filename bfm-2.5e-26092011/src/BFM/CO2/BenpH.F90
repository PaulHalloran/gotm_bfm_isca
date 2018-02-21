#include "INCLUDE.h"
#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenpH
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
  subroutine BenpHDynamics
!

#ifdef INCLUDE_BENCO2

! !USES:

  ! For the following Benthic-states fluxes are defined: G13c, G3c
  ! The following Benthic-states are used (NOT in fluxes): D1m, Q1c, D2m
  ! The following global scalar vars are used: &
  !    NO_BOXES_XY,  &
  !   BoxNumberXY, InitializeModel, LocalDelta
  ! The following Benthic 1-d global boxvars are modified : KCO2, jbotO3c
  ! The following Benthic 1-d global boxvars got a value: DICae, DICan
  ! The following Benthic 1-d global boxvars are used: rrBTo, KQ1, &
  ! irrenh, ETW_Ben, rrATo, shiftD1m
  ! The following Benthic 2-d global boxvars  are used: ruHI
  ! The following groupmember vars  are used: iiH1
  ! The following Benthic 1-d global boxpars  are used: p_poro
  ! The following 0-d global parameters are used: p_d_tot, p_q10diff
  ! The following global constants are used: RLEN
  ! The following constants are used: GET, &
  ! LABDA_1, COEFFICIENT, LAYERS, LAYER1, LAYER2, &
  ! DIFFUSION, FOR_ALL_LAYERS, POROSITY, ADSORPTION, DOUBLE_DEFINE, &
  ! ZERO_EXPONENTIAL_TERM, DEFINE, QUADRATIC_TERM, LINEAR_TERM, CONSTANT_TERM, &
  ! SET_CONTINUITY, FLAG, MASS, SET_BOUNDARY, EQUATION, &
  ! INPUT_TERM, PARAMETER, START_ADD_TERM, INPUT_ADD_TERM, &
  ! SET_LAYER_INTEGRAL_UNTIL, LAYER3, SET_LAYER_INTEGRAL, ADD, DERIVATIVE, &
  ! RFLUX, SHIFT, ONE_PER_DAY

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem,  ONLY: G23h,G13h, G3h, G23c,G13c, G3c, D1m, D2m,O3c_Ben, D2STATE
  use mem, ONLY: NO_BOXES_XY,   &
     BoxNumberXY, DICae,  pHAe, pCO2ae, DICan,  pHan, pCO2an,  ETW_Ben, &
     HCO3ae, CO3ae, CO2ae,iiBen, ppG3h,ppG13h,ppG23h,set_for_state_fluxes_zero, &
    ESW_Ben, ERHO_Ben, M1p, M5s,AcAe, AcAn,M11p,M21p,D1m,D2m
  USE BFM_ERROR_MSG, ONLY: BFM_ERROR,set_warning_for_getm


  use CO2System,ONLY: CalcCO2System,HplusBASIS
  use mem_Param,  ONLY: p_d_tot,p_poro,p_d_tot_2



  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:GetInfoFromSet, &
  ! InitializeSet, DefineSet, CompleteSet, CalculateSet, CalculateTau, &
  ! CalculateFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
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
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN)  :: CO2
  real(RLEN)  :: HCO3
  real(RLEN)  :: CO3
  real(RLEN)  :: m1
  integer     :: error

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do BoxNumberXY=1,NO_BOXES_XY


      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate the belonging concentrations for the state variables
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

       error= CalcCO2System(2,ESW_Ben(BoxNumberXY),& 
                   ETW_Ben(BoxNumberXY),ERHO_Ben(BoxNumberXY),&
                   M1p(BoxNumberXY),M5s(BoxNumberXY),Acae(BoxNumberXY),&
                   CO2ae(BoxNUmberXY),HCO3ae(BoxNUmberXY),CO3ae(BoxNUmberXY), &
                   pHae(BoxNumberXY),&
                   DIC_in=DICae(BoxNumberXY),pCO2_out=pCO2ae(BoxNumberXY))
        if ( error > 0 ) then
            write(LOGUNIT,*)"BpH: pH outside range"
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:ESW_Ben',ESW_Ben(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:ETW_Ben',ETW_Ben(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:ERHO_Ben',ERHO_Ben(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:DICae',DICae(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:M1p',M1p(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:M5s',M5s(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:Acae',Acae(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:D1m',D1m(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',3G12.6)') 'BpH:G3h,G13h,G23h',&
                             G3h(BoxNumberXY),G13h(BoxNumberXY),G23h(BoxNumberXY)
            write(LOGUNIT,'('' pBpH:Hae='',G12.6)') pHae(BoxNumberXY)
            write(LOGUNIT,*) "BenpHDynamics pHae outside range 2-11"
            write(LOGUNIT,*) "GBpH:3h reset to appropiate values"
            G3h(BoxNumberXY)=1.0*HplusBASIS*p_poro(BoxNumberXY)*D1m(BoxNumberXY)
            call set_for_state_fluxes_zero(iiBen,ppG3h)
            pHae(BoxNumberXY)=-1
            call set_warning_for_getm()
!           call BFM_ERROR("BenpHDynamics","pHae outside range 2-11")
        endif
       m1=M11p(BoxNumberXY)*(D2m(BoxNumberXY)-D1m(BoxNumberXY)) + &
        M21p(BoxNumberXY)*(p_d_tot-D2m(BoxNumberXY))/ (p_d_tot-D1m(BoxNumberXY))

       error= CalcCO2System(2,ESW_Ben(BoxNumberXY),& 
                   ETW_Ben(BoxNumberXY),ERHO_Ben(BoxNumberXY),&
                   m1,M5s(BoxNumberXY),Acan(BoxNumberXY),&
                   CO2,HCO3,CO3,pHan(BoxNumberXY),&
                   DIC_in=DICan(BoxNumberXY),pCO2_out=pCO2an(BoxNumberXY))
        if ( error > 0 ) then
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:DIC_Ben',O3c_Ben(BoxNumberXY)/12.0
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:DICan',DICan(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:Acan,G3h',Acan(BoxNumberXY),G3h(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:M11p',m1
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:D1m',D1m(BoxNumberXY)
            write(LOGUNIT,'(A,'' ='',G12.6)') 'BpH:D2m',D2m(BoxNumberXY)
            write(LOGUNIT,'('' pBpH:Han='',G12.6)') pHan(BoxNumberXY)
            write(LOGUNIT,*) "BenpHDynamics:pHan outside range 2-11"
            if (Acan(BoxNumberXY) < 0.0.or.Acan(BoxNumberXY).gt.1.0e6 ) then
              if (G13h(BoxNumberXY) < 0.0.or. G13h(BoxNumberXY).gt.1.0e6 ) then
                G13h(BoxNumberXY)=1.0*HplusBASIS*p_poro(BoxNumberXY)*(D2m(BoxNumberXY)-D1m(BoxNumberXY))
                call set_for_state_fluxes_zero(iiBen,ppG13h)
              endif
              if (G23h(BoxNumberXY) < 0.0 .or. G13h(BoxNumberXY).gt.1.0e6 ) then
                G23h(BoxNumberXY)=1.0*HplusBASIS*p_poro(BoxNumberXY)*(p_d_tot_2-D2m(BoxNumberXY))
                call set_for_state_fluxes_zero(iiBen,ppG23h)
              endif
              write(LOGUNIT,*) "BpH:G13c and G23c reset to appropiate values"
            endif
            pHan(BoxNumberXY)=-1
            call set_warning_for_getm()
!           call BFM_ERROR("BenpHDynamics","pHan outside range 2-11")
        endif
  end do
#endif

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
