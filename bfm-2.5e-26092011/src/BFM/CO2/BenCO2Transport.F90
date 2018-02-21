#include "INCLUDE.h"
#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenCO2Transport
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
  subroutine BenCO2TransportDynamics
!

#ifdef INCLUDE_BENCO2

! !USES:

  ! For the following Benthic-states fluxes are defined: G13c, G3c
  ! The following Benthic-states are used (NOT in fluxes): D1m, D6m, D2m
  ! The following global vars are modified: dummy
  ! The following global scalar vars are used: &
  !    NO_BOXES_XY,  &
  !   BoxNumberXY, InitializeModel, LocalDelta
  ! The following Benthic 1-d global boxvars are modified : KCO2, jbotO3c
  ! The following Benthic 1-d global boxvars got a value: DICae, DICan
  ! The following Benthic 1-d global boxvars are used: rrBTo, &
  ! irrenh, ETW_Ben, rrATo, O3c_Ben, shiftD1m
  ! The following Benthic 2-d global boxvars  are used: ruHI
  ! The following Benthic 1-d global boxpars  are used: p_poro
  ! The following 0-d global parameters are used: p_d_tot, p_q10diff
  ! The following global constants are used: RLEN,ZERO
  ! The following constants are used: GET, &
  ! LABDA_1, LAYERS, LAYER1, LAYER2, &
  ! DIFFUSION, FOR_ALL_LAYERS, POROSITY, ADSORPTION, DOUBLE_DEFINE, &
  ! ZERO_EXPONENTIAL_TERM, DEFINE, QUADRATIC_TERM, LINEAR_TERM, CONSTANT_TERM, &
  ! SET_CONTINUITY, FLAG, MASS, SET_BOUNDARY, EQUATION, &
  ! INPUT_TERM, PARAMETER, &
  ! SET_LAYER_INTEGRAL_UNTIL, LAYER3, SET_LAYER_INTEGRAL, ADD, DERIVATIVE, &
  ! RFLUX, SHIFT, ONE_PER_DAY

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  USE BFM_ERROR_MSG, ONLY: set_warning_for_getm
  use global_mem, ONLY:RLEN,ZERO,LOGUNIT
  use mem,  ONLY: G23c,G13c, G3c, D1m, D6m, D2m, D2STATE
  use mem, ONLY: ppG23c, ppG13c, ppG3c, &
    dummy,    NO_BOXES_XY, jG33G23c,LocalDelta, max_change_per_step,   &
     BoxNumberXY, InitializeModel, KCO2, jbotO3c, DICae, &
    DICan, rrBTo, irrenh, ETW_Ben, rrATo, O3c_Ben, shiftD1m, shiftD2m, &
    Depth_Ben,iiBen, flux
  use constants, ONLY: LAYERS, LAYER1, LAYER2, DIFFUSION, FOR_ALL_LAYERS, POROSITY, &
    ADSORPTION, ZERO_EXPONENTIAL_TERM, DEFINE, QUADRATIC_TERM, &
    LINEAR_TERM, CONSTANT_TERM, SET_CONTINUITY, FLAG, MASS, SET_BOUNDARY, &
    EQUATION, INPUT_TERM, PARAMETER, &
    SET_LAYER_INTEGRAL_UNTIL, LAYER3, SET_LAYER_INTEGRAL, &
    ADD, DERIVATIVE, RFLUX, SHIFT, LAYER4,LAYER5
  use mem_Param,  ONLY: p_poro, p_d_tot,p_d_tot_2, p_q10diff
  use mem_BenCO2Transport
  use LimitRates, ONLY:LimitShift,LimitShift_m3,LimitChange
   use CO2System,ONLY: DICBasis

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:&
  ! InitializeSet, DefineSet, CompleteSet, CalculateSet, CalculateTau, &
  ! CalculateFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use bennut_interface, ONLY: InitializeSet, DefineSet, &
    CompleteSet, CalculateSet, CalculateTau, CalculateFromSet


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following global functions are used:eTq
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use global_interface,   ONLY: eTq

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following sesame functions are used:IntegralExp
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: IntegralExp,insw

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
  real(RLEN)  :: r,s
  real(RLEN)  :: alpha
  real(RLEN)  :: diff
  real(RLEN)  :: jflux
  real(RLEN)  :: Tau
  real(RLEN)  :: cG3c
  real(RLEN)  :: zu
  real(RLEN)  :: zuD1
  real(RLEN)  :: zuD2
  real(RLEN)  :: jG13G3c
  real(RLEN)  :: jG23G13c
  real(RLEN)  :: jG3O3c
  real(RLEN)  :: Dnew
  real(RLEN)  :: Dx
  real(RLEN)  :: Dy
  real(RLEN)  :: DIC13,DIC23

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do BoxNumberXY=1,NO_BOXES_XY


      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate the belonging concentrations for the state variables
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      DICae(BoxNumberXY) = G3c(BoxNumberXY)/ 12.0D+00/ p_poro(BoxNumberXY)/ &
          ( p_p+ 1.0D+00)/( D1m(BoxNumberXY))
      DICan(BoxNumberXY) = (G13c(BoxNumberXY)+G23c(BoxNumberXY)) &
          / 12.0D+00/ p_poro(BoxNumberXY)/( p_p+ 1.0D+00)/( p_d_tot- D1m(BoxNumberXY))

    DIC23 = G23c(BoxNumberXY)/12.0/ p_poro(BoxNumberXY)/( &
        p_p+ 1.0D+00)/( p_d_tot_2- D2m(BoxNumberXY))
      DIC13 = G13c(BoxNumberXY)/12.0/ p_poro(BoxNumberXY)/( &
        p_p+ 1.0D+00)/( D2m(BoxNumberXY)- D1m(BoxNumberXY))

!     if ( DICae(BoxNumberXY).gt.2.1*DICBASIS) then
!       G3c(BoxNumberXY) = DICBASIS*12.0 *  p_poro(BoxNumberXY)*(  p_p+ 1.0D+00)*D1m(BoxNumberXY)
!       write(LOGUNIT,*) 'G3c reset!'
!       call set_warning_for_getm
!     endif
!     if ( DIC13.gt.2.0*DICbasis) then
!       G13c(BoxNumberXY) = DICBASIS*12.0 * p_poro(BoxNumberXY)*( &
!       p_p+ 1.0D+00)*( D2m(BoxNumberXY)- D1m(BoxNumberXY)) 
!       write(LOGUNIT,*) 'G13c reset!'
!       call set_warning_for_getm
!     endif
!     if ( DIC23.gt.2.0*DICbasis) then
!       G23c(BoxNumberXY) = DICBASIS*12.0 * p_poro(BoxNumberXY)*( &
!       p_p+ 1.0D+00)*( p_d_tot_2- D2m(BoxNumberXY))
!       write(LOGUNIT,*) 'G23c reset!'
!       call set_warning_for_getm
!     endif

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Recalculate Mineralization mmo/m2 --> mgC/m3 porewater
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      zu = ( rrBTo(BoxNumberXY)*12.0) / D1m(BoxNumberXY)/ p_poro(BoxNumberXY)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Temperature Correction (diffusion)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      diff = p_diff* irrenh(BoxNumberXY)* p_poro(BoxNumberXY)* &
        eTq( ETW_Ben(BoxNumberXY), p_q10diff)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Assume Negative Exponential Distribution of Part.Carb. according D6.m
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      alpha  =   1.0D+00/ D6m(BoxNumberXY)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Recalculate Mineralization m2 --> m3 porewater
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      ! Anoxic Mineralization at D1.m of Q6; (mgC /m3/d)

      zuD1 = ( rrATo(BoxNumberXY)* 12.0D+00)/ p_poro(BoxNumberXY)/ IntegralExp( &
        -alpha, p_d_tot- D1m(BoxNumberXY))
     
      zuD2  =   zuD1* exp( - alpha*( D2m(BoxNumberXY)- D1m(BoxNumberXY)))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Initialize and input physical boundaries and forcing:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      KCO2(BoxNumberXY)  =   InitializeSet(  KCO2(BoxNumberXY),  5,  14)
      Dx=(D1m(BoxNumberXY) +D2m(BoxNumberXY)) * 0.5
      Dy=(D2m(BoxNumberXY) +p_d_tot) * 0.5

      call DefineSet( KCO2(BoxNumberXY), LAYERS, LAYER1, LAYER2, D1m(BoxNumberXY),Dx)
      call DefineSet( KCO2(BoxNumberXY), LAYERS, LAYER3, LAYER4, D2m(BoxNumberXY),Dy)

      call DefineSet( KCO2(BoxNumberXY), DIFFUSION, FOR_ALL_LAYERS, 0, diff,dummy)

      call DefineSet( KCO2(BoxNumberXY), POROSITY, FOR_ALL_LAYERS, 0, &
                                                       p_poro(BoxNumberXY), dummy)

      call DefineSet( KCO2(BoxNumberXY), ADSORPTION, FOR_ALL_LAYERS, 0, p_p,dummy)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Give particular solution for all layers:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      call DefineSet( KCO2(BoxNumberXY), DEFINE, 13, QUADRATIC_TERM, dummy, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 14, LINEAR_TERM, dummy, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 15, CONSTANT_TERM, dummy, dummy)

      call DefineSet( KCO2(BoxNumberXY), DEFINE, 21, ZERO_EXPONENTIAL_TERM, &
        -alpha, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 24, LINEAR_TERM, dummy, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 25, CONSTANT_TERM, dummy, dummy)

      call DefineSet( KCO2(BoxNumberXY), DEFINE, 31, ZERO_EXPONENTIAL_TERM, &
        -alpha, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 34, LINEAR_TERM, dummy, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 35, CONSTANT_TERM, dummy, dummy)

      call DefineSet( KCO2(BoxNumberXY), DEFINE, 41, ZERO_EXPONENTIAL_TERM, &
        -alpha, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 44, LINEAR_TERM, dummy, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 45, CONSTANT_TERM, dummy, dummy)

      call DefineSet( KCO2(BoxNumberXY), DEFINE, 51, ZERO_EXPONENTIAL_TERM, &
        -alpha, dummy)
      call DefineSet( KCO2(BoxNumberXY), DEFINE, 55, CONSTANT_TERM, dummy, dummy)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Insert boundary conditions:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      !1-8 boundary conditions:
      call CompleteSet( KCO2(BoxNumberXY), SET_CONTINUITY, FLAG, MASS, &
        dummy, dummy)

      !9th boundary condition:
      call CompleteSet( KCO2(BoxNumberXY), SET_BOUNDARY, LAYER1, &
        EQUATION, ZERO, O3c_Ben(BoxNumberXY))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! a11 / (labda * labda * diff) = a12 / (labda * labda * diff)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      !10th boundary condition:
      r  =   exp( - alpha*( Dx - D1m(BoxNumberXY)))
      call FixProportionCoeff(KCO2(BoxNumberXY),21,31,1.0D+00,r)

      !11-12 boundary condition:
      select case ( InitializeModel)
        case ( 0 )
          call CompleteSet( KCO2(BoxNumberXY), SET_LAYER_INTEGRAL, &
            LAYER2, LAYER3, dummy, G13c(BoxNumberXY))
          call CompleteSet( KCO2(BoxNumberXY), SET_LAYER_INTEGRAL_UNTIL, &
            LAYER4, LAYER5, p_d_tot_2, G23c(BoxNumberXY))
        case ( 1 )
          call CompleteSet( KCO2(BoxNumberXY), INPUT_TERM, 21, PARAMETER, dummy, zuD1)
          r  =   exp( - alpha*( D2m(BoxNumberXY)-Dx) )
          call FixProportionCoeff(KCO2(BoxNumberXY),31,41,1.0D+00,r)
      end select

      !13th boundary condition:
      r  =   exp( - alpha*( Dy- D2m(BoxNumberXY)))
      call FixProportionCoeff(KCO2(BoxNumberXY),41,51,1.0D+00,r)

      !14th boundary condition:
      call CompleteSet( KCO2(BoxNumberXY), INPUT_TERM, 13, PARAMETER, dummy, zu)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! 1. Calculate for above defined set of boundary conditions
      !      the gradient of the nutrient
      ! 2. Replace last condition by an alternative new one
      ! 3. Calculate the value belongin by the alternative condition with
      !  the gradient calculated under 1.
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      cG3c = CalculateSet( KCO2(BoxNumberXY), SET_LAYER_INTEGRAL, LAYER1, &
        LAYER1, dummy, ZERO)

      if ( InitializeModel== 0) then
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Calculate adaptation time absolute and Delta() relative to Tau:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        Tau  =   CalculateTau(  ZERO,  diff,  p_p,  D1m(BoxNumberXY))

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Estimate the average value of K4n during the next time step:
        ! Hence this value depend as well as on adaptation time,
        ! ''old'' value, and on ''equilibrium value''
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        cG3c = cG3c+( G3c(BoxNumberXY)- cG3c)* IntegralExp( - LocalDelta/ &
          Tau, 1.0D+00)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Recalulate gradient, now using cG3c
        ! Complete first the alternative condition by inputting the value
        ! for G3c (cG3c)
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        dummy = CalculateSet( KCO2(BoxNumberXY), ADD, 0, 0, dummy, cG3c)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! flux at D1.m
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jG13G3c = CalculateFromSet( KCO2(BoxNumberXY), DERIVATIVE, &
          RFLUX, D1m(BoxNumberXY), dummy)

        Dnew  =   D1m(BoxNumberXY)+ LocalDelta* shiftD1m(BoxNumberXY)
        jG13G3c = jG13G3c+ CalculateFromSet( KCO2(BoxNumberXY), SHIFT, &
          LAYER1, D1m(BoxNumberXY), Dnew)/ LocalDelta


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Damp for too large fluxes
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        r=D1m(BoxNumberXY); s=D2m(BoxNumberXY)-D1m(BoxNUmberXY)
        call LimitShift_m3(jG13G3c,G3c(BoxNumberXY),G13c(BoxNumberXY), &
                      r,s,shiftD1m(BoxNumberXY),max_change_per_step)

!       call LimitShift(jG13G3c,G3c(BoxNumberXY),G13c(BoxNumberXY),max_change_per_step)
        call flux(BoxNumberXY, iiBen, ppG13c, ppG3c,   jG13G3c* insw(  jG13G3c) )
        call flux(BoxNumberXY, iiBen, ppG3c, ppG13c, - jG13G3c* insw( -jG13G3c) )

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! flux at D2m
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jG23G13c = CalculateFromSet( KCO2(BoxNumberXY), DERIVATIVE, RFLUX, &
          D2m(BoxNumberXY), dummy)

        Dnew  =   D2m(BoxNumberXY)+ LocalDelta* shiftD2m(BoxNumberXY)
        jG23G13c = jG23G13c+ CalculateFromSet( KCO2(BoxNumberXY), SHIFT, &
          LAYER3, D2m(BoxNumberXY), Dnew)/ LocalDelta

        s=p_d_tot_2-D2m(BoxNumberXY); r=D2m(BoxNumberXY)-D1m(BoxNUmberXY)
        call LimitShift_m3(jG23G13c,G13c(BoxNumberXY),G23c(BoxNumberXY), &
                      r,s,shiftD2m(BoxNumberXY),max_change_per_step)

!       call LimitShift(jG23G13c,G13c(BoxNumberXY),G23c(BoxNumberXY),max_change_per_step)
        jG23G13c = jG23G13c - zuD2* &
           p_poro(BoxNumberXY)* IntegralExp( -alpha, p_d_tot- D2m(BoxNumberXY));

        call flux(BoxNumberXY, iiBen, ppG23c, ppG13c,   jG23G13c* insw(  jG23G13c) )
        call flux(BoxNumberXY, iiBen, ppG13c, ppG23c, - jG23G13c* insw( -jG23G13c) )


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! flux at underside
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         select case (p_flux_at_deep_end)  ! 0=no flux, 1= only fluxes_downwards (sink), 2,=full_flux
           case(0); jflux=0.0
           case(1); jflux=min(0.0,CalculateFromSet(KCO2(BoxNumberXY), DERIVATIVE, RFLUX, p_d_tot_2, ZERO))
           case(2); jflux=CalculateFromSet(KCO2(BoxNumberXY), DERIVATIVE, RFLUX, p_d_tot_2, ZERO)
        end select

        jG33G23c(BoxNumberXY) = jflux
        call LimitChange(1,jG33G23c(BoxNumberXY),G23c(BoxNumberXY),max_change_per_step)
        call flux(BoxNumberXY, iiBen, ppG23c, ppG23c, jG33G23c(BoxNumberXY) )

        jG3O3c = CalculateFromSet( KCO2(BoxNumberXY), DERIVATIVE, &
          RFLUX, ZERO, ZERO)

        call LimitShift(jG3O3c,O3c_Ben(BoxNumberXY)*Depth_Ben(BoxNumberXY) ,&
                                                         G3c(BoxNumberXY),max_change_per_step)

        call flux(BoxNumberXY, iiBen, ppG3c, ppG3c, -jG3O3c )
        jbotO3c(BoxNumberXY)=jbotO3c(BoxNumberXY)+jG3O3c

      end if

  end do
#endif

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
