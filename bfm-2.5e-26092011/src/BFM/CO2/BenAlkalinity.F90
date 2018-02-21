#include "INCLUDE.h"
#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenAlkalinity
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
  subroutine BenAlkalinityDynamics
!

#ifdef INCLUDE_BENCO2

! !USES:

  ! For the following Benthic-states fluxes are defined: G13h, G3h
  ! The following Benthic-states are used (NOT in fluxes): D1m, D6m, D2m
  ! The following global vars are modified: dummy
  ! The following global scalar vars are used: &
  !    NO_BOXES_XY,  &
  !   BoxNumberXY, InitializeModel, LocalDelta
  ! The following Benthic 1-d global boxvars are modified : KHplus, jbotO3h
  ! The following Benthic 1-d global boxvars got a value: Acae, Acan
  ! The following Benthic 1-d global boxvars are used: rrBTo, KQ1, &
  ! irrenh, ETW_Ben, rrATo, O3h_Ben, shiftD1m
  ! The following Benthic 1-d global boxpars  are used: p_poro
  ! The following 0-d global parameters are used: p_d_tot, p_q10diff
  ! The following global constants are used: RLEN
  ! The following constants are used: GET, &
  ! LABDA_1, COEFFICIENT, LAYERS, LAYER1, LAYER2,LAYER3,  LAYER5, &
  ! DIFFUSION, FOR_ALL_LAYERS, POROSITY, ADSORPTION, DOUBLE_DEFINE, &
  ! ZERO_EXPONENTIAL_TERM, DEFINE, QUADRATIC_TERM, LINEAR_TERM, CONSTANT_TERM, &
  ! SET_CONTINUITY, FLAG, MASS, SET_BOUNDARY, EQUATION, &
  ! INPUT_TERM, PARAMETER, &
  ! SET_LAYER_INTEGRAL_UNTIL, SET_LAYER_INTEGRAL, ADD, DERIVATIVE, &
  ! RFLUX, SHIFT

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  USE BFM_ERROR_MSG, ONLY: set_warning_for_getm
  use CO2System, ONLY:HplusBASIS
  use global_mem, ONLY:RLEN,ZERO
  use mem,  ONLY: G23h,G13h, G3h, D1m, D6m,D7m, D2m, D2STATE
  use mem, ONLY: ppG23h, ppG13h, ppG3h, Acae, Acan, &
    idummy,dummy,NO_BOXES_XY, LocalDelta,sK4K3,  &
     BoxNumberXY, InitializeModel, KHplus,KNO3,KNH4,KRED, jbotO3h,&
    irrenh, ETW_Ben, jK4K3n,jG2K7o,rrATo,reBTn,reATn, O3h_Ben,ctO3m2h, &
    shiftD1m, shiftD2m, iiBen, flux,jG33G23h,max_change_per_step
  use constants, ONLY: GET, LABDA_1, DIFFUSION, COEFFICIENT,&
    LAYERS, LAYER1, LAYER2, LAYER3,LAYER4,LAYER5,FOR_ALL_LAYERS, POROSITY, &
    ADSORPTION, DOUBLE_DEFINE, DEFINE, QUADRATIC_TERM, &
    LINEAR_TERM, CONSTANT_TERM, SET_CONTINUITY, FLAG, MASS, SET_BOUNDARY, &
    EQUATION, INPUT_TERM, PARAMETER, ZERO_EXPONENTIAL_TERM, &
    SET_LAYER_INTEGRAL_UNTIL, SET_LAYER_INTEGRAL,LABDA_2, &
    ADD, DERIVATIVE, RFLUX, SHIFT,INTEGRAL
  use mem_Param,  ONLY: p_poro, p_d_tot, p_d_tot_2,p_q10diff,p_small, p_qon_dentri,p_clDxm
  use mem_BenAlkalinity
  use mem_BenAmmonium,only:p_slK4K3
  use mem_BenNitrate,only:p_slK3G4
  use mem_PelCO2,ONLY:p_qhK4K3n,p_qhATo,p_qhK3G4n
  use LimitRates, ONLY:LimitChange,LimitShift,LimitShift_m3


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:GetInfoFromSet, &
  ! InitializeSet, DefineSet, CompleteSet, CalculateSet, CalculateTau, &
  ! CalculateFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use bennut_interface, ONLY: GetInfoFromSet, InitializeSet, DefineSet, &
    CompleteSet, CalculateSet, CalculateTau, CalculateFromSet,GetInfoFromSet


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
  integer     :: i,j
  real(RLEN)  :: r
  real(RLEN)  :: s
  real(RLEN)  :: loss
  real(RLEN)  :: jflux
  real(RLEN)  :: lambda
  real(RLEN)  :: gamma,beta
  real(RLEN)  :: alpha
  real(RLEN)  :: diff
  real(RLEN)  :: Tau
  real(RLEN)  :: cG3h
  real(RLEN)  :: zuD1_n
  real(RLEN)  :: zuD1
  real(RLEN)  :: zuD2
  real(RLEN)  :: jG3O3h
  real(RLEN)  :: jG13G3h
  real(RLEN)  :: jG23G13h
  real(RLEN)  :: Dnew
  real(RLEN)  :: jAT1AT2o
  real(RLEN)  :: jG24G23h
  real(RLEN)  :: jG14G13h
  real(RLEN)  :: sK13G4h
  real(RLEN)  :: sK13G4o
  real(RLEN)  :: sK4K3h,sOh
  real(RLEN)  :: jK6G4o
  real(RLEN)  :: jK26G4o
  real(RLEN)  :: local_change_per_step
  real(RLEN)  :: corr_1,corr_2
  real(RLEN)  :: a11,a12,a15,n15,n11,n13
  real(RLEN)  :: r11,r12,r15
  real(RLEN)  :: n21,n22,n31
  real(RLEN)  :: reBTh,reATh,jK24K14h
  real(RLEN)  :: Dxm,Dym,Ac13,Ac23
  real(RLEN)  :: limit_rate_0,limit_rate_1,limit_rate_2,limit_rate_3,limit_rate_4

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do BoxNumberXY=1,NO_BOXES_XY

   local_change_per_step=max_change_per_step

   ! Check if there is an exponential term defined for KRED 
   r=GetInfoFromSet(KRED(BoxNumberXY),-GET,idummy,11)
   j=0;if (r>0.0) j=1;

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate the belonging concentrations for the state variables
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      Acae(BoxNumberXY) = G3h(BoxNumberXY)/ p_poro(BoxNumberXY)/( &
        p_p+ 1.0D+00)/D1m(BoxNumberXY)
      Acan(BoxNumberXY) = (G23h(BoxNumberXY)+G13h(BoxNumberXY))/ p_poro(BoxNumberXY)/( &
        p_p+ 1.0D+00)/( p_d_tot_2- D1m(BoxNumberXY))
      Ac23 = G23h(BoxNumberXY)/ p_poro(BoxNumberXY)/( &
        p_p+ 1.0D+00)/( p_d_tot_2- D2m(BoxNumberXY))
      Ac13 = G13h(BoxNumberXY)/ p_poro(BoxNumberXY)/( &
        p_p+ 1.0D+00)/( D2m(BoxNumberXY)- D1m(BoxNumberXY))

      if ( Acae(BoxNumberXY).lt.0.0) then
        G3h(BoxNumberXY) = HplusBASIS *  p_poro(BoxNumberXY)*(  p_p+ 1.0D+00)*D1m(BoxNumberXY)
        write(LOGUNIT,*) 'BA:G3h reset!'
        call set_warning_for_getm
      endif
      if ( Ac13.gt.2.0*Hplusbasis.or.Ac13.lt.0.0) then
        G13h(BoxNumberXY) = HplusBASIS * p_poro(BoxNumberXY)*( &
        p_p+ 1.0D+00)*( D2m(BoxNumberXY)- D1m(BoxNumberXY)) 
        write(LOGUNIT,*) 'BA:G13h reset!'
        call set_warning_for_getm
      endif
      if ( Ac23.gt.2.0*Hplusbasis) then
        G23h(BoxNumberXY) = HplusBASIS * p_poro(BoxNumberXY)*( &
        p_p+ 1.0D+00)*( p_d_tot_2- D2m(BoxNumberXY))
        write(LOGUNIT,*) 'BA:G23h reset!'
        call set_warning_for_getm
      endif

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! NH4-minrealization
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      alpha  =   1.0D+00/ max(  p_clDxm,  D6m(BoxNumberXY))
      zuD1_n = max( 1.D-20, reATn(BoxNumberXY))/ p_poro(BoxNumberXY)/ &
                        IntegralExp( -alpha, p_d_tot_2- D1m(BoxNumberXY))
      zuD2  =   zuD1_n* exp( - alpha*( D2m(BoxNumberXY)- D1m(BoxNumberXY)))

       
      reBTh=0.0
      reATh=0.0
      jK24K14h = 0.0
!     reBTh=-0.125*reBTn(BoxNumberXY)
!     reATh=-0.125*reATn(BoxNumberXY)
!     jK24K14h = 0.125*zuD2* p_poro(BoxNumberXY)* IntegralExp( - alpha, &
!       p_d_tot_2- D2m(BoxNumberXY))


      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! H+-loss  due to nitrification and deoxidation of H--
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      loss= p_qhK4K3n* jK4K3n(BoxNumberXY)+  p_qhATo* jG2K7o(BoxNumberXY) 

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Temperature Correction (diffusion)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      diff = p_diff* irrenh(BoxNumberXY)* eTq( ETW_Ben(BoxNumberXY), p_q10diff)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Get coefficients describing ammonium in the oxic layer :
      ! 1. gamma of the exponential curve
      ! 2. parameter of the nitrification term
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      gamma = GetInfoFromSet( KNH4(BoxNumberXY), GET, LABDA_1, 11)
      sK4K3h = p_qhK4K3n*max(p_slK4K3,sK4K3(BoxNumberXY))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Get coefficients of all terms of equation valid for the
      ! first layer of ammonium (integration constants)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      a11 = -GetInfoFromSet( KNH4(BoxNumberXY), GET, COEFFICIENT, 11)
      a12 = -GetInfoFromSet( KNH4(BoxNumberXY), GET, COEFFICIENT, 12)
      a15 = -GetInfoFromSet( KNH4(BoxNumberXY), GET, COEFFICIENT, 15)

      if (j==1) then
         beta = GetInfoFromSet( KRED(BoxNumberXY), GET, LABDA_1, 11)
         sOh=GetInfoFromSet( KRED(BoxNumberXY), GET, LABDA_2, 11)*p_qhATo
         r11 = -GetInfoFromSet( KRED(BoxNumberXY), GET, COEFFICIENT, 11)
         r12 = -GetInfoFromSet( KRED(BoxNumberXY), GET, COEFFICIENT, 12)
         r15 = -GetInfoFromSet( KRED(BoxNumberXY), GET, COEFFICIENT, 15)
         n15  =  sK4K3h* a15 +  sOh*r15
         n13  =  r11 * sOh
         n11  =  a11 * sK4K3h
      else
        n15  =  sK4K3h* a15 -  p_qhATo* jG2K7o(BoxNumberXY) /(D1m(BoxNumberXY)*p_poro(BoxNumberXY))
      endif

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Assume Negative Exponential Distribution of Part.Carb. according D6.m
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      alpha  =   1.0D+00/ D6m(BoxNumberXY)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Recalculate Mineralization m2 --> m3 porewater
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      ! anoxidation rate at interface D1m 

      sK13G4h = GetInfoFromSet( KNO3(BoxNumberXY), GET, LABDA_2, 21) * p_qhK3G4n
      sK13G4o = GetInfoFromSet( KNO3(BoxNumberXY), GET, LABDA_2, 21) / p_qon_dentri
      jK6G4o=max(0.0,CalculateFromSet( KNO3(BoxNumberXY), INTEGRAL, &
                      RFLUX, D1m(BoxNumberXY),D2m(BoxNumberXY)))* sK13G4o !*limit_flux
      jK26G4o=max(0.0,CalculateFromSet( KNO3(BoxNumberXY), INTEGRAL, &
                   RFLUX, D2m(BoxNumberXY),p_d_tot_2 ))* sK13G4o !*limit_flux
      if ( max(rrATo(BoxNumberXY),jK6G4o) > 1.0D-10 ) then
        zuD1 = (rrATo(BoxNumberXY))/ p_poro(BoxNumberXY)/ IntegralExp( & 
                                          -alpha, p_d_tot- D1m(BoxNumberXY))
        zuD2  = zuD1* exp( - alpha*( D2m(BoxNumberXY)- D1m(BoxNumberXY)))

        jAT1AT2o= zuD2 * IntegralExp( -alpha, p_d_tot- D2m(BoxNumberXY))*p_poro(BoxNumberXY) 
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! 
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        s=rrATo(BoxNumberXY)-jAT1At2o
!       s=max(rrATo(BoxNumberXY)-jAT1At2o,jK6G4o)
        r=s- jK6G4o;        ;corr_1=r/ (s+p_small)

        s=jAT1AT2o
!       s=max(jAT1AT2o,jK26G4o)
        r=s-jK26G4o; corr_2=r/(s+p_small)
      else
        zuD1=1.0D-6;zuD2=0.0;
        corr_1=0.0;corr_2=0.0;
        jAT1AT2o=0.0;
      endif

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Get coefficients describing Nitrate in anoxic layer :
      ! 1. lambda of the exponential curve, and the denitrification rate
      ! 2. parameter of the denitrification term (integration constant)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      lambda = abs(GetInfoFromSet( KNO3(BoxNumberXY), GET, LABDA_1, 21))
      r=max(p_slK3G4* p_qhK3G4n, sK13G4h)
      n21 =  GetInfoFromSet( KNO3(BoxNumberXY), GET, COEFFICIENT, 21)* r
      n22 =  GetInfoFromSet( KNO3(BoxNumberXY), GET, COEFFICIENT, 22)* r
      n31 =  GetInfoFromSet( KNO3(BoxNumberXY), GET, COEFFICIENT, 31)* r

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Initialize and input physical boundaries and forcing:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      ! With no boundary condition at the end (sw_set=0) and 0-flux at 30cm 
      ! (p_flux_at_deep_end=1) assumption is made for calculation of gradient 
      ! that the boundary condition for this calculation  at 30 cm  is 0
      i=0;if (p_flux_at_deep_end>1.or.sw_set==1)i=1;
      KHplus(BoxNumberXY)  =   InitializeSet(  KHplus(BoxNumberXY),  5,  16+i+sw_set+j*2)

      Dxm=(D1m(BoxNumberXY)+D2m(BoxNumberXY)) *0.5
      Dym=(D2m(BoxNumberXY)+p_d_tot) *0.5
      call DefineSet( KHplus(BoxNumberXY), LAYERS, LAYER1, &
                                         LAYER2, D1m(BoxNumberXY),Dxm)
      call DefineSet( KHplus(BoxNumberXY), LAYERS, LAYER3, &
                                         LAYER4, D2m(BoxNumberXY),Dym)
      call DefineSet( KHplus(BoxNumberXY), DIFFUSION, FOR_ALL_LAYERS, 0, diff, dummy)
      call DefineSet( KHplus(BoxNumberXY), POROSITY, FOR_ALL_LAYERS, 0, &
                                                          p_poro(BoxNumberXY), dummy)
      call DefineSet( KHplus(BoxNumberXY), ADSORPTION, FOR_ALL_LAYERS, 0, p_p, dummy)


      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Give particular solution for all layers:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      call DefineSet( KHplus(BoxNumberXY), DOUBLE_DEFINE, 11, &
                               ZERO_EXPONENTIAL_TERM, gamma, sK4K3h)

      call DefineSet( KHplus(BoxNumberXY), DOUBLE_DEFINE, 12, &
                               ZERO_EXPONENTIAL_TERM, - gamma, sK4K3h)

      if ( j==1) then
        call DefineSet( KHplus(BoxNumberXY), DOUBLE_DEFINE, 13, &
                               ZERO_EXPONENTIAL_TERM, beta, sOh)

        call DefineSet( KHplus(BoxNumberXY), DOUBLE_DEFINE, 14, &
                               ZERO_EXPONENTIAL_TERM, - beta, sOh)
      endif

      call DefineSet( KHplus(BoxNumberXY), DEFINE, 15, QUADRATIC_TERM, dummy, dummy)
      call DefineSet( KHplus(BoxNumberXY), DEFINE, 16, LINEAR_TERM, dummy, dummy)
      call DefineSet( KHplus(BoxNumberXY), DEFINE, 17, CONSTANT_TERM, dummy, dummy)

      call DefineSet( KHplus(BoxNumberXY), DEFINE, 21, ZERO_EXPONENTIAL_TERM, &
                                                                  -alpha, dummy)
      call DefineSet(KHplus(BoxNumberXY), DOUBLE_DEFINE, 22, &
                                           ZERO_EXPONENTIAL_TERM, -lambda, sK13G4h)
      call DefineSet(KHplus(BoxNumberXY), DOUBLE_DEFINE, 23, &
                                           ZERO_EXPONENTIAL_TERM, +lambda, sK13G4h)

      call DefineSet( KHplus(BoxNumberXY), DEFINE, 25, CONSTANT_TERM, dummy, dummy)

      call DefineSet(KHplus(BoxNumberXY), DEFINE, 31, ZERO_EXPONENTIAL_TERM, - &
                                                                     alpha, dummy)
      call DefineSet(KHplus(BoxNumberXY), DOUBLE_DEFINE, 32, &
                                           ZERO_EXPONENTIAL_TERM, -lambda, sK13G4h)
      call DefineSet(KHplus(BoxNumberXY), DEFINE, 35, CONSTANT_TERM, dummy, dummy)

      call DefineSet( KHplus(BoxNumberXY), DEFINE, 41, ZERO_EXPONENTIAL_TERM, &
                                                                  -alpha, dummy)
      call DefineSet( KHplus(BoxNumberXY), DEFINE, 44, LINEAR_TERM, dummy, dummy)
      call DefineSet( KHplus(BoxNumberXY), DEFINE, 45, CONSTANT_TERM, dummy, dummy)
      select case (sw_set)
        case (0)
         if (p_flux_at_deep_end>1) &
         call DefineSet( KHplus(BoxNumberXY), DEFINE, 51, ZERO_EXPONENTIAL_TERM, &
                                                                  -alpha, dummy)
         call DefineSet( KHplus(BoxNumberXY), DEFINE, 55, CONSTANT_TERM, dummy, dummy)
        case (1)
         call DefineSet( KHplus(BoxNumberXY), DEFINE, 51, ZERO_EXPONENTIAL_TERM, &
                                                                  -alpha, dummy)
         call DefineSet( KHplus(BoxNumberXY), DEFINE, 54, LINEAR_TERM, dummy, dummy)
         call DefineSet( KHplus(BoxNumberXY), DEFINE, 55, CONSTANT_TERM, dummy, dummy)
      end select

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Insert boundary conditions:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      !1-8:1-8 boundary conditions:
      call CompleteSet( KHplus(BoxNumberXY), SET_CONTINUITY, FLAG, MASS, dummy, dummy)

      !9:9th boundary condition:
      call CompleteSet( KHplus(BoxNumberXY), SET_BOUNDARY, LAYER1, &
                                               EQUATION, ZERO, O3h_Ben(BoxNumberXY))

      !10-11:10-11 boundary condition:
      select case  ( InitializeModel ) 
        case (0)
          call CompleteSet( KHplus(BoxNumberXY), SET_LAYER_INTEGRAL, &
                                            LAYER2, LAYER3, dummy, G13h(BoxNumberXY))
          call CompleteSet( KHplus(BoxNumberXY), SET_LAYER_INTEGRAL_UNTIL, &
                                         LAYER4, LAYER5, p_d_tot_2, G23h(BoxNumberXY))
        case(1)
          r  =   exp( - alpha*( Dxm - D1m(BoxNumberXY)))
          call FixProportionCoeff(KHplus(BoxNumberXY),21,31,1.0D+00,r)
      end select

     !12-13:12-13:
      call FixProportionCoeff(KHplus(BoxNumberXY),22,32,n21,n31)
      if (p_flux_at_deep_end>1.or.sw_set==1) &
      call FixProportionCoeff(KHplus(BoxNumberXY),22,23,n21,n22)
      
      !14:14
      call CompleteSet( KHplus(BoxNumberXY), INPUT_TERM, 21, PARAMETER, dummy, zuD1-zuD1_n)

      !15-16:15-16:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! a11 / (labda * labda * diff) = a12 / (labda * labda * diff)
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        call FixProportionCoeff(KHplus(BoxNumberXY),12,11,a12,a11)
      if ( j==1) then
        call FixProportionCoeff(KHplus(BoxNumberXY),14,13,r12,r11)
        call CompleteSet( KHplus(BoxNumberXY), INPUT_TERM, 11, PARAMETER, dummy, &
           value=n11)
        call CompleteSet( KHplus(BoxNumberXY), INPUT_TERM, 13, PARAMETER, dummy, &
           value=n13)
      else
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! a11 / (labda * labda * diff) = a15 / (2 * diff)
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        call FixProportionCoeff(KHplus(BoxNumberXY),11,13,a11,a15)
      endif

      if (sw_set==1) &
        call CompleteSet( KHplus(BoxNumberXY), SET_BOUNDARY, LAYER5, &
                                           EQUATION, 1.0* p_d_tot_2,  HplusBASIS )
      !17:18
      call CompleteSet( KHplus(BoxNumberXY), INPUT_TERM, 15, PARAMETER, dummy, &
           value=n15)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! 1. Calculate for above defined set of boundary conditions
      !      the gradient of the nutrient
      ! 2. Replace last condition by an alternative new one
      ! 3. Calculate the value belongin by the alternative condition with
      !  the gradient calculated under 1.
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      cG3h = CalculateSet( KHplus(BoxNumberXY), SET_LAYER_INTEGRAL, LAYER1, &
        LAYER1, dummy, ZERO)
!       cG3h = max(O3h_Ben(BoxNumberXY)*p_poro(BoxNumberXY)*D1m(BoxNumberXY), &
!               min(cG3h,Ac13*p_poro(BoxNumberXY)*D1m(BoxNumberXY)))

      if ( InitializeModel== 0) then
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Calculate adaptation time absolute and Delta() relative to Tau:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        Tau  =   CalculateTau( abs(loss)/(p_small+G3h(BoxNumberXY)),  diff,  p_p,  D1m(BoxNumberXY))

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Estimate the average value of K4n during the next time step:
        ! Hence this value depend as well as on adaptation time,
        ! ''old'' value, and on ''equilibrium value''
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        cG3h = cG3h+( G3h(BoxNumberXY)- cG3h)* IntegralExp( - LocalDelta/ &
          Tau, 1.0D+00)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Recalulate gradient, now using cG3h
        ! Complete first the alternative condition by inputting the value
        ! for G3h (cG3h)
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        dummy = CalculateSet( KHplus(BoxNumberXY), ADD, 0, 0, dummy, cG3h)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! flux affecting G13h and G23h (fixed fluxes)
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jG14G13h= p_qhATo* (corr_1* rrATo(BoxNumberXY)+ jK6G4o-jAT1AT2o )+(reATh +jK24K14h)
        jG24G23h= p_qhATo* (corr_2*jAT1AT2o+jK26G4o )-jK24K14h

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! flux at sediment water interface
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jG3O3h = CalculateFromSet(KHplus(BoxNumberXY),DERIVATIVE,RFLUX, ZERO, ZERO) 

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! flux at D1m
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jG13G3h = CalculateFromSet( KHplus(BoxNumberXY), DERIVATIVE, &
          RFLUX, D1m(BoxNumberXY), dummy)

        Dnew  =   D1m(BoxNumberXY)+ LocalDelta* shiftD1m(BoxNumberXY)
        jG13G3h = jG13G3h+ CalculateFromSet( KHplus(BoxNumberXY), SHIFT, &
            LAYER1, D1m(BoxNumberXY), Dnew)/ LocalDelta

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! flux at D2m
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jG23G13h =  CalculateFromSet( KHplus(BoxNumberXY), DERIVATIVE, RFLUX, &
          D2m(BoxNumberXY), dummy)

        Dnew  =   D2m(BoxNumberXY)+ LocalDelta* shiftD2m(BoxNumberXY)
        jG23G13h = jG23G13h+ CalculateFromSet( KHplus(BoxNumberXY), SHIFT, &
          LAYER3, D2m(BoxNumberXY), Dnew)/ LocalDelta

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! flux at underside
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         select case (p_flux_at_deep_end)  ! 1=no flux, 2= only fluxes_downwards (sink), 3,=full_flux
           case(1); jflux=0.0
           case(2); jflux=min(0.0,CalculateFromSet(KHplus(BoxNumberXY), DERIVATIVE, RFLUX, p_d_tot_2, ZERO))
           case(3); jflux=CalculateFromSet(KHplus(BoxNumberXY), DERIVATIVE, RFLUX, p_d_tot_2, ZERO)
        end select
        jG33G23h(BoxNumberXY) = jflux

        ! Damp for too large fluxes

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! limit_rates
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        r=(G23h(BoxNumberXY)+jG24G23h*LocalDelta) 
        s= jG33G23h(BoxNumberXY)
        s= s*insw(s) *insw(Ac13-AC23)   ! +s *insw(-s)*insw(2.0*Ac23-Ac13) 
        jG33G23h(BoxNumberXY)=s
        call LimitChange(1,s,r,   local_change_per_step,limit_rate_4)
        r=G23h(BoxNumberXY)+(jG24G23h+jG33G23h(BoxNumberXY)*limit_rate_4)*LocalDelta !&
!                                                     *insw(Ac23-0.5*(Ac13+HplusBASIS)) 
        s=p_d_tot_2-D2m(BoxNumberXY); r=D2m(BoxNumberXY)-D1m(BoxNUmberXY)
        call LimitShift_m3(jG23G13h,G13h(BoxNumberXY),G23h(BoxNumberXY), &
                      r,s,shiftD2m(BoxNumberXY),local_change_per_step,limit_rate_3)

        !Calculate first rate at sediment water interface.
        !If volume of the water above sediment limit the shift to the water
        !Consider the first layer (nearly) as a part of the pealgic system:
        ! s is optimal value
        call LimitShift(jG3O3h,G3h(BoxNumberXY),ctO3m2h(BoxNumberXY), &
                      local_change_per_step,r)
        call LimitChange(3,jG3O3h,ctO3m2h(BoxNumberXY),local_change_per_step,limit_rate_0)
        limit_rate_0=min(r,limit_rate_0)
        jG3O3h=jG3O3h*limit_rate_0

        r=D1m(BoxNumberXY); s=D2m(BoxNumberXY)-D1m(BoxNUmberXY)
        call LimitShift_m3(jG13G3h,G3h(BoxNumberXY),G13h(BoxNumberXY), &
                      r,s,shiftD1m(BoxNumberXY),local_change_per_step,limit_rate_2)
        limit_rate_2=min(limit_rate_0,limit_rate_2)

        !Forces flux to water column, especially when D1m is low, to keep G3h at appropiate values....
        s=O3h_Ben(BoxNumberXY)*D1m(BoxNumberXY)*p_poro(BoxNumberXY)
        r=diff* (G3h(BoxNumberXY)-s)*2.0/D1m(BoxNumberXY)
        ! Unrealistic profile:
        !if Acae > O3h and flux of O3h is into the sediment reset the flux to zero!
        jG3O3h=1.0D-80+jG3O3h *(1.0- insw(r)*insw(-jG3O3h))
        jG3O3h=max(0.0,jG3O3h,r) *insw(jG3O3h)+ jG3O3h*insw(-jG3O3h) 
        !In case of loss of 2 negative fluxes added them and distribute them over 
        !G3h and the lowest layer of the benthos
        s=loss + jG3O3h*insw(jG3O3h)
        r=G3h(boxNumberXY)+(jG13G3h*limit_rate_2+ jG3O3h*insw(-jG3O3h))*LocalDelta
        call LimitChange(1,s,r,    local_change_per_step,limit_rate_1)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! calculate limitation  and correct the fluxes
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jG13G3h =              jG13G3h   * limit_rate_2
        jG23G13h=              jG23G13h  * limit_rate_3
        jG33G23h(BoxNumberXY)= jG33G23h(BoxNumberXY)* limit_rate_4

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! set rates
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        call flux(BoxNumberXY, iiBen, ppG3h, ppG3h,  -loss*limit_rate_1 )

        call flux(BoxNumberXY, iiBen, ppG13h, ppG13h, jG14G13h )
        call flux(BoxNumberXY, iiBen, ppG23h, ppG23h, jG24G23h)

        !limit flux from sedimt is case of hight losses 
        r=jG3O3h*limit_rate_1*insw(jG3O3h)+jG3O3h*insw(-jG3O3h)
        !jbot > 0.0 : input to pelagic, loss which cannot be effecuated in 
        ! benthos wil be subtracted in pelagic. Hence the rest ofloess must be subtracted
        ! from flux
        jbotO3h(BoxNumberXY)=r-(1.0-limit_rate_1)*loss
        call flux(BoxNumberXY, iiBen, ppG3h, ppG3h,-r )

        call flux(BoxNumberXY, iiBen, ppG13h, ppG3h,   jG13G3h* insw(  jG13G3h) )
        call flux(BoxNumberXY, iiBen, ppG3h, ppG13h, - jG13G3h* insw( -jG13G3h) )

        call flux(BoxNumberXY, iiBen, ppG23h, ppG13h,   jG23G13h* insw(  jG23G13h) )
        call flux(BoxNumberXY, iiBen, ppG13h, ppG23h, - jG23G13h* insw( -jG23G13h) )

        call flux(BoxNumberXY, iiBen, ppG23h, ppG23h, jG33G23h(BoxNumberXY) )

        r=-jG3O3h*limit_rate_1+jG13G3h-loss*limit_rate_1
        if (Acae(BoxNumberXY) > 5.0* HplusBASIS.or.-r*LocalDelta> G3h(BoxNumberXY).or.G3h(BoxNumberXY)<0.0) then 
            write(LOGUNIT,*) 'BA:O3h',O3h_Ben(BoxNumberXY)
            write(LOGUNIT,*) 'BA:ACae,G3h',ACae(BoxNumberXY),G3h(BoxNumberXY)
            write(LOGUNIT,*) 'BA:diff',ACae(BoxNumberXY)-O3h_Ben(BoxNumberXY)
            write(LOGUNIT,*) 'BA:netchange from G3h',r,r*LocalDelta 
            write(LOGUNIT,*) 'BA:loss',loss,limit_rate_1
            write(LOGUNIT,*) 'BA:jG3O3h',jG3O3h
            write(LOGUNIT,*) 'BA:jG13G3h',jG13G3h,limit_rate_2
            write(LOGUNIT,*) 'BA:jG23G13h',jG23G13h
            write(LOGUNIT,*) 'BA:jG33G23h',jG33G23h(BoxNumberXY)
            write(LOGUNIT,*) 'BA:jG14G13h',jG14G13h
            call set_warning_for_getm
        endif
      end if
  end do
#endif

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
