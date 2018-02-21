#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenAnoxic
!
! DESCRIPTION
!   Description of the anoxic diagenetic processes in the sediment
!       Details on the equations and the method used to calculate
!       the equilibrium and transient profiles can be found in
!       Ruardij et al., 1995. Neth. J. Sea Res. 33(3/4):453-483
!
!

!   This file is generated directly from OpenSesame model code, using a code
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine BenAnoxicDynamics
!
! !USES:

  ! For the following Benthic-states fluxes are defined: K16r, K6r, G2o
  ! The following Benthic-states are used (NOT in fluxes): D6m, D1m,D2m
  ! The following global vars are modified: dummy
  ! The following global scalar vars are used: &
  !    NO_BOXES_XY,  &
  !   BoxNumberXY, LocalDelta, InitializeModel
  ! The following Benthic 1-d global boxvars are modified : M6r, KRED, jbotN6r, &
  ! jG2K7o,jK26K16r
  ! The following Benthic 1-d global boxvars are used: rrATo, rrBTo, irrenh, &
  ! ETW_Ben, KNO3, N6r_Ben
  ! The following Benthic 1-d global boxpars  are used: p_poro
  ! The following 0-d global parameters are used: p_d_tot, p_clDxm, &
  ! p_qro, p_q10diff, p_qon_dentri
  ! The following global constants are used: RLEN,ZERO
  ! The following constants are used: GET, &
  ! LABDA_1, LABDA_2, COEFFICIENT, LAYERS, LAYER1, &
  ! DIFFUSION, FOR_ALL_LAYERS, POROSITY, ADSORPTION, DEFINE, &
  ! DOUBLE_DEFINE, CONSTANT_TERM, &
  ! SET_CONTINUITY, FLAG, MASS, SET_BOUNDARY, EQUATION, INPUT_TERM, &
  ! PARAMETER, SET_LAYER_INTEGRAL_UNTIL, LAYER2, ADD, DERIVATIVE, RFLUX, &
  ! INTEGRAL, INPUT_ADD_TERM,LAYER3

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,ZERO,LOGUNIT
  use mem,  ONLY: K26r, K16r, K6r, G2o, D6m, D1m, D2m, D2STATE
  use mem, ONLY: ppK26r, ppK16r, ppK6r, ppG2o, dummy,NO_BOXES_XY,    &
    BoxNumberXY, LocalDelta, InitializeModel, M6r, KRED, jbotN6r, jbotO2o, jG2K7o, rrATo, &
    rrBTo, jK36K26r, irrenh, ETW_Ben, KNO3, N6r_Ben,Depth_Ben, &
    shiftD1m, shiftD2m,iiBen, flux,jK3G4n,max_change_per_step
  use constants, ONLY: GET, LABDA_1, LABDA_2, COEFFICIENT, SET_LAYER_INTEGRAL,&
    LAYERS, LAYER1, DIFFUSION, FOR_ALL_LAYERS, POROSITY, ADSORPTION, &
    DEFINE, DOUBLE_DEFINE, ZERO_EXPONENTIAL_TERM,LINEAR_TERM, EXPONENTIAL_TERM, &
    CONSTANT_TERM, SET_CONTINUITY, FLAG, MASS, SET_BOUNDARY, EQUATION, &
    INPUT_TERM, PARAMETER, STANDARD,SET_LAYER_INTEGRAL_UNTIL, ADD, &
    RFLUX, DERIVATIVE, INTEGRAL,LAYER2,  LAYER3, LAYER4,LAYER5,SHIFT, &
    QUADRATIC_TERM
  use mem_Param,  ONLY: p_poro, p_d_tot,p_d_tot_2, p_clDxm, p_qro, p_q10diff, p_qon_dentri
  use mem_BenAnoxic
  use LimitRates, ONLY:LimitChange
  USE BFM_ERROR_MSG, ONLY: set_warning_for_getm

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:GetInfoFromSet, &
  ! InitializeSet, DefineSet, CompleteSet, CalculateSet, CalculateTau, &
  ! CalculateFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use bennut_interface, ONLY: GetInfoFromSet, InitializeSet, DefineSet, &
    CompleteSet, CalculateSet, CalculateTau, CalculateFromSet

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following global functions are used:eTq
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use global_interface,   ONLY: eTq

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following sesame functions are used:IntegralExp, insw
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: IntegralExp, insw
!
!
! !AUTHORS
!   Original version by  P. Ruardij
!
! !REVISION_HISTORY
!   September 1999 by M. Vichi Commented version
!
!
! COPYING
!
!   Copyright (C) 2006 P. Ruardij & M.VIchi
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
  real(RLEN)  :: alpha
  real(RLEN)  :: zuD1
  real(RLEN)  :: diff
  real(RLEN)  :: gamma
  real(RLEN)  :: lambda
  real(RLEN)  :: sK13G4
  real(RLEN)  :: n21
  real(RLEN)  :: n22
  real(RLEN)  :: n31
  real(RLEN)  :: Tau
  real(RLEN)  :: cK6r
  real(RLEN)  :: jBTK6r
  real(RLEN)  :: jATK6r
  real(RLEN)  :: jK26K16r
  real(RLEN)  :: jK6BTr
  real(RLEN)  :: jK6G4r
  real(RLEN)  :: jK16K6r
  real(RLEN)  :: limit_rate_0,limit_rate_1,limit_rate_2,limit_rate_3
  real(RLEN)  :: jATK26r
  real(RLEN)  :: jK26G4r 
  real(RLEN)  :: Dnew
  real(RLEN)  :: sOS
  real(RLEN)  :: r
  real(RLEN)  :: s
  real(RLEN)  :: Dxm,Dym,M16r
  real(RLEN)  :: M0

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do BoxNumberXY=1,NO_BOXES_XY

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate the pore-water average concentrations from the state variables
      ! (Diagnostic variables, not used in calculations)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      if ( K6r(BoxNumberXY)< 0.0 ) then
         K6r(BoxNumberXY)=0.0
         write(LOGUNIT,*) 'K6r< 0.0, reset on zero'
         call set_warning_for_getm
      endif
      if ( K16r(BoxNumberXY)< 0.0 ) then
         K16r(BoxNumberXY)=0.0
         write(LOGUNIT,*) 'K16r< 0.0, reset on zero'
         call set_warning_for_getm
      endif
!     if ( K26r(BoxNumberXY)< 0.0 ) then
!        K26r(BoxNumberXY)=0.0
!        write(LOGUNIT,*) 'K26r< 0.0, reset on zero'
!        call set_warning_for_getm
!     endif
      M6r(BoxNumberXY) = K6r(BoxNumberXY)/ p_poro(BoxNumberXY)/( p_p+ 1.0D+00)/( &
        D1m(BoxNumberXY))
      M16r = K16r(BoxNumberXY)/ p_poro(BoxNumberXY)/( p_p+ 1.0D+00)/( &
        D2m(BoxNumberXY)-D1m(BoxNumberXY))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate coefficient for the e-folding distribution of the anoxic
      ! mineralization. D6.m is the average penetration depth for C-detritus
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      alpha  =   1.0D+00/ max(  p_clDxm,  D6m(BoxNumberXY))


      if ( InitializeModel == 0 ) then
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Convert anoxic mineralization (mmol S/m2/d)
        ! This rate is already assigned to the dynamical equation for K6.r
        ! in BenBacDyanmics for H2:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jATK6r  =   p_qro* rrATo(BoxNumberXY)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Recalculate Mineralization m2 --> m3 porewater
        ! Anoxic mineralization at D1.m
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        zuD1 = jATK6r/ p_poro(BoxNumberXY)/ IntegralExp( - alpha, &
             p_d_tot- D1m(BoxNumberXY))
      else
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! In case of no info about anoxi mineralization at the start
        ! Reconstruct using detritus distribution alpha and oxic mineralization
        ! an anoxic mineralization at the upperside of the denitrification layer
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        jBTK6r= p_qro * rrBTo(BoxNumberXY)
        zuD1 = 0.01 * jBTK6r / p_poro(BoxNumberXY)/   &
            IntegralExp( - alpha, D1m(BoxNumberXY)) *exp(-alpha * D1m(BoxNumberXY))
      endif

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Correction due to environmental regulating factors,
      ! diffusion coefficient: temperature and bioirrigation
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      diff = p_diff* irrenh(BoxNumberXY)* eTq( ETW_Ben(BoxNumberXY), p_q10diff)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate coefficient for the exponential terms of the solution
      ! limitation of reoxidation of first order process at low anoxic 
      ! mineralization 
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      sOS=max(0.001,p_sOS * eTq( ETW_Ben(BoxNumberXY),p_q10)  &
            *rrATo(BoxNumberXY)/(0.01+rrAto(BoxNumberXY)))
      select case (sw_set) 
        case (0)
          r=sOS*K6r(BoxNumberXY)
          call LimitChange(1,r,p_qro*G2o(BoxNumberXY) , max_change_per_step,s)
          sOS=max(0.0,s* sOS)
        case (1)
           gamma= sqrt(sOS/diff)
           M0=sOS*min(p_qro*G2o(BoxnumberXY),K6r(BoxNumberXY))
       end select

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Get coefficients describing Nitrate in anoxic layer :
      ! 1. lambda of the exponential curve, and the denitrification rate
      ! 2. parameter of the denitrification term (integration constant)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      lambda = abs(GetInfoFromSet( KNO3(BoxNumberXY), GET, LABDA_1, 21))
      sK13G4 = GetInfoFromSet( KNO3(BoxNumberXY), GET, LABDA_2, 21) &
                                               * p_qro/ p_qon_dentri
      n21 = - GetInfoFromSet( KNO3(BoxNumberXY), GET, COEFFICIENT, 21)* sK13G4
      n22 = - GetInfoFromSet( KNO3(BoxNumberXY), GET, COEFFICIENT, 22)* sK13G4
      n31 = - GetInfoFromSet( KNO3(BoxNumberXY), GET, COEFFICIENT, 31)* sK13G4

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Initialize the set of differential equations giving:
      ! - n. of layers;
      ! - n. of coefficients
      ! - layers depths
      ! - environmental conditions (diffusion, porosity and adsorption coeff.)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      Dxm=(D1m(BoxNumberXY)+D2m(BoxNumberXY)) *0.5
      Dym=(D2m(BoxNumberXY)+p_d_tot) *0.5

      select case (p_flux_at_deep_end)
        case (1)     ; KRED(BoxNumberXY) = InitializeSet( KRED(BoxNumberXY), 5, 15)
        case default ; KRED(BoxNumberXY) = InitializeSet( KRED(BoxNumberXY), 5, 16)
      end select

      call DefineSet(KRED(BoxNumberXY), LAYERS,LAYER1,LAYER2, D1m(BoxNumberXY),Dxm)
      call DefineSet(KRED(BoxNumberXY), LAYERS,LAYER3,LAYER4, D2m(BoxNumberXY),Dym)

      call DefineSet(KRED(BoxNumberXY), DIFFUSION, FOR_ALL_LAYERS, 0, diff, dummy)
      call DefineSet(KRED(BoxNumberXY), POROSITY, FOR_ALL_LAYERS, 0, &
        p_poro(BoxNumberXY), dummy)

      call DefineSet(KRED(BoxNumberXY), ADSORPTION, FOR_ALL_LAYERS, 0, p_p, dummy)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Define coefficients for the steady-state solutions in each layer
      ! General solution of the equilibrium profile:
      ! 1st layer:
      ! R(z) = r11*exp(gamma*z) + r12*exp(-gamma*z)
      ! 2nd layer:
      ! R(z) = r21*exp[-alpha*(z-D1.m)] + r22*exp(-lambda*z) + r23*z^2 + r24*z + &
      ! r25
      !    r23, r24 = 0 (boundary condition)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      select case  (sw_set )
      case (0)
         call DefineSet(KRED(BoxNumberXY), DEFINE, 13, QUADRATIC_TERM, dummy, dummy)
         call DefineSet(KRED(BoxNumberXY), DEFINE, 14, LINEAR_TERM, dummy, dummy)
         call DefineSet(KRED(BoxNumberXY), DEFINE, 15, CONSTANT_TERM, dummy, dummy)
      case (1)
         call DefineSet(KRED(BoxNumberXY), DOUBLE_DEFINE, 11, EXPONENTIAL_TERM, -gamma, sOS)
         call DefineSet(KRED(BoxNumberXY), DOUBLE_DEFINE, 12, EXPONENTIAL_TERM, gamma, sOS)
         call DefineSet(KRED(BoxNumberXY), DOUBLE_DEFINE, 15, CONSTANT_TERM, dummy, dummy)
      end select

      call DefineSet(KRED(BoxNumberXY), DEFINE, 21, ZERO_EXPONENTIAL_TERM,-alpha, dummy)
      call DefineSet(KRED(BoxNumberXY), DOUBLE_DEFINE, 22, &
                                           ZERO_EXPONENTIAL_TERM, -lambda, sK13G4)
      call DefineSet(KRED(BoxNumberXY), DOUBLE_DEFINE, 23, &
                                           ZERO_EXPONENTIAL_TERM, +lambda, sK13G4)
      call DefineSet(KRED(BoxNumberXY), DEFINE, 25, CONSTANT_TERM, dummy, dummy)

      call DefineSet(KRED(BoxNumberXY), DEFINE, 31, ZERO_EXPONENTIAL_TERM, - alpha, dummy)
      call DefineSet(KRED(BoxNumberXY), DOUBLE_DEFINE, 32, &
                                           ZERO_EXPONENTIAL_TERM, -lambda, sK13G4)
      call DefineSet(KRED(BoxNumberXY), DOUBLE_DEFINE, 33, &
                                           ZERO_EXPONENTIAL_TERM, +lambda, sK13G4)
      call DefineSet(KRED(BoxNumberXY), DEFINE, 35, CONSTANT_TERM, dummy, dummy)

      call DefineSet(KRED(BoxNumberXY), DEFINE, 41, ZERO_EXPONENTIAL_TERM,-alpha, dummy)
      call DefineSet(KRED(BoxNumberXY), DEFINE, 44, LINEAR_TERM, dummy, dummy)
      call DefineSet(KRED(BoxNumberXY), DEFINE, 45, CONSTANT_TERM, dummy, dummy)

      if (p_flux_at_deep_end > 1 ) &
          call DefineSet(KRED(BoxNumberXY), DEFINE, 51, ZERO_EXPONENTIAL_TERM,-alpha, dummy)
      call DefineSet(KRED(BoxNumberXY), DEFINE, 55, CONSTANT_TERM, dummy, dummy)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Insert other boundary conditions and continuity between layers:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      !1-6:
      call CompleteSet( KRED(BoxNumberXY), SET_CONTINUITY, FLAG, MASS, dummy)

      !7:
      call CompleteSet( KRED(BoxNumberXY), SET_BOUNDARY, LAYER1, &
        EQUATION, ZERO, value=N6r_Ben(BoxNumberXY))

      !8-9:
      if (p_flux_at_deep_end > 1 ) &
         call FixProportionCoeff(KRED(BoxNumberXY),22,32,n21,n31)
      call FixProportionCoeff(KRED(BoxNumberXY),22,23,n21,n22)

      select case (InitializeModel)
        !10 -11
        case(0)
          call CompleteSet(KRED(BoxNumberXY), SET_LAYER_INTEGRAL, &
                                   LAYER2, LAYER3,dummy ,     K16r(BoxNumberXY))
          call CompleteSet(KRED(BoxNumberXY), SET_LAYER_INTEGRAL_UNTIL, &
                                   LAYER4, LAYER5, p_d_tot_2, K26r(BoxNumberXY))
        case(1)
           call CompleteSet( KRED(BoxNumberXY), INPUT_TERM, 31, PARAMETER, &
                dummy, value=zuD1*exp(-alpha *(Dxm-D1m(BoxNumberXY))))
           call CompleteSet( KRED(BoxNumberXY), INPUT_TERM, 41, PARAMETER, &
                dummy, value=zuD1*exp(-alpha * (D2m(BoxNumberXY)-D1m(BoxNumberXY))))
      end select

      !12
      call FixProportionCoeff(KRED(BoxNumberXY),21,31, &
                                   1.0D+00,exp(-alpha * (Dxm-D1m(BoxNumberXY))))
      call CompleteSet( KRED(BoxNumberXY), INPUT_TERM, 21, PARAMETER, dummy, max(1.0D-4,zuD1))

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
     ! Calculate for the above defined set of boundary conditions
     ! the steady-state profiles and return the vertically integrated
     ! concentration.
     !
     ! Technical improvements: in case of utlimate low mineralization rates and
     ! a nearly empty reduction equivalent pool. There is a chance the
     ! estimated equilibrium value is negative. Therfore cK6r is limited to &
     ! values >=0
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

     if ( InitializeModel== 0) then
         !13
         select case (sw_set)
           case(0)
               ! max. 0 order input available
               call CompleteSet( KRED(BoxNumberXY), INPUT_TERM, 13, &
                             PARAMETER, dummy, -sOS*K6r(BoxNumberXY))
           case(1)
               ! max. 0 order input available
               call CompleteSet( KRED(BoxNumberXY), INPUT_TERM, 15, &
                             STANDARD, dummy,M0/sOS)
         end select

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! Calculate the adaptation time to the steady-state profile
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          cK6r = max( ZERO, CalculateSet( KRED(BoxNumberXY), &
                 SET_LAYER_INTEGRAL, LAYER1, LAYER1, ZERO , ZERO))
          cK6r= max(N6r_Ben(BoxNumberXY)*p_poro(BoxNumberXY)*D1m(BoxNumberXY),&
                           min(cK6r,K16r(BoxNumberXY)*D1m(BoxNumberXY)/(D2m(BoxNumberXY)-D1m(BoxNumberXY))))

          Tau  =   CalculateTau(  sOS,  diff,  p_p,  D1m(BoxNumberXY))

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! Estimate the average value of K6r over the actual time step
         ! (transient value).
         ! This value depends on the adaptation time, the actual time step,
         ! the ''old'' value and the ''equilibrium value''
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         cK6r = cK6r+( K6r(BoxNumberXY)- cK6r)* IntegralExp( -LocalDelta/Tau, 1.0D+00)

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! Derive the equations for the transient profiles, assuming the same
         ! solution as for the steady-state case and using cK6r as new &
         ! constraint.
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          dummy = CalculateSet( KRED(BoxNumberXY), ADD, 0, 0, dummy, cK6r)

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! flux at  sediment/water interface
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         jbotN6r(BoxNumberXY) = CalculateFromSet( KRED(BoxNumberXY), DERIVATIVE, &
               RFLUX, ZERO, dummy)

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         !  processes affecting K6r
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         jK6BTr=sOS *K6r(BoxNumberXY)

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! flux at the D1m
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         jK16K6r= CalculateFromSet( KRED(BoxNumberXY), DERIVATIVE, RFLUX, D1m(BoxNumberXY), dummy)

         ! Calculate mass shifted in upwards direction:
         Dnew  =   D1m(BoxNumberXY)+ LocalDelta* shiftD1m(BoxNumberXY)
         jK16K6r = jK16K6r+  CalculateFromSet( KRED(BoxNumberXY), SHIFT, LAYER1, & 
                   D1m(BoxNumberXY), Dnew)/ LocalDelta

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         !  processes affecting K16r
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         jATK26r = zuD1*exp( -alpha * ( D2m(BoxNumberXY)-D1m(BoxNumberXY))) &
               * IntegralExp( -alpha, p_d_tot- D2m(BoxNumberXY)) * p_poro(BoxNumberXY)

         ! loss flux due to denitrification (fixed rate, calculate elsewhere)
         jK6G4r= p_qro/ p_qon_dentri* jK3G4n(BoxNumberXY)


         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! flux at the D2m
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         ! Calculate mass shifted in upwards direction:
         Dnew  =   D2m(BoxNumberXY)+ LocalDelta* shiftD2m(BoxNumberXY)
         jK26K16r = CalculateFromSet( KRED(BoxNumberXY), SHIFT, LAYER3, &
                                 D2m(BoxNumberXY), Dnew)/ LocalDelta    &
                +CalculateFromSet( KRED(BoxNumberXY), DERIVATIVE, RFLUX, D2m(BoxNumberXY), dummy)

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         !  processes affecting K26r
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         ! fixed rate ( calculated elsewhere)
         jK26G4r=CalculateFromSet( KNO3(BoxNumberXY), INTEGRAL, &
                      RFLUX, D2m(BoxNumberXY),p_d_tot_2 )* sK13G4 !*limit_flux
        
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! flux at underside
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         select case (p_flux_at_deep_end)  ! 1=no flux, 2= only fluxes_downwars (sink), 3,=full_flux
          case (1); s=  0.0
          case (2); s = min(0.0,CalculateFromSet(KRED(BoxNumberXY), DERIVATIVE, RFLUX, p_d_tot_2, dummy))
          case (3); s = CalculateFromSet(KRED(BoxNumberXY), DERIVATIVE, RFLUX, p_d_tot_2, dummy)
        end select
        jK36K26r(BoxNumberXY)=s; 


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! set limits
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         ! limit for change in K26r
         r=K26r(BoxNumberXY)+(jATK26r-jK26G4r) *LocalDelta
         s=jK36K26r(BoxNumberXY)-jK26K16r
         call LimitChange(1,s,r ,      max_change_per_step,limit_rate_3)

         ! limit for change in K16r
         r=K16r(BoxNumberXY)+(jATK6r-jK6G4r-jATK26r+ &
             jK26K16r*insw(jK26K16r)*limit_rate_3)*LocalDelta
         s= -jK16K6r*insw(jK16K6r)+jK26K16r*insw(-jK26K16r)
         call LimitChange(2,s,r ,           max_change_per_step,limit_rate_2)


 !       call LimitChange(1,jK16K6r,K6r(BoxNumberXY) ,      max_change_per_step,limit_rate_a)
         ! limit for change in K6r
         s=-jbotN6r(BoxNumberXY)-jK6BTr+jK16K6r*insw(-jK16K6r)
         r=K6r(BoxNumberXY)+jK16K6r*insw(jK16K6r)*limit_rate_2*LocalDelta
         call LimitChange(2,s,r ,                   max_change_per_step,limit_rate_1)

         ! limit for change in N6r
         r= N6r_Ben(BoxNumberXY)*Depth_Ben(BoxNumberXY) 
         call LimitChange(2,jbotN6r(BoxNumberXY),r ,             max_change_per_step,limit_rate_0)

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! calculate limitation  and correct the fluxes
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         jbotN6r(BoxNumberXY) =jbotN6r(BoxNumberXY) *min(limit_rate_0,limit_rate_1) 
         jK6BTr               =jK6BTr     *limit_rate_1;
         jK16K6r              =jK16K6r    *(limit_rate_2 *insw(jK16K6r) + limit_rate_1 *insw(-jK16K6r))
         jK26K16r             =jK26K16r   *min(limit_rate_2 , limit_rate_3 )
         jK36K26r(BoxNumberXY)=jK36K26r(BoxNumberXY)*limit_rate_3;

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! set fluxes
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         call flux(BoxNumberXY, iiBen, ppK6r, ppK6r,  -jbotN6r(BoxNumberXY)*limit_rate_1 )
         call flux(BoxNumberXY, iiBen, ppK6r, ppK6r,  -jK6BTr*limit_rate_1 )
         jbotN6r(BoxNumberXY)=jbotN6r(BoxNumberXY)+jK6BTr* ( 1.0D+00-limit_rate_1)
         jbotO2o(BoxNumberXY)=jbotO2o(BoxNumberXY)+jK6BTr* ( 1.0D+00-limit_rate_1)/p_qro
         jG2K7o(BoxNumberXY)  =   jK6BTr/ p_qro
         r  =   jK6BTr*limit_rate_1/ p_qro
         call flux(BoxNumberXY, iiBen, ppG2o, ppG2o,r )
         call flux(BoxNumberXY, iiBen, ppK16r, ppK16r, -jK6G4r )

         call flux(BoxNumberXY, iiBen, ppK26r, ppK26r, -jK26G4r*limit_rate_3 )
         call flux(BoxNumberXY, iiBen, ppK16r, ppK16r, -jK26G4r*(1.0D+00-limit_rate_3 ))
         call flux(BoxNumberXY, iiBen, ppK26r, ppK26r, jK36K26r(BoxNumberXY))

         call flux(BoxNumberXY, iiBen, ppK16r, ppK6r,   jK16K6r* insw(  jK16K6r) )
         call flux(BoxNumberXY, iiBen, ppK6r,  ppK16r,- jK16K6r* insw( -jK16K6r) )

         jK26K16r = jK26K16r -jATK26r
         call flux(BoxNumberXY, iiBen, ppK26r, ppK16r,   jK26K16r* insw(  jK26K16r) )
         call flux(BoxNumberXY, iiBen, ppK16r, ppK26r, - jK26K16r* insw( -jK26K16r) )
    else
         call CompleteSet( KRED(BoxNumberXY), INPUT_TERM, 21, PARAMETER, &
                dummy, value=zuD1)

          dummy = CalculateSet( KRED(BoxNumberXY), 0, 0,  0, dummy, dummy)

          jK6BTr = sOS* CalculateFromSet( KRED(BoxNumberXY), INTEGRAL, &
                                         RFLUX, ZERO, D1m(BoxNumberXY))
          jG2K7o(BoxNumberXY)  =   jK6BTr/ p_qro
      endif

  end do

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
