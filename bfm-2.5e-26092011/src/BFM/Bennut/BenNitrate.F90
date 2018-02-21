#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenNitrate
!
! DESCRIPTION
!   Description of the diagenetic nitrate processes in the sediment
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
  subroutine BenNitrateDynamics
!
! !USES:

  ! For the following Benthic-states fluxes are defined: K3n, G4n
  ! The following Benthic-states are used (NOT in fluxes): D2m, D6m, D1m, K16r
  ! The following global vars are modified: dummy
  ! The following global scalar vars are used: &
  !    NO_BOXES_XY,  &
  !   BoxNumberXY, InitializeModel, LocalDelta
  ! The following Benthic 1-d global boxvars are modified : KNO3, jbotN3n
  ! The following Benthic 1-d global boxvars got a value: M3n,jK3G4n
  ! The following Benthic 1-d global boxvars are used: rrATo, irrenh, ETW_Ben, &
  ! KNH4, N3n_Ben,Depth_Ben
  ! The following Benthic 1-d global boxpars  are used: p_poro
  ! The following 0-d global parameters are used: p_d_tot, p_q10diff, &
  ! p_qro, p_qon_dentri
  ! The following global constants are used: RLEN,ZERO
  ! The following constants are used: GET, &
  ! LABDA_1, LABDA_2, COEFFICIENT, LAYERS, LAYER1, &
  ! DIFFUSION, FOR_ALL_LAYERS, POROSITY, ADSORPTION, DOUBLE_DEFINE, &
  ! ZERO_EXPONENTIAL_TERM, DEFINE, QUADRATIC_TERM, LINEAR_TERM, CONSTANT_TERM, &
  ! EXPONENTIAL_TERM, SET_CONTINUITY, FLAG, MASS, SET_BOUNDARY, &
  ! EQUATION, INPUT_TERM, PARAMETER, START_ADD_TERM, STANDARD, &
  ! SET_LAYER_INTEGRAL_UNTIL, LAYER2, ADD, DERIVATIVE, RFLUX, INTEGRAL

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,ZERO
  use mem,  ONLY: K3n, D2m, D6m, D1m, K16r,K26r, D2STATE
  use mem, ONLY: ppK3n, ppG4n, sK4K3, &
    dummy,    NO_BOXES_XY, LocalDelta,max_change_per_step,   &
     BoxNumberXY, InitializeModel, KNO3, KNO3E, jbotN3n, M3n, jK3G4n,&
    jK4K3n, rrATo, irrenh, ETW_Ben, KNH4, N3n_Ben, iiBen, flux,Depth_Ben
  use constants, ONLY: GET, LABDA_1, &
    COEFFICIENT, LAYERS, LAYER1, DIFFUSION, FOR_ALL_LAYERS, POROSITY, &
    ADSORPTION, DOUBLE_DEFINE, ZERO_EXPONENTIAL_TERM, DEFINE, QUADRATIC_TERM, &
    LINEAR_TERM, CONSTANT_TERM, EXPONENTIAL_TERM, SET_CONTINUITY, FLAG, MASS, &
    SET_BOUNDARY, EQUATION, INPUT_TERM, PARAMETER, &
    SET_LAYER_INTEGRAL_UNTIL, LAYER2, ADD, DERIVATIVE, RFLUX,&
    INTEGRAL, MIN_VAL_EXPFUN,LAYER3
  use mem_Param,  ONLY: p_poro, p_d_tot, p_q10diff, p_qro, p_qon_dentri
  use mem_BenNitrate
  use mem_BenAmmonium,only:p_slK4K3
  use LimitRates, ONLY:LimitChange

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:GetInfoFromSet, &
  ! InitializeSet, DefineSet, CompleteSet, CalculateSet, CalculateTau, &
  ! CalculateFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use bennut_interface, ONLY: GetInfoFromSet, InitializeSet, DefineSet, &
    CompleteSet, CalculateSet, CalculateTau, CalculateFromSet,CopySet

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following global functions are used:eTq
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use global_interface,   ONLY: eTq

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following sesame functions are used:IntegralExp
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: IntegralExp

!  
!
! !AUTHORS
!   Original version by  P. Ruardij
!
! !REVISION_HISTORY
!   September 1999 by M. Vichi  Commented version 
!
!
! COPYING
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
  real(RLEN)  :: Dxm
  real(RLEN)  :: sK13G4
  real(RLEN)  :: diff
  real(RLEN)  :: gamma
  real(RLEN)  :: labda
  real(RLEN)  :: a11
  real(RLEN)  :: a12
  real(RLEN)  :: a15
  real(RLEN)  :: n12
  real(RLEN)  :: cK3n
  real(RLEN)  :: Tau
  real(RLEN)  :: zATo
  real(RLEN)  :: alpha
  real(RLEN)  :: s
  real(RLEN)  :: r
  real(RLEN)  :: limit_rate_0,limit_rate_1,limit_rate_r,limit_rate_n

  real(RLEN), external  :: GetDelta
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do BoxNumberXY=1,NO_BOXES_XY

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate the pore-water average concentrations from the state variables
      ! (Diagnostic variables, not used in calculations)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      M3n(BoxNumberXY) = K3n(BoxNumberXY)/ p_poro(BoxNumberXY)/( p_p+ &
        1.0D+00)/( D2m(BoxNumberXY))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate coefficient for the e-folding distribution of the anoxic
      ! mineralization. D6.m is the average penetration depth for C-detritus
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      alpha  =   1.0D+00/ D6m(BoxNumberXY)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Recalculate Mineralization m2 --> m3 porewater
      ! Calculate the total anoxic mineralization in mmol O/m3/d
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      zATo = rrATo(BoxNumberXY)/ p_poro(BoxNumberXY)/ IntegralExp( &
        -alpha, p_d_tot- D1m(BoxNumberXY))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Correction due to environmental regulating factors,
      ! diffusion coefficient: temperature and bioirrigation
      ! denitrification: temperature and coupling with anoxic mineralization
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      diff = p_diff* irrenh(BoxNumberXY)* eTq( ETW_Ben(BoxNumberXY), p_q10diff)

      r=K3n(BoxNumberXY)+jK4K3n(BoxNumberXY)
!     if (isnan(r) ) write(LOGUNIT,*) 's=',r
!     if (isnan(jK4K3n(BoxNumberXY)) ) write(LOGUNIT,*) 'jK4K3n(BoxNumberXY)=',jK4K3n(BoxNumberXY)
      if (p_cK3G4<=ZERO ) then
        !  old_ formulation:
        !  There are  temp.correction: 
        !  1.normal temp. coorection.
        !  2.when compare zATo is comapre with z_aAT0 at ref.temperature.
        ! sK13G4  =   p_s1K3G4* eTq(  ETW_Ben(BoxNumberXY),  p_q10) * zATo/eTq(  ETW_Ben(BoxNumberXY),  p_q10)/ p_zATo
        ! hence simplify:
        sK13G4  =   p_sK3G4* zATo/ p_zATo
      else
        ! new formulation
        ! The old formulation assumed implicitly that a higher first order nitrifiation rate sK13G4
        ! should to a higher N-loss due to denitrification.
        ! This is not true because the Nlass per m2 ( = integral of first order process over depth) 
        ! proportional to 1/sK13G4. 
        ! We define now a proportion of the K3n which is (potentially) available for denitrification.
        ! The estimated flux can differ because it will depend on the estimation of the acutal gradient 
        sK13G4  =rrATo(BoxNumberXY)/(p_cK3G4*r/p_qon_dentri)
      endif
      !limit denitrification at low concentration of K3n
      s=sK13G4*r
      call LimitChange(1,s,r,max_change_per_step ,limit_rate_n)
      !limit denitrification at low concentration of K.6r
      r=(K16r(BoxNumberXY)+K26r(BoxNumberXY) +rrATo(BoxNumberXY)/p_qro)/p_qon_dentri
      call LimitChange(1,s,r,max_change_per_step ,limit_rate_r)
      sK13G4=min(limit_rate_n,limit_rate_r)*sK13G4
      if (isnan(sK13G4) ) then
          write(LOGUNIT,*) ' BenNitrate r=',r
          write(LOGUNIT,*) 'K16r(BoxNumberXY)=',K16r(BoxNumberXY)
          write(LOGUNIT,*) 'K26r(BoxNumberXY)=',K26r(BoxNumberXY)
          write(LOGUNIT,*) 'rrAto(BoxNumberXY)=',rrAto(BoxNumberXY)
          write(LOGUNIT,*) 'jK4K3n(BoxNumberXY)=',jK4K3n(BoxNumberXY)
          write(LOGUNIT,*) 'K3n(BoxNumberXY)=',K3n(BoxNumberXY)
      endif
      if (isnan(s) ) write(LOGUNIT,*) 'BenNitrate s=',s

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate coefficient for the exponential terms of the solution
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      sK13G4=min(p_shK3G4,sK13G4)
      gamma  = max(MIN_VAL_EXPFUN,  sqrt( max(p_slK3G4, sK13G4)/ diff))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Get coefficients describing ammonium in the oxic layer :
      ! 1. labda of the exponential curve
      ! 2. parameter of the nitrification term
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      labda = GetInfoFromSet( KNH4(BoxNumberXY), GET, LABDA_1, 11)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Get coefficients of all terms of equation valid for the
      ! first layer of ammonium (integration constants)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      a11 = GetInfoFromSet( KNH4(BoxNumberXY), GET, COEFFICIENT, 11)
      a12 = GetInfoFromSet( KNH4(BoxNumberXY), GET, COEFFICIENT, 12)
      a15 = GetInfoFromSet( KNH4(BoxNumberXY), GET, COEFFICIENT, 15)
      n12  =   max(p_slK4K3, sK4K3(BoxNumberXY))* a12

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Initialize the set of differential equations giving:
      ! - n. of layers;
      ! - n. of coefficients
      ! - layer depths
      ! - environmental conditions (diffusion, porosity and adsorption coeff.)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   
      Dxm=(D1m(BoxNumberXY)+D2m(BoxNumberXY) ) * 0.5
      KNO3(BoxNumberXY) = InitializeSet( KNO3(BoxNumberXY), 3, 8)
      call DefineSet(KNO3(BoxNumberXY), LAYERS, LAYER1, LAYER2, &
        D1m(BoxNumberXY), Dxm)

      call DefineSet(KNO3(BoxNumberXY), DIFFUSION, FOR_ALL_LAYERS, 0, diff, &
        dummy)

      call DefineSet(KNO3(BoxNumberXY), POROSITY, FOR_ALL_LAYERS, 0, &
        p_poro(BoxNumberXY), dummy)

      call DefineSet( KNO3(BoxNumberXY), ADSORPTION, FOR_ALL_LAYERS, 0, p_p, &
        dummy)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Define coefficients for the steady-state solutions in each layer
      ! General solution of the equilibrium profile:
      ! 1st layer:
      ! N(z) = n11*exp(labda*z) + n12*exp(-labda*z) + n13*z^2 + n14*z + n15
      ! 2nd layer:
      ! N(z) = n21*exp(gamma*z) + n22*exp(-gamma*z)
      !    n22 = 0 (boundary condition)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      call DefineSet( KNO3(BoxNumberXY), DOUBLE_DEFINE, 11, &
        ZERO_EXPONENTIAL_TERM, labda, sK4K3(BoxNumberXY))

      call DefineSet( KNO3(BoxNumberXY), DOUBLE_DEFINE, 12, &
        ZERO_EXPONENTIAL_TERM, - labda, sK4K3(BoxNumberXY))

      call DefineSet( KNO3(BoxNumberXY), DEFINE, 13, QUADRATIC_TERM, dummy, &
        dummy)

      call DefineSet(KNO3(BoxNumberXY), DEFINE, 14, LINEAR_TERM, dummy,dummy)
      call DefineSet(KNO3(BoxNumberXY), DEFINE, 15, CONSTANT_TERM, dummy,dummy)

      call DefineSet( KNO3(BoxNumberXY), DOUBLE_DEFINE, 21, EXPONENTIAL_TERM, - &
        gamma, sK13G4)

      call DefineSet( KNO3(BoxNumberXY), DOUBLE_DEFINE, 22, EXPONENTIAL_TERM, &
        gamma, sK13G4)

      call DefineSet( KNO3(BoxNumberXY), DOUBLE_DEFINE, 31, EXPONENTIAL_TERM, - &
        gamma, sK13G4)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Insert other boundary conditions and continuity between layers:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      call CompleteSet( KNO3(BoxNumberXY), SET_CONTINUITY, FLAG, MASS, dummy)

      call CompleteSet( KNO3(BoxNumberXY), SET_BOUNDARY, LAYER1, &
        EQUATION, ZERO, value=N3n_Ben(BoxNumberXY))


      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! a11 / (labda * labda * diff) = a12 / (labda * labda * diff)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      call FixProportionCoeff(KNO3(BoxNumberXY),12,11,a12,a11)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! a11 / (labda * labda * diff) = a15 / (2 * diff)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      call FixProportionCoeff(KNO3(BoxNumberXY),11,13,a11,a15)

      if ( InitializeModel== 0) then

        call CompleteSet( KNO3(BoxNumberXY), INPUT_TERM, 12, PARAMETER, dummy, &
           value=n12)


      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate for the above defined set of boundary conditions
      ! the steady-state profiles and return the vertically integrated
      ! concentration
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        cK3n = CalculateSet( KNO3(BoxNumberXY), SET_LAYER_INTEGRAL_UNTIL, &
          LAYER1, LAYER3, D2m(BoxNumberXY), ZERO)
        KNO3E(BoxNumberXY) = CopySet( KNO3(BoxNumberXY), KNO3E(BoxNumberXY))


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Calculate the adaptation time to the steady-state profile
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        Tau  =   CalculateTau(  sK13G4,  diff,  p_p,  D2m(BoxNumberXY))

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Estimate the average value of K3n over the actual time step
        ! (transient value).
        ! This value depends on the adaptation time, the actual time step,
        ! the ''old'' value and the ''equilibrium value''
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        cK3n = cK3n+( K3n(BoxNumberXY)- cK3n)* IntegralExp( - LocalDelta/ &
          Tau, 1.0D+00)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Derive the equations for the transient profiles, assuming the same
        ! solution as for the steady-state case and using cK3n as new &
        ! constraint.
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        dummy = CalculateSet( KNO3(BoxNumberXY), ADD, 0, 0, dummy, &
          cK3n)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Start calculation of fluxes:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Denitrification:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jK3G4n(BoxNumberXY) = max(0.0,CalculateFromSet( KNO3(BoxNumberXY), INTEGRAL, &
          RFLUX, D1m(BoxNumberXY), D2m(BoxNumberXY)))* sK13G4 !*limit_flux
        if (jK3G4n(BoxNumberXY) > ZERO ) then
           r= min(K3n(BoxNumberXY)+jK4K3n(BoxNumberXY)*LocalDelta, K16r(BoxNumberXY)/p_qro* p_qon_dentri)
           call LimitChange(1,jK3G4n(BoxNumberXY),r,max_change_per_step,limit_rate_r)
        else
           call PrintSet(KNO3(BoxNumberXY),"Negative (or Nan) flux for nitrate")
           write(LOGUNIT,'(''D1m='',F10.3)') D1m(BoxNumberXY)
           write(LOGUNIT,'(''D2m='',F10.3)') D2m(BoxNumberXY)
           write(LOGUNIT,'(''nitrate/m2 (K3n)='',F10.3)') K3n(BoxNumberXY)
        endif

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Flux at the water/sediment interface
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jbotN3n(BoxNumberXY) = CalculateFromSet( KNO3(BoxNumberXY), DERIVATIVE, &
          RFLUX, ZERO, dummy) 

        !Check on negative flux: flux from water into sediment
        call LimitChange(2,jbotN3n(BoxNumberXY),N3n_Ben(BoxNumberXY)*Depth_Ben(BoxNumberXY) ,&
                                                         max_change_per_step,limit_rate_0)
        !Test the som of 2 ( mostly) positive fluxes.
        r=jK3G4n(BoxNumberXY)+jbotN3n(BoxNumberXY)
        call LimitChange(3,r,K3n(BoxNumberXY)+jK4K3n(BoxNumberXY)*LocalDelta, &
                                                        max_change_per_step,limit_rate_1)
        jbotN3n(BoxNumberXY)=jbotN3n(BoxNumberXY)* min(limit_rate_0,limit_rate_1)
        jK3G4n(BoxNumberXY) =jK3G4n(BoxNumberXY)* min(limit_rate_1,limit_rate_r)
        call flux(BoxNumberXY, iiBen, ppK3n, ppK3n, - jbotN3n(BoxNumberXY) )
        call flux(BoxNumberXY, iiBen, ppK3n, ppG4n, jK3G4n(BoxNumberXY) )

      else
        call CompleteSet( KNO3(BoxNumberXY), INPUT_TERM, 12, PARAMETER, dummy, &
           value=n12)
        dummy = CalculateSet( KNO3(BoxNumberXY), 0, 0,  0, dummy, dummy)
      end if

  end do

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
