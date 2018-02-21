#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenSilica
!
! DESCRIPTION
!   Description of the diagenitic processes in the sediment
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
  subroutine BenSilicaDynamics
!
! !USES:

  ! For the following Benthic-states fluxes are defined: K5s, Q6s, D9m
  ! The following Benthic-states are used (NOT in fluxes): D1m, D2m
  ! The following global vars are modified: dummy
  ! The following global scalar vars are used: &
  !    NO_BOXES_XY,   &
  !  BoxNumberXY, idummy, InitializeModel, LocalDelta
  ! The following Benthic 1-d global boxvars are modified : M5s, KSiO3, KSiO3E, &
  ! jK25K15s, jbotN5s
  ! The following Benthic 1-d global boxvars are used: irrenh, ETW_Ben, &
  ! N5s_Ben, shiftD2m
  ! The following Benthic 1-d global boxpars  are used: p_poro
  ! The following 0-d global parameters are used: p_clD1D2m, p_q10diff, &
  ! p_clDxm, p_d_tot
  ! The following global constants are used: RLEN
  ! The following constants are used: LAYERS, &
  ! LAYER1, DIFFUSION, FOR_ALL_LAYERS, POROSITY, ADSORPTION, &
  ! DEFINE, CONSTANT_TERM, PARAMETER_DEFINE, &
  ! BESSELI_EXP_TERM, SET_CONTINUITY, STANDARD, SET_BOUNDARY, EQUATION, &
  ! INPUT_TERM, SET_LAYER_INTEGRAL_UNTIL, LAYER2, ADD, INTEGRAL, &
  ! DERIVATIVE, RFLUX, MASS, EXPONENTIAL_INTEGRAL

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,ZERO
  use mem,  ONLY: K5s, K15s,  Q6s, D9m, D1m, D2m, D2STATE
  use mem, ONLY: ppK5s,ppK15s, ppQ6s, ppD9m, &
    dummy,    NO_BOXES_XY, LocalDelta,max_change_per_step,   &
     BoxNumberXY, InitializeModel, M5s, KSiO3, Depth_Ben, &
    KSiO3E, jbotN5s, jK25K15s, irrenh, ETW_Ben, N5s_Ben, shiftD2m, iiBen, flux
  use constants, ONLY: LAYER1,LAYER2,LAYER3,LAYER4, SHIFT, STANDARD, &
    EQUATION, SET_LAYER_INTEGRAL, &
    LAYER2, ADD, INTEGRAL, DERIVATIVE, RFLUX, EXPONENTIAL_INTEGRAL
  use mem_Param,  ONLY: p_poro, p_clD1D2m, p_q10diff, p_clDxm, p_d_tot,p_d_tot_2,p_small
  use mem_BenSilica
  use mem_BenthicNutrient3,ONLY:p_pAn2Ni
  use LimitRates, ONLY:LimitShift,LimitChange
  USE BFM_ERROR_MSG, ONLY: set_warning_for_getm


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:InitializeSet, &
  ! DefineSet, CompleteSet, CalculateSet, CalculateTau, CopySet, &
  ! CalculateFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use bennut_interface, ONLY: CalculateSet, CalculateTau, CopySet, CalculateFromSet


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
!       September 1999 by M. Vichi     Commented version
!
! COPYING
!   
!   Copyright (C) 2006 P. Ruardij & M. Vichi
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
  integer     :: i,mode
  real(RLEN)  :: h
  real(RLEN)  :: r
  real(RLEN)  :: s
  real(RLEN)  :: cD1m
  real(RLEN)  :: cD2m
  real(RLEN)  :: cD2mnew
  real(RLEN)  :: cShiftD2m
  real(RLEN)  :: chM5s
  real(RLEN)  :: cM5s
  real(RLEN)  :: Tau
  real(RLEN)  :: alpha
  real(RLEN)  :: diff
  real(RLEN)  :: diff_deep
  real(RLEN)  :: M5b0
  real(RLEN)  :: M5b_0_d1
  real(RLEN)  :: M5bD1
  real(RLEN)  :: zuBT
  real(RLEN)  :: suD1
  real(RLEN)  :: suD2
  real(RLEN)  :: suDn
  real(RLEN)  :: shiftmass
  real(RLEN)  :: mjQ6K5s    !minus total flux of Q6 to K5
  real(RLEN)  :: mjQ6K5s_1  !minus       flux of Q6 to K5 in first layer
  real(RLEN)  :: mjQ6K15s   !minus total flux of Q6 to K5
  real(RLEN)  :: mjK15K5s
  real(RLEN)  :: jK15K5s
  real(RLEN)  :: smQ6
  real(RLEN)  :: smQ6_l1
  real(RLEN)  :: sM5s
  real(RLEN)  :: lambda
  real(RLEN)  :: M0
  real(RLEN)  :: R5s
  real(RLEN)  :: R15s
  real(RLEN)  :: tom3_K5s
  real(RLEN)  :: tom3_ae
  real(RLEN)  :: tom3_an
  real(RLEN)  :: mM5s       ! =chM5s- M5s
  real(RLEN)  :: pShift,mShift,lShift,dn       

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do BoxNumberXY=1,NO_BOXES_XY

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Correction due to environmental regulating factors,
      ! saturation value: temperature
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      chM5s = p_chM5s+ p_cvM5s*( eTq( ETW_Ben(BoxNumberXY), p_q10)- 1.0D+00)

      cD2m  =   min(  max(  D2m(BoxNumberXY),   p_clD2m),  p_chD2m)
      cD1m  =   min(  max(  D1m(BoxNumberXY),   p_clD1m),  cD2m- p_clD1D2m)
      dn=    (1.0-p_pAn2Ni) * cD2m +p_pAn2Ni*p_d_tot

      tom3_ae=1.0D+00 / ( 1.0D+00+p_p_ae) / p_poro(BoxNumberXY)
      tom3_an=1.0D+00 / ( 1.0D+00+p_p_an) / p_poro(BoxNumberXY)
      tom3_K5s= (tom3_ae*cD2m +tom3_an*(dn-cD2m))/dn

      if ( InitializeModel== 1) then
         K5s(BoxNumberXY)= 0.1* (chM5s)*cD2m/tom3_ae 
         K15s(BoxNumberXY)=0.9*chM5s*(p_d_tot_2-cD2m)/tom3_an 
      endif
      if ( K5s(BoxNumberXY)<ZERO .or. K15s(BoxNumberXY) <ZERO ) then
         K5s(BoxNumberXY)=0.5D+00* chM5s/tom3_ae*dn             
         K15s(BoxNumberXY)=0.25+00* chM5s/tom3_an*(p_d_tot_2-dn)
         write(LOGUNIT,*) "K5s and K15s reset to normal values"
         call set_warning_for_getm();  
     endif

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Correction due to environmental regulating factors,
      ! diffusion coefficient: temperature and bioirrigation
      ! dissolution rate: temperature
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      diff = p_diff* irrenh(BoxNumberXY)* eTq( ETW_Ben(BoxNumberXY), p_q10diff)
      diff_deep = p_diff* eTq( ETW_Ben(BoxNumberXY), p_q10diff)
      smQ6  =   p_smQ6* eTq(  ETW_Ben(BoxNumberXY),  p_q10diff)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate coefficient for the e-folding distribution of the anoxic
      ! mineralization. D9.m is the average penetration depth for biogenic Si
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      alpha  =   1.0D+00/ min(max(  p_clDxm,  D9m(BoxNumberXY)),0.30D+00)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate total biogenic silica from m2 --> m3 porewater
      ! Avoid that Q6s decrease to ZERO: Subtract 2 of Q6s before doing any calcualtioS
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      M5b0 = max(ZERO,Q6s(BoxNumberXY)-2.0D+00)/ p_poro(BoxNumberXY)/ IntegralExp(- alpha, p_d_tot)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Average content of Biogenic silica in the oxic layer
      ! and calculation of the zero-order dissolution term
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      M5b_0_d1  =   M5b0* IntegralExp( - alpha,  cD1m)/ cD1m 
      zuBT  =   smQ6* M5b_0_d1*cD1m*p_poro(BoxNumberXY)
      if ( InitializeModel ==0 ) then
          mM5s=max(p_small,chM5s-K5s(BoxNumberXY)/dn *tom3_K5s)
          zuBT=zuBT *mM5s/chM5s 
      else
          mM5s=0.5 * chM5s
          zuBT=0.5* zuBT *mM5s/chM5s 
      endif

      R5s =chM5s*dn/tom3_K5s -K5s(BoxNumberXY)
      R15s=chM5s/tom3_an*(p_d_tot_2-dn)-K15s(BoxNumberXY)



      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Check if the amount of biogenic silicate is so large that within one step 
      ! the dissolution will reach  the equilibrium concentration and no feed back 
      ! is possible that the profile adapt its self to a new situation
      ! This is done by limiting the dissolution rates....
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      M5bD1  =  M5b0* exp( - alpha* cD1m)
      suD1  =   smQ6* M5bD1/ chM5s
      suD2  =   smQ6* M5bD1/ chM5s*exp(-alpha *(cD2m-cD1m))
      if (isnan(suD1)) then
          write(LOGUNIT,*) 'suD1=',suD1
          write(LOGUNIT,*) 'alpha=',alpha
          write(LOGUNIT,*) 'cD1m=',cD1m
          write(LOGUNIT,*) 'M5b0=',M5b0
          write(LOGUNIT,*) 'Q6s=',Q6s
      endif
      ! Make an a rough estimation about the the dissolution resolution from Q6s->K5s
      mjQ6K5s  =-zuBT- suD1* IntegralExp(-alpha,(cD2m-cD1m))*R5s/dn*tom3_K5s &
                     - suD2* IntegralExp(-alpha,(dn-cD2m))*R5s/dn*tom3_K5s 
      !Calculate the possible limitation   of the rate in s 
      r=-mjQ6K5s
      call LimitChange(1,mjQ6K5s,max(p_small,R5s),max_change_per_step,s)
      call LimitChange(1,r,K5s(BoxNumberXY),max_change_per_step,h)
      r=min(h,s)
      !Limit dissolution such that rate of dissolution is smaller than reciproke biomass
      zuBT=r*zuBT                    !correct dissolution per/m2 for change
!     mM15s=max(p_small,chM5s-K15s(BoxNumberXY)/(p_d_tot_2-dn) *tom3_an)
      smQ6_l1=r*smQ6*mM5s/chM5s      !calculate smq6 in layer 1 limited by (ChM5s-M5s)/chM5s
      M0=smQ6_l1 * M5b_0_d1          !correct dissolution rate mmol//m3  pore water
      sM5s= M0/mM5s                  !calculate dissolution per mol recM5s 
                                     !in order to describe dissolution as /desorption process 
      suDn  =   smQ6* M5bD1/ chM5s*exp(-alpha *(dn-cD1m))
      mjQ6K15s  =- suDn*IntegralExp(-alpha,(p_d_tot_2-dn))*R15s/(p_d_tot_2-dn)*tom3_an 
      call LimitChange(2,mjQ6K15s,max(p_small,R15s),max_change_per_step,s)
      suD1=r*suD1
      suD2=+r*suD2
      suDn=s*suDn

      lambda= sqrt(max(0.001D+00,sM5s)/diff)
      if (isnan(lambda)) then
         write(LOGUNIT,*)'Lambda is NAN'
         write(LOGUNIT,*)'mmM5s', mM5s
         write(LOGUNIT,*)'smQ6_l1', smQ6_l1
         write(LOGUNIT,*)'chM5s', chM5s
         write(LOGUNIT,*)'r', r
         write(LOGUNIT,*)'zuBt', zuBt
      endif

      if ( R5s.lt.-5.0D+00*K5s(BoxNumberXY)) then
           R5s=0.5*chM5s/tom3_K5s*dn
           Write(LOGUNIT,*) 'Value of K5s larger than max.equilibirum value'
           Write(LOGUNIT,*) 'K5s reset to:', R5s
           K5s(BoxNumberXY)=R5s
           K15s(BoxNumberXY)=R15s
      endif
      if ( R15s.lt.-5.0D+00*K15s(BoxNumberXY)) then
           R15s=0.5*chM5s/tom3_an*(p_d_tot_2-dn)
           Write(LOGUNIT,*) 'Value of K15s larger than max.equilibirum value'
           Write(LOGUNIT,*) 'K15s reset to:', R15s
           K5s(BoxNumberXY)=R5s
           K15s(BoxNumberXY)=R15s
      endif

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! Calculate for the above defined set of boundary conditions
         ! the steady-state profiles and return the average concentration
         ! in the oxic and denitrification layers
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         call BenSilicaEquation(KSiO3(BoxNumberXY),InitializeModel,chM5s-N5s_Ben(BoxNumberXY), &
             R5s,R15s,cD1m,cD2m,dn,p_d_tot_2,&
             p_s_ads,pShift,mShift,lShift, &
             p_poro(BoxNumberXY),p_p_ae,p_p_an,diff,diff_deep,lambda,sM5s,M0,alpha,0.001D+00,suD1,suD2,suDn)

         if ( InitializeModel== 1) then
           cM5s = CalculateSet( KSiO3(BoxNumberXY), 0, 0, 0, dummy, dummy)
           return
        endif


         r= chM5s-N5s_Ben(BoxNumberXY) 
         cM5s =min(r, CalculateSet( KSiO3(BoxNumberXY), SET_LAYER_INTEGRAL, LAYER1, &
                                            LAYER3, dn, ZERO)/ dn*tom3_K5s)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Calculate the adaptation time to the steady-state profile
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        r=(sM5s*cD1m + suD1*IntegralExp(- alpha, dn-cD1m))/dn
        Tau  =   CalculateTau(r,  diff,  p_p_ae,  cD2m)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Estimate the average value of M5s over the actual time step
        ! (transient value).
        ! This value depends on the adaptation time, the actual time step,
        ! the ''old'' value and the ''equilibrium value''
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        cM5s = cM5s+( mM5s- cM5s)* IntegralExp(-LocalDelta/Tau,1.0D+00)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! 1.Store equilibrium profile
        ! 2.Derive the equations for the transient profiles, assuming the same
        ! solution as for the steady-state case and using cM5s as new &
        ! constraint.
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        KSiO3E(BoxNumberXY) = CopySet( KSiO3(BoxNumberXY), KSiO3E(BoxNumberXY))
        dummy = CalculateSet( KSiO3(BoxNumberXY), ADD, 0, 0, dummy, dn* cM5s/tom3_K5s)

        if ( InitializeModel== 0) then

           lShift=shiftD2m(BoxNumberXY)*LocalDelta;
           cD2mnew = cD2m+ lShift
           if ( abs(lShift) > 0.000005D+00.and. p_p_ae> p_p_an) then
             mode=1;
             if ( lShift .gt.ZERO) then
               mShift=-max(ZERO,suD2* CalculateFromSet( KSiO3E(BoxNumberXY), EXPONENTIAL_INTEGRAL, RFLUX, cD2m,cD2mnew))
               pShift= CalculateFromSet( KSiO3(BoxNumberXY), SHIFT, LAYER2,cD2m, cD2mnew) &
                                         *(p_p_ae-p_p_an)/p_p_ae
             else
               mshift=-max(ZERO,suD1* CalculateFromSet( KSiO3E(BoxNumberXY), EXPONENTIAL_INTEGRAL, RFLUX, cD2mnew, cD2m)) 
               pShift=ZERO;
             endif
             call BenSilicaEquation(KSiO3(BoxNumberXY),1,chM5s-N5s_Ben(BoxNumberXY),R5s,R15s,cD1m,cD2m,dn,p_d_tot_2,&
                          p_s_ads,pShift,mShift,lShift, &
             p_poro(BoxNumberXY),p_p_ae,p_p_an,diff,diff_deep,lambda,sM5s,M0,alpha,0.001D+00,suD1,suD2,suDn)
             cM5s = min(CalculateSet( KSiO3(BoxNumberXY), SET_LAYER_INTEGRAL, LAYER1, &
                                            LAYER4, dn, ZERO)/ dn*tom3_K5s,chM5s-N5s_Ben(BoxNumberXY))

             !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
             ! Calculate the adaptation time to the steady-state profile
             !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

              r=(sM5s*cD1m + suD1*IntegralExp(- alpha, dn-cD1m))/dn
              Tau  =   CalculateTau(r,  diff,  p_p_ae,  dn)

              !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
              ! Estimate the average value of M5s over the actual time step
              ! (transient value).
              ! This value depends on the adaptation time, the actual time step,
              ! the ''old'' value and the ''equilibrium value''
              !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

               cM5s = cM5s+( mM5s- cM5s)* IntegralExp(-LocalDelta/Tau,1.0D+00)
               dummy = CalculateSet( KSiO3(BoxNumberXY), ADD, 0, 0, dummy, dn* cM5s/tom3_K5s)
           else
               mode=0;lShift=ZERO;
           endif


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Start calculation of fluxes:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Calculate flux at the sediment/water interface:
        ! Flux limitation at very low values of N5s
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        jbotN5s(BoxNumberXY) = -CalculateFromSet(KSiO3(BoxNumberXY), DERIVATIVE, RFLUX, ZERO, dummy)
        call LimitShift(jbotN5s(BoxNumberXY),&
                N5s_Ben(BoxNumberXY)*Depth_Ben(BoxNumberXY),K5s(BoxNumberXY),max_change_per_step);
        
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! the dissolution fluxes for K5s+K15s
        ! calculate dfissolution in first layer seperately
        ! limit fluxes for to large rates.
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        mjQ6K5s_1= -zuBT

        mjQ6K5s   =  mjQ6K5s_1  &
            -max(ZERO,suD1* CalculateFromSet( KSiO3E(BoxNumberXY), EXPONENTIAL_INTEGRAL, RFLUX, cD1m, cD2m)) &
               -max(ZERO,suD2* CalculateFromSet( KSiO3E(BoxNumberXY), EXPONENTIAL_INTEGRAL, RFLUX, cD2m, dn))
      if (isnan(mjQ6K15s)) then
        write(LOGUNIT,*) 'suD1=',suD1
        write(LOGUNIT,*) 'suD2=',suD2
        write(LOGUNIT,*) 'zuBt=',zuBT
        write(LOGUNIT,*) 'alpha=',alpha
        write(LOGUNIT,*) 'dn=',dn
      endif
        call LimitChange(2,mjQ6K5s,R5s,max_change_per_step,r)
        
        mjQ6K15s = -max(ZERO,suDn* CalculateFromSet( &
          KSiO3E(BoxNumberXY), EXPONENTIAL_INTEGRAL, RFLUX, dn, p_d_tot_2))
        call LimitChange(2,mjQ6K15s,max(p_small,R15s) ,max_change_per_step)
      if (isnan(mjQ6K15s)) then
        write(LOGUNIT,*) 'suD1=',suD1
        write(LOGUNIT,*) 'lambda=',lambda
        write(LOGUNIT,*) 'suD2=',suD2
        write(LOGUNIT,*) 'suDn=',suDn
        write(LOGUNIT,*) 'alpha=',alpha
        write(LOGUNIT,*) 'dn=',dn
      endif

        h= mjQ6K5s+mjQ6K15s
        call LimitChange(2,h,max(ZERO,Q6s(BoxNumberXY)),max_change_per_step,s)

        mjQ6K5s_1=mjQ6K5s_1* min(r,s)
        mjQ6K5s  =mjQ6K5s  * min(r,s)
        mjQ6K15s =mjQ6K15s * s
       
        call flux(BoxNumberXY, iiBen, ppK5s, ppK5s, -jbotN5s(BoxNumberXY) )
        call flux(BoxNumberXY, iiBen, ppQ6s, ppK5s, -mjQ6K5s )
        call flux(BoxNumberXY, iiBen, ppQ6s, ppK15s,-mjQ6K15s )

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! -Calculate flux at cD2m
        ! + Calculate new depth of of cD2m and the flux of silicate related to this shifting
        ! -limit fluxes for too large changes
        ! -Calculate flux at underside
        ! -limit fluxes for too large changes
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        shiftmass=ZERO; cShiftD2m=ZERO;
        mjK15K5s=CalculateFromSet(KSiO3(BoxNumberXY), DERIVATIVE, RFLUX, dn, dummy)

        if (isnan(mjK15K5s)) then
          write(LOGUNIT,*) 'Calculation flux mjK15K5s is NaN, mjK15K5s is reset on zero.'
          mjK15K5s=ZERO;
          write(LOGUNIT,*) 'alpha=',alpha
          write(LOGUNIT,*) 'zuBT=',zuBT
          write(LOGUNIT,*) 'M0=',M0
          write(LOGUNIT,*) 'M5bd1=',M5bD1
          write(LOGUNIT,*) 'chM5s=',chM5s
          write(LOGUNIT,*) 'mM5s=',mM5s
          write(LOGUNIT,*) 'smQ6=',smQ6
          write(LOGUNIT,*) 'cD1m=',cD1m
          write(LOGUNIT,*) 'cD2m=',cD2m
          write(LOGUNIT,*) 'mjQ6K5s=',mjQ6K5s
          write(LOGUNIT,*) 'mjQ6K15s=',mjQ6K15s
          write(LOGUNIT,*) 'R15s=',R15s
          write(LOGUNIT,*) 'K15s=',K15s(BoxNumberXY)
          write(LOGUNIT,*) 'Q6s=',Q6s(BoxNumberXY)
          write(LOGUNIT,*) 'M5s=',K5s(BoxNumberXY)*tom3_ae/cD2m
          write(LOGUNIT,*) 'M15s=',K15s(BoxNumberXY)*tom3_an/(p_d_tot_2-cD2m)
          call PrintSet(KSiO3(BoxNumberXY),'Problem with Silica')
        endif

        cD2mnew  =   dn+ lShift
        if ( abs(lShift)> ZERO) then
        shiftmass= CalculateFromSet( KSiO3(BoxNumberXY), SHIFT, LAYER4+mode, &
          dn, cD2mnew)/ LocalDelta
        endif

        jK15K5s=-mjK15K5s+ sign(chM5s/tom3_an*abs(lShift),lShift)/LocalDelta-shiftmass
        r=mjK15K5s+shiftmass
        call LimitShift(jK15K5s,max(ZERO,K5s(BoxNumberXY)),max(ZERO,K15s(BoxNumberXY)) ,max_change_per_step,s)
        call LimitShift(r,max(ZERO,R5s),max(ZERO,R15s) ,max_change_per_step,r)
        jK15K5s=min(r,s)*jK15K5s
 

        call flux(BoxNumberXY, iiBen, ppK15s,ppK5s,  jK15K5s *insw( jK15K5s)  )
        call flux(BoxNumberXY, iiBen, ppK5s, ppK15s,-jK15K5s *insw(-jK15K5s)  )

        r= chM5s- CalculateFromSet( KSiO3(BoxNumberXY), &
                                       EQUATION, STANDARD, p_d_tot_2, dummy)
        i=p_flux_at_deep_end; if ( r< ZERO) i=1
        select case (i)
          case (1); s=   ZERO
          case (2); s = -max(ZERO,CalculateFromSet(KSiO3(BoxNumberXY), DERIVATIVE, RFLUX, p_d_tot_2, dummy))
          case (3); s = -CalculateFromSet(KSiO3(BoxNumberXY), DERIVATIVE, RFLUX, p_d_tot_2, dummy)
        end select
        jK25K15s(BoxNumberXY)=s; s=-s;
        call LimitChange(p_flux_at_deep_end,s,max(p_small,R15s) ,max_change_per_step,r)

        call LimitChange(1,jK25K15s(BoxNumberXY),K15s(BoxNumberXY) ,max_change_per_step,s)
        jK25K15s(BoxNumberXY)=jK25K15s(BoxNumberXY)* min(r,s)
        call flux(BoxNumberXY, iiBen, ppK15s,ppK15s, jK25K15s(BoxNumberXY) )

        ! Determine where the median is of Q6 in the range from clm to D1m

        call flux(BoxNumberXY,iiBen, ppD9m,ppD9m,-mjQ6K5s_1*(cD1m/2.0D+00- D9m(BoxNumberXY))/(p_small+Q6s(BoxNumberXY)))

        call flux(BoxNumberXY,iiBen, ppD9m,ppD9m,-(mjQ6K5s-mjQ6K5s_1)* &
              ((dn+cD1m)*0.5- D9m(BoxNumberXY))/(p_small+Q6s(BoxNumberXY)))

        call flux(BoxNumberXY, iiBen, ppD9m, ppD9m,((p_d_tot_2+dn)*0.5D+00   &
            - D9m(BoxNumberXY))* (-mjQ6K15s)/( p_small+Q6s(BoxNumberXY)) )

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate the pore-water average concentrations from the state variables
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      M5s(BoxNumberXY) = max(ZERO,chM5s- max(ZERO,CalculateFromSet( KSiO3(BoxNumberXY), &
                INTEGRAL, STANDARD, ZERO, D1m(BoxNumberXY)))/ D1m(BoxNumberXY))
      end if
  enddo

  end
  subroutine BenSilicaEquation(KSiO3,mode,N5s,R5s,R15s,D1m,D2m,dn,d_tot, &
                          p_s_ads,pShift,mShift,lShift, &
                          poro,p_ae,p_an,diff,diff_deep,lambda,sM5s,M0,alpha,slu,suD1,suD2,suDn)
  use global_mem, ONLY:RLEN,ZERO
  use mem_param,only:p_small 
  use mem,only: dummy,idummy
  use constants, ONLY: LAYERS, &
    LAYER1,LAYER2,LAYER3,LAYER5,DIFFUSION, FOR_ALL_LAYERS, POROSITY, &
    ADSORPTION, LAYER4, DEFINE, CONSTANT_TERM, SET_CONTINUITY, &
    FLAG, MASS, SET_BOUNDARY, EQUATION, BESSELI_EXP_TERM, &
    BESSELK_EXP_TERM, EXPONENTIAL_TERM,PARAMETER_DEFINE, &
    SET_LAYER_INTEGRAL_UNTIL, INPUT_TERM,&
    STANDARD, MIN_VAL_EXPFUN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:InitializeSet, &
  ! DefineSet, CompleteSet, CalculateSet, CalculateTau, CalculateFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use bennut_interface, ONLY: InitializeSet, DefineSet, CompleteSet 

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following sesame functions are used:IntegralExp, insw
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  implicit none


    integer,intent(INOUT)         ::KSiO3
    integer ,intent(IN)           ::mode
    real(RLEN),intent(IN)         ::N5s
    real(RLEN),intent(IN)         ::R5s
    real(RLEN),intent(IN)         ::R15s
    real(RLEN),intent(IN)         ::D1m
    real(RLEN),intent(IN)         ::D2m
    real(RLEN),intent(IN)         ::dn
    real(RLEN),intent(IN)         ::d_tot
    real(RLEN),intent(IN)         ::p_s_ads
    real(RLEN),intent(IN)         ::pShift
    real(RLEN),intent(IN)         ::mShift
    real(RLEN),intent(IN)         ::lShift
    real(RLEN),intent(IN)         ::poro
    real(RLEN),intent(IN)         ::p_ae
    real(RLEN),intent(IN)         ::p_an
    real(RLEN),intent(IN)         ::diff
    real(RLEN),intent(IN)         ::diff_deep
    real(RLEN),intent(IN)         ::lambda
    real(RLEN),intent(IN)         ::sM5s
    real(RLEN),intent(IN)         ::M0
    real(RLEN),intent(IN)         ::alpha
    real(RLEN),intent(IN)         ::slu
    real(RLEN),intent(IN)         ::suD1
    real(RLEN),intent(IN)         ::suD2
    real(RLEN),intent(IN)         ::suDn

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Initialize the set of differential equations giving:
      ! - n. of layers;
      ! - n. of coefficients
      ! - layer depths
      ! - environmental conditions (diffusion, porosity and adsorption coeff.)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

     integer         ::i
     real(RLEN)      :: ds1,ds2,p_ads_3
     real(RLEN)      ::gamma
    real(RLEN)       ::r

      if ( mode <= 0) then
         p_ads_3=p_an; ds1=D2m;                   ds2=D2m
      else if (lShift.lt.ZERO) then
         p_ads_3=p_an; ds1=D2m+min(-0.0001D+00,lShift);ds2=D2m
      else
         p_ads_3=p_ae; ds1=D2m;                   ds2=ds1 +max( 0.0001D+00,lShift)
      endif


    select case (mode) 
      case (-1)
         KSiO3 = InitializeSet( KSiO3, 2, 4)
         call  DefineSet( KSiO3, LAYERS, LAYER1, 0,      D1m, dummy)
         call DefineSet(KSiO3,DIFFUSION, LAYER1,LAYER2,diff,diff)
      case (0)
         KSiO3 = InitializeSet( KSiO3, 4, 9)
         call  DefineSet( KSiO3, LAYERS, LAYER1, LAYER2, D1m, D2m)
         call  DefineSet( KSiO3, LAYERS, LAYER3, idummy, dn, dummy)
         call DefineSet(KSiO3,DIFFUSION, LAYER1,LAYER2,diff,diff)
         call DefineSet(KSiO3,DIFFUSION, LAYER3,LAYER4,diff_deep,diff_deep)
      case (1)
         KSiO3 = InitializeSet( KSiO3, 5, 12)
         call  DefineSet( KSiO3, LAYERS, LAYER1, LAYER2, D1m, ds1)
         call  DefineSet( KSiO3, LAYERS, LAYER3, LAYER4, ds2, dn)
         call DefineSet(KSiO3,DIFFUSION, LAYER1,LAYER2,diff,diff)
         call DefineSet(KSiO3,DIFFUSION, LAYER3,LAYER4,diff,diff_deep)
         call DefineSet(KSiO3,DIFFUSION, LAYER5,0,diff_deep,dummy)
    end select

      call DefineSet(KSiO3,POROSITY,  FOR_ALL_LAYERS, idummy, poro, dummy)
      call DefineSet(KSiO3,ADSORPTION,LAYER1, LAYER2, p_ae, p_ae)

      select case (mode) 
        case (0)
          call DefineSet(KSiO3,ADSORPTION,LAYER3, LAYER4, p_an, p_an)
        case (1)
          call DefineSet(KSiO3,ADSORPTION,LAYER3, LAYER4, p_ads_3, p_an)
          call DefineSet(KSiO3,ADSORPTION,LAYER5, idummy,p_an,dummy)
      end select
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Define coefficients for the steady-state solutions in each layer
      ! General solution of the equilibrium profile:
      ! C = Ssat - S
      !
      ! 1st layer:C(z) = c13*z^2 + c14*z + c15
      ! 2nd layer:C(z) = c21*I0*exp[-alpha*(z-cD1m)]+ c22*K0*exp[-alpha*(z-cD1m)]  
      ! 3nd layer:C(z) = c21*I0*exp[-alpha*(z-cD1m)]+ c22*K0*exp[-alpha*(z-cD1m)]  
      ! ( I0 and K0  = modified Bessel functions of 0-order)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      call DefineSet( KSiO3, DEFINE, 11, EXPONENTIAL_TERM, -lambda, dummy)
      call DefineSet( KSiO3, DEFINE, 12, EXPONENTIAL_TERM, +lambda, dummy)

        select case (mode) 
          case (-1)
              call DefineSet( KSiO3, DEFINE, 15, CONSTANT_TERM, dummy, dummy)
              call DefineSet( KSiO3,    PARAMETER_DEFINE, 21, &
                                                BESSELI_EXP_TERM, - alpha, max(slu,suD1))
          case (0)
             call DefineSet( KSiO3, DEFINE, 15, CONSTANT_TERM, dummy, dummy)
             call DefineSet( KSiO3,    PARAMETER_DEFINE, 21, &
                                                BESSELI_EXP_TERM, - alpha, max(slu,suD1))
             call DefineSet( KSiO3, PARAMETER_DEFINE, 22, &
                                                BESSELK_EXP_TERM, - alpha, max(slu,suD1))
            i=30
          case (1)
            call DefineSet( KSiO3, DEFINE, 15, CONSTANT_TERM, dummy, dummy)
            call DefineSet( KSiO3,    PARAMETER_DEFINE, 21, &
                                                BESSELI_EXP_TERM, - alpha, max(slu,suD1))
            call DefineSet( KSiO3, PARAMETER_DEFINE, 22, &
                                                BESSELK_EXP_TERM, - alpha, max(slu,suD1))
            gamma=max(MIN_VAL_EXPFUN,sqrt(p_s_ads/diff)); 
            call DefineSet( KSiO3, DEFINE, 31, EXPONENTIAL_TERM, -gamma,  dummy)
            call DefineSet( KSiO3, DEFINE, 32, EXPONENTIAL_TERM,  gamma, dummy)
            call DefineSet( KSiO3, DEFINE, 35, CONSTANT_TERM, dummy, dummy)

            i=40
         end select

         if ( mode.ge.0) then
          call DefineSet( KSiO3, PARAMETER_DEFINE, i+1, &
                                                BESSELI_EXP_TERM, - alpha, max(slu,suD2))
          call DefineSet( KSiO3, PARAMETER_DEFINE, i+2, &
                                                BESSELK_EXP_TERM, - alpha, max(slu,suD2))

         call DefineSet( KSiO3, PARAMETER_DEFINE, i+11, &
                                                BESSELI_EXP_TERM, - alpha, max(slu,suDn))
         call DefineSet( KSiO3, PARAMETER_DEFINE, i+12, &
                                                BESSELK_EXP_TERM, - alpha, max(slu,suDn))
      endif

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Insert other boundary conditions and continuity between layers:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      !2/6/8
      call CompleteSet( KSiO3, SET_CONTINUITY, FLAG, MASS, dummy)

      !3/7/9
      call CompleteSet( KSiO3, SET_BOUNDARY, LAYER1, EQUATION, ZERO, value=N5s)

      select case (mode) 
         case(-1) 
            !4
             call CompleteSet( KSiO3, INPUT_TERM, 15, STANDARD, dummy, value=-M0/max(slu,sM5s))
         case(0)
           !8,9
           call  CompleteSet(KSiO3, SET_LAYER_INTEGRAL_UNTIL, LAYER4, LAYER4,d_tot, &
                                                                              max(p_small,R15s));
           call CompleteSet( KSiO3, INPUT_TERM, 15, STANDARD, dummy, value=-M0/max(slu,sM5s))
        case (1)
          !10,12
          r=+p_s_ads * abs(pShift/lShift)/(1.0D+00+p_ads_3)/poro + abs(mShift/lShift);
          call CompleteSet( KSiO3, INPUT_TERM, 35, STANDARD, dummy, value=r/p_s_ads)

          call  CompleteSet(KSiO3, SET_LAYER_INTEGRAL_UNTIL, LAYER5, LAYER5,d_tot, &
                                                                              max(p_small,R15s));

!         call CompleteSet(KSiO3,SET_BOUNDARY,LAYER5,DERIVATIVE,d_tot,ZERO);
          call CompleteSet( KSiO3, INPUT_TERM, 15, STANDARD, dummy, value=-M0/max(slu,sM5s))


      end select

         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
         ! Calculate for the above defined set of boundary conditions
         ! the steady-state profiles and return the average concentration
         ! in the oxic and denitrification layers
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  return 
  end

!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
