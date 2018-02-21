#include "DEBUG.h"
#include "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenNBac
!
! DESCRIPTION
!   !    This submodel describes the carbon dynamics and associated
!    nutrient dynamics in benthic nitrifying bacteria (represented
!    by state variables H3) 
!      Based on:Keen & Presser,1987 ,ArchBiol.147:73-79
!      Based on :knowlegdeNBook geo,VU.nl
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine BenNBacDynamics
!
! !USES:

  ! For the following Benthic-states fluxes are defined: Q6c, Q6n, Q6p, &
  ! D6m, D7m, D8m
  ! The following Benthic-states are used (NOT in fluxes): D1m, D2m
  ! For the following Benthic-group-states fluxes are defined: &
  ! BenDetritus, BenthicAmmonium, BenthicPhosphate
  ! The following Benthic 1-d global boxvars  are used: ETW_Ben
  ! The following Benthic 2-d global boxvars got a value: ruHI,reHI
  ! The following constituent constants  are used: iiC, iiN, iiP
  ! The following 0-d global parameters are used: p_d_tot, p_small, &
  ! p_pe_R1c, p_pe_R1n, p_pe_R1p, p_qro
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
#ifdef NOPOINTERS
  use mem,  ONLY: D2STATE
#else
  use mem, ONLY: D2STATE, H3c, H3n,H3p,D6m, D7m, D8m, D1m, K1p,K4n , &
                          Q6c, Q6n , Q6p,G2o
#endif
  use mem, ONLY:  ppG3c, ppG2o, ppD6m, ppQ6c,ppQ6n, ppQ6p, &
    ppD7m, ppD8m, ppQ1c,ppQ1n,  ppQ1p, ppK4n, &
    ppK1p, ppH3c, ppH3p, ppH3n, ETW_Ben,rrBTo,reBTn,reBTp, sK4K3,&
    iiQ1, NO_BOXES_XY, iiBen, flux_vector, reHI, &
    sourcesink_flux_vector 
  use mem_Param,  ONLY: p_small, p_pe_R1c, p_pe_R1n, p_pe_R1p
  use mem_Param,  ONLY: p_poro,p_p_ads_K1=>p_p_ae,p_qon_nitri
  use mem_BenthicNutrient3, ONLY:p_max_state_change
  use mem_BenAmmonium, ONLY:p_p_ads_K4=>p_p
  use mem_BenNBac
  use LimitRates, ONLY:LimitChange_vector
  use mem_BenthicNutrient3,ONLY:p_max_state_change

#ifdef INCLUDE_BENCO2
  use mem,ONLY: CO2ae
#endif

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:eTq_vector, MM_vector, &
  ! PartQ_vector, eramp_vector, insw_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun, ONLY: eTq_vector, MM_vector, insw_vector


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE

! !INPUT:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  
!
! !AUTHORS
!   P. Ruardij 
!
!
! !REVISION_HISTORY
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
  ! Set up Local Variable for copy of state var. object
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN)                         :: LocalDelta
  real(RLEN)                         :: max_change_per_step
  real(RLEN),dimension(NO_BOXES_XY)  :: clm
  real(RLEN),dimension(NO_BOXES_XY)  :: cm
  real(RLEN),dimension(NO_BOXES_XY)  :: cmm
  real(RLEN),dimension(NO_BOXES_XY)  :: et
  real(RLEN),dimension(NO_BOXES_XY)  :: eo
  real(RLEN),dimension(NO_BOXES_XY)  :: rK4K3n
  real(RLEN),dimension(NO_BOXES_XY)  :: rumn
  real(RLEN),dimension(NO_BOXES_XY)  :: rump
  real(RLEN),dimension(NO_BOXES_XY)  :: sm
  real(RLEN),dimension(NO_BOXES_XY)  :: misn
  real(RLEN),dimension(NO_BOXES_XY)  :: misp
  real(RLEN),dimension(NO_BOXES_XY)  :: ren
  real(RLEN),dimension(NO_BOXES_XY)  :: rep
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6c
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6n
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6p
  real(RLEN),dimension(NO_BOXES_XY)  :: run
  real(RLEN),dimension(NO_BOXES_XY)  :: rug
  real(RLEN),dimension(NO_BOXES_XY)  :: rea
  real(RLEN),dimension(NO_BOXES_XY)  :: rrc
  real(RLEN),dimension(NO_BOXES_XY)  :: netgrowth
  real(RLEN),dimension(NO_BOXES_XY)  :: r
  real(RLEN),dimension(NO_BOXES_XY)  :: s
  real(RLEN),dimension(NO_BOXES_XY)  :: runp
  real(RLEN),dimension(NO_BOXES_XY)  :: runn
  real(RLEN),dimension(NO_BOXES_XY)  :: rupp
  real(RLEN),dimension(NO_BOXES_XY)  :: rupn
  real(RLEN),dimension(NO_BOXES_XY)  :: M4n
  real(RLEN),dimension(NO_BOXES_XY)  :: c_ads_K1
  real(RLEN),dimension(NO_BOXES_XY)  :: c_ads_K4

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! External functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN), external  :: GetDelta
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  LocalDelta=GetDelta()
  max_change_per_step=p_max_state_change/LocalDelta


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Assign functional group-dependent parameters:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      c_ads_K1=p_p_ads_K1+1.0D+00
      c_ads_K4=p_p_ads_K4+1.0D+00

      clm  =   0.0D+00
      cm  =   D1m(:)
      cmm  =   D1m(:)/ 2.0D+00



  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Physiological temperature response
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  et  =   eTq_vector(  ETW_Ben(:),  p_q10)
  eo  =    MM_vector(  cm- clm,  p_cdm)

  M4n  =   K4n/ p_poro/c_ads_K4/ D1m

  ! Enhance growth of concentration of H3c is too low!!
  ! r=max(1.0,10.0 * 0.1/(H3c+0.1))

  rK4K3n= H3c(:) * p_km  *et * eo * max(1.0D-80,M4n/(M4n+p_clM4n)) 
  r=rK4K3n*p_qon_nitri
  ! oxygen flux due top nitrification is defined in BenAmmonium.F90
  call LimitChange_vector(1,r,G2o,max_change_per_step,s)
  call LimitChange_vector(1,rK4K3n,K4n,max_change_per_step,r)
  rK4K3n=rK4K3n*min(r,s)

! if ( rK4K3n(1) < 0.0D+00) then
!    write(LOGUNIT,*) 'D1m',D1m(1)
!    write(LOGUNIT,*) 'K4n',K4n(1)
!    write(LOGUNIT,*) 'G20',G2o(1)
!    write(LOGUNIT,*) 'et',et
! endif


  rug = p_cyn * rK4K3n 
#ifdef INCLUDE_BENCO2
  !limitation at hight pH == low [CO2] in oxuc layer
  r  =   rug/ p_poro/D1m/12.0    ! prim prod recalculated to mmolC/m3 porewater
  call LimitChange_vector(1,r,CO2ae,max_change_per_step,s)
  rK4K3n=rK4K3n*s
  rug = p_cyn * rK4K3n 
#endif
  rrc = p_cyn * et * p_sr_nc * H3c(:) *H3c(:)/ (0.01+H3c(:))
  run=max(0.0,rug-rrc)
  call sourcesink_flux_vector(iiBen, ppG3c,ppH3c,  rug)
  call sourcesink_flux_vector(iiBen, ppH3c,ppG3c,  rrc)

  rrBTo(:)  =   rrBTo(:)+ (rrc-rug)/12.0D+00
  call flux_vector( iiBen, ppG2o,ppG2o, (rrc-rug)/12.0D+00)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of potential nutrient uptake
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rumn =   K4n/p_poro/c_ads_K4 * p_sumKIn *H3c
  call LimitChange_vector(1,rumn,K4n,max_change_per_step)
  rump =   K1p/p_poro/c_ads_K1 * p_sumKIp *H3c
  call LimitChange_vector(1,rump,K1p,max_change_per_step)
   
  ! a too high nutrient content of food is related to the food upake ( r < 0) 
  ! a too low  nutrient content of food is related to net growth of the organisms   ( r > 0)

  r=(p_qnc* H3c(:)-max(0.0,H3n(:)))*insw_vector(rug)
  misn = (run*r*insw_vector(r)+max(rrc,run)*r*insw_vector(-r))/(p_small+H3c(:))
  r=(p_qpc* H3c(:)-max(0.0,H3p(:)))*insw_vector(rug)
  misp = (run*r*insw_vector(r)+max(rrc,run)*r*insw_vector(-r))/(p_small+H3c(:))

  rupn=run*p_qnc
  rupp=run*p_qpc

  runn=min(rumn-min(0.0,misn),rupn+max(0.0,misn))
  runp=min(rump-min(0.0,misp),rupp+max(0.0,misp))


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Carbon correction: all C which cannot be used for growth due to
  ! lack of nutrients is excreted!
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  netgrowth  =   min(  run,runn/ p_qlnc,runp/ p_qlpc)
  netgrowth  =   max(  netgrowth,  0.0D+00)

  !excretion if gross fixation of carbon cannot be used for growth
  rea =run-netgrowth
  run  =   netgrowth

  ren  =   max(-run*p_qnc,-runn) *insw_vector(run) -min(0.0D+00,misn)
  rep  =   max(-run*p_qpc,-runp) *insw_vector(run) -min(0.0D+00,misp)
!  if (isnan(rep(1))) write(LOGUNIT,*) 'rep Nan in BenNBac'

  reBTn(:)  =   reBTn(:)+ ren
  reBTp(:)  =   reBTp(:)+ rep

  rK4K3n=max(0.0,rK4K3n-max(0.0,-ren))
  sK4K3 =max(1.0D-80,(p_small+rK4K3n)/(p_small+ K4n(:) /p_poro/c_ads_K4))
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of mortality
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  sm  =   p_sd2*H3c(:)/(cm-clm)+max(0.0D+00,rrc-rug)/rrc*p_sm_resp

  rqt6c  =   H3c(:)* sm*( 1.0D+00- p_pe_R1c)
  rqt6n  =   H3n(:)* sm*( 1.0D+00- p_pe_R1n)
  rqt6p  =   H3p(:)* sm*( 1.0D+00- p_pe_R1p)

  call flux_vector( iiBen, ppH3c,ppQ1c, rea+ H3c(:)* sm* p_pe_R1c )
  call flux_vector( iiBen, ppH3n,ppQ1n,      H3n(:)* sm* p_pe_R1n )
  call flux_vector( iiBen, ppH3p,ppQ1p,      H3p(:)* sm* p_pe_R1p )

  reHI(iiQ1,:)  =  reHI(iiQ1,:) + H3c(:)* sm* p_pe_R1c;

  call flux_vector( iiBen, ppH3c,ppQ6c, rqt6c )
  call flux_vector( iiBen, ppH3n,ppQ6n, rqt6n )
  call flux_vector( iiBen, ppH3p,ppQ6p, rqt6p )

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Fluxes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call flux_vector(iiBen, ppK4n,ppH3n, -ren * insw_vector(-ren))
  call flux_vector(iiBen, ppH3n,ppK4n,  ren * insw_vector(ren))
  call flux_vector(iiBen, ppK1p,ppH3p, -rep * insw_vector(-rep))
  call flux_vector(iiBen, ppH3p,ppK1p,  rep * insw_vector(rep))


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Assigning depends on type of bacteria and layers in which they occur:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of changes due to uptake of detritus in distribution
  ! of state variables (Dx.m is a undetermined source)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call flux_vector(iiBen, ppD6m,ppD6m,( cmm- D6m(:))*( - rqt6c)/(p_small+ Q6c(:)))
  call flux_vector(iiBen, ppD7m,ppD7m,( cmm- D7m(:))*( - rqt6n)/(p_small+ Q6n(:)))
  call flux_vector(iiBen, ppD8m,ppD8m,( cmm- D8m(:))*( - rqt6p)/(p_small+ Q6p(:)))

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
