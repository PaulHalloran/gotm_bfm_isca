#include "DEBUG.h"
#include "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenBac
!
! DESCRIPTION
!   !    This submodel describes the carbon dynamics and associated
!    nutrient dynamics in benthic bacteria (represented
!    by state variables H1-H2) 
! 
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine BenBacDynamics(hx,  pphxc, pphxn, pphxp)
!
! !USES:

  ! For the following Benthic-states fluxes are defined: Q6c, Q6n, Q6p, G2o, &
  ! K16r, D6m, D7m, D8m
  ! The following Benthic-states are used (NOT in fluxes): D1m, D2m
  ! For the following Benthic-group-states fluxes are defined: &
  ! BenLabileDetritus, BenthicAmmonium, BenthicPhosphate
  ! The following Benthic 1-d global boxvars are modified : rrBTo, reBTn, &
  ! reBTp, rrATo, reATn, reATp
  ! The following Benthic 1-d global boxvars  are used: ETW_Ben
  ! The following Benthic 2-d global boxvars got a value: ruHI,reHI
  ! The following groupmember vars  are used: iiH1, iiH2
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
  use mem, ONLY: D2STATE, Q6c, Q6n, Q6p, D6m,K11p,K21p, &
    D7m, D8m, D1m, D2m, BenLabileDetritus, BenthicAmmonium, BenthicPhosphate
#endif
  use mem, ONLY: ppQ6c, ppQ6n, ppQ6p, ppG3c,ppG13c, ppG2o, ppK16r, ppD6m, &
    ppD7m, ppD8m, ppBenLabileDetritus, ppBenthicAmmonium, &
    ppBenthicPhosphate, rrBTo, reBTn, reBTp, rrATo, reATn, reATp, ETW_Ben, ruHI, &
    iiC, iiN, iiP, NO_BOXES_XY, iiBen, flux_vector, reHI, sourcesink_flux_vector 
  use mem_Param,  ONLY: p_d_tot, p_small, p_pe_R1c, p_pe_R1n, p_pe_R1p, p_qro
  use mem_Param, ONLY: p_poro,p_p_ads_K1_ae=>p_p_ae,p_qpPhc,p_qnUlc
  use mem_BenAmmonium, ONLY:p_p_ads_K4=>p_p
  use mem_BenPhosphate, ONLY:p_p_ads_K1_an=>p_p_an
  use mem_BenBac
  use constants,  ONLY:p_qnUc
  use LimitRates, ONLY:LimitChange_vector
  use mem_BenthicNutrient3,ONLY:p_max_state_change

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:eTq_vector, MM_vector, &
  ! PartQ_vector, insw_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun, ONLY: eTq_vector, MM_vector, PartQ_vector, insw_vector

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE

! !INPUT:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer,intent(IN)  :: hx
  integer,intent(IN) :: pphxc
  integer,intent(IN) :: pphxn
  integer,intent(IN) :: pphxp

!  
!
! !AUTHORS
!   W. Ebenhoh and C. Kohlmeier 
! 
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
  real(RLEN),dimension(NO_BOXES_XY) :: hxc
  real(RLEN),dimension(NO_BOXES_XY) :: hxn
  real(RLEN),dimension(NO_BOXES_XY) :: hxp
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN)                         :: LocalDelta
  real(RLEN)                         :: max_change_per_step
  real(RLEN),dimension(NO_BOXES_XY)  :: clm
  real(RLEN),dimension(NO_BOXES_XY)  :: cm
  real(RLEN),dimension(NO_BOXES_XY)  :: chm
  real(RLEN),dimension(NO_BOXES_XY)  :: cmm
  real(RLEN),dimension(NO_BOXES_XY)  :: et
  real(RLEN),dimension(NO_BOXES_XY)  :: eN
  real(RLEN),dimension(NO_BOXES_XY)  :: eo
  real(RLEN),dimension(NO_BOXES_XY)  :: availQ6_c
  real(RLEN),dimension(NO_BOXES_XY)  :: availQ6_p
  real(RLEN),dimension(NO_BOXES_XY)  :: availQ6_n
  real(RLEN),dimension(NO_BOXES_XY)  :: suQ1
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ1c
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ1n
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ1p
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ6c
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ6n
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ6p
  real(RLEN),dimension(NO_BOXES_XY)  :: rumn4
  real(RLEN),dimension(NO_BOXES_XY)  :: rumnu
  real(RLEN),dimension(NO_BOXES_XY)  :: rumpu
  real(RLEN),dimension(NO_BOXES_XY)  :: rump
  real(RLEN),dimension(NO_BOXES_XY)  :: rrc
  real(RLEN),dimension(NO_BOXES_XY)  :: sm
  real(RLEN),dimension(NO_BOXES_XY)  :: misn
  real(RLEN),dimension(NO_BOXES_XY)  :: misp
  real(RLEN),dimension(NO_BOXES_XY)  :: ren
  real(RLEN),dimension(NO_BOXES_XY)  :: rep
  real(RLEN),dimension(NO_BOXES_XY)  :: reR7c
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6c
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6n
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6p
  real(RLEN),dimension(NO_BOXES_XY)  :: qnQ1c
  real(RLEN),dimension(NO_BOXES_XY)  :: qpQ1c
  real(RLEN),dimension(NO_BOXES_XY)  :: qnQ6c
  real(RLEN),dimension(NO_BOXES_XY)  :: qpQ6c
  real(RLEN),dimension(NO_BOXES_XY)  :: rut
  real(RLEN),dimension(NO_BOXES_XY)  :: rum
  real(RLEN),dimension(NO_BOXES_XY)  :: runc
  real(RLEN),dimension(NO_BOXES_XY)  :: rug
  real(RLEN),dimension(NO_BOXES_XY)  :: r,xeff
  real(RLEN),dimension(NO_BOXES_XY)  :: runp
  real(RLEN),dimension(NO_BOXES_XY)  :: runn
  real(RLEN),dimension(NO_BOXES_XY)  :: rupp
  real(RLEN),dimension(NO_BOXES_XY)  :: rupn
  real(RLEN),dimension(NO_BOXES_XY)  :: cK4n
  real(RLEN),dimension(NO_BOXES_XY)  :: cK1p
  real(RLEN),dimension(NO_BOXES_XY)  :: c_ads_K4
  real(RLEN),dimension(NO_BOXES_XY)  :: c_ads_K1
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! External functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN), external  :: GetDelta
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  LocalDelta=GetDelta()
  max_change_per_step=p_max_state_change/LocalDelta

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  Copy  state var. object in local var
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  hxc = D2STATE(pphxc,:)
  hxn = D2STATE(pphxn,:)
  hxp = D2STATE(pphxp,:)


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Assign functional group-dependent parameters:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  select case ( hx)

    case ( iiH1 )
      clm  =   0.0D+00
      cm  =   D1m(:)
      chm  =   D1m(:)
      cmm  =   D1m(:)/ 2.0D+00
      c_ads_K4=p_p_ads_K4   +1.0D+00
      c_ads_K1=p_p_ads_K1_ae+1.0D+00
      cK1p =   BenthicPhosphate(p_iK1(hx),iiP)/c_ads_K1

    case ( iiH2 )
      clm  =   D1m(:)
      cm  =   D2m(:)
      chm  =   p_d_tot
      cmm  =   D6m(:)+ D1m(:)
      r=PartQ_vector(  D6m(:),  clm,  chm,  p_d_tot)
      c_ads_K4=p_p_ads_K4+1.0D+00
      cK1p= (PartQ_vector(D6m(:),clm,cm,p_d_tot)*K11p/(p_p_ads_K1_ae+1.0)+  &
             PartQ_vector(D6m(:),cm,chm,p_d_tot)*K21p/(p_p_ads_K1_an+1.0))  &
                                                       /(1.0D-80+r)
  end select
  cK4n =   BenthicAmmonium(p_iK4(hx),iiN)/c_ads_K4 

  ! Determine where the median is of Q6 in the range from clm to D1m

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Physiological temperature response
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  et  =   eTq_vector(  ETW_Ben(:),  p_q10(hx))
  eo  =   MM_vector(  cm- clm,  p_cdm(hx))


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate total food
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Detritus (if eaten): calculate available amount
  ! and add it to the total amount of food
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  availQ6_c  =   Q6c(:)* PartQ_vector(  D6m(:),  clm,  chm,  p_d_tot)
  availQ6_n  =   Q6n(:)* PartQ_vector(  D7m(:),  clm,  chm,  p_d_tot)
  availQ6_p  =   Q6p(:)* PartQ_vector(  D8m(:),  clm,  chm,  p_d_tot)

  qnQ6c  =   availQ6_n* p_cuQ6np(hx)/( availQ6_c+ 1.0D-30)
  qpQ6c  =   availQ6_p* p_cuQ6np(hx)/( availQ6_c+ 1.0D-30)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Growth is controlled by quality of detritus (N and P content):
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

! eN  =   eramp_vector(qnQ6c, p_qnc(hx))* eramp_vector(qpQ6c, p_qpc(hx))
  xeff=1.0-p_pur(hx)
  eN=max(0.0,1.0-max(abs(qnQ6c-xeff*p_qnc(hx))/(xeff*p_qnc(hx)), &
                            abs(qpQ6c-xeff*p_qpc(hx))/(xeff*p_qpc(hx))))


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Total substrate availability:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ruQ6c  =  availQ6_c* ( p_suhQ6(hx)* eN+ p_sulQ6(hx)*(1.0-eN))

  qnQ1c=BenLabileDetritus(p_iQ1(hx),iiN)/(BenLabileDetritus(p_iQ1(hx),iiC)+1.0D-80)
  qpQ1c=BenLabileDetritus(p_iQ1(hx),iiP)/(BenLabileDetritus(p_iQ1(hx),iiC)+1.0D-80)

  r=max(0.0,1.0-max(abs(qnQ1c-xeff*p_qnc(hx))/(xeff*p_qnc(hx)), &
                            abs(qpQ1c-xeff*p_qpc(hx))/(xeff*p_qpc(hx))))
  suQ1= r*p_suUQ1(hx)+(1.0-r)*p_suQ1(hx)
  ruQ1c=suQ1* BenLabileDetritus(p_iQ1(hx),iiC)

  rut  =   ruQ6c+ ruQ1c

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Potential uptake by bacteria
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rum  =   p_sum(hx)* et* eo* hxc

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Actual uptake by bacteria
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rug  = min(rum, rut)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Carbon fluxes into bacteria
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ruQ6c = rug* ruQ6c/ (1.0D-80 + rut )
  ruQ1c = rug* ruQ1c/ (1.0D-80 + rut )

  call flux_vector( iiBen, ppQ6c,pphxc, ruQ6c )
  ruHI(p_iQ1(hx),:)  =   ruQ1c

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Nutrient fluxes into bacteria from carbon fluxes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ruQ6n  =   ruQ6c* qnQ6c
  ruQ6p  =   ruQ6c* qpQ6c
  ruQ1n  =   ruQ1c* qnQ1c
  ruQ1p  =   ruQ1c* qpQ1c

  call LimitChange_vector(1,ruQ6n,Q6n,max_change_per_step)
  call LimitChange_vector(1,ruQ6p,Q6p,max_change_per_step)
  call flux_vector( iiBen, ppQ6n,pphxn, ruQ6n )
  call flux_vector( iiBen, ppQ6p,pphxp, ruQ6p )


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of respiration:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rrc  =   p_srr(hx)* hxc* et+( ruQ1c+ ruQ6c)* p_pur(hx)
  runc  =   max(  0.0D+00,  rug- rrc)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of potential nutrient uptake
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rumn4 = max(0.0,cK4n * p_sumKIn(hx) *hxc/p_poro)
  call LimitChange_vector(1,rumn4,cK4n,max_change_per_step)
  r= max(0.0, BenLabileDetritus(p_iQ1(hx),iiN) &
                    -p_qnUlc*BenLabileDetritus(p_iQ1(hx),iiC))
  rumnu = r * p_sumKIn(hx) *hxc/p_poro
  rump =   max(0.0,cK1p* p_sumKIp(hx) *hxc/p_poro)
  call LimitChange_vector(1,rump,cK1p,max_change_per_step)
  r= max(0.0, BenLabileDetritus(p_iQ1(hx),iiP) &
                    -p_qpPhc*BenLabileDetritus(p_iQ1(hx),iiC))
  rumpu = r * p_sumKIp(hx) *hxc/p_poro
   
  r  =   min(  runc,( rumn4+ rumnu +ruQ6n+ ruQ1n )/ p_qlnc(hx))
  r  =   min(  r,  ( rump  +rumpu+ ruQ6p+ ruQ1p )/ p_qlpc(hx))
  reR7c  =  runc -  max(  r,  0.0D+00)

  runc  =   r

  r=p_qnc(hx)* hxc(:)-hxn(:)
  ! a too high nutrient content of food is related to the food upake             (r< 0) 
  ! a too low nutrient content of food is related to net growth of the organisms (r> 0)
  misn = r*(runc*insw_vector(r)+max(rrc,rug)*insw_vector(-r))/(1.0D-80+hxc(:))
  r=p_qpc(hx)* hxc(:)-hxp(:)
  misp = r*(runc*insw_vector(r)+max(rrc,rug)*insw_vector(-r))/(1.0D-80+hxc(:))
   
  !nutrient uptake for growth
  rupn=runc*p_qnc(hx)
  rupp=runc*p_qpc(hx)

  !runn>0 : runn= nett part+dissolved nutrient uptake, run<0 :runn=net nutrient release
  runn=min(rumn4+rumnu-min(0.0,misn), rupn+max(0.0,misn))
  runp=min(rump +rumpu-min(0.0,misp), rupp+max(0.0,misp))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Carbon correction: all C which cannot be used for growth due to
  ! lack of nutrients is excreted!
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ren  =   max(ruQ6n+ruQ1n-runc*p_qnc(hx),-runn) *insw_vector(runc) -min(0.0,misn)
  rep  =   max(ruQ6p+ruQ1p-runc*p_qpc(hx),-runp) *insw_vector(runc) -min(0.0,misp)


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of mortality
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  sm  =  p_sd(hx)+ (p_sd2(hx)*hxc(:))/(chm-clm)

  rqt6c  =   hxc(:)* sm*( 1.0D+00- p_pe_R1c)
  rqt6n  =   hxn(:)* sm*( 1.0D+00- p_pe_R1n)
  rqt6p  =   hxp(:)* sm*( 1.0D+00- p_pe_R1p)

  call flux_vector( iiBen, pphxc,ppBenLabileDetritus(p_iQ1(hx),iiC), hxc(:)* sm* p_pe_R1c )
  call flux_vector( iiBen, pphxn,ppBenLabileDetritus(p_iQ1(hx),iiN), hxn(:)* sm* p_pe_R1n )
  call flux_vector( iiBen, pphxp,ppBenLabileDetritus(p_iQ1(hx),iiP), hxp(:)* sm* p_pe_R1p )

  reHI(p_iQ1(hx),:)  =  hxc(:)* sm* p_pe_R1c;

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Fluxes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  call flux_vector(iiBen, pphxn,ppBenthicAmmonium(p_iK4(hx),iiN),  ren*insw_vector( ren))
  r=-ren*rumn4/(1.0D-80+ rumn4+rumnu)*insw_vector(-ren)
  call flux_vector(iiBen, ppBenthicAmmonium(p_iK4(hx),iiN),pphxn,  r)

  call flux_vector(iiBen, pphxp,ppBenthicPhosphate(p_iK1(hx),iiP),  rep* insw_vector(rep) )
  r=-rep*rump/(1.0D-80+ rump+rumpu)*insw_vector(-rep)
  call flux_vector(iiBen, ppBenthicPhosphate(p_iK1(hx),iiP),pphxp,  r)

  r=-ren*rumnu/(1.0D-80+ rumn4+rumnu)*insw_vector(-ren)
  ruQ1c=ruQ1c+r/p_qnUc
  ruQ1n=ruQ1n+r
  r=-rep*rumpu/(1.0D-80+ rump+rumpu)*insw_vector(-rep)
  ruQ1p=ruQ1p+r

  call flux_vector( iiBen, ppBenLabileDetritus(p_iQ1(hx),iiC),pphxc, ruQ1c )
  call flux_vector( iiBen, ppBenLabileDetritus(p_iQ1(hx),iiN),pphxn, ruQ1n )
  call flux_vector( iiBen, ppBenLabileDetritus(p_iQ1(hx),iiP),pphxp, ruQ1p )
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Assigning depends on type of bacteria and layers in which they occur:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ! if (isnan(rep(1))) write(LOGUNIT,*) 'rep Nan in BenBac'

  select case (hx)

    case ( iiH1 )
      call sourcesink_flux_vector( iiBen, pphxc,ppG3c,rrc )
      call flux_vector(iiBen, ppG2o,ppG2o,-( rrc/ 12.0D+00))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Add respiration and excretion to benthic totals:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=a
      rrBTo(:)  =   rrBTo(:)+ rrc/ 12.0D+00
      reBTn(:)  =   reBTn(:)+ ren
      reBTp(:)  =   reBTp(:)+ rep

    case ( iiH2 )
      call sourcesink_flux_vector( iiBen, pphxc,ppG13c,rrc )

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Respiration in anoxic circumstances produces reduced material
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      call flux_vector( iiBen, ppK16r,ppK16r, rrc/ 12.0D+00* p_qro )

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Add respiration and excretion to benthic totals:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      rrATo(:)  =   rrATo(:)+ rrc/ 12.0D+00
      reATn(:)  =   reATn(:)+ ren
      reATp(:)  =   reATp(:)+ rep

  end select

  if (p_sulQ6(hx) <p_suhQ6(hx) .and. p_cuQ6np(hx) > 1.0 )  then
     rqt6c=rqt6c+reR7c
  else
     call flux_vector( iiBen, pphxc,pphxc, -reR7c )
  endif

  call flux_vector( iiBen, pphxc,ppQ6c, rqt6c )
  call flux_vector( iiBen, pphxn,ppQ6n, rqt6n )
  call flux_vector( iiBen, pphxp,ppQ6p, rqt6p )

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of changes due to uptake of detritus in distribution
  ! of state variables (Dx.m is a undetermined source)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call flux_vector(iiBen, ppD6m,ppD6m,( cmm- D6m(:))*( ruQ6c- rqt6c)/(p_small+Q6c(:)))
  call flux_vector(iiBen, ppD7m,ppD7m,( cmm- D7m(:))*( ruQ6n- rqt6n)/(p_small+Q6n(:)))
  call flux_vector(iiBen, ppD8m,ppD8m,( cmm- D8m(:))*( ruQ6p- rqt6p)/(p_small+Q6p(:)))

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
