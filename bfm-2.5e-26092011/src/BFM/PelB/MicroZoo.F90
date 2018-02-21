#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: MicroZoo
!
! DESCRIPTION
!   !

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine MicroZooDynamics(zoo,  ppzooc, ppzoon, ppzoop)
!
! !USES:

  ! For the following Pelagic-states fluxes are defined: B1c, B1n, B1p, O2o, &
  ! R1c, R6c, R1n, R6n, R1p, R6p, N4n, N1p
  ! For the following Pelagic-group-states fluxes are defined: &
  ! PhytoPlankton, MicroZooPlankton
  ! The following Pelagic 1-d global boxvars are modified : flPIR6s
  ! The following Pelagic 1-d global boxvars are used: ETW, eO2mO2, qnB1c, &
  ! qpB1c
  ! The following Pelagic 2-d global boxvars are used: qn_mz, qp_mz, &
  ! The following groupmember vars are used: iiPhytoPlankton
  ! The following constituent constants  are used: iiC, iiN, iiP, iiL
  ! The following 0-d global parameters are used: p_pe_R1c, p_pe_R1n, &
  ! p_pe_R1p, p_small
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
  use constants,ONLY:p_qnUc
#IFDEF NOPOINTERS
  use mem,  ONLY: D3STATE
#ELSE
  use mem, ONLY: D3STATE, B1c,R2c,R3c, PhytoPlankton, MicroZooPlankton
#ENDIF
  use mem, ONLY: ppB1c, ppB1n, ppB1p, ppO2o, ppO3c, ppR6c, &
    ppR1n, ppR6n, ppR1p, ppR6p, ppR1c, ppR1n, ppN1p, ppPhytoPlankton, & 
    ppMicroZooPlankton,Depth, NO_BOXES,flR3R2c, &
    flPIR6s, ETW, eO2mO2, qnB1c, qpB1c, qn_mz, qp_mz, jnetMiZc,jeZIR6n,jeZIR6p, &
    jeZIDIn,jeZIDIp, iiPhytoPlankton, iiMicroZooPlankton, iiC, iiN, iiP, iiL,iiS, &
    iiPel, iiP6, flux_vector,fixed_quota_flux_vector
  use mem_Param,  ONLY: p_pe_R1c, p_pe_R1n, p_pe_R1p, p_small,check_fixed_quota
  use mem_MicroZoo
  use mem_Phaeo, ONLY:CALC_FOOD_MICROZOO,CALC_GRAZING_MICROZOO

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:eTq_vector, MM_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: eTq_vector, MM_vector,insw_vector


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE

! !INPUT:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer,intent(IN)  :: zoo
  integer,intent(IN) :: ppzooc
  integer,intent(IN) :: ppzoon
  integer,intent(IN) :: ppzoop

!  
!
! !AUTHORS
!   ERSEM group, Hanneke Baretta-Bekker
!
!
!
! !REVISION_HISTORY
!   by Piet Ruardij at Thu Mar 16 08:34:04 CET 2006
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
  real(RLEN),dimension(:),pointer :: zooc
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer  :: i,j,iout
  real(RLEN)                      :: r_scalar
  real(RLEN),dimension(NO_BOXES)  :: put_u
  real(RLEN),dimension(NO_BOXES)  :: et
  real(RLEN),dimension(NO_BOXES)  :: eO2
  real(RLEN),dimension(NO_BOXES)  :: rumc
  real(RLEN),dimension(NO_BOXES)  :: rumn
  real(RLEN),dimension(NO_BOXES)  :: rump
  real(RLEN),dimension(NO_BOXES)  :: rugc
  real(RLEN),dimension(NO_BOXES)  :: rugn
  real(RLEN),dimension(NO_BOXES)  :: rugp
  real(RLEN),dimension(NO_BOXES)  :: runc
  real(RLEN),dimension(NO_BOXES)  :: runn
  real(RLEN),dimension(NO_BOXES)  :: runp
  real(RLEN),dimension(NO_BOXES)  :: efood
  real(RLEN),dimension(NO_BOXES)  :: rrsc
  real(RLEN),dimension(NO_BOXES)  :: rrac
  real(RLEN),dimension(NO_BOXES)  :: reac
  real(RLEN),dimension(NO_BOXES)  :: rean
  real(RLEN),dimension(NO_BOXES)  :: reap
  real(RLEN),dimension(NO_BOXES)  :: rdc
  real(RLEN),dimension(NO_BOXES)  :: ruB1c
  real(RLEN),dimension(NO_BOXES)  :: ruPIc
  real(RLEN),dimension(NO_BOXES)  :: ruZIc
  real(RLEN),dimension(NO_BOXES)  :: rumR3c
  real(RLEN),dimension(NO_BOXES)  :: rumB1c
  real(RLEN),dimension(NO_BOXES,iiPhytoPlankton)  :: rumPIc
  real(RLEN),dimension(NO_BOXES,iiPhytoPlankton)  :: food
  real(RLEN),dimension(NO_BOXES,iiPhytoPlankton)  :: suPI
  real(RLEN),dimension(NO_BOXES,iiMicroZooPlankton)  :: rumZIc
  real(RLEN),dimension(NO_BOXES)  :: rric
  real(RLEN),dimension(NO_BOXES)  :: rr1c
  real(RLEN),dimension(NO_BOXES)  :: rr6c
  real(RLEN),dimension(NO_BOXES)  :: rr1p
  real(RLEN),dimension(NO_BOXES)  :: rr1n
  real(RLEN),dimension(NO_BOXES)  :: rr6p
  real(RLEN),dimension(NO_BOXES)  :: renp
  real(RLEN),dimension(NO_BOXES)  :: rr6n
  real(RLEN),dimension(NO_BOXES)  :: renn
  real(RLEN),dimension(NO_BOXES)  :: renc
  real(RLEN),dimension(NO_BOXES)  :: pu_ra
  real(RLEN),dimension(NO_BOXES)  :: r
  real(RLEN),dimension(NO_BOXES)  :: tfluxc
  real(RLEN),dimension(NO_BOXES)  :: tfluxn
  real(RLEN),dimension(NO_BOXES)  :: tfluxp
  real(RLEN),dimension(:),pointer :: phytoc,phytonps,micro
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  Copy  state var. object in local var
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  zooc => D3STATE(ppzooc,:)

  tfluxc=0.0;
  tfluxn=0.0;
  tfluxp=0.0;

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Temperature effect
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  et  =   eTq_vector( ETW(:),  p_q10(zoo))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Oxygen limitation
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! If there is saturation (eO2mO2i==1) eO2 is 1!
  eO2  = min(1.0,(1.0D+00+ p_chro(zoo))  * MM_vector(  eO2mO2(:),   p_chro(zoo)))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Available food, etc...
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rumB1c  =   p_suB1(zoo)* B1c(:)* B1c(:)/( B1c(:)+ p_minfood(zoo))
  rumc    =   rumB1c
  rumn    =   rumB1c* qnB1c(:)
  rump    =   rumB1c* qpB1c(:)

  food=0.0;
  do i = 1 , iiPhytoPlankton
    j=i;if (p_type(i)>0 ) j=p_type(i)
    r=p_suPI(zoo,i)
    !if phyto is NOT phaeocystis output (r) is equal to input (r)
    call PhaeocystisCalc(CALC_FOOD_MICROZOO,i,r,r,p_suPI(zoo,i))
    suPI(:,i)=r
    phytoc => PhytoPlankton(i,iiC)
    food(:,j)=food(:,j)+r*phytoc
  enddo
  do i = 1 , iiPhytoPlankton
    if (p_type(i)>0 ) food(:,i)=food(:,p_type(i))
  enddo
  do i = 1 , iiPhytoPlankton
    if ( p_suPI(zoo,i) .gt.0.0) then
      phytoc => PhytoPlankton(i,iiC)
      rumPIc(:, i) = suPI(:,i)* phytoc
      rumPIc(:, i) =rumPIc(:, i) * food(:,i)/(food(:,i) + p_minfood(zoo))
      rumc  =   rumc+ rumPIc(:, i)
      phytonps=>PhytoPlankton(i,iiN)
      rumn  =   rumn+ rumPIc(:, i)* phytonps/(1.0D-80+phytoc)
      phytonps=>PhytoPlankton(i,iiP)
      rump  =   rump+ rumPIc(:, i)* phytonps/(1.0D-80+phytoc)
      if ( i==iiP6) then
      !TEP material in colony`
        where (insw_vector(phytoc) >1.0D-80 )
         rumR3c =  rumPIc(:,i) *R3c/(1.0D-80+ phytoc)
         rumc= rumc+ rumR3c
        endwhere
      endif
    endif
 enddo

  do i = 1 , ( iiMicroZooPlankton)
    micro =>MicroZooPlankton(i,iiC)
    rumZIc(:, i) = p_suZI(zoo,i)* micro* micro/( micro+  p_minfood(zoo))
    rumc  =   rumc+ rumZIc(:, i)
    rumn  =   rumn+ rumZIc(:, i)* qn_mz(i,:)
    rump  =   rump+ rumZIc(:, i)* qp_mz(i,:)
  end do

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Uptake
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rumc=rumc+p_small
  efood  =   MM_vector(  rumc,  p_chuc(zoo))
  rugc  =   p_sum(zoo)* et* zooc* efood* eO2

  put_u  =   rugc/ rumc


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Fluxes into microzooplankton
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ruB1c  =   put_u* rumB1c
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzooc,ppB1c,ppzooc, ruB1c ,tfluxc)
  
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoon,ppB1n,ppzoon, &
                                                       ruB1c* qnB1c(:),tfluxn)
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoop,ppB1p,ppzoop, &
                                                       ruB1c* qpB1c(:),tfluxp)
  rugc  =   ruB1c
  rugn  =   ruB1c* qnB1c(:)
  rugp  =   ruB1c* qpB1c(:)

  do i = 1 , iiPhytoPlankton
    if (  p_suPI(zoo,i) > 0.0 ) then
      ruPIc  =   put_u* rumPIc(:, i)
      !if phyto is NOT phaeocystis output (ruPIc) is equal to input (ruPIc)
      call PhaeocystisCalc(CALC_GRAZING_MICROZOO,i,ruPIc,ruPIc,p_suPI(zoo,i))
      iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzooc,ppPhytoPlankton(i,iiC),&
                                                      ppzooc, ruPIc ,tfluxc)
      rugc  =   rugc+ ruPIc
      phytoc => PhytoPlankton(i,iiC);phytonps=>PhytoPlankton(i,iiN)
      r=ruPIc* phytonps/(1.0D-80+phytoc)
      rugn  =   rugn+ r
      iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoon,ppPhytoPlankton(i,iiN),&
                                           ppzoon, r,tfluxn)
      phytonps=>PhytoPlankton(i,iiP)
      r=ruPIc* phytonps/(1.0D-80+phytoc)
      rugp  =   rugp+ r
      iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoop,ppPhytoPlankton(i,iiP),&
                                           ppzoop, r,tfluxp)
      ! Chl is transferred to the sink
      phytonps=>PhytoPlankton(i,iiL)
      r=ruPIc* phytonps/(1.0D-80+phytoc)
      call flux_vector( iiPel, ppPhytoPlankton(i,iiL),ppPhytoPlankton(i,iiL), -r  )
      j=ppPhytoPlankton(i,iiS)
      if ( j.gt.0 ) then
        ! P1s is directly transferred to R6s
        ! PhytoPlankton[i].s -> R6.s = ruPIc * qsPc[i];
        phytonps=>PhytoPlankton(i,iiS)
        flPIR6s(i,:)  =   flPIR6s(i,:)+ ruPIc* phytonps/(1.0D-80+phytoc)
      end if
      if ( i==iiP6) then
        !assumed is that all R3 not selected as food.
        flR3R2c=flR3R2c+ruPIc/(1.0D-80+rumPIc(:, i)) * rumR3c
      endif

    endif
  end do


  do i = 1 , ( iiMicroZooPlankton)

    ruZIc  =   put_u* rumZIc(:, i)
    ! intra-group predation is not computed
    if ( i/= zoo) then
      iout= fixed_quota_flux_vector( check_fixed_quota,iiPel,ppzooc,ppMicroZooPlankton(i,iiC),&
                                                          ppzooc, ruZIc,tfluxc )
      iout= fixed_quota_flux_vector( check_fixed_quota,iiPel,ppzoon,ppMicroZooPlankton(i,iiN),&
                                              ppzoon, ruZIc* qn_mz(i,:) ,tfluxn)
      iout= fixed_quota_flux_vector( check_fixed_quota,iiPel,ppzoop,ppMicroZooPlankton(i,iiP),&
                                              ppzoop, ruZIc* qp_mz(i,:) ,tfluxp)
    end if

    rugc  =   rugc+ ruZIc
    rugn  =   rugn+ ruZIc* qn_mz(i,:)
    rugp  =   rugp+ ruZIc* qp_mz(i,:)
  end do

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Rest respiration
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


  rrsc  =   p_srs(zoo)* et* zooc 

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Activity excretion
  ! It is assumed that a larger part of the loc-part in the food (1.0-p_peaR1) is
  ! used and digested by the microzooplankton. This implies that a larger
  ! part of the excretion products will consist of R6, which if using the parameters
  ! describing the content of a cell in terms of LOC  (p_pe_R1*) that these
  ! products will contain less nutrients.
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  r_scalar=p_pu_ea(zoo)*(p_peaR1(zoo)*p_pe_R1c +(1.0-p_peaR1(zoo))*(1.0-p_pe_R1c)) 
  reac  =   rugc* r_scalar
  r_scalar=p_pu_ea(zoo)*(p_peaR1(zoo)*p_pe_R1n +(1.0-p_peaR1(zoo))*(1.0-p_pe_R1n)) 
  rean  =   rugn*r_scalar 
  r_scalar=p_pu_ea(zoo)*(p_peaR1(zoo)*p_pe_R1p +(1.0-p_peaR1(zoo))*(1.0-p_pe_R1p)) 
  reap  =   rugp* r_scalar

  ! All nutrients freed by respiration can be reused to build new biomass
  ! If there is a shortage of nutrients activity respiration will be increased 
  ! to free more nutrients.
  pu_ra=p_pu_ra(zoo)
  where ( put_u > 0.1)
    r =  rrsc + min((rugn-rean)/ p_qnc(zoo), (rugp-reap)/ p_qpc(zoo))
    pu_ra  =   max( pu_ra,  1.0D+00- r/ (p_small+rugc-reac))
  end where

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  !       Fluxes from microzooplankton
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !  activity, total respiration fluxes
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    rrac  =   rugc* pu_ra

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    ! Mortality (rdc) + Excretion (reac)
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    rdc  =  (( 1.0D+00- eO2)* p_sdo(zoo)+ p_sd(zoo))* zooc

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    ! Fluxes due to mortality and excretion
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    rric  =   reac+ rdc
    r_scalar=p_peaR1(zoo)*p_pe_R1c +(1.0-p_peaR1(zoo))*(1.0-p_pe_R1c) 
    rr1c  =   rric* p_peaR1(zoo)*p_pe_R1c/r_scalar
    rr6c  =   rric -rr1c

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    ! Dissolved nutrient dynamics
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    runc  =   max(  0.0D+00,  rugc-reac-rrac)
    runn  =   max(  0.0D+00,  rugn-rean+ rrsc* qn_mz(zoo, :))
    runp  =   max(  0.0D+00,  rugp-reap+ rrsc* qp_mz(zoo, :))

    ! Carbon which will be excreted/respired when internal quota (N/C) are below optimum
    ! renc has te be renc >=rrsc
    renc  =   max(0.0,(-runn/( p_small+ runc)/ p_qnc(zoo)+1.0),   &
                    (-runp/( p_small+ runc)/ p_qpc(zoo)+1.0))* runc


    ! take in consideration that a (small) part of runC is used to make urea
    runc=runc-renc
    renn=max(0.0,runn-p_qnc(zoo)*max(0.0,runc))/(1.0-p_qnc(zoo)/p_qnUc)
    runc=runc-renn/p_qnUc
    renp=max(0.0,runp-p_qpc(zoo)*max(0.0,runc));


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  !     Nutrient dynamics in microzooplankton
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Organic Nitrogen dynamics
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  r_scalar=p_peaR1(zoo)*p_pe_R1n +(1.0-p_peaR1(zoo))*(1.0-p_pe_R1n) 
  rr1n  =   rean* p_peaR1(zoo)*p_pe_R1n/r_scalar   + rdc* qn_mz(zoo,:)* p_pe_R1n
  rr6n  =   rean+ rdc* qn_mz(zoo,:) -rr1n

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Organic Phosphorus dynamics
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  r_scalar=p_peaR1(zoo)*p_pe_R1p +(1.0-p_peaR1(zoo))*(1.0-p_pe_R1p) 
  rr1p  =   reap* p_peaR1(zoo)*p_pe_R1p/r_scalar+  rdc* qp_mz(zoo,:)* p_pe_R1p
  rr6p  =   reap+ rdc* qp_mz(zoo,:) -rr1p

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Fluxes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  !r=min(rrsc,renc)
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzooc,ppzooc,ppO3c, &
                                                  rrac+rrsc,tfluxc )
  call flux_vector( iiPel, ppO2o,ppO2o,- (rrac+rrsc)/ 12.0D+00 )

  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzooc,ppzooc,ppR6c, rr6c+renc,tfluxc)
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoon,ppzoon,ppR6n, rr6n ,      tfluxn)
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoop,ppzoop,ppR6p, rr6p ,      tfluxp)

  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzooc,ppzooc,ppR1c, rr1c+renn/p_qnUc ,tfluxc)
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoop,ppzoop,ppR1p, rr1p            ,tfluxp)
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoon,ppzoon,ppR1n, rr1n+ renn       ,tfluxn)
  iout= fixed_quota_flux_vector( check_fixed_quota,iiPel, ppzoop,ppzoop,ppN1p, renp ,tfluxp)

  r=rugc-rrac-reac
  jnetMiZc(1)=jnetMiZc(1)+sum(Depth(:)*r)
  jeZIDIn(1)= jeZIDIn(1)+sum(Depth(:)*(rr1n+renn))
  jeZIDIp(1)= jeZIDIp(1)+sum(Depth(:)*(rr1p+renp))
  jeZIR6n(1)= jeZIR6n(1)+sum(Depth(:)*rr6n)
  jeZIR6p(1)= jeZIR6p(1)+sum(Depth(:)*rr6p)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! test if assumed fixed-quota fits 
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  r=tfluxc*p_qnc(zoo)
  iout= fixed_quota_flux_vector( check_fixed_quota,-iiN,0,0,0,r,tfluxn,"MicroZoo")
  r=tfluxc*p_qpc(zoo)
  iout= fixed_quota_flux_vector( check_fixed_quota,-iiP,0,0,0,r,tfluxp,"MicroZoo")

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
