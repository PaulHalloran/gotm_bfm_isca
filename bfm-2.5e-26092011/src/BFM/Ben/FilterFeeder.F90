#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: FilterFeeder
!
! DESCRIPTION
!   This process describes the carbon dynamics and associated
!   nutrient dynamics in benthic organism Y3 (suspension feeders)
!   Y3 is handled separately because it also feeds from the water column.
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine FilterFeederDynamics
!
! !USES:

  ! For the following Benthic-states fluxes are defined: Y3c, Y3n, Y3p, &
  ! G2o, K4n, K1p, D9m
  ! The following Benthic-states are used (NOT in fluxes): D1m
  ! The following Benthic 1-d global boxvars are modified : rrBTo, reBTn, &
  ! reBTp, 
  ! The following Benthic 1-d global boxvars got a value: jPIY3c, jZIY3c, &
  ! jY3RIc, jY3RIn, jY3RIp, jY3RIs
  ! The following Benthic 1-d global boxvars are used: ETW_Ben, PI_Benc, RI_Fc, &
  ! ZI_Fc, PI_Benn, PI_Benp, PI_Bens, ZI_Fn, ZI_Fp, RI_Fn, RI_Fp, RI_Fs
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
  use constants,  ONLY:p_qnUc
#IFDEF NOPOINTERS
  use mem,  ONLY: D2STATE
#ELSE
  use mem, ONLY: Y3c, Y3n, Y3p ,Yy3c
#ENDIF
  use mem, ONLY: ppY3c, ppY3n, ppY3p, ppYy3c, ppQ6c, ppQ6n, ppQ6p, ppG2o, &
    ppQ1c, ppQ1n,O2o_Ben, ppK1p, iiBen, ppG3c,iiP6, iiY3,flux_vector,   &
    rrBTo, reBTp,jPIY3c, jZIY3c, jRIY3c, jY3RIc, jY3RIn, jY3RIp, jY3RIs, &
    jY3QIc,jY3QIs, iiPhytoPlankton,jnetY3c,jnetYy3c,jCaCO3Y3c,jY3O3c,jO2Y3o,  &
    jRIQIc,jRIQIn,jRIQIp,jRIQIs,jY3N1p,jY3N4n,sediPI_Ben,sediR6_Ben,rugYIc, &
    ETW_Ben, PI_Benc, PI_Benn, PI_Benp, PI_Bens, ZI_Fc, &
    RI_Fc, ZI_Fn, ZI_Fp, RI_Fn, RI_Fp, RI_Fs,&
    R3_Benc,R3_Benn,R3_Benp,NO_BOXES_XY, Depth_Ben, puPIY3, &
    efilP6Y3,pyfoodY3,ctfPm2c,ctfZm2c,ctfRm2c,cZ2m2c

! Input: Calculated in PelForcingFoBen-------------------------------------
!   ETW_Ben : temperature
!   PI_Benc : potential food phytoplankton
!   PI_Benn :
!   PI_Benp : 
!   PI_Bens :
!   ZI_Fc   : potential food zooplankton
!   ZI_Fn   :
!   ZI_Fp   :
!   RI_Fc   : potential food detritus
!   RI_Fn   :
!   RI_Fp   :
!   RI_Fs   :
!  R3_Benc   : potential food TEP
!  R3_Benn   :
!  R3_Benp   :
!  Depth_Ben :depth(height) lowest layer
! sediPI_Ben   : sedimentation rate of Phyto
! sediR6_Ben   : sedimetation rate of detritus
!  efilP6Y3  : filter limitaion due too high Phaeocystis concentration
!  ctfPm2c   : total amount of Phyto in pelagic per m2 ( used to limit rates dependng on time step)
!  ctfZm2c   :total amount of zooplankton in pelagic per m2
!  ctfRm2c   :total amount of detritus in pelagic per m2
!  cZ2m2c    :total amount of young larvae in pelagic per m2 (used to caluclate quotient young/total filterfeeders)
! Output: ---- Used in BentoPelCoup.F90
!  pyfoodY3  : part of larvae available for other benthic orgranisms
!  jPIY3c   : flux phyto ->filterfeeder (mgC/m2/d)
!  jZIY3c   : flux zoo ->filterfeeder (mgC/m2/d)
!  jRIY3c   : flux detritus ->filterfeeder (mgC/m2/d)
!  jY3RIc   : flux  filterfeeder -> detritus (mgC/m2/d)
!  jY3RIn   : flux  filterfeeder -> detritus (mmolN/m2/d)
!  jY3RIp   : flux  filterfeeder -> detritus (mmolP/m2/d)
!  jY3RIs   : flux  filterfeeder -> detritus (mmolSi/m2/d)
!  jY3QIc   : flux  filterfeeder -> benthic detritus (mgC/m2/d)
!  jY3QIs   : flux  filterfeeder -> benthic detritus (mgC/m2/d)
! jCaCO3Y3c : shellformation
! jY3O3c    :  CO2 release to pelagic due to respiration 
! jO2Y3o    :  O2 uptake from pelagic due to respiration 
! jRIQIc    :  pseudofaeces production form detritus to benthic detritus
! jRIQIn   :   pseudofaeces production form detritus to benthic detritus
! jRIQIp   :   pseudofaeces production form detritus to benthic detritus
! jRIQIs   :   pseudofaeces production form detritus to benthic detritus
! jY3N1p   :   P flux to pelagic
! jY3N4n   :   N (=urea) flux to pelagic
! diagnostic:
!  puPIY3   : relative availability for food corrected for power distribution near sediment 
!  jnetY3c  : nett production of filterfeeders
!  jnetYy3c : nett production of filterfeeders
!  rugYIc  :  rate uptake gros of filterfeeders

 
  use mem_Param,  ONLY: p_pe_R1c, p_pe_R1n, p_pe_R1p, &
                        p_small,CalcYy3,p_s_max
  use LimitRates, ONLY:LimitChange_vector
  use mem_FilterFeeder
! use mem,  ONLY: Output2d_1,Output2d_2,Output2d_3, Output2d_4

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:eTq_vector, eramp_vector, &
  ! MM_vector, PartQ_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: eTq_vector, MM_vector, insw_vector

!
! !AUTHORS
!   P.Ruardij ,        W. Ebenhoeh and C. Kohlmeier 
!
!
! !REVISION_HISTORY
!   !
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
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer  :: i
  real(RLEN) :: clu
  real(RLEN) :: corr_max
  real(RLEN) :: max_change_per_step
  real(RLEN)  :: eNC
  real(RLEN)  :: ePC
  real(RLEN),dimension(NO_BOXES_XY)  :: R6_corr  !correction for power distitustion of R6 ln layer near sediment
  real(RLEN),dimension(NO_BOXES_XY)  :: et       !temperature correction
  real(RLEN),dimension(NO_BOXES_XY)  :: eO       !correction for low oxygen
  real(RLEN),dimension(NO_BOXES_XY)  :: foodpm2
  real(RLEN),dimension(NO_BOXES_XY)  :: food
  real(RLEN),dimension(iiPhytoPlankton,NO_BOXES_XY)  :: food_PIc ! calculated food 
  real(RLEN),dimension(NO_BOXES_XY)  :: food_PT                  ! calculated phyto food
  real(RLEN),dimension(NO_BOXES_XY)  :: food_ZI                  ! calculated zoo food
  real(RLEN),dimension(NO_BOXES_XY)  :: food_RI                  ! calculated detritus food
  real(RLEN),dimension(NO_BOXES_XY)  :: food_R3                  ! calculated TEP food included in Phaeo colonies
  real(RLEN),dimension(NO_BOXES_XY)  :: suf                      ! calculated specific uptake (uptake/food)
  real(RLEN),dimension(NO_BOXES_XY)  :: rugc                     ! food uptake
  real(RLEN),dimension(NO_BOXES_XY)  :: sunPI                    ! specific Phyto uptake (m2 food/m2 biomass Y3)
  real(RLEN),dimension(NO_BOXES_XY)  :: sunR3                    ! specific TEP uptake 
  real(RLEN),dimension(NO_BOXES_XY)  :: sunZI                    ! specific Zoo uptake
  real(RLEN),dimension(NO_BOXES_XY)  :: sunR6                    ! specific detritus uptake
  real(RLEN),dimension(NO_BOXES_XY)  :: se_uPI                   ! specific excretion
  real(RLEN),dimension(NO_BOXES_XY)  :: se_uR3                   ! specific excretion
  real(RLEN),dimension(NO_BOXES_XY)  :: se_uZI                   ! specific excretion
  real(RLEN),dimension(NO_BOXES_XY)  :: se_uR6                   ! specific excretion
  real(RLEN),dimension(NO_BOXES_XY)  :: choice
  real(RLEN),dimension(NO_BOXES_XY)  :: runc                     ! rate uptake net C
  real(RLEN),dimension(NO_BOXES_XY)  :: runn                     ! rate uptake net P
  real(RLEN),dimension(NO_BOXES_XY)  :: runp                     ! rate uptake net N
  real(RLEN),dimension(NO_BOXES_XY)  :: rrc                      ! rate respiration C
  real(RLEN),dimension(NO_BOXES_XY)  :: sm                       ! specific mortality
  real(RLEN),dimension(NO_BOXES_XY)  :: renc                     ! rate exretion netto C
  real(RLEN),dimension(NO_BOXES_XY)  :: renn                     ! rate exretion netto C
  real(RLEN),dimension(NO_BOXES_XY)  :: renp                     ! rate exretion netto C
  real(RLEN),dimension(NO_BOXES_XY)  :: retR6c                   ! rate excretion total slow degrading detritus C
  real(RLEN),dimension(NO_BOXES_XY)  :: retR6n                   ! rate excretion total slow degrading detritus N
  real(RLEN),dimension(NO_BOXES_XY)  :: retR6p                   ! rate excretion total slow degrading detritus P
  real(RLEN),dimension(NO_BOXES_XY)  :: retQ6c               ! rate excretion total slow degrading benthic detritus C
  real(RLEN),dimension(NO_BOXES_XY)  :: retQ6n               ! rate excretion total slow degrading benthic detritus N
  real(RLEN),dimension(NO_BOXES_XY)  :: retQ6p               ! rate excretion total slow degrading benthic detritus P
  real(RLEN),dimension(NO_BOXES_XY)  :: rePIc                ! rate excretion phytoplankton C
  real(RLEN),dimension(NO_BOXES_XY)  :: rePIn                ! rate excretion phytoplankton C
  real(RLEN),dimension(NO_BOXES_XY)  :: rePIp                ! rate excretion phytoplankton C
  real(RLEN),dimension(NO_BOXES_XY)  :: reZIc                ! rate excretion zooplankton C
  real(RLEN),dimension(NO_BOXES_XY)  :: reZIn                ! rate excretion zooplankton C
  real(RLEN),dimension(NO_BOXES_XY)  :: reZIp                ! rate excretion zooplankton C
  real(RLEN),dimension(NO_BOXES_XY)  :: reR3c                ! rate excretion TEP C
  real(RLEN),dimension(NO_BOXES_XY)  :: reR3n                ! rate excretion TEP N
  real(RLEN),dimension(NO_BOXES_XY)  :: reR3p                ! rate excretion TEP P
  real(RLEN),dimension(NO_BOXES_XY)  :: reR6c                   ! rate excretion slow degrading detritus C
  real(RLEN),dimension(NO_BOXES_XY)  :: reR6n                   ! rate excretion slow degrading detritus N
  real(RLEN),dimension(NO_BOXES_XY)  :: reR6p                   ! rate excretion slow degrading detritus p
  real(RLEN),dimension(NO_BOXES_XY)  :: reQ6c                   ! rate excretion slow benthic degrading detritus C
  real(RLEN),dimension(NO_BOXES_XY)  :: reQ6n                   ! rate excretion slow benthic degrading detritus N
  real(RLEN),dimension(NO_BOXES_XY)  :: reQ6p                   ! rate excretion slow benthic degrading detritus P
  real(RLEN),dimension(NO_BOXES_XY)  :: ruPIc                ! rate uptake Phytoplankton C
  real(RLEN),dimension(NO_BOXES_XY)  :: ruPIn                ! rate uptake Phytoplankton N
  real(RLEN),dimension(NO_BOXES_XY)  :: ruPIp                ! rate uptake Phytoplankton P
  real(RLEN),dimension(NO_BOXES_XY)  :: ruPIs                ! rate uptake Phytoplankton Si
  real(RLEN),dimension(NO_BOXES_XY)  :: ruR3c                ! rate uptake TEP C
  real(RLEN),dimension(NO_BOXES_XY)  :: ruR3n                ! rate uptake TEP N
  real(RLEN),dimension(NO_BOXES_XY)  :: ruR3p                ! rate uptake TEP P
  real(RLEN),dimension(NO_BOXES_XY)  :: ruZIc                ! rate uptake Zooplankton C
  real(RLEN),dimension(NO_BOXES_XY)  :: ruZIn               ! rate uptake Zooplankton N
  real(RLEN),dimension(NO_BOXES_XY)  :: ruZIp               ! rate uptake Zooplankton P
  real(RLEN),dimension(NO_BOXES_XY)  :: RTc                  ! total available R6c for food uptake 
  real(RLEN),dimension(NO_BOXES_XY)  :: ruR6c                ! rate uptake detritus C
  real(RLEN),dimension(NO_BOXES_XY)  :: ruR6n                ! rate uptake detritus N
  real(RLEN),dimension(NO_BOXES_XY)  :: ruR6p                ! rate uptake detritus P
  real(RLEN),dimension(NO_BOXES_XY)  :: ruR6s                ! rate uptake detritus Si
  real(RLEN),dimension(NO_BOXES_XY)  :: su                   ! specific uptake
  real(RLEN),dimension(NO_BOXES_XY)  :: r,s                  ! --help variables---
  real(RLEN),dimension(NO_BOXES_XY)  :: fluc                 ! -- help variables--
  real(RLEN),dimension(NO_BOXES_XY)  :: vum                  ! volume filtered for uptake maximum (m3 m-2/mgC)
  real(RLEN),dimension(NO_BOXES_XY)  :: erqu       ! ratio between filtering respiration loss  and max. uptake (-)
  real(RLEN),dimension(NO_BOXES_XY)  :: active               ! if enough foood active > 0.0
  real(RLEN),dimension(NO_BOXES_XY)  :: fsat                 ! filtering saturation : at high feed levels 
                                                             ! less filtering is necessary 
  real(RLEN),dimension(NO_BOXES_XY)  :: runmc                !  rate uptake net maximal C

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  real(RLEN), external  :: GetDelta

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Physiological temperature response
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  et  =   eTq_vector(  ETW_Ben(:),  p_q10)

  eO=max(0.0,1.0D+00-exp(-(O2o_Ben(:)-0.5*p_clO2o)/(0.5*p_clO2o))) 

  !max. density of food in layer filtered compared with average density in layer
  corr_max=p_max/GetDelta()
  !max. amount per timestep which can be eaten from whole pelagic.
  max_change_per_step=p_s_max/GetDelta()

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate total food Cfluxes!
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  clu=p_clu+p_small;
  food  =   p_small

  ! For phytoplankton:

  food_PT=0.0
  do i=1,iiPhytoPlankton
     r =  PI_Benc(i,:) * MM_vector(  PI_Benc(i,:),  clu)
     call CorrectConcNearBed_vector(Depth_Ben(:), sediPI_Ben(i,:), p_height,  corr_max,NO_BOXES_XY,  puPIY3(i,:))
     
     food_PIc(i,:)=r*puPIY3(i,:)*p_PI
     food_PT(:)  = food_PT(:)+ food_PIc(i,:)
     if ( i==iiP6 ) then
        r =  R3_Benc* MM_vector(  PI_Benc(i,:),  clu)
        food_R3=r*puPIY3(i,:)*p_PI
        food=food+food_R3
     endif
  enddo
  food  =   food  + food_PT(:)

  if (isnan((food_PT(1)))) then
    do i=1,iiPhytoPlankton
       write(LOGUNIT,*) 'food_PIc:', i,food_PIc(i,1)
    enddo
  endif

  ! For microzooplankton:

  food_ZI  =   p_ZI * ZI_Fc(:) * MM_vector(  ZI_Fc(:),  clu)
  food  =   food+ food_ZI


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Detritus, (if eaten) first calculate available amount
  ! and add it to the total amount of food
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  r=   RI_Fc(:)* MM_vector(  RI_Fc(:),  clu)
  call CorrectConcNearBed_vector(Depth_Ben(:), sediR6_Ben(:), p_height,  corr_max,NO_BOXES_XY, R6_corr)
  RTc=r*R6_corr
  food_RI=RTc*p_R6
  food  =   food+ food_RI
  vum=p_vum* efilP6Y3

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  !  Food uptake as in zooplankton:
  !  using the modifed Holling response equation which tkae in account
  !  the maximum growth rate and the volume filtered.
  !  Further is assumed that the filterfeeder (nearly) stop filtering 
  !  as soon as the costs for filtering  are lower than the  profit
  !  For this we solve the next equation in which r is the unknown:
  !    (left side == profit , right side=costs)
  !    r* p_sum* MM_vector(  vum* food,  r* p_sum)* Y3c(:)*runmc = p_sra *r 
  !  If r > 1.0 : there is enough food to grow
  !  if r < 1.0 : there is balance between costs and profit if  
  !  r*erqu*su is larger than the rest respiration.
  !
  !  It is assumed that the detritus sedimentation is defined as a netto 
  !  process (p_bursel << P_sediR6). Therefor it assumed that 
  ! filterfeeders do noet eat benthic detritus (Q6c).  
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  erqu = p_sra/p_sum;

  runmc= (1.0D+00-(p_pueR6*food_RI+p_puePI*food_PT +p_pueR3*food_R3  &
              +p_pueZI*food_ZI)/food ) * (1.0D+00-p_pur);

  active= runmc /erqu - p_sum/(p_small+ vum *food);

  r=min(1.0D+00,max(1.0D-6,active));

  ! Calculate relative uptake
  su= 1.0D+00/( 1.0D+00/(p_small+ r* vum *food * et *eO ) + 1.0D+00/(p_small+ p_sum *et *eO  ))  ;
  ! The minimal uptake rate is equal to rest respiration.
  ! With filtering the filterfeeder provide himself also with oxygen.
  rugc= su *max(p_small,Y3c(:))
  ! filtering saturation ( high at low , low at hight food)
  fsat=su/(p_small+ et*eO*vum*food);
  ! Calculate cost of energy for filtering based on realized rate of uptake.
  rrc = max(eO * p_sum*erqu*fsat, p_srs)* max(p_small,Y3c(:))* et
  
  foodpm2 =food*Depth_Ben

  !diagnostic output:
  rugYIc(iiY3,:)=rugc

  ! Relative growth rate corrected for actual amount of food:

  ! suf= food uptake per unit of food
  suf  =   rugc/ foodpm2

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Net uptake:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  sunPI  =   suf*( 1.0D+00- p_puePI)
  sunR3  =   suf*( 1.0D+00- p_pueR3)
  sunZI  =   suf*( 1.0D+00- p_pueZI)
  sunR6  =   suf*( 1.0D+00- p_pueR6)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Execreted part:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  se_uPI  =   suf- sunPI
  se_uR3  =   suf- sunR3
  se_uZI  =   suf- sunZI
  se_uR6  =   suf- sunR6

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! CALCULATION OF UPTAKE RATE:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! calculation of quotum N/C P/C in exretion product slow degrading detritus
  ! (R6)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  eNC=(1.0D+00-p_pe_R1n)/(1.0D+00-p_pe_R1c)
  ePC=(1.0D+00-p_pe_R1p)/(1.0D+00-p_pe_R1c)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Pelagic Phytoplankton:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ruPIc  =   0.0 ; rePIc=0.0;  ruR3c  =   0.0 ; reR3c=0.0
  ruPIn  =   0.0 ; rePIn=0.0;  ruR3n  =   0.0 ; reR3n=0.0
  ruPIp  =   0.0 ; rePIp=0.0;  ruR3p  =   0.0 ; reR3p=0.0
  ruPIs  =   0.0

  ! calculate max uptake  of food/m2. Compare uptake pressure
  ! with food in whole water column and limit if necessary
  fluc=food_PT *suf *Depth_Ben ! pot. uptken food  mgC/m2
  call LimitChange_vector(1,fluc,ctfPm2c,max_change_per_step,r)
  
  do i=1,iiPhytoPlankton
    ! calculate rel max uptake  of /m3/d. Check uptake pressure
    ! in lowest layer and limit if necessary
    fluc = suf*food_PIc(i,:)
    call LimitChange_vector(1,fluc, PI_Benc(i,:),corr_max,s)
    choice=food_PIc(i,:)* Depth_Ben/(p_small + PI_Benc(i,:))*min(r,s)
    ! all uptake below are uptakes per square m2  dimension choice :m
    jPIY3c(i,:) =       PI_Benc(i,:)* suf* choice
    ruPIc  = ruPIc  +   PI_Benc(i,:)* suf* choice
    ruPIn  = ruPIn  +   PI_Benn(i,:)* suf* choice
    ruPIp  = ruPIp  +   PI_Benp(i,:)* suf* choice
    ruPIs  = ruPIs  +   PI_Bens(i,:)* suf* choice

    rePIc  = rePIc  +   PI_Benc(i,:)* se_uPI* choice
    rePIn  = rePIn  +   PI_Benn(i,:)* se_uPI* choice*eNC 
    rePIp  = rePIp  +   PI_Benp(i,:)* se_uPI* choice*ePC 

    if ( i==iiP6) then !Phaeo.uptake: carbon outside celss but in colony
        ruR3c= R3_Benc *suf * choice
        ruR3n= R3_Benn *suf * choice
        ruR3p= R3_Benp *suf * choice
        reR3c= R3_Benc* se_uR3* choice
        reR3n= R3_Benn* se_uR3* choice
        reR3p= R3_Benp* se_uR3* choice
    endif
  enddo

  ! new food originating for phytoplankton for Y3 is added here to Y3
  ! losses to phytoplankton are set in BenPelCoup using the total 
  ! total phytoplankton uptake jPIY3c

  call flux_vector( iiBen, ppY3c,ppY3c, ruPIc + ruR3c )
  call flux_vector( iiBen, ppY3n,ppY3n, ruPIn + ruR3n )
  call flux_vector( iiBen, ppY3p,ppY3p, ruPIp + ruR3p )

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Pelagic MicroZooplankton:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ! calculate max uptake  of food/m3. Check uptake pressure
  ! in lowest layer and limit if necessary
  fluc=food_ZI *suf 
  call LimitChange_vector(1,fluc,ZI_Fc,corr_max,s)
  ! calculate max uptake  of food/m2. Compare uptake pressure
  ! with food in whole water column and limit if necessary
  fluc=fluc *Depth_Ben 
  call LimitChange_vector(1,fluc,ctfZm2c,max_change_per_step,r)

  !Calculate relative uptaken food  (m)
  choice  =   min(r,s) *food_ZI*Depth_Ben/(1.0D-80+ ZI_Fc(:))

  ! mgC/m2/d = mgC/m3  */d * m
  ruZIc  =   ZI_Fc(:)* suf* choice
  ruZIn  =   ZI_Fn(:)* suf* choice
  ruZIp  =   ZI_Fp(:)* suf* choice
  jZIY3c(:)  =   ruZIc

  ! new food originating fro microzooplankton for Y3 is added here to Y3
  ! losses to microzooplankton are set in BenPelCoup using the total 
  ! total microzooplankton uptake jZIY3c

  call flux_vector( iiBen, ppY3c,ppY3c, ruZIc )
  call flux_vector( iiBen, ppY3n,ppY3n, ruZIn )
  call flux_vector( iiBen, ppY3p,ppY3p, ruZIp )

  reZIc  =   ZI_Fc(:)* se_uZI* choice
  reZIn  =   ZI_Fn(:)* se_uZI* choice* eNC
  reZIp  =   ZI_Fp(:)* se_uZI* choice* ePC

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Pelagic Detritus
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  fluc=RI_Fc *suf *Depth_Ben 
  !limit uptake to maximum of p_max per step (see at calulation of p_max)
  call LimitChange_vector(1,fluc,ctfRm2c,max_change_per_step,r)
  choice  =   food_RI * Depth_Ben/(p_small+RI_Fc(:))*r

  jRIY3c(:)  =   RI_Fc(:)* suf* choice
  ruR6c      =   RI_Fc(:)* suf* choice
  ruR6n      =   RI_Fn(:)* suf* choice
  ruR6p      =   RI_Fp(:)* suf* choice
  ruR6s      =   RI_Fs(:)* suf* choice

  ! new food originating fro detritus for Y3 is added here to Y3
  ! losses to detritus are set in BenPelCoup using the total 
  ! total detritus uptake jRIY3c

  call flux_vector( iiBen, ppY3c,ppY3c, ruR6c )
  call flux_vector( iiBen, ppY3n,ppY3n, ruR6n )
  call flux_vector( iiBen, ppY3p,ppY3p, ruR6p )

! if ( isnan(ruR6c(1))) write(LOGUNIT,*)'Y3 ruR6c',ruR6c

  reR6c  =   RI_Fc(:)* se_uR6* choice
  reR6n  =   RI_Fn(:)* se_uR6* choice *eNC
  reR6p  =   RI_Fp(:)* se_uR6* choice *ePC

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Book keeping
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  runc  =   ruPIc+ ruR3c +  ruZIc+ ruR6c
  runn  =   ruPIn+ ruR3n + ruZIn+ ruR6n
  runp  =   ruPIp+ ruR3p + ruZIp+ ruR6p

  retR6c  =   rePIc+ reR3c + reZIc+ reR6c
  retR6n  =   rePIn+ reR3n + reZIn+ reR6n
  retR6p  =   rePIp+ reR3p + reZIp+ reR6p

  retQ6c  =   0.0
  retQ6n  =   0.0
  retQ6p  =   0.0

  ! in case of a negative value of one of the following values there is a &
  ! situation
  ! of startvation and very low biomass values. Check on quota in the food is &
  ! out of order

  runc  =   max(  0.0D+00,  runc -retR6c-retQ6c-rrc)
  runn  =   max(  0.0D+00,  runn -retR6n-retQ6n)
  runp  =   max(  0.0D+00,  runp -retR6p-retQ6p)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! net growth rate ( only for output)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  jnetY3c=ruPIc+ ruR3c +  ruZIc+ ruR6c &
          - p_pur*( foodpm2* suf- retR6c- retQ6c)-retQ6c-retR6c
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of nutrient release and correction of C:N:P
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ! Carbon which will be excreted/respired when internal quota (N/C) are 
  ! below optimum
  renc  =   max(0.0,(-(p_small+runn)/( p_small+ runc)/ p_qnc+1.0),   &
                    (-(p_small+runp)/( p_small+ runc)/ p_qpc+1.0))* runc

  ! take in consideration that a (small) part of runC is used to make urea
  renn=max(0.0,runn-p_qnc*(runc-renc))/(1.0-p_qnc/p_qnUc)
  ! Correct uptake of N for loss to ure production
  runc=runc-renn/p_qnUc;
  renp=max(0.0,runp-p_qpc*(runc-renc));

  ! redistribute renc ovr flux to pelagic and benthic
  retQ6c= retQ6c +renc * retQ6c/(1.0D-80 + retQ6c + retR6c);
  retR6c= retR6c +renc * retR6c/(1.0D-80 + retQ6c + retR6c);

  renn = max( 0.0D+00, renn+ Y3n(:) -1.0 * p_qnc* Y3c(:))
  renp = max( 0.0D+00, renp+ Y3p(:) -1.0 * p_qpc* Y3c(:))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of respiration:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  r  =  rrc+ max(0.0,p_pur*( foodpm2* suf- retR6c- retQ6c))
  
  ! fluxes to/from Y3 from/to Pleagic are only defined here in jO2Y30 en jY3O3o
  ! the Pelagic flux statements are found in BenPelCoup.
  ! if no CO2 module implementend this a sink! other wise a fl;ux to G3c

  call flux_vector(iiBen, ppY3c,ppG3c,  r*(1.0D+00-p_pePel) )
  call flux_vector(iiBen, ppY3c,ppY3c, -r*p_pePel )
  jY3O3c(:)=r*p_pePel

  call flux_vector(iiBen, ppG2o,ppG2o,-( r/ 12.0D+00)* ( 1.0D+00-p_pePel))
  jO2Y3o(:)=r/12.0D+0 *p_pePel

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Add respiration and excretion to the benthic totals
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rrBTo(:)  =   rrBTo(:)+ r/ 12.0D+00 *(1.0-p_pePel)
  reBTp(:)  =   reBTp(:)+ renp * ( 1.0D+00-p_pePel)

  ! Here only sink flux defined for Y3
  ! Source fluxes are here only calculated in jY3N4n and JY3N1p
  ! Flux correction for N4n and N1p in BenPelCoup

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! fluxes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  call flux_vector( iiBen, ppY3n,ppY3n, -renn * p_pePel)        !urea flux
  call flux_vector( iiBen, ppY3c,ppY3c, -renn * p_pePel/p_qnUc) !urea flux
  call flux_vector( iiBen, ppY3p,ppY3p, -renp * p_pePel)

  jY3N4n(:)=renn *p_pePel
  jY3N1p(:)=renp *p_pePel

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  !Full flux defintion
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  call flux_vector( iiBen, ppY3n,ppQ1n, renn * (1.0D+00-p_pePel))
  call flux_vector( iiBen, ppY3c,ppQ1c, renn * (1.0D+00-p_pePel)/p_qnUc)
  call flux_vector( iiBen, ppY3p,ppK1p, renp * (1.0D+00-p_pePel))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of mortality
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- =-=-=-=-=-=-=-=-=-=-=-=-=-

  sm  =   p_sd* et  +p_sd2 * (Y3c(:)-Yy3c(:))

  reQ6c  =   Y3c(:)* sm
  reQ6n  =   reQ6c * p_qnc
  reQ6p  =   reQ6c * p_qpc

  retQ6c  =   retQ6c+ reQ6c
  retQ6n  =   retQ6n+ reQ6n
  retQ6p  =   retQ6p+ reQ6p

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate total flux from Suspension feeders to Q6:
  ! Full flux:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call flux_vector( iiBen, ppY3c,ppQ6c, retQ6c )
  call flux_vector( iiBen, ppY3n,ppQ6n, retQ6n )
  call flux_vector( iiBen, ppY3p,ppQ6p, retQ6p )

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate total flux from Suspension feeders to R6:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call flux_vector( iiBen, ppY3c,ppY3c,- retR6c )
  call flux_vector( iiBen, ppY3n,ppY3n,- retR6n )
  call flux_vector( iiBen, ppY3p,ppY3p,- retR6p )

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate NET flux from R6 to Suspension feeders :
  ! (can be negative!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ! The ruR6s which is uptaken is directly relased back to R6: net food flux &
  ! from Y3 to/from R6 is 0
  jY3RIc(:)  =   retR6c
  jY3RIn(:)  =   retR6n
  jY3RIp(:)  =   retR6p
  jY3RIs(:)  =   ruPIs +ruR6s

  
  jY3QIc(:)  =   retR6c * (1.0D+00-p_pR6Pel)
  jY3QIs(:)  =   jY3RIs * (1.0D+00-p_pR6Pels)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! pseudo faeces production
  ! This production lead only to a flux to the sediment!
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    
  !uptake of detritus for pseudofaeces production per m3
  fluc =   p_Rpsfcnp * et * eO * vum * fsat* Y3c(:) *RTc 
  r= fluc/Depth_Ben
  ! Check flux in lowest layer
  call LimitChange_vector(1,r,RTc,p_s_max,s)
  ! Check flux with mass of R6 in the whole water-column
  call LimitChange_vector(1,fluc,ctfRm2c,corr_max,r)
  r = fluc * min(r,s)/(p_small + RI_Fc(:))
  jRIQIc(:)=  max(0.0,r * RI_Fc(:) -ruR6c)
  jRIQIn(:)=  max(0.0,r * RI_Fn(:) -ruR6n)
  jRIQIp(:)=  max(0.0,r * RI_Fp(:) -ruR6p)

  !uptake of detritus for pseudofaeces production per m3
  fluc =  p_Rpsfs * et * eO * vum * fsat* Y3c(:) *RTc
  r= fluc/Depth_Ben
  ! Check flux in lowest layer
  call LimitChange_vector(1,r,RTc,p_s_max,s)
  ! Check flux with mass of R6 in the whole water-column
  call LimitChange_vector(1,fluc,ctfRm2c,corr_max,r)
  r = fluc * min(r,s)/(p_small + RI_Fc(:))
  jRIQIs(:)=  max(0.0,r * RI_Fs(:) -ruR6s)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! larvae
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


  if (CalcYy3) then
    if (p_xspawning>0.0) then
      r=max(0.0,p_xspawning-(Yy3c+cZ2m2c)/(1.0D-80+Y3c+cZ2m2c))
      jnetYy3c=max(0.0,r*Y3c)/GetDelta() &
           *insw_vector(active-2.0) *max(0.0,min(1.0,(0.7-fsat)/0.1))
      call LimitChange_vector(1,jnetYy3c,Y3c-Yy3c,max_change_per_step)
    else
     !Calculate maximum fraction which is eggs/larvae
     jnetYy3c=max(0.0D+00,jnetY3c)*max(0.0,Y3c-max(0.0D+00,Yy3c))/(1.0D-80+Y3c)
     ! However only "old" organsims can produce egss/larvae
     !Calculate new flux with which the young increases corrected for mortality,
     !transfer rate to old and loss to maintenance respiration.
    endif
    call flux_vector( iiBen, ppYy3c,ppYy3c,jnetYy3c &
         - Yy3c * (p_smYy3c/et +rrc/(1.0D-80+Y3c))*insw_vector(Yy3c) )
    pyfoodY3=p_pyfoodYy3
  else
    jnetYy3c=0.0;
    pyfoodY3=1.0D+00;
  endif

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! shell formation
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  jCaCO3Y3c=jCaCO3Y3c+p_qCaCO3Y3c * max(0.0D+00,jnetY3c-jnetYy3c) 
  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
