#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: MesoZoo
!
! DESCRIPTION
!   This submodel describes the carbon dynamics and associated
!    nutrient dynamics in carnivorous mesozooplankton (represented
!    by the state variable Z3) and in omnivorous zooplankton (in
!    the model known as Z4).
!    
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine MesoZooDynamics(zoo,  ppzooc, ppzoon, ppzoop)
!
! !USES:

  ! For the following Pelagic-states fluxes are defined: O2o, N1p, N4n, R6c, &
  ! R6p, R6n
  ! For the following Pelagic-group-states fluxes are &
  ! defined: PhytoPlankton, MicroZooPlankton, MesoZooPlankton
  ! The following Pelagic 1-d global boxvars are modified : flPIR6s
  ! The following Pelagic 1-d global boxvars  are used: ETW
  ! The following Pelagic 2-d global boxvars are used: &
  ! qn_mz, qp_mz, qnZc, qpZc
  ! The following groupmember vars are used: iiPhytoPlankton, &
  ! iiMicroZooPlankton, iiMesoZooPlankton
  ! The following constituent constants  are used: iiC, iiN, iiP, iiL
  ! The following 0-d global parameters are used: p_small
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
#IFDEF NOPOINTERS
  use mem,  ONLY: D3STATE
#ELSE
  use mem, ONLY: D3STATE, O2o, &
    PhytoPlankton, MicroZooPlankton, MesoZooPlankton
#ENDIF
  use mem, ONLY: ppO2o, ppO3c, ppN1p, ppR1c, ppR1n, ppRZc, ppR6c, ppR6p, Depth, &
    ppR6n, ppPhytoPlankton, ppMicroZooPlankton, ppMesoZooPlankton, flPIR6s, ETW, &
    qn_mz, qp_mz, qnZc, qpZc, iiPhytoPlankton, jnetY3c, jnetMeZc, &
    iiMicroZooPlankton, iiMesoZooPlankton, iiC, iiN, iiP, iiL,iiS, NO_BOXES,&
    jPelFishInput, iiPel, flux_vector,fixed_quota_flux_vector,&
    pMIupZ4,iiZ4,iiZ3,iiZ2,iiP6,R3c,ppR3c,sediMeZ,jeZIR6n,jeZIR6p,jeZIDIn,jeZIDIp
  use mem_Param,  ONLY: p_small,check_fixed_quota,p_pe_R1c, p_pe_R1n, p_pe_R1p, &
                   CalcMesoZooplankton,CalcMicroZooPlankton,CalcPhytoPlankton
  use constants,ONLY: p_qnUc
  use mem_Phaeo,ONLY:CALC_FOOD_MESOZOO,CALC_GRAZING_MESOZOO
  use mem_MesoZoo
  use mem,only:Source_D3_vector

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:eTq_vector, MM_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: eTq_vector, MM_vector, insw_vector


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
!   N. Broekhuizen and A.D. Bryant, ERSEM group
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
  ! Set up Local Variable for copy of state var. object
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN),dimension(:),pointer :: zooc
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer  :: i,iout
  integer  :: j
  real(RLEN),dimension(NO_BOXES)  :: put_u
  real(RLEN),dimension(NO_BOXES)  :: cmuc
  real(RLEN),dimension(NO_BOXES)  :: ceuc
  real(RLEN),dimension(NO_BOXES)  :: eo
  real(RLEN),dimension(NO_BOXES)  :: et
  real(RLEN),dimension(NO_BOXES)  :: rrsc
  real(RLEN),dimension(NO_BOXES)  :: rrsn
  real(RLEN),dimension(NO_BOXES)  :: rrsp
  real(RLEN),dimension(NO_BOXES)  :: rrac
  real(RLEN),dimension(NO_BOXES)  :: rugc
  real(RLEN),dimension(NO_BOXES)  :: rugn
  real(RLEN),dimension(NO_BOXES)  :: rugp
  real(RLEN),dimension(NO_BOXES)  :: reuc
  real(RLEN),dimension(NO_BOXES)  :: reun
  real(RLEN),dimension(NO_BOXES)  :: reup
  real(RLEN),dimension(NO_BOXES)  :: rmc
  real(RLEN),dimension(NO_BOXES)  :: rmn
  real(RLEN),dimension(NO_BOXES)  :: rmp
  real(RLEN),dimension(NO_BOXES)  :: sm
  real(RLEN),dimension(NO_BOXES)  :: rmdc
  real(RLEN),dimension(NO_BOXES)  :: rmdn
  real(RLEN),dimension(NO_BOXES)  :: rmdp
  real(RLEN),dimension(NO_BOXES)  :: runc
  real(RLEN),dimension(NO_BOXES)  :: runn
  real(RLEN),dimension(NO_BOXES)  :: runp
  real(RLEN),dimension(NO_BOXES,iiPhytoPlankton)     :: cumPIc
  real(RLEN),dimension(NO_BOXES)  :: cumR3c
  real(RLEN),dimension(NO_BOXES,iiMicroZooPlankton)  :: cumMIZc
  real(RLEN),dimension(NO_BOXES,iiMesoZooPlankton)   :: cumMEZc
  real(RLEN),dimension(NO_BOXES)  :: ruPIc
  real(RLEN),dimension(NO_BOXES)  :: ruMIZc
  real(RLEN),dimension(NO_BOXES)  :: ruMEZc
  real(RLEN),dimension(NO_BOXES)  :: rq6c
  real(RLEN),dimension(NO_BOXES)  :: rq6n
  real(RLEN),dimension(NO_BOXES)  :: rq6p
  real(RLEN),dimension(NO_BOXES)  :: renc
  real(RLEN),dimension(NO_BOXES)  :: renn
  real(RLEN),dimension(NO_BOXES)  :: renp
  real(RLEN),dimension(NO_BOXES)  :: tfluxc
  real(RLEN),dimension(NO_BOXES)  :: tfluxn
  real(RLEN),dimension(NO_BOXES)  :: tfluxp
  real(RLEN),dimension(NO_BOXES)  :: net
  real(RLEN),dimension(NO_BOXES)  :: r
  real(RLEN),dimension(NO_BOXES)  :: pvum
  real(RLEN),dimension(NO_BOXES)  :: limit_grazing
  real(RLEN),dimension(NO_BOXES)  :: eNC
  real(RLEN),dimension(NO_BOXES)  :: ePC
  real(RLEN),dimension(NO_BOXES)  :: rZ2Z3c
  real(RLEN),dimension(:),pointer :: phytoc,phytonps
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  Copy  state var. object in local var
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  zooc => D3STATE(ppzooc,:)

  tfluxc=0.0D+00
  tfluxn=0.0D+00
  tfluxp=0.0D+00

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  !Physiological temperature response
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  eo  =   MM_vector(  max(p_small,O2o(:)),  p_clO2o(zoo))
  et  =   eTq_vector(  ETW(:),  p_q10(zoo))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate total food
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! rua: food uptake generalized. Addition of new FG become much more simpler!

  cmuc  =   0.0D+00
  ceuc  =   0.0D+00
  cumR3c=   0.0;
  do i = 1 , ( iiPhytoPlankton)
    if (CalcPhytoPlankton(i)) then
     phytoc=> PhytoPlankton(i,iiC)
     cumPIc(:, i)  =   p_puPI(zoo,i)* phytoc
     limit_grazing=cumPIc(:,i)/(1.0D-80+cumPIc(:,i) +p_minfood)

     cumPIc(:,i)=cumPIc(:,i)*limit_grazing
     call PhaeocystisCalc(CALC_FOOD_MESOZOO,i,cumPIc(:,i),cumPIc(:,i),p_puPI(zoo,i))
     cmuc  =   cmuc+ cumPIc(:, i)
     ceuc  =   ceuc+ cumPIc(:, i) *p_peuPI(zoo)
     if ( i==iiP6) then
      !TEP material in colony`
        cumR3c =  cumPIc(:,i) *R3c/(1.0D-80+phytoc)
        cmuc= cmuc+ cumR3c
        ceuc  =   ceuc+ cumR3c *p_peuR3(zoo)
     endif
   endif
!   call findnan(cmuc,NO_BOXES,iout)
!   if (iout>0) then
!     write(LOGUNIT,*) 'MesoZoo:r Calc i=',i
!     write(LOGUNIT,*) 'cumPIc',cumPIc(iout,i)
!     write(LOGUNIT,*) 'phytoc',phytoc(iout)
!     write(LOGUNIT,*) 'R3c',R3c(iout)
!   endif
  end do


  do i = 1 , ( iiMicroZooPlankton)
    if (CalcMicroZooPlankton(i).and.p_puMiZ(zoo,i)>0.0D+00) then
      cumMIZc(:, i)  =   p_puMIZ(zoo,i)* MicroZooPlankton(i,iiC)
      cmuc  =   cmuc+ cumMIZc(:, i)
      ceuc  =   ceuc+ cumMIZc(:, i) *p_peuMIZ(zoo)
    endif
  end do


  do i = 1 , ( iiMesoZooPlankton)
    if (CalcMesoZooPlankton(i).and.p_puMEZ(zoo,i)>0.0) then
      cumMEZc(:, i)  =   p_puMEZ(zoo,i)* MesoZooPlankton(i,iiC)
      cmuc  =   cmuc+ cumMEZc(:, i)
      ceuc  =   ceuc+ cumMEZc(:, i) *p_peuMEZ(zoo)
   endif
  end do


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate total food uptake
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   if ( p_srm(zoo) > 0.0) then
     r=1.0- (cmuc-ceuc)/cmuc - p_pur(zoo)
     r= r /(p_srs(zoo)/p_sum(zoo)) - p_sum(zoo)/(p_small+ p_vum(zoo) *cmuc);
     pvum=min(1.0D+00,max(1.0D-6,r));
   else
     pvum=1.0
   endif


  rugc  =eo*et* p_sum(zoo)* MM_vector(  pvum* p_vum(zoo)* cmuc,p_sum(zoo))* zooc
  put_u  =   rugc/ ( 1.0D-80 + cmuc)


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of uptake rate:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  if (p_sw_nut_part(zoo)==1 ) then
    eNC=(1.0D+00-p_pe_R1n)/(1.0D+00-p_pe_R1c)
    ePC=(1.0D+00-p_pe_R1p)/(1.0D+00-p_pe_R1c)
  else
    eNC=1.0
    ePC=1.0
  endif

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Total Gross Uptakes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  rugc  =   0.0D+00
  rugn  =   0.0D+00
  rugp  =   0.0D+00
  reuc  =   0.0D+00
  reun  =   0.0D+00
  reup  =   0.0D+00

  do i = 1 , iiPhytoPlankton
    phytoc=> PhytoPlankton(i,iiC)
    if (CalcPhytoPlankton(i).and.p_puPI(zoo,i) > 0.0 ) then
      call PhaeocystisCalc(CALC_GRAZING_MESOZOO,i,ruPIc,ruPIc,p_puPI(zoo,i))
      ruPIc  =   put_u* cumPIc(:, i)
      rugc  =   rugc+ ruPIc
      reuc  =   reuc+ ruPIc            *p_peuPI(zoo)

      iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzooc, &
                          ppPhytoPlankton(i,iiC),ppzooc, ruPIc ,tfluxc)
      phytonps=> PhytoPlankton(i,iiN)
      r= ruPIc* phytonps/(1.0D-80+phytoc)
      rugn  =   rugn+ r
      reun  =   reun+ r *p_peuPI(zoo) *eNC
      iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoon, &
                ppPhytoPlankton(i,iiN),ppzoon, r,tfluxn )
      phytonps=> PhytoPlankton(i,iiP)
      r= ruPIc* phytonps/(1.0D-80+phytoc)
      rugp  =   rugp+ r
      reup  =   reup+ r *p_peuPI(zoo) *ePC
      iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoop, &
              ppPhytoPlankton(i,iiP),ppzoop, r,tfluxp )
      ! Chl is transferred to the sink
      phytonps=> PhytoPlankton(i,iiL)
      r= ruPIc* phytonps/(1.0D-80+phytoc)
      call flux_vector( iiPel, ppPhytoPlankton(i,iiL), ppPhytoPlankton(i,iiL),-r )
      ! PIs is directly transferred to R6s
      j=ppPhytoPlankton(i,iiS)
      if (j>0 ) then
         phytonps=> PhytoPlankton(i,iiS)
          flPIR6s(i,:)  =   flPIR6s(i,:)+ ruPIc* phytonps/(1.0D-80+phytoc)
      endif
      if ( i==iiP6) then
        r=ruPIc/(1.0D-80+cumPIc(:, i)) * cumR3c
        iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzooc, &
                       ppR3c,ppzooc, r ,tfluxc)
        rugc  =   rugc+ r
        reuc  =   reuc+ r *p_peuR3(zoo)
      endif
    endif
  end do
  if ( zoo== iiZ4) pMIupZ4=rugc 
 
  do i = 1 , ( iiMicroZooPlankton)
    if (CalcMicroZooPlankton(i).and.p_puMiZ(zoo,i)>0.0D+00) then
      ruMIZc  =   put_u* cumMIZc(:, i)
      iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzooc, &
             ppMicroZooPlankton(i,iiC),ppzooc, ruMIZc,tfluxc )
      iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoon, &
           ppMicroZooPlankton(i,iiN),ppzoon, ruMIZc*qn_mz(i,:) ,tfluxn)
      iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoop, &
           ppMicroZooPlankton(i,iiP),ppzoop, ruMIZc* qp_mz(i,:) ,tfluxp)
      rugc  =   rugc+ ruMIZc
      rugn  =   rugn+ ruMIZc* qn_mz(i,:)
      rugp  =   rugp+ ruMIZc* qp_mz(i,:)
      reuc  =   reuc+ ruMIZc             *p_peuMIZ(zoo)
      reun  =   reun+ ruMIZc* qn_mz(i,:) *p_peuMIZ(zoo) *eNC
      reup  =   reup+ ruMIZc* qp_mz(i,:) *p_peuMIZ(zoo) *ePC
   endif
  end do

  if ( zoo== iiZ4) pMIupZ4=(rugc-pMIupZ4)/(put_u *cmuc) 


  rZ2Z3c=0.0;
  do i = 1 , ( iiMesoZooPlankton)
    if (CalcMesoZooPlankton(i).and.p_puMEZ(zoo,i)>0.0) then
      ruMEZc  =   put_u* cumMEZc(:, i)
      ! intra-group predation is not computed
      if ( i/= zoo) then
        iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzooc, &
                       ppMesoZooPlankton(i,iiC),ppzooc, ruMEZc, tfluxc )
        iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoon, &
            ppMesoZooPlankton(i,iiN),ppzoon, ruMEZc* qnZc(i,:) , tfluxn)
        iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoop, &
            ppMesoZooPlankton(i,iiP),ppzoop, ruMEZc* qpZc(i,:) , tfluxp)
      end if
      if ( zoo.eq.i) jPelFishInput(1)= jPelFishInput(1) + sum(ruMEZc*Depth)

      rugc  =   rugc+ ruMEZc
      rugn  =   rugn+ ruMEZc* qnZc(i,:)
      rugp  =   rugp+ ruMEZc* qpZc(i,:)
      reuc  =   reuc+ ruMEZc            *p_peuMEZ(zoo)
      reun  =   reun+ ruMEZc* qnZc(i,:) *p_peuMEZ(zoo)*eNC
      reup  =   reup+ ruMEZc* qpZc(i,:) *p_peuMEZ(zoo)*ePC
      if ( zoo.eq.iiZ3.and.i.eq.iiZ2 ) rZ2Z3c=ruMeZc-reuc
    endif
  end do


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Respiration and basal metabolism
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  rrac  =   p_pur(zoo)* rugc

  rrsc  =   max(pvum*p_srs(zoo),p_srm(zoo))* et*eo* zooc
  rrsn  =   rrsc * qnZc(zoo,:)
  rrsp  =   rrsc * qpZc(zoo,:)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Assimilated material
  ! Respectively Carbon, Nitrogen and Phosphorus
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  runc  =    rugc -reuc -rrac
  runn  =    rugn -reun + rrsn
  runp  =    rugp -reup + rrsp

! ! calculate how C will be excreted
  renc  =   max(0.0,(-(p_small+runn)/( p_small+ runc)/ p_qnc(zoo)+1.0),   &
                    (-(p_small+runp)/( p_small+ runc)/ p_qpc(zoo)+1.0))* runc

  !Correct excretion of renn for the fact that for ecretion some C is needed
  ! for excretion as urea.`
  renn=max(0.0,runn-p_qnc(zoo)*max(0.0,runc-renc)) /(1.0-p_qnc(zoo)/p_qnUc)
  runc=runc-renn/p_qnUc
  renp=max(0.0,runp-p_qpc(zoo)*max(0.0,runc-renc));

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Natural mortality + low oxygen mortality
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  rmc  =   (p_sd(zoo)+ p_srs(zoo)*(1.0D+00-eo))* zooc *et
  rmn  =   (p_sd(zoo)+ p_srs(zoo)*(1.0D+00-eo))* zooc *et * qnZc(zoo,:)
  rmp  =   (p_sd(zoo)+ p_srs(zoo)*(1.0D+00-eo))* zooc *et*  qpZc(zoo,:)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Density dependent mortality
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  sm   =   p_smd(zoo)* zooc**p_emd(zoo)
  rmdc  =   sm * zooc
  rmdn  =   sm * zooc* qnZc(zoo,:)
  rmdp  =   sm * zooc* qpZc(zoo,:)

  jPelFishInput(1)= jPelFishInput(1) + sum((rmdc+rmc)*Depth)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Fluxes for eliminated excess nutrients
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  rq6c  =   rmc+ reuc+ rmdc
  rq6n  =   rmn+ reun+ rmdn
  rq6p  =   rmp+ reup+ rmdp

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! flow statements
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call flux_vector( iiPel, ppO2o,ppO2o,- (rrac+rrsc)/ 12.0D+00 )
  iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzooc, &
                             ppzooc,ppO3c,rrac+rrsc,tfluxc )
  iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoon, &
                             ppzoon,ppR1n, renn ,tfluxn)
  iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzooc, &
                             ppzooc,ppR1c, renn/p_qnUc ,tfluxc)
  iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoop, &
                             ppzoop,ppN1p, renp ,tfluxp)


  if (p_sw_faecelpell_sed(zoo)==1) call flux_vector( iiPel, ppRZc,ppRZc, rq6c+renc)
  iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzooc, &
                             ppzooc,ppR6c, rq6c+renc ,tfluxc )
  iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoon, &
                             ppzoon,ppR6n, rq6n ,tfluxn)
  iout= fixed_quota_flux_vector( check_fixed_quota, iiPel, ppzoop, &
                             ppzoop,ppR6p, rq6p ,tfluxp)

  jeZIDIn= jeZIDIn+sum(Depth(:)*renn)
  jeZIDIp= jeZIDIp+sum(Depth(:)*renp)

  jeZIR6n= jeZIR6n+sum(Depth(:)*rq6n)
  jeZIR6p= jeZIR6p+sum(Depth(:)*rq6p)

  net=rugc-rrac-reuc-renc
  if ( zoo.ne.iiZ2) then
      jnetMeZc(1)=jnetMeZc(1)+sum(Depth(:)*net)
     !Correction for young filterfooder larvae eaten by carnivorous microzooplankton
     r= net*rZ2Z3c/(1.0D-80+rugc-reuc)
     jnetY3c(1)=jnetY3c(1)-sum(Depth(:)*r)
  else
     !feeding of benthic larvae is added to the filterfeeder production.
      jnetY3c(1)=jnetY3c(1)+sum(Depth(:)*net)
     !decreases the filterfeeder larvae will hesitate 
     !          to go back to benthos in case of presence of much (pelagic) food...
     r=1.0/(1.0+p_sum(zoo)/(1.0D-80+p_vum(zoo) *cmuc))
     sediMeZ(zoo,:)=sediMeZ(zoo,:)*min(1.0D+00,r)
  endif

  renn=tfluxc*p_qnc(zoo)
  iout= fixed_quota_flux_vector( check_fixed_quota,-iiN,0,0,0,renn,tfluxn,"Mesozoo")
  renp=tfluxc*p_qpc(zoo)
  iout= fixed_quota_flux_vector( check_fixed_quota,-iiP,0,0,0,renp,tfluxp,"Mesozoo")

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
