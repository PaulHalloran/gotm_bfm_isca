#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: Phyto
!
! DESCRIPTION
!   This process describes the dynamics of all phytoplankton
!    groups in the ERSEM model. The differences in behaviour
!    are expressed by differences in parameter-values only.
!
!
!

!   This file is generated directly from OpenSesame model code, using a code
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine PhytoDynamics(phyto, ppphytoc, ppphyton, ppphytop, ppphytos, &
    ppphytol)
!
! !USES:

  ! For the following Pelagic-states fluxes are defined: R6c, O2o, R2c, &
  ! N3n, N4n, N1p, R1n, R6n, R1p, R6p, N5s
  ! The following global scalar vars are used: SUNQ, ThereIsLight
  ! The following Pelagic 1-d global boxvars are modified : flPIR6s
  ! The following Pelagic 1-d global boxvars  are used: ETW, EIR, xEPS, Depth
  ! The following Pelagic 2-d global boxvars are modified : eiPI, sediPI
  ! The following Pelagic 2-d global boxvars got a value: sunPI
  ! The following Pelagic 2-d global boxvars  are used: qpPc, qnPc, qsPc, qlPc
  ! The following 0-d global parameters are used: p_small, &
  ! ChlLightFlag, LightForcingFlag
  ! The following global constants are used: RLEN
  ! The following constants are used: SEC_PER_DAY, HOURS_PER_DAY

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
#IFDEF NOPOINTERS
  use mem,  ONLY: D3STATE
#ELSE
  use mem, ONLY: D3STATE, O2o, N5s
#ENDIF
  use mem, ONLY:ppO2o, ppR1c, ppR2c, ppR6c,  ppN3n, ppN4n, ppN1p, ppR1n, ppO3c,&
    ppR1p, ppN5s, SUNQ, ThereIsLight, ETW, EIR, xEPS, jnetPTc, rnetPTc, &
    Depth, eiPI, sediPI, sunPI, sugPI, qpPc,qnPc, qsPc, qlPc,NO_BOXES,flP6R3c, &
    iiR1,iiR2,iiR3,iiPel, rml,flux_vector,sourcesink_flux_vector, &
    iiP6,rumn4,rumn3,rumnu,rump1,rumpu,rums5,flPIR6n,flPIR1n, &
    flPIR6p,flPIR1p,flPIR6s,Xantho,cxoO2
#ifdef INCLUDE_PELCO2
  use mem,  ONLY: HCO3
#ENDIF
  use constants,  ONLY: SEC_PER_DAY, HOURS_PER_DAY
  use mem_Param,  ONLY: p_pe_R1c, p_pe_R1n, p_pe_R1p,p_small, ChlLightFlag, &
                        LightForcingFlag, p_qon_nitri
  use mem_Phyto
  use mem_BenthicNutrient3,ONLY:p_max_state_change
  use LimitRates, ONLY:LimitChange_vector
  use mem_Phaeo,ONLY:CALC_MORTALITY,FORCE_MORTALITY,MAX_MORTALITY,NEW_COLONIES,&
                     CALC_MORTALITY_CELLS_IN_COLONY,CALC_LOC_DET_FLUX,CALC_NET_NITROGEN_UPTAKE

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:eTq_vector, MM_vector, insw_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: eTq_vector, MM_vector, MM_power_vector, insw_vector,exp_limit

  use mem,  ONLY: Output2d_1,Output2d_2,Output2d_3, Output2d_4,Source_D3_vector

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE
  real(RLEN),external  ::GetDelta

! !INPUT:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer,intent(IN)  :: phyto
  integer,intent(IN) :: ppphytoc
  integer,intent(IN) :: ppphyton
  integer,intent(IN) :: ppphytop
  integer,intent(IN) :: ppphytos
  integer,intent(IN) :: ppphytol

!
!
! !AUTHORS
!   ERSEM group + J.G. Baretta-Bekker + W.Ebenhoeh
!     P. Ruardij (NIOZ)
!
!
! COPYING
!
!   Copyright (C) 2006 P. Ruardij, the mfstep group, the ERSEM team
!   (rua@nioz.nl, vichi@bo.ingv.it)
!
!   This program is free software; you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation
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
  real(RLEN),dimension(NO_BOXES):: phytoc
  real(RLEN),dimension(NO_BOXES):: phyton
  real(RLEN),dimension(NO_BOXES):: phytop
  real(RLEN),dimension(NO_BOXES):: phytos
  real(RLEN),dimension(NO_BOXES):: phytol
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer                         :: silica_control  ,iout
  real(RLEN)                      :: avPhyto
  real(RLEN)                      :: sudc !max growth rate related to max.number of divions perday.             
  real(RLEN)                      :: LocalDelta
  real(RLEN)                      :: max_change_per_step
  real(RLEN),parameter            :: rzero=0.0D+00
  real(RLEN),dimension(NO_BOXES)  :: r,s
  real(RLEN),dimension(NO_BOXES)  :: dl   !day length
  real(RLEN),dimension(NO_BOXES)  :: cg   !correction for growth at hight temperatures
  real(RLEN),dimension(NO_BOXES)  :: eo
  real(RLEN),dimension(NO_BOXES)  :: et
  real(RLEN),dimension(NO_BOXES)  :: sumc,sumc0
  real(RLEN),dimension(NO_BOXES)  :: sea
  real(RLEN),dimension(NO_BOXES)  :: set
  real(RLEN),dimension(NO_BOXES)  :: ses
  real(RLEN),dimension(NO_BOXES)  :: sdo
  real(RLEN),dimension(NO_BOXES)  :: sadap
  real(RLEN),dimension(NO_BOXES)  :: rugc
  real(RLEN),dimension(NO_BOXES)  :: sra
  real(RLEN),dimension(NO_BOXES)  :: srs
  real(RLEN),dimension(NO_BOXES)  :: srt
  real(RLEN),dimension(NO_BOXES)  :: slc
  real(RLEN),dimension(NO_BOXES)  :: ruf
  real(RLEN),dimension(NO_BOXES)  :: rupp
  real(RLEN),dimension(NO_BOXES)  :: rump
  real(RLEN),dimension(NO_BOXES)  :: misp
  real(RLEN),dimension(NO_BOXES)  :: rupn
  real(RLEN),dimension(NO_BOXES)  :: rumn
  real(RLEN),dimension(NO_BOXES)  :: run
  real(RLEN),dimension(NO_BOXES)  :: misn
  real(RLEN),dimension(NO_BOXES)  :: rums
  real(RLEN),dimension(NO_BOXES)  :: rups
  real(RLEN),dimension(NO_BOXES)  :: miss
  real(RLEN),dimension(NO_BOXES)  :: tN
  real(RLEN),dimension(NO_BOXES)  :: iN
  real(RLEN),dimension(NO_BOXES)  :: iN1p
  real(RLEN),dimension(NO_BOXES)  :: iNIn
  real(RLEN),dimension(NO_BOXES)  :: iNIs
  real(RLEN),dimension(NO_BOXES)  :: eN5s
  real(RLEN),dimension(NO_BOXES)  :: rrc
  real(RLEN),dimension(NO_BOXES)  :: rr2c
  real(RLEN),dimension(NO_BOXES)  :: rr1c
  real(RLEN),dimension(NO_BOXES)  :: rr1n
  real(RLEN),dimension(NO_BOXES)  :: rr1p
  real(RLEN),dimension(NO_BOXES)  :: rr6c
  real(RLEN),dimension(NO_BOXES)  :: rr6n
  real(RLEN),dimension(NO_BOXES)  :: rr6p
  real(RLEN),dimension(NO_BOXES)  :: rr6s
  real(RLEN),dimension(NO_BOXES)  :: runn
  real(RLEN),dimension(NO_BOXES)  :: runnu
  real(RLEN),dimension(NO_BOXES)  :: runn3
  real(RLEN),dimension(NO_BOXES)  :: runn4
  real(RLEN),dimension(NO_BOXES)  :: runp
  real(RLEN),dimension(NO_BOXES)  :: runpu
  real(RLEN),dimension(NO_BOXES)  :: runp1
  real(RLEN),dimension(NO_BOXES)  :: runs
  real(RLEN),dimension(NO_BOXES)  :: runs5
  real(RLEN),dimension(NO_BOXES)  :: Irr
  real(RLEN),dimension(NO_BOXES)  :: rho_Chl
  real(RLEN),dimension(NO_BOXES)  :: rate_Chl
  real(RLEN),dimension(NO_BOXES)  :: new_Chl
  real(RLEN),dimension(NO_BOXES)  :: flChydrate
  real(RLEN),dimension(NO_BOXES)  :: rx_leftover
  real(RLEN),dimension(NO_BOXES)  :: alpha_chl

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  silica_control =0 : no silica component present in cell
  !  silica_control =1 : external regulation of silica limitation & limitation of
  !                      carbon fixation under silica depletion
  !  silica_control =2 : internal regulation of silica limitation & excretion
  !                      of fixed carbon under nutrient stress
  !                      Process description based on:
  !                      Growth physiology and fate of diatoms in the ocean: a review
  !                      G.Sarthou, K.R. Timmermans, S. Blain, & P. Treguer
  !                      JSR 53 (2005) 25-42
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  LocalDelta=GetDelta()
  max_change_per_step=p_max_state_change/LocalDelta

  silica_control=0
  if ( p_qus(phyto) > 0.0 )  then
    silica_control=2
    if ( p_xqs(phyto)>0.0 ) silica_control=3
  elseif ( p_chPs(phyto) > 0.0 ) then
    silica_control=1
  endif


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  Copy  state var. object in local var
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  phytoc = D3STATE(ppphytoc,:)
  phyton = D3STATE(ppphyton,:)
  phytop = D3STATE(ppphytop,:)
  phytol = D3STATE(ppphytol,:)
  if ( phyto==iiP6 ) then
     avPhyto=sum(phytoc*Depth)/sum(Depth)
     if (avPhyto <1.0e-20 ) then
       return
     endif
  endif
  if ( ppphytos > 0 )  phytos = D3STATE(ppphytos,:)



  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Nutrient limitation (intracellular) N, P
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  iN1p = min( 1.0D+00, max( p_small, ( qpPc(phyto,:)&
                             - p_qplc(phyto))/( p_qpRc(phyto)- p_qplc(phyto))))
  iNIn = min( 1.0D+00, max( p_small, ( qnPc(phyto,:)&
                            - p_qnlc(phyto))/( p_qnRc(phyto)- p_qnlc(phyto))))
  iNIs = min( 1.0D+00, max( p_small, ( qsPc(phyto,:) &
                              - p_qslc(phyto))/( p_qsRc(phyto)- p_qslc(phyto))))


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Phytoplankton growth is limited by nitrogen and phosphorus
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  select case ( p_limnut)
    case ( 0 ) ; iN  =   (iN1p* iNIn)**(0.5D+00)  ! geometric mean
    case ( 1 ) ; iN  =   min(  iN1p,  iNIn)  ! Liebig rule
    case ( 2 ) ; iN  =   2.0D+00/( 1.0D+00/ iN1p+ 1.0D+00/ iNIn)  ! combined
  end select

  ! tN controls sedimentation of phytoplankton
  tN= iN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Nutrient limitation due to intra- extracellular silicate.
  ! eN5s limit externally nutrient limitation.
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  !calculate internal quota
  eN5s  =   1.0D+00
  if ( silica_control > 0 ) then
    select case (silica_control)
      case(1)
         eN5s = min( eN5s,N5s/(N5s + p_chPs(phyto)))
         tN=min(iN,eN5s)
      case(2,3)
        tN=min(iNIs,iN)
      end select
  endif

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Temperature response of Phytoplankton
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   et  =   eTq_vector(  ETW(:),  p_q10(phyto))
   eo  =   min(1.0,exp(-p_clO2o(phyto)/O2o(:)) +p_clO2o(phyto)/cxoO2) 

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Photosynthesis (Irradiance EIR is in uE m-2 s-1, Irr is mid-layer EIR)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! if (p_xantho(phyto) >0.0 ) &
!  Xantho=p_xantho(phyto) * phytol * (1.0-exp(-EIR/p_EIR(phyto)))   

  if ( ChlLightFlag== 2) then
    !Irr = EIR*exp(-xEPS* 0.5 * Depth)*SEC_PER_DAY
    Irr = max( p_small, EIR(:)/ xEPS(:)/ Depth(:)*( 1.0D+00- exp( &
      - xEPS(:)* Depth(:)))* SEC_PER_DAY)
    alpha_chl=p_alpha_chl(phyto) +p_alpha_add(phyto)*exp(0.4*(4.0-ETW))
    r=qlPc(phyto,:)+xantho/(1.0D-80+phytoc)
    eiPI(phyto,:) = max(0.0, 1.0D+00- exp_limit(- r(:)* alpha_chl/ p_sum(phyto)* Irr))
!   eiPI(phyto,:) = max(0.0, 1.0D+00- exp_limit(- qlPc(phyto, :)* alpha_chl/ p_sum(phyto)* Irr))
  end if

  select case ( LightForcingFlag)
    case ( 1 ) ; sumc  = p_sum(phyto)* et* eiPI(phyto,:)   *  eN5s
    case ( 2 ) ; sumc = p_sum(phyto)* et* eiPI(phyto,:)*( SUNQ/ HOURS_PER_DAY) * eN5s
    case ( 3 ) ; sumc  = p_sum(phyto)* et* eiPI(phyto,:)* ThereIsLight * eN5s
  end select


  sumc=max(0.0,sumc)
  !density dependent mortality: only used for phytoplankton which is not beeaten
  sdo  =   p_seo(phyto)* MM_vector(  phytoc,  100.0D+00)
  !low oxygen  dependent mortality
  sdo  =   sdo+ p_srs(phyto)*et* (1.0-eo)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Lysis and excretion
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   if ( phyto==1) rml=0.0
   if ( phyto .eq.iiP6) then
     !Calculate mortality add this to sdo
     call PhaeocystisCalc(CALC_MORTALITY,phyto,sdo,sdo,rzero)
     !higher mortality at higerh temperature of cells in colonies
     call PhaeocystisCalc(CALC_MORTALITY_CELLS_IN_COLONY,phyto,r,et,rzero)
     flP6R3c=flP6R3c+r*phytoc

     call PhaeocystisCalc(MAX_MORTALITY,phyto,sdo,sdo,rzero)
     !r is dummy var.
     call PhaeocystisCalc(CALC_LOC_DET_FLUX,phyto,r,sdo,rzero)
   else
     sdo  = sdo +  ( p_thdo(phyto)* &
     (1.0/( tN+ p_thdo(phyto))))* p_sdmo(phyto)  ! nutr. -stress lysis
!    sdo(:)= sdo(:) +   min(1.0D+00,exp_limit(-tN(:)/(p_small+p_thdo(phyto))) &
!      * 0.82436063535)  *p_sdmo(phyto) * phytoc(:)/(phytoc(:)+0.01D+00)
   endif
   rml=rml+sdo *phytoc

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Activity processes 
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  sea  =   sumc* p_pu_ea(phyto) 
  sra  =   p_pu_ra(phyto)*max(0.0, sumc- sea)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Rest processes 
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ses  =   p_ses(phyto)*et  ! activity +rest excretion
  srs  =   eo* et* p_srs(phyto)

#ifdef INCLUDE_PELCO2
  r  = sumc*phytoc/12.0 
  call LimitChange_vector(1,r,HCO3,max_change_per_step,s) !2 ways to limit Phyto
  where (HCO3.gt.0.0)              ! HCO3==0 PH calculation is not succeeded....
    sumc=sumc*min(s,HCO3/(100.0D+00+HCO3))                  ! gross production
  end where
#endif
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Potential-Net prim prod. (mgC /m3/d):
  !  a. limit prim prod at high temperature wth the (maximum) number of 
  !     divisions per day by make use of the length of the light period.
  !  c. assume that activity respiration increases with temperature and 
  !     is NOT limited by the number of devisions per day.
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  dl=(SUNQ/ HOURS_PER_DAY)
  s  =   max(  0.0D+00, dl*( sumc- sea))  ! net production
  r= max(0.0,s-(ses+srs+dl*sra))
  sudc=log(p_xdiv(phyto))
  r= r*min(1.0,2.0*sudc/(sudc+r))
  cg=1.0
  where (sumc.gt.rzero) cg=min(1.0,(r+(ses+srs+dl*sra))/(1.0D-80+s))
  sumc=cg*sumc; sea=cg*sea

  !-=-=0-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Correct for mortality : in order to model an effective  
  ! mortality at the end of the bloom it is necessary to make all Phaeo 
  ! which will die during next time-step inactive, otherwise the mortality
  ! is counteracted by the new production 
!   call PhaeocystisCalc(FORCE_MORTALITY,phyto,r,sdo,rzero)
  r=1.0-min(max_change_per_step,sdo)*LocalDelta
  sumc0=sumc
  sumc=r*sumc; sea=r*sea; sra=r*sra

! if ( phyto==2)Output2d_3(1)=sum(sumc0*Depth)/sum(Depth)
! if ( phyto==2)Output2d_4(1)=sum(sumc*Depth)/sum(Depth)
  !-=-=0-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Apportioning over R1 and R6:
  ! Cell lysis generates both DOM and POM.
  ! The nutr.-depleted cell has a nutrient-carbon ratio equal to p_q?lc.
  ! Assuming that this structural part is not easily degradable,
  ! at least a fraction equal to the minimum quota is released as POM.
  ! Therefore, nutrients (and C) in the structural part go to R6.
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  r=sdo*phytoc
  rr1c  =  p_pe_R1c* r
  rr6c  =  (1.0D+00-p_pe_R1c)* r

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! total processes 
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  srt  =   sra+ srs     ! total specific respiration
  set  =   sea+ ses     ! total specific excretion
  slc  =   sea+ sra+ sdo  ! specific loss terms

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Production, productivity and respiration flows
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rugc=sumc * phytoc  ! gross production
  call sourcesink_flux_vector( iiPel, ppO3c,ppphytoc, rugc )  ! source/sink.c
  call flux_vector( iiPel, ppO2o,ppO2o, rugc/ 12.0D+00 )      ! source/sink.o
  rrc  =   srt* phytoc  ! total actual respiration

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Potential-Net prim prod. (mgC /m3/d)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ruf  =   max(  0.0D+00, ( sumc- slc)* phytoc)  ! net production
  sugPI(phyto,:)  =   ruf/( p_small+ phytoc)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Nutrient Uptake: calculate maximal uptake of N,P
  ! Check if C-fixation is larger to make of all C new biomass
  ! If not increase excretion of C
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  r=qnPc(phyto,:); s=qpPc(phyto,:)
  if ( phyto==iiP6) then
    ! Special construction: here Phaeocystis can use N-buffer which is outside 
    ! the cells but inside the colony (See more in PelGlobal.F90 where quota 
    ! are calculated)
    where (phytoc>1.0D-10) 
       r=phyton/phytoc;s=phytop/phytoc
    endwhere
  endif
  r= max( 0.0D+00, p_sbuf(phyto)* rugc(:)*max(p_qnlc(phyto),r- p_qnlc(phyto)))
  call LimitChange_vector(1,r,Phyton-Phytoc*p_qnlc(phyto),max_change_per_step) 
  ! N uptake brom 4 sources: nitrate, ammonium, organic N, from buffer incell.
  rumn(:)  =   rumn3(phyto,:)+ rumn4(phyto,:)  +rumnu(phyto,:) +r(:)  

  ! P uptake brom 3 sources: phosphate, organic P, from buffer in cell.
  s(:) = max( 0.0D+00, p_sbuf(phyto)* rugc(:)*max(p_qplc(phyto), s- p_qplc(phyto)))
  call LimitChange_vector(1,s,Phytop-Phytoc*p_qplc(phyto),max_change_per_step) 
  rump(:)  =   rump1(phyto,:) +rumpu(phyto,:)+s(:)  ! max pot. uptake of NI

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Check if which fraction C-fixation can be used for new biomass
  ! by checking the potential nutrient avilability
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  run(:) = min( ruf(:), rumn(:)/p_qnlc(phyto), rump(:)/p_qplc(phyto))

  ! Si uptake brom 2 sources: dissolved silicate, from diatom-skeleton  
  if ( silica_control  >=  2) then
    s(:) = max( 0.0D+00,rugc(:)*max(p_qslc(phyto),qsPc(phyto,:)- p_qslc(phyto)))
    call LimitChange_vector(1,s,Phytos-Phytoc*p_qslc(phyto),max_change_per_step) 
    rums=rums5(phyto,:)+s(:)
    run(:) = min( run(:), ( rums(:)/ p_qslc(phyto)))
  endif

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! C flows ; phyto --> detritus
  ! a. activity excretion of sugars:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  flChydrate  =   set* phytoc

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! b.stress:All C which cannot be used for growth is excreted :
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rr2c=0.0;
  select case (p_iRI(phyto))
     case (iiR1) ; rr1c=rr1c+ flChydrate   ! carbon to let grow bacteria as a protection against viruses 
     case (iiR2)  
       r=min(run* p_pu_ea(phyto),flChydrate)
       rr2c=rr2c+ r              ! carbon as protection again dissolution of Silica wall
       rr1c=rr1c+ flChydrate-r   ! carbon as protection again dissoltuion of Silica wall
     case (iiR3) ; flP6R3c=flP6R3c+flChydrate  !TEP in Phaeo-colony
  end select

  run          = min(ruf,max( run,0.0D+00))
  rx_leftover  = ruf- run !production of carbohydrates
  flChydrate   = p_pos_e(phyto)*rx_leftover  
  rrc          = rrc+(1.0-p_pos_e(phyto))*rx_leftover  ; sra= rrc/(1.0D-80+phytoc) -srs
 
  ! C flows: calculate fluxes
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  select case (p_iRI(phyto))
     case (iiR1,iiR2)  
        rr1c =rr1c +  flChydrate*(1.0-p_R2qec(phyto)) !LOC-hydrates
        rr2c= rr2c +  flChydrate*p_R2qec(phyto)          !TEP-hydrates
     case (iiR3)                             !TEP in Phaeo-colony
          flP6R3c=flP6R3c+flChydrate;
          if ( phyto.ne.iiP6) stop 'error in definition of flP6R3c'
  end select

  call flux_vector( iiPel, ppphytoc,ppR1c, rr1c )  ! source/sink.c
  call flux_vector( iiPel, ppphytoc,ppR2c, rr2c )  ! source/sink.c
 
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Respiration flows
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  call sourcesink_flux_vector( iiPel, ppphytoc,ppO3c, rrc )  ! source/sink.c
  call flux_vector( iiPel, ppO2o,ppO2o,-( rrc/ 12.0D+00) )    ! source/sink.o


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Apparent Net prim prod. (mgC /m3/d)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  sunPI(phyto,:)  =   run/( p_small+ phytoc)
  sadap=max(srs,sumc0)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Nutrient dynamics: NITROGEN
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

! Intracellular missing amount of N
  s=set*phytoc
  call PhaeocystisCalc(CALC_NET_NITROGEN_UPTAKE,phyto,r,s,rzero)
  rupn = r+ p_xqn(phyto)* p_qnRc(phyto)* run- (ses+srs)*qnPc(phyto,:)* phytoc
  misn = sadap*(p_xqn(phyto)* p_qnRc(phyto)*(1.0-0.25)- qnPc(phyto,:))* phytoc    
  where (misn<0.0) &
   misn=min(0.0,misn+sadap*(0.25*p_xqn(phyto)* p_qnRc(phyto))* phytoc)   

  runn  =   min( rumn,  rupn+ misn)  ! actual uptake of NI

  !Calculate the uptake of the different sources.(uptake form the cell itself changes automatically internal buffer)
  r  =   insw_vector(runn)
  runn3  =   r* runn* rumn3(phyto,:)/( p_small+ rumn)  ! actual uptake of Nn
  runn4  =   r* runn* rumn4(phyto,:)/( p_small+ rumn)  ! actual uptake of Nn
  runnu  =   r* runn* rumnu(phyto,:)/( p_small+ rumn)  ! actual uptake of Nn

  call flux_vector( iiPel, ppN3n,ppphyton, runn3 )  ! source/sink.n
  call flux_vector( iiPel, ppN4n,ppphyton, runn4 )  ! source/sink.n
  call flux_vector( iiPel, ppR1n,ppphyton, runnu )  ! source/sink.n
  ! oxygen which freed by phytoplankton when they take up nitrate
  call flux_vector( iiPel, ppO2o,ppO2o,runn3*p_qon_nitri )  ! source/sink.o
  !correction in winter situation of negative growth to keep nutrients below max.ratio
  call flux_vector(iiPel, ppphyton,ppN4n,- runn*insw_vector( -runn))  ! source/sink.n

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Nuttrient dynamics: PHOSPHORUS
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Intracellular missing amount of P
  misp  =   sadap*(p_xqp(phyto)* p_qpRc(phyto)*(1.0-0.25)- qpPc(phyto,:))* phytoc    
  where (misp<0.0D+00) &
      misp=min(0.0D+00,misp+sadap*(0.25D+00*p_xqp(phyto)* p_qpRc(phyto))* phytoc)
  rupp  =   p_xqp(phyto)* p_qpRc(phyto)* run- (ses+srs)*qpPc(phyto,:)* phytoc 
  runp  =   min( rump,rupp+ misp)  ! actual uptake

  !Calculate the uptake of the different sources. (uptake form the cell itself changes automatically internal buffer)
  r  =   insw_vector(runp)
  runp1  =   r* runp* rump1(phyto,:)/( p_small+ rump)  ! actual uptake of Nn
  runpu  =   r* runp* rumpu(phyto,:)/( p_small+ rump)  ! actual uptake of Nn

  call flux_vector( iiPel, ppN1p,ppphytop, runp1 )  ! source/sink.n

  call flux_vector( iiPel, ppR1p,ppphytop, runpu )  ! source/sink.n
  !correction in winter situation of negative growth to keep nutrients below max.ratio
  call flux_vector(iiPel, ppphytop,ppN1p,- runp* insw_vector(-runp))  ! source/sink.p

  runn  =   min( rumn,  rupn+ misn)                                   ! actual uptake of NI

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Excretion of N and P to PON and POP
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  r=sdo*qnPc(phyto,:)*phytoc;s=phyton-p_qnlc(phyto)*1.0D-10
  ! extreme cases no flux at lower below :
  call LimitChange_vector(2,r,s,max_change_per_step)
  rr1n  =   p_pe_R1n* r
  rr6n  =   r- rr1n

  r=sdo*qpPc(phyto,:)*phytoc;s=phytop-p_qplc(phyto)*1.0D-10
  ! extreme cases no flux at lower below :
  call LimitChange_vector(2,r,s,max_change_per_step)
  rr1p  =   p_pe_R1p* r
  rr6p  =   r- rr1p

  flPIR1n(phyto,:)=flPIR1n(phyto,:) + rr1n
  flPIR1p(phyto,:)=flPIR1p(phyto,:) + rr1p

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Nutrient dynamics: SILICATE
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  if ( silica_control > 0 )  then
    select case (silica_control)

    case (1)
     runs = max(0.0, p_qsRc(phyto) * run );          ! net uptake
     call flux_vector( iiPel, ppN5s,ppphytos, runs)  ! source/sink.c

    case (2,3)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      !  Nutrient uptake
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      rups  =   ( p_xqs(phyto) * run* p_qsRc(phyto)- (ses+srs*qsPc(phyto,:))* phytoc)   ! Si uptake based on C uptake
      !all silica can only be used to make new biomass!
      miss  =   0.0 ! intracellular missing Si
      runs  =   min( rums,rups+ miss)  ! actual uptake
      !Calculate the uptake of the different sources. (uptake form the cell itself changes automatically internal buffer)
      runs5  =   runs* rums5(phyto,:)/( p_small+ rums)  ! actual uptake of Nn
      call flux_vector( iiPel, ppN5s,ppphytos, runs5* insw_vector(runs) )  ! source/sink.c

      !correction in winter situation of negative growth to keep nutrients below max.ratio
      ! In this situation diatoms have a 4* times higher winter ratio
      miss  =   et* p_srs*( p_xqs(phyto)*p_qsRc(phyto)* phytoc- phytos)  ! intracellular missing amount of P
      r     =   min( rums,rups+ miss)  ! actual uptake
      call flux_vector( iiPel, ppphytos,ppN5s,- r*insw_vector(-r)*insw_vector(-runs))  ! source/sink.c
    end select

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    ! Losses of Si
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    rr6s  =   sdo* phytos  ! Lysis, particulate

    ! Collect first all fluxes of P-->silica
    flPIR6s(phyto,:)  =   flPIR6s(phyto,:)+ rr6s
  endif

  if ( ChlLightFlag== 2) then
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    ! Chl-a synthesis and photoacclimation
    ! Carbon fixation is limited at high temperatures leading to a lower growth rate.
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      rho_Chl = p_qchlc(phyto)* p_sum(phyto)* cg * eiPI(phyto,:)* phytoc/&
        (alpha_chl*(phytol+ p_small)* Irr)* &
                      min(1.0,(qnPc(phyto,:)-p_qnlc(phyto))/(p_lqnlc(phyto)-p_qnlc(phyto)))

! total synthesis, only when there is net production (run > 0)
      new_Chl = rho_Chl*(( sumc- sra)* phytoc-flChydrate)
      rate_Chl = new_Chl- sdo* phytol+ min( &
          0.0, sumc- slc+ sdo)* max( 0.0D+00, phytol- p_qchlc(phyto)* phytoc) &
          -et*p_sdchl_d(phyto) * phytol
      if (p_xantho(phyto) <=0.0 ) then
        r=p_sdchl_l(phyto) * (1.0-exp(-EIR/p_EIR(phyto)))   
        rate_Chl=rate_Chl-phytol * r
        rr6n=rr6n+p_qnlc(phyto)*phytoc *r
        rr6c=rr6c+phytoc*r
      endif
    s=phytol-p_qchlc(phyto)*1.0D-10
    call LimitChange_vector(2,rate_Chl,s,max_change_per_step)
    call flux_vector( iiPel, ppphytol,ppphytol, rate_Chl )
  endif
  call flux_vector( iiPel, ppphytoc,ppR6c, rr6c )  ! source/sink.c
  flPIR6n(phyto,:)=flPIR6n(phyto,:) + rr6n !fluxes define in PelChem
  flPIR6p(phyto,:)=flPIR6p(phyto,:) + rr6p

! if (phyto==2) then
!   r=Source_D3_vector(ppphytoc)
!   Output2d_3(1)=sum(r/(1.0D-80+phytoc)*Depth)/sum(Depth)
! endif

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Sedimentation  old way: sedimentation is cuopled to nutrient depletion...
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  if ( p_res(phyto)> rzero) then
    r=0.0D+00
    where(phytoc > 0.01D+00 ) &
        r= p_res(phyto)* max( 0.0D+00, ( p_esNI(phyto)- tN)/p_esNI(phyto))
    where(phytoc > 0.01D+00 ) sediPI(phyto,:) = sediPI(phyto,:) +r
  endif

  rnetPTc=(sumc-sra-sdo)*phytoc-flChydrate
  jnetPTc(1)=jnetPTc(1)+sum(rnetPTc*Depth(:))

! End of computation section for process PhytoDynamics


  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
