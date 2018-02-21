#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenOrganism
!
! DESCRIPTION
!   !    This submodel describes the carbon dynamics and associated
!    nutrient dynamics in benthic organisms
! 
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine BenOrganismDynamics(y,  ppyc, ppyn, ppyp)
!
! !USES:

  ! For the following Benthic-states fluxes are defined: Q6c, Q6n, Q6p, G2o, &
  ! K4n, K1p, D6m, D7m, D8m
  ! The following Benthic-states are used (NOT in fluxes): D1m
  ! For the following Benthic-group-states fluxes are defined: BenOrganisms, &
  ! BenBacteria
  ! The following Benthic 1-d global boxvars are modified : rrBTo, reBTn, reBTp
  ! The following Benthic 1-d global boxvars  are used: ETW_Ben
  ! The following groupmember vars are used: iiBenOrganisms, iiBenBacteria, &
  ! iiY1, iiY2, iiY4, iiY5
  ! The following constituent constants  are used: iiC, iiN, iiP
  ! The following 0-d global parameters are used: p_d_tot
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
  use constants,ONLY:p_qnUc

#IFDEF NOPOINTERS
  use mem,  ONLY: D2STATE
#ELSE
  use mem, ONLY: D2STATE, Q6c, Q6n, Q6p, D6m, D7m,Y3c, &
    D8m, D1m, BenOrganisms, BenBacteria
#ENDIF
  use mem, ONLY: ppQ6c, ppQ6n, ppQ6p, ppG3c, ppG2o, ppQ1c,ppQ1n, ppK1p,&
    ppD6m, ppD7m, ppD8m, ppBenOrganisms, ppBenBacteria,iiBenOrganisms, &
    iiBenBacteria,iiY3 ,iiH2, iiC, iiN, iiP, iiBen, & 
    flux_vector, sourcesink_flux_vector,NO_BOXES_XY,jCaCO3Y3c,  &
    rrBTo, reBTp, rugYIc, ETW_Ben,jBenFishInput,ppYy3c,pyfoodY3,Yy3c
  use mem_Param,  ONLY: p_d_tot,p_small,CalcYy3
  use mem_BenOrganism
  use mem_FilterFeeder,only:p_qCaCO3Y3c


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:eTq_vector, eramp_vector, &
  ! MM_vector, PartQ_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: eTq_vector, eramp_vector, MM_vector, PartQ_vector,insw_vector


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE

! !INPUT:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer,intent(IN)  :: y
  integer,intent(IN) :: ppyc
  integer,intent(IN) :: ppyn
  integer,intent(IN) :: ppyp
!  
!
! !AUTHORS
!   W. Ebenhoh and C. Kohlmeier.
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
  real(RLEN),dimension(NO_BOXES_XY) :: yc
  real(RLEN),dimension(NO_BOXES_XY) :: yn
  real(RLEN),dimension(NO_BOXES_XY) :: yp
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer  :: i
  real(RLEN),dimension(NO_BOXES_XY,iiBenOrganisms)  :: avail_Y
  real(RLEN),dimension(NO_BOXES_XY,iiBenBacteria)  :: avail_H
  real(RLEN),dimension(NO_BOXES_XY)  :: clm
  real(RLEN),dimension(NO_BOXES_XY)  :: cm
  real(RLEN),dimension(NO_BOXES_XY)  :: cmm
  real(RLEN),dimension(NO_BOXES_XY)  :: et
  real(RLEN),dimension(NO_BOXES_XY)  :: eO
  real(RLEN),dimension(NO_BOXES_XY)  :: food
  real(RLEN),dimension(NO_BOXES_XY)  :: food_HIc
  real(RLEN),dimension(NO_BOXES_XY)  :: food_src
  real(RLEN),dimension(NO_BOXES_XY)  :: eF
  real(RLEN),dimension(NO_BOXES_XY)  :: sug
  real(RLEN),dimension(NO_BOXES_XY)  :: sug_y
  real(RLEN),dimension(NO_BOXES_XY)  :: sun
  real(RLEN),dimension(NO_BOXES_XY)  :: sunQ6
  real(RLEN),dimension(NO_BOXES_XY)  :: se_u
  real(RLEN),dimension(NO_BOXES_XY)  :: se_uQ6
  real(RLEN),dimension(NO_BOXES_XY)  :: choice
  real(RLEN),dimension(NO_BOXES_XY)  :: availQ6_c
  real(RLEN),dimension(NO_BOXES_XY)  :: availQ6_n
  real(RLEN),dimension(NO_BOXES_XY)  :: availQ6_p
  real(RLEN),dimension(NO_BOXES_XY)  :: rugc
  real(RLEN),dimension(NO_BOXES_XY)  :: rugn
  real(RLEN),dimension(NO_BOXES_XY)  :: rugp
  real(RLEN),dimension(NO_BOXES_XY)  :: runc
  real(RLEN),dimension(NO_BOXES_XY)  :: runn
  real(RLEN),dimension(NO_BOXES_XY)  :: runp
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6c
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6n
  real(RLEN),dimension(NO_BOXES_XY)  :: rqt6p
  real(RLEN),dimension(NO_BOXES_XY)  :: rq6c
  real(RLEN),dimension(NO_BOXES_XY)  :: rq6n
  real(RLEN),dimension(NO_BOXES_XY)  :: rq6p
  real(RLEN),dimension(NO_BOXES_XY)  :: ruYIc
  real(RLEN),dimension(NO_BOXES_XY)  :: ruYIn
  real(RLEN),dimension(NO_BOXES_XY)  :: ruYIp
  real(RLEN),dimension(NO_BOXES_XY)  :: ruBIc
  real(RLEN),dimension(NO_BOXES_XY)  :: ruBIn
  real(RLEN),dimension(NO_BOXES_XY)  :: ruBIp
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ6c
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ6n
  real(RLEN),dimension(NO_BOXES_XY)  :: ruQ6p
  real(RLEN),dimension(NO_BOXES_XY)  :: rrc
  real(RLEN),dimension(NO_BOXES_XY)  :: sm
  real(RLEN),dimension(NO_BOXES_XY)  :: qnc
  real(RLEN),dimension(NO_BOXES_XY)  :: qpc
  real(RLEN),dimension(NO_BOXES_XY)  :: renc
  real(RLEN),dimension(NO_BOXES_XY)  :: renn
  real(RLEN),dimension(NO_BOXES_XY)  :: renp
  real(RLEN),dimension(NO_BOXES_XY)  :: bc
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  Copy  state var. object in local var
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  yc = D2STATE(ppyc,:)
  yn = D2STATE(ppyn,:)
  yp = D2STATE(ppyp,:)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Physiological temperature and oxygen response
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  !grazing on other benthic oranism is defined according parameter 
  !predation-matrix (p_Yn) . With exception for grazing on Y3: there are only the young ones
  !available as food
  avail_Y=1.0; if (CalcYy3) avail_Y(:,iiY3)=max(0.0,pyfoodY3*Yy3c/(1.0D-80+Y3c))
  cm  =   p_cm(y); avail_H=1.0; clm=p_d_tot;avail_H(:,iiH2)=0.0;
  where (cm.gt.D1m) avail_H(:,iiH2)=min(1.0, PartQ_vector(D6m(:),D1m(:),cm, p_d_tot) / &
                                PartQ_vector(D6m(:),D1m(:),D6m(:),  p_d_tot))
  et  =   eTq_vector(  ETW_Ben(:),  p_q10(y))
  eO  =   eramp_vector(  D1m(:),  p_sdm(y))

  ! As alternative the following function can be used
  ! eO = MM(pow(O2.o, 3.0), p_chdo);
  ! and the parameter p_chdo must be defined


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate total food
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  food  =   p_small

  rugc  =   0.0D+00
  rugn  =   0.0D+00
  rugp  =   0.0D+00

  rqt6c  =   0.0D+00
  rqt6n  =   0.0D+00
  rqt6p  =   0.0D+00

  ! For other benthic organisms:

  do i = 1 , ( iiBenOrganisms)
    food_src  = max(0.0,BenOrganisms(i,iiC))* p_Yn(y,i)*avail_Y(:,i)
    food  =   food+ food_src* MM_vector(  food_src,  p_clu(y))
  end do

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Bacteria:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  food_HIc=0.0;
  do i = 1 , ( iiBenBacteria)
    bc  =   BenBacteria(i,iiC)*p_Hn(y,i)*avail_H(:,i)
    food_HIc  =   food_HIc + max(0.0D+00,bc)* MM_vector(  bc,  p_clu(y))
  end do
  food      =  food+ food_HIc

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Detritus (if eaten) First calculate the available portion
  ! and then add it to the total amount of food
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  if ( p_puQ6(y)> 0.0D+00) then
    clm  =   p_clm(y)
    cm  =   p_cm(y)
    availQ6_c  =   Q6c(:)* PartQ_vector(  D6m(:),  clm,  cm,  p_d_tot)
    availQ6_n  =   Q6n(:)* PartQ_vector(  D7m(:),  clm,  cm,  p_d_tot)
    availQ6_p  =   Q6p(:)* PartQ_vector(  D8m(:),  clm,  cm,  p_d_tot)

    food_src  =   p_puQ6(y)* availQ6_c
    food  =   food+ food_src* MM_vector(  food_src,  p_clu(y))
  else 
    availQ6_c  =   0.0D+00;
    availQ6_n  =   0.0D+00;
    availQ6_p  =   0.0D+00;
  end if

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Correct for too much food...
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  eF  =   MM_vector(  food,  p_chu(y))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Correction of growth rate for environmental factors
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  ! Growth rate at actual amount:

  sug_y  =   p_su(y)* et* eO* eF

  !if maintenance costs for searching is defined there is only eaten
  !when the gain of searching is laerger than the costs:
  if ( p_sra(y) > 0.0) then
     sug_y=sug_y*insw_vector(sug_y*( 1.0D+00- p_pue(y))-p_sra(y)*et)
  endif

  ! Relative growth rate corrected for actual amount of food:
  sug  =  ( sug_y* yc)/ ( 1.0D-80 + food)

  ! Net uptake:
  sun    =   sug*( 1.0D+00- p_pue(y))
  sunQ6  =   sug*( 1.0D+00- p_pueQ6(y))
  ! Diagnostic output: gross Uptake of food 
  rugYIc(y,:)=sug *yc

  ! Execreted part:
  se_u    =   sug- sun
  se_uQ6  =   sug- sunQ6

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! Calculation of uptake rate:
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Benthic organisms:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  do i = 1 , ( iiBenOrganisms)
    qnc  = BenOrganisms(i,iiN)/ (1.0D-80+BenOrganisms(i,iiC))
    qpc  = BenOrganisms(i,iiP)/ (1.0D-80+BenOrganisms(i,iiC))
    food_src  = max(0.0,BenOrganisms(i,iiC))* p_Yn(y,i)*avail_Y(:,i)
    choice = MM_vector( food_src, p_clu(y))
    ruYIc  = food_src* sug* choice
    ruYIn  = qnc* ruYIc
    ruYIp  = qpc* ruYIc

    if ( ruYIc(1) < 0.0D+00 .or. ruYIn(1) < 0.0D+00 .or. ruYIp(1) < 0.0D+00 ) then
        write(LOGUNIT,*) 'Yy3c=',Yy3c(1)
        write(LOGUNIT,*) 'pyfoodY3=',pyfoodY3(1)
        write(LOGUNIT,*) 'food_src=',food_src(1)
        write(LOGUNIT,*) 'eO=',eO(1)
        write(LOGUNIT,*) 'et=',et(1)
        write(LOGUNIT,*) 'eF=',eF(1)
        write(LOGUNIT,*) 'food=',food(1)
        write(LOGUNIT,*) 'choice=',choice(1)
        write(LOGUNIT,*) 'sug=',sug(1)
        write(LOGUNIT,*) 'Q6c=',Q6c(1)
        write(LOGUNIT,*) 'D6m=',D6m(1)
        write(LOGUNIT,*) 'BenOrganism Y1=',BenOrganisms(1,iiC)
        write(LOGUNIT,*) 'BenOrganism Y2=',BenOrganisms(2,iiC)
        write(LOGUNIT,*) 'BenOrganism Y3=',BenOrganisms(3,iiC)
        write(LOGUNIT,*) 'BenOrganism Y4=',BenOrganisms(4,iiC)
        write(LOGUNIT,*) 'BenOrganism Y5=',BenOrganisms(5,iiC)
        write(LOGUNIT,*) 'BenBacteria=',BenBacteria(1,iiC)
        write(LOGUNIT,*) 'BenBacteria=',BenBacteria(2,iiC)
    endif

   
    ! In case of cannibalism rate of change in state = zero!
    if ( i/= y) then
      call flux_vector( iiBen, ppBenOrganisms(i,iiC),ppyc, ruYIc )
      call flux_vector( iiBen, ppBenOrganisms(i,iiN),ppyn, ruYIn )
      call flux_vector( iiBen, ppBenOrganisms(i,iiP),ppyp, ruYIp )
    elseif ( y==1 .or. y==5) then 
      ! grazing within a functional group is considered as "closure' of the model
      ! This grazing can be seen therefor seen as food available for the higher trophic levels.
      ! The higher trophic levels (=demersel fish) eatonly of the 2 top predators Y1 and Y5
      jBenFishInput=jBenFishInput + ruYIc
    end if
    !Every loss of Y3 is aslo a loss in Yy3 becasue the preadotrs eat only young filterfeeders.
    if ( i==iiY3 .and. CalcYy3) then
       call flux_vector( iiBen, ppYy3c,ppYy3c, -ruYIc*insw_vector(food_src))
       !Assumption:Shell of youngfilterfeeders are directly dissolved in the seawater
       jCaCO3Y3c=jCaCO3Y3c-p_qCaCO3Y3c *ruYIc
    endif


    rugc  =   rugc+ ruYIc
    rugn  =   rugn+ ruYIn
    rugp  =   rugp+ ruYIp

    rq6c  =   food_src* se_u* choice
    rq6n  =   food_src* qnc * se_u* p_pudil(y)* choice
    rq6p  =   food_src* qpc * se_u* p_pudil(y)* choice

    rqt6c  =   rqt6c+ rq6c
    rqt6n  =   rqt6n+ rq6n
    rqt6p  =   rqt6p+ rq6p

  end do

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Bacteria:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  do i = 1 , ( iiBenBacteria)
    bc  =   BenBacteria(i,iiC)*p_Hn(y,i)*avail_H(:,i)
    choice =  p_Hn(y,i)* MM_vector(  bc,  p_clu(y))
    ruBIc  =   BenBacteria(i,iiC)* sug* choice
    ruBIn  =   BenBacteria(i,iiN)* sug* choice
    ruBIp  =   BenBacteria(i,iiP)* sug* choice

    call flux_vector( iiBen, ppBenBacteria(i,iiC),ppyc, ruBIc )
    call flux_vector( iiBen, ppBenBacteria(i,iiN),ppyn, ruBIn )
    call flux_vector( iiBen, ppBenBacteria(i,iiP),ppyp, ruBIp )

    rugc  =   rugc+ ruBIc
    rugn  =   rugn+ ruBIn
    rugp  =   rugp+ ruBIp

    rq6c  =   BenBacteria(i,iiC)* se_u* choice
    rq6n  =   BenBacteria(i,iiN)* se_u* p_pudil(y)* choice
    rq6p  =   BenBacteria(i,iiP)* se_u* p_pudil(y)* choice

    rqt6c  =   rqt6c+ rq6c
    rqt6n  =   rqt6n+ rq6n
    rqt6p  =   rqt6p+ rq6p

  end do


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! detritus
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  select case ( p_puQ6(y)> 0.0D+00)

    case( .TRUE. )
      choice  =   p_puQ6(y)* MM_vector(  p_puQ6(y)* availQ6_c,  p_clu(y))
      ruQ6c  =   sug* choice* availQ6_c
      ruQ6n  =   sug* choice* availQ6_n
      ruQ6p  =   sug* choice* availQ6_p

      call flux_vector( iiBen, ppQ6c,ppyc, ruQ6c )
      call flux_vector( iiBen, ppQ6n,ppyn, ruQ6n )
      call flux_vector( iiBen, ppQ6p,ppyp, ruQ6p )

      rugc  =   rugc+ ruQ6c
      rugn  =   rugn+ ruQ6n
      rugp  =   rugp+ ruQ6p

      rq6c  =   se_uQ6* choice* availQ6_c
      rq6n  =   se_uQ6* p_pudil(y)* choice* availQ6_n
      rq6p  =   se_uQ6* p_pudil(y)* choice* availQ6_p

      rqt6c  =   rqt6c+ rq6c
      rqt6n  =   rqt6n+ rq6n
      rqt6p  =   rqt6p+ rq6p

    case( .FALSE. )
      ruQ6c  =   0.0D+00
      ruQ6n  =   0.0D+00
      ruQ6p  =   0.0D+00

  end select


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of respiration:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rrc  =   p_sr(y)* yc* et+ p_pur(y)*( sug* food- rqt6c) +p_sra(y)*yc*et*insw_vector(sug)

  call sourcesink_flux_vector( iiBen, ppyc, ppG3c, rrc )
  call flux_vector(iiBen, ppG2o,ppG2o,-( rrc/ 12.0D+00))


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of mortality
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  sm  =   p_sd(y)* et !+ (1.0-eO) * 0.1

  rq6c  =   yc* sm
  rq6n  =   yn* sm
  rq6p  =   yp* sm

  rqt6c  =   rqt6c+ rq6c
  rqt6n  =   rqt6n+ rq6n
  rqt6p  =   rqt6p+ rq6p

  if ( y==1 .or. y==5)  jBenFishInput=jBenFishInput + rq6c
  ! in case of a negative value of one of the following values there is a &
  ! situation
  ! of startvation and very low biomass values. Check on quota in the food is &
  ! out of order
  runc  =   max(  0.0D+00,  rugc- rqt6c -rrc)
  runn  =   max(  0.0D+00,  rugn- rqt6n)
  runp  =   max(  0.0D+00,  rugp- rqt6p)

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of nutrient release and correction of C:N:P
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   renc  =   max(0.0,(-(p_small+runn)/( p_small+ runc)/ p_qn(y)+1.0),   &
                    (-(p_small+runp)/( p_small+ runc)/ p_qp(y)+1.0))* runc

  !Correct excretion of renn for the fact that for ecretion some C is needed
  ! for excretion as urea.`
  renn=max(0.0,runn-p_qn(y)*(runc-renc))/(1.0-p_qn(y)/p_qnUc)
  runc=runc-renn/p_qnUc;
  renp=max(0.0,runp-p_qp(y)*(runc-renc));

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Correction for cases where initial conditions deviate strongly from
  ! Redfield C:N:P. In this way the C:N:P does not become too extreme
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  if (isnan(renp(1))) write(LOGUNIT,*) 'renp Nan in BenOrg II'

  renn = min( max( 0.0D+00, renn), max( 0.0D+00, renn-( p_qn(y)* yc- yn)))
  renp = min( max( 0.0D+00, renp), max( 0.0D+00, renp-( p_qp(y)* yc- yp)))

  if (isnan(renp(1))) write(LOGUNIT,*) 'renp Nan in BenOrg III'

! call flux_vector( iiBen, ppyn,ppK4n, renn )
  call flux_vector( iiBen, ppyn,ppQ1n, renn )
  call flux_vector( iiBen, ppyc,ppQ1c, renn/p_qnUc )
  call flux_vector( iiBen, ppyp,ppK1p, renp )

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculate total flux of Y to Q6:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call flux_vector( iiBen, ppyc,ppQ6c, rqt6c +renc )
  call flux_vector( iiBen, ppyn,ppQ6n, rqt6n )
  call flux_vector( iiBen, ppyp,ppQ6p, rqt6p )


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Add respiration and excretion to the benthic totals
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  rrBTo(:)  =   rrBTo(:)+ rrc/ 12.0D+00
! reBTn(:)  =   reBTn(:)+ renn  no output of ammonium: organism excrete is urea : flux to R1n 
  reBTp(:)  =   reBTp(:)+ renp


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Assign organism-dependent parameters
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  select case ( y)
    case ( iiY1 ) ; cmm  =   0.0D+00 
    case ( iiY2 ) ; cmm  =   D6m(:)/ 2.0D+00 
    case ( iiY4 ) ; cmm  =  ( p_cm(y)+ p_clm(y))/ 2.0D+00 
    case ( iiY5 ) ; cmm  =   D1m(:)/ 2.0D+00
  end select

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of changes due to uptake of detritus in distribution of
  ! state variables (Dx.m is an undetermined source).
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call flux_vector(iiBen, ppD6m,ppD6m,( cmm- D6m(:))*( rqt6c- ruQ6c)/(p_small+ Q6c(:)))
  call flux_vector(iiBen, ppD7m,ppD7m,( cmm- D7m(:))*( rqt6n- ruQ6n)/(p_small+Q6n(:)))
  call flux_vector(iiBen, ppD8m,ppD8m,( cmm- D8m(:))*( rqt6p- ruQ6p)/(p_small+ Q6p(:)))

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
