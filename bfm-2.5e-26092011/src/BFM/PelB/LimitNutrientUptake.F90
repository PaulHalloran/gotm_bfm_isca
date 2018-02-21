#include "DEBUG.h"
#include "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: LimitNutrientUptake
!
! DESCRIPTION
!   The routine is a predictor/corrector to keep up the nutrients to
!   minimal to (very) small positive values.
!   Therfore the maximal nutrient uptake is limited to a 
!   rate which is fixed_fraction per delta.  
!
!

!
! !INTERFACE
      subroutine  LimitNutrientUptake
!
! !USES:
      use global_mem, only: RLEN,ZERO
#IFDEF NOPOINTERS
      use mem,  ONLY: D3STATE
#ELSE
      use mem, ONLY: N3n, N4n, N1p, N5s, R1c,R1n, R1p, B1c,R2c,RZc,R6c,R1c,B1n, B1p
#ENDIF
      use mem, ONLY:  NO_BOXES, &
        iiC,rumn3,rumn4,rumnu,rump1,rumpu,rums5,rumn4B,rumn3B,rumnuB,rumpuB,rumpB, & 
        lim_rumn4,lim_rumn3,lim_rumnu,lim_rump1,lim_rumpu,lim_rums5, &
        PhytoPlankton,iiPhytoPlankton,ppPhytoPlankton,iiP6,ETW,SUNQ 
      use mem_Param,  ONLY:  p_s_max,p_small,p_qpPhc,p_qnUlc,p_q10diff
      use mem_Phyto
      use mem_PelBac,ONLY:p_qunB=>p_qun,p_qupB=>p_qup,p_lN3N4B=>p_lN3N4n, &
              p_qnBc=>p_qnc,p_qpBc=>p_qpc,p_luN4B=>p_lureaN4n,p_lN1B=>p_lN1, &
              p_suR6B=>p_suR6,p_suR2B=>p_suR2,p_suR1B=>p_suhR1
      use constants,  ONLY: p_qnUc,HOURS_PER_DAY
      use mem_Phaeo, ONLY:CALC_REL_PHOSPHATE_UPTAKE,CALC_REL_NITRATE_UPTAKE, &
             CALC_REL_AMMONIUM_UPTAKE,CALC_REL_UREA_UPTAKE,CALC_TEMPERATURE_DEPENDENCE
      use mem_globalfun,   ONLY: eTq_vector



     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE
     real(RLEN),external  :: GetDelta

!  
!
! !AUTHORS
!   ERSEM group 
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
     integer     :: i
     real(RLEN)  :: delta,dl
     real(RLEN),dimension(NO_BOXES)  :: r,c,n,p
     real(RLEN),dimension(NO_BOXES)  :: un
     real(RLEN),dimension(NO_BOXES)  :: up
     real(RLEN),dimension(NO_BOXES)  :: cqun3
     real(RLEN),dimension(NO_BOXES)  :: cquR1n
     real(RLEN),dimension(NO_BOXES)  :: cqup
     real(RLEN),dimension(NO_BOXES)  :: lim_qu
     real(RLEN),dimension(NO_BOXES)  :: phytoc
     real(RLEN),dimension(NO_BOXES)  :: t_rumn4
     real(RLEN),dimension(NO_BOXES)  :: t_rumn3
     real(RLEN),dimension(NO_BOXES)  :: t_rumnu
     real(RLEN),dimension(NO_BOXES)  :: t_rump1
     real(RLEN),dimension(NO_BOXES)  :: t_rumpu
     real(RLEN),dimension(NO_BOXES)  :: t_rums5
     real(RLEN),dimension(NO_BOXES)  :: et
     real(RLEN),dimension(NO_BOXES)  :: rutB

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Modules (use of ONLY is strongly encouraged!)
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
     ! Nutrient Uptake: calculate maximal uptake of N, P
     ! Check if C-fixation is # larger to make of all C new biomass
     ! Assumed is that Si-depletion directly the growth rate in contradiction
     ! to N and P.
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
     
     dl=(SUNQ/ HOURS_PER_DAY)
     t_rumn4=1.0D-80
     t_rumn3=1.0D-80
     t_rumnu=1.0D-80
     t_rump1=1.0D-80
     t_rumpu=1.0D-80
     t_rums5=1.0D-80
     !Phytoplankon prefer urea or ammonium
!    et  =   eTq_vector(  ETW(:),  p_q10diff)
     et  =1.0
     do i = 1 , iiPhytoPlankton
!      select case  (i)
!      case (iiP6);  call PhaeocystisCalc(CALC_TEMPERATURE_DEPENDENCE,i,et,ETW(:),0.0D+00)
!      case default; et  =   eTq_vector(  ETW(:),  p_q10(i))
!    end select

      un(:)=max(ZERO,R1n(:)- R1c(:) * p_qnUlc)  ! rich R1 :urea
      up(:)=max(ZERO,R1p(:)- R1c(:) * p_qpPhc)  ! rich R1 :
      cqun3(:)  =   p_lN3N4n(i)/( p_lN3N4n(i)+ N4n(:))
      cquR1n(:)  =   p_lureaN4n(i)/( p_lureaN4n(i)+ N4n(:))
      cqup(:)  =    p_lN1(i)/( p_lN1(i)+ N1p(:))
      phytoc(:)=PhytoPlankton(i,iiC)

      lim_qu(:)=1.0D+00;
! max pot. uptake of N4
      call PhaeocystisCalc(CALC_REL_AMMONIUM_UPTAKE,i,lim_qu(:),N4n,p_qun(i))
      rumn4(i,:)  =   max(ZERO,p_qun(i)* N4n(:)* phytoc(:)*et* lim_qu)  
!max pot.uptake of N3
      call PhaeocystisCalc(CALC_REL_NITRATE_UPTAKE,i,lim_qu(:),N3n,p_qun(i))
      rumn3(i,:)  =  max(ZERO,p_qun(i)* N3n(:)* phytoc(:)*et *lim_qu(:)*cqun3(:) ) 
!max pot. uptake of R1n
      call PhaeocystisCalc(CALC_REL_UREA_UPTAKE,i,lim_qu(:),un(:),p_qun(i))
      rumnu(i,:)  =   max(ZERO,p_qun(i)*un(:)* phytoc(:)*et *lim_qu*cquR1n(:) ) 
!maxmial uptake at maximal net growth 
      r=min(1.0,(rumn4(i,:)+rumn3(i,:)+rumnu(i,:))/ &
           (1.0D-80+p_xqn(i)*p_qnRc(i)*log((p_xdiv(i)+1.0))/dl*phytoc))
      rumn4(i,:)=rumn4(i,:)*r
      rumn3(i,:)=rumn3(i,:)*r
      rumnu(i,:)=rumnu(i,:)*r

      lim_qu(:)=1.0D+00;
! max pot. uptake
      call PhaeocystisCalc(CALC_REL_PHOSPHATE_UPTAKE,i,lim_qu(:),N1p,p_qup(i))
      rump1(i,:)  =  max(ZERO,p_qup(i)* N1p(:)* phytoc(:)*et *lim_qu(:) ) 
! max pot. uptake of org. P
      call PhaeocystisCalc(CALC_REL_PHOSPHATE_UPTAKE,i,lim_qu(:),up(:),p_qup(i))
      rumpu(i,:)  =  max(ZERO,p_qup(i)*up(:)* phytoc(:)*et*lim_qu(:)*cqup(:))  
!maxmial uptake at maximal net growth 
      r=min(1.0,(rump1(i,:)+rumpu(i,:))/ &
           (1.0D-80+p_xqp(i)*p_qpRc(i)*log((p_xdiv(i)+1.0))/dl*phytoc))
      rump1(i,:)=rump1(i,:)*r
      rumpu(i,:)=rumpu(i,:)*r
! max pot. uptake of N5
      rums5(i,:)  =  min(max(ZERO,p_qus(i)*N5s(:)* phytoc(:)*et), &  
           p_xqs(i)*p_qsRc(i)*log((p_xdiv(i)+1.0))/dl*phytoc)
!     call findnan( rumn4(i,:),NO_BOXES,iout )
!     if (iout.gt.0) write(LOGUNIT,*) 'rumn4=',i,rumn4(i,iout)

      t_rumn4(:)=t_rumn4(:)+rumn4(i,:)
      t_rumn3(:)=t_rumn3(:)+rumn3(i,:)
      t_rumnu(:)=t_rumnu(:)+rumnu(i,:)
      t_rump1(:)=t_rump1(:)+rump1(i,:)
      t_rumpu(:)=t_rumpu(:)+rumpu(i,:)
      t_rums5(:)=t_rums5(:)+rums5(i,:)

     enddo

     !Bacteria
     un(:)=max(ZERO,R1n(:)- R1c(:) *  p_qnUlc)  ! rich R1 :urea
     up(:)=max(ZERO,R1p(:)- R1c(:) *  p_qpPhc)  ! rich R1 :
     cqun3(:)  =  p_lN3N4B/( p_lN3N4B+ N4n(:))
     cquR1n(:)  =  p_luN4B/( p_luN4B+ N4n(:))
     cqup(:)  =  p_lN1B/( p_lN1B+ N1p(:))
! max pot. uptake of N4
     rumn4B(:)  =  max(ZERO,et*p_qunB* N4n(:)* B1c(:))  
! max pot. uptake of N3
     rumn3B(:)  =  max(ZERO,et*p_qunB* N3n(:)* B1c(:)* cqun3(:))  
! max pot. uptake of R1n
     rumnuB(:)  =  max(ZERO,et*p_qunB*  un(:)* B1c(:)* cquR1n(:)) 

  rutB  =   1.0D-80 +et*(p_suR1B* R1c(:)+ p_suR6B* max(0.0,R6c(:)-RZc(:)) +p_suR2B* R2c(:))

! minmial uptake at maximal uptake
     r=min(1.0,rumn4B(:)+rumn3B(:)+rumnuB(:)/ &
                               (1.0D-80+p_qnBc*rutB))
     rumn4B(:)=rumn4B(:)*r
     rumn3B(:)=rumn3B(:)*r
     rumnuB(:)=rumnuB(:)*r

! max pot. uptake
     rumpB(:)   =  max(ZERO,et*p_qupB* N1p(:)* B1c(:) )  
! max pot. uptake of R1n
     rumpuB(:)  =  max(ZERO,et*p_qupB*  up(:)* B1c(:)*cqup(:) )  
!minmial uptake at maximal net growth 
     r=min(1.0,(rumpB(:)+rumpuB(:))/ (1.0D-80+p_qpBc*rutB))
     rumpB(:)=rumpB(:)*r
     rumpuB(:)=rumpuB(:)*r

     t_rumn4(:)=t_rumn4(:)+rumn4B(:)
     t_rumn3(:)=t_rumn3(:)+rumn3B(:)
     t_rumnu(:)=t_rumnu(:)+rumnuB(:)
     t_rump1(:)=t_rump1(:)+rumpB(:)
     t_rumpu(:)=t_rumpu(:)+rumpuB(:)

     delta=GetDelta()
     lim_rumn4(:)=min(t_rumn4(:),p_s_max* (N4n(:))/delta)/(p_small+t_rumn4(:))
     lim_rumn3(:)=min(t_rumn3(:),p_s_max* N3n(:)/delta)/(p_small+t_rumn3(:))
     r(:)=max(ZERO,R1n(:)- R1c(:) * (p_qnUc+ p_qnBc)*0.5)  ! rich R1 :
     lim_rumnu(:)=min(t_rumnu(:),p_s_max*r(:)/delta)/(p_small+t_rumnu(:)); 
     lim_rump1(:)=min(t_rump1(:),p_s_max *N1p(:)/delta)/(p_small+t_rump1(:)); 
     r(:)=max(ZERO,R1p(:)- R1c(:) * p_qpPhc)  ! rich R1 :
     lim_rumpu(:)=min(t_rumpu(:),p_s_max*r(:)/delta)/(p_small+t_rumpu(:));
     lim_rums5(:)=min(t_rums5(:),p_s_max*N5s(:)/delta)/(p_small+t_rums5(:));

     do i = 1 , iiPhytoPlankton
        rumn4(i,:)=rumn4(i,:)*lim_rumn4(:)
        rumn3(i,:)=rumn3(i,:)*lim_rumn3(:)
        rumnu(i,:)=rumnu(i,:)*lim_rumnu(:)
        rump1(i,:)=rump1(i,:)*lim_rump1(:)
        rumpu(i,:)=rumpu(i,:)*lim_rumpu(:)
        rums5(i,:)=rums5(i,:)*lim_rums5(:)
     enddo
     rumn4B(:)=rumn4B(:)*lim_rumn4(:)
     rumn3B(:)=rumn3B(:)*lim_rumn3(:)
     rumnuB(:)=rumnuB(:)*lim_rumnu(:)
     rumpB(:)=rumpB(:)*lim_rump1(:)
     rumpuB(:)=rumpuB(:)*lim_rumpu(:)

  end
  !BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

