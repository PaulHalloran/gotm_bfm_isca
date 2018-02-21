#INCLUDE "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: Settling
!
! DESCRIPTION
!   This process describes the dynamics of sedimentation and
!    deposition of phytoplankton (P1, P2, P3, P4, ...) and detritus (R6)
!    in the benthic system.
!    A burial velocity is defined, which controls the magnitude
!    of the inflow rate of detritus from the pelagic form R6 to
!    the benthic form Q6.
!    The processes described here taks only place in the lowest
!    boxes in the Z-direction.
!
!

!   This file is generated directly from OpenSesame model code, using a code
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine SettlingDynamics
!
! !USES:

  ! For the following Pelagic-states fluxes are defined: R1c, R6c, R6n, &
  ! R1p, R6p, R6s
  ! For the following Pelagic-group-states fluxes are defined: PhytoPlankton
  ! The following global scalar vars are used: &
  ! BoxNumberZ, BoxNumberX, NO_BOXES_X, BoxNumberY, NO_BOXES_Y, &
  ! BoxNumber, BoxNumberXY
  ! The following Pelagic 1-d global boxvars  are used: Depth
  ! The following Benthic 1-d global boxvars are modified : jbotR6c, &
  ! jbotR6n, jbotR6p, jbotR6s, jbotR1c, jbotR1n, jbotR1p
  ! The following Pelagic 2-d global boxvars  are used: sediPI
  ! The following groupmember vars  are used: iiPhytoPlankton, iiP1
  ! The following constituent constants  are used: iiC, iiN, iiP, iiS
  ! The following 0-d global parameters are used: &
  ! p_pe_R1c, p_pe_R1n, p_pe_R1p
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
  use mem,  ONLY: R6c, RZc, R6n, R6p, R6s, PhytoPlankton, D3STATE
  use mem, ONLY: ppR1c, ppRZc, ppR6c, ppR1n, ppR6n, ppR1p, ppR6p, &
    ppR3c,ppR6s, ppPhytoPlankton, BoxNumberZ, BoxNumberX, NO_BOXES_X, &
    BoxNumberY, NO_BOXES_Y, BoxNumber, BoxNumberXY, Depth, jbotR6c, jbotR6n, jbotR6p, &
    jbotR6s, jbotR1c, jbotR1n, jbotR1p, iiPhytoPlankton, sediR6,sediRZ,sediPI, &
    iiC, iiN, iiP, iiL, iiS, iiPel, PELBOTTOM, flux,jbotR3c
  use mem_Param,  ONLY: p_pe_R1c, p_pe_R1n, p_pe_R1p,p_small
  use mem_Settling
  use Track, ONLY:define_track_bot_rate
!
!
! !AUTHORS
!   ERSEM-team
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
  ! Local Vectors used  of group vectors
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN), dimension(:), pointer  ::lcl_PhytoPlankton
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer  :: i
  integer  :: j
  real(RLEN)  :: delta
  real(RLEN)  :: max_change
  real(RLEN)  :: corr
  real(RLEN)  :: sedi
  real(RLEN)  :: ruQIc
  real(RLEN)  :: ruQIn
  real(RLEN)  :: ruQIp
  real(RLEN)  :: ruQ1c
  real(RLEN)  :: ruQ1n
  real(RLEN)  :: ruQ1p
  real(RLEN)  :: ruQ6c
  real(RLEN)  :: ruQ6n
  real(RLEN)  :: ruQ6p
  real(RLEN)  :: ruQIl
  real(RLEN)  :: ruQ6s
  real(RLEN)  :: s

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! user defined external functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer, external  :: D3toD1
  integer, external  :: D2toD1
  real(RLEN), external  :: GetDelta
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  BoxNumberZ = 1
  DO BoxNumberY=1,NO_BOXES_Y
    DO BoxNumberX=1,NO_BOXES_X
      BoxNumber=D3toD1(BoxNumberX,BoxNumberY,BoxNumberZ)
      BoxNumberXY=D2toD1(BoxNumberX,BoxNumberY)


      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Starting real settling:
      ! phytoplankton settling are tmeporarly stalled as R1 and R6 and
      ! subsequently exported (BentoPelCoup) adn sedimentated in bnethos &
      ! (sedimentation)
      ! All fluxes are seperated in sink and sources fluxes:
      ! to avoid problems with the definitions of Pel. fluxes .
      ! !!!!!!! ALL DETRITUS FLUXES TO THE SEDIMENT ARE DIRECTED VIA R6 TO Q6 !!!!!!!!
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      jbotR6c(BoxNumberXY)  =   0.0D+00
      jbotR6n(BoxNumberXY)  =   0.0D+00
      jbotR6p(BoxNumberXY)  =   0.0D+00
      jbotR6s(BoxNumberXY)  =   0.0D+00

      jbotR1c(BoxNumberXY)  =   0.0D+00
      jbotR1n(BoxNumberXY)  =   0.0D+00
      jbotR1p(BoxNumberXY)  =   0.0D+00

      jbotR3c(BoxNumberXY)  =   0.0D+00

    
       delta=GetDelta()
       max_change=0.05D+00/delta
       do i = 1 , ( iiPhytoPlankton)

        sedi  =   sediPI(i,BoxNumber)
        if ( sedi> 0.0D+00.and.   p_burvel_PI > 0.0  ) then
          call CorrectConcNearBed(Depth(BoxNumber),sedi, p_height,max_change,corr)
          j=ppPhytoPlankton(i,iiC)
          lcl_PhytoPlankton => PhytoPlankton(i,iiC)
          ruQIc  =   corr* p_burvel_PI* lcl_PhytoPlankton(BoxNumber)
          ruQ1c  =   p_pe_R1c* ruQIc
          ruQ6c  =   ruQIc- ruQ1c
          PELBOTTOM(j,BoxNumberXY)=  - ruQIc
          call flux(BoxNumber, iiPel, ppR1c, ppR1c, ruQ1c/ Depth(BoxNumber) )
          call flux(BoxNumber, iiPel, ppR6c, ppR6c, ruQ6c/ Depth(BoxNumber) )
          jbotR1c(BoxNumberXY)  =   jbotR1c(BoxNumberXY)- ruQ1c
          jbotR6c(BoxNumberXY)  =   jbotR6c(BoxNumberXY)- ruQ6c

          j=ppPhytoPlankton(i,iiN)
          lcl_PhytoPlankton => PhytoPlankton(i,iiN)
          ruQIn  =   corr* p_burvel_PI* lcl_PhytoPlankton(BoxNumber)
          call define_track_bot_rate(iiPel,j,BoxNumber,BoxNumberXY,0,0,-ruQIn)
          ruQ1n  =   p_pe_R1n* ruQIn
          ruQ6n  =   ruQIn- ruQ1n
          PELBOTTOM(j,BoxNumberXY)=  - ruQIn
          call flux(BoxNumber, iiPel, ppR1n, ppR1n, ruQ1n/ Depth(BoxNumber) )
          call flux(BoxNumber, iiPel, ppR6n, ppR6n, ruQ6n/ Depth(BoxNumber) )
          jbotR1n(BoxNumberXY)  =   jbotR1n(BoxNumberXY)- ruQ1n
          jbotR6n(BoxNumberXY)  =   jbotR6n(BoxNumberXY)- ruQ6n


          j=ppPhytoPlankton(i,iiP)
          lcl_PhytoPlankton => PhytoPlankton(i,iiP)
          ruQIp  =   corr* p_burvel_PI* lcl_PhytoPlankton(BoxNumber)
          call define_track_bot_rate(iiPel,j,BoxNumber,BoxNumberXY,0,0,-ruQIp)
          ruQ1p  =   p_pe_R1p* ruQIp
          ruQ6p  =   ruQIp- ruQ1p
          PELBOTTOM(j,BoxNumberXY)= - ruQIp
          call flux(BoxNumber, iiPel, ppR1p, ppR1p, ruQ1p/ Depth(BoxNumber) )
          call flux(BoxNumber, iiPel, ppR6p, ppR6p, ruQ6p/ Depth(BoxNumber) )
          jbotR1p(BoxNumberXY) =jbotR1p(BoxNumberXY)- ruQ1p
          jbotR6p(BoxNumberXY) =jbotR6p(BoxNumberXY)- ruQ6p

          j=ppPhytoPlankton(i,iiL)
          lcl_PhytoPlankton => PhytoPlankton(i,iiL)
          ruQIl  =   corr* p_burvel_PI* lcl_PhytoPlankton(BoxNumber)
          PELBOTTOM(j,BoxNumberXY)=  - ruQIl

          j=ppPhytoPlankton(i,iiS)
          if ( j> 0) then
            lcl_PhytoPlankton => PhytoPlankton(i,iiS)
            ruQ6s  =   corr* p_burvel_PI* lcl_PhytoPlankton(BoxNumber)
            call define_track_bot_rate(iiPel,j,BoxNumber,BoxNumberXY,0,0,-ruQ6s)
            PELBOTTOM(j,BoxNumberXY)  = - ruQ6s
            call flux(BoxNumber, iiPel, ppR6s, ppR6s, ruQ6s/ Depth(BoxNumber))
            jbotR6s(BoxNumberXY)  =   jbotR6s(BoxNumberXY)- ruQ6s ! << Phyto
          end if

        else
          PELBOTTOM(ppPhytoPlankton(i,iiC),BoxNumberXY) = 0.0D+00
          PELBOTTOM(ppPhytoPlankton(i,iiN),BoxNumberXY) = 0.0D+00
          PELBOTTOM(ppPhytoPlankton(i,iiP),BoxNumberXY) = 0.0D+00
          PELBOTTOM(ppPhytoPlankton(i,iiL),BoxNumberXY) = 0.0D+00
          j=ppPhytoPlankton(i,iiS)
          if ( j>0) PELBOTTOM(j,BoxNumberXY) = 0.0D+00
        end if
      end do
      PELBOTTOM(ppR3c,BoxNumberXY) = 0.0D+00


      ! R2 into Q1:nd Q6
      ! !!!!!!! ALL DETRITUS FLUXES TO THE SEDIMENT ARE DIRECTED VIA R6 TO Q6 !!!!!!!!
!     if ( sediR2(BoxNumber) > 0.0 .and. p_burvel_R2 > 0.0) then
!       ! Calculate how the intermediate degradable R2 has to be redistrubuted 
!       ! between R1 and R6 in such a way that the degradability is the same

!       ! Calculate first actual degradability of LOC ( dependent of quotum NC,PC)
!       p = min(1.0D+00, R1n(BoxNumber)/(R1c(BoxNumber)* p_qnBc), &
!                                R1p(BoxNumber)/(R1c(BoxNumber)*p_qpBc))
!       ! Calculate actual degradability of R1
!       s= p_suhR1*p-p_sulR1* ( 1.0D+00-p)
!       ! Calculate distribution factor for R2 between R1 and R6
!       p=(p_suR2-p_suR6)/(s-p_suR6)

!       jbotR1c(BoxNumberXY)=jbotR1c(BoxNumberXY)- &
!                                        p*p_burvel_R2 * R2c(BoxNumberXY)
!       jbotR6c(BoxNumberXY)=jbotR6c(BoxNumberXY)-&
!                                (1.0D+00-p)*p_burvel_R2 * R2c(BoxNumberXY)
!       
!     endif
      ! !!!!!!! ALL DETRITUS FLUXES TO THE SEDIMENT ARE DIRECTED VIA R6 TO Q6 !!!!!!!!
      ! R6 into Q6:

      ! Near the sediment fecalpellets fall apart with rate of p_smRZ
      call flux(BoxNumber, iiPel, ppRZc, ppRZc, -p_smRZ*RZc(BoxNumber))

      s= max(0.0,min(sediR6(BoxNumber),(sediR6(BoxNumber)*R6c(BoxNumber)-sediRZ(BoxNumber)*RZc(BoxNumber))/   &
                                                          (p_small+max(0.0,R6c(BoxNumber)-RZc(BoxNumber)))))
      call CorrectConcNearBed(Depth(BoxNumber),s, p_height,max_change,corr)

    
      s=max(0.0,R6c(BoxNumber)-RZc(BoxNumber))/(p_small+R6c(BoxNumber))  
      ruQ6c  =   p_burvel_R6cnp*corr * s*R6c(BoxNumber)
      ruQ6n  =   p_burvel_R6cnp*corr * s*R6n(BoxNumber)
      ruQ6p  =   p_burvel_R6cnp*corr * s*R6p(BoxNumber)
      ruQ6s  =   p_burvel_R6s*corr * R6s(BoxNumber)

      jbotR6c(BoxNumberXY)  =   jbotR6c(BoxNumberXY)- ruQ6c
      jbotR6n(BoxNumberXY)  =   jbotR6n(BoxNumberXY)- ruQ6n
      jbotR6p(BoxNumberXY)  =   jbotR6p(BoxNumberXY)- ruQ6p
      jbotR6s(BoxNumberXY)  =   jbotR6s(BoxNumberXY)- ruQ6s ! << R6
    end DO
  end DO

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
