#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: CheckMassConservationNPS
!
! DESCRIPTION
!   !

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine CheckMassConservationNPSDynamics
!
! !USES:
  ! The following Pelagic-states are used (NOT in fluxes): P1p, P2p, P3p, P4p, &
  ! B1p, Z3p, Z4p, Z5p, Z6p, R1p, R6p, N1p, P1n, P2n, P3n, P4n, B1n, Z3n, Z4n, &
  ! Z5n, Z6n, R1n, R6n, N3n, N4n, O4n, P1s, R6s, N5s
  ! The following Benthic-states are used (NOT in fluxes): Q1p, Q6p, Q1n, Q6n, &
  ! Q6s, Y1p, Y2p, Y3p, Y4p, Y5p, H1p, H2p, Q11p, K1p, K11p, Y1n, Y2n, Y3n, &
  ! Y4n, Y5n, H1n, H2n, Q11n, K4n, K14n, K21p, G4n, K3n, K24n, K5s
  ! The following Benthic 1-d global boxvars got a value: totBENp, totBENn, &
  ! totBENs
  ! The following 0-d global parameters are used: CalcBenthicFlag
  ! The following global constants are used: RLEN
  ! The following constants are used: BENTHIC_RETURN, BENTHIC_BIO, BENTHIC_FULL

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
#IFDEF NOPOINTERS
  use mem,  ONLY: D2STATE,D3STATE
#ELSE
  use mem, ONLY: P1p, P2p, P3p, P4p, B1p,  R1p, R6p, N1p, &
    P1n, P2n, P3n, P4n, B1n, R1n, R6n, N3n, N4n, O4n, P1s, &
    R6s, N5s, D2STATE
  use mem, ONLY: Q1p, Q6p, Q1n, Q6n, Q6s, Y1p, Y2p, Y3p, Y4p, Y5p, H1p, H2p, &
    Q11p, K1p, K11p, Y1n, Y2n, Y3n, Y4n, Y5n, H1n, H2n, Q11n, K4n, K14n, K21p, &
    G4n, K3n, K24n, K5s, K15s, D3STATE
#ENDIF
  use mem, ONLY: ppP1p, ppP2p, ppP3p, ppP4p, ppB1p, ppZ3p, ppZ4p, ppZ5p, ppZ6p, &
    ppR1p, ppR6p, ppN1p, ppP1n, ppP2n, ppP3n, ppP4n, ppB1n, ppZ3n, ppZ4n, ppZ5n, &
    ppZ6n, ppR1n, ppR6n, ppN3n, ppN4n, ppO4n, ppP1s, ppR6s, ppN5s, D2STATE, Depth
  use mem, ONLY: ppQ1p, ppQ6p, ppQ1n, ppQ6n, ppQ6s, ppY1p, ppY2p, &
    ppY3p, ppY4p, ppY5p, ppH1p, ppH2p, ppQ11p, ppK1p, ppK11p, ppY1n, ppY2n, &
    ppY3n, ppY4n, ppY5n, ppH1n, ppH2n, ppQ11n, ppK4n, ppK14n, ppK21p, &
    ppG4n, ppK3n, ppK24n, ppK5s, totBENp, totBENn, totBENs, iiBen, &
    totPELp, totPELn, totPELs, totSYSp, totSYSn, totSYSs, BoxNumberZ,NO_BOXES_XY, &
    NO_BOXES_Z, BoxNumberX, NO_BOXES_X, BoxNumberY, NO_BOXES_Y, BoxNumber, BoxNumberXY,&
    iiPel, flux_vector,ppMicroZooplankton,ppMesoZooPlankton,MicroZooplankton,MesoZooPlankton, &
    iiMicroZooplankton,iiMesoZooPlankton,NO_BOXES,iiC,iiN,iiP,iiS, &
    iiPhytoPlankton,ppPhytoPlankton,PhytoPlankton,iiBenBacteria,ppBenBacteria,BenBacteria, &
    iiBenLabileDetritus,ppBenLabileDetritus,BenLabileDetritus,&
    iiPelDetritus,ppPelDetritus,PelDetritus, iiBenthicAmmonium,ppBenthicAmmonium,&
    BenthicAmmonium, iiBenthicPhosphate,ppBenthicPhosphate,BenthicPhosphate, &
    iiBenOrganisms,ppBenOrganisms,BenOrganisms,jtotBENPELn,jtotBENPELp,jtotBENPELs,PELBOTTOM
 use mem,ONLY: Output2D_1,Output2d_2

  use constants,  ONLY: BENTHIC_RETURN, BENTHIC_BIO, BENTHIC_FULL
  use mem_Param,  ONLY: CalcBenthicFlag
  use mem_MesoZoo, ONLY: p_qnMec=>p_qnc,p_qpMec=>p_qpc
  use mem_MicroZoo, ONLY: p_qnMic=>p_qnc,p_qpMic=>p_qpc
!  
!
! !AUTHORS
!   Piet Ruardij
!
!
! !REVISION_HISTORY
!   Created at Mon Nov 21 09:44:23 CET 2005
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
  real(RLEN),dimension(:),pointer  :: sc
  real(RLEN),dimension(NO_BOXES_Z) :: s
  real(RLEN),dimension(NO_BOXES_Z) :: d
  real(RLEN),dimension(:),pointer  :: rc
  real(RLEN),dimension(NO_BOXES_XY):: r,fl
  integer                           ::i,j,k,l
  integer                           ::f,t

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! user defined external functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer, external  :: D3toD1
  integer, external  :: D2toD1
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  BoxNumberZ = NO_BOXES_Z
  DO BoxNumberY=1,NO_BOXES_Y
    DO BoxNumberX=1,NO_BOXES_X
      f=D3toD1(BoxNumberX,BoxNumberY,1)
      t=D3toD1(BoxNumberX,BoxNumberY,BoxNumberZ)
      BoxNumberXY=D2toD1(BoxNumberX,BoxNumberY)

      totPELp(BoxNumberXY)=0.0D+00;
      totPELn(BoxNumberXY)=0.0D+00;
      totPELs(BoxNumberXY)=0.0D+00;

      jtotBENPELp=0.0
      jtotBENPELn=0.0
      jtotBENPELs=0.0

      d=Depth(f:t);
      do i=1, iiMicroZooplankton
         l= ppMicroZooplankton(i,iiC)
         sc=> MicroZooplankton(i,iiC)
         k=ppMicroZooPlankton(i,iiN)
         if ( k==0) then
           s=sc(f:t);s=s*p_qnMic(i)
           fl=PELBOTTOM(l,:)*p_qnMic(i)
         else
           rc=> MicroZooplankton(i,iiN);s=rc(f:t);
           fl=PELBOTTOM(k,:)
         endif
         totPELn(BoxNumberXY)=totPELn(BoxNumberXY) +sum(s*d)
         jtotBENPELn=jtotBENPELn+fl
         k=ppMicroZooPlankton(i,iiP)
         if ( k==0) then
           s=sc(f:t);s=s*p_qpMic(i)
           fl=PELBOTTOM(l,:)*p_qpMic(i)
         else
           rc=> MicroZooplankton(i,iiP);s=rc(f:t);
           fl=PELBOTTOM(k,:)
         endif
         totPELp(BoxNumberXY)=totPELp(BoxNumberXY) +sum(s*d)
         jtotBENPELp=jtotBENPELp+fl
      enddo
      do i=1, iiMesoZooplankton
         l= ppMesoZooplankton(i,iiC)
         sc=> MesoZooplankton(i,iiC)
         k=ppMesoZooPlankton(i,iiN)
         if ( k==0) then
           s=sc(f:t);s=s*p_qnMec(i)
           fl=PELBOTTOM(l,:)*p_qnMec(i)
         else
           rc=> MesoZooplankton(i,iiN);s=rc(f:t);
           fl=PELBOTTOM(k,:)
         endif
         totPELn(BoxNumberXY)=totPELn(BoxNumberXY) +sum(s*d)
         jtotBENPELn=jtotBENPELn+fl
         k=ppMesoZooPlankton(i,iiP)
         if ( k==0) then
           s=sc(f:t);s=s*p_qpMec(i)
           fl=PELBOTTOM(l,:)*p_qPMec(i)
         else
           rc=> MesoZooplankton(i,iiP);s=rc(f:t);
           fl=PELBOTTOM(k,:)
         endif
         totPELp(BoxNumberXY)=totPELp(BoxNumberXY) +sum(s*d)
         jtotBENPELp=jtotBENPELp+fl
      enddo
      do i=1, iiPhytoPlankton
         sc=> PhytoPlankton(i,iiN); s=sc(f:t);
         totPELn(BoxNumberXY)=totPELn(BoxNumberXY) +sum(s*d)
         fl=PELBOTTOM(ppPhytoPlankton(i,iiN),:)
         jtotBENPELn=jtotBENPELn+fl
         sc=> PhytoPlankton(i,iiP); s=sc(f:t);
         totPELp(BoxNumberXY)=totPELp(BoxNumberXY) +sum(s*d)
         fl=PELBOTTOM(ppPhytoPlankton(i,iiP),:)
         jtotBENPELp=jtotBENPELp+fl
         j=ppPhytoPlankton(i,iiS)
         if (j>0) then
           sc=> PhytoPlankton(i,iiS); s=sc(f:t);
           totPELs(BoxNumberXY)=totPELs(BoxNumberXY) +sum(s*d)
           fl=PELBOTTOM(ppPhytoPlankton(i,iiS),:)
           jtotBENPELs=jtotBENPELs+fl
         endif
      enddo
      do i=1, iiPelDetritus
         if ( ppPelDetritus(i,iiN)>0) then
           sc=> PelDetritus(i,iiN);s=sc(f:t);
           totPELn(BoxNumberXY)=totPELn(BoxNumberXY) +sum(s*d)
           fl=PELBOTTOM(ppPelDetritus(i,iiN),:)
           jtotBENPELn=jtotBENPELn+fl
         endif
         if (ppPelDetritus(i,iiP)>0)then
           sc=> PelDetritus(i,iiP);s=sc(f:t);
           totPELp(BoxNumberXY)=totPELp(BoxNumberXY) +sum(s*d)
           fl=PELBOTTOM(ppPelDetritus(i,iiP),:)
           jtotBENPELn=jtotBENPELn+fl
         endif
         if (ppPelDetritus(i,iiS)>0) then
           sc=> PelDetritus(i,iiS);s=sc(f:t);
           totPELs(BoxNumberXY)=totPELs(BoxNumberXY) +sum(s*d)
           fl=PELBOTTOM(ppPelDetritus(i,iiS),:)
           jtotBENPELs=jtotBENPELs+fl
         endif
      enddo

      totPELp(BoxNumberXY)=totPELp(BoxNumberXY)+ sum((B1p(f:t)+ N1p(f:t))* d)
      totPELn(BoxNumberXY)=totPELn(BoxNumberXY)+ sum((B1n(f:t)+ N3n(f:t)+ N4n(f:t))* d)
      totPELs(BoxNumberXY)=totPELs(BoxNumberXY)+ sum(N5s(f:t)* d)

      jtotBENPELp=jtotBENPELp+PELBOTTOM(ppB1p,:)+PELBOTTOM(ppN1p,:)
      jtotBENPELn=jtotBENPELn+PELBOTTOM(ppB1n,:)+PELBOTTOM(ppN3n,:)+PELBOTTOM(ppN4n,:)
      jtotBENPELs=jtotBENPELs+PELBOTTOM(ppN5s,:)

      totBENp =   0.0D+00
      totBENn =   0.0D+00
      totBENs =   0.0D+00

      select case ( CalcBenthicFlag)
        case ( 0 )
        case ( BENTHIC_RETURN )  ! Simple benthic return
          ! Mass conservation variables
          totBENp  =  ( Q1p+ Q6p)
          totBENn  =  ( Q1n+ Q6n)
          totBENs  =  ( Q6s)
        case ( BENTHIC_BIO )  ! Intermediate benthic return
         do i=1, iiBenOrganisms
           rc => BenOrganisms(i,iiN)
           totBENn=totBENn+rc
           rc => BenOrganisms(i,iiP)
           totBENp=totBENp+rc
         enddo
         do i=1, iiBenLabileDetritus
           rc => BenLabileDetritus(i,iiN)
           totBENn=totBENn+rc
           rc => BenLabileDetritus(i,iiP)
           totBENp=totBENp+rc
         enddo
         do i=1, iiBenBacteria
           rc => BenBacteria(i,iiN)
           totBENn=totBENn+rc
           rc => BenBacteria(i,iiP)
           totBENp=totBENp+rc
         enddo
          ! Mass conservation variables
          totBENp = totBENp +Q6p+ K1p+ K11p
          totBENn = totBENn +Q6n+ K4n+ K14n
          totBENs  =  ( Q6s)
    
        case ( BENTHIC_FULL )  ! Full benthic nutrients
         do i=1, iiBenOrganisms
           rc => BenOrganisms(i,iiN);
           totBENn=totBENn+rc
           rc => BenOrganisms(i,iiP)
           totBENp=totBENp+rc
         enddo
         do i=1, iiBenLabileDetritus
           rc => BenLabileDetritus(i,iiN)
           totBENn=totBENn+rc
           rc => BenLabileDetritus(i,iiP)
           totBENp=totBENp+rc
         enddo
         do i=1, iiBenBacteria
           rc => BenBacteria(i,iiN)
           totBENn=totBENn+rc
           rc => BenBacteria(i,iiP)
           totBENp=totBENp+rc
         enddo
         do i=1,iiBenthicPhosphate
           rc => BenthicPhosphate(i,iiP)
           totBENp=totBENp+rc
         enddo
         do i=1,iiBenthicAmmonium
           rc => BenthicAmmonium(i,iiN)
           totBENn=totBENn+rc
         enddo
         totBENn =totBENn + Q6n+K3n
         totBENp =totBENp + Q6p
         totBENs =totBENs + K5s+K15s+Q6s
      end select
    
      totSYSn=totPELn+totBENn
      totSYSp=totPELp+totBENp
      totSYSs=totPELs+totBENs
    enddo
  enddo
  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
