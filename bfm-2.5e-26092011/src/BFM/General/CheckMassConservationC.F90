#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: CheckMassConservationC
!
! DESCRIPTION
!   !

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
      subroutine CheckMassConservationCDynamics
!
      use constants, ONLY:RLEN,ZERO,SEC_PER_DAY
      use mem,only:D3SINK,D3SOURCE,D2SINK,D2SOURCE,D2STATE,D3STATE
      use mem,only:jupPELc,jminPELc,jupBenc,jminBENc,jY3O3c,jtotbenpelc
      use mem,only:B1c,Q1c,Q6c,totPelc,totBENc,totPELInc,totBENInc
      use mem,only:totPELh,totBENh,PELBOTTOM,Depth
      use mem,only:NO_BOXES_X,NO_BOXES_Y,NO_BOXES_Z,NO_BOXES
      use mem,only:BoxNumberX,BoxNumberY,BoxNumberXY,BoxNumberZ
      use mem,only:iiBen,iiPel,ppB1c,ppO3c,ppG3c,ppG13c,ppG23c, &
         ppO3h,ppG3h,ppG13h,ppG23h,iiC,iiH2,iiH3
      use mem,only:ppMicroZooplankton,ppMesoZooPlankton,&
        iiMicroZooplankton,iiMesoZooPlankton,&
        iiPhytoPlankton,ppPhytoPlankton,iiBenBacteria,ppBenBacteria,&
        iiBenOrganisms,ppBenOrganisms
      use mem,only:MicroZooplankton,MesoZooPlankton,PhytoPlankton, &
        BenBacteria, BenOrganisms,ppPelDetritus, &
        iiPelDetritus,PelDetritus,iiBenLabileDetritus,BenLabileDetritus
      use mem_Param,only:CalcBenthicFlag
      use mem_PelCO2, ONLY: p_qhK4K3n
      use constants,  ONLY: BENTHIC_RETURN, BENTHIC_BIO, BENTHIC_FULL

      implicit none
      real(RLEN),dimension(:),pointer  :: sc
      real(RLEN),dimension(NO_BOXES_Z) :: d
      integer                           ::i,j,nr
      integer                           ::f,t
    
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      ! user defined external functions
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
       integer, external  :: D3toD1
       integer, external  :: D2toD1
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


      BoxNumberZ = NO_BOXES_Z
      DO BoxNumberY=1,NO_BOXES_Y
        DO BoxNumberX=1,NO_BOXES_X
         f=D3toD1(BoxNumberX,BoxNumberY,1)
         t=D3toD1(BoxNumberX,BoxNumberY,BoxNumberZ)
         BoxNumberXY=D2toD1(BoxNumberX,BoxNumberY)

         d=Depth(f:t)
         jtotbenpelc=0.0
         jupPELc=0.0
         jminPELc=0.0
         totPELc=0.0
         totPELInc=0.0
         totPELh=0.0

         nr=ppO3c
         do i=1,iiPhytoPlankton
            j=ppPhytoPlankton(i,iiC)
            if (ppO3c.eq.0) nr=j
            jupPELc(BoxNumberXY)=jupPELc(BoxNumberXY) &
                    +sum(D3SOURCE(j,nr,f:t)*d)*SEC_PER_DAY 
            if (ppO3c.gt.0)  &
              jminPELc(BoxNumberXY)=jminPELc(BoxNumberXY) &
                    +sum(D3SINK(j,nr,f:t)*d)*SEC_PER_DAY
            sc=> PhytoPlankton(i,iiC); 
            totPELc(BoxNumberXY)=totPELc(BoxNumberXY) +sum(sc(f:t)*d)
            jtotbenpelc(BoxNumberXY)=jtotbenpelc(BoxNumberXY) &
                                         +PELBOTTOM(j,BoxNumberXY)
         enddo
         do i=1,iiMesoZooPlankton
            j=ppMesoZooPlankton(i,iiC)
            if (ppO3c.eq.0) nr=j
            jminPELc(BoxNumberXY)=jminPELc(BoxNumberXY) &
                    +sum(D3SINK(j,nr,f:t)*d) *SEC_PER_DAY
            sc=> MesoZooplankton(i,iiC)
            totPELc(BoxNumberXY)=totPELc(BoxNumberXY) +sum(sc(f:t)*d)
            jtotbenpelc(BoxNumberXY)=jtotbenpelc(BoxNumberXY)&
                                 +PELBOTTOM(j,BoxNumberXY)
         enddo
         do i=1,iiMicroZooPlankton
            j=ppMicroZooPlankton(i,iiC)
            if (ppO3c.eq.0) nr=j
            jminPELc(BoxNumberXY)=jminPELc(BoxNumberXY) &
                    +sum(D3SINK(j,nr,f:t)*d) *SEC_PER_DAY
            sc=> MicroZooplankton(i,iiC)
            totPELc(BoxNumberXY)=totPELc(BoxNumberXY) +sum(sc(f:t)*d)
            jtotbenpelc(BoxNumberXY)=jtotbenpelc(BoxNumberXY)&
                                      +PELBOTTOM(j,BoxNumberXY)
         enddo
         do i=1, iiPelDetritus
           j= ppPelDetritus(i,iiC)
           sc=> PelDetritus(i,iiC)
           totPELc(BoxNumberXY)=totPELc(BoxNumberXY) +sum(sc(f:t)*d)
           jtotbenpelc(BoxNumberXY)=jtotbenpelc(BoxNumberXY)&
                                   +PELBOTTOM(j,BoxNumberXY)
         enddo

         totPELc(BoxNumberXY)=totPELc(BoxNumberXY)+ sum(B1c(f:t)* d)
         !jtotbenpelc and B1c : no flux to sediment of B1
         if ( ppO3c.eq.0) nr=ppB1c
         jminPELc(BoxNumberXY)=jminPELc(BoxNumberXY) &
                    +sum(D3SINK(ppB1c,nr,f:t)*d)*SEC_PER_DAY

         if ( ppO3c.gt.0) & 
         totPELInc(BoxNumberXY)=sum(D3STATE(ppO3c,f:t)*d)
         if ( ppO3h.gt.0)  totPELh(BoxNumberXY)= &
                     sum(D3STATE(ppO3h,f:t)*d) 
         
         !respiration of FIlterFeeders(Y3), which take partly place in pelagic
         jminPELc(BoxNumberXY)=jminPELc(BoxNumberXY) + jY3O3c(BoxNumberXY)

         jminBENc=0.0
         jupBENc=0.0
         totBENc = 0.0D+00
         totBENInc=0.0
         totBENh=0.0
         select case ( CalcBenthicFlag)
          case ( 0 )
          case ( BENTHIC_RETURN )  ! Simple benthic return
            ! Mass conservation variables
            totBENc  =  ( Q1c+ Q6c)
          case ( BENTHIC_BIO,BENTHIC_FULL )  ! Intermediate benthic return
            nr=ppG3c
            do i=1, iiBenOrganisms
              j=ppBenOrganisms(i,iiC)
              if ( ppG3c.eq.0) nr=j
              jminBENc(BoxNumberXY)=jminBENc(BoxNumberXY) &
                    +D2SINK(j,nr,BoxNumberXY) *SEC_PER_DAY
              sc => BenOrganisms(i,iiC)
              totBENc(BoxNumberXY)=totBENc(BoxNumberXY)+sc(BoxNumberXY)
            enddo
            do i=1, iiBenLabileDetritus
              sc => BenLabileDetritus(i,iiC)
              totBENc(BoxNumberXY)=totBENc(BoxNumberXY)+sc(BoxNumberXY)
            enddo
            do i=1, iiBenBacteria
              j=ppBenBacteria(i,iiC)
              if (i.ne.iiH2) then
                nr=ppG3c;if ( ppG3c.eq.0) nr=j
                if (i.eq.iiH3)jupBENc(BoxNumberXY)=jupBENc(BoxNumberXY)&
                            +D2SOURCE(j,nr,BoxNumberXY) *SEC_PER_DAY
                !Aerobic bacteria+ nitrifiers
                jminBENc(BoxNumberXY)=jminBENc(BoxNumberXY) &
                    +D2SINK(j,nr,BoxNumberXY) *SEC_PER_DAY
              else
                !Anaerobic bacteria
                nr=ppG13c;if (ppG13c.eq.0) nr=j
                jminBENc(BoxNumberXY)=jminBENc(BoxNumberXY) &
                    +D2SINK(j,nr,BoxNumberXY) *SEC_PER_DAY
                nr=ppG23c;if (ppG23c.gt.0) & 
                   jminBENc(BoxNumberXY)=jminBENc(BoxNumberXY) &
                      +D2SINK(j,nr,BoxNumberXY) *SEC_PER_DAY
              endif
              sc => BenBacteria(i,iiC)
              totBENc(BoxNumberXY)=totBENc(BoxNumberXY)+sc(BoxNumberXY)
            enddo
            ! Mass conservation variables
            totBENc(BoxNumberXY) = totBENc(BoxNumberXY)+Q6c(BoxNumberXY)
            if ( ppG3c.gt.0) & 
               totBENInc(BoxNumberXY)=D2STATE(ppG3c,BoxNumberXY) &
               +D2STATE(ppG13c,BoxNumberXY)+D2STATE(ppG23c,BoxNumberXY)
            if ( ppG3h.gt.0)  totBENh(BoxNumberXY)= &
                D2STATE(ppG3h,BoxNumberXY) &
               +D2STATE(ppG13h,BoxNumberXY)+D2STATE(ppG23h,BoxNumberXY)
         end select
        enddo
      enddo

      return
      end
