#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BentoPelCoup
!
! DESCRIPTION
!   !

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine Y3Z2CoupDynamics
!
! !USES:


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN  ! ,LOGUNIT
  use mem, ONLY:    Z2c,sediMeZ,  D2STATE
  use mem, ONLY: BoxNumberZ, NO_BOXES_XY, Yy3c,&
    BoxNumberX, NO_BOXES_X, BoxNumberY, NO_BOXES_Y, BoxNumber, jnetYy3c, &
    ppYy3c,ppY3c,ppY3n,ppY3p,ppZ2c,ppZ2n,ppZ2p,PELBOTTOM, &
    BoxNumberXY, Depth,flux,cZ2m2c,iiPel,iiBen,iiZ2,iiY3

  use mem_Param,  ONLY: CalcMesoZooPlankton,CalcYy3,CalcBenOrganisms
  use mem_FilterFeeder,ONLY:p_qnY3c=>p_qnc,p_qpY3c=>p_qpc
  use mem_MesoZoo,ONLY:p_qnMec=>p_qnc,p_qpMec=>p_qpc
  use mem_FilterFeeder,ONLY:p_heighty,p_pYy3Z2
  use mem_globalfun,   ONLY: insw
! use mem,  ONLY: Output2d_1,Output2d_2,Output2d_3, Output2d_4

!  
!
! !AUTHORS
!   Piet Ruardij  
!
!
! !REVISION_HISTORY
!   Created at Mon Apr 19 00:08:12 CEST 2004
!
!
! COPYING
!   
!   Copyright (C) 2006 P. Ruardij & M. Vichi
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
  real(RLEN)  :: max_change,delta
  real(RLEN)  :: corr
  real(RLEN)  :: ruY3c,ruY3n,ruY3p
  real(RLEN)  :: toZ2c
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! user defined external functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer, external  :: D3toD1
  integer, external  :: D2toD1
  real(RLEN), external  :: GetDelta
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if (CalcMesoZooPlankton(iiZ2) .and. CalcYy3 .and.CalcBenOrganisms(iiY3) ) then

     delta=GetDelta()
     max_change=0.05D+00/delta

     BoxNumberZ = 1
     DO BoxNumberY=1,NO_BOXES_Y
       DO BoxNumberX=1,NO_BOXES_X
         BoxNumber=D3toD1(BoxNumberX,BoxNumberY,BoxNumberZ)
         BoxNumberXY=D2toD1(BoxNumberX,BoxNumberY)

          toZ2c=jnetYy3c(BoxNumberXY)* p_pYy3Z2*insw(Yy3c(BoxNumberXY))
          call flux(BoxNumberXY,iiBen,ppYy3c,ppYy3c,-toZ2c)
          call flux(BoxNumberXY,iiBen,ppY3c,ppY3c,-toZ2c)
          call flux(BoxNumberXY,iiBen,ppY3n,ppY3n,-toZ2c*p_qnY3c)
          call flux(BoxNumberXY,iiBen,ppY3p,ppY3p,-toZ2c*p_qpY3c)

           call flux(BoxNumber,iiPel,&
                  ppZ2c,ppZ2c,toZ2c/Depth(BoxNumber))
           if ( ppZ2n>0) call flux(BoxNumber,iiPel,&
                  ppZ2n,ppZ2n,toZ2c*p_qnY3c/Depth(BoxNumber))
           if ( ppZ2p>0) call flux(BoxNumber,iiPel,&
                  ppZ2p,ppZ2p,toZ2c*p_qpY3c/Depth(BoxNumber))

           call CorrectConcNearBed(Depth(BoxNumber),sediMeZ(iiZ2,BoxNumber) &
                                    , p_heighty,1.0D+40,corr)

           ruY3c  = min(sediMeZ(iiZ2,BoxNumber) *  p_heighty*corr* Z2c(BoxNumber), &
                                            max_change*cZ2m2c(BoxNUmberXY))
!          Output2d_2(BoxNumberXY)=ruY3c

           ruY3n  =   ruY3c*p_qnMec(iiZ2)
           ruY3p  =   ruY3c*p_qpMec(iiZ2)

           call flux(BoxNumber,iiPel,ppZ2c,ppZ2c,-ruY3c/Depth(BoxNumber))
           if (ppZ2n>0)call flux(BoxNumber,iiPel,&
                             ppZ2n,ppZ2n,-ruY3n/Depth(BoxNumber))
           if (ppZ2p>0)call flux(BoxNumber,iiPel,&
                             ppZ2p,ppZ2p,-ruY3p/Depth(BoxNumber))

           PELBOTTOM(ppZ2c,BoxNumberXY)=-ruY3c +toZ2c
           if ( ppZ2n>0)PELBOTTOM(ppZ2n,BoxNumberXY)=-ruY3n+toZ2c*p_qnY3c
           if ( ppZ2p>0)PELBOTTOM(ppZ2p,BoxNumberXY)=-ruY3p+toZ2c*p_qpY3c

           call flux(BoxNumberXY,iiBen,ppYy3c,ppYy3c,ruY3c)
           call flux(BoxNumberXY,iiBen,ppY3c,ppY3c,ruY3c)
           call flux(BoxNumberXY,iiBen,ppY3n,ppY3n,ruY3n)
           call flux(BoxNumberXY,iiBen,ppY3p,ppY3p,ruY3p)
       end DO
     end DO
    

   endif

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
