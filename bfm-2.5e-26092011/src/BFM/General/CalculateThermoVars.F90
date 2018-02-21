#INCLUDE "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: CalculateThermoVars
!
! DESCRIPTION
!   !	This submodel calls all other submodels
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine CalculateThermoVars
!
! !USES:
  ! The following 0-d global parameters are used: CalcPelagicFlag, &
  ! CalcBenthicFlag
  ! The following global constants are used: RLEN
  ! The following constants are used: BENTHIC_RETURN, BENTHIC_BIO, BENTHIC_FULL

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem,  ONLY: ETW, Depth, SdTdzTH, TMLd, BMLd,OCDepth
  use mem, ONLY: BoxNumberX,BoxNumberY,BoxNumberZ,BoxNumberXY, &
                 NO_BOXES_X,NO_BOXES_Y,NO_BOXES_Z,NO_BOXES
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following global functions are used:ResetTotMassVar
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use global_interface,   ONLY: ResetTotMassVar
  use mem_Param, ONLY: p_mdTdz

!  
!
! !AUTHORS
!   Piet Ruardij
!
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
  integer                            :: i,f,t
  integer                             :: iiTMLd, iiBMLd,iimdTdz
  real(RLEN),dimension(NO_BOXES_Z)   :: r, s,dz
  real(RLEN)                :: h
  real(RLEN),dimension(1)   :: smax,rmax, smin

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
! IN combination with gotm OCDepth is already calculated in gotm_bfm.F90
!     OCDepth(NO_BOXES_Z) =Depth(t)
!     do i=NO_BOXES_Z-1,1,-1
!        OCDepth(i)=depth(f+i)+ OCDepth(n+1)
!     enddo
     BoxNumberXY=D2toD1(BoxNumberX,BoxNumberY)
     dz(f:t-1)=0.5*(Depth(f:t-1)+Depth(1+f:t))
     r(f:t-1)=(ETW(1+f:t)-ETW(f:t-1))/ DZ(f:t-1)
     h=maxval((r(f:t-1)))
     if ( h >p_mdTdz)  then
       ! Find interface beween two ;layer with the highest dTdZ
       rmax=maxloc((r(f:t-1))); iimdTdz=rmax(1)+f-1;
       ! Calculated dt2/dz2
       dz(f:t-2)=0.5*(dz(f:t-2)+dz(1+f:t-1))
       s(f:t-2)=(r(1+f:t-1)-r(f:t-2))/dz 
       ! Determine in lower part of the gradient where the maximum is:
       ! depth of the the top of the bottom mixed layer
       smax=maxloc((s(f:iimdTdz)));iiBMLd=smax(1)+f-1;
       BMLd(BoxNumberXY)=-OCDepth(iiBMLd+1)
       ! Determine in upper part of the gradient where the minimum is:
       ! depth of the top/surface mixed layer
       smin=minloc((s(iimdTdz:t-1))); iiTMLd=smin(1)+rmax(1)-1
       TMLd(BoxNumberXY)=-OCDepth(iiTMLd+1)
       SdTdzTh=sum(r(iiBMLd:iiTMLd))
    else
       SdTdzTh(BoxNumberXY)=0.0
       BMLd(BoxNumberXY)=0.0;TMLd(BoxNumberXY)=0.0
    endif
   ENDDO
  ENDDO

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
