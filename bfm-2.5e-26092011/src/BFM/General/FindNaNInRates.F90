#INCLUDE "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: FindNaNInRates
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
  subroutine FindNaNInRates(iiSys,ppState,message)
!
! !USES:

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem,    ONLY: iiBen, iiPel,Source_D2_vector,Source_D3_vector, &
              NO_BOXES_XY,NO_BOXES,D2STATE,D3STATE
  use bio_var,ONLY: var_names, stPelStates,stBenStates
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following global functions are used:ResetTotMassVar
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

!  
!
! !AUTHORS
!   ERSEM team	
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

  integer,intent(IN)                   ::iiSys
  integer,intent(IN)                   ::ppState
  character(len=*)                     ::message

  real(RLEN),dimension(NO_BOXES)    :: r3
  real(RLEN),dimension(NO_BOXES_XY) :: r2
  integer                           :: jout

  
  jout=0;
  select case (iiSys)
    case (iiPel) 
      r3=Source_D3_vector(ppState)
      call findnan(r3,NO_BOXES,jout)
      if ( jout>0) then
         write(logunit,'(A)') message
         write(logunit,'(A,A,A,I2)') &
                'NaN in rate ',trim(var_names(stPelStateS+ppState-1)),' layer:',jout
         write(logunit,'(A,''('',I2,'')='',G12.6)') &
              trim(var_names(stPelStateS+ppState-1)),jout,D3STATE(ppSTATE,jout)
      endif
    case (iiBen)
      r2=Source_D2_vector(ppState)
      call findnan(r2,NO_BOXES_XY,jout)
      if ( jout>0) then
         write(logunit,'(A)') message
         write(logunit,'(A,A,A,I2)') &
               'NaN in rate ',trim(var_names(stBenStateS+ppState-1)),' layer:',jout
         write(logunit,'(A,''('',I2,'')='',G12.6)') &
              trim(var_names(stBenStateS+ppState-1)),jout,D2STATE(ppSTATE,jout)
      endif
   end select
  end

 subroutine findnan( vector,n,iout )
      use global_mem, only:RLEN,LOGUNIT
 
      implicit none
      integer,intent(IN)               :: n
      REAL(RLEN),intent(IN)            :: vector(n)
      integer,intent(OUT)              :: iout

      integer      ::i

      do i=1,n
        if (isnan(vector(i))) then
            write(LOGUNIT,*) '-------------case:NaN----------'
            iout=i
            return
        endif
!       if (abs(vector(i))>1.0D+8 ) then
!           write(LOGUNIT,*) '-------------case:InfL----------'
!           iout=i
!           return
!       endif
!       if (abs(vector(i))<1.0D-80 ) then
!           write(LOGUNIT,*) '-------------case:InfS----------'
!           iout=i
!           return
!       endif
!       if ((vector(i))<-1.0D-80 ) then
!           write(LOGUNIT,*) '-------------case:negative----------'
!           iout=i
!           return
!       endif
      enddo

      iout=0
      return
   end


!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
