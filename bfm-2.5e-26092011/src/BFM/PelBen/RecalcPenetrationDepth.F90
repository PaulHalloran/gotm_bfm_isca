#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL
!	   BFM - Biogeochemical Flux Model version 2.3
!
! FUNCTION
!   RecalcPenetrationDepth.f90
!
! FILE
!   RecalcPenetrationDepth.f90
!
! DESCRIPTION
!   
!	In subroutine RecalcPenetrationDepth the penetration depth of detritus is recalculated when
!       detritius is sedimentated on top of the sediment in such a way that the amount of detritus
!       in the lower layers keep the same mass. 
!       The new thickness is calculated Newton's rule of approximation:
!           x(i+1)=x(i)-f(x)/f'(x for f(x)==0
!  
! !INTERFACE
    subroutine RecalcPenetrationDepth(D1m, Dxm, input, mass,newDxm )
!
! !AUTHORS
!   Piet Ruardij   
!
! !USES:
     use global_mem, ONLY:RLEN
     use mem,ONLY:NO_BOXES_XY
     use mem_Param,  ONLY: p_small ,p_clDxm

!
! CHANGE_LOG
!   
!
! COPYING
!   
!   Copyright (C) 2004 P. Ruardij, the mfstep group, the ERSEM team 
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
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!
!
        IMPLICIT  NONE
 
        REAL(RLEN),intent(IN)     ::D1m
        REAL(RLEN),intent(IN)     ::Dxm
        REAL(RLEN),intent(IN)     ::input
        REAL(RLEN),intent(IN)     :: mass
        REAL(RLEN),intent(OUT)    :: newDxm
        
        REAL(RLEN)                ::alpha
        REAL(RLEN)                ::old
        REAL(RLEN)                ::fx
        REAL(RLEN)                ::dfx
        REAL(RLEN)                ::c
        REAL(RLEN)                ::newalpha
      
        alpha=1.0/max(p_clDxm,Dxm)
        ! calculated mass present below D1m
        old = mass * exp(-alpha *D1m)/alpha 

        !start iteration with old_alpha
        newalpha= alpha
        if ( abs(input) > p_small) then
          c=1.0
          do while ( c > 1.e-5) 
            !calc mass below D1m with new input
            fx = (mass +input) * exp(-newalpha *D1m)/newalpha
            !determin first derivative
            dfx  = fx * ( -D1m -1.0/newalpha ) 
            !keep old value
            c =newalpha
            ! calc new alpha for (fx-old)==0
            newalpha = newalpha - (fx-old) /dfx
            ! use c to calculate iteration precision
            c = abs(c -newalpha)/c
          enddo
       endif

        newDxm=1.0/newalpha
        return
        end



