#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"
#include"cppdefs.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: Silt
!
! DESCRIPTION
!   !    This process describes the additional dynamics of dissolved
!       compounds in the watercolumn. Parameterized processes are:
!       - nitrification
!       - denitrification
!       - reoxidation of reduction equivalents
!        - dissolution of biogenic silica
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine SiltDynamics
!
! !USES:

  ! For the following Pelagic-states fluxes are defined: N4n, N3n, O2o, O4n, &
  ! N6r, R6s, N5s, P1s
  ! The following Pelagic 1-d global boxvars are modified : flN3O4n
  ! The following Pelagic 1-d global boxvars  are used: ETW, flPTN6r, flP1R6s
  ! The following 0-d global parameters are used: p_qon_nitri, p_qro, &
  ! p_qon_dentri
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
#IFDEF NOPOINTERS
  use mem,  ONLY: D3STATE
#ELSE
  use mem,  ONLY: R9x, ppR9x,ERHO,Wind,ETW
#ENDIF
  use mem, ONLY: Depth,ETW, NO_BOXES, iiBen, iiPel, flux_vector,InitializeModel
  use mem_Param,  ONLY: p_small, p_poro
  use global_interface,   ONLY: eTq
  use mem_Silt


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:MM_vector, eTq_vector, insw_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

!  
!
! !AUTHORS
!   Original version by P. Ruardij and  J. van der Molen
!
!
!
! !REVISION_HISTORY
!   ! 2007: JM introduced simple wave-driven resuspension model
!   ! 2009: JM introduced concentration-dependent settling rate
!   ! April 2010: JM: changed to work without using rate calculation
!                     changed to produce linear depth dependence
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
     REAL(RLEN)                        :: delta
     REAL(RLEN)                        :: psilt
     REAL(RLEN)                        :: tdepth
     REAL(RLEN),dimension(NO_BOXES)    :: new_R9x
     REAL(RLEN),dimension(NO_BOXES)    :: rate
     REAL(RLEN)                        :: U, Hs, Tz, tau, uw, rate_1, R9x_new, R9x_old
     REAL(RLEN)                        :: k_timelag, R9x_surf, R9x_bot, R9x_grad
     REAL(RLEN)                        :: deltatemp,strat_thresh
     REAL(RLEN), PARAMETER             :: g=9.81, fw=0.1, psilt_min=0.15
     REAL(RLEN), PARAMETER             :: kt0=-0.02, kt1=8.0 !E-5
     REAL(RLEN), PARAMETER             :: strat_fact=2
     INTEGER                           :: n
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     real(RLEN), external  :: GetDelta
     character :: ch

     delta=GetDelta()
     psilt=(p_poro(1) - 0.38662 )/ 0.00415
     tdepth=sum(Depth)
     select case (method)
       case (1)
        STDERR , 'silt=', R9x(1:2)
        delta=GetDelta()
        psilt=max(0.01,(p_poro(1) - 0.38662 )/ 0.00415)
        new_R9x(:) = 10000.0 * min(1.0,10.0/tdepth)* max(0.5,psilt)/7.0 &
                      /eTq(  ETW(1), 2.0D+00) 
        if ( new_R9x(1) < 0.0 ) then
         STDERR, 'NewSilt=', new_R9x(1:2)
        endif
        select case ( InitializeModel ) 
         case(0) 
           rate=(new_R9x(:)-R9x(:))/delta
          case(1)
           rate=new_R9x(:)/delta;
        end select
        select case (InitializeModel)         
          case (0)
            call flux_vector(iiPel,ppR9x,ppR9x,rate)
          case (1)
            R9x(:)=rate*delta
          end select
       case (2)
         ! simple wave generation: vRijn p 331
         U=0.7*(Wind**1.2)
         Hs=max(1.0,0.243*(U**2)/g)
         Hs=min(Hs,0.4*tdepth)
         Tz=max(5.0,8.14*U/g)

         ! shear stress calculation
         uw=PI*Hs/(Tz*sinh(2*PI/Tz*sqrt(tdepth/g)))     !JM linear wave theory
         tau=0.5*fw*ERHO(1)*(uw**2)                     !JM 0.5*fw*rho*u**2

         ! update concentration
         k_timelag=max(_ZERO_,kt0+kt1*R9x(NO_BOXES))
         R9x_old=R9x(NO_BOXES)*exp(-k_timelag*delta/(24*3600))   ! time lag in settling
         R9x(NO_BOXES)=100.0 * max(psilt_min,psilt/100.0) * tau
         R9x(NO_BOXES)=max(R9x(NO_BOXES),R9x_old)

         ! create linear depth dependence
         R9x_surf=R9x(NO_BOXES)
         R9x_bot=R9x_surf/p_pSurBot
         R9x_grad=(R9x_bot-R9x_surf)/(tdepth-0.5*(Depth(1)+Depth(NO_BOXES)))
         R9x(1)=R9x_bot
         do n=2,NO_BOXES-1
           R9x(n)=R9x(n-1)-R9x_grad*0.5*(Depth(n-1)+Depth(n))
         end do

         ! add temperature-dependence to simulate higher concentrations beneath thermocline
         do n=1,NO_BOXES-1
           deltatemp=-(ETW(n)-ETW(NO_BOXES))
           strat_thresh=-(ETW(1)-ETW(NO_BOXES))-0.2
           if (strat_thresh>0.3 .and. deltatemp>strat_thresh) R9x(n)=strat_fact*R9x(n)
         end do

        select case (InitializeModel)         !JM always 0 because InitBenthicNutrient happens first
          case (0)
          case (1)
           R9x_new = 400.0 * max(psilt_min,psilt/100.0) * tau
           R9x(:)=(R9x_new-R9x(NO_BOXES))/delta*delta
        end select

     end select


  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
