#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenNitrogenShifting
!
! DESCRIPTION
!   Description of shifting of dissolved N (amm and nitrate) between
!       layers
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine BenNitrogenShiftingDynamics
!
! !USES:

  ! For the following Benthic-states fluxes are defined: K14n, K4n, K24n, K3n
  ! The following Benthic-states are used (NOT in fluxes): D1m, D7m, D2m
  ! The following global scalar vars are used: &
  !    NO_BOXES_XY,   &
  !  BoxNumberXY, LocalDelta
  ! The following Benthic 1-d global boxvars are used: shiftD1m, KNH4, reATn, &
  ! shiftD2m, KNO3
  ! The following Benthic 1-d global boxpars  are used: p_poro
  ! The following 0-d global parameters are used: p_clDxm, p_d_tot
  ! The following global constants are used: RLEN,ZERO
  ! The following constants are used: SHIFT, LAYER1, DERIVATIVE, RFLUX, LAYER2

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT,ZERO
  use mem,  ONLY: K14n, K4n, K24n, K3n, D1m, D7m, D2m, D2STATE
  use mem, ONLY: ppK14n, ppK4n, ppK24n, ppK3n, &
    NO_BOXES_XY,LocalDelta,max_change_per_step, sK4K3,&
     BoxNumberXY, shiftD1m, KNH4, reATn, shiftD2m, &
    KNO3, jK34K24n, jK13K3n, iiBen, flux,source
  use constants,  ONLY: SHIFT, LAYER1, DERIVATIVE, RFLUX, LAYER2,LAYER3
  use mem_Param,  ONLY: p_poro, p_clDxm, p_d_tot_2
  use mem_BenAmmonium, ONLY:  p_slK4K3,p_flux_K24_at_deep_end=>p_flux_at_deep_end
  use mem_BenNitrate, ONLY:  p_flux_K3_at_deep_end=>p_flux_at_deep_end

  use LimitRates, ONLY:LimitShift,LimitChange

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:CalculateFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use bennut_interface,   ONLY: CalculateFromSet


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following sesame functions are used:insw, IntegralExp
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: insw, IntegralExp
!  
!
! !AUTHORS
!   Original version by  P. Ruardij
!
!
!
! !REVISION_HISTORY
!   April 15, 1994 by EGM Embsen and P Ruardij:
!               Created a new version of the this process
!               so that it can be used with OpenSESAME.
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
  real(RLEN)  :: Dnew
  real(RLEN)  :: shiftmass
  real(RLEN)  :: zuD1
  real(RLEN)  :: zuD2
  real(RLEN)  :: jK14K4n
  real(RLEN)  :: jK24K14n
  real(RLEN)  :: alpha
  real(RLEN)  :: r,s

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do BoxNumberXY=1,NO_BOXES_XY

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Ammonium Fluxes at the oxic/denitrification boundary
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      Dnew  =   D1m(BoxNumberXY)+ LocalDelta* shiftD1m(BoxNumberXY)

      ! Calculate mass shifted in upwards direction:
      shiftmass = CalculateFromSet( KNH4(BoxNumberXY), SHIFT, LAYER1, &
        D1m(BoxNumberXY), Dnew)/ LocalDelta

      jK14K4n = CalculateFromSet( KNH4(BoxNumberXY), DERIVATIVE, RFLUX, &
        D1m(BoxNumberXY), ZERO)+ shiftmass

  !     r=D2m(BoxNumberXY)-D1m(BoxNUmberXY)
  !     call LimitShift_m3(jK14K4n,(K4n(BoxNumberXY)+ &
  !       source(iiBen,BoxNumberXY,ppK4n)*LocalDelta), K14n(BoxNumberXY), &
  !       D1m(BoxNumberXY),r,shiftD1m(BoxNumberXY),max_change_per_step)

      call LimitShift(jK14K4n,max(0.0,K4n(BoxNumberXY)+ &
           source(iiBen,BoxNumberXY,ppK4n)*LocalDelta),K14n(BoxNumberXY),max_change_per_step)
      call flux(BoxNumberXY, iiBen, ppK14n, ppK4n,   jK14K4n* insw(  jK14K4n) )
      call flux(BoxNumberXY, iiBen, ppK4n, ppK14n, - jK14K4n* insw( -jK14K4n) )

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! All the nutrient mineralization source term in the anoxic layer
      ! has been added to K14.n in BenBacDynamics
      ! However in the model this layer is subdivided and hence a partition
      ! flux is here calculated according to the exponential distribution.
      !
      ! Calculate coefficient for the e-folding distribution of the anoxic
      ! mineralization. D7.m is the average penetration depth for N-detritus
      !                 +
      ! Anoxic Mineralization at D1.m, using the exponential distribution
      !                 +
      !          Anoxic Mineralization at D2.m
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      alpha  =   1.0D+00/ max(  p_clDxm,  D7m(BoxNumberXY))
      zuD1 = max( 1.D-20, reATn(BoxNumberXY))/ p_poro(BoxNumberXY)/ &
                        IntegralExp( -alpha, p_d_tot_2- D1m(BoxNumberXY))
      zuD2  =   zuD1* exp( - alpha*( D2m(BoxNumberXY)- D1m(BoxNumberXY)))

      jK24K14n = - zuD2* p_poro(BoxNumberXY)* IntegralExp( - alpha, &
        p_d_tot_2- D2m(BoxNumberXY))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Ammonium Fluxes at the denitrification/anoxic boundary
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       
      Dnew  =   D2m(BoxNumberXY)+ LocalDelta* shiftD2m(BoxNumberXY)

      ! Calculate mass shifted in upwards direction:
      shiftmass = CalculateFromSet( KNH4(BoxNumberXY), SHIFT, LAYER2, &
        D2m(BoxNumberXY), Dnew)/ LocalDelta  &
      + CalculateFromSet( KNH4(BoxNumberXY), DERIVATIVE, RFLUX, D2m(BoxNumberXY), ZERO)

       r=D2m(BoxNumberXY)-D1m(BoxNumberXY);s=p_d_tot_2-D2m(BoxNumberXY)
!      call LimitShift_m3(shiftmass,K14n(BoxNumberXY),K24n(BoxNumberXY), &
!          r,s,shiftD2m(BoxNumberXY),max_change_per_step)
      call LimitShift(shiftmass,K14n(BoxNumberXY),K24n(BoxNumberXY),max_change_per_step)
      jK24K14n=jK24K14n + shiftmass
       
    
      call flux(BoxNumberXY, iiBen, ppK24n, ppK14n, jK24K14n* insw( jK24K14n) )
      call flux(BoxNumberXY, iiBen, ppK14n, ppK24n,-jK24K14n* insw(-jK24K14n) )

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Flux at the lower boundary of ammonium 
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      select case (p_flux_K24_at_deep_end)  ! 1=no flux, 2= only fluxes_downwards (sink), 3,=full_flux
        case(1); r=0.0;
        case(2); r=min(0.0,CalculateFromSet(KNH4(BoxNumberXY),DERIVATIVE,RFLUX,p_d_tot_2, ZERO))
        case(3); r=CalculateFromSet(KNH4(BoxNumberXY),DERIVATIVE,RFLUX,p_d_tot_2, ZERO)
      end select
 
      jK34K24n(BoxNumberXY)=r

      call flux(BoxNumberXY, iiBen, ppK24n, ppK24n, jK34K24n(BoxNumberXY) )

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Flux at the lower boundary of nitrate 
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        shiftmass = CalculateFromSet( KNO3(BoxNumberXY), SHIFT, LAYER3, &
            D2m(BoxNumberXY), Dnew)/ LocalDelta
        !if sign of shift mass is different from the shift in the dentrification depth
        !the calculate profile in unrealistic 
        ! the may happen when nitrifation input is very low.
        shiftmass=shiftmass * insw(shiftmass *shiftD2m(BoxNumberXY))
        !the nitrate profile will be high near the source and low a near the denitrifation depth
        ! due to denitrfication in the denitrifcation layer. Therefor it can be assumed
        ! that the shift in biomass will be always smaller that total conentration per m3 
        ! this is protection  to avoid to use results from  unrealistic calculated profiles 
        r=abs(shiftmass)/D2m(BoxNumberXY)*K3n(BoxNumberXY)/LocalDelta
        shiftmass=max(-r,min(r,shiftmass)) 
        !In case of low nitrification profile is not mainly deterimend by nitrifcation input
        !An again a chance on unrealitic profiles: therfore we do not take in account
        ! the flux at the denitrification depth
        if (sK4K3(BoxNumberXY) > p_slK4K3) then
          select case (p_flux_K3_at_deep_end)  ! 1=no flux, 2= only fluxes_downwards (sink), 3,=full_flux
             case(1); r=0.0;
             case(2); r=min(0.0,CalculateFromSet(KNO3(BoxNumberXY), &
                               DERIVATIVE,RFLUX,D2m(BoxNumberXY), ZERO))
             case(3); r=CalculateFromSet(KNO3(BoxNumberXY), &
                               DERIVATIVE,RFLUX,D2m(BoxNumberXY), ZERO)
          end select
          jK13K3n(BoxNumberXY)  = r+ shiftmass
        else
          jK13K3n(BoxNumberXY)  =  shiftmass
        endif
        call LimitChange(1,jK13K3n(BoxNumberXY),K3n(BoxNumberXY),max_change_per_step)
        call flux(BoxNumberXY, iiBen, ppK3n, ppK3n,  jK13K3n(BoxNumberXY) ) 

  enddo
  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
