#include "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenDenitriDepth
!
! DESCRIPTION
!   !

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine BenDenitriDepthDynamics
!
! !USES:

  ! For the following Benthic-states fluxes are defined: D2m
  ! The following Benthic-states are used (NOT in fluxes): K3n,K4n,D1m
  ! The following global scalar vars are used: &
  !    NO_BOXES_XY,   &
  !  BoxNumberXY, dummy, InitializeModel
  ! The following Benthic 1-d global boxvars are modified : shiftD1m,shiftD2m
  ! The following Benthic 1-d global boxvars  are used: KNO3E, KNH4
  ! The following 0-d global parameters are used: p_d_tot, p_clD1D2m
  ! The following global constants are used: RLEN
  ! The following constants are used: EQUATION, STANDARD, GET, LABDA_1, LABDA_2,&
  ! ONE_PER_DAY

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,ZERO,LOGUNIT
  use mem,  ONLY: D2m, D1m, D2STATE
  use mem, ONLY: ppD2m, sK4K3,jK3G4n,NO_BOXES_XY,BoxNumberXY, dummy, &
    InitializeModel, shiftD1m, shiftD2m, KNO3E, iiBen, flux
  use constants,  ONLY: EQUATION, STANDARD, GET, LABDA_1, ONE_PER_DAY,LABDA_2
  use mem_Param,  ONLY: p_d_tot, p_clD1D2m
  use mem_BenDenitriDepth,ONLY:p_pK3G4n
  use mem_BenAmmonium,only: p_slK4K3
  use LimitRates, ONLY:LimitChange
  use mem,  ONLY: reBTn,reATn,K4n,K14n,K24n,K3n,N4n_ben,N3n_ben,jbotN4n,jbotN3n,D6m,D7m,KNH4,K6r



  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following bennut functions are used:CalculateFromSet, GetInfoFromSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use bennut_interface,   ONLY: CalculateFromSet, GetInfoFromSet
!  
!
! !AUTHORS
!   Original version by  P. Ruardij
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

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
! integer  :: control
  real(RLEN)  :: Dxm
  real(RLEN)  :: Dzm
  real(RLEN)  :: D2mNew
  real(RLEN)  :: M3n_D1m
  real(RLEN)  :: M3n_Dxm
  real(RLEN)  :: sK3G4n
  real(RLEN)  :: jlK3G4n
  real(RLEN)  :: lambda
  real(RLEN)  :: clD1D2m

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
! integer, external  :: PrintSet
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do BoxNumberXY=1,NO_BOXES_XY
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Set minimum distance of D2m  to D1m accroding nexct rule:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       
    clD1D2m=min(0.03D+00,max(D1m(BoxNumberXY),p_clD1D2m))

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    ! Calculate the depth of the denitrification layer on basis of the nitrate 
    ! This depth is the depth where the nitrate concentration is 0.1 of the concentration at 
    ! at oxygen penetration depth.
    ! This is done only when enough input is of nitrate by nitrification
    ! In this case only is the profile determined by the large source of nitrate in the oxic layer
    ! If not the dentriffication depth is dtermined by parmeters which set the denitrification
    ! dept on a fixed distance of the oxygen penetration depth
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    if (sK4K3(BoxNumberXY) > p_slK4K3) then

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate concentration of nitrate in porewater in M3n:
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      M3n_D1m = CalculateFromSet( KNO3E(BoxNumberXY), EQUATION, &
        STANDARD, D1m(BoxNumberXY), dummy)
      Dxm=D1m(BoxNumberXY)+D2m(BoxNumberXY) * 0.5
      M3n_Dxm= CalculateFromSet( KNO3E(BoxNumberXY), EQUATION, &
        STANDARD, Dxm, dummy)

      select case ( M3n_D1m<= ZERO)

        case( .TRUE. )

          ! Do nothing give a warning
          ! Let if D1m increase let D2m increase too to avoid zero thickness of 
          ! denitrifiaction layer
          if ( InitializeModel ==0 ) then
            write(LOGUNIT,'(''D1m='',F12.3,'' D2m='',F12.3)') D1m(BoxNumberXY), D2m(BoxNumberXY)
            write(LOGUNIT,'(''D6m='',F12.3,'' D7m='',F12.3)') D6m(BoxNumberXY), D7m(BoxNumberXY)
            write(LOGUNIT,'(''K3n='',F12.3,'' K4n='',F12.3)') K3n(BoxNumberXY), K4n(BoxNumberXY)
            write(LOGUNIT,'(''K14n='',F12.3,'' K24n='',F12.3)') K14n(BoxNumberXY), K24n(BoxNumberXY)
            write(LOGUNIT,'(''reBTn='',F12.3,'' reATn='',F12.3)') reBTn(BoxNumberXY), reATn(BoxNumberXY)
            write(LOGUNIT,'(''fluxN3='',F12.3,'' fluxK4n='',F12.3)') jbotN3n(BoxNumberXY), jbotN4n(BoxNumberXY)
            write(LOGUNIT,'(''N3n='',F12.3,'' N4n='',F12.3)') N3n_Ben(BoxNumberXY), N4n_Ben(BoxNumberXY)
            write(LOGUNIT,'(''K6r='',F12.3,'' sK3G4n='',F12.3)') &
                                     K6r(BoxNumberXY),GetInfoFromSet( KNO3E(BoxNumberXY), GET, LABDA_2, 21)  
            call   PrintSet(  KNH4(BoxNumberXY),"concentration nitrate on D1m < 0")
            call   PrintSet(  KNO3E(BoxNumberXY),"concentration nitrate on D1m < 0")
         endif

          D2mNew =D2m(BoxNumberXY)
          shiftD2m(BoxNumberXY) = max( min( p_d_tot- &
            p_clD1D2m, D2mNew), D1m(BoxNumberXY)+ clD1D2m)- D2m(BoxNumberXY)
!         shiftDnM=shiftD2m(BoxNumberXY)

        case( .FALSE. )

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
          ! According solution nitrate concentration decreases
          ! exponentially. Calculate depth at where below the 
          ! denitrification /m2 is  below a (small) fixed number. 
          ! Use this new depth as the (uncorrected) new denitrification depth
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          sK3G4n= GetInfoFromSet( KNO3E(BoxNumberXY), GET, LABDA_2, 31,dummy,dummy) 
          lambda= GetInfoFromSet( KNO3E(BoxNumberXY), GET, LABDA_1, 31,dummy,dummy) 
          jlK3G4n=(jK3G4n(BoxNumberXY)+M3n_Dxm/sK3G4n) * p_pK3G4n
          D2mNew= log(-jlK3G4n*lambda/(sK3G4n* M3n_Dxm))/(2.0*lambda) +Dxm
          Dzm=p_d_tot-p_clD1D2m

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
          ! 1. Calculate uncorrected shift of D2.m
          ! 2. limit shift incase of D2mNew moves in the direction of D1m
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          shiftD2m(BoxNumberXY) = (D2mNew - D2m(BoxNumberXY))*ONE_PER_DAY

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
          ! Correct by damping the change of D2m in case large changes:
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

          if ( InitializeModel== 0) then

            call LimitChange(3,shiftD2m(BoxNumberXY),D2m(BoxNumberXY),0.01D+00)
            call LimitChange(2,shiftD2m(BoxNumberXY),D2m(BoxNumberXY),0.04D+00)
            shiftD2m(BoxNumberXY) = shiftD2m(BoxNumberXY)* &
                Dzm/(Dzm+D2mNew)
           
              shiftD2m(BoxNumberXY)=min(Dzm,max(shiftD2m(BoxNumberXY)+D2m(BoxNumberXY),&
                 clD1D2m+D1m(BoxNumberXY)+shiftD1m(BoxNumberXY))) -D2m(BoxNumberXY)


             call flux(BoxNumberXY, iiBen, ppD2m, ppD2m, shiftD2m(BoxNumberXY) )

          end if


      end select

   else
     Dzm=p_d_tot-p_clD1D2m
     shiftD2m(BoxNumberXY)=min(Dzm,max(D2m(BoxNumberXY),&
        clD1D2m+D1m(BoxNumberXY)+shiftD1m(BoxNumberXY))) -D2m(BoxNumberXY)
          call flux(BoxNumberXY, iiBen, ppD2m, ppD2m, shiftD2m(BoxNumberXY) )
   endif
  end do

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model 
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
