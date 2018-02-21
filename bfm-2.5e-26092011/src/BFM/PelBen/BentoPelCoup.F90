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
  subroutine BentoPelCoupDynamics
!
! !USES:

  ! For the following Pelagic-states fluxes are defined: R6c, R6n, R6p, R6s, &
  ! O2o, N3n, N4n, N5s, N6r, R1c, R1n, R1p
  ! For the following Pelagic-group-states fluxes are defined: PhytoPlankton
  ! The following global scalar vars are used: &
  ! BoxNumberZ, NO_BOXES_Z, BoxNumberX, NO_BOXES_X, BoxNumberY, NO_BOXES_Y, &
  ! BoxNumber, BoxNumberXY
  ! The following Pelagic 1-d global boxvars  are used: Depth
  ! The following Benthic 1-d global boxvars are used: jPIY3c, jZIY3c, &
  ! PI_Benc, jY3RIc, jY3RIn, jY3RIp, jY3RIs, jbotO2o, jbotN1p, jbotN3n, jbotN4n, &
  ! jbotN5s, jbotN6r, jbotR6c, jbotR6n, jbotR6p, jbotR6s, jbotR1c, jbotR1n, jbotR1p
  ! ,jbotO3c, ppO3h, ppO3c, ppG3c
  ! The following Benthic 2-d global boxvars are modified : PELBOTTOM
  ! The following groupmember vars  are used: iiPhytoPlankton, iiP1
  ! The following constituent constants  are used: iiC, iiN, iiP, iiL, iiS
  ! The following 0-d global parameters are used: &
  ! AssignPelBenFluxesInBFMFlag
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem, ONLY:   PhytoPlankton, ppPhytoPlankton,iiPhytoPlankton,  &
    MicroZooPlankton, ppMicroZooPlankton,iiMicroZooPlankton,  D2STATE
  use mem, ONLY: ppO2o, ppN1p, &
    ppN3n, ppN4n, ppN5s, ppN6r, ppR2c,ppR1c, ppR1n, ppR1p, BoxNumberZ, &
    BoxNumberX, NO_BOXES_X, BoxNumberY, NO_BOXES_Y, BoxNumber, &
    BoxNumberXY, Depth, jPIY3c, jZIY3c, ZI_Fc, jRIY3c, jY3RIc, jY3RIn, jY3RIp, &
    jY3RIs, jbotO2o, jbotN1p, jbotN3n, jbotN4n, jbotN5s, jbotN6r, jbotR6c, jbotR6n, &
    jY3QIc, jY3QIs, jRIQIc,jRIQIn,jRIQIp,jRIQIs,RI_Fc,RI_Fn,RI_Fp,RI_Fs,PELBOTTOM, &
    jbotR6p, jbotR6s, jbotR1c, jbotR1n, jbotR1p,jY3O3c,jO2Y3o,jY3N1p,jY3N4n, &
    jPIY3n,jPIY3p,jCaCO3Y3c,iiC, iiN, iiP, iiL, iiS, iiBen, iiPel,iiP6,iiY3,&
    flux, qsPc,qnPc,qpPc,jbotR3c,R3c,R3_Benc,R3_Benn,R3_Benp
 use mem, ONLY: ppY3p,ppY3n,ppK1p,ppN1p,ppN4n,ppQ1c, ppQ1n, &
                ppQ1p, ppR6c,ppR6n,ppR6p,ppR6s,ppR3c

#ifdef INCLUDE_PELCO2
#ifdef INCLUDE_BENCO2
  use mem, ONLY: ppO3h, ppO3c,jbotO3c,jbotO3h
#endif
#endif
  use mem_Param,  ONLY: AssignPelBenFluxesInBFMFlag, p_small,CalcBenOrganisms,CalcMicroZooPlankton
  use constants,  ONLY: p_qnUc
  use mem_Phaeo,ONLY:CALC_GRAZING_FILTERFEEDER
  use Track, ONLY:define_track_bot_rate
  use mem_FilterFeeder,ONLY:p_pR6PelY3=>p_pR6Pel,p_pePelY3=>p_pePel,p_pueR3
  use mem,only: qnPc,qpPc
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
  ! Local Vectors used  of group vectors
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN), dimension(:), pointer  ::lcl_PhytoPlankton
  real(RLEN), dimension(:), pointer  ::lcl_MicroZooPlankton
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer  :: i
  integer  :: j
  real(RLEN)  :: uptake_m2,uptake_m3
  real(RLEN)  :: r,p,d
  real(RLEN)  :: Pc
  real(RLEN)  :: Zc

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! user defined external functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer, external  :: D3toD1
  integer, external  :: D2toD1
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  BoxNumberZ = 1
  DO BoxNumberY=1,NO_BOXES_Y
    DO BoxNumberX=1,NO_BOXES_X
      BoxNumber=D3toD1(BoxNumberX,BoxNumberY,BoxNumberZ)
      BoxNumberXY=D2toD1(BoxNumberX,BoxNumberY)
      ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate Phyto Fluxes to Filterfeeder from Pelagic for
      ! all phyt types/constituents
      ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        if (CalcBenOrganisms(iiY3) ) then
          jPIY3n=0.0;jPIY3p=0.0;
          do i = 1 , iiPhytoPlankton
            lcl_PhytoPlankton => PhytoPlankton(i,iiC)
            Pc  =   lcl_PhytoPlankton(BoxNumber)
            uptake_m2  =   jPIY3c(i,BoxNumberXY)
            if ( Pc> 1.0D-10.and.uptake_m2>0.0) then
              Pc  =Pc +1.0D-80
              d=uptake_m2/Depth(BoxNumber)  !d= grazing/m3
              call PhaeocystisCalc_1l(CALC_GRAZING_FILTERFEEDER,&
                                      i,BoxNumberXY,BoxNumber,uptake_m3,d,uptake_m2)
              uptake_m2=uptake_m3*Depth(BoxNumber) !uptake : grazing/m2  r=grazing/m3
              j = ppPhytoPlankton(i,iiC)
              PELBOTTOM(j,BoxNumberXY)  =  PELBOTTOM(j,BoxNumberXY)  -uptake_m2
              if (i==iiP6) then
                 jbotR3c(BoxNumberXY)  =  -uptake_m2/Pc*R3c(Boxnumber) ! unit C/m2
              endif
              j = ppPhytoPlankton(i,iiN)
              r=uptake_m2*qnPc(i,Boxnumber)
              jPIY3n(BoxNumberXY)= jPIY3n(BoxNumberXY)+r
              if (i==iiP6) r=r - jbotR3c(BoxNumberXY) * R3_Benn(BoxNumberXY)/(1.0D-80+R3_Benc(BoxNumberXY))
              PELBOTTOM(j,BoxNumberXY) =  PELBOTTOM(j,BoxNumberXY) -r  
              call define_track_bot_rate(iiPel,j,BoxNumber,BoxNumberXY,iiBen,ppY3n,-r)
              j = ppPhytoPlankton(i,iiP)
              r=uptake_m2*qpPc(i,BoxNumber) 
              jPIY3p(BoxNumberXY)= jPIY3p(BoxNumberXY)+r
              if (i==iiP6) r=r - jbotR3c(BoxNumberXY) * R3_Benp(BoxNumberXY)/(1.0D-80+R3_Benc(BoxNumberXY))
              PELBOTTOM(j,BoxNumberXY) =  PELBOTTOM(j,BoxNumberXY)   - r
              call define_track_bot_rate(iiPel,j,BoxNumber,BoxNumberXY,iiBen,ppY3p,-r)
              j = ppPhytoPlankton(i,iiL)
              lcl_PhytoPlankton => PhytoPlankton(i,iiL)
              r=lcl_PhytoPlankton(BoxNumber)
              PELBOTTOM(j,BoxNumberXY) =  PELBOTTOM(j,BoxNumberXY)  -uptake_m2*r/(1.0D-80+Pc)
              j = ppPhytoPlankton(i,iiS)
              if ( j> 0) then
                r=uptake_m2* qsPc(i,BoxNumber)
                PELBOTTOM(j,BoxNumberXY) =  PELBOTTOM(j,BoxNumberXY) -r
                call define_track_bot_rate(iiPel,j,BoxNumber,BoxNumberXY,0,0,-r)
              end if
            end if
          end do

          do i = 1 , iiMicroZooPlankton
            lcl_MicroZooPlankton => MicroZooPlankton(i,iiC)
            Zc  =   lcl_MicroZooPlankton(BoxNumber)
            if ( Zc> p_small .and. CalcMicroZooPlankton(i)) then
              p= jZIY3c(BoxNumberXY)/ (ZI_Fc(BoxNumberXY) * Depth(BoxNumber))
              j = ppMicroZooPlankton(i,iiC)
              uptake_m2  =  p * Zc*Depth(BoxNumber)
              PELBOTTOM(j,BoxNumberXY)  =   -uptake_m2
              j = ppMicroZooPlankton(i,iiN)
              if ( j> 0) then
                lcl_MicroZooPlankton => MicroZooPlankton(i,iiN)
                r=  p* lcl_MicroZooPlankton(BoxNumber)*Depth(BoxNumber)
                PELBOTTOM(j,BoxNumberXY) =  -r
                call define_track_bot_rate(iiPel,j,BoxNumber,BoxNumberXY,iiBen,ppY3n,-r)
              endif
              j = ppMicroZooPlankton(i,iiP)
              if ( j> 0) then
                lcl_MicroZooPlankton => MicroZooPlankton(i,iiP)
                r =  p* lcl_MicroZooPlankton(BoxNumber)*Depth(BoxNumber)
                PELBOTTOM(j,BoxNumberXY) =  -r
                call define_track_bot_rate(iiPel,j,BoxNumber,BoxNumberXY,iiBen,ppY3p,-r)
              endif
            else
              PELBOTTOM(ppMicroZooPlankton(i,iiC),BoxNumberXY)  =   0.0D+00
              j = ppMicroZooPlankton(i,iiN)
              if ( j> 0) PELBOTTOM(j,BoxNumberXY)  =   0.0D+00
              j = ppMicroZooPlankton(i,iiP)
              if ( j> 0) PELBOTTOM(j,BoxNumberXY)  =   0.0D+00
            end if
          end do

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Net uptake of detritus
        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        call flux(BoxNumber, iiPel,ppR6c,ppR6c,-jRIY3c(BoxNumberXY)/ Depth(BoxNumber) )

        p  =   jRIY3c(BoxNumberXY)/(p_small+ RI_Fc(BoxNumberXY))
        uptake_m2  =   p* RI_Fn(BoxNumberXY)
        call flux(BoxNumber, iiPel,ppR6n,ppR6n,-uptake_m2/ Depth(BoxNumber) )
        call define_track_bot_rate(iiPel,ppR6n,BoxNumber,BoxNumberXY,iiBen,ppY3n,-uptake_m2)

        uptake_m2  =   p* RI_Fp(BoxNumberXY)
        call flux(BoxNumber, iiPel,ppR6p,ppR6p,-uptake_m2/ Depth(BoxNumber) )
        call define_track_bot_rate(iiPel,ppR6p,BoxNumber,BoxNumberXY,iiBen,ppY3p,-uptake_m2)

        uptake_m2  =   p* RI_Fs(BoxNumberXY)
        call define_track_bot_rate(iiPel,ppR6s,BoxNumber,BoxNumberXY,0,0,-uptake_m2)

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Net detritus Fluxes to Benthic from Pelagic by Y3
        ! net flux= uptake - excretion of food : flux may ber negative!
        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        call flux(BoxNumber,iiPel, ppR6c, ppR6c, jY3RIc(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber,iiPel, ppR6n, ppR6n, jY3RIn(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber,iiPel, ppR6p, ppR6p, jY3RIp(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber,iiPel, ppR6s, ppR6s, jY3RIs(BoxNumberXY)/ Depth(BoxNumber) )

        call define_track_bot_rate(iiBen,ppY3n,BoxNumber,BoxNumberXY, iiPel,ppR6n,jY3RIn(BoxNumberXY))
        call define_track_bot_rate(iiBen,ppY3p,BoxNumber,BoxNumberXY, &
                                                iiPel,ppR6p,jY3RIp(BoxNumberXY))

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !  part of excretion of detritus sedimentated  
        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         p=jY3QIc(BoxNumberXY)/(p_small+jY3RIc(BoxNumberXY))
         jbotR6c(BoxNumberXY)  =   jbotR6c(BoxNumberXY)- jY3QIc(BoxNumberXY) 
         jbotR6n(BoxNumberXY)  =   jbotR6n(BoxNumberXY)- jY3RIn(BoxNumberXY)*p
         jbotR6p(BoxNumberXY)  =   jbotR6p(BoxNumberXY)- jY3RIp(BoxNumberXY)*p
         p=jY3QIs(BoxNumberXY)/(p_small+jY3RIs(BoxNumberXY))
         jbotR6s(BoxNumberXY)  =   jbotR6s(BoxNumberXY)- jY3RIs(BoxNumberXY)*p

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !  Pseudofaeces flux by Filterfeeders
        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

         jbotR6c(BoxNumberXY)  =   jbotR6c(BoxNumberXY)- jRIQIc(BoxNumberXY)
         jbotR6n(BoxNumberXY)  =   jbotR6n(BoxNumberXY)- jRIQIn(BoxNumberXY)
         jbotR6p(BoxNumberXY)  =   jbotR6p(BoxNumberXY)- jRIQIp(BoxNumberXY)
         jbotR6s(BoxNumberXY)  =   jbotR6s(BoxNumberXY)- jRIQIs(BoxNumberXY)


#ifdef INCLUDE_BENCO2
         jbotO3c(BoxNumberXY)=jbotO3c(BoxNumberXY)+jY3O3c(BoxNumberXY)
#endif
         jbotO2o(BoxNumberXY)=jbotO2o(BoxNumberXY)-jO2Y3o(BoxNumberXY)

!      instead of N4n flux now urea flux is defined 
         call flux(BoxNumber,iiPel, ppR1c, ppR1c, jY3N4n(BoxNumberXY)/p_qnUc/Depth(BoxNumber) )
         call flux(BoxNumber,iiPel, ppR1n, ppR1n, jY3N4n(BoxNumberXY)/Depth(BoxNumber) )
         call flux(BoxNumber,iiPel, ppN1p, ppN1p, jY3N1p(BoxNumberXY)/Depth(BoxNumber) )

         call define_track_bot_rate(iiBen,ppY3n,BoxNumber,BoxNumberXY,&
                                          iiPel,ppR1n,jY3N4n(BoxNumberXY))
         call define_track_bot_rate(iiBen,ppY3p,BoxNumber,BoxNumberXY,&
                                          iiPel,ppN1p,jY3N1p(BoxNumberXY))

         if ( jbotN1p(BoxNumberXY) >0 ) then
            call define_track_bot_rate(iiBen,ppK1p,BoxNumber,BoxNumberXY,&
                                                iiPel,ppN1p,jbotN1p(BoxNumberXY))
         else
            call define_track_bot_rate(iiPel,ppN1p,BoxNumber,BoxNumberXY,&
                                                iiBen,ppK1p,jbotN1p(BoxNumberXY))
         endif
       endif

       call define_track_bot_rate(iiPel,ppR1n,BoxNumber,BoxNumberXY,&
                                              iiBen,ppQ1n,jbotR1n(BoxNumberXY))
       call define_track_bot_rate(iiPel,ppR1p,BoxNumber,BoxNumberXY,&
                                              iiBen,ppQ1p,jbotR1n(BoxNumberXY))
       call define_track_bot_rate(iiPel,ppR1c,BoxNumber,BoxNumberXY,&
                                              iiBen,ppQ1c,jbotR1c(BoxNumberXY))

      ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! All Fluxes to Benthic from Pelagic defined for the
      ! Pelagic State variables
      ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      if ( AssignPelBenFluxesInBFMFlag) then

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Nutrient Fluxes to Benthic from Pelagic
        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        call flux(BoxNumber, iiPel, ppO2o, ppO2o, jbotO2o(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppN1p, ppN1p, jbotN1p(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppN3n, ppN3n, jbotN3n(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppN4n, ppN4n, jbotN4n(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppN5s, ppN5s, jbotN5s(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppN6r, ppN6r, jbotN6r(BoxNumberXY)/ Depth(BoxNumber) )
#ifdef INCLUDE_PELCO2
#ifdef INCLUDE_BENCO2
         call flux(BoxNumber, iiPel, ppO3c, ppO3c,   jbotO3c(BoxNumberXY)/ Depth(BoxNumber) &
                                                    -jCaCO3Y3c(BoxNumberXY)/ Depth(BoxNumber))
         call flux(BoxNumber, iiPel, ppO3h, ppO3h,   jbotO3h(BoxNumberXY)/ Depth(BoxNumber) &
                                         +0.5* jCaCO3Y3c(BoxNumberXY)/12.0/ Depth(BoxNumber) )  
#endif
#endif

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! PhytoPlankton Fluxes to Benthic from Pelagic by Y3
        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        do i = 1 , iiPhytoPlankton
              j = ppPhytoPlankton(i,iiC)
              call flux(BoxNumber, iiPel, j, j, PELBOTTOM(j,BoxNumberXY)/ Depth(BoxNumber) )
              j = ppPhytoPlankton(i,iiN)
              call flux(BoxNumber, iiPel, j, j, PELBOTTOM(j,BoxNumberXY)/ Depth(BoxNumber) )
              j = ppPhytoPlankton(i,iiP)
              call flux(BoxNumber, iiPel, j, j, PELBOTTOM(j,BoxNumberXY)/ Depth(BoxNumber) )
              j = ppPhytoPlankton(i,iiL)
              call flux(BoxNumber, iiPel, j, j, PELBOTTOM(j,BoxNumberXY)/ Depth(BoxNumber) )
              j = ppPhytoPlankton(i,iiS)
              if ( j> 0) &
                call flux(BoxNumber, iiPel, j, j, PELBOTTOM(j,BoxNumberXY)/ Depth(BoxNumber) )
        end do
        call flux(BoxNumber, iiPel, ppR3c, ppR3c, jbotR3c(BoxNumberXY)/ Depth(BoxNumber) )

        do i = 1 , iiMicroZooPlankton
              j = ppMicroZooPlankton(i,iiC)
              call flux(BoxNumber, iiPel, j, j, PELBOTTOM(j,BoxNumberXY)/ Depth(BoxNumber) )
              j = ppMicroZooPlankton(i,iiN)
              if ( j> 0) &
                call flux(BoxNumber, iiPel, j, j, PELBOTTOM(j,BoxNumberXY)/ Depth(BoxNumber) )
              j = ppMicroZooPlankton(i,iiP)
              if ( j> 0) &
                call flux(BoxNumber, iiPel, j, j, PELBOTTOM(j,BoxNumberXY)/Depth(BoxNumber) )
        end do

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! Total Sedimentation flux to Benthic from Pelagic defined
        ! for the pealgic state variables.
        ! (See sedimentation for definition of the fluxes of benthic &
        ! variables)
        ! !!!!!!! ALL DETRITUS FLUXES TO THE SEDIMENT ARE DIRECTED VIA R6 TO Q6 !!!!!!!!
        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        call flux(BoxNumber, iiPel, ppR6c, ppR6c, jbotR6c(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppR6n, ppR6n, jbotR6n(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppR6p, ppR6p, jbotR6p(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppR6s, ppR6s, jbotR6s(BoxNumberXY)/ Depth(BoxNumber) )

        call flux(BoxNumber, iiPel, ppR1c, ppR1c, jbotR1c(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppR1n, ppR1n, jbotR1n(BoxNumberXY)/ Depth(BoxNumber) )
        call flux(BoxNumber, iiPel, ppR1p, ppR1p, jbotR1p(BoxNumberXY)/ Depth(BoxNumber) )

      end if
    end DO
  end DO

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
