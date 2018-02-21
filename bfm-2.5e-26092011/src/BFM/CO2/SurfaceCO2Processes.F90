#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: SurfaceCO2Processes
!
! DESCRIPTION
!   Model describes reaeration between air and water column.
!       as forced by temperature, wind and chemical enhancement.
!	second routine to be applied according to Schneider et al., 1999
!
!       The equation and correlation used in this routine
!       are found in the
!
!		R. Wanninkhof (1992), Relationship between windspeed and gas
!		exchange over the oecean
!               J. GeoPhys. Res. 97, 7373-7382
!
!	notes: K0 = co2/pco2 => dimension = 1.e-6mol/(l*1.e-6atm)
!	exchange coefficient: deltapCO2 * k660 * K0
!	=> 1.e-6atm * cm/hr * 1.e-6mol/(l * 1.e-6atm) = cm/hr * 1.e-6mol / l
!	Temp in degrees C
!	test parameter see CalculateCO2system.f (DIC=2133,
!	AC=2260, pco2=341), O7.c = AC-2210,
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine SurfaceCO2Processes(BoxNumber,BoxNumberXY,BoxNumberZ)
!


! !USES:

  ! For the following Pelagic-states fluxes are defined: O3c
  ! The following global scalar vars are used: Wind
  ! The following Pelagic 1-d global boxvars are used: ETW, ERHO, pCO2, &
  ! Depth
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
#ifdef INCLUDE_PELCO2
  use mem,  ONLY: O3c, D2STATE
  use mem, ONLY: ppO3c, Wind, ETW, ESW, ERHO, pCO2, jsurO3c,Ac,pH,DIC,O3c,&
    Depth, iiBen, iiPel, flux,NO_BOXES_Z
  use CO2System, ONLY:CalcK0Ac


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following global functions are used:BoxAbove, CalcSchmidtNumberCO2
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use global_interface,   ONLY: BoxAbove, CalcSchmidtNumberCO2
  use mem_param,   ONLY: AssignAirPelFluxesInBFMFlag
  use mem_PelCO2

#ENDIF

!  
!
! !AUTHORS
!   16 March 1999 Original version by H. Thomas
!
!
!
! !REVISION_HISTORY
!   
!
! COPYING
!   
!   Copyright (C) 2006 P. Ruardij and M. Vichi
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

  integer, INTENT(IN)    :: BoxNumber
  integer, INTENT(IN)    :: BoxNumberXY
  integer, INTENT(IN)    :: BoxNumberZ

#ifdef INCLUDE_PELCO2
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN)  :: p_schmidt
  real(RLEN)  :: k660
  real(RLEN)  :: kex
  real(RLEN)  :: K0
  real(RLEN)  :: t
  real(RLEN)  :: s
  real(RLEN)  :: cN7c
  real(RLEN)  :: mkg_to_mmm3   ! mol/kg --> mmol/m3  (density to volume measure) 


      if ( BoxNumberZ==NO_BOXES_Z ) then

        !   call CalcCO2System(DYNAMIC, false);

        t  =   ETW(BoxNumber)
        s  =   ESW(BoxNumber)

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        ! air-sea exchange
        !  ( In lower boxes co2-input take place via vertical transport)
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        !
        ! The authors assumed a Schimdt number of CO2 (=reference) of 660.0
        !
        ! replaced by file CalcSchmidtNumberCO2.f
        ! p_schmidt= (2073.1 -125.62 * Temp + 3.6276 * pow(Temp, 2.0) -
        !                  0.043219 * pow(Temp, 3.0)) / 660.0;

        p_schmidt  =   CalcSchmidtNumberCO2(  t)/ 660.0D+00



        !
        ! Calculate temperature dependency:
        !

        k660 = 2.5D+00*( 0.5246D+00+ 1.6256D-02* t+ 4.9946D-04* &
          (t)**(2.0D+00))

        !
        ! Calculate wind dependency:
        !	including conversion cm/hr => m/dag :
        !

        kex  =  ( 0.31D+00* (Wind)**(2.0D+00))/ sqrt(  p_schmidt)* 0.24D+00  ! kex is in cm / hr
        ! kex *.24 is then m/day

        !units of CO2 flux: kex * delta pCO2 * K0 = 10-3 mol m-2 d-1
        ! unit: (m/day) * (1.e-6atm) * (1.e-6mol / (l *1.e-6atm)) = 10-3 mol m-2 &
        ! d-1
        !
    
        mkg_to_mmm3=ERHO(BoxNumber)*1000.0;

!       CO2AirSea(1) = -(( kex*( pCO2(BoxNumber)- pCO2_air)* &
!         K0Ac* mkg_to_mmm3)/ 1000.0D+00)  ! flux co2 in mol/m2/d

        ! mg C/m2/d 
         jsurO3c(BoxNumberXY)= -( kex * ( pCO2(BoxNumber)- pCO2_air)  &
                         * CalcK0Ac(s,t)* mkg_to_mmm3* 12.0D+00)  

         if ( AssignAirPelFluxesInBFMFlag) then
             call flux(BoxNumber, iiPel, ppO3c, ppO3c, jsurO3c(BoxNumberXY) /Depth(BoxNumber) )  
         endif

      end if


#ENDIF

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
