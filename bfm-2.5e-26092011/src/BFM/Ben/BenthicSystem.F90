#INCLUDE "DEBUG.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: BenthicSystem
!
! DESCRIPTION
!   !   This is the top level of the benthic submodel.
!       All the biological processes affecting the benthic dynamics are called
!       in a specified sequence according to the calculation flags.
!
!

!   This file is generated directly from OpenSesame model code, using a code
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine BenthicSystemDynamics
!
! !USES:
  ! The following Benthic-states are used (NOT in fluxes): Y1c, Y1n, Y1p, Y2c, &
  ! Y2n, Y2p, Y4c, Y4n, Y4p, Y5c, Y5n, Y5p, H1c, H1n, H1p, H2c 
  ! The following groupmember vars  are used: iiY1, iiY2, iiY4, iiY5, iiH1, iiH2
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem, ONLY:  ppY1c, ppY1n, ppY1p, ppY2c, ppY2n, ppY2p, &
    ppY4c, ppY4n, ppY4p, ppY5c, ppY5n, ppY5p, ppH1c, ppH1n, ppH1p, ppH2c,  &
    iiY1, iiY2, iiY4,iiY3, iiY5, iiH1, iiH2, iiH3,iiBen,flux_vector, &
    H1c,H2c,H3c,H1p,H1n,ppH1c,ppH2c,ppH3c,ppH1p,ppH2p,ppH3p,ppH1n,ppH2n,ppH3n
  use mem_Param, ONLY: CalcBenOrganisms, CalcBenBacteria

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following group processes are used: BenOrganismDynamics, BenBacDynamics
  use global_interface, ONLY: BenOrganismDynamics, BenBacDynamics
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



!
!
! !AUTHORS
!   ERSEM group
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


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Calculation of turenh and irrenh in Bioturbation...
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  call BioturbationDynamics

  call BenGlobalDynamics

  if ( CalcBenOrganisms(iiY1)) &
    call BenOrganismDynamics( iiY1, ppY1c, ppY1n, ppY1p)

  if ( CalcBenOrganisms(iiY2)) &
    call BenOrganismDynamics( iiY2, ppY2c, ppY2n, ppY2p)

  if ( CalcBenOrganisms(iiY4)) &
    call BenOrganismDynamics( iiY4, ppY4c, ppY4n, ppY4p)

  if ( CalcBenOrganisms(iiY5)) &
    call BenOrganismDynamics( iiY5, ppY5c, ppY5n, ppY5p)

  if ( CalcBenOrganisms(iiY3)) &
    call FilterFeederDynamics

  if ( CalcBenBacteria(iiH1)) &
    call BenBacDynamics( iiH1, ppH1c, ppH1n, ppH1p)

  if ( CalcBenBacteria(iiH2))  &
    call BenBacDynamics( iiH2, ppH2c, ppH2n, ppH2p)

  if ( CalcBenBacteria(iiH3)) &
    call BenNBacDynamics

! call flux_vector(iiBen,ppH1c,ppH3c,0.1*max(0.0,min(0.1,H1c-H3c)))
! call flux_vector(iiBen,ppH1n,ppH3n,0.1*max(0.0,min(0.1,H1c-H3c))*H1n/(1.0D-80+H1c))
! call flux_vector(iiBen,ppH1p,ppH3p,0.1*max(0.0,min(0.1,H1c-H3c))*H1p/(1.0D-80+H1c))
! call flux_vector(iiBen,ppH1c,ppH2c,0.1*max(0.0,min(0.1,H1c-H2c)))
! call flux_vector(iiBen,ppH1n,ppH2n,0.1*max(0.0,min(0.1,H1c-H2c))*H1n/(1.0D-80+H1c))
! call flux_vector(iiBen,ppH1p,ppH2p,0.1*max(0.0,min(0.1,H1c-H2c))*H1p/(1.0D-80+H1c))


  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
