!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: Phaeo
!
! DESCRIPTION
!   Parameter values for the Phaeocystis functinal group
!   +Integer which describe the different actions
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  module mem_Phaeo
!
! !USES:

  use global_mem
  use mem,  ONLY: NO_BOXES

!  
!
! !AUTHORS
!   the ERSEM group, Piet Ruardij
!
!
!
! !REVISION_HISTORY
!   !
!
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
  ! Default all is public
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  public

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Phaeo PARAMETERS (read from nml)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !
  !  ---------------- Physiological parameters -----------------
  !
  ! Parameters valid only for one functional group/all functional groups:
  real(RLEN)  :: p_qmR3P6  ! quotum TEP in Phaeo where mortality is maximal
  real(RLEN)  :: p_qkR3P6  ! quotom TEP in PhaeoCol. where nutrients uptake are is half.
  real(RLEN)  :: p_sP2P6 ! specific rate in which P2 transfer themselves to Phaeocystis colonies
  real(RLEN)  :: p_clP2P6n ! dissolved N concentration where above a optimal transfer from P2 to P6 is
  real(RLEN)  :: p_clP2P6p ! dissolved P concentration where above a optimal transfer from P2 to P6 is.
  real(RLEN)  :: p_cmP2P6n ! dissolved N concentration where below a optimal transfer from P2 to P6 is
  real(RLEN)  :: p_cmP2P6p ! dissolved P concentration where below a optimal transfer from P2 to P6 is.
  real(RLEN)  :: p_wP6c ! weight of one Phaeocystis colonial cell
  real(RLEN)  :: p_dP6m ! diameter of one Phaeocystis colonial cell
  real(RLEN)  :: p_max_ColSize ! maximal colonial size
  real(RLEN)  :: p_steep_mort ! steepness of the increase of mortality at unfavorable cirvumstances.
  real(RLEN)  :: p_shearE ! shearrate E,due to trubulence in the water
  real(RLEN)  :: p_diff_n1 ! molecular diffusion of N1p
  real(RLEN)  :: p_diff_n3 ! molecular diffusion of N3n
  real(RLEN)  :: p_diff_n4 ! molecular diffusion of N4n
  real(RLEN)  :: p_diff_urea ! molecular diffusion of urea
  real(RLEN)  :: p_qnR3c !NC quotum in organic carbon inb colony but outside cell 
  real(RLEN)  :: p_cuY3a(2) ! size range of Phaeocystis as food for FilterFeeders (number of cells in colony)
  real(RLEN)  :: p_cuZ5a(2) ! size range of Phaeocystis as food for Ciliates (number of cells in colony)
  real(RLEN)  :: p_cuZ4a(2) ! size range of Phaeocystis as food for Onnivorous Mesozooplankton (number of cells in colony)
  real(RLEN)  :: p_sdmo     ! mortality at extreme circumstances value :0.2
  real(RLEN)  :: p_smxInC   ! mortality of celss in colony
  real(RLEN)  :: p_chnxInC  ! nr cells in colony at which rate p_smxInCa result in halvwe a rate 
  integer     :: sw_select=1!if  Phaeo as food is smaller or large than according selection in p_cuY3,p_cuZ5a,p_cuZ4a
                            ! biomass of Pcc which is grazing-rate=input/pcu(1 or 2) in stead of rate=input/SizeCol
  integer,parameter        :: CALC_MORTALITY=1
  integer,parameter        :: NEW_COLONIES=2
  integer,parameter        :: CALC_FOOD_FILTERFEEDER=3
  integer,parameter        :: CALC_FOOD_MESOZOO=4
  integer,parameter        :: CALC_FOOD_MICROZOO=5
  integer,parameter        :: CALC_GRAZING_FILTERFEEDER=-3
  integer,parameter        :: CALC_GRAZING_MESOZOO=-4
  integer,parameter        :: CALC_GRAZING_MICROZOO=-5
  integer,parameter        :: CALC_LIMIT_FILTERCAP=8
  integer,parameter        :: CALC_REL_SEDIMENTATION=9
  integer,parameter        :: CALC_REL_PHOSPHATE_UPTAKE=11
  integer,parameter        :: CALC_REL_NITRATE_UPTAKE=13
  integer,parameter        :: CALC_REL_AMMONIUM_UPTAKE=14
  integer,parameter        :: CALC_REL_UREA_UPTAKE=15
  integer,parameter        :: CALC_NET_NITROGEN_UPTAKE=16
  integer,parameter        :: CALC_NITROGEN_RELEASE=17
  integer,parameter        :: CALC_MORTALITY_CELLS_IN_COLONY=20
  integer,parameter        :: FORCE_MORTALITY=21
  integer,parameter        :: MAX_MORTALITY=22
  integer,parameter        :: CALC_LOC_DET_FLUX=23
  integer,parameter        :: CALC_TEMPERATURE_DEPENDENCE=25

!CALC_MORTALITY,NEW_COLONIES,CALC_FOOD_FILTERFEEDER,CALC_FOOD_MESOZOO,CALC_FOOD_MICROZOO,CALC_GRAZING_FILTERFEEDER,
!CALC_GRAZING_MESOZOO,CALC_GRAZING_MICROZOO,CALC_LIMIT_FILTERCAP,CALC_REL_SEDIMENTATION,
!CALC_PHOSPHATE_UPTAKE,CALC_NITRATE_UPTAKE,CALC_AMMONIUM_UPTAKE,CALC_UREA_UPTAKE,FORCE_MORTALITY,MAX_MORTALITY
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! SHARED PUBLIC FUNCTIONS (must be explicited below "contains")

  public InitPhaeo
  contains

 !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  subroutine InitPhaeo()

! default value:
  p_sdmo=0.2
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  namelist /Phaeo_parameters/ p_qmR3P6, p_qkR3P6 ,p_sP2P6,p_clP2P6n,p_clP2P6p, &
           p_cmP2P6n,p_cmP2P6p,p_wP6c,p_dP6m,p_max_ColSize,p_steep_mort,p_shearE,&
           p_diff_n1,p_diff_n3, p_diff_n4,p_diff_urea,p_qnR3c,p_cuY3a,p_cuZ4a, &
           p_cuZ5a,p_sdmo,p_smxInC,p_chnxInC,sw_select


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !BEGIN compute
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  Default values the namelist file(s)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   p_qnR3c =0.0

  !  Open the namelist file(s)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   write(LOGUNIT,*) "#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
   write(LOGUNIT,*) "#  Reading Phaeo parameters.."
   open(NMLUNIT,file='Phaeo.nml',status='old',action='read',err=100)
    read(NMLUNIT,nml=Phaeo_parameters,err=101)
    close(NMLUNIT)
    write(LOGUNIT,*) "#  Namelist is:"
    write(LOGUNIT,nml=Phaeo_parameters)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !END compute
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  return
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Error Messages
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
100 call error_msg_prn(NML_OPEN,"InitPhaeo.f90","Phaeo.nml")
101 call error_msg_prn(NML_READ,"InitPhaeo.f90","Phaeo_parameters")
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  end  subroutine InitPhaeo
  end module mem_Phaeo
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
