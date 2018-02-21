!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: FilterFeeder
!
! DESCRIPTION
!   Parameter value file for suspension feeders (Y3)
!   
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
!INTERFACE#
  module mem_FilterFeeder
!
! !USES:

  use global_mem
  use mem, only: iiPhytoPlankton

!  
!
! !AUTHORS
!   ERSEM group, HBB
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
  ! Default all is public
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  public
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! FilterFeeder PARAMETERS (read from nml)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN)  :: p_clu  ! Lower limit of availability of a food source
  real(RLEN)  :: p_sum  ! Growth rate
  real(RLEN)  :: p_q10  ! q10
  real(RLEN)  :: p_Rpsfcnp  !  fraction of pseudofaces which is transported to the benthos ( valid for CNP)
  real(RLEN)  :: p_Rpsfs  !  fraction of pseudofaces which is transported to the benthos ( valid for s)
  real(RLEN)  :: p_R6  ! Food matrix detritus on pelagic det. (R6)
  real(RLEN)  :: p_puPI(iiPhytoPlankton)  ! available part of phytoplankton  for uptake
  real(RLEN)  :: p_puePI  ! Excreted fraction of phytoplankton uptake
  real(RLEN)  :: p_pueR3  ! Excreted fraction of TEP in phaeo
  real(RLEN)  :: p_pueZI  ! Excreted fraction of microzooplankton uptake
  real(RLEN)  :: p_pueR6  ! Excreted fraction of detritus uptake
  real(RLEN)  :: p_srs  ! Relative respiration rate
  real(RLEN)  :: p_sra  ! respired Part of uptake  used for filtering
  real(RLEN)  :: p_pur  ! respired Part of uptake  used for digesting
  real(RLEN)  :: p_sd  ! Specific Mortality
  real(RLEN)  :: p_sd2  ! Specific Density Dependent Mortality (mort. o1 0.1 at 25000)
  real(RLEN)  :: p_qnc  ! Fixed nutrient quotum N:C
  real(RLEN)  :: p_qpc  ! Fixed nutrient quotum P:C
  real(RLEN)  :: p_PI  ! Food parameter Y3 on phytoplankton
  real(RLEN)  :: p_ZI  ! Food parameter Y3 on microzooplankton
  real(RLEN)  :: p_vum  ! Volume filtered by 1mgC Y3 
  real(RLEN)  :: p_clO2o  ! oxygen con. at which activity is lowered to half 
  real(RLEN)  :: p_height ! height of the layer from which is filtered
  real(RLEN)  :: p_max ! proportion of sedimentation entering gridlayer which can be used for food uptake.
  real(RLEN)  :: p_pePel =0.0 ! part of excretion and respiration which is coupled to pelagic 
  real(RLEN)  :: p_pR6Pel =0.0 ! part of produced R6[cnp] which is excreted to the pelagic 
  real(RLEN)  :: p_pR6Pels =0.0 ! part of produced R6s which is excreted to the pelagic 
                                ! assumed that frustiles of diatoms has another sinking behaviour than 
                                ! the  the other particaulate carbon
  real(RLEN)  :: p_qCaCO3Y3c  ! proportion between CO2 use for shell foramtion and netgrowth
  real(RLEN)  :: p_pyfoodYy3  ! part of young animals for avalaible food...
  real(RLEN)  :: p_smYy3c     ! removal of young animal  (aging)
  real(RLEN)  :: p_pYy3Z2     !fraction of filterfeeder larvae which is resuspend to pelagic
  real(RLEN)  :: p_xspawning   !fraction of filterfeeder excreted as younng
  real(RLEN)  :: p_heighty    ! height where blow pelagic larvae decide to go back to the benthic
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! SHARED PUBLIC FUNCTIONS (must be explicited below "contains")

  public InitFilterFeeder
  contains

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  subroutine InitFilterFeeder()

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  namelist /FilterFeeder_parameters/ p_sum, p_q10, p_Rpsfcnp,p_Rpsfs, & 
    p_R6, p_clu, p_puPI,p_puePI,p_pueR3,p_pueZI, p_pueR6, p_srs, p_sra, p_pur, &
    p_sd, p_sd2, p_qnc, p_qpc, p_PI,p_ZI,p_vum,p_clO2o, &
   p_height,p_max, p_pePel,p_pR6Pel,p_pR6Pels,p_qCaCO3Y3c,p_pyfoodYy3, &
   p_smYy3c,p_pYy3Z2,p_xspawning,p_heighty
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !BEGIN compute
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !  Open the namelist file(s)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  p_xspawning=0.0;
   write(LOGUNIT,*) "#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
   write(LOGUNIT,*) "#  Reading FilterFeeder parameters.."
   open(NMLUNIT,file='FilterFeeder.nml',status='old',action='read',err=100)
    read(NMLUNIT,nml=FilterFeeder_parameters,err=101)
    close(NMLUNIT)
        write(LOGUNIT,*) "Uptake according Holling modified response"
        write(LOGUNIT,*) "Lower Threhold in food uptake is set by comparing energy gain with loss"
        if  ( p_vum ==0.0.or.p_sra==0.0) then
          write(LOGUNIT,*) "w_uptake=3 and p_vum==0 or p_sra==0.0: wrong combination"
          goto 101
        endif
         
    write(LOGUNIT,*) "#  Namelist is:"
    write(LOGUNIT,nml=FilterFeeder_parameters)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !END compute
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  return
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Error Messages
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
100 call error_msg_prn(NML_OPEN,"InitFilterFeeder.f90","FilterFeeder.nml")
101 call error_msg_prn(NML_READ,"InitFilterFeeder.f90","FilterFeeder_parameters")
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  end  subroutine InitFilterFeeder
  end module mem_FilterFeeder
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
