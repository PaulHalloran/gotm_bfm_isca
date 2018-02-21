!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: Phyto
!
! DESCRIPTION
!   Parameter values for the phytoplankton groups
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  module mem_Phyto
!
! !USES:

  use global_mem
  use mem,  ONLY: iiPhytoPlankton,NO_BOXES

!  
!
! !AUTHORS
!   the ERSEM group, Marcello Vichi, JWB, HBB
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
  ! Phyto PARAMETERS (read from nml)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !
  !  ---------------- Physiological parameters -----------------
  !
  real(RLEN)  :: p_q10(iiPhytoPlankton)   ! Doubling temperature
  real(RLEN)  :: p_sum(iiPhytoPlankton)   ! Maximal productivity at 10 degrees C
  real(RLEN)  :: p_srs(iiPhytoPlankton)   ! Respiration rate at 10 degrees C
  real(RLEN)  :: p_ses(iiPhytoPlankton)   ! Excretion rate at 10 degrees C
  real(RLEN)  :: p_sdmo(iiPhytoPlankton)  ! Max.specific nutrient-stress lysis rate
  real(RLEN)  :: p_thdo(iiPhytoPlankton)  ! Half value for nutrient stress lysis
  real(RLEN)  :: p_seo(iiPhytoPlankton)   ! Extra lysis rate for P4
  real(RLEN)  :: p_pu_ea(iiPhytoPlankton) ! Fraction of pp excreted as PLOC/PDET
  real(RLEN)  :: p_pu_ra(iiPhytoPlankton) ! Activity respiration rate
  !
  !  ---------------- Nutrient parameters in phytoplankton -----------------
  !
  integer     :: p_iRI(iiPhytoPlankton)  ! excretion as sugars or as TEP
  real(RLEN)  :: p_qnlc(iiPhytoPlankton) !miniumum quoata
  real(RLEN)  :: p_lqnlc(iiPhytoPlankton) !quoata where below limiting of chla systhere
  real(RLEN)  :: p_qnRc(iiPhytoPlankton) !optimal quoata
  real(RLEN)  :: p_xqn(iiPhytoPlankton)
  real(RLEN)  :: p_qplc(iiPhytoPlankton)
  real(RLEN)  :: p_qpRc(iiPhytoPlankton)
  real(RLEN)  :: p_xqp(iiPhytoPlankton)
  real(RLEN)  :: p_qslc(iiPhytoPlankton)  ! Minimum quotum Si in PI
  real(RLEN)  :: p_qsRc(iiPhytoPlankton)  ! Reference quotum Si in PI
  real(RLEN)  :: p_xqs(iiPhytoPlankton)
  real(RLEN)  :: p_qun(iiPhytoPlankton)
  real(RLEN)  :: p_qup(iiPhytoPlankton)
  real(RLEN)  :: p_qus(iiPhytoPlankton)  ! affinity of PI for Si
  real(RLEN)  :: p_sbuf(iiPhytoPlankton) ! rate with nutrients from internal buffer are used to from new biomass ( old value:0.05)
  real(RLEN)  :: p_lN3N4n(iiPhytoPlankton)   ! NH4-concentration where limitation of NO3 uptake to 0.5 
  real(RLEN)  :: p_lureaN4n(iiPhytoPlankton)! NH4-concentration where limitation of urea uptake to 0.5 
  real(RLEN)  :: p_lN1(iiPhytoPlankton)
  real(RLEN)  :: p_chPs(iiPhytoPlankton)
  real(RLEN)  :: p_alpha_chl(iiPhytoPlankton)  ! Initial slope P-I curve
  real(RLEN)  :: p_alpha_add(iiPhytoPlankton)  ! Initial slope P-I curve
  real(RLEN)  :: p_clO2o(iiPhytoPlankton)
  real(RLEN)  :: p_pos_e(iiphytoplankton)     ! part of stress output as ecreted c  
  real(RLEN)  :: p_R2qec(iiphytoplankton)     ! part of stress output as ecreted c  

  !
  !  ------------- Chlorophyll parameters -----------
  !  skel: Skeletonema costatum pav: Pavlova lutheri
  !  syn: Synechoccus sp. (significant alpha decrease with irradiance)
  !  gyr: Gyrodinium sp. iso: Isochrysis galbana
  !              skel     iso      syn      gyr
  real(RLEN)  :: p_sdchl_l(iiPhytoPlankton)  ! Specific turnover rate for Chla [d-1] in light
  real(RLEN)  :: p_sdchl_d(iiPhytoPlankton)  ! Specific turnover rate for Chla [d-1] in dark
  ! p_qchlc =    0.03,    0.025,   0.1,     0.02    # Maximum quotum Chla:C
  !             +-0.024  +-0.001  +-0.003  +-0.004
  !  Thalassiosira sp. [0.05+-0.01]
  !              skel     pav      syn      gyr
  real(RLEN)  :: p_esNI(iiPhytoPlankton)  ! Nutrient stress threshold for Sinking
  ! p_alpha_chl = 1.0e-5, 0.46e-5*2.0, 2.0e-5, 0.68e-5 # Initial slope P-I curve
  !  Thalassiosira sp. [0.48-0.63]
  real(RLEN)  :: p_res(iiPhytoPlankton)  ! Sinking velocity (m/d)
  !   p_qchlc  = 0.05,      0.03,      0.07,      0.02 # Maximum quotum Chla:C
  real(RLEN)   :: p_qchlc(iiPhytoPlankton)  ! Fixed/Maximum quotum Chla:C dependent on ChlLightFlag [mg Chla (mg C)-1]
  !                                          %p_qchlc%   0.05, 0.03,  0.07,  0.02
  real(RLEN)   :: p_qlPlc(iiPhytoPlankton)  ! Fixed/Maximum quotum Chla:C dependent on ChlLightFlag [mg Chla (mg C)-1]
  real(RLEN)   :: p_xdiv(iiPhytoPlankton)  ! max number of division per day
  !                                          %p_qchlc%   0.05, 0.03,  0.07,  0.02
  real(RLEN)   :: p_EIR(iiPhytoPlankton)  ! ligt where above bleaching appears
  real(RLEN)   :: p_xantho(iiPhytoPlankton)  ! ligt where above bleaching appears
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Parameters valid only for one functional group/all functional groups:
  integer     :: sw_rhochl ! to play with rho_chl an d to tryother formulations 
  integer     :: p_limnut  ! switch for nut. limitation (Liebig is default)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! SHARED PUBLIC FUNCTIONS (must be explicited below "contains")

  public InitPhyto
  contains

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  subroutine InitPhyto()

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer    ::i

  namelist /Phyto_parameters/ p_q10, p_sum, p_srs, p_ses,p_sdmo, p_seo, p_pu_ea, &
    p_pu_ra, p_pos_e, p_R2qec, p_qnlc,p_lqnlc, p_qplc, p_qslc, p_qnRc, p_qpRc, p_qsRc, &
    p_qun, p_qup, p_qus, p_sbuf, p_xqn, p_xqp, p_xqs, p_esNI, p_thdo, p_res, &
    p_lN3N4n,p_lureaN4n,p_lN1,p_chPs, p_iRI, p_alpha_chl,p_alpha_add,p_sdchl_l, &
    p_sdchl_d,p_clO2o,p_qchlc,p_qlPlc,p_xdiv, p_EIR,p_xantho, &
    p_limnut, sw_rhochl
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !BEGIN compute
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !  Open the namelist file(s)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    p_EIR=300.0
    p_alpha_add(:)=0.0
    sw_rhochl=1   !Default value!!
write(LOGUNIT,*) "#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
   write(LOGUNIT,*) "#  Reading Phyto parameters.."
open(NMLUNIT,file='Phyto.nml',status='old',action='read',err=100)
    read(NMLUNIT,nml=Phyto_parameters,err=101)
    close(NMLUNIT)
    do i=1,iiPhytoPlankton
       if ( p_qnRc(i) < p_lqnlc(i) .or. &
            p_lqnlc(i) < p_qnlc(i) .or. &
            p_qpRc(i) < p_qplc(i) .or. &
            p_qsRc(i) < p_qslc(i)  ) then
         write(LOGUNIT,*)  &
          'Error in the quoata parameters: a minimum quota is larger than the optimum'
         goto 101
       endif
    enddo
    write(LOGUNIT,*) "#  Namelist is:"
    write(LOGUNIT,nml=Phyto_parameters)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !END compute
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  return
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Error Messages
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
100 call error_msg_prn(NML_OPEN,"InitPhyto.f90","Phyto.nml")
101 call error_msg_prn(NML_READ,"InitPhyto.f90","Phyto_parameters")
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  end  subroutine InitPhyto
  end module mem_Phyto
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
