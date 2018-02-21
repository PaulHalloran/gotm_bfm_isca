!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: PelGlobal
!
! DESCRIPTION
!   !

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  module mem_PelGlobal
!
! !USES:

  use global_mem
  use mem,  ONLY: iiMesoZooPlankton, iiMicroZooPlankton,iiPhytoPlankton 


!  
!
! !AUTHORS
!   Piet Ruardij
!
!
!
! !REVISION_HISTORY
!   Created at Tue Apr 20 09:11:59 AM CEST 2004
!
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
  ! PelGlobal PARAMETERS (read from nml)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  All parameter were copied from the .p-file:
  integer     :: sw_os=1 !! testing of how oterh phyt than diaotms stik to the macroaggregatges
  integer     :: p_sw_sticky !! the way how TEP changes for normal to sticky: linear (1), non-linear(2)
  real(RLEN)  :: p_steep   !! steepness of the non=-linear function
  real(RLEN)  :: p_rrR6m   ! m/d# passive detritus sinking rate
  real(RLEN)  :: p_raRZm   !! m/d# max. sinking rate in complex of R2/R6 detritus sinking rate
  real(RLEN)  :: p_raR2m   !! m/d# max. sinking rate in complex of R2/R2 detritus sinking rate
  real(RLEN)  :: p_chR2R2c !! concentration at which 
                           ! R2(=TEP) become sticky.
  real(RLEN)  :: p_raPIm ! !!maximal sedimentation rate due to interaction with R2
  real(RLEN)  :: p_raP6m ! !!maximal sedimentation rate of Phaeo colonies
  real (RLEN) :: p_poR2PI(iiPhytoPlankton) !! optimal proportion between macro-aggregates and phyto at which macro aggregates occur
  real(RLEN)  :: p_rrPIm(iiPhytoPlankton) ! !maximal rest sedimenation rte 
  real(RLEN)  :: p_clMiO2o(iiMicroZooPlankton) !! value of Low oxygen which control escape upwards
  real(RLEN)  :: p_clMeO2o(iiMesoZooPlankton)  !! value of Low oxygen which control escape upwards
  real(RLEN)  :: p_rMem(iiMesoZooPlankton)  !! value of Low oxygen which control escape upwards
  real(RLEN)  :: p_rMim(iiMicroZooPlankton) !! value of Low oxygen which control escape upwards
  integer     :: p_seq_calc(iiPhytoPlankton)
  

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! SHARED PUBLIC FUNCTIONS (must be explicited below "contains")

  public InitPelGlobal
  contains

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  subroutine InitPelGlobal()

  integer         :: i,j
  real(RLEN)      :: r
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  namelist /PelGlobal_parameters/p_sw_sticky,p_steep,p_rrR6m,p_raRZm,p_raR2m, &
                        p_chR2R2c,p_raPIm,p_raP6m, &
                        p_poR2PI,p_rrPIm,p_clMiO2o,p_clMeO2o,p_rMem,p_rMim,sw_os
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !BEGIN compute
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  !  Open the namelist file(s)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   write(LOGUNIT,*) "#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
   write(LOGUNIT,*) "#  Reading PelGlobal parameters.."
   open(NMLUNIT,file='PelGlobal.nml',status='old',action='read',err=100)
    read(NMLUNIT,nml=PelGlobal_parameters,err=101)
    close(NMLUNIT)
    write(LOGUNIT,*) "#  Namelist is:"
    write(LOGUNIT,nml=PelGlobal_parameters)
    j=0;p_seq_calc=0;
    do while (j<iiPhytoPlankton)
      r=1.0D+80
      do i=1,iiPhytoPlankton
         if (p_seq_calc(i)==0) r=min(r,p_poR2Pi(i))
      enddo
      j=j+1
      do i=1,iiPhytoPlankton
         write(LOGUNIT,*)'r,i,p_poRI2PI',r,i,p_poR2PI(i),p_seq_calc(i)
         if (p_seq_calc(i)==0.and.abs(r-p_poR2PI(i))/r<1.0e-6) then
           p_seq_calc(i)=j;
           write(LOGUNIT,*) 'j,i=',j,i
         endif
      enddo
    enddo
    write(LOGUNIT,*) " p_seq_calc:=",p_seq_calc
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !END compute
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  return
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Error Messages
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
100 call error_msg_prn(NML_OPEN,"InitPelGlobal.f90","PelGlobal.nml")
101 call error_msg_prn(NML_READ,"InitPelGlobal.f90","PelGlobal_parameters")
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  end  subroutine InitPelGlobal
  end module mem_PelGlobal
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
