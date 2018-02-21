#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: PelChem
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
  subroutine PelChemDynamics
!
! !USES:

  ! For the following Pelagic-states fluxes are defined: N4n, N3n, O2o, O4n, &
  ! N6r, R6s, N5s, P1s
  ! The following Pelagic 1-d global boxvars are modified : flN3O4n
  ! The following Pelagic 1-d global boxvars  are used: ETW, flPTN6r, flPIR6s
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
  use mem,  ONLY: N4n, N3n, O2o, N6r, R6s
#ENDIF
  use mem, ONLY: ppN4n, ppN3n,ppN1p,ppN5s,  ppO2o, ppO4n, ppN6r, &
     ppR6c,ppR6s, ppP6c,ppR3c,ppR2c,ppR6n,ppR1c, iiPel,  &
    flN3O4n, flN4N3n,ETW, flPTN6r, flPIR6s, NO_BOXES,flP6R3c,R2c, &
    iiPhytoPlankton,ppPhytoPlankton,flux_vector, &
    flPIR6n,flPIR1n,flPIR6p,flPIR1p,flR3R2c,flnDIp,flnDIn,&
    ppR1n,ppR6p,ppR1p,iiC,iiN,iiP,iiS,Source_D3_vector
  use mem_Param,  ONLY: p_qon_nitri, p_qro, p_qon_dentri, p_small
  use mem_PelChem
  use mem_BenthicNutrient3,ONLY:p_max_state_change
  use LimitRates, ONLY:LimitChange_vector

#ifdef INCLUDE_PELCO2
  use mem,ONLY:CO2
#ENDIF

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:MM_vector, eTq_vector, insw_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: MM_vector, eTq_vector, insw_vector



!  
!
! !AUTHORS
!   Original version by P. Ruardij and M. Vichi
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
  integer                         :: i
  integer                         :: j
  real(RLEN)                      :: LocalDelta
  real(RLEN)                      :: max_change_per_step
  real(RLEN),dimension(NO_BOXES)  :: fN6O2r
  real(RLEN),dimension(NO_BOXES)  :: eo
  real(RLEN),dimension(NO_BOXES)  :: er
  real(RLEN),dimension(NO_BOXES)  :: rPAo
  real(RLEN),dimension(NO_BOXES)  :: fR6N5s
  real(RLEN),dimension(NO_BOXES)  :: o,r,s
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! External functions
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN), external  :: GetDelta
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  LocalDelta=GetDelta()
  max_change_per_step=p_max_state_change/LocalDelta




  !
  ! Regulating factors
  !

  eo  =   MM_vector(  max(p_small,O2o(:)),  p_clO2o)
  er  =   MM_vector(  max(p_small,N6r(:)),  p_clN6r)

  !
  ! Nitrification in the water
  !

  flN4N3n(:)  =   max(0.0,p_sN4N3* N4n(:)* eTq_vector(  ETW(:),  p_q10N4N3)* eo)

#ifdef INCLUDE_PELCO2
  !limitation at hight pH == low [CO2] in oxic layer : no growth possible
  r  = p_cyn * flN4N3n /12.0    ! prim prod due to nitrification recalculated to mmolC/m3 porewater
  call LimitChange_vector(1,r,CO2,max_change_per_step,o)
  flN4N3n=flN4N3n*o
#endif

  call flux_vector( iiPel, ppN4n,ppN3n,   flN4N3n(:) )
  call flux_vector( iiPel, ppO2o,ppO2o,-( flN4N3n(:)* p_qon_nitri) )

  !
  ! Denitrification in the water
  !

  rPAo  =   flPTN6r(:)/ p_qro +p_small
  flN3O4n(:) = max(0.0,p_sN3O4n* eTq_vector( ETW(:), p_q10N4N3)*  &
    N3n(:)) * insw_vector( -( O2o(:)- N6r(:)/ p_qro))

  !reclalculate to r-unirs(=o-units)
  r=flN3O4n(:)/ p_qon_dentri 
  !Check only on negative fluxes. and limit if necessary.......
  call LimitChange_vector(3,r,N6r/p_qro,max_change_per_step,s)
  call flux_vector( iiPel, ppN3n,ppO4n, flN3O4n(:) )
  call flux_vector( iiPel, ppN6r,ppN6r,-p_qro*r*s);
  call flux_vector( iiPel, ppO2o,ppO2o, r*(1.0-s));

  !
  ! Reoxidation of reduction equivalents
  !

  fN6O2r  =   p_sOS* N6r(:)* eo * er

  r=fN6O2r/p_qro
  call LimitChange_vector(1,r,O2o,max_change_per_step,o)
  call LimitChange_vector(1,fN6O2r,N6r,max_change_per_step,r)
  fN6O2r=fN6O2r*min(o,r)
  call flux_vector( iiPel, ppN6r,ppN6r,-( fN6O2r) )
  call flux_vector( iiPel, ppO2o,ppO2o,-( fN6O2r/ p_qro) )


  !
  ! Regeneration of dissolved silica
  !

  fR6N5s  =   p_sR6N5* eTq_vector(  ETW(:),  p_q10R6N5)* R6s(:) *100.0/(R2c(:)+100.0) 
  call flux_vector( iiPel, ppR6s,ppN5s, fR6N5s )

  !
  ! TEPFlux from  out of colony
  !
  call flux_vector( iiPel, ppP6c,ppR3c, flP6R3c )
  !
  !
  !

  do i=1,iiPhytoPlankton
    call flux_vector( iiPel, ppPhytoPlankton(i,iiN),ppR6n, flPIR6n(i,:) )
    call flux_vector( iiPel, ppPhytoPlankton(i,iiN),ppR1n, flPIR1n(i,:) )
    call flux_vector( iiPel, ppPhytoPlankton(i,iiP),ppR6p, flPIR6p(i,:) )
    call flux_vector( iiPel, ppPhytoPlankton(i,iiP),ppR1p, flPIR1p(i,:) )
    j=ppPhytoPlankton(i,iiS)
    if ( j.gt.0)  call flux_vector( iiPel, j,       ppR6s, flPIR6s(i,:) )
  enddo
  call flux_vector(iiPel,ppR3c,ppR2c,flR3R2c(:))

  flnDin=Source_D3_vector(ppN4n)+ Source_D3_vector(ppN3n)
  flnDip=Source_D3_vector(ppN1p)



  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
