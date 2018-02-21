#include "DEBUG.h"
#include "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: PelGlobal
!
! DESCRIPTION
!   !
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine PelGlobalDynamics
!
! !USES:
  ! The following Pelagic-states are used (NOT in fluxes): R6p, R6c, R6n, R6s, &
  ! P1s, P1c, B1p, B1c, B1n
  ! The following box states are used (NOT in fluxes): &
  ! MicroZooPlankton, MesoZooPlankton, PhytoPlankton
  ! The following Pelagic 1-d global boxvars got a value: flP1R6s, flPTN6r, &
  ! qpR6c, qnR6c, qsR6c, qpB1c, qnB1c, sediR6
  ! The following Pelagic 2-d global boxvars got a value: qp_mz, qn_mz, qpZc, &
  ! qnZc, qpPc, qnPc, qlPc, qsPc, sediPI
  ! The following groupmember vars are used: iiMicroZooPlankton, &
  ! iiMesoZooPlankton, iiPhytoPlankton
  ! The following constituent constants  are used: iiP, iiC, iiN, iiL
  ! The following 0-d global parameters are used: p_small
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
#ifdef NOPOINTERS
  use mem,  ONLY: D3STATE
#else
  use mem, ONLY: R2c, R6p, R6c, R6n, R6s, RZc,  B1p, &
    B1c, B1n, MicroZooPlankton, MesoZooPlankton, PhytoPlankton
#endif
  use mem, ONLY: ppR2c,ppR6p, ppR6c, ppR6n, ppR6s, ppRZc, &
    ppMicroZooPlankton, ppMesoZooPlankton, ppPhytoPlankton, &
    flPTN6r, qpR6c, qnR6c, qsR6c, qpB1c, qnB1c, sediR2, sediR6, sediRZ, &
    jnetPTc, jnetB1c, jnetMeZc, jnetMiZc,jPelFishInput, sediMeZ,sediMiZ,O2o, &
    qp_mz, qn_mz, qpZc, qnZc, qpPc, qnPc, qlPc, qsPc, sediPI, iiMicroZooPlankton, &
    iiMesoZooPlankton, iiPhytoPlankton, iiP, iiC, iiN, iiL, NO_BOXES, NO_BOXES_Z, &
    iiS, iiP6,limnuti,PTi,flP6R3c,iiPhytoPlankton,jeZIR6n,jeZIR6p,jeZIDIn,jeZIDIp, &
    flPIR6n,flPIR1n,flPIR6p,flPIR1p,flPIR6s,flR3R2c,xantho
  use mem,ONLY: iiPELSINKREF,ppsediR2,ppsediRZ,ppsediR6,ppsediPI,ppsediMiZ,ppsediMeZ, &
                ppP6c,ppR3c,ppPcc
  use mem_Param,  ONLY: p_small
  use mem_PelGlobal
  use mem_globalfun,   ONLY: MM_power_vector,exp_limit
  use mem_Phyto, ONLY:p_qnlc, p_qplc, p_qslc, p_qnRc, p_qpRc, p_qsRc,p_iRI, &
                      p_xqp,p_xqn,p_xqs,p_qlPlc,p_qchlc
  use mem_PelBac, ONLY:p_qpBc=>p_qpc,p_qnBc=>p_qnc,p_qpBlc=>p_qplc,p_qnBlc=>p_qnlc

!  
!
! !AUTHORS
!   Piet Ruardij
!
!
! !REVISION_HISTORY
!   Created at Tue Apr 20 09:11:59 AM CEST 2004
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
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer  :: i,j,j0,k
  real(RLEN),dimension(NO_BOXES)  :: s
  real(RLEN),dimension(NO_BOXES)  :: r
  real(RLEN),dimension(NO_BOXES)  :: newmacro
  real(RLEN),dimension(NO_BOXES)  :: totmacro
  real(RLEN),dimension(NO_BOXES)  :: phyt
  real(RLEN),dimension(NO_BOXES)  :: eo
  real(RLEN),dimension(NO_BOXES)  :: wfR2
  real(RLEN),dimension(iiPhytoPlankton,NO_BOXES)  :: fr_nutlim
  real(RLEN),dimension(NO_BOXES)  :: rsinkDiam
  real(RLEN),dimension(NO_BOXES)  :: rsinkOther
  real(RLEN),dimension(NO_BOXES)  :: nn,np,ns

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Vectors used  of group vectors
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN), dimension(:), pointer  ::lcl_PhytoPlankton

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Sedimentation scheme
  ! A diagnotic variable in which the sedimentation rate is coupled in 
  ! this way to the state variables:
  ! For example for all R6 state varables in bio_bfm.F90 the sinking rates
  ! as defined in array sediR6 are used. In bio_bfm.F90 a recalculation take
  ! place from tates perda to rates per second and if necessay a limitation
  ! of the sinking rate for shallow grid points. (iiPelSINKREF(ppR6c)>0). 
  ! For the other constituents in R6 the values are recopied copied  from
  ! the values for R6c (iiPelSINKREF<0 )  
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   if ( iiPELSINKREF(ppR6c)==0 ) then
     ! These assigments are done only once: the first time this routine is called
     iiPELSINKREF(ppR2c)=  ppsediR2
     iiPELSINKREF(ppRZc)=  ppsediRZ
     iiPELSINKREF(ppR6c)=  ppsediR6
     iiPELSINKREF(ppR6n)=-ppR6c
     iiPELSINKREF(ppR6p)=-ppR6c
     iiPELSINKREF(ppR6s)=-ppR6c
     do i = 1 , ( iiPhytoPlankton)
        j0=ppPhytoPlankton(i,iiC)
        iiPELSINKREF(j0)=ppsediPI(i);
        j=ppPhytoPlankton(i,iiN)
        iiPELSINKREF(j)=-j0
        j=ppPhytoPlankton(i,iiP)
        iiPELSINKREF(j)=-j0
        j=ppPhytoPlankton(i,iiL)
        iiPELSINKREF(j)=-j0
        j=ppPhytoPlankton(i,iiS)
        if (j>0) iiPELSINKREF(j)=-j0
     enddo
     iiPELSINKREF(ppR3c)=-ppP6c
     iiPELSINKREF(ppPcc)=-ppP6c
     do i = 1 , iiMesoZooPlankton
        j0=ppMesoZooPlankton(i,iiC)
        iiPELSINKREF(j0)=ppsediMeZ(i);
        j=ppMesoZooPlankton(i,iiN)
        if ( j>0 )iiPELSINKREF(j)=-j0
        j=ppMesoZooPlankton(i,iiP)
        if ( j>0 )iiPELSINKREF(j)=-j0
     enddo
     do i = 1 , iiMicroZooPlankton
        j0=ppMicroZooPlankton(i,iiC)
        iiPELSINKREF(j0)=ppsediMiZ(i);
        j=ppMicroZooPlankton(i,iiN)
        if ( j>0 )iiPELSINKREF(j)=-j0
        j=ppMicroZooPlankton(i,iiP)
        if ( j>0 )iiPELSINKREF(j)=-j0
     enddo
   endif 

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Reset xantho: aaditiona pigments when it too sunny
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  xantho=0.0;
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Reset var in which silica and TEP fluxes are collected:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  flP6R3c(:)  =   0.0D+00
  flR3R2c(:)  =   0.0D+00
  flPTN6r(:)  =   0.0D+00

  jnetPTc(:)=0.0D+00
  jnetB1c(:)=0.0D+00
  jnetMeZc(:)=0.0D+00
  jnetMiZc(:)=0.0D+00
  jeZIR6n=0.0D+00
  jeZIR6p=0.0D+00
  jeZIDIn=0.0D+00
  jeZIDIp=0.0D+00
  jPelFishInput(:)=0.0D+00
  do i = 1 ,  iiPhytoPlankton
     flPIR6n(i,:)=0.0
     flPIR1n(i,:)=0.0
     flPIR6p(i,:)=0.0
     flPIR1p(i,:)=0.0
  end do

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Compute nutrient quota in pelagic detritus
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  qpR6c(:)  =   R6p(:)/( p_small+ R6c(:))
  qnR6c(:)  =   R6n(:)/( p_small+ R6c(:))
  qsR6c(:)  =   R6s(:)/( p_small+ R6c(:))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Compute nutrient quota in microzooplankton and HNAN
  ! in case of fixed quota qp_mz and qn_mz are one time calculated in the Initialize.F90
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  do i = 1 , ( iiMicroZooPlankton)
    if ( ppMicroZooPlankton(i,iiP) > 0 ) &
      qp_mz(i,:)  =   MicroZooPlankton(i,iiP)/( p_small+ MicroZooPlankton(i,iiC))
    if ( ppMicroZooPlankton(i,iiN) > 0 ) &
      qn_mz(i,:)  =   MicroZooPlankton(i,iiN)/( p_small+ MicroZooPlankton(i,iiC))
  end do


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Compute nutrient quota in omnivorous and herbivorous mesozooplankton
  ! in case of fixed quota qp_mz and qn_mz are one time calculated in the Initialize.F90
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  do i = 1 , ( iiMesoZooPlankton)
    if ( ppMesoZooPlankton(i,iiP) > 0 ) &
      qpZc(i,:)  =   MesoZooPlankton(i,iiP)/( p_small+ MesoZooPlankton(i,iiC))
    if ( ppMesoZooPlankton(i,iiN) > 0 ) &
      qnZc(i,:)  =   MesoZooPlankton(i,iiN)/( p_small+ MesoZooPlankton(i,iiC))
  end do


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Compute nutrient quota in phytoplankton
  ! Compute light prop.or chl. quota in phytoplankton (dep. on ChlLightFlag)
  ! Max quotum is limited to p_xqn*p_nrc because Phaeocystis may contain mode N
  ! than the maximum. This N is found in the colony but ouside the cells. 
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  do i = 1 , ( iiPhytoPlankton)
    qnPc(i,:)  =   PhytoPlankton(i,iiN)/( p_small+ PhytoPlankton(i,iiC))
    qpPc(i,:)  =   PhytoPlankton(i,iiP)/( p_small+ PhytoPlankton(i,iiC))
    if ( i== iiP6 ) then
       qnPc(i,:)=min(qnPc(i,:),p_xqn(i)*p_qnRc(i))
       qpPc(i,:)=min(qpPc(i,:),p_xqp(i)*p_qpRc(i))
    endif
    qlPc(i,:)=min(1.0*p_qchlc(i), &
       max(0.1* p_qlPlc(i),PhytoPlankton(i,iiL)/( p_small+ PhytoPlankton(i,iiC))))
    qsPc(i,:)=0.0;
    j=ppPhytoplankton(i,iiS)
    if ( j>0 ) then
         qsPc(i,:)  =   PhytoPlankton(i,iiS)/( p_small+ PhytoPlankton(i,iiC))
!        qsPc(i,:)=max(qsPc(i,:),p_qslc(i))
    endif
    flPIR6s(i,:)  =   0.0D+00
  end do


! do i = 1 , ( iiPhytoPlankton)
!     r= PhytoPlankton(i,iic)
!     call findnan(r,NO_BOXES,j)
!     if (j.gt.0) then
!       write(LOGUNIT,*) 'Nan in phyto,layer',i,j
!     endif
! end do
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Compute nutrient quota in Pelagic Bacteria
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  qpB1c(:)  =   min(p_qpBc,max(p_qpBlc,B1p(:)/( p_small+ B1c(:))))
  qnB1c(:)  =   min(p_qnBc,max(p_qnBlc,B1n(:)/( p_small+ B1c(:))))

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ! Compute sedimentation velocities
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


  sediR2    =1.0D-80 ;
  wfR2=1.0D-80
  sediR6    =1.0D-80;


  do i = 1 , iiPhytoPlankton
   sediPI(i,:)  =   p_rrPIm(i)
  enddo

  ! concentration TEP determines sinking rate!

  if ( p_raPIm> 0.0) then
    select case (p_sw_sticky)
       case (2); s=exp_limit(-p_steep*(1.0-R2c(:)/p_chR2R2c)) !non linear start of stikyness
       case (3); s=min(1.0D+00,exp_limit(-p_steep*(p_chR2R2c/(1.0D+00+R2c(:)))))
    end select

  ! if proportion is favorable to make macroaggregates sinking of R2 and Phyt is enhanced!
  ! sinking start if R2 concentration >= phyto
  
    totmacro=R2c(:)
    j=0;k=0
    do while (k<iiPhytoplankton)
      j=j+1;newmacro=0;
      do i = 1 , iiPhytoPlankton
        if ( p_seq_calc(i)==j ) then
          k=k+1
          lcl_PhytoPlankton => PhytoPlankton(i,iiC)
          phyt=max(1.0D-80,lcl_PhytoPlankton-1.0)/(1.0D-80+lcl_PhytoPlankton)
          r=min(1.0D+00,max(0.0D+00,  &
                  totmacro/(p_poR2PI(i)*(p_small+lcl_PhytoPlankton))-1.0))*phyt
          sediR2=sediR2+max(0.0,min(R2c-wfR2,r*lcl_PhytoPlankton*p_poR2PI(i))*s*p_raPIm)
          sediPI(i,:)=sediPI(i,:)+r*s*p_raPIm
          wfR2=  wfR2  +max(0.0,min(R2c-wfR2,r*lcl_PhytoPLankton*p_poR2PI(i)))
          newmacro=newmacro+r*lcl_PhytoPlankton
        endif
      enddo
      totmacro=totmacro+newmacro
    enddo

    do i = 1 , iiPhytoPlankton
      sediPI(i,2:NO_BOXES)=max(sediPI(i,2:NO_BOXES),sediPI(i,1:NO_BOXES-1))
    end do
  
    where (R2c(:) > 1.0D-10) 
       sediR2(:)=sediR2(:)/wfR2
       sediR2(2:NO_BOXES)=max(sediR2(2:NO_BOXES),sediR2(1:NO_BOXES-1))
    endwhere
  endif
  sediRZ(:)=p_raRZm
  sediR6(:)=p_rrR6m

  ! Calculate rlative sinking rate (s is here a dummy variable)
  call PhaeocystisCalc(9,iiP6,r,s,0.0D+00)
  sediPI(iiP6,:)=max(sediPI(iiP6,:) , r*p_raP6m)
  sediR6(:)= (p_rrR6m*max(0.0,R6c(:)-RZc(:))+p_raRZm*(p_small+RZc(:)))/(p_small+ max(RZc(:),R6c(:))) 

  do i = 1 , ( iiMesoZooPlankton)
    if (p_rMem(i) < 0.0 ) then
      eo  =   MM_power_vector(  max(p_small,O2o(:)),  p_clMeO2o(i),3)
      sediMeZ(i,:)=p_rMem(i)* (1.0D+00-eo)
      sediMeZ(i,NO_BOXES_Z)=0.0;
    else
      sediMeZ(i,:)=p_rMem(i)
      sediMeZ(i,NO_BOXES_Z)=0.0;
    endif
  end do

  do i = 1 , ( iiMicroZooPlankton)
    eo  =   MM_power_vector(  max(p_small,O2o(:)),  p_clMiO2o(i),3)
    sediMiZ(i,:)=p_rMim(i)* (1.0D+00-eo)
    sediMiZ(i,NO_BOXES_Z)=0.0;
  end do

  !----------------------------------------------------
  ! Calculation of nutrient limitation index 
  ! 1=n-limitation, 5=p-limitation,3=si-limitations,
  ! 2= si-limitation for diatoms,n-limitation for other 
  ! 4= si-limitation for diatoms,p-limitation for other 
  !----------------------------------------------------

  nn=0.0;np=0.0;ns=0.0;r=0.0;s=0.0;
  do i=1,iiPhytoplankton
      nn=nn+ PhytoPlankton(i,iiN)/p_qnRc(i)
      np=np+ PhytoPlankton(i,iiP)/p_qpRc(i)
      if (ppPhytoPlankton(i,iiS) >0) then
        ns=ns+ PhytoPlankton(i,iiS)/p_qsRc(i)
        s=s+ PhytoPlankton(i,iiC)
      endif
      r=r+ PhytoPlankton(i,iiC)
  enddo


  nn=nn/(p_small+r)
  np=np/(p_small+r)
  ns=ns/(p_small+s)
  limnuti=0.0 ;s=0.0;
  where ( ns < 1.0 .and. ns<nn .and. ns<np  ) s=3.0
  where ( nn < 1.0 .and.nn < np )
    limnuti=1.0
  elsewhere (np < 1.0 .and. np < nn )
     limnuti=5.0
  endwhere
  where (s>0.0 .and. limnuti>0.0) 
        limnuti=(limnuti + s)* 0.5 
  elsewhere(s > 0.0) 
        limnuti=s
  endwhere

  !----------------------------------------------------
  ! Calculation of Phyto functional group index
  ! nr of functional group with the hihgest biomass which is larger tan 5.0 
  !----------------------------------------------------
  r=0.0; s=0.0 ;PTi=0.0;
  do i=1,iiPhytoplankton
    s=  PhytoPlankton(i,iiC) 
    where (s> 5.0 .and. s>r) 
       r=s; PTi=real(i)
    endwhere
  enddo
  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
