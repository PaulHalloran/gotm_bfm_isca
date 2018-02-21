#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: PhaecoystisCalc
!
! DESCRIPTION
!   This process describes the dynamics of all phytoplankton
!    groups in the ERSEM model. The differences in behaviour
!    are expressed by differences in parameter-values only.
!    
!
!

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine PhaeocystisCalc(mode,phyto,output,input1,param)
!
! !USES:

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN
#IFDEF NOPOINTERS
  use mem,  ONLY: D3STATE
#ELSE
  use mem, ONLY: D3STATE, R3c,N3n,N4n,N1p,Pcc,P1c
#ENDIF
  use mem, ONLY: ETW, &
    NO_BOXES, iiPel, flux_vector,iiC,iiN,iiP,PhytoPlankton,&
    ppP2c,ppP2n,ppP2p,ppP2l,ppP6c,ppP6n,ppP6p,ppP6l,ppPcc,ppR1c,ppR3c,&
    iiP2,iiP6,ppR6c,ppR1c,&
    flPIR6n,flPIR1n, flPIR6p,flPIR1p,flR3R2c,flnDIn,flnDIp,Depth,OCDepth,qnPc,qpPc
  use bio_bfm, ONLY: DeriveFromGotm
  use mem_Param,  ONLY: p_small, p_s_max,p_q10diff
  use mem_Phaeo
  use mem_Phyto,  ONLY: p_xqn,p_xqp,p_qnlc,p_qplc,p_qnRc,p_qpRc, &
                   p_qun,p_qup,p_qlPlc,p_qchlc,p_pu_ea,p_pu_ra,p_lN3N4n
  use mem_Param,  ONLY:  p_q10diff,p_pe_R1c, p_pe_R1n,p_pe_R1p


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! The following vector functions are used:eTq_vector, MM_vector
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use mem_globalfun,   ONLY: eTq_vector, exp_limit,insw_vector,insw

! use mem,  ONLY: Output2d_1,Output2d_2,Output2d_3, Output2d_4

  IMPLICIT NONE
  real(RLEN),external  ::GetDelta;


! !INPUT:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer,intent(IN)                         :: mode
  integer,intent(IN)                         :: phyto
  real(RLEN),dimension(NO_BOXES),intent(INOUT) :: output
  real(RLEN),dimension(NO_BOXES),intent(IN)  :: input1
  real(RLEN),intent(IN)  :: param

!  
!
! !AUTHORS
!   ERSEM group !     P. Ruardij (NIOZ)
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
  ! Set up Local Variable for copy of state var. object
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN),dimension(NO_BOXES):: phytoc
  real(RLEN),dimension(NO_BOXES):: phyton
  real(RLEN),dimension(NO_BOXES):: phytop

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer    :: iout
  real(RLEN)                      :: delta,sir,siq,sis,q_er
  real(RLEN),dimension(NO_BOXES)  :: diff
  real(RLEN),dimension(NO_BOXES)  :: nrCells
  real(RLEN),dimension(NO_BOXES)  :: nrCols
  real(RLEN),dimension(NO_BOXES)  :: ShCol
  real(RLEN),dimension(NO_BOXES)  :: ShCell
  real(RLEN),dimension(NO_BOXES)  :: sizeCol
  real(RLEN),dimension(NO_BOXES)  :: r,t,et
  real(RLEN),dimension(NO_BOXES)  :: s
  real(RLEN),dimension(NO_BOXES)  :: q
  real(RLEN),dimension(NO_BOXES)  :: c0
  real(RLEN),dimension(NO_BOXES)  :: C
  real(RLEN),dimension(NO_BOXES)  :: vmax
  real(RLEN),dimension(NO_BOXES)  :: sdo
  real(RLEN),dimension(NO_BOXES)  :: rump
  real(RLEN),dimension(NO_BOXES)  :: rumn
  real(RLEN),dimension(NO_BOXES)  :: run
  real(RLEN),dimension(NO_BOXES)  :: test_optimal_growth
  real(RLEN),dimension(NO_BOXES)  :: E !shearrate
  real(RLEN)                      :: rn(NO_BOXES,2)
  real(RLEN)                      :: r2(2)
  real(RLEN),parameter            :: rPI= 3.1415926535897D+00

  
  phytoc = PhytoPlankton(phyto,iiC)

  select case (mode)
    case (CALC_MORTALITY)  !1 Mortality of degradation of colonies and mortality in cells
      output=input1
      if (phyto.ne.iiP6 )return
      if (sum(phytoc*depth)/NO_BOXES< 10.D-5) return
      sdo=output;q=output
      where (phytoc >1.0D-10 )
      ! next construction (r=0.001*)  is built in if biomass is very near zeo.
      ! when quata shows sometimes strange not understood behaviour.....
        t=0.0;
        sdo=sdo+t
        ! survival rate  due to high proportion TEP in colony
        r=(1.0- (1.0D-10+R3c)/(1.0D-10+phytoc)/p_qmR3P6) 
        ! survival rate  due too large sized colonies
        t= max(1.0,(1.0D-15+phytoc)/(1.0D-10+Pcc))
        s= (1.0-t/p_max_ColSize) 
        !small colonies do not survive shallow depths due turbulence stress.
        ! OCDEPTH(1)=cumulativve depth
        t= 1.0-2.0/t *insw(5.0-OCDepth(1))

        ! mortality ==0 if min(r,s,t)==1
        q=max(0.0,exp_limit(-p_steep_mort*min(r,s,t))-exp(-p_steep_mort))
        ! total mortality is limitted at low concentration of phytoc to avoid NaN's
        sdo=sdo+min(5.0,q* phytoc/(phytoc+0.001)*p_sdmo)
        q=sdo*1.0+1.0*max(0.0D+00,Pcc-phytoc)/(1.0D-10+Pcc)
      endwhere  

      delta=GetDelta();
      ! phytoc >=Pcc!
      r=min(p_s_max/delta,q)
      call flux_vector(iiPel,ppPcc,ppPcc,-r*Pcc)

      output=sdo  
    case (CALC_MORTALITY_CELLS_IN_COLONY)  !20  mortality of cells in colony
      ! after a direct return the value in output keep its value!
      output=0.0
      if (sum(phytoc*Depth)/NO_BOXES< 10.D-5) return
      if (param > 0.0 .or. phyto.ne.iiP6 )return
      et=input1
      !Calculate number of colonies per m3 
      nrCols=(1.0D-10+Pcc)/p_wP6c
      !Calculate number of cells per m3 
      nrCells=phytoc/p_wP6c
      ! Calculate of Cel per colony
      sizeCol=max(1.0D+00,nrCells/nrCols)
      where (sizeCol.gt.1.5) 
         output=et*p_smxInC*sizeCol/(sizeCol+p_chnxInC)
      endwhere
    case (FORCE_MORTALITY)  !21 
      !at high rates prim. production can counteract mortality
      !output of this option is used to correct sumc at forehand
      output=1.0
      delta=GetDelta();
      if (phyto.ne.iiP6 )return
      output=1.0-min(p_s_max/delta,input1)*delta
    case (MAX_MORTALITY)  !22 determine maximal allowable mortality per timestep
    !this is used to correct mortality with option 1. See further at Phyto.F90
      output=input1
      if (phyto.ne.iiP6 )return
      delta=GetDelta();
      output=min(p_s_max/delta,input1)
    case (CALC_LOC_DET_FLUX)
      !calculate fluxes from Carbon & nutrients collected in the interstial room between the colonies
      if (phyto.ne.iiP6 )return
      phyton = PhytoPlankton(phyto,iiN)
      phytop = PhytoPlankton(phyto,iiP)
      sdo=input1;
      r=0.0;s=0.0
      ! Calculate the proportion between TEP in the colony, but outside the cells : to C in cells
      q_er=p_pu_ea(phyto)/(1.0-p_pu_ea(phyto))/(1.0-p_pu_ra(phyto))
      where (sdo>0.0.and.R3c>1.0D-5)
        !subract from the total C outside the cells "the TEP made by the phytoplankton"
        !now you have the mass of the number of dead cells in the colony
        r=max(0.0,R3c-q_er*phytoc)
!       r=max(0.0,(R3c-q_er*phytoc)/(1.0+q_er))
        !calculate now the maximal TEP_mass  produced by the total number of cells:
        c=q_er*(phytoc+r)
        !Calculate C according quota how C is present, assuming that all N and P
        !which is larger than the actual quotum is outside the cell but in the colony   
        !max function is introduced for very low values of Phaeocystis where internal show not
        ! an approriate behaviour
        s=max(0.0,(phyton/max(p_qnlc(phyto),qnPc(phyto,:))-phytoc), &
                      (phytop/max(p_qplc(phyto),qpPc(phyto,:))-phytoc))
      endwhere

       ! s in the real TEP  limited in bounds 
       s=max(0.0,min(R3c,r,s))
       r=0.0
       where (R3c > 0.0) &
          r=min(5.0,R3c/(1.0D-80+phytoc)/p_max_ColSize)*R3c*insw_vector(1.0D-04-phytoc)
       !remove last R3 when P6 is neraly zero
       flR3R2c(:)=flR3R2c(:)+sdo*(R3c-s)+r
        
       q=max(0.0,phyton-qnPc(phyto,:)*phytoc)
       flPIR1n(phyto,:)=flPIR1n(phyto,:) + sdo* p_pe_R1n* q
       flPIR6n(phyto,:)=flPIR6n(phyto,:) + sdo* (1.0-p_pe_R1n)* q
       q=max(0.0,phytop-qpPc(phyto,:)*phytoc)
       flPIR1p(phyto,:)=flPIR1p(phyto,:) + sdo* p_pe_R1p* q
       flPIR6p(phyto,:)=flPIR6p(phyto,:) + sdo* (1.0-p_pe_R1p)* q
       call flux_vector(iiPel,ppR3c,ppR1c,sdo* p_pe_R1c* s)
       call flux_vector(iiPel,ppR3c,ppR6c,sdo* (1.0-p_pe_R1c)* s)
    case (NEW_COLONIES)  !2 transfer of new cells which form colonies
      !in very shallow areas (<5meter) no colonies can be formed: 
      !there is too much physical stress...
      if (phyto.ne.iiP2.or.OCDepth(1)<5.0 )return
!     et  =   eTq_vector(  ETW(:),  p_q10diff)
      r=p_lN3N4n(phyto)/(p_lN3N4n(phyto)+N4n(:))
      rumn(:)  = p_qun(phyto)* (N3n(:)) * phytoc
      rump(:)  = p_qup(phyto)* N1p(:) *phytoc
      run=input1*phytoc ;test_optimal_growth=0.0

      where (run >0)
        test_optimal_growth=min(rumn/(p_xqn(phyto)*p_qnRc(phyto)), &
                              rump/(p_xqp(phyto)*p_qpRc(phyto)))/max(p_small,run) 
        test_optimal_growth=max(0.0,test_optimal_growth)*insw(OCDepth(1)-5.0)
      end where
      

      ! use uncorrected rumn's instead of corrected rumn's by corrector-predictor..:1
      s=max(0.0,-flnDIn/(N3n),-flnDIp/N1p)
      rn=-1.0; r=-1.0;;t=-1.0
      rn(:,1)=+p_small+0.040
      rn(:,2)=+p_small+0.4
      t=1.0
      where (s.gt.0.0) t=min(1.0,(abs(s-rn(:,1)) +abs(rn(:,2)-s)-(rn(:,2)-rn(:,1)))/s)
      ! use uncorrected rumn's instedad of corrected rumn's by corrector-predictor..:1
      where (test_optimal_growth>0.0D+00 )
        s= (p_small+rumn(:))/( 1.0D-80+ p_qun(phyto)*phytoc )
        rn(:,1)=+p_small+p_clP2P6n
        rn(:,2)=+p_small+p_cmP2P6n
        r=min(1.0,(abs(s-rn(:,1)) +abs(rn(:,2)-s)-(rn(:,2)-rn(:,1)))/s)
      endwhere
      where (test_optimal_growth>0.0D+00 )
        q= (p_small +rump(:))/( 1.0D-80+ p_qup(phyto)*phytoc )
        rn(:,1)=+p_small+p_clP2P6p 
        rn(:,2)=+p_small+p_cmP2P6p 
        s=min(1.0,(abs(q-rn(:,1)) +abs(rn(:,2)-q)-(rn(:,2)-rn(:,1)))/q)
        test_optimal_growth=&
          min(test_optimal_growth,max(0.0,min(1.0-r,1.0-s,1.0-t)))
      endwhere

      r= max(0.0D+00,min(1.00,test_optimal_growth))* p_sP2P6  
      call flux_vector(iiPel,ppP2c,ppP6c,r*run);
      call flux_vector(iiPel,ppP2n,ppP6n,r*run*p_qnRc(phyto));
      call flux_vector(iiPel,ppP2p,ppP6p,r*run*p_qpRc(phyto));
      call flux_vector(iiPel,ppP2l,ppP6l,r*run*p_qchlc(phyto));
      call flux_vector(iiPel,ppPcc,ppPcc,r*run);
    !11,13,14,15,nutrient  limitation  of cells in colonies
    case (CALC_REL_PHOSPHATE_UPTAKE,CALC_REL_NITRATE_UPTAKE, &
                         CALC_REL_AMMONIUM_UPTAKE,CALC_REL_UREA_UPTAKE) 
      output=1.0D+00
      if (phyto.ne.iiP6 )return
      select case (mode)
        case(CALC_REL_PHOSPHATE_UPTAKE) ; r=p_diff_N1  ! case 11 : phophate
        case(CALC_REL_NITRATE_UPTAKE)   ; r=p_diff_N3  ! case 13 : nitrate
        case(CALC_REL_AMMONIUM_UPTAKE)  ; r=p_diff_N4  ! case 14 : ammonium
        case(CALC_REL_UREA_UPTAKE)      ; r=p_diff_urea !case 15 : urea
      end select
      vmax=param
      C=input1 ! externe nutrient concentration
      diff=r* eTq_vector( ETW, p_q10diff)
      !Calculate number of colonies per m3 
      nrCols=(1.0D-10+Pcc)/p_wP6c
      !Calculate number of cells per m3 
      nrCells=phytoc/p_wP6c
      ! Calculate of Cel per colony
      sizeCol=max(1.0D+00,nrCells/nrCols)
      s=p_dP6m
      output=1.0;
      if (sum(phytoc)/NO_BOXES< 1.0D-5) return
      where (sizeCol.gt.1.5) 
        !Calculate the volume of one colony according Rousseau(1990) and transfer from mm3-->m3
        r=1.0D-9*10.0D+00**((log10(sizeCol)-3.67D+00)/0.51D+00)
        !Calculate from volume -->diameter
        s=2.0D+00*((r*3.0D+00/(4.0D+00*rPI))**0.33333D+00)
      endwhere
      s=max(p_dP6m,s);
      call DeriveFromGotm(1,NO_BOXES,E)
      call CalcSh(E,s,diff,ShCol)
      r=p_dP6m
      call CalcSh(E,r,diff,ShCell)
      output=1.0D+00
      where (sizeCol.gt.1.5D+00) 
        ! Calculate the uptake of 1 cell (always) located at Colony Wall
        q=ShCol*rPI*8.0D+00*s*diff
        c0=q*C/(q+sizeCol*vmax)
        !Calculate the uptake of 1 single cell
        q=ShCell*rPI*8.0D+00*p_dP6m*diff
        r=q*C/(q+vmax)
        !Use quotient of surface of colonies and surface of cells as a measure for the nutrient limitation
        output=(p_small+c0)/(p_small+r)
      end where
      output=min(1.0D+00,output)
    case (CALC_NET_NITROGEN_UPTAKE) ! 9 relative sedimentation rate of cells in colonies
      output=0.0D+00
      if (phyto.eq.iiP6) output=input1*p_qnR3c
    case (CALC_REL_SEDIMENTATION) ! 9 relative sedimentation rate of cells in colonies
      !Colonies sinks due to size or if the colonies  include more TEP than the maximum
      output=0.0D+00
      if (phyto.ne.iiP6)return
      sir=sum(Depth);siq=sum(phytoc*Depth)/sir;sis=sum(pcc*Depth)/sir
      if (siq.gt.1.0D-2.and.sis.gt.1.0D-4)  then
        sizecol=1.0
        where (phytoc>10.0) sizeCol=min(1.0D+06,max(1.0D+00,1.0d-10+phytoc/(1.0D-10+Pcc)))
        r=max(0.0,1.0-max(0.0,log10(sizeCol))/log10(p_max_ColSize))
        s=max(0.0,1.0-max(0.0,(1.0D-10+R3c)/(1.0D-10+phytoc)/p_qmR3P6)) 
        output= exp(-4.0*(r+s)) 
      endif
    case (CALC_FOOD_FILTERFEEDER,CALC_FOOD_MESOZOO,CALC_FOOD_MICROZOO) 
      output=input1
      if (phyto.ne.iiP6.or.param.eq.0.0D+00 )return
      if (sum(phytoc)/NO_BOXES< 1.0D-5) then
        output=0.0D+00;return;
      endif
      select case (mode)
        case (CALC_FOOD_FILTERFEEDER) ; r2=p_cuY3a  !read range min->max
        case (CALC_FOOD_MESOZOO)      ; r2=p_cuZ4a 
        case (CALC_FOOD_MICROZOO)     ; r2=p_cuZ5a 
      end select
      ! Calculate number of Cells per colony
      sizeCol=min(1.0D+06,max(1.0,phytoc/(1.0D-10+Pcc)))
      ! Only for the given size range the results of this calculation ==0
      where (phytoc > 1.0D-10.and.Pcc.gt.1.0D-10)
         r=min(1.0D+00,(abs(sizeCol-r2(1)) +abs(r2(2)-sizeCol)-(r2(2)-r2(1)))/sizeCol)
         output=output*max(0.0D+00,1.0D+00-r)
      elsewhere
         output=0.0D+00
      endwhere
      if (sw_select ==1) then
          select case (mode)
            case (CALC_GRAZING_FILTERFEEDER) ; r2=p_cuY3a  !read range min->max
            case (CALC_GRAZING_MESOZOO)      ; r2=p_cuZ4a 
            case (CALC_GRAZING_MICROZOO)     ; r2=p_cuZ5a 
          end select
          where ( phytoc>1.0D-10 .and. Pcc >1.0D-10  ) 
           s=min(1.0D+06,(1.0D-10+phytoc)/(1.0D-10+Pcc))
           where ( s >= r2(2)) 
             r=(p_small+input1)/r2(2)/(1.0D-10+Pcc)
           elsewhere  ( s <= r2(1).and.s>=1.0D+00) 
             r=(p_small+input1)/r2(1)/(1.0D-10+Pcc)
           endwhere
          endwhere
       endif
      if ( mode == CALC_FOOD_MICROZOO) then
        !young Phaeocystis colonies seem protected by diatoms.
        output=output*min(1.0,phytoc*100.0/(100.0+P1c*insw(OCDepth(1)-5.0)))
      endif
!   -3,-4,-5   Calculation of rates due to grazing  of colonies
             ! by MesoZoo and MicroZoo for R3 (export to R2), Pcc (mortality) 
    case (CALC_GRAZING_FILTERFEEDER,CALC_GRAZING_MESOZOO,CALC_GRAZING_MICROZOO) 
      if (phyto.eq.iiP6.and.param.ne.0.0D+00 ) then
        r=0.0D+00;
        where (phytoc .gt. 1.0D-10.and.Pcc.gt.1.0D-10)
           r=(p_small+input1)/(p_small+phytoc);
        endwhere
        delta=GetDelta(); r=min(p_s_max/delta,r)
        call flux_vector(iiPel,ppPcc,ppPcc,-r*Pcc);
     endif
     output=input1;
  end select


  end subroutine PhaeocystisCalc
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

! !INTERFACE
  subroutine PhaeocystisCalc_1l(mode,phyto,place,layer,output,input1,param)
!
! !USES:
  use global_mem, ONLY:RLEN
#IFDEF NOPOINTERS
  use mem,  ONLY: D3STATE
#ELSE
  use mem, ONLY: D3STATE,R3c
#ENDIF
  use mem, ONLY: NO_BOXES, iiPel, flux,iiC,iiN,iiP,PhytoPlankton, &
    ppPcc,Pcc, iiP6,qnPc,qpPc 
  use mem_Phyto,  ONLY: p_pu_ea,p_pu_ra ,p_qnlc,p_qplc
  use mem_Param,  ONLY: p_small, p_s_max,p_pe_R1c, p_pe_R1n,p_pe_R1p
  use mem_Phaeo

! !INPUT:
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  implicit none
  integer,intent(IN)                         :: mode
  integer,intent(IN)                         :: phyto
  integer,intent(IN)                         :: place
  integer,intent(IN)                         :: layer
  real(RLEN),intent(OUT)                     :: output
  real(RLEN),intent(IN)                      :: input1
  real(RLEN),intent(IN)                      :: param

  real(RLEN),external  ::GetDelta;
  real(RLEN),dimension(NO_BOXES)             :: phytoc_all
  real(RLEN)                                 ::phytoc,phyton,phytop
  real(RLEN)                                 ::r
  real(RLEN)                                 ::s
  real(RLEN)                                 ::q,q_er,c
  real(RLEN)                                 ::sizeCol
  real(RLEN)                                 ::nrCols
  real(RLEN)                                 ::nrCells
  real(RLEN)                                 ::r2(2)
  real(RLEN)                                 ::sdo
  real(RLEN)                      :: delta
  real(RLEN),parameter            :: rPI= 3.1415926535897D+00

  phytoc_all= PhytoPlankton(phyto,iiC);phytoc=phytoc_all(layer)

  select case (mode)
    case (CALC_FOOD_FILTERFEEDER,CALC_FOOD_MESOZOO,CALC_FOOD_MICROZOO) 
!   (3,4,5) 
      output=input1
      if (phyto.ne.iiP6.or.param.eq.0.0D+00 )return
      if ( phytoc<1.0D-10.or.Pcc(layer)<1.0D-10) then
        output=0.0D+00;return
      endif
      select case (mode)
        case (CALC_FOOD_FILTERFEEDER) ; r2=p_cuY3a  !read range min->max
        case (CALC_FOOD_MESOZOO)      ; r2=p_cuZ4a 
        case (CALC_FOOD_MICROZOO)     ; r2=p_cuZ5a 
      end select
      ! Calculate number of Cells per colony
      sizeCol=min(1.0D+06,max(1.0D+00,phytoc/(1.0D-10+Pcc(layer))))
      ! Only for the given size range the results of this calculation ==0
      r=min(1.0D+00,(abs(sizeCol-r2(1)) +abs(r2(2)-sizeCol)-(r2(2)-r2(1)))/sizeCol)
      output=output*max(0.D+00,1.0D+00-r)
!   -3,-4,-5  ! Calculation of rates due to grazing  of colonies
              ! by MesoZoo and MicroZoo for R3 (export to R2), Pcc (mortality) 
      output=input1;
    case (CALC_GRAZING_FILTERFEEDER,CALC_GRAZING_MESOZOO,CALC_GRAZING_MICROZOO) 
      if (phyto.ne.iiP6.or.param.eq.0.0D+00.or.input1.eq.0.0D+00 ) then
           output=input1;return
      endif
      if (phytoc>1.0D-10 .and.Pcc(layer)>1.0D-10) then
        r=(p_small+input1)/(p_small+phytoc);
        select case (mode)
          case (CALC_GRAZING_FILTERFEEDER) ; r2=p_cuY3a  !read range min->max
          case (CALC_GRAZING_MESOZOO)      ; r2=p_cuZ4a 
          case (CALC_GRAZING_MICROZOO)     ; r2=p_cuZ5a 
        end select
        if (sw_select ==1) then
          sizeCol=min(1.0D+06,phytoc/(1.0D-10+Pcc(layer)))
          if ( sizeCol <= r2(1).and.sizeCol>1.0D+00) r=input1/r2(1)/(1.0D-10+Pcc(layer))
          if ( sizeCol >= r2(2)) r=input1/r2(2)/(1.0D-10+Pcc(layer))
        endif
        ! Calculate number of Cells per colony
        delta=GetDelta(); r=min(p_s_max/delta,r)
        call flux(layer,iiPel,ppPcc,ppPcc,-r*Pcc(layer))
        output=input1
      endif
    case (CALC_LIMIT_FILTERCAP) ! Hampering of filtering of filterfeeders by Phaeocystis
      output=1.0D+00;
      if (phyto.ne.iiP6.or.param.eq.0.0D+00.or.input1.eq.0.0 )return
      if ( phytoc<1.0D-10.or.Pcc(layer)<1.0D-10)return 
     !Calculate number of colonies per m3 
      nrCols=(1.0D-10+Pcc(layer))/p_wP6c
      !Calculate number of cells per m3 
      nrCells=phytoc/p_wP6c
      ! Calculate of Cel per colony
      sizeCol=max(1.0,nrCells/nrCols)
      s=4.0*rPI*(p_dP6m/2.0D+00)**2 ; ! p_dP6m= minimal surface colony=single cell!
      if (sizeCol.gt.1.5D+00)  then
        !Calculate the volume of one colony according Rousseau(1990) and transfer from mm3-->m3
        r=1.0D-9*10.0D+00**((log10(sizeCol)-3.67D+00)/0.51D+00)
        !Calculate from volume -->radius
        s=((r*3.0D+00/(4.0D+00*rPI))**0.33333D+00)
        !Calculate from radius --> (2-dimensional) circular equivalent surface area
        s=rPI*s*s
      endif
      !Calculate total surface
      q=s*nrCols     
      !Calculate filterlimitations according
      !Smaal,AC && Twisk,F(1997), JournalOF Exp. Mar. Biologyand Ecology,209:33-46
      !(unit of Smaal) mm2/ml ====  m2/m3/ (unit of BFM) 
      ! use only to calculate the fraction of the maximum filtration
      output=max(0.D+00,0.774D+00-0.12D+00*q)/0.774D+00
  end select
  end subroutine PhaeocystisCalc_1l
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

! DESCRIPTION
  !Calculate Sherwood Number
! !INTERFACE
  subroutine CalcSh(E, diameter, diffusion,sh)
!
! !USES:
  use global_mem, ONLY:RLEN
  use mem,ONLY:NO_BOXES

! !INPUT:
  implicit none

  REAL(RLEN),intent(IN)         ::E(NO_BOXES)
  REAL(RLEN),intent(IN)         ::diameter(NO_BOXES)
  REAL(RLEN),intent(IN)         ::diffusion(NO_BOXES)
  REAL(RLEN),intent(OUT)        ::sh(NO_BOXES)

  real(RLEN)                   :: Pe(NO_BOXES) 

  Pe= (0.5D+00* diameter)**2.0D+00 * E /diffusion;

  where ( Pe < 0.01D+00 ) 
    sh= ( 1.0D+00 + 0.29D+00 *  sqrt(Pe));
  elsewhere ( Pe < 100.0D+00 ) 
    sh= ( 1.014D+00 + 0.51D+00 *  sqrt(Pe));
  elsewhere
    sh= ( 0.55D+00 * Pe**0.3333D+00);
  endwhere

  end subroutine CalcSh
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

