#INCLUDE "DEBUG.h"
#INCLUDE "INCLUDE.h"

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: PelForcingForBen
!
! DESCRIPTION
!   !

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  subroutine PelForcingForBenDynamics
!
! !USES:
  ! The following Pelagic-states are used (NOT in fluxes): R6c, R6n, R6p, R6s, &
  ! N1p, N3n, N4n, N5s, N6r, O2o
  ! The following box states are used (NOT in fluxes): PhytoPlankton
  ! The following global scalar vars are used: &
  ! BoxNumberZ, NO_BOXES_X, BoxNumberY, NO_BOXES_Y, &
  ! BoxNumber, BoxNumberXY
  ! The following Pelagic 1-d global boxvars  are used: ETW, Depth
  ! The following Benthic 1-d global boxvars are modified : PI_Benc, &
  ! PI_Benn, PI_Benp, PI_Bens, ZI_Fc, ZI_Fn, ZI_Fp
  ! The following Benthic 1-d global boxvars got a value: RI_Fc, &
  ! RI_Fn, RI_Fp, RI_Fs, N1p_Ben, N3n_Ben, N4n_Ben, N5s_Ben, N6r_Ben, O2o_Ben, &
  ! ETW_Ben, Depth_Ben
  ! The following groupmember vars  are used: iiPhytoPlankton, iiP1
  ! The following constituent constants  are used: iiC, iiN, iiP, iiS
  ! The following global constants are used: RLEN

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Modules (use of ONLY is strongly encouraged!)
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  use global_mem, ONLY:RLEN,LOGUNIT
  use mem, ONLY: RZc,R6c, R6n, R6p, R6s, N1p, N3n, N4n, N5s, N6r, O2o, &
    iiPhytoPlankton, ppPhytoPlankton, PhytoPlankton, Z2c, D2STATE, &
    iiMicroZooPlankton, ppMicroZooPlankton, MicroZooPlankton
  use mem, ONLY: puP6Y3,BoxNumberZ, &
    BoxNumberX, NO_BOXES, NO_BOXES_X, BoxNumberY, NO_BOXES_Y, BoxNumber, &
    BoxNumberXY, ETW, ESW, ERHO, ETW_Ben, ESW_Ben, ERHO_Ben, &
    PI_Benc, PI_Benn, PI_Benp, PI_Bens, sediPI_Ben,sediR6_Ben, sediPI, sediR6, &
    Depth, RI_Fc, ZI_Fc, ZI_Fn, ZI_Fp, RI_Fn, RI_Fp,sediRZ, &
    RI_Fs, N1p_Ben, N3n_Ben, N4n_Ben, N5s_Ben, N6r_Ben, O2o_Ben, ETW_Ben, &
    Depth_Ben, iiC, iiN, iiP, iiS, iiP6, efilP6Y3, R3c,&
    ctfPm2c,ctfZm2c,ctfRm2c,cZ2m2c,qnPc,qpPc,qsPc,R3_Benc ,R3_Benn ,R3_Benp 
#ifdef INCLUDE_BENCO2
    use mem, ONLY: O3c_Ben,O3c,O3h_Ben,O3h,ctO3m2h
#endif
  use mem_MicroZoo, ONLY:p_qnMic=>p_qnc,p_qpMic=>p_qpc
  use mem_FilterFeeder, ONLY:p_puPI,p_vum
  use mem_Param,  ONLY: p_small,CalcPhytoPlankton,CalcMicroZooPlankton
  use mem_Phaeo, ONLY:CALC_LIMIT_FILTERCAP,CALC_FOOD_FILTERFEEDER
  use mem_GlobalFun, ONLY: insw
!  
!
! !AUTHORS
!   Piet Ruardij
!
!
! !REVISION_HISTORY
!   Created at Wed Jun 16 02:04:44 PM CEST 2004
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
  ! Local Vectors used  of group vectors
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  real(RLEN), dimension(:), pointer  ::lcl_PhytoPlankton
  real(RLEN), dimension(:), pointer  ::lcl_MicroZooPlankton
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Local Variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  integer        :: i
  integer        :: j
  real(RLEN)     :: r
  real(RLEN)     :: Pc,Pnp

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

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Compute total phytoplankton conc. used as food for filtereeders
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      ctfPm2c=0.0
      R3_Benc=0.0;
      do i = 1 , ( iiPhytoPlankton)
        if (CalcPhytoPlankton(i)) then
          lcl_PhytoPlankton => PhytoPlankton(i,iiC)
          Pc=lcl_PhytoPlankton(BoxNumber); Pc=Pc * insw(Pc-1.0D-10)
          ! r=1 if p_puPI(i) >0 and and index i is not representing Phaeocysytis
          call PhaeocystisCalc_1l(CALC_FOOD_FILTERFEEDER,i,BoxNumberXY,&
                                         BoxNumber,r,1.0D+00,p_puPI(i))
          PI_Benc(i,BoxNumberXY)=r*p_puPI(i)*Pc
          ctfPm2c=ctfPm2c+r*p_puPI(i)*sum(Depth*lcl_PhytoPlankton)
          PI_Benn(i,BoxNumberXY) =  r*p_puPI(i)*qnPc(i,BoxNumber)* Pc
          PI_Benp(i,BoxNumberXY) =  r*p_puPI(i)*qpPc(i,BoxNumber)* Pc
              j=ppPhytoPlankton(i,iiS)
          if ( j > 0 ) then
            PI_Bens(i,BoxNumberXY)  =  r*p_puPI(i)*qsPc(i,BoxNumber)* Pc
          else
            PI_Bens(i,BoxNumberXY)  =   0.0
          end if
          if (i==iiP6) then
            call PhaeocystisCalc_1l(CALC_FOOD_FILTERFEEDER,i,BoxNumberXY,&
                    BoxNumber,puP6Y3(BoxNumberXY),1.0D+00,p_puPI(i))
            R3_Benc(BoxNumberXY)=puP6Y3(BoxNumberXY)*p_puPI(i)*R3c(BoxNumber)
            lcl_PhytoPlankton => PhytoPlankton(i,iiN)
            Pnp=lcl_PhytoPlankton(BoxNumber); 
            r=max(0.0,Pnp-qnPc(i,BoxNumber)*Pc)
            R3_Benn(BoxNumberXY)= puP6Y3(BoxNumberXY)*p_puPI(i)*r
            lcl_PhytoPlankton => PhytoPlankton(i,iiP)
            Pnp=lcl_PhytoPlankton(BoxNumber); 
            r=max(0.0,Pnp-qpPc(i,BoxNumber)*Pc)
            R3_Benp(BoxNumberXY)= puP6Y3(BoxNumberXY)*p_puPI(i)*r
          endif
        else
          PI_Benc(i,BoxNumberXY) = 0.0
          PI_Benn(i,BoxNumberXY) = 0.0 
          PI_Benp(i,BoxNumberXY) = 0.0
          PI_Bens(i,BoxNumberXY) = 0.0
        endif
      end do
      ! if no Phaeocystis is included infood uptake for filterfeeder there is no limitation of filtering
      ! in the presence of Phaeocystis 

      efilP6y3(BoxNumberXY)=1.0;
      if (CalcPhytoPlankton(iiP6)) then
        call PhaeocystisCalc_1l(CALC_LIMIT_FILTERCAP,iiP6,BoxNumberXY,&
          BoxNumberXY,efilP6Y3(BoxNumberXY), PI_Benc(iiP6,BoxNumberXY),p_vum)
      endif
      sediPI_Ben(:,BoxNumberXY)  =  sediPI(:,BoxNumber)    

      ZI_Fc(BoxNumberXY)  =   0.0D+00
      ZI_Fn(BoxNumberXY)  =   0.0D+00
      ZI_Fp(BoxNumberXY)  =   0.0D+00
      ctfZm2c=0.0

      do i = 1 , iiMicroZooPlankton
        if (CalcMicroZooPlankton(i)) then
          lcl_MicroZooPlankton => MicroZooPlankton(i,iiC)
          ZI_Fc(BoxNumberXY)  =   ZI_Fc(BoxNumberXY)+ lcl_MicroZooPlankton(BoxNumber)
          ctfZm2c=ctfPm2c+sum(Depth*lcl_MicroZooPlankton)
          j = ppMicroZooPlankton(i,iiN)
          if ( j> 0) then
           lcl_MicroZooPlankton => MicroZooPlankton(i,iiN)
           ZI_Fn(BoxNumberXY)  =   ZI_Fn(BoxNumberXY)+ lcl_MicroZooPlankton(BoxNumber)
          else
           ZI_Fn(BoxNumberXY)  =   ZI_Fn(BoxNumberXY)+ lcl_MicroZooPlankton(BoxNumber)*p_qnMic(i)
          endif
          j = ppMicroZooPlankton(i,iiP)
          if ( j> 0) then
            lcl_MicroZooPlankton => MicroZooPlankton(i,iiP)
            ZI_Fp(BoxNumberXY)  =   ZI_Fp(BoxNumberXY)+ lcl_MicroZooPlankton(BoxNumber)
          else
            ZI_Fp(BoxNumberXY)  =   ZI_Fp(BoxNumberXY)+ lcl_MicroZooPlankton(BoxNumber)*p_qpMic(i)
          endif
        endif
      enddo

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Compute total detritus conc. used as food for filtereeders
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      RI_Fc(BoxNumberXY)  =   max(0.0,R6c(BoxNumber)-RZc(BoxNumber))
      r=RI_Fc(BoxNumberXY)/(p_small+R6c(BoxNumber))
      ctfRm2c=sum(Depth*max(0.0,R6c-RZc))
      RI_Fn(BoxNumberXY)  =   R6n(BoxNumber)*r
      RI_Fp(BoxNumberXY)  =   R6p(BoxNumber)*r
      RI_Fs(BoxNumberXY)  =   R6s(BoxNumber)*r
      sediR6_Ben(BoxNumberXY) = max(0.0,min(sediR6(BoxNumber),&
            (sediR6(BoxNumber)*R6c(BoxNumber)-sediRZ(BoxNumber)*RZc(BoxNumber))/   &
                (p_small+max(0.0,R6c(BoxNumber)-RZc(BoxNumber)))))

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Calculate total mass of young filterfeeders larvae in pelagic (see FilterFeeder.F90)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      cZ2m2c=sum(Z2c*Depth)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      ! Derive Forcing for benthos
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      !Nutrient Forcing:
      N1p_Ben(BoxNumberXY)  =   max(p_small,N1p(BoxNumber))
      N3n_Ben(BoxNumberXY)  =   max(p_small,N3n(BoxNumber))
      N4n_Ben(BoxNumberXY)  =   max(p_small,N4n(BoxNumber))
      N5s_Ben(BoxNumberXY)  =   max(p_small,N5s(BoxNumber))
      N6r_Ben(BoxNumberXY)  =   max(p_small,N6r(BoxNumber))

      !Oxygen Forcing:
      O2o_Ben(BoxNumberXY)  =   max(p_small,O2o(BoxNumber))

      ! Temperature in the benthos is made equal of the temperature of the
      ! adjacent level (layer) of the pelagic
      ETW_Ben(BoxNumberXY)  =   ETW(BoxNumber)
      ESW_Ben(BoxNumberXY)  =   ESW(BoxNumber)
      ERHO_Ben(BoxNumberXY) =   ERHO(BoxNumber)

#ifdef INCLUDE_BENCO2
      O3c_Ben(BoxNumberXY)  =   O3c(BoxNumber)
      O3h_Ben(BoxNumberXY)  =   O3h(BoxNumber)
      ctO3m2h               =   sum(O3h *Depth)
#endif

      ! depth of the level aboce the sediment
      Depth_Ben(BoxNumberXY)  =   Depth(BoxNumber)

    end DO

  end DO

  end
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
