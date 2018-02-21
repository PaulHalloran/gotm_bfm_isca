!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50-g
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!BOP
!
! !ROUTINE: Initialize
!
! DESCRIPTION
!   Initialization of model
!   Allocation of memory for variables, reading of data files 

!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! !INTERFACE
  SUBROUTINE Initialize
!
! USES:
  use global_mem, only: LOGUNIT
  use mem, only: InitializeModel,ppMicroZooplankton,ppMesoZooPlankton,MicroZooplankton,MesoZooPlankton, &
    iiMicroZooplankton,iiMesoZooPlankton,NO_BOXES,iiN,iiP,qpZc,qnZc,qp_mz,qn_mz
  use mem_Param
  use mem_WindOxReaeration_3
  use mem_PelGlobal
  use mem_PelChem
  use mem_PelBac
  use mem_MesoZoo,p_qnMec=>p_qnc,p_qpMec=>p_qpc
  use mem_MicroZoo,p_qnMic=>p_qnc,p_qpMic=>p_qpc
  use mem_Phyto
  use mem_Phaeo
  use mem_PhotoAvailableRadiation
  use mem_LightAdaptation
  use mem_BenOrganism
  use mem_FilterFeeder
  use mem_BenBac
  use mem_BenNBac
  use mem_Bioturbation
  use mem_BenthicReturn1
  use mem_BenthicReturn2
  use mem_BenthicNutrient3
  use mem_BenAmmonium
  use mem_BenNitrate
  use mem_BenOxygen
  use mem_BenAnoxic
  use mem_BenDenitriDepth
  use mem_BenPhosphate
  use mem_BenSilica
  use mem_BenQ1Transport
  use mem_Settling
  use mem_ControlBenPartNutrientBuffers
  use mem_PelCO2
  use mem_BenCO2Transport
  use mem_BenAlkalinity
  use mem_Silt

!  
!
! !AUTHORS
!   mfstep ERSEM team
!
! !REVISION_HISTORY
!   ---
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
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



      InitializeModel=0
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      ! Allocate Memory for All global variables
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      call AllocateMem

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      ! Read all data files:(namelist files)
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      write(LOGUNIT,*) 'BEGIN of READING PARMETERS'
      call InitParam
      call InitWindOxReaeration_3
      call InitPelGlobal
      call InitPelChem
      call InitPelBac
      call InitMesoZoo
      call InitMicroZoo
      call InitPhyto
      call InitPhaeo
      call InitPhotoAvailableRadiation
      call InitLightAdaptation
      call InitBenOrganism
      call InitFilterFeeder
      call InitBenBac
      call InitBenNBac
      call InitBioturbation
      call InitBenthicReturn1
      call InitBenthicReturn2
      call InitBenthicNutrient3
      call InitBenAmmonium
      call InitBenNitrate
      call InitBenOxygen
      call InitBenAnoxic
      call InitBenDenitriDepth
      call InitBenPhosphate
      call InitBenSilica
      call InitBenQ1Transport
      call InitSettling
      call InitControlBenPartNutrientBuffers
#ifdef INCLUDE_PELCO2
#ifdef INCLUDE_BENCO2
      call InitBenCO2Transport
      call InitBenAlkalinity
#endif
      call InitPelCO2
#endif
      call InitSilt
      write(LOGUNIT,*) 'END of READING PARMETERS'
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      ! Read all other Init* files
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      call InitTransportStateTypes
      call InitBoxParams

      !! intialized fixed quota:

       do i = 1 , ( iiMicroZooPlankton)
         if ( ppMicroZooPlankton(i,iiP) == 0 ) qp_mz(i,:)  =  p_qpMic(i) 
         if ( ppMicroZooPlankton(i,iiN) == 0 ) qn_mz(i,:)  =  p_qnMic(i)
       end do
               

       !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       ! Compute nutrient quota in omnivorous and herbivorous mesozooplankton
       ! in case of fixed quota qp_mz and qn_mz are one time calculated in the Initialize.F90
       !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       do i = 1 , ( iiMesoZooPlankton)
         if ( ppMesoZooPlankton(i,iiP) == 0 ) qpZc(i,:)  =   p_qpMec(i)
         if ( ppMesoZooPlankton(i,iiN) == 0 ) qnZc(i,:)  =   p_qnMec(i)
       end do


    END SUBROUTINE
!BOP
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL  BFM - Biogeochemical Flux Model version 2.50
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
