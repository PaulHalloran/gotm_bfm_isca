!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL
!	   BFM - Biogeochemical Flux Model version 2.50-g
!
! MODULE
!   ModuleMem
!
! FILE
!   ModuleMem
!
! DESCRIPTION
!   Definition of Global Shared Memory
!  
!   This module contains all the structural definitions of the BFM
!   and sets up the memory layout.
!   It is automatically generated from the prototype file 
!   BFM/proto/ModuleMem.proto by including the information from 
!   BFM/General/GlobalDefsBFM.model
!   Do not directly edit this code because changes will be lost at
!   any new compilation.
!
! AUTHORS
!   Piet Ruardij & Marcello Vichi
!
! CHANGE_LOG
!   ---
!
! COPYING
!   
!   Copyright (C) 2006 P. Ruardij, the BFM team
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
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!
!
!
!! IMPORTANT NOTE:
!! Do not change the lines starting with two comment characters "!" 
!! These lines are used by the parser to generate the final module file

!

#include"cppdefs.h"

#include "DEBUG.h"

      module mem
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! Modules can optionally use (import) other modules
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        USE BFM_ERROR_MSG, ONLY: BFM_ERROR
        use global_mem, only:RLEN, ZERO
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! Implicit typing is never allowed
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        implicit none
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! Default all is private
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        private
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! next variable can be used to track errors.
        ! By modifying the value in bio.F90 and introduction of output statements
        ! behind an if-statement on can more easy track error by llimiting
        ! the output only for certain cases........ Example:
        !  if ( track_error ===....) write( LOGUNIT,*)............ 
        integer,public          :: track_error=0

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! State variables Info
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

        !    3d name                                                  description            unit
! ---------- ------------------------------------------------------------ ---------------
!        R9x                                           Suspended sediment           mg/m3
!        O2o                                                       Oxygen      mmol O2/m3
!        N1p                                                    Phosphate       mmol P/m3
!        N3n                                                      Nitrate       mmol N/m3
!        N4n                                                     Ammonium       mmol N/m3
!        O4n                                                 NitrogenSink       mmol N/m3
!        N5s                                                     Silicate      mmol Si/m3
!        N6r                                        Reduction Equivalents     mmol S--/m3
!        B1c                                             Pelagic Bacteria         mg C/m3
!        B1n                                             Pelagic Bacteria       mmol N/m3
!        B1p                                             Pelagic Bacteria       mmol P/m3
!        P1c                                                      Diatoms         mg C/m3
!        P1n                                                      Diatoms       mmol N/m3
!        P1p                                                      Diatoms       mmol P/m3
!        P1l                                                      Diatoms       mg Chl/m3
!        P1s                                                      Diatoms     mmmol Si/m3
!        P2c                                                  Flagellates         mg C/m3
!        P2n                                                  Flagellates       mmol N/m3
!        P2p                                                  Flagellates       mmol P/m3
!        P2l                                                  Flagellates       mg Chl/m3
!        P3c                                            PicoPhytoPlankton         mg C/m3
!        P3n                                            PicoPhytoPlankton       mmol N/m3
!        P3p                                            PicoPhytoPlankton       mmol P/m3
!        P3l                                            PicoPhytoPlankton       mg Chl/m3
!        P4c                                              Dinoflagellates         mg C/m3
!        P4n                                              Dinoflagellates       mmol N/m3
!        P4p                                              Dinoflagellates       mmol P/m3
!        P4l                                              Dinoflagellates       mg Chl/m3
!        P5c                                                Small Diatoms         mg C/m3
!        P5n                                                Small Diatoms       mmol N/m3
!        P5p                                                Small Diatoms       mmol P/m3
!        P5l                                                Small Diatoms       mg Chl/m3
!        P5s                                                Small Diatoms     mmmol Si/m3
!        P6c                                         Phaeocystis colonies         mg C/m3
!        P6n                                         Phaeocystis colonies       mmol N/m3
!        P6p                                         Phaeocystis colonies       mmol P/m3
!        P6l                                         Phaeocystis colonies       mg Chl/m3
!        Pcc                               Phaeo mass initiating colonies          mgC/m3
!        R1c                                  Labile Organic Carbon (LOC)         mg C/m3
!        R1n                                  Labile Organic Carbon (LOC)       mmol N/m3
!        R1p                                  Labile Organic Carbon (LOC)      mmiol P/m3
!        R2c                                            TEP (long sugars)         mg C/m3
!        R3c                                           TEP in Phaeocystis         mg C/m3
!        R6c                             Particulate Organic Carbon (POC)         mg C/m3
!        R6n                             Particulate Organic Carbon (POC)       mmol N/m3
!        R6p                             Particulate Organic Carbon (POC)      mmiol P/m3
!        R6s                             Particulate Organic Carbon (POC)      mmol Si/m3
!        RZc                        FaecelPellet Particulate Carbon (POC)         mg C/m3
!        R7c                                               Refractory DOC         mg C/m3
!        Z3c                                  Carnivorous mesozooplankton         mg C/m3
!        Z4c                                   Omnivorous mesozooplankton         mg C/m3
!        Z2c                                           Filterfeederlarvae         mg C/m3
!        Z5c                                             Microzooplankton         mg C/m3
!        Z6c                         Heterotrophic nanoflagellates (HNAN)         mg C/m3


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

        !    2d name                                                  description            unit
! ---------- ------------------------------------------------------------ ---------------
!        Y1c                                                   Epibenthos         mg C/m2
!        Y1n                                                   Epibenthos       mmol N/m2
!        Y1p                                                   Epibenthos       mmol P/m2
!        Y2c                                              Deposit feeders         mg C/m2
!        Y2n                                              Deposit feeders       mmol N/m2
!        Y2p                                              Deposit feeders       mmol P/m2
!        Y3c                                           Suspension feeders         mg C/m2
!        Y3n                                           Suspension feeders       mmol N/m2
!        Y3p                                           Suspension feeders       mmol P/m2
!        Y4c                                                  Meiobenthos         mg C/m2
!        Y4n                                                  Meiobenthos       mmol N/m2
!        Y4p                                                  Meiobenthos       mmol P/m2
!        Y5c                                            Benthic predators         mg C/m2
!        Y5n                                            Benthic predators       mmol N/m2
!        Y5p                                            Benthic predators       mmol P/m2
!       Yy3c                                 Young Filterfeeders ;mg C/m2                
!        Q6c                                   Particulate organic carbon         mg C/m2
!        Q6n                                   Particulate organic carbon       mmol N/m2
!        Q6p                                   Particulate organic carbon       mmol P/m2
!        Q6s                                   Particulate organic carbon       mmolSi/m2
!        Q1c                                        Labile organic carbon         mg C/m2
!        Q1n                                        Labile organic carbon       mmol N/m2
!        Q1p                                        Labile organic carbon       mmol P/m2
!       Q11c                                        Labile organic carbon         mg C/m2
!       Q11n                                        Labile organic carbon       mmol N/m2
!       Q11p                                        Labile organic carbon       mmol P/m2
!        H1c                                     Aerobic benthic bacteria         mg C/m2
!        H1n                                     Aerobic benthic bacteria       mmol N/m2
!        H1p                                     Aerobic benthic bacteria       mmol P/m2
!        H2c                                   Anaerobic benthic bacteria         mg C/m2
!        H2n                                   Anaerobic benthic bacteria       mmol N/m2
!        H2p                                   Anaerobic benthic bacteria       mmol P/m2
!        H3c                          Aerobic benthic nitrifying bacteria         mg C/m2
!        H3n                          Aerobic benthic nitrifying bacteria       mmol N/m2
!        H3p                          Aerobic benthic nitrifying bacteria       mmol P/m2
!        K1p                                      Phosphate in oxic layer       mmol P/m2
!       K11p                           Phosphate in denitrification layer       mmol P/m2
!       K21p                                    Phosphate in anoxic layer       mmol P/m2
!        K4n                                       Ammonium in oxic layer       mmol N/m2
!       K14n                            Ammonium in denitrification layer       mmol N/m2
!       K24n                                     Ammonium in anoxic layer       mmol N/m2
!        K3n                                         Nitrate in sediments       mmol N/m2
!        K5s                                       Silicate in oxic layer      mmol Si/m2
!       K15s                            Silicate in denitrification layer      mmol Si/m2
!        K6r                          Reduction equivalents in oxic layer      mmolS--/m2
!       K16r               Reduction equivalents in denitrification layer      mmolS--/m2
!       K26r                        Reduction equivalents in anoxic layer      mmolS--/m2
!        G2o                                                   Benthic O2      mmol O2/m2
!        G4n                                  N2 sink for benthic system.       mmol N/m2
!        D1m                                     Oxygen penetration depth               m
!        D2m                                  Denitrification layer depth               m
!        D6m                                  Penetration Depth organic C               m
!        D7m                                  Penetration Depth organic N               m
!        D8m                                  Penetration Depth organic P               m
!        D9m                                 Penetration Depth organic Si               m


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! Definition of arrays which will hold all state variables and other
        ! global variables  used for exchange between submodels and/or output
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    real(RLEN),public,pointer,dimension(:,:) :: D3STATE
    real(RLEN),public,pointer,dimension(:,:,:) :: D3SOURCE
    real(RLEN),public,pointer,dimension(:,:,:) :: D3SINK
    integer,public,pointer,dimension(:) :: D3STATETYPE

    real(RLEN),public,pointer,dimension(:,:) :: D2STATE
    real(RLEN),public,pointer,dimension(:,:,:) :: D2SOURCE
    real(RLEN),public,pointer,dimension(:,:,:) :: D2SINK
    integer,public,pointer,dimension(:) :: D2STATETYPE

    real(RLEN),public,pointer,dimension(:,:) :: PELSURFACE

    real(RLEN),public,pointer,dimension(:,:) :: PELBOTTOM

    integer,public,pointer,dimension(:) :: iiPELSINKREF

    real(RLEN),public,pointer,dimension(:,:) :: D3DIAGNOS

    real(RLEN),public,pointer,dimension(:,:) :: D2DIAGNOS


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! GLOBAL system CONSTANTS
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

        integer,parameter,public  ::NO_D3_BOX_STATES= 54

        integer,parameter,public  ::NO_D2_BOX_STATES= 55

        integer,parameter,public  ::NO_D3_BOX_DIAGNOSS= 179

        integer,parameter,public  ::NO_D2_BOX_DIAGNOSS= 281

        integer,parameter,public  ::NO_D3_BOX_FLUX= 8

        integer,parameter,public  ::NO_D2_BOX_FLUX= 11

        integer,public  ::NO_BOXES
        integer,public  ::NO_BOXES_X
        integer,public  ::NO_BOXES_Y
        integer,public  ::NO_BOXES_Z
        integer,public  ::NO_STATES
        integer,public  ::NO_BOXES_XY
        integer,parameter,public  ::iiPel= 0
        integer,parameter,public  ::iiBen= 1000
        integer,parameter,public  ::iiRESET= -1000

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !! GLOBAL definition of Pelagic (D3) state variables
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    integer,parameter,public :: ppR9x=1, ppO2o=2, ppN1p=3, ppN3n=4, ppN4n=5,&
     ppO4n=6, ppN5s=7, ppN6r=8, ppB1c=9, ppB1n=10, ppB1p=11, ppP1c=12,&
     ppP1n=13, ppP1p=14, ppP1l=15, ppP1s=16, ppP2c=17, ppP2n=18, ppP2p=19,&
     ppP2l=20, ppP3c=21, ppP3n=22, ppP3p=23, ppP3l=24, ppP4c=25, ppP4n=26,&
     ppP4p=27, ppP4l=28, ppP5c=29, ppP5n=30, ppP5p=31, ppP5l=32, ppP5s=33,&
     ppP6c=34, ppP6n=35, ppP6p=36, ppP6l=37, ppPcc=38, ppR1c=39, ppR1n=40,&
     ppR1p=41, ppR2c=42, ppR3c=43, ppR6c=44, ppR6n=45, ppR6p=46, ppR6s=47,&
     ppRZc=48, ppR7c=49, ppZ3c=50, ppZ4c=51, ppZ2c=52, ppZ5c=53, ppZ6c=54,&
     ppP2s=0, ppP3s=0, ppP4s=0, ppP6s=0, ppR1s=0, ppR2n=0, ppR2p=0, ppR2s=0,&
     ppR3n=0, ppR3p=0, ppR3s=0, ppRZn=0, ppRZp=0, ppRZs=0, ppR7n=0, ppR7p=0,&
     ppR7s=0, ppO3c=0, ppO3h=0, ppZ3n=0, ppZ3p=0, ppZ4n=0, ppZ4p=0, ppZ2n=0,&
     ppZ2p=0, ppZ5n=0, ppZ5p=0, ppZ6n=0, ppZ6p=0


    real(RLEN),public,dimension(:),pointer :: R9x, O2o, N1p, N3n, N4n, O4n,&
     N5s, N6r, B1c, B1n, B1p, P1c, P1n, P1p, P1l, P1s, P2c, P2n, P2p, P2l,&
     P3c, P3n, P3p, P3l, P4c, P4n, P4p, P4l, P5c, P5n, P5p, P5l, P5s, P6c,&
     P6n, P6p, P6l, Pcc, R1c, R1n, R1p, R2c, R3c, R6c, R6n, R6p, R6s, RZc,&
     R7c, Z3c, Z4c, Z2c, Z5c, Z6c


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !! GLOBAL definition of Benthic (D2) state variables
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    integer,parameter,public :: ppY1c=1, ppY1n=2, ppY1p=3, ppY2c=4, ppY2n=5,&
     ppY2p=6, ppY3c=7, ppY3n=8, ppY3p=9, ppY4c=10, ppY4n=11, ppY4p=12,&
     ppY5c=13, ppY5n=14, ppY5p=15, ppYy3c=16, ppQ6c=17, ppQ6n=18, ppQ6p=19,&
     ppQ6s=20, ppQ1c=21, ppQ1n=22, ppQ1p=23, ppQ11c=24, ppQ11n=25, ppQ11p=26,&
     ppH1c=27, ppH1n=28, ppH1p=29, ppH2c=30, ppH2n=31, ppH2p=32, ppH3c=33,&
     ppH3n=34, ppH3p=35, ppK1p=36, ppK11p=37, ppK21p=38, ppK4n=39, ppK14n=40,&
     ppK24n=41, ppK3n=42, ppK5s=43, ppK15s=44, ppK6r=45, ppK16r=46,&
     ppK26r=47, ppG2o=48, ppG4n=49, ppD1m=50, ppD2m=51, ppD6m=52, ppD7m=53,&
     ppD8m=54, ppD9m=55, ppG3c=0, ppG3h=0, ppG13c=0, ppG13h=0, ppG23c=0,&
     ppG23h=0


    real(RLEN),public,dimension(:),pointer :: Y1c, Y1n, Y1p, Y2c, Y2n, Y2p,&
     Y3c, Y3n, Y3p, Y4c, Y4n, Y4p, Y5c, Y5n, Y5p, Yy3c, Q6c, Q6n, Q6p,&
     Q6s, Q1c, Q1n, Q1p, Q11c, Q11n, Q11p, H1c, H1n, H1p, H2c, H2n, H2p,&
     H3c, H3n, H3p, K1p, K11p, K21p, K4n, K14n, K24n, K3n, K5s, K15s, K6r,&
     K16r, K26r, G2o, G4n, D1m, D2m, D6m, D7m, D8m, D9m



        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! Constituent parameters:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    integer,parameter,public :: iiC=1, iiN=2, iiP=3, iiL=4, iiS=5, iiR=6,&
     iiO=7, iiM=8


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! Group parameters:
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    integer,parameter,public :: iiPhytoPlankton=6, iiP1=1, iiP2=2, iiP3=3,&
     iiP4=4, iiP5=5, iiP6=6
    integer,parameter,public :: iiPelDetritus=6, iiR1=1, iiR2=2, iiR3=3,&
     iiR6=4, iiRZ=5, iiR7=6
    integer,parameter,public :: iiMesoZooPlankton=3, iiZ3=1, iiZ4=2,&
     iiZ2=3
    integer,parameter,public     :: iiMicroZooPlankton=2, iiZ5=1, iiZ6=2


    integer,parameter,public :: iiBenOrganisms=5, iiY1=1, iiY2=2, iiY3=3,&
     iiY4=4, iiY5=5
    integer,parameter,public     :: iiBenLabileDetritus=2, iiQ1=1, iiQ11=2
    integer,parameter,public     :: iiBenBacteria=3, iiH1=1, iiH2=2, iiH3=3
    integer,parameter,public :: iiBenthicPhosphate=3, iiK1=1, iiK11=2,&
     iiK21=3
    integer,parameter,public :: iiBenthicAmmonium=3, iiK4=1, iiK14=2,&
     iiK24=3


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !  Global Variables
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        integer,public  :: BoxNumber
        integer,public  :: BoxNumberX
        integer,public  :: BoxNumberY
        integer,public  :: BoxNumberZ
        integer,public  :: BoxNumberXY

    real(RLEN),public                                    :: &
     dummy,& !
     LocalDelta,& !
     max_change_per_step,& !
     Wind,& !Wind (m/s)
     SUNQ,& !Daylength in hours (h)
     ThereIsLight      !Forcing for day/night

    integer,public                                    :: &
     InitializeModel,& !
     idummy      !

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !! GLOBAL definition of Pelagic (D3) variables which can be outputted in netcdf
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-

!    3d name                                                  description            unit
! ---------- ------------------------------------------------------------ ---------------
!        ETW                                                  Temperature               C
!        ESW                                                     Salinity             psu
!        EIR                                                   Irradiance            W/m2
!       ERHO                                                      Density            g/m3
!        ESS                                           Suspended sediment           mg/m3
!      cxoO2                      Temperature-dependent oxygen saturation      mmol O2/m3
!       XO2o                                      Net O2 conc. = O2 - H2S      mmol O2/m3
!     eO2mO2                                   Relative Oxygen saturation               %
!       Chla                                                 Chlorophylla       mg Chl/m3
!     Xantho                                                  Xanthophyll       mg Chl/m3
!    flP6R3c                                       flux from Pheao colony       mg C/m3/d
!    flPTN6r                                     anaerobic mineralization   mmol S--/m3/d
!    flN4N3n                                           nitrification flux     mmol N/m3/d
!    flN3O4n                                  denitrification flux (sink)     mmol N/m3/d
!     rumn4B                              Max uptake Ammonium by bacteria    mmmol N/m3/d
!     rumn3B                               Max uptake Nitrate by bacteria    mmmol N/m3/d
!     rumnuB                                  Max uptake urea by bacteria    mmmol N/m3/d
!      rumpB                             Max uptake Phosphate by bacteria    mmmol P/m3/d
!     rumpuB                     Max uptake organic phosphate by bacteria    mmmol N/m3/d
!  lim_rumn4                                      ammonium C-P limitation               -
!  lim_rumn3                                       nitrate C-P limitation               -
!  lim_rumnu                                         ureum C-N limitation               -
!  lim_rump1                                     phosphate C-P limitation               -
!  lim_rumpu                                   phosphatase C-P limitation               -
!  lim_rums5                            silica limitation C-Si limitation               -
!        rml                                              Lysis mortality        mgC/m3/d
!      qpR6c                                             Quotum p/c in R6       mol P/g C
!      qnR6c                                             Quotum n/c in R6       mol N/g C
!      qsR6c                                            Quotum si/c in R6      mol Si/g C
!      qpB1c                                             Quotum p/c in B1       mol P/g C
!      qnB1c                                             Quotum n/c in B1       mol N/g C
!    pMIupZ4           part of food orginating from small food web for Z4               -
!    rnetPTc                            netto production of Phytoplankton       mg C/m3/d
!     flnDIn                             net uptake of dissolved nitrogen      mmolN/m3/d
!     flnDIp                            net uptake of dissolved phosphate      mmolP/m3/d
!     sediR2                                  Detritus sedimentation rate             m/d
!     sediR6                                  Detritus sedimentation rate             m/d
!     sediRZ                              FaecelPellet sedimentation rate             m/d
!        PTi                             index of most common Phyto group               -
!    limnuti                                    nutrient limitation index               -
!       xEPS                                 Total extinction coefficient             1/m
!     xEPS_0                       cdom+background extinction coefficient             1/m
!   xEPS_ESS                                  silt extinction coefficient             1/m
!   xEPS_Chl                                   Chl extinction coefficient             1/m

! sugPI(iiP1)                       Specific Gross Prod. P1(PhytoPlankton)       mgC/mgC/d
! sugPI(iiP2)                       Specific Gross Prod. P2(PhytoPlankton)       mgC/mgC/d
! sugPI(iiP3)                       Specific Gross Prod. P3(PhytoPlankton)       mgC/mgC/d
! sugPI(iiP4)                       Specific Gross Prod. P4(PhytoPlankton)       mgC/mgC/d
! sugPI(iiP5)                       Specific Gross Prod. P5(PhytoPlankton)       mgC/mgC/d
! sugPI(iiP6)                       Specific Gross Prod. P6(PhytoPlankton)       mgC/mgC/d
! sunPI(iiP1)                         Specific Net Prod. P1(PhytoPlankton)       mgC/mgC/d
! sunPI(iiP2)                         Specific Net Prod. P2(PhytoPlankton)       mgC/mgC/d
! sunPI(iiP3)                         Specific Net Prod. P3(PhytoPlankton)       mgC/mgC/d
! sunPI(iiP4)                         Specific Net Prod. P4(PhytoPlankton)       mgC/mgC/d
! sunPI(iiP5)                         Specific Net Prod. P5(PhytoPlankton)       mgC/mgC/d
! sunPI(iiP6)                         Specific Net Prod. P6(PhytoPlankton)       mgC/mgC/d
! qpPc(iiP1)                              Quotum P/C in P1(PhytoPlankton)       mol P/g C
! qpPc(iiP2)                              Quotum P/C in P2(PhytoPlankton)       mol P/g C
! qpPc(iiP3)                              Quotum P/C in P3(PhytoPlankton)       mol P/g C
! qpPc(iiP4)                              Quotum P/C in P4(PhytoPlankton)       mol P/g C
! qpPc(iiP5)                              Quotum P/C in P5(PhytoPlankton)       mol P/g C
! qpPc(iiP6)                              Quotum P/C in P6(PhytoPlankton)       mol P/g C
! qnPc(iiP1)                              Quotum N/C in P1(PhytoPlankton)       mol N/g C
! qnPc(iiP2)                              Quotum N/C in P2(PhytoPlankton)       mol N/g C
! qnPc(iiP3)                              Quotum N/C in P3(PhytoPlankton)       mol N/g C
! qnPc(iiP4)                              Quotum N/C in P4(PhytoPlankton)       mol N/g C
! qnPc(iiP5)                              Quotum N/C in P5(PhytoPlankton)       mol N/g C
! qnPc(iiP6)                              Quotum N/C in P6(PhytoPlankton)       mol N/g C
! qsPc(iiP1)                             Quotum Si/C in P1(PhytoPlankton)      mol Si/g C
! qsPc(iiP2)                             Quotum Si/C in P2(PhytoPlankton)      mol Si/g C
! qsPc(iiP3)                             Quotum Si/C in P3(PhytoPlankton)      mol Si/g C
! qsPc(iiP4)                             Quotum Si/C in P4(PhytoPlankton)      mol Si/g C
! qsPc(iiP5)                             Quotum Si/C in P5(PhytoPlankton)      mol Si/g C
! qsPc(iiP6)                             Quotum Si/C in P6(PhytoPlankton)      mol Si/g C
! qlPc(iiP1)                            Quotum Chl/C in P1(PhytoPlankton)       g Chl/g C
! qlPc(iiP2)                            Quotum Chl/C in P2(PhytoPlankton)       g Chl/g C
! qlPc(iiP3)                            Quotum Chl/C in P3(PhytoPlankton)       g Chl/g C
! qlPc(iiP4)                            Quotum Chl/C in P4(PhytoPlankton)       g Chl/g C
! qlPc(iiP5)                            Quotum Chl/C in P5(PhytoPlankton)       g Chl/g C
! qlPc(iiP6)                            Quotum Chl/C in P6(PhytoPlankton)       g Chl/g C
! qpZc(iiZ3)                               Quotum P/C Z3(MesoZooPlankton)       mol P/g C
! qpZc(iiZ4)                               Quotum P/C Z4(MesoZooPlankton)       mol P/g C
! qpZc(iiZ2)                               Quotum P/C Z2(MesoZooPlankton)       mol P/g C
! qnZc(iiZ3)                               Quotum N/C Z3(MesoZooPlankton)       mol N/g C
! qnZc(iiZ4)                               Quotum N/C Z4(MesoZooPlankton)       mol N/g C
! qnZc(iiZ2)                               Quotum N/C Z2(MesoZooPlankton)       mol N/g C
! qp_mz(iiZ5)                              Quotum P/C Z5(MicroZooPlankton)       mol P/g C
! qp_mz(iiZ6)                              Quotum P/C Z6(MicroZooPlankton)       mol P/g C
! qn_mz(iiZ5)                              Quotum N/C Z5(MicroZooPlankton)       mol N/g C
! qn_mz(iiZ6)                              Quotum N/C Z6(MicroZooPlankton)       mol N/g C
! rumn4(iiP1)                    Max. Ammonium Uptake of P1(PhytoPlankton)    mmmol N/m3/d
! rumn4(iiP2)                    Max. Ammonium Uptake of P2(PhytoPlankton)    mmmol N/m3/d
! rumn4(iiP3)                    Max. Ammonium Uptake of P3(PhytoPlankton)    mmmol N/m3/d
! rumn4(iiP4)                    Max. Ammonium Uptake of P4(PhytoPlankton)    mmmol N/m3/d
! rumn4(iiP5)                    Max. Ammonium Uptake of P5(PhytoPlankton)    mmmol N/m3/d
! rumn4(iiP6)                    Max. Ammonium Uptake of P6(PhytoPlankton)    mmmol N/m3/d
! rumn3(iiP1)                     Max. Nitrate Uptake by P1(PhytoPlankton)    mmmol N/m3/d
! rumn3(iiP2)                     Max. Nitrate Uptake by P2(PhytoPlankton)    mmmol N/m3/d
! rumn3(iiP3)                     Max. Nitrate Uptake by P3(PhytoPlankton)    mmmol N/m3/d
! rumn3(iiP4)                     Max. Nitrate Uptake by P4(PhytoPlankton)    mmmol N/m3/d
! rumn3(iiP5)                     Max. Nitrate Uptake by P5(PhytoPlankton)    mmmol N/m3/d
! rumn3(iiP6)                     Max. Nitrate Uptake by P6(PhytoPlankton)    mmmol N/m3/d
! rumnu(iiP1)                       Max. Ureum Uptake by P1(PhytoPlankton)    mmmol N/m3/d
! rumnu(iiP2)                       Max. Ureum Uptake by P2(PhytoPlankton)    mmmol N/m3/d
! rumnu(iiP3)                       Max. Ureum Uptake by P3(PhytoPlankton)    mmmol N/m3/d
! rumnu(iiP4)                       Max. Ureum Uptake by P4(PhytoPlankton)    mmmol N/m3/d
! rumnu(iiP5)                       Max. Ureum Uptake by P5(PhytoPlankton)    mmmol N/m3/d
! rumnu(iiP6)                       Max. Ureum Uptake by P6(PhytoPlankton)    mmmol N/m3/d
! rump1(iiP1)                   Max. Phosphate Uptake by P1(PhytoPlankton)    mmmol P/m3/d
! rump1(iiP2)                   Max. Phosphate Uptake by P2(PhytoPlankton)    mmmol P/m3/d
! rump1(iiP3)                   Max. Phosphate Uptake by P3(PhytoPlankton)    mmmol P/m3/d
! rump1(iiP4)                   Max. Phosphate Uptake by P4(PhytoPlankton)    mmmol P/m3/d
! rump1(iiP5)                   Max. Phosphate Uptake by P5(PhytoPlankton)    mmmol P/m3/d
! rump1(iiP6)                   Max. Phosphate Uptake by P6(PhytoPlankton)    mmmol P/m3/d
! rumpu(iiP1)                        Max. Phosphatase by P1(PhytoPlankton)    mmmol P/m3/d
! rumpu(iiP2)                        Max. Phosphatase by P2(PhytoPlankton)    mmmol P/m3/d
! rumpu(iiP3)                        Max. Phosphatase by P3(PhytoPlankton)    mmmol P/m3/d
! rumpu(iiP4)                        Max. Phosphatase by P4(PhytoPlankton)    mmmol P/m3/d
! rumpu(iiP5)                        Max. Phosphatase by P5(PhytoPlankton)    mmmol P/m3/d
! rumpu(iiP6)                        Max. Phosphatase by P6(PhytoPlankton)    mmmol P/m3/d
! rums5(iiP1)                     Max. Silicaa Uptake by P1(PhytoPlankton)    mmmol P/m3/d
! rums5(iiP2)                     Max. Silicaa Uptake by P2(PhytoPlankton)    mmmol P/m3/d
! rums5(iiP3)                     Max. Silicaa Uptake by P3(PhytoPlankton)    mmmol P/m3/d
! rums5(iiP4)                     Max. Silicaa Uptake by P4(PhytoPlankton)    mmmol P/m3/d
! rums5(iiP5)                     Max. Silicaa Uptake by P5(PhytoPlankton)    mmmol P/m3/d
! rums5(iiP6)                     Max. Silicaa Uptake by P6(PhytoPlankton)    mmmol P/m3/d
! flPIR1n(iiP1)                                                   flux PI>R1    mmmol N/m3/d
! flPIR1n(iiP2)                                                   flux PI>R1    mmmol N/m3/d
! flPIR1n(iiP3)                                                   flux PI>R1    mmmol N/m3/d
! flPIR1n(iiP4)                                                   flux PI>R1    mmmol N/m3/d
! flPIR1n(iiP5)                                                   flux PI>R1    mmmol N/m3/d
! flPIR1n(iiP6)                                                   flux PI>R1    mmmol N/m3/d
! flPIR1p(iiP1)                                                   flux PI>R1    mmmol P/m3/d
! flPIR1p(iiP2)                                                   flux PI>R1    mmmol P/m3/d
! flPIR1p(iiP3)                                                   flux PI>R1    mmmol P/m3/d
! flPIR1p(iiP4)                                                   flux PI>R1    mmmol P/m3/d
! flPIR1p(iiP5)                                                   flux PI>R1    mmmol P/m3/d
! flPIR1p(iiP6)                                                   flux PI>R1    mmmol P/m3/d
! flPIR6n(iiP1)                                                   flux PI>R6    mmmol N/m3/d
! flPIR6n(iiP2)                                                   flux PI>R6    mmmol N/m3/d
! flPIR6n(iiP3)                                                   flux PI>R6    mmmol N/m3/d
! flPIR6n(iiP4)                                                   flux PI>R6    mmmol N/m3/d
! flPIR6n(iiP5)                                                   flux PI>R6    mmmol N/m3/d
! flPIR6n(iiP6)                                                   flux PI>R6    mmmol N/m3/d
! flPIR6p(iiP1)                                                   flux PI>R6    mmmol P/m3/d
! flPIR6p(iiP2)                                                   flux PI>R6    mmmol P/m3/d
! flPIR6p(iiP3)                                                   flux PI>R6    mmmol P/m3/d
! flPIR6p(iiP4)                                                   flux PI>R6    mmmol P/m3/d
! flPIR6p(iiP5)                                                   flux PI>R6    mmmol P/m3/d
! flPIR6p(iiP6)                                                   flux PI>R6    mmmol P/m3/d
! flPIR6s(iiP1)                                                   flux PI>R6   mmol Si /m3/d
! flPIR6s(iiP2)                                                   flux PI>R6   mmol Si /m3/d
! flPIR6s(iiP3)                                                   flux PI>R6   mmol Si /m3/d
! flPIR6s(iiP4)                                                   flux PI>R6   mmol Si /m3/d
! flPIR6s(iiP5)                                                   flux PI>R6   mmol Si /m3/d
! flPIR6s(iiP6)                                                   flux PI>R6   mmol Si /m3/d
! sediPI(iiP1)                         P1(PhytoPlankton) sedimentation rate             m/d
! sediPI(iiP2)                         P2(PhytoPlankton) sedimentation rate             m/d
! sediPI(iiP3)                         P3(PhytoPlankton) sedimentation rate             m/d
! sediPI(iiP4)                         P4(PhytoPlankton) sedimentation rate             m/d
! sediPI(iiP5)                         P5(PhytoPlankton) sedimentation rate             m/d
! sediPI(iiP6)                         P6(PhytoPlankton) sedimentation rate             m/d
! sediMiZ(iiZ5)                      Z5(MicroZooPlankton) sedimentation rate             m/d
! sediMiZ(iiZ6)                      Z6(MicroZooPlankton) sedimentation rate             m/d
! sediMeZ(iiZ3)                       Z3(MesoZooPlankton) sedimentation rate             m/d
! sediMeZ(iiZ4)                       Z4(MesoZooPlankton) sedimentation rate             m/d
! sediMeZ(iiZ2)                       Z2(MesoZooPlankton) sedimentation rate             m/d
! eiPI(iiP1)             Regulating factor for light in P1(PhytoPlankton)               -
! eiPI(iiP2)             Regulating factor for light in P2(PhytoPlankton)               -
! eiPI(iiP3)             Regulating factor for light in P3(PhytoPlankton)               -
! eiPI(iiP4)             Regulating factor for light in P4(PhytoPlankton)               -
! eiPI(iiP5)             Regulating factor for light in P5(PhytoPlankton)               -
! eiPI(iiP6)             Regulating factor for light in P6(PhytoPlankton)               -
! EPLi(iiP1)                           Optimal light in P1(PhytoPlankton)            W/m2
! EPLi(iiP2)                           Optimal light in P2(PhytoPlankton)            W/m2
! EPLi(iiP3)                           Optimal light in P3(PhytoPlankton)            W/m2
! EPLi(iiP4)                           Optimal light in P4(PhytoPlankton)            W/m2
! EPLi(iiP5)                           Optimal light in P5(PhytoPlankton)            W/m2
! EPLi(iiP6)                           Optimal light in P6(PhytoPlankton)            W/m2

    integer,parameter,public :: ppETW=1, ppESW=2, ppEIR=3,&
     ppERHO=4, ppESS=5, ppcxoO2=6, ppXO2o=7, ppeO2mO2=8,&
     ppChla=9, ppXantho=10, ppflP6R3c=11, ppflPTN6r=12, ppflN4N3n=13,&
     ppflN3O4n=14, pprumn4B=15, pprumn3B=16, pprumnuB=17, pprumpB=18,&
     pprumpuB=19, pplim_rumn4=20, pplim_rumn3=21, pplim_rumnu=22,&
     pplim_rump1=23, pplim_rumpu=24, pplim_rums5=25, pprml=26, ppqpR6c=27,&
     ppqnR6c=28, ppqsR6c=29, ppqpB1c=30, ppqnB1c=31, pppMIupZ4=32,&
     pprnetPTc=33, ppflnDIn=34, ppflnDIp=35, ppsediR2=36, ppsediR6=37,&
     ppsediRZ=38, ppPTi=39, pplimnuti=40, ppxEPS=41, ppxEPS_0=42,&
     ppxEPS_ESS=43, ppxEPS_Chl=44

    integer,public ::&
     ppsugPI(iiPhytoPlankton), ppsunPI(iiPhytoPlankton),&
     ppqpPc(iiPhytoPlankton), ppqnPc(iiPhytoPlankton),&
     ppqsPc(iiPhytoPlankton), ppqlPc(iiPhytoPlankton),&
     ppqpZc(iiMesoZooPlankton), ppqnZc(iiMesoZooPlankton),&
     ppqp_mz(iiMicroZooPlankton), ppqn_mz(iiMicroZooPlankton),&
     pprumn4(iiPhytoPlankton), pprumn3(iiPhytoPlankton),&
     pprumnu(iiPhytoPlankton), pprump1(iiPhytoPlankton),&
     pprumpu(iiPhytoPlankton), pprums5(iiPhytoPlankton),&
     ppflPIR1n(iiPhytoPlankton), ppflPIR1p(iiPhytoPlankton),&
     ppflPIR6n(iiPhytoPlankton), ppflPIR6p(iiPhytoPlankton),&
     ppflPIR6s(iiPhytoPlankton), ppsediPI(iiPhytoPlankton),&
     ppsediMiZ(iiMicroZooPlankton), ppsediMeZ(iiMesoZooPlankton),&
     ppeiPI(iiPhytoPlankton), ppEPLi(iiPhytoPlankton)

    real(RLEN),public,dimension(:),pointer :: ETW, ESW, EIR, ERHO, ESS,&
     cxoO2, XO2o, eO2mO2, Chla, Xantho, flP6R3c, flPTN6r, flN4N3n,&
     flN3O4n, rumn4B, rumn3B, rumnuB, rumpB, rumpuB, lim_rumn4, lim_rumn3,&
     lim_rumnu, lim_rump1, lim_rumpu, lim_rums5, rml, qpR6c, qnR6c, qsR6c,&
     qpB1c, qnB1c, pMIupZ4, rnetPTc, flnDIn, flnDIp, sediR2, sediR6, sediRZ,&
     PTi, limnuti, xEPS, xEPS_0, xEPS_ESS, xEPS_Chl

    real(RLEN),public,dimension(:,:),pointer :: sugPI, sunPI, qpPc, qnPc,&
     qsPc, qlPc, qpZc, qnZc, qp_mz, qn_mz, rumn4, rumn3, rumnu,&
     rump1, rumpu, rums5, flPIR1n, flPIR1p, flPIR6n, flPIR6p, flPIR6s,&
     sediPI, sediMiZ, sediMeZ, eiPI, EPLi


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !!  GLOBAL definition of Benthic (D2) variables which can be outputted in netcdf
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!    2d name                                                  description            unit
! ---------- ------------------------------------------------------------ ---------------
!     EUWIND                                                  EasternWind             m/s
!     EVWIND                                                 SouthernWind             m/s
!      ETAUB                                                 BottomStress             m/s
!      rrBTo                               Total Benthic oxic respiration    mmol O2/m2/d
!      rrATo                             Total Benthic anoxic respiration    mmol O2/m2/d
!      reBTn                               Total Benthic oxic respiration     mmol N/m2/d
!      reBTp                               Total Benthic oxic respiration     mmol P/m2/d
!      reATn                             Total Benthic anoxic respiration     mmol N/m2/d
!      reATp                             Total Benthic anoxic respiration     mmol P/m2/d
!     irrenh                      Enhancement factor due to bioirrigation               -
!     turenh                       Enhancement factor due to bioturbation               -
!   shiftD1m                                        Rate of change of D1m             m/d
!   shiftD2m                                        Rate of change of D2m             m/d
!     jG2K3o                          Oxygen consumption by nitrification    mmol O2/m2/d
!     jG2K7o                      ReOxidation of Red.Equiv. in oxic layer    mmol O2/m2/d
!        M1p                                      phosphate in oxic layer       mmol P/m3
!       M11p                           phosphate in denitrification layer       mmol P/m3
!       M21p                                    phosphate in anoxic layer       mmol P/m3
!        M4n                                       ammonium in oxic layer       mmol N/m3
!       M14n                            ammonium in denitrification layer       mmol N/m3
!       M24n                                     ammonium in anoxic layer       mmol N/m3
!        M3n                                         nitrate in porewater       mmol N/m3
!        M5s                       silicate in oxic+denitrification layer      mmol Si/m3
!        M6r                            reduction equivalent in porewater     mmol S--/m3
!      RI_Fc                               Detritus Food for Filterfeeder         mg C/m3
!      RI_Fn                               Detritus Food for Filterfeeder      mmol N /m3
!      RI_Fp                               Detritus Food for Filterfeeder       mmol N/m3
!      RI_Fs                               Detritus Food for Filterfeeder      mmol Si/m3
!      ZI_Fc                                     Total phytoplankton Food         mg C/m3
!      ZI_Fn                                     Total phytoplankton Food      mmol N /m3
!      ZI_Fp                                     Total phytoplankton Food       mmol N/m3
!      ZI_Fs                                     Total phytoplankton Food      mmol Si/m3
!     jZIY3c                  microzooplankton filtered by filter feeders       mg C/m2/d
!     jRIY3c                          detritus filtered by filter feeders       mg C/m2/d
!     jY3QIc detritus filtered by filter feeders sedimentated on sediment       mg C/m2/d
!     jY3QIs detritus filtered by filter feeders sedimentated on sediment    mmol Si/m2/d
!     jY3RIc                    total detritus excreted by filter feeders       mg C/m2/d
!     jY3RIn                    total detritus excreted by filter feeders     mmol N/m2/d
!     jY3RIp                    total detritus excreted by filter feeders     mmol P/m2/d
!     jY3RIs                    total detritus excreted by filter feeders    mmol Si/m2/d
!     jRIQIc                       R6->pseudofaeces->Q6 by filter feeders       mg C/m2/d
!     jRIQIn                       R6->pseudofaeces->Q6 by filter feeders     mmol N/m2/d
!     jRIQIp                       R6->pseudofaeces->Q6 by filter feeders     mmol P/m2/d
!     jRIQIs                       R6->pseudofaeces->Q6 by filter feeders    mmol Si/m2/d
!     jY3O3c          CO2 production by filterfeeders released to pelagic       mg C/m2/d
!     jO2Y3o                O2 consumption by filterfeeders from  pelagic       mg C/m2/d
!     jY3N4n                 ammonium release by filterfeeders to pelagic     mmol N/m2/d
!     jY3N1p                phosphate release by filterfeeders to pelagic     mmol P/m2/d
!  Depth_Ben                           depth of the  layer above sediment               m
!    ETW_Ben                                                  temperature               C
!   ERHO_Ben                                                      density            g/m3
!    ESW_Ben                                                     salinity             psu
!     puP6Y3 size related availability of PhaeoColonies  for suspension feeders               -
!    R3c_Ben                                                 TEP in Pheao         mg C/m3
!    O2o_Ben                                  oxygen conc. in the pelagic      mmol O2/m3
!    N1p_Ben                               phosphate conc. in the pelagic       mmol P/m3
!    N3n_Ben                                 nitrate conc. in the pelagic       mmol N/m3
!    N4n_Ben                                ammonium conc. in the pelagic       mmol N/m3
!    N5s_Ben                                silicate conc. in the pelagic      mmol Si/m3
!    N6r_Ben                             red. equiv. conc. in the pelagic     mmol S--/m3
! sediR6_Ben                                  Detritus sedimentation rate             m/d
!  flP6R6_Bn                                                flux P6>Y3>R6    mmmol N/m2/d
!  flP6R6_Bp                                                flux P6>Y3>R6    mmmol P/m2/d
!  flP6R1_Bn                                                flux P6>Y3>R1    mmmol N/m2/d
!  flP6R1_Bp                                                flux P6>Y3>R1    mmmol P/m2/d
!  flR3R1_Bc                                                flux R3>Y3>R1        mgC/m2/d
!  flR3R2_Bc                                                flux R3>Y3>R2        mgC/m2/d
!  flR3R6_Bc                                                flux R3>Y3>R6        mgC/m2/d
!   efilP6Y3  suspension-feeder-filtering limitation due to PhaeoColonies               -
!   pyfoodY3                  fraction of filterfeeders available as food               -
!    ctfPm2c                  max. available phyto-food for Filterfeeders         mg C/m2
!    ctfZm2c               max. available microzoo-food for filterfeeders         mg C/m2
!    ctfRm2c                    max. available detritus for filterfeeders         mg C/m2
!     cZ2m2c                                          FilterFeeder Larvae         mg C/m2
!      sK4K3                               first order nitrification rate              /d
!     jK4K3n                                           nitrification flux       mmol/m2/d
!     jK3G4n                                         denitrification flux       mmol/m2/d
!   jK31K21p                  flux of phosphate at lower benthic boundary       mmol/m2/d
!   jK34K24n                   flux of ammonium at lower benthic boundary       mmol/m2/d
!    jK13K3n                    flux of nitrate at lower benthic boundary       mmol/m2/d
!   jK25K15s                   flux of silicate at lower benthic boundary       mmol/m2/d
!   jK36K26r             flux of red.equivalent at lower benthic boundary   mmol S--/m2/d
!    totPELc                                total mass present in pelagic          mgC/m2
!    totPELn                                total mass present in pelagic       mmol N/m2
!    totPELp                                total mass present in pelagic       mmol P/m2
!    totPELs                                total mass present in pelagic      mmol Si/m2
!    totBENc                                total mass present in benthos          mgC/m2
!    totBENn                                total mass present in benthos       mmol N/m2
!    totBENp                                total mass present in benthos       mmol P/m2
!    totBENs                                total mass present in benthos      mmol Si/m2
!    totSYSc                                 total mass present in system          mgC/m2
!    totSYSn                                 total mass present in system       mmol N/m2
!    totSYSp                                 total mass present in system       mmol P/m2
!    totSYSs                                 total mass present in system      mmol Si/m2
! jtotbenpelc                                    total flux from bento pel        mgC/m2/d
! jtotbenpeln                                    total flux from bento pel     mmol N/m2/d
! jtotbenpelp                                    total flux from bento pel     mmol P/m2/d
! jtotbenpels                                    total flux from bento pel    mmol Si/m2/d
!    jupPELc                            total CO2 uptake by Phytoplankton       mg C/m2/d
!   jminPELc                                 total pelagic mineralization       mg C/m2/d
!  totPELInc                                       total pelagic CO2 mass         mg C/m2
!    jupBENc                               total CO2 uptake by Nitrifiers       mg C/m3/d
!   jminBENc                                 total benthic mineralization       mg C/m2/d
!  totBENInc                                       total benthic CO2 mass         mg C/m2
!  jsdoMesoc            auto mortality loss (=grazing) of mesozooplankton       mg C/m2/d
!  jrsMicroc                            rest respiration microzooplankton        mgC/m2/d
!    jnetPTc                                       net primary production       mg C/m2/d
!   jnetMeZc                                     net second. mesoz. prod.        mgC/m2/d
!   jnetMiZc                                     net second. microz prod.        mgC/m2/d
!    jnetB1c                                            net bact. produc.        mgC/m2/d
!    jnetY3c                                     net filterfeeder produc.        mgC/m2/d
!   jnetYy3c                              net filterfeeder larvae produc.        mgC/m2/d
!  jCaCO3Y3c                                            net shell produc.        mgC/m2/d
! Output2d_1                                                  Output_2d_1             any
! Output2d_2                                                  Output_2d_2             any
! Output2d_3                                                  Output_2d_3             any
! Output2d_4                                                  Output_2d_4             any
! jPelFishInput                   PotentialFoodAvailability for Pelagic Fish        mgC/m2/d
! jBenFishInput                   PotentialFoodAvailability for Benthic Fish        mgC/m2/d
!    SdTdzTh                                       dtdZ sum in thermoline               T
!       TMLd                                          Top Mix Layer Depth               m
!       BMLd                                   Bottom Mix Layer Top Depth               m

! ruHI(iiQ1)                              uptake of Q1/Q11 by BenBacteria       mg C/m2/d
! ruHI(iiQ11)                              uptake of Q1/Q11 by BenBacteria       mg C/m2/d
! reHI(iiQ1)                              excretion of Q1/Q11 BenBacteria       mg C/m2/d
! reHI(iiQ11)                              excretion of Q1/Q11 BenBacteria       mg C/m2/d
! rugYIc(iiY1)                           gross food uptake Y1(BenOrganisms)       mg C/m2/d
! rugYIc(iiY2)                           gross food uptake Y2(BenOrganisms)       mg C/m2/d
! rugYIc(iiY3)                           gross food uptake Y3(BenOrganisms)       mg C/m2/d
! rugYIc(iiY4)                           gross food uptake Y4(BenOrganisms)       mg C/m2/d
! rugYIc(iiY5)                           gross food uptake Y5(BenOrganisms)       mg C/m2/d
! jPIY3c(iiP1)                 P1(PhytoPlankton) filtered by filter feeders       mg C/m2/d
! jPIY3c(iiP2)                 P2(PhytoPlankton) filtered by filter feeders       mg C/m2/d
! jPIY3c(iiP3)                 P3(PhytoPlankton) filtered by filter feeders       mg C/m2/d
! jPIY3c(iiP4)                 P4(PhytoPlankton) filtered by filter feeders       mg C/m2/d
! jPIY3c(iiP5)                 P5(PhytoPlankton) filtered by filter feeders       mg C/m2/d
! jPIY3c(iiP6)                 P6(PhytoPlankton) filtered by filter feeders       mg C/m2/d
! PI_Benc(iiP1)                 P1(PhytoPlankton) Forcing for Benthic System         mg C/m3
! PI_Benc(iiP2)                 P2(PhytoPlankton) Forcing for Benthic System         mg C/m3
! PI_Benc(iiP3)                 P3(PhytoPlankton) Forcing for Benthic System         mg C/m3
! PI_Benc(iiP4)                 P4(PhytoPlankton) Forcing for Benthic System         mg C/m3
! PI_Benc(iiP5)                 P5(PhytoPlankton) Forcing for Benthic System         mg C/m3
! PI_Benc(iiP6)                 P6(PhytoPlankton) Forcing for Benthic System         mg C/m3
! PI_Benn(iiP1)                 P1(PhytoPlankton) Forcing for Benthic System      mmol N /m3
! PI_Benn(iiP2)                 P2(PhytoPlankton) Forcing for Benthic System      mmol N /m3
! PI_Benn(iiP3)                 P3(PhytoPlankton) Forcing for Benthic System      mmol N /m3
! PI_Benn(iiP4)                 P4(PhytoPlankton) Forcing for Benthic System      mmol N /m3
! PI_Benn(iiP5)                 P5(PhytoPlankton) Forcing for Benthic System      mmol N /m3
! PI_Benn(iiP6)                 P6(PhytoPlankton) Forcing for Benthic System      mmol N /m3
! PI_Benp(iiP1)                 P1(PhytoPlankton) Forcing for Benthic System       mmol N/m3
! PI_Benp(iiP2)                 P2(PhytoPlankton) Forcing for Benthic System       mmol N/m3
! PI_Benp(iiP3)                 P3(PhytoPlankton) Forcing for Benthic System       mmol N/m3
! PI_Benp(iiP4)                 P4(PhytoPlankton) Forcing for Benthic System       mmol N/m3
! PI_Benp(iiP5)                 P5(PhytoPlankton) Forcing for Benthic System       mmol N/m3
! PI_Benp(iiP6)                 P6(PhytoPlankton) Forcing for Benthic System       mmol N/m3
! PI_Bens(iiP1)                 P1(PhytoPlankton) Forcing for Benthic System      mmol Si/m3
! PI_Bens(iiP2)                 P2(PhytoPlankton) Forcing for Benthic System      mmol Si/m3
! PI_Bens(iiP3)                 P3(PhytoPlankton) Forcing for Benthic System      mmol Si/m3
! PI_Bens(iiP4)                 P4(PhytoPlankton) Forcing for Benthic System      mmol Si/m3
! PI_Bens(iiP5)                 P5(PhytoPlankton) Forcing for Benthic System      mmol Si/m3
! PI_Bens(iiP6)                 P6(PhytoPlankton) Forcing for Benthic System      mmol Si/m3
! puPIY3(iiP1)        P1(PhytoPlankton) availability for suspension feeders               -
! puPIY3(iiP2)        P2(PhytoPlankton) availability for suspension feeders               -
! puPIY3(iiP3)        P3(PhytoPlankton) availability for suspension feeders               -
! puPIY3(iiP4)        P4(PhytoPlankton) availability for suspension feeders               -
! puPIY3(iiP5)        P5(PhytoPlankton) availability for suspension feeders               -
! puPIY3(iiP6)        P6(PhytoPlankton) availability for suspension feeders               -
! sediPI_Ben(iiP1)                         P1(PhytoPlankton) sedimentation rate             m/d
! sediPI_Ben(iiP2)                         P2(PhytoPlankton) sedimentation rate             m/d
! sediPI_Ben(iiP3)                         P3(PhytoPlankton) sedimentation rate             m/d
! sediPI_Ben(iiP4)                         P4(PhytoPlankton) sedimentation rate             m/d
! sediPI_Ben(iiP5)                         P5(PhytoPlankton) sedimentation rate             m/d
! sediPI_Ben(iiP6)                         P6(PhytoPlankton) sedimentation rate             m/d

    integer,parameter,public :: ppEUWIND=1, ppEVWIND=2, ppETAUB=3,&
     pprrBTo=4, pprrATo=5, ppreBTn=6, ppreBTp=7, ppreATn=8,&
     ppreATp=9, ppirrenh=10, ppturenh=11, ppshiftD1m=12, ppshiftD2m=13,&
     ppjG2K3o=14, ppjG2K7o=15, ppM1p=16, ppM11p=17, ppM21p=18,&
     ppM4n=19, ppM14n=20, ppM24n=21, ppM3n=22, ppM5s=23,&
     ppM6r=24, ppRI_Fc=25, ppRI_Fn=26, ppRI_Fp=27,&
     ppRI_Fs=28, ppZI_Fc=29, ppZI_Fn=30, ppZI_Fp=31,&
     ppZI_Fs=32, ppjZIY3c=33, ppjRIY3c=34, ppjY3QIc=35,&
     ppjY3QIs=36, ppjY3RIc=37, ppjY3RIn=38, ppjY3RIp=39,&
     ppjY3RIs=40, ppjRIQIc=41, ppjRIQIn=42, ppjRIQIp=43,&
     ppjRIQIs=44, ppjY3O3c=45, ppjO2Y3o=46, ppjY3N4n=47,&
     ppjY3N1p=48, ppDepth_Ben=49, ppETW_Ben=50, ppERHO_Ben=51,&
     ppESW_Ben=52, pppuP6Y3=53, ppR3c_Ben=54, ppO2o_Ben=55,&
     ppN1p_Ben=56, ppN3n_Ben=57, ppN4n_Ben=58, ppN5s_Ben=59,&
     ppN6r_Ben=60, ppsediR6_Ben=61, ppflP6R6_Bn=62, ppflP6R6_Bp=63,&
     ppflP6R1_Bn=64, ppflP6R1_Bp=65, ppflR3R1_Bc=66, ppflR3R2_Bc=67,&
     ppflR3R6_Bc=68, ppefilP6Y3=69, pppyfoodY3=70, ppctfPm2c=71,&
     ppctfZm2c=72, ppctfRm2c=73, ppcZ2m2c=74, ppsK4K3=75,&
     ppjK4K3n=76, ppjK3G4n=77, ppjK31K21p=78, ppjK34K24n=79,&
     ppjK13K3n=80, ppjK25K15s=81, ppjK36K26r=82, pptotPELc=83,&
     pptotPELn=84, pptotPELp=85, pptotPELs=86, pptotBENc=87,&
     pptotBENn=88, pptotBENp=89, pptotBENs=90, pptotSYSc=91,&
     pptotSYSn=92, pptotSYSp=93, pptotSYSs=94, ppjtotbenpelc=95,&
     ppjtotbenpeln=96, ppjtotbenpelp=97, ppjtotbenpels=98, ppjupPELc=99,&
     ppjminPELc=100, pptotPELInc=101, ppjupBENc=102, ppjminBENc=103,&
     pptotBENInc=104, ppjsdoMesoc=105, ppjrsMicroc=106, ppjnetPTc=107,&
     ppjnetMeZc=108, ppjnetMiZc=109, ppjnetB1c=110, ppjnetY3c=111,&
     ppjnetYy3c=112, ppjCaCO3Y3c=113, ppOutput2d_1=114, ppOutput2d_2=115,&
     ppOutput2d_3=116, ppOutput2d_4=117, ppjPelFishInput=118,&
     ppjBenFishInput=119, ppSdTdzTh=120, ppTMLd=121, ppBMLd=122

    integer,public ::&
     ppruHI(iiBenLabileDetritus), ppreHI(iiBenLabileDetritus),&
     pprugYIc(iiBenOrganisms), ppjPIY3c(iiPhytoPlankton),&
     ppPI_Benc(iiPhytoPlankton), ppPI_Benn(iiPhytoPlankton),&
     ppPI_Benp(iiPhytoPlankton), ppPI_Bens(iiPhytoPlankton),&
     pppuPIY3(iiPhytoPlankton), ppsediPI_Ben(iiPhytoPlankton)

    real(RLEN),public,dimension(:),pointer :: EUWIND, EVWIND, ETAUB, rrBTo,&
     rrATo, reBTn, reBTp, reATn, reATp, irrenh, turenh, shiftD1m,&
     shiftD2m, jG2K3o, jG2K7o, M1p, M11p, M21p, M4n,&
     M14n, M24n, M3n, M5s, M6r, RI_Fc, RI_Fn,&
     RI_Fp, RI_Fs, ZI_Fc, ZI_Fn, ZI_Fp, ZI_Fs, jZIY3c,&
     jRIY3c, jY3QIc, jY3QIs, jY3RIc, jY3RIn, jY3RIp, jY3RIs,&
     jRIQIc, jRIQIn, jRIQIp, jRIQIs, jY3O3c, jO2Y3o, jY3N4n,&
     jY3N1p, Depth_Ben, ETW_Ben, ERHO_Ben, ESW_Ben, puP6Y3, R3c_Ben,&
     O2o_Ben, N1p_Ben, N3n_Ben, N4n_Ben, N5s_Ben, N6r_Ben, sediR6_Ben,&
     flP6R6_Bn, flP6R6_Bp, flP6R1_Bn, flP6R1_Bp, flR3R1_Bc, flR3R2_Bc,&
     flR3R6_Bc, efilP6Y3, pyfoodY3, ctfPm2c, ctfZm2c, ctfRm2c, cZ2m2c,&
     sK4K3, jK4K3n, jK3G4n, jK31K21p, jK34K24n, jK13K3n, jK25K15s,&
     jK36K26r, totPELc, totPELn, totPELp, totPELs, totBENc, totBENn,&
     totBENp, totBENs, totSYSc, totSYSn, totSYSp, totSYSs,&
     jtotbenpelc, jtotbenpeln, jtotbenpelp, jtotbenpels, jupPELc, jminPELc,&
     totPELInc, jupBENc, jminBENc, totBENInc, jsdoMesoc, jrsMicroc,&
     jnetPTc, jnetMeZc, jnetMiZc, jnetB1c, jnetY3c, jnetYy3c,&
     jCaCO3Y3c, Output2d_1, Output2d_2, Output2d_3, Output2d_4,&
     jPelFishInput, jBenFishInput, SdTdzTh, TMLd, BMLd

    real(RLEN),public,dimension(:,:),pointer :: ruHI, reHI, rugYIc, jPIY3c,&
     PI_Benc, PI_Benn, PI_Benp, PI_Bens, puPIY3, sediPI_Ben


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !  boundary fluxes
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    real(RLEN),public,dimension(:),pointer :: jsurR9x, jsurO2o, jsurN1p,&
     jsurN3n, jsurN4n, jsurO4n, jsurN5s, jsurN6r, jsurB1c, jsurB1n, jsurB1p,&
     jsurP1c, jsurP1n, jsurP1p, jsurP1l, jsurP1s, jsurP2c, jsurP2n, jsurP2p,&
     jsurP2l, jsurP3c, jsurP3n, jsurP3p, jsurP3l, jsurP4c, jsurP4n, jsurP4p,&
     jsurP4l, jsurP5c, jsurP5n, jsurP5p, jsurP5l, jsurP5s, jsurP6c, jsurP6n,&
     jsurP6p, jsurP6l, jsurPcc, jsurR1c, jsurR1n, jsurR1p, jsurR2c, jsurR3c,&
     jsurR6c, jsurR6n, jsurR6p, jsurR6s, jsurRZc, jsurR7c, jsurZ3c, jsurZ4c,&
     jsurZ2c, jsurZ5c, jsurZ6c

    real(RLEN),public,dimension(:),pointer :: jbotR9x, jbotO2o, jbotN1p,&
     jbotN3n, jbotN4n, jbotO4n, jbotN5s, jbotN6r, jbotB1c, jbotB1n, jbotB1p,&
     jbotP1c, jbotP1n, jbotP1p, jbotP1l, jbotP1s, jbotP2c, jbotP2n, jbotP2p,&
     jbotP2l, jbotP3c, jbotP3n, jbotP3p, jbotP3l, jbotP4c, jbotP4n, jbotP4p,&
     jbotP4l, jbotP5c, jbotP5n, jbotP5p, jbotP5l, jbotP5s, jbotP6c, jbotP6n,&
     jbotP6p, jbotP6l, jbotPcc, jbotR1c, jbotR1n, jbotR1p, jbotR2c, jbotR3c,&
     jbotR6c, jbotR6n, jbotR6p, jbotR6s, jbotRZc, jbotR7c, jbotZ3c, jbotZ4c,&
     jbotZ2c, jbotZ5c, jbotZ6c


        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !  Other 3d-Global Variables 
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    real(RLEN),public,dimension(:),allocatable           :: &
     OCDepth,& ! Cumulative/Column Depth measured from the surface
     Depth,& !depth of a layer
     ABIO_eps      ! the abiotic extinction coeff. calculated in silt models 



        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !  Other 2d-Global Variables 
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    integer,public,dimension(:),allocatable           :: &
     KPO4,& !
     KPO4_2,& !
     KNH4,& !
     KNO3,& !
     KNO3E,& !
     KRED,& !
     KSIO3,& !
     KSIO3E,& !
     KQ1      !

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !  variables to generate flux_output 
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           real(RLEN), public,dimension(:),allocatable ::flx_t
           integer,    public,dimension(:),allocatable ::flx_SS
           integer,    public,dimension(:),allocatable ::flx_states
           integer,    public,dimension(:),allocatable ::flx_ostates
           integer,    public,dimension(:),allocatable ::flx_calc_nr
           integer,    public,dimension(:),allocatable ::flx_CalcIn
           integer,    public,dimension(:),allocatable ::flx_option
           integer,    public                          ::flx_cal_ben_start
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !! variables to  Track a nutrient throught the model
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! iitrack>=1 : tracking procedures are active!
        integer,public     :: iiTrack=0
        integer,parameter,public     :: ii3dTrack=0
        integer,parameter,public     :: ii3dptTrack=0
        integer,parameter,public     :: ii3daptTrack=0
        integer,public,dimension(:),allocatable ::nr_3d_track
        integer,parameter,public     :: ii2dTrack=0
        integer,parameter,public     :: ii2dptTrack=0
        integer,public,dimension(:),allocatable ::nr_2d_track
        integer,public,dimension(:),allocatable ::flag_3d_track_bot
        integer,public,dimension(:),allocatable ::fix_3d_track_bot
        integer,public,dimension(:),allocatable ::fix_2d_track_bot
        real(RLEN),public,dimension(:),allocatable ::check_3d_track_bot

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        !! SHARED GLOBAL FUNCTIONS (must be below contains)
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        public flux, flux_vector, Source, Source_D3_vector, Source_D2_vector , &  
               fixed_quota_flux_vector,sourcesink_flux_vector,set_for_state_fluxes_zero
    public ppPhytoPlankton, ppPelDetritus, ppMesoZooPlankton,&
     ppMicroZooPlankton, PhytoPlankton, PelDetritus, MesoZooPlankton,&
     MicroZooPlankton

    public ppBenOrganisms, ppBenLabileDetritus, ppBenBacteria,&
     ppBenthicPhosphate, ppBenthicAmmonium, BenOrganisms, BenLabileDetritus,&
     BenBacteria, BenthicPhosphate, BenthicAmmonium


        contains

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          !! Group Pelagic (D3) state functions
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    function ppPhytoPlankton(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppPhytoPlankton
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(6) :: referto=[ppP1c,ppP2c,ppP3c,ppP4c,ppP5c,ppP6c]
     integer,dimension(6) :: const_max=[5,4,4,4,5,4]
     integer,dimension(8) :: constituent_add=[0,1,2,3,4,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppPhytoPlankton=referto(n)+ constituent_add(constituent)
     else
      ppPhytoPlankton=0
     endif

    END function

    function ppPelDetritus(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppPelDetritus
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(6) :: referto=[ppR1c,ppR2c,ppR3c,ppR6c,ppRZc,ppR7c]
     integer,dimension(6) :: const_max=[3,1,1,5,1,1]
     integer,dimension(8) :: constituent_add=[0,1,2,0,3,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppPelDetritus=referto(n)+ constituent_add(constituent)
     else
      ppPelDetritus=0
     endif

    END function

    function ppMesoZooPlankton(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppMesoZooPlankton
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(3) :: referto=[ppZ3c,ppZ4c,ppZ2c]
     integer,dimension(3) :: const_max=[1,1,1]
     integer,dimension(8) :: constituent_add=[0,1,2,0,0,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppMesoZooPlankton=referto(n)+ constituent_add(constituent)
     else
      ppMesoZooPlankton=0
     endif

    END function

    function ppMicroZooPlankton(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppMicroZooPlankton
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(2) :: referto=[ppZ5c,ppZ6c]
     integer,dimension(2) :: const_max=[1,1]
     integer,dimension(8) :: constituent_add=[0,1,2,0,0,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppMicroZooPlankton=referto(n)+ constituent_add(constituent)
     else
      ppMicroZooPlankton=0
     endif

    END function

    function PhytoPlankton(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::PhytoPlankton
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     PhytoPlankton => D3STATE(ppPhytoPlankton(n,constituent),:)

    END function

    function PelDetritus(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::PelDetritus
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     PelDetritus => D3STATE(ppPelDetritus(n,constituent),:)

    END function

    function MesoZooPlankton(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::MesoZooPlankton
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     MesoZooPlankton => D3STATE(ppMesoZooPlankton(n,constituent),:)

    END function

    function MicroZooPlankton(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::MicroZooPlankton
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     MicroZooPlankton => D3STATE(ppMicroZooPlankton(n,constituent),:)

    END function


          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          !! Group Benthic (D2) state functions
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    function ppBenOrganisms(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppBenOrganisms
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(5) :: referto=[ppY1c,ppY2c,ppY3c,ppY4c,ppY5c]
     integer,dimension(5) :: const_max=[3,3,3,3,3]
     integer,dimension(8) :: constituent_add=[0,1,2,0,0,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppBenOrganisms=referto(n)+ constituent_add(constituent)
     else
      ppBenOrganisms=0
     endif

    END function

    function ppBenLabileDetritus(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppBenLabileDetritus
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(2) :: referto=[ppQ1c,ppQ11c]
     integer,dimension(2) :: const_max=[3,3]
     integer,dimension(8) :: constituent_add=[0,1,2,0,0,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppBenLabileDetritus=referto(n)+ constituent_add(constituent)
     else
      ppBenLabileDetritus=0
     endif

    END function

    function ppBenBacteria(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppBenBacteria
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(3) :: referto=[ppH1c,ppH2c,ppH3c]
     integer,dimension(3) :: const_max=[3,3,3]
     integer,dimension(8) :: constituent_add=[0,1,2,0,0,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppBenBacteria=referto(n)+ constituent_add(constituent)
     else
      ppBenBacteria=0
     endif

    END function

    function ppBenthicPhosphate(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppBenthicPhosphate
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(3) :: referto=[ppK1p,ppK11p,ppK21p]
     integer,dimension(3) :: const_max=[3,3,3]
     integer,dimension(8) :: constituent_add=[0,0,0,0,0,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppBenthicPhosphate=referto(n)+ constituent_add(constituent)
     else
      ppBenthicPhosphate=0
     endif

    END function

    function ppBenthicAmmonium(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppBenthicAmmonium
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(3) :: referto=[ppK4n,ppK14n,ppK24n]
     integer,dimension(3) :: const_max=[2,2,2]
     integer,dimension(8) :: constituent_add=[0,0,0,0,0,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppBenthicAmmonium=referto(n)+ constituent_add(constituent)
     else
      ppBenthicAmmonium=0
     endif

    END function

    function BenOrganisms(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::BenOrganisms
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     BenOrganisms => D2STATE(ppBenOrganisms(n,constituent),:)

    END function

    function BenLabileDetritus(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::BenLabileDetritus
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     BenLabileDetritus => D2STATE(ppBenLabileDetritus(n,constituent),:)

    END function

    function BenBacteria(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::BenBacteria
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     BenBacteria => D2STATE(ppBenBacteria(n,constituent),:)

    END function

    function BenthicPhosphate(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::BenthicPhosphate
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     BenthicPhosphate => D2STATE(ppBenthicPhosphate(n,constituent),:)

    END function

    function BenthicAmmonium(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     real(RLEN),dimension(:),pointer ::BenthicAmmonium
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     BenthicAmmonium => D2STATE(ppBenthicAmmonium(n,constituent),:)

    END function



          ! This routine can be used if especially to overcome problems with
          ! very low biomasses. Instead of flux calculations all fofluxes for one stae variable 
          ! are reset on zero.
          ! be aware: use only this routine only, when no fluxes are already
          ! are assigred.
          subroutine set_for_state_fluxes_zero( iiSub,iistate)
            use constants, only: RLEN, ZERO,  SEC_PER_DAY
            use global_mem, only: LOGUNIT
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            ! Implicit typing is never allowed
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            implicit none

            integer,intent(IN) :: iiSub
            integer,intent(IN) :: iistate
          
            if ( iiSub== iiBen) then
              D2SINK(iistate,:,:) =  0.0
              D2SOURCE(:,iistate,:)  = 0.0 
            else
             D3SINK(iistate,:,:)  = 0.0 
             D3SOURCE(:,iistate,:)=0.0  
            endif
          end subroutine set_for_state_fluxes_zero

          subroutine flux_vector(iiSub,origin,destination,flux)

            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

            use constants, only: RLEN, ZERO,  SEC_PER_DAY
            use global_mem, only: LOGUNIT
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            ! Implicit typing is never allowed
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            implicit none

            integer,intent(IN) :: iiSub
            integer,intent(IN) :: origin
            integer,intent(IN) :: destination
            real(RLEN),intent(IN) :: flux(:)

            integer :: i
            character(len=8) :: D23

            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            !BEGIN compute
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

            TESTNANVECTOR(flux,iiSub,origin,destination)
            CHECKFLUX(-1,iiSub,origin,destination)

            if ( origin /= destination )  then
              if ( minval(flux) < ZERO) then
                D23="Pelagic"
                if ( iiSub == iiBen) D23="Benthic"
                do i=1,size(flux)
                  if (flux(i)< 0.0D+00)  then
                    write(LOGUNIT,'(''Error flux_vector: negative flux at level:'',I4)') i
                    write(LOGUNIT,'(''In '',A,'':origin='',i4,'' destination='',i4)') &
                      D23, origin,destination
                    write(LOGUNIT,'(''In '',A,'':origin='',i4,'' &
                      destination='',i4)') D23, origin,destination
                    write(LOGUNIT,'(''flux='',(G16.8))') flux(i)
                    if ( iiSub== iiBen) then
                      write(LOGUNIT,*) "state value origin:",D2STATE(origin,i)
                      write(LOGUNIT,*) "state value destination:",D2STATE(destination,i)
                    else
                      write(LOGUNIT,*) "state value origin:",D3STATE(origin,i)
                      write(LOGUNIT,*) "state value destination:",D3STATE(destination,i)
                    endif
                  endif
                enddo
                call BFM_ERROR("flux_vector","negative flux")
              endif ! minval<0
              select case ( iiSub )
                case (iiPel)
                  D3SINK(origin,destination,:)  =  flux/SEC_PER_DAY
                  D3SOURCE(destination,origin,:)=  flux/SEC_PER_DAY
                case (iiBen)
                  D2SINK(origin,destination,:) =  flux/SEC_PER_DAY
                  D2SOURCE(destination,origin,:)   = flux/SEC_PER_DAY
              end select
            else
              select case ( iiSub )
                case (iiPel)
                  where (flux > 0.0D+00 )
                    D3SOURCE(origin,destination,:) =D3SOURCE(origin,destination,:) &
                      + flux/SEC_PER_DAY
                  elsewhere
                    D3SINK(destination,origin,:) =D3SINK(destination,origin,:) - &
                      flux/SEC_PER_DAY
                  end where
                case (iiBen)
                  where (flux > 0.0D+00 )
                    D2SOURCE(destination,origin,:) =D2SOURCE(destination,origin,:) &
                      + flux/SEC_PER_DAY
                  elsewhere
                    D2SINK(origin,destination,:) =D2SINK(origin,destination,:) - &
                      flux/SEC_PER_DAY
                  end where
              end select
            endif !origin <> destination

            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            !END compute
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

            return
          end subroutine flux_vector

          subroutine testnan_vector(array,iiSub,origin,destination)
          use global_mem, only: LOGUNIT

            real(RLEN),intent(IN)    :: array(:)
            integer,intent(IN) :: iiSub
            integer,intent(IN) :: origin
            integer,intent(IN) :: destination
            integer:: i=0
            do i=1,size(array)
              if (isnan(array(i))== .true. ) then
                write(LOGUNIT,'(''at level:'',I4)') i
                write(LOGUNIT,'(''origin='',i4,'' destination='',i4)') &
                  origin,destination
                if ( iiSub== iiBen) then
                    write(LOGUNIT,*) "state value origin:",D2STATE(origin,i)
                    write(LOGUNIT,*) "state value destination:",D2STATE(destination,i)
                else
                    write(LOGUNIT,*) "state value origin:",D3STATE(origin,i)
                    write(LOGUNIT,*) "state value destination:",D3STATE(destination,i)
                endif
                STDERR 'Nan value in flux'
                stop 1002
              endif
            enddo
          end subroutine testnan_vector

          subroutine testnan(scalar,grid_nr,iiSub,origin,destination)
          use global_mem, only: LOGUNIT

            real(RLEN),intent(IN)    :: scalar
            integer,intent(IN) :: grid_nr
            integer,intent(IN) :: iiSub
            integer,intent(IN) :: origin
            integer,intent(IN) :: destination
            if (isnan(scalar)== .true. ) then
            write(LOGUNIT,'(''origin='',i4,'' destination='',i4)') origin,destination
            if ( iiSub == iiBen)  then
                 write(LOGUNIT,*) "state value origin:",D2STATE(origin,grid_nr)
                 write(LOGUNIT,*) "state value destination:",D2STATE(destination,grid_nr)
              else 
                 write(LOGUNIT,*) "state value origin:",D3STATE(origin,grid_nr)
                 write(LOGUNIT,*) "state value destination:",D3STATE(destination,grid_nr)
            endif
            write(LOGUNIT,*) 'Nan value in scalar flux'
            stop 1003
          endif
        end subroutine testnan
        subroutine flux(grid_nr,iiSub,origin,destination,flow,error)

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

          use constants, only: RLEN, ZERO, SEC_PER_DAY
          use global_mem, only: LOGUNIT
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          ! Implicit typing is never allowed
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          implicit none

          integer,intent(IN)                 :: grid_nr
          integer,intent(IN)                 :: iiSub
          integer,intent(IN)                 :: origin
          integer,intent(IN)                 :: destination
          real(RLEN),intent(IN)              :: flow
          integer,intent(INOUT),optional     :: error

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          character(len=8)    :: D23
          !BEGIN compute
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

          TESTNAN(flow,grid_nr,iiSub,origin,destination)
          CHECKFLUX(grid_nr,iiSub,origin,destination)

          if ( origin /= destination ) then
            if ( flow < ZERO) then
              D23="Pelagic"
              if ( iiSub == iiBen) D23="Benthic"
              write(LOGUNIT,'(''In '',A,'':origin='',i4,'' destination='',i4)') &
                D23, origin,destination
              write(LOGUNIT,*) "Error in (scalar) vector  function: negative flux!"
              write(LOGUNIT,*) "origin,destination:", origin,destination
              write(LOGUNIT,*) flow
              if ( iiSub == iiBen)  then
                 write(LOGUNIT,*) "state value origin:",D2STATE(origin,grid_nr)
                 write(LOGUNIT,*) "state value destination:",D2STATE(destination,grid_nr)
              else 
                 write(LOGUNIT,*) "state value origin:",D3STATE(origin,grid_nr)
                 write(LOGUNIT,*) "state value destination:",D3STATE(destination,grid_nr)
              endif
              STDERR "Error in (scalar)flux function:negative flux !"
              call BFM_ERROR("flux","negative flux")
              if ( present(error)) error=1
            endif ! flow<0
            select case ( iiSub )
              case (iiPel)
                D3SINK(origin,destination,grid_nr)=  flow/SEC_PER_DAY
                D3SOURCE(destination,origin,grid_nr)= flow/SEC_PER_DAY
              case (iiBen)
                D2SINK(origin,destination,grid_nr)=  flow/SEC_PER_DAY
                D2SOURCE(destination,origin,grid_nr)= flow/SEC_PER_DAY
            end select
          else
            select case ( iiSub )
              case (iiPel)
                if (flow > 0.0 ) then
                  D3SOURCE(destination,origin,grid_nr)= D3SOURCE(destination,origin,grid_nr) &
                    +flow/SEC_PER_DAY
                else
                  D3SINK(origin,destination,grid_nr)= D3SINK(origin,destination,grid_nr) &
                    -flow/SEC_PER_DAY
                endif
              case (iiBen)
                if (flow > 0.0 ) then
                  D2SOURCE(destination,origin,grid_nr)= D2SOURCE(destination,origin,grid_nr) &
                    +flow/SEC_PER_DAY
                else
                  D2SINK(origin,destination,grid_nr)= D2SINK(origin,destination,grid_nr) &
                    -flow/SEC_PER_DAY
                endif
            end select
          endif
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          !END compute
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

          return
        end subroutine flux

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! vector function to get actual rate of change in the pelagic
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

        function Source_D3_vector(iistate)
          use constants, only: RLEN, ZERO, SEC_PER_DAY

          implicit none

          integer, intent(IN) ::iistate
          real(RLEN) ::Source_D3_vector(size(D3SOURCE,DIM=3))
          ! Array in sum is by sum seen as 2D-array: DIM=1 and NOT 2
          Source_D3_vector=(sum(D3SOURCE(iistate,:,:),DIM=1)- sum(D3SINK(iistate,:,:),DIM=1))*SEC_PER_DAY
        end function Source_D3_vector

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! vector function to get actual rate of change in the benthic
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

        function Source_D2_vector(iistate)
          use constants, only: RLEN, ZERO, SEC_PER_DAY

          implicit none

          integer, intent(IN) ::iistate
          real(RLEN) ::Source_D2_vector(size(D2SOURCE,DIM=3))
          ! Array in sum is by sum seen as 2D-array: DIM=1 and NOT 2
          Source_D2_vector=(sum(D2SOURCE(iistate,:,:),DIM=1)- sum(D2SINK(iistate,:,:),DIM=1))*SEC_PER_DAY
        end function Source_D2_vector
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! function to get actual rate of change
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

        function source(iiSub,iibox,iistate)
          use constants, only: RLEN, ZERO, SEC_PER_DAY

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          ! Implicit typing is never allowed
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          implicit none

          real(RLEN)  ::Source
          integer, intent(IN)  ::iiSub
          integer, intent(IN)  ::iibox
          integer, intent(IN)  ::iistate
          if ( iiSub == iiPel )  then
            Source = (sum(D3SOURCE(iistate,:,iibox))- &
              sum(D3SINK(iistate,:,iibox)))*SEC_PER_DAY
          elseif ( iiSub == iiBen )  then
            Source = (sum(D2SOURCE(iistate,:,iibox))- &
              sum(D2SINK(iistate,:,iibox)))*SEC_PER_DAY
          endif
        end function source

        subroutine unicflux(grid_nr,iiSub,origin,destination)
        use constants, only: RLEN

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

          use global_mem, only: LOGUNIT
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          ! Implicit typing is never allowed
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          implicit none

          integer,intent(IN)    :: grid_nr
          integer,intent(IN)    :: origin
          integer,intent(IN)    :: iiSub
          integer,intent(IN)    :: destination

          real(RLEN) :: tot
          character(len=20):: type

          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
          !BEGIN compute
          !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


          select case ( iiSub )
            case (iiPel)
              type="D3"
              if ( grid_nr <=0  ) then
                tot=sum(D3SINK(origin,destination,:))
              else
                tot=D3SINK(origin,destination,grid_nr)
              endif
            case (iiBen)
              type="D2"
              if ( grid_nr <=0  ) then
                tot=sum(D2SINK(origin,destination,:))
              else
                tot=D2SINK(origin,destination,grid_nr)
              endif
            case (iiReset)
              D3SINK(:,:,:)=0.0D+00
              D2SINK(:,:,:)=0.0D+00
              return
          end select
          if ( tot > 0.0D+00  ) then
            write(LOGUNIT,'(''Double defintion '',A2,''-flux'')')type
            write(LOGUNIT,'(''origin:'',I3,'' destination:'',I3)') origin, destination
            if ( origin /= destination ) then
              STDERR 'double definition of fluxes'
              stop 1006
            endif
          endif
        !END compute
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

        return
      end subroutine unicflux

      function fixed_quota_flux_vector(mode,iiSub,which,origin, &
                                          destination,flux,collect,name_routine)

      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

      use global_mem, only: LOGUNIT
      use constants, only: RLEN, LOGUNIT
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      ! Implicit typing is never allowed
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      implicit none

      integer            :: fixed_quota_flux_vector
      integer,intent(IN) :: mode
      integer,intent(IN) :: iiSub
      integer,intent(IN) :: which
      integer,intent(IN) :: origin
      integer,intent(IN) :: destination
      real(RLEN),intent(IN),dimension(:) :: flux
      real(RLEN),intent(INOUT),dimension(:) :: collect
      character(len=*),optional	            :: name_routine

      character(len=32)	                    :: h
      real(RLEN)                            :: r

      fixed_quota_flux_vector=0
      if ( origin> 0 .and.destination >0) then
         call flux_vector(iiSub,origin, destination,flux)
      else if ( origin > 0 ) then
         call flux_vector(iiSub,origin, origin,-flux)
      elseif ( destination > 0 ) then
         call flux_vector(iiSub,destination, destination,flux)
      elseif (iiSub < 0 ) then
         if ( mode==0)  return
         if ( sum(abs(flux)/(1.0D-80+abs(collect))-1.0D+00)> 1.0D-6) then
           !Check if we have to do with small numbers
           !If this is the case do not check!
           r=abs(sum(flux)) 
           if ( r> (1.0D-80 -r+abs(sum(collect))/1.0D-6 )) then
              h=''; if ( present(name_routine)) h=' in '//name_routine
              if ( iiSub==-iiN) then
                write(LOGUNIT,'(''Warning'',A,'': N:C quotumn not fixed'')'),trim(h)
                fixed_quota_flux_vector=1
              elseif (iiSub==-iiP) then
                write(LOGUNIT,'(''Warning'',A,'': P:C quotumn not fixed'')'),trim(h)
                fixed_quota_flux_vector=1
              endif
              return
           endif
         endif
      endif      
      if ( mode==1 ) then
        if ( (which == origin) .and.(origin.ne.destination)) then
           collect=collect-flux
        else
           collect=collect+flux
        endif
      endif
      end function fixed_quota_flux_vector

      subroutine sourcesink_flux_vector(iiSub,origin,destination,flux)
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            ! Implicit typing is never allowed
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            implicit none

            integer,intent(IN) :: iiSub
            integer,intent(IN) :: origin
            integer,intent(IN) :: destination
            real(RLEN),intent(IN) :: flux(:)

            if ( destination ==0 ) then
              call flux_vector(iiSub,origin,origin,-flux)
            elseif ( origin ==0 ) then
              call flux_vector(iiSub,destination,destination,flux)
            else
             call  flux_vector(iiSub,origin,destination,flux)
            endif
      end subroutine sourcesink_flux_vector


    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    ! end of contain section
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  end module mem

