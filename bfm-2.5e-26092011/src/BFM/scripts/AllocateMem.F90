!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
! MODEL
!	   BFM - Biogeochemical Flux Model version 2.50-g
!
! SUBROUTINE
!   AllocateMem
!
! FILE
!   AllocateMem
!
! DESCRIPTION
!   Allocation of memory for Global State variables and other Variables
!  
!   This file is generated directly from OpenSesame model code, using a code 
!   generator which transposes from the sesame meta language into F90.
!   F90 code generator written by P. Ruardij.
!   structure of the code based on ideas of M. Vichi.
!
! AUTHORS
!   mfstep/ERSEM team
!
! CHANGE_LOG
!   ---
!
! COPYING
!   
!   Copyright (C) 2004 P. Ruardij, the mfstep group, the ERSEM team 
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
  subroutine AllocateMem
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !  use (import) other modules
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  use global_mem
  use mem
  use mem_Param
  use bio_bfm, only: calc_sigma_depth
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Implicit typing is never allowed
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  IMPLICIT NONE
  integer:: status
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  !Start the allocation of pelagic state global
  ! matrix and pointers
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  write(LOGUNIT,*) "#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-"
  write(LOGUNIT,*) "# Allocating State Variables and Rates array ..."

#ifndef NOT_STANDALONE
 
     allocate(D3STATE(1:NO_D3_BOX_STATES,1:NO_BOXES),stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem", "D3STATE")
     D3STATE = ZERO
     allocate(D3SOURCE(1:NO_D3_BOX_STATES,1:NO_D3_BOX_STATES,1:NO_BOXES),stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem", "D3SOURCE")
     D3SOURCE = ZERO
     allocate(D3SINK(1:NO_D3_BOX_STATES,1:NO_D3_BOX_STATES,1:NO_BOXES)&
      ,stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem", "D3SINK")
     D3SINK = ZERO
     allocate(D3STATETYPE(1:NO_D3_BOX_STATES ),stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem","D3STATETYPE")
     D3STATETYPE = ZERO

 
     allocate(D2STATE(1:NO_D2_BOX_STATES,1:NO_BOXES_XY),stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem", "D2STATE")
     D2STATE = ZERO
     allocate(D2SOURCE(1:NO_D2_BOX_STATES,1:NO_D2_BOX_STATES,1:NO_BOXES_XY),stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem", "D2SOURCE")
     D2SOURCE = ZERO
     allocate(D2SINK(1:NO_D2_BOX_STATES,1:NO_D2_BOX_STATES,1:NO_BOXES_XY)&
      ,stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem", "D2SINK")
     D2SINK = ZERO
     allocate(D2STATETYPE(1:NO_D2_BOX_STATES ),stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem","D2STATETYPE")
     D2STATETYPE = ZERO

 
     allocate(D3DIAGNOS(1:NO_D3_BOX_DIAGNOSS,1:NO_BOXES),stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem", "D3DIAGNOS")
     D3DIAGNOS = ZERO

 
     allocate(D2DIAGNOS(1:NO_D2_BOX_DIAGNOSS,1:NO_BOXES_XY),stat=status)
     if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem", "D2DIAGNOS")
     D2DIAGNOS = ZERO

#endif




  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Allocation of Pelagic variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    R9x => D3STATE(ppR9x,:); R9x=ZERO
    O2o => D3STATE(ppO2o,:); O2o=ZERO
    N1p => D3STATE(ppN1p,:); N1p=ZERO
    N3n => D3STATE(ppN3n,:); N3n=ZERO
    N4n => D3STATE(ppN4n,:); N4n=ZERO
    O4n => D3STATE(ppO4n,:); O4n=ZERO
    N5s => D3STATE(ppN5s,:); N5s=ZERO
    N6r => D3STATE(ppN6r,:); N6r=ZERO
    B1c => D3STATE(ppB1c,:); B1c=ZERO
    B1n => D3STATE(ppB1n,:); B1n=ZERO
    B1p => D3STATE(ppB1p,:); B1p=ZERO
    P1c => D3STATE(ppP1c,:); P1c=ZERO
    P1n => D3STATE(ppP1n,:); P1n=ZERO
    P1p => D3STATE(ppP1p,:); P1p=ZERO
    P1l => D3STATE(ppP1l,:); P1l=ZERO
    P1s => D3STATE(ppP1s,:); P1s=ZERO
    P2c => D3STATE(ppP2c,:); P2c=ZERO
    P2n => D3STATE(ppP2n,:); P2n=ZERO
    P2p => D3STATE(ppP2p,:); P2p=ZERO
    P2l => D3STATE(ppP2l,:); P2l=ZERO
    P3c => D3STATE(ppP3c,:); P3c=ZERO
    P3n => D3STATE(ppP3n,:); P3n=ZERO
    P3p => D3STATE(ppP3p,:); P3p=ZERO
    P3l => D3STATE(ppP3l,:); P3l=ZERO
    P4c => D3STATE(ppP4c,:); P4c=ZERO
    P4n => D3STATE(ppP4n,:); P4n=ZERO
    P4p => D3STATE(ppP4p,:); P4p=ZERO
    P4l => D3STATE(ppP4l,:); P4l=ZERO
    P5c => D3STATE(ppP5c,:); P5c=ZERO
    P5n => D3STATE(ppP5n,:); P5n=ZERO
    P5p => D3STATE(ppP5p,:); P5p=ZERO
    P5l => D3STATE(ppP5l,:); P5l=ZERO
    P5s => D3STATE(ppP5s,:); P5s=ZERO
    P6c => D3STATE(ppP6c,:); P6c=ZERO
    P6n => D3STATE(ppP6n,:); P6n=ZERO
    P6p => D3STATE(ppP6p,:); P6p=ZERO
    P6l => D3STATE(ppP6l,:); P6l=ZERO
    Pcc => D3STATE(ppPcc,:); Pcc=ZERO
    R1c => D3STATE(ppR1c,:); R1c=ZERO
    R1n => D3STATE(ppR1n,:); R1n=ZERO
    R1p => D3STATE(ppR1p,:); R1p=ZERO
    R2c => D3STATE(ppR2c,:); R2c=ZERO
    R3c => D3STATE(ppR3c,:); R3c=ZERO
    R6c => D3STATE(ppR6c,:); R6c=ZERO
    R6n => D3STATE(ppR6n,:); R6n=ZERO
    R6p => D3STATE(ppR6p,:); R6p=ZERO
    R6s => D3STATE(ppR6s,:); R6s=ZERO
    RZc => D3STATE(ppRZc,:); RZc=ZERO
    R7c => D3STATE(ppR7c,:); R7c=ZERO
    Z3c => D3STATE(ppZ3c,:); Z3c=ZERO
    Z4c => D3STATE(ppZ4c,:); Z4c=ZERO
    Z2c => D3STATE(ppZ2c,:); Z2c=ZERO
    Z5c => D3STATE(ppZ5c,:); Z5c=ZERO
    Z6c => D3STATE(ppZ6c,:); Z6c=ZERO


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Allocation of Benthic variables
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    Y1c => D2STATE(ppY1c,:); Y1c=ZERO
    Y1n => D2STATE(ppY1n,:); Y1n=ZERO
    Y1p => D2STATE(ppY1p,:); Y1p=ZERO
    Y2c => D2STATE(ppY2c,:); Y2c=ZERO
    Y2n => D2STATE(ppY2n,:); Y2n=ZERO
    Y2p => D2STATE(ppY2p,:); Y2p=ZERO
    Y3c => D2STATE(ppY3c,:); Y3c=ZERO
    Y3n => D2STATE(ppY3n,:); Y3n=ZERO
    Y3p => D2STATE(ppY3p,:); Y3p=ZERO
    Y4c => D2STATE(ppY4c,:); Y4c=ZERO
    Y4n => D2STATE(ppY4n,:); Y4n=ZERO
    Y4p => D2STATE(ppY4p,:); Y4p=ZERO
    Y5c => D2STATE(ppY5c,:); Y5c=ZERO
    Y5n => D2STATE(ppY5n,:); Y5n=ZERO
    Y5p => D2STATE(ppY5p,:); Y5p=ZERO
    Yy3c => D2STATE(ppYy3c,:); Yy3c=ZERO
    Q6c => D2STATE(ppQ6c,:); Q6c=ZERO
    Q6n => D2STATE(ppQ6n,:); Q6n=ZERO
    Q6p => D2STATE(ppQ6p,:); Q6p=ZERO
    Q6s => D2STATE(ppQ6s,:); Q6s=ZERO
    Q1c => D2STATE(ppQ1c,:); Q1c=ZERO
    Q1n => D2STATE(ppQ1n,:); Q1n=ZERO
    Q1p => D2STATE(ppQ1p,:); Q1p=ZERO
    Q11c => D2STATE(ppQ11c,:); Q11c=ZERO
    Q11n => D2STATE(ppQ11n,:); Q11n=ZERO
    Q11p => D2STATE(ppQ11p,:); Q11p=ZERO
    H1c => D2STATE(ppH1c,:); H1c=ZERO
    H1n => D2STATE(ppH1n,:); H1n=ZERO
    H1p => D2STATE(ppH1p,:); H1p=ZERO
    H2c => D2STATE(ppH2c,:); H2c=ZERO
    H2n => D2STATE(ppH2n,:); H2n=ZERO
    H2p => D2STATE(ppH2p,:); H2p=ZERO
    H3c => D2STATE(ppH3c,:); H3c=ZERO
    H3n => D2STATE(ppH3n,:); H3n=ZERO
    H3p => D2STATE(ppH3p,:); H3p=ZERO
    K1p => D2STATE(ppK1p,:); K1p=ZERO
    K11p => D2STATE(ppK11p,:); K11p=ZERO
    K21p => D2STATE(ppK21p,:); K21p=ZERO
    K4n => D2STATE(ppK4n,:); K4n=ZERO
    K14n => D2STATE(ppK14n,:); K14n=ZERO
    K24n => D2STATE(ppK24n,:); K24n=ZERO
    K3n => D2STATE(ppK3n,:); K3n=ZERO
    K5s => D2STATE(ppK5s,:); K5s=ZERO
    K15s => D2STATE(ppK15s,:); K15s=ZERO
    K6r => D2STATE(ppK6r,:); K6r=ZERO
    K16r => D2STATE(ppK16r,:); K16r=ZERO
    K26r => D2STATE(ppK26r,:); K26r=ZERO
    G2o => D2STATE(ppG2o,:); G2o=ZERO
    G4n => D2STATE(ppG4n,:); G4n=ZERO
    D1m => D2STATE(ppD1m,:); D1m=ZERO
    D2m => D2STATE(ppD2m,:); D2m=ZERO
    D6m => D2STATE(ppD6m,:); D6m=ZERO
    D7m => D2STATE(ppD7m,:); D7m=ZERO
    D8m => D2STATE(ppD8m,:); D8m=ZERO
    D9m => D2STATE(ppD9m,:); D9m=ZERO



  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Start the allocation of other pelagic variables which can be outputted
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  write(LOGUNIT,*) "#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-"
  write(LOGUNIT,*) "# Allocating Other Global Variables .."

    ppsugPI(iiP1)=45
    ppsugPI(iiP2)=46
    ppsugPI(iiP3)=47
    ppsugPI(iiP4)=48
    ppsugPI(iiP5)=49
    ppsugPI(iiP6)=50
    ppsunPI(iiP1)=51
    ppsunPI(iiP2)=52
    ppsunPI(iiP3)=53
    ppsunPI(iiP4)=54
    ppsunPI(iiP5)=55
    ppsunPI(iiP6)=56
    ppqpPc(iiP1)=57
    ppqpPc(iiP2)=58
    ppqpPc(iiP3)=59
    ppqpPc(iiP4)=60
    ppqpPc(iiP5)=61
    ppqpPc(iiP6)=62
    ppqnPc(iiP1)=63
    ppqnPc(iiP2)=64
    ppqnPc(iiP3)=65
    ppqnPc(iiP4)=66
    ppqnPc(iiP5)=67
    ppqnPc(iiP6)=68
    ppqsPc(iiP1)=69
    ppqsPc(iiP2)=70
    ppqsPc(iiP3)=71
    ppqsPc(iiP4)=72
    ppqsPc(iiP5)=73
    ppqsPc(iiP6)=74
    ppqlPc(iiP1)=75
    ppqlPc(iiP2)=76
    ppqlPc(iiP3)=77
    ppqlPc(iiP4)=78
    ppqlPc(iiP5)=79
    ppqlPc(iiP6)=80
    ppqpZc(iiZ3)=81
    ppqpZc(iiZ4)=82
    ppqpZc(iiZ2)=83
    ppqnZc(iiZ3)=84
    ppqnZc(iiZ4)=85
    ppqnZc(iiZ2)=86
    ppqp_mz(iiZ5)=87
    ppqp_mz(iiZ6)=88
    ppqn_mz(iiZ5)=89
    ppqn_mz(iiZ6)=90
    pprumn4(iiP1)=91
    pprumn4(iiP2)=92
    pprumn4(iiP3)=93
    pprumn4(iiP4)=94
    pprumn4(iiP5)=95
    pprumn4(iiP6)=96
    pprumn3(iiP1)=97
    pprumn3(iiP2)=98
    pprumn3(iiP3)=99
    pprumn3(iiP4)=100
    pprumn3(iiP5)=101
    pprumn3(iiP6)=102
    pprumnu(iiP1)=103
    pprumnu(iiP2)=104
    pprumnu(iiP3)=105
    pprumnu(iiP4)=106
    pprumnu(iiP5)=107
    pprumnu(iiP6)=108
    pprump1(iiP1)=109
    pprump1(iiP2)=110
    pprump1(iiP3)=111
    pprump1(iiP4)=112
    pprump1(iiP5)=113
    pprump1(iiP6)=114
    pprumpu(iiP1)=115
    pprumpu(iiP2)=116
    pprumpu(iiP3)=117
    pprumpu(iiP4)=118
    pprumpu(iiP5)=119
    pprumpu(iiP6)=120
    pprums5(iiP1)=121
    pprums5(iiP2)=122
    pprums5(iiP3)=123
    pprums5(iiP4)=124
    pprums5(iiP5)=125
    pprums5(iiP6)=126
    ppflPIR1n(iiP1)=127
    ppflPIR1n(iiP2)=128
    ppflPIR1n(iiP3)=129
    ppflPIR1n(iiP4)=130
    ppflPIR1n(iiP5)=131
    ppflPIR1n(iiP6)=132
    ppflPIR1p(iiP1)=133
    ppflPIR1p(iiP2)=134
    ppflPIR1p(iiP3)=135
    ppflPIR1p(iiP4)=136
    ppflPIR1p(iiP5)=137
    ppflPIR1p(iiP6)=138
    ppflPIR6n(iiP1)=139
    ppflPIR6n(iiP2)=140
    ppflPIR6n(iiP3)=141
    ppflPIR6n(iiP4)=142
    ppflPIR6n(iiP5)=143
    ppflPIR6n(iiP6)=144
    ppflPIR6p(iiP1)=145
    ppflPIR6p(iiP2)=146
    ppflPIR6p(iiP3)=147
    ppflPIR6p(iiP4)=148
    ppflPIR6p(iiP5)=149
    ppflPIR6p(iiP6)=150
    ppflPIR6s(iiP1)=151
    ppflPIR6s(iiP2)=152
    ppflPIR6s(iiP3)=153
    ppflPIR6s(iiP4)=154
    ppflPIR6s(iiP5)=155
    ppflPIR6s(iiP6)=156
    ppsediPI(iiP1)=157
    ppsediPI(iiP2)=158
    ppsediPI(iiP3)=159
    ppsediPI(iiP4)=160
    ppsediPI(iiP5)=161
    ppsediPI(iiP6)=162
    ppsediMiZ(iiZ5)=163
    ppsediMiZ(iiZ6)=164
    ppsediMeZ(iiZ3)=165
    ppsediMeZ(iiZ4)=166
    ppsediMeZ(iiZ2)=167
    ppeiPI(iiP1)=168
    ppeiPI(iiP2)=169
    ppeiPI(iiP3)=170
    ppeiPI(iiP4)=171
    ppeiPI(iiP5)=172
    ppeiPI(iiP6)=173
    ppEPLi(iiP1)=174
    ppEPLi(iiP2)=175
    ppEPLi(iiP3)=176
    ppEPLi(iiP4)=177
    ppEPLi(iiP5)=178
    ppEPLi(iiP6)=179

    ETW => D3DIAGNOS(ppETW,:); ETW=ZERO
    ESW => D3DIAGNOS(ppESW,:); ESW=ZERO
    EIR => D3DIAGNOS(ppEIR,:); EIR=ZERO
    ERHO => D3DIAGNOS(ppERHO,:); ERHO=ZERO
    ESS => D3DIAGNOS(ppESS,:); ESS=ZERO
    cxoO2 => D3DIAGNOS(ppcxoO2,:); cxoO2=ZERO
    XO2o => D3DIAGNOS(ppXO2o,:); XO2o=ZERO
    eO2mO2 => D3DIAGNOS(ppeO2mO2,:); eO2mO2=ZERO
    Chla => D3DIAGNOS(ppChla,:); Chla=ZERO
    Xantho => D3DIAGNOS(ppXantho,:); Xantho=ZERO
    flP6R3c => D3DIAGNOS(ppflP6R3c,:); flP6R3c=ZERO
    flPTN6r => D3DIAGNOS(ppflPTN6r,:); flPTN6r=ZERO
    flN4N3n => D3DIAGNOS(ppflN4N3n,:); flN4N3n=ZERO
    flN3O4n => D3DIAGNOS(ppflN3O4n,:); flN3O4n=ZERO
    rumn4B => D3DIAGNOS(pprumn4B,:); rumn4B=ZERO
    rumn3B => D3DIAGNOS(pprumn3B,:); rumn3B=ZERO
    rumnuB => D3DIAGNOS(pprumnuB,:); rumnuB=ZERO
    rumpB => D3DIAGNOS(pprumpB,:); rumpB=ZERO
    rumpuB => D3DIAGNOS(pprumpuB,:); rumpuB=ZERO
    lim_rumn4 => D3DIAGNOS(pplim_rumn4,:); lim_rumn4=ZERO
    lim_rumn3 => D3DIAGNOS(pplim_rumn3,:); lim_rumn3=ZERO
    lim_rumnu => D3DIAGNOS(pplim_rumnu,:); lim_rumnu=ZERO
    lim_rump1 => D3DIAGNOS(pplim_rump1,:); lim_rump1=ZERO
    lim_rumpu => D3DIAGNOS(pplim_rumpu,:); lim_rumpu=ZERO
    lim_rums5 => D3DIAGNOS(pplim_rums5,:); lim_rums5=ZERO
    rml => D3DIAGNOS(pprml,:); rml=ZERO
    qpR6c => D3DIAGNOS(ppqpR6c,:); qpR6c=ZERO
    qnR6c => D3DIAGNOS(ppqnR6c,:); qnR6c=ZERO
    qsR6c => D3DIAGNOS(ppqsR6c,:); qsR6c=ZERO
    qpB1c => D3DIAGNOS(ppqpB1c,:); qpB1c=ZERO
    qnB1c => D3DIAGNOS(ppqnB1c,:); qnB1c=ZERO
    pMIupZ4 => D3DIAGNOS(pppMIupZ4,:); pMIupZ4=ZERO
    rnetPTc => D3DIAGNOS(pprnetPTc,:); rnetPTc=ZERO
    flnDIn => D3DIAGNOS(ppflnDIn,:); flnDIn=ZERO
    flnDIp => D3DIAGNOS(ppflnDIp,:); flnDIp=ZERO
    sediR2 => D3DIAGNOS(ppsediR2,:); sediR2=ZERO
    sediR6 => D3DIAGNOS(ppsediR6,:); sediR6=ZERO
    sediRZ => D3DIAGNOS(ppsediRZ,:); sediRZ=ZERO
    PTi => D3DIAGNOS(ppPTi,:); PTi=ZERO
    limnuti => D3DIAGNOS(pplimnuti,:); limnuti=ZERO
    xEPS => D3DIAGNOS(ppxEPS,:); xEPS=ZERO
    xEPS_0 => D3DIAGNOS(ppxEPS_0,:); xEPS_0=ZERO
    xEPS_ESS => D3DIAGNOS(ppxEPS_ESS,:); xEPS_ESS=ZERO
    xEPS_Chl => D3DIAGNOS(ppxEPS_Chl,:); xEPS_Chl=ZERO

    sugPI => D3DIAGNOS(ppsugPI(iiP1): ppsugPI(iiP6),:)
    sugPI=ZERO
    sunPI => D3DIAGNOS(ppsunPI(iiP1): ppsunPI(iiP6),:)
    sunPI=ZERO
    qpPc => D3DIAGNOS(ppqpPc(iiP1): ppqpPc(iiP6),:)
    qpPc=ZERO
    qnPc => D3DIAGNOS(ppqnPc(iiP1): ppqnPc(iiP6),:)
    qnPc=ZERO
    qsPc => D3DIAGNOS(ppqsPc(iiP1): ppqsPc(iiP6),:)
    qsPc=ZERO
    qlPc => D3DIAGNOS(ppqlPc(iiP1): ppqlPc(iiP6),:)
    qlPc=ZERO
    qpZc => D3DIAGNOS(ppqpZc(iiZ3): ppqpZc(iiZ2),:)
    qpZc=ZERO
    qnZc => D3DIAGNOS(ppqnZc(iiZ3): ppqnZc(iiZ2),:)
    qnZc=ZERO
    qp_mz => D3DIAGNOS(ppqp_mz(iiZ5): ppqp_mz(iiZ6),:)
    qp_mz=ZERO
    qn_mz => D3DIAGNOS(ppqn_mz(iiZ5): ppqn_mz(iiZ6),:)
    qn_mz=ZERO
    rumn4 => D3DIAGNOS(pprumn4(iiP1): pprumn4(iiP6),:)
    rumn4=ZERO
    rumn3 => D3DIAGNOS(pprumn3(iiP1): pprumn3(iiP6),:)
    rumn3=ZERO
    rumnu => D3DIAGNOS(pprumnu(iiP1): pprumnu(iiP6),:)
    rumnu=ZERO
    rump1 => D3DIAGNOS(pprump1(iiP1): pprump1(iiP6),:)
    rump1=ZERO
    rumpu => D3DIAGNOS(pprumpu(iiP1): pprumpu(iiP6),:)
    rumpu=ZERO
    rums5 => D3DIAGNOS(pprums5(iiP1): pprums5(iiP6),:)
    rums5=ZERO
    flPIR1n => D3DIAGNOS(ppflPIR1n(iiP1): ppflPIR1n(iiP6),:)
    flPIR1n=ZERO
    flPIR1p => D3DIAGNOS(ppflPIR1p(iiP1): ppflPIR1p(iiP6),:)
    flPIR1p=ZERO
    flPIR6n => D3DIAGNOS(ppflPIR6n(iiP1): ppflPIR6n(iiP6),:)
    flPIR6n=ZERO
    flPIR6p => D3DIAGNOS(ppflPIR6p(iiP1): ppflPIR6p(iiP6),:)
    flPIR6p=ZERO
    flPIR6s => D3DIAGNOS(ppflPIR6s(iiP1): ppflPIR6s(iiP6),:)
    flPIR6s=ZERO
    sediPI => D3DIAGNOS(ppsediPI(iiP1): ppsediPI(iiP6),:)
    sediPI=ZERO
    sediMiZ => D3DIAGNOS(ppsediMiZ(iiZ5): ppsediMiZ(iiZ6),:)
    sediMiZ=ZERO
    sediMeZ => D3DIAGNOS(ppsediMeZ(iiZ3): ppsediMeZ(iiZ2),:)
    sediMeZ=ZERO
    eiPI => D3DIAGNOS(ppeiPI(iiP1): ppeiPI(iiP6),:)
    eiPI=ZERO
    EPLi => D3DIAGNOS(ppEPLi(iiP1): ppEPLi(iiP6),:)
    EPLi=ZERO


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Start the allocation of other benthic vairables which can be outputted 
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    ppruHI(iiQ1)=123
    ppruHI(iiQ11)=124
    ppreHI(iiQ1)=125
    ppreHI(iiQ11)=126
    pprugYIc(iiY1)=127
    pprugYIc(iiY2)=128
    pprugYIc(iiY3)=129
    pprugYIc(iiY4)=130
    pprugYIc(iiY5)=131
    ppjPIY3c(iiP1)=132
    ppjPIY3c(iiP2)=133
    ppjPIY3c(iiP3)=134
    ppjPIY3c(iiP4)=135
    ppjPIY3c(iiP5)=136
    ppjPIY3c(iiP6)=137
    ppPI_Benc(iiP1)=138
    ppPI_Benc(iiP2)=139
    ppPI_Benc(iiP3)=140
    ppPI_Benc(iiP4)=141
    ppPI_Benc(iiP5)=142
    ppPI_Benc(iiP6)=143
    ppPI_Benn(iiP1)=144
    ppPI_Benn(iiP2)=145
    ppPI_Benn(iiP3)=146
    ppPI_Benn(iiP4)=147
    ppPI_Benn(iiP5)=148
    ppPI_Benn(iiP6)=149
    ppPI_Benp(iiP1)=150
    ppPI_Benp(iiP2)=151
    ppPI_Benp(iiP3)=152
    ppPI_Benp(iiP4)=153
    ppPI_Benp(iiP5)=154
    ppPI_Benp(iiP6)=155
    ppPI_Bens(iiP1)=156
    ppPI_Bens(iiP2)=157
    ppPI_Bens(iiP3)=158
    ppPI_Bens(iiP4)=159
    ppPI_Bens(iiP5)=160
    ppPI_Bens(iiP6)=161
    pppuPIY3(iiP1)=162
    pppuPIY3(iiP2)=163
    pppuPIY3(iiP3)=164
    pppuPIY3(iiP4)=165
    pppuPIY3(iiP5)=166
    pppuPIY3(iiP6)=167
    ppsediPI_Ben(iiP1)=168
    ppsediPI_Ben(iiP2)=169
    ppsediPI_Ben(iiP3)=170
    ppsediPI_Ben(iiP4)=171
    ppsediPI_Ben(iiP5)=172
    ppsediPI_Ben(iiP6)=173

    EUWIND => D2DIAGNOS(ppEUWIND,:); EUWIND=ZERO
    EVWIND => D2DIAGNOS(ppEVWIND,:); EVWIND=ZERO
    ETAUB => D2DIAGNOS(ppETAUB,:); ETAUB=ZERO
    rrBTo => D2DIAGNOS(pprrBTo,:); rrBTo=ZERO
    rrATo => D2DIAGNOS(pprrATo,:); rrATo=ZERO
    reBTn => D2DIAGNOS(ppreBTn,:); reBTn=ZERO
    reBTp => D2DIAGNOS(ppreBTp,:); reBTp=ZERO
    reATn => D2DIAGNOS(ppreATn,:); reATn=ZERO
    reATp => D2DIAGNOS(ppreATp,:); reATp=ZERO
    irrenh => D2DIAGNOS(ppirrenh,:); irrenh=ZERO
    turenh => D2DIAGNOS(ppturenh,:); turenh=ZERO
    shiftD1m => D2DIAGNOS(ppshiftD1m,:); shiftD1m=ZERO
    shiftD2m => D2DIAGNOS(ppshiftD2m,:); shiftD2m=ZERO
    jG2K3o => D2DIAGNOS(ppjG2K3o,:); jG2K3o=ZERO
    jG2K7o => D2DIAGNOS(ppjG2K7o,:); jG2K7o=ZERO
    M1p => D2DIAGNOS(ppM1p,:); M1p=ZERO
    M11p => D2DIAGNOS(ppM11p,:); M11p=ZERO
    M21p => D2DIAGNOS(ppM21p,:); M21p=ZERO
    M4n => D2DIAGNOS(ppM4n,:); M4n=ZERO
    M14n => D2DIAGNOS(ppM14n,:); M14n=ZERO
    M24n => D2DIAGNOS(ppM24n,:); M24n=ZERO
    M3n => D2DIAGNOS(ppM3n,:); M3n=ZERO
    M5s => D2DIAGNOS(ppM5s,:); M5s=ZERO
    M6r => D2DIAGNOS(ppM6r,:); M6r=ZERO
    RI_Fc => D2DIAGNOS(ppRI_Fc,:); RI_Fc=ZERO
    RI_Fn => D2DIAGNOS(ppRI_Fn,:); RI_Fn=ZERO
    RI_Fp => D2DIAGNOS(ppRI_Fp,:); RI_Fp=ZERO
    RI_Fs => D2DIAGNOS(ppRI_Fs,:); RI_Fs=ZERO
    ZI_Fc => D2DIAGNOS(ppZI_Fc,:); ZI_Fc=ZERO
    ZI_Fn => D2DIAGNOS(ppZI_Fn,:); ZI_Fn=ZERO
    ZI_Fp => D2DIAGNOS(ppZI_Fp,:); ZI_Fp=ZERO
    ZI_Fs => D2DIAGNOS(ppZI_Fs,:); ZI_Fs=ZERO
    jZIY3c => D2DIAGNOS(ppjZIY3c,:); jZIY3c=ZERO
    jRIY3c => D2DIAGNOS(ppjRIY3c,:); jRIY3c=ZERO
    jY3QIc => D2DIAGNOS(ppjY3QIc,:); jY3QIc=ZERO
    jY3QIs => D2DIAGNOS(ppjY3QIs,:); jY3QIs=ZERO
    jY3RIc => D2DIAGNOS(ppjY3RIc,:); jY3RIc=ZERO
    jY3RIn => D2DIAGNOS(ppjY3RIn,:); jY3RIn=ZERO
    jY3RIp => D2DIAGNOS(ppjY3RIp,:); jY3RIp=ZERO
    jY3RIs => D2DIAGNOS(ppjY3RIs,:); jY3RIs=ZERO
    jRIQIc => D2DIAGNOS(ppjRIQIc,:); jRIQIc=ZERO
    jRIQIn => D2DIAGNOS(ppjRIQIn,:); jRIQIn=ZERO
    jRIQIp => D2DIAGNOS(ppjRIQIp,:); jRIQIp=ZERO
    jRIQIs => D2DIAGNOS(ppjRIQIs,:); jRIQIs=ZERO
    jY3O3c => D2DIAGNOS(ppjY3O3c,:); jY3O3c=ZERO
    jO2Y3o => D2DIAGNOS(ppjO2Y3o,:); jO2Y3o=ZERO
    jY3N4n => D2DIAGNOS(ppjY3N4n,:); jY3N4n=ZERO
    jY3N1p => D2DIAGNOS(ppjY3N1p,:); jY3N1p=ZERO
    Depth_Ben => D2DIAGNOS(ppDepth_Ben,:); Depth_Ben=ZERO
    ETW_Ben => D2DIAGNOS(ppETW_Ben,:); ETW_Ben=ZERO
    ERHO_Ben => D2DIAGNOS(ppERHO_Ben,:); ERHO_Ben=ZERO
    ESW_Ben => D2DIAGNOS(ppESW_Ben,:); ESW_Ben=ZERO
    puP6Y3 => D2DIAGNOS(pppuP6Y3,:); puP6Y3=ZERO
    R3c_Ben => D2DIAGNOS(ppR3c_Ben,:); R3c_Ben=ZERO
    O2o_Ben => D2DIAGNOS(ppO2o_Ben,:); O2o_Ben=ZERO
    N1p_Ben => D2DIAGNOS(ppN1p_Ben,:); N1p_Ben=ZERO
    N3n_Ben => D2DIAGNOS(ppN3n_Ben,:); N3n_Ben=ZERO
    N4n_Ben => D2DIAGNOS(ppN4n_Ben,:); N4n_Ben=ZERO
    N5s_Ben => D2DIAGNOS(ppN5s_Ben,:); N5s_Ben=ZERO
    N6r_Ben => D2DIAGNOS(ppN6r_Ben,:); N6r_Ben=ZERO
    sediR6_Ben => D2DIAGNOS(ppsediR6_Ben,:); sediR6_Ben=ZERO
    flP6R6_Bn => D2DIAGNOS(ppflP6R6_Bn,:); flP6R6_Bn=ZERO
    flP6R6_Bp => D2DIAGNOS(ppflP6R6_Bp,:); flP6R6_Bp=ZERO
    flP6R1_Bn => D2DIAGNOS(ppflP6R1_Bn,:); flP6R1_Bn=ZERO
    flP6R1_Bp => D2DIAGNOS(ppflP6R1_Bp,:); flP6R1_Bp=ZERO
    flR3R1_Bc => D2DIAGNOS(ppflR3R1_Bc,:); flR3R1_Bc=ZERO
    flR3R2_Bc => D2DIAGNOS(ppflR3R2_Bc,:); flR3R2_Bc=ZERO
    flR3R6_Bc => D2DIAGNOS(ppflR3R6_Bc,:); flR3R6_Bc=ZERO
    efilP6Y3 => D2DIAGNOS(ppefilP6Y3,:); efilP6Y3=ZERO
    pyfoodY3 => D2DIAGNOS(pppyfoodY3,:); pyfoodY3=ZERO
    ctfPm2c => D2DIAGNOS(ppctfPm2c,:); ctfPm2c=ZERO
    ctfZm2c => D2DIAGNOS(ppctfZm2c,:); ctfZm2c=ZERO
    ctfRm2c => D2DIAGNOS(ppctfRm2c,:); ctfRm2c=ZERO
    cZ2m2c => D2DIAGNOS(ppcZ2m2c,:); cZ2m2c=ZERO
    sK4K3 => D2DIAGNOS(ppsK4K3,:); sK4K3=ZERO
    jK4K3n => D2DIAGNOS(ppjK4K3n,:); jK4K3n=ZERO
    jK3G4n => D2DIAGNOS(ppjK3G4n,:); jK3G4n=ZERO
    jK31K21p => D2DIAGNOS(ppjK31K21p,:); jK31K21p=ZERO
    jK34K24n => D2DIAGNOS(ppjK34K24n,:); jK34K24n=ZERO
    jK13K3n => D2DIAGNOS(ppjK13K3n,:); jK13K3n=ZERO
    jK25K15s => D2DIAGNOS(ppjK25K15s,:); jK25K15s=ZERO
    jK36K26r => D2DIAGNOS(ppjK36K26r,:); jK36K26r=ZERO
    totPELc => D2DIAGNOS(pptotPELc,:); totPELc=ZERO
    totPELn => D2DIAGNOS(pptotPELn,:); totPELn=ZERO
    totPELp => D2DIAGNOS(pptotPELp,:); totPELp=ZERO
    totPELs => D2DIAGNOS(pptotPELs,:); totPELs=ZERO
    totBENc => D2DIAGNOS(pptotBENc,:); totBENc=ZERO
    totBENn => D2DIAGNOS(pptotBENn,:); totBENn=ZERO
    totBENp => D2DIAGNOS(pptotBENp,:); totBENp=ZERO
    totBENs => D2DIAGNOS(pptotBENs,:); totBENs=ZERO
    totSYSc => D2DIAGNOS(pptotSYSc,:); totSYSc=ZERO
    totSYSn => D2DIAGNOS(pptotSYSn,:); totSYSn=ZERO
    totSYSp => D2DIAGNOS(pptotSYSp,:); totSYSp=ZERO
    totSYSs => D2DIAGNOS(pptotSYSs,:); totSYSs=ZERO
    jtotbenpelc => D2DIAGNOS(ppjtotbenpelc,:); jtotbenpelc=ZERO
    jtotbenpeln => D2DIAGNOS(ppjtotbenpeln,:); jtotbenpeln=ZERO
    jtotbenpelp => D2DIAGNOS(ppjtotbenpelp,:); jtotbenpelp=ZERO
    jtotbenpels => D2DIAGNOS(ppjtotbenpels,:); jtotbenpels=ZERO
    jupPELc => D2DIAGNOS(ppjupPELc,:); jupPELc=ZERO
    jminPELc => D2DIAGNOS(ppjminPELc,:); jminPELc=ZERO
    totPELInc => D2DIAGNOS(pptotPELInc,:); totPELInc=ZERO
    jupBENc => D2DIAGNOS(ppjupBENc,:); jupBENc=ZERO
    jminBENc => D2DIAGNOS(ppjminBENc,:); jminBENc=ZERO
    totBENInc => D2DIAGNOS(pptotBENInc,:); totBENInc=ZERO
    jsdoMesoc => D2DIAGNOS(ppjsdoMesoc,:); jsdoMesoc=ZERO
    jrsMicroc => D2DIAGNOS(ppjrsMicroc,:); jrsMicroc=ZERO
    jnetPTc => D2DIAGNOS(ppjnetPTc,:); jnetPTc=ZERO
    jnetMeZc => D2DIAGNOS(ppjnetMeZc,:); jnetMeZc=ZERO
    jnetMiZc => D2DIAGNOS(ppjnetMiZc,:); jnetMiZc=ZERO
    jnetB1c => D2DIAGNOS(ppjnetB1c,:); jnetB1c=ZERO
    jnetY3c => D2DIAGNOS(ppjnetY3c,:); jnetY3c=ZERO
    jnetYy3c => D2DIAGNOS(ppjnetYy3c,:); jnetYy3c=ZERO
    jCaCO3Y3c => D2DIAGNOS(ppjCaCO3Y3c,:); jCaCO3Y3c=ZERO
    Output2d_1 => D2DIAGNOS(ppOutput2d_1,:); Output2d_1=ZERO
    Output2d_2 => D2DIAGNOS(ppOutput2d_2,:); Output2d_2=ZERO
    Output2d_3 => D2DIAGNOS(ppOutput2d_3,:); Output2d_3=ZERO
    Output2d_4 => D2DIAGNOS(ppOutput2d_4,:); Output2d_4=ZERO
    jPelFishInput => D2DIAGNOS(ppjPelFishInput,:); jPelFishInput=ZERO
    jBenFishInput => D2DIAGNOS(ppjBenFishInput,:); jBenFishInput=ZERO
    SdTdzTh => D2DIAGNOS(ppSdTdzTh,:); SdTdzTh=ZERO
    TMLd => D2DIAGNOS(ppTMLd,:); TMLd=ZERO
    BMLd => D2DIAGNOS(ppBMLd,:); BMLd=ZERO

    ruHI => D2DIAGNOS(ppruHI(iiQ1): ppruHI(iiQ11),:)
    ruHI=ZERO
    reHI => D2DIAGNOS(ppreHI(iiQ1): ppreHI(iiQ11),:)
    reHI=ZERO
    rugYIc => D2DIAGNOS(pprugYIc(iiY1): pprugYIc(iiY5),:)
    rugYIc=ZERO
    jPIY3c => D2DIAGNOS(ppjPIY3c(iiP1): ppjPIY3c(iiP6),:)
    jPIY3c=ZERO
    PI_Benc => D2DIAGNOS(ppPI_Benc(iiP1): ppPI_Benc(iiP6),:)
    PI_Benc=ZERO
    PI_Benn => D2DIAGNOS(ppPI_Benn(iiP1): ppPI_Benn(iiP6),:)
    PI_Benn=ZERO
    PI_Benp => D2DIAGNOS(ppPI_Benp(iiP1): ppPI_Benp(iiP6),:)
    PI_Benp=ZERO
    PI_Bens => D2DIAGNOS(ppPI_Bens(iiP1): ppPI_Bens(iiP6),:)
    PI_Bens=ZERO
    puPIY3 => D2DIAGNOS(pppuPIY3(iiP1): pppuPIY3(iiP6),:)
    puPIY3=ZERO
    sediPI_Ben => D2DIAGNOS(ppsediPI_Ben(iiP1): ppsediPI_Ben(iiP6),:)
    sediPI_Ben=ZERO


  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! 
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    PELSURFACE => D2DIAGNOS(173+1:173+54,:); PELSURFACE=ZERO
    jsurR9x => D2DIAGNOS(173+ppR9x,:); jsurR9x=ZERO
    jsurO2o => D2DIAGNOS(173+ppO2o,:); jsurO2o=ZERO
    jsurN1p => D2DIAGNOS(173+ppN1p,:); jsurN1p=ZERO
    jsurN3n => D2DIAGNOS(173+ppN3n,:); jsurN3n=ZERO
    jsurN4n => D2DIAGNOS(173+ppN4n,:); jsurN4n=ZERO
    jsurO4n => D2DIAGNOS(173+ppO4n,:); jsurO4n=ZERO
    jsurN5s => D2DIAGNOS(173+ppN5s,:); jsurN5s=ZERO
    jsurN6r => D2DIAGNOS(173+ppN6r,:); jsurN6r=ZERO
    jsurB1c => D2DIAGNOS(173+ppB1c,:); jsurB1c=ZERO
    jsurB1n => D2DIAGNOS(173+ppB1n,:); jsurB1n=ZERO
    jsurB1p => D2DIAGNOS(173+ppB1p,:); jsurB1p=ZERO
    jsurP1c => D2DIAGNOS(173+ppP1c,:); jsurP1c=ZERO
    jsurP1n => D2DIAGNOS(173+ppP1n,:); jsurP1n=ZERO
    jsurP1p => D2DIAGNOS(173+ppP1p,:); jsurP1p=ZERO
    jsurP1l => D2DIAGNOS(173+ppP1l,:); jsurP1l=ZERO
    jsurP1s => D2DIAGNOS(173+ppP1s,:); jsurP1s=ZERO
    jsurP2c => D2DIAGNOS(173+ppP2c,:); jsurP2c=ZERO
    jsurP2n => D2DIAGNOS(173+ppP2n,:); jsurP2n=ZERO
    jsurP2p => D2DIAGNOS(173+ppP2p,:); jsurP2p=ZERO
    jsurP2l => D2DIAGNOS(173+ppP2l,:); jsurP2l=ZERO
    jsurP3c => D2DIAGNOS(173+ppP3c,:); jsurP3c=ZERO
    jsurP3n => D2DIAGNOS(173+ppP3n,:); jsurP3n=ZERO
    jsurP3p => D2DIAGNOS(173+ppP3p,:); jsurP3p=ZERO
    jsurP3l => D2DIAGNOS(173+ppP3l,:); jsurP3l=ZERO
    jsurP4c => D2DIAGNOS(173+ppP4c,:); jsurP4c=ZERO
    jsurP4n => D2DIAGNOS(173+ppP4n,:); jsurP4n=ZERO
    jsurP4p => D2DIAGNOS(173+ppP4p,:); jsurP4p=ZERO
    jsurP4l => D2DIAGNOS(173+ppP4l,:); jsurP4l=ZERO
    jsurP5c => D2DIAGNOS(173+ppP5c,:); jsurP5c=ZERO
    jsurP5n => D2DIAGNOS(173+ppP5n,:); jsurP5n=ZERO
    jsurP5p => D2DIAGNOS(173+ppP5p,:); jsurP5p=ZERO
    jsurP5l => D2DIAGNOS(173+ppP5l,:); jsurP5l=ZERO
    jsurP5s => D2DIAGNOS(173+ppP5s,:); jsurP5s=ZERO
    jsurP6c => D2DIAGNOS(173+ppP6c,:); jsurP6c=ZERO
    jsurP6n => D2DIAGNOS(173+ppP6n,:); jsurP6n=ZERO
    jsurP6p => D2DIAGNOS(173+ppP6p,:); jsurP6p=ZERO
    jsurP6l => D2DIAGNOS(173+ppP6l,:); jsurP6l=ZERO
    jsurPcc => D2DIAGNOS(173+ppPcc,:); jsurPcc=ZERO
    jsurR1c => D2DIAGNOS(173+ppR1c,:); jsurR1c=ZERO
    jsurR1n => D2DIAGNOS(173+ppR1n,:); jsurR1n=ZERO
    jsurR1p => D2DIAGNOS(173+ppR1p,:); jsurR1p=ZERO
    jsurR2c => D2DIAGNOS(173+ppR2c,:); jsurR2c=ZERO
    jsurR3c => D2DIAGNOS(173+ppR3c,:); jsurR3c=ZERO
    jsurR6c => D2DIAGNOS(173+ppR6c,:); jsurR6c=ZERO
    jsurR6n => D2DIAGNOS(173+ppR6n,:); jsurR6n=ZERO
    jsurR6p => D2DIAGNOS(173+ppR6p,:); jsurR6p=ZERO
    jsurR6s => D2DIAGNOS(173+ppR6s,:); jsurR6s=ZERO
    jsurRZc => D2DIAGNOS(173+ppRZc,:); jsurRZc=ZERO
    jsurR7c => D2DIAGNOS(173+ppR7c,:); jsurR7c=ZERO
    jsurZ3c => D2DIAGNOS(173+ppZ3c,:); jsurZ3c=ZERO
    jsurZ4c => D2DIAGNOS(173+ppZ4c,:); jsurZ4c=ZERO
    jsurZ2c => D2DIAGNOS(173+ppZ2c,:); jsurZ2c=ZERO
    jsurZ5c => D2DIAGNOS(173+ppZ5c,:); jsurZ5c=ZERO
    jsurZ6c => D2DIAGNOS(173+ppZ6c,:); jsurZ6c=ZERO

    PELBOTTOM => D2DIAGNOS(227+1:227+54,:); PELBOTTOM=ZERO
    jbotR9x => D2DIAGNOS(227+ppR9x,:); jbotR9x=ZERO
    jbotO2o => D2DIAGNOS(227+ppO2o,:); jbotO2o=ZERO
    jbotN1p => D2DIAGNOS(227+ppN1p,:); jbotN1p=ZERO
    jbotN3n => D2DIAGNOS(227+ppN3n,:); jbotN3n=ZERO
    jbotN4n => D2DIAGNOS(227+ppN4n,:); jbotN4n=ZERO
    jbotO4n => D2DIAGNOS(227+ppO4n,:); jbotO4n=ZERO
    jbotN5s => D2DIAGNOS(227+ppN5s,:); jbotN5s=ZERO
    jbotN6r => D2DIAGNOS(227+ppN6r,:); jbotN6r=ZERO
    jbotB1c => D2DIAGNOS(227+ppB1c,:); jbotB1c=ZERO
    jbotB1n => D2DIAGNOS(227+ppB1n,:); jbotB1n=ZERO
    jbotB1p => D2DIAGNOS(227+ppB1p,:); jbotB1p=ZERO
    jbotP1c => D2DIAGNOS(227+ppP1c,:); jbotP1c=ZERO
    jbotP1n => D2DIAGNOS(227+ppP1n,:); jbotP1n=ZERO
    jbotP1p => D2DIAGNOS(227+ppP1p,:); jbotP1p=ZERO
    jbotP1l => D2DIAGNOS(227+ppP1l,:); jbotP1l=ZERO
    jbotP1s => D2DIAGNOS(227+ppP1s,:); jbotP1s=ZERO
    jbotP2c => D2DIAGNOS(227+ppP2c,:); jbotP2c=ZERO
    jbotP2n => D2DIAGNOS(227+ppP2n,:); jbotP2n=ZERO
    jbotP2p => D2DIAGNOS(227+ppP2p,:); jbotP2p=ZERO
    jbotP2l => D2DIAGNOS(227+ppP2l,:); jbotP2l=ZERO
    jbotP3c => D2DIAGNOS(227+ppP3c,:); jbotP3c=ZERO
    jbotP3n => D2DIAGNOS(227+ppP3n,:); jbotP3n=ZERO
    jbotP3p => D2DIAGNOS(227+ppP3p,:); jbotP3p=ZERO
    jbotP3l => D2DIAGNOS(227+ppP3l,:); jbotP3l=ZERO
    jbotP4c => D2DIAGNOS(227+ppP4c,:); jbotP4c=ZERO
    jbotP4n => D2DIAGNOS(227+ppP4n,:); jbotP4n=ZERO
    jbotP4p => D2DIAGNOS(227+ppP4p,:); jbotP4p=ZERO
    jbotP4l => D2DIAGNOS(227+ppP4l,:); jbotP4l=ZERO
    jbotP5c => D2DIAGNOS(227+ppP5c,:); jbotP5c=ZERO
    jbotP5n => D2DIAGNOS(227+ppP5n,:); jbotP5n=ZERO
    jbotP5p => D2DIAGNOS(227+ppP5p,:); jbotP5p=ZERO
    jbotP5l => D2DIAGNOS(227+ppP5l,:); jbotP5l=ZERO
    jbotP5s => D2DIAGNOS(227+ppP5s,:); jbotP5s=ZERO
    jbotP6c => D2DIAGNOS(227+ppP6c,:); jbotP6c=ZERO
    jbotP6n => D2DIAGNOS(227+ppP6n,:); jbotP6n=ZERO
    jbotP6p => D2DIAGNOS(227+ppP6p,:); jbotP6p=ZERO
    jbotP6l => D2DIAGNOS(227+ppP6l,:); jbotP6l=ZERO
    jbotPcc => D2DIAGNOS(227+ppPcc,:); jbotPcc=ZERO
    jbotR1c => D2DIAGNOS(227+ppR1c,:); jbotR1c=ZERO
    jbotR1n => D2DIAGNOS(227+ppR1n,:); jbotR1n=ZERO
    jbotR1p => D2DIAGNOS(227+ppR1p,:); jbotR1p=ZERO
    jbotR2c => D2DIAGNOS(227+ppR2c,:); jbotR2c=ZERO
    jbotR3c => D2DIAGNOS(227+ppR3c,:); jbotR3c=ZERO
    jbotR6c => D2DIAGNOS(227+ppR6c,:); jbotR6c=ZERO
    jbotR6n => D2DIAGNOS(227+ppR6n,:); jbotR6n=ZERO
    jbotR6p => D2DIAGNOS(227+ppR6p,:); jbotR6p=ZERO
    jbotR6s => D2DIAGNOS(227+ppR6s,:); jbotR6s=ZERO
    jbotRZc => D2DIAGNOS(227+ppRZc,:); jbotRZc=ZERO
    jbotR7c => D2DIAGNOS(227+ppR7c,:); jbotR7c=ZERO
    jbotZ3c => D2DIAGNOS(227+ppZ3c,:); jbotZ3c=ZERO
    jbotZ4c => D2DIAGNOS(227+ppZ4c,:); jbotZ4c=ZERO
    jbotZ2c => D2DIAGNOS(227+ppZ2c,:); jbotZ2c=ZERO
    jbotZ5c => D2DIAGNOS(227+ppZ5c,:); jbotZ5c=ZERO
    jbotZ6c => D2DIAGNOS(227+ppZ6c,:); jbotZ6c=ZERO

!    3d-state-field-alloc-pointer river
    allocate(iiPELSINKREF(1:NO_D3_BOX_STATES ),stat=status)
    if (status /= 0) call error_msg_prn(ALLOC,"AllocateMem","iiPELSINKREF")
    iiPELSINKREF = 0





    allocate(OCDepth(1:NO_BOXES), stat=status); OCDepth = ZERO
    allocate(Depth(1:NO_BOXES), stat=status); Depth = ZERO
    allocate(ABIO_eps(1:NO_BOXES), stat=status); ABIO_eps = ZERO



    allocate(KPO4(1:NO_BOXES_XY), stat=status); KPO4 = ZERO
    allocate(KPO4_2(1:NO_BOXES_XY), stat=status); KPO4_2 = ZERO
    allocate(KNH4(1:NO_BOXES_XY), stat=status); KNH4 = ZERO
    allocate(KNO3(1:NO_BOXES_XY), stat=status); KNO3 = ZERO
    allocate(KNO3E(1:NO_BOXES_XY), stat=status); KNO3E = ZERO
    allocate(KRED(1:NO_BOXES_XY), stat=status); KRED = ZERO
    allocate(KSIO3(1:NO_BOXES_XY), stat=status); KSIO3 = ZERO
    allocate(KSIO3E(1:NO_BOXES_XY), stat=status); KSIO3E = ZERO
    allocate(KQ1(1:NO_BOXES_XY), stat=status); KQ1 = ZERO

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Start the allocation of vars for calculation of combined fluxes for output
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    allocate(flx_calc_nr(0:19),stat=status)
    allocate(flx_CalcIn(1:19),stat=status)
    allocate(flx_option(1:19),stat=status)
    allocate(flx_t(1:176),stat=status)
    allocate(flx_SS(1:176),stat=status)
    allocate(flx_states(1:176),stat=status)
    allocate(flx_ostates(1:176),stat=status)
    flx_calc_nr(0)=0
    flx_cal_ben_start=8


    ! ruPTc=P.c <- O3c        (flux):
    flx_calc_nr(1)= 7; flx_CalcIn(1)=iiPel; flx_option(1)=0
    flx_t(1)=+1.00;flx_SS(1)=0; flx_states(1)=ppP1c;flx_ostates(1)=ppP1c
    flx_t(2)=+1.00;flx_SS(2)=0; flx_states(2)=ppP2c;flx_ostates(2)=ppP2c
    flx_t(3)=+1.00;flx_SS(3)=0; flx_states(3)=ppP3c;flx_ostates(3)=ppP3c
    flx_t(4)=+1.00;flx_SS(4)=0; flx_states(4)=ppP4c;flx_ostates(4)=ppP4c
    flx_t(5)=+1.00;flx_SS(5)=0; flx_states(5)=ppP5c;flx_ostates(5)=ppP5c
    flx_t(6)=+1.00;flx_SS(6)=0; flx_states(6)=ppP6c;flx_ostates(6)=ppP6c
    flx_t(7)=+1.00;flx_SS(7)=0; flx_states(7)=ppPcc;flx_ostates(7)=ppPcc

    ! ruPTn=P.n <- N3n+N4n        (flux):
    flx_calc_nr(2)= 19; flx_CalcIn(2)=iiPel; flx_option(2)=0
    flx_t(8)=+1.00;flx_SS(8)=0; flx_states(8)=ppP1n;flx_ostates(8)=ppN3n
    flx_t(9)=+1.00;flx_SS(9)=0; flx_states(9)=ppP1n;flx_ostates(9)=ppN4n
    flx_t(10)=+1.00;flx_SS(10)=0; flx_states(10)=ppP2n;flx_ostates(10)=ppN3n
    flx_t(11)=+1.00;flx_SS(11)=0; flx_states(11)=ppP2n;flx_ostates(11)=ppN4n
    flx_t(12)=+1.00;flx_SS(12)=0; flx_states(12)=ppP3n;flx_ostates(12)=ppN3n
    flx_t(13)=+1.00;flx_SS(13)=0; flx_states(13)=ppP3n;flx_ostates(13)=ppN4n
    flx_t(14)=+1.00;flx_SS(14)=0; flx_states(14)=ppP4n;flx_ostates(14)=ppN3n
    flx_t(15)=+1.00;flx_SS(15)=0; flx_states(15)=ppP4n;flx_ostates(15)=ppN4n
    flx_t(16)=+1.00;flx_SS(16)=0; flx_states(16)=ppP5n;flx_ostates(16)=ppN3n
    flx_t(17)=+1.00;flx_SS(17)=0; flx_states(17)=ppP5n;flx_ostates(17)=ppN4n
    flx_t(18)=+1.00;flx_SS(18)=0; flx_states(18)=ppP6n;flx_ostates(18)=ppN3n
    flx_t(19)=+1.00;flx_SS(19)=0; flx_states(19)=ppP6n;flx_ostates(19)=ppN4n

    ! ruPTP=P.p <- N1p        (flux):
    flx_calc_nr(3)= 25; flx_CalcIn(3)=iiPel; flx_option(3)=0
    flx_t(20)=+1.00;flx_SS(20)=0; flx_states(20)=ppP1p;flx_ostates(20)=ppN1p
    flx_t(21)=+1.00;flx_SS(21)=0; flx_states(21)=ppP2p;flx_ostates(21)=ppN1p
    flx_t(22)=+1.00;flx_SS(22)=0; flx_states(22)=ppP3p;flx_ostates(22)=ppN1p
    flx_t(23)=+1.00;flx_SS(23)=0; flx_states(23)=ppP4p;flx_ostates(23)=ppN1p
    flx_t(24)=+1.00;flx_SS(24)=0; flx_states(24)=ppP5p;flx_ostates(24)=ppN1p
    flx_t(25)=+1.00;flx_SS(25)=0; flx_states(25)=ppP6p;flx_ostates(25)=ppN1p

    ! exPP=(P.c->R1c+R2c+R6c)        (flux):
    flx_calc_nr(4)= 46; flx_CalcIn(4)=iiPel; flx_option(4)=0
    flx_t(26)=+1.00;flx_SS(26)=1; flx_states(26)=ppP1c;flx_ostates(26)=ppR1c
    flx_t(27)=+1.00;flx_SS(27)=1; flx_states(27)=ppP1c;flx_ostates(27)=ppR2c
    flx_t(28)=+1.00;flx_SS(28)=1; flx_states(28)=ppP1c;flx_ostates(28)=ppR6c
    flx_t(29)=+1.00;flx_SS(29)=1; flx_states(29)=ppP2c;flx_ostates(29)=ppR1c
    flx_t(30)=+1.00;flx_SS(30)=1; flx_states(30)=ppP2c;flx_ostates(30)=ppR2c
    flx_t(31)=+1.00;flx_SS(31)=1; flx_states(31)=ppP2c;flx_ostates(31)=ppR6c
    flx_t(32)=+1.00;flx_SS(32)=1; flx_states(32)=ppP3c;flx_ostates(32)=ppR1c
    flx_t(33)=+1.00;flx_SS(33)=1; flx_states(33)=ppP3c;flx_ostates(33)=ppR2c
    flx_t(34)=+1.00;flx_SS(34)=1; flx_states(34)=ppP3c;flx_ostates(34)=ppR6c
    flx_t(35)=+1.00;flx_SS(35)=1; flx_states(35)=ppP4c;flx_ostates(35)=ppR1c
    flx_t(36)=+1.00;flx_SS(36)=1; flx_states(36)=ppP4c;flx_ostates(36)=ppR2c
    flx_t(37)=+1.00;flx_SS(37)=1; flx_states(37)=ppP4c;flx_ostates(37)=ppR6c
    flx_t(38)=+1.00;flx_SS(38)=1; flx_states(38)=ppP5c;flx_ostates(38)=ppR1c
    flx_t(39)=+1.00;flx_SS(39)=1; flx_states(39)=ppP5c;flx_ostates(39)=ppR2c
    flx_t(40)=+1.00;flx_SS(40)=1; flx_states(40)=ppP5c;flx_ostates(40)=ppR6c
    flx_t(41)=+1.00;flx_SS(41)=1; flx_states(41)=ppP6c;flx_ostates(41)=ppR1c
    flx_t(42)=+1.00;flx_SS(42)=1; flx_states(42)=ppP6c;flx_ostates(42)=ppR2c
    flx_t(43)=+1.00;flx_SS(43)=1; flx_states(43)=ppP6c;flx_ostates(43)=ppR6c
    flx_t(44)=+1.00;flx_SS(44)=1; flx_states(44)=ppPcc;flx_ostates(44)=ppR1c
    flx_t(45)=+1.00;flx_SS(45)=1; flx_states(45)=ppPcc;flx_ostates(45)=ppR2c
    flx_t(46)=+1.00;flx_SS(46)=1; flx_states(46)=ppPcc;flx_ostates(46)=ppR6c

    ! resPP=(P.c->O3c)        (flux):
    flx_calc_nr(5)= 53; flx_CalcIn(5)=iiPel; flx_option(5)=0
    flx_t(47)=+1.00;flx_SS(47)=1; flx_states(47)=ppP1c;flx_ostates(47)=ppP1c
    flx_t(48)=+1.00;flx_SS(48)=1; flx_states(48)=ppP2c;flx_ostates(48)=ppP2c
    flx_t(49)=+1.00;flx_SS(49)=1; flx_states(49)=ppP3c;flx_ostates(49)=ppP3c
    flx_t(50)=+1.00;flx_SS(50)=1; flx_states(50)=ppP4c;flx_ostates(50)=ppP4c
    flx_t(51)=+1.00;flx_SS(51)=1; flx_states(51)=ppP5c;flx_ostates(51)=ppP5c
    flx_t(52)=+1.00;flx_SS(52)=1; flx_states(52)=ppP6c;flx_ostates(52)=ppP6c
    flx_t(53)=+1.00;flx_SS(53)=1; flx_states(53)=ppPcc;flx_ostates(53)=ppPcc

    ! ruZTc=(Z.c<-P.c+B1c+Z.c)-(Z.c->R1c+R6c)        (flux):
    flx_calc_nr(6)= 123; flx_CalcIn(6)=iiPel; flx_option(6)=0
    flx_t(54)=-1.00;flx_SS(54)=1; flx_states(54)=ppZ3c;flx_ostates(54)=ppR1c
    flx_t(55)=+1.00;flx_SS(55)=0; flx_states(55)=ppZ3c;flx_ostates(55)=ppB1c
    flx_t(56)=+1.00;flx_SS(56)=0; flx_states(56)=ppZ3c;flx_ostates(56)=ppP1c
    flx_t(57)=+1.00;flx_SS(57)=0; flx_states(57)=ppZ3c;flx_ostates(57)=ppP2c
    flx_t(58)=+1.00;flx_SS(58)=0; flx_states(58)=ppZ3c;flx_ostates(58)=ppP3c
    flx_t(59)=+1.00;flx_SS(59)=0; flx_states(59)=ppZ3c;flx_ostates(59)=ppP4c
    flx_t(60)=+1.00;flx_SS(60)=0; flx_states(60)=ppZ3c;flx_ostates(60)=ppP5c
    flx_t(61)=+1.00;flx_SS(61)=0; flx_states(61)=ppZ3c;flx_ostates(61)=ppP6c
    flx_t(62)=+1.00;flx_SS(62)=0; flx_states(62)=ppZ3c;flx_ostates(62)=ppPcc
    flx_t(63)=+1.00;flx_SS(63)=0; flx_states(63)=ppZ3c;flx_ostates(63)=ppZ3c
    flx_t(64)=+1.00;flx_SS(64)=0; flx_states(64)=ppZ3c;flx_ostates(64)=ppZ4c
    flx_t(65)=+1.00;flx_SS(65)=0; flx_states(65)=ppZ3c;flx_ostates(65)=ppZ2c
    flx_t(66)=+1.00;flx_SS(66)=0; flx_states(66)=ppZ3c;flx_ostates(66)=ppZ5c
    flx_t(67)=+1.00;flx_SS(67)=0; flx_states(67)=ppZ3c;flx_ostates(67)=ppZ6c
    flx_t(68)=-1.00;flx_SS(68)=1; flx_states(68)=ppZ4c;flx_ostates(68)=ppR1c
    flx_t(69)=+1.00;flx_SS(69)=0; flx_states(69)=ppZ4c;flx_ostates(69)=ppB1c
    flx_t(70)=+1.00;flx_SS(70)=0; flx_states(70)=ppZ4c;flx_ostates(70)=ppP1c
    flx_t(71)=+1.00;flx_SS(71)=0; flx_states(71)=ppZ4c;flx_ostates(71)=ppP2c
    flx_t(72)=+1.00;flx_SS(72)=0; flx_states(72)=ppZ4c;flx_ostates(72)=ppP3c
    flx_t(73)=+1.00;flx_SS(73)=0; flx_states(73)=ppZ4c;flx_ostates(73)=ppP4c
    flx_t(74)=+1.00;flx_SS(74)=0; flx_states(74)=ppZ4c;flx_ostates(74)=ppP5c
    flx_t(75)=+1.00;flx_SS(75)=0; flx_states(75)=ppZ4c;flx_ostates(75)=ppP6c
    flx_t(76)=+1.00;flx_SS(76)=0; flx_states(76)=ppZ4c;flx_ostates(76)=ppPcc
    flx_t(77)=+1.00;flx_SS(77)=0; flx_states(77)=ppZ4c;flx_ostates(77)=ppZ3c
    flx_t(78)=+1.00;flx_SS(78)=0; flx_states(78)=ppZ4c;flx_ostates(78)=ppZ4c
    flx_t(79)=+1.00;flx_SS(79)=0; flx_states(79)=ppZ4c;flx_ostates(79)=ppZ2c
    flx_t(80)=+1.00;flx_SS(80)=0; flx_states(80)=ppZ4c;flx_ostates(80)=ppZ5c
    flx_t(81)=+1.00;flx_SS(81)=0; flx_states(81)=ppZ4c;flx_ostates(81)=ppZ6c
    flx_t(82)=-1.00;flx_SS(82)=1; flx_states(82)=ppZ2c;flx_ostates(82)=ppR1c
    flx_t(83)=+1.00;flx_SS(83)=0; flx_states(83)=ppZ2c;flx_ostates(83)=ppB1c
    flx_t(84)=+1.00;flx_SS(84)=0; flx_states(84)=ppZ2c;flx_ostates(84)=ppP1c
    flx_t(85)=+1.00;flx_SS(85)=0; flx_states(85)=ppZ2c;flx_ostates(85)=ppP2c
    flx_t(86)=+1.00;flx_SS(86)=0; flx_states(86)=ppZ2c;flx_ostates(86)=ppP3c
    flx_t(87)=+1.00;flx_SS(87)=0; flx_states(87)=ppZ2c;flx_ostates(87)=ppP4c
    flx_t(88)=+1.00;flx_SS(88)=0; flx_states(88)=ppZ2c;flx_ostates(88)=ppP5c
    flx_t(89)=+1.00;flx_SS(89)=0; flx_states(89)=ppZ2c;flx_ostates(89)=ppP6c
    flx_t(90)=+1.00;flx_SS(90)=0; flx_states(90)=ppZ2c;flx_ostates(90)=ppPcc
    flx_t(91)=+1.00;flx_SS(91)=0; flx_states(91)=ppZ2c;flx_ostates(91)=ppZ3c
    flx_t(92)=+1.00;flx_SS(92)=0; flx_states(92)=ppZ2c;flx_ostates(92)=ppZ4c
    flx_t(93)=+1.00;flx_SS(93)=0; flx_states(93)=ppZ2c;flx_ostates(93)=ppZ2c
    flx_t(94)=+1.00;flx_SS(94)=0; flx_states(94)=ppZ2c;flx_ostates(94)=ppZ5c
    flx_t(95)=+1.00;flx_SS(95)=0; flx_states(95)=ppZ2c;flx_ostates(95)=ppZ6c
    flx_t(96)=-1.00;flx_SS(96)=1; flx_states(96)=ppZ5c;flx_ostates(96)=ppR1c
    flx_t(97)=+1.00;flx_SS(97)=0; flx_states(97)=ppZ5c;flx_ostates(97)=ppB1c
    flx_t(98)=+1.00;flx_SS(98)=0; flx_states(98)=ppZ5c;flx_ostates(98)=ppP1c
    flx_t(99)=+1.00;flx_SS(99)=0; flx_states(99)=ppZ5c;flx_ostates(99)=ppP2c
    flx_t(100)=+1.00;flx_SS(100)=0; flx_states(100)=ppZ5c
    flx_ostates(100)=ppP3c
    flx_t(101)=+1.00;flx_SS(101)=0; flx_states(101)=ppZ5c
    flx_ostates(101)=ppP4c
    flx_t(102)=+1.00;flx_SS(102)=0; flx_states(102)=ppZ5c
    flx_ostates(102)=ppP5c
    flx_t(103)=+1.00;flx_SS(103)=0; flx_states(103)=ppZ5c
    flx_ostates(103)=ppP6c
    flx_t(104)=+1.00;flx_SS(104)=0; flx_states(104)=ppZ5c
    flx_ostates(104)=ppPcc
    flx_t(105)=+1.00;flx_SS(105)=0; flx_states(105)=ppZ5c
    flx_ostates(105)=ppZ3c
    flx_t(106)=+1.00;flx_SS(106)=0; flx_states(106)=ppZ5c
    flx_ostates(106)=ppZ4c
    flx_t(107)=+1.00;flx_SS(107)=0; flx_states(107)=ppZ5c
    flx_ostates(107)=ppZ2c
    flx_t(108)=+1.00;flx_SS(108)=0; flx_states(108)=ppZ5c
    flx_ostates(108)=ppZ5c
    flx_t(109)=+1.00;flx_SS(109)=0; flx_states(109)=ppZ5c
    flx_ostates(109)=ppZ6c
    flx_t(110)=-1.00;flx_SS(110)=1; flx_states(110)=ppZ6c
    flx_ostates(110)=ppR1c
    flx_t(111)=+1.00;flx_SS(111)=0; flx_states(111)=ppZ6c
    flx_ostates(111)=ppB1c
    flx_t(112)=+1.00;flx_SS(112)=0; flx_states(112)=ppZ6c
    flx_ostates(112)=ppP1c
    flx_t(113)=+1.00;flx_SS(113)=0; flx_states(113)=ppZ6c
    flx_ostates(113)=ppP2c
    flx_t(114)=+1.00;flx_SS(114)=0; flx_states(114)=ppZ6c
    flx_ostates(114)=ppP3c
    flx_t(115)=+1.00;flx_SS(115)=0; flx_states(115)=ppZ6c
    flx_ostates(115)=ppP4c
    flx_t(116)=+1.00;flx_SS(116)=0; flx_states(116)=ppZ6c
    flx_ostates(116)=ppP5c
    flx_t(117)=+1.00;flx_SS(117)=0; flx_states(117)=ppZ6c
    flx_ostates(117)=ppP6c
    flx_t(118)=+1.00;flx_SS(118)=0; flx_states(118)=ppZ6c
    flx_ostates(118)=ppPcc
    flx_t(119)=+1.00;flx_SS(119)=0; flx_states(119)=ppZ6c
    flx_ostates(119)=ppZ3c
    flx_t(120)=+1.00;flx_SS(120)=0; flx_states(120)=ppZ6c
    flx_ostates(120)=ppZ4c
    flx_t(121)=+1.00;flx_SS(121)=0; flx_states(121)=ppZ6c
    flx_ostates(121)=ppZ2c
    flx_t(122)=+1.00;flx_SS(122)=0; flx_states(122)=ppZ6c
    flx_ostates(122)=ppZ5c
    flx_t(123)=+1.00;flx_SS(123)=0; flx_states(123)=ppZ6c
    flx_ostates(123)=ppZ6c

    ! rrPTo=(O2o->*)        (flux):
    flx_calc_nr(7)= 124; flx_CalcIn(7)=iiPel; flx_option(7)=0
    flx_t(124)=+1.00;flx_SS(124)=1; flx_states(124)=ppO2o
    flx_ostates(124)=ppO2o

    ! fR2B1c=R2c->B1c        (flux):
    flx_calc_nr(8)= 125; flx_CalcIn(8)=iiPel; flx_option(8)=0
    flx_t(125)=+1.00;flx_SS(125)=1; flx_states(125)=ppR2c
    flx_ostates(125)=ppB1c


    ! grsPPm2=(P.c<- O3c)        (flux perm2):
    flx_calc_nr(9)= 132; flx_CalcIn(9)=iiPel; flx_option(9)=2
    flx_t(126)=+1.00;flx_SS(126)=0; flx_states(126)=ppP1c
    flx_ostates(126)=ppP1c
    flx_t(127)=+1.00;flx_SS(127)=0; flx_states(127)=ppP2c
    flx_ostates(127)=ppP2c
    flx_t(128)=+1.00;flx_SS(128)=0; flx_states(128)=ppP3c
    flx_ostates(128)=ppP3c
    flx_t(129)=+1.00;flx_SS(129)=0; flx_states(129)=ppP4c
    flx_ostates(129)=ppP4c
    flx_t(130)=+1.00;flx_SS(130)=0; flx_states(130)=ppP5c
    flx_ostates(130)=ppP5c
    flx_t(131)=+1.00;flx_SS(131)=0; flx_states(131)=ppP6c
    flx_ostates(131)=ppP6c
    flx_t(132)=+1.00;flx_SS(132)=0; flx_states(132)=ppPcc
    flx_ostates(132)=ppPcc

    ! netPPm2=(P.c<- O3c)-(P.c->O3c)        (flux perm2):
    flx_calc_nr(10)= 146; flx_CalcIn(10)=iiPel; flx_option(10)=2
    flx_t(133)=+1.00;flx_SS(133)=0; flx_states(133)=ppP1c
    flx_ostates(133)=ppP1c
    flx_t(134)=-1.00;flx_SS(134)=1; flx_states(134)=ppP1c
    flx_ostates(134)=ppP1c
    flx_t(135)=+1.00;flx_SS(135)=0; flx_states(135)=ppP2c
    flx_ostates(135)=ppP2c
    flx_t(136)=-1.00;flx_SS(136)=1; flx_states(136)=ppP2c
    flx_ostates(136)=ppP2c
    flx_t(137)=+1.00;flx_SS(137)=0; flx_states(137)=ppP3c
    flx_ostates(137)=ppP3c
    flx_t(138)=-1.00;flx_SS(138)=1; flx_states(138)=ppP3c
    flx_ostates(138)=ppP3c
    flx_t(139)=+1.00;flx_SS(139)=0; flx_states(139)=ppP4c
    flx_ostates(139)=ppP4c
    flx_t(140)=-1.00;flx_SS(140)=1; flx_states(140)=ppP4c
    flx_ostates(140)=ppP4c
    flx_t(141)=+1.00;flx_SS(141)=0; flx_states(141)=ppP5c
    flx_ostates(141)=ppP5c
    flx_t(142)=-1.00;flx_SS(142)=1; flx_states(142)=ppP5c
    flx_ostates(142)=ppP5c
    flx_t(143)=+1.00;flx_SS(143)=0; flx_states(143)=ppP6c
    flx_ostates(143)=ppP6c
    flx_t(144)=-1.00;flx_SS(144)=1; flx_states(144)=ppP6c
    flx_ostates(144)=ppP6c
    flx_t(145)=+1.00;flx_SS(145)=0; flx_states(145)=ppPcc
    flx_ostates(145)=ppPcc
    flx_t(146)=-1.00;flx_SS(146)=1; flx_states(146)=ppPcc
    flx_ostates(146)=ppPcc

    ! J1_PTc=P.c        (sedimentation 1):
    flx_calc_nr(11)= 153; flx_CalcIn(11)=iiBen; flx_option(11)=20
    flx_t(147)=+1.00;flx_SS(147)=1; flx_states(147)=ppP1c;flx_ostates(147)=1
    flx_t(148)=+1.00;flx_SS(148)=1; flx_states(148)=ppP2c;flx_ostates(148)=1
    flx_t(149)=+1.00;flx_SS(149)=1; flx_states(149)=ppP3c;flx_ostates(149)=1
    flx_t(150)=+1.00;flx_SS(150)=1; flx_states(150)=ppP4c;flx_ostates(150)=1
    flx_t(151)=+1.00;flx_SS(151)=1; flx_states(151)=ppP5c;flx_ostates(151)=1
    flx_t(152)=+1.00;flx_SS(152)=1; flx_states(152)=ppP6c;flx_ostates(152)=1
    flx_t(153)=+1.00;flx_SS(153)=1; flx_states(153)=ppPcc;flx_ostates(153)=1

    ! J1_R6c=R6c        (sedimentation 1):
    flx_calc_nr(12)= 154; flx_CalcIn(12)=iiBen; flx_option(12)=20
    flx_t(154)=+1.00;flx_SS(154)=1; flx_states(154)=ppR6c;flx_ostates(154)=1

    ! J1_R2c=R2c        (sedimentation 1):
    flx_calc_nr(13)= 155; flx_CalcIn(13)=iiBen; flx_option(13)=20
    flx_t(155)=+1.00;flx_SS(155)=1; flx_states(155)=ppR2c;flx_ostates(155)=1

    ! J0_Ndn=N.n        (sum bot):
    flx_calc_nr(14)= 157; flx_CalcIn(14)=iiBen; flx_option(14)=12
    flx_t(156)=+1.00;flx_SS(156)=1; flx_states(156)=ppN3n
    flx_ostates(156)=ppN3n
    flx_t(157)=+1.00;flx_SS(157)=1; flx_states(157)=ppN4n
    flx_ostates(157)=ppN4n

    ! J0_Ntn=N.n+P.n+R6n        (sum bot):
    flx_calc_nr(15)= 166; flx_CalcIn(15)=iiBen; flx_option(15)=12
    flx_t(158)=+1.00;flx_SS(158)=1; flx_states(158)=ppN3n
    flx_ostates(158)=ppN3n
    flx_t(159)=+1.00;flx_SS(159)=1; flx_states(159)=ppN4n
    flx_ostates(159)=ppN4n
    flx_t(160)=+1.00;flx_SS(160)=1; flx_states(160)=ppP1n
    flx_ostates(160)=ppP1n
    flx_t(161)=+1.00;flx_SS(161)=1; flx_states(161)=ppP2n
    flx_ostates(161)=ppP2n
    flx_t(162)=+1.00;flx_SS(162)=1; flx_states(162)=ppP3n
    flx_ostates(162)=ppP3n
    flx_t(163)=+1.00;flx_SS(163)=1; flx_states(163)=ppP4n
    flx_ostates(163)=ppP4n
    flx_t(164)=+1.00;flx_SS(164)=1; flx_states(164)=ppP5n
    flx_ostates(164)=ppP5n
    flx_t(165)=+1.00;flx_SS(165)=1; flx_states(165)=ppP6n
    flx_ostates(165)=ppP6n
    flx_t(166)=+1.00;flx_SS(166)=1; flx_states(166)=ppR6n
    flx_ostates(166)=ppR6n

    ! ntr_Ndn=N3n+N4n        (nettransport +):
    flx_calc_nr(16)= 168; flx_CalcIn(16)=iiBen; flx_option(16)=61
    flx_t(167)=+1.00;flx_SS(167)=1; flx_states(167)=ppN3n;flx_ostates(167)=1
    flx_t(168)=+1.00;flx_SS(168)=1; flx_states(168)=ppN4n;flx_ostates(168)=1

    ! qqriv_w= 1        (riv):
    flx_calc_nr(17)= 169; flx_CalcIn(17)=iiBen; flx_option(17)=50
    flx_t(169)=+1.00;flx_SS(169)=1; flx_states(169)=0;flx_ostates(169)=0

    ! qqrivNtn=N3n+N4n+R1n+R6n        (riv):
    flx_calc_nr(18)= 173; flx_CalcIn(18)=iiBen; flx_option(18)=50
    flx_t(170)=+1.00;flx_SS(170)=1; flx_states(170)=ppN3n;flx_ostates(170)=0
    flx_t(171)=+1.00;flx_SS(171)=1; flx_states(171)=ppN4n;flx_ostates(171)=0
    flx_t(172)=+1.00;flx_SS(172)=1; flx_states(172)=ppR1n;flx_ostates(172)=0
    flx_t(173)=+1.00;flx_SS(173)=1; flx_states(173)=ppR6n;flx_ostates(173)=0

    ! qqrivNtp=N1p+R1p+R6p        (riv):
    flx_calc_nr(19)= 176; flx_CalcIn(19)=iiBen; flx_option(19)=50
    flx_t(174)=+1.00;flx_SS(174)=1; flx_states(174)=ppN1p;flx_ostates(174)=0
    flx_t(175)=+1.00;flx_SS(175)=1; flx_states(175)=ppR1p;flx_ostates(175)=0
    flx_t(176)=+1.00;flx_SS(176)=1; flx_states(176)=ppR6p;flx_ostates(176)=0

  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ! Start the allocation of vars for  track of constituents
  !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        ! In this Setup is tracking not active


  end subroutine

