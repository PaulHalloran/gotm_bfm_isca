!$Id: $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_bfm --- BFM bio model \label{sec:bio_bfm}
!
! !INTERFACE:
   module bio_bfm
!
! !DESCRIPTION:
!
!
! !USES:
!  default: all is private.
   use bio_var
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio_bfm, pointers_gotm_bfm,            &
          var_info_bfm, envforcing_bfm, do_bio_bfm,   &
          allocate_memory_bfm,reset_diagonal,         &
          test_on_all_negative_states,                &
          test_mass_conservation,                     &
          test_on_negative_states, end_bio_bfm,       &
          do_bfm_river_loads,assign_adv_rates,        &
          CalcVertFluxAtLev,calc_sigma_depth,         &
          DeriveFromGotm
!
!
! !PRIVATE DATA MEMBERS:
   REALTYPE,dimension(:),allocatable :: wx
!
! !REVISION HISTORY:
!  Original author(s): Marcello Vichi
!  from a template by Hans Burchard & Karsten Bolding
!
!  $Log: $
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the template bio module
!
! !INTERFACE:
   subroutine init_bio_bfm(nlev,out_unit)
!
! !DESCRIPTION:
!  Here, the main communication of array dimensions between GOTM
!  and BFM is done.
!
!
! !USES:
   use mem, only: NO_D3_BOX_STATES, NO_BOXES,          &
                  NO_BOXES_X, NO_BOXES_Y, NO_BOXES_Z,  &
                  NO_D2_BOX_STATES, NO_BOXES_XY,       &
                  NO_D2_BOX_DIAGNOSS, NO_D3_BOX_DIAGNOSS,&
                  NO_D2_BOX_FLUX, NO_D3_BOX_FLUX,&
                  NO_STATES

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,          intent(in)   :: nlev
   integer,          intent(in)   :: out_unit
!
   integer :: i,rc
! !REVISION HISTORY:
!  Original author(s): Marcello Vichi
!  from a template by Hans Burchard & Karsten Bolding
!!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'init_bio_bfm'


   ! BFM  --> GOTM
   numc  = NO_D3_BOX_STATES
   numbc = NO_D2_BOX_STATES
   numc_diag  = NO_D3_BOX_DIAGNOSS
   numbc_diag = NO_D2_BOX_DIAGNOSS
   numc_flux  = NO_D3_BOX_FLUX
   numbc_flux = NO_D2_BOX_FLUX
   ! numcc is the number of transported variables
   numcc = numc

   ! GOTM --> BFM
   NO_BOXES_X  = 1
   NO_BOXES_Y  = 1
   NO_BOXES_Z  = nlev
   NO_BOXES    = NO_BOXES_X * NO_BOXES_Y * NO_BOXES_Z
   NO_BOXES_XY = NO_BOXES_X * NO_BOXES_Y
   NO_STATES   = NO_D3_BOX_STATES * NO_BOXES +   &
                 NO_D2_BOX_STATES * NO_BOXES_XY
   !LOGUNIT = out_unit

   LEVEL3 'pelagic variables =',numc
   LEVEL3 'pelagic transported variables =',numcc
   LEVEL3 'benthic variables =',numbc
   LEVEL3 'pelagic variables prepared for output',numc_diag
   LEVEL3 'benthic variables prepared for output',numbc_diag
   LEVEL3 'NO_BOXES_X=',NO_BOXES_X
   LEVEL3 'NO_BOXES_Y=',NO_BOXES_Y
   LEVEL3 'NO_BOXES_Z=',NO_BOXES_Z
   LEVEL3 'NO_BOXES=',NO_BOXES
   LEVEL3 'NO_BOXES_XY=',NO_BOXES_XY
   LEVEL3 'NO_STATES=',NO_STATES
   LEVEL3 'Step 1 of GOTM <-> BFM initialisation done ...'

!  sfl=_ZERO_
!  sfl_read=_ZERO_
   allocate(wx(1:NO_BOXES),stat=rc)
   return

   end subroutine init_bio_bfm
!EOC
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Initialize BFM and GETM shared memory
!
! !INTERFACE:
   subroutine pointers_gotm_bfm()
!
! !DESCRIPTION:
! Allocate pointers to GOTM memory
!
! !USES:
   use mem, only: D3STATE,D3SOURCE,D3SINK,D3STATETYPE, &
                  D3DIAGNOS,D2STATE,D2SOURCE,D2SINK,   &
                  D2STATETYPE,NO_BOXES,NO_BOXES_XY,    &
                  D2DIAGNOS,NO_D2_BOX_STATES,          &
                  NO_D2_BOX_DIAGNOSS

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Marcello Vichi
!
! !LOCAL VARIABLES:

   !---------------------------------------------
   ! Pelagic pointers
   !---------------------------------------------
   D3STATE  => cc(:,1:NO_BOXES)
   D3SOURCE => pp(:,:,1:NO_BOXES)
   D3SINK   => dd(:,:,1:NO_BOXES)
   D3STATETYPE => pelvar_type
   if (numc_diag > 0) D3DIAGNOS => diag(:,1:NO_BOXES)

   !---------------------------------------------
   ! Benthic pointers
   !---------------------------------------------
   if (bio_setup >=2 ) then
      D2STATE  => ccb(:,1:NO_BOXES_XY)
      D2SOURCE => ppb(:,:,1:NO_BOXES_XY)
      D2SINK   => ddb(:,:,1:NO_BOXES_XY)
      D2STATETYPE => benvar_type
      if (numbc_diag>0) D2DIAGNOS => diagb(:,1:NO_BOXES_XY)
   else
      ! allocate memory anyhow to avoid problems with BFM allocation
      allocate(D2STATE(1:NO_D2_BOX_STATES,1:NO_BOXES_XY))
      allocate(D2SOURCE(1:NO_D2_BOX_STATES,1:NO_D2_BOX_STATES,1:NO_BOXES_XY))
      allocate(D2SINK(1:NO_D2_BOX_STATES,1:NO_D2_BOX_STATES,1:NO_BOXES_XY))
      allocate(D2STATETYPE(1:NO_D2_BOX_STATES ))
      if (numbc_diag>0)  &
         allocate(D2DIAGNOS(1:NO_D2_BOX_DIAGNOSS,1:NO_BOXES_XY))
   end if

   end subroutine pointers_gotm_bfm
!EOC
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Providing info on variables
!
! !INTERFACE:
   subroutine var_info_bfm()
!
! !DESCRIPTION:
!  This subroutine provides information on the variables. To be used
!  when storing data in NetCDF files.
!
! !USES:
   use mem
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Marcello Vichi
!  from a template by Hans Burchard & Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   call set_var_info_bfm
   return
   end subroutine var_info_bfm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Light and other environmental forcing used in the BFM
!
! !INTERFACE
   subroutine envforcing_bfm(nlev,h,t,s,rho,I_0,dl,uwind_gotm,vwind_gotm, &
          u_taub,bioshade_feedback,bioshade,abioshade)
!
! !DESCRIPTION
!
! !USES
! BFM modules
use constants, ONLY: E2W
use mem_Param, ONLY: p_eps0, p_epsESS, p_PAR,p_small
use mem,       ONLY: NO_BOXES, PhytoPlankton, xEPS, ESS, ERHO,SUNQ, &
                     iiPhytoPlankton, iiL, Chla, ETW, ESW, Wind,R9x,    &
                     EUWIND,EVWIND,ETAUB,OCDepth,Depth, EIR, ABIO_eps,p_eps0
use mem_Param,  ONLY: p_eps0, p_epsESS,p_poro
use global_interface,   ONLY: eTq


IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer                              :: nlev
   REALTYPE, intent(in)                 :: h(0:nlev)
   REALTYPE, intent(in)                 :: t(0:nlev)
   REALTYPE, intent(in)                 :: rho(0:nlev)
   REALTYPE, intent(in)                 :: s(0:nlev)
   REALTYPE, intent(in)                 :: I_0
   REALTYPE, intent(in)                 :: dl
   REALTYPE, intent(in)                 :: uwind_gotm,vwind_gotm,u_taub
   logical, intent(in)                  :: bioshade_feedback
   REALTYPE,intent(in),optional        :: abioshade(0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)                :: bioshade(0:nlev)!
! !REVISION HISTORY:
!  Original author(s): Marcello Vichi
!  from a template by Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   integer             :: i,n
   REALTYPE            :: psilt
!EOP
!-----------------------------------------------------------------------
!BOC

!   LEVEL2 'calculating environmental forcings for the BFM'

   !---------------------------------------------
   ! Assign depths of layers
   ! temperature and salinity
   !---------------------------------------------
   Depth(:) = h(1:nlev)
    OCDepth(NO_BOXES) =depth(NO_BOXES)
    EUWIND(1)=uwind_gotm
    EVWIND(1)=vwind_gotm
    Wind= sqrt( EUWIND(1) * EUWIND(1) + EVWIND(1) *EVWIND(1))
    ETAUB(1)=u_taub
    do n=NO_BOXES-1,1,-1
       OCDepth(n)=depth(n)+ OCDepth(n+1)
    enddo
   ETW(:) = t(1:nlev)
   ESW(:) = s(1:nlev)
   ERHO(:) = rho(1:nlev)
   psilt=(p_poro(1) - 0.38662 )/ 0.00415
!  ESS(:) = 10000.0 * min(1.0,20.0/OCDepth(1))* max(0.5,psilt)/7.0 
!  ESS(:) = 10000.0 * min(1.0,10.0/OCDepth(1))* max(0.5,psilt)/7.0 &
!                     /eTq(  ETW(nlev), 2.0D+00) 
   ESS(:)=R9x(:)
   p_eps0=1.17692307692-0.0307692307692*min(35.0,ESW(1))


            
   !---------------------------------------------
   ! Compute extinction coefficient
   !---------------------------------------------

   if (p_eps0 ==0.0 ) then
     ABIO_eps(:) = abioshade(1:nlev)
   end if

   if (bioshade_feedback) then
     ! 0= Special calculation  of vertical extinction only controlled by
     !   biological constituents for use in gotm
     ! 1= full calulation used to caluclate bilolgical vertical extinction
     ! 2= Special calculation  of vertical extinction only controlled by
     !   biological constituents and silt when dilt inis include in the BFM part.
     call  CalcVerticalExtinction(0)
     bioshade(1)=1.0
     do i=nlev,2,-1
       bioshade(i-1) = bioshade(i)*exp(-xEPS(i)*Depth(i))
     end do
     bioshade(1:nlev) =  bioshade(1:nlev)*exp(-xEPS(:)*Depth(:)*0.5)
   endif

   !---------------------------------------------
   ! Notice that irradiance in the BFM is in
   ! uE/m2/s and is defined at the top of each
   ! layer (the derivation of the middle-layer
   ! EIR for production is done in the
   ! Phytoplankton routines)
   !---------------------------------------------
   SUNQ=dl
   call  CalcVerticalExtinction(1)
   EIR(nlev) = max(p_small,p_PAR*I_0/E2W)
   do i=nlev,2,-1
     EIR(i-1) = EIR(i)*exp(-xEPS(i)*Depth(i))
   end do

   !---------------------------------------------
   ! bioshade is instead derived in the
   ! middle of the layer and it's non-dimensional
   !---------------------------------------------

   end subroutine envforcing_bfm
!EOC
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Right hand sides of the BFM model
!
! !INTERFACE
   subroutine do_bio_bfm(first)
!
! !DESCRIPTION
!  This subroutine is a wrapper for the computing core of the BFM
!
! !USES
   use mem_param, only: AssignAirPelFluxesInBFMFlag,AssignPelBenFluxesInBFMFlag,CalcBenthicFlag
   use mem, only: sediPI, sediR6, sediRZ, sediR2,iiC,iiN,iiP,iiS,iiL, &
                  ppR2c, ppRZc, ppR6c, ppR6n, ppR6p, ppR6s, NO_BOXES_Z,   &
                  ppR1c, ppR1n, ppR1p,ppO3c,NO_BOXES,   &
                  ppO2o,ppN1p,ppN3n,ppN4n,ppN5s,ppN6r,ppO3h,  &
                  NO_D3_BOX_STATES, Depth,totsysn,              &
                  ppPhytoPlankton,iiPhytoPlankton, &
                  PELBOTTOM, PELSURFACE, &
                  jK3G4n,jK13K3n,jK34K24n,flN3O4n

   use constants,  only: SEC_PER_DAY
   use gotm_error_msg, only:gotm_error,get_d3_model_flag_from_getm

   IMPLICIT NONE
!
   logical,intent(in)          :: first


   logical                     :: ll_larger,d3_model_flag
   integer                     :: n,k,i,j,l
   REALTYPE                    ::tl
   REALTYPE                    :: topm3psec,corr,Nloss,Hloss
   REALTYPE                    :: shelp(1:NO_BOXES)
!See "USE association" above
!
! !REVISION HISTORY:
!  Original author(s): Marcello Vichi
!  from template by Hans Burchard, Karsten Bolding
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC

   !---------------------------------------------
   ! Compute BFM terms
   !---------------------------------------------
   call SiltDynamics
   call EcologyDynamics
   call get_d3_model_flag_from_getm(d3_model_flag)
   Nloss=0.0;Hloss =0.0;
   if ( .not.d3_model_flag) then
      if (CalcBenthicFlag==3)  then
        Nloss=0.0;
        Nloss=jK3G4n(1)-jK13K3n(1)-jK34K24n(1)+sum(flN3O4n(:)*Depth(:));
        Hloss=0.0;
!       Hloss= -0.5 * jK3G4n(1)- 0.5*sum(flN3O4n(:)*Depth(:))
      elseif (CalcBenthicFlag==0)  then
        Nloss=sum(flN3O4n(:)*Depth(:));
        Hloss=sum(flN3O4n(:)*Depth(:));
      endif
   endif
   !---------------------------------------------
   ! Surface fluxes
   !---------------------------------------------
   if ( bio_setup ==2 ) return
!  topm3psec=_ONE_/Depth(NO_BOXES_Z)/ SEC_PER_DAY
   topm3psec=_ONE_/SEC_PER_DAY
   sfl=0.0D+00;
   if ( .NOT. AssignAirPelFluxesInBFMFlag ) then
     sfl(ppO2o) =   PELSURFACE(ppO2o,1) *topm3psec
     if ( ppO3C > 0 ) sfl(ppO3c) =   PELSURFACE(ppO3c,1) *topm3psec
   endif
   select case (surface_flux_method)
        case (-1)! absolutely nothing
        case (0) ! constant
           sfl(ppN3n) =   0.12  *topm3psec
           sfl(ppN4n) =   0.09  *topm3psec
           !this is called here to test track when the 1d model is used in 1D-mode.
           !In this case get sfl(pptrN3n) the same value as slf(ppN3n)
           ! It work only if d3_model_flag ==flase and if trakcing is active.
!          call fill_sfl(d3_model_flag,ppN3n,numc,sfl)
!          call fill_sfl(d3_model_flag,ppN4n,numc,sfl)
           sfl(ppN1p) =   0.0  !0.0
        case (1) ! from file via sfl_read
           ! fluxes are in mmol m-2 d-1
           sfl(ppN3n) =    sfl_N3n  *topm3psec
           sfl(ppN4n) =    sfl_N4n  *topm3psec
!          call fill_sfl(d3_model_flag,ppN3n,numc,sfl)
!          call fill_sfl(d3_model_flag,ppN4n,numc,sfl)
        case (3) ! sfl array filled externally - for 3D models
         ! option 3 works only in 1D-mode!!!!!!!
           sfl(ppN3n)= Nloss * topm3psec
           PELSURFACE(ppN3n,1)=Nloss
           if ( ppO3h> 0) sfl(ppO3h)= Hloss * topm3psec
           PELSURFACE(ppO3h,1)=Hloss
!          call fill_sfl(d3_model_flag,ppN3n,numc,sfl)
!          call fill_sfl(d3_model_flag,ppN4n,numc,sfl)
        case default
   end select

   !---------------------------------------------
   ! Bottom fluxes
   !---------------------------------------------
   topm3psec=1.0/Depth(1)/ SEC_PER_DAY
   bfl=0.0;
   if ((bio_setup == 3 ) .and. ( .NOT.AssignPelBenFluxesInBFMFlag)) then

      bfl(ppRZc) = PELBOTTOM(ppRZc,1)*topm3psec
      bfl(ppR6c) = PELBOTTOM(ppR6c,1)*topm3psec
      bfl(ppR6n) = PELBOTTOM(ppR6n,1)*topm3psec
      bfl(ppR6p) = PELBOTTOM(ppR6p,1)*topm3psec
      bfl(ppR6s) = PELBOTTOM(ppR6s,1)*topm3psec

      bfl(ppR1c) =  PELBOTTOM(ppR1c,1)*topm3psec
      bfl(ppR1n) =  PELBOTTOM(ppR1n,1)*topm3psec
      bfl(ppR1p) =  PELBOTTOM(ppR1p,1)*topm3psec

      bfl(ppO2o) = PELBOTTOM(ppO2o,1)*topm3psec
      bfl(ppN1p) = PELBOTTOM(ppN1p,1)*topm3psec
      bfl(ppN3n) = PELBOTTOM(ppN3n,1)*topm3psec
      bfl(ppN4n) = PELBOTTOM(ppN4n,1)*topm3psec
      bfl(ppN5s) = PELBOTTOM(ppN5s,1)*topm3psec
      bfl(ppN6r) = PELBOTTOM(ppN6r,1)*topm3psec

      do i=1,iiPhytoPlankton
        k=ppPhytoPlankton(i,iiC) 
        bfl(k) = PELBOTTOM(k,1)*topm3psec
        k=ppPhytoPlankton(i,iiN) 
        bfl(k) = PELBOTTOM(k,1)*topm3psec
        k=ppPhytoPlankton(i,iiP) 
        bfl(k) = PELBOTTOM(k,1)*topm3psec
        k=ppPhytoPlankton(i,iiL) 
        bfl(k) = PELBOTTOM(k,1)*topm3psec
        k=ppPhytoPlankton(i,iiS)
        if ( k > 0 ) bfl(k) = PELBOTTOM(k,1)*topm3psec
      enddo
   endif
   end subroutine do_bio_bfm
!EOC
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Assign  adv_rates
!
! !INTERFACE
   subroutine CalcVertFluxAtLev(statenr,lev,nlev,Depth,dt,out)
!
! !DESCRIPTION
! !USES
   use mem,only: PELBOTTOM
   use constants,  only: SEC_PER_DAY
   IMPLICIT NONE
!
   integer,intent(IN)          :: statenr
   integer,intent(IN)          :: lev
   integer,intent(IN)          :: nlev
   REALTYPE,intent(IN)         :: Depth(1:nlev)
   REALTYPE,intent(IN)         :: dt
   REALTYPE,intent(OUT)        :: out

   if ( lev > 0 ) then
       ! rate calculate in time unit of physical model (secs)
       out= sum((cc_before_transport(statenr,lev+1:nlev) &
               -cc(statenr,lev+1:nlev))*Depth(lev+1:nlev))/dt
   elseif ( lev==0 ) then
       ! rate calculate transferred from time unit of eco model 
       ! to the one  of physical model (secs)
       out=PELBOTTOM(statenr,1)/SEC_PER_DAY;
   endif
   return
   end subroutine CalcVertFluxAtLev
!EOC
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Assign  adv_rates
!
! !INTERFACE
   subroutine assign_adv_rates(dt)
!
! !DESCRIPTION
!  This subroutine is a wrapper for the computing core of the BFM
!
! !USES
   use mem, only: iiC,iiN,iiP,iiS,iiL, &
                  ppR2c, ppR3c,ppP6c, ppR6n, ppR6p, ppR6s, NO_BOXES_Z,   &
                  ppR1c, ppR1n, ppR1p,   &
                  NO_D3_BOX_STATES, OCDepth,Depth,              &
                  D3DIAGNOS,iiPELSINKREF, &
                  PELBOTTOM, PELSURFACE
   use constants,  only: SEC_PER_DAY
   use gotm_error_msg, only:gotm_error

   IMPLICIT NONE
!
   REALTYPE,intent(in)          :: dt

   logical                     :: ll_larger
   integer                     :: n,k,i,j,l,ldep
   REALTYPE                    :: topm3psec,corr2
   REALTYPE                    :: corr(1:NO_BOXES_Z)        

   !---------------------------------------------
   ! Transfer sinking velocities (m/d -> m/s)
   !---------------------------------------------
   if ( bio_setup ==2 ) return
   ldep=1
   do j=1,NO_D3_BOX_STATES
     i= iiPELSINKREF(j) 
     if ( i > 0 ) then
        ll_larger=(maxval(abs(D3DIAGNOS(i,1:NO_BOXES_Z)))>0.001)
        c1dimz(NO_BOXES_Z)=0.0;
        if ( ll_larger) then
           c1dimz(1:NO_BOXES_Z-1)=(Depth(2:NO_BOXES_Z)*D3DIAGNOS(i,1:NO_BOXES_Z-1) &
                            +Depth(1:NO_BOXES_Z-1)*D3DIAGNOS(i,2:NO_BOXES_Z))/ &
                                (Depth(1:NO_BOXES_Z)+Depth(2:NO_BOXES_Z-1))
           corr=min(OCDepth(ldep)*rel_max_sedi_rate,abs(c1dimz(1:NO_BOXES_Z)))/ &
                                       (1.0D-80+abs(c1dimz(1:NO_BOXES_Z)))
           corr=corr*OCDepth(ldep)/(5.0+OCdepth(ldep))
           ws(j,1:NO_BOXES_Z) = -c1dimz(1:NO_BOXES_Z)/SEC_PER_DAY*corr(1:NO_BOXES_Z)
        else
           ws(j,1:NO_BOXES_Z) = 0.0;
        endif
        llws(j)=ll_larger
        ws(j,0)= ws(j,1)
     elseif (i <0) then
        i=-i
        if ( i>=j) stop'error' 
        ws(j,0:NO_BOXES_Z) =ws(i,0:NO_BOXES_Z)
        llws(j)=llws(i)
     endif
   enddo

   return

   end subroutine assign_adv_rates
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Reset diagonal id a 3d array
!
! !INTERFACE:
   subroutine reset_diagonal(n,pp)
!
! !DESCRIPTION:
!    Reset of the diagonal
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                    :: n
   REALTYPE,dimension(:,:,:),intent(inout) :: pp
!
! !REVISION HISTORY:
!  Original author(s): Piet Ruardij
!
! !LOCAL VARIABLES:
   integer                   :: i
!EOP
!-----------------------------------------------------------------------
!BOC
     do i=1,n
       pp(i,i,:) = _ZERO_
     end do

   return
   end subroutine reset_diagonal
!EOC
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: allocate_bfm
!
! !INTERFACE:
        subroutine allocate_memory_bfm(nlev)
!
! !INPUT PARAMETERS:
        implicit none
        integer,intent(IN)            ::nlev
!
! !LOCAL VARAIBELS:
   integer                   :: rc
!
! !DESCRIPTION:
!
! !BUGS:
!
! !SEE ALSO:
!
! !SYSTEM ROUTINES:
!
! !FILES USED:
!
! !REVISION HISTORY:
!
!  28-04-2006  Piet Ruardij Initial code.
!
!EOP
!-------------------------------------------------------------------------
!BOC

   if ( numc_diag > 0 ) then
     allocate(diag(1:numc_diag,0:nlev),stat=rc)
     if (rc /= 0) STOP 'allocate_memory_bfm: Error allocating (cc)'


     diag=_ZERO_                                                     !BFM
   endif


   if (bio_setup >= 2) then                                         !BFM
     ! allocate benthic state variables                             !BFM
     allocate(ccb(1:numbc,0:1),stat=rc)                             !BFM
     if (rc /= 0) STOP 'allocate_memory_bfm: Error allocating (ccb)'           !BFM
     allocate(ppb(1:numbc,1:numbc,0:1),stat=rc)                     !BFM
     if (rc /= 0) STOP 'allocate_memory_bfm: Error allocating (ppb)'           !BFM
     allocate(ddb(1:numbc,1:numbc,0:1),stat=rc)                     !BFM
     if (rc /= 0) STOP 'allocate_memory_bfm: Error allocating (ppb)'           !BFM

     ccb=_ZERO_                                                     !BFM
     ppb=_ZERO_                                                     !BFM
     ddb=_ZERO_                                                     !BFM

     ! allocate variable holding type and save attributes           !BFM
     allocate(benvar_type(1:numbc),stat=rc)                         !BFM
     if (rc /= 0) STOP 'allocate_memory_bfm: Error allocating (benvar_type)'   !BFM
     benvar_type = 0

     if ( numbc_diag > 0 ) then
       allocate(diagb(1:numbc_diag,0:1),stat=rc)
       if (rc /= 0) STOP 'allocate_memory_bfm: Error allocating (cc)'

       diagb=_ZERO_                                                     !BFM
     endif

   end if



 end subroutine allocate_memory_bfm
!EOC
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Test negative concentrations
!
! !INTERFACE:
       subroutine test_on_negative_states ( statenr,lldeep, h, nlev, after, c1, error,counter )
!
! !USES:
       use gotm_error_msg, only:set_warning_for_getm
       IMPLICIT NONE
!
! !INPUT PARAMETERS:
       integer,intent(in)                      :: statenr
       integer,intent(in)                      :: nlev
       logical,intent(in)                      :: lldeep
       REALTYPE,intent(in)                     :: h(0:nlev)
       character(len=*),intent(IN)             :: after
!
! !OUTPUT PARAMETERS:
       REALTYPE,intent(inout)                  :: c1(0:nlev)
       integer,intent(OUT)                     :: error
       integer,intent(INOUT),optional            :: counter
!          Array c1 is modified if ncecessry
!
! !LOCAL VARAIBELS:
        integer              ::k
        integer              ::i,n
        REALTYPE             ::r
        REALTYPE             ::sumbefore,sumafter
        character(len=160)   ::msg
        character(len=20)    ::onem

! !DESCRIPTION:
!   Routine to check for negative values.
!   Negative values are corrected with the aveage of neighbouring
!   grid points. A warning is given.
!
! !BUGS:
!
! !SEE ALSO:
!
! !SYSTEM ROUTINES:
!
! !FILES USED:
!
! !REVISION HISTORY:
!      created by P. Ruardij 21-06-2006
!
!
!EOP
!-------------------------------------------------------------------------
!BOC
       error=0
       ! in this way NaN are directly found!
       if (minval(c1(1:nlev)) .ge. 0.00D+00) then           !BFM
          continue
       else
          if ( .not.lldeep ) then
              r=max(0.0D+00,sum(h(1:nlev)* c1(1:nlev))/sum(h(1:nlev)))
              if ( r > 1.0D-10) then
                error=1
                write(msg,'(''statenr:'',I4,'' Negative value after call to '',A)') & 
                      statenr, after
                i=len_trim(msg)
                STDERR msg(1:i)
                STDERR "Averaging over the vertical value:",r
                call set_warning_for_getm()
             endif
             c1(1:nlev)=r;
          else
            k=0                                                !BFM
            n=0
            sumbefore=sum(c1(1:nlev)*h(1:nlev))
            do i = 1,nlev                                      !BFM
              if ( c1(i).lt.0.0D+00) then                   !BFM
                  write(onem,'(I2,'':'',F10.3,''/'')') i,c1(i) 
                  if (index(onem,'-')>0 )n=n+1
                  k=-i                                          !BFM
                  if ( i == 1 ) then
                     if ( c1(i+1) > 0.0 )  then
                       c1(i)=0.1* c1(i+1)
                       k=i
                     else
                       c1(i)=0.0
                       k=i
                     endif
                  elseif ( i == nlev ) then
                     if ( c1(i-1) > 0.0 )  then
                       c1(i)=0.1* c1(i-1)
                       k=i
                     else
                       c1(i)=0.0
                       k=i
                     endif
                  else if ( (c1(i-1) > 0.0) .and. ( c1(i+1)>0.0 ) ) then
                     k=i
                     c1(i)=(c1(i-1)+c1(i+1)) * 0.1
                  else if ( c1(i-1) >= 0.0 ) then
                       c1(i)=0.1* c1(i-1)
                       k=i
                  else if ( c1(i+1) >= 0.0 ) then
                       c1(i)=0.1* c1(i+1)
                       k=i
                  endif
               endif
               if ( error.ge.0) error=k
            end do                                    !BFM
            sumafter=1.0D-80+sum(c1(1:nlev)*h(1:nlev))
            r=max(0.0,sumbefore/sumafter)
            ! recalculate percentage shift
            c1=c1*min(1.0,r);r =100.0D+00*(1.0-r) 
            if (present(counter).and. n==1.and.r< 0.001) then
              counter=counter+1
            elseif ( n > 0 ) then
              call set_warning_for_getm()
              write(msg,'(''statenr='',I3,'' Negative values after '' &
                    ,A,'' n='',I2,'' shift='',F10.3,''%'')') &
                    statenr,after,n,r
              i=len_trim(msg)
              STDERR msg(1:i)
            endif
         endif
       endif

     end subroutine test_on_negative_states
!EOC
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Test negative concentrations for all_states
!
! !INTERFACE:
       subroutine test_on_all_negative_states (lldo, lldeep,b_setup, h, nstates,nlev, message, ccx, error )
!
! !USES:
       IMPLICIT NONE
!
! !INPUT PARAMETERS:
       logical,intent(in)                      :: lldo
       logical,intent(in)                      :: lldeep
       integer,intent(in)                      :: b_setup
       integer,intent(in)                      :: nstates
       integer,intent(in)                      :: nlev
       REALTYPE,intent(in)                     :: h(0:nlev)
       character(len=*),intent(IN)             :: message
!
! !OUTPUT PARAMETERS:
!          Array ccx is modified if ncecessry
       REALTYPE,intent(inout)                  :: ccx(1:nstates,0:nlev)
       integer,intent(OUT)                     :: error
! !LOCAL VARAIBELS:
        integer              ::j
           error=0
           if (lldo) then 
              do j=1,nstates
               if (b_setup /= 2 ) then
                 if (pelvar_type(j)>=ALLTRANSPORT) then
                   c1dimz=ccx(j,:)
                   call test_on_negative_states ( j,lldeep,h,nlev, message,c1dimz, error )
                   ccx(j,:)=c1dimz
                 endif
               endif
             enddo
           endif
     end subroutine test_on_all_negative_states


!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Test mass conservation concentrations
!
! !INTERFACE:
!
       subroutine test_mass_conservation(lldo,mode,after,totsysn_old,totsysp_old)
! !USES:
       use gotm_error_msg, only:set_warning_for_getm
       use mem, only: totsysn,totsysp
       IMPLICIT NONE
!
! !INPUT PARAMETERS:
       logical,intent(in)                      :: lldo
       integer,intent(in)                      :: mode
       character(len=*),intent(IN)             :: after
!
!
! !INPUT/OUTPUT PARAMETERS:
       REALTYPE,intent(INOUT)                   :: totsysn_old
       REALTYPE,intent(INOUT)                   :: totsysp_old
!
! !LOCAL VARAIBELS:
        REALTYPE             ::r
        integer              ::i
        character(len=160)   ::msg

        if (lldo ) then
            if ( mode.le.2) then
              !initialize
              if ( mode.eq.1) call CheckMassConservationNPSDynamics
              totsysn_old=totsysn(1);totsysp_old=totsysp(1)
            else
              ! CheckMassConservation need only be called if this routine is called if state vars
              ! are changed by physical processes.
              if ( mode.eq.3) call CheckMassConservationNPSDynamics
              r= (totsysn(1)-totsysn_old)/abs(totsysn(1)) * 100.0
              if ( r> 0.0001) then
                  call set_warning_for_getm 
                  write(msg,'(''No Mass conservation for N after call to '',A,'': '',F7.4,''%'')') &
                        after,r
                  i=len_trim(msg); STDERR msg(1:i)

              endif
              r= (totsysp(1)-totsysp_old)/abs(totsysp(1)) * 100.0
              if ( r> 0.0001) then
                 call set_warning_for_getm 
                 write(msg,'(''No Mass conservation for P after call to '',A,'': '',F7.4,''%'')') &
                        after,r
                  i=len_trim(msg); STDERR msg(1:i)
              endif
              totsysn_old=totsysn(1);totsysp_old=totsysp(1)
            endif
        endif
    end subroutine test_mass_conservation


!EOC
!-------------------------------------------------------------------------




!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the bio calculations
!
! !INTERFACE:
   subroutine calc_sigma_depth( nlev,ddu,maxdepth,arr)
!
! !DESCRIPTION:
!  Calculate Sigma depth.
!  This routine is used to calculate the profiles of the benthic
!  nutrient model.
!  This routine is a simplification of 
!  the calculation used in gotm/getm
!
! !USES:
   IMPLICIT NONE
! !INPUT PARAMETERS:
     integer,intent(IN)           :: nlev
     REALTYPE,intent(IN)          :: ddu
     REALTYPE,intent(IN)          :: maxdepth
! !OUTPUT PARAMETERS:
     REALTYPE,intent(OUT)        :: arr(1:nlev)
! !LOCAL PARAMETERS:
     REALTYPE                    :: r,s
     integer                     :: i
!
!
! !REVISION HISTORY:
!  Original by Piet Ruardij
!
!EOP
!-----------------------------------------------------------------------
!BOC

   r =0.0
   do i=1,nlev
      s= maxdepth*(1.0 - tanh(ddu*float(nlev-i)/float(nlev))/tanh(ddu)) 
      arr(i)=(s+r) * 0.5
      r=s
   enddo
   return
   end subroutine calc_sigma_depth
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the bio calculations
!
! !INTERFACE:
   subroutine end_bio_bfm
!
! !DESCRIPTION:
!  Nothing done here --- supplied for completeness
!  with GOTM bio structure.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC

   return
   end subroutine end_bio_bfm
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: DeriveFromGotm
!
! !INTERFACE:
   subroutine DeriveFromGotm(mode,nlev,arr)
!
! !DESCRIPTION:
!  Nothing done here --- supplied for completeness
!  with GOTM bio structure.
!
! !USES:
   use turbulence,   only: num,eps

   IMPLICIT NONE
! !INPUT PARAMETERS:
     integer,intent(IN)           :: mode
     integer,intent(IN)           :: nlev
! !OUTPUT PARAMETERS:
     REALTYPE,intent(OUT)        :: arr(1:nlev)
! !REVISION HISTORY:
     select case (mode)
      case(1)   ! Calculate Shear Rate (1/s)
          arr(1:nlev)= sqrt(eps(1:nlev)/(1.0D-80+num(1:nlev)))
     end select
     

!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC

   return
   end subroutine DeriveFromGotm
!EOC

!-----------------------------------------------------------------------


   end module bio_bfm

!-----------------------------------------------------------------------
! Copyright by the GOTM-team and BFM-team under the GNU Public License
! www.gnu.org
!-----------------------------------------------------------------------
