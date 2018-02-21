!$Id: bio_save.F90,v 1.5 2005-12-02 20:57:27 hb Exp $
#include"cppdefs.h"
!EOC
!-----------------------------------------------------------------------
!BOP
! !ROUTINE: Defining extra dimension vars
!
! !INTERFACE:
   integer function special_dims(mode,ncid,nlev,name,extname,units,lon_dim,lat_dim,time_dim,vars_id)
!
! !DESCRIPTION:
! Here, the output of biogeochemical parameters either as ascii or as
! NetCDF files is managed.
!
! !USES:
   use bio_bfm, only: calc_sigma_depth
#ifdef NETCDF_FMT
   use ncdfout, only: set_attributes,store_data
#endif
   IMPLICIT NONE
#ifdef NETCDF_FMT
#include "netcdf.inc"
#endif
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: mode
   integer, intent(in)                 :: ncid
   integer, intent(in)                 :: nlev
   character(*), intent(in)            :: name
   character(*), intent(in)            :: extname
   character(*), intent(in)            :: units
   integer, intent(in)                 :: lon_dim
   integer, intent(in)                 :: lat_dim
   integer, intent(in)                 :: time_dim
   integer, intent(inout)              :: vars_id
!
! !REVISION HISTORY:
!  Original author(s): Piet Ruardij
!
! !LOCAL VARIABLES:
   logical, save             :: first=.true.
   integer, save             :: nn
   integer                   :: iret
   REALTYPE,parameter        :: ddu=2.0
   REALTYPE                  :: zz,r,s
   integer                   :: dims(4)
   integer                   :: i,j,n,status,altZ_id,dim_altZ
   REALTYPE                  :: arr(0:nlev)
   character(len=30)         :: altZ,altZ_longname
   character(len=6)          :: dum,alt_unit
   REAL_4B                   :: vals(2)
!EOP
   select case (mode)
     case (1)
      vals(1) = 1.0
       i = nf_put_att_real(ncid,vars_id,'averaged',NF_FLOAT,1,vals)
       status=1;if (i.eq.NF_NOERR) status=0
       special_dims=1
     case (2)
       if ( index(extname,'__Z' ) ==1 ) then
          j=index(extname,':')-1
          read(extname(1:j),*) dum,altZ, zz,alt_unit, altZ_longname
          status = nf_inq_dimid(ncid, altZ, dim_altZ)
          if (status.ne.NF_NOERR) then
            status=nf_def_dim(ncid,altZ,nlev,dim_altZ)
            if (status.eq.NF_NOERR) then
               dims(1)=dim_altZ
               status = nf_def_var(ncid,altZ,NF_REAL,1,dims,altZ_id)
               if (status.eq.NF_NOERR) then
                  i=len_trim(altZ_longname);
                  i=index(extname(1:j),altZ_longname(1:i))
                  status= set_attributes(ncid,altZ_id,long_name=extname(i:j),units=alt_unit,missing_value=-9999.0D+00)
                  call calc_sigma_depth(nlev,ddu,zz,arr(1:nlev))
                  status = nf_enddef(ncid)
                  status = store_data(ncid,altZ_id,Z_SHAPE,nlev,array=arr)
                  status = nf_redef(ncid)
               endif
            endif
          endif
          dims(1)=lon_dim;dims(2)=lat_dim
          dims(3)=dim_altZ;dims(4)=time_dim
          status = nf_def_var(ncid,name,NF_REAL,4,dims,vars_id)
          status= set_attributes(ncid,vars_id,long_name=trim(extname(j+2:)))
          status= set_attributes(ncid,vars_id,units=units,missing_value=-9999.0D+00)
          special_dims=1
       else
          special_dims=0
       endif
    end select
   end function special_dims
!-----------------------------------------------------------------------
!BOP
! !ROUTINE: Storing the results
!
! !INTERFACE:
   subroutine bio_save(nlev,h,dt,totn)
!
! !DESCRIPTION:
! Here, the output of biogeochemical parameters either as ascii or as
! NetCDF files is managed.
!
! !USES:
   use bio_var
   use output, only: out_fmt,ts
#ifdef NETCDF_FMT
   use ncdfout, only: ncid
   use ncdfout, only: lon_dim,lat_dim,z_dim,time_dim,dims
   use ncdfout, only: define_mode,new_nc_variable,set_attributes,store_data
#endif
   IMPLICIT NONE
#ifdef NETCDF_FMT
#include "netcdf.inc"
#endif
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: h(0:nlev)
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: totn
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   logical, save             :: first=.true.
   integer, save             :: nn
   integer, save             :: totn_id
   integer                   :: iret
   integer                   :: out_unit=67
   REALTYPE                  :: zz
   logical                   :: llcalc
   integer                   :: i,j,n

   integer,external          :: special_dims
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (out_fmt)
      case (ASCII)
         if(first) then
            open(out_unit,file='bio.out',status='unknown')
            nn = ubound(cc(1,:),1)
            first = .false.
         end if
         write(out_unit,*)
         write(out_unit,*) trim(ts)
         zz = _ZERO_
         do i=nn,1,-1
            zz=zz+0.5*h(i)
            write(out_unit,115) zz,(cc(j,i) , j=1,numc)
            zz=zz+0.5*h(i)
         end do
115 format(F10.4,100(1x,E10.4E2))

      case (NETCDF)
#ifdef NETCDF_FMT
#ifdef BFM_GOTM
         if (bio_model==6) then
            if(first) then
               first = .false.
               iret = define_mode(ncid,.true.)
               if (bio_setup/=2) then 
               dims(1) = lon_dim
               dims(2) = lat_dim
               dims(3) = z_dim
               dims(4) = time_dim
               do n=stPelStateS,stPelFluxE
                  j=0;if ( var_ids(n) /= 0 )  & 
                  j=special_dims(2,ncid,nlev,var_names(n),var_long(n),var_units(n),lon_dim,lat_dim,time_dim,var_ids(n))
                  if ( j.eq.0 .and. var_ids(n) /= 0 )  then 
                     iret = new_nc_variable(ncid,var_names(n),NF_REAL, &
                                            4,dims,var_ids(n))
                     iret = set_attributes(ncid,var_ids(n),       &
                                           units=var_units(n),    &
                                           long_name=var_long(n),missing_value=-9999.0D+00)
                  end if
                  if ( j.eq.0 .and. var_ave(n) )  & 
                  j=special_dims(1,ncid,nlev,var_names(n),var_long(n),var_units(n),lon_dim,lat_dim,time_dim,var_ids(n))
               end do
            end if
            if (bio_setup>1) then ! define benthic variables
               dims(1) = lon_dim
               dims(2) = lat_dim
               dims(3) = time_dim
               do n=stBenStateS,stBenFluxE
                  if ( var_ids(n) /= 0 )  then 
                     iret = new_nc_variable(ncid,var_names(n),NF_REAL, &
                                       3,dims,var_ids(n))
                     iret = set_attributes(ncid,var_ids(n),       &
                                      units=var_units(n),    &
                                      long_name=var_long(n),missing_value=-9999.0D+00)
                  endif
                  if ( var_ave(n) )  & 
                  j=special_dims(1,ncid,nlev,var_names(n),var_long(n),var_units(n),lon_dim,lat_dim,time_dim,var_ids(n))
               end do
            end if   
            iret = define_mode(ncid,.false.)
         end if

         do n=stPelStateS,stPelStateE
            if ( (var_ids(n) > 0) .and. (.not.var_ave(n) )) &
              iret = store_data(ncid,var_ids(n),XYZT_SHAPE,nlev,array=cc(n,:))
         end do
         i=0
         do n=stPelDiagS,stPelDiagE
            i=i+1
            if ( (var_ids(n) > 0).and. (.not.var_ave(n) ) ) & 
               iret = store_data(ncid,var_ids(n),XYZT_SHAPE,nlev,array=diag(i,:))
         end do

         i=0
         do n=stPelFluxS,stPelFluxE
            i=i+1
            if ( (var_ids(n) > 0)  .and. (.not.var_ave(n))) then
               call make_flux_output(1,i,0,nlev,dt,c1dimz,llcalc)
               iret = store_data(ncid,var_ids(n),XYZT_SHAPE,nlev,array=c1dimz)
            end if
         end do
         j=0
         do n=stPelStateS,stPelFluxE
            if ( (var_ids(n) > 0) .and.var_ave(n) ) then
              j=j+1
              iret = store_data(ncid,var_ids(n),XYZT_SHAPE,nlev,array=cc_ave(j,:))
            endif
         end do

! storage of benthic variables
! stored as scalar: to be modified if benvar are arrays
         if (bio_setup>1) then
           i=0
           do n=stBenStateS,stBenStateE
              i=i+1
              if ( (var_ids(n) > 0)  .and. (.not.var_ave(n))) &
               iret = store_data(ncid,var_ids(n),XYT_SHAPE,1,scalar=ccb(i,1))
           end do
           i=0
           do n=stBenDiagS,stBenDiagE
             i=i+1
             if ( (var_ids(n) > 0)  .and. (.not.var_ave(n))) &
               iret = store_data(ncid,var_ids(n),XYT_SHAPE,1,scalar=diagb(i,1))
           end do
           i=0
           do n=stBenFluxS,stBenFluxE
             i=i+1
             if ( (var_ids(n) > 0)  .and. (.not.var_ave(n))) then
               call make_flux_output(2,i,0,nlev,dt,c1dimz,llcalc)
               iret = store_data(ncid,var_ids(n),XYT_SHAPE,1,scalar=c1dimz(1))
             endif
           end do 
           j=0
           do n=stBenStateS,stBenFluxE
              if ( (var_ids(n) > 0) .and. var_ave(n)) then
                 j=j+1
                 iret = store_data(ncid,var_ids(n),XYT_SHAPE,1,scalar=ccb_ave(j,1))
              endif
           end do
         end if                
       else ! other bio_model
#endif
! Standard GOTM save
         if(first) then
            first = .false.

            iret = define_mode(ncid,.true.)

            dims(1) = lon_dim
            dims(2) = lat_dim
            dims(3) = z_dim
            dims(4) = time_dim
            do n=1,numc
               iret = new_nc_variable(ncid,var_names(n),NF_REAL, &
                                      4,dims,var_ids(n))
               iret = set_attributes(ncid,var_ids(n),       &
                                     units=var_units(n),    &
                                     long_name=var_long(n))
            end do
            nn = ubound(cc(1,:),1)
            dims(1) = time_dim
            iret = new_nc_variable(ncid,'totn',NF_REAL,1,dims,totn_id)
            iret = set_attributes(ncid,totn_id,units='mmol/m**2',    &
                   long_name='total N')
            iret = define_mode(ncid,.false.)
         end if !first

         do n=1,numc
            iret = store_data(ncid,var_ids(n),XYZT_SHAPE,nn,array=cc(n,:))
         end do
!KBK         iret = store_data(ncid,phy_id,XYZT_SHAPE,nn,array=cc(2,:)+P0)
!KBK         iret = store_data(ncid,zoo_id,XYZT_SHAPE,nn,array=cc(3,:)+Z0)

         iret = store_data(ncid,totn_id,T_SHAPE,1,scalar=totn)
#ifdef BFM_GOTM
       end if !bio_model
#endif
#endif
      case default
         FATAL 'A non valid output format has been chosen'
         stop 'bio_save'
   end select

   return
   end subroutine bio_save
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------
