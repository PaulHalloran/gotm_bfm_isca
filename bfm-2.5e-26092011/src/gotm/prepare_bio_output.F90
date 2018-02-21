#include"cppdefs.h"

subroutine prepare_bio_output(mode, nlev, h,dt)
   use  bio_var, only: stPelStateS,stPelDiagS,stPelFluxS,stBenStateS,stBenDiagS,stBenFluxS, & 
                      stPelStateE,stPelDiagE,stPelFluxE,stBenStateE,stBenDiagE,stBenFluxE, &
                      var_ave, cc_ave,ccb_ave, ave_count,numc, &
                      cc,ccb,diag,diagb,c1dimz,bio_setup,adv1d_courant,adv1d_number

    implicit none 
    integer,intent(IN)                     ::mode
    integer,intent(IN)                     ::nlev
    REALTYPE,dimension(nlev),intent(IN)    ::h
    REALTYPE,intent(IN)                    ::dt

    integer                     ::i
    integer                     ::j
    integer                     ::k
    integer                     ::rc
    logical                     ::llcalc
    character(len=90)           ::msg

    select case (mode)
        case(0)   ! initialization
          i=count(var_ave(stPelStateS:stPelFluxE))
          if ( (i > 0) .and. bio_setup/=2) then
            allocate(cc_ave(1:i,0:nlev),stat=rc)
            if (rc /= 0) stop 'init_bio(): Error allocating cc_ave)'
             cc_ave=0;
          endif
          i=count(var_ave(stBenStateS:stBenFluxE))
          if ( ( i > 0) .and. bio_setup>1) then
            allocate(ccb_ave(1:i,0:1),stat=rc)
            if (rc /= 0) stop 'init_bio(): Error allocating cc_ave)'
            ccb_ave=0;
          endif
           ave_count=0.0
        case(1)    ! prepare for printing
           do j=1,numc
             if ( adv1d_courant(j) > 0.0 ) then
              write(msg, &
                  '(''Warning state '',i2,'' adv_center:Av.Max.Courant N.='',F8.4,'' Av.iterations='',F4.1)') &
                     j,adv1d_courant(j)/ave_count,adv1d_number(j)/ave_count
              i=len_trim(msg);write(stderr,'(''        '',A)') msg(1:i)
              adv1d_courant(j)=0.0;adv1d_number(j)=0.0;
             endif
           enddo
           if (bio_setup/=2) cc_ave=cc_ave/ave_count
           if (bio_setup>1) ccb_ave=ccb_ave/ave_count
           ave_count=0.0
        case(10)   ! Start of new time-step
           ave_count=ave_count+1.0
        case(11)   ! add pel value
           k=0
           j=0
           if (stPelStateE ==0 .or. bio_setup==2) return
           do i=stPelStateS,stPelStateE
             j=j+1
             if ( var_ave(i) ) then
                k=k+1
                if ( ave_count < 1.5 ) then
                   cc_ave(k,:)=cc(j,:)
                else
                   cc_ave(k,:)=cc_ave(k,:)+cc(j,:)
                endif
             endif
           enddo
           j=0
           do i=stPelDiagS,stPelDiagE
             j=j+1
             if ( var_ave(i) ) then
                k=k+1
                if ( ave_count < 1.5 ) then
                   cc_ave(k,:)=diag(j,:)
                else
                   cc_ave(k,:)=cc_ave(k,:)+diag(j,:)
                endif
              endif
           enddo
           j=0
           do i=stPelFluxS,stPelFluxE
             j=j+1
             if ( var_ave(i) ) then
                k=k+1
                call make_flux_output(1,j,0,nlev,dt,c1dimz,llcalc)
                if ( ave_count < 1.5 ) then
                   cc_ave(k,:)=c1dimz
                else
                   cc_ave(k,:)=cc_ave(k,:)+c1dimz
                endif
              endif
           enddo
        case(12)   ! add ben value 
           k=0
           j=0
           if (stBenStateE ==0 .or. bio_setup==1) return
           do i=stBenStateS,stBenStateE
             j=j+1
             if ( var_ave(i) ) then
                k=k+1
                if ( ave_count < 1.5 ) then
                   ccb_ave(k,0:1)=ccb(j,0:1)
                else
                   ccb_ave(k,0:1)=ccb_ave(k,0:1)+ccb(j,0:1)
                endif
              endif
           enddo
           j=0
           do i=stBenDiagS,stBenDiagE
             j=j+1
             if ( var_ave(i) ) then
                k=k+1
                if ( ave_count < 1.5 ) then
                   ccb_ave(k,0:1)=diagb(j,0:1)
                else
                   ccb_ave(k,0:1)=ccb_ave(k,0:1)+diagb(j,0:1)
                endif
              endif
           enddo
           j=0
           do i=stBenFluxS,stBenFluxE
             j=j+1
             if ( var_ave(i) ) then
                k=k+1
                call make_flux_output(2,j,0,nlev,dt,c1dimz,llcalc)
                if ( ave_count < 1.5 ) then
                   ccb_ave(k,0:1)=c1dimz(0:1)
                else
                   ccb_ave(k,:)=ccb_ave(k,:)+c1dimz(0:1)
                endif
              endif
           enddo
    end select

return
end subroutine prepare_bio_output

