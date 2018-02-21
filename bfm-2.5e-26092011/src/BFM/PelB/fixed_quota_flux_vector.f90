          subroutine fixed_quota_flux_vector(iiSub,origin, &
                                          destination,flux,collect)

            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

            use mem, only: iiN, iiP,flux_vector
            use global_mem, only: LOGUNIT
            use constants, only: RLEN, LOGUNIT
            use mem_Param, only: check_fixed_quota
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            ! Implicit typing is never allowed
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
            implicit none

            integer,intent(IN) :: iiSub
            integer,intent(IN) :: origin
            integer,intent(IN) :: destination
            real(RLEN),intent(IN),dimension(:) :: flux
            real(RLEN),intent(INOUT),dimension(:) :: collect

!           integer :: i
!           character(len=8) :: D23


            if ( origin ==0 ) then
              if ( destination ==0 ) then
                 if ( abs(sum(flux/(1.0D-80+collect)-1.0D+00))> 1.0D-6  &
                                               .and. check_fixed_quota == 1) then
                    if ( iiSub==iiN) then
                      write(LOGUNIT,'(''Warning: N:C quotumn not fixed'')')
                    elseif (iiSub==iiP) then
                      write(LOGUNIT,'(''Warning: P:C quotumn not fixed'')')
                    endif
                    return
                 endif
              else
                 call flux_vector(iiSub,destination, destination,flux)
              endif
            elseif ( destination ==0 ) then
             call flux_vector(iiSub,origin, origin,-flux)
            else
             call flux_vector(iiSub,origin, destination,flux)
            endif      
          if ( check_fixed_quota==1 ) collect=collect+flux
          end subroutine fixed_quota_flux_vector

