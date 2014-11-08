!==============================================================================!
!                          B E G I N    M O D U L E :                          !
!                               C A L C U L U S                                !
!==============================================================================!
!
!!  @author Christopher MacMackin
!
!!  A module containing subroutines which can perform numerical
!!  calculus. Currently only a couple of calculus operations are 
!!  available, but they should work on any function returning real(8) 
!!  data.
!
!------------------------------------------------------------------------------!
module calculus
contains

    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                        D I F F E R E N T I A T E                         !
    !==========================================================================!
    !
    !!              Uses Richardson Extrapolation and finite differences to
    !!              calculate a function's derivative to the desired precision.
    !!              Will perform up to five iterations of Richardson 
    !!              Extrapolation.
    !!
    !!              Adapted from a FORTRAN77 routine by Dr. David A. Clarke. 
    !
    !--------------------------------------------------------------------------!
    recursive subroutine differentiate( derivval, dxval, error, func, iter1,   &
                                        nderiv, tol, xval, verbose )
        implicit none
        
        !! The function for which to calculate the derivative. Must return a 
        !! real(8) value.
        interface
            real(8) function func     ( x        )
                real(8), intent(in) ::  x
            end function func
        end interface
        
        ! Input and output variables:
        !! What derivative to take.
        integer, intent(in)     ::  nderiv
        !! Whether to print Richardson extrapolation tables.
        logical, intent(in)     ::  verbose
        !! The initial value to use for computing the finite difference.
        real(8), intent(in)     ::  dxval
        !! The value of _x_ at which to calculate the derivative of function 
        !! _f_(_x_).
        real(8), intent(in)     ::  xval
        !! The desired accuracy with which to compute the derivative to the 
        !! desired accuracy.
        real(8), intent(in)     ::  tol
        !! The number of iterations required to calculate the derivative. 
        integer, intent(out)    ::  iter1
        !! The computed value of the derivative.
        real(8), intent(out)    ::  derivval
        !! An estimate of the error in the computed derivative.
        real(8), intent(out)    ::  error
        
        ! Other variables:
        integer                     ::  iter2,                         &
                                        iter3
        integer, parameter          ::  imax = 5
        logical                     ::  cnvrgd
        real(8)                     ::  anew,                          &
                                        fact,                          &
                                        hval,                          &
                                        recur1,                        &
                                        recur2,                        &
                                        recurerr
        real(8), dimension(0:imax)  ::  new,                           &
                                        old
        real(8), parameter          ::  base = 4.d0
    !------------------------------------------------------------------!

        ! Check that taking a positive derivative (nderiv > 0)
        if ( nderiv <= 0 ) then
            write(0 ,2000) nderiv
        end if
        
        ! Initialize variables
        if ( verbose ) write(6 ,2010)
        hval  = dxval
        iter1 = 1
        new = 0.d0
        cnvrgd = .false.

        ! Get first estimate of zeroth of derivative
        if ( nderiv == 1 ) then
            new(0) = (func(xval + hval) - func(xval - hval))/(2.d0*hval)
        else
            call rederiv(recur1, dxval, recurerr, func, iter3,         &
                         nderiv - 1, tol/1.d1, xval + hval, .false.)
            call rederiv(recur2, dxval, recurerr, func, iter3,         &
                         nderiv - 1, tol/1.d1, xval - hval, .false.)
            new(0) = (recur1 - recur2)/(2.d0*hval)
        end if
        if ( verbose ) write(6 ,2020) hval, new(0)
        hval = hval/2.d0
        
        ! Run Richardson Extrapolation
        do iter1 = 1,imax
            ! Update zeroth values of derivative
            old(0) = new(0)
            if ( nderiv == 1 ) then
                new(0) = (func(xval + hval) - func(xval - hval))/      &
                         (2.d0*hval)
            else
                call rederiv(recur1, dxval, recurerr, func, iter3,     &
                             nderiv - 1, tol/1.d1, xval + hval, .false.)
                call rederiv(recur2, dxval, recurerr, func, iter3,     &
                             nderiv - 1, tol/1.d1, xval - hval, .false.)
                new(0) = (recur1 - recur2)/(2.d0*hval)
            end if
            
            ! Refine the estimate of the derivative
            do iter2 = 1,iter1
                fact = base**iter2
                old(iter2) = new(iter2)
                new(iter2) = (fact*new(iter2-1) - old(iter2-1))/       &
                             (fact - 1.d0)
            end do

            ! Update errors
            error = abs((new(iter1) - new(iter1-1))/(5.d-1*(new(iter1) &
                    + new(iter1-1))))
            anew = abs(new(iter1))

            ! Write new row of extrapolation table. If extrapolation
            ! is to be continued for another cycle, halve h and return
            ! to top of loop.
            if ( verbose ) write(6 ,2020) hval, new(0:iter1)
            if ( ( error <= tol ) .or. ( anew <= tol ) ) then
                cnvrgd = .true.
                exit
            end if
            hval = hval/2.d0
        end do

        ! Write warning message if necessary and set first derivative
        if ( .not. cnvrgd ) write(6 ,2030) imax
        derivval = new(min(iter1,imax))

    !------------------------------------------------------------------!
    !                      Write format statements                     !
    !------------------------------------------------------------------!
        2000 format('REDERIV : ERROR: Can not compute ',I3,'th ',      &
                    'derivative.')
        2010 format('REDERIV : hval     D0               D1         ', &
                    '      D2               D3               D4')
        2020 format('REDERIV : ',F6.4,' ',1P5G17.9)
        2030 format('REDERIV : WARNING: Richardson extrapolation ',    &
                    'failed to converge after ',/,                     &
                    'REDERIV : ',I2,' iterations.',/,                  &
                    'REDERIV : Extrapolation terminated.')
    !------------------------------------------------------------------!

        return
    end subroutine differentiate
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                        D I F F E R E N T I A T E                         !
    !==========================================================================!



    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                            I N T E G R A T E                             !
    !==========================================================================!
    !
    !!          This subroutine computes the 1D definite integral of a
    !!          function over the desired range and to the desired accuracy. It
    !!          uses the technique of Romberg Integration, performing up to 10
    !!          iterations.
    !
    !--------------------------------------------------------------------------!
    subroutine romberg ( intval  , error   , func    , iter1   ,       &
                         left    , numcalls, right   , tol     ,       &
                         verbose  )
        implicit none
        
        !! The function (returning a real(8) value) whose integral is to be 
        !! calculated.
        interface
            real(8) function func     ( x        )
                real(8), intent(in) ::  x
            end function func
        end interface
        
        ! Input and output variables:
        !! Indicates whether to print the Richardson  Extrapolation table for 
        !! the integration.
        logical, intent(in)     ::  verbose
        !! The lower limit of integration.
        real(8), intent(in)     ::  left
        !! The upper limit of integration.
        real(8), intent(in)     ::  right
        !! The desired accuracy to which to calculate the integral.
        real(8), intent(in)     ::  tol
        !! The number of iterations required to calculate the integral to the 
        !! desired precision.
        integer, intent(out)    ::  iter1
        !! The number of function calls needed to calculate the integral to the 
        !! desired precision.
        integer, intent(out)    ::  numcalls
        !! The value calculated for the definite integral.
        real(8), intent(out)    ::  intval
        !! An estimate of the error in the computed integral.
        real(8), intent(out)    ::  error
        
        ! Other variables:
        integer                     ::  iter2,                         &
                                        mval
        integer, parameter          ::  imax = 10
        logical                     ::  cnvrgd
        real(8)                     ::  fact,                          &
                                        hval,                          &
                                        rhombval,                      &
                                        oldest
        real(8), dimension(0:imax)  ::  new,                           &
                                        old
        real(8), parameter          ::  base = 4.d0
    !------------------------------------------------------------------!
        
        ! Initialize variables
        mval = 1
        cnvrgd = .FALSE.
        hval  = (right - left)/2.d0**mval
        new = 0.d0
        old = 0.d0
        numcalls = 0
        iter1 = 0
        
        ! Check that integration is over non-zero range
        if ( left == right ) then
            intval = 0.d0
            error = 0.d0
            return
        end if
        
        ! Get first set of values for integral
        do iter2 = 1, (2**mval - 1)
            new(0) = new(0) + func(left + iter2*hval)
            numcalls = numcalls + 1
        end do
        new(0) = hval*new(0) + hval/2.d0*(func(left) + func(right))
        numcalls = numcalls + 2
        oldest = new(0)
        call update()
        old(0) = new(0)
        call update()
        rhombval = ABS((oldest - old(0))/(old(0) - new(0)))
        
        ! Decrease step size until second order error is dominant
        do while ( rhombval <= 3 )
            if ( mval > imax ) then
                write( 6,2000) imax
                exit
            end if
            oldest = old(0)
            old(0) = new(0)
            call update()
            rhombval = abs((oldest - old(0))/(old(0) - new(0)))
        end do
        
        ! Perform iteration with Richardson Extrapolation
        if ( verbose ) write( 6,2010)
        if ( verbose ) write( 6,2020) (mval - 1), new(0)
        do iter1 = 2,imax
            if ( iter1 == 1 ) then
                new(1) = (4.d0*new(0) - old(0))/3.d0
            else
                old(0) = new(0)
                call update()
                
                ! Refine the estimate of the integral
                fact = 1.d0
                do iter2 = 1,iter1
                    fact = fact*base
                    old(iter2) = new(iter2)
                    new(iter2) = (fact*new(iter2-1) - old(iter2-1))/   &
                                 (fact - 1.d0)
                end do
            end if

            ! Update errors
            error = 2.d0*ABS((new(iter1) - new(iter1-1))/((new(iter1)  &
                    + new(iter1-1))))

            ! Write new row of extrapolation table
            if ( ( verbose ) .and. ( iter1 < 5 ) ) write( 6,2020) mval,&
                                                   new(0:iter1)
            
            ! Check if integration has converged
            if ( ( error <= tol ) .or. ( abs(new(iter1)) <= tol ) ) then
                cnvrgd = .true.
                exit
            end if
        end do

        if ( ( verbose ) .and. ( iter1 > 5 ) ) write( 6,2021)          &
                                               min(iter1,imax)

        ! Write warning message if necessary and set 
        if ( .not. cnvrgd ) write( 6,2030) imax
        intval = new(min(iter1,imax))

    !------------------------------------------------------------------!
    !                      Write format statements                     !
    !------------------------------------------------------------------!
        2000 format('ROMBERG : WARNING: Rhomburg ratio < 3 after ',    &
                    'more than ',I0,' iterations.',/,                  &
                    'ROMBERG : WARNING: Starting Richardson ',         &
                    'Extrapolation anyway.')
        2010 format('ROMBERG : mval   T0               T1           ', &
                    '    T2               T3               T4')
        2020 format('ROMBERG : ',I3,' ',1P11G17.9)
        2021 format('ROMBERG : Continuing for ',I2,' iterations...')
        2030 format('ROMBERG : WARNING: Rhomburg integration failed ', &
                    'to converge after ',I2,' iterations.',/,          &
                    'ROMBERG : Extrapolation terminated.')
    !------------------------------------------------------------------!

        return
    contains
        !==============================================================!
        !              B E G I N    S U B R O U T I N E :              !
        !                         U P D A T E                          !
        !==============================================================!
        !                                                              !
        !   AUTHOR:         Christopher MacMackin                      !
        !   WRITTEN:        March, 2014                                !
        !   MODIFICATIONS:  None                                       !
        !                                                              !
        !   PURPOSE:        Computes the integration at the next level !
        !                   of resolution.                             !
        !                                                              !
        !   ARGUMENTS:      None                                       !
        !   EXTERNALS:      None                                       !
        !                                                              !
        !--------------------------------------------------------------!
        subroutine update
            implicit none
            
            ! Declare variables:
            integer ::  iter
        !--------------------------------------------------------------!
        
            ! Increase resolution of integration
            mval = mval + 1
            hval = hval/2.d0
            new(0) = new(0)/2.d0
            
            ! Compute integral at points needed for higher resolution
            do iter = 1, (2**(mval - 1))
                new(0) = new(0) + hval*func(left + dble(2*iter - 1)    &
                         *hval)
                numcalls = numcalls + 1
            end do
            
        end subroutine update
        !==============================================================!
        !                E N D    S U B R O U T I N E :                !
        !                         U P D A T E                          !
        !==============================================================!

    end subroutine romberg
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                              R O M B E R G                               !
    !==========================================================================!


end module calculus
!==============================================================================!
!                            E N D    M O D U L E :                            !
!                               C A L C U L U S                                !
!==============================================================================!
