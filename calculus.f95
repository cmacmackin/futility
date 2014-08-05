!======================================================================!
!                      B E G I N    M O D U L E :                      !
!                           C A L C U L U S                            !
!======================================================================!
!                                                                      !
!   PURPOSE:    Performs calculus functions.                           !
!   CONTAINS:   rederiv (subroutine), romberg (subroutine)             !
!   EXTERNALS:  none                                                   !
!                                                                      !
!----------------------------------------------------------------------!
MODULE calculus
CONTAINS

    !==================================================================!
    !                B E G I N    S U B R O U T I N E :                !
    !                          R E D E R I V                           !
    !==================================================================!
    !                                                                  !
    !   AUTHOR:         Christopher MacMackin                          !
    !   WRITTEN:        March, 2014                                    !
    !   MODIFICATIONS:  Adapted from a FORTRAN77 routine by Dr. David  !
    !                   A. Clarke.                                     !
    !                                                                  !
    !   PURPOSE:        Recursively finds the nth derivative of the    !
    !                   provided function, to the desired precision,   !
    !                   using a finite difference with a Richardson    !
    !                   Extrapolation.                                 !
    !                                                                  !
    !   ARGUMENTS:      derivval, the computed value of the            !
    !                             derivative.                          !
    !                  *dxval   , the inital value to use for the      !
    !                             finite difference.                   !
    !                   error   , the estimate of the error in the     !
    !                             computed derivative.                 !
    !                  *func    , the function whose derivative is to  !
    !                             be computed.                         !
    !                   iter1   , the number of iterations required to !
    !                             calculate the derivative to the      !
    !                             desired precision.                   !
    !                  *nderiv  , the number of the derivative to be   !
    !                             calculated.                          !
    !                  *tol     , the desired accuracy to which to     !
    !                             calculate the derivative.            !
    !                  *xval    , the value of x at which to calculate !
    !                             the derivative.                      !
    !                  *verbose , indicates whether to print the       !
    !                             Richardson Extrapolation table for   !
    !                             the derivative.                      !
    !                                                                  !
    !                   Arguments marked with an asterisk (*) are ones !
    !                   for which the user must specify a value when   !
    !                   the subroutine is called. Those without an     !
    !                   asterisk (*) are used only to return a value   !
    !                   to the caller.                                 !
    !                                                                  !
    !   EXTERNALS:      None                                           !
    !                                                                  !
    !------------------------------------------------------------------!
    RECURSIVE SUBROUTINE rederiv  ( derivval, dxval   , error   ,      &
                                    func    , iter1   , nderiv  ,      &
                                    tol     , xval    , verbose  )
        IMPLICIT NONE
        
        ! Give compiler information about function whose derivative is
        ! being calculated.
        INTERFACE
            REAL(8) FUNCTION func     ( x        )
                REAL(8), INTENT(IN) ::  x
            END FUNCTION func
        END INTERFACE
        
        ! Input and output variables:
        INTEGER, INTENT(IN)     ::  nderiv
        LOGICAL, INTENT(IN)     ::  verbose
        REAL(8), INTENT(IN)     ::  dxval,                             &
                                    xval,                              &
                                    tol
        INTEGER, INTENT(OUT)    ::  iter1
        REAL(8), INTENT(OUT)    ::  derivval,                          &
                                    error
        
        ! Other variables:
        INTEGER                     ::  iter2,                         &
                                        iter3
        INTEGER, PARAMETER          ::  imax = 4
        LOGICAL                     ::  cnvrgd
        REAL(8)                     ::  anew,                          &
                                        fact,                          &
                                        hval,                          &
                                        recur1,                        &
                                        recur2,                        &
                                        recurerr
        REAL(8), DIMENSION(0:imax)  ::  new,                           &
                                        old
        REAL(8), PARAMETER          ::  base = 4.d0
    !------------------------------------------------------------------!

        ! Check that taking a positive derivative (nderiv > 0)
        IF ( nderiv <= 0 ) THEN
            WRITE(0 ,2000) nderiv
        END IF
        
        ! Initialize variables
        IF ( verbose ) WRITE(6 ,2010)
        hval  = dxval
        iter1 = 1
        new = 0.d0
        cnvrgd = .FALSE.

        ! Get first estimate of zeroth of derivative
        IF ( nderiv == 1 ) THEN
            new(0) = (func(xval + hval) - func(xval - hval))/(2.d0*hval)
        ELSE
            CALL rederiv(recur1, dxval, recurerr, func, iter3,         &
                         nderiv - 1, tol/1.d1, xval + hval, .FALSE.)
            CALL rederiv(recur2, dxval, recurerr, func, iter3,         &
                         nderiv - 1, tol/1.d1, xval - hval, .FALSE.)
            new(0) = (recur1 - recur2)/(2.d0*hval)
        END IF
        IF ( verbose ) WRITE(6 ,2020) hval, new(0)
        hval = hval/2.d0
        
        ! Run Richardson Extrapolation
        DO iter1 = 1,imax
            ! Update zeroth values of derivative
            old(0) = new(0)
            IF ( nderiv == 1 ) THEN
                new(0) = (func(xval + hval) - func(xval - hval))/      &
                         (2.d0*hval)
            ELSE
                CALL rederiv(recur1, dxval, recurerr, func, iter3,     &
                             nderiv - 1, tol/1.d1, xval + hval, .FALSE.)
                CALL rederiv(recur2, dxval, recurerr, func, iter3,     &
                             nderiv - 1, tol/1.d1, xval - hval, .FALSE.)
                new(0) = (recur1 - recur2)/(2.d0*hval)
            END IF
            
            ! Refine the estimate of the derivative
            DO iter2 = 1,iter1
                fact = base**iter2
                old(iter2) = new(iter2)
                new(iter2) = (fact*new(iter2-1) - old(iter2-1))/       &
                             (fact - 1.d0)
            END DO

            ! Update errors
            error = ABS((new(iter1) - new(iter1-1))/(5.d-1*(new(iter1) &
                    + new(iter1-1))))
            anew = ABS(new(iter1))

            ! Write new row of extrapolation table. If extrapolation
            ! is to be continued for another cycle, halve h and return
            ! to top of loop.
            IF ( verbose ) WRITE(6 ,2020) hval, new(0:iter1)
            IF ( ( error <= tol ) .OR. ( anew <= tol ) ) THEN
                cnvrgd = .TRUE.
                EXIT
            END IF
            hval = hval/2.d0
        END DO

        ! Write warning message if necessary and set first derivative
        IF ( .NOT. cnvrgd ) WRITE(6 ,2030) imax
        derivval = new(MIN(iter1,imax))

    !------------------------------------------------------------------!
    !                      Write format statements                     !
    !------------------------------------------------------------------!
        2000 FORMAT('REDERIV : ERROR: Can not compute ',I3,'th ',      &
                    'derivative.')
        2010 FORMAT('REDERIV : hval     D0               D1         ', &
                    '      D2               D3               D4')
        2020 FORMAT('REDERIV : ',F6.4,' ',1P5G17.9)
        2030 FORMAT('REDERIV : WARNING: Richardson extrapolation ',    &
                    'failed to converge after ',/,                     &
                    'REDERIV : ',I2,' iterations.',/,                  &
                    'REDERIV : Extrapolation terminated.')
    !------------------------------------------------------------------!

        RETURN
    END SUBROUTINE rederiv
    !==================================================================!
    !                  E N D    S U B R O U T I N E :                  !
    !                          R E D E R I V                           !
    !==================================================================!



    !==================================================================!
    !                B E G I N    S U B R O U T I N E :                !
    !                          R O M B E R G                           !
    !==================================================================!
    !                                                                  !
    !   AUTHOR:         Christopher MacMackin                          !
    !   WRITTEN:        March, 2014                                    !
    !   MODIFICATIONS:  None                                           !
    !                                                                  !
    !   PURPOSE:        Implements a Romberg Integrator. Integrates    !
    !                   the passed function over the given range.      !
    !                                                                  !
    !   ARGUMENTS:      intval  , the computed value of the integral.  !
    !                   error   , the estimate of the error in the     !
    !                             computed integral.                   !
    !                  *func    , the function whose integral is to be !
    !                             computed.                            !
    !                   iter1   , the number of iterations required to !
    !                             calculate the integral to the        !
    !                             desired precision.                   !
    !                  *left    , the lower limit of the integral.     !
    !                   numcalls, number of function calls needed to   !
    !                             calculate derivative.
    !                  *right   , the upper limit of the integral.     !
    !                  *tol     , the desired accuracy to which to     !
    !                             calculate the integral.              !
    !                  *verbose , indicates whether to print the       !
    !                             Richardson Extrapolation table for   !
    !                             the integral.                        !
    !                                                                  !
    !                   Arguments marked with an asterisk (*) are ones !
    !                   for which the user must specify a value when   !
    !                   the subroutine is called. Those without an     !
    !                   asterisk (*) are used only to return a value   !
    !                   to the caller.                                 !
    !                                                                  !
    !   EXTERNALS:      None                                           !
    !                                                                  !
    !------------------------------------------------------------------!
    SUBROUTINE romberg ( intval  , error   , func    , iter1   ,       &
                         left    , numcalls, right   , tol     ,       &
                         verbose  )
        IMPLICIT NONE
        
        ! Give compiler information about function whose derivative is
        ! being calculated.
        INTERFACE
            REAL(8) FUNCTION func     ( x        )
                REAL(8), INTENT(IN) ::  x
            END FUNCTION func
        END INTERFACE
        
        ! Input and output variables:
        LOGICAL, INTENT(IN)     ::  verbose
        REAL(8), INTENT(IN)     ::  left,                              &
                                    right,                             &
                                    tol
        INTEGER, INTENT(OUT)    ::  iter1,                             &
                                    numcalls
        REAL(8), INTENT(OUT)    ::  intval,                            &
                                    error
        
        ! Other variables:
        INTEGER                     ::  iter2,                         &
                                        mval
        INTEGER, PARAMETER          ::  imax = 10
        LOGICAL                     ::  cnvrgd
        REAL(8)                     ::  fact,                          &
                                        hval,                          &
                                        rhombval,                      &
                                        oldest
        REAL(8), DIMENSION(0:imax)  ::  new,                           &
                                        old
        REAL(8), PARAMETER          ::  base = 4.d0
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
        IF ( left == right ) THEN
            intval = 0.d0
            error = 0.d0
            RETURN
        END IF
        
        ! Get first set of values for integral
        DO iter2 = 1, (2**mval - 1)
            new(0) = new(0) + func(left + iter2*hval)
            numcalls = numcalls + 1
        END DO
        new(0) = hval*new(0) + hval/2.d0*(func(left) + func(right))
        numcalls = numcalls + 2
        oldest = new(0)
        CALL update()
        old(0) = new(0)
        CALL update()
        rhombval = ABS((oldest - old(0))/(old(0) - new(0)))
        
        ! Decrease step size until second order error is dominant
        DO WHILE ( rhombval <= 3 )
            IF ( mval > imax ) THEN
                WRITE( 6,2000) imax
                EXIT
            END IF
            oldest = old(0)
            old(0) = new(0)
            CALL update()
            rhombval = ABS((oldest - old(0))/(old(0) - new(0)))
        END DO
        
        ! Perform iteration with Richardson Extrapolation
        IF ( verbose ) WRITE( 6,2010)
        IF ( verbose ) WRITE( 6,2020) (mval - 1), new(0)
        DO iter1 = 2,imax
            IF ( iter1 == 1 ) THEN
                new(1) = (4.d0*new(0) - old(0))/3.d0
            ELSE
                old(0) = new(0)
                CALL update()
                
                ! Refine the estimate of the integral
                fact = 1.d0
                DO iter2 = 1,iter1
                    fact = fact*base
                    old(iter2) = new(iter2)
                    new(iter2) = (fact*new(iter2-1) - old(iter2-1))/   &
                                 (fact - 1.d0)
                END DO
            END IF

            ! Update errors
            error = 2.d0*ABS((new(iter1) - new(iter1-1))/((new(iter1)  &
                    + new(iter1-1))))

            ! Write new row of extrapolation table
            IF ( ( verbose ) .AND. ( iter1 < 5 ) ) WRITE( 6,2020) mval,&
                                                   new(0:iter1)
            
            ! Check if integration has converged
            IF ( ( error <= tol ) .OR. ( ABS(new(iter1)) <= tol ) ) THEN
                cnvrgd = .TRUE.
                EXIT
            END IF
        END DO

        IF ( ( verbose ) .AND. ( iter1 > 5 ) ) WRITE( 6,2021)          &
                                               MIN(iter1,imax)

        ! Write warning message if necessary and set 
        IF ( .NOT. cnvrgd ) WRITE( 6,2030) imax
        intval = new(MIN(iter1,imax))

    !------------------------------------------------------------------!
    !                      Write format statements                     !
    !------------------------------------------------------------------!
        2000 FORMAT('ROMBERG : WARNING: Rhomburg ratio < 3 after ',    &
                    'more than ',I0,' iterations.',/,                  &
                    'ROMBERG : WARNING: Starting Richardson ',         &
                    'Extrapolation anyway.')
        2010 FORMAT('ROMBERG : mval   T0               T1           ', &
                    '    T2               T3               T4')
        2020 FORMAT('ROMBERG : ',I3,' ',1P11G17.9)
        2021 FORMAT('ROMBERG : Continuing for ',I2,' iterations...')
        2030 FORMAT('ROMBERG : WARNING: Rhomburg integration failed ', &
                    'to converge after ',I2,' iterations.',/,          &
                    'ROMBERG : Extrapolation terminated.')
    !------------------------------------------------------------------!

        RETURN
    CONTAINS
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
        SUBROUTINE update
            IMPLICIT NONE
            
            ! Declare variables:
            INTEGER ::  iter
        !--------------------------------------------------------------!
        
            ! Increase resolution of integration
            mval = mval + 1
            hval = hval/2.d0
            new(0) = new(0)/2.d0
            
            ! Compute integral at points needed for higher resolution
            DO iter = 1, (2**(mval - 1))
                new(0) = new(0) + hval*func(left + DBLE(2*iter - 1)    &
                         *hval)
                numcalls = numcalls + 1
            END DO
            
        END SUBROUTINE update
        !==============================================================!
        !                E N D    S U B R O U T I N E :                !
        !                         U P D A T E                          !
        !==============================================================!

    END SUBROUTINE romberg
    !==================================================================!
    !                  E N D    S U B R O U T I N E :                  !
    !                          R O M B E R G                           !
    !==================================================================!


END MODULE calculus
!======================================================================!
!                        E N D    M O D U L E :                        !
!                           C A L C U L U S                            !
!======================================================================!
