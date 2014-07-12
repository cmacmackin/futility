!======================================================================!
!                      B E G I N    M O D U L E :                      !
!                             L I N A L G                              !
!======================================================================!
!                                                                      !
!   PURPOSE:    Provides subroutines and functions to perform linear   !
!               algebra tasks.                                         !
!   CONTAINS:   linsolve (subroutine), lsfit (subroutine), ludecomp    !
!               (subroutine)                                           !
!   EXTERNALS:  none                                                   !
!                                                                      !
!----------------------------------------------------------------------!
MODULE linalg
CONTAINS

    !==================================================================!
    !                B E G I N    S U B R O U T I N E :                !
    !                         L I N S O L V E                          !
    !==================================================================!
    !                                                                  !
    !   AUTHOR:         Christopher MacMackin                          !
    !   WRITTEN:        March, 2014                                    !
    !   MODIFICATIONS:  None                                           !
    !                                                                  !
    !   PURPOSE:        Solves a system of linear equations using the  !
    !                   (pre-computed) LU decomposition of the matrix  !
    !                   describing the system. The decomposition can   !
    !                   be computed using the subroutine ludecomp,     !
    !                   below.                                         !
    !                                                                  !
    !   ARGUMENTS:     *matrix  , a square, 2-dimensional array, which !
    !                             is the LU factorization of the       !
    !                             matrix containing the coefficients   !
    !                             of the linear system to be solved.   !
    !                   reval   , the return value of the subroutine.  !
    !                             A value of 0 indicates that the      !
    !                             subroutine executed as expected. A   !
    !                             value of -1 indicates that the       !
    !                             matrix of coefficients passed to     !
    !                             either linsolve or ludecomp was not  !
    !                             square, as it should have been. A    !
    !                             value of -2 indicates that the       !
    !                             coeffs array passed to lsfit is not  !
    !                             large enough to hold all of the      !
    !                             coefficients for the desired fit.    !
    !                             A value of -3 means that there is    !
    !                             insufficient data for lsfit to fit a !
    !                             polynomial of the desired order.     !
    !                  *vals    , a 1-dimensional array containing the !
    !                             right-hand-side of each equation in  !
    !                             the linear system. It is in this     !
    !                             array that the solution will be      !
    !                             returned.                            !
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
    SUBROUTINE linsolve ( matrix  , reval   , vals     )
        IMPLICIT NONE
        
        ! Input and output variables:
        REAL(8), INTENT(IN), DIMENSION(:,:)                 ::  matrix
        REAL(8), INTENT(INOUT), DIMENSION(SIZE(matrix,1))   ::  vals
        INTEGER, INTENT(OUT)                                ::  reval
        
        ! Other variables:
        INTEGER                 ::  i,                                 &
                                    j,                                 &
                                    rank
        INTEGER, DIMENSION(2)   ::  matshape
    !------------------------------------------------------------------!

        ! Initialize error check variable
        reval = 0

        ! Make sure that matrix is square
        matshape = SHAPE(matrix)
        IF ( matshape(1) == matshape(2) ) THEN
            rank = matshape(1)
        ELSE
            WRITE(0 ,2000) matshape
            reval = -1
            RETURN
        END IF
        
        ! Perform forward substitution with L matrix
        vals(1) = vals(1)/matrix(1,1)
        DO i = 2,rank
            DO j = 1,(i-1)
                vals(i) = vals(i) - matrix(i,j)*vals(j)
            END DO
            vals(i) = vals(i)/matrix(i,i)
        END DO

        ! Perform backward substitution with U matrix
        DO i = (rank-1),1,-1
            DO j = (i+1),rank
                vals(i) = vals(i) - matrix(i,j)*vals(j)
            END DO
        END DO
        
    !------------------------------------------------------------------!
    !                      Write format statements                     !
    !------------------------------------------------------------------!
        2000 FORMAT('LINSOLVE: ERROR: Argument is matrix of size ',I5, &
                    ' by ',I5,'.',/,                                   &
                    'LINSOLVE: ERROR: Argument must be a square ',     &
                    'matrix.')
    !------------------------------------------------------------------!

        RETURN
    END SUBROUTINE linsolve
    !==================================================================!
    !                  E N D    S U B R O U T I N E :                  !
    !                         L I N S O L V E                          !
    !==================================================================!



    !==================================================================!
    !                B E G I N    S U B R O U T I N E :                !
    !                            L S F I T                             !
    !==================================================================!
    !                                                                  !
    !   AUTHOR:         Christopher MacMackin                          !
    !   WRITTEN:        March, 2014                                    !
    !   MODIFICATIONS:  None                                           !
    !                                                                  !
    !   PURPOSE:        Finds the least-square quadratic fit for a set !
    !                   of data.                                       !
    !                                                                  !
    !   ARGUMENTS:      coeffs  , a 1-dimensional array with a length  !
    !                             of at least 'order,' in which the    !
    !                             coefficients of the fit will be      !
    !                             returned.                            !
    !                   lsval   , the square difference values (error) !
    !                             of the fit.                          !
    !                  *order   , the order of the polynomial to be    !
    !                             fit to the data.                     !
    !                   reval   , the return value of the subroutine.  !
    !                             A value of 0 indicates that the      !
    !                             subroutine executed as expected. A   !
    !                             value of -1 indicates that the       !
    !                             matrix of coefficients passed to     !
    !                             either linsolve or ludecomp was not  !
    !                             square, as it should have been. A    !
    !                             value of -2 indicates that the       !
    !                             coeffs array passed to lsfit is not  !
    !                             large enough to hold all of the      !
    !                             coefficients for the desired fit.    !
    !                             A value of -3 means that there is    !
    !                             insufficient data for lsfit to fit a !
    !                             polynomial of the desired order.     !
    !                  *xvals   , a 1-dimensional array containing the !
    !                             independent variable.                !
    !                  *yvals   , a 1-dimensional array containing the !
    !                             dependent variable.                  !
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
    SUBROUTINE lsfit    ( coeffs  , lsval   , order   , reval   ,      &
                          xvals   , yvals    )
        IMPLICIT NONE
        
        ! Input and output variables:
        INTEGER, INTENT(IN)                 ::  order
        REAL(8), INTENT(IN), DIMENSION(:)   ::  xvals,                 &
                                                yvals
        INTEGER, INTENT(OUT)                ::  reval
        REAL(8), INTENT(OUT)                ::  lsval
        REAL(8), INTENT(OUT), DIMENSION(:)  ::  coeffs
        
        ! Other variables:
        INTEGER                                 ::  iter1,             &
                                                    iter2,             &
                                                    iter3,             &
                                                    numdat
        REAL(8)                                 ::  square
        REAL(8), ALLOCATABLE, DIMENSION(:,:)    ::  matrix
    !------------------------------------------------------------------!

        ! Initialize error check variable
        reval = 0

        ! Check that the coeffs array is sufficiently large
        IF ( (order + 1) > SIZE(coeffs) ) THEN
            WRITE(0 ,2000) order
            reval = -2
            RETURN
        END IF
        
        ! Check that sufficient number of data-points to fit polynomial
        ! of desired order
        numdat = MIN(SIZE(xvals),SIZE(yvals))
        IF ( numdat < (order + 1) ) THEN
            WRITE(0 ,2010) order
            reval = -3
            RETURN
        END IF
        
        ! Initialize variables
        ALLOCATE(matrix(order+1,order+1))
        matrix = 0.d0
        lsval = 0.d0
        coeffs = 0.d0
        
        ! Fill arrays
        DO iter1 = 1,numdat
            DO iter2 = 1,(order + 1)
                coeffs(iter2) = coeffs(iter2) +                        &
                                xvals(iter1)**DBLE(iter2 - 1)*         &
                                yvals(iter1)
                DO iter3 = 1,(order + 1)
                    matrix(iter2,iter3) = matrix(iter2,iter3) +        &
                                          xvals(iter1)**DBLE(iter2 +   &
                                          iter3 - 2)
                END DO
            END DO
        END DO
        
        ! Compute best fit
        CALL ludecomp(matrix, reval)
        CALL linsolve(matrix, reval, coeffs)
        
        ! Find square differences
        DO iter1 = 1,numdat
            square = -yvals(iter1)
            DO iter2 = 0,order
                square = square + coeffs(iter2+1)*                     &
                         xvals(iter1)**DBLE(iter2)
            END DO
            lsval = lsval + square**2.d0
        END DO
        
        DEALLOCATE(matrix)
        
    !------------------------------------------------------------------!
    !                      Write format statements                     !
    !------------------------------------------------------------------!
        2000 FORMAT('LINSOLVE: ERROR: Coeffs array can not hold all ', &
                    'coefficients needed for',/,                       &
                    'LINSOLVE: ERROR: an ',I5,'th order polynomial.')
        2010 FORMAT('LINSOLVE: ERROR: Insufficient data to fit ',I5,   &
                    'th order polynomial.')
    !------------------------------------------------------------------!

        RETURN
    END SUBROUTINE lsfit
    !==================================================================!
    !                  E N D    S U B R O U T I N E :                  !
    !                            L S F I T                             !
    !==================================================================!



    !==================================================================!
    !                B E G I N    S U B R O U T I N E :                !
    !                         L U D E C O M P                          !
    !==================================================================!
    !                                                                  !
    !   AUTHOR:         Christopher MacMackin                          !
    !   WRITTEN:        March, 2014                                    !
    !   MODIFICATIONS:  None                                           !
    !                                                                  !
    !   PURPOSE:        Performs LU decomposition on the provided      !
    !                   square matrix.                                 !
    !                                                                  !
    !   ARGUMENTS:     *matrix  , a square, 2-dimensional array, which !
    !                             is to be factored. The factorization !
    !                             is returned within this matrix as    !
    !                             well. The diagonal of ones in the    !
    !                             upper triangular matrix is not       !
    !                             explicitly represented, as the       !
    !                             values of these elements are known.  ! 
    !                   reval   , the return value of the subroutine.  !
    !                             A value of 0 indicates that the      !
    !                             subroutine executed as expected. A   !
    !                             value of -1 indicates that the       !
    !                             matrix of coefficients passed to     !
    !                             either linsolve or ludecomp was not  !
    !                             square, as it should have been. A    !
    !                             value of -2 indicates that the       !
    !                             coeffs array passed to lsfit is not  !
    !                             large enough to hold all of the      !
    !                             coefficients for the desired fit.    !
    !                             A value of -3 means that there is    !
    !                             insufficient data for lsfit to fit a !
    !                             polynomial of the desired order.     !
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
    SUBROUTINE ludecomp ( matrix  , reval    )
        IMPLICIT NONE
        
        ! Input and output variables:
        REAL(8), INTENT(INOUT), DIMENSION(:,:)  ::  matrix
        INTEGER, INTENT(OUT)                    ::  reval
        
        ! Other variables:
        INTEGER                 ::  i,                                 &
                                    j,                                 &
                                    k,                                 &
                                    rank
        INTEGER, DIMENSION(2)   ::  matshape
    !------------------------------------------------------------------!

        ! Initialize error check variable
        reval = 0

        ! Make sure that matrix is square
        matshape = SHAPE(matrix)
        IF ( matshape(1) == matshape(2) ) THEN
            rank = matshape(1)
        ELSE
            WRITE(0 ,2000) matshape
            reval = -1
            RETURN
        END IF
        
        ! Perform factorization
        DO k = 1,rank
            DO i = k,rank
                DO j = 1,(k-1)
                    matrix(i,k) = matrix(i,k) - matrix(i,j)*matrix(j,k)
                    IF ( i /= k ) matrix(k,i) = matrix(k,i) -          &
                                                matrix(k,j)*matrix(j,i)
                END DO
                IF ( i /= k ) matrix(k,i) = matrix(k,i)/matrix(k,k)
            END DO
        END DO
        
    !------------------------------------------------------------------!
    !                      Write format statements                     !
    !------------------------------------------------------------------!
        2000 FORMAT('LINSOLVE: ERROR: Argument is matrix of size ',I5, &
                    ' by ',I5,'.',/,                                   &
                    'LINSOLVE: ERROR: Argument must be a square ',     &
                    'matrix.')
    !------------------------------------------------------------------!

        RETURN
    END SUBROUTINE ludecomp
    !==================================================================!
    !                  E N D    S U B R O U T I N E :                  !
    !                         L U D E C O M P                          !
    !==================================================================!


END MODULE linalg
!======================================================================!
!                        E N D    M O D U L E :                        !
!                             L I N A L G                              !
!======================================================================!
