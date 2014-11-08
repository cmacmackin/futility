!==============================================================================!
!                          B E G I N    M O D U L E :                          !
!                               F U T I L I T Y                                !
!==============================================================================!
!
!!  @author     Christopher MacMackin
!!  @version    v0.1
!
!!  A collection of utilities for Fortran. It uses elements of various Fortran
!!  standards, up to and including Fortran 2008. This includes numerical 
!!  subroutines, miscellanious helpful procedures, and at some point data 
!!  structures and a plotting system. This module loads all of the other ones 
!!  available in the _Futility_ package.
!
!------------------------------------------------------------------------------!
module futility
    use array_io
    use calculus
    use linalg
    use rootfind
    use physvals
end module futility
!==============================================================================!
!                            E N D    M O D U L E :                            !
!                               F U T I L I T Y                                !
!==============================================================================!
