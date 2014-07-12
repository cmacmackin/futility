!======================================================================!
!                      B E G I N    M O D U L E :                      !
!                           P H Y S V A L S                            !
!======================================================================!
!                                                                      !
!   PURPOSE:  Contains physical constants which can be easily imported !
!             into other programs. This is an evolving module, with    !
!             new constants being added as they are needed. All values !
!             are SI, unless otherwise indicated.                      !
!   CONTAINS: None                                                     !
!                                                                      !
!----------------------------------------------------------------------!
MODULE physvals
    IMPLICIT NONE

    REAL(8), PARAMETER  ::  hbar = 1.0545727d-34,                      &
                            m_elec = 9.1093897d-31,                    &
                            q_elec = 1.6021774d-19

END MODULE physvals    
!======================================================================!
!                        E N D    M O D U L E :                        !
!                           P H Y S V A L S                            !
!======================================================================!
