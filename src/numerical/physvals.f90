!======================================================================!
!                      B E G I N    M O D U L E :                      !
!                           P H Y S V A L S                            !
!======================================================================!
!
!!  @author Christopher MacMackin
!
!!  Contains physical constants which can be easily imported into 
!!  other programs. This is an evolving module, with new constants 
!!  being added as they are needed. All values are SI, unless 
!!  otherwise indicated.
!
!----------------------------------------------------------------------!
module physvals
    implicit none

    !! An astronomical unit.
    real(8), parameter  ::  auval = 1.4959787066d11 
    !! The density of the Earth.
    real(8), parameter  ::  d_earth = 5.514d3
    !! The density of the Sun.
    real(8), parameter  ::  d_sol = 1.409d3
    !! The speed of light.
    real(8), parameter  ::  cvel = 2.99792458d8
    !! Newton's universal gravitational constant.
    real(8), parameter  ::  gconst = 6.6742867d-11
    !! The reduced Planck constant.
    real(8), parameter  ::  hbar = 1.05457162853d-34
    !! The luminosity of the Sun.
    real(8), parameter  ::  l_sol = 3.8395d26
    !! The mass of the Earth.
    real(8), parameter  ::  m_earth = 5.9736d24
    !! The mass of an electron.
    real(8), parameter  ::  m_elec = 9.10938215d-31
    !! The mass of the Sun.
    real(8), parameter  ::  m_sol = 1.9891d30
    !! Pi, the ratio of the circumference of a circle to its diameter.
    real(8), parameter  ::  pi = 3.1415926535897932d0
    !! The charge of an electron or a proton.
    real(8), parameter  ::  q_elec = 1.60217648740d-19
    !! The number of seconds in a year.
    real(8), parameter  ::  secsyear = 3.15576d7

end module physvals    
!======================================================================!
!                        E N D    M O D U L E :                        !
!                           P H Y S V A L S                            !
!======================================================================!
