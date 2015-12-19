!  container_test.f90
!  
!  Copyright 2015 Christopher MacMackin <cmacmackin@gmail.com>
!  
!  This program is free software; you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation; either version 2 of the License, or
!  (at your option) any later version.
!  
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
!  MA 02110-1301, USA.
!  
!  

program container_test
  !! Author: Chris MacMackin
  !! Date: December 2015
  !! License: GPLv3
  !! 
  !! A test suite for the various concrete implementations of the
  !! [[container_type]]. These implementations can be found int
  !! [[container_mod]] and are for all of the default scalar data types.
  use container_mod
  use abstract_container_mod
  use iso_fortran_env, only: i1 => int8, i2 => int16, i4 => int32, &
                             i8 => int64, r4 => real32, r8 => real64, &
                             r16 => real128
  implicit none
  
  ! container types to test
  type(character_container) :: char_c
  type(complex_container) :: comp_c
  type(complex4_container) :: comp4_c
  type(complex8_container) :: comp8_c
  type(complex16_container) :: comp16_c
  type(int_container) :: int_c
  type(int1_container) :: int1_c
  type(int2_container) :: int2_c
  type(int4_container) :: int4_c
  type(int8_container) :: int8_c
  type(logical_container) :: log_c
  type(real_container) :: real_c
  type(real4_container) :: real4_c
  type(real8_container) :: real8_c
  type(real16_container) :: real16_c

  ! variables in which to store input/output from containers
  character(len=12) :: charin = 'Hello world!', charout = ''
  complex :: compin = (1.0, 2.0), compout = (0.0, 0.0)
  complex(r4) :: comp4in = (1.0_r4, 2.0_r4), comp4out = (0.0_r4, 0.0_r4)
  complex(r8) :: comp8in = (1.0_r8, 2.0_r8), comp8out = (0.0_r8, 0.0_r8)
  complex(r16) :: comp16in = (1.0_r16, 2.0_r16), comp16out = (0.0_r16, 0.0_r16)
  logical :: login = .true., logout = .false.
  integer :: intin = 1, intout = 0
  integer(i1) :: int1in = 1_i1, int1out = 0_i1
  integer(i2) :: int2in = 1_i2, int2out = 0_i2
  integer(i4) :: int4in = 1_i4, int4out = 0_i4
  integer(i8) :: int8in = 1_i8, int8out = 0_i8
  real :: realin = 1.0, realout = 0.0
  real(r4) :: real4in = 1.0_r4, real4out = 0.0_r4
  real(r8) :: real8in = 1.0_r8, real8out = 0.0_r8
  real(r16) :: real16in = 1.0_r16, real16out = 0.0_r16
  
  call perform_test(char_c, charin, charout, 'character_container')
  call perform_test(comp_c, compin, compout, 'complex_container')
  call perform_test(comp4_c, comp4in, comp4out, 'complex4_container')
  call perform_test(comp8_c, comp8in, comp8out, 'complex8_container')
  call perform_test(comp16_c, comp16in, comp16out, 'complex16_container')
  call perform_test(log_c, login, logout, 'logical_container')
  call perform_test(int_c, intin, intout, 'int_container')
  call perform_test(int1_c, int1in, int1out, 'int1_container')
  call perform_test(int2_c, int2in, int2out, 'int2_container')
  call perform_test(int4_c, int4in, int4out, 'int4_container')
  call perform_test(int8_c, int8in, int8out, 'int8_container')
  call perform_test(real_c, realin, realout, 'real_container')
  call perform_test(real4_c, real4in, real4out, 'real4_container')
  call perform_test(real8_c, real8in, real8out, 'real8_container')
  call perform_test(real16_c, real16in, real16out, 'real16_container')

contains

  subroutine perform_test (container, inval, outval, cont_name)
    !! Performs the actual test on the containers. Also is effectively
    !! testing that the polymorphism of [[container_type]] is behaving
    !! as desired.
    
    class(container_type), intent(inout) :: container
      !! The particular container which is to be tested
    class(*), intent(in) :: inval
      !! The value to place into the container
    class(*), intent(inout) :: outval
      !! A variable in which to place the container's contents
    character(len=*), intent(in) :: cont_name
      !! The name of the container class, to be used when printing output
      
    character, dimension(1) :: mold
    character(len=30), parameter :: header_form = '("Running test on ", a, "...")'
    character(len=39), parameter :: set_form = '("Setting container to input value...")'
    character(len=44), parameter :: get_form = '("Retreiving value from container...")'
    character(len=31), parameter :: result_form = '("Test of ", a, " ", a, ".", /)'

    write(*,header_form) cont_name
    write(*,set_form)
    call container%set(inval)
    write(*,get_form)
    outval = container
    if (all(transfer(outval, mold) == transfer(inval, mold))) then
      write(*,result_form) cont_name, 'PASSED'
    else
      write(*,result_form) cont_name, 'FAILED'
    end if
    
  end subroutine perform_test

end program container_test
