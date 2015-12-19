!  abstract_container_mod.f90
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

module abstract_container_mod
  use iso_fortran_env, only: stderr => error_unit
  implicit none
  private

  type, abstract ::   container_type
    private
    character(len=1), dimension(:), allocatable   ::  storage
    logical ::  filled = .false.
  contains
    private
    procedure(guard), deferred ::  typeguard
    procedure, public   ::  contents
    procedure, public   ::  set
    procedure, pass(rhs)    ::  assign_container
    generic, public :: assignment(=) => assign_container
  end type container_type

  abstract interface
    logical function guard(this, lhs)
      import container_type
      class(container_type), intent(in) ::  this
      class(*), intent(inout) ::  lhs
    end function guard
  end interface
  
  public    ::  container_type
  
contains
  
  subroutine assign_container(lhs, rhs)
    class(*), intent(inout) ::  lhs
    class(container_type), intent(in)  ::  rhs
    !-------------------------------------------------------------------
    select type(lhs)
      class is(container_type)
        lhs%storage = rhs%storage
      class default
        if (rhs%filled) then
          if (rhs%typeguard(lhs)) return
          write(stderr,*) "ERROR: Can not assign this container's contents to given variable"
!~           call backtrace
          stop
        else
          write(stderr,*) "ERROR: Container is empty."
!~           call backtrace
          stop
        end if
    end select
  end subroutine assign_container

  pure function contents(this)
    class(container_type), intent(in)   ::  this
    character(len=1), dimension(:), allocatable ::  contents
    contents = this%storage
  end function contents

  subroutine set(this, content)
    class(container_type), intent(out)  ::  this
    class(*), intent(in)    ::  content
    class(*), allocatable   ::  tmp
!~     select type(content)
!~       type is(character(len=*))
!~       print*, content
!~     end select
    allocate(tmp, source=content)
    if (.not. allocated(this%storage)) allocate(this%storage(1))
    if (this%typeguard(tmp)) then
      this%filled = .true.
      this%storage = transfer(content, this%storage)
    else
      write(stderr,*) "ERROR: Can not assign given variable to this container"      
!~       call backtrace
      stop
    end if
  end subroutine set

end module abstract_container_mod


!~ module container_test
!~   use abstract_container_mod
  
!~   type, extends(container_type) ::  int_container
!~   contains
!~     private
!~     procedure   ::  typeguard => intguard
!~   end type int_container
  
!~ contains
  
!~   logical function intguard(this, lhs)
!~     class(int_container), intent(in) ::  this
!~     class(*), intent(inout) ::  lhs
!~     select type(lhs)
!~       type is(integer)
!~         lhs = transfer(this%contents(), lhs)
!~         intguard = .true.
!~       class default
!~         intguard = .false.
!~     end select
!~   end function intguard
  
!~ end module container_test

!~ program test
!~   use container_test
!~   type(int_container)   ::  t
!~   integer   ::  inval = 1, outval = -1
!~   real :: rtest = 1.0
!~   call t%set(inval)
!~   outval = t
!~   print*, outval
!~!~   call t%set(rtest)
!~   rtest = t
!~ end program test
