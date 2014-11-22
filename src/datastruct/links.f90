!  links.f90
!  
!  Copyright 2014 Christopher MacMackin <cmacmackin@gmail.com>
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
 

!==============================================================================!
!                          B E G I N    M O D U L E :                          !
!                                  L I N K S                                   !
!==============================================================================!
!
!!  @author  Christopher MacMackin
!
!!  Contains a link derived types from which various linked-list type structures
!!  can be made. End users most likely wouldn't make direct use of anything in
!!  this module.
!!
!!  *Date*:  November, 2014
!
!------------------------------------------------------------------------------!
module links
    implicit none
    private
    
    ! Variable declarations
    
    !! The component element of a linked-list. The procedures here do not
    !! protect against memory leaks because this is not meant to be used as an
    !! opaque object.
    type :: link
        private
        !! The datum contained by the link.
        class(*), pointer       ::  contents => null()
        !! The next link in the chain.
        class(link), pointer    ::  next => null()
    contains
        procedure   ::  get_contents    !! Returns the value held by the link.
        procedure   ::  set_contents    !! Sets the value to be held by the link.
        procedure   ::  get_next        !! Returns the next link in the chain.
        procedure   ::  set_next        !! Sets the next link in the chain.
    end type link
    
    
    !! The component object of a doubly-linked list. The procedures here do not
    !! protect against memory leaks because this is not meant to be used as an
    !! opaque object.
    type, extends(link) :: double_link
        private
        !! The previous link in the chain
        class(double_link), pointer ::  prev => null()
    contains
        procedure   ::  get_prev    !! Returns the previous link in the chain.
        procedure   ::  set_prev    !! Sets the previous link in the chain.
    end type double_link
    
    public  ::  link, double_link
    
!------------------------------------------------------------------------------!
contains

    !==========================================================================!
    !                      B E G I N    F U N C T I O N :                      !
    !                         G E T _ C O N T E N T S                          !
    !==========================================================================!
    !
    !!  Returns the value held by the link.
    !
    !!  *Date*:  November, 2014
    !
    !--------------------------------------------------------------------------!
    function get_contents(this)
        
        ! Input and output variables:
        !! The object which calls this function
        class(link), intent(in) ::  this
        class(*), pointer       ::  get_contents
    !--------------------------------------------------------------------------!  
        
        get_contents => this%contents
        return
    end function get_contents
    !==========================================================================!
    !                       E N D    F U N C T I O N S :                       !
    !                         G E T _ C O N T E N T S                          !
    !==========================================================================!

    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                         S E T _ C O N T E N T S                          !
    !==========================================================================!
    !!  Places new contents in the link.  
    subroutine set_contents(this,new_contents)
        
        ! Input and output variables:
        !! The object which calls this function
        class(link), intent(inout)       ::  this
        !! The datum which will now be held by this link.
        class(*), pointer, intent(in) ::  new_contents
    !--------------------------------------------------------------------------!  
        
        this%contents => new_contents
        return
    end subroutine set_contents
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                         S E T _ C O N T E N T S                          !
    !==========================================================================!
    
    !==========================================================================!
    !                      B E G I N    F U N C T I O N :                      !
    !                             G E T _ N E X T                              !
    !==========================================================================!
    !
    !!  Returns the next link in the chain.  
    !
    !!  *Date*:  November, 2014
    !
    !--------------------------------------------------------------------------!
    function get_next(this)
        
        ! Input and output variables:
        !! The object which calls this function
        class(link), intent(in) ::  this
        class(link), pointer    ::  get_next
    !--------------------------------------------------------------------------!  
        
        get_next => this%next
        return
    end function get_next
    !==========================================================================!
    !                       E N D    F U N C T I O N S :                       !
    !                             G E T _ N E X T                              !
    !==========================================================================!
        
    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                             S E T _ N E X T                              !
    !==========================================================================!
    !
    !!  Sets the next link in the chain.  
    !
    !!  *Date*:  November, 2014
    !
    !--------------------------------------------------------------------------!
    subroutine set_next(this,new_link)
        
        ! Input and output variables:
        !! The object which calls this function
        class(link), intent(inout)          ::  this
        !! The link which will now be next in the chain.
        class(link), pointer, intent(in)    ::  new_link
    !--------------------------------------------------------------------------!  
        
        this%next => new_link
        return
    end subroutine set_next
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                             S E T _ N E X T                              !
    !==========================================================================!


    !==========================================================================!
    !                      B E G I N    F U N C T I O N :                      !
    !                             G E T _ P R E V                              !
    !==========================================================================!
    !
    !!  Returns the next link in the chain.  
    !
    !!  *Date*:  November, 2014
    !
    !--------------------------------------------------------------------------!
    function get_prev(this)
    !!  Returns the next link in the chain.  
        
        class(double_link), intent(in) ::  this !! The object which calls this function
        class(double_link), pointer    ::  get_prev
    !--------------------------------------------------------------------------!  
        
        get_prev => this%prev
        return
    end function get_prev
    !==========================================================================!
    !                       E N D    F U N C T I O N S :                       !
    !                             G E T _ P R E V                              !
    !==========================================================================!
        
    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                             S E T _ P R E V                              !
    !==========================================================================!
    !
    !!  Sets the previous link in the chain.  
    !
    !!  *Date*:  November, 2014
    !
    !--------------------------------------------------------------------------!
    subroutine set_prev(this,new_link)
        
        ! Input and output variables:
        !! The object which calls this function
        class(double_link), intent(inout)       ::  this
        !! The link which will now be next in the chain.
        class(double_link), pointer, intent(in) ::  new_link
    !--------------------------------------------------------------------------!  
        
        this%prev => new_link
        return
    end subroutine set_prev
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                             S E T _ P R E V                              !
    !==========================================================================!


end module links
!==============================================================================!
!                            E N D    M O D U L E :                            !
!                                  L I N K S                                   !
!==============================================================================!
