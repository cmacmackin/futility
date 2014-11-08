!==============================================================================!
!                          B E G I N    M O D U L E :                          !
!                               A R R A Y _ I O                                !
!==============================================================================!
!
!!  @author Chris MacMackin
!
!!  Provides subroutines to easily perform array input and output. They are 
!!  modelled after the "loadtxt" and "savetxt" subroutines in Numpy. Note: if 
!!  using this module, do not use logical unit 90 for any IO, as it is reserved 
!!  for use by these subroutines. Lines in the file beginning with the pound 
!!  symbol (#) are considered comments and will not be read. The comment 
!!  character can be changed using the set_comment() command. Works for both 
!!  integer and real data. Complex data IO has not yet been properly 
!!  implemented.
!
!------------------------------------------------------------------------------!
module array_io
    use, intrinsic  :: iso_fortran_env
    implicit none
    private

    ! Variable declarations
    character(len=1)                ::  com_char = '#'
    character(len=64), parameter    ::  allocate_err = '("ARRAY_IO: Array &
                                        &already allocated and NOCLOBBER & 
                                        &specified.")',                        &
                                        empty_err = '("ARRAY_IO: No data in &
                                        &file ",a,".")',                      &
                                        open_err = '("ARRAY_IO: Problem when & 
                                        &opening file. Error code ",I0,".")',  &
                                        read_err = '("ARRAY_IO: Problem reading&
                                        & from file. Error code ",I0,".")',    &
                                        write_err = '("ARRAY_IO: Problem &
                                        &writing to file. Error code ",I0,".")'
    character(len=128), parameter   ::  clob_err = '("ARRAY_IO: Unrecognized &
                                        &CLOBBER specification. Please select & 
                                        &''clobber'',",/,&
                                        &"ARRAY_IO: ''noclobber'', or &
                                        &''append''.")'
    integer, parameter              ::  io_unit = 90,                          &
                                        line_len = 1024,                       &
                                        stderr = 0,                            &
                                        stdin = 5,                             &
                                        stdout = 6
    
    ! Generic interfaces
    public  :: loadtxt, savetxt, file_size
    
    !! Loads arrays from text files into an array. Can work with one, two, four,
    !! and eight byte integer data and with four, eight, and sixteen byte real 
    !! data.
    interface loadtxt
        !module procedure loadtxt_dynamic_c8
        !module procedure loadtxt_dynamic_c16
        module procedure loadtxt_dynamic_i1
        module procedure loadtxt_dynamic_i2
        module procedure loadtxt_dynamic_i4
        module procedure loadtxt_dynamic_i8
        module procedure loadtxt_dynamic_r4
        module procedure loadtxt_dynamic_r8
        module procedure loadtxt_dynamic_r16
        !module procedure loadtxt_static_c8
        !module procedure loadtxt_static_c16
        module procedure loadtxt_static_i1
        module procedure loadtxt_static_i2
        module procedure loadtxt_static_i4
        module procedure loadtxt_static_i8
        module procedure loadtxt_static_r4
        module procedure loadtxt_static_r8
        module procedure loadtxt_static_r16
    end interface loadtxt
    
    !! Write quantities from an array to a text file. Can work with one, two, 
    !! four, and eight byte integer data and with four, eight, and sixteen  
    !! byte real data.
    interface savetxt
        !module procedure savetxt_c8
        !module procedure savetxt_c16
        module procedure savetxt_i1
        module procedure savetxt_i2
        module procedure savetxt_i4
        module procedure savetxt_i8
        module procedure savetxt_r4
        module procedure savetxt_r8
        module procedure savetxt_r16
    end interface savetxt
    
!------------------------------------------------------------------------------!
contains

    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                             F I L E _ S I Z E                            !
    !==========================================================================!
    !
    !!  Finds the amount of data in whatever file is currently associated with 
    !!  IO unit 90. Doesn't count lines with no data or entirely commented out.
    !
    !--------------------------------------------------------------------------!
    subroutine file_size ( nrows, ncols )
        implicit none
        
        ! Input and output variables
        !! Returns the number of rows of data present in the file. An empty 
        !! row is interpreted as the end of the file.
        integer, intent(out)    ::  nrows
        !! The number of columns of data present. This is determined from the 
        !! number of pieces of data in first row. Each datum must be separated 
        !! by white-space.
        integer, intent(out)    ::  ncols
        
        ! Other variables
        character(len=line_len) ::  line,                                      &
                                    tmp
        integer                 ::  i,                                         &
                                    ioerr
        logical                 ::  white_last
    !--------------------------------------------------------------------------!
        
        ! Initialize variables
        nrows = 0
        ncols = 0
        tmp = com_char
        
        ! Read in first non-blank, non-comment row
        do while ( tmp(1:1) == com_char )
            read(unit=io_unit, fmt='(a)', iostat=ioerr) line
            if ( ioerr /= 0 ) return
            tmp = adjustl(line(1:line_len-1)//com_char)
        end do
        
        ! Get number of columns
        white_last = .TRUE.
        do i = 1, len(trim(line))
            ! Check for beginning of a comment
            if ( line(i:i) == com_char ) exit
            
            ! Check for white-space
            if ( ( line(i:i) == ' ' ) .OR. ( line(i:i) == CHAR(11) ) ) then
                if ( .NOT. white_last ) then
                    white_last = .TRUE.
                end if
            else
                if ( white_last ) then
                    white_last = .false.
                    ncols = ncols + 1
                end if
            end if
        end do
        
        ! Get number of rows
        nrows = 1
        do while ( ioerr == 0 )
            read(unit=io_unit, fmt='(a)', iostat=ioerr) line
            tmp = adjustl(line(1:line_len-1)//com_char)
            if ( tmp(1:1) /= com_char ) nrows = nrows + 1
        end do

        return
    end subroutine file_size
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                             F I L E _ S I Z E                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                   L O A D T X T _ D Y N A M I C _ C 1 6                  !
    !==========================================================================!
    !
    !!  Reads a 4 byte complex 2D array of unknown size into an allocatable 
    !!  array.
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       character                                          !
    !                   c16array_dyn, the allocatable array which is to be     !
    !                       filled with data read from the file. complex(16)   !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. integer              !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       array if already allocated. Options are 'clobber'  !
    !                       (overwrite array if allocated), 'noclobber'        !
    !                       (produce error if array allocated), and 'append'   !
    !                       (append data to array if already allocated).       !
    !                       character                                          !
    !                                                                          !
    !   EXTERNALS:      file_size (subroutine)                                 !
    !                                                                          !
    !--------------------------------------------------------------------------!
!~     subroutine loadtxt_dynamic_c16 ( filename, c16array_dyn, clobber, errval )
!~         implicit none
!~         
!~         ! Input and output variables:
!~         character(len=*), intent(in)                            ::  filename
!~         character(len=*), optional, intent(in)                  ::  clobber
!~         integer, optional, intent(out)                          ::  errval
!~         complex(16), allocatable, dimension(:,:), intent(inout) ::  c16array_dyn
!~         
!~         ! Other variables
!~         character(len=line_len)                     ::  line,                  &
!~                                                         tmp_char
!~         integer                                     ::  i,                     &
!~                                                         ioerr,                 &
!~                                                         ncols,                 &
!~                                                         nrows,                 &
!~                                                         nrows_old
!~         complex(16), allocatable, dimension(:,:)    ::  tmp
!~     !--------------------------------------------------------------------------!
!~ 
!~         ! Determine options for overwriting array
!~         if ( present(clobber) ) then
!~             select case (clobber)
!~                 case ( 'clobber' ) 
!~                     if ( allocated(c16array_dyn) ) deallocate(c16array_dyn)
!~                 case ( 'noclobber' )
!~                     if ( allocated(c16array_dyn) ) then
!~                         if ( present(errval) ) then
!~                             errval = -6000
!~                             return
!~                         else
!~                             write(unit=stderr, fmt=allocate_err) 
!~                             stop
!~                         end if
!~                     end if
!~                 case ( 'append' )
!~                     if ( allocated(c16array_dyn) ) then
!~                         tmp = c16array_dyn
!~                         deallocate(c16array_dyn)
!~                     end if
!~                 case default
!~                     if ( present(errval) ) then
!~                         errval = -5000
!~                         return
!~                     else
!~                         write(unit=stderr, fmt=clob_err)
!~                         stop
!~                     end if
!~             end select
!~         else
!~             if ( allocated(c16array_dyn) ) deallocate(c16array_dyn)
!~         end if
!~ 
!~         ! Open IO stream
!~         open(unit=io_unit, file=trim(filename), action='read', status='old',   &
!~              iostat=ioerr)
!~         if ( ioerr /= 0 ) then
!~             if ( present(errval) ) then
!~                 errval = ioerr
!~                 return
!~             else
!~                 write(unit=stderr, fmt=open_err) ioerr
!~                 stop
!~             end if
!~         end if
!~         
!~         ! Find number of rows and columns of data to collect.
!~         call file_size(nrows,ncols)
!~         ncols = ncols / 2
!~         if ( ncols == 0 ) then
!~             if ( present(errval) ) then
!~                 errval = -8000
!~                 return
!~             else
!~                 write(unit=stderr, fmt=empty_err) filename
!~                 stop
!~             end if
!~         end if
!~         
!~         ! Allocate array to correct size
!~         if ( allocated(tmp) ) then
!~             ncols = size(tmp,2)
!~             nrows_old = size(tmp,1)
!~             nrows = nrows + nrows_old
!~         else
!~             nrows_old = 0
!~         end if
!~         allocate(c16array_dyn(nrows,ncols))
!~         
!~         ! Read in array
!~         rewind(io_unit)
!~         i = nrows_old + 1
!~         do
!~             read(unit=io_unit, fmt='(A)', iostat=ioerr) line
!~             if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
!~             tmp_char = adjustl(line(1:line_len-1)//com_char)
!~             if ( tmp_char(1:1) /= com_char ) then
!~                 if ( index(line,com_char) == 0 ) then
!~                     read(unit=line, fmt=*, iostat=ioerr) c16array_dyn(i,:)
!~                 else
!~                     read(unit=line(1:index(line,com_char)-1), fmt=*,           &
!~                       iostat=ioerr) c16array_dyn(i,:)
!~                 end if
!~                 i = i + 1
!~             end if
!~             if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
!~                 if ( present(errval) ) then
!~                     errval = ioerr
!~                     return
!~                 else
!~                     write(unit=stderr, fmt=read_err) ioerr
!~                     stop
!~                 end if
!~             end if
!~         end do
!~             
!~         ! Append to array, if necessary
!~         if ( allocated(tmp) ) then
!~             c16array_dyn(1:nrows_old,:) = tmp(:,:)
!~             deallocate(tmp)
!~         end if
!~         
!~         ! Close IO stream
!~         close( io_unit )
!~         
!~         return
!~     end subroutine loadtxt_dynamic_c16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                   L O A D T X T _ D Y N A M I C _ C 1 6                  !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ C 8                   !
    !==========================================================================!
    !
    !!  Reads an 8 byte complex 2D array of unknown size into an allocatable 
    !!  array. 
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       character                                          !
    !                   c8array_dyn, the allocatable array which is to be      !
    !                       filled with data read in from the file. complex(8) !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. integer              !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       array if already allocated. Options are 'clobber'  !
    !                       (overwrite array if allocated), 'noclobber'        !
    !                       (produce error if array allocated), and 'append'   !
    !                       (append data to array if already allocated).       !
    !                       character                                          !
    !                                                                          !
    !   EXTERNALS:      file_size (subroutine)                                 !
    !                                                                          !
    !--------------------------------------------------------------------------!
!~     subroutine loadtxt_dynamic_c8 ( filename, c8array_dyn, clobber, errval )
!~         implicit none
!~         
!~         ! Input and output variables:
!~         character(len=*), intent(in)                            ::  filename
!~         character(len=*), optional, intent(in)                  ::  clobber
!~         integer, optional, intent(out)                          ::  errval
!~         complex(8), allocatable, dimension(:,:), intent(inout)  ::  c8array_dyn
!~         
!~         ! Other variables
!~         character(len=line_len)                 ::  line,                      &
!~                                                     tmp_char
!~         integer                                 ::  i,                         &
!~                                                     ioerr,                     &
!~                                                     ncols,                     &
!~                                                     nrows,                     &
!~                                                     nrows_old
!~         complex(8), allocatable, dimension(:,:) ::  tmp
!~     !--------------------------------------------------------------------------!
!~ 
!~         ! Determine options for overwriting array
!~         if ( present(clobber) ) then
!~             select case (clobber)
!~                 case ( 'clobber' ) 
!~                     if ( allocated(c8array_dyn) ) deallocate(c8array_dyn)
!~                 case ( 'noclobber' )
!~                     if ( allocated(c8array_dyn) ) then
!~                         if ( present(errval) ) then
!~                             errval = -6000
!~                             return
!~                         else
!~                             write(unit=stderr, fmt=allocate_err) 
!~                             stop
!~                         end if
!~                     end if
!~                 case ( 'append' )
!~                     if ( allocated(c8array_dyn) ) then
!~                         tmp = c8array_dyn
!~                         deallocate(c8array_dyn)
!~                     end if
!~                 case default
!~                     if ( present(errval) ) then
!~                         errval = -5000
!~                         return
!~                     else
!~                         write(unit=stderr, fmt=clob_err)
!~                         stop
!~                     end if
!~             end select
!~         else
!~             if ( allocated(c8array_dyn) ) deallocate(c8array_dyn)
!~         end if
!~ 
!~         ! Open IO stream
!~         open(unit=io_unit, file=trim(filename), action='read', status='old',   &
!~              iostat=ioerr)
!~         if ( ioerr /= 0 ) then
!~             if ( present(errval) ) then
!~                 errval = ioerr
!~                 return
!~             else
!~                 write(unit=stderr, fmt=open_err) ioerr
!~                 stop
!~             end if
!~         end if
!~         
!~         ! Find number of rows and columns of data to collect.
!~         call file_size(nrows,ncols)
!~         ncols = ncols / 2
!~         if ( ncols == 0 ) then
!~             if ( present(errval) ) then
!~                 errval = -8000
!~                 return
!~             else
!~                 write(unit=stderr, fmt=empty_err) filename
!~                 stop
!~             end if
!~         end if
!~         
!~         ! Allocate array to correct size
!~         if ( allocated(tmp) ) then
!~             ncols = size(tmp,2)
!~             nrows_old = size(tmp,1)
!~             nrows = nrows + nrows_old
!~         else
!~             nrows_old = 0
!~         end if
!~         allocate(c8array_dyn(nrows,ncols))
!~         
!~         ! Read in array
!~         rewind(io_unit)
!~         i = nrows_old + 1
!~         do
!~             read(unit=io_unit, fmt='(A)', iostat=ioerr) line
!~             if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
!~             tmp_char = adjustl(line(1:line_len-1)//com_char)
!~             if ( tmp_char(1:1) /= com_char ) then
!~                 if ( index(line,com_char) == 0 ) then
!~                     read(unit=line, fmt=*, iostat=ioerr) c8array_dyn(i,:)
!~                 else
!~                     read(unit=line(1:index(line,com_char)-1), fmt=*,           &
!~                       iostat=ioerr) c8array_dyn(i,:)
!~                 end if
!~                 i = i + 1
!~             end if
!~             if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
!~                 if ( present(errval) ) then
!~                     errval = ioerr
!~                     return
!~                 else
!~                     write(unit=stderr, fmt=read_err) ioerr
!~                     stop
!~                 end if
!~             end if
!~         end do
!~             
!~         ! Append to array, if necessary
!~         if ( allocated(tmp) ) then
!~             c8array_dyn(1:nrows_old,:) = tmp(:,:)
!~             deallocate(tmp)
!~         end if
!~         
!~         ! Close IO stream
!~         close( io_unit )
!~         
!~         return
!~     end subroutine loadtxt_dynamic_c8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ C 8                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ I 1                   !
    !==========================================================================!
    !
    !!  Reads in a 1 byte integer 2D array of unknown size into an allocatable
    !!  array.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_dynamic_i1 ( filename, i1array_dyn, clobber, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array.
        character(len=*), intent(in)            ::  filename
        !! Determines whether to overwrite array if already allocated. Options 
        !! are 'clobber' (overwrite array if allocated, default), 'noclobber'  
        !! (produce error if array allocated), and 'append' (append data to 
        !! array if already allocated).
        character(len=*), optional, intent(in)  ::  clobber
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)          ::  errval
        !! The allocatable array which is to be filled with data read in from 
        !! the file. 
        integer(int8), allocatable, dimension(:,:), intent(inout)              &
                                                ::  i1array_dyn
        
        ! Other variables
        character(len=line_len)                     ::  line,                  &
                                                        tmp_char
        integer                                     ::  i,                     &
                                                        ioerr,                 &
                                                        ncols,                 &
                                                        nrows,                 &
                                                        nrows_old
        integer(int8), allocatable, dimension(:,:)  ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        if ( present(clobber) ) then
            select case (clobber)
                case ( 'clobber' ) 
                    if ( allocated(i1array_dyn) ) deallocate(i1array_dyn)
                case ( 'noclobber' )
                    if ( allocated(i1array_dyn) ) then
                        if ( present(errval) ) then
                            errval = -6000
                            return
                        else
                            write(unit=stderr, fmt=allocate_err) 
                            stop
                        end if
                    end if
                case ( 'append' )
                    if ( allocated(i1array_dyn) ) then
                        tmp = i1array_dyn
                        deallocate(i1array_dyn)
                    end if
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            if ( allocated(i1array_dyn) ) deallocate(i1array_dyn)
        end if

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Find number of rows and columns of data to collect.
        call file_size(nrows,ncols)
        if ( ncols == 0 ) then
            if ( present(errval) ) then
                errval = -8000
                return
            else
                write(unit=stderr, fmt=empty_err) filename
                stop
            end if
        end if
        
        ! Allocate array to correct size
        if ( allocated(tmp) ) then
            ncols = size(tmp,2)
            nrows_old = size(tmp,1)
            nrows = nrows + nrows_old
        else
            nrows_old = 0
        end if
        allocate(i1array_dyn(nrows,ncols))
        
        ! Read in array
        rewind(io_unit)
        i = nrows_old + 1
        do
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) i1array_dyn(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) i1array_dyn(i,:)
                end if
                i = i + 1
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
            
        ! Append to array, if necessary
        if ( allocated(tmp) ) then
            i1array_dyn(1:nrows_old,:) = tmp(:,:)
            deallocate(tmp)
        end if
        
        ! Close IO stream
        close( io_unit )
        
        return
    end subroutine loadtxt_dynamic_i1
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ I 1                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ I 2                   !
    !==========================================================================!
    !
    !!  Reads in a 2 byte integer 2D array of unknown size into an allocatable 
    !!  array.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_dynamic_i2 ( filename, i2array_dyn, clobber, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array.
        character(len=*), intent(in)            ::  filename
        !! Determines whether to overwrite array if already allocated. Options 
        !! are 'clobber' (overwrite array if allocated, default), 'noclobber'  
        !! (produce error if array allocated), and 'append' (append data to 
        !! array if already allocated).
        character(len=*), optional, intent(in)  ::  clobber
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)          ::  errval
        !! The allocatable array which is to be filled with data read in from 
        !! the file.
        integer(int16), allocatable, dimension(:,:), intent(inout)             &
                                                ::  i2array_dyn
        
        ! Other variables
        character(len=line_len)                     ::  line,                  &
                                                        tmp_char
        integer                                     ::  i,                     &
                                                        ioerr,                 &
                                                        ncols,                 &
                                                        nrows,                 &
                                                        nrows_old
        integer(int16), allocatable, dimension(:,:) ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        if ( present(clobber) ) then
            select case (clobber)
                case ( 'clobber' ) 
                    if ( allocated(i2array_dyn) ) deallocate(i2array_dyn)
                case ( 'noclobber' )
                    if ( allocated(i2array_dyn) ) then
                        if ( present(errval) ) then
                            errval = -6000
                            return
                        else
                            write(unit=stderr, fmt=allocate_err) 
                            stop
                        end if
                    end if
                case ( 'append' )
                    if ( allocated(i2array_dyn) ) then
                        tmp = i2array_dyn
                        deallocate(i2array_dyn)
                    end if
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            if ( allocated(i2array_dyn) ) deallocate(i2array_dyn)
        end if

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Find number of rows and columns of data to collect.
        call file_size(nrows,ncols)
        if ( ncols == 0 ) then
            if ( present(errval) ) then
                errval = -8000
                return
            else
                write(unit=stderr, fmt=empty_err) filename
                stop
            end if
        end if
        
        ! Allocate array to correct size
        if ( allocated(tmp) ) then
            ncols = size(tmp,2)
            nrows_old = size(tmp,1)
            nrows = nrows + nrows_old
        else
            nrows_old = 0
        end if
        allocate(i2array_dyn(nrows,ncols))
        
        ! Read in array
        rewind(io_unit)
        i = nrows_old + 1
        do
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) i2array_dyn(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) i2array_dyn(i,:)
                end if
                i = i + 1
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
            
        ! Append to array, if necessary
        if ( allocated(tmp) ) then
            i2array_dyn(1:nrows_old,:) = tmp(:,:)
            deallocate(tmp)
        end if
        
        ! Close IO stream
        close( io_unit )
        
        return
    end subroutine loadtxt_dynamic_i2
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ I 2                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ I 4                   !
    !==========================================================================!
    !
    !!  Reads in a 4 byte integer 2D array of unknown size into an allocatable !!  array
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_dynamic_i4 ( filename, i4array_dyn, clobber, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array.
        character(len=*), intent(in)            ::  filename
        !! Determines whether to overwrite array if already allocated. Options 
        !! are 'clobber' (overwrite array if allocated, default), 'noclobber' 
        !! (produce error if array allocated), and 'append' (append data to 
        !! array if already allocated).
        character(len=*), optional, intent(in)  ::  clobber
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)          ::  errval
        !! The allocatable array which is to be filled with data read in from
        !! the file.
        integer(int32), allocatable, dimension(:,:), intent(inout)             &
                                                ::  i4array_dyn
        
        ! Other variables
        character(len=line_len)                     ::  line,                  &
                                                        tmp_char
        integer                                     ::  i,                     &
                                                        ioerr,                 &
                                                        ncols,                 &
                                                        nrows,                 &
                                                        nrows_old
        integer(int32), allocatable, dimension(:,:) ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        if ( present(clobber) ) then
            select case (clobber)
                case ( 'clobber' ) 
                    if ( allocated(i4array_dyn) ) deallocate(i4array_dyn)
                case ( 'noclobber' )
                    if ( allocated(i4array_dyn) ) then
                        if ( present(errval) ) then
                            errval = -6000
                            return
                        else
                            write(unit=stderr, fmt=allocate_err) 
                            stop
                        end if
                    end if
                case ( 'append' )
                    if ( allocated(i4array_dyn) ) then
                        tmp = i4array_dyn
                        deallocate(i4array_dyn)
                    end if
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            if ( allocated(i4array_dyn) ) deallocate(i4array_dyn)
        end if

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Find number of rows and columns of data to collect.
        call file_size(nrows,ncols)
        if ( ncols == 0 ) then
            if ( present(errval) ) then
                errval = -8000
                return
            else
                write(unit=stderr, fmt=empty_err) filename
                stop
            end if
        end if
        
        ! Allocate array to correct size
        if ( allocated(tmp) ) then
            ncols = size(tmp,2)
            nrows_old = size(tmp,1)
            nrows = nrows + nrows_old
        else
            nrows_old = 0
        end if
        allocate(i4array_dyn(nrows,ncols))
        
        ! Read in array
        rewind(io_unit)
        i = nrows_old + 1
        do
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) i4array_dyn(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) i4array_dyn(i,:)
                end if
                i = i + 1
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
            
        ! Append to array, if necessary
        if ( allocated(tmp) ) then
            i4array_dyn(1:nrows_old,:) = tmp(:,:)
            deallocate(tmp)
        end if
        
        ! Close IO stream
        close( io_unit )
        
        return
    end subroutine loadtxt_dynamic_i4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ I 4                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ I 4                   !
    !==========================================================================!
    !
    !!  Reads in an 8 byte integer 2D array of unknown size into an 
    !!  allocatable array.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_dynamic_i8 ( filename, i8array_dyn, clobber, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array.
        character(len=*), intent(in)            ::  filename
        !! Determines whether to overwrite array if already allocated. Options 
        !! are 'clobber' (overwrite array if allocated, default), 'noclobber' 
        !! (produce error if array allocated), and 'append' (append data to 
        !! array if already allocated).
        character(len=*), optional, intent(in)  ::  clobber
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)          ::  errval
        !! The allocatable array which is to be filled with data read in from 
        !! the file.
        integer(int64), allocatable, dimension(:,:), intent(inout)             &
                                                ::  i8array_dyn
        
        ! Other variables
        character(len=line_len)                     ::  line,                  &
                                                        tmp_char
        integer                                     ::  i,                     &
                                                        ioerr,                 &
                                                        ncols,                 &
                                                        nrows,                 &
                                                        nrows_old
        integer(int64), allocatable, dimension(:,:) ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        if ( present(clobber) ) then
            select case (clobber)
                case ( 'clobber' ) 
                    if ( allocated(i8array_dyn) ) deallocate(i8array_dyn)
                case ( 'noclobber' )
                    if ( allocated(i8array_dyn) ) then
                        if ( present(errval) ) then
                            errval = -6000
                            return
                        else
                            write(unit=stderr, fmt=allocate_err) 
                            stop
                        end if
                    end if
                case ( 'append' )
                    if ( allocated(i8array_dyn) ) then
                        tmp = i8array_dyn
                        deallocate(i8array_dyn)
                    end if
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            if ( allocated(i8array_dyn) ) deallocate(i8array_dyn)
        end if

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Find number of rows and columns of data to collect.
        call file_size(nrows,ncols)
        if ( ncols == 0 ) then
            if ( present(errval) ) then
                errval = -8000
                return
            else
                write(unit=stderr, fmt=empty_err) filename
                stop
            end if
        end if
        
        ! Allocate array to correct size
        if ( allocated(tmp) ) then
            ncols = size(tmp,2)
            nrows_old = size(tmp,1)
            nrows = nrows + nrows_old
        else
            nrows_old = 0
        end if
        allocate(i8array_dyn(nrows,ncols))
        
        ! Read in array
        rewind(io_unit)
        i = nrows_old + 1
        do
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) i8array_dyn(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) i8array_dyn(i,:)
                end if
                i = i + 1
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
            
        ! Append to array, if necessary
        if ( allocated(tmp) ) then
            i8array_dyn(1:nrows_old,:) = tmp(:,:)
            deallocate(tmp)
        end if
        
        ! Close IO stream
        close( io_unit )
        
        return
    end subroutine loadtxt_dynamic_i8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ I 8                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ R 4                   !
    !==========================================================================!
    !
    !!  Reads in a single precision 2D array of unknown size into an 
    !! allocatable array.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_dynamic_r4 ( filename, r4array_dyn, clobber, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array.
        character(len=*), intent(in)            ::  filename
        !! Determines whether to overwrite array if already allocated. Options 
        !! are 'clobber' (overwrite array if allocated, default), 'noclobber' 
        !! (produce error if array allocated), and 'append' (append data to  
        !! array if already allocated).
        character(len=*), optional, intent(in)  ::  clobber
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)          ::  errval
        !! The allocatable array which is to be filled with data read in from 
        !! the file.
        real(real32), allocatable, dimension(:,:), intent(inout)               &
                                                ::  r4array_dyn
        
        ! Other variables
        character(len=line_len)                     ::  line,                  &
                                                        tmp_char
        integer                                     ::  i,                     &
                                                        ioerr,                 &
                                                        ncols,                 &
                                                        nrows,                 &
                                                        nrows_old
        real(real32), allocatable, dimension(:,:)   ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        if ( present(clobber) ) then
            select case (clobber)
                case ( 'clobber' ) 
                    if ( allocated(r4array_dyn) ) deallocate(r4array_dyn)
                case ( 'noclobber' )
                    if ( allocated(r4array_dyn) ) then
                        if ( present(errval) ) then
                            errval = -6000
                            return
                        else
                            write(unit=stderr, fmt=allocate_err) 
                            stop
                        end if
                    end if
                case ( 'append' )
                    if ( allocated(r4array_dyn) ) then
                        tmp = r4array_dyn
                        deallocate(r4array_dyn)
                    end if
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            if ( allocated(r4array_dyn) ) deallocate(r4array_dyn)
        end if

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Find number of rows and columns of data to collect.
        call file_size(nrows,ncols)
        if ( ncols == 0 ) then
            if ( present(errval) ) then
                errval = -8000
                return
            else
                write(unit=stderr, fmt=empty_err) filename
                stop
            end if
        end if
        
        ! Allocate array to correct size
        if ( allocated(tmp) ) then
            ncols = size(tmp,2)
            nrows_old = size(tmp,1)
            nrows = nrows + nrows_old
        else
            nrows_old = 0
        end if
        allocate(r4array_dyn(nrows,ncols))
        
        ! Read in array
        rewind(io_unit)
        i = nrows_old + 1
        do
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) r4array_dyn(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) r4array_dyn(i,:)
                end if
                i = i + 1
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
            
        ! Append to array, if necessary
        if ( allocated(tmp) ) then
            r4array_dyn(1:nrows_old,:) = tmp(:,:)
            deallocate(tmp)
        end if
        
        ! Close IO stream
        close( io_unit )
        
        return
    end subroutine loadtxt_dynamic_r4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ R 4                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ R 8                   !
    !==========================================================================!
    !
    !!  Reads in a double precision 2D array of unknown size into an 
    !!  allocatable array.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_dynamic_r8 ( filename, r8array_dyn, clobber, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array.
        character(len=*), intent(in)            ::  filename
        !! Determines whether to overwrite array if already allocated. Options 
        !! are 'clobber' (overwrite array if allocated, default), 'noclobber' 
        !! (produce error if array allocated), and 'append' (append data to 
        !! array if already allocated).
        character(len=*), optional, intent(in)  ::  clobber
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)          ::  errval
        !! The allocatable array which is to be filled with data read in from 
        !! the file.
        real(real64), allocatable, dimension(:,:), intent(inout)               &
                                                ::  r8array_dyn
        
        ! Other variables
        character(len=line_len)                     ::  line,                  &
                                                        tmp_char
        integer                                     ::  i,                     &
                                                        ioerr,                 &
                                                        ncols,                 &
                                                        nrows,                 &
                                                        nrows_old
        real(real64), allocatable, dimension(:,:)   ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        if ( present(clobber) ) then
            select case (clobber)
                case ( 'clobber' ) 
                    if ( allocated(r8array_dyn) ) deallocate(r8array_dyn)
                case ( 'noclobber' )
                    if ( allocated(r8array_dyn) ) then
                        if ( present(errval) ) then
                            errval = -6000
                            return
                        else
                            write(unit=stderr, fmt=allocate_err) 
                            stop
                        end if
                    end if
                case ( 'append' )
                    if ( allocated(r8array_dyn) ) then
                        tmp = r8array_dyn
                        deallocate(r8array_dyn)
                    end if
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            if ( allocated(r8array_dyn) ) deallocate(r8array_dyn)
        end if

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Find number of rows and columns of data to collect.
        call file_size(nrows,ncols)
        if ( ncols == 0 ) then
            if ( present(errval) ) then
                errval = -8000
                return
            else
                write(unit=stderr, fmt=empty_err) filename
                stop
            end if
        end if
        
        ! Allocate array to correct size
        if ( allocated(tmp) ) then
            ncols = size(tmp,2)
            nrows_old = size(tmp,1)
            nrows = nrows + nrows_old
        else
            nrows_old = 0
        end if
        allocate(r8array_dyn(nrows,ncols))
        
        ! Read in array
        rewind(io_unit)
        i = nrows_old + 1
        do
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) r8array_dyn(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) r8array_dyn(i,:)
                end if
                i = i + 1
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
            
        ! Append to array, if necessary
        if ( allocated(tmp) ) then
            r8array_dyn(1:nrows_old,:) = tmp(:,:)
            deallocate(tmp)
        end if
        
        ! Close IO stream
        close( io_unit )
        
        return
    end subroutine loadtxt_dynamic_r8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ R 8                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                  L O A D T X T _ D Y N A M I C _ R 1 6                   !
    !==========================================================================!
    !
    !!  Reads in a 16 byte 2D real array of unknown size into an allocatable 
    !!  array.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_dynamic_r16 ( filename, r16array_dyn, clobber, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array.
        character(len=*), intent(in)            ::  filename
        !! Determines whether to overwrite array if already allocated. Options 
        !! are 'clobber' (overwrite array if allocated, default), 'noclobber' 
        !! (produce error if array allocated), and 'append' (append data to 
        !! array if already allocated).
        character(len=*), optional, intent(in)  ::  clobber
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)          ::  errval
        !! The allocatable array which is to be filled with data read in from 
        !! the file.
        real(real128), allocatable, dimension(:,:), intent(inout)              &
                                                ::  r16array_dyn
        
        ! Other variables
        character(len=line_len)                     ::  line,                  &
                                                        tmp_char
        integer                                     ::  i,                     &
                                                        ioerr,                 &
                                                        ncols,                 &
                                                        nrows,                 &
                                                        nrows_old
        real(real128), allocatable, dimension(:,:)  ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        if ( present(clobber) ) then
            select case (clobber)
                case ( 'clobber' ) 
                    if ( allocated(r16array_dyn) ) deallocate(r16array_dyn)
                case ( 'noclobber' )
                    if ( allocated(r16array_dyn) ) then
                        if ( present(errval) ) then
                            errval = -6000
                            return
                        else
                            write(unit=stderr, fmt=allocate_err) 
                            stop
                        end if
                    end if
                case ( 'append' )
                    if ( allocated(r16array_dyn) ) then
                        tmp = r16array_dyn
                        deallocate(r16array_dyn)
                    end if
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            if ( allocated(r16array_dyn) ) deallocate(r16array_dyn)
        end if

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Find number of rows and columns of data to collect.
        call file_size(nrows,ncols)
        if ( ncols == 0 ) then
            if ( present(errval) ) then
                errval = -8000
                return
            else
                write(unit=stderr, fmt=empty_err) filename
                stop
            end if
        end if
        
        ! Allocate array to correct size
        if ( allocated(tmp) ) then
            ncols = size(tmp,2)
            nrows_old = size(tmp,1)
            nrows = nrows + nrows_old
        else
            nrows_old = 0
        end if
        allocate(r16array_dyn(nrows,ncols))
        
        ! Read in array
        rewind(io_unit)
        i = nrows_old + 1
        do
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) exit
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) r16array_dyn(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) r16array_dyn(i,:)
                end if
                i = i + 1
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
            
        ! Append to array, if necessary
        if ( allocated(tmp) ) then
            r16array_dyn(1:nrows_old,:) = tmp(:,:)
            deallocate(tmp)
        end if
        
        ! Close IO stream
        close( io_unit )
        
        return
    end subroutine loadtxt_dynamic_r16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                   L O A D T X T _ D Y N A M I C _ R 1 6                  !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ C 8                     !
    !==========================================================================!
    !
    !!  Reads in an 8 byte complex 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !! because the array was too small.
    !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       character                                          !
    !                   c8array, the array which is to be filled with values   !
    !                       read in from the file. complex(8)                  !
    !                   nrows, returns the number of rows of data which were   !
    !                       read into the array. integer                       !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. integer              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
!~     subroutine loadtxt_static_c8 ( filename, c8array, nrows, errval )
!~         implicit none
!~         
!~         ! Input and output variables:
!~         character(len=*), intent(in)            ::  filename
!~         integer, intent(out)                    ::  nrows
!~         integer, optional, intent(out)          ::  errval
!~         complex(8), dimension(:,:), intent(out) ::  c8array
!~         
!~         ! Other variables:
!~         character(len=line_len) ::  line,                                      &
!~                                     tmp_char
!~         integer                 ::  i,                                         &
!~                                     ioerr
!~     !--------------------------------------------------------------------------!
!~         
!~         ! Open IO stream
!~         open(unit=io_unit, file=trim(filename), action='read', status='old',   &
!~              iostat=ioerr)
!~         if ( ioerr /= 0 ) then
!~             if ( present(errval) ) then
!~                 errval = ioerr
!~                 return
!~             else
!~                 write(unit=stderr, fmt=open_err) ioerr
!~                 stop
!~             end if
!~         end if
!~                 
!~         i = 0
!~         do while ( i < size(c8array,1) )
!~             read(unit=io_unit, fmt='(A)', iostat=ioerr) line
!~             print*,line
!~             if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
!~                 i = i + 1
!~                 exit
!~             end if
!~             tmp_char = adjustl(line(1:line_len-1)//com_char)
!~             if ( tmp_char(1:1) /= com_char ) then
!~                 i = i + 1
!~                 if ( index(line,com_char) == 0 ) then
!~                     read(unit=line, fmt=*, iostat=ioerr) c8array(i,:)
!~                 else
!~                     read(unit=line(1:index(line,com_char)-1), fmt=*,           &
!~                       iostat=ioerr) c8array(i,:)
!~                 end if
!~             end if
!~             if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
!~                 if ( present(errval) ) then
!~                     errval = ioerr
!~                     return
!~                 else
!~                     write(unit=stderr, fmt=read_err) ioerr
!~                     stop
!~                 end if
!~             end if
!~         end do
!~         nrows = i
!~ 
!~         ! Close IO stream
!~         close( io_unit )
!~ 
!~         return
!~     end subroutine loadtxt_static_c8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ C 8                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                   L O A D T X T _ S T A T I C _ C 1 6                    !
    !==========================================================================!
    !
    !!  Reads in a 16 byte complex 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !!  because the array was too small.
    !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       character                                          !
    !                   c16array, the array which is to be filled with values  !
    !                       read in from the file. complex(16)                 !
    !                   nrows, returns the number of rows of data which were   !
    !                       read into the array. integer                       !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. integer              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
!~     subroutine loadtxt_static_c16 ( filename, c16array, nrows, errval )
!~         implicit none
!~         
!~         ! Input and output variables:
!~         character(len=*), intent(in)                ::  filename
!~         integer, intent(out)                        ::  nrows
!~         integer, optional, intent(out)              ::  errval
!~         complex(16), dimension(:,:), intent(out)    ::  c16array
!~         
!~         ! Other variables:
!~         character(len=line_len) ::  line,                                      &
!~                                     tmp_char
!~         integer                 ::  i,                                         &
!~                                     ioerr
!~     !--------------------------------------------------------------------------!
!~         
!~         ! Open IO stream
!~         open(unit=io_unit, file=trim(filename), action='read', status='old',   &
!~              iostat=ioerr)
!~         if ( ioerr /= 0 ) then
!~             if ( present(errval) ) then
!~                 errval = ioerr
!~                 return
!~             else
!~                 write(unit=stderr, fmt=open_err) ioerr
!~                 stop
!~             end if
!~         end if
!~                 
!~         i = 0
!~         do while ( i < size(c16array,1) )
!~             read(unit=io_unit, fmt='(A)', iostat=ioerr) line
!~             if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
!~                 i = i + 1
!~                 exit
!~             end if
!~             tmp_char = adjustl(line(1:line_len-1)//com_char)
!~             if ( tmp_char(1:1) /= com_char ) then
!~                 i = i + 1
!~                 if ( index(line,com_char) == 0 ) then
!~                     read(unit=line, fmt=*, iostat=ioerr) c16array(i,:)
!~                 else
!~                     read(unit=line(1:index(line,com_char)-1), fmt=*,           &
!~                       iostat=ioerr) c16array(i,:)
!~                 end if
!~             end if
!~             if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
!~                 if ( present(errval) ) then
!~                     errval = ioerr
!~                     return
!~                 else
!~                     write(unit=stderr, fmt=read_err) ioerr
!~                     stop
!~                 end if
!~             end if
!~         end do
!~         nrows = i-1
!~ 
!~         ! Close IO stream
!~         close( io_unit )
!~ 
!~         return
!~     end subroutine loadtxt_static_c16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                   L O A D T X T _ S T A T I C _ C 1 6                    !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ I 1                     !
    !==========================================================================!
    !
    !!  Reads in a 1 byte integer 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !!  because the array was too small.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_static_i1 ( filename, i1array, nrows, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array.
        character(len=*), intent(in)                ::  filename
        !! Returns the number of rows of data which were read into the array.
        integer, intent(out)                        ::  nrows
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)              ::  errval
        !! The array which is to be filled with values read in from the file.
        integer(int8), dimension(:,:), intent(out)  ::  i1array
        
        ! Other variables:
        character(len=line_len) ::  line,                                      &
                                    tmp_char
        integer                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
                
        i = 0
        do while ( i < size(i1array,1) )
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
                i = i + 1
                exit
            end if
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                i = i + 1
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) i1array(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) i1array(i,:)
                end if
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
        nrows = i-1

        ! Close IO stream
        close( io_unit )

        return
    end subroutine loadtxt_static_i1
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ I 1                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ I 2                     !
    !==========================================================================!
    !
    !!  Reads in a 2 byte integer 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !!  because the array was too small.
    !
    !   ARGUMENTS:     *filename,                                        !
    !                   i2array,  integer(2)                  !
    !                   nrows,  integer                       !
    !                   [errval],  integer              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_static_i2 ( filename, i2array, nrows, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array. 
        character(len=*), intent(in)                ::  filename
        !! Returns the number of rows of data which were read into the array.
        integer, intent(out)                        ::  nrows
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)              ::  errval
        !! The array which is to be filled with values read in from the file.
        integer(int16), dimension(:,:), intent(out) ::  i2array
        
        ! Other variables:
        character(len=line_len) ::  line,                                      &
                                    tmp_char
        integer                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
                
        i = 0
        do while ( i < size(i2array,1) )
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
                i = i + 1
                exit
            end if
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                i = i + 1
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) i2array(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) i2array(i,:)
                end if
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
        nrows = i-1

        ! Close IO stream
        close( io_unit )

        return
    end subroutine loadtxt_static_i2
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ I 2                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ I 4                     !
    !==========================================================================!
    !
    !!  Reads in a 4 byte integer 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !!  because the array was too small.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_static_i4 ( filename, i4array, nrows, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array. 
        character(len=*), intent(in)                ::  filename
        !! Returns the number of rows of data which were read into the array.
        integer, intent(out)                        ::  nrows
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)              ::  errval
        !! The array which is to be filled with values read in from the file.
        integer(int32), dimension(:,:), intent(out) ::  i4array
        
        ! Other variables:
        character(len=line_len) ::  line,                                      &
                                    tmp_char
        integer                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
                
        i = 0
        do while ( i < size(i4array,1) )
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
                i = i + 1
                exit
            end if
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                i = i + 1
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) i4array(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) i4array(i,:)
                end if
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
        nrows = i-1

        ! Close IO stream
        close( io_unit )

        return
    end subroutine loadtxt_static_i4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ I 4                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ I 8                     !
    !==========================================================================!
    !
    !!  Reads in an 8 byte integer 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !!  because the array was too small.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_static_i8 ( filename, i8array, nrows, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array. 
        character(len=*), intent(in)                ::  filename
        !! Returns the number of rows of data which were read into the array.
        integer, intent(out)                        ::  nrows
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)              ::  errval
        !! The array which is to be filled with values read in from the file.
        integer(int64), dimension(:,:), intent(out) ::  i8array
        
        ! Other variables:
        character(len=line_len) ::  line,                                      &
                                    tmp_char
        integer                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!

        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
                
        i = 0
        do while ( i < size(i8array,1) )
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
                i = i + 1
                exit
            end if
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                i = i + 1
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) i8array(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) i8array(i,:)
                end if
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
        nrows = i-1

        ! Close IO stream
        close( io_unit )

        return
    end subroutine loadtxt_static_i8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ I 8                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ R 8                     !
    !==========================================================================!
    !
    !!  Reads in a single precision 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !!  because the array was too small.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_static_r4 ( filename, r4array, nrows, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array. 
        character(len=*), intent(in)                ::  filename
        !! Returns the number of rows of data which were read into the array.
        integer, intent(out)                        ::  nrows
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)              ::  errval
        !! The array which is to be filled with values read in from the file.
        real(real32), dimension(:,:), intent(out)   ::  r4array
        
        ! Other variables:
        character(len=line_len) ::  line,                                      &
                                    tmp_char
        integer                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
                
        i = 0
        do while ( i < size(r4array,1) )
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
                i = i + 1
                exit
            end if
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                i = i + 1
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) r4array(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) r4array(i,:)
                end if
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
        nrows = i

        ! Close IO stream
        close( io_unit )

        return
    end subroutine loadtxt_static_r4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ R 4                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ R 8                     !
    !==========================================================================!
    !
    !!  Reads in a double precision 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !!  because the array was too small.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_static_r8 ( filename, r8array, nrows, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array. 
        character(len=*), intent(in)                ::  filename
        !! Returns the number of rows of data which were read into the array.
        integer, intent(out)                        ::  nrows
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)              ::  errval
        !! The array which is to be filled with values read in from the file.
        real(real64), dimension(:,:), intent(out)   ::  r8array
        
        ! Other variables:
        character(len=line_len) ::  line,                                      &
                                    tmp_char
        integer                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
                
        i = 0
        do while ( i < size(r8array,1) )
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
                i = i + 1
                exit
            end if
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                i = i + 1
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) r8array(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) r8array(i,:)
                end if
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
        nrows = i-1

        ! Close IO stream
        close( io_unit )

        return
    end subroutine loadtxt_static_r8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ R 8                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                   L O A D T X T _ S T A T I C _ R 1 6                    !
    !==========================================================================!
    !
    !!  Reads in a quad precision 2D array whose dimensions are known in 
    !!  advance. Note that there is no way to know if some data was missed 
    !!  because the array was too small.
    !
    !--------------------------------------------------------------------------!
    subroutine loadtxt_static_r16 ( filename, r16array, nrows, errval )
        implicit none
        
        ! Input and output variables:
        !! The file from which to read in the array. 
        character(len=*), intent(in)                ::  filename
        !! Returns the number of rows of data which were read into the array.
        integer, intent(out)                        ::  nrows
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)              ::  errval
        !! The array which is to be filled with values read in from the file.
        real(real128), dimension(:,:), intent(out)  ::  r16array
        
        ! Other variables:
        character(len=line_len) ::  line,                                      &
                                    tmp_char
        integer                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        open(unit=io_unit, file=trim(filename), action='read', status='old',   &
             iostat=ioerr)
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
                
        i = 0
        do while ( i < size(r16array,1) )
            read(unit=io_unit, fmt='(A)', iostat=ioerr) line
            if ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) then
                i = i + 1
                exit
            end if
            tmp_char = adjustl(line(1:line_len-1)//com_char)
            if ( tmp_char(1:1) /= com_char ) then
                i = i + 1
                if ( index(line,com_char) == 0 ) then
                    read(unit=line, fmt=*, iostat=ioerr) r16array(i,:)
                else
                    read(unit=line(1:index(line,com_char)-1), fmt=*,           &
                      iostat=ioerr) r16array(i,:)
                end if
            end if
            if ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=read_err) ioerr
                    stop
                end if
            end if
        end do
        nrows = i-1

        ! Close IO stream
        close( io_unit )

        return
    end subroutine loadtxt_static_r16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                   L O A D T X T _ S T A T I C _ R 1 6                    !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                          S E T _ C O M M E N T                           !
    !==========================================================================!
    !
    !!  Sets the character which designates a comment in input files.
    !
    !--------------------------------------------------------------------------!
    subroutine set_comment ( com )
        implicit none
        
        !! The character which designates the beginning of a comment.
        character(len=1), intent(in)    ::  com
    !--------------------------------------------------------------------------!
        
        com_char = com
        
    end subroutine set_comment
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                          S E T _ C O M M E N T                           !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ C 8                            !
    !==========================================================================!
    !
    !!  Writes out an 8 byte complex 2D array.
    !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. character                                    !
    !                   c8array, the array which is to be written to the file. !
    !                       complex(8)                                         !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). character                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. character                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. integer              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
!~     subroutine savetxt_c8 ( filename, c8array, clobber, user_format, errval )
!~         implicit none
!~         
!~         ! Input and output variables:
!~         character(len=*), intent(in)                    ::  filename
!~         character(len=*), optional, intent(in)          ::  clobber,           &
!~                                                             user_format
!~         complex(8), dimension(:,:), target, intent(in)  ::  c8array
!~         integer, optional, intent(out)                  ::  errval
!~         
!~         ! Other variables:
!~         character(len=8)                ::  file_pos,                          &
!~                                             file_stat
!~         character(len=128)              ::  u_format
!~         character(len=16), parameter    ::  def_form = '(*(g26.18))'
!~         integer                         ::  i,                                 &
!~                                             ioerr,                             &
!~                                             out_unit
!~     !--------------------------------------------------------------------------!
!~         
!~         ! Determine options for opening file
!~         if ( present(clobber) ) then
!~             select case (clobber)
!~                 case ('clobber') 
!~                     file_pos = 'asis'
!~                     file_stat = 'replace'
!~                 case ('noclobber')
!~                     file_pos = 'asis'
!~                     file_stat = 'new'
!~                 case ('append')
!~                     file_pos = 'append'
!~                     file_stat = 'unknown'
!~                 case default
!~                     if ( present(errval) ) then
!~                         errval = -5000
!~                         return
!~                     else
!~                         write(unit=stderr, fmt=clob_err)
!~                         stop
!~                     end if
!~             end select
!~         else
!~             file_pos = 'asis'
!~             file_stat = 'replace'
!~         end if
!~         
!~         ! Determine whether to use a user-supplied format string
!~         if ( present(user_format) ) then
!~             u_format = user_format
!~         else
!~             u_format = def_form
!~         end if
!~         
!~         ! Open IO stream
!~         if ( trim(filename) == 'stdout' ) then
!~             out_unit = stdout
!~             ioerr = 0
!~         else
!~             out_unit = io_unit
!~             open(unit=out_unit, file=trim(filename), action='write',           &
!~                  iostat=ioerr, status=file_stat, position=file_pos)
!~         end if
!~         if ( ioerr /= 0 ) then
!~             if ( present(errval) ) then
!~                 errval = ioerr
!~                 return
!~             else
!~                 write(unit=stderr, fmt=open_err) ioerr
!~                 stop
!~             end if
!~         end if
!~         
!~         ! Write out array
!~         do i = 1, size(c8array,1)
!~             write(unit=out_unit, fmt=u_format, iostat=ioerr) c8array(i,:)
!~             if ( ioerr /= 0 ) then
!~                 if ( present(errval) ) then
!~                     errval = ioerr
!~                     return
!~                 else
!~                     write(unit=stderr, fmt=write_err) ioerr
!~                     stop
!~                 end if
!~             end if
!~         end do
!~         
!~         ! Clean up
!~         if ( trim(filename) /= 'stdout' ) close( io_unit )
!~ 
!~         return
!~     end subroutine savetxt_c8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ C 8                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                          S A V E T X T _ C 1 6                           !
    !==========================================================================!
    !
    !!  Writes out a 16 byte complex 2D array.
    !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. character                                    !
    !                   c16array, the array which is to be written to the      !
    !                       file. complex(16)                                  !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). character                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. character                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. integer              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
!~     subroutine savetxt_c16 ( filename, c16array, clobber, user_format, errval )
!~         implicit none
!~         
!~         ! Input and output variables:
!~         character(len=*), intent(in)                    ::  filename
!~         character(len=*), optional, intent(in)          ::  clobber,           &
!~                                                             user_format
!~         complex(16), dimension(:,:), target, intent(in) ::  c16array
!~         integer, optional, intent(out)                  ::  errval
!~         
!~         ! Other variables:
!~         character(len=8)                ::  file_pos,                          &
!~                                             file_stat
!~         character(len=128)              ::  u_format
!~         character(len=16), parameter    ::  def_form = '(*(g26.18))'
!~         integer                         ::  i,                                 &
!~                                             ioerr,                             &
!~                                             out_unit
!~     !--------------------------------------------------------------------------!
!~         
!~         ! Determine options for opening file
!~         if ( present(clobber) ) then
!~             select case (clobber)
!~                 case ('clobber') 
!~                     file_pos = 'asis'
!~                     file_stat = 'replace'
!~                 case ('noclobber')
!~                     file_pos = 'asis'
!~                     file_stat = 'new'
!~                 case ('append')
!~                     file_pos = 'append'
!~                     file_stat = 'unknown'
!~                 case default
!~                     if ( present(errval) ) then
!~                         errval = -5000
!~                         return
!~                     else
!~                         write(unit=stderr, fmt=clob_err)
!~                         stop
!~                     end if
!~             end select
!~         else
!~             file_pos = 'asis'
!~             file_stat = 'replace'
!~         end if
!~         
!~         ! Determine whether to use a user-supplied format string
!~         if ( present(user_format) ) then
!~             u_format = user_format
!~         else
!~             u_format = def_form
!~         end if
!~         
!~         ! Open IO stream
!~         if ( trim(filename) == 'stdout' ) then
!~             out_unit = stdout
!~             ioerr = 0
!~         else
!~             out_unit = io_unit
!~             open(unit=out_unit, file=trim(filename), action='write',           &
!~                  iostat=ioerr, status=file_stat, position=file_pos)
!~         end if
!~         if ( ioerr /= 0 ) then
!~             if ( present(errval) ) then
!~                 errval = ioerr
!~                 return
!~             else
!~                 write(unit=stderr, fmt=open_err) ioerr
!~                 stop
!~             end if
!~         end if
!~         
!~         ! Write out array
!~         do i = 1, size(c16array,1)
!~             write(unit=out_unit, fmt=u_format, iostat=ioerr) c16array(i,:)
!~             if ( ioerr /= 0 ) then
!~                 if ( present(errval) ) then
!~                     errval = ioerr
!~                     return
!~                 else
!~                     write(unit=stderr, fmt=write_err) ioerr
!~                     stop
!~                 end if
!~             end if
!~         end do
!~         
!~         ! Clean up
!~         if ( trim(filename) /= 'stdout' ) close( io_unit )
!~ 
!~         return
!~     end subroutine savetxt_c16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                          S A V E T X T _ C 1 6                           !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ I 1                            !
    !==========================================================================!
    !
    !!  Writes out a 1 byte integer 2D array.
    !
    !--------------------------------------------------------------------------!
    subroutine savetxt_i1 ( filename, i1array, clobber, user_format, errval )
        implicit none
        
        ! Input and output variables:
        !! The file to which to write in the array. If it is 'stdout' then 
        !! writes to terminal instead of file.
        character(len=*), intent(in)                        ::  filename
        !! Determines whether to overwrite existing files. Options are 
        !! 'clobber' (overwrite file if exists, default), 'noclobber' (produce 
        !! error if file exists), and 'append' (append output to file if 
        !! exists).
        character(len=*), optional, intent(in)              ::  clobber
        !! A format string which can be used to provide a custom specification 
        !! for the array output.
        character(len=*), optional, intent(in)              ::  user_format
        !! The array which is to be written to the file.
        integer(int8), dimension(:,:), target, intent(in)   ::  i1array
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)                      ::  errval
        
        ! Other variables:
        character(len=8)                ::  file_pos,                          &
                                            file_stat
        character(len=128)              ::  u_format
        character(len=16), parameter    ::  def_form = '(*(i7))'
        integer                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        if ( present(clobber) ) then
            select case (clobber)
                case ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                case ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                case ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            file_pos = 'asis'
            file_stat = 'replace'
        end if
        
        ! Determine whether to use a user-supplied format string
        if ( present(user_format) ) then
            u_format = user_format
        else
            u_format = def_form
        end if
        
        ! Open IO stream
        if ( trim(filename) == 'stdout' ) then
            out_unit = stdout
            ioerr = 0
        else
            out_unit = io_unit
            open(unit=out_unit, file=trim(filename), action='write',           &
                 iostat=ioerr, status=file_stat, position=file_pos)
        end if
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Write out array
        do i = 1, size(i1array,1)
            write(unit=out_unit, fmt=u_format, iostat=ioerr) i1array(i,:)
            if ( ioerr /= 0 ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=write_err) ioerr
                    stop
                end if
            end if
        end do
        
        ! Clean up
        if ( trim(filename) /= 'stdout' ) close( io_unit )

        return
    end subroutine savetxt_i1
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ I 1                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ I 2                            !
    !==========================================================================!
    !
    !!  Writes out a 2 byte integer 2D array.
    !
    !--------------------------------------------------------------------------!
    subroutine savetxt_i2 ( filename, i2array, clobber, user_format, errval )
        implicit none
        
        ! Input and output variables:
        !! The file to which to write in the array. If it is 'stdout' then 
        !! writes to terminal instead of file.
        character(len=*), intent(in)                        ::  filename
        !! Determines whether to overwrite existing files. Options are 
        !! 'clobber' (overwrite file if exists, default), 'noclobber' (produce 
        !! error if file exists), and 'append' (append output to file if 
        !! exists).
        character(len=*), optional, intent(in)              ::  clobber
        !! A format string which can be used to provide a custom specification 
        !! for the array output.
        character(len=*), optional, intent(in)              ::  user_format
        !! The array which is to be written to the file.
        integer(int16), dimension(:,:), target, intent(in)  ::  i2array
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)                      ::  errval
        
        ! Other variables:
        character(len=8)                ::  file_pos,                          &
                                            file_stat
        character(len=128)              ::  u_format
        character(len=16), parameter    ::  def_form = '(*(i7))'
        integer                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        if ( present(clobber) ) then
            select case (clobber)
                case ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                case ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                case ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            file_pos = 'asis'
            file_stat = 'replace'
        end if
        
        ! Determine whether to use a user-supplied format string
        if ( present(user_format) ) then
            u_format = user_format
        else
            u_format = def_form
        end if
        
        ! Open IO stream
        if ( trim(filename) == 'stdout' ) then
            out_unit = stdout
            ioerr = 0
        else
            out_unit = io_unit
            open(unit=out_unit, file=trim(filename), action='write',           &
                 iostat=ioerr, status=file_stat, position=file_pos)
        end if
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Write out array
        do i = 1, size(i2array,1)
            write(unit=out_unit, fmt=u_format, iostat=ioerr) i2array(i,:)
            if ( ioerr /= 0 ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=write_err) ioerr
                    stop
                end if
            end if
        end do
        
        ! Clean up
        if ( trim(filename) /= 'stdout' ) close( io_unit )

        return
    end subroutine savetxt_i2
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ I 2                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ I 4                            !
    !==========================================================================!
    !
    !!  Writes out a 4 byte integer 2D array.
    !
    !--------------------------------------------------------------------------!
    subroutine savetxt_i4 ( filename, i4array, clobber, user_format, errval )
        implicit none
        
        ! Input and output variables:
        !! The file to which to write in the array. If it is 'stdout' then 
        !! writes to terminal instead of file.
        character(len=*), intent(in)                        ::  filename
        !! Determines whether to overwrite existing files. Options are 
        !! 'clobber' (overwrite file if exists, default), 'noclobber' (produce 
        !! error if file exists), and 'append' (append output to file if 
        !! exists).
        character(len=*), optional, intent(in)              ::  clobber
        !! A format string which can be used to provide a custom specification 
        !! for the array output.
        character(len=*), optional, intent(in)              ::  user_format
        !! The array which is to be written to the file.
        integer(int32), dimension(:,:), target, intent(in)  ::  i4array
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)                      ::  errval
        
        ! Other variables:
        character(len=8)                ::  file_pos,                          &
                                            file_stat
        character(len=128)              ::  u_format
        character(len=16), parameter    ::  def_form = '(*(i12))'
        integer                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        if ( present(clobber) ) then
            select case (clobber)
                case ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                case ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                case ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            file_pos = 'asis'
            file_stat = 'replace'
        end if
        
        ! Determine whether to use a user-supplied format string
        if ( present(user_format) ) then
            u_format = user_format
        else
            u_format = def_form
        end if
        
        ! Open IO stream
        if ( trim(filename) == 'stdout' ) then
            out_unit = stdout
            ioerr = 0
        else
            out_unit = io_unit
            open(unit=out_unit, file=trim(filename), action='write',           &
                 iostat=ioerr, status=file_stat, position=file_pos)
        end if
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Write out array
        do i = 1, size(i4array,1)
            write(unit=out_unit, fmt=u_format, iostat=ioerr) i4array(i,:)
            if ( ioerr /= 0 ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=write_err) ioerr
                    stop
                end if
            end if
        end do
        
        ! Clean up
        if ( trim(filename) /= 'stdout' ) close( io_unit )

        return
    end subroutine savetxt_i4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ I 4                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ I 8                            !
    !==========================================================================!
    !
    !!  Writes out an 8 byte integer 2D array.
    !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. character                                    !
    !                   i8array, the array which is to be written to the file. !
    !                       integer(8)                                         !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). character                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. character                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. integer              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    subroutine savetxt_i8 ( filename, i8array, clobber, user_format, errval )
        implicit none
        
        ! Input and output variables:
        !! The file to which to write in the array. If it is 'stdout' then 
        !! writes to terminal instead of file.
        character(len=*), intent(in)                        ::  filename
        !! Determines whether to overwrite existing files. Options are 
        !! 'clobber' (overwrite file if exists, default), 'noclobber' (produce 
        !! error if file exists), and 'append' (append output to file if 
        !! exists).
        character(len=*), optional, intent(in)              ::  clobber
        !! A format string which can be used to provide a custom specification 
        !! for the array output.
        character(len=*), optional, intent(in)              ::  user_format
        !! The array which is to be written to the file.
        integer(int64), dimension(:,:), target, intent(in)  ::  i8array
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)                      ::  errval
        
        ! Other variables:
        character(len=8)                ::  file_pos,                          &
                                            file_stat
        character(len=128)              ::  u_format
        character(len=16), parameter    ::  def_form = '(*(i12))'
        integer                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        if ( present(clobber) ) then
            select case (clobber)
                case ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                case ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                case ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            file_pos = 'asis'
            file_stat = 'replace'
        end if
        
        ! Determine whether to use a user-supplied format string
        if ( present(user_format) ) then
            u_format = user_format
        else
            u_format = def_form
        end if
        
        ! Open IO stream
        if ( trim(filename) == 'stdout' ) then
            out_unit = stdout
            ioerr = 0
        else
            out_unit = io_unit
            open(unit=out_unit, file=trim(filename), action='write',           &
                 iostat=ioerr, status=file_stat, position=file_pos)
        end if
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Write out array
        do i = 1, size(i8array,1)
            write(unit=out_unit, fmt=u_format, iostat=ioerr) i8array(i,:)
            if ( ioerr /= 0 ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=write_err) ioerr
                    stop
                end if
            end if
        end do
        
        ! Clean up
        if ( trim(filename) /= 'stdout' ) close( io_unit )

        return
    end subroutine savetxt_i8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ I 8                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ R 4                            !
    !==========================================================================!
    !
    !!  Writes out a single precision 2D array.
    !
    !--------------------------------------------------------------------------!
    subroutine savetxt_r4 ( filename, r4array, clobber, user_format, errval )
        implicit none
        
        ! Input and output variables:
        !! The file to which to write in the array. If it is 'stdout' then 
        !! writes to terminal instead of file.
        character(len=*), intent(in)                        ::  filename
        !! Determines whether to overwrite existing files. Options are 
        !! 'clobber' (overwrite file if exists, default), 'noclobber' (produce 
        !! error if file exists), and 'append' (append output to file if 
        !! exists).
        character(len=*), optional, intent(in)              ::  clobber
        !! A format string which can be used to provide a custom specification 
        !! for the array output.
        character(len=*), optional, intent(in)              ::  user_format
        !! The array which is to be written to the file.
        real(real32), dimension(:,:), target, intent(in)    ::  r4array
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)                      ::  errval
        
        ! Other variables:
        character(len=8)                ::  file_pos,                          &
                                            file_stat
        character(len=128)              ::  u_format
        character(len=16), parameter    ::  def_form = '(*(g17.9))'
        integer                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        if ( present(clobber) ) then
            select case (clobber)
                case ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                case ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                case ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            file_pos = 'asis'
            file_stat = 'replace'
        end if
        
        ! Determine whether to use a user-supplied format string
        if ( present(user_format) ) then
            u_format = user_format
        else
            u_format = def_form
        end if
        
        ! Open IO stream
        if ( trim(filename) == 'stdout' ) then
            out_unit = stdout
            ioerr = 0
        else
            out_unit = io_unit
            open(unit=out_unit, file=trim(filename), action='write',           &
                 iostat=ioerr, status=file_stat, position=file_pos)
        end if
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Write out array
        do i = 1, size(r4array,1)
            write(unit=out_unit, fmt=u_format, iostat=ioerr) r4array(i,:)
            if ( ioerr /= 0 ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=write_err) ioerr
                    stop
                end if
            end if
        end do
        
        ! Clean up
        if ( trim(filename) /= 'stdout' ) close( io_unit )

        return
    end subroutine savetxt_r4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ R 4                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ R 8                            !
    !==========================================================================!
    !
    !!  Writes out a double precision 2D array.
    !
    !--------------------------------------------------------------------------!
    subroutine savetxt_r8 ( filename, r8array, clobber, user_format, errval )
        implicit none
        
        ! Input and output variables:
        !! The file to which to write in the array. If it is 'stdout' then 
        !! writes to terminal instead of file.
        character(len=*), intent(in)                        ::  filename
        !! Determines whether to overwrite existing files. Options are 
        !! 'clobber' (overwrite file if exists, default), 'noclobber' (produce 
        !! error if file exists), and 'append' (append output to file if 
        !! exists).
        character(len=*), optional, intent(in)              ::  clobber
        !! A format string which can be used to provide a custom specification 
        !! for the array output.
        character(len=*), optional, intent(in)              ::  user_format
        !! The array which is to be written to the file.
        real(real64), dimension(:,:), target, intent(in)    ::  r8array
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)                      ::  errval
        
        ! Other variables:
        character(len=8)                ::  file_pos,                          &
                                            file_stat
        character(len=128)              ::  u_format
        character(len=16), parameter    ::  def_form = '(*(g26.18))'
        integer                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        if ( present(clobber) ) then
            select case (clobber)
                case ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                case ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                case ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            file_pos = 'asis'
            file_stat = 'replace'
        end if
        
        ! Determine whether to use a user-supplied format string
        if ( present(user_format) ) then
            u_format = user_format
        else
            u_format = def_form
        end if
        
        ! Open IO stream
        if ( trim(filename) == 'stdout' ) then
            out_unit = stdout
            ioerr = 0
        else
            out_unit = io_unit
            open(unit=out_unit, file=trim(filename), action='write',           &
                 iostat=ioerr, status=file_stat, position=file_pos)
        end if
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Write out array
        do i = 1, size(r8array,1)
            write(unit=out_unit, fmt=u_format, iostat=ioerr) r8array(i,:)
            if ( ioerr /= 0 ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=write_err) ioerr
                    stop
                end if
            end if
        end do
        
        ! Clean up
        if ( trim(filename) /= 'stdout' ) close( io_unit )

        return
    end subroutine savetxt_r8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ R 8                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                          S A V E T X T _ R 1 6                           !
    !==========================================================================!
    !
    !!  Writes out a quad precision 2D array.
    !
    !--------------------------------------------------------------------------!
    subroutine savetxt_r16 ( filename, r16array, clobber, user_format, errval )
        implicit none
        
        ! Input and output variables:
        !! The file to which to write in the array. If it is 'stdout' then 
        !! writes to terminal instead of file.
        character(len=*), intent(in)                        ::  filename
        !! Determines whether to overwrite existing files. Options are 
        !! 'clobber' (overwrite file if exists, default), 'noclobber' (produce 
        !! error if file exists), and 'append' (append output to file if 
        !! exists).
        character(len=*), optional, intent(in)              ::  clobber
        !! A format string which can be used to provide a custom specification 
        !! for the array output.
        character(len=*), optional, intent(in)              ::  user_format
        !! The array which is to be written to the file.
        real(real128), dimension(:,:), target, intent(in)   ::  r16array
        !! Returns any error codes generated during the execution of this 
        !! subroutine.
        integer, optional, intent(out)                      ::  errval
        
        ! Other variables:
        character(len=8)                ::  file_pos,                          &
                                            file_stat
        character(len=128)              ::  u_format
        character(len=16), parameter    ::  def_form = '(*(g26.18))'
        integer                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        if ( present(clobber) ) then
            select case (clobber)
                case ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                case ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                case ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                case default
                    if ( present(errval) ) then
                        errval = -5000
                        return
                    else
                        write(unit=stderr, fmt=clob_err)
                        stop
                    end if
            end select
        else
            file_pos = 'asis'
            file_stat = 'replace'
        end if
        
        ! Determine whether to use a user-supplied format string
        if ( present(user_format) ) then
            u_format = user_format
        else
            u_format = def_form
        end if
        
        ! Open IO stream
        if ( trim(filename) == 'stdout' ) then
            out_unit = stdout
            ioerr = 0
        else
            out_unit = io_unit
            open(unit=out_unit, file=trim(filename), action='write',           &
                 iostat=ioerr, status=file_stat, position=file_pos)
        end if
        if ( ioerr /= 0 ) then
            if ( present(errval) ) then
                errval = ioerr
                return
            else
                write(unit=stderr, fmt=open_err) ioerr
                stop
            end if
        end if
        
        ! Write out array
        do i = 1, size(r16array,1)
            write(unit=out_unit, fmt=u_format, iostat=ioerr) r16array(i,:)
            if ( ioerr /= 0 ) then
                if ( present(errval) ) then
                    errval = ioerr
                    return
                else
                    write(unit=stderr, fmt=write_err) ioerr
                    stop
                end if
            end if
        end do
        
        ! Clean up
        if ( trim(filename) /= 'stdout' ) close( io_unit )

        return
    end subroutine savetxt_r16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                          S A V E T X T _ R 1 6                           !
    !==========================================================================!

end module array_io
!==============================================================================!
!                            E N D    M O D U L E :                            !
!                               A R R A Y _ I O                                !
!==============================================================================!
