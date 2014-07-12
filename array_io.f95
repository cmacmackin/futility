!==============================================================================!
!                          B E G I N    M O D U L E :                          !
!                               A R R A Y _ I O                                !
!==============================================================================!
!                                                                              !
!   PURPOSE:    Provides subroutines to easily perform array input and output. !
!               They are modelled after the "loadtxt" and "savetxt"            !
!               subroutines in Numpy. Note: if using this module, do not use   !
!               logical unit 90 for any IO, as it is reserved for use by these !
!               subroutines. Lines in the filebeginning with the pound symbol  !
!               (#) are considered comments and will not be read. Works for    !
!               both integer and real data. Complex data IO has not yet been   !
!               properly implemented.                                          !
!                                                                              !
!   CONTAINS:                                                                  !
!   EXTERNALS:  None                                                           !
!                                                                              !
!------------------------------------------------------------------------------!
MODULE array_io
    IMPLICIT NONE
    PRIVATE

    ! Variable declarations
    CHARACTER(LEN=1), PARAMETER     ::  com_char = '#'
    CHARACTER(LEN=64), PARAMETER    ::  allocate_err = '("ARRAY_IO: Array &
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
    CHARACTER(LEN=128), PARAMETER   ::  clob_err = '("ARRAY_IO: Unrecognized &
                                        &CLOBBER specification. Please select & 
                                        &''clobber'',",/,&
                                        &"ARRAY_IO: ''noclobber'', or &
                                        &''append''.")'
    INTEGER, PARAMETER              ::  io_unit = 90,                          &
                                        line_len = 1024,                       &
                                        stderr = 0,                            &
                                        stdin = 5,                             &
                                        stdout = 6
    
    ! Generic interfaces
    PUBLIC  :: loadtxt, savetxt
    
    INTERFACE loadtxt
        !MODULE PROCEDURE loadtxt_dynamic_c8
        !MODULE PROCEDURE loadtxt_dynamic_c16
        MODULE PROCEDURE loadtxt_dynamic_i2
        MODULE PROCEDURE loadtxt_dynamic_i4
        MODULE PROCEDURE loadtxt_dynamic_r4
        MODULE PROCEDURE loadtxt_dynamic_r8
        !MODULE PROCEDURE loadtxt_static_c8
        !MODULE PROCEDURE loadtxt_static_c16
        MODULE PROCEDURE loadtxt_static_i2
        MODULE PROCEDURE loadtxt_static_i4
        MODULE PROCEDURE loadtxt_static_r4
        MODULE PROCEDURE loadtxt_static_r8
    END INTERFACE loadtxt
    
    INTERFACE savetxt
        !MODULE PROCEDURE savetxt_c8
        !MODULE PROCEDURE savetxt_c16
        MODULE PROCEDURE savetxt_i2
        MODULE PROCEDURE savetxt_i4
        MODULE PROCEDURE savetxt_r4
        MODULE PROCEDURE savetxt_r8
    END INTERFACE savetxt
    
!------------------------------------------------------------------------------!
CONTAINS

! TODO: consider allowing some lines of the file to be commented out.
    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                             F I L E _ S I Z E                            !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Finds the amount of data in whatever file is currently ! 
    !                   associated with IO unit 90. Doesn't count lines with   !
    !                   no data or entirely commented out.                     !
    !                                                                          !
    !   ARGUMENTS:      nrows, returns the number of rows of data present in   !
    !                       the file. An empty row is interpreted as the end   !
    !                       of the file. INTEGER                               !
    !                   ncols, the number of columns of data present. This is  !
    !                       determined from the number of pieces of data in    !
    !                       first row. Each datum must be separated by white-  !
    !                       space.                                             !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE file_size ( nrows, ncols )
        IMPLICIT NONE
        
        ! Input and output variables
        INTEGER, INTENT(OUT)    ::  nrows,                                     &
                                    ncols
        
        ! Other variables
        CHARACTER(LEN=line_len) ::  line,                                      &
                                    tmp
        INTEGER                 ::  i,                                         &
                                    ioerr
        LOGICAL                 ::  white_last
    !--------------------------------------------------------------------------!
        
        ! Initialize variables
        nrows = 0
        ncols = 0
        tmp = '#'
        
        ! Read in first non-blank, non-comment row
        DO WHILE ( tmp(1:1) == com_char )
            READ(UNIT=io_unit, FMT='(a)', IOSTAT=ioerr) line
            IF ( ioerr /= 0 ) RETURN
            tmp = ADJUSTL(line(1:line_len-1)//com_char)
        END DO
        
        ! Get number of columns
        white_last = .TRUE.
        DO i = 1, LEN(TRIM(line))
            ! Check for beginning of a comment
            IF ( line(i:i) == com_char ) EXIT
            
            ! Check for white-space
            IF ( ( line(i:i) == ' ' ) .OR. ( line(i:i) == CHAR(11) ) ) THEN
                IF ( .NOT. white_last ) THEN
                    white_last = .TRUE.
                END IF
            ELSE
                IF ( white_last ) THEN
                    white_last = .FALSE.
                    ncols = ncols + 1
                END IF
            END IF
        END DO
        
        ! Get number of rows
        nrows = 1
        DO WHILE ( ioerr == 0 )
            READ(UNIT=io_unit, FMT='(a)', IOSTAT=ioerr) line
            tmp = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp(1:1) /= com_char ) nrows = nrows + 1
        END DO

        RETURN
    END SUBROUTINE file_size
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                             F I L E _ S I Z E                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                   L O A D T X T _ D Y N A M I C _ C 1 6                  !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads a 4 byte complex 2D array of unknown size into   !
    !                   an allocatable array.                                  !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   c16array_dyn, the allocatable array which is to be     !
    !                       filled with data read from the file. COMPLEX(16)   !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       array if already allocated. Options are 'clobber'  !
    !                       (overwrite array if allocated), 'noclobber'        !
    !                       (produce error if array allocated), and 'append'   !
    !                       (append data to array if already allocated).       !
    !                       CHARACTER                                          !
    !                                                                          !
    !   EXTERNALS:      file_size (subroutine)                                 !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_dynamic_c16 ( filename, c16array_dyn, clobber, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                            ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)                  ::  clobber
        INTEGER, OPTIONAL, INTENT(OUT)                          ::  errval
        COMPLEX(16), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) ::  c16array_dyn
        
        ! Other variables
        CHARACTER(LEN=line_len)                     ::  line,                  &
                                                        tmp_char
        INTEGER                                     ::  i,                     &
                                                        ioerr,                 &
                                                        ncols,                 &
                                                        nrows,                 &
                                                        nrows_old
        COMPLEX(16), ALLOCATABLE, DIMENSION(:,:)    ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ( 'clobber' ) 
                    IF ( ALLOCATED(c16array_dyn) ) DEALLOCATE(c16array_dyn)
                CASE ( 'noclobber' )
                    IF ( ALLOCATED(c16array_dyn) ) THEN
                        IF ( PRESENT(errval) ) THEN
                            errval = -6000
                            RETURN
                        ELSE
                            WRITE(UNIT=stderr, FMT=allocate_err) 
                            STOP
                        END IF
                    END IF
                CASE ( 'append' )
                    IF ( ALLOCATED(c16array_dyn) ) THEN
                        tmp = c16array_dyn
                        DEALLOCATE(c16array_dyn)
                    END IF
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            IF ( ALLOCATED(c16array_dyn) ) DEALLOCATE(c16array_dyn)
        END IF

        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Find number of rows and columns of data to collect.
        CALL file_size(nrows,ncols)
        ncols = ncols / 2
        IF ( ncols == 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = -8000
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=empty_err) filename
                STOP
            END IF
        END IF
        
        ! Allocate array to correct size
        IF ( ALLOCATED(tmp) ) THEN
            ncols = SIZE(tmp,2)
            nrows_old = SIZE(tmp,1)
            nrows = nrows + nrows_old
        ELSE
            nrows_old = 0
        END IF
        ALLOCATE(c16array_dyn(nrows,ncols))
        
        ! Read in array
        REWIND(io_unit)
        i = nrows_old + 1
        DO
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) EXIT
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) c16array_dyn(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) c16array_dyn(i,:)
                END IF
                i = i + 1
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
            
        ! Append to array, if necessary
        IF ( ALLOCATED(tmp) ) THEN
            c16array_dyn(1:nrows_old,:) = tmp(:,:)
            DEALLOCATE(tmp)
        END IF
        
        ! Close IO stream
        CLOSE( io_unit )
        
        RETURN
    END SUBROUTINE loadtxt_dynamic_c16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                   L O A D T X T _ D Y N A M I C _ C 1 6                  !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ C 8                   !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads an 8 byte complex 2D array of unknown size into  !
    !                   an allocatable array.                                  !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   c8array_dyn, the allocatable array which is to be      !
    !                       filled with data read in from the file. COMPLEX(8) !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       array if already allocated. Options are 'clobber'  !
    !                       (overwrite array if allocated), 'noclobber'        !
    !                       (produce error if array allocated), and 'append'   !
    !                       (append data to array if already allocated).       !
    !                       CHARACTER                                          !
    !                                                                          !
    !   EXTERNALS:      file_size (subroutine)                                 !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_dynamic_c8 ( filename, c8array_dyn, clobber, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                            ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)                  ::  clobber
        INTEGER, OPTIONAL, INTENT(OUT)                          ::  errval
        COMPLEX(8), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT)  ::  c8array_dyn
        
        ! Other variables
        CHARACTER(LEN=line_len)                 ::  line,                      &
                                                    tmp_char
        INTEGER                                 ::  i,                         &
                                                    ioerr,                     &
                                                    ncols,                     &
                                                    nrows,                     &
                                                    nrows_old
        COMPLEX(8), ALLOCATABLE, DIMENSION(:,:) ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ( 'clobber' ) 
                    IF ( ALLOCATED(c8array_dyn) ) DEALLOCATE(c8array_dyn)
                CASE ( 'noclobber' )
                    IF ( ALLOCATED(c8array_dyn) ) THEN
                        IF ( PRESENT(errval) ) THEN
                            errval = -6000
                            RETURN
                        ELSE
                            WRITE(UNIT=stderr, FMT=allocate_err) 
                            STOP
                        END IF
                    END IF
                CASE ( 'append' )
                    IF ( ALLOCATED(c8array_dyn) ) THEN
                        tmp = c8array_dyn
                        DEALLOCATE(c8array_dyn)
                    END IF
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            IF ( ALLOCATED(c8array_dyn) ) DEALLOCATE(c8array_dyn)
        END IF

        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Find number of rows and columns of data to collect.
        CALL file_size(nrows,ncols)
        ncols = ncols / 2
        IF ( ncols == 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = -8000
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=empty_err) filename
                STOP
            END IF
        END IF
        
        ! Allocate array to correct size
        IF ( ALLOCATED(tmp) ) THEN
            ncols = SIZE(tmp,2)
            nrows_old = SIZE(tmp,1)
            nrows = nrows + nrows_old
        ELSE
            nrows_old = 0
        END IF
        ALLOCATE(c8array_dyn(nrows,ncols))
        
        ! Read in array
        REWIND(io_unit)
        i = nrows_old + 1
        DO
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) EXIT
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) c8array_dyn(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) c8array_dyn(i,:)
                END IF
                i = i + 1
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
            
        ! Append to array, if necessary
        IF ( ALLOCATED(tmp) ) THEN
            c8array_dyn(1:nrows_old,:) = tmp(:,:)
            DEALLOCATE(tmp)
        END IF
        
        ! Close IO stream
        CLOSE( io_unit )
        
        RETURN
    END SUBROUTINE loadtxt_dynamic_c8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ C 8                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ I 2                   !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a 2 byte integer 2D array of unknown size     !
    !                   into an allocatable array.                             !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   i2array_dyn, the allocatable array which is to be      !
    !                       filled with data read in from the file. INTEGER(2) !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       array if already allocated. Options are 'clobber'  !
    !                       (overwrite array if allocated), 'noclobber'        !
    !                       (produce error if array allocated), and 'append'   !
    !                       (append data to array if already allocated).       !
    !                       CHARACTER                                          !
    !                                                                          !
    !   EXTERNALS:      file_size (subroutine)                                 !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_dynamic_i2 ( filename, i2array_dyn, clobber, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                            ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)                  ::  clobber
        INTEGER, OPTIONAL, INTENT(OUT)                          ::  errval
        INTEGER(2), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT)  ::  i2array_dyn
        
        ! Other variables
        CHARACTER(LEN=line_len)                 ::  line,                      &
                                                    tmp_char
        INTEGER                                 ::  i,                         &
                                                    ioerr,                     &
                                                    ncols,                     &
                                                    nrows,                     &
                                                    nrows_old
        INTEGER(2), ALLOCATABLE, DIMENSION(:,:) ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ( 'clobber' ) 
                    IF ( ALLOCATED(i2array_dyn) ) DEALLOCATE(i2array_dyn)
                CASE ( 'noclobber' )
                    IF ( ALLOCATED(i2array_dyn) ) THEN
                        IF ( PRESENT(errval) ) THEN
                            errval = -6000
                            RETURN
                        ELSE
                            WRITE(UNIT=stderr, FMT=allocate_err) 
                            STOP
                        END IF
                    END IF
                CASE ( 'append' )
                    IF ( ALLOCATED(i2array_dyn) ) THEN
                        tmp = i2array_dyn
                        DEALLOCATE(i2array_dyn)
                    END IF
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            IF ( ALLOCATED(i2array_dyn) ) DEALLOCATE(i2array_dyn)
        END IF

        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Find number of rows and columns of data to collect.
        CALL file_size(nrows,ncols)
        IF ( ncols == 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = -8000
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=empty_err) filename
                STOP
            END IF
        END IF
        
        ! Allocate array to correct size
        IF ( ALLOCATED(tmp) ) THEN
            ncols = SIZE(tmp,2)
            nrows_old = SIZE(tmp,1)
            nrows = nrows + nrows_old
        ELSE
            nrows_old = 0
        END IF
        ALLOCATE(i2array_dyn(nrows,ncols))
        
        ! Read in array
        REWIND(io_unit)
        i = nrows_old + 1
        DO
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) EXIT
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) i2array_dyn(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) i2array_dyn(i,:)
                END IF
                i = i + 1
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
            
        ! Append to array, if necessary
        IF ( ALLOCATED(tmp) ) THEN
            i2array_dyn(1:nrows_old,:) = tmp(:,:)
            DEALLOCATE(tmp)
        END IF
        
        ! Close IO stream
        CLOSE( io_unit )
        
        RETURN
    END SUBROUTINE loadtxt_dynamic_i2
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ I 2                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ I 4                   !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a 4 byte integer 2D array of unknown size     !
    !                   into an allocatable array.                             !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   i4array_dyn, the allocatable array which is to be      !
    !                       filled with data read in from the file. INTEGER(4) !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       array if already allocated. Options are 'clobber'  !
    !                       (overwrite array if allocated), 'noclobber'        !
    !                       (produce error if array allocated), and 'append'   !
    !                       (append data to array if already allocated).       !
    !                       CHARACTER                                          !
    !                                                                          !
    !   EXTERNALS:      file_size (subroutine)                                 !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_dynamic_i4 ( filename, i4array_dyn, clobber, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                            ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)                  ::  clobber
        INTEGER, OPTIONAL, INTENT(OUT)                          ::  errval
        INTEGER(4), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT)  ::  i4array_dyn
        
        ! Other variables
        CHARACTER(LEN=line_len)                 ::  line,                      &
                                                    tmp_char
        INTEGER                                 ::  i,                         &
                                                    ioerr,                     &
                                                    ncols,                     &
                                                    nrows,                     &
                                                    nrows_old
        INTEGER(4), ALLOCATABLE, DIMENSION(:,:) ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ( 'clobber' ) 
                    IF ( ALLOCATED(i4array_dyn) ) DEALLOCATE(i4array_dyn)
                CASE ( 'noclobber' )
                    IF ( ALLOCATED(i4array_dyn) ) THEN
                        IF ( PRESENT(errval) ) THEN
                            errval = -6000
                            RETURN
                        ELSE
                            WRITE(UNIT=stderr, FMT=allocate_err) 
                            STOP
                        END IF
                    END IF
                CASE ( 'append' )
                    IF ( ALLOCATED(i4array_dyn) ) THEN
                        tmp = i4array_dyn
                        DEALLOCATE(i4array_dyn)
                    END IF
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            IF ( ALLOCATED(i4array_dyn) ) DEALLOCATE(i4array_dyn)
        END IF

        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Find number of rows and columns of data to collect.
        CALL file_size(nrows,ncols)
        IF ( ncols == 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = -8000
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=empty_err) filename
                STOP
            END IF
        END IF
        
        ! Allocate array to correct size
        IF ( ALLOCATED(tmp) ) THEN
            ncols = SIZE(tmp,2)
            nrows_old = SIZE(tmp,1)
            nrows = nrows + nrows_old
        ELSE
            nrows_old = 0
        END IF
        ALLOCATE(i4array_dyn(nrows,ncols))
        
        ! Read in array
        REWIND(io_unit)
        i = nrows_old + 1
        DO
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) EXIT
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) i4array_dyn(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) i4array_dyn(i,:)
                END IF
                i = i + 1
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
            
        ! Append to array, if necessary
        IF ( ALLOCATED(tmp) ) THEN
            i4array_dyn(1:nrows_old,:) = tmp(:,:)
            DEALLOCATE(tmp)
        END IF
        
        ! Close IO stream
        CLOSE( io_unit )
        
        RETURN
    END SUBROUTINE loadtxt_dynamic_i4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ I 4                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ R 4                   !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a single precision 2D array of unknown size   !
    !                   into an allocatable array.                             !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   r4array_dyn, the allocatable array which is to be      !
    !                       filled with data read in from the file. REAL(4)    !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       array if already allocated. Options are 'clobber'  !
    !                       (overwrite array if allocated), 'noclobber'        !
    !                       (produce error if array allocated), and 'append'   !
    !                       (append data to array if already allocated).       !
    !                       CHARACTER                                          !
    !                                                                          !
    !   EXTERNALS:      file_size (subroutine)                                 !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_dynamic_r4 ( filename, r4array_dyn, clobber, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                        ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)              ::  clobber
        INTEGER, OPTIONAL, INTENT(OUT)                      ::  errval
        REAL(4), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) ::  r4array_dyn
        
        ! Other variables
        CHARACTER(LEN=line_len)                 ::  line,                      &
                                                    tmp_char
        INTEGER                                 ::  i,                         &
                                                    ioerr,                     &
                                                    ncols,                     &
                                                    nrows,                     &
                                                    nrows_old
        REAL(4), ALLOCATABLE, DIMENSION(:,:)    ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ( 'clobber' ) 
                    IF ( ALLOCATED(r4array_dyn) ) DEALLOCATE(r4array_dyn)
                CASE ( 'noclobber' )
                    IF ( ALLOCATED(r4array_dyn) ) THEN
                        IF ( PRESENT(errval) ) THEN
                            errval = -6000
                            RETURN
                        ELSE
                            WRITE(UNIT=stderr, FMT=allocate_err) 
                            STOP
                        END IF
                    END IF
                CASE ( 'append' )
                    IF ( ALLOCATED(r4array_dyn) ) THEN
                        tmp = r4array_dyn
                        DEALLOCATE(r4array_dyn)
                    END IF
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            IF ( ALLOCATED(r4array_dyn) ) DEALLOCATE(r4array_dyn)
        END IF

        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Find number of rows and columns of data to collect.
        CALL file_size(nrows,ncols)
        IF ( ncols == 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = -8000
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=empty_err) filename
                STOP
            END IF
        END IF
        
        ! Allocate array to correct size
        IF ( ALLOCATED(tmp) ) THEN
            ncols = SIZE(tmp,2)
            nrows_old = SIZE(tmp,1)
            nrows = nrows + nrows_old
        ELSE
            nrows_old = 0
        END IF
        ALLOCATE(r4array_dyn(nrows,ncols))
        
        ! Read in array
        REWIND(io_unit)
        i = nrows_old + 1
        DO
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) EXIT
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) r4array_dyn(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) r4array_dyn(i,:)
                END IF
                i = i + 1
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
            
        ! Append to array, if necessary
        IF ( ALLOCATED(tmp) ) THEN
            r4array_dyn(1:nrows_old,:) = tmp(:,:)
            DEALLOCATE(tmp)
        END IF
        
        ! Close IO stream
        CLOSE( io_unit )
        
        RETURN
    END SUBROUTINE loadtxt_dynamic_r4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ R 4                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ D Y N A M I C _ R 8                   !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a double precision 2D array of unknown size   !
    !                   into an allocatable array.                             !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   r8array_dyn, the allocatable array which is to be      !
    !                       filled with data read in from the file. REAL(8)    !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       array if already allocated. Options are 'clobber'  !
    !                       (overwrite array if allocated), 'noclobber'        !
    !                       (produce error if array allocated), and 'append'   !
    !                       (append data to array if already allocated).       !
    !                       CHARACTER                                          !
    !                                                                          !
    !   EXTERNALS:      file_size (subroutine)                                 !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_dynamic_r8 ( filename, r8array_dyn, clobber, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                        ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)              ::  clobber
        INTEGER, OPTIONAL, INTENT(OUT)                      ::  errval
        REAL(8), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) ::  r8array_dyn
        
        ! Other variables
        CHARACTER(LEN=line_len)                 ::  line,                      &
                                                    tmp_char
        INTEGER                                 ::  i,                         &
                                                    ioerr,                     &
                                                    ncols,                     &
                                                    nrows,                     &
                                                    nrows_old
        REAL(8), ALLOCATABLE, DIMENSION(:,:)    ::  tmp
    !--------------------------------------------------------------------------!

        ! Determine options for overwriting array
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ( 'clobber' ) 
                    IF ( ALLOCATED(r8array_dyn) ) DEALLOCATE(r8array_dyn)
                CASE ( 'noclobber' )
                    IF ( ALLOCATED(r8array_dyn) ) THEN
                        IF ( PRESENT(errval) ) THEN
                            errval = -6000
                            RETURN
                        ELSE
                            WRITE(UNIT=stderr, FMT=allocate_err) 
                            STOP
                        END IF
                    END IF
                CASE ( 'append' )
                    IF ( ALLOCATED(r8array_dyn) ) THEN
                        tmp = r8array_dyn
                        DEALLOCATE(r8array_dyn)
                    END IF
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            IF ( ALLOCATED(r8array_dyn) ) DEALLOCATE(r8array_dyn)
        END IF

        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Find number of rows and columns of data to collect.
        CALL file_size(nrows,ncols)
        IF ( ncols == 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = -8000
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=empty_err) filename
                STOP
            END IF
        END IF
        
        ! Allocate array to correct size
        IF ( ALLOCATED(tmp) ) THEN
            ncols = SIZE(tmp,2)
            nrows_old = SIZE(tmp,1)
            nrows = nrows + nrows_old
        ELSE
            nrows_old = 0
        END IF
        ALLOCATE(r8array_dyn(nrows,ncols))
        
        ! Read in array
        REWIND(io_unit)
        i = nrows_old + 1
        DO
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) EXIT
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) r8array_dyn(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) r8array_dyn(i,:)
                END IF
                i = i + 1
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
            
        ! Append to array, if necessary
        IF ( ALLOCATED(tmp) ) THEN
            r8array_dyn(1:nrows_old,:) = tmp(:,:)
            DEALLOCATE(tmp)
        END IF
        
        ! Close IO stream
        CLOSE( io_unit )
        
        RETURN
    END SUBROUTINE loadtxt_dynamic_r8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ D Y N A M I C _ R 8                   !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ C 8                     !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in an 8 byte complex 2D array whose dimensions   !
    !                   are known in advance. Note that there is no way to     !
    !                   know if some data was missed because the array was too !
    !                   small.                                                 !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   c8array, the array which is to be filled with values   !
    !                       read in from the file. COMPLEX(8)                     !
    !                   nrows, returns the number of rows of data which were   !
    !                       read into the array. INTEGER                       !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_static_c8 ( filename, c8array, nrows, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)            ::  filename
        INTEGER, INTENT(OUT)                    ::  nrows
        INTEGER, OPTIONAL, INTENT(OUT)          ::  errval
        COMPLEX(8), DIMENSION(:,:), INTENT(OUT) ::  c8array
        
        ! Other variables:
        CHARACTER(LEN=line_len) ::  line,                                      &
                                    tmp_char
        INTEGER                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
                
        i = 0
        DO WHILE ( i < SIZE(c8array,1) )
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            print*,line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) THEN
                i = i + 1
                EXIT
            END IF
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                i = i + 1
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) c8array(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) c8array(i,:)
                END IF
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        nrows = i

        ! Close IO stream
        CLOSE( io_unit )

        RETURN
    END SUBROUTINE loadtxt_static_c8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ C 8                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                   L O A D T X T _ S T A T I C _ C 1 6                    !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a 16 byte complex 2D array whose dimensions   !
    !                   are known in advance. Note that there is no way to     !
    !                   know if some data was missed because the array was too !
    !                   small.                                                 !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   c16array, the array which is to be filled with values  !
    !                       read in from the file. COMPLEX(16)                 !
    !                   nrows, returns the number of rows of data which were   !
    !                       read into the array. INTEGER                       !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_static_c16 ( filename, c16array, nrows, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                ::  filename
        INTEGER, INTENT(OUT)                        ::  nrows
        INTEGER, OPTIONAL, INTENT(OUT)              ::  errval
        COMPLEX(16), DIMENSION(:,:), INTENT(OUT)    ::  c16array
        
        ! Other variables:
        CHARACTER(LEN=line_len) ::  line,                                      &
                                    tmp_char
        INTEGER                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
                
        i = 0
        DO WHILE ( i < SIZE(c16array,1) )
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) THEN
                i = i + 1
                EXIT
            END IF
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                i = i + 1
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) c16array(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) c16array(i,:)
                END IF
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        nrows = i-1

        ! Close IO stream
        CLOSE( io_unit )

        RETURN
    END SUBROUTINE loadtxt_static_c16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                   L O A D T X T _ S T A T I C _ C 1 6                    !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ I 2                     !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a 2 byte integer 2D array whose dimensions    !
    !                   are known in advance. Note that there is no way to     !
    !                   know if some data was missed because the array was too !
    !                   small.                                                 !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   i2array, the array which is to be filled with values   !
    !                       read in from the file. INTEGER(2)                  !
    !                   nrows, returns the number of rows of data which were   !
    !                       read into the array. INTEGER                       !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_static_i2 ( filename, i2array, nrows, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)            ::  filename
        INTEGER, INTENT(OUT)                    ::  nrows
        INTEGER, OPTIONAL, INTENT(OUT)          ::  errval
        INTEGER(2), DIMENSION(:,:), INTENT(OUT) ::  i2array
        
        ! Other variables:
        CHARACTER(LEN=line_len) ::  line,                                      &
                                    tmp_char
        INTEGER                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
                
        i = 0
        DO WHILE ( i < SIZE(i2array,1) )
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) THEN
                i = i + 1
                EXIT
            END IF
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                i = i + 1
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) i2array(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) i2array(i,:)
                END IF
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        nrows = i-1

        ! Close IO stream
        CLOSE( io_unit )

        RETURN
    END SUBROUTINE loadtxt_static_i2
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ I 2                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ I 4                     !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a 4 byte integer 2D array whose dimensions    !
    !                   are known in advance. Note that there is no way to     !
    !                   know if some data was missed because the array was too !
    !                   small.                                                 !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   i4array, the array which is to be filled with values   !
    !                       read in from the file. INTEGER(4)                  !
    !                   nrows, returns the number of rows of data which were   !
    !                       read into the array. INTEGER                       !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_static_i4 ( filename, i4array, nrows, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)            ::  filename
        INTEGER, INTENT(OUT)                    ::  nrows
        INTEGER, OPTIONAL, INTENT(OUT)          ::  errval
        INTEGER(4), DIMENSION(:,:), INTENT(OUT) ::  i4array
        
        ! Other variables:
        CHARACTER(LEN=line_len) ::  line,                                      &
                                    tmp_char
        INTEGER                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!

        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
                
        i = 0
        DO WHILE ( i < SIZE(i4array,1) )
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) THEN
                i = i + 1
                EXIT
            END IF
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                i = i + 1
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) i4array(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) i4array(i,:)
                END IF
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        nrows = i-1

        ! Close IO stream
        CLOSE( io_unit )

        RETURN
    END SUBROUTINE loadtxt_static_i4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ I 4                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ R 8                     !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a single precision 2D array whose dimensions  !
    !                   are known in advance. Note that there is no way to     !
    !                   know if some data was missed because the array was too !
    !                   small.                                                 !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   r8array, the array which is to be filled with values   !
    !                       read in from the file. REAL(8)                     !
    !                   nrows, returns the number of rows of data which were   !
    !                       read into the array. INTEGER                       !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_static_r4 ( filename, r4array, nrows, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)            ::  filename
        INTEGER, INTENT(OUT)                    ::  nrows
        INTEGER, OPTIONAL, INTENT(OUT)          ::  errval
        REAL(4), DIMENSION(:,:), INTENT(OUT)    ::  r4array
        
        ! Other variables:
        CHARACTER(LEN=line_len) ::  line,                                      &
                                    tmp_char
        INTEGER                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
                
        i = 0
        DO WHILE ( i < SIZE(r4array,1) )
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) THEN
                i = i + 1
                EXIT
            END IF
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                i = i + 1
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) r4array(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) r4array(i,:)
                END IF
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        nrows = i

        ! Close IO stream
        CLOSE( io_unit )

        RETURN
    END SUBROUTINE loadtxt_static_r4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ R 4                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                    L O A D T X T _ S T A T I C _ R 8                     !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Reads in a double precision 2D array whose dimensions  !
    !                   are known in advance. Note that there is no way to     !
    !                   know if some data was missed because the array was too !
    !                   small.                                                 !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file from which to read in the array.    !
    !                       CHARACTER                                          !
    !                   r8array, the array which is to be filled with values   !
    !                       read in from the file. REAL(8)                     !
    !                   nrows, returns the number of rows of data which were   !
    !                       read into the array. INTEGER                       !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE loadtxt_static_r8 ( filename, r8array, nrows, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)            ::  filename
        INTEGER, INTENT(OUT)                    ::  nrows
        INTEGER, OPTIONAL, INTENT(OUT)          ::  errval
        REAL(8), DIMENSION(:,:), INTENT(OUT)    ::  r8array
        
        ! Other variables:
        CHARACTER(LEN=line_len) ::  line,                                      &
                                    tmp_char
        INTEGER                 ::  i,                                         &
                                    ioerr
    !--------------------------------------------------------------------------!
        
        ! Open IO stream
        OPEN(UNIT=io_unit, FILE=TRIM(filename), ACTION='read', STATUS='old',   &
             IOSTAT=ioerr)
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
                
        i = 0
        DO WHILE ( i < SIZE(r8array,1) )
            READ(UNIT=io_unit, FMT='(A)', IOSTAT=ioerr) line
            IF ( ( ioerr == 5008 ) .OR. ( ioerr == -1 ) ) THEN
                i = i + 1
                EXIT
            END IF
            tmp_char = ADJUSTL(line(1:line_len-1)//com_char)
            IF ( tmp_char(1:1) /= com_char ) THEN
                i = i + 1
                IF ( INDEX(line,com_char) == 0 ) THEN
                    READ(UNIT=line, FMT=*, IOSTAT=ioerr) r8array(i,:)
                ELSE
                    READ(UNIT=line(1:INDEX(line,com_char)-1), FMT=*,           &
                      IOSTAT=ioerr) r8array(i,:)
                END IF
            END IF
            IF ( ( ioerr /= 0 ) .AND. ( ioerr /= -1 ) ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=read_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        nrows = i-1

        ! Close IO stream
        CLOSE( io_unit )

        RETURN
    END SUBROUTINE loadtxt_static_r8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                    L O A D T X T _ S T A T I C _ R 8                     !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ C 8                            !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Writes out an 8 byte complex 2D array.                 !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. CHARACTER                                    !
    !                   c8array, the array which is to be written to the file. !
    !                       COMPLEX(8)                                         !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). CHARACTER                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. CHARACTER                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE savetxt_c8 ( filename, c8array, clobber, user_format, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                    ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)          ::  clobber,           &
                                                            user_format
        COMPLEX(8), DIMENSION(:,:), TARGET, INTENT(IN)  ::  c8array
        INTEGER, OPTIONAL, INTENT(OUT)                  ::  errval
        
        ! Other variables:
        CHARACTER(LEN=8)                ::  file_pos,                          &
                                            file_stat
        CHARACTER(LEN=128)              ::  u_format
        CHARACTER(LEN=16), PARAMETER    ::  def_form = '(*(g26.18))'
        INTEGER                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                CASE ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                CASE ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            file_pos = 'asis'
            file_stat = 'replace'
        END IF
        
        ! Determine whether to use a user-supplied format string
        IF ( PRESENT(user_format) ) THEN
            u_format = user_format
        ELSE
            u_format = def_form
        END IF
        
        ! Open IO stream
        IF ( TRIM(filename) == 'stdout' ) THEN
            out_unit = stdout
            ioerr = 0
        ELSE
            out_unit = io_unit
            OPEN(UNIT=out_unit, FILE=TRIM(filename), ACTION='write',           &
                 IOSTAT=ioerr, STATUS=file_stat, POSITION=file_pos)
        END IF
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Write out array
        DO i = 1, SIZE(c8array,1)
            WRITE(UNIT=out_unit, FMT=u_format, IOSTAT=ioerr) c8array(i,:)
            IF ( ioerr /= 0 ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=write_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        
        ! Clean up
        IF ( TRIM(filename) /= 'stdout' ) CLOSE( io_unit )

        RETURN
    END SUBROUTINE savetxt_c8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ C 8                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                          S A V E T X T _ C 1 6                           !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Writes out a 16 byte complex 2D array.                 !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. CHARACTER                                    !
    !                   c16array, the array which is to be written to the      !
    !                       file. COMPLEX(16)                                  !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). CHARACTER                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. CHARACTER                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE savetxt_c16 ( filename, c16array, clobber, user_format, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                    ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)          ::  clobber,           &
                                                            user_format
        COMPLEX(16), DIMENSION(:,:), TARGET, INTENT(IN) ::  c16array
        INTEGER, OPTIONAL, INTENT(OUT)                  ::  errval
        
        ! Other variables:
        CHARACTER(LEN=8)                ::  file_pos,                          &
                                            file_stat
        CHARACTER(LEN=128)              ::  u_format
        CHARACTER(LEN=16), PARAMETER    ::  def_form = '(*(g26.18))'
        INTEGER                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                CASE ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                CASE ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            file_pos = 'asis'
            file_stat = 'replace'
        END IF
        
        ! Determine whether to use a user-supplied format string
        IF ( PRESENT(user_format) ) THEN
            u_format = user_format
        ELSE
            u_format = def_form
        END IF
        
        ! Open IO stream
        IF ( TRIM(filename) == 'stdout' ) THEN
            out_unit = stdout
            ioerr = 0
        ELSE
            out_unit = io_unit
            OPEN(UNIT=out_unit, FILE=TRIM(filename), ACTION='write',           &
                 IOSTAT=ioerr, STATUS=file_stat, POSITION=file_pos)
        END IF
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Write out array
        DO i = 1, SIZE(c16array,1)
            WRITE(UNIT=out_unit, FMT=u_format, IOSTAT=ioerr) c16array(i,:)
            IF ( ioerr /= 0 ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=write_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        
        ! Clean up
        IF ( TRIM(filename) /= 'stdout' ) CLOSE( io_unit )

        RETURN
    END SUBROUTINE savetxt_c16
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                          S A V E T X T _ C 1 6                           !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ I 2                            !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Writes out a 2 byte integer 2D array.                  !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. CHARACTER                                    !
    !                   i2array, the array which is to be written to the file. !
    !                       INTEGER(2)                                         !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). CHARACTER                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. CHARACTER                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE savetxt_i2 ( filename, i2array, clobber, user_format, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                    ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)          ::  clobber,           &
                                                            user_format
        INTEGER(2), DIMENSION(:,:), TARGET, INTENT(IN)  ::  i2array
        INTEGER, OPTIONAL, INTENT(OUT)                  ::  errval
        
        ! Other variables:
        CHARACTER(LEN=8)                ::  file_pos,                          &
                                            file_stat
        CHARACTER(LEN=128)              ::  u_format
        CHARACTER(LEN=16), PARAMETER    ::  def_form = '(*(i7))'
        INTEGER                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                CASE ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                CASE ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            file_pos = 'asis'
            file_stat = 'replace'
        END IF
        
        ! Determine whether to use a user-supplied format string
        IF ( PRESENT(user_format) ) THEN
            u_format = user_format
        ELSE
            u_format = def_form
        END IF
        
        ! Open IO stream
        IF ( TRIM(filename) == 'stdout' ) THEN
            out_unit = stdout
            ioerr = 0
        ELSE
            out_unit = io_unit
            OPEN(UNIT=out_unit, FILE=TRIM(filename), ACTION='write',           &
                 IOSTAT=ioerr, STATUS=file_stat, POSITION=file_pos)
        END IF
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Write out array
        DO i = 1, SIZE(i2array,1)
            WRITE(UNIT=out_unit, FMT=u_format, IOSTAT=ioerr) i2array(i,:)
            IF ( ioerr /= 0 ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=write_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        
        ! Clean up
        IF ( TRIM(filename) /= 'stdout' ) CLOSE( io_unit )

        RETURN
    END SUBROUTINE savetxt_I2
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ I 2                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ I 4                            !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Writes out a 4 byte integer 2D array.                  !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. CHARACTER                                    !
    !                   i4array, the array which is to be written to the file. !
    !                       INTEGER(4)                                         !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). CHARACTER                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. CHARACTER                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE savetxt_i4 ( filename, i4array, clobber, user_format, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                    ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)          ::  clobber,           &
                                                            user_format
        INTEGER(4), DIMENSION(:,:), TARGET, INTENT(IN)  ::  i4array
        INTEGER, OPTIONAL, INTENT(OUT)                  ::  errval
        
        ! Other variables:
        CHARACTER(LEN=8)                ::  file_pos,                          &
                                            file_stat
        CHARACTER(LEN=128)              ::  u_format
        CHARACTER(LEN=16), PARAMETER    ::  def_form = '(*(i12))'
        INTEGER                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                CASE ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                CASE ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            file_pos = 'asis'
            file_stat = 'replace'
        END IF
        
        ! Determine whether to use a user-supplied format string
        IF ( PRESENT(user_format) ) THEN
            u_format = user_format
        ELSE
            u_format = def_form
        END IF
        
        ! Open IO stream
        IF ( TRIM(filename) == 'stdout' ) THEN
            out_unit = stdout
            ioerr = 0
        ELSE
            out_unit = io_unit
            OPEN(UNIT=out_unit, FILE=TRIM(filename), ACTION='write',           &
                 IOSTAT=ioerr, STATUS=file_stat, POSITION=file_pos)
        END IF
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Write out array
        DO i = 1, SIZE(i4array,1)
            WRITE(UNIT=out_unit, FMT=u_format, IOSTAT=ioerr) i4array(i,:)
            IF ( ioerr /= 0 ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=write_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        
        ! Clean up
        IF ( TRIM(filename) /= 'stdout' ) CLOSE( io_unit )

        RETURN
    END SUBROUTINE savetxt_I4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ I 4                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ R 4                            !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Writes out a single precision 2D array.                !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. CHARACTER                                    !
    !                   r4array, the array which is to be written to the file. !
    !                       REAL(4)                                            !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). CHARACTER                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. CHARACTER                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE savetxt_r4 ( filename, r4array, clobber, user_format, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)      ::  clobber,               &
                                                        user_format
        REAL(4), DIMENSION(:,:), TARGET, INTENT(IN) ::  r4array
        INTEGER, OPTIONAL, INTENT(OUT)              ::  errval
        
        ! Other variables:
        CHARACTER(LEN=8)                ::  file_pos,                          &
                                            file_stat
        CHARACTER(LEN=128)              ::  u_format
        CHARACTER(LEN=16), PARAMETER    ::  def_form = '(*(g17.9))'
        INTEGER                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                CASE ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                CASE ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            file_pos = 'asis'
            file_stat = 'replace'
        END IF
        
        ! Determine whether to use a user-supplied format string
        IF ( PRESENT(user_format) ) THEN
            u_format = user_format
        ELSE
            u_format = def_form
        END IF
        
        ! Open IO stream
        IF ( TRIM(filename) == 'stdout' ) THEN
            out_unit = stdout
            ioerr = 0
        ELSE
            out_unit = io_unit
            OPEN(UNIT=out_unit, FILE=TRIM(filename), ACTION='write',           &
                 IOSTAT=ioerr, STATUS=file_stat, POSITION=file_pos)
        END IF
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Write out array
        DO i = 1, SIZE(r4array,1)
            WRITE(UNIT=out_unit, FMT=u_format, IOSTAT=ioerr) r4array(i,:)
            IF ( ioerr /= 0 ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=write_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        
        ! Clean up
        IF ( TRIM(filename) /= 'stdout' ) CLOSE( io_unit )

        RETURN
    END SUBROUTINE savetxt_r4
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ R 4                            !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                           S A V E T X T _ R 8                            !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Writes out a double precision 2D array.                !
    !                                                                          !
    !   ARGUMENTS:     *filename, the file to which to write in the array. If  !
    !                       it is 'stdout' then writes to terminal instead of  !
    !                       file. CHARACTER                                    !
    !                   r8array, the array which is to be written to the file. !
    !                       REAL(8)                                            !
    !                  *[clobber = 'clobber'], determines whether to overwrite !
    !                       existing files. Options are 'clobber' (overwrite   !
    !                       file if exists), 'noclobber' (produce error if     !
    !                       file exists), and 'append' (append output to file  !
    !                       if exists). CHARACTER                              !
    !                  *[user_format], a format string which can be used to    !
    !                       provide a custom specification for the array       !
    !                       output. CHARACTER                                  !
    !                   [errval], returns any error codes generated during the !
    !                       execution of this subroutine. INTEGER              !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE savetxt_r8 ( filename, r8array, clobber, user_format, errval )
        IMPLICIT NONE
        
        ! Input and output variables:
        CHARACTER(LEN=*), INTENT(IN)                ::  filename
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN)      ::  clobber,               &
                                                        user_format
        REAL(8), DIMENSION(:,:), TARGET, INTENT(IN) ::  r8array
        INTEGER, OPTIONAL, INTENT(OUT)              ::  errval
        
        ! Other variables:
        CHARACTER(LEN=8)                ::  file_pos,                          &
                                            file_stat
        CHARACTER(LEN=128)              ::  u_format
        CHARACTER(LEN=16), PARAMETER    ::  def_form = '(*(g26.18))'
        INTEGER                         ::  i,                                 &
                                            ioerr,                             &
                                            out_unit
    !--------------------------------------------------------------------------!
        
        ! Determine options for opening file
        IF ( PRESENT(clobber) ) THEN
            SELECT CASE (clobber)
                CASE ('clobber') 
                    file_pos = 'asis'
                    file_stat = 'replace'
                CASE ('noclobber')
                    file_pos = 'asis'
                    file_stat = 'new'
                CASE ('append')
                    file_pos = 'append'
                    file_stat = 'unknown'
                CASE DEFAULT
                    IF ( PRESENT(errval) ) THEN
                        errval = -5000
                        RETURN
                    ELSE
                        WRITE(UNIT=stderr, FMT=clob_err)
                        STOP
                    END IF
            END SELECT
        ELSE
            file_pos = 'asis'
            file_stat = 'replace'
        END IF
        
        ! Determine whether to use a user-supplied format string
        IF ( PRESENT(user_format) ) THEN
            u_format = user_format
        ELSE
            u_format = def_form
        END IF
        
        ! Open IO stream
        IF ( TRIM(filename) == 'stdout' ) THEN
            out_unit = stdout
            ioerr = 0
        ELSE
            out_unit = io_unit
            OPEN(UNIT=out_unit, FILE=TRIM(filename), ACTION='write',           &
                 IOSTAT=ioerr, STATUS=file_stat, POSITION=file_pos)
        END IF
        IF ( ioerr /= 0 ) THEN
            IF ( PRESENT(errval) ) THEN
                errval = ioerr
                RETURN
            ELSE
                WRITE(UNIT=stderr, FMT=open_err) ioerr
                STOP
            END IF
        END IF
        
        ! Write out array
        DO i = 1, SIZE(r8array,1)
            WRITE(UNIT=out_unit, FMT=u_format, IOSTAT=ioerr) r8array(i,:)
            IF ( ioerr /= 0 ) THEN
                IF ( PRESENT(errval) ) THEN
                    errval = ioerr
                    RETURN
                ELSE
                    WRITE(UNIT=stderr, FMT=write_err) ioerr
                    STOP
                END IF
            END IF
        END DO
        
        ! Clean up
        IF ( TRIM(filename) /= 'stdout' ) CLOSE( io_unit )

        RETURN
    END SUBROUTINE savetxt_r8
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                           S A V E T X T _ R 8                            !
    !==========================================================================!

END MODULE array_io
!==============================================================================!
!                            E N D    M O D U L E :                            !
!                               A R R A Y _ I O                                !
!==============================================================================!
