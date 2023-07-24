MODULE msgtools
    IMPLICIT NONE

    PUBLIC :: size_msg, save_msg, load_msg

CONTAINS

    INTEGER FUNCTION size_msg(nf)
        IMPLICIT NONE
        ! Variables in/out
        CHARACTER(len=*), INTENT(in)         :: nf  
        ! Variables locales
        CHARACTER(len=20)                     :: word
        INTEGER, PARAMETER                    :: file_number=19, eof=144
        INTEGER                               :: i

        OPEN(UNIT=file_number, FILE=TRIM(nf), STATUS='old')
        DO i=1, eof   
            READ (file_number, *, END=77) word
        END DO
        77 CONTINUE
        CLOSE(file_number)

        IF (i ==  eof) THEN
            PRINT  *, "El mensaje es más largo de lo permitido, this is twitter!"
            size_msg = -1
        END IF
        size_msg = i - 1

        RETURN 
    END FUNCTION size_msg

    SUBROUTINE load_msg(msg, sm, nf)
        IMPLICIT NONE
        ! Variables in/out
        INTEGER, INTENT(in)                            :: sm
        CHARACTER(len=*), INTENT(in)                   :: nf
        CHARACTER(len=20), DIMENSION(sm), INTENT(out)  :: msg
        ! Variables locales
        INTEGER, PARAMETER                             :: file_number=20
        
        OPEN(UNIT=file_number, FILE=TRIM(nf), STATUS='old')
        READ (file_number, *, END=77) msg(1:sm)
        77 CONTINUE
        CLOSE(file_number)
        RETURN
        
    END SUBROUTINE load_msg


    SUBROUTINE save_msg(msg, sm, nf)
        IMPLICIT NONE
        ! Variables in/out
        INTEGER, INTENT(in)                            :: sm
        CHARACTER(len=*), INTENT(in)                   :: nf
        CHARACTER(len=20), DIMENSION(sm), INTENT(out)  :: msg
        ! Variables locales
        INTEGER, PARAMETER                             :: file_number=21
        
        OPEN(UNIT=file_number, FILE=TRIM(nf), STATUS='new')
        WRITE (file_number, *) msg(1:sm)
        CLOSE(file_number)
        RETURN
        
    END SUBROUTINE save_msg

END MODULE msgtools