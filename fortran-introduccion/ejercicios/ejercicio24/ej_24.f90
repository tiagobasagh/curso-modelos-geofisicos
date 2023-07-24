PROGRAM main

    IMPLICIT NONE
    ! Defino variables argumentos
    CHARACTER(LEN=500), ALLOCATABLE, DIMENSION(:) :: args
    INTEGER                                       :: narg
    LOGICAL                                       :: errarg
    ! Defino Variables programa
    CHARACTER(len=6)                              :: rgbin
    CHARACTER(len=6)                              :: method
    

    errarg =.FALSE.
    narg=COMMAND_ARGUMENT_COUNT()
    IF (ALLOCATED(args)) DEALLOCATE(args)
    ALLOCATE(args(narg))
    
    CALL get_args(args, narg, errarg, rgbin, method)

    IF (.NOT. errarg) THEN
        CALL convert_colors(rgbin, method) 
    END IF

END PROGRAM main

SUBROUTINE convert_colors(rgbin, method)
    IMPLICIT NONE
    CHARACTER(len=6), INTENT(inout)                 :: rgbin
    CHARACTER(len=6), INTENT(inout)                 :: method
    SELECT CASE (method)
        CASE ("rgb255")
            CALL method_integer(rgbin)
        CASE ("rgb01")
            CALL method_real(rgbin)
    END SELECT 
    RETURN 
END SUBROUTINE convert_colors

SUBROUTINE method_real(hexadecimal)
    IMPLICIT NONE 
    CHARACTER(len=6), INTENT(in)  :: hexadecimal
    ! Variables locales
    REAL, DIMENSION(3)            :: rgb 
    INTEGER                       :: i
    ! Funciones
    REAL                          :: normalize
    INTEGER                       :: to_integer
    DO i=0, 2
        rgb(i+1) = normalize(to_integer(hexadecimal((i*2) + 1: (i*2) + 2)), 0, 255)
    END DO

    PRINT *, rgb
END SUBROUTINE method_real

SUBROUTINE method_integer(hexadecimal)
    IMPLICIT NONE 
    CHARACTER(len=6), INTENT(in)  :: hexadecimal
    ! Variables locales
    INTEGER, DIMENSION(3)            :: rgb 
    INTEGER                       :: i
    ! Funciones
    REAL                          :: normalize
    INTEGER                       :: to_integer
    DO i=0, 2
        rgb(i+1) = to_integer(hexadecimal((i*2) + 1: (i*2) + 2))
    END DO

    PRINT *, rgb
END SUBROUTINE method_integer


REAL FUNCTION normalize(number, minv, maxv)
    IMPLICIT NONE
    INTEGER, INTENT(in)         :: number, minv, maxv
    normalize = number/ (maxv - minv)
    RETURN
END FUNCTION normalize

INTEGER FUNCTION to_integer(hexadecimal)
    IMPLICIT NONE 
    CHARACTER(len=2), INTENT(IN) :: hexadecimal
    INTEGER                      :: to_integer_digit
    to_integer = 16 * to_integer_digit(hexadecimal(1:1)) + to_integer_digit(hexadecimal(2:2))
    RETURN
END FUNCTION to_integer


INTEGER FUNCTION to_integer_digit(hexadecimal)
    IMPLICIT NONE
    CHARACTER(len=1), INTENT(in)    :: hexadecimal
    CHARACTER(len=2)                :: aux
    INTEGER                         :: num
    WRITE(aux, '(Z2)') hexadecimal
    read (aux, *) num
    IF (num <= 9) THEN
        to_integer_digit = num - 30
    ELSE 
        to_integer_digit = num -31
    END IF
RETURN 
END FUNCTION to_integer_digit


SUBROUTINE get_args(args, narg, errarg, rgbin, method)
    IMPLICIT NONE
    ! Input/Output variables
    INTEGER, INTENT(in)                              :: narg
    CHARACTER(LEN=500), INTENT(in), DIMENSION(narg)  :: args
    LOGICAL, INTENT(inout)                           :: errarg
    CHARACTER(len=6), INTENT(inout)                 :: rgbin
    CHARACTER(len=6), INTENT(inout)                 :: method
    ! Local variables
    CHARACTER(LEN=15)                                :: arg1
    CHARACTER(LEN=15)                                :: arg2
    INTEGER                                          :: iarg
    

    DO iarg=1, narg
        CALL GETARG(iarg, args(iarg))
    END DO
    
    errarg =.FALSE.
    arg1 = TRIM(args(1))
    arg2 = TRIM(args(2))

    IF ((LEN_TRIM(arg1) == 2 ) .AND. (arg1 == "-h")) THEN
        errarg =.TRUE.
        CALL help()
    ELSE IF (.NOT. (narg==2)) THEN
        errarg =.TRUE.
        CALL invalid_input("numberargs")
    ELSE 
        CALL check_hexa_input(arg1, errarg)
        IF (.NOT. errarg) CALL check_method_input(arg2, errarg)
    END IF

    rgbin = arg1(1:6)
    method = arg2(1:6)

RETURN
END SUBROUTINE

SUBROUTINE check_method_input(arg, errarg)
    IMPLICIT NONE
    CHARACTER(len=15), INTENT(in)                    :: arg
    LOGICAL, INTENT(inout)                           :: errarg

    IF (.NOT.((TRIM(arg)=="rgb255") .OR. (TRIM(arg)=="rgb01"))) THEN
        errarg = .TRUE.
        CALL invalid_input("method")
    END IF
    RETURN
END SUBROUTINE check_method_input

SUBROUTINE check_hexa_input(arg, errarg)
    IMPLICIT NONE
    CHARACTER(len=15), INTENT(in)        :: arg
    LOGICAL, INTENT(inout)               :: errarg

    CHARACTER(LEN=16), PARAMETER         :: valid_hexas="0123456789ABCDEF"
    INTEGER                              :: i

    IF ( .NOT.( LEN_TRIM(arg)==6 ) ) THEN
        errarg = .TRUE.
        CALL invalid_input("tolong")
    ELSE
        DO i=1, 6
            IF (SCAN(valid_hexas, arg(i:i))==0) errarg = .TRUE.    
        END DO
        IF (errarg) CALL invalid_input("nothexa")
    END IF
    RETURN
END SUBROUTINE check_hexa_input

SUBROUTINE help()
    IMPLICIT NONE
    PRINT *, "Uso de RGB.app:"
    PRINT *, "    hexrgb_num [RR][GG][BB](hexanumerico) [tipo](tipo de transformacion)"
    PRINT *, "    rgb255: transformado a un vector con 3 valores enteros [0, 255]"
    PRINT *, "    rgb01: transformado a un vector con 3 valores reales [0., 1.]"
END SUBROUTINE

SUBROUTINE invalid_input(terror)
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(in)     :: terror
   
    SELECT CASE (terror)
        CASE ("tolong")
            PRINT *, "RBG Hexa color need have only 6 digits."
        CASE ("nothexa")
            PRINT *, "Some digit is not a hexadecimal number."
            PRINT *, "Remember: 0-1-2-3-4-5-6-7-8-9-0 AND A-B-C-D-E-F are the hexadecimal digits."
        CASE ("method")
            PRINT *, "It's a invalid conversion method. Please try again."
        CASE ("numberargs")
            PRINT *, "Insufficient arguments. They are RGB and METHOD!"
        CASE DEFAULT
            PRINT *, "Mysterius error, are you a magician?"
    END SELECT
    
    PRINT *, "add -h to help"
    RETURN
END SUBROUTINE invalid_input