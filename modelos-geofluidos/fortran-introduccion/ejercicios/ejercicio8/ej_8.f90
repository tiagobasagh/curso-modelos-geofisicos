PROGRAM main
    IMPLICIT NONE

   
    INTEGER :: i
    CHARACTER(len=50) :: palabra
    CHARACTER(len=1) :: change_case

    PRINT *, "Ingrese una palabra: "
    READ *, palabra

    looppalabra: DO i=1, LEN(TRIM(palabra))
        palabra(i:i) = change_case(palabra(i:i))
    END DO looppalabra

    PRINT *, palabra

END PROGRAM main

CHARACTER(len=1) FUNCTION change_case(letra)
    
    CHARACTER(len=1), INTENT(in) :: letra
    INTEGER, PARAMETER ::  ascii_distance=32
    LOGICAL :: is_minuscula, is_mayuscula
    
    
    isminmay: IF (is_minuscula(letra)) THEN 
        change_case = ACHAR( ICHAR(letra) + ascii_distance )
    ELSE IF (is_mayuscula(letra)) THEN
        change_case = ACHAR( ICHAR(letra) - ascii_distance )
    ELSE
        change_case = letra
    END IF isminmay
    
    RETURN 

END FUNCTION change_case

LOGICAL FUNCTION is_minuscula(c)
    CHARACTER(len=1), INTENT(in) :: c
    INTEGER, PARAMETER :: min=65, max=90
    is_minuscula = .FALSE.
    IF ((ICHAR(c) >= min) .AND. (ICHAR(c) <= max)) is_minuscula = .TRUE.
    
    RETURN
END FUNCTION is_minuscula

LOGICAL FUNCTION is_mayuscula(c)
    CHARACTER(len=1), INTENT(in) :: c
    INTEGER, PARAMETER :: min=97, max=122
    is_mayuscula = .FALSE.
    IF ((ICHAR(c) >= min) .AND. (ICHAR(c) <= max)) is_mayuscula = .TRUE.
    
    RETURN
END FUNCTION is_mayuscula