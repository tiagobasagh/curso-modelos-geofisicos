PROGRAM main
    IMPLICIT NONE
    ! Defino funciones
    LOGICAL :: is_valid
    ! Defino parametros
    INTEGER, PARAMETER :: sizeArray = 26
    ! Defino variables
    CHARACTER(len=20) :: nf="listprimos.dat"
    INTEGER, DIMENSION(26) :: array_divisors
    INTEGER :: N=0, index=0, aux=0
    
    CALL init_empty_array(array_divisors, sizeArray)
    CALL get_divisors(nf, array_divisors, sizeArray)

    PRINT *, "Ingrese un numero entero entre 1 y 200"
    READ *, N
    PRINT *, "          Divisor, veces que se repite"
    IF (is_valid(N)) THEN
        DO index=2, sizeArray
            
            DO WHILE (mod(N, INT(index))==0) 
                aux = aux + 1
                n = INT(n/index)
            END DO
            
            IF (aux /= 0) THEN
                PRINT *, index, ",", aux
                aux = 0
            END IF

        END DO
    ELSE 
        PRINT *, "Del 1 al 200 carajo!" 
    END IF

END PROGRAM main

LOGICAL FUNCTION is_valid(N)
    ! inicio funcion
    is_valid = .FALSE.

    IF ((N >= 1) .AND. (N <= 200) ) is_valid = .TRUE.
    
    RETURN
END FUNCTION is_valid


SUBROUTINE init_empty_array(array, sizeArray)
    ! Defino variables in/out
    INTEGER, DIMENSION(26), INTENT(inout) :: array
    INTEGER, INTENT(in) :: sizeArray

    cleanloop: DO i=1, sizeArray
        array(i) = 0
    END DO cleanloop

    RETURN
    
END SUBROUTINE init_empty_array

SUBROUTINE get_divisors(nf, array_divisors, sizeArray)
    ! Variables externas
    CHARACTER(len=20), INTENT(in) :: nf
    INTEGER, DIMENSION(26), INTENT(inout) :: array_divisors
    INTEGER, INTENT(in) :: sizeArray
    
    OPEN(2, file=TRIM(nf), status='old')

        DO i = 1, sizeArray
            READ(2, *) array_divisors(i)
        END DO 

   CLOSE(2)

   RETURN
END SUBROUTINE get_divisors