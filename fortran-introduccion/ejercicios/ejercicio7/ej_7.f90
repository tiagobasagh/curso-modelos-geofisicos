PROGRAM factorial
    IMPLICIT NONE
    REAL :: realfact
    INTEGER :: N, fact

    N = 0

    PRINT *, "Ingrese un numero entero: "
    READ *, N

    PRINT *, "Factorial resuelto en variable entera: ", fact(N)
    PRINT *, "Factorial resuelto en variable real ", realfact(N)

END PROGRAM factorial

INTEGER FUNCTION fact(N)
    ! funcion para calcular el factorial de un numero
    ! devuelve un valor entero
    IMPLICIT NONE

    INTEGER, INTENT(in) :: N 
    INTEGER :: i

    fact = 1
    loopnumber: DO i=1, N
        fact = fact * i
    END DO loopnumber

    RETURN 
END FUNCTION fact

REAL FUNCTION realfact(N)
    ! funcion para calcular el factorial de un numero
    ! devuelve un valor real
    IMPLICIT NONE

    INTEGER, INTENT(in) :: N 
    INTEGER :: i

    realfact = 1
    loopnumber: DO i=1, N
        realfact = realfact * i
    END DO loopnumber

    RETURN 
END FUNCTION realfact