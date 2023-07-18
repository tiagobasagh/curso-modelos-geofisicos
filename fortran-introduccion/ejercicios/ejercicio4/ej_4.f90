PROGRAM volumencubo
    ! Programa que dada la longitud de un arista, calcula el volumen del cubo
    IMPLICIT NONE

    REAL :: r_arista, volumen_cubo
    r_arista = 0

    PRINT *, "Ingrese la longitud de la arista (valor real): "
    READ *, r_arista
    PRINT *, volumen_cubo(r_arista)

END PROGRAM volumencubo

REAL FUNCTION volumen_cubo(arista)
! Funcion para calcular el volumen de un cubo dada la longitud de una
    IMPLICIT NONE

    REAL, INTENT(in):: arista

    volumen_cubo = arista**3

    RETURN
END FUNCTION volumen_cubo