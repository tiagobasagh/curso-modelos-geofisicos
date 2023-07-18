PROGRAM circunferencia
    ! Programa que dada un valor (real) de radio calcula area y volumen del circulo
    IMPLICIT NONE

    REAL :: r_radio, superficie_circulo, volumen_circulo
    r_radio = 0

    PRINT *, "Ingrese el radio (valor real): "
    READ *, r_radio
    PRINT *, superficie_circulo(r_radio)
    PRINT *, volumen_circulo(r_radio)

END PROGRAM circunferencia

REAL FUNCTION volumen_circulo(radio)
! Funcion para calcular el volumen de un circulo dado su radio
    IMPLICIT NONE

    REAL, INTENT(in):: radio

    volumen_circulo = radio**2 * ACOS(-1.)

    RETURN
END FUNCTION volumen_circulo

REAL FUNCTION superficie_circulo(radio)
! Funcion para calcular la superficie de un circulo dado su radio
    IMPLICIT NONE

    REAL, INTENT(in):: radio

    superficie_circulo = radio**2 * ACOS(-1.)

    RETURN
END FUNCTION superficie_circulo