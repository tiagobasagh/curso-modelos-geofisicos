PROGRAM main
    IMPLICIT NONE
    
    ! Parametros del problema
    INTEGER, PARAMETER    :: file_number=12
    REAL, PARAMETER       :: m=5
    ! Funcion del problema
    REAL                  :: energia_cinetica, energia_potencial
    ! Variables del problema
    REAL, DIMENSION(2)    :: x, v
    REAL                  :: t, ec, ep
    INTEGER               :: index
    OPEN(UNIT=file_number, FILE='parabola.csv', STATUS='old')
        DO index=1, 100
            READ (file_number, *, END=77) t, x(1), x(2), v(1), v(2)
            ec = energia_potencial(x(2), m)  
            ep = energia_cinetica(v, m)
            print *, ec+ep, ec, ep
        END DO
        77 CONTINUE
    CLOSE(file_number)

END PROGRAM main

REAL FUNCTION energia_cinetica(v, m)
    IMPLICIT NONE
    REAL, INTENT(in)                :: m
    REAL, INTENT(in), DIMENSION(2)  :: v

    energia_cinetica = m * DOT_PRODUCT(v, v) / 2

    RETURN
END FUNCTION energia_cinetica

REAL FUNCTION energia_potencial(h, m)
    IMPLICIT NONE
    REAL, PARAMETER    :: g=9.81
    REAL, INTENT(in)   :: h, m
    
    energia_potencial = h * m * g

    RETURN 
END FUNCTION energia_potencial