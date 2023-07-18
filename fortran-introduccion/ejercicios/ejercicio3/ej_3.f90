PROGRAM faranheit
    ! Programa para dada una temperatura en C, devuelve en K
    IMPLICIT NONE 
    
    REAL :: r_temp, celciustofaranheit
    r_temp = 0

    PRINT *, "Ingrese un valor de temperatura (◦C): "
    READ *, r_temp
    PRINT *, celciustofaranheit(r_temp)

END PROGRAM faranheit

REAL FUNCTION celciustofaranheit(temp)
    ! Funcion que transforma celcius a kelvin
    REAL, INTENT(in) :: temp

    celciustofaranheit = 32 + temp * 180/100
    
    RETURN
END FUNCTION celciustofaranheit