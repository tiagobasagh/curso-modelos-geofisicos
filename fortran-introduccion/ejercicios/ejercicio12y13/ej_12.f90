PROGRAM main
    IMPLICIT NONE
    ! Variables del programa
    INTEGER :: N=1, total=100
    ! Funciones a usar
    LOGICAL :: is_primo

    OPEN(UNIT=9, FILE='listprimos.dat', STATUS='new')
        
        loopprimos: DO N=1, total
            IF (is_primo(N)) THEN
                WRITE(9, *) N
            END IF
        END DO loopprimos 
    
    CLOSE(UNIT=9)
    
END PROGRAM main

LOGICAL FUNCTION is_primo(n)
    ! Defino variables externas de la funcion
    INTEGER, INTENT(in) :: n
    ! Doy un valor inicial a la funcion
    is_primo = .TRUE.
    
    
    loopnumber: DO i=2, INT(n/2)
        IF (MOD(n, i) == 0) is_primo = .FALSE.
    END DO loopnumber

END FUNCTION is_primo