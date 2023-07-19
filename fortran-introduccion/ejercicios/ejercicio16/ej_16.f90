PROGRAM main
    !!Resolucion ejercicio 16: Serie DCAO
    IMPLICIT NONE
    ! declaro variables
    INTEGER :: N=0, step=0, dn=0
    INTEGER, DIMENSION(4) :: array=[0, 1, 0, 1]
    ! declaro funciones a usar
    INTEGER :: dcaonumber

    PRINT *, "Cuantos numero de la serie DCAO se quieren?"
    READ *, N
    PRINT *, "Sus numeros son: "
 
    DO step=1, N
        IF (step <=4) THEN
            PRINT *, array(step)
        ELSE
            dn = dcaonumber(array)
            PRINT *, dn 
            CALL next_dcao(array, dn)
        END IF
    END DO

END PROGRAM main

SUBROUTINE next_dcao(array, dn)
    !!Rutina que mueve el array al siguiente valor dcao(dn)
    IMPLICIT NONE
    ! Declaro variables in/out
    INTEGER, DIMENSION(4), INTENT(inout) :: array
    INTEGER, INTENT(IN) :: dn
    ! Declaro variables internas
    INTEGER :: i

    DO i=2, 4
        array(i-1) = array(i)
    END DO 
    array(4) = dn
    RETURN
END SUBROUTINE next_dcao

INTEGER FUNCTION dcaonumber(array)
    !!Funcion que calcula el numero DCAO
    ! dado un array de 4 posiciones
    IMPLICIT NONE
    ! Declaro variables in/out
    INTEGER, DIMENSION(4), INTENT(inout) :: array

    dcaonumber = -array(1) - array(2) + array(3) + array(4)
END FUNCTION dcaonumber