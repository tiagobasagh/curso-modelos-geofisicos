
MODULE dcaoserie
    IMPLICIT NONE

    PUBLIC  :: get_dcao_number
    PRIVATE :: next_array, formule

CONTAINS

    INTEGER FUNCTION get_dcao_number(n)
        IMPLICIT NONE
        ! Varibles in/out
        INTEGER, INTENT(in)      :: n
        ! Variables locales
        INTEGER, DIMENSION(4)    :: array=[0, 1, 0, 1]
        INTEGER                  :: i, dn=0

        DO i=1, n
            IF (i <=4) THEN
                dn = array(i)
            ELSE
                dn = formule(array)
                CALL next_array(array, dn)
            END IF
        END DO

        get_dcao_number = dn
        RETURN
    END FUNCTION get_dcao_number
    
    SUBROUTINE next_array(array, dn)
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
    END SUBROUTINE next_array

    INTEGER FUNCTION formule(array)
        !!Funcion que calcula el numero DCAO
        ! dado un array de 4 posiciones
        IMPLICIT NONE
        ! Declaro variables in/out
        INTEGER, DIMENSION(4), INTENT(inout) :: array

        formule = -array(1) - array(2) + array(3) + array(4)
    END FUNCTION formule

END MODULE dcaoserie