PROGRAM main
    IMPLICIT NONE

    REAL, DIMENSION(5, 5)   :: matrix
    INTEGER, DIMENSION(25, 2)  :: coor
    CHARACTER(len=5)        :: condition 
    ! Defino funciones
    REAL                    :: get_value

    PRINT *, "Ingrese la condicion sobre la cual quiere encontrar coordenadas"
    PRINT *, "Las opciones son: igual, mayor, menor, par, impar"
    READ *, condition

    CALL clean_coor(coor)
    CALL fill_matrix(matrix)

    SELECT CASE (condition)
        CASE ("igual")
            CALL get_iguales(coor, matrix, get_value())
        CASE ("menor")
            CALL get_menores(coor, matrix, get_value())
        CASE ("mayor")
            CALL get_mayores(coor, matrix, get_value())
        CASE ("par")
            CALL get_parimpar(coor, matrix, .TRUE.)
        CASE ("impar")
            CALL get_parimpar(coor, matrix, .FALSE.)
        CASE DEFAULT
            PRINT *, "Error! Intentelo nuevamente"
    END SELECT
    CALL print_coor(coor, matrix)
END PROGRAM


SUBROUTINE print_coor(coor, matrix)
    REAL, DIMENSION(5, 5), INTENT(in)      :: matrix
    INTEGER, DIMENSION(25, 2), INTENT(in)  :: coor
    INTEGER                             :: i=1
    LOGICAL                             :: emptycoor=.FALSE.

    DO WHILE ((.NOT. emptycoor) .AND. (i <= 25))
        IF ( ( coor(i, 1)==0) .OR. ( coor(i, 2)==0 ) ) THEN
            emptycoor = .TRUE.
        ELSE
            PRINT *, i, ":", coor(i, 1:2), "=", matrix(coor(i, 1), coor(i, 2))
        END IF
        i = i + 1
    END DO
    PRINT *, "Total coordenadas: ", i-2
END SUBROUTINE print_coor

SUBROUTINE get_parimpar(coor, matrix, par)
    IMPLICIT NONE
    REAL, DIMENSION(5, 5), INTENT(in)      :: matrix
    INTEGER, DIMENSION(25, 2), INTENT(inout)  :: coor
    INTEGER                                :: row, col, i=1
    LOGICAL, INTENT(in)                    :: par

    rows: DO row=1, 5
        columns: DO col=1, 5
            IF ( (MOD(matrix(row, col), 2.)==0) .eqv. par ) THEN
                coor(i, 1) = row
                coor(i, 2) = col
                i = i + 1
            END IF
        END DO columns
    END DO rows
    RETURN
END SUBROUTINE get_parimpar

SUBROUTINE get_menores(coor, matrix, valor)
    IMPLICIT NONE
    REAL, DIMENSION(5, 5), INTENT(in)      :: matrix
    INTEGER, DIMENSION(25, 2), INTENT(inout)  :: coor
    REAL, INTENT(in)                       :: valor
    INTEGER                                :: row, col, i=1
    rows: DO row=1, 5
        columns: DO col=1, 5
            IF (matrix(row, col) < valor) THEN
                coor(i, 1) = row
                coor(i, 2) = col
                i = i + 1
            END IF
        END DO columns
    END DO rows
    RETURN
END SUBROUTINE get_menores

SUBROUTINE get_mayores(coor, matrix, valor)
    IMPLICIT NONE
    REAL, DIMENSION(5, 5), INTENT(in)      :: matrix
    INTEGER, DIMENSION(25, 2), INTENT(inout)  :: coor
    REAL, INTENT(in)                       :: valor
    INTEGER                                :: row, col, i=1
    rows: DO row=1, 5
        columns: DO col=1, 5
            IF (matrix(row, col) > valor) THEN
                coor(i, 1) = row
                coor(i, 2) = col
                i = i + 1
            END IF
        END DO columns
    END DO rows
    RETURN
END SUBROUTINE get_mayores

SUBROUTINE get_iguales(coor, matrix, valor)
    IMPLICIT NONE
    REAL, DIMENSION(5, 5), INTENT(in)      :: matrix
    INTEGER, DIMENSION(25, 2), INTENT(inout)  :: coor
    REAL, INTENT(in)                       :: valor
    INTEGER                                :: row, col, i=1
    rows: DO row=1, 5
        columns: DO col=1, 5
            IF (matrix(row, col) == valor) THEN
                coor(i, 1) = row
                coor(i, 2) = col
                i = i + 1
            END IF
        END DO columns
    END DO rows
    RETURN
END SUBROUTINE get_iguales

REAL FUNCTION get_value()
    IMPLICIT NONE
    PRINT *, "Ingrese el valor para la condicion: "
    READ "(F8.2)", get_value

    RETURN
END FUNCTION get_value

SUBROUTINE clean_coor(coor)
    IMPLICIT NONE
    INTEGER, DIMENSION(25, 2), INTENT(out)  :: coor
    INTEGER                              :: i
    DO i=1, 25
        coor(i, 1) = 0
        coor(i, 2) = 0
    END DO
END SUBROUTINE clean_coor


SUBROUTINE fill_matrix(matrix)
    IMPLICIT NONE
    REAL, DIMENSION(5, 5), INTENT(INOUT) :: matrix 
    INTEGER :: i, j

    filas:DO i=1, 5
        columnas:DO j=1, 5
            matrix(i, j) = (i - 1) * 5 + j
        END DO columnas 
    END DO filas

    RETURN
END SUBROUTINE fill_matrix