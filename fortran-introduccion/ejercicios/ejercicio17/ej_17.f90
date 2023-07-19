PROGRAM main
    IMPLICIT NONE

    ! Declaro Matrices/Arrays
    REAL, DIMENSION(10, 10) :: matrix
    REAL, DIMENSION(10) :: diag 
    ! funciones a usar
    REAL :: stdmatrix, mean_pair_matrix, diff_updown

    CALL fill_matrix(matrix)
    CALL print_matrix(matrix)
    CALL diagonal(diag, matrix)
    PRINT "(A9, 10(F6.2, 1x))", "diagonal: ", diag
    PRINT *, "El valor medio es: ", SUM(matrix)/100
    PRINT *, "Su desviacion estandar es: ", stdmatrix(matrix)
    CALL mean_rows(matrix)
    CALL mean_columns(matrix)
    PRINT *, "Valor medio en posiciones pares: ", mean_pair_matrix(matrix) 
    PRINT *, "Diagonal inferior - Diagonal superior: ", diff_updown(matrix)

END PROGRAM main

REAL FUNCTION diff_updown(matrix)
    IMPLICIT NONE
    REAL, DIMENSION(10, 10), INTENT(IN) :: matrix
    INTEGER :: i, j
    REAL :: sup=0., inf=0.
    rows: DO i=1, 10
        columns: DO j=1, 10
            IF (i < j) THEN 
                sup = sup + matrix(i, j)
            ELSE IF (i > J) THEN
                inf = inf + matrix(i, j)
            END IF
        END DO columns
    END DO rows

    diff_updown = inf - sup
    RETURN
END FUNCTION diff_updown

REAL FUNCTION mean_pair_matrix(matrix)
    IMPLICIT NONE
    REAL, DIMENSION(10, 10), INTENT(IN) :: matrix
    
    INTEGER :: i, j, pares=1
    REAL :: aux=0
  
    rows:DO i=0, 10
        columns:DO j=1, 10
            IF (MOD(i+j, 2)==0.) THEN
                aux = aux + matrix(i, j)
                pares = pares + 1   
            END IF
        END DO columns
    END DO rows
    mean_pair_matrix = aux/pares
    RETURN 
END FUNCTION mean_pair_matrix




SUBROUTINE mean_rows(matrix)
    IMPLICIT NONE
    REAL, DIMENSION(10, 10), INTENT(IN) :: matrix
    INTEGER :: i
    ! funciones a usar
    REAL :: meanarray
    PRINT *, "Valor medio filas"
    DO i=1, 10
        PRINT *, meanarray(matrix(i, 1:10))
    END DO
END SUBROUTINE mean_rows

SUBROUTINE mean_columns(matrix)
    IMPLICIT NONE
    REAL, DIMENSION(10, 10), INTENT(IN) :: matrix
    INTEGER :: j
    ! funciones a usar
    REAL :: meanarray
    PRINT *, "Valor medio columnas"
    DO j=1, 10
        PRINT *, meanarray(matrix(1:10, j))
    END DO
END SUBROUTINE mean_columns


REAl FUNCTION meanarray(array)
    IMPLICIT NONE
    REAL, DIMENSION(10), INTENT(IN) :: array
    meanarray = SUM(array)/10
    RETURN
END FUNCTION meanarray

REAL FUNCTION stdmatrix(matrix)
    IMPLICIT NONE
    ! Variables externas
    REAL, DIMENSION(10, 10), INTENT(IN) :: matrix
    ! Variables internas
    REAL :: mean, std
    INTEGER :: i,j
    std = 0
    mean = SUM(matrix)/100

    filas:DO i=1, 10
        columnas:DO j=1, 10
            std = std + (mean - matrix(i, j))**2
            END DO columnas 
    END DO filas
    stdmatrix = SQRT(std/100)

END FUNCTION stdmatrix

SUBROUTINE diagonal(array, matrix)
    IMPLICIT NONE
    REAL, DIMENSION(10), INTENT(OUT) ::array
    REAL, DIMENSION(10, 10), INTENT(IN) :: matrix
    
    INTEGER :: i

    DO i=1, 10
        array(i) = matrix(i, i)
    END DO

    RETURN
END SUBROUTINE 

SUBROUTINE print_matrix(matrix)
    IMPLICIT NONE
    REAL, DIMENSION(10, 10), INTENT(IN) :: matrix
    INTEGER :: i
    DO i=1, 10
        PRINT "(10(F6.2, 1x))", (matrix(i, 1:10))
    END DO
END SUBROUTINE print_matrix


SUBROUTINE fill_matrix(matrix)
    IMPLICIT NONE
    REAL, DIMENSION(10, 10), INTENT(INOUT) :: matrix 
    INTEGER :: i, j

    filas:DO i=1, 10
        columnas:DO j=1, 10
            matrix(i, j) = (i - 1) * 10 + j
        END DO columnas 
    END DO filas

    RETURN
END SUBROUTINE