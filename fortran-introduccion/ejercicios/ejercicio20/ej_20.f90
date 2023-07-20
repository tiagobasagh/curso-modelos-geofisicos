PROGRAM main
    IMPLICIT NONE
    ! Declaro variables
    INTEGER                          :: rows, columns
    CHARACTER(len=50)                :: nf
    ! Declaro arrays sin dimensiones
    REAL, DIMENSION(:, :), ALLOCATABLE           :: matrix
    
    CALL init_values(nf, rows, columns)
    
    IF (ALLOCATED(matrix)) DEALLOCATE(matrix)
    ALLOCATE(matrix(rows, columns))
    
    CALL load_file(matrix, nf, rows, columns)
    CALL print_matrix(matrix, rows, columns)
    
    DEALLOCATE(matrix)

END PROGRAM main

SUBROUTINE print_matrix(matrix, rows, columns)
    IMPLICIT NONE
    INTEGER, INTENT(in)                         :: rows, columns
    REAL, DIMENSION(rows, columns), INTENT(in)  :: matrix
    INTEGER                                     :: i

    DO i=1, rows
        PRINT *, matrix(i, 1:columns)
    END DO

    PRINT *, "Su valor maximo es: ", MAXVAL(matrix)
    PRINT *, "Su valor minimo es: ", MINVAL(matrix)
    
END SUBROUTINE

SUBROUTINE load_file(matrix, nf, rows, columns)
    IMPLICIT NONE
    INTEGER, PARAMETER                             :: file_number=19
    INTEGER, INTENT(in)                            :: rows, columns
    CHARACTER(len=50), INTENT(in)                  :: nf
    REAL, DIMENSION(rows, columns), INTENT(inout)  :: matrix

    INTEGER                                        :: i
    OPEN(UNIT=file_number, FILE=nf, STATUS='old')
    DO i=1, rows    
        READ (file_number, *, END=77) matrix(i, 1:columns)
    END DO
    77 CONTINUE
    CLOSE(file_number)

    RETURN
END SUBROUTINE load_file

SUBROUTINE init_values(nf, rows, columns)
    IMPLICIT NONE
    INTEGER                :: rows, columns
    CHARACTER(len=50)      :: nf

    PRINT *, "Ingese el nombre del archivo: "
    READ *, nf

    PRINT *, "Ingrese el numero de filas de la matriz"
    READ *, rows

    PRINT *, "Ingrese el numero ode columnas de la matriz"
    READ *, columns

    RETURN
END SUBROUTINE init_values

