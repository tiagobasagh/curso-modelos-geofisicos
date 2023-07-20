PROGRAM main
    IMPLICIT NONE

    TYPE gridheader
        INTEGER             :: year, month, day, hour, minute, second
        INTEGER             :: varcode, varlevel, level, dimx, dimy
        REAL                :: ms
        CHARACTER(len=10)   :: unit
    END TYPE gridheader

    ! Defino variables y parametros
    TYPE(gridheader)                               :: header
    REAL, DIMENSION(:, :), ALLOCATABLE :: matrix
    INTEGER, PARAMETER                             :: file_number=151, eof=3
    CHARACTER(len=20), PARAMETER                   :: nf="variables_grib.dat"
    INTEGER                                        :: ixf=1
    ! Defino funciones a usar
    CHARACTER(len=10)                              :: get_name, get_type_level

    OPEN(UNIT=file_number, FILE=nf, STATUS='old')
    DO ixf=1, eof
        ! Levanto el header del archivo
        READ (file_number, *, END=77) header%year, header%month, header%day, &
                                      header%hour, header%minute, header%second, &
                                      header%ms, header%varcode, header%varlevel, &
                                      header%level, header%dimx, header%dimy, header%unit
        ! Defino el size de la matriz
        IF (ALLOCATED(matrix)) DEALLOCATE(matrix)
        ALLOCATE(matrix(header%dimx, header%dimy))

        ! levanto la matriz
        READ (file_number, *, END=77) matrix(1: header%dimx, 1:header%dimy)

        ! Hago cosisas conn la matriz
        PRINT *, "#######################################"
        PRINT *, "El nombre de la variable es: ", get_name(header%varcode)
        PRINT *, "El tipo de nivel es: ", get_type_level(header%varlevel)
        PRINT *, "Su valor maximo es: ", MAXVAL(matrix) 
        PRINT *, "Su valor minimo es: ", MINVAL(matrix)
        PRINT *, "Su valor medio es: ", SUM(matrix)/(header%dimx * header%dimy)
        ! libero la matriz
        DEALLOCATE(matrix)
    END DO 
        77 CONTINUE
    CLOSE(file_number)

END PROGRAM main


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


CHARACTER(len=10) FUNCTION get_name(code)
    INTEGER, INTENT(in)          :: code
    INTEGER, PARAMETER           :: eof=22, file_number=33

    INTEGER                      :: varcode, i=1
    CHARACTER(len=10)            :: name
    CHARACTER(len=50)            :: description
    LOGICAL                      :: in_file
    
    in_file=.FALSE.
    i=1
    name="Unkown"

    OPEN(UNIT=file_number, FILE="variables.dat", STATUS='old')
    DO WHILE ((.NOT. in_file) .AND. (i < eof))
        READ (file_number, *, END=111) varcode, name, description
        IF (code == varcode) in_file = .TRUE.    
        i = i + 1
    END DO
    111 CONTINUE
    CLOSE(file_number)
    
    get_name = name
    
    RETURN

END FUNCTION get_name


CHARACTER(len=10) FUNCTION get_type_level(level)
    IMPLICIT NONE
    INTEGER, INTENT(in)         :: level
    CHARACTER(len=10)           :: tlevel

    tlevel = "unknow"
    SELECT CASE (level)
        CASE (1)
            tlevel="surface"
        CASE (3)
            tlevel="pressure"
        CASE (4)
            tlevel="depth"
        CASE (5)
            tlevel="eta"
    END SELECT

    get_type_level=tlevel
    RETURN 
END FUNCTION get_type_level
