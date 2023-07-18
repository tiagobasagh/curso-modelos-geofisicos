PROGRAM main
    IMPLICIT NONE 
    
    REAL :: in_number
    REAL, DIMENSION(2) :: vector
    vector(1) = in_number()
    vector(2) = in_number()
    PRINT *, vector

END PROGRAM main

REAL FUNCTION in_number()
    in_number = 0

    PRINT *, "Ingrese un numero: "
    READ *, in_number
    ! Agregar checkeo de si es un numero entero
    RETURN
END FUNCTION in_number

