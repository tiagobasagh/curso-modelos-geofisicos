PROGRAM bievenida
    ! Programa para dar la bienvenida a la materia
    IMPLICIT NONE 
    
    CHARACTER(len=15)    :: str_name
    
    PRINT *, "Como te llamas?"
    READ *, str_name
    PRINT *, "Bienvenido a Frotran ", TRIM(str_name), "!!"

END PROGRAM bievenida
