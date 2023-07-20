PROGRAM main
    IMPLICIT NONE
    ! Declaro variables
    REAL, DIMENSION(2)       :: r, v
    REAL                     :: dt
    INTEGER                  :: t
    ! Parametros 
    INTEGER, PARAMETER       :: salvaguarda = 50
    r = [0., 30.]
    v = [5., 2.]
    dt = 0.1
    t = 0
    
    OPEN(UNIT=11, FILE='parabola.csv', STATUS='new')
    
    parabolico:  DO WHILE ( (r(2) > 0) .AND. (t < salvaguarda) )
        CALL posicion(r, v, dt)
        CALL velocidad(v, dt)
        t = t + 1
        WRITE(11, "(5(F8.2, 2x))") t * dt, r, v
    END DO parabolico

    CLOSE(UNIT=11)

    IF (t >= salvaguarda) PRINT *, "Muchos pasos! tal vez se escapo de la tierra!"

END PROGRAM main


SUBROUTINE velocidad(v, dt)
    IMPLICIT NONE
    ! Defino constantes que usa esta funcion
    REAL, DIMENSION(2), PARAMETER      :: g = [0., -9.81]
    ! Defino variables externas in/out
    REAL, DIMENSION(2), INTENT(inout)  :: v
    REAL, INTENT(in)                   :: dt  
    ! Defino variables locales
    INTEGER                            :: i
    loopdiemnsion: DO i=1,2
        v(i) = v(i) + g(i) * dt
    END DO loopdiemnsion

    RETURN 
END SUBROUTINE

SUBROUTINE posicion(r, v, dt)
    IMPLICIT NONE
    ! Defino variables externas in/out
    REAL, DIMENSION(2), INTENT(in)     :: v
    REAL, INTENT(in)                   :: dt
    REAL, DIMENSION(2), INTENT(inout)  :: r
      
    ! Defino variables locales
    INTEGER                            :: i
    loopdiemnsion: DO i=1,2
        r(i) = r(i) + v(i) * dt
    END DO loopdiemnsion

    RETURN  
END SUBROUTINE