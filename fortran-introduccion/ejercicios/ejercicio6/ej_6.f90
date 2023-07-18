PROGRAM toCartesian
    ! Dado coordenadas en esfericas las transforma en esfericas
    IMPLICIT NONE
    REAL :: r_theta, r_phi, r_radio, r_x,r_y, r_z
    r_theta = 0 
    r_phi = 0 
    r_radio = 0
    r_x = 0
    r_y = 0
    r_z = 0

    PRINT *, "Ingrese sus coordeandas esferias"
    PRINT *, "theta: "
    READ *, r_theta
    PRINT *, "phi: "
    READ *, r_phi
    PRINT *, "r o radio: "
    READ *, r_radio

    CALL to_cartesian(r_theta, r_phi, r_radio, r_x, r_y, r_z)
    PRINT *, " x: ", r_x, " y: ", r_y, " z: ", r_z 

END PROGRAM toCartesian

SUBROUTINE to_cartesian(theta, phi, r, x, y, z)
    IMPLICIT NONE

    REAL, INTENT(in) :: theta, phi, r
    REAL, INTENT(out) :: x, y, z

    x = r * SIN(theta) * COS (phi)
    y = r * SIN(theta) * COS(phi)
    z = r * COS(phi)

    RETURN
END SUBROUTINE to_cartesian