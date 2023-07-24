MODULE mathtools
    IMPLICIT NONE

    PUBLIC :: generate_random_int

CONTAINS
    
    SUBROUTINE generate_random_int(v, minvalue, maxvalue)
        IMPLICIT NONE
        ! Variables in/out
        INTEGER, INTENT(in)     :: maxvalue,  minvalue
        INTEGER, INTENT(out)    :: v
        ! Varibles locales
        REAL                    :: r

        CALL RANDOM_NUMBER(r)
        v = INT(minvalue + (maxvalue - minvalue) * r)

        RETURN
    END SUBROUTINE generate_random_int

END MODULE mathtools