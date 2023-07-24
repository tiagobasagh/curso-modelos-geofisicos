MODULE encripttools
    USE dcaoserie, ONLY: get_dcao_number
    
    IMPLICIT NONE

    PUBLIC :: encript_msg
    PRIVATE :: encript_letter, encript_seed

CONTAINS

    CHARACTER(len=1) FUNCTION encript_letter(letter, encriptPar, encriptImpar)
        IMPLICIT NONE
        ! Variables IN/OUT
        CHARACTER(len=1), INTENT(in)                   :: letter
        INTEGER, INTENT(in)                            :: encriptPar, encriptImpar
        ! Variables locales
        CHARACTER(len=1)                               :: el
        INTEGER                                        :: ic
        
        ic = ICHAR(letter)
        el = ""
        IF ( MOD(ic, 2) == 0) THEN
            el = CHAR(ic + encriptPar)
        ELSE 
            el = CHAR(ic - encriptImpar)
        END IF

        encript_letter = el

        RETURN
    END FUNCTION encript_letter

    CHARACTER(len=2) FUNCTION encript_seed(encriptPar, encriptImpar)
        
        USE dcaoserie, ONLY: get_dcao_number
        
        IMPLICIT NONE
        INTEGER, INTENT(in)           :: encriptPar, encriptImpar
        
        encript_seed(1:1) = CHAR(encriptPar + get_dcao_number(2)) 
        encript_seed(2:2) = CHAR(encriptImpar + get_dcao_number(14))

    RETURN
    END FUNCTION encript_seed

    SUBROUTINE encript_msg(msg, sm, encmsg)
        IMPLICIT NONE
        ! Variables in/out
        INTEGER, INTENT(in)                                ::  sm
        CHARACTER(len=20), DIMENSION(sm), INTENT(IN)       :: msg
        CHARACTER(len=20), DIMENSION(sm + 2), INTENT(out)  :: encmsg
        ! Varibles locales
        INTEGER                                            :: encriptPar=-1, encriptImpar=-1
        INTEGER                                            :: i, j
        CHARACTER(len=20)                                  :: word="", encword=""
        CHARACTER(len=1)                                   :: auxletter=""

        encmsg(1) = encript_seed(encriptPar, encriptImpar)
        loopmsg:DO i=1, sm
            word = msg(i)
            encword = msg(i)
            loopword:DO j=1, LEN_TRIM(word)
                
                auxletter = encript_letter(word(j:j), encriptPar, encriptImpar)
                IF (auxletter == "") THEN
                    PRINT *, "Impossible to encript the letter"
                ELSE
                    encword(j:j) = auxletter
                END IF
            END DO loopword

            encmsg(i + 1) = encword
        END DO loopmsg

        RETURN
    END SUBROUTINE encript_msg

END MODULE encripttools