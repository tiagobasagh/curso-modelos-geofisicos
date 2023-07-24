PROGRAM main
    
    USE encripttools, ONLY: encript_msg
    USE msgtools, ONLY: size_msg, load_msg, save_msg
    USE mathtools, ONLY: generate_random_int
    USE dcaoserie, ONLY: get_dcao_number

    IMPLICIT NONE
    ! Variables para obtener argumentos
    CHARACTER(LEN=500), ALLOCATABLE, DIMENSION(:) :: args
    INTEGER                                       :: narg
    ! Defino paremetros del programa
    CHARACTER(len=50)                              :: dir_msg="msg.txt"
    CHARACTER(len=50)                              :: dir_encmsg="msg.enc"
    LOGICAL                                        :: errarg
    ! Defino variables del programa 
    INTEGER                                       :: sm
    CHARACTER(len=20), DIMENSION(:), ALLOCATABLE  :: msg, encmsg

    errarg =.FALSE.
    narg=COMMAND_ARGUMENT_COUNT()
    IF (ALLOCATED(args)) DEALLOCATE(args)
    ALLOCATE(args(narg))
    
    CALL get_args(args, narg, errarg, dir_msg, dir_encmsg)

    IF (.NOT. errarg) THEN
        sm = size_msg(dir_msg)
        IF (.NOT. (sm == -1)) THEN
            IF (ALLOCATED(msg)) DEALLOCATE(msg)
            ALLOCATE(msg(sm))

            IF (ALLOCATED(encmsg)) DEALLOCATE(encmsg)
            ALLOCATE(encmsg(sm + 1))

            CALL load_msg(msg, sm, dir_msg)
            CALL encript_msg(msg, sm, encmsg)
            CALL save_msg(encmsg, sm, dir_encmsg)
            
        END IF
    END IF

END PROGRAM main


SUBROUTINE get_args(args, narg, errarg, dir_msg, dir_encmsg)
    IMPLICIT NONE
    ! Input/Output variables
    INTEGER, INTENT(in)                              :: narg
    CHARACTER(LEN=500), INTENT(in), DIMENSION(narg)  :: args
    LOGICAL, INTENT(inout)                           :: errarg
    CHARACTER(len=50), INTENT(inout)                 :: dir_msg
    CHARACTER(len=50), INTENT(inout)                 :: dir_encmsg
    ! Local variables
    CHARACTER(LEN=15)                                :: arg
    INTEGER                                          :: iarg
    

    DO iarg=1, narg
        CALL GETARG(iarg, args(iarg))
    END DO
    
    iarg = 1
    errarg =.FALSE.
    arg = TRIM(args(iarg))

    IF ((LEN_TRIM(arg) == 2 ) .AND. (arg == "-h"))THEN
        errarg =.TRUE.
        CALL help()
    ELSE
        DO WHILE((.NOT. errarg) .AND.(iarg<=narg))
            arg = TRIM(args(iarg))
            IF (LEN_TRIM(arg) == 2) THEN
                SELECT CASE (arg)
                    CASE ("-i")
                        CALL arg_dir(args, iarg, narg, errarg, dir_msg) 
                    CASE ("-o")
                        CALL arg_dir(args, iarg, narg, errarg, dir_encmsg) 
                    CASE DEFAULT
                        CALL err_cmd(errarg)
                END SELECT
            ELSE
                CALL err_cmd(errarg)
            END IF
            iarg = iarg + 2
        END DO
    END IF

RETURN
END SUBROUTINE

SUBROUTINE arg_dir(args, iarg, narg, errarg, dir_file)
    IMPLICIT NONE
    CHARACTER(LEN=500), INTENT(in), DIMENSION(narg)  :: args
    INTEGER, INTENT(in)                              :: narg, iarg
    LOGICAL, INTENT(inout)                           :: errarg
    CHARACTER(len=50), INTENT(inout)                 :: dir_file

    IF ( (iarg+1) > narg ) THEN
        errarg = .TRUE.
        PRINT *, "Add the name file!"
    ELSE
        dir_file = TRIM(args(iarg+1))
    END IF

    RETURN
END SUBROUTINE arg_dir

SUBROUTINE help()
    IMPLICIT NONE
    PRINT *, "AYUDITAAAA!!!!"
END SUBROUTINE

SUBROUTINE err_cmd(errarg)
    IMPLICIT NONE
    LOGICAL, INTENT(inout)      :: errarg

    errarg = .TRUE.
    PRINT *, "It's a not valid command"
    PRINT *, "add -h to help"
    RETURN
END SUBROUTINE err_cmd