PROGRAM main
    character :: c
    character (len=2) :: tempH
    INTEGER :: num
    write(*,*) 'Enter a character'
    read(*,*) c
    write(tempH,'(Z2)') c
    read (tempH, *) num 
    IF (num <= 9) THEN
        PRINT *, num - 30
    ELSE 
        PRINT *, num - 31
    END IF 
END PROGRAM main