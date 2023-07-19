PROGRAM main
    INTEGER, PARAMETER :: longserie=12
    INTEGER :: i
    INTEGER, DIMENSION(12) :: array_fibo

    array_fibo(1) = 0
    array_fibo(2) = 1 
    fibonnaci: DO i=3, longserie
        array_fibo(i) = array_fibo(i - 1) + array_fibo(i - 2)
    END DO fibonnaci

    PRINT *, array_fibo
END PROGRAM


