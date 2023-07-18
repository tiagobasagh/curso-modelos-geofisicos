PROGRAM ejemplo4
    IMPLICIT NONE
    
    INTEGER :: valorI, i, j
    REAL :: valorR
    INTEGER, DIMENSION(3,3) :: matriz1
    
    PRINT *,'Dame un valor entero'
    READ *, valorI
    PRINT '(I3)',valorI
    
    PRINT *,'Dame un valor Real'
    READ *, valorR
    PRINT '(A12, F5.3, 1X, A10, F5.1)','3 decimales:', valorR, '1 decimal:', valorR
    
    matriz1 = valorI
    DO i=1, 3
        PRINT '(3(I3,1x))',(matriz1(i,j), j=1, 3)
    END DO

END PROGRAM ejemplo4