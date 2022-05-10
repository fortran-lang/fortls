PROGRAM test_associate_block
    IMPLICIT NONE
    REAL :: A(5), B(5,5), C, III = 1
    ASSOCIATE (X => A, Y => C)
        PRINT*, X, Y, III
    END ASSOCIATE
    ASSOCIATE (X => 1)
        PRINT*, X
    END ASSOCIATE
    ASSOCIATE (ARRAY => B(:,1))
        ARRAY (3) = ARRAY (1) + ARRAY (2)
    END ASSOCIATE
END PROGRAM test_associate_block
