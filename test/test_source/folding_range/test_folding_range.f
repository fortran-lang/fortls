C SHORT COMMENT
      PROGRAM test_folding_range_fixed_form
         DO I=1,5
            DO 100 J=1,5
               PRINT *, N
               IF (N == M) THEN
                  PRINT *, N
               END IF
  100       CONTINUE

            IF (N == M) THEN
               PRINT *, N
            ELSE
     *   IF (N == M)
     *   THEN

C BLOCK COMMENT
C BLOCK COMMENT
C BLOCK COMMENT
               PRINT *, N
            ELSE
               PRINT *, N
            END IF
         END DO
      END PROGRAM
