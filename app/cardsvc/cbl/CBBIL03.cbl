      ******************************************************************
      * CBBIL03 - CARDBILL STEP 03 - MINIMUM PAYMENT AND DUE DATE      *
      *                                                                *
      * THE MINIMUM PAYMENT IS THE GREATER OF A PERCENTAGE OF THE      *
      * CLOSING BALANCE AND A FLOOR AMOUNT, PLUS ANY ARREARS CARRIED   *
      * FROM EARLIER CYCLES AND ANY AMOUNT OVER THE CREDIT LIMIT.  IT  *
      * IS NEVER MORE THAN THE CLOSING BALANCE.                        *
      *                                                                *
      * THE DUE DATE IS THE CYCLE DATE PLUS THE GRACE DAYS ON THE      *
      * CONTROL CARD, ROLLED FORWARD OFF A WEEKEND OR A HOLIDAY GIVEN  *
      * ON A HOLIDAY CARD.                                             *
      *                                                                *
      * CALLED BY   - JOB CBBIL03J, STEP MINPAY                        *
      * CALLS       - CBCRD91 (FATAL ERROR / ABEND HANDLER)            *
      * READS       - STMTWRK, CARDSVC.ACCOUNT                         *
      * UPDATES     - CARDSVC.ACCOUNT (MIN_PAY_DUE, PAY_DUE_DATE,      *
      *               STMT_BAL, LAST_CYCLE_DATE, NEXT_CYCLE_DATE)      *
      * WRITES      - STMTWK2 (STMT-RECORD, VB 24208)                  *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - ALL ACCOUNTS UPDATED                                    *
      *   04 - ONE OR MORE ACCOUNTS NOT UPDATED - SEE SYSOUT           *
      *   12 - FATAL                                                   *
      * ABEND U0803 - UNRECOVERABLE FILE OR SQL CONDITION              *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBBIL03.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STMTWRK-FILE ASSIGN TO STMTWRK
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTWRK-STATUS.
      *
           SELECT STMTWK2-FILE ASSIGN TO STMTWK2
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTWK2-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  STMTWRK-FILE
           RECORD IS VARYING IN SIZE FROM 284 TO 24204 CHARACTERS
               DEPENDING ON WS-IN-LEN
           BLOCK CONTAINS 0 RECORDS.
       01  STMTWRK-REC                     PIC X(24204).
      *
       FD  STMTWK2-FILE
           RECORD IS VARYING IN SIZE FROM 284 TO 24204 CHARACTERS
               DEPENDING ON WS-OUT-LEN
           BLOCK CONTAINS 0 RECORDS.
       01  STMTWK2-REC                     PIC X(24204).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBBIL03 '.
      *
       01  WS-FILE-STATUS-AREA.
           05  WS-STMTWRK-STATUS           PIC X(2)  VALUE '00'.
               88  WS-STMTWRK-OK                     VALUE '00'.
           05  WS-STMTWK2-STATUS           PIC X(2)  VALUE '00'.
               88  WS-STMTWK2-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-SYSIN-EOF-SW             PIC X     VALUE 'N'.
               88  WS-SYSIN-EOF                      VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
           05  WS-WARNING-SW               PIC X     VALUE 'N'.
               88  WS-WARNING                        VALUE 'Y'.
           05  WS-ROLL-SW                  PIC X     VALUE 'N'.
               88  WS-ROLL-NEEDED                    VALUE 'Y'.
      *
      ******************************************************************
      * CONTROL CARDS                                                  *
      *                                                                *
      *   CYCLE-DATE   CCYYMMDD                                        *
      *   CYCLE-ID     CCCCCCCC                                        *
      *   MIN-PAY-PCT  NN.NNNNN   PERCENT OF CLOSING BALANCE           *
      *   MIN-PAY-FLR  NNNNN.NN   FLOOR AMOUNT                         *
      *   GRACE-DAYS   NN                                              *
      *   COMMIT-FREQ  NNNNNN                                          *
      *   HOLIDAY      CCYYMMDD   REPEATABLE, UP TO 30                 *
      ******************************************************************
       01  WS-CONTROL-CARDS.
           05  WS-CYCLE-DATE               PIC 9(8)  VALUE ZERO.
           05  WS-CYCLE-ID                 PIC X(8)  VALUE SPACES.
           05  WS-MIN-PAY-PCT              PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-MIN-PAY-FLOOR            PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-GRACE-DAYS               PIC 9(2)  VALUE ZERO.
           05  WS-COMMIT-FREQ              PIC 9(6)  VALUE ZERO.
      *
       01  WS-HOLIDAY-TABLE.
           05  WS-HOLIDAY-CNT              PIC 9(2)  VALUE ZERO.
           05  WS-HOLIDAY OCCURS 30 TIMES
                         INDEXED BY WS-HOL-IDX       PIC 9(8).
      *
       01  WS-CARD-IMAGE.
           05  WS-CARD-KEYWORD             PIC X(12).
           05  FILLER                      PIC X.
           05  WS-CARD-VALUE               PIC X(20).
           05  FILLER                      PIC X(47).
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-UPDATE-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-NOTFND-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-ARREARS-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-FULLBAL-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
      *
       01  WS-IN-LEN                       PIC S9(8) COMP VALUE 284.
       01  WS-OUT-LEN                      PIC S9(8) COMP VALUE 284.
      *
       01  WS-CALC-AREA.
           05  WS-PCT-AMT                  PIC S9(9)V99 COMP-3.
           05  WS-MIN-PAY                  PIC S9(9)V99 COMP-3.
           05  WS-ARREARS                  PIC S9(9)V99 COMP-3.
           05  WS-OVER-LIMIT               PIC S9(9)V99 COMP-3.
           05  WS-TOTAL-MIN-PAY            PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-DATE-AREA.
           05  WS-DUE-DATE                 PIC 9(8)  VALUE ZERO.
           05  WS-NEXT-CYCLE-DT            PIC 9(8)  VALUE ZERO.
           05  WS-INT-DATE                 PIC S9(9) COMP VALUE ZERO.
           05  WS-DAY-OF-WEEK              PIC S9(4) COMP VALUE ZERO.
           05  WS-ROLL-DAYS                PIC S9(4) COMP VALUE ZERO.
      *
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-YYYY                  PIC 9(4).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
      *
       01  WS-ISO-DATE.
           05  WS-ISO-YYYY                 PIC 9(4).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-MM                   PIC 9(2).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-DD                   PIC 9(2).
      *
       01  WS-DISP-AMT                     PIC ---,---,---,--9.99.
       01  WS-DISP-CNT                     PIC ZZZ,ZZZ,ZZ9.
      *
           COPY CVCONSTY.
           COPY CVSTMT01Y.
           COPY CVERRS01Y.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-ACCT.
           05  DCL-ACCT-ID                 PIC S9(11) COMP-3.
           05  DCL-CURR-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-DELQ-AMT                PIC S9(9)V99 COMP-3.
           05  DCL-DELQ-BUCKET             PIC S9(4) COMP.
           05  DCL-CREDIT-LIMIT            PIC S9(11)V99 COMP-3.
           05  DCL-MIN-PAY-DUE             PIC S9(9)V99 COMP-3.
           05  DCL-STMT-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-STMT-COUNT              PIC S9(9) COMP.
           05  DCL-ACCT-STATUS             PIC X(1).
      *
       01  DCL-DATES.
           05  DCL-DUE-DATE                PIC X(10).
           05  DCL-CYCLE-DATE              PIC X(10).
           05  DCL-NEXT-CYCLE-DT           PIC X(10).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 1000-INITIALISE
           IF WS-FATAL
               GO TO 0000-TERMINATE
           END-IF
      *
           PERFORM 2000-READ-STATEMENT
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               ADD 1                       TO WS-READ-CNT
               PERFORM 3000-PROCESS-STATEMENT
               PERFORM 2000-READ-STATEMENT
           END-PERFORM
      *
           PERFORM 8000-CLOSE-DOWN
           PERFORM 8500-REPORT-TOTALS
           .
       0000-TERMINATE.
           PERFORM 9900-SET-RETURN-CODE
           GOBACK
           .
      *
       1000-INITIALISE.
           MOVE 'CBBIL03 '                 TO ER-PGM-NAME
           MOVE '1000-INITIALISE'          TO ER-PARAGRAPH
           MOVE WS-COMMIT-FREQUENCY        TO WS-COMMIT-FREQ
      *
           PERFORM 1100-READ-SYSIN
           IF WS-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           OPEN INPUT  STMTWRK-FILE
           IF NOT WS-STMTWRK-OK
               MOVE 'STMTWRK '             TO ER-FILE-NAME
               MOVE WS-STMTWRK-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STMTWRK OPEN FAILED'  TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
               GO TO 1000-EXIT
           END-IF
      *
           OPEN OUTPUT STMTWK2-FILE
           IF NOT WS-STMTWK2-OK
               MOVE 'STMTWK2 '             TO ER-FILE-NAME
               MOVE WS-STMTWK2-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STMTWK2 OPEN FAILED'  TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
       1100-READ-SYSIN.
           PERFORM UNTIL WS-SYSIN-EOF
               ACCEPT WS-CARD-IMAGE FROM SYSIN
                   ON EXCEPTION
                       MOVE 'Y'            TO WS-SYSIN-EOF-SW
               END-ACCEPT
               IF NOT WS-SYSIN-EOF
                   PERFORM 1150-APPLY-CARD
               END-IF
           END-PERFORM
      *
           IF WS-CYCLE-DATE   = ZERO
           OR WS-MIN-PAY-PCT  = ZERO
           OR WS-MIN-PAY-FLOOR = ZERO
           OR WS-GRACE-DAYS   = ZERO
               MOVE 'MANDATORY CONTROL CARD MISSING'
                                           TO ER-MESSAGE
               MOVE 'DATA'                 TO ER-ERROR-TYPE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       1150-APPLY-CARD.
           EVALUATE WS-CARD-KEYWORD
               WHEN 'CYCLE-DATE '
                   MOVE WS-CARD-VALUE(1:8) TO WS-CYCLE-DATE
               WHEN 'CYCLE-ID   '
                   MOVE WS-CARD-VALUE(1:8) TO WS-CYCLE-ID
               WHEN 'MIN-PAY-PCT'
                   COMPUTE WS-MIN-PAY-PCT =
                           FUNCTION NUMVAL(WS-CARD-VALUE)
               WHEN 'MIN-PAY-FLR'
                   COMPUTE WS-MIN-PAY-FLOOR =
                           FUNCTION NUMVAL(WS-CARD-VALUE)
               WHEN 'GRACE-DAYS '
                   MOVE WS-CARD-VALUE(1:2) TO WS-GRACE-DAYS
               WHEN 'COMMIT-FREQ'
                   MOVE WS-CARD-VALUE(1:6) TO WS-COMMIT-FREQ
               WHEN 'HOLIDAY    '
                   IF WS-HOLIDAY-CNT < 30
                       ADD 1               TO WS-HOLIDAY-CNT
                       MOVE WS-CARD-VALUE(1:8)
                                       TO WS-HOLIDAY(WS-HOLIDAY-CNT)
                   ELSE
                       DISPLAY 'CBBIL03 HOLIDAY TABLE FULL - CARD '
                               'IGNORED ' WS-CARD-VALUE
                   END-IF
               WHEN '*          '
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'CBBIL03 BAD CONTROL CARD - ' WS-CARD-IMAGE
           END-EVALUATE
           .
      *
       2000-READ-STATEMENT.
           READ STMTWRK-FILE INTO STMT-RECORD
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
           END-READ
           .
      *
      ******************************************************************
      * 3000 - ONE STATEMENT                                           *
      ******************************************************************
       3000-PROCESS-STATEMENT.
           MOVE '3000-PROCESS-STATEMENT'   TO ER-PARAGRAPH
           PERFORM 3100-READ-ACCOUNT
           IF WS-WARNING
               MOVE 'N'                    TO WS-WARNING-SW
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 3200-CALC-MIN-PAYMENT
           PERFORM 3300-DERIVE-DUE-DATE
           PERFORM 3400-UPDATE-ACCOUNT
           PERFORM 3500-WRITE-STATEMENT
      *
           ADD 1                           TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
               EXEC SQL COMMIT END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
       3100-READ-ACCOUNT.
           MOVE STMT-ACCT-ID               TO DCL-ACCT-ID
      *
           EXEC SQL
               SELECT CURR_BAL
                    , DELQ_AMT
                    , DELQ_BUCKET
                    , MIN_PAY_DUE
                    , STMT_BAL
                    , STMT_COUNT
                    , ACCT_STATUS
                 INTO :DCL-CURR-BAL
                    , :DCL-DELQ-AMT
                    , :DCL-DELQ-BUCKET
                    , :DCL-MIN-PAY-DUE
                    , :DCL-STMT-BAL
                    , :DCL-STMT-COUNT
                    , :DCL-ACCT-STATUS
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   ADD 1                   TO WS-NOTFND-CNT
                   MOVE 'Y'                TO WS-WARNING-SW
                   DISPLAY 'CBBIL03 ACCOUNT NOT FOUND ' STMT-ACCT-ID
               WHEN OTHER
                   MOVE 'ACCOUNT           ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '         TO ER-SQL-OPERATION
                   PERFORM 9400-SQL-ERROR
                   PERFORM 9500-ABEND
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3200 - MINIMUM PAYMENT                                         *
      *                                                                *
      * A CREDIT BALANCE OWES NOTHING.  A BALANCE AT OR UNDER THE      *
      * FLOOR IS DUE IN FULL.  OTHERWISE THE PERCENTAGE APPLIES, WITH  *
      * ARREARS AND THE OVER LIMIT EXCESS ADDED ON TOP AND THE WHOLE   *
      * CAPPED AT THE CLOSING BALANCE.                                 *
      ******************************************************************
       3200-CALC-MIN-PAYMENT.
           MOVE ZERO                       TO WS-MIN-PAY
                                              WS-PCT-AMT
                                              WS-ARREARS
                                              WS-OVER-LIMIT
      *
           IF STMT-CLOSE-BAL NOT > ZERO
               MOVE ZERO                   TO WS-MIN-PAY
               GO TO 3200-EXIT
           END-IF
      *
           IF STMT-CLOSE-BAL NOT > WS-MIN-PAY-FLOOR
               MOVE STMT-CLOSE-BAL         TO WS-MIN-PAY
               ADD 1                       TO WS-FULLBAL-CNT
               GO TO 3200-EXIT
           END-IF
      *
           COMPUTE WS-PCT-AMT ROUNDED =
                   STMT-CLOSE-BAL * WS-MIN-PAY-PCT / 100
      *
           IF WS-PCT-AMT < WS-MIN-PAY-FLOOR
               MOVE WS-MIN-PAY-FLOOR       TO WS-MIN-PAY
           ELSE
               MOVE WS-PCT-AMT             TO WS-MIN-PAY
           END-IF
      *
      *    ARREARS - EVERYTHING THE ACCOUNT SHOULD ALREADY HAVE PAID
           IF DCL-DELQ-BUCKET > ZERO
           AND DCL-DELQ-AMT   > ZERO
               MOVE DCL-DELQ-AMT           TO WS-ARREARS
               ADD WS-ARREARS              TO WS-MIN-PAY
               ADD 1                       TO WS-ARREARS-CNT
           END-IF
      *
      *    OVER LIMIT EXCESS IS DUE IMMEDIATELY
           IF STMT-CLOSE-BAL > STMT-CREDIT-LIMIT
               COMPUTE WS-OVER-LIMIT =
                       STMT-CLOSE-BAL - STMT-CREDIT-LIMIT
               ADD WS-OVER-LIMIT           TO WS-MIN-PAY
           END-IF
      *
           IF WS-MIN-PAY > STMT-CLOSE-BAL
               MOVE STMT-CLOSE-BAL         TO WS-MIN-PAY
           END-IF
           .
       3200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3300 - DUE DATE                                                *
      *                                                                *
      * CYCLE DATE PLUS GRACE DAYS.  IF THE RESULT IS A SATURDAY, A    *
      * SUNDAY OR A LISTED HOLIDAY IT ROLLS FORWARD A DAY AT A TIME.   *
      * ROLLING FORWARD NEVER SHORTENS THE GRACE PERIOD, WHICH IS WHY  *
      * IT IS NEVER ROLLED BACK.                                       *
      ******************************************************************
       3300-DERIVE-DUE-DATE.
           MOVE STMT-CYCLE-DATE            TO WS-DATE-WORK
           COMPUTE WS-INT-DATE =
                   FUNCTION INTEGER-OF-DATE(WS-DATE-WORK)
                   + WS-GRACE-DAYS
      *
           MOVE ZERO                       TO WS-ROLL-DAYS
           MOVE 'Y'                        TO WS-ROLL-SW
      *
           PERFORM UNTIL NOT WS-ROLL-NEEDED
                      OR WS-ROLL-DAYS > 10
               COMPUTE WS-DUE-DATE =
                       FUNCTION DATE-OF-INTEGER(WS-INT-DATE)
               PERFORM 3350-CHECK-BUSINESS-DAY
               IF WS-ROLL-NEEDED
                   ADD 1                   TO WS-INT-DATE
                                              WS-ROLL-DAYS
               END-IF
           END-PERFORM
      *
      *    THE NEXT CYCLE DATE IS ONE MONTH ON FROM THIS ONE, WITH THE
      *    DAY HELD DOWN TO 28 SO IT ALWAYS EXISTS.
           MOVE STMT-CYCLE-DATE            TO WS-DATE-WORK
           IF WS-DW-MM = 12
               ADD 1                       TO WS-DW-YYYY
               MOVE 01                     TO WS-DW-MM
           ELSE
               ADD 1                       TO WS-DW-MM
           END-IF
           IF WS-DW-DD > 28
               MOVE 28                     TO WS-DW-DD
           END-IF
           MOVE WS-DATE-WORK               TO WS-NEXT-CYCLE-DT
           .
      *
       3350-CHECK-BUSINESS-DAY.
           MOVE 'N'                        TO WS-ROLL-SW
      *
      *    INTEGER-OF-DATE DAY 1 WAS A MONDAY, SO THE REMAINDER
      *    IDENTIFIES SATURDAY AND SUNDAY.
           COMPUTE WS-DAY-OF-WEEK = FUNCTION MOD(WS-INT-DATE, 7)
           IF WS-DAY-OF-WEEK = 6
           OR WS-DAY-OF-WEEK = 0
               MOVE 'Y'                    TO WS-ROLL-SW
               GO TO 3350-EXIT
           END-IF
      *
           IF WS-HOLIDAY-CNT > ZERO
               PERFORM VARYING WS-HOL-IDX FROM 1 BY 1
                         UNTIL WS-HOL-IDX > WS-HOLIDAY-CNT
                            OR WS-ROLL-NEEDED
                   IF WS-HOLIDAY(WS-HOL-IDX) = WS-DUE-DATE
                       MOVE 'Y'            TO WS-ROLL-SW
                   END-IF
               END-PERFORM
           END-IF
           .
       3350-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3400 - WRITE THE BILLED POSITION BACK TO THE ACCOUNT           *
      ******************************************************************
       3400-UPDATE-ACCOUNT.
           MOVE WS-DUE-DATE                TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-DUE-DATE
      *
           MOVE STMT-CYCLE-DATE            TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-CYCLE-DATE
      *
           MOVE WS-NEXT-CYCLE-DT           TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-NEXT-CYCLE-DT
      *
           MOVE WS-MIN-PAY                 TO DCL-MIN-PAY-DUE
           MOVE STMT-CLOSE-BAL             TO DCL-STMT-BAL
      *
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET MIN_PAY_DUE     = :DCL-MIN-PAY-DUE
                    , PAY_DUE_DATE    = DATE(:DCL-DUE-DATE)
                    , STMT_BAL        = :DCL-STMT-BAL
                    , LAST_CYCLE_DATE = DATE(:DCL-CYCLE-DATE)
                    , NEXT_CYCLE_DATE = DATE(:DCL-NEXT-CYCLE-DT)
                    , STMT_COUNT      = STMT_COUNT + 1
                    , LAST_MAINT_PGM  = 'CBBIL03 '
                    , LAST_MAINT_TS   = CURRENT TIMESTAMP
                WHERE ACCT_ID         = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-UPDATE-CNT
                   ADD WS-MIN-PAY          TO WS-TOTAL-MIN-PAY
               WHEN +100
                   ADD 1                   TO WS-NOTFND-CNT
                   DISPLAY 'CBBIL03 UPDATE MATCHED NO ROW ACCT '
                           STMT-ACCT-ID
               WHEN OTHER
                   MOVE 'ACCOUNT           ' TO ER-SQL-TABLE
                   MOVE 'UPDATE  '         TO ER-SQL-OPERATION
                   PERFORM 9400-SQL-ERROR
                   PERFORM 9500-ABEND
           END-EVALUATE
           .
      *
       3500-WRITE-STATEMENT.
           MOVE WS-MIN-PAY                 TO STMT-MIN-PAY
           MOVE WS-DUE-DATE                TO STMT-DUE-DATE
      *
           COMPUTE WS-OUT-LEN = 204 + (80 * STMT-LINE-CNT)
      *
           WRITE STMTWK2-REC FROM STMT-RECORD
           IF NOT WS-STMTWK2-OK
               MOVE 'STMTWK2 '             TO ER-FILE-NAME
               MOVE WS-STMTWK2-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STMTWK2 WRITE FAILED' TO ER-MESSAGE
               PERFORM 9500-ABEND
           END-IF
           .
      *
       7200-FORMAT-ISO-DATE.
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           .
      *
       8000-CLOSE-DOWN.
           EXEC SQL COMMIT END-EXEC
           CLOSE STMTWRK-FILE
                 STMTWK2-FILE
           .
      *
       8500-REPORT-TOTALS.
           DISPLAY '*---------------------------------------------*'
           DISPLAY '* CBBIL03 MINIMUM PAYMENT AND DUE DATE        *'
           DISPLAY '*---------------------------------------------*'
           DISPLAY '  CYCLE ID          ' WS-CYCLE-ID
           DISPLAY '  CYCLE DATE        ' WS-CYCLE-DATE
           DISPLAY '  GRACE DAYS        ' WS-GRACE-DAYS
           MOVE WS-READ-CNT                TO WS-DISP-CNT
           DISPLAY '  STATEMENTS READ   ' WS-DISP-CNT
           MOVE WS-UPDATE-CNT              TO WS-DISP-CNT
           DISPLAY '  ACCOUNTS UPDATED  ' WS-DISP-CNT
           MOVE WS-ARREARS-CNT             TO WS-DISP-CNT
           DISPLAY '  WITH ARREARS      ' WS-DISP-CNT
           MOVE WS-FULLBAL-CNT             TO WS-DISP-CNT
           DISPLAY '  DUE IN FULL       ' WS-DISP-CNT
           MOVE WS-NOTFND-CNT              TO WS-DISP-CNT
           DISPLAY '  ACCOUNTS MISSING  ' WS-DISP-CNT
           MOVE WS-TOTAL-MIN-PAY           TO WS-DISP-AMT
           DISPLAY '  TOTAL MINIMUM DUE ' WS-DISP-AMT
           .
      *
       9400-SQL-ERROR.
           MOVE 'CBBIL03 '                 TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE 'E'                        TO ER-SEVERITY
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBBIL03 SQL ERROR SQLCODE=' SQLCODE
                   ' ACCT=' STMT-ACCT-ID
           .
      *
       9500-FATAL-ERROR.
           MOVE 'CBBIL03 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL03 FATAL - ' ER-MESSAGE
           .
      *
       9500-ABEND.
           MOVE 'CBBIL03 '                 TO ER-PGM-NAME
           MOVE 'U803'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL03 ABEND U0803 - ' ER-MESSAGE
           EXEC SQL ROLLBACK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           .
      *
       9900-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO RETURN-CODE
               WHEN WS-NOTFND-CNT > ZERO
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   MOVE 0                  TO RETURN-CODE
           END-EVALUATE
           .
