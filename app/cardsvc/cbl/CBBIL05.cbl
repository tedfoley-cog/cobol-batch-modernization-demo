      ******************************************************************
      * CBBIL05 - CARDBILL STEP 05 - STATEMENT PERSISTENCE             *
      *                                                                *
      * INSERTS THE RENDERED STATEMENTS INTO CARDSVC.STATEMENT AND     *
      * WRITES THE SAME IMAGE TO THE ARCHIVE DATASET.  THE ARCHIVE IS  *
      * VARIABLE BLOCKED - THE RECORD LENGTH FOLLOWS THE OCCURS        *
      * DEPENDING ON LINE COUNT, 204 BYTES OF FIXED PART AND TRAILER   *
      * PLUS 80 BYTES A LINE, SO 24204 AT THE 300 LINE MAXIMUM.        *
      *                                                                *
      * A RERUN OF THE STEP FINDS ITS OWN ROWS ALREADY THERE.  SQLCODE *
      * -803 IS THEREFORE TREATED AS A REPLACE, NOT AS AN ERROR.       *
      *                                                                *
      * CALLED BY   - JOB CBBIL05J, STEP PERSIST                       *
      * CALLS       - CBCRD91 (FATAL ERROR / ABEND HANDLER)            *
      * READS       - STMTWK3                                          *
      * UPDATES     - CARDSVC.STATEMENT                                *
      * WRITES      - STMTARC (VB 24208)                               *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - ALL STATEMENTS PERSISTED                                *
      *   04 - ONE OR MORE STATEMENTS REPLACED ON RERUN                *
      *   12 - FATAL                                                   *
      * ABEND U0805 - UNRECOVERABLE FILE OR SQL CONDITION              *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBBIL05.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STMTWK3-FILE ASSIGN TO STMTWK3
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTWK3-STATUS.
      *
           SELECT STMTARC-FILE ASSIGN TO STMTARC
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTARC-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  STMTWK3-FILE
           RECORD IS VARYING IN SIZE FROM 284 TO 24204 CHARACTERS
               DEPENDING ON WS-IN-LEN
           BLOCK CONTAINS 0 RECORDS.
       01  STMTWK3-REC                     PIC X(24204).
      *
       FD  STMTARC-FILE
           RECORD IS VARYING IN SIZE FROM 284 TO 24204 CHARACTERS
               DEPENDING ON WS-ARC-LEN
           BLOCK CONTAINS 0 RECORDS.
       01  STMTARC-REC                     PIC X(24204).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBBIL05 '.
      *
       01  WS-FILE-STATUS-AREA.
           05  WS-STMTWK3-STATUS           PIC X(2)  VALUE '00'.
               88  WS-STMTWK3-OK                     VALUE '00'.
           05  WS-STMTARC-STATUS           PIC X(2)  VALUE '00'.
               88  WS-STMTARC-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-SYSIN-EOF-SW             PIC X     VALUE 'N'.
               88  WS-SYSIN-EOF                      VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
      *
       01  WS-CONTROL-CARDS.
           05  WS-CYCLE-DATE               PIC 9(8)  VALUE ZERO.
           05  WS-CYCLE-ID                 PIC X(8)  VALUE SPACES.
           05  WS-COMMIT-FREQ              PIC 9(6)  VALUE ZERO.
           05  WS-ARCHIVE-FLG              PIC X     VALUE 'Y'.
               88  WS-ARCHIVE-WANTED                 VALUE 'Y'.
      *
       01  WS-CARD-IMAGE.
           05  WS-CARD-KEYWORD             PIC X(12).
           05  FILLER                      PIC X.
           05  WS-CARD-VALUE               PIC X(20).
           05  FILLER                      PIC X(47).
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-INSERT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-REPLACE-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-ARCHIVE-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-LINE-TOTAL               PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
           05  WS-MAX-LEN-SEEN             PIC 9(5)  VALUE ZERO.
      *
       01  WS-IN-LEN                       PIC S9(8) COMP VALUE 284.
       01  WS-ARC-LEN                      PIC S9(8) COMP VALUE 284.
      *
       01  WS-TOTALS.
           05  WS-TOTAL-BILLED             PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-TOTAL-MIN-PAY            PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
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
       01  DCL-STMT.
           05  DCL-ACCT-ID                 PIC S9(11) COMP-3.
           05  DCL-CYCLE-DATE              PIC X(10).
           05  DCL-STMT-NUMBER             PIC S9(9) COMP-3.
           05  DCL-CUST-ID                 PIC S9(9) COMP-3.
           05  DCL-FORMAT-CD               PIC X(4).
           05  DCL-PERIOD-FROM             PIC X(10).
           05  DCL-PERIOD-TO               PIC X(10).
           05  DCL-DUE-DATE                PIC X(10).
           05  DCL-OPEN-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-CLOSE-BAL               PIC S9(11)V99 COMP-3.
           05  DCL-PURCHASES               PIC S9(11)V99 COMP-3.
           05  DCL-CASH-ADV                PIC S9(11)V99 COMP-3.
           05  DCL-PAYMENTS                PIC S9(11)V99 COMP-3.
           05  DCL-FEES                    PIC S9(9)V99 COMP-3.
           05  DCL-INTEREST                PIC S9(9)V99 COMP-3.
           05  DCL-MIN-PAY                 PIC S9(9)V99 COMP-3.
           05  DCL-CREDIT-LIMIT            PIC S9(11)V99 COMP-3.
           05  DCL-AVAIL-CREDIT            PIC S9(11)V99 COMP-3.
           05  DCL-APR-PCT                 PIC S9(3)V9(5) COMP-3.
           05  DCL-REWARD-PTS              PIC S9(11) COMP-3.
           05  DCL-LINE-CNT                PIC S9(9) COMP.
           05  DCL-PAGE-CNT                PIC S9(4) COMP.
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
               PERFORM 3000-PERSIST-STATEMENT
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
           MOVE 'CBBIL05 '                 TO ER-PGM-NAME
           MOVE '1000-INITIALISE'          TO ER-PARAGRAPH
           MOVE WS-COMMIT-FREQUENCY        TO WS-COMMIT-FREQ
      *
           PERFORM 1100-READ-SYSIN
      *
           OPEN INPUT  STMTWK3-FILE
           IF NOT WS-STMTWK3-OK
               MOVE 'STMTWK3 '             TO ER-FILE-NAME
               MOVE WS-STMTWK3-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STMTWK3 OPEN FAILED'  TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
               GO TO 1000-EXIT
           END-IF
      *
           IF WS-ARCHIVE-WANTED
               OPEN OUTPUT STMTARC-FILE
               IF NOT WS-STMTARC-OK
                   MOVE 'STMTARC '         TO ER-FILE-NAME
                   MOVE WS-STMTARC-STATUS  TO ER-FILE-STATUS
                   MOVE 'VSAM'             TO ER-ERROR-TYPE
                   MOVE 'ARCHIVE OPEN FAILED'
                                           TO ER-MESSAGE
                   PERFORM 9500-FATAL-ERROR
               END-IF
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
                   EVALUATE WS-CARD-KEYWORD
                       WHEN 'CYCLE-DATE '
                           MOVE WS-CARD-VALUE(1:8)
                                           TO WS-CYCLE-DATE
                       WHEN 'CYCLE-ID   '
                           MOVE WS-CARD-VALUE(1:8)
                                           TO WS-CYCLE-ID
                       WHEN 'COMMIT-FREQ'
                           MOVE WS-CARD-VALUE(1:6)
                                           TO WS-COMMIT-FREQ
                       WHEN 'ARCHIVE    '
                           MOVE WS-CARD-VALUE(1:1)
                                           TO WS-ARCHIVE-FLG
                       WHEN '*          '
                           CONTINUE
                       WHEN OTHER
                           DISPLAY 'CBBIL05 BAD CONTROL CARD - '
                                   WS-CARD-IMAGE
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
      *
       2000-READ-STATEMENT.
           READ STMTWK3-FILE INTO STMT-RECORD
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
           END-READ
           .
      *
       3000-PERSIST-STATEMENT.
           MOVE '3000-PERSIST-STATEMENT'   TO ER-PARAGRAPH
           PERFORM 3100-MOVE-HOST-VARS
           PERFORM 3200-INSERT-STATEMENT
      *
           IF WS-ARCHIVE-WANTED
               PERFORM 3300-ARCHIVE-STATEMENT
           END-IF
      *
           ADD STMT-LINE-CNT               TO WS-LINE-TOTAL
           ADD STMT-CLOSE-BAL              TO WS-TOTAL-BILLED
           ADD STMT-MIN-PAY                TO WS-TOTAL-MIN-PAY
      *
           ADD 1                           TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
               EXEC SQL COMMIT END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
           .
      *
       3100-MOVE-HOST-VARS.
           MOVE STMT-ACCT-ID               TO DCL-ACCT-ID
           MOVE STMT-CYCLE-DATE            TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-CYCLE-DATE
           MOVE STMT-PERIOD-FROM           TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-PERIOD-FROM
           MOVE STMT-PERIOD-TO             TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-PERIOD-TO
           MOVE STMT-DUE-DATE              TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-DUE-DATE
      *
           MOVE STMT-NUMBER                TO DCL-STMT-NUMBER
           MOVE STMT-CUST-ID               TO DCL-CUST-ID
           MOVE STMT-FORMAT-CD             TO DCL-FORMAT-CD
           MOVE STMT-OPEN-BAL              TO DCL-OPEN-BAL
           MOVE STMT-CLOSE-BAL             TO DCL-CLOSE-BAL
           MOVE STMT-PURCHASES             TO DCL-PURCHASES
           MOVE STMT-CASH-ADV              TO DCL-CASH-ADV
           MOVE STMT-PAYMENTS              TO DCL-PAYMENTS
           MOVE STMT-FEES                  TO DCL-FEES
           MOVE STMT-INTEREST              TO DCL-INTEREST
           MOVE STMT-MIN-PAY               TO DCL-MIN-PAY
           MOVE STMT-CREDIT-LIMIT          TO DCL-CREDIT-LIMIT
           MOVE STMT-AVAIL-CREDIT          TO DCL-AVAIL-CREDIT
           MOVE STMT-APR                   TO DCL-APR-PCT
           MOVE STMT-REWARD-PTS            TO DCL-REWARD-PTS
           MOVE STMT-LINE-CNT              TO DCL-LINE-CNT
           MOVE STMT-PAGE-CNT              TO DCL-PAGE-CNT
           IF DCL-PAGE-CNT = ZERO
               MOVE 1                      TO DCL-PAGE-CNT
           END-IF
           .
      *
       3200-INSERT-STATEMENT.
           EXEC SQL
               INSERT INTO CARDSVC.STATEMENT
                     (ACCT_ID
                    , CYCLE_DATE
                    , STMT_NUMBER
                    , CUST_ID
                    , FORMAT_CD
                    , PERIOD_FROM
                    , PERIOD_TO
                    , DUE_DATE
                    , OPEN_BAL
                    , CLOSE_BAL
                    , PURCHASES_AMT
                    , CASH_ADV_AMT
                    , PAYMENTS_AMT
                    , FEES_AMT
                    , INTEREST_AMT
                    , MIN_PAY_AMT
                    , CREDIT_LIMIT
                    , AVAIL_CREDIT
                    , APR_PCT
                    , REWARD_POINTS
                    , LINE_CNT
                    , PAGE_CNT
                    , GEN_PGM
                    , GEN_TS)
               VALUES (:DCL-ACCT-ID
                    , DATE(:DCL-CYCLE-DATE)
                    , :DCL-STMT-NUMBER
                    , :DCL-CUST-ID
                    , :DCL-FORMAT-CD
                    , DATE(:DCL-PERIOD-FROM)
                    , DATE(:DCL-PERIOD-TO)
                    , DATE(:DCL-DUE-DATE)
                    , :DCL-OPEN-BAL
                    , :DCL-CLOSE-BAL
                    , :DCL-PURCHASES
                    , :DCL-CASH-ADV
                    , :DCL-PAYMENTS
                    , :DCL-FEES
                    , :DCL-INTEREST
                    , :DCL-MIN-PAY
                    , :DCL-CREDIT-LIMIT
                    , :DCL-AVAIL-CREDIT
                    , :DCL-APR-PCT
                    , :DCL-REWARD-PTS
                    , :DCL-LINE-CNT
                    , :DCL-PAGE-CNT
                    , 'CBBIL05 '
                    , CURRENT TIMESTAMP)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-INSERT-CNT
               WHEN -803
                   PERFORM 3250-REPLACE-STATEMENT
               WHEN OTHER
                   MOVE 'STATEMENT         ' TO ER-SQL-TABLE
                   MOVE 'INSERT  '         TO ER-SQL-OPERATION
                   PERFORM 9400-SQL-ERROR
                   PERFORM 9500-ABEND
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3250 - RERUN PATH.  THE STATEMENT NUMBER IS NOT CHANGED - THE  *
      *        CUSTOMER MAY ALREADY HAVE BEEN SENT THE FIRST COPY.     *
      ******************************************************************
       3250-REPLACE-STATEMENT.
           EXEC SQL
               UPDATE CARDSVC.STATEMENT
                  SET CUST_ID       = :DCL-CUST-ID
                    , FORMAT_CD     = :DCL-FORMAT-CD
                    , PERIOD_FROM   = DATE(:DCL-PERIOD-FROM)
                    , PERIOD_TO     = DATE(:DCL-PERIOD-TO)
                    , DUE_DATE      = DATE(:DCL-DUE-DATE)
                    , OPEN_BAL      = :DCL-OPEN-BAL
                    , CLOSE_BAL     = :DCL-CLOSE-BAL
                    , PURCHASES_AMT = :DCL-PURCHASES
                    , CASH_ADV_AMT  = :DCL-CASH-ADV
                    , PAYMENTS_AMT  = :DCL-PAYMENTS
                    , FEES_AMT      = :DCL-FEES
                    , INTEREST_AMT  = :DCL-INTEREST
                    , MIN_PAY_AMT   = :DCL-MIN-PAY
                    , CREDIT_LIMIT  = :DCL-CREDIT-LIMIT
                    , AVAIL_CREDIT  = :DCL-AVAIL-CREDIT
                    , APR_PCT       = :DCL-APR-PCT
                    , REWARD_POINTS = :DCL-REWARD-PTS
                    , LINE_CNT      = :DCL-LINE-CNT
                    , PAGE_CNT      = :DCL-PAGE-CNT
                    , GEN_PGM       = 'CBBIL05 '
                    , GEN_TS        = CURRENT TIMESTAMP
                WHERE ACCT_ID       = :DCL-ACCT-ID
                  AND CYCLE_DATE    = DATE(:DCL-CYCLE-DATE)
           END-EXEC
      *
           IF SQLCODE = 0
               ADD 1                       TO WS-REPLACE-CNT
           ELSE
               MOVE 'STATEMENT         '   TO ER-SQL-TABLE
               MOVE 'UPDATE  '             TO ER-SQL-OPERATION
               PERFORM 9400-SQL-ERROR
               PERFORM 9500-ABEND
           END-IF
           .
      *
      ******************************************************************
      * 3300 - ARCHIVE.  THE LENGTH IS DERIVED FROM THE LINE COUNT     *
      *        BEFORE THE WRITE - THE RDW IS FOUR BYTES MORE.          *
      ******************************************************************
       3300-ARCHIVE-STATEMENT.
           COMPUTE WS-ARC-LEN = 204 + (80 * STMT-LINE-CNT)
      *
           IF WS-ARC-LEN > WS-MAX-LEN-SEEN
               MOVE WS-ARC-LEN             TO WS-MAX-LEN-SEEN
           END-IF
      *
           WRITE STMTARC-REC FROM STMT-RECORD
           IF NOT WS-STMTARC-OK
               MOVE 'STMTARC '             TO ER-FILE-NAME
               MOVE WS-STMTARC-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'ARCHIVE WRITE FAILED' TO ER-MESSAGE
               PERFORM 9500-ABEND
           END-IF
      *
           ADD 1                           TO WS-ARCHIVE-CNT
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
           CLOSE STMTWK3-FILE
           IF WS-ARCHIVE-WANTED
               CLOSE STMTARC-FILE
           END-IF
           .
      *
       8500-REPORT-TOTALS.
           DISPLAY '*---------------------------------------------*'
           DISPLAY '* CBBIL05 STATEMENT PERSISTENCE               *'
           DISPLAY '*---------------------------------------------*'
           DISPLAY '  CYCLE ID          ' WS-CYCLE-ID
           DISPLAY '  CYCLE DATE        ' WS-CYCLE-DATE
           MOVE WS-READ-CNT                TO WS-DISP-CNT
           DISPLAY '  STATEMENTS READ   ' WS-DISP-CNT
           MOVE WS-INSERT-CNT              TO WS-DISP-CNT
           DISPLAY '  ROWS INSERTED     ' WS-DISP-CNT
           MOVE WS-REPLACE-CNT             TO WS-DISP-CNT
           DISPLAY '  ROWS REPLACED     ' WS-DISP-CNT
           MOVE WS-ARCHIVE-CNT             TO WS-DISP-CNT
           DISPLAY '  ARCHIVE RECORDS   ' WS-DISP-CNT
           MOVE WS-LINE-TOTAL              TO WS-DISP-CNT
           DISPLAY '  STATEMENT LINES   ' WS-DISP-CNT
           DISPLAY '  LONGEST RECORD    ' WS-MAX-LEN-SEEN
           MOVE WS-TOTAL-BILLED            TO WS-DISP-AMT
           DISPLAY '  TOTAL BILLED      ' WS-DISP-AMT
           MOVE WS-TOTAL-MIN-PAY           TO WS-DISP-AMT
           DISPLAY '  TOTAL MINIMUM DUE ' WS-DISP-AMT
           .
      *
       9400-SQL-ERROR.
           MOVE 'CBBIL05 '                 TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE 'E'                        TO ER-SEVERITY
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBBIL05 SQL ERROR SQLCODE=' SQLCODE
                   ' ACCT=' STMT-ACCT-ID
                   ' CYCLE=' STMT-CYCLE-DATE
           .
      *
       9500-FATAL-ERROR.
           MOVE 'CBBIL05 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL05 FATAL - ' ER-MESSAGE
           .
      *
       9500-ABEND.
           MOVE 'CBBIL05 '                 TO ER-PGM-NAME
           MOVE 'U805'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL05 ABEND U0805 - ' ER-MESSAGE
           EXEC SQL ROLLBACK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           .
      *
       9900-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO RETURN-CODE
               WHEN WS-REPLACE-CNT > ZERO
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   MOVE 0                  TO RETURN-CODE
           END-EVALUATE
           .
