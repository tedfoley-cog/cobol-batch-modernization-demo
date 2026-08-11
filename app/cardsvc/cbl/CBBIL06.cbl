      ******************************************************************
      * CBBIL06 - CARDBILL STEP 06 - CYCLE CLOSE AND RECONCILIATION    *
      *                                                                *
      * PROVES THE CYCLE BEFORE IT IS MARKED COMPLETE.  EVERY ACCOUNT  *
      * SELECTED BY CBBIL01 MUST HAVE A STATEMENT ROW, THE TOTAL       *
      * BILLED MUST AGREE WITH THE STATEMENT BALANCE HELD ON THE       *
      * ACCOUNT AND THE STATEMENT COUNT MUST AGREE WITH THE WORK FILE  *
      * COUNT.  EVERYTHING THAT DOES NOT AGREE IS LISTED ON THE        *
      * EXCEPTION REPORT.                                              *
      *                                                                *
      * THE CYCLE IS ONLY SET COMPLETE IN CYCLCTL WHEN THE PROOF       *
      * PASSES.  A FAILED PROOF LEAVES THE CYCLE RUNNING SO THE CHAIN  *
      * CAN BE RE-DRIVEN AFTER THE EXCEPTIONS ARE CLEARED.             *
      *                                                                *
      * CALLED BY   - JOB CBBIL06J, STEP RECON                         *
      * CALLS       - CBCRD91 (FATAL ERROR / ABEND HANDLER)            *
      * READS       - BILLWRK, CARDSVC.STATEMENT, CARDSVC.ACCOUNT      *
      * UPDATES     - VSAM CYCLCTL                                     *
      * WRITES      - RECONRPT (FBA 133)                               *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - CYCLE PROVED AND CLOSED                                 *
      *   04 - CLOSED WITH WARNINGS                                    *
      *   08 - PROOF FAILED - CYCLE LEFT OPEN                          *
      *   12 - FATAL                                                   *
      * ABEND U0806 - CONTROL FILE UNUSABLE                            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBBIL06.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILLWRK-FILE ASSIGN TO BILLWRK
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-BILLWRK-STATUS.
      *
           SELECT CYCLCTL-FILE ASSIGN TO CYCLCTL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CTL-KEY
                  FILE STATUS  IS WS-CYCLCTL-STATUS.
      *
           SELECT RECONRPT-FILE ASSIGN TO RECONRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-RECONRPT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  BILLWRK-FILE
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  BILLWRK-REC                     PIC X(200).
      *
       FD  CYCLCTL-FILE
           RECORD CONTAINS 256 CHARACTERS.
       01  CYCLCTL-REC.
           05  CTL-KEY.
               10  CTL-CYCLE-TYPE          PIC X(8).
               10  CTL-CYCLE-DATE          PIC 9(8).
           05  CTL-REST                    PIC X(240).
      *
      *    CARRIAGE CONTROL IN COLUMN 1 - THE REPORT IS FBA.
       FD  RECONRPT-FILE
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  RECONRPT-REC                    PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBBIL06 '.
      *
       01  WS-FILE-STATUS-AREA.
           05  WS-BILLWRK-STATUS           PIC X(2)  VALUE '00'.
               88  WS-BILLWRK-OK                     VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
               88  WS-CYCLCTL-NOTFND                 VALUE '23'.
           05  WS-RECONRPT-STATUS          PIC X(2)  VALUE '00'.
               88  WS-RECONRPT-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-SYSIN-EOF-SW             PIC X     VALUE 'N'.
               88  WS-SYSIN-EOF                      VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
           05  WS-PROOF-FAILED-SW          PIC X     VALUE 'N'.
               88  WS-PROOF-FAILED                   VALUE 'Y'.
           05  WS-WARNING-SW               PIC X     VALUE 'N'.
               88  WS-WARNING                        VALUE 'Y'.
      *
       01  WS-CONTROL-CARDS.
           05  WS-CYCLE-DATE               PIC 9(8)  VALUE ZERO.
           05  WS-CYCLE-ID                 PIC X(8)  VALUE SPACES.
           05  WS-BAL-TOLERANCE            PIC S9(7)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-MAX-EXCEPTIONS           PIC 9(5)  VALUE 00200.
      *
       01  WS-CARD-IMAGE.
           05  WS-CARD-KEYWORD             PIC X(12).
           05  FILLER                      PIC X.
           05  WS-CARD-VALUE               PIC X(20).
           05  FILLER                      PIC X(47).
      *
       01  WS-COUNTERS.
           05  WS-WORK-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-MISSING-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-MISMATCH-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-EXCEPTION-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-LISTED-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE 99.
           05  WS-PAGE-NBR                 PIC 9(3)  VALUE ZERO.
      *
       01  WS-DB2-TOTALS.
           05  WS-STMT-ROW-CNT             PIC S9(9) COMP VALUE ZERO.
           05  WS-STMT-BILLED              PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-STMT-MIN-PAY             PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-STMT-FEES                PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-ACCT-ROW-CNT             PIC S9(9) COMP VALUE ZERO.
           05  WS-ACCT-BILLED              PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-PROOF-DIFF               PIC S9(13)V99 COMP-3
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
       01  WS-SQL-TS                       PIC X(26) VALUE SPACES.
      *
      ******************************************************************
      * REPORT LINES - CARRIAGE CONTROL IN COLUMN 1                    *
      ******************************************************************
       01  RPT-HEAD1.
           05  FILLER                      PIC X     VALUE '1'.
           05  FILLER                      PIC X(8)  VALUE 'CBBIL06 '.
           05  FILLER                      PIC X(40) VALUE
               'CARDBILL CYCLE RECONCILIATION           '.
           05  FILLER                      PIC X(12) VALUE
               'CYCLE DATE  '.
           05  RH1-CYCLE-DATE              PIC 9(8).
           05  FILLER                      PIC X(10) VALUE SPACES.
           05  FILLER                      PIC X(6)  VALUE 'PAGE  '.
           05  RH1-PAGE                    PIC ZZ9.
           05  FILLER                      PIC X(45) VALUE SPACES.
      *
       01  RPT-HEAD2.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(12) VALUE
               'CYCLE ID    '.
           05  RH2-CYCLE-ID                PIC X(8).
           05  FILLER                      PIC X(12) VALUE
               '  RUN AT    '.
           05  RH2-TIMESTAMP               PIC X(26).
           05  FILLER                      PIC X(74) VALUE SPACES.
      *
       01  RPT-HEAD3.
           05  FILLER                      PIC X     VALUE '-'.
           05  FILLER                      PIC X(132) VALUE
               'ACCOUNT      CUST      PRODUCT  EXCEPTION            '
            &  '        WORK FILE AMOUNT   STATEMENT AMOUNT   DIFFERE'
            &  'NCE                                                 '.
      *
       01  RPT-DETAIL.
           05  FILLER                      PIC X     VALUE ' '.
           05  RD-ACCT-ID                  PIC 9(11).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-CUST-ID                  PIC 9(9).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-PRODUCT                  PIC X(4).
           05  FILLER                      PIC X(5)  VALUE SPACES.
           05  RD-EXCEPTION                PIC X(28).
           05  RD-WORK-AMT                 PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-STMT-AMT                 PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-DIFF-AMT                 PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(9)  VALUE SPACES.
      *
       01  RPT-TOTAL.
           05  FILLER                      PIC X     VALUE ' '.
           05  RT-TEXT                     PIC X(40).
           05  RT-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  RT-AMOUNT                   PIC ---,---,---,---,--9.99.
           05  FILLER                      PIC X(52) VALUE SPACES.
      *
       01  RPT-BLANK.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(132) VALUE SPACES.
      *
           COPY CVCONSTY.
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVBWRK1Y.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-PROOF.
           05  DCL-P-CYCLE-DATE            PIC X(10).
           05  DCL-P-ACCT-ID               PIC S9(11) COMP-3.
           05  DCL-STMT-CNT                PIC S9(9) COMP.
           05  DCL-STMT-BILLED             PIC S9(13)V99 COMP-3.
           05  DCL-STMT-MIN-PAY            PIC S9(13)V99 COMP-3.
           05  DCL-STMT-FEES               PIC S9(13)V99 COMP-3.
           05  DCL-ACCT-CNT                PIC S9(9) COMP.
           05  DCL-ACCT-BILLED             PIC S9(13)V99 COMP-3.
           05  DCL-CLOSE-BAL               PIC S9(11)V99 COMP-3.
           05  DCL-ACCT-STMT-BAL           PIC S9(11)V99 COMP-3.
      *
       01  IND-VARS.
           05  IND-BILLED                  PIC S9(4) COMP.
           05  IND-MIN-PAY                 PIC S9(4) COMP.
           05  IND-FEES                    PIC S9(4) COMP.
           05  IND-ACCT-BILLED             PIC S9(4) COMP.
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
           PERFORM 2000-SUMMARISE-DB2
           PERFORM 3000-CHECK-WORK-FILE
           PERFORM 4000-PROVE-CYCLE
           PERFORM 5000-PRINT-SUMMARY
           PERFORM 6000-CLOSE-CYCLE
           PERFORM 8000-CLOSE-DOWN
           .
       0000-TERMINATE.
           PERFORM 9900-SET-RETURN-CODE
           GOBACK
           .
      *
       1000-INITIALISE.
           MOVE 'CBBIL06 '                 TO ER-PGM-NAME
           MOVE '1000-INITIALISE'          TO ER-PARAGRAPH
      *
           PERFORM 1100-READ-SYSIN
           IF WS-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           OPEN INPUT  BILLWRK-FILE
           OPEN I-O    CYCLCTL-FILE
           OPEN OUTPUT RECONRPT-FILE
      *
           IF NOT WS-BILLWRK-OK
               MOVE 'BILLWRK '             TO ER-FILE-NAME
               MOVE WS-BILLWRK-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'WORK FILE OPEN FAILED'
                                           TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'CYCLCTL OPEN FAILED'  TO ER-MESSAGE
               PERFORM 9500-ABEND
           END-IF
      *
           MOVE WS-CYCLE-DATE              TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-P-CYCLE-DATE
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
                       WHEN 'BAL-TOL    '
                           COMPUTE WS-BAL-TOLERANCE =
                                   FUNCTION NUMVAL(WS-CARD-VALUE)
                       WHEN 'MAX-EXCEPT '
                           MOVE WS-CARD-VALUE(1:5)
                                           TO WS-MAX-EXCEPTIONS
                       WHEN '*          '
                           CONTINUE
                       WHEN OTHER
                           DISPLAY 'CBBIL06 BAD CONTROL CARD - '
                                   WS-CARD-IMAGE
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           IF WS-CYCLE-DATE = ZERO
               MOVE 'CYCLE-DATE CARD MISSING'
                                           TO ER-MESSAGE
               MOVE 'DATA'                 TO ER-ERROR-TYPE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 2000 - WHAT THE DATABASE SAYS THE CYCLE PRODUCED               *
      ******************************************************************
       2000-SUMMARISE-DB2.
           MOVE '2000-SUMMARISE-DB2'       TO ER-PARAGRAPH
      *
           EXEC SQL
               SELECT COUNT(*)
                    , SUM(CLOSE_BAL)
                    , SUM(MIN_PAY_AMT)
                    , SUM(FEES_AMT)
                 INTO :WS-STMT-ROW-CNT
                    , :DCL-STMT-BILLED  :IND-BILLED
                    , :DCL-STMT-MIN-PAY :IND-MIN-PAY
                    , :DCL-STMT-FEES    :IND-FEES
                 FROM CARDSVC.STATEMENT
                WHERE CYCLE_DATE = DATE(:DCL-P-CYCLE-DATE)
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               MOVE 'STATEMENT         '   TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               PERFORM 9400-SQL-ERROR
               PERFORM 9500-FATAL-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           IF IND-BILLED NOT < ZERO
               MOVE DCL-STMT-BILLED        TO WS-STMT-BILLED
           END-IF
           IF IND-MIN-PAY NOT < ZERO
               MOVE DCL-STMT-MIN-PAY       TO WS-STMT-MIN-PAY
           END-IF
           IF IND-FEES NOT < ZERO
               MOVE DCL-STMT-FEES          TO WS-STMT-FEES
           END-IF
      *
      *    THE SAME POSITION AS THE ACCOUNT MASTER HOLDS IT
           EXEC SQL
               SELECT COUNT(*)
                    , SUM(STMT_BAL)
                 INTO :WS-ACCT-ROW-CNT
                    , :DCL-ACCT-BILLED :IND-ACCT-BILLED
                 FROM CARDSVC.ACCOUNT
                WHERE LAST_CYCLE_DATE = DATE(:DCL-P-CYCLE-DATE)
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               MOVE 'ACCOUNT           '   TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               PERFORM 9400-SQL-ERROR
               PERFORM 9500-FATAL-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           IF IND-ACCT-BILLED NOT < ZERO
               MOVE DCL-ACCT-BILLED        TO WS-ACCT-BILLED
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - EVERY SELECTED ACCOUNT MUST HAVE A STATEMENT            *
      ******************************************************************
       3000-CHECK-WORK-FILE.
           MOVE '3000-CHECK-WORK-FILE'     TO ER-PARAGRAPH
           PERFORM 3050-READ-WORK
      *
           PERFORM UNTIL WS-EOF
               ADD 1                       TO WS-WORK-CNT
               PERFORM 3100-LOOKUP-STATEMENT
               PERFORM 3050-READ-WORK
           END-PERFORM
           .
      *
       3050-READ-WORK.
           READ BILLWRK-FILE INTO BILL-WORK-RECORD
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
           END-READ
           .
      *
       3100-LOOKUP-STATEMENT.
           MOVE BW-ACCT-ID                 TO DCL-P-ACCT-ID
      *
           EXEC SQL
               SELECT S.CLOSE_BAL
                    , A.STMT_BAL
                 INTO :DCL-CLOSE-BAL
                    , :DCL-ACCT-STMT-BAL
                 FROM CARDSVC.STATEMENT S
                    , CARDSVC.ACCOUNT   A
                WHERE S.ACCT_ID    = :DCL-P-ACCT-ID
                  AND S.CYCLE_DATE = DATE(:DCL-P-CYCLE-DATE)
                  AND A.ACCT_ID    = S.ACCT_ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   PERFORM 3200-COMPARE-BALANCE
               WHEN +100
                   ADD 1                   TO WS-MISSING-CNT
                                              WS-EXCEPTION-CNT
                   MOVE 'NO STATEMENT PRODUCED     '
                                           TO RD-EXCEPTION
                   MOVE BW-CURR-BAL        TO RD-WORK-AMT
                   MOVE ZERO               TO RD-STMT-AMT
                   MOVE BW-CURR-BAL        TO RD-DIFF-AMT
                   PERFORM 3300-PRINT-EXCEPTION
               WHEN OTHER
                   MOVE 'STATEMENT         ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '         TO ER-SQL-OPERATION
                   PERFORM 9400-SQL-ERROR
                   PERFORM 9500-FATAL-ERROR
           END-EVALUATE
           .
      *
       3200-COMPARE-BALANCE.
           COMPUTE WS-PROOF-DIFF = DCL-CLOSE-BAL - DCL-ACCT-STMT-BAL
      *
           IF FUNCTION ABS(WS-PROOF-DIFF) > WS-BAL-TOLERANCE
               ADD 1                       TO WS-MISMATCH-CNT
                                              WS-EXCEPTION-CNT
               MOVE 'STATEMENT / ACCOUNT DIFFER'
                                           TO RD-EXCEPTION
               MOVE DCL-ACCT-STMT-BAL      TO RD-WORK-AMT
               MOVE DCL-CLOSE-BAL          TO RD-STMT-AMT
               MOVE WS-PROOF-DIFF          TO RD-DIFF-AMT
               PERFORM 3300-PRINT-EXCEPTION
           END-IF
           .
      *
      ******************************************************************
      * 3300 - EXCEPTION LINE.  THE LIST IS CAPPED SO A CYCLE WIDE     *
      *        FAILURE DOES NOT PRODUCE A MILLION PAGES.               *
      ******************************************************************
       3300-PRINT-EXCEPTION.
           IF WS-LISTED-CNT >= WS-MAX-EXCEPTIONS
               GO TO 3300-EXIT
           END-IF
      *
           IF WS-LINE-CNT > 55
               PERFORM 7500-PAGE-HEAD
           END-IF
      *
           MOVE BW-ACCT-ID                 TO RD-ACCT-ID
           MOVE BW-CUST-ID                 TO RD-CUST-ID
           MOVE BW-PRODUCT-CD              TO RD-PRODUCT
           WRITE RECONRPT-REC FROM RPT-DETAIL
           ADD 1                           TO WS-LINE-CNT
                                              WS-LISTED-CNT
           .
       3300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - THE CYCLE LEVEL PROOF                                   *
      ******************************************************************
       4000-PROVE-CYCLE.
           MOVE '4000-PROVE-CYCLE'         TO ER-PARAGRAPH
      *
           COMPUTE WS-PROOF-DIFF = WS-STMT-BILLED - WS-ACCT-BILLED
      *
           EVALUATE TRUE
               WHEN WS-MISSING-CNT > ZERO
                   MOVE 'Y'                TO WS-PROOF-FAILED-SW
               WHEN FUNCTION ABS(WS-PROOF-DIFF) > WS-BAL-TOLERANCE
                   MOVE 'Y'                TO WS-PROOF-FAILED-SW
               WHEN WS-STMT-ROW-CNT NOT = WS-WORK-CNT
                   MOVE 'Y'                TO WS-WARNING-SW
               WHEN WS-MISMATCH-CNT > ZERO
                   MOVE 'Y'                TO WS-WARNING-SW
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
       5000-PRINT-SUMMARY.
           IF WS-LINE-CNT > 45
               PERFORM 7500-PAGE-HEAD
           END-IF
      *
           WRITE RECONRPT-REC FROM RPT-BLANK
           MOVE SPACES                     TO RT-TEXT
           MOVE ZERO                       TO RT-AMOUNT
      *
           MOVE 'ACCOUNTS SELECTED BY CBBIL01'
                                           TO RT-TEXT
           MOVE WS-WORK-CNT                TO RT-COUNT
           MOVE ZERO                       TO RT-AMOUNT
           WRITE RECONRPT-REC FROM RPT-TOTAL
      *
           MOVE 'STATEMENT ROWS FOR THE CYCLE'
                                           TO RT-TEXT
           MOVE WS-STMT-ROW-CNT            TO RT-COUNT
           MOVE WS-STMT-BILLED             TO RT-AMOUNT
           WRITE RECONRPT-REC FROM RPT-TOTAL
      *
           MOVE 'ACCOUNTS CYCLED ON THE MASTER'
                                           TO RT-TEXT
           MOVE WS-ACCT-ROW-CNT            TO RT-COUNT
           MOVE WS-ACCT-BILLED             TO RT-AMOUNT
           WRITE RECONRPT-REC FROM RPT-TOTAL
      *
           MOVE 'BALANCE PROOF DIFFERENCE'  TO RT-TEXT
           MOVE ZERO                       TO RT-COUNT
           COMPUTE WS-PROOF-DIFF = WS-STMT-BILLED - WS-ACCT-BILLED
           MOVE WS-PROOF-DIFF              TO RT-AMOUNT
           WRITE RECONRPT-REC FROM RPT-TOTAL
      *
           MOVE 'TOTAL MINIMUM PAYMENTS'   TO RT-TEXT
           MOVE ZERO                       TO RT-COUNT
           MOVE WS-STMT-MIN-PAY            TO RT-AMOUNT
           WRITE RECONRPT-REC FROM RPT-TOTAL
      *
           MOVE 'TOTAL FEES BILLED'        TO RT-TEXT
           MOVE ZERO                       TO RT-COUNT
           MOVE WS-STMT-FEES               TO RT-AMOUNT
           WRITE RECONRPT-REC FROM RPT-TOTAL
      *
           MOVE 'ACCOUNTS WITH NO STATEMENT' TO RT-TEXT
           MOVE WS-MISSING-CNT             TO RT-COUNT
           MOVE ZERO                       TO RT-AMOUNT
           WRITE RECONRPT-REC FROM RPT-TOTAL
      *
           MOVE 'ACCOUNTS OUT OF AGREEMENT' TO RT-TEXT
           MOVE WS-MISMATCH-CNT            TO RT-COUNT
           MOVE ZERO                       TO RT-AMOUNT
           WRITE RECONRPT-REC FROM RPT-TOTAL
      *
           IF WS-EXCEPTION-CNT > WS-LISTED-CNT
               MOVE 'EXCEPTIONS NOT LISTED - LIST CAPPED'
                                           TO RT-TEXT
               COMPUTE RT-COUNT = WS-EXCEPTION-CNT - WS-LISTED-CNT
               MOVE ZERO                   TO RT-AMOUNT
               WRITE RECONRPT-REC FROM RPT-TOTAL
           END-IF
           .
      *
      ******************************************************************
      * 6000 - CLOSE THE CYCLE IN CYCLCTL                              *
      ******************************************************************
       6000-CLOSE-CYCLE.
           MOVE '6000-CLOSE-CYCLE'         TO ER-PARAGRAPH
           MOVE SPACES                     TO CYCLE-CTRL-RECORD
           MOVE 'CARDBILL'                 TO CC-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CC-CYCLE-DATE
           MOVE CC-KEY                     TO CTL-KEY
      *
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'CYCLE CONTROL RECORD MISSING'
                                           TO ER-MESSAGE
                   MOVE 'VSAM'             TO ER-ERROR-TYPE
                   PERFORM 9500-ABEND
           END-READ
      *
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
      *
           MOVE WS-STMT-ROW-CNT            TO CC-RECS-WRITTEN
           MOVE WS-WORK-CNT                TO CC-RECS-READ
           MOVE WS-EXCEPTION-CNT           TO CC-RECS-REJECTED
           MOVE WS-STMT-BILLED             TO CC-TOTAL-DR-AMT
           MOVE WS-STMT-MIN-PAY            TO CC-TOTAL-CR-AMT
           MOVE 'CBBIL06 '                 TO CC-CURRENT-STEP
           PERFORM 7000-GET-TIMESTAMP
           MOVE WS-SQL-TS                  TO CC-END-TS
      *
           IF WS-PROOF-FAILED
               MOVE 'F'                    TO CC-STATUS
               MOVE 'CBBIL05 '             TO CC-LAST-GOOD-STEP
               DISPLAY 'CBBIL06 PROOF FAILED - CYCLE LEFT OPEN'
           ELSE
               MOVE 'C'                    TO CC-STATUS
               MOVE 'CBBIL06 '             TO CC-LAST-GOOD-STEP
               DISPLAY 'CBBIL06 CYCLE ' WS-CYCLE-ID ' COMPLETE'
           END-IF
      *
           MOVE CC-KEY                     TO CTL-KEY
           REWRITE CYCLCTL-REC FROM CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'CONTROL REWRITE FAILED'
                                           TO ER-MESSAGE
                   MOVE 'VSAM'             TO ER-ERROR-TYPE
                   PERFORM 9500-ABEND
           END-REWRITE
           .
       6000-EXIT.
           EXIT
           .
      *
       7000-GET-TIMESTAMP.
           EXEC SQL
               SET :WS-SQL-TS = CHAR(CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE ALL '0'                TO WS-SQL-TS
           END-IF
           .
      *
       7200-FORMAT-ISO-DATE.
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           .
      *
       7500-PAGE-HEAD.
           ADD 1                           TO WS-PAGE-NBR
           MOVE WS-PAGE-NBR                TO RH1-PAGE
           MOVE WS-CYCLE-DATE              TO RH1-CYCLE-DATE
           MOVE WS-CYCLE-ID                TO RH2-CYCLE-ID
           PERFORM 7000-GET-TIMESTAMP
           MOVE WS-SQL-TS                  TO RH2-TIMESTAMP
      *
           WRITE RECONRPT-REC FROM RPT-HEAD1
           WRITE RECONRPT-REC FROM RPT-HEAD2
           WRITE RECONRPT-REC FROM RPT-HEAD3
           MOVE 4                          TO WS-LINE-CNT
           .
      *
       8000-CLOSE-DOWN.
           EXEC SQL COMMIT END-EXEC
           CLOSE BILLWRK-FILE
                 CYCLCTL-FILE
                 RECONRPT-FILE
           .
      *
       9400-SQL-ERROR.
           MOVE 'CBBIL06 '                 TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE 'E'                        TO ER-SEVERITY
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBBIL06 SQL ERROR SQLCODE=' SQLCODE
                   ' TABLE=' ER-SQL-TABLE
           .
      *
       9500-FATAL-ERROR.
           MOVE 'CBBIL06 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL06 FATAL - ' ER-MESSAGE
           .
      *
       9500-ABEND.
           MOVE 'CBBIL06 '                 TO ER-PGM-NAME
           MOVE 'U806'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL06 ABEND U0806 - ' ER-MESSAGE
           CALL 'CBCRD91' USING ERROR-AREA
           .
      *
       9900-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO RETURN-CODE
               WHEN WS-PROOF-FAILED
                   MOVE 8                  TO RETURN-CODE
               WHEN WS-WARNING
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   MOVE 0                  TO RETURN-CODE
           END-EVALUATE
           .
