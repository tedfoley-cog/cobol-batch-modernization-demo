      ******************************************************************
      * CBBIL02 - CARDBILL STEP 02 - STATEMENT ASSEMBLY                *
      *                                                                *
      * FOR EVERY ACCOUNT ON THE BILLING WORK FILE THE POSTED          *
      * TRANSACTIONS OF THE CYCLE PERIOD ARE READ AND TURNED INTO A    *
      * STATEMENT RECORD - THE OCCURS DEPENDING ON LINE ARRAY OF       *
      * CVSTMT01Y - WITH THE OPENING AND CLOSING BALANCES AND THE      *
      * PURCHASE, CASH, PAYMENT, FEE AND INTEREST BUCKETS.             *
      *                                                                *
      * THE LINE ARRAY HOLDS 300 OCCURRENCES.  AN ACCOUNT WITH MORE    *
      * ACTIVITY THAN THAT GETS THE FIRST 299 LINES AND A SUMMARY      *
      * LINE FOR THE REMAINDER - THE BUCKETS STILL CARRY EVERY         *
      * TRANSACTION SO THE STATEMENT STILL BALANCES.                   *
      *                                                                *
      * CALLED BY   - JOB CBBIL02J, STEP ASSEMBLE                      *
      * CALLS       - CBCRD91 (FATAL ERROR / ABEND HANDLER)            *
      * READS       - BILLWRK, CARDSVC.TRANSACTION, CARDSVC.REWARDS    *
      * WRITES      - STMTWRK (STMT-RECORD, VB 24208)                  *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - ALL STATEMENTS ASSEMBLED                                *
      *   04 - ONE OR MORE ACCOUNTS OUT OF BALANCE - SEE SYSOUT        *
      *   12 - FATAL                                                   *
      * ABEND U0802 - UNRECOVERABLE FILE OR SQL CONDITION              *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBBIL02.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILLWRK-FILE ASSIGN TO BILLWRK
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-BILLWRK-STATUS.
      *
           SELECT STMTWRK-FILE ASSIGN TO STMTWRK
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTWRK-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  BILLWRK-FILE
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  BILLWRK-REC                     PIC X(200).
      *
      *    THE STATEMENT IMAGE IS VARIABLE - ITS LENGTH FOLLOWS THE
      *    LINE COUNT.  204 BYTES OF FIXED PART AND TRAILER PLUS 80
      *    BYTES A LINE.
       FD  STMTWRK-FILE
           RECORD IS VARYING IN SIZE FROM 284 TO 24204 CHARACTERS
               DEPENDING ON WS-STMT-LEN
           BLOCK CONTAINS 0 RECORDS.
       01  STMTWRK-REC                     PIC X(24204).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBBIL02 '.
      *
       01  WS-FILE-STATUS-AREA.
           05  WS-BILLWRK-STATUS           PIC X(2)  VALUE '00'.
               88  WS-BILLWRK-OK                     VALUE '00'.
               88  WS-BILLWRK-EOF                    VALUE '10'.
           05  WS-STMTWRK-STATUS           PIC X(2)  VALUE '00'.
               88  WS-STMTWRK-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-WORK-EOF-SW              PIC X     VALUE 'N'.
               88  WS-WORK-EOF                       VALUE 'Y'.
           05  WS-TXN-EOF-SW               PIC X     VALUE 'N'.
               88  WS-TXN-EOF                        VALUE 'Y'.
           05  WS-SYSIN-EOF-SW             PIC X     VALUE 'N'.
               88  WS-SYSIN-EOF                      VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
           05  WS-OUT-OF-BAL-SW            PIC X     VALUE 'N'.
               88  WS-OUT-OF-BAL                     VALUE 'Y'.
      *
       01  WS-CONTROL-CARDS.
           05  WS-CYCLE-DATE               PIC 9(8)  VALUE ZERO.
           05  WS-CYCLE-ID                 PIC X(8)  VALUE SPACES.
           05  WS-COMMIT-FREQ              PIC 9(6)  VALUE ZERO.
           05  WS-BAL-TOLERANCE            PIC S9(5)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-MAX-LINES                PIC 9(4)  VALUE 0300.
      *
       01  WS-CARD-IMAGE.
           05  WS-CARD-KEYWORD             PIC X(12).
           05  FILLER                      PIC X.
           05  WS-CARD-VALUE               PIC X(20).
           05  FILLER                      PIC X(47).
      *
       01  WS-COUNTERS.
           05  WS-ACCT-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-STMT-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-TXN-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-TRUNC-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-BAL-ERR-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
      *
       01  WS-STMT-LEN                     PIC S9(8) COMP VALUE 284.
       01  WS-LINE-IDX                     PIC S9(4) COMP VALUE ZERO.
       01  WS-OVERFLOW-CNT                 PIC 9(6)  VALUE ZERO.
       01  WS-OVERFLOW-AMT                 PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-ACCUMULATORS.
           05  WS-PURCHASES                PIC S9(11)V99 COMP-3.
           05  WS-CASH-ADV                 PIC S9(11)V99 COMP-3.
           05  WS-PAYMENTS                 PIC S9(11)V99 COMP-3.
           05  WS-FEES                     PIC S9(9)V99  COMP-3.
           05  WS-INTEREST                 PIC S9(9)V99  COMP-3.
           05  WS-ADJUSTMENTS              PIC S9(11)V99 COMP-3.
           05  WS-CLOSE-BAL                PIC S9(11)V99 COMP-3.
           05  WS-PROVED-BAL               PIC S9(11)V99 COMP-3.
           05  WS-BAL-DIFF                 PIC S9(11)V99 COMP-3.
           05  WS-AVAIL-CREDIT             PIC S9(11)V99 COMP-3.
      *
       01  WS-RUN-TOTALS.
           05  WS-RUN-PURCHASES            PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-RUN-PAYMENTS             PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-RUN-CLOSE-BAL            PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-DISP-AMT                     PIC ---,---,---,--9.99.
       01  WS-DISP-CNT                     PIC ZZZ,ZZZ,ZZ9.
       01  WS-SQL-TS                       PIC X(26) VALUE SPACES.
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
       01  WS-ISO-FROM                     PIC X(10) VALUE SPACES.
       01  WS-ISO-TO                       PIC X(10) VALUE SPACES.
      *
           COPY CVCONSTY.
           COPY CVSTMT01Y.
           COPY CVERRS01Y.
           COPY CVBWRK1Y.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-TXN.
           05  DCL-TXN-ID                  PIC X(16).
           05  DCL-POST-DATE               PIC X(10).
           05  DCL-TXN-TYPE-CD             PIC X(4).
           05  DCL-TXN-SOURCE              PIC X(2).
           05  DCL-TXN-AMT                 PIC S9(11)V99 COMP-3.
           05  DCL-BILLING-AMT             PIC S9(11)V99 COMP-3.
           05  DCL-CURRENCY-CD             PIC X(3).
           05  DCL-TXN-DESC                PIC X(40).
           05  DCL-MERCHANT-ID             PIC X(15).
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-DISPUTE-FLG             PIC X(1).
      *
       01  DCL-REWARD.
           05  DCL-CLOSE-POINTS            PIC S9(11) COMP-3.
      *
       01  DCL-P-ACCT-ID                   PIC S9(11) COMP-3.
       01  DCL-P-FROM-DATE                 PIC X(10).
       01  DCL-P-TO-DATE                   PIC X(10).
       01  DCL-P-CYCLE-DATE                PIC X(10).
      *
       01  IND-MERCHANT                    PIC S9(4) COMP.
       01  IND-POINTS                      PIC S9(4) COMP.
      *
      ******************************************************************
      * TRANSACTIONS OF THE CYCLE PERIOD IN POSTING ORDER.  DISPUTED   *
      * ITEMS ARE STILL STATED - THE PROVISIONAL CREDIT POSTS AS ITS   *
      * OWN TRANSACTION.                                               *
      ******************************************************************
           EXEC SQL DECLARE STMTXCSR CURSOR FOR
               SELECT TXN_ID
                    , CHAR(POST_DATE, ISO)
                    , TXN_TYPE_CD
                    , TXN_SOURCE
                    , TXN_AMT
                    , BILLING_AMT
                    , CURRENCY_CD
                    , TXN_DESC
                    , MERCHANT_ID
                    , CARD_NUM
                    , DISPUTE_FLG
                 FROM CARDSVC.TRANSACTION
                WHERE ACCT_ID   = :DCL-P-ACCT-ID
                  AND POST_DATE > DATE(:DCL-P-FROM-DATE)
                  AND POST_DATE <= DATE(:DCL-P-TO-DATE)
                ORDER BY POST_DATE, TXN_ID
                WITH UR
           END-EXEC.
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
           PERFORM 2000-READ-WORK-RECORD
           PERFORM UNTIL WS-WORK-EOF
                      OR WS-FATAL
               ADD 1                       TO WS-ACCT-CNT
               PERFORM 3000-BUILD-STATEMENT
               PERFORM 2000-READ-WORK-RECORD
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
      ******************************************************************
      * 1000 - INITIALISATION                                          *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'CBBIL02 '                 TO ER-PGM-NAME
           MOVE '1000-INITIALISE'          TO ER-PARAGRAPH
           MOVE WS-COMMIT-FREQUENCY        TO WS-COMMIT-FREQ
      *
           PERFORM 1100-READ-SYSIN
           IF WS-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           OPEN INPUT  BILLWRK-FILE
           IF NOT WS-BILLWRK-OK
               MOVE 'BILLWRK '             TO ER-FILE-NAME
               MOVE WS-BILLWRK-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'WORK FILE OPEN FAILED'
                                           TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
               GO TO 1000-EXIT
           END-IF
      *
           OPEN OUTPUT STMTWRK-FILE
           IF NOT WS-STMTWRK-OK
               MOVE 'STMTWRK '             TO ER-FILE-NAME
               MOVE WS-STMTWRK-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STATEMENT FILE OPEN FAILED'
                                           TO ER-MESSAGE
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
                       WHEN 'MAX-LINES  '
                           MOVE WS-CARD-VALUE(1:4)
                                           TO WS-MAX-LINES
                       WHEN 'BAL-TOL    '
                           COMPUTE WS-BAL-TOLERANCE =
                                   FUNCTION NUMVAL(WS-CARD-VALUE)
                       WHEN '*          '
                           CONTINUE
                       WHEN OTHER
                           DISPLAY 'CBBIL02 BAD CONTROL CARD - '
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
      *
           IF WS-MAX-LINES > 300
               MOVE 300                    TO WS-MAX-LINES
           END-IF
           .
      *
       2000-READ-WORK-RECORD.
           READ BILLWRK-FILE INTO BILL-WORK-RECORD
               AT END
                   MOVE 'Y'                TO WS-WORK-EOF-SW
           END-READ
      *
           IF NOT WS-WORK-EOF
              AND NOT WS-BILLWRK-OK
               MOVE 'BILLWRK '             TO ER-FILE-NAME
               MOVE WS-BILLWRK-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'WORK FILE READ FAILED'
                                           TO ER-MESSAGE
               PERFORM 9500-ABEND
           END-IF
           .
      *
      ******************************************************************
      * 3000 - ONE STATEMENT                                           *
      ******************************************************************
       3000-BUILD-STATEMENT.
           MOVE '3000-BUILD-STATEMENT'     TO ER-PARAGRAPH
           PERFORM 3100-INIT-STATEMENT
           PERFORM 3200-READ-TRANSACTIONS
           PERFORM 3600-GET-REWARD-POINTS
           PERFORM 3700-CLOSE-BALANCES
           PERFORM 3800-WRITE-STATEMENT
      *
           ADD 1                           TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
               EXEC SQL COMMIT END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
           .
      *
       3100-INIT-STATEMENT.
           MOVE SPACES                     TO STMT-RECORD
           MOVE ZERO                       TO WS-PURCHASES
                                              WS-CASH-ADV
                                              WS-PAYMENTS
                                              WS-FEES
                                              WS-INTEREST
                                              WS-ADJUSTMENTS
                                              WS-OVERFLOW-CNT
                                              WS-OVERFLOW-AMT
                                              WS-LINE-IDX
           MOVE 'N'                        TO WS-TXN-EOF-SW
      *
           MOVE BW-ACCT-ID                 TO STMT-ACCT-ID
           MOVE BW-CYCLE-DATE              TO STMT-CYCLE-DATE
           MOVE BW-STMT-NUMBER             TO STMT-NUMBER
           MOVE BW-CUST-ID                 TO STMT-CUST-ID
      *    THE DELIVERY PREFERENCE IS NOT KNOWN UNTIL CBBIL04 READS
      *    CUSTPREF - THE STATEMENT IS ASSEMBLED AS PAPER AND THE
      *    FORMAT CODE IS OVERWRITTEN THERE.
           MOVE 'PAPR'                     TO STMT-FORMAT-CD
           MOVE BW-PERIOD-FROM             TO STMT-PERIOD-FROM
           MOVE BW-PERIOD-TO               TO STMT-PERIOD-TO
           MOVE ZERO                       TO STMT-DUE-DATE
           MOVE BW-OPEN-BAL                TO STMT-OPEN-BAL
           MOVE BW-CREDIT-LIMIT            TO STMT-CREDIT-LIMIT
           MOVE BW-APR-PCT                 TO STMT-APR
           MOVE ZERO                       TO STMT-MIN-PAY
                                              STMT-REWARD-PTS
           MOVE 1                          TO STMT-LINE-CNT
           .
      *
      ******************************************************************
      * 3200 - TRANSACTION CURSOR FOR THE PERIOD                       *
      ******************************************************************
       3200-READ-TRANSACTIONS.
           MOVE BW-ACCT-ID                 TO DCL-P-ACCT-ID
           MOVE BW-PREV-CYCLE-DT           TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-P-FROM-DATE
           MOVE BW-PERIOD-TO               TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-P-TO-DATE
      *
           EXEC SQL OPEN STMTXCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'TRANSACTION       '   TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               PERFORM 9400-SQL-ERROR
               PERFORM 9500-ABEND
               GO TO 3200-EXIT
           END-IF
      *
           PERFORM UNTIL WS-TXN-EOF
               PERFORM 3300-FETCH-TRANSACTION
               IF NOT WS-TXN-EOF
                   ADD 1                   TO WS-TXN-CNT
                   PERFORM 3400-BUCKET-TRANSACTION
                   PERFORM 3500-ADD-LINE
               END-IF
           END-PERFORM
      *
           EXEC SQL CLOSE STMTXCSR END-EXEC
           .
       3200-EXIT.
           EXIT
           .
      *
       3300-FETCH-TRANSACTION.
           EXEC SQL
               FETCH STMTXCSR
                INTO :DCL-TXN-ID
                   , :DCL-POST-DATE
                   , :DCL-TXN-TYPE-CD
                   , :DCL-TXN-SOURCE
                   , :DCL-TXN-AMT
                   , :DCL-BILLING-AMT
                   , :DCL-CURRENCY-CD
                   , :DCL-TXN-DESC
                   , :DCL-MERCHANT-ID :IND-MERCHANT
                   , :DCL-CARD-NUM
                   , :DCL-DISPUTE-FLG
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   MOVE 'Y'                TO WS-TXN-EOF-SW
               WHEN OTHER
                   MOVE 'TRANSACTION       ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '         TO ER-SQL-OPERATION
                   PERFORM 9400-SQL-ERROR
                   PERFORM 9500-ABEND
                   MOVE 'Y'                TO WS-TXN-EOF-SW
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3400 - CATEGORY BUCKETS.  THE BILLING AMOUNT IS ALWAYS USED -  *
      *        IT IS THE ACCOUNT CURRENCY AMOUNT AFTER CONVERSION.     *
      ******************************************************************
       3400-BUCKET-TRANSACTION.
           EVALUATE DCL-TXN-TYPE-CD
               WHEN 'PURC'
               WHEN 'RECU'
                   ADD DCL-BILLING-AMT     TO WS-PURCHASES
               WHEN 'CASH'
               WHEN 'QCSH'
                   ADD DCL-BILLING-AMT     TO WS-CASH-ADV
               WHEN 'PYMT'
                   ADD DCL-BILLING-AMT     TO WS-PAYMENTS
               WHEN 'FEE '
               WHEN 'ANNU'
               WHEN 'LATE'
               WHEN 'OVLM'
               WHEN 'FRGN'
                   ADD DCL-BILLING-AMT     TO WS-FEES
               WHEN 'INTR'
                   ADD DCL-BILLING-AMT     TO WS-INTEREST
               WHEN 'RFND'
               WHEN 'ADJT'
               WHEN 'CHGB'
                   ADD DCL-BILLING-AMT     TO WS-ADJUSTMENTS
               WHEN OTHER
                   ADD DCL-BILLING-AMT     TO WS-ADJUSTMENTS
                   DISPLAY 'CBBIL02 UNCLASSIFIED TXN TYPE '
                           DCL-TXN-TYPE-CD ' TXN ' DCL-TXN-ID
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3500 - ADD A LINE TO THE ODO ARRAY.  ONCE THE ARRAY IS ONE     *
      *        SHORT OF FULL THE REMAINING ITEMS ARE COUNTED AND       *
      *        SUMMARISED ONTO THE LAST LINE.                          *
      ******************************************************************
       3500-ADD-LINE.
           IF WS-LINE-IDX >= WS-MAX-LINES - 1
               ADD 1                       TO WS-OVERFLOW-CNT
               ADD DCL-BILLING-AMT         TO WS-OVERFLOW-AMT
               GO TO 3500-EXIT
           END-IF
      *
           ADD 1                           TO WS-LINE-IDX
           MOVE WS-LINE-IDX                TO STMT-LINE-CNT
      *
           MOVE DCL-POST-DATE              TO WS-ISO-DATE
           MOVE WS-ISO-YYYY                TO WS-DW-YYYY
           MOVE WS-ISO-MM                  TO WS-DW-MM
           MOVE WS-ISO-DD                  TO WS-DW-DD
           MOVE WS-DATE-WORK               TO STMT-LN-DATE(WS-LINE-IDX)
                                        STMT-LN-POST-DATE(WS-LINE-IDX)
      *
           IF IND-MERCHANT < ZERO
               MOVE DCL-TXN-DESC           TO STMT-LN-DESC(WS-LINE-IDX)
           ELSE
               STRING DCL-TXN-DESC(1:24)   DELIMITED BY SIZE
                      ' '                  DELIMITED BY SIZE
                      DCL-MERCHANT-ID      DELIMITED BY SIZE
                 INTO STMT-LN-DESC(WS-LINE-IDX)
               END-STRING
           END-IF
      *
           MOVE DCL-TXN-ID                 TO STMT-LN-REF(WS-LINE-IDX)
           MOVE DCL-BILLING-AMT            TO STMT-LN-AMT(WS-LINE-IDX)
      *
           IF DCL-BILLING-AMT < ZERO
               MOVE 'C'                    TO STMT-LN-DR-CR
                                              (WS-LINE-IDX)
           ELSE
               MOVE 'D'                    TO STMT-LN-DR-CR
                                              (WS-LINE-IDX)
           END-IF
      *
      *    A DISPUTED ITEM IS FLAGGED IN THE DESCRIPTION - THE PAPER
      *    FORMATTER PRINTS IT UNDER THE DISPUTE HEADING.
           IF DCL-DISPUTE-FLG = 'Y'
               MOVE '*DISPUTED*'
                 TO STMT-LN-DESC(WS-LINE-IDX)(31:10)
           END-IF
           .
       3500-EXIT.
           EXIT
           .
      *
       3600-GET-REWARD-POINTS.
           MOVE BW-ACCT-ID                 TO DCL-P-ACCT-ID
           MOVE BW-CYCLE-DATE              TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-P-CYCLE-DATE
      *
           EXEC SQL
               SELECT SUM(CLOSE_POINTS)
                 INTO :DCL-CLOSE-POINTS :IND-POINTS
                 FROM CARDSVC.REWARDS
                WHERE ACCT_ID    = :DCL-P-ACCT-ID
                  AND CYCLE_DATE = DATE(:DCL-P-CYCLE-DATE)
           END-EXEC
      *
           EVALUATE TRUE
               WHEN SQLCODE = 0 AND IND-POINTS NOT < ZERO
                   MOVE DCL-CLOSE-POINTS   TO STMT-REWARD-PTS
               WHEN SQLCODE = 0
               WHEN SQLCODE = 100
                   MOVE ZERO               TO STMT-REWARD-PTS
               WHEN OTHER
                   MOVE 'REWARDS           ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '         TO ER-SQL-OPERATION
                   PERFORM 9400-SQL-ERROR
                   MOVE ZERO               TO STMT-REWARD-PTS
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3700 - CLOSING BALANCE AND THE BALANCE PROOF                   *
      *                                                                *
      *   OPENING + PURCHASES + CASH + FEES + INTEREST                 *
      *           + ADJUSTMENTS - PAYMENTS = CLOSING                   *
      *                                                                *
      * PAYMENTS ARE HELD AS NEGATIVE AMOUNTS BY THE POSTING CYCLE SO  *
      * THEY ARE ADDED, NOT SUBTRACTED.  THE RESULT IS COMPARED WITH   *
      * THE ACCOUNT BALANCE SNAPSHOT TAKEN BY CBBIL01.                 *
      ******************************************************************
       3700-CLOSE-BALANCES.
           COMPUTE WS-CLOSE-BAL = BW-OPEN-BAL
                                + WS-PURCHASES
                                + WS-CASH-ADV
                                + WS-FEES
                                + WS-INTEREST
                                + WS-ADJUSTMENTS
                                + WS-PAYMENTS
      *
           COMPUTE WS-BAL-DIFF = WS-CLOSE-BAL - BW-CURR-BAL
           IF FUNCTION ABS(WS-BAL-DIFF) > WS-BAL-TOLERANCE
               ADD 1                       TO WS-BAL-ERR-CNT
               MOVE 'Y'                    TO WS-OUT-OF-BAL-SW
               MOVE WS-BAL-DIFF            TO WS-DISP-AMT
               DISPLAY 'CBBIL02 OUT OF BALANCE ACCT ' BW-ACCT-ID
                       ' DIFF ' WS-DISP-AMT
           END-IF
      *
           COMPUTE WS-AVAIL-CREDIT = BW-CREDIT-LIMIT - WS-CLOSE-BAL
           IF WS-AVAIL-CREDIT < ZERO
               MOVE ZERO                   TO WS-AVAIL-CREDIT
           END-IF
      *
           MOVE WS-CLOSE-BAL               TO STMT-CLOSE-BAL
           MOVE WS-PURCHASES               TO STMT-PURCHASES
           MOVE WS-CASH-ADV                TO STMT-CASH-ADV
           MOVE WS-PAYMENTS                TO STMT-PAYMENTS
           MOVE WS-FEES                    TO STMT-FEES
           MOVE WS-INTEREST                TO STMT-INTEREST
           MOVE WS-AVAIL-CREDIT            TO STMT-AVAIL-CREDIT
      *
           ADD WS-PURCHASES                TO WS-RUN-PURCHASES
           ADD WS-PAYMENTS                 TO WS-RUN-PAYMENTS
           ADD WS-CLOSE-BAL                TO WS-RUN-CLOSE-BAL
           .
      *
      ******************************************************************
      * 3800 - WRITE THE VARIABLE LENGTH STATEMENT                     *
      *                                                                *
      * AN ACCOUNT WITH NO ACTIVITY STILL GETS A STATEMENT - THE ODO   *
      * MINIMUM IS ONE OCCURRENCE, FILLED WITH A NO ACTIVITY LINE.     *
      ******************************************************************
       3800-WRITE-STATEMENT.
           IF WS-LINE-IDX = ZERO
               MOVE 1                      TO WS-LINE-IDX
                                              STMT-LINE-CNT
               MOVE BW-PERIOD-TO           TO STMT-LN-DATE(1)
                                              STMT-LN-POST-DATE(1)
               MOVE 'NO ACTIVITY THIS STATEMENT PERIOD'
                                           TO STMT-LN-DESC(1)
               MOVE SPACES                 TO STMT-LN-REF(1)
               MOVE ZERO                   TO STMT-LN-AMT(1)
               MOVE 'D'                    TO STMT-LN-DR-CR(1)
           END-IF
      *
           IF WS-OVERFLOW-CNT > ZERO
               ADD 1                       TO WS-LINE-IDX
               MOVE WS-LINE-IDX            TO STMT-LINE-CNT
               MOVE BW-PERIOD-TO           TO STMT-LN-DATE
                                              (WS-LINE-IDX)
                                              STMT-LN-POST-DATE
                                              (WS-LINE-IDX)
               MOVE SPACES                 TO STMT-LN-DESC
                                              (WS-LINE-IDX)
               STRING 'FURTHER ITEMS NOT LISTED - COUNT '
                                           DELIMITED BY SIZE
                      WS-OVERFLOW-CNT      DELIMITED BY SIZE
                 INTO STMT-LN-DESC(WS-LINE-IDX)
               END-STRING
               MOVE 'OVERFLOW        '     TO STMT-LN-REF
                                              (WS-LINE-IDX)
               MOVE WS-OVERFLOW-AMT        TO STMT-LN-AMT
                                              (WS-LINE-IDX)
               MOVE 'D'                    TO STMT-LN-DR-CR
                                              (WS-LINE-IDX)
               ADD 1                       TO WS-TRUNC-CNT
           END-IF
      *
           MOVE 'CBBIL02 '                 TO STMT-GEN-PGM
           PERFORM 7000-GET-TIMESTAMP
           MOVE WS-SQL-TS                  TO STMT-GEN-TS
           MOVE ZERO                       TO STMT-PAGE-CNT
           MOVE SPACES                     TO STMT-TRAILER-FILLER
      *
           COMPUTE WS-STMT-LEN = 204 + (80 * STMT-LINE-CNT)
      *
           WRITE STMTWRK-REC FROM STMT-RECORD
           IF NOT WS-STMTWRK-OK
               MOVE 'STMTWRK '             TO ER-FILE-NAME
               MOVE WS-STMTWRK-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STATEMENT WRITE FAILED'
                                           TO ER-MESSAGE
               PERFORM 9500-ABEND
           END-IF
      *
           ADD 1                           TO WS-STMT-CNT
           .
      *
      ******************************************************************
      * 7000 - COMMON SERVICES                                         *
      ******************************************************************
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
       8000-CLOSE-DOWN.
           EXEC SQL COMMIT END-EXEC
           CLOSE BILLWRK-FILE
                 STMTWRK-FILE
           .
      *
       8500-REPORT-TOTALS.
           DISPLAY '*---------------------------------------------*'
           DISPLAY '* CBBIL02 STATEMENT ASSEMBLY                  *'
           DISPLAY '*---------------------------------------------*'
           DISPLAY '  CYCLE ID          ' WS-CYCLE-ID
           DISPLAY '  CYCLE DATE        ' WS-CYCLE-DATE
           MOVE WS-ACCT-CNT                TO WS-DISP-CNT
           DISPLAY '  ACCOUNTS READ     ' WS-DISP-CNT
           MOVE WS-STMT-CNT                TO WS-DISP-CNT
           DISPLAY '  STATEMENTS BUILT  ' WS-DISP-CNT
           MOVE WS-TXN-CNT                 TO WS-DISP-CNT
           DISPLAY '  TRANSACTIONS READ ' WS-DISP-CNT
           MOVE WS-TRUNC-CNT               TO WS-DISP-CNT
           DISPLAY '  LINE OVERFLOWS    ' WS-DISP-CNT
           MOVE WS-BAL-ERR-CNT             TO WS-DISP-CNT
           DISPLAY '  OUT OF BALANCE    ' WS-DISP-CNT
           MOVE WS-RUN-PURCHASES           TO WS-DISP-AMT
           DISPLAY '  TOTAL PURCHASES   ' WS-DISP-AMT
           MOVE WS-RUN-PAYMENTS            TO WS-DISP-AMT
           DISPLAY '  TOTAL PAYMENTS    ' WS-DISP-AMT
           MOVE WS-RUN-CLOSE-BAL           TO WS-DISP-AMT
           DISPLAY '  TOTAL BILLED      ' WS-DISP-AMT
           .
      *
      ******************************************************************
      * 9000 - ERROR HANDLING                                          *
      ******************************************************************
       9400-SQL-ERROR.
           MOVE 'CBBIL02 '                 TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE 'E'                        TO ER-SEVERITY
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBBIL02 SQL ERROR SQLCODE=' SQLCODE
                   ' TABLE=' ER-SQL-TABLE
                   ' ACCT=' BW-ACCT-ID
           .
      *
       9500-FATAL-ERROR.
           MOVE 'CBBIL02 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL02 FATAL - ' ER-MESSAGE
           .
      *
       9500-ABEND.
           MOVE 'CBBIL02 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'U802'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL02 ABEND U0802 - ' ER-MESSAGE
           CALL 'CBCRD91' USING ERROR-AREA
           .
      *
       9900-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO RETURN-CODE
               WHEN WS-OUT-OF-BAL
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   MOVE 0                  TO RETURN-CODE
           END-EVALUATE
           .
