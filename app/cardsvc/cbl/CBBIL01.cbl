      ******************************************************************
      * CBBIL01 - CARDBILL STEP 01 - CYCLE SELECTION                   *
      *                                                                *
      * SELECTS THE ACCOUNTS WHOSE CYCLE DAY MATCHES THE CYCLE DAY ON  *
      * THE SYSIN CONTROL CARD, SNAPSHOTS THEM ONTO THE BILLING WORK   *
      * FILE AND RECORDS THE START OF THE CYCLE IN VSAM CYCLCTL.       *
      *                                                                *
      * THE PROGRAM IS RESTARTABLE.  ON A RESTART RUN THE CONTROL      *
      * RECORD SUPPLIES THE LAST COMMITTED ACCOUNT NUMBER AND THE      *
      * CURSOR IS REPOSITIONED PAST IT.                                *
      *                                                                *
      * CALLED BY   - JOB CBBIL01J, STEP SELECT                        *
      * CALLS       - CBCRD91 (FATAL ERROR / ABEND HANDLER)            *
      * READS       - CARDSVC.ACCOUNT, CARDSVC.CARD_LIMIT              *
      * UPDATES     - VSAM CYCLCTL                                     *
      * WRITES      - BILLWRK (BILL-WORK-RECORD, FB 200)               *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - CYCLE SELECTED                                          *
      *   04 - SELECTED WITH REJECTS - SEE SYSOUT                      *
      *   12 - FATAL - CYCLE NOT STARTED                               *
      * ABEND U0801 - CONTROL FILE UNUSABLE                            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBBIL01.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CYCLCTL-FILE ASSIGN TO CYCLCTL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CTL-KEY
                  FILE STATUS  IS WS-CYCLCTL-STATUS.
      *
           SELECT BILLWRK-FILE ASSIGN TO BILLWRK
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-BILLWRK-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CYCLCTL-FILE
           RECORD CONTAINS 256 CHARACTERS.
       01  CYCLCTL-REC.
           05  CTL-KEY.
               10  CTL-CYCLE-TYPE          PIC X(8).
               10  CTL-CYCLE-DATE          PIC 9(8).
           05  CTL-REST                    PIC X(240).
      *
       FD  BILLWRK-FILE
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  BILLWRK-REC                     PIC X(200).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBBIL01 '.
       01  WS-ABEND-SELECT                 PIC S9(4) COMP VALUE 0801.
      *
       01  WS-FILE-STATUS-AREA.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
               88  WS-CYCLCTL-NOTFND                 VALUE '23'.
           05  WS-BILLWRK-STATUS           PIC X(2)  VALUE '00'.
               88  WS-BILLWRK-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-END-CURSOR-SW            PIC X     VALUE 'N'.
               88  WS-END-CURSOR                     VALUE 'Y'.
           05  WS-SYSIN-EOF-SW             PIC X     VALUE 'N'.
               88  WS-SYSIN-EOF                      VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
           05  WS-RESTART-SW               PIC X     VALUE 'N'.
               88  WS-RESTART-RUN                    VALUE 'Y'.
      *
      ******************************************************************
      * CONTROL CARD VALUES - NO BUSINESS PARAMETER IS CODED AS A      *
      * LITERAL IN THE PROCEDURE DIVISION.                             *
      ******************************************************************
       01  WS-CONTROL-CARDS.
           05  WS-CYCLE-DATE               PIC 9(8)  VALUE ZERO.
           05  WS-CYCLE-DAY                PIC 9(2)  VALUE ZERO.
           05  WS-CYCLE-ID                 PIC X(8)  VALUE SPACES.
           05  WS-COMMIT-FREQ              PIC 9(6)  VALUE ZERO.
           05  WS-MIN-BAL-TO-BILL          PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-BILL-CLOSED-FLG          PIC X     VALUE 'N'.
               88  WS-BILL-CLOSED                    VALUE 'Y'.
           05  WS-CARD-COUNT               PIC 9(4)  VALUE ZERO.
      *
       01  WS-CARD-IMAGE.
           05  WS-CARD-KEYWORD             PIC X(12).
           05  FILLER                      PIC X.
           05  WS-CARD-VALUE               PIC X(20).
           05  FILLER                      PIC X(47).
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-WRITTEN-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SKIP-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-COMMIT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
      *
       01  WS-TOTALS.
           05  WS-TOTAL-BAL                PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-TOTAL-CASH               PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-HASH-ACCT                PIC S9(15) COMP-3
                                                     VALUE ZERO.
      *
       01  WS-WORK-FIELDS.
           05  WS-LAST-ACCT-ID             PIC 9(11) VALUE ZERO.
           05  WS-PERIOD-FROM              PIC 9(8)  VALUE ZERO.
           05  WS-PERIOD-TO                PIC 9(8)  VALUE ZERO.
           05  WS-DISP-AMT                 PIC ---,---,---,--9.99.
           05  WS-DISP-CNT                 PIC ZZZ,ZZZ,ZZ9.
      *
      *    DATE DECOMPOSITION - THE 1990S HABIT, KEPT BECAUSE THE
      *    PERIOD ARITHMETIC BELOW NEEDS THE PARTS SEPARATELY.
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-YYYY                  PIC 9(4).
           05  WS-DW-YY REDEFINES WS-DW-YYYY.
               10  WS-DW-CC                PIC 9(2).
               10  WS-DW-YEAR              PIC 9(2).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
      *
       01  WS-SQL-TS                       PIC X(26) VALUE SPACES.
      *
           COPY CVCONSTY.
           COPY CVACCT01Y.
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVBWRK1Y.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      ******************************************************************
      * HOST VARIABLES                                                 *
      ******************************************************************
       01  DCL-ACCOUNT.
           05  DCL-ACCT-ID                 PIC S9(11) COMP-3.
           05  DCL-CUST-ID                 PIC S9(9)  COMP-3.
           05  DCL-PARTY-ID                PIC X(11).
           05  DCL-PRODUCT-CD              PIC X(4).
           05  DCL-ACCT-STATUS             PIC X(1).
           05  DCL-CURRENCY-CD             PIC X(3).
           05  DCL-CURR-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-STMT-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-CASH-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-DELQ-BUCKET             PIC S9(4) COMP.
           05  DCL-DELQ-AMT                PIC S9(9)V99 COMP-3.
           05  DCL-CYCLE-DAY               PIC S9(4) COMP.
           05  DCL-STMT-COUNT              PIC S9(9) COMP.
           05  DCL-LAST-CYCLE-DT           PIC X(10).
           05  DCL-OPEN-DATE               PIC X(10).
      *
       01  DCL-LIMIT.
           05  DCL-LIMIT-AMT               PIC S9(11)V99 COMP-3.
           05  DCL-APR-PCT                 PIC S9(3)V9(5) COMP-3.
           05  DCL-CASH-APR-PCT            PIC S9(3)V9(5) COMP-3.
      *
       01  DCL-PREDICATES.
           05  DCL-P-CYCLE-DAY             PIC S9(4) COMP.
           05  DCL-P-LAST-ACCT             PIC S9(11) COMP-3.
           05  DCL-P-MIN-BAL               PIC S9(9)V99 COMP-3.
           05  DCL-P-CYCLE-DATE            PIC X(10).
      *
       01  IND-VARS.
           05  IND-LAST-CYCLE-DT           PIC S9(4) COMP.
           05  IND-LIMIT-AMT               PIC S9(4) COMP.
      *
      *    ISO DATE EDIT AREAS - DB2 DATE COLUMNS ARE HANDLED AS
      *    CHARACTER AND CONVERTED TO THE 8 DIGIT INTERNAL FORM.
       01  WS-ISO-DATE.
           05  WS-ISO-YYYY                 PIC 9(4).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-MM                   PIC 9(2).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-DD                   PIC 9(2).
      *
      ******************************************************************
      * THE SELECTION CURSOR.  ACCOUNTS ARE TAKEN IN ACCOUNT NUMBER    *
      * ORDER SO THAT A RESTART CAN REPOSITION ON THE LAST COMMITTED   *
      * KEY.  THE OUTER JOIN KEEPS ACCOUNTS WITH NO CURRENT CREDIT     *
      * LIMIT ROW - THOSE ARE REPORTED AS REJECTS.                     *
      ******************************************************************
           EXEC SQL DECLARE BILCYCSR CURSOR WITH HOLD FOR
               SELECT A.ACCT_ID
                    , A.CUST_ID
                    , A.PARTY_ID
                    , A.PRODUCT_CD
                    , A.ACCT_STATUS
                    , A.CURRENCY_CD
                    , A.CURR_BAL
                    , A.STMT_BAL
                    , A.CASH_BAL
                    , A.DELQ_BUCKET
                    , A.DELQ_AMT
                    , A.CYCLE_DAY
                    , A.STMT_COUNT
                    , CHAR(A.LAST_CYCLE_DATE, ISO)
                    , CHAR(A.OPEN_DATE, ISO)
                    , L.LIMIT_AMT
                    , L.APR_PCT
                    , L.CASH_APR_PCT
                 FROM CARDSVC.ACCOUNT A
                 LEFT OUTER JOIN CARDSVC.CARD_LIMIT L
                   ON L.CARD_NUM =
                      (SELECT MIN(C.CARD_NUM)
                         FROM CARDSVC.CARD C
                        WHERE C.ACCT_ID = A.ACCT_ID
                          AND C.CARD_STATUS IN ('A','B'))
                  AND L.LIMIT_TYPE = 'CRED'
                  AND DATE(:DCL-P-CYCLE-DATE)
                      BETWEEN L.EFF_DATE AND L.EXP_DATE
                WHERE A.CYCLE_DAY  = :DCL-P-CYCLE-DAY
                  AND A.ACCT_ID    > :DCL-P-LAST-ACCT
                  AND A.ACCT_STATUS IN ('O','S','C')
                ORDER BY A.ACCT_ID
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
           PERFORM 2000-START-CYCLE
           IF WS-FATAL
               GO TO 0000-TERMINATE
           END-IF
      *
           PERFORM 3000-SELECT-ACCOUNTS
           PERFORM 8000-END-CYCLE
           PERFORM 8500-REPORT-TOTALS
           .
       0000-TERMINATE.
           PERFORM 9900-SET-RETURN-CODE
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - OPEN FILES AND READ THE CONTROL CARDS                   *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'CBBIL01 '                 TO ER-PGM-NAME
           MOVE '1000-INITIALISE'          TO ER-PARAGRAPH
      *
           PERFORM 1100-READ-SYSIN
           IF WS-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           OPEN I-O    CYCLCTL-FILE
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'CYCLCTL OPEN FAILED'  TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
               GO TO 1000-EXIT
           END-IF
      *
           OPEN OUTPUT BILLWRK-FILE
           IF NOT WS-BILLWRK-OK
               MOVE 'BILLWRK '             TO ER-FILE-NAME
               MOVE WS-BILLWRK-STATUS      TO ER-FILE-STATUS
               MOVE 'BILLWRK OPEN FAILED'  TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 1100 - SYSIN CONTROL CARDS                                     *
      *                                                                *
      *   CYCLE-DATE   CCYYMMDD                                        *
      *   CYCLE-DAY    NN                                              *
      *   CYCLE-ID     CCCCCCCC                                        *
      *   COMMIT-FREQ  NNNNNN                                          *
      *   MIN-BAL      NNNNNNNNN.NN                                    *
      *   BILL-CLOSED  Y OR N                                          *
      ******************************************************************
       1100-READ-SYSIN.
           MOVE WS-COMMIT-FREQUENCY        TO WS-COMMIT-FREQ
      *
           PERFORM UNTIL WS-SYSIN-EOF
               ACCEPT WS-CARD-IMAGE FROM SYSIN
                   ON EXCEPTION
                       MOVE 'Y'            TO WS-SYSIN-EOF-SW
               END-ACCEPT
               IF NOT WS-SYSIN-EOF
                   ADD 1                   TO WS-CARD-COUNT
                   PERFORM 1150-APPLY-CARD
               END-IF
               IF WS-CARD-COUNT > 50
                   MOVE 'Y'                TO WS-SYSIN-EOF-SW
               END-IF
           END-PERFORM
      *
           IF WS-CYCLE-DATE = ZERO
           OR WS-CYCLE-DAY  = ZERO
           OR WS-CYCLE-ID   = SPACES
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
               WHEN 'CYCLE-DAY  '
                   MOVE WS-CARD-VALUE(1:2) TO WS-CYCLE-DAY
               WHEN 'CYCLE-ID   '
                   MOVE WS-CARD-VALUE(1:8) TO WS-CYCLE-ID
               WHEN 'COMMIT-FREQ'
                   MOVE WS-CARD-VALUE(1:6) TO WS-COMMIT-FREQ
               WHEN 'MIN-BAL    '
                   COMPUTE WS-MIN-BAL-TO-BILL =
                           FUNCTION NUMVAL(WS-CARD-VALUE)
               WHEN 'BILL-CLOSED'
                   MOVE WS-CARD-VALUE(1:1) TO WS-BILL-CLOSED-FLG
               WHEN '*          '
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'CBBIL01 UNKNOWN CONTROL CARD - '
                           WS-CARD-IMAGE
                   ADD 1                   TO WS-REJECT-CNT
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2000 - CLAIM THE CYCLE IN CYCLCTL                              *
      *                                                                *
      * A RECORD ALREADY MARKED RUNNING MEANS A PREVIOUS RUN DIED.     *
      * THAT IS A RESTART, NOT AN ERROR - THE RESTART COUNT IS BUMPED  *
      * AND SELECTION RESUMES AFTER CC-LAST-KEY.                       *
      ******************************************************************
       2000-START-CYCLE.
           MOVE '2000-START-CYCLE'         TO ER-PARAGRAPH
           MOVE SPACES                     TO CYCLE-CTRL-RECORD
           MOVE 'CARDBILL'                 TO CC-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CC-CYCLE-DATE
           MOVE CC-KEY                     TO CTL-KEY
      *
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'N'                TO WS-RESTART-SW
           END-READ
      *
           EVALUATE TRUE
               WHEN WS-CYCLCTL-NOTFND
                   PERFORM 2100-CREATE-CONTROL
               WHEN WS-CYCLCTL-OK
                   PERFORM 2200-RESUME-CONTROL
               WHEN OTHER
                   MOVE 'CYCLCTL '         TO ER-FILE-NAME
                   MOVE WS-CYCLCTL-STATUS  TO ER-FILE-STATUS
                   MOVE 'VSAM'             TO ER-ERROR-TYPE
                   MOVE 'CYCLCTL READ FAILED'
                                           TO ER-MESSAGE
                   PERFORM 9500-FATAL-ERROR
           END-EVALUATE
           .
      *
       2100-CREATE-CONTROL.
           MOVE SPACES                     TO CYCLE-CTRL-RECORD
           MOVE 'CARDBILL'                 TO CC-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CC-CYCLE-DATE
           MOVE WS-CYCLE-ID                TO CC-CYCLE-ID
           MOVE 'R'                        TO CC-STATUS
           MOVE 'CBBIL01 '                 TO CC-CURRENT-STEP
           MOVE SPACES                     TO CC-LAST-GOOD-STEP
           PERFORM 7000-GET-TIMESTAMP
           MOVE WS-SQL-TS                  TO CC-START-TS
           MOVE SPACES                     TO CC-END-TS
           MOVE WS-COMMIT-FREQ             TO CC-COMMIT-FREQ
           MOVE ZERO                       TO CC-RECS-READ
                                              CC-RECS-WRITTEN
                                              CC-RECS-REJECTED
                                              CC-RESTART-CNT
                                              CC-TOTAL-DR-AMT
                                              CC-TOTAL-CR-AMT
                                              CC-HASH-TOTAL
           MOVE SPACES                     TO CC-LAST-KEY
           MOVE 'N'                        TO CC-ONLINE-CLOSED-FLG
      *
           WRITE CYCLCTL-REC FROM CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'CYCLCTL '         TO ER-FILE-NAME
                   MOVE WS-CYCLCTL-STATUS  TO ER-FILE-STATUS
                   MOVE 'CYCLE CONTROL WRITE FAILED'
                                           TO ER-MESSAGE
                   PERFORM 9500-ABEND
           END-WRITE
           .
      *
       2200-RESUME-CONTROL.
           EVALUATE TRUE
               WHEN CC-COMPLETE
                   MOVE 'CYCLE ALREADY COMPLETE FOR THIS DATE'
                                           TO ER-MESSAGE
                   MOVE 'BUSN'             TO ER-ERROR-TYPE
                   PERFORM 9500-FATAL-ERROR
               WHEN CC-RUNNING
               WHEN CC-FAILED
                   MOVE 'Y'                TO WS-RESTART-SW
                   ADD 1                   TO CC-RESTART-CNT
                   MOVE 'S'                TO CC-STATUS
                   MOVE CC-LAST-KEY(1:11)  TO WS-LAST-ACCT-ID
                   DISPLAY 'CBBIL01 RESTART - RESUMING AFTER ACCT '
                           WS-LAST-ACCT-ID
                   PERFORM 7100-REWRITE-CONTROL
               WHEN OTHER
                   MOVE 'R'                TO CC-STATUS
                   MOVE 'CBBIL01 '         TO CC-CURRENT-STEP
                   PERFORM 7100-REWRITE-CONTROL
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - DRIVE THE SELECTION CURSOR                              *
      ******************************************************************
       3000-SELECT-ACCOUNTS.
           MOVE '3000-SELECT-ACCOUNTS'     TO ER-PARAGRAPH
           PERFORM 3050-DERIVE-PERIOD
      *
           MOVE WS-CYCLE-DAY               TO DCL-P-CYCLE-DAY
           MOVE WS-LAST-ACCT-ID            TO DCL-P-LAST-ACCT
           MOVE WS-MIN-BAL-TO-BILL         TO DCL-P-MIN-BAL
           MOVE WS-CYCLE-DATE              TO WS-DATE-WORK
           PERFORM 7200-FORMAT-ISO-DATE
           MOVE WS-ISO-DATE                TO DCL-P-CYCLE-DATE
      *
           EXEC SQL OPEN BILCYCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT           '   TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               PERFORM 9400-SQL-ERROR
               PERFORM 9500-ABEND
           END-IF
      *
           PERFORM UNTIL WS-END-CURSOR
               PERFORM 3100-FETCH-ACCOUNT
               IF NOT WS-END-CURSOR
                   ADD 1                   TO WS-READ-CNT
                   PERFORM 3200-EVALUATE-ACCOUNT
               END-IF
           END-PERFORM
      *
           EXEC SQL CLOSE BILCYCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT           '   TO ER-SQL-TABLE
               MOVE 'CLOSE   '             TO ER-SQL-OPERATION
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           PERFORM 7300-COMMIT-POINT
           .
      *
      ******************************************************************
      * 3050 - THE BILLED PERIOD RUNS FROM THE DAY AFTER THE PREVIOUS  *
      *        CYCLE DATE TO THE CYCLE DATE ITSELF.  WHERE NO PREVIOUS *
      *        CYCLE EXISTS THE PERIOD OPENS ONE MONTH BACK.           *
      ******************************************************************
       3050-DERIVE-PERIOD.
           MOVE WS-CYCLE-DATE              TO WS-PERIOD-TO
                                              WS-DATE-WORK
           IF WS-DW-MM = 01
               SUBTRACT 1 FROM WS-DW-YYYY
               MOVE 12                     TO WS-DW-MM
           ELSE
               SUBTRACT 1 FROM WS-DW-MM
           END-IF
           MOVE WS-DATE-WORK               TO WS-PERIOD-FROM
           .
      *
       3100-FETCH-ACCOUNT.
           EXEC SQL
               FETCH BILCYCSR
                INTO :DCL-ACCT-ID
                   , :DCL-CUST-ID
                   , :DCL-PARTY-ID
                   , :DCL-PRODUCT-CD
                   , :DCL-ACCT-STATUS
                   , :DCL-CURRENCY-CD
                   , :DCL-CURR-BAL
                   , :DCL-STMT-BAL
                   , :DCL-CASH-BAL
                   , :DCL-DELQ-BUCKET
                   , :DCL-DELQ-AMT
                   , :DCL-CYCLE-DAY
                   , :DCL-STMT-COUNT
                   , :DCL-LAST-CYCLE-DT :IND-LAST-CYCLE-DT
                   , :DCL-OPEN-DATE
                   , :DCL-LIMIT-AMT      :IND-LIMIT-AMT
                   , :DCL-APR-PCT
                   , :DCL-CASH-APR-PCT
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   MOVE 'Y'                TO WS-END-CURSOR-SW
               WHEN OTHER
                   MOVE 'ACCOUNT           ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '         TO ER-SQL-OPERATION
                   PERFORM 9400-SQL-ERROR
                   PERFORM 9500-ABEND
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3200 - BUSINESS SELECTION RULES                                *
      *                                                                *
      * CLOSED ACCOUNTS ARE BILLED ONLY WHEN THE BILL-CLOSED CARD SAYS *
      * SO AND A BALANCE REMAINS.  ACCOUNTS UNDER THE MINIMUM BALANCE  *
      * WITH NOTHING TO REPORT ARE SKIPPED.  A MISSING CREDIT LIMIT    *
      * ROW IS A REJECT - THE STATEMENT COULD NOT SHOW AVAILABLE       *
      * CREDIT.                                                        *
      ******************************************************************
       3200-EVALUATE-ACCOUNT.
           EVALUATE TRUE
               WHEN DCL-ACCT-STATUS = 'C'
                AND NOT WS-BILL-CLOSED
                   ADD 1                   TO WS-SKIP-CNT
               WHEN DCL-ACCT-STATUS = 'C'
                AND DCL-CURR-BAL = ZERO
                   ADD 1                   TO WS-SKIP-CNT
               WHEN IND-LIMIT-AMT < ZERO
                   ADD 1                   TO WS-REJECT-CNT
                   DISPLAY 'CBBIL01 REJECT NO CREDIT LIMIT ACCT '
                           DCL-ACCT-ID
               WHEN DCL-CURR-BAL < WS-MIN-BAL-TO-BILL
                AND DCL-CURR-BAL NOT < ZERO
                AND DCL-STMT-BAL = ZERO
                   ADD 1                   TO WS-SKIP-CNT
               WHEN OTHER
                   PERFORM 3300-BUILD-WORK-RECORD
                   PERFORM 3400-WRITE-WORK-RECORD
           END-EVALUATE
      *
           MOVE DCL-ACCT-ID                TO WS-LAST-ACCT-ID
           ADD 1                           TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
               PERFORM 7300-COMMIT-POINT
           END-IF
           .
      *
       3300-BUILD-WORK-RECORD.
           MOVE SPACES                     TO BILL-WORK-RECORD
           MOVE DCL-ACCT-ID                TO BW-ACCT-ID
           MOVE WS-CYCLE-DATE              TO BW-CYCLE-DATE
           MOVE DCL-CUST-ID                TO BW-CUST-ID
           MOVE DCL-PARTY-ID               TO BW-PARTY-ID
           MOVE DCL-PRODUCT-CD             TO BW-PRODUCT-CD
           MOVE DCL-CURRENCY-CD            TO BW-CURRENCY
           MOVE DCL-CYCLE-DAY              TO BW-CYCLE-DAY
           MOVE DCL-ACCT-STATUS            TO BW-ACCT-STATUS
           MOVE WS-PERIOD-FROM             TO BW-PERIOD-FROM
           MOVE WS-PERIOD-TO               TO BW-PERIOD-TO
      *
           IF IND-LAST-CYCLE-DT < ZERO
               MOVE WS-PERIOD-FROM         TO BW-PREV-CYCLE-DT
           ELSE
               MOVE DCL-LAST-CYCLE-DT      TO WS-ISO-DATE
               MOVE WS-ISO-YYYY            TO WS-DW-YYYY
               MOVE WS-ISO-MM              TO WS-DW-MM
               MOVE WS-ISO-DD              TO WS-DW-DD
               MOVE WS-DATE-WORK           TO BW-PREV-CYCLE-DT
           END-IF
      *
      *    THE OPENING BALANCE OF THIS CYCLE IS THE CLOSING BALANCE
      *    THE LAST STATEMENT SHOWED.
           MOVE DCL-STMT-BAL               TO BW-OPEN-BAL
           MOVE DCL-CURR-BAL               TO BW-CURR-BAL
           MOVE DCL-CASH-BAL               TO BW-CASH-BAL
           MOVE DCL-DELQ-BUCKET            TO BW-DELQ-BUCKET
           MOVE DCL-DELQ-AMT               TO BW-DELQ-AMT
           MOVE DCL-LIMIT-AMT              TO BW-CREDIT-LIMIT
           MOVE DCL-APR-PCT                TO BW-APR-PCT
           MOVE DCL-CASH-APR-PCT           TO BW-CASH-APR-PCT
           MOVE DCL-STMT-COUNT             TO BW-STMT-COUNT
           COMPUTE BW-STMT-NUMBER = DCL-STMT-COUNT + 1
           MOVE SPACES                     TO BW-DELIVERY-CD
           PERFORM 7000-GET-TIMESTAMP
           MOVE WS-SQL-TS                  TO BW-SELECT-TS
           MOVE 'CBBIL01 '                 TO BW-SELECT-PGM
           .
      *
       3400-WRITE-WORK-RECORD.
           WRITE BILLWRK-REC FROM BILL-WORK-RECORD
           IF NOT WS-BILLWRK-OK
               MOVE 'BILLWRK '             TO ER-FILE-NAME
               MOVE WS-BILLWRK-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'WORK FILE WRITE FAILED'
                                           TO ER-MESSAGE
               PERFORM 9500-ABEND
           END-IF
      *
           ADD 1                           TO WS-WRITTEN-CNT
           ADD BW-CURR-BAL                 TO WS-TOTAL-BAL
           ADD BW-CASH-BAL                 TO WS-TOTAL-CASH
           ADD BW-ACCT-ID                  TO WS-HASH-ACCT
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
       7100-REWRITE-CONTROL.
           MOVE CC-KEY                     TO CTL-KEY
           REWRITE CYCLCTL-REC FROM CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'CYCLCTL '         TO ER-FILE-NAME
                   MOVE WS-CYCLCTL-STATUS  TO ER-FILE-STATUS
                   MOVE 'CONTROL REWRITE FAILED'
                                           TO ER-MESSAGE
                   PERFORM 9500-ABEND
           END-REWRITE
           .
      *
       7200-FORMAT-ISO-DATE.
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           .
      *
      ******************************************************************
      * 7300 - CHECKPOINT.  THE CONTROL RECORD CARRIES THE LAST        *
      *        COMMITTED ACCOUNT SO A RESTART DOES NOT RE-SELECT.      *
      ******************************************************************
       7300-COMMIT-POINT.
           MOVE WS-READ-CNT                TO CC-RECS-READ
           MOVE WS-WRITTEN-CNT             TO CC-RECS-WRITTEN
           MOVE WS-REJECT-CNT              TO CC-RECS-REJECTED
           MOVE WS-TOTAL-BAL               TO CC-TOTAL-DR-AMT
           MOVE WS-HASH-ACCT               TO CC-HASH-TOTAL
           MOVE SPACES                     TO CC-LAST-KEY
           MOVE WS-LAST-ACCT-ID            TO CC-LAST-KEY(1:11)
           PERFORM 7100-REWRITE-CONTROL
      *
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'COMMIT  '             TO ER-SQL-OPERATION
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           ADD 1                           TO WS-COMMIT-CNT
           MOVE ZERO                       TO WS-SINCE-COMMIT
           .
      *
      ******************************************************************
      * 8000 - CLOSE THE STEP.  THE CYCLE STAYS IN RUNNING STATUS -    *
      *        ONLY CBBIL06 SETS IT COMPLETE.                          *
      ******************************************************************
       8000-END-CYCLE.
           MOVE 'CBBIL01 '                 TO CC-LAST-GOOD-STEP
           MOVE 'CBBIL02 '                 TO CC-CURRENT-STEP
           PERFORM 7000-GET-TIMESTAMP
           MOVE WS-SQL-TS                  TO CC-END-TS
           PERFORM 7100-REWRITE-CONTROL
      *
           CLOSE CYCLCTL-FILE
                 BILLWRK-FILE
           .
      *
       8500-REPORT-TOTALS.
           DISPLAY '*---------------------------------------------*'
           DISPLAY '* CBBIL01 CYCLE SELECTION                     *'
           DISPLAY '*---------------------------------------------*'
           DISPLAY '  CYCLE ID          ' WS-CYCLE-ID
           DISPLAY '  CYCLE DATE        ' WS-CYCLE-DATE
           DISPLAY '  CYCLE DAY         ' WS-CYCLE-DAY
           DISPLAY '  PERIOD FROM       ' WS-PERIOD-FROM
           DISPLAY '  PERIOD TO         ' WS-PERIOD-TO
           MOVE WS-READ-CNT                TO WS-DISP-CNT
           DISPLAY '  ACCOUNTS READ     ' WS-DISP-CNT
           MOVE WS-WRITTEN-CNT             TO WS-DISP-CNT
           DISPLAY '  ACCOUNTS SELECTED ' WS-DISP-CNT
           MOVE WS-SKIP-CNT                TO WS-DISP-CNT
           DISPLAY '  ACCOUNTS SKIPPED  ' WS-DISP-CNT
           MOVE WS-REJECT-CNT              TO WS-DISP-CNT
           DISPLAY '  REJECTS           ' WS-DISP-CNT
           MOVE WS-TOTAL-BAL               TO WS-DISP-AMT
           DISPLAY '  BALANCE SNAPSHOT  ' WS-DISP-AMT
           MOVE WS-TOTAL-CASH              TO WS-DISP-AMT
           DISPLAY '  CASH BALANCE      ' WS-DISP-AMT
           MOVE WS-COMMIT-CNT              TO WS-DISP-CNT
           DISPLAY '  COMMIT POINTS     ' WS-DISP-CNT
           IF WS-RESTART-RUN
               DISPLAY '  RUN TYPE          RESTART'
           ELSE
               DISPLAY '  RUN TYPE          NORMAL'
           END-IF
           .
      *
      ******************************************************************
      * 9000 - ERROR HANDLING                                          *
      ******************************************************************
       9400-SQL-ERROR.
           MOVE 'CBBIL01 '                 TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE 'E'                        TO ER-SEVERITY
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBBIL01 SQL ERROR SQLCODE=' SQLCODE
                   ' TABLE=' ER-SQL-TABLE
                   ' OP=' ER-SQL-OPERATION
           .
      *
       9500-FATAL-ERROR.
           MOVE 'CBBIL01 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL01 FATAL - ' ER-MESSAGE
           .
      *
       9500-ABEND.
           MOVE 'CBBIL01 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'U801'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL01 ABEND U0801 - ' ER-MESSAGE
      *
           MOVE 'F'                        TO CC-STATUS
           PERFORM 7100-REWRITE-CONTROL
      *
           CALL 'CBCRD91' USING ERROR-AREA
           .
      *
       9900-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO RETURN-CODE
               WHEN WS-REJECT-CNT > ZERO
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   MOVE 0                  TO RETURN-CODE
           END-EVALUATE
           .
