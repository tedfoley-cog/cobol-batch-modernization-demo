      ******************************************************************
      * CBCRD05B - INTEREST AND REWARDS BRANCH                         *
      *                                                                *
      * STEP 5B OF THE CARDNITE CYCLE.  JOB CBCRD05BJ.                 *
      * SUBMITTED IN PARALLEL WITH CBCRD05AJ AFTER CBCRD04J ENDS.      *
      * THE TWO BRANCHES TOUCH DIFFERENT COLUMNS OF THE SAME ACCOUNT   *
      * ROWS, WHICH IS WHY BOTH COMMIT FREQUENTLY.                     *
      *                                                                *
      * THREE THINGS HAPPEN PER ACCOUNT                                *
      *   1  RETAIL INTEREST ON THE AVERAGE DAILY BALANCE AT THE       *
      *      ACCOUNT'S RETAIL APR                                      *
      *   2  CASH INTEREST ON THE AVERAGE DAILY CASH BALANCE AT THE    *
      *      CASH APR, WHICH IS ALWAYS A SEPARATE RATE AND IS NOT      *
      *      SUBJECT TO THE GRACE PERIOD                               *
      *   3  REWARD POINT ACCRUAL ON THE CYCLE'S QUALIFYING SPEND      *
      *                                                                *
      * ALL MONEY IS PACKED DECIMAL.  EVERY INTEREST COMPUTATION       *
      * ROUNDS HALF UP ON THE LAST DECIMAL PLACE - THAT IS WHAT THE    *
      * COBOL ROUNDED PHRASE DOES BY DEFAULT AND IT IS THE CONVENTION  *
      * THE BANK'S TERMS AND CONDITIONS QUOTE.  THE ROUNDING           *
      * CONVENTION AND THE DAY COUNT BASIS ARRIVE ON SYSIN SO THAT     *
      * TREASURY CAN CHANGE THEM WITHOUT A RECOMPILE - AN UNKNOWN      *
      * CONVENTION IS A FATAL CONDITION, NOT A SILENT DEFAULT.         *
      *                                                                *
      * AS ITS LAST ACT THE PROGRAM POSTS THE INTEREST BRANCH          *
      * COMPLETION FLAG INTO THE CYCLE CONTROL FILE FOR CBCRD06W.      *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - CYCLCTL  VSAM KSDS I-O                           *
      *             - SYSIN    CONTROL CARDS                           *
      *             - INTRPT   QSAM OUTPUT LRECL 133                   *
      * TABLES      - CARDSVC.ACCOUNT       (CURSOR / UPDATE)          *
      *             - CARDSVC.CARD_LIMIT    (SELECT)                   *
      *             - CARDSVC.TRANSACTION   (SELECT / INSERT)          *
      *             - CARDSVC.REWARDS       (INSERT / UPDATE)          *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 CLEAN                                       *
      *               0004 ONE OR MORE ACCOUNTS SKIPPED                *
      *               0012 FATAL                                       *
      * USER ABEND  - U0501 CYCLE CONTROL UNUSABLE                     *
      *               U0503 UNRECOVERABLE SQL ERROR                    *
      *               U0506 ROUNDING CONVENTION ON SYSIN NOT SUPPORTED *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD05B.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1999-02-15.
      *
      * MAINTENANCE
      * 1999-02-15 CRD0409 ORIGINAL
      * 2003-06-30 CRD3120 CASH APR SEPARATED FROM RETAIL APR
      * 2008-04-21 CRD5904 AVERAGE DAILY BALANCE REPLACED CLOSING
      *                    BALANCE AFTER THE OMBUDSMAN RULING
      * 2015-10-05 CRD9111 ROUNDING CONVENTION MOVED TO SYSIN
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CYCLCTL-FILE  ASSIGN TO CYCLCTL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CTL-KEY
                  FILE STATUS  IS WS-CYCLCTL-STATUS.
      *
           SELECT REPORT-FILE   ASSIGN TO INTRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REPORT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  CYCLCTL-FILE
           RECORD CONTAINS 256 CHARACTERS.
       01  CYCLCTL-REC.
           05  CTL-KEY.
               10  CTL-CYCLE-TYPE          PIC X(8).
               10  CTL-CYCLE-DATE          PIC 9(8).
           05  CTL-REST                    PIC X(240).
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  REPORT-REC                      PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD05B'.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
           05  WS-REPORT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REPORT-OK                      VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-ACCT-EOF-SW              PIC X     VALUE 'N'.
               88  WS-ACCT-EOF                       VALUE 'Y'.
           05  WS-SKIP-SW                  PIC X     VALUE 'N'.
               88  WS-SKIP-ACCOUNT                   VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-ACCT-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-INT-POSTED-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-CASH-INT-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-REWARD-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SKIP-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-GRACE-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-COMMIT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
           05  WS-TXN-SERIAL-NBR           PIC 9(8)  VALUE ZERO.
      *
       01  WS-TOTALS.
           05  WS-RETAIL-INT-TOT           PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-CASH-INT-TOT             PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-POINTS-TOT               PIC S9(13)   COMP-3
                                                     VALUE ZERO.
      *
      *    CONTROL CARD PARAMETERS
       01  WS-CONTROL-CARD                 PIC X(80) VALUE SPACES.
       01  WS-PARM-CARDS.
           05  WS-ROUNDING-CONV            PIC X(8)  VALUE SPACES.
               88  WS-ROUND-HALF-UP        VALUE 'HALFUP  '.
               88  WS-ROUND-HALF-EVEN      VALUE 'HALFEVEN'.
               88  WS-ROUND-TRUNCATE       VALUE 'TRUNCATE'.
           05  WS-DAY-COUNT                PIC 9(3)  VALUE 365.
           05  WS-CYCLE-DAYS               PIC 9(3)  VALUE 030.
           05  WS-GRACE-DAYS               PIC 9(3)  VALUE 025.
           05  WS-POINTS-PER-UNIT          PIC 9(3)  VALUE 001.
           05  WS-POINT-UNIT-AMT           PIC S9(5)V99 COMP-3
                                                     VALUE 1.00.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYC-DT                       PIC X(10) VALUE SPACES.
       01  WS-PERIOD-FROM                  PIC X(10) VALUE SPACES.
       01  WS-CYCLE-ID                     PIC X(8)  VALUE SPACES.
      *
       01  WS-CALC-FIELDS.
           05  WS-ADB-RETAIL               PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-ADB-CASH                 PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-DAILY-RATE               PIC S9(3)V9(9) COMP-3
                                                     VALUE ZERO.
           05  WS-RETAIL-INT               PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-CASH-INT                 PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-RAW-INT                  PIC S9(9)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-QUALIFYING-SPEND         PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-POINTS-EARNED            PIC S9(11) COMP-3
                                                     VALUE ZERO.
           05  WS-POINTS-RAW               PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-TXN-ID                       PIC X(16) VALUE SPACES.
       01  WS-TXN-ID-R REDEFINES WS-TXN-ID.
           05  WS-TXN-PREFIX               PIC X(2).
           05  WS-TXN-DATE-PART            PIC 9(6).
           05  WS-TXN-SERIAL               PIC 9(8).
      *
       01  WS-CURRENT-DATE.
           05  WS-CD-DATE                  PIC 9(8).
           05  WS-CD-TIME                  PIC 9(8).
           05  WS-CD-FILLER                PIC X(5).
       01  WS-TIMESTAMP                    PIC X(26) VALUE SPACES.
      *
       01  WS-RETURN-CODE                  PIC 9(4)  VALUE ZERO.
       01  WS-ABEND-CODE                   PIC 9(4)  VALUE ZERO.
      *
       01  WS-REPORT-LINE.
           05  RL-ACCT-ID                  PIC 9(11).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RL-PRODUCT                  PIC X(4).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RL-ADB                      PIC ---,---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RL-APR                      PIC ZZ9.99999.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RL-RETAIL-INT               PIC ---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RL-CASH-INT                 PIC ---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RL-POINTS                   PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(60) VALUE SPACES.
      *
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-ACCOUNT.
           05  DCL-ACCT-ID                 PIC S9(11)    COMP-3.
           05  DCL-CUST-ID                 PIC S9(9)     COMP-3.
           05  DCL-PRODUCT-CD              PIC X(4).
           05  DCL-CURRENCY-CD             PIC X(3).
           05  DCL-CURR-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-STMT-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-CASH-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-DELQ-BUCKET             PIC S9(4)     COMP.
           05  DCL-LAST-PAY-AMT            PIC S9(9)V99  COMP-3.
      *
       01  DCL-RATES.
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-APR-PCT                 PIC S9(3)V9(5) COMP-3.
           05  DCL-CASH-APR-PCT            PIC S9(3)V9(5) COMP-3.
      *
       01  DCL-WORK.
           05  DCL-SPEND-AMT               PIC S9(11)V99 COMP-3.
           05  DCL-ADB-RETAIL              PIC S9(11)V99 COMP-3.
           05  DCL-ADB-CASH                PIC S9(11)V99 COMP-3.
           05  DCL-TXN-ID                  PIC X(16).
           05  DCL-INT-AMT                 PIC S9(9)V99  COMP-3.
           05  DCL-NARRATIVE               PIC X(40).
           05  DCL-POINTS                  PIC S9(11)    COMP-3.
           05  DCL-OPEN-POINTS             PIC S9(11)    COMP-3.
           05  DCL-EARN-RATE               PIC S9(3)V9(5) COMP-3.
           05  DCL-LEG-DATA.
               10  DCL-LEG-LEN             PIC S9(4) COMP.
               10  DCL-LEG-TXT             PIC X(400).
      *
       01  IND-VARS.
           05  IND-ADB-RETAIL              PIC S9(4) COMP.
           05  IND-ADB-CASH                PIC S9(4) COMP.
           05  IND-SPEND                   PIC S9(4) COMP.
      *
       01  WS-LEG-IMAGE.
           05  WS-LI-SEQ                   PIC 9(2).
           05  WS-LI-TYPE                  PIC X(4).
           05  WS-LI-AMT                   PIC S9(11)V99 COMP-3.
           05  WS-LI-GL                    PIC X(10).
           05  WS-LI-REVERSED              PIC X.
      *
           EXEC SQL DECLARE INTACCSR CURSOR WITH HOLD FOR
               SELECT ACCT_ID
                    , CUST_ID
                    , PRODUCT_CD
                    , CURRENCY_CD
                    , CURR_BAL
                    , STMT_BAL
                    , CASH_BAL
                    , DELQ_BUCKET
                    , LAST_PAY_AMT
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_STATUS = 'O'
                ORDER BY ACCT_ID
           END-EXEC.
      *
       01  WS-DISPLAY-CNT                  PIC ZZZ,ZZZ,ZZ9.
       01  WS-DISPLAY-AMT                  PIC ---,---,---,--9.99.
      *
       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-PARM-LEN                 PIC S9(4) COMP.
           05  LK-PARM-DATA                PIC X(20).
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-PARM.
      *
       0000-MAIN-LINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-ACCOUNT
               UNTIL WS-ACCT-EOF
           PERFORM 3000-TERMINATE
           MOVE WS-RETURN-CODE             TO RETURN-CODE
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - INITIALISATION                                          *
      ******************************************************************
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           STRING WS-CD-DATE(1:4) '-' WS-CD-DATE(5:2) '-'
                  WS-CD-DATE(7:2) '-' WS-CD-TIME(1:2) '.'
                  WS-CD-TIME(3:2) '.' WS-CD-TIME(5:2) '.000000'
             DELIMITED BY SIZE INTO WS-TIMESTAMP
           END-STRING
      *
           PERFORM 1100-READ-CONTROL-CARDS
           PERFORM 1200-VALIDATE-CONVENTION
      *
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO WS-CYC-DT
           END-STRING
           PERFORM 1300-DERIVE-PERIOD-FROM
      *
           MOVE 'IN'                       TO WS-TXN-PREFIX
           MOVE WS-CYCLE-DATE(3:6)         TO WS-TXN-DATE-PART
      *
           OPEN I-O    CYCLCTL-FILE
           OPEN OUTPUT REPORT-FILE
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0501                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 0501               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE CC-CYCLE-ID                TO WS-CYCLE-ID
      *
           EXEC SQL OPEN INTACCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          '    TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               MOVE 'OPEN OF INTACCSR FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           DISPLAY 'CBCRD05B - INTEREST AND REWARDS BRANCH'
           DISPLAY '           CYCLE DATE ' WS-CYCLE-DATE
           DISPLAY '           ROUNDING   ' WS-ROUNDING-CONV
           DISPLAY '           DAY COUNT  ' WS-DAY-COUNT
           DISPLAY '           CYCLE DAYS ' WS-CYCLE-DAYS
      *
           PERFORM 1900-FETCH-ACCOUNT
           .
      *
      ******************************************************************
      * 1100 - CONTROL CARDS.  CYCLE-DATE, ROUNDING, DAY-COUNT,        *
      *        CYCLE-DAYS, GRACE-DAYS AND THE REWARD EARN UNIT ALL     *
      *        ARRIVE HERE - NONE OF THEM ARE CODED IN THE PROGRAM.    *
      ******************************************************************
       1100-READ-CONTROL-CARDS.
           ACCEPT WS-CONTROL-CARD FROM SYSIN
           PERFORM UNTIL WS-CONTROL-CARD = SPACES
                      OR WS-CONTROL-CARD(1:3) = 'END'
               EVALUATE WS-CONTROL-CARD(1:11)
                   WHEN 'CYCLE-DATE='
                       MOVE WS-CONTROL-CARD(12:8)
                                           TO WS-CYCLE-DATE
                   WHEN 'ROUNDING=  '
                       MOVE WS-CONTROL-CARD(10:8)
                                           TO WS-ROUNDING-CONV
                   WHEN 'DAY-COUNT= '
                       MOVE WS-CONTROL-CARD(11:3)
                                           TO WS-DAY-COUNT
                   WHEN 'CYCLE-DAYS='
                       MOVE WS-CONTROL-CARD(12:3)
                                           TO WS-CYCLE-DAYS
                   WHEN 'GRACE-DAYS='
                       MOVE WS-CONTROL-CARD(12:3)
                                           TO WS-GRACE-DAYS
                   WHEN 'POINT-UNIT='
                       MOVE WS-CONTROL-CARD(12:5)
                                           TO WS-POINT-UNIT-AMT
                   WHEN OTHER
                       IF WS-CONTROL-CARD(1:1) NOT = '*'
                           DISPLAY 'CBCRD05B - CARD IGNORED '
                                   WS-CONTROL-CARD(1:40)
                       END-IF
               END-EVALUATE
               MOVE SPACES                 TO WS-CONTROL-CARD
               ACCEPT WS-CONTROL-CARD FROM SYSIN
           END-PERFORM
           .
      *
      ******************************************************************
      * 1200 - THE ONLY CONVENTION THIS PROGRAM IMPLEMENTS IS HALF UP  *
      *        ON THE LAST DECIMAL PLACE.  ANYTHING ELSE ON THE CARD   *
      *        MEANS TREASURY CHANGED THE RULE WITHOUT THE CODE        *
      *        FOLLOWING - FAIL LOUDLY RATHER THAN COMPUTE INTEREST    *
      *        ON THE WRONG BASIS.                                     *
      ******************************************************************
       1200-VALIDATE-CONVENTION.
           IF NOT WS-ROUND-HALF-UP
               MOVE 'ROUNDING CONVENTION ON SYSIN NOT SUPPORTED'
                                           TO ER-MESSAGE
               MOVE 0506                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF WS-CYCLE-DATE = ZERO
               MOVE 'CYCLE-DATE CARD MISSING FROM SYSIN'
                                           TO ER-MESSAGE
               MOVE 0506                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       1300-DERIVE-PERIOD-FROM.
           EXEC SQL
               SET :WS-PERIOD-FROM =
                   CHAR(DATE(:WS-CYC-DT) - :WS-CYCLE-DAYS DAYS, ISO)
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'DERIVATION OF PERIOD START DATE FAILED'
                                           TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           .
      *
       1900-FETCH-ACCOUNT.
           EXEC SQL
               FETCH INTACCSR
                INTO :DCL-ACCT-ID
                   , :DCL-CUST-ID
                   , :DCL-PRODUCT-CD
                   , :DCL-CURRENCY-CD
                   , :DCL-CURR-BAL
                   , :DCL-STMT-BAL
                   , :DCL-CASH-BAL
                   , :DCL-DELQ-BUCKET
                   , :DCL-LAST-PAY-AMT
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-ACCT-CNT
               WHEN 100
                   MOVE 'Y'                TO WS-ACCT-EOF-SW
               WHEN OTHER
                   MOVE 'ACCOUNT          ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '          TO ER-SQL-OPERATION
                   MOVE 'FETCH OF INTACCSR FAILED' TO ER-MESSAGE
                   MOVE 0503                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2000 - ONE ACCOUNT                                             *
      ******************************************************************
       2000-PROCESS-ACCOUNT.
           MOVE 'N'                        TO WS-SKIP-SW
           MOVE ZERO                       TO WS-RETAIL-INT
                                              WS-CASH-INT
                                              WS-POINTS-EARNED
      *
           PERFORM 2100-READ-RATES
           IF WS-SKIP-ACCOUNT
               GO TO 2000-NEXT
           END-IF
      *
           PERFORM 2200-AVERAGE-DAILY-BALANCE
           PERFORM 2300-RETAIL-INTEREST
           PERFORM 2400-CASH-INTEREST
      *
           IF WS-RETAIL-INT NOT = ZERO
           OR WS-CASH-INT   NOT = ZERO
               PERFORM 2500-POST-INTEREST
           END-IF
      *
           PERFORM 2600-ACCRUE-REWARDS
           PERFORM 2700-WRITE-REPORT-LINE
      *
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQUENCY
               PERFORM 2900-COMMIT-POINT
           END-IF
           .
       2000-NEXT.
           PERFORM 1900-FETCH-ACCOUNT
           .
      *
       2100-READ-RATES.
           MOVE ZERO                       TO DCL-APR-PCT
                                              DCL-CASH-APR-PCT
           MOVE SPACES                     TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT MIN(L.CARD_NUM)
                    , MAX(L.APR_PCT)
                    , MAX(L.CASH_APR_PCT)
                 INTO :DCL-CARD-NUM
                    , :DCL-APR-PCT
                    , :DCL-CASH-APR-PCT
                 FROM CARDSVC.CARD_LIMIT L
                    , CARDSVC.CARD C
                WHERE C.ACCT_ID    = :DCL-ACCT-ID
                  AND C.CARD_STATUS IN ('A','B')
                  AND L.CARD_NUM   = C.CARD_NUM
                  AND L.LIMIT_TYPE = 'CRED'
                  AND DATE(:WS-CYC-DT)
                      BETWEEN L.EFF_DATE AND L.EXP_DATE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   MOVE 'Y'                TO WS-SKIP-SW
                   ADD 1                   TO WS-SKIP-CNT
               WHEN OTHER
                   MOVE 'CARD_LIMIT       ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '          TO ER-SQL-OPERATION
                   MOVE 'SELECT OF APR FAILED' TO ER-MESSAGE
                   MOVE 0503                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
      *
           IF DCL-CARD-NUM = SPACES
               MOVE 'Y'                    TO WS-SKIP-SW
               ADD 1                       TO WS-SKIP-CNT
           END-IF
           .
      *
      ******************************************************************
      * 2200 - AVERAGE DAILY BALANCE                                   *
      *                                                                *
      * THE CYCLE'S POSTED MOVEMENT IS WEIGHTED BY THE NUMBER OF DAYS  *
      * IT WAS OUTSTANDING AND ADDED TO THE OPENING STATEMENT BALANCE. *
      * DB2 DOES THE WEIGHTING - THE CASE EXPRESSION SPLITS RETAIL     *
      * FROM CASH BY TRANSACTION TYPE.                                 *
      ******************************************************************
       2200-AVERAGE-DAILY-BALANCE.
           MOVE ZERO                       TO DCL-ADB-RETAIL
                                              DCL-ADB-CASH
      *
           EXEC SQL
               SELECT SUM(CASE WHEN T.TXN_TYPE_CD = 'CASH'
                               THEN 0
                               ELSE T.BILLING_AMT
                                  * (DAYS(DATE(:WS-CYC-DT))
                                   - DAYS(T.POST_DATE) + 1)
                          END)
                    , SUM(CASE WHEN T.TXN_TYPE_CD = 'CASH'
                               THEN T.BILLING_AMT
                                  * (DAYS(DATE(:WS-CYC-DT))
                                   - DAYS(T.POST_DATE) + 1)
                               ELSE 0
                          END)
                 INTO :DCL-ADB-RETAIL :IND-ADB-RETAIL
                    , :DCL-ADB-CASH   :IND-ADB-CASH
                 FROM CARDSVC.TRANSACTION T
                WHERE T.ACCT_ID = :DCL-ACCT-ID
                  AND T.POST_DATE BETWEEN DATE(:WS-PERIOD-FROM)
                                      AND DATE(:WS-CYC-DT)
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               MOVE 'TRANSACTION      '    TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               MOVE 'AVERAGE DAILY BALANCE QUERY FAILED'
                                           TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           IF IND-ADB-RETAIL < 0
               MOVE ZERO                   TO DCL-ADB-RETAIL
           END-IF
           IF IND-ADB-CASH < 0
               MOVE ZERO                   TO DCL-ADB-CASH
           END-IF
      *
           COMPUTE WS-ADB-RETAIL ROUNDED =
                   DCL-STMT-BAL + (DCL-ADB-RETAIL / WS-CYCLE-DAYS)
           COMPUTE WS-ADB-CASH ROUNDED =
                   DCL-CASH-BAL + (DCL-ADB-CASH / WS-CYCLE-DAYS)
      *
           IF WS-ADB-RETAIL < ZERO
               MOVE ZERO                   TO WS-ADB-RETAIL
           END-IF
           IF WS-ADB-CASH < ZERO
               MOVE ZERO                   TO WS-ADB-CASH
           END-IF
           .
      *
      ******************************************************************
      * 2300 - RETAIL INTEREST                                         *
      *                                                                *
      * NO RETAIL INTEREST WHERE THE PREVIOUS STATEMENT WAS SETTLED IN *
      * FULL WITHIN THE GRACE PERIOD.  THE COMPUTATION IS CARRIED AT   *
      * FIVE DECIMALS AND ROUNDED HALF UP INTO THE TWO DECIMAL POSTED  *
      * AMOUNT ON THE LAST STATEMENT - ROUNDED ON A COBOL COMPUTE IS   *
      * HALF UP, WHICH IS THE CONVENTION THE T AND CS QUOTE.           *
      ******************************************************************
       2300-RETAIL-INTEREST.
           IF DCL-STMT-BAL > ZERO
           AND DCL-LAST-PAY-AMT >= DCL-STMT-BAL
               ADD 1                       TO WS-GRACE-CNT
               MOVE ZERO                   TO WS-RETAIL-INT
               GO TO 2300-EXIT
           END-IF
      *
           IF WS-ADB-RETAIL = ZERO OR DCL-APR-PCT = ZERO
               MOVE ZERO                   TO WS-RETAIL-INT
               GO TO 2300-EXIT
           END-IF
      *
           COMPUTE WS-DAILY-RATE ROUNDED =
                   DCL-APR-PCT / 100 / WS-DAY-COUNT
      *
           COMPUTE WS-RAW-INT =
                   WS-ADB-RETAIL * WS-DAILY-RATE * WS-CYCLE-DAYS
      *
           COMPUTE WS-RETAIL-INT ROUNDED = WS-RAW-INT
      *
           ADD WS-RETAIL-INT               TO WS-RETAIL-INT-TOT
           .
       2300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2400 - CASH INTEREST.  CHARGED FROM THE DAY OF THE ADVANCE     *
      *        AND NEVER SUBJECT TO THE GRACE PERIOD - CRD3120.        *
      ******************************************************************
       2400-CASH-INTEREST.
           IF WS-ADB-CASH = ZERO OR DCL-CASH-APR-PCT = ZERO
               MOVE ZERO                   TO WS-CASH-INT
               GO TO 2400-EXIT
           END-IF
      *
           COMPUTE WS-DAILY-RATE ROUNDED =
                   DCL-CASH-APR-PCT / 100 / WS-DAY-COUNT
      *
           COMPUTE WS-RAW-INT =
                   WS-ADB-CASH * WS-DAILY-RATE * WS-CYCLE-DAYS
      *
           COMPUTE WS-CASH-INT ROUNDED = WS-RAW-INT
      *
           ADD WS-CASH-INT                 TO WS-CASH-INT-TOT
           ADD 1                           TO WS-CASH-INT-CNT
           .
       2400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2500 - POST THE INTEREST TRANSACTION.  RETAIL AND CASH ARE     *
      *        TWO LEGS OF ONE TRANSACTION SO THE STATEMENT SHOWS      *
      *        THEM SEPARATELY UNDER A SINGLE ENTRY.                   *
      ******************************************************************
       2500-POST-INTEREST.
           ADD 1                           TO WS-TXN-SERIAL-NBR
           MOVE WS-TXN-SERIAL-NBR          TO WS-TXN-SERIAL
           MOVE WS-TXN-ID                  TO DCL-TXN-ID
           COMPUTE DCL-INT-AMT = WS-RETAIL-INT + WS-CASH-INT
      *
           MOVE SPACES                     TO DCL-LEG-TXT
           MOVE 01                         TO WS-LI-SEQ
           MOVE 'INTR'                     TO WS-LI-TYPE
           MOVE WS-RETAIL-INT              TO WS-LI-AMT
           MOVE '4100100100'               TO WS-LI-GL
           MOVE 'N'                        TO WS-LI-REVERSED
           MOVE WS-LEG-IMAGE               TO DCL-LEG-TXT(1:24)
      *
           IF WS-CASH-INT NOT = ZERO
               MOVE 02                     TO WS-LI-SEQ
               MOVE 'INTR'                 TO WS-LI-TYPE
               MOVE WS-CASH-INT            TO WS-LI-AMT
               MOVE '4100200100'           TO WS-LI-GL
               MOVE 'N'                    TO WS-LI-REVERSED
               MOVE WS-LEG-IMAGE           TO DCL-LEG-TXT(25:24)
               MOVE 48                     TO DCL-LEG-LEN
           ELSE
               MOVE 24                     TO DCL-LEG-LEN
           END-IF
      *
           STRING 'INTEREST CHARGE CYCLE ' WS-CYCLE-ID
             DELIMITED BY SIZE INTO DCL-NARRATIVE
           END-STRING
      *
           EXEC SQL
               INSERT INTO CARDSVC.TRANSACTION
                     (TXN_ID
                    , POST_DATE
                    , ACCT_ID
                    , CARD_NUM
                    , TXN_TYPE_CD
                    , TXN_SOURCE
                    , TXN_AMT
                    , CURRENCY_CD
                    , BILLING_AMT
                    , FX_RATE
                    , TXN_DESC
                    , TXN_LEG_CNT
                    , TXN_LEG_DATA
                    , CYCLE_ID
                    , GL_POSTED_FLG
                    , DISPUTE_FLG
                    , POSTED_BY)
               VALUES (:DCL-TXN-ID
                    , DATE(:WS-CYC-DT)
                    , :DCL-ACCT-ID
                    , :DCL-CARD-NUM
                    , 'INTR'
                    , 'BT'
                    , :DCL-INT-AMT
                    , :DCL-CURRENCY-CD
                    , :DCL-INT-AMT
                    , 1
                    , :DCL-NARRATIVE
                    , 1
                    , :DCL-LEG-DATA
                    , :WS-CYCLE-ID
                    , 'N'
                    , 'N'
                    , :WS-PROGRAM-ID)
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = -803
               MOVE 'TRANSACTION      '    TO ER-SQL-TABLE
               MOVE 'INSERT  '             TO ER-SQL-OPERATION
               MOVE 'INSERT OF INTEREST TRANSACTION FAILED'
                                           TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET CURR_BAL       = CURR_BAL + :DCL-INT-AMT
                    , LAST_MAINT_PGM = :WS-PROGRAM-ID
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          '    TO ER-SQL-TABLE
               MOVE 'UPDATE  '             TO ER-SQL-OPERATION
               MOVE 'BALANCE UPDATE FOR INTEREST FAILED'
                                           TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           ADD 1                           TO WS-INT-POSTED-CNT
                                              WS-SINCE-COMMIT
           .
      *
      ******************************************************************
      * 2600 - REWARD POINT ACCRUAL                                    *
      *                                                                *
      * QUALIFYING SPEND IS THE CYCLE'S PURCHASES.  CASH ADVANCES,     *
      * FEES, INTEREST AND REFUNDS DO NOT EARN.  POINTS ARE WHOLE      *
      * NUMBERS - THE FRACTION IS DROPPED, NOT ROUNDED, BECAUSE THE    *
      * SCHEME RULES SAY POINTS ARE EARNED PER COMPLETED UNIT.         *
      ******************************************************************
       2600-ACCRUE-REWARDS.
           MOVE ZERO                       TO DCL-SPEND-AMT
      *
           EXEC SQL
               SELECT SUM(T.BILLING_AMT)
                 INTO :DCL-SPEND-AMT :IND-SPEND
                 FROM CARDSVC.TRANSACTION T
                WHERE T.ACCT_ID     = :DCL-ACCT-ID
                  AND T.POST_DATE   = DATE(:WS-CYC-DT)
                  AND T.TXN_TYPE_CD = 'PURC'
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               MOVE 'TRANSACTION      '    TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               MOVE 'QUALIFYING SPEND QUERY FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           IF IND-SPEND < 0 OR DCL-SPEND-AMT <= ZERO
               MOVE ZERO                   TO WS-POINTS-EARNED
               GO TO 2600-EXIT
           END-IF
      *
           MOVE DCL-SPEND-AMT              TO WS-QUALIFYING-SPEND
           COMPUTE WS-POINTS-RAW =
                   WS-QUALIFYING-SPEND / WS-POINT-UNIT-AMT
           COMPUTE WS-POINTS-EARNED =
                   FUNCTION INTEGER-PART (WS-POINTS-RAW)
                   * WS-POINTS-PER-UNIT
      *
           IF WS-POINTS-EARNED = ZERO
               GO TO 2600-EXIT
           END-IF
      *
           MOVE WS-POINTS-EARNED           TO DCL-POINTS
           PERFORM 2650-UPDATE-REWARDS
           ADD WS-POINTS-EARNED            TO WS-POINTS-TOT
           ADD 1                           TO WS-REWARD-CNT
           .
       2600-EXIT.
           EXIT
           .
      *
       2650-UPDATE-REWARDS.
           EXEC SQL
               UPDATE CARDSVC.REWARDS
                  SET EARNED_POINTS = EARNED_POINTS + :DCL-POINTS
                    , CLOSE_POINTS  = OPEN_POINTS
                                    + EARNED_POINTS + :DCL-POINTS
                                    - REDEEMED_POINTS
                                    - EXPIRED_POINTS
                    , CALC_PGM      = :WS-PROGRAM-ID
                    , CALC_TS       = CURRENT TIMESTAMP
                WHERE ACCT_ID    = :DCL-ACCT-ID
                  AND CYCLE_DATE = DATE(:WS-CYC-DT)
                  AND PROGRAM_CD = 'STND'
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   PERFORM 2660-INSERT-REWARDS
               WHEN OTHER
                   MOVE 'REWARDS          ' TO ER-SQL-TABLE
                   MOVE 'UPDATE  '          TO ER-SQL-OPERATION
                   MOVE 'UPDATE OF REWARDS FAILED' TO ER-MESSAGE
                   MOVE 0503                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       2660-INSERT-REWARDS.
      *    CARRY THE PREVIOUS CYCLE'S CLOSING BALANCE FORWARD AS THIS
      *    CYCLE'S OPENING BALANCE.  NO PREVIOUS ROW MEANS ZERO.
           MOVE ZERO                       TO DCL-OPEN-POINTS
           EXEC SQL
               SELECT CLOSE_POINTS
                 INTO :DCL-OPEN-POINTS
                 FROM CARDSVC.REWARDS
                WHERE ACCT_ID    = :DCL-ACCT-ID
                  AND PROGRAM_CD = 'STND'
                  AND CYCLE_DATE = (SELECT MAX(CYCLE_DATE)
                                      FROM CARDSVC.REWARDS
                                     WHERE ACCT_ID    = :DCL-ACCT-ID
                                       AND PROGRAM_CD = 'STND')
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               MOVE 'REWARDS          '    TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               MOVE 'OPENING POINTS QUERY FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           MOVE 1.00000                    TO DCL-EARN-RATE
      *
           EXEC SQL
               INSERT INTO CARDSVC.REWARDS
                     (ACCT_ID
                    , CYCLE_DATE
                    , PROGRAM_CD
                    , OPEN_POINTS
                    , EARNED_POINTS
                    , REDEEMED_POINTS
                    , EXPIRED_POINTS
                    , CLOSE_POINTS
                    , EARN_RATE
                    , BONUS_AMT
                    , CALC_PGM)
               VALUES (:DCL-ACCT-ID
                    , DATE(:WS-CYC-DT)
                    , 'STND'
                    , :DCL-OPEN-POINTS
                    , :DCL-POINTS
                    , 0
                    , 0
                    , :DCL-OPEN-POINTS + :DCL-POINTS
                    , :DCL-EARN-RATE
                    , 0
                    , :WS-PROGRAM-ID)
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = -803
               MOVE 'REWARDS          '    TO ER-SQL-TABLE
               MOVE 'INSERT  '             TO ER-SQL-OPERATION
               MOVE 'INSERT OF REWARDS ROW FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           .
      *
       2700-WRITE-REPORT-LINE.
           MOVE DCL-ACCT-ID                TO RL-ACCT-ID
           MOVE DCL-PRODUCT-CD             TO RL-PRODUCT
           MOVE WS-ADB-RETAIL              TO RL-ADB
           MOVE DCL-APR-PCT                TO RL-APR
           MOVE WS-RETAIL-INT              TO RL-RETAIL-INT
           MOVE WS-CASH-INT                TO RL-CASH-INT
           MOVE WS-POINTS-EARNED           TO RL-POINTS
           WRITE REPORT-REC FROM WS-REPORT-LINE
           .
      *
       2900-COMMIT-POINT.
           EXEC SQL COMMIT WORK END-EXEC
           ADD 1                           TO WS-COMMIT-CNT
           MOVE ZERO                       TO WS-SINCE-COMMIT
           .
      *
      ******************************************************************
      * 3000 - TERMINATION AND BRANCH COMPLETION FLAG                  *
      ******************************************************************
       3000-TERMINATE.
           EXEC SQL CLOSE INTACCSR END-EXEC
           EXEC SQL COMMIT WORK END-EXEC
           ADD 1                           TO WS-COMMIT-CNT
      *
           IF WS-SKIP-CNT > ZERO
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
           END-IF
      *
           PERFORM 3100-POST-BRANCH-FLAG
      *
           CLOSE CYCLCTL-FILE
                 REPORT-FILE
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-ACCT-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05B ACCOUNTS READ     ' WS-DISPLAY-CNT
           MOVE WS-INT-POSTED-CNT          TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05B INTEREST POSTINGS ' WS-DISPLAY-CNT
           MOVE WS-CASH-INT-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05B CASH INTEREST     ' WS-DISPLAY-CNT
           MOVE WS-GRACE-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05B IN GRACE PERIOD   ' WS-DISPLAY-CNT
           MOVE WS-REWARD-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05B REWARD ACCRUALS   ' WS-DISPLAY-CNT
           MOVE WS-SKIP-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05B ACCOUNTS SKIPPED  ' WS-DISPLAY-CNT
           MOVE WS-RETAIL-INT-TOT          TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD05B RETAIL INTEREST   ' WS-DISPLAY-AMT
           MOVE WS-CASH-INT-TOT            TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD05B CASH INTEREST     ' WS-DISPLAY-AMT
           DISPLAY 'CBCRD05B POINTS ACCRUED    ' WS-POINTS-TOT
           DISPLAY '----------------------------------------------'
           .
      *
      ******************************************************************
      * 3100 - POST THE INTEREST BRANCH COMPLETION FLAG.               *
      *        THIS BRANCH OWNS THE SECOND BYTE OF CC-FILLER.  THE     *
      *        RECORD IS RE-READ IMMEDIATELY BEFORE THE REWRITE SO     *
      *        THE FEE BRANCH'S FLAG IS NOT OVERLAID.                  *
      ******************************************************************
       3100-POST-BRANCH-FLAG.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
      *
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'CYCLE CONTROL LOST BEFORE FLAG POST'
                                           TO ER-MESSAGE
                   MOVE 0501               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
      *
           IF WS-RETURN-CODE > WS-RC-WARNING
               MOVE 'F'                    TO CC-FILLER(2:1)
           ELSE
               MOVE 'B'                    TO CC-FILLER(2:1)
           END-IF
           MOVE WS-STEP-NAME               TO CC-LAST-GOOD-STEP
      *
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'BRANCH FLAG REWRITE FAILED' TO ER-MESSAGE
               MOVE 0501                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           DISPLAY 'CBCRD05B - INTEREST BRANCH FLAG POSTED AS '
                   CC-FILLER(2:1)
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD05B SQL ERROR SQLCODE=' SQLCODE
                   ' TABLE=' ER-SQL-TABLE
                   ' ACCT=' DCL-ACCT-ID
           PERFORM 9500-FATAL-ERROR
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD05B FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
