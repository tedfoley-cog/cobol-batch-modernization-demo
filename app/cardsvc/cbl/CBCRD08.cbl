      ******************************************************************
      * CBCRD08 - DELINQUENCY ROLL AND AGING REPORT                    *
      *                                                                *
      * JOB CBCRD08J STEP010.                                          *
      *                                                                *
      * ROLLS ACCOUNT.DELQ_BUCKET FORWARD FOR EVERY OPEN ACCOUNT       *
      * WHOSE PAYMENT DUE DATE HAS PASSED AND WHOSE MINIMUM PAYMENT    *
      * HAS NOT BEEN MET, AND CLEARS THE BUCKET WHERE IT HAS.  THE     *
      * DELINQUENT AMOUNT IS THE UNPAID PART OF THE MINIMUM PAYMENT    *
      * PLUS ANYTHING ALREADY CARRIED.                                 *
      *                                                                *
      * BUCKETS                                                        *
      *   0  CURRENT                                                   *
      *   1  1 TO 29 DAYS                                              *
      *   2  30 TO 59 DAYS                                             *
      *   3  60 TO 89 DAYS                                             *
      *   4  90 TO 119 DAYS                                            *
      *   5  120 TO 149 DAYS                                           *
      *   6  150 DAYS AND OVER - CHARGE OFF CANDIDATE                  *
      *                                                                *
      * A BUCKET ONLY EVER MOVES ONE STEP PER CYCLE.  AN ACCOUNT THAT  *
      * MISSES A CYCLE OF PROCESSING IS THEREFORE UNDERSTATED RATHER   *
      * THAN OVERSTATED, WHICH IS WHAT COLLECTIONS ASKED FOR IN 1997.  *
      *                                                                *
      * THE AGING REPORT BREAKS ON PRODUCT AND WITHIN PRODUCT ON       *
      * BUCKET, WITH A FINAL REPORT TOTAL.                             *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - AGERPT   SYSOUT LRECL 133                        *
      *             - COLLECT  QSAM OUTPUT LRECL 100 COLLECTIONS FEED  *
      *             - CYCLCTL  VSAM KSDS UPDATE                        *
      * TABLES      - CARDSVC.ACCOUNT  (CURSOR, POSITIONED UPDATE)     *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 NORMAL                                      *
      *               0004 CHARGE OFF CANDIDATES PRESENT               *
      *               0012 FATAL                                       *
      * USER ABEND  - U0802 FILE OR VSAM FAILURE                       *
      *               U0803 UNRECOVERABLE SQL ERROR                    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD08.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1997-03-11.
      *
      * MAINTENANCE
      * 1997-03-11 CRD0912 ORIGINAL
      * 1999-06-30 CRD1533 SINGLE STEP ROLL ONLY - COLLECTIONS
      * 2004-09-14 CRD4110 GRACE DAYS APPLIED BEFORE BUCKET 1
      * 2011-02-21 CRD7302 COLLECTIONS EXTRACT ADDED AT BUCKET 3
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE   ASSIGN TO AGERPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REPORT-STATUS.
      *
           SELECT COLLECT-FILE  ASSIGN TO COLLECT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-COLLECT-STATUS.
      *
           SELECT CYCLCTL-FILE  ASSIGN TO CYCLCTL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CTL-KEY
                  FILE STATUS  IS WS-CYCLCTL-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  REPORT-REC                      PIC X(133).
      *
       FD  COLLECT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.
       01  COLLECT-REC                     PIC X(100).
      *
       FD  CYCLCTL-FILE
           RECORD CONTAINS 256 CHARACTERS.
       01  CYCLCTL-REC.
           05  CTL-KEY.
               10  CTL-CYCLE-TYPE          PIC X(8).
               10  CTL-CYCLE-DATE          PIC 9(8).
           05  CTL-REST                    PIC X(240).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD08 '.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-REPORT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REPORT-OK                      VALUE '00'.
           05  WS-COLLECT-STATUS           PIC X(2)  VALUE '00'.
               88  WS-COLLECT-OK                     VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-FIRST-SW                 PIC X     VALUE 'Y'.
               88  WS-FIRST-ROW                      VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-FETCH-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-ROLLED-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-CURED-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-HELD-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-COLLECT-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-CHARGEOFF-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE 99.
           05  WS-PAGE-CNT                 PIC 9(4)  VALUE ZERO.
      *
       01  WS-CONTROL-FIELDS.
           05  WS-HOLD-PRODUCT             PIC X(4)  VALUE SPACES.
           05  WS-HOLD-BUCKET              PIC 9     VALUE ZERO.
      *
       01  WS-ACCUMULATORS.
           05  WS-BKT-CNT                  PIC 9(9)      VALUE ZERO.
           05  WS-BKT-BAL                  PIC S9(13)V99 COMP-3
                                                         VALUE ZERO.
           05  WS-BKT-DELQ                 PIC S9(13)V99 COMP-3
                                                         VALUE ZERO.
           05  WS-PRD-CNT                  PIC 9(9)      VALUE ZERO.
           05  WS-PRD-BAL                  PIC S9(13)V99 COMP-3
                                                         VALUE ZERO.
           05  WS-PRD-DELQ                 PIC S9(13)V99 COMP-3
                                                         VALUE ZERO.
           05  WS-RPT-CNT                  PIC 9(9)      VALUE ZERO.
           05  WS-RPT-BAL                  PIC S9(13)V99 COMP-3
                                                         VALUE ZERO.
           05  WS-RPT-DELQ                 PIC S9(13)V99 COMP-3
                                                         VALUE ZERO.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYC-DT                       PIC X(10) VALUE SPACES.
       01  WS-GRACE-DAYS                   PIC 9(3)  VALUE 003.
      *
       01  WS-NEW-BUCKET                   PIC 9     VALUE ZERO.
       01  WS-NEW-DELQ-AMT                 PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
       01  WS-SHORTFALL                    PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-CONTROL-CARD                 PIC X(80) VALUE SPACES.
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
      ******************************************************************
      * BUCKET DESCRIPTIONS                                            *
      ******************************************************************
       01  WS-BUCKET-TEXT-TABLE.
           05  FILLER      PIC X(20) VALUE 'CURRENT             '.
           05  FILLER      PIC X(20) VALUE '1 TO 29 DAYS        '.
           05  FILLER      PIC X(20) VALUE '30 TO 59 DAYS       '.
           05  FILLER      PIC X(20) VALUE '60 TO 89 DAYS       '.
           05  FILLER      PIC X(20) VALUE '90 TO 119 DAYS      '.
           05  FILLER      PIC X(20) VALUE '120 TO 149 DAYS     '.
           05  FILLER      PIC X(20) VALUE '150 DAYS AND OVER   '.
       01  WS-BUCKET-TEXTS REDEFINES WS-BUCKET-TEXT-TABLE.
           05  WS-BUCKET-TEXT              PIC X(20) OCCURS 7 TIMES.
      *
      ******************************************************************
      * REPORT LINES                                                   *
      ******************************************************************
       01  RPT-HEAD-1.
           05  FILLER                      PIC X(9)  VALUE 'CBCRD08  '.
           05  FILLER                      PIC X(42) VALUE
               'CARD SERVICES - DELINQUENCY AGING REPORT  '.
           05  FILLER                      PIC X(11) VALUE
               'CYCLE DATE '.
           05  RH1-CYCLE-DATE              PIC 9(8).
           05  FILLER                      PIC X(48) VALUE SPACES.
           05  FILLER                      PIC X(5)  VALUE 'PAGE '.
           05  RH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(6)  VALUE SPACES.
      *
       01  RPT-HEAD-2.
           05  FILLER                      PIC X(9)  VALUE 'PRODUCT  '.
           05  FILLER                      PIC X(22) VALUE
               'BUCKET                '.
           05  FILLER                      PIC X(14) VALUE
               '      ACCOUNTS'.
           05  FILLER                      PIC X(22) VALUE
               '               BALANCE'.
           05  FILLER                      PIC X(22) VALUE
               '            DELINQUENT'.
           05  FILLER                      PIC X(44) VALUE SPACES.
      *
       01  RPT-BUCKET-LINE.
           05  RB-PRODUCT                  PIC X(4).
           05  FILLER                      PIC X(5)  VALUE SPACES.
           05  RB-BUCKET-NO                PIC 9.
           05  FILLER                      PIC X     VALUE SPACES.
           05  RB-BUCKET-TEXT              PIC X(20).
           05  RB-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RB-BALANCE                  PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RB-DELQ                     PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(51) VALUE SPACES.
      *
       01  RPT-BREAK-LINE.
           05  FILLER                      PIC X(9)  VALUE SPACES.
           05  RK-TEXT                     PIC X(22).
           05  RK-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RK-BALANCE                  PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RK-DELQ                     PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(51) VALUE SPACES.
      *
      ******************************************************************
      * COLLECTIONS FEED RECORD                                        *
      ******************************************************************
       01  COLLECT-DETAIL.
           05  CO-ACCT-ID                  PIC 9(11).
           05  CO-CUST-ID                  PIC 9(9).
           05  CO-PARTY-ID                 PIC X(11).
           05  CO-PRODUCT-CD               PIC X(4).
           05  CO-CYCLE-DATE               PIC 9(8).
           05  CO-BUCKET                   PIC 9.
           05  CO-DELQ-AMT                 PIC S9(11)V99 COMP-3.
           05  CO-CURR-BAL                 PIC S9(11)V99 COMP-3.
           05  CO-MIN-PAY-DUE              PIC S9(9)V99  COMP-3.
           05  CO-LAST-PAY-DATE            PIC 9(8).
           05  CO-DAYS-OVERDUE             PIC 9(4).
           05  CO-FILLER                   PIC X(24).
      *
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-ACCT.
           05  DCL-ACCT-ID                 PIC S9(11)   COMP-3.
           05  DCL-CUST-ID                 PIC S9(9)    COMP-3.
           05  DCL-PARTY-ID                PIC X(11).
           05  DCL-PRODUCT-CD              PIC X(4).
           05  DCL-CURR-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-MIN-PAY-DUE             PIC S9(9)V99  COMP-3.
           05  DCL-LAST-PAY-AMT            PIC S9(9)V99  COMP-3.
           05  DCL-LAST-PAY-DATE           PIC X(10).
           05  DCL-PAY-DUE-DATE            PIC X(10).
           05  DCL-DELQ-BUCKET             PIC S9(4)    COMP.
           05  DCL-DELQ-AMT                PIC S9(9)V99  COMP-3.
           05  DCL-DAYS-OVERDUE            PIC S9(9)    COMP-3.
      *
       01  IND-VARS.
           05  IND-LAST-PAY-DATE           PIC S9(4) COMP.
           05  IND-PAY-DUE-DATE            PIC S9(4) COMP.
      *
       01  DCL-NEW-BUCKET                  PIC S9(4)     COMP.
       01  DCL-NEW-DELQ                    PIC S9(9)V99  COMP-3.
      *
      ******************************************************************
      * DAYS OVERDUE IS CALCULATED BY DB2 RATHER THAN IN COBOL - THE   *
      * ACCOUNTS CARRY BOTH 8 DIGIT DB2 DATES AND, HISTORICALLY, 6     *
      * DIGIT ONES ON THE FEED FILES, AND THE ARITHMETIC IS SAFER      *
      * DONE ONCE HERE.                                                *
      ******************************************************************
           EXEC SQL DECLARE DELQCSR CURSOR FOR
               SELECT ACCT_ID
                    , CUST_ID
                    , PARTY_ID
                    , PRODUCT_CD
                    , CURR_BAL
                    , MIN_PAY_DUE
                    , LAST_PAY_AMT
                    , CHAR(LAST_PAY_DATE, ISO)
                    , CHAR(PAY_DUE_DATE, ISO)
                    , DELQ_BUCKET
                    , DELQ_AMT
                    , COALESCE(DAYS(DATE(:WS-CYC-DT))
                             - DAYS(PAY_DUE_DATE), 0)
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_STATUS IN ('O','S')
                  AND (DELQ_BUCKET > 0
                   OR (PAY_DUE_DATE < DATE(:WS-CYC-DT)
                  AND  MIN_PAY_DUE  > 0))
                ORDER BY PRODUCT_CD
                       , DELQ_BUCKET
                       , ACCT_ID
                  FOR UPDATE OF DELQ_BUCKET
                              , DELQ_AMT
                              , LAST_MAINT_PGM
                              , LAST_MAINT_TS
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
               UNTIL WS-EOF
           PERFORM 3000-FINAL-BREAKS
           PERFORM 4000-TERMINATE
           MOVE WS-RETURN-CODE             TO RETURN-CODE
           GOBACK
           .
      *
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           STRING WS-CD-DATE(1:4) '-' WS-CD-DATE(5:2) '-'
                  WS-CD-DATE(7:2) '-' WS-CD-TIME(1:2) '.'
                  WS-CD-TIME(3:2) '.' WS-CD-TIME(5:2) '.000000'
             DELIMITED BY SIZE INTO WS-TIMESTAMP
           END-STRING
      *
           IF LK-PARM-LEN < 8
               MOVE 'PARM MUST SUPPLY CCYYMMDD CYCLE DATE'
                                           TO ER-MESSAGE
               MOVE 0802                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO WS-CYC-DT
           END-STRING
           MOVE WS-CYCLE-DATE              TO RH1-CYCLE-DATE
      *
           PERFORM 1100-READ-CONTROL-CARDS
      *
           OPEN OUTPUT REPORT-FILE
                       COLLECT-FILE
           OPEN I-O    CYCLCTL-FILE
           IF NOT WS-REPORT-OK OR NOT WS-COLLECT-OK
               MOVE 'AGERPT  '             TO ER-FILE-NAME
               MOVE WS-REPORT-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF REPORT OR COLLECTIONS FILE FAILED'
                                           TO ER-MESSAGE
               MOVE 0802                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM 1200-UPDATE-CYCLE-STEP
      *
           EXEC SQL OPEN DELQCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          '    TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               MOVE 'OPEN OF DELQCSR FAILED' TO ER-MESSAGE
               MOVE 0803                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           PERFORM 1900-FETCH-ACCOUNT
           .
      *
       1100-READ-CONTROL-CARDS.
           ACCEPT WS-CONTROL-CARD FROM SYSIN
           PERFORM UNTIL WS-CONTROL-CARD = SPACES
                      OR WS-CONTROL-CARD(1:3) = 'END'
               EVALUATE WS-CONTROL-CARD(1:11)
                   WHEN 'GRACE-DAYS='
                       MOVE WS-CONTROL-CARD(12:3)
                                           TO WS-GRACE-DAYS
                   WHEN OTHER
                       IF WS-CONTROL-CARD(1:1) NOT = '*'
                           DISPLAY 'CBCRD08 - CARD IGNORED '
                                   WS-CONTROL-CARD(1:40)
                       END-IF
               END-EVALUATE
               MOVE SPACES                 TO WS-CONTROL-CARD
               ACCEPT WS-CONTROL-CARD FROM SYSIN
           END-PERFORM
           DISPLAY 'CBCRD08 - GRACE DAYS ' WS-GRACE-DAYS
           .
      *
       1200-UPDATE-CYCLE-STEP.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 0802               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE 'CBCRD08 '                 TO CC-CURRENT-STEP
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'REWRITE OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0802                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       1900-FETCH-ACCOUNT.
           EXEC SQL
               FETCH DELQCSR
                INTO :DCL-ACCT-ID
                   , :DCL-CUST-ID
                   , :DCL-PARTY-ID
                   , :DCL-PRODUCT-CD
                   , :DCL-CURR-BAL
                   , :DCL-MIN-PAY-DUE
                   , :DCL-LAST-PAY-AMT
                   , :DCL-LAST-PAY-DATE :IND-LAST-PAY-DATE
                   , :DCL-PAY-DUE-DATE   :IND-PAY-DUE-DATE
                   , :DCL-DELQ-BUCKET
                   , :DCL-DELQ-AMT
                   , :DCL-DAYS-OVERDUE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-FETCH-CNT
               WHEN 100
                   MOVE 'Y'                TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'ACCOUNT          ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '          TO ER-SQL-OPERATION
                   MOVE 'FETCH OF DELQCSR FAILED' TO ER-MESSAGE
                   MOVE 0803                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2000 - ONE ACCOUNT.  CONTROL BREAKS ARE TAKEN BEFORE THE ROW   *
      *        IS ACCUMULATED SO THAT THE BUCKET AND PRODUCT LINES     *
      *        DESCRIBE THE GROUP THAT HAS JUST ENDED.                 *
      ******************************************************************
       2000-PROCESS-ACCOUNT.
           IF WS-FIRST-ROW
               MOVE DCL-PRODUCT-CD         TO WS-HOLD-PRODUCT
               MOVE DCL-DELQ-BUCKET        TO WS-HOLD-BUCKET
               MOVE 'N'                    TO WS-FIRST-SW
           ELSE
               IF DCL-PRODUCT-CD NOT = WS-HOLD-PRODUCT
                   PERFORM 2900-BUCKET-BREAK
                   PERFORM 2950-PRODUCT-BREAK
                   MOVE DCL-PRODUCT-CD     TO WS-HOLD-PRODUCT
                   MOVE DCL-DELQ-BUCKET    TO WS-HOLD-BUCKET
               ELSE
                   IF DCL-DELQ-BUCKET NOT = WS-HOLD-BUCKET
                       PERFORM 2900-BUCKET-BREAK
                       MOVE DCL-DELQ-BUCKET TO WS-HOLD-BUCKET
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 2100-ROLL-BUCKET
           PERFORM 2200-UPDATE-ACCOUNT
           PERFORM 2300-ACCUMULATE
      *
           IF WS-NEW-BUCKET >= 3
               PERFORM 2400-WRITE-COLLECTIONS
           END-IF
      *
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQUENCY
               EXEC SQL COMMIT WORK END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
      *
           PERFORM 1900-FETCH-ACCOUNT
           .
      *
      ******************************************************************
      * 2100 - THE ROLL ITSELF                                         *
      ******************************************************************
       2100-ROLL-BUCKET.
           MOVE DCL-DELQ-BUCKET            TO WS-NEW-BUCKET
           MOVE DCL-DELQ-AMT               TO WS-NEW-DELQ-AMT
      *
           COMPUTE WS-SHORTFALL =
                   DCL-MIN-PAY-DUE - DCL-LAST-PAY-AMT
           IF WS-SHORTFALL < ZERO
               MOVE ZERO                   TO WS-SHORTFALL
           END-IF
      *
           EVALUATE TRUE
      *
      *        NOTHING OWING AND NOTHING CARRIED - LEAVE IT ALONE.
               WHEN WS-SHORTFALL = ZERO
                AND DCL-DELQ-BUCKET = ZERO
                   ADD 1                   TO WS-HELD-CNT
                   GO TO 2100-EXIT
      *
      *        THE MINIMUM PAYMENT WAS MET.  THE ACCOUNT CURES IN ONE
      *        MOVE - COLLECTIONS DO NOT WANT A STAGED RECOVERY.
               WHEN WS-SHORTFALL = ZERO
                   MOVE ZERO               TO WS-NEW-BUCKET
                                              WS-NEW-DELQ-AMT
                   ADD 1                   TO WS-CURED-CNT
      *
      *        STILL INSIDE THE GRACE PERIOD - HELD AT ITS CURRENT
      *        BUCKET FOR ANOTHER CYCLE.
               WHEN DCL-DAYS-OVERDUE <= WS-GRACE-DAYS
                AND DCL-DELQ-BUCKET = ZERO
                   ADD 1                   TO WS-HELD-CNT
                   GO TO 2100-EXIT
      *
               WHEN DCL-DELQ-BUCKET >= 6
                   MOVE 6                  TO WS-NEW-BUCKET
                   COMPUTE WS-NEW-DELQ-AMT =
                           DCL-DELQ-AMT + WS-SHORTFALL
                   ADD 1                   TO WS-CHARGEOFF-CNT
                   MOVE WS-RC-WARNING      TO WS-RETURN-CODE
      *
               WHEN OTHER
                   COMPUTE WS-NEW-BUCKET = DCL-DELQ-BUCKET + 1
                   COMPUTE WS-NEW-DELQ-AMT =
                           DCL-DELQ-AMT + WS-SHORTFALL
                   ADD 1                   TO WS-ROLLED-CNT
                   IF WS-NEW-BUCKET = 6
                       ADD 1               TO WS-CHARGEOFF-CNT
                       MOVE WS-RC-WARNING  TO WS-RETURN-CODE
                   END-IF
           END-EVALUATE
           .
       2100-EXIT.
           EXIT
           .
      *
       2200-UPDATE-ACCOUNT.
           MOVE WS-NEW-BUCKET              TO DCL-NEW-BUCKET
           MOVE WS-NEW-DELQ-AMT            TO DCL-NEW-DELQ
      *
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET DELQ_BUCKET    = :DCL-NEW-BUCKET
                    , DELQ_AMT       = :DCL-NEW-DELQ
                    , LAST_MAINT_PGM = 'CBCRD08 '
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE CURRENT OF DELQCSR
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          '    TO ER-SQL-TABLE
               MOVE 'UPDATE  '             TO ER-SQL-OPERATION
               MOVE 'POSITIONED UPDATE OF ACCOUNT FAILED'
                                           TO ER-MESSAGE
               MOVE 0803                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           ADD 1                           TO WS-SINCE-COMMIT
           .
      *
       2300-ACCUMULATE.
           ADD 1                           TO WS-BKT-CNT
                                              WS-PRD-CNT
                                              WS-RPT-CNT
           ADD DCL-CURR-BAL                TO WS-BKT-BAL
                                              WS-PRD-BAL
                                              WS-RPT-BAL
           ADD WS-NEW-DELQ-AMT             TO WS-BKT-DELQ
                                              WS-PRD-DELQ
                                              WS-RPT-DELQ
           .
      *
       2400-WRITE-COLLECTIONS.
           MOVE DCL-ACCT-ID                TO CO-ACCT-ID
           MOVE DCL-CUST-ID                TO CO-CUST-ID
           MOVE DCL-PARTY-ID               TO CO-PARTY-ID
           MOVE DCL-PRODUCT-CD             TO CO-PRODUCT-CD
           MOVE WS-CYCLE-DATE              TO CO-CYCLE-DATE
           MOVE WS-NEW-BUCKET              TO CO-BUCKET
           MOVE WS-NEW-DELQ-AMT            TO CO-DELQ-AMT
           MOVE DCL-CURR-BAL               TO CO-CURR-BAL
           MOVE DCL-MIN-PAY-DUE            TO CO-MIN-PAY-DUE
           MOVE DCL-DAYS-OVERDUE           TO CO-DAYS-OVERDUE
      *
           IF IND-LAST-PAY-DATE < 0
               MOVE ZERO                   TO CO-LAST-PAY-DATE
           ELSE
               MOVE DCL-LAST-PAY-DATE(1:4) TO CO-LAST-PAY-DATE(1:4)
               MOVE DCL-LAST-PAY-DATE(6:2) TO CO-LAST-PAY-DATE(5:2)
               MOVE DCL-LAST-PAY-DATE(9:2) TO CO-LAST-PAY-DATE(7:2)
           END-IF
           MOVE SPACES                     TO CO-FILLER
      *
           WRITE COLLECT-REC FROM COLLECT-DETAIL
           IF NOT WS-COLLECT-OK
               MOVE 'COLLECT '             TO ER-FILE-NAME
               MOVE WS-COLLECT-STATUS      TO ER-FILE-STATUS
               MOVE 'WRITE TO COLLECTIONS FEED FAILED' TO ER-MESSAGE
               MOVE 0802                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           ADD 1                           TO WS-COLLECT-CNT
           .
      *
       2900-BUCKET-BREAK.
           IF WS-LINE-CNT > 55
               PERFORM 2990-PRINT-HEADINGS
           END-IF
      *
           MOVE WS-HOLD-PRODUCT            TO RB-PRODUCT
           MOVE WS-HOLD-BUCKET             TO RB-BUCKET-NO
           MOVE WS-BUCKET-TEXT(WS-HOLD-BUCKET + 1)
                                           TO RB-BUCKET-TEXT
           MOVE WS-BKT-CNT                 TO RB-COUNT
           MOVE WS-BKT-BAL                 TO RB-BALANCE
           MOVE WS-BKT-DELQ                TO RB-DELQ
           WRITE REPORT-REC FROM RPT-BUCKET-LINE
           ADD 1                           TO WS-LINE-CNT
      *
           MOVE ZERO                       TO WS-BKT-CNT
                                              WS-BKT-BAL
                                              WS-BKT-DELQ
           .
      *
       2950-PRODUCT-BREAK.
           MOVE 'TOTAL FOR PRODUCT'        TO RK-TEXT
           MOVE WS-PRD-CNT                 TO RK-COUNT
           MOVE WS-PRD-BAL                 TO RK-BALANCE
           MOVE WS-PRD-DELQ                TO RK-DELQ
           WRITE REPORT-REC FROM RPT-BREAK-LINE
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
           ADD 2                           TO WS-LINE-CNT
      *
           MOVE ZERO                       TO WS-PRD-CNT
                                              WS-PRD-BAL
                                              WS-PRD-DELQ
           .
      *
       2990-PRINT-HEADINGS.
           ADD 1                           TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT                TO RH1-PAGE
           WRITE REPORT-REC FROM RPT-HEAD-1
           WRITE REPORT-REC FROM RPT-HEAD-2
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
           MOVE 4                          TO WS-LINE-CNT
           .
      *
       3000-FINAL-BREAKS.
           EXEC SQL CLOSE DELQCSR END-EXEC
      *
           IF WS-FETCH-CNT > ZERO
               PERFORM 2900-BUCKET-BREAK
               PERFORM 2950-PRODUCT-BREAK
           END-IF
      *
           MOVE 'REPORT TOTAL'             TO RK-TEXT
           MOVE WS-RPT-CNT                 TO RK-COUNT
           MOVE WS-RPT-BAL                 TO RK-BALANCE
           MOVE WS-RPT-DELQ                TO RK-DELQ
           WRITE REPORT-REC FROM RPT-BREAK-LINE
      *
           EXEC SQL COMMIT WORK END-EXEC
           .
      *
       4000-TERMINATE.
           CLOSE REPORT-FILE
                 COLLECT-FILE
                 CYCLCTL-FILE
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-FETCH-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD08 ACCOUNTS EXAMINED  ' WS-DISPLAY-CNT
           MOVE WS-ROLLED-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD08 BUCKETS ROLLED     ' WS-DISPLAY-CNT
           MOVE WS-CURED-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD08 ACCOUNTS CURED     ' WS-DISPLAY-CNT
           MOVE WS-HELD-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD08 HELD IN GRACE      ' WS-DISPLAY-CNT
           MOVE WS-COLLECT-CNT             TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD08 COLLECTIONS FEED   ' WS-DISPLAY-CNT
           MOVE WS-CHARGEOFF-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD08 CHARGE OFF CANDS   ' WS-DISPLAY-CNT
           MOVE WS-RPT-DELQ                TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD08 DELINQUENT TOTAL  ' WS-DISPLAY-AMT
           DISPLAY '----------------------------------------------'
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD08 SQL ERROR SQLCODE=' SQLCODE
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
           DISPLAY 'CBCRD08 FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
