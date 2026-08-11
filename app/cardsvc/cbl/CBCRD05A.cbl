      ******************************************************************
      * CBCRD05A - FEE ASSESSMENT BRANCH                               *
      *                                                                *
      * STEP 5A OF THE CARDNITE CYCLE.  JOB CBCRD05AJ.                 *
      * SUBMITTED IN PARALLEL WITH CBCRD05BJ AFTER CBCRD04J ENDS.      *
      *                                                                *
      * FOR EVERY ACCOUNT TOUCHED BY THE CYCLE THE APPLICABLE FEE      *
      * TYPES ARE READ FROM CARDSVC.FEE_SCHEDULE FOR THE ACCOUNT'S     *
      * PRODUCT.  EACH FEE TYPE IS HANDED TO ITS OWN HANDLER THROUGH   *
      * THE BATCH DISPATCHER - THIS PROGRAM DOES NOT KNOW WHICH LOAD   *
      * MODULE SERVES A FEE TYPE, ONLY THE ROUTE KEY.  ADDING A FEE    *
      * TYPE IS A PGM_ROUTE AND FEE_SCHEDULE CHANGE, NOT A RECOMPILE.  *
      *                                                                *
      * COMPILE WITH DYNAM.                                            *
      *                                                                *
      * AS ITS LAST ACT THE PROGRAM POSTS THE FEE BRANCH COMPLETION    *
      * FLAG INTO THE CYCLE CONTROL FILE.  CBCRD06W WILL NOT LET THE   *
      * JOIN RUN UNTIL BOTH BRANCH FLAGS ARE PRESENT.                  *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD90 (BATCH DISPATCHER, ROUTE TYPE FEEC)      *
      *             - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - CYCLCTL  VSAM KSDS I-O                           *
      *             - FEEAUDIT QSAM OUTPUT LRECL 133                   *
      * TABLES      - CARDSVC.ACCOUNT       (CURSOR)                   *
      *             - CARDSVC.FEE_SCHEDULE  (CURSOR)                   *
      *             - CARDSVC.TRANSACTION   (INSERT)                   *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 ALL FEES ASSESSED                           *
      *               0004 ONE OR MORE HANDLERS RETURNED A WARNING     *
      *               0012 FATAL                                       *
      * USER ABEND  - U0501 CYCLE CONTROL UNUSABLE                     *
      *               U0503 UNRECOVERABLE SQL ERROR                    *
      *               U0505 FEE HANDLER RETURNED A FATAL CONDITION     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD05A.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1999-01-22.
      *
      * MAINTENANCE
      * 1999-01-22 CRD0402 ORIGINAL - FEES CODED INLINE
      * 2002-10-11 CRD2810 FEE HANDLERS SPLIT OUT AND DISPATCHED
      *                    THROUGH THE ROUTE TABLE
      * 2011-05-06 CRD7204 WAIVER RULE HONOURED BEFORE DISPATCH
      * 2018-08-13 CRD9902 BRANCH COMPLETION FLAG POSTED FOR CBCRD06W
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
           SELECT AUDIT-FILE    ASSIGN TO FEEAUDIT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-AUDIT-STATUS.
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
       FD  AUDIT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  AUDIT-REC                       PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD05A'.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
           05  WS-AUDIT-STATUS             PIC X(2)  VALUE '00'.
               88  WS-AUDIT-OK                       VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-ACCT-EOF-SW              PIC X     VALUE 'N'.
               88  WS-ACCT-EOF                       VALUE 'Y'.
           05  WS-FEE-EOF-SW               PIC X     VALUE 'N'.
               88  WS-FEE-EOF                        VALUE 'Y'.
           05  WS-WAIVED-SW                PIC X     VALUE 'N'.
               88  WS-FEE-WAIVED                     VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-ACCT-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-FEE-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-DISPATCH-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-WAIVED-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-WARNING-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-DECLINE-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-POSTED-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-COMMIT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
      *
       01  WS-FEE-TOTAL                    PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
       01  WS-FEE-AMT                      PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYC-DT               PIC X(10) VALUE SPACES.
       01  WS-CYCLE-ID                     PIC X(8)  VALUE SPACES.
      *
       01  WS-TXN-ID                       PIC X(16) VALUE SPACES.
       01  WS-TXN-ID-R REDEFINES WS-TXN-ID.
           05  WS-TXN-PREFIX               PIC X(2).
           05  WS-TXN-DATE-PART            PIC 9(6).
           05  WS-TXN-SERIAL               PIC 9(8).
       01  WS-TXN-SERIAL-NBR               PIC 9(8)  VALUE ZERO.
      *
       01  WS-CURRENT-DATE.
           05  WS-CD-DATE                  PIC 9(8).
           05  WS-CD-TIME                  PIC 9(8).
           05  WS-CD-FILLER                PIC X(5).
       01  WS-TIMESTAMP                    PIC X(26) VALUE SPACES.
      *
       01  WS-RETURN-CODE                  PIC 9(4)  VALUE ZERO.
       01  WS-WORST-RC                     PIC 9(4)  VALUE ZERO.
       01  WS-ABEND-CODE                   PIC 9(4)  VALUE ZERO.
      *
      *    PARAMETER AREA HANDED TO THE FEE HANDLER.  THE DISPATCHER
      *    PASSES IT STRAIGHT THROUGH WITHOUT LOOKING INSIDE.
       01  FEE-REQUEST-AREA.
           05  FR-VERSION                  PIC 9(4)  VALUE 0002.
           05  FR-CALLER-ID                PIC X(8)  VALUE 'CBCRD05A'.
           05  FR-CYCLE-DATE               PIC 9(8).
           05  FR-CYCLE-ID                 PIC X(8).
           05  FR-ACCT-ID                  PIC 9(11).
           05  FR-CUST-ID                  PIC 9(9).
           05  FR-CARD-NUM                 PIC X(16).
           05  FR-PRODUCT-CD               PIC X(4).
           05  FR-FEE-TYPE                 PIC X(4).
           05  FR-CURRENCY                 PIC X(3).
           05  FR-FLAT-AMT                 PIC S9(9)V99 COMP-3.
           05  FR-PCT-RATE                 PIC S9(3)V9(5) COMP-3.
           05  FR-MIN-AMT                  PIC S9(9)V99 COMP-3.
           05  FR-MAX-AMT                  PIC S9(9)V99 COMP-3.
           05  FR-WAIVER-RULE              PIC X(4).
           05  FR-CURR-BAL                 PIC S9(11)V99 COMP-3.
           05  FR-CASH-BAL                 PIC S9(11)V99 COMP-3.
           05  FR-CREDIT-LIMIT             PIC S9(11)V99 COMP-3.
           05  FR-DELQ-BUCKET              PIC 9.
           05  FR-LAST-PAY-DATE            PIC 9(8).
      *    HANDLER OUTPUT
           05  FR-ASSESSED-AMT             PIC S9(9)V99 COMP-3.
           05  FR-WAIVED-FLG               PIC X.
           05  FR-GL-ACCOUNT               PIC X(10).
           05  FR-NARRATIVE                PIC X(40).
           05  FR-REASON-CD                PIC X(4).
           05  FR-FILLER                   PIC X(319).
      *
       01  WS-RETURN-AREA.
           05  WS-RETURN-CD                PIC S9(4) COMP VALUE ZERO.
           05  WS-RETURN-PGM               PIC X(8)  VALUE SPACES.
           05  WS-RETURN-MSG               PIC X(60) VALUE SPACES.
      *
       01  WS-AUDIT-LINE.
           05  AL-ACCT-ID                  PIC 9(11).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  AL-FEE-TYPE                 PIC X(4).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  AL-AMOUNT                   PIC ---,---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  AL-WAIVED                   PIC X.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  AL-RC                       PIC 9(4).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  AL-MSG                      PIC X(60).
           05  FILLER                      PIC X(30) VALUE SPACES.
      *
           COPY CVROUT01Y.
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
           05  DCL-CASH-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-DELQ-BUCKET             PIC S9(4)     COMP.
           05  DCL-LAST-PAY-DATE           PIC X(10).
      *
       01  DCL-FEE.
           05  DCL-FEE-TYPE                PIC X(4).
           05  DCL-FLAT-AMT                PIC S9(9)V99  COMP-3.
           05  DCL-PCT-RATE                PIC S9(3)V9(5) COMP-3.
           05  DCL-MIN-AMT                 PIC S9(9)V99  COMP-3.
           05  DCL-MAX-AMT                 PIC S9(9)V99  COMP-3.
           05  DCL-WAIVER-RULE             PIC X(4).
           05  DCL-FEE-CURRENCY            PIC X(3).
      *
       01  DCL-LIMIT.
           05  DCL-CREDIT-LIMIT            PIC S9(11)V99 COMP-3.
           05  DCL-CARD-NUM                PIC X(16).
      *
       01  DCL-TXN.
           05  DCL-TXN-ID                  PIC X(16).
           05  DCL-TXN-AMT                 PIC S9(11)V99 COMP-3.
           05  DCL-LEG-DATA.
               10  DCL-LEG-LEN             PIC S9(4) COMP.
               10  DCL-LEG-TXT             PIC X(400).
           05  DCL-NARRATIVE               PIC X(40).
      *
       01  IND-VARS.
           05  IND-LAST-PAY                PIC S9(4) COMP.
           05  IND-WAIVER                  PIC S9(4) COMP.
      *
       01  WS-LEG-IMAGE.
           05  WS-LI-SEQ                   PIC 9(2).
           05  WS-LI-TYPE                  PIC X(4).
           05  WS-LI-AMT                   PIC S9(11)V99 COMP-3.
           05  WS-LI-GL                    PIC X(10).
           05  WS-LI-REVERSED              PIC X.
      *
      *    ACCOUNTS TOUCHED BY THE CYCLE
           EXEC SQL DECLARE ACCTCSR CURSOR WITH HOLD FOR
               SELECT DISTINCT A.ACCT_ID
                    , A.CUST_ID
                    , A.PRODUCT_CD
                    , A.CURRENCY_CD
                    , A.CURR_BAL
                    , A.CASH_BAL
                    , A.DELQ_BUCKET
                    , CHAR(A.LAST_PAY_DATE, ISO)
                 FROM CARDSVC.ACCOUNT A
                WHERE A.ACCT_STATUS = 'O'
                  AND EXISTS (SELECT 1
                                FROM CARDSVC.TRANSACTION T
                               WHERE T.ACCT_ID   = A.ACCT_ID
                                 AND T.POST_DATE = DATE(:WS-CYCLE-DATE-
                                     SQL))
                ORDER BY A.ACCT_ID
           END-EXEC.
      *
      *    FEE TYPES IN FORCE FOR THE PRODUCT ON THE CYCLE DATE
           EXEC SQL DECLARE FEECSR CURSOR FOR
               SELECT FEE_TYPE
                    , FLAT_AMT
                    , PCT_RATE
                    , MIN_AMT
                    , MAX_AMT
                    , WAIVER_RULE_CD
                    , CURRENCY_CD
                 FROM CARDSVC.FEE_SCHEDULE
                WHERE PRODUCT_CD = :DCL-PRODUCT-CD
                  AND DATE(:WS-CYC-DT)
                      BETWEEN EFF_DATE AND EXP_DATE
                ORDER BY FEE_TYPE
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
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           STRING WS-CD-DATE(1:4) '-' WS-CD-DATE(5:2) '-'
                  WS-CD-DATE(7:2) '-' WS-CD-TIME(1:2) '.'
                  WS-CD-TIME(3:2) '.' WS-CD-TIME(5:2) '.000000'
             DELIMITED BY SIZE INTO WS-TIMESTAMP
           END-STRING
      *
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO WS-CYC-DT
           END-STRING
      *
           MOVE 'FE'                       TO WS-TXN-PREFIX
           MOVE WS-CYCLE-DATE(3:6)         TO WS-TXN-DATE-PART
      *
           OPEN I-O    CYCLCTL-FILE
           OPEN OUTPUT AUDIT-FILE
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
           EXEC SQL OPEN ACCTCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          '    TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               MOVE 'OPEN OF ACCTCSR FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           DISPLAY 'CBCRD05A - FEE BRANCH, CYCLE ' WS-CYCLE-DATE
           PERFORM 1900-FETCH-ACCOUNT
           .
      *
       1900-FETCH-ACCOUNT.
           EXEC SQL
               FETCH ACCTCSR
                INTO :DCL-ACCT-ID
                   , :DCL-CUST-ID
                   , :DCL-PRODUCT-CD
                   , :DCL-CURRENCY-CD
                   , :DCL-CURR-BAL
                   , :DCL-CASH-BAL
                   , :DCL-DELQ-BUCKET
                   , :DCL-LAST-PAY-DATE :IND-LAST-PAY
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
                   MOVE 'FETCH OF ACCTCSR FAILED' TO ER-MESSAGE
                   MOVE 0503                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2000 - ONE ACCOUNT                                             *
      ******************************************************************
       2000-PROCESS-ACCOUNT.
           PERFORM 2050-READ-LIMIT
           PERFORM 2100-OPEN-FEE-CURSOR
           MOVE 'N'                        TO WS-FEE-EOF-SW
           PERFORM 2200-PROCESS-FEE-TYPE
               UNTIL WS-FEE-EOF
           PERFORM 2900-CLOSE-FEE-CURSOR
      *
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQUENCY
               PERFORM 2950-COMMIT-POINT
           END-IF
      *
           PERFORM 1900-FETCH-ACCOUNT
           .
      *
       2050-READ-LIMIT.
           MOVE ZERO                       TO DCL-CREDIT-LIMIT
           MOVE SPACES                     TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT MAX(L.LIMIT_AMT)
                    , MIN(L.CARD_NUM)
                 INTO :DCL-CREDIT-LIMIT
                    , :DCL-CARD-NUM
                 FROM CARDSVC.CARD_LIMIT L
                    , CARDSVC.CARD C
                WHERE C.ACCT_ID    = :DCL-ACCT-ID
                  AND L.CARD_NUM   = C.CARD_NUM
                  AND L.LIMIT_TYPE = 'CRED'
                  AND DATE(:WS-CYC-DT)
                      BETWEEN L.EFF_DATE AND L.EXP_DATE
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               MOVE 'CARD_LIMIT       '    TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               MOVE 'SELECT ON CARD_LIMIT FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           .
      *
       2100-OPEN-FEE-CURSOR.
           EXEC SQL OPEN FEECSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'FEE_SCHEDULE     '    TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               MOVE 'OPEN OF FEECSR FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           .
      *
       2200-PROCESS-FEE-TYPE.
           EXEC SQL
               FETCH FEECSR
                INTO :DCL-FEE-TYPE
                   , :DCL-FLAT-AMT
                   , :DCL-PCT-RATE
                   , :DCL-MIN-AMT
                   , :DCL-MAX-AMT
                   , :DCL-WAIVER-RULE :IND-WAIVER
                   , :DCL-FEE-CURRENCY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-FEE-CNT
                   PERFORM 2300-BUILD-FEE-REQUEST
                   PERFORM 2400-DISPATCH-FEE-HANDLER
                   PERFORM 2500-APPLY-HANDLER-RESULT
               WHEN 100
                   MOVE 'Y'                TO WS-FEE-EOF-SW
               WHEN OTHER
                   MOVE 'FEE_SCHEDULE     ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '          TO ER-SQL-OPERATION
                   MOVE 'FETCH OF FEECSR FAILED' TO ER-MESSAGE
                   MOVE 0503                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       2300-BUILD-FEE-REQUEST.
           INITIALIZE FEE-REQUEST-AREA
           MOVE 0002                       TO FR-VERSION
           MOVE WS-PROGRAM-ID              TO FR-CALLER-ID
           MOVE WS-CYCLE-DATE              TO FR-CYCLE-DATE
           MOVE WS-CYCLE-ID                TO FR-CYCLE-ID
           MOVE DCL-ACCT-ID                TO FR-ACCT-ID
           MOVE DCL-CUST-ID                TO FR-CUST-ID
           MOVE DCL-CARD-NUM               TO FR-CARD-NUM
           MOVE DCL-PRODUCT-CD             TO FR-PRODUCT-CD
           MOVE DCL-FEE-TYPE               TO FR-FEE-TYPE
           MOVE DCL-FEE-CURRENCY           TO FR-CURRENCY
           MOVE DCL-FLAT-AMT               TO FR-FLAT-AMT
           MOVE DCL-PCT-RATE               TO FR-PCT-RATE
           MOVE DCL-MIN-AMT                TO FR-MIN-AMT
           MOVE DCL-MAX-AMT                TO FR-MAX-AMT
           MOVE DCL-CURR-BAL               TO FR-CURR-BAL
           MOVE DCL-CASH-BAL               TO FR-CASH-BAL
           MOVE DCL-CREDIT-LIMIT           TO FR-CREDIT-LIMIT
           MOVE DCL-DELQ-BUCKET            TO FR-DELQ-BUCKET
      *
           IF IND-WAIVER < 0
               MOVE SPACES                 TO FR-WAIVER-RULE
           ELSE
               MOVE DCL-WAIVER-RULE        TO FR-WAIVER-RULE
           END-IF
      *
           IF IND-LAST-PAY < 0
               MOVE ZERO                   TO FR-LAST-PAY-DATE
           ELSE
               MOVE DCL-LAST-PAY-DATE(1:4) TO FR-LAST-PAY-DATE(1:4)
               MOVE DCL-LAST-PAY-DATE(6:2) TO FR-LAST-PAY-DATE(5:2)
               MOVE DCL-LAST-PAY-DATE(9:2) TO FR-LAST-PAY-DATE(7:2)
           END-IF
      *
           MOVE ZERO                       TO FR-ASSESSED-AMT
           MOVE 'N'                        TO FR-WAIVED-FLG
           .
      *
      ******************************************************************
      * 2400 - DISPATCH                                                *
      *                                                                *
      * THE FEE TYPE IS THE ROUTE KEY.  THE DISPATCHER RESOLVES IT TO  *
      * A LOAD MODULE FROM CARDSVC.PGM_ROUTE AND CALLS IT DYNAMICALLY. *
      ******************************************************************
       2400-DISPATCH-FEE-HANDLER.
           MOVE SPACES                     TO ROUTE-REQUEST
           MOVE 'FEEC'                     TO RQ-ROUTE-TYPE
           MOVE DCL-FEE-TYPE               TO RQ-ROUTE-KEY
           MOVE 1                          TO RQ-SEQ-NBR
           MOVE ZERO                       TO RQ-RC
      *
           MOVE ZERO                       TO WS-RETURN-CD
           MOVE SPACES                     TO WS-RETURN-PGM
                                              WS-RETURN-MSG
      *
           CALL 'CBCRD90' USING ROUTE-REQUEST
                                FEE-REQUEST-AREA
                                WS-RETURN-AREA
      *
           ADD 1                           TO WS-DISPATCH-CNT
           .
      *
       2500-APPLY-HANDLER-RESULT.
           EVALUATE WS-RETURN-CD
               WHEN 0
                   CONTINUE
               WHEN 4
                   ADD 1                   TO WS-WARNING-CNT
                   IF WS-WORST-RC < WS-RC-WARNING
                       MOVE WS-RC-WARNING  TO WS-WORST-RC
                   END-IF
               WHEN 8
      *            THE HANDLER DECLINED THE FEE FOR A BUSINESS REASON.
      *            NOTHING IS POSTED AND THE CYCLE CARRIES ON.
                   ADD 1                   TO WS-DECLINE-CNT
                   IF WS-WORST-RC < WS-RC-WARNING
                       MOVE WS-RC-WARNING  TO WS-WORST-RC
                   END-IF
               WHEN OTHER
                   MOVE 'FEE HANDLER RETURNED A FATAL CONDITION'
                                           TO ER-MESSAGE
                   MOVE WS-RETURN-MSG      TO ER-MESSAGE(40:39)
                   MOVE 0505               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-EVALUATE
      *
           PERFORM 2600-WRITE-AUDIT
      *
           IF WS-RETURN-CD > 4
               GO TO 2500-EXIT
           END-IF
      *
           IF FR-WAIVED-FLG = 'Y'
               ADD 1                       TO WS-WAIVED-CNT
               GO TO 2500-EXIT
           END-IF
      *
           IF FR-ASSESSED-AMT = ZERO
               GO TO 2500-EXIT
           END-IF
      *
           MOVE FR-ASSESSED-AMT            TO WS-FEE-AMT
           PERFORM 2700-POST-FEE-TRANSACTION
           ADD WS-FEE-AMT                  TO WS-FEE-TOTAL
           ADD 1                           TO WS-POSTED-CNT
                                              WS-SINCE-COMMIT
           .
       2500-EXIT.
           EXIT
           .
      *
       2600-WRITE-AUDIT.
           MOVE DCL-ACCT-ID                TO AL-ACCT-ID
           MOVE DCL-FEE-TYPE               TO AL-FEE-TYPE
           MOVE FR-ASSESSED-AMT            TO AL-AMOUNT
           MOVE FR-WAIVED-FLG              TO AL-WAIVED
           MOVE WS-RETURN-CD               TO AL-RC
           MOVE WS-RETURN-MSG              TO AL-MSG
           WRITE AUDIT-REC FROM WS-AUDIT-LINE
           .
      *
      ******************************************************************
      * 2700 - POST THE FEE AS A SINGLE LEG TRANSACTION                *
      ******************************************************************
       2700-POST-FEE-TRANSACTION.
           ADD 1                           TO WS-TXN-SERIAL-NBR
           MOVE WS-TXN-SERIAL-NBR          TO WS-TXN-SERIAL
           MOVE WS-TXN-ID                  TO DCL-TXN-ID
           MOVE WS-FEE-AMT                 TO DCL-TXN-AMT
      *
           MOVE 01                         TO WS-LI-SEQ
           MOVE 'FEE '                     TO WS-LI-TYPE
           MOVE WS-FEE-AMT                 TO WS-LI-AMT
           MOVE FR-GL-ACCOUNT              TO WS-LI-GL
           MOVE 'N'                        TO WS-LI-REVERSED
           MOVE SPACES                     TO DCL-LEG-TXT
           MOVE WS-LEG-IMAGE               TO DCL-LEG-TXT(1:24)
           MOVE 24                         TO DCL-LEG-LEN
      *
           IF FR-NARRATIVE = SPACES
               STRING DCL-FEE-TYPE ' FEE - CYCLE ' WS-CYCLE-ID
                 DELIMITED BY SIZE INTO DCL-NARRATIVE
               END-STRING
           ELSE
               MOVE FR-NARRATIVE           TO DCL-NARRATIVE
           END-IF
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
                    , :DCL-FEE-TYPE
                    , 'BT'
                    , :DCL-TXN-AMT
                    , :DCL-FEE-CURRENCY
                    , :DCL-TXN-AMT
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
               MOVE 'INSERT OF FEE TRANSACTION FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET CURR_BAL       = CURR_BAL + :DCL-TXN-AMT
                    , LAST_MAINT_PGM = :WS-PROGRAM-ID
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          '    TO ER-SQL-TABLE
               MOVE 'UPDATE  '             TO ER-SQL-OPERATION
               MOVE 'BALANCE UPDATE FOR FEE FAILED' TO ER-MESSAGE
               MOVE 0503                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           .
      *
       2900-CLOSE-FEE-CURSOR.
           EXEC SQL CLOSE FEECSR END-EXEC
           .
      *
       2950-COMMIT-POINT.
           EXEC SQL COMMIT WORK END-EXEC
           ADD 1                           TO WS-COMMIT-CNT
           MOVE ZERO                       TO WS-SINCE-COMMIT
           .
      *
      ******************************************************************
      * 3000 - TERMINATION AND BRANCH COMPLETION FLAG                  *
      ******************************************************************
       3000-TERMINATE.
           EXEC SQL CLOSE ACCTCSR END-EXEC
           EXEC SQL COMMIT WORK END-EXEC
           ADD 1                           TO WS-COMMIT-CNT
      *
           MOVE WS-WORST-RC                TO WS-RETURN-CODE
           PERFORM 3100-POST-BRANCH-FLAG
      *
           CLOSE CYCLCTL-FILE
                 AUDIT-FILE
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-ACCT-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05A ACCOUNTS READ     ' WS-DISPLAY-CNT
           MOVE WS-FEE-CNT                 TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05A FEE TYPES EXAMINED' WS-DISPLAY-CNT
           MOVE WS-DISPATCH-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05A HANDLERS CALLED   ' WS-DISPLAY-CNT
           MOVE WS-POSTED-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05A FEES POSTED       ' WS-DISPLAY-CNT
           MOVE WS-WAIVED-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05A FEES WAIVED       ' WS-DISPLAY-CNT
           MOVE WS-DECLINE-CNT             TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05A HANDLER DECLINES  ' WS-DISPLAY-CNT
           MOVE WS-WARNING-CNT             TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD05A HANDLER WARNINGS  ' WS-DISPLAY-CNT
           MOVE WS-FEE-TOTAL               TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD05A FEE AMOUNT TOTAL  ' WS-DISPLAY-AMT
           DISPLAY '----------------------------------------------'
           .
      *
      ******************************************************************
      * 3100 - POST THE FEE BRANCH COMPLETION FLAG                     *
      *                                                                *
      * THE FLAG LIVES IN THE FIRST BYTE OF CC-FILLER ON THE CYCLE     *
      * CONTROL RECORD.  CBCRD05B OWNS THE SECOND BYTE.  CBCRD06W      *
      * READS BOTH BEFORE IT LETS THE JOIN RUN.  THE RECORD IS         *
      * RE-READ IMMEDIATELY BEFORE THE REWRITE SO THE OTHER BRANCH'S   *
      * FLAG IS NOT OVERLAID - THE TWO JOBS RUN AT THE SAME TIME.      *
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
               MOVE 'F'                    TO CC-FILLER(1:1)
           ELSE
               MOVE 'A'                    TO CC-FILLER(1:1)
           END-IF
           MOVE WS-CYCLE-ID                TO CC-FILLER(3:8)
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
           DISPLAY 'CBCRD05A - FEE BRANCH FLAG POSTED AS '
                   CC-FILLER(1:1)
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD05A SQL ERROR SQLCODE=' SQLCODE
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
           DISPLAY 'CBCRD05A FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
