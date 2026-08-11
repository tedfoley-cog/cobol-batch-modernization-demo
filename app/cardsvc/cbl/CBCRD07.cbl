      ******************************************************************
      * CBCRD07 - GENERAL LEDGER POSTING                               *
      *                                                                *
      * JOB CBCRD07J STEP010.  RUNS AFTER THE EXPOSURE JOIN.           *
      *                                                                *
      * AGGREGATES THE CYCLE'S POSTED TRANSACTIONS INTO DOUBLE ENTRY   *
      * ROWS ON CARDSVC.GL_POSTING.  EVERY TRANSACTION TYPE MAPS TO A  *
      * DEBIT LEG AND A CREDIT LEG.  THE MAPPING OF TRANSACTION TYPE   *
      * TO GL ACCOUNT AND COST CENTRE IS NOT COMPILED IN - IT IS READ  *
      * FROM THE SYSIN CONTROL CARDS SO THAT FINANCE CAN MOVE A        *
      * PORTFOLIO BETWEEN COST CENTRES WITHOUT A CODE CHANGE.          *
      *                                                                *
      * CONTROL CARD FORMAT, COLUMNS 1 TO 40                           *
      *                                                                *
      *   MAP TTTT PPPP DDDDDDDDDD CCCCCCCCCC NNNNNN                   *
      *       |    |    |          |          |                        *
      *       |    |    |          |          COST CENTRE              *
      *       |    |    |          CREDIT GL ACCOUNT                   *
      *       |    |    DEBIT GL ACCOUNT                               *
      *       |    PRODUCT CODE OR **** FOR ANY                        *
      *       TRANSACTION TYPE CODE                                    *
      *                                                                *
      *   BATCH NNNNNNNNNNNN     GL BATCH IDENTIFIER                   *
      *   SUSPENSE DDDDDDDDDD    ACCOUNT FOR UNMAPPED TYPES            *
      *   END                                                          *
      *                                                                *
      * THE STEP PROVES THAT THE SUM OF THE DEBIT LEGS EQUALS THE SUM  *
      * OF THE CREDIT LEGS BEFORE IT COMMITS.  IF IT DOES NOT, THE     *
      * WHOLE UNIT OF WORK IS ROLLED BACK AND THE STEP ABENDS - AN     *
      * UNBALANCED FEED IS NEVER SENT TO THE LEDGER.                   *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - SYSIN    CONTROL CARDS                           *
      *             - GLRPT    SYSOUT LRECL 133                        *
      *             - CYCLCTL  VSAM KSDS UPDATE                        *
      * TABLES      - CARDSVC.TRANSACTION  (CURSOR, UPDATE)            *
      *             - CARDSVC.GL_POSTING   (INSERT)                    *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 LEDGER FEED BALANCED AND COMMITTED          *
      *               0004 SOME TYPES POSTED TO SUSPENSE               *
      *               0012 FATAL                                       *
      * USER ABEND  - U0701 CONTROL CARDS MISSING OR INVALID           *
      *               U0702 FILE OR VSAM FAILURE                       *
      *               U0703 UNRECOVERABLE SQL ERROR                    *
      *               U0704 LEDGER FEED DOES NOT BALANCE               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD07.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1998-10-05.
      *
      * MAINTENANCE
      * 1998-10-05 CRD1044 ORIGINAL
      * 2002-02-11 CRD2701 COST CENTRE MAPPING MOVED OUT OF THE
      *                    PROGRAM AND INTO SYSIN
      * 2006-11-20 CRD5210 SUSPENSE POSTING ADDED - FINANCE PREFER A
      *                    BALANCED FEED WITH A SUSPENSE LINE TO A
      *                    FAILED NIGHT
      * 2014-04-02 CRD8702 BALANCE PROOF TIGHTENED TO THE CENT
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE   ASSIGN TO GLRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REPORT-STATUS.
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD07 '.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-REPORT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REPORT-OK                      VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-FOUND-SW                 PIC X     VALUE 'N'.
               88  WS-MAP-FOUND                      VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-FETCH-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-TXN-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-GL-ROW-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SUSPENSE-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-FLAG-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
           05  WS-CARD-CNT                 PIC 9(4)  VALUE ZERO.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE 99.
           05  WS-PAGE-CNT                 PIC 9(4)  VALUE ZERO.
      *
       01  WS-TOTALS.
           05  WS-DEBIT-TOTAL              PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-CREDIT-TOTAL             PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-OUT-OF-BALANCE           PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-SUSPENSE-TOTAL           PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYC-DT                       PIC X(10) VALUE SPACES.
       01  WS-VALUE-DT                     PIC X(10) VALUE SPACES.
       01  WS-CYCLE-ID                     PIC X(8)  VALUE SPACES.
      *
       01  WS-GL-SEQ                       PIC 9(9)  VALUE ZERO.
      *
      ******************************************************************
      * THE MAPPING TABLE.  SIXTY ENTRIES HAS BEEN ENOUGH SINCE 1998   *
      * AND THE LOAD ABENDS RATHER THAN OVERRUN IT.                    *
      ******************************************************************
       01  WS-MAP-TABLE.
           05  WS-MAP-CNT                  PIC 9(4)  VALUE ZERO.
           05  WS-MAP-ENTRY OCCURS 60 TIMES
                       INDEXED BY WS-MAP-IDX.
               10  WS-MAP-TXN-TYPE         PIC X(4).
               10  WS-MAP-PRODUCT          PIC X(4).
               10  WS-MAP-DEBIT-GL         PIC X(10).
               10  WS-MAP-CREDIT-GL        PIC X(10).
               10  WS-MAP-COST-CENTRE      PIC X(6).
      *
       01  WS-SUSPENSE-GL                  PIC X(10) VALUE SPACES.
       01  WS-BATCH-ID                     PIC X(12) VALUE SPACES.
      *
       01  WS-CONTROL-CARD                 PIC X(80) VALUE SPACES.
       01  WS-CONTROL-CARD-R REDEFINES WS-CONTROL-CARD.
           05  WS-CC-VERB                  PIC X(8).
           05  WS-CC-REST                  PIC X(72).
      *
       01  WS-MAP-CARD REDEFINES WS-CONTROL-CARD.
           05  FILLER                      PIC X(4).
           05  WS-MC-TXN-TYPE              PIC X(4).
           05  FILLER                      PIC X.
           05  WS-MC-PRODUCT               PIC X(4).
           05  FILLER                      PIC X.
           05  WS-MC-DEBIT-GL              PIC X(10).
           05  FILLER                      PIC X.
           05  WS-MC-CREDIT-GL             PIC X(10).
           05  FILLER                      PIC X.
           05  WS-MC-COST-CENTRE           PIC X(6).
           05  FILLER                      PIC X(38).
      *
       01  WS-WORK-AREAS.
           05  WS-DEBIT-GL                 PIC X(10) VALUE SPACES.
           05  WS-CREDIT-GL                PIC X(10) VALUE SPACES.
           05  WS-COST-CENTRE              PIC X(6)  VALUE SPACES.
           05  WS-NARRATIVE                PIC X(40) VALUE SPACES.
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
      * REPORT LINES                                                   *
      ******************************************************************
       01  RPT-HEAD-1.
           05  FILLER                      PIC X(9)  VALUE 'CBCRD07  '.
           05  FILLER                      PIC X(42) VALUE
               'CARD SERVICES - GENERAL LEDGER FEED       '.
           05  FILLER                      PIC X(11) VALUE
               'CYCLE DATE '.
           05  RH1-CYCLE-DATE              PIC 9(8).
           05  FILLER                      PIC X(10) VALUE
               '  BATCH   '.
           05  RH1-BATCH-ID                PIC X(12).
           05  FILLER                      PIC X(25) VALUE SPACES.
           05  FILLER                      PIC X(5)  VALUE 'PAGE '.
           05  RH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(7)  VALUE SPACES.
      *
       01  RPT-HEAD-2.
           05  FILLER                      PIC X(6)  VALUE 'TYPE  '.
           05  FILLER                      PIC X(6)  VALUE 'PROD  '.
           05  FILLER                      PIC X(12) VALUE
               'GL ACCOUNT  '.
           05  FILLER                      PIC X(9)  VALUE 'COST CTR '.
           05  FILLER                      PIC X(4)  VALUE 'D/C '.
           05  FILLER                      PIC X(22) VALUE
               '                AMOUNT'.
           05  FILLER                      PIC X(14) VALUE
               '         COUNT'.
           05  FILLER                      PIC X(60) VALUE SPACES.
      *
       01  RPT-DETAIL.
           05  RD-TXN-TYPE                 PIC X(4).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-PRODUCT                  PIC X(4).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-GL-ACCOUNT               PIC X(10).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-COST-CENTRE              PIC X(6).
           05  FILLER                      PIC X(3)  VALUE SPACES.
           05  RD-DR-CR                    PIC X.
           05  FILLER                      PIC X(3)  VALUE SPACES.
           05  RD-AMOUNT                   PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(61) VALUE SPACES.
      *
       01  RPT-PROOF.
           05  FILLER                      PIC X(20).
           05  RP-TEXT                     PIC X(28).
           05  RP-AMOUNT                   PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(67) VALUE SPACES.
      *
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-AGG.
           05  DCL-TXN-TYPE-CD             PIC X(4).
           05  DCL-PRODUCT-CD              PIC X(4).
           05  DCL-CURRENCY-CD             PIC X(3).
           05  DCL-BILL-TOTAL              PIC S9(13)V99 COMP-3.
           05  DCL-ROW-CNT                 PIC S9(9)     COMP-3.
      *
       01  DCL-GL.
           05  DCL-BATCH-ID                PIC X(12).
           05  DCL-GL-SEQ                  PIC S9(9)     COMP-3.
           05  DCL-GL-ACCOUNT              PIC X(10).
           05  DCL-COST-CENTRE             PIC X(6).
           05  DCL-DR-CR-IND               PIC X(1).
           05  DCL-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  DCL-NARRATIVE               PIC X(40).
           05  DCL-CYCLE-ID                PIC X(8).
           05  DCL-POSTED-BY               PIC X(8).
      *
      ******************************************************************
      * THE FEED IS SUMMARY LEVEL - FINANCE TAKE ONE PAIR OF LINES PER *
      * TRANSACTION TYPE, PRODUCT AND CURRENCY, NOT ONE PER ACCOUNT.   *
      ******************************************************************
           EXEC SQL DECLARE GLCSR CURSOR FOR
               SELECT T.TXN_TYPE_CD
                    , A.PRODUCT_CD
                    , T.CURRENCY_CD
                    , SUM(T.BILLING_AMT)
                    , COUNT(*)
                 FROM CARDSVC.TRANSACTION T
                    , CARDSVC.ACCOUNT     A
                WHERE T.POST_DATE     = DATE(:WS-CYC-DT)
                  AND T.GL_POSTED_FLG = 'N'
                  AND A.ACCT_ID       = T.ACCT_ID
                GROUP BY T.TXN_TYPE_CD
                       , A.PRODUCT_CD
                       , T.CURRENCY_CD
                ORDER BY T.TXN_TYPE_CD
                       , A.PRODUCT_CD
                       , T.CURRENCY_CD
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
           PERFORM 2000-POST-GROUP
               UNTIL WS-EOF
           PERFORM 3000-PROVE-BALANCE
           PERFORM 4000-TERMINATE
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
           IF LK-PARM-LEN < 8
               MOVE 'PARM MUST SUPPLY CCYYMMDD CYCLE DATE'
                                           TO ER-MESSAGE
               MOVE 0701                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO WS-CYC-DT
           END-STRING
           MOVE WS-CYC-DT                  TO WS-VALUE-DT
           MOVE WS-CYCLE-DATE              TO RH1-CYCLE-DATE
      *
           OPEN OUTPUT REPORT-FILE
           OPEN I-O    CYCLCTL-FILE
           IF NOT WS-REPORT-OK
               MOVE 'GLRPT   '             TO ER-FILE-NAME
               MOVE WS-REPORT-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF GL REPORT FAILED' TO ER-MESSAGE
               MOVE 0702                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM 1100-READ-CONTROL-CARDS
           PERFORM 1200-VALIDATE-CONTROLS
           PERFORM 1300-READ-CYCLE-CONTROL
      *
           EXEC SQL OPEN GLCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'TRANSACTION      '    TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               MOVE 'OPEN OF GLCSR FAILED' TO ER-MESSAGE
               MOVE 0703                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           PERFORM 1900-FETCH-GROUP
           .
      *
      ******************************************************************
      * 1100 - LOAD THE MAPPING SUPPLIED BY FINANCE                    *
      ******************************************************************
       1100-READ-CONTROL-CARDS.
           ACCEPT WS-CONTROL-CARD FROM SYSIN
           PERFORM UNTIL WS-CONTROL-CARD = SPACES
                      OR WS-CC-VERB(1:3) = 'END'
               ADD 1                       TO WS-CARD-CNT
               EVALUATE WS-CC-VERB(1:4)
                   WHEN 'MAP '
                       PERFORM 1150-LOAD-MAP-ENTRY
                   WHEN 'BATC'
                       MOVE WS-CONTROL-CARD(7:12)
                                           TO WS-BATCH-ID
                   WHEN 'SUSP'
                       MOVE WS-CONTROL-CARD(10:10)
                                           TO WS-SUSPENSE-GL
                   WHEN OTHER
                       IF WS-CONTROL-CARD(1:1) NOT = '*'
                           DISPLAY 'CBCRD07 - CARD IGNORED '
                                   WS-CONTROL-CARD(1:40)
                       END-IF
               END-EVALUATE
               MOVE SPACES                 TO WS-CONTROL-CARD
               ACCEPT WS-CONTROL-CARD FROM SYSIN
           END-PERFORM
           .
      *
       1150-LOAD-MAP-ENTRY.
           IF WS-MAP-CNT >= 60
               MOVE 'MORE THAN 60 GL MAPPING CARDS SUPPLIED'
                                           TO ER-MESSAGE
               MOVE 0701                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           ADD 1                           TO WS-MAP-CNT
           SET WS-MAP-IDX                  TO WS-MAP-CNT
           MOVE WS-MC-TXN-TYPE      TO WS-MAP-TXN-TYPE(WS-MAP-IDX)
           MOVE WS-MC-PRODUCT       TO WS-MAP-PRODUCT(WS-MAP-IDX)
           MOVE WS-MC-DEBIT-GL      TO WS-MAP-DEBIT-GL(WS-MAP-IDX)
           MOVE WS-MC-CREDIT-GL     TO WS-MAP-CREDIT-GL(WS-MAP-IDX)
           MOVE WS-MC-COST-CENTRE   TO WS-MAP-COST-CENTRE(WS-MAP-IDX)
      *
           IF WS-MC-DEBIT-GL = SPACES OR WS-MC-CREDIT-GL = SPACES
               MOVE 'GL MAPPING CARD IS MISSING AN ACCOUNT'
                                           TO ER-MESSAGE
               MOVE 0701                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       1200-VALIDATE-CONTROLS.
           IF WS-MAP-CNT = ZERO
               MOVE 'NO GL MAPPING CARDS SUPPLIED IN SYSIN'
                                           TO ER-MESSAGE
               MOVE 0701                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF WS-SUSPENSE-GL = SPACES
               MOVE 'NO SUSPENSE ACCOUNT SUPPLIED IN SYSIN'
                                           TO ER-MESSAGE
               MOVE 0701                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
      *    A BATCH IDENTIFIER OF ITS OWN IS BUILT WHEN FINANCE DO NOT
      *    NOMINATE ONE - CCYYMMDD PLUS THE FEED SUFFIX.
           IF WS-BATCH-ID = SPACES
               STRING WS-CYCLE-DATE 'CN01'
                 DELIMITED BY SIZE INTO WS-BATCH-ID
               END-STRING
           END-IF
           MOVE WS-BATCH-ID                TO RH1-BATCH-ID
           MOVE WS-BATCH-ID                TO DCL-BATCH-ID
      *
           DISPLAY 'CBCRD07 - ' WS-MAP-CNT ' MAPPING CARDS LOADED'
           DISPLAY 'CBCRD07 - GL BATCH ' WS-BATCH-ID
           DISPLAY 'CBCRD07 - SUSPENSE ' WS-SUSPENSE-GL
           .
      *
       1300-READ-CYCLE-CONTROL.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 0702               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE CC-CYCLE-ID                TO WS-CYCLE-ID
                                              DCL-CYCLE-ID
           MOVE 'CBCRD07 '                 TO CC-CURRENT-STEP
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'REWRITE OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0702                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       1900-FETCH-GROUP.
           EXEC SQL
               FETCH GLCSR
                INTO :DCL-TXN-TYPE-CD
                   , :DCL-PRODUCT-CD
                   , :DCL-CURRENCY-CD
                   , :DCL-BILL-TOTAL
                   , :DCL-ROW-CNT
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-FETCH-CNT
               WHEN 100
                   MOVE 'Y'                TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'TRANSACTION      ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '          TO ER-SQL-OPERATION
                   MOVE 'FETCH OF GLCSR FAILED' TO ER-MESSAGE
                   MOVE 0703                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2000 - ONE SUMMARY GROUP BECOMES ONE DEBIT AND ONE CREDIT      *
      ******************************************************************
       2000-POST-GROUP.
           PERFORM 2100-RESOLVE-MAPPING
           PERFORM 2200-BUILD-NARRATIVE
      *
           ADD DCL-ROW-CNT                 TO WS-TXN-CNT
      *
      *    A NEGATIVE GROUP TOTAL - REFUNDS OUTWEIGHING PURCHASES ON
      *    A QUIET PRODUCT - IS POSTED THE OTHER WAY ROUND RATHER THAN
      *    AS A NEGATIVE AMOUNT.  THE LEDGER REJECTS SIGNED AMOUNTS.
           IF DCL-BILL-TOTAL < ZERO
               COMPUTE DCL-AMOUNT = DCL-BILL-TOTAL * -1
               MOVE WS-CREDIT-GL           TO DCL-GL-ACCOUNT
               MOVE 'D'                    TO DCL-DR-CR-IND
               PERFORM 2300-INSERT-GL-ROW
               MOVE WS-DEBIT-GL            TO DCL-GL-ACCOUNT
               MOVE 'C'                    TO DCL-DR-CR-IND
               PERFORM 2300-INSERT-GL-ROW
           ELSE
               MOVE DCL-BILL-TOTAL         TO DCL-AMOUNT
               MOVE WS-DEBIT-GL            TO DCL-GL-ACCOUNT
               MOVE 'D'                    TO DCL-DR-CR-IND
               PERFORM 2300-INSERT-GL-ROW
               MOVE WS-CREDIT-GL           TO DCL-GL-ACCOUNT
               MOVE 'C'                    TO DCL-DR-CR-IND
               PERFORM 2300-INSERT-GL-ROW
           END-IF
      *
           PERFORM 2400-FLAG-TRANSACTIONS
      *
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQUENCY
      *        THE FEED IS SMALL ENOUGH THAT ONE UNIT OF WORK IS THE
      *        NORM, BUT A MONTH END REBUILD CAN EXCEED THE LOG SO
      *        THE FREQUENCY IS STILL HONOURED.
               EXEC SQL COMMIT WORK END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
      *
           PERFORM 1900-FETCH-GROUP
           .
      *
       2100-RESOLVE-MAPPING.
           MOVE 'N'                        TO WS-FOUND-SW
           MOVE SPACES                     TO WS-DEBIT-GL
                                              WS-CREDIT-GL
                                              WS-COST-CENTRE
      *
      *    EXACT PRODUCT MATCH FIRST, THEN THE WILDCARD ENTRY.
           PERFORM VARYING WS-MAP-IDX FROM 1 BY 1
                     UNTIL WS-MAP-IDX > WS-MAP-CNT
                        OR WS-MAP-FOUND
               IF WS-MAP-TXN-TYPE(WS-MAP-IDX) = DCL-TXN-TYPE-CD
              AND WS-MAP-PRODUCT(WS-MAP-IDX)  = DCL-PRODUCT-CD
                   PERFORM 2150-TAKE-MAPPING
               END-IF
           END-PERFORM
      *
           IF NOT WS-MAP-FOUND
               PERFORM VARYING WS-MAP-IDX FROM 1 BY 1
                         UNTIL WS-MAP-IDX > WS-MAP-CNT
                            OR WS-MAP-FOUND
                   IF WS-MAP-TXN-TYPE(WS-MAP-IDX) = DCL-TXN-TYPE-CD
                  AND WS-MAP-PRODUCT(WS-MAP-IDX)  = '****'
                       PERFORM 2150-TAKE-MAPPING
                   END-IF
               END-PERFORM
           END-IF
      *
           IF NOT WS-MAP-FOUND
               MOVE WS-SUSPENSE-GL         TO WS-DEBIT-GL
                                              WS-CREDIT-GL
               MOVE 'SUSPNS'               TO WS-COST-CENTRE
               ADD 1                       TO WS-SUSPENSE-CNT
               ADD DCL-BILL-TOTAL          TO WS-SUSPENSE-TOTAL
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
               DISPLAY 'CBCRD07 - UNMAPPED TYPE ' DCL-TXN-TYPE-CD
                       ' PRODUCT ' DCL-PRODUCT-CD
                       ' POSTED TO SUSPENSE'
           END-IF
      *
           MOVE WS-COST-CENTRE             TO DCL-COST-CENTRE
           .
      *
       2150-TAKE-MAPPING.
           MOVE WS-MAP-DEBIT-GL(WS-MAP-IDX)    TO WS-DEBIT-GL
           MOVE WS-MAP-CREDIT-GL(WS-MAP-IDX)   TO WS-CREDIT-GL
           MOVE WS-MAP-COST-CENTRE(WS-MAP-IDX) TO WS-COST-CENTRE
           MOVE 'Y'                            TO WS-FOUND-SW
           .
      *
       2200-BUILD-NARRATIVE.
           MOVE SPACES                     TO WS-NARRATIVE
           STRING 'CARDNITE ' DCL-TXN-TYPE-CD ' '
                  DCL-PRODUCT-CD ' ' DCL-CURRENCY-CD ' '
                  WS-CYCLE-DATE
             DELIMITED BY SIZE INTO WS-NARRATIVE
           END-STRING
           MOVE WS-NARRATIVE               TO DCL-NARRATIVE
           MOVE WS-PROGRAM-ID              TO DCL-POSTED-BY
           .
      *
       2300-INSERT-GL-ROW.
           ADD 1                           TO WS-GL-SEQ
           MOVE WS-GL-SEQ                  TO DCL-GL-SEQ
      *
           EXEC SQL
               INSERT INTO CARDSVC.GL_POSTING
                     (GL_BATCH_ID
                    , GL_SEQ_NUM
                    , POST_DATE
                    , VALUE_DATE
                    , GL_ACCOUNT
                    , COST_CENTRE
                    , DR_CR_IND
                    , AMOUNT
                    , CURRENCY_CD
                    , NARRATIVE
                    , CYCLE_ID
                    , POSTED_BY)
               VALUES (:DCL-BATCH-ID
                    , :DCL-GL-SEQ
                    , DATE(:WS-CYC-DT)
                    , DATE(:WS-VALUE-DT)
                    , :DCL-GL-ACCOUNT
                    , :DCL-COST-CENTRE
                    , :DCL-DR-CR-IND
                    , :DCL-AMOUNT
                    , :DCL-CURRENCY-CD
                    , :DCL-NARRATIVE
                    , :DCL-CYCLE-ID
                    , :DCL-POSTED-BY)
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'GL_POSTING       '    TO ER-SQL-TABLE
               MOVE 'INSERT  '             TO ER-SQL-OPERATION
               MOVE 'INSERT INTO GL_POSTING FAILED' TO ER-MESSAGE
               MOVE 0703                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           ADD 1                           TO WS-GL-ROW-CNT
                                              WS-SINCE-COMMIT
      *
           IF DCL-DR-CR-IND = 'D'
               ADD DCL-AMOUNT              TO WS-DEBIT-TOTAL
           ELSE
               ADD DCL-AMOUNT              TO WS-CREDIT-TOTAL
           END-IF
      *
           PERFORM 2500-PRINT-DETAIL
           .
      *
       2400-FLAG-TRANSACTIONS.
           EXEC SQL
               UPDATE CARDSVC.TRANSACTION T
                  SET GL_POSTED_FLG = 'Y'
                WHERE T.POST_DATE     = DATE(:WS-CYC-DT)
                  AND T.GL_POSTED_FLG = 'N'
                  AND T.TXN_TYPE_CD   = :DCL-TXN-TYPE-CD
                  AND T.CURRENCY_CD   = :DCL-CURRENCY-CD
                  AND EXISTS (SELECT 1
                                FROM CARDSVC.ACCOUNT A
                               WHERE A.ACCT_ID    = T.ACCT_ID
                                 AND A.PRODUCT_CD = :DCL-PRODUCT-CD)
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               MOVE 'TRANSACTION      '    TO ER-SQL-TABLE
               MOVE 'UPDATE  '             TO ER-SQL-OPERATION
               MOVE 'FLAGGING OF POSTED TRANSACTIONS FAILED'
                                           TO ER-MESSAGE
               MOVE 0703                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           ADD SQLERRD(3)                  TO WS-FLAG-CNT
           .
      *
       2500-PRINT-DETAIL.
           IF WS-LINE-CNT > 55
               PERFORM 2550-PRINT-HEADINGS
           END-IF
      *
           MOVE DCL-TXN-TYPE-CD            TO RD-TXN-TYPE
           MOVE DCL-PRODUCT-CD             TO RD-PRODUCT
           MOVE DCL-GL-ACCOUNT             TO RD-GL-ACCOUNT
           MOVE DCL-COST-CENTRE            TO RD-COST-CENTRE
           MOVE DCL-DR-CR-IND              TO RD-DR-CR
           MOVE DCL-AMOUNT                 TO RD-AMOUNT
           MOVE DCL-ROW-CNT                TO RD-COUNT
           WRITE REPORT-REC FROM RPT-DETAIL
           ADD 1                           TO WS-LINE-CNT
           .
      *
       2550-PRINT-HEADINGS.
           ADD 1                           TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT                TO RH1-PAGE
           WRITE REPORT-REC FROM RPT-HEAD-1
           WRITE REPORT-REC FROM RPT-HEAD-2
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
           MOVE 4                          TO WS-LINE-CNT
           .
      *
      ******************************************************************
      * 3000 - THE PROOF.  NOTHING IS COMMITTED UNTIL THE TWO SIDES    *
      *        AGREE TO THE CENT.  AN OUT OF BALANCE FEED IS ROLLED    *
      *        BACK IN FULL AND THE STEP ABENDS - FINANCE WOULD RATHER *
      *        HAVE NO FEED THAN A WRONG ONE.                          *
      ******************************************************************
       3000-PROVE-BALANCE.
           EXEC SQL CLOSE GLCSR END-EXEC
      *
           COMPUTE WS-OUT-OF-BALANCE =
                   WS-DEBIT-TOTAL - WS-CREDIT-TOTAL
      *
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
      *
           MOVE 'TOTAL DEBITS'             TO RP-TEXT
           MOVE WS-DEBIT-TOTAL             TO RP-AMOUNT
           WRITE REPORT-REC FROM RPT-PROOF
      *
           MOVE 'TOTAL CREDITS'            TO RP-TEXT
           MOVE WS-CREDIT-TOTAL            TO RP-AMOUNT
           WRITE REPORT-REC FROM RPT-PROOF
      *
           MOVE 'DIFFERENCE'               TO RP-TEXT
           MOVE WS-OUT-OF-BALANCE          TO RP-AMOUNT
           WRITE REPORT-REC FROM RPT-PROOF
      *
           MOVE 'POSTED TO SUSPENSE'       TO RP-TEXT
           MOVE WS-SUSPENSE-TOTAL          TO RP-AMOUNT
           WRITE REPORT-REC FROM RPT-PROOF
      *
           IF WS-OUT-OF-BALANCE NOT = ZERO
               MOVE WS-DEBIT-TOTAL         TO WS-DISPLAY-AMT
               DISPLAY 'CBCRD07 DEBITS  ' WS-DISPLAY-AMT
               MOVE WS-CREDIT-TOTAL        TO WS-DISPLAY-AMT
               DISPLAY 'CBCRD07 CREDITS ' WS-DISPLAY-AMT
               MOVE WS-OUT-OF-BALANCE      TO WS-DISPLAY-AMT
               DISPLAY 'CBCRD07 OUT OF BALANCE BY ' WS-DISPLAY-AMT
               MOVE 'LEDGER FEED DOES NOT BALANCE - FEED BACKED OUT'
                                           TO ER-MESSAGE
               MOVE 0704                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           EXEC SQL COMMIT WORK END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'GL_POSTING       '    TO ER-SQL-TABLE
               MOVE 'COMMIT  '             TO ER-SQL-OPERATION
               MOVE 'COMMIT OF THE LEDGER FEED FAILED' TO ER-MESSAGE
               MOVE 0703                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           .
      *
       4000-TERMINATE.
           PERFORM 4100-UPDATE-CYCLE-TOTALS
      *
           CLOSE REPORT-FILE
                 CYCLCTL-FILE
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-FETCH-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD07 SUMMARY GROUPS     ' WS-DISPLAY-CNT
           MOVE WS-TXN-CNT                 TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD07 TRANSACTIONS FED   ' WS-DISPLAY-CNT
           MOVE WS-FLAG-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD07 TRANSACTIONS FLAGD ' WS-DISPLAY-CNT
           MOVE WS-GL-ROW-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD07 GL ROWS INSERTED   ' WS-DISPLAY-CNT
           MOVE WS-SUSPENSE-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD07 SUSPENSE GROUPS    ' WS-DISPLAY-CNT
           MOVE WS-DEBIT-TOTAL             TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD07 TOTAL DEBITS      ' WS-DISPLAY-AMT
           MOVE WS-CREDIT-TOTAL            TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD07 TOTAL CREDITS     ' WS-DISPLAY-AMT
           DISPLAY '----------------------------------------------'
           .
      *
       4100-UPDATE-CYCLE-TOTALS.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'CYCLE CONTROL RECORD DISAPPEARED MID STEP'
                                           TO ER-MESSAGE
                   MOVE 0702               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
      *
           MOVE WS-DEBIT-TOTAL             TO CC-TOTAL-DR-AMT
           MOVE WS-CREDIT-TOTAL            TO CC-TOTAL-CR-AMT
           MOVE 'CBCRD07 '                 TO CC-LAST-GOOD-STEP
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'REWRITE OF CYCLE TOTALS FAILED' TO ER-MESSAGE
               MOVE 0702                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD07 SQL ERROR SQLCODE=' SQLCODE
                   ' TABLE=' ER-SQL-TABLE
                   ' TYPE=' DCL-TXN-TYPE-CD
                   ' SEQ=' WS-GL-SEQ
           PERFORM 9500-FATAL-ERROR
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD07 FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
