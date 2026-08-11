      ******************************************************************
      * CBCRD06A - ASSEMBLE THE PARTY EXPOSURE WORK LIST               *
      *                                                                *
      * STEP010 OF JOB CBCRD06J.                                       *
      *                                                                *
      * EVERY ACCOUNT THAT THE CYCLE POSTED TO IS TURNED INTO A WORK   *
      * LIST RECORD CARRYING THE PARTY IDENTIFIER, THE MOVEMENT THE    *
      * CYCLE PUT THROUGH THE ACCOUNT, THE RESULTING BALANCE AND THE   *
      * CURRENT CREDIT LIMIT AND RISK BAND.  A PARTY CAN HOLD SEVERAL  *
      * ACCOUNTS, SO THE FILE IS DELIBERATELY LEFT WITH DUPLICATE      *
      * PARTIES - CBCRD06X COLLAPSES THEM.                             *
      *                                                                *
      * ACCOUNTS WITH NO CYCLE MOVEMENT ARE STILL SELECTED WHEN THEY   *
      * CARRY A DELINQUENCY BUCKET, BECAUSE THE RISK SERVICE MUST SEE  *
      * A DETERIORATING PARTY EVEN ON A QUIET CYCLE - CRD5502.         *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - PARTYWK  QSAM OUTPUT LRECL 150                   *
      *             - CYCLCTL  VSAM KSDS INPUT                         *
      * TABLES      - CARDSVC.ACCOUNT       (CURSOR)                   *
      *             - CARDSVC.TRANSACTION   (CURSOR)                   *
      *             - CARDSVC.CARD_LIMIT    (SELECT)                   *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 WORK LIST BUILT                             *
      *               0004 WORK LIST EMPTY                             *
      *               0012 FATAL                                       *
      * USER ABEND  - U0601 CYCLE CONTROL UNUSABLE                     *
      *               U0602 FILE OPEN OR I/O FAILURE                   *
      *               U0603 UNRECOVERABLE SQL ERROR                    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD06A.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  2001-04-17.
      *
      * MAINTENANCE
      * 2001-04-17 CRD2109 ORIGINAL
      * 2007-01-15 CRD5502 DELINQUENT ACCOUNTS SELECTED EVEN WITH NO
      *                    CYCLE MOVEMENT
      * 2013-11-08 CRD8330 CLOSED ACCOUNTS WITH A BALANCE INCLUDED
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORK-FILE     ASSIGN TO PARTYWK
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-WORK-STATUS.
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
       FD  WORK-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       01  WORK-REC                        PIC X(150).
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD06A'.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-WORK-STATUS              PIC X(2)  VALUE '00'.
               88  WS-WORK-OK                        VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-FETCH-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-WRITE-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-NO-LIMIT-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-QUIET-CNT                PIC 9(9)  VALUE ZERO.
      *
       01  WS-EXPOSURE-TOTAL               PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYC-DT                       PIC X(10) VALUE SPACES.
       01  WS-CYCLE-ID                     PIC X(8)  VALUE SPACES.
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
           COPY CVPWRK01Y.
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-WORK.
           05  DCL-PARTY-ID                PIC X(11).
           05  DCL-ACCT-ID                 PIC S9(11)    COMP-3.
           05  DCL-CUST-ID                 PIC S9(9)     COMP-3.
           05  DCL-PRODUCT-CD              PIC X(4).
           05  DCL-CURR-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-CASH-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-DELQ-BUCKET             PIC S9(4)     COMP.
           05  DCL-POSTED-AMT              PIC S9(11)V99 COMP-3.
           05  DCL-TXN-CNT                 PIC S9(9)     COMP-3.
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-LIMIT-AMT               PIC S9(11)V99 COMP-3.
           05  DCL-RISK-BAND               PIC X(1).
      *
       01  IND-VARS.
           05  IND-POSTED-AMT              PIC S9(4) COMP.
           05  IND-RISK-BAND               PIC S9(4) COMP.
      *
      ******************************************************************
      * THE CYCLE'S MOVEMENT IS JOINED IN RATHER THAN LOOKED UP PER    *
      * ACCOUNT - THE OPTIMIZER MATERIALISES THE TABLE EXPRESSION ONCE *
      * AND THE STEP DROPPED FROM FORTY MINUTES TO FOUR - CRD5502.     *
      ******************************************************************
           EXEC SQL DECLARE WRKCSR CURSOR FOR
               SELECT A.PARTY_ID
                    , A.ACCT_ID
                    , A.CUST_ID
                    , A.PRODUCT_CD
                    , A.CURR_BAL
                    , A.CASH_BAL
                    , A.DELQ_BUCKET
                    , COALESCE(T.POSTED_AMT, 0)
                    , COALESCE(T.TXN_CNT, 0)
                 FROM CARDSVC.ACCOUNT A
                 LEFT OUTER JOIN
                      (SELECT ACCT_ID
                            , SUM(BILLING_AMT) AS POSTED_AMT
                            , COUNT(*)         AS TXN_CNT
                         FROM CARDSVC.TRANSACTION
                        WHERE POST_DATE = DATE(:WS-CYC-DT)
                        GROUP BY ACCT_ID) AS T
                   ON T.ACCT_ID = A.ACCT_ID
                WHERE (T.ACCT_ID IS NOT NULL
                   OR  A.DELQ_BUCKET > 0
                   OR (A.ACCT_STATUS = 'C' AND A.CURR_BAL <> 0))
                ORDER BY A.PARTY_ID
                       , A.ACCT_ID
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
           PERFORM 2000-BUILD-WORK-RECORD
               UNTIL WS-EOF
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
           IF LK-PARM-LEN < 8
               MOVE 'PARM MUST SUPPLY CCYYMMDD CYCLE DATE'
                                           TO ER-MESSAGE
               MOVE 0601                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO WS-CYC-DT
           END-STRING
      *
           OPEN OUTPUT WORK-FILE
           OPEN INPUT  CYCLCTL-FILE
           IF NOT WS-WORK-OK
               MOVE 'PARTYWK '             TO ER-FILE-NAME
               MOVE WS-WORK-STATUS         TO ER-FILE-STATUS
               MOVE 'OPEN OF WORK LIST FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 0601               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE CC-CYCLE-ID                TO WS-CYCLE-ID
      *
           EXEC SQL OPEN WRKCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          '    TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               MOVE 'OPEN OF WRKCSR FAILED' TO ER-MESSAGE
               MOVE 0603                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           DISPLAY 'CBCRD06A - WORK LIST BUILD, CYCLE ' WS-CYCLE-DATE
           PERFORM 1900-FETCH-WORK-ROW
           .
      *
       1900-FETCH-WORK-ROW.
           EXEC SQL
               FETCH WRKCSR
                INTO :DCL-PARTY-ID
                   , :DCL-ACCT-ID
                   , :DCL-CUST-ID
                   , :DCL-PRODUCT-CD
                   , :DCL-CURR-BAL
                   , :DCL-CASH-BAL
                   , :DCL-DELQ-BUCKET
                   , :DCL-POSTED-AMT :IND-POSTED-AMT
                   , :DCL-TXN-CNT
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
                   MOVE 'FETCH OF WRKCSR FAILED' TO ER-MESSAGE
                   MOVE 0603                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       2000-BUILD-WORK-RECORD.
           INITIALIZE PARTY-WORK-REC
           MOVE DCL-PARTY-ID               TO PW-PARTY-ID
           MOVE DCL-ACCT-ID                TO PW-ACCT-ID
           MOVE DCL-CUST-ID                TO PW-CUST-ID
           MOVE WS-CYCLE-DATE              TO PW-CYCLE-DATE
           MOVE WS-CYCLE-ID                TO PW-CYCLE-ID
           MOVE DCL-PRODUCT-CD             TO PW-PRODUCT-CD
           MOVE DCL-CURR-BAL               TO PW-CURR-BAL
           MOVE DCL-TXN-CNT                TO PW-TXN-CNT
      *
           IF IND-POSTED-AMT < 0
               MOVE ZERO                   TO PW-POSTED-AMT
           ELSE
               MOVE DCL-POSTED-AMT         TO PW-POSTED-AMT
           END-IF
      *
           IF DCL-TXN-CNT = ZERO
               ADD 1                       TO WS-QUIET-CNT
           END-IF
      *
           PERFORM 2100-READ-LIMIT
      *
      *    THE EXPOSURE HANDED OVER IS THE DRAWN BALANCE PLUS THE CASH
      *    BALANCE.  THE RISK SERVICE OWNS THE FINAL FIGURE - THIS IS
      *    ONLY THE CARD SIDE OF IT.
           COMPUTE PW-EXPOSURE-AMT = DCL-CURR-BAL + DCL-CASH-BAL
           ADD PW-EXPOSURE-AMT             TO WS-EXPOSURE-TOTAL
      *
           MOVE ZERO                       TO PW-RC
           MOVE SPACES                     TO PW-REASON-CD
                                              PW-ADVICE-CD
           MOVE 'N'                        TO PW-SANCTION-FLG
           MOVE ZERO                       TO PW-SCORE
      *
           WRITE WORK-REC FROM PARTY-WORK-REC
           IF NOT WS-WORK-OK
               MOVE 'PARTYWK '             TO ER-FILE-NAME
               MOVE WS-WORK-STATUS         TO ER-FILE-STATUS
               MOVE 'WRITE TO WORK LIST FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           ADD 1                           TO WS-WRITE-CNT
      *
           PERFORM 1900-FETCH-WORK-ROW
           .
      *
       2100-READ-LIMIT.
           MOVE SPACES                     TO DCL-CARD-NUM
                                              DCL-RISK-BAND
           MOVE ZERO                       TO DCL-LIMIT-AMT
      *
           EXEC SQL
               SELECT MIN(L.CARD_NUM)
                    , MAX(L.LIMIT_AMT)
                    , MAX(L.RISK_BAND)
                 INTO :DCL-CARD-NUM
                    , :DCL-LIMIT-AMT
                    , :DCL-RISK-BAND :IND-RISK-BAND
                 FROM CARDSVC.CARD_LIMIT L
                    , CARDSVC.CARD C
                WHERE C.ACCT_ID    = :DCL-ACCT-ID
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
                   ADD 1                   TO WS-NO-LIMIT-CNT
               WHEN OTHER
                   MOVE 'CARD_LIMIT       ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '          TO ER-SQL-OPERATION
                   MOVE 'SELECT ON CARD_LIMIT FAILED' TO ER-MESSAGE
                   MOVE 0603                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
      *
           MOVE DCL-CARD-NUM               TO PW-CARD-NUM
           MOVE DCL-LIMIT-AMT              TO PW-CREDIT-LIMIT
      *
           IF IND-RISK-BAND < 0 OR DCL-RISK-BAND = SPACES
               MOVE 'U'                    TO PW-RISK-BAND-OLD
           ELSE
               MOVE DCL-RISK-BAND          TO PW-RISK-BAND-OLD
           END-IF
           MOVE PW-RISK-BAND-OLD           TO PW-RISK-BAND-NEW
           .
      *
       3000-TERMINATE.
           EXEC SQL CLOSE WRKCSR END-EXEC
      *
           CLOSE WORK-FILE
                 CYCLCTL-FILE
      *
           IF WS-WRITE-CNT = ZERO
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
               DISPLAY 'CBCRD06A - WARNING, WORK LIST IS EMPTY'
           END-IF
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-FETCH-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06A ROWS FETCHED      ' WS-DISPLAY-CNT
           MOVE WS-WRITE-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06A WORK RECORDS      ' WS-DISPLAY-CNT
           MOVE WS-QUIET-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06A NO CYCLE MOVEMENT ' WS-DISPLAY-CNT
           MOVE WS-NO-LIMIT-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06A NO ACTIVE LIMIT   ' WS-DISPLAY-CNT
           MOVE WS-EXPOSURE-TOTAL          TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD06A EXPOSURE TOTAL    ' WS-DISPLAY-AMT
           DISPLAY '----------------------------------------------'
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD06A SQL ERROR SQLCODE=' SQLCODE
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
           DISPLAY 'CBCRD06A FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
