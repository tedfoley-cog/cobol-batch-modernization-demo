      ******************************************************************
      * CBCRD06C - APPLY THE ACCEPTED RISK OUTCOMES                    *
      *                                                                *
      * STEP040 OF JOB CBCRD06J.  ONLY RUNS WHEN STEP030 ENDED WITH A  *
      * RETURN CODE OF 4 OR LESS - SEE THE IF RC <= 4 GATE IN THE JCL. *
      *                                                                *
      * READS THE ACCEPTED FILE WRITTEN BY CBCRD06B AND APPLIES THE    *
      * RETURNED BAND TO EVERY CURRENT CREDIT LIMIT ROW HELD FOR THE   *
      * PARTY'S ACCOUNTS.  A PARTY THE CROSSING SENT TO MANUAL REVIEW  *
      * IS NOT ON THIS FILE AND IS THEREFORE NEVER APPLIED HERE.       *
      *                                                                *
      * A BAND THAT HAS DETERIORATED IS WRITTEN TO THE SERVICING TEAM  *
      * REPORT.  A MOVE TO BAND X ALSO SUPPRESSES THE CARD FOR NEW     *
      * AUTHORISATION BY SETTING THE AVAILABLE AMOUNT TO ZERO - THE    *
      * LIMIT ITSELF IS LEFT ALONE SO THE SERVICING TEAM CAN SEE WHAT  *
      * THE CUSTOMER USED TO HAVE.                                     *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - PARTYACC QSAM INPUT   LRECL 150                  *
      *             - BANDRPT  SYSOUT       LRECL 133                  *
      *             - CYCLCTL  VSAM KSDS UPDATE                        *
      * TABLES      - CARDSVC.CARD_LIMIT    (CURSOR, UPDATE)           *
      *             - CARDSVC.ACCOUNT       (SELECT)                   *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 ALL OUTCOMES APPLIED                        *
      *               0004 SOME PARTIES HAD NO CURRENT LIMIT ROW       *
      *               0012 FATAL                                       *
      * USER ABEND  - U0602 FILE OPEN OR I/O FAILURE                   *
      *               U0608 UNRECOVERABLE SQL ERROR                    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD06C.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  2001-04-26.
      *
      * MAINTENANCE
      * 2001-04-26 CRD2112 ORIGINAL
      * 2008-08-04 CRD5810 BAND X SUPPRESSES AVAILABLE AMOUNT
      * 2015-05-11 CRD9012 REPORT SPLIT BY PRODUCT FOR THE SERVICING
      *                    TEAM WHO WORK IT BY PORTFOLIO
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCEPT-FILE   ASSIGN TO PARTYACC
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-ACCEPT-STATUS.
      *
           SELECT REPORT-FILE   ASSIGN TO BANDRPT
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
       FD  ACCEPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       01  ACCEPT-REC                      PIC X(150).
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD06C'.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP040 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-ACCEPT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-ACCEPT-OK                      VALUE '00'.
           05  WS-REPORT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REPORT-OK                      VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-CURSOR-EOF-SW            PIC X     VALUE 'N'.
               88  WS-CURSOR-EOF                     VALUE 'Y'.
           05  WS-WORSE-SW                 PIC X     VALUE 'N'.
               88  WS-BAND-WORSE                     VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-UPDATE-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-PARTY-UPD-CNT            PIC 9(4)  VALUE ZERO.
           05  WS-NO-LIMIT-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-WORSE-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-BETTER-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SUPPRESS-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-UNCHANGED-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE 99.
           05  WS-PAGE-CNT                 PIC 9(4)  VALUE ZERO.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYC-DT                       PIC X(10) VALUE SPACES.
      *
       01  WS-BAND-RANK-INPUT              PIC X     VALUE SPACE.
       01  WS-BAND-RANK                    PIC 9     VALUE ZERO.
       01  WS-RANK-OLD                     PIC 9     VALUE ZERO.
       01  WS-RANK-NEW                     PIC 9     VALUE ZERO.
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
           05  FILLER                      PIC X(9)  VALUE 'CBCRD06C '.
           05  FILLER                      PIC X(44) VALUE
               'CARD SERVICES - RISK BAND MOVEMENT REPORT   '.
           05  FILLER                      PIC X(11) VALUE
               'CYCLE DATE '.
           05  RH1-CYCLE-DATE              PIC 9(8).
           05  FILLER                      PIC X(46) VALUE SPACES.
           05  FILLER                      PIC X(5)  VALUE 'PAGE '.
           05  RH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(6)  VALUE SPACES.
      *
       01  RPT-HEAD-2.
           05  FILLER                      PIC X(11) VALUE
               'PARTY      '.
           05  FILLER                      PIC X(13) VALUE
               'ACCOUNT      '.
           05  FILLER                      PIC X(17) VALUE
               'CARD             '.
           05  FILLER                      PIC X(5)  VALUE 'PROD '.
           05  FILLER                      PIC X(9)  VALUE 'BAND OLD '.
           05  FILLER                      PIC X(9)  VALUE 'BAND NEW '.
           05  FILLER                      PIC X(18) VALUE
               '         EXPOSURE '.
           05  FILLER                      PIC X(9)  VALUE 'MOVEMENT '.
           05  FILLER                      PIC X(42) VALUE SPACES.
      *
       01  RPT-DETAIL.
           05  RD-PARTY-ID                 PIC X(11).
           05  RD-ACCT-ID                  PIC 9(11).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-CARD-NUM                 PIC X(16).
           05  FILLER                      PIC X     VALUE SPACES.
           05  RD-PRODUCT-CD               PIC X(4).
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  RD-BAND-OLD                 PIC X.
           05  FILLER                      PIC X(8)  VALUE SPACES.
           05  RD-BAND-NEW                 PIC X.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-EXPOSURE                 PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-MOVEMENT                 PIC X(12).
           05  RD-REASON                   PIC X(4).
           05  FILLER                      PIC X(36) VALUE SPACES.
      *
       01  RPT-TOTAL.
           05  FILLER                      PIC X(30).
           05  RT-TEXT                     PIC X(30).
           05  RT-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(62) VALUE SPACES.
      *
           COPY CVPWRK01Y.
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-LIMIT.
           05  DCL-PARTY-ID                PIC X(11).
           05  DCL-ACCT-ID                 PIC S9(11)    COMP-3.
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-LIMIT-TYPE              PIC X(4).
           05  DCL-EFF-DATE                PIC X(10).
           05  DCL-RISK-BAND               PIC X(1).
           05  DCL-LIMIT-AMT               PIC S9(11)V99 COMP-3.
           05  DCL-USED-AMT                PIC S9(11)V99 COMP-3.
           05  DCL-PRODUCT-CD              PIC X(4).
      *
       01  IND-RISK-BAND                   PIC S9(4) COMP.
      *
      ******************************************************************
      * EVERY CURRENT CREDIT LIMIT ROW BELONGING TO THE PARTY.  THE    *
      * WORK LIST HOLDS ONE REPRESENTATIVE ACCOUNT PER PARTY, SO THE   *
      * CURSOR GOES BACK THROUGH ACCOUNT ON PARTY_ID TO PICK UP THE    *
      * REST OF THE RELATIONSHIP.                                      *
      ******************************************************************
           EXEC SQL DECLARE LIMCSR CURSOR FOR
               SELECT A.ACCT_ID
                    , L.CARD_NUM
                    , L.LIMIT_TYPE
                    , CHAR(L.EFF_DATE, ISO)
                    , L.RISK_BAND
                    , L.LIMIT_AMT
                    , L.USED_AMT
                    , A.PRODUCT_CD
                 FROM CARDSVC.ACCOUNT    A
                    , CARDSVC.CARD       C
                    , CARDSVC.CARD_LIMIT L
                WHERE A.PARTY_ID   = :DCL-PARTY-ID
                  AND A.ACCT_STATUS IN ('O','S')
                  AND C.ACCT_ID    = A.ACCT_ID
                  AND C.CARD_STATUS IN ('A','N')
                  AND L.CARD_NUM   = C.CARD_NUM
                  AND L.LIMIT_TYPE IN ('CRED','CASH')
                  AND DATE(:WS-CYC-DT)
                      BETWEEN L.EFF_DATE AND L.EXP_DATE
                ORDER BY A.ACCT_ID
                       , L.CARD_NUM
                       , L.LIMIT_TYPE
                  FOR UPDATE OF RISK_BAND
                              , LAST_REVIEW_DATE
                              , AVAIL_AMT
                              , LAST_MAINT_TS
           END-EXEC.
      *
       01  WS-DISPLAY-CNT                  PIC ZZZ,ZZZ,ZZ9.
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
           PERFORM 2000-APPLY-PARTY
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
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO WS-CYC-DT
           END-STRING
           MOVE WS-CYCLE-DATE              TO RH1-CYCLE-DATE
      *
           OPEN INPUT  ACCEPT-FILE
           OPEN OUTPUT REPORT-FILE
           OPEN I-O    CYCLCTL-FILE
      *
           IF NOT WS-ACCEPT-OK OR NOT WS-REPORT-OK
               MOVE 'PARTYACC'             TO ER-FILE-NAME
               MOVE WS-ACCEPT-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF ACCEPTED FILE OR REPORT FAILED'
                                           TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM 1800-UPDATE-CYCLE-STEP
           PERFORM 1900-READ-ACCEPTED
           .
      *
       1800-UPDATE-CYCLE-STEP.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 0602               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE 'CBCRD06C'                 TO CC-CURRENT-STEP
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'REWRITE OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       1900-READ-ACCEPTED.
           READ ACCEPT-FILE INTO PARTY-WORK-REC
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
               NOT AT END
                   ADD 1                   TO WS-READ-CNT
           END-READ
           .
      *
      ******************************************************************
      * 2000 - ONE ACCEPTED PARTY                                      *
      ******************************************************************
       2000-APPLY-PARTY.
           MOVE PW-PARTY-ID                TO DCL-PARTY-ID
           MOVE 'N'                        TO WS-CURSOR-EOF-SW
      *
           PERFORM 2100-RANK-MOVEMENT
      *
           EXEC SQL OPEN LIMCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'CARD_LIMIT       '    TO ER-SQL-TABLE
               MOVE 'OPEN    '             TO ER-SQL-OPERATION
               MOVE 'OPEN OF LIMCSR FAILED' TO ER-MESSAGE
               MOVE 0608                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           MOVE ZERO                       TO WS-PARTY-UPD-CNT
           PERFORM 2200-FETCH-LIMIT
           PERFORM UNTIL WS-CURSOR-EOF
               PERFORM 2300-UPDATE-LIMIT
               PERFORM 2200-FETCH-LIMIT
           END-PERFORM
      *
           EXEC SQL CLOSE LIMCSR END-EXEC
      *
           IF WS-PARTY-UPD-CNT = ZERO
               ADD 1                       TO WS-NO-LIMIT-CNT
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
               DISPLAY 'CBCRD06C - NO CURRENT LIMIT ROW FOR PARTY '
                       PW-PARTY-ID
           END-IF
      *
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQUENCY
               EXEC SQL COMMIT WORK END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
      *
           PERFORM 1900-READ-ACCEPTED
           .
      *
       2100-RANK-MOVEMENT.
           MOVE 'N'                        TO WS-WORSE-SW
           MOVE PW-RISK-BAND-OLD           TO WS-BAND-RANK-INPUT
           PERFORM 2150-RANK-BAND
           MOVE WS-BAND-RANK               TO WS-RANK-OLD
           MOVE PW-RISK-BAND-NEW           TO WS-BAND-RANK-INPUT
           PERFORM 2150-RANK-BAND
           MOVE WS-BAND-RANK               TO WS-RANK-NEW
      *
           EVALUATE TRUE
               WHEN WS-RANK-NEW > WS-RANK-OLD
                   MOVE 'Y'                TO WS-WORSE-SW
                   ADD 1                   TO WS-WORSE-CNT
               WHEN WS-RANK-NEW < WS-RANK-OLD
                   ADD 1                   TO WS-BETTER-CNT
               WHEN OTHER
                   ADD 1                   TO WS-UNCHANGED-CNT
           END-EVALUATE
           .
      *
       2150-RANK-BAND.
           EVALUATE WS-BAND-RANK-INPUT
               WHEN 'X'
                   MOVE 4                  TO WS-BAND-RANK
               WHEN 'C'
                   MOVE 3                  TO WS-BAND-RANK
               WHEN 'B'
                   MOVE 2                  TO WS-BAND-RANK
               WHEN 'A'
                   MOVE 1                  TO WS-BAND-RANK
               WHEN OTHER
                   MOVE 0                  TO WS-BAND-RANK
           END-EVALUATE
           .
      *
       2200-FETCH-LIMIT.
           EXEC SQL
               FETCH LIMCSR
                INTO :DCL-ACCT-ID
                   , :DCL-CARD-NUM
                   , :DCL-LIMIT-TYPE
                   , :DCL-EFF-DATE
                   , :DCL-RISK-BAND :IND-RISK-BAND
                   , :DCL-LIMIT-AMT
                   , :DCL-USED-AMT
                   , :DCL-PRODUCT-CD
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   MOVE 'Y'                TO WS-CURSOR-EOF-SW
               WHEN OTHER
                   MOVE 'CARD_LIMIT       ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '          TO ER-SQL-OPERATION
                   MOVE 'FETCH OF LIMCSR FAILED' TO ER-MESSAGE
                   MOVE 0608                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2300 - THE UPDATE ITSELF.  BAND X ALSO ZEROES THE AVAILABLE    *
      *        AMOUNT SO THE ONLINE AUTHORISATION PATH STOPS APPROVING *
      *        AGAINST A LIMIT THE RISK SERVICE HAS JUST REFUSED.      *
      ******************************************************************
       2300-UPDATE-LIMIT.
           IF PW-RISK-BAND-NEW = 'X'
               EXEC SQL
                   UPDATE CARDSVC.CARD_LIMIT
                      SET RISK_BAND        = :PW-RISK-BAND-NEW
                        , LAST_REVIEW_DATE = DATE(:WS-CYC-DT)
                        , AVAIL_AMT        = 0
                        , LAST_MAINT_TS    = CURRENT TIMESTAMP
                    WHERE CURRENT OF LIMCSR
               END-EXEC
               ADD 1                       TO WS-SUPPRESS-CNT
           ELSE
               EXEC SQL
                   UPDATE CARDSVC.CARD_LIMIT
                      SET RISK_BAND        = :PW-RISK-BAND-NEW
                        , LAST_REVIEW_DATE = DATE(:WS-CYC-DT)
                        , AVAIL_AMT        = LIMIT_AMT - USED_AMT
                        , LAST_MAINT_TS    = CURRENT TIMESTAMP
                    WHERE CURRENT OF LIMCSR
               END-EXEC
           END-IF
      *
           IF SQLCODE NOT = 0
               MOVE 'CARD_LIMIT       '    TO ER-SQL-TABLE
               MOVE 'UPDATE  '             TO ER-SQL-OPERATION
               MOVE 'POSITIONED UPDATE OF CARD_LIMIT FAILED'
                                           TO ER-MESSAGE
               MOVE 0608                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           ADD 1                           TO WS-UPDATE-CNT
                                              WS-PARTY-UPD-CNT
                                              WS-SINCE-COMMIT
      *
           IF WS-BAND-WORSE
               PERFORM 2400-PRINT-DETAIL
           END-IF
           .
      *
       2400-PRINT-DETAIL.
           IF WS-LINE-CNT > 55
               PERFORM 2450-PRINT-HEADINGS
           END-IF
      *
           MOVE PW-PARTY-ID                TO RD-PARTY-ID
           MOVE DCL-ACCT-ID                TO RD-ACCT-ID
           MOVE DCL-CARD-NUM               TO RD-CARD-NUM
           MOVE DCL-PRODUCT-CD             TO RD-PRODUCT-CD
      *
           IF IND-RISK-BAND < 0
               MOVE 'U'                    TO RD-BAND-OLD
           ELSE
               MOVE DCL-RISK-BAND          TO RD-BAND-OLD
           END-IF
           MOVE PW-RISK-BAND-NEW           TO RD-BAND-NEW
           MOVE PW-EXPOSURE-AMT            TO RD-EXPOSURE
           MOVE PW-REASON-CD               TO RD-REASON
      *
           IF PW-RISK-BAND-NEW = 'X'
               MOVE 'REFUSED     '         TO RD-MOVEMENT
           ELSE
               MOVE 'DETERIORATED'         TO RD-MOVEMENT
           END-IF
      *
           WRITE REPORT-REC FROM RPT-DETAIL
           IF NOT WS-REPORT-OK
               MOVE 'BANDRPT '             TO ER-FILE-NAME
               MOVE WS-REPORT-STATUS       TO ER-FILE-STATUS
               MOVE 'WRITE TO BAND REPORT FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           ADD 1                           TO WS-LINE-CNT
           .
      *
       2450-PRINT-HEADINGS.
           ADD 1                           TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT                TO RH1-PAGE
           WRITE REPORT-REC FROM RPT-HEAD-1
           WRITE REPORT-REC FROM RPT-HEAD-2
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
           MOVE 4                          TO WS-LINE-CNT
           .
      *
       3000-TERMINATE.
           EXEC SQL COMMIT WORK END-EXEC
      *
           PERFORM 3100-PRINT-TOTALS
      *
           CLOSE ACCEPT-FILE
                 REPORT-FILE
                 CYCLCTL-FILE
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-READ-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06C PARTIES APPLIED   ' WS-DISPLAY-CNT
           MOVE WS-UPDATE-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06C LIMIT ROWS UPDATED' WS-DISPLAY-CNT
           MOVE WS-WORSE-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06C BANDS WORSE       ' WS-DISPLAY-CNT
           MOVE WS-BETTER-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06C BANDS IMPROVED    ' WS-DISPLAY-CNT
           MOVE WS-UNCHANGED-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06C BANDS UNCHANGED   ' WS-DISPLAY-CNT
           MOVE WS-SUPPRESS-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06C LIMITS SUPPRESSED ' WS-DISPLAY-CNT
           MOVE WS-NO-LIMIT-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06C NO LIMIT ROW      ' WS-DISPLAY-CNT
           DISPLAY '----------------------------------------------'
           .
      *
       3100-PRINT-TOTALS.
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
      *
           MOVE 'PARTIES APPLIED'          TO RT-TEXT
           MOVE WS-READ-CNT                TO RT-COUNT
           WRITE REPORT-REC FROM RPT-TOTAL
      *
           MOVE 'BANDS DETERIORATED'       TO RT-TEXT
           MOVE WS-WORSE-CNT               TO RT-COUNT
           WRITE REPORT-REC FROM RPT-TOTAL
      *
           MOVE 'LIMITS SUPPRESSED'        TO RT-TEXT
           MOVE WS-SUPPRESS-CNT            TO RT-COUNT
           WRITE REPORT-REC FROM RPT-TOTAL
      *
           MOVE 'PARTIES WITH NO LIMIT ROW'  TO RT-TEXT
           MOVE WS-NO-LIMIT-CNT            TO RT-COUNT
           WRITE REPORT-REC FROM RPT-TOTAL
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD06C SQL ERROR SQLCODE=' SQLCODE
                   ' TABLE=' ER-SQL-TABLE
                   ' PARTY=' DCL-PARTY-ID
                   ' CARD=' DCL-CARD-NUM
           PERFORM 9500-FATAL-ERROR
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD06C FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
