      ******************************************************************
      * CBCRD10 - CYCLE CLOSE AND NIGHTLY CONTROL REPORT               *
      *                                                                *
      * JOB CBCRD10J STEP010.  THE LAST JOB OF THE NIGHTLY CYCLE.      *
      *                                                                *
      * MARKS THE CYCLE CONTROL RECORD COMPLETE, PRINTS THE CONTROL    *
      * REPORT THAT OPERATIONS FILE EVERY MORNING, AND DECIDES WHETHER *
      * THE ONLINE REGION MAY REOPEN.  THE DECISION IS CARRIED BOTH IN *
      * CC-ONLINE-CLOSED-FLG AND IN THE STEP RETURN CODE, BECAUSE THE  *
      * REGION START JOB TESTS THE RETURN CODE AND THE OPERATOR TESTS  *
      * THE FLAG.                                                      *
      *                                                                *
      * THE REPORT CARRIES                                             *
      *   - RECORDS READ, WRITTEN AND REJECTED AT EACH STAGE           *
      *   - THE AUTHORIZATION HASH TOTAL CARRIED FROM CBCRD01 AND      *
      *     RE-CALCULATED BY CBCRD09                                   *
      *   - DEBIT AND CREDIT TOTALS FROM THE LEDGER FEED               *
      *   - ELAPSED TIME PER STAGE FROM THE STAGE STATISTICS FILE      *
      *   - AN EXCEPTION SUMMARY                                       *
      *                                                                *
      * THE STAGE STATISTICS FILE STGSTAT IS BUILT BY THE JOB          *
      * ACCOUNTING EXTRACT THAT THE SCHEDULER RUNS AFTER EACH JOB OF   *
      * THE CYCLE AND IS ALLOCATED HERE AS DISP=SHR.  A STAGE WITH NO  *
      * STATISTICS RECORD IS REPORTED, NOT ABENDED - THE OPERATOR      *
      * NEEDS THE REPORT MOST ON THE NIGHT SOMETHING DID NOT RUN.      *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - STGSTAT  QSAM INPUT  LRECL 120                   *
      *             - CTLRPT   SYSOUT      LRECL 133                   *
      *             - CYCLCTL  VSAM KSDS UPDATE                        *
      * TABLES      - CARDSVC.TRANSACTION (COUNT)                      *
      *             - CARDSVC.GL_POSTING  (COUNT AND TOTALS)           *
      *             - CARDSVC.ACCOUNT     (DELINQUENCY COUNT)          *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 CYCLE CLEAN, ONLINE REGION MAY REOPEN       *
      *               0004 CYCLE COMPLETED WITH EXCEPTIONS, REGION MAY *
      *                    REOPEN, EXCEPTION REPORT MUST BE ACTIONED   *
      *               0008 CYCLE INCOMPLETE OR OUT OF BALANCE, REGION  *
      *                    STAYS CLOSED PENDING THE DUTY MANAGER       *
      *               0012 FATAL                                       *
      * USER ABEND  - U1002 FILE OR VSAM FAILURE                       *
      *               U1003 UNRECOVERABLE SQL ERROR                    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD10.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1996-11-30.
      *
      * MAINTENANCE
      * 1996-11-30 CRD0640 ORIGINAL
      * 2001-08-08 CRD2233 HASH COMPARISON ADDED
      * 2009-03-30 CRD6104 REGION REOPEN DECISION MOVED OUT OF THE
      *                    OPERATOR PROCEDURE AND INTO THIS PROGRAM
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STGSTAT-FILE  ASSIGN TO STGSTAT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STGSTAT-STATUS.
      *
           SELECT REPORT-FILE   ASSIGN TO CTLRPT
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
       FD  STGSTAT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS.
       01  STGSTAT-REC.
           05  ST-CYCLE-DATE               PIC 9(8).
           05  ST-JOB-NAME                 PIC X(8).
           05  ST-STEP-NAME                PIC X(8).
           05  ST-PGM-NAME                 PIC X(8).
           05  ST-RECS-READ                PIC 9(9).
           05  ST-RECS-WRITTEN             PIC 9(9).
           05  ST-RECS-REJECTED            PIC 9(9).
           05  ST-START-TIME               PIC 9(6).
           05  ST-END-TIME                 PIC 9(6).
           05  ST-RETURN-CODE              PIC 9(4).
           05  ST-EXCEPTION-CNT            PIC 9(7).
           05  ST-FILLER                   PIC X(38).
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD10 '.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-STGSTAT-STATUS           PIC X(2)  VALUE '00'.
               88  WS-STGSTAT-OK                     VALUE '00'.
               88  WS-STGSTAT-EOF                    VALUE '10'.
           05  WS-REPORT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REPORT-OK                      VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-REOPEN-SW                PIC X     VALUE 'Y'.
               88  WS-MAY-REOPEN                     VALUE 'Y'.
               88  WS-STAY-CLOSED                    VALUE 'N'.
      *
       01  WS-COUNTERS.
           05  WS-STAGE-CNT                PIC 9(4)  VALUE ZERO.
           05  WS-MISSING-CNT              PIC 9(4)  VALUE ZERO.
           05  WS-EXCEPTION-TOT            PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-TOT               PIC 9(9)  VALUE ZERO.
           05  WS-WORST-RC                 PIC 9(4)  VALUE ZERO.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE 99.
           05  WS-PAGE-CNT                 PIC 9(4)  VALUE ZERO.
      *
       01  WS-ELAPSED-FIELDS.
           05  WS-START-SECS               PIC 9(6)  VALUE ZERO.
           05  WS-END-SECS                 PIC 9(6)  VALUE ZERO.
           05  WS-ELAPSED-SECS             PIC S9(7) VALUE ZERO.
           05  WS-TOTAL-SECS               PIC 9(7)  VALUE ZERO.
           05  WS-ELAPSED-HH               PIC 9(2)  VALUE ZERO.
           05  WS-ELAPSED-MM               PIC 9(2)  VALUE ZERO.
           05  WS-ELAPSED-SS               PIC 9(2)  VALUE ZERO.
      *
       01  WS-TIME-PARTS.
           05  WS-TP-HH                    PIC 9(2).
           05  WS-TP-MM                    PIC 9(2).
           05  WS-TP-SS                    PIC 9(2).
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYC-DT                       PIC X(10) VALUE SPACES.
      *
       01  WS-DB-COUNTS.
           05  WS-TXN-ROWS                 PIC 9(9)  VALUE ZERO.
           05  WS-GL-ROWS                  PIC 9(9)  VALUE ZERO.
           05  WS-DELQ-ACCTS               PIC 9(9)  VALUE ZERO.
           05  WS-UNPOSTED-TXN             PIC 9(9)  VALUE ZERO.
      *
       01  WS-DB-TOTALS.
           05  WS-GL-DEBITS                PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-GL-CREDITS               PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-GL-DIFFERENCE            PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
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
      * THE EXPECTED STAGE LIST.  A STAGE THAT NEVER WROTE A           *
      * STATISTICS RECORD DID NOT RUN, AND THE CYCLE IS NOT CLEAN.     *
      ******************************************************************
       01  WS-EXPECTED-TABLE.
           05  FILLER      PIC X(8) VALUE 'CBCRD01 '.
           05  FILLER      PIC X(8) VALUE 'CBCRD02 '.
           05  FILLER      PIC X(8) VALUE 'CBCRD03 '.
           05  FILLER      PIC X(8) VALUE 'CBCRD04 '.
           05  FILLER      PIC X(8) VALUE 'CBCRD05A'.
           05  FILLER      PIC X(8) VALUE 'CBCRD05B'.
           05  FILLER      PIC X(8) VALUE 'CBCRD06B'.
           05  FILLER      PIC X(8) VALUE 'CBCRD06C'.
           05  FILLER      PIC X(8) VALUE 'CBCRD07 '.
           05  FILLER      PIC X(8) VALUE 'CBCRD08 '.
           05  FILLER      PIC X(8) VALUE 'CBCRD09 '.
       01  WS-EXPECTED-LIST REDEFINES WS-EXPECTED-TABLE.
           05  WS-EXPECTED-PGM             PIC X(8) OCCURS 11 TIMES
                                           INDEXED BY WS-EXP-IDX.
      *
       01  WS-SEEN-TABLE.
           05  WS-SEEN-FLAG                PIC X OCCURS 11 TIMES.
      *
      ******************************************************************
      * REPORT LINES                                                   *
      ******************************************************************
       01  RPT-HEAD-1.
           05  FILLER                      PIC X(9)  VALUE 'CBCRD10  '.
           05  FILLER                      PIC X(42) VALUE
               'CARD SERVICES - NIGHTLY CYCLE CONTROL RPT '.
           05  FILLER                      PIC X(11) VALUE
               'CYCLE DATE '.
           05  RH1-CYCLE-DATE              PIC 9(8).
           05  FILLER                      PIC X(10) VALUE
               '  CYCLE   '.
           05  RH1-CYCLE-ID                PIC X(8).
           05  FILLER                      PIC X(29) VALUE SPACES.
           05  FILLER                      PIC X(5)  VALUE 'PAGE '.
           05  RH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(7)  VALUE SPACES.
      *
       01  RPT-HEAD-2.
           05  FILLER                      PIC X(10) VALUE
               'JOB       '.
           05  FILLER                      PIC X(9)  VALUE 'PROGRAM  '.
           05  FILLER                      PIC X(14) VALUE
               '          READ'.
           05  FILLER                      PIC X(14) VALUE
               '       WRITTEN'.
           05  FILLER                      PIC X(14) VALUE
               '      REJECTED'.
           05  FILLER                      PIC X(12) VALUE
               '  EXCEPTIONS'.
           05  FILLER                      PIC X(11) VALUE
               '    ELAPSED'.
           05  FILLER                      PIC X(5)  VALUE '   RC'.
           05  FILLER                      PIC X(44) VALUE SPACES.
      *
       01  RPT-STAGE-LINE.
           05  RS-JOB-NAME                 PIC X(8).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RS-PGM-NAME                 PIC X(8).
           05  FILLER                      PIC X     VALUE SPACES.
           05  RS-READ                     PIC Z,ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X     VALUE SPACES.
           05  RS-WRITTEN                  PIC Z,ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X     VALUE SPACES.
           05  RS-REJECTED                 PIC Z,ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RS-EXCEPTIONS               PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  RS-ELAPSED                  PIC X(8).
           05  FILLER                      PIC X(3)  VALUE SPACES.
           05  RS-RC                       PIC ZZZ9.
           05  FILLER                      PIC X(45) VALUE SPACES.
      *
       01  RPT-MISSING-LINE.
           05  FILLER                      PIC X(11) VALUE
               '*** STAGE  '.
           05  RM-PGM-NAME                 PIC X(8).
           05  FILLER                      PIC X(46) VALUE
               ' WROTE NO STATISTICS RECORD - DID IT RUN ?    '.
           05  FILLER                      PIC X(68) VALUE SPACES.
      *
       01  RPT-TOTAL-LINE.
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  RT-TEXT                     PIC X(36).
           05  RT-VALUE                    PIC X(24).
           05  FILLER                      PIC X(69) VALUE SPACES.
      *
       01  WS-EDIT-COUNT                   PIC Z,ZZZ,ZZZ,ZZ9.
       01  WS-EDIT-AMOUNT                  PIC ---,---,---,--9.99.
       01  WS-EDIT-HASH                    PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
      *
       01  WS-ELAPSED-EDIT.
           05  WS-EE-HH                    PIC 9(2).
           05  FILLER                      PIC X     VALUE ':'.
           05  WS-EE-MM                    PIC 9(2).
           05  FILLER                      PIC X     VALUE ':'.
           05  WS-EE-SS                    PIC 9(2).
      *
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-COUNTS.
           05  DCL-TXN-ROWS                PIC S9(9)     COMP-3.
           05  DCL-GL-ROWS                 PIC S9(9)     COMP-3.
           05  DCL-DELQ-ACCTS              PIC S9(9)     COMP-3.
           05  DCL-UNPOSTED                PIC S9(9)     COMP-3.
           05  DCL-GL-DEBITS               PIC S9(13)V99 COMP-3.
           05  DCL-GL-CREDITS              PIC S9(13)V99 COMP-3.
      *
       01  IND-VARS.
           05  IND-GL-DEBITS               PIC S9(4) COMP.
           05  IND-GL-CREDITS              PIC S9(4) COMP.
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
           PERFORM 2000-PROCESS-STAGE
               UNTIL WS-EOF
           PERFORM 3000-REPORT-MISSING
           PERFORM 4000-DATABASE-PROOF
           PERFORM 5000-REPORT-TOTALS
           PERFORM 6000-CLOSE-CYCLE
           PERFORM 7000-TERMINATE
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
               MOVE 1002                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO WS-CYC-DT
           END-STRING
           MOVE WS-CYCLE-DATE              TO RH1-CYCLE-DATE
      *
           PERFORM VARYING WS-EXP-IDX FROM 1 BY 1
                     UNTIL WS-EXP-IDX > 11
               MOVE 'N'                    TO WS-SEEN-FLAG(WS-EXP-IDX)
           END-PERFORM
      *
           OPEN INPUT  STGSTAT-FILE
           OPEN OUTPUT REPORT-FILE
           OPEN I-O    CYCLCTL-FILE
      *
           IF NOT WS-REPORT-OK
               MOVE 'CTLRPT  '             TO ER-FILE-NAME
               MOVE WS-REPORT-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF CONTROL REPORT FAILED' TO ER-MESSAGE
               MOVE 1002                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
      *    AN EMPTY OR ABSENT STATISTICS FILE IS NOT FATAL, BUT IT
      *    MEANS THE REPORT CANNOT PROVE THE CYCLE RAN.
           IF NOT WS-STGSTAT-OK
               DISPLAY 'CBCRD10 - STGSTAT OPEN STATUS '
                       WS-STGSTAT-STATUS
               MOVE 'Y'                    TO WS-EOF-SW
               MOVE WS-RC-ERROR            TO WS-WORST-RC
           END-IF
      *
           PERFORM 1100-READ-CYCLE-CONTROL
           PERFORM 8000-PRINT-HEADINGS
      *
           IF NOT WS-EOF
               PERFORM 1900-READ-STAGE
           END-IF
           .
      *
       1100-READ-CYCLE-CONTROL.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 1002               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE CC-CYCLE-ID                TO RH1-CYCLE-ID
      *
      *    A CYCLE THAT WAS RESTARTED IS STILL ACCEPTABLE, BUT IT IS
      *    REPORTED AND IT COSTS A WARNING.
           IF CC-RESTART-CNT > ZERO
               MOVE WS-RC-WARNING          TO WS-WORST-RC
           END-IF
           .
      *
       1900-READ-STAGE.
           READ STGSTAT-FILE
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
           END-READ
      *
           IF NOT WS-STGSTAT-OK AND NOT WS-STGSTAT-EOF
               MOVE 'STGSTAT '             TO ER-FILE-NAME
               MOVE WS-STGSTAT-STATUS      TO ER-FILE-STATUS
               MOVE 'READ OF STAGE STATISTICS FAILED' TO ER-MESSAGE
               MOVE 1002                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 2000 - ONE STAGE STATISTICS RECORD                             *
      ******************************************************************
       2000-PROCESS-STAGE.
      *    STATISTICS FROM AN EARLIER CYCLE ARE LEFT ON THE FILE UNTIL
      *    THE WEEKLY HOUSEKEEPING RUNS, SO THEY ARE SKIPPED HERE.
           IF ST-CYCLE-DATE NOT = WS-CYCLE-DATE
               GO TO 2000-EXIT
           END-IF
      *
           ADD 1                           TO WS-STAGE-CNT
           PERFORM 2100-MARK-SEEN
           PERFORM 2200-CALCULATE-ELAPSED
           PERFORM 2300-PRINT-STAGE
      *
           ADD ST-RECS-REJECTED            TO WS-REJECT-TOT
           ADD ST-EXCEPTION-CNT            TO WS-EXCEPTION-TOT
           ADD WS-ELAPSED-SECS             TO WS-TOTAL-SECS
      *
           IF ST-RETURN-CODE > WS-WORST-RC
               MOVE ST-RETURN-CODE         TO WS-WORST-RC
           END-IF
           .
       2000-EXIT.
           PERFORM 1900-READ-STAGE
           .
      *
       2100-MARK-SEEN.
           PERFORM VARYING WS-EXP-IDX FROM 1 BY 1
                     UNTIL WS-EXP-IDX > 11
               IF WS-EXPECTED-PGM(WS-EXP-IDX) = ST-PGM-NAME
                   MOVE 'Y'                TO WS-SEEN-FLAG(WS-EXP-IDX)
               END-IF
           END-PERFORM
           .
      *
      ******************************************************************
      * 2200 - ELAPSED TIME.  THE STATISTICS CARRY HHMMSS ONLY, SO A   *
      *        STAGE THAT CROSSED MIDNIGHT SHOWS A NEGATIVE ELAPSED    *
      *        AND HAS A DAY ADDED BACK.                               *
      ******************************************************************
       2200-CALCULATE-ELAPSED.
           MOVE ST-START-TIME              TO WS-TIME-PARTS
           COMPUTE WS-START-SECS =
                   WS-TP-HH * 3600 + WS-TP-MM * 60 + WS-TP-SS
      *
           MOVE ST-END-TIME                TO WS-TIME-PARTS
           COMPUTE WS-END-SECS =
                   WS-TP-HH * 3600 + WS-TP-MM * 60 + WS-TP-SS
      *
           COMPUTE WS-ELAPSED-SECS = WS-END-SECS - WS-START-SECS
           IF WS-ELAPSED-SECS < ZERO
               ADD 86400                   TO WS-ELAPSED-SECS
           END-IF
      *
           DIVIDE WS-ELAPSED-SECS BY 3600 GIVING WS-ELAPSED-HH
                  REMAINDER WS-ELAPSED-SECS
           DIVIDE WS-ELAPSED-SECS BY 60   GIVING WS-ELAPSED-MM
                  REMAINDER WS-ELAPSED-SS
      *
           MOVE WS-ELAPSED-HH              TO WS-EE-HH
           MOVE WS-ELAPSED-MM              TO WS-EE-MM
           MOVE WS-ELAPSED-SS              TO WS-EE-SS
      *
      *    THE DIVIDES CONSUMED THE SECONDS FIELD, SO IT IS REBUILT
      *    FOR THE RUNNING TOTAL.
           COMPUTE WS-ELAPSED-SECS =
                   WS-ELAPSED-HH * 3600
                 + WS-ELAPSED-MM * 60
                 + WS-ELAPSED-SS
           .
      *
       2300-PRINT-STAGE.
           IF WS-LINE-CNT > 55
               PERFORM 8000-PRINT-HEADINGS
           END-IF
      *
           MOVE ST-JOB-NAME                TO RS-JOB-NAME
           MOVE ST-PGM-NAME                TO RS-PGM-NAME
           MOVE ST-RECS-READ               TO RS-READ
           MOVE ST-RECS-WRITTEN            TO RS-WRITTEN
           MOVE ST-RECS-REJECTED           TO RS-REJECTED
           MOVE ST-EXCEPTION-CNT           TO RS-EXCEPTIONS
           MOVE WS-ELAPSED-EDIT            TO RS-ELAPSED
           MOVE ST-RETURN-CODE             TO RS-RC
           WRITE REPORT-REC FROM RPT-STAGE-LINE
           ADD 1                           TO WS-LINE-CNT
           .
      *
       3000-REPORT-MISSING.
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
           ADD 1                           TO WS-LINE-CNT
      *
           PERFORM VARYING WS-EXP-IDX FROM 1 BY 1
                     UNTIL WS-EXP-IDX > 11
               IF WS-SEEN-FLAG(WS-EXP-IDX) = 'N'
                   ADD 1                   TO WS-MISSING-CNT
                   MOVE WS-EXPECTED-PGM(WS-EXP-IDX)
                                           TO RM-PGM-NAME
                   WRITE REPORT-REC FROM RPT-MISSING-LINE
                   ADD 1                   TO WS-LINE-CNT
                   DISPLAY 'CBCRD10 - NO STATISTICS FOR '
                           WS-EXPECTED-PGM(WS-EXP-IDX)
               END-IF
           END-PERFORM
      *
           IF WS-MISSING-CNT > ZERO
               MOVE WS-RC-ERROR            TO WS-WORST-RC
           END-IF
           .
      *
      ******************************************************************
      * 4000 - THE DATABASE MUST AGREE WITH THE STATISTICS.  THESE     *
      *        COUNTS ARE CHEAP AND HAVE CAUGHT A HALF POSTED CYCLE    *
      *        MORE THAN ONCE.                                         *
      ******************************************************************
       4000-DATABASE-PROOF.
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-TXN-ROWS
                 FROM CARDSVC.TRANSACTION
                WHERE POST_DATE = DATE(:WS-CYC-DT)
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'TRANSACTION      '    TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               MOVE 'COUNT OF POSTED TRANSACTIONS FAILED'
                                           TO ER-MESSAGE
               MOVE 1003                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           MOVE DCL-TXN-ROWS               TO WS-TXN-ROWS
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-UNPOSTED
                 FROM CARDSVC.TRANSACTION
                WHERE POST_DATE     = DATE(:WS-CYC-DT)
                  AND GL_POSTED_FLG = 'N'
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'TRANSACTION      '    TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               MOVE 'COUNT OF UNPOSTED TRANSACTIONS FAILED'
                                           TO ER-MESSAGE
               MOVE 1003                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           MOVE DCL-UNPOSTED               TO WS-UNPOSTED-TXN
      *
           EXEC SQL
               SELECT COUNT(*)
                    , SUM(CASE WHEN DR_CR_IND = 'D'
                               THEN AMOUNT ELSE 0 END)
                    , SUM(CASE WHEN DR_CR_IND = 'C'
                               THEN AMOUNT ELSE 0 END)
                 INTO :DCL-GL-ROWS
                    , :DCL-GL-DEBITS  :IND-GL-DEBITS
                    , :DCL-GL-CREDITS :IND-GL-CREDITS
                 FROM CARDSVC.GL_POSTING
                WHERE POST_DATE = DATE(:WS-CYC-DT)
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'GL_POSTING       '    TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               MOVE 'LEDGER TOTALS COULD NOT BE READ BACK'
                                           TO ER-MESSAGE
               MOVE 1003                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           MOVE DCL-GL-ROWS                TO WS-GL-ROWS
      *
           IF IND-GL-DEBITS < 0
               MOVE ZERO                   TO WS-GL-DEBITS
           ELSE
               MOVE DCL-GL-DEBITS          TO WS-GL-DEBITS
           END-IF
           IF IND-GL-CREDITS < 0
               MOVE ZERO                   TO WS-GL-CREDITS
           ELSE
               MOVE DCL-GL-CREDITS         TO WS-GL-CREDITS
           END-IF
           COMPUTE WS-GL-DIFFERENCE = WS-GL-DEBITS - WS-GL-CREDITS
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-DELQ-ACCTS
                 FROM CARDSVC.ACCOUNT
                WHERE DELQ_BUCKET > 0
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          '    TO ER-SQL-TABLE
               MOVE 'SELECT  '             TO ER-SQL-OPERATION
               MOVE 'COUNT OF DELINQUENT ACCOUNTS FAILED'
                                           TO ER-MESSAGE
               MOVE 1003                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
           MOVE DCL-DELQ-ACCTS             TO WS-DELQ-ACCTS
      *
      *    UNPOSTED TRANSACTIONS OR AN UNBALANCED LEDGER KEEP THE
      *    ONLINE REGION CLOSED WHATEVER THE STAGE RETURN CODES SAID.
           IF WS-UNPOSTED-TXN > ZERO
            OR WS-GL-DIFFERENCE NOT = ZERO
               MOVE WS-RC-ERROR            TO WS-WORST-RC
           END-IF
           .
      *
      ******************************************************************
      * 5000 - CONTROL TOTALS                                          *
      ******************************************************************
       5000-REPORT-TOTALS.
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
      *
           MOVE 'AUTHORIZATIONS READ (CBCRD01)'    TO RT-TEXT
           MOVE CC-RECS-READ               TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'EXTRACT RECORDS WRITTEN'          TO RT-TEXT
           MOVE CC-RECS-WRITTEN            TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'RECORDS REJECTED IN THE CYCLE'    TO RT-TEXT
           MOVE WS-REJECT-TOT              TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'AUTHORIZATION HASH TOTAL'         TO RT-TEXT
           MOVE CC-HASH-TOTAL              TO WS-EDIT-HASH
           MOVE WS-EDIT-HASH               TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'TRANSACTIONS POSTED THIS CYCLE'   TO RT-TEXT
           MOVE WS-TXN-ROWS                TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'TRANSACTIONS NOT YET IN THE LEDGER' TO RT-TEXT
           MOVE WS-UNPOSTED-TXN            TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'LEDGER ROWS WRITTEN'              TO RT-TEXT
           MOVE WS-GL-ROWS                 TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'LEDGER DEBIT TOTAL'               TO RT-TEXT
           MOVE WS-GL-DEBITS               TO WS-EDIT-AMOUNT
           MOVE WS-EDIT-AMOUNT             TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'LEDGER CREDIT TOTAL'              TO RT-TEXT
           MOVE WS-GL-CREDITS              TO WS-EDIT-AMOUNT
           MOVE WS-EDIT-AMOUNT             TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'LEDGER DIFFERENCE'                TO RT-TEXT
           MOVE WS-GL-DIFFERENCE           TO WS-EDIT-AMOUNT
           MOVE WS-EDIT-AMOUNT             TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'DELINQUENT ACCOUNTS AFTER THE ROLL' TO RT-TEXT
           MOVE WS-DELQ-ACCTS              TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'EXCEPTIONS RAISED IN THE CYCLE'   TO RT-TEXT
           MOVE WS-EXCEPTION-TOT           TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           MOVE 'RESTARTS TAKEN THIS CYCLE'        TO RT-TEXT
           MOVE CC-RESTART-CNT             TO WS-EDIT-COUNT
           MOVE WS-EDIT-COUNT              TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
      *
           PERFORM 5100-REPORT-ELAPSED
           .
      *
       5100-REPORT-ELAPSED.
           DIVIDE WS-TOTAL-SECS BY 3600 GIVING WS-ELAPSED-HH
                  REMAINDER WS-ELAPSED-SECS
           DIVIDE WS-ELAPSED-SECS BY 60 GIVING WS-ELAPSED-MM
                  REMAINDER WS-ELAPSED-SS
           MOVE WS-ELAPSED-HH              TO WS-EE-HH
           MOVE WS-ELAPSED-MM              TO WS-EE-MM
           MOVE WS-ELAPSED-SS              TO WS-EE-SS
      *
           MOVE 'TOTAL ELAPSED PROCESSING TIME'    TO RT-TEXT
           MOVE WS-ELAPSED-EDIT            TO RT-VALUE
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
           .
      *
      ******************************************************************
      * 6000 - CLOSE THE CYCLE AND DECIDE ON THE ONLINE REGION         *
      ******************************************************************
       6000-CLOSE-CYCLE.
           MOVE WS-WORST-RC                TO WS-RETURN-CODE
      *
           EVALUATE TRUE
               WHEN WS-WORST-RC >= WS-RC-ERROR
                   MOVE 'N'                TO WS-REOPEN-SW
                   MOVE 'Y'                TO CC-ONLINE-CLOSED-FLG
                   SET CC-FAILED           TO TRUE
               WHEN OTHER
                   MOVE 'Y'                TO WS-REOPEN-SW
                   MOVE 'N'                TO CC-ONLINE-CLOSED-FLG
                   SET CC-COMPLETE         TO TRUE
           END-EVALUATE
      *
           MOVE WS-PROGRAM-ID              TO CC-CURRENT-STEP
           IF WS-MAY-REOPEN
               MOVE WS-PROGRAM-ID          TO CC-LAST-GOOD-STEP
           END-IF
           MOVE WS-TIMESTAMP               TO CC-END-TS
      *
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'CYCLE COULD NOT BE MARKED COMPLETE' TO ER-MESSAGE
               MOVE 1002                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
      *
           IF WS-MAY-REOPEN
               MOVE 'ONLINE REGION'                TO RT-TEXT
               MOVE 'MAY REOPEN'                   TO RT-VALUE
           ELSE
               MOVE 'ONLINE REGION'                TO RT-TEXT
               MOVE 'HELD CLOSED - CALL DUTY MGR'  TO RT-VALUE
           END-IF
           WRITE REPORT-REC FROM RPT-TOTAL-LINE
           .
      *
       7000-TERMINATE.
           IF WS-STGSTAT-OK OR WS-STGSTAT-EOF
               CLOSE STGSTAT-FILE
           END-IF
           CLOSE REPORT-FILE
                 CYCLCTL-FILE
      *
           DISPLAY '----------------------------------------------'
           DISPLAY 'CBCRD10 CYCLE DATE         ' WS-CYCLE-DATE
           DISPLAY 'CBCRD10 STAGES REPORTED    ' WS-STAGE-CNT
           DISPLAY 'CBCRD10 STAGES MISSING     ' WS-MISSING-CNT
           MOVE WS-EXCEPTION-TOT           TO WS-EDIT-COUNT
           DISPLAY 'CBCRD10 EXCEPTIONS         ' WS-EDIT-COUNT
           MOVE WS-GL-DIFFERENCE           TO WS-EDIT-AMOUNT
           DISPLAY 'CBCRD10 LEDGER DIFFERENCE ' WS-EDIT-AMOUNT
           IF WS-MAY-REOPEN
               DISPLAY 'CBCRD10 ONLINE REGION MAY REOPEN'
           ELSE
               DISPLAY 'CBCRD10 ONLINE REGION HELD CLOSED'
           END-IF
           DISPLAY '----------------------------------------------'
           .
      *
       8000-PRINT-HEADINGS.
           ADD 1                           TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT                TO RH1-PAGE
           WRITE REPORT-REC FROM RPT-HEAD-1
           WRITE REPORT-REC FROM RPT-HEAD-2
           MOVE SPACES                     TO REPORT-REC
           WRITE REPORT-REC
           MOVE 4                          TO WS-LINE-CNT
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD10 SQL ERROR SQLCODE=' SQLCODE
                   ' TABLE=' ER-SQL-TABLE
           PERFORM 9500-FATAL-ERROR
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD10 FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
