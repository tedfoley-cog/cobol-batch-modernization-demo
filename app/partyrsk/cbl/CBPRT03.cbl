      ******************************************************************
      * CBPRT03 - FULL PORTFOLIO RESCORE DRIVER                        *
      *                                                                *
      * PART OF THE PARTYWK WEEKLY CYCLE, STEP THREE.                  *
      *                                                                *
      * DRIVES THE WHOLE ACTIVE PARTY BASE THROUGH THE RISK            *
      * RECALCULATION ENTRY POINT.  THE ENTRY POINT IS THE SAME ONE    *
      * THE CROSS MODULE NIGHTLY CALL USES, SO A PARTY SCORED HERE     *
      * AND A PARTY SCORED THERE GO THROUGH IDENTICAL CODE.            *
      *                                                                *
      * RESTART                                                        *
      *   THE CYCLE CONTROL RECORD ON VSAM CYCLCTL (CVCTRL01Y) HOLDS   *
      *   THE LAST PARTY ID COMMITTED.  ON A RESTART THE DRIVER READS  *
      *   THAT RECORD, POSITIONS THE CURSOR PAST IT AND CARRIES ON.    *
      *   THE RECORD IS REWRITTEN AT EVERY CHECKPOINT, IMMEDIATELY     *
      *   AFTER THE DB2 COMMIT, SO A FAILURE BETWEEN THE TWO REPEATS   *
      *   AT MOST ONE CHECKPOINT INTERVAL OF PARTIES.  RESCORING A     *
      *   PARTY TWICE IS HARMLESS - THE SCORE ROW IS TIMESTAMPED AND   *
      *   THE EXPOSURE ROW IS REPLACED.                                *
      *                                                                *
      *   PARM='RESTART' TELLS THE DRIVER TO PICK THE POSITION UP.     *
      *   PARM='COLD'    STARTS FROM THE BEGINNING AND RESETS THE      *
      *                  CONTROL RECORD.  SEE THE OPERATOR RUNBOOK.    *
      *                                                                *
      * RUN BY     - CBPRT03J STEP RESCORE                             *
      * CALLS      - PRBRSK1                                           *
      * FILES      - CYCLCTL   CYCLE CONTROL, KSDS, UPDATE             *
      *              RSKEXCP   EXCEPTION LIST, FB 133                  *
      * TABLES     - PARTYRSK.CUSTOMER   SELECT                        *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - WHOLE PORTFOLIO RESCORED CLEAN                          *
      *   04 - RESCORED WITH WARNINGS                                  *
      *   08 - RESCORED, MANUAL REVIEW CASES ON RSKEXCP                *
      *   12 - FAILED - RESTARTABLE FROM THE LAST CHECKPOINT           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBPRT03.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CYCLCTL-FILE  ASSIGN TO CYCLCTL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CC-KEY
                  FILE STATUS  IS WS-CTRL-STATUS.
      *
           SELECT RSKEXCP-FILE  ASSIGN TO RSKEXCP
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-EXCP-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CYCLCTL-FILE
           RECORD CONTAINS 256 CHARACTERS.
       01  CYCLCTL-REC                 PIC X(256).
      *
       FD  RSKEXCP-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  RSKEXCP-REC                 PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CBPRT03 '.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
       01  WS-JOB-NAME                 PIC X(8)  VALUE 'CBPRT03J'.
       01  WS-STEP-NAME                PIC X(8)  VALUE 'RESCORE '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-CTRL-STATUS          PIC X(2)  VALUE '00'.
           05  WS-EXCP-STATUS          PIC X(2)  VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X     VALUE 'N'.
               88  WS-EOF                        VALUE 'Y'.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
           05  WS-RESTART-SW           PIC X     VALUE 'N'.
               88  WS-RESTART-RUN                VALUE 'Y'.
           05  WS-CTRL-FOUND-SW        PIC X     VALUE 'N'.
               88  WS-CTRL-FOUND                 VALUE 'Y'.
      *
       01  WS-PARM-TEXT                PIC X(8)  VALUE SPACES.
           88  WS-PARM-RESTART                   VALUE 'RESTART '.
           88  WS-PARM-COLD                      VALUE 'COLD    '.
      *
       01  WS-TOTALS.
           05  WS-READ-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-SCORED-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-WARN-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-REVIEW-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-FAIL-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-SKIP-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-COMMIT-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT         PIC 9(9)  VALUE ZERO.
      *
       01  WS-BAND-TOTALS.
           05  WS-BAND-CNT OCCURS 4 TIMES PIC 9(9).
      *
       01  WS-WORK-FIELDS.
           05  WS-BAND-SUB             PIC S9(4) COMP VALUE ZERO.
           05  WS-WORST-RC             PIC 9(4)  VALUE ZERO.
           05  WS-CORREL-SEQ           PIC 9(8)  VALUE ZERO.
           05  WS-COMMIT-FREQ          PIC 9(6)  VALUE ZERO.
      *
       01  WS-EDIT-FIELDS.
           05  WS-ED-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-SQL-DISP             PIC -(9)9.
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME-R REDEFINES WS-CURR-TIME.
               10  WS-CURR-HHMMSS      PIC 9(6).
               10  WS-CURR-HUND        PIC 9(2).
           05  WS-TIMESTAMP            PIC X(26) VALUE SPACES.
      *
      ******************************************************************
      * EXCEPTION LINE.  ONE PER PARTY THE CHAIN WOULD NOT PASS.       *
      ******************************************************************
       01  WS-EXCP-LINE.
           05  EX-PARTY                PIC X(11).
           05  FILLER                  PIC X     VALUE SPACES.
           05  EX-RC                   PIC 9(4).
           05  FILLER                  PIC X     VALUE SPACES.
           05  EX-BAND                 PIC X.
           05  FILLER                  PIC X     VALUE SPACES.
           05  EX-SCORE                PIC 9(3).
           05  FILLER                  PIC X     VALUE SPACES.
           05  EX-SANCTION             PIC X.
           05  FILLER                  PIC X     VALUE SPACES.
           05  EX-KYC                  PIC X(2).
           05  FILLER                  PIC X     VALUE SPACES.
           05  EX-FAIL-PGM             PIC X(8).
           05  FILLER                  PIC X     VALUE SPACES.
           05  EX-REASON               PIC X(60).
           05  FILLER                  PIC X(35) VALUE SPACES.
      *
       01  LK-RETURN-AREA.
           05  LK-RETURN-CD            PIC S9(4) COMP VALUE ZERO.
           05  LK-RETURN-PGM           PIC X(8)  VALUE SPACES.
           05  LK-RETURN-MSG           PIC X(60) VALUE SPACES.
      *
      *    HOST VARIABLES
       01  DCL-CUST.
           05  DCL-PARTY-ID            PIC X(11).
           05  DCL-LAST-PARTY-ID       PIC X(11).
           05  DCL-CUST-ID             PIC S9(9) COMP-3.
           05  DCL-PARTY-TYPE          PIC X(1).
           05  DCL-SEGMENT-CD          PIC X(4).
      *
       01  DCL-IND.
           05  IND-SEGMENT             PIC S9(4) COMP.
      *
           COPY CVRISK01Y.
      *
           COPY CVCTRL01Y.
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      ******************************************************************
      * DRIVING CURSOR.  ONE ROW PER PARTY, NOT PER CUSTOMER - A       *
      * PARTY MAY CARRY SEVERAL LINKED CUSTOMER ROWS.  DECLARED WITH   *
      * HOLD SO THE CHECKPOINT COMMIT DOES NOT CLOSE IT.               *
      ******************************************************************
           EXEC SQL DECLARE PARTYCSR CURSOR WITH HOLD FOR
               SELECT PARTY_ID
                    , MIN(CUST_ID)
                    , MIN(PARTY_TYPE)
                    , MIN(SEGMENT_CD)
                 FROM PARTYRSK.CUSTOMER
                WHERE CUST_STATUS = 'A'
                  AND PARTY_ID > :DCL-LAST-PARTY-ID
                GROUP BY PARTY_ID
                ORDER BY PARTY_ID
                 WITH UR
           END-EXEC.
      *
       LINKAGE SECTION.
      *
       01  LK-PARM-AREA.
           05  LK-PARM-LEN             PIC S9(4) COMP.
           05  LK-PARM-TEXT            PIC X(8).
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-PARM-AREA.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
           PERFORM 0200-READ-CONTROL
           PERFORM 0300-OPEN-CURSOR
      *
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               PERFORM 2000-RESCORE-ONE-PARTY
           END-PERFORM
      *
           PERFORM 3000-CLOSE-DOWN
           PERFORM 9000-REPORT-TOTALS
           GOBACK
           .
      *
       0100-INITIALISE.
           MOVE '0100-INITIALISE'      TO WS-PARAGRAPH
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           ACCEPT WS-CURR-TIME         FROM TIME
      *
      *    PARM='RESTART' OR PARM='COLD'.  ANYTHING ELSE, INCLUDING
      *    AN OMITTED PARM, IS TREATED AS A COLD START.
           IF LK-PARM-LEN > ZERO
               MOVE LK-PARM-TEXT(1:LK-PARM-LEN) TO WS-PARM-TEXT
           END-IF
      *
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
           PERFORM VARYING WS-BAND-SUB FROM 1 BY 1
                     UNTIL WS-BAND-SUB > 4
               MOVE ZERO               TO WS-BAND-CNT(WS-BAND-SUB)
           END-PERFORM
      *
           STRING WS-CURR-DATE(1:4)  '-'
                  WS-CURR-DATE(5:2)  '-'
                  WS-CURR-DATE(7:2)  '-'
                  WS-CURR-HHMMSS(1:2) '.'
                  WS-CURR-HHMMSS(3:2) '.'
                  WS-CURR-HHMMSS(5:2) '.'
                  WS-CURR-HUND '0000'
                  DELIMITED BY SIZE INTO WS-TIMESTAMP
      *
           IF WS-PARM-RESTART
               MOVE 'Y'                TO WS-RESTART-SW
           END-IF
      *
           OPEN I-O CYCLCTL-FILE
           IF WS-CTRL-STATUS NOT = '00'
               MOVE 'CYCLCTL '         TO ER-FILE-NAME
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE WS-CTRL-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           OPEN OUTPUT RSKEXCP-FILE
           IF WS-EXCP-STATUS NOT = '00'
               MOVE 'RSKEXCP '         TO ER-FILE-NAME
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE WS-EXCP-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           DISPLAY 'CBPRT03  PORTFOLIO RESCORE STARTED PARM='
                   WS-PARM-TEXT ' DATE=' WS-CURR-DATE
           .
      *
      ******************************************************************
      * 0200 - CYCLE CONTROL AND RESTART POSITION                      *
      ******************************************************************
       0200-READ-CONTROL.
           MOVE '0200-READ-CONTROL'    TO WS-PARAGRAPH
      *
           MOVE SPACES                 TO CC-KEY
           MOVE 'PARTYWK '             TO CC-CYCLE-TYPE
           MOVE WS-CURR-DATE           TO CC-CYCLE-DATE
      *
           READ CYCLCTL-FILE
           EVALUATE WS-CTRL-STATUS
               WHEN '00'
                   MOVE 'Y'            TO WS-CTRL-FOUND-SW
                   MOVE CYCLCTL-REC    TO CYCLE-CTRL-RECORD
               WHEN '23'
                   CONTINUE
               WHEN OTHER
                   MOVE 'CYCLCTL '     TO ER-FILE-NAME
                   MOVE 'READ    '     TO ER-SQL-OPERATION
                   MOVE WS-CTRL-STATUS TO ER-FILE-STATUS
                   PERFORM 9200-FILE-ERROR
                   PERFORM 9900-ABEND
           END-EVALUATE
      *
           IF WS-CTRL-FOUND AND WS-RESTART-RUN
               PERFORM 0250-RESUME-POSITION
           ELSE
               PERFORM 0260-START-CYCLE
           END-IF
      *
           MOVE CC-COMMIT-FREQ         TO WS-COMMIT-FREQ
           IF WS-COMMIT-FREQ = ZERO
               MOVE WS-COMMIT-FREQUENCY TO WS-COMMIT-FREQ
               MOVE WS-COMMIT-FREQ     TO CC-COMMIT-FREQ
           END-IF
      *
           DISPLAY 'CBPRT03  CYCLE=' CC-CYCLE-TYPE CC-CYCLE-DATE
                   ' STATUS=' CC-STATUS
                   ' RESTART CNT=' CC-RESTART-CNT
           DISPLAY 'CBPRT03  RESUMING AFTER PARTY=' DCL-LAST-PARTY-ID
                   ' COMMIT FREQ=' WS-COMMIT-FREQ
           .
      *
       0250-RESUME-POSITION.
           IF CC-COMPLETE
               DISPLAY 'CBPRT03  *** CYCLE ALREADY COMPLETE FOR '
                       CC-CYCLE-DATE ' - NOTHING TO RESTART'
               MOVE 'Y'                TO WS-EOF-SW
               GO TO 0250-EXIT
           END-IF
      *
           MOVE CC-LAST-KEY(1:11)      TO DCL-LAST-PARTY-ID
           ADD 1                       TO CC-RESTART-CNT
           MOVE 'S'                    TO CC-STATUS
           MOVE WS-STEP-NAME           TO CC-CURRENT-STEP
           MOVE WS-TIMESTAMP           TO CC-START-TS
           PERFORM 0290-REWRITE-CONTROL
      *
      *    THE COUNTS CARRY FORWARD SO THE TOTALS ON THE JOB LOG COVER
      *    THE WHOLE CYCLE AND NOT JUST THE LAST LEG.
           MOVE CC-RECS-READ           TO WS-READ-CNT
           MOVE CC-RECS-WRITTEN        TO WS-SCORED-CNT
           MOVE CC-RECS-REJECTED       TO WS-REVIEW-CNT
           .
       0250-EXIT.
           EXIT
           .
      *
       0260-START-CYCLE.
           MOVE LOW-VALUES             TO DCL-LAST-PARTY-ID
      *
           IF WS-RESTART-RUN AND NOT WS-CTRL-FOUND
               DISPLAY 'CBPRT03  *** RESTART REQUESTED BUT NO '
                       'CONTROL RECORD - STARTING FROM THE TOP'
           END-IF
      *
           IF NOT WS-CTRL-FOUND
               MOVE SPACES             TO CYCLE-CTRL-RECORD
               MOVE 'PARTYWK '         TO CC-CYCLE-TYPE
               MOVE WS-CURR-DATE       TO CC-CYCLE-DATE
               MOVE ZERO               TO CC-RESTART-CNT
           END-IF
      *
           MOVE WS-JOB-NAME            TO CC-CYCLE-ID
           MOVE 'R'                    TO CC-STATUS
           MOVE WS-STEP-NAME           TO CC-CURRENT-STEP
           MOVE SPACES                 TO CC-LAST-GOOD-STEP
           MOVE WS-TIMESTAMP           TO CC-START-TS
           MOVE SPACES                 TO CC-END-TS
           MOVE WS-COMMIT-FREQUENCY    TO CC-COMMIT-FREQ
           MOVE ZERO                   TO CC-RECS-READ
           MOVE ZERO                   TO CC-RECS-WRITTEN
           MOVE ZERO                   TO CC-RECS-REJECTED
           MOVE SPACES                 TO CC-LAST-KEY
           MOVE ZERO                   TO CC-TOTAL-DR-AMT
           MOVE ZERO                   TO CC-TOTAL-CR-AMT
           MOVE ZERO                   TO CC-HASH-TOTAL
           MOVE 'N'                    TO CC-ONLINE-CLOSED-FLG
      *
           IF WS-CTRL-FOUND
               PERFORM 0290-REWRITE-CONTROL
           ELSE
               PERFORM 0280-WRITE-CONTROL
           END-IF
           .
      *
       0280-WRITE-CONTROL.
           WRITE CYCLCTL-REC FROM CYCLE-CTRL-RECORD
           IF WS-CTRL-STATUS NOT = '00'
               MOVE 'CYCLCTL '         TO ER-FILE-NAME
               MOVE 'WRITE   '         TO ER-SQL-OPERATION
               MOVE WS-CTRL-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
           MOVE 'Y'                    TO WS-CTRL-FOUND-SW
           .
      *
       0290-REWRITE-CONTROL.
           REWRITE CYCLCTL-REC FROM CYCLE-CTRL-RECORD
           IF WS-CTRL-STATUS NOT = '00'
               MOVE 'CYCLCTL '         TO ER-FILE-NAME
               MOVE 'REWRITE '         TO ER-SQL-OPERATION
               MOVE WS-CTRL-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
           .
      *
       0300-OPEN-CURSOR.
           MOVE '0300-OPEN-CURSOR'     TO WS-PARAGRAPH
           IF WS-EOF
               GO TO 0300-EXIT
           END-IF
      *
           EXEC SQL
               OPEN PARTYCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'CUSTOMER         ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               PERFORM 9900-ABEND
           END-IF
           .
       0300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - ONE PARTY                                               *
      ******************************************************************
       2000-RESCORE-ONE-PARTY.
           MOVE '2000-RESCORE-ONE-PARTY' TO WS-PARAGRAPH
      *
           PERFORM 2100-FETCH-PARTY
           IF WS-EOF OR WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           ADD 1                       TO WS-READ-CNT
           PERFORM 2200-BUILD-REQUEST
           PERFORM 2300-DRIVE-CHAIN
           PERFORM 2400-HANDLE-RESULT
           PERFORM 2900-CHECKPOINT
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-FETCH-PARTY.
           EXEC SQL
               FETCH PARTYCSR
                INTO :DCL-PARTY-ID
                   , :DCL-CUST-ID
                   , :DCL-PARTY-TYPE
                   , :DCL-SEGMENT-CD :IND-SEGMENT
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'CUSTOMER         ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2200-BUILD-REQUEST.
           MOVE SPACES                 TO CV-RISK-AREA
           MOVE 0003                   TO CV-RISK-VERSION
           MOVE WS-PROGRAM-ID          TO CV-RISK-CALLER-ID
           MOVE WS-MODULE-PARTYRSK     TO CV-RISK-CALLER-MOD
      *
           ADD 1                       TO WS-CORREL-SEQ
           MOVE SPACES                 TO CV-RISK-CORREL-ID
           STRING 'WK' WS-CURR-DATE(3:6) WS-CORREL-SEQ
                  DELIMITED BY SIZE INTO CV-RISK-CORREL-ID
      *
           MOVE WS-CURR-DATE           TO CV-RISK-REQ-DATE
           MOVE WS-CURR-HHMMSS         TO CV-RISK-REQ-TIME
           MOVE 'B'                    TO CV-RISK-CHANNEL
           MOVE 'RCAL'                 TO CV-RISK-REQ-TYPE
      *
           MOVE DCL-PARTY-ID           TO CV-RISK-PARTY-ID
           MOVE DCL-CUST-ID            TO CV-RISK-CUST-ID
           MOVE ZERO                   TO CV-RISK-ACCT-ID
           MOVE SPACES                 TO CV-RISK-CARD-NUM
           MOVE ZERO                   TO CV-RISK-REQ-AMT
           MOVE WS-CURRENCY-USD        TO CV-RISK-REQ-CURR
           MOVE ZERO                   TO CV-RISK-MCC
      *
           MOVE ZERO                   TO CV-RISK-SCORE
           MOVE ZERO                   TO CV-RISK-EXPOSURE-AMT
           MOVE ZERO                   TO CV-RISK-AVAIL-AMT
           MOVE ZERO                   TO CV-RISK-SCORE-DATE
           MOVE ZERO                   TO CV-RISK-RC
           MOVE ZERO                   TO CV-RISK-SQLCODE
           MOVE ZERO                   TO CV-RISK-HOP-CNT
           MOVE 'N'                    TO CV-RISK-SANCTION-FLG
      *
           MOVE ZERO                   TO LK-RETURN-CD
           MOVE SPACES                 TO LK-RETURN-PGM
           MOVE SPACES                 TO LK-RETURN-MSG
           .
      *
      ******************************************************************
      * 2300 - THE RECALCULATION CHAIN                                 *
      *                                                                *
      * THE ENTRY POINT OWNS THE UNIT OF WORK AND TAKES ITS OWN        *
      * COMMITS IN BULK MODE, SO THIS DRIVER DOES NOT COMMIT ROUND     *
      * EVERY PARTY.  IT ONLY RECORDS THE POSITION.                    *
      ******************************************************************
       2300-DRIVE-CHAIN.
           MOVE '2300-DRIVE-CHAIN'     TO WS-PARAGRAPH
      *
           CALL 'PRBRSK1' USING CV-RISK-AREA
                                LK-RETURN-AREA
           .
      *
       2400-HANDLE-RESULT.
           MOVE '2400-HANDLE-RESULT'   TO WS-PARAGRAPH
      *
           IF LK-RETURN-CD > WS-WORST-RC
               MOVE LK-RETURN-CD       TO WS-WORST-RC
           END-IF
      *
           EVALUATE TRUE
               WHEN LK-RETURN-CD = 0
                   ADD 1               TO WS-SCORED-CNT
                   PERFORM 2500-COUNT-BAND
               WHEN LK-RETURN-CD = 4
                   ADD 1               TO WS-SCORED-CNT
                   ADD 1               TO WS-WARN-CNT
                   PERFORM 2500-COUNT-BAND
               WHEN LK-RETURN-CD = 8
      *            SANCTIONS HIT OR REFUSE BAND.  THE PARTY IS SCORED
      *            AND WRITTEN, IT GOES TO MANUAL REVIEW AS WELL.
                   ADD 1               TO WS-SCORED-CNT
                   ADD 1               TO WS-REVIEW-CNT
                   PERFORM 2500-COUNT-BAND
                   PERFORM 2600-WRITE-EXCEPTION
               WHEN OTHER
                   ADD 1               TO WS-FAIL-CNT
                   PERFORM 2600-WRITE-EXCEPTION
                   PERFORM 2700-ASSESS-FAILURE
           END-EVALUATE
           .
      *
       2500-COUNT-BAND.
           EVALUATE CV-RISK-BAND
               WHEN 'A'
                   MOVE 1              TO WS-BAND-SUB
               WHEN 'B'
                   MOVE 2              TO WS-BAND-SUB
               WHEN 'C'
                   MOVE 3              TO WS-BAND-SUB
               WHEN OTHER
                   MOVE 4              TO WS-BAND-SUB
           END-EVALUATE
           ADD 1                       TO WS-BAND-CNT(WS-BAND-SUB)
           .
      *
       2600-WRITE-EXCEPTION.
           MOVE SPACES                 TO WS-EXCP-LINE
           MOVE DCL-PARTY-ID           TO EX-PARTY
           MOVE LK-RETURN-CD           TO EX-RC
           MOVE CV-RISK-BAND           TO EX-BAND
           MOVE CV-RISK-SCORE          TO EX-SCORE
           MOVE CV-RISK-SANCTION-FLG   TO EX-SANCTION
           MOVE CV-RISK-KYC-STATUS     TO EX-KYC
           IF CV-RISK-FAIL-PGM = SPACES
               MOVE LK-RETURN-PGM      TO EX-FAIL-PGM
           ELSE
               MOVE CV-RISK-FAIL-PGM   TO EX-FAIL-PGM
           END-IF
           MOVE CV-RISK-REASON-TXT     TO EX-REASON
      *
           WRITE RSKEXCP-REC FROM WS-EXCP-LINE
           IF WS-EXCP-STATUS NOT = '00'
               MOVE 'RSKEXCP '         TO ER-FILE-NAME
               MOVE 'WRITE   '         TO ER-SQL-OPERATION
               MOVE WS-EXCP-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
           .
      *
      ******************************************************************
      * 2700 - FAILURE ASSESSMENT                                      *
      *                                                                *
      * A HANDFUL OF FATAL RETURNS ACROSS SEVERAL MILLION PARTIES IS   *
      * BAD DATA AND THE RUN CARRIES ON.  A RUN OF THEM MEANS THE      *
      * DATABASE OR THE PARAMETER FILE HAS GONE, AND CARRYING ON       *
      * WOULD ONLY BURN THE BATCH WINDOW.                              *
      ******************************************************************
       2700-ASSESS-FAILURE.
           DISPLAY 'CBPRT03  CHAIN FAILED PARTY=' DCL-PARTY-ID
                   ' RC=' LK-RETURN-CD
                   ' PGM=' CV-RISK-FAIL-PGM
                   ' SQLCODE=' CV-RISK-SQLCODE
      *
           IF WS-FAIL-CNT > 100
               DISPLAY 'CBPRT03  *** FAILURE LIMIT REACHED AFTER '
                       WS-FAIL-CNT ' PARTIES - STOPPING'
               MOVE 'Y'                TO WS-FATAL-SW
           END-IF
           .
      *
      ******************************************************************
      * 2900 - CHECKPOINT                                              *
      *                                                                *
      * COMMIT FIRST, THEN RECORD THE POSITION.  DOING IT THE OTHER    *
      * WAY ROUND WOULD LOSE PARTIES ON A RESTART.                     *
      ******************************************************************
       2900-CHECKPOINT.
           ADD 1                       TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT < WS-COMMIT-FREQ
               GO TO 2900-EXIT
           END-IF
      *
           EXEC SQL
               COMMIT
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'COMMIT           ' TO ER-SQL-TABLE
               MOVE 'COMMIT  '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           MOVE SPACES                 TO CC-LAST-KEY
           MOVE DCL-PARTY-ID           TO CC-LAST-KEY(1:11)
           MOVE WS-READ-CNT            TO CC-RECS-READ
           MOVE WS-SCORED-CNT          TO CC-RECS-WRITTEN
           MOVE WS-REVIEW-CNT          TO CC-RECS-REJECTED
           MOVE WS-STEP-NAME           TO CC-LAST-GOOD-STEP
           PERFORM 0290-REWRITE-CONTROL
      *
           ADD 1                       TO WS-COMMIT-CNT
           MOVE ZERO                   TO WS-SINCE-COMMIT
           MOVE WS-READ-CNT            TO WS-ED-COUNT
           DISPLAY 'CBPRT03  CHECKPOINT ' WS-COMMIT-CNT
                   ' READ=' WS-ED-COUNT
                   ' LAST PARTY=' DCL-PARTY-ID
           .
       2900-EXIT.
           EXIT
           .
      *
       3000-CLOSE-DOWN.
           MOVE '3000-CLOSE-DOWN'      TO WS-PARAGRAPH
      *
           EXEC SQL
               CLOSE PARTYCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'CUSTOMER         ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
      *
           IF WS-FATAL
               EXEC SQL
                   ROLLBACK
               END-EXEC
               MOVE 'F'                TO CC-STATUS
           ELSE
               EXEC SQL
                   COMMIT
               END-EXEC
               MOVE 'C'                TO CC-STATUS
               MOVE SPACES             TO CC-LAST-KEY
           END-IF
      *
           ACCEPT WS-CURR-TIME         FROM TIME
           STRING WS-CURR-DATE(1:4)  '-'
                  WS-CURR-DATE(5:2)  '-'
                  WS-CURR-DATE(7:2)  '-'
                  WS-CURR-HHMMSS(1:2) '.'
                  WS-CURR-HHMMSS(3:2) '.'
                  WS-CURR-HHMMSS(5:2) '.'
                  WS-CURR-HUND '0000'
                  DELIMITED BY SIZE INTO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO CC-END-TS
           MOVE WS-READ-CNT            TO CC-RECS-READ
           MOVE WS-SCORED-CNT          TO CC-RECS-WRITTEN
           MOVE WS-REVIEW-CNT          TO CC-RECS-REJECTED
      *
           IF WS-CTRL-FOUND
               PERFORM 0290-REWRITE-CONTROL
           END-IF
      *
           CLOSE CYCLCTL-FILE
           IF WS-CTRL-STATUS NOT = '00'
               MOVE 'CYCLCTL '         TO ER-FILE-NAME
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE WS-CTRL-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
           END-IF
      *
           CLOSE RSKEXCP-FILE
           IF WS-EXCP-STATUS NOT = '00'
               MOVE 'RSKEXCP '         TO ER-FILE-NAME
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE WS-EXCP-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
           END-IF
           .
      *
       9000-REPORT-TOTALS.
           DISPLAY '******************************************'
           DISPLAY 'CBPRT03  PORTFOLIO RESCORE SUMMARY'
           MOVE WS-READ-CNT            TO WS-ED-COUNT
           DISPLAY '   PARTIES READ       ' WS-ED-COUNT
           MOVE WS-SCORED-CNT          TO WS-ED-COUNT
           DISPLAY '   PARTIES SCORED     ' WS-ED-COUNT
           MOVE WS-WARN-CNT            TO WS-ED-COUNT
           DISPLAY '   WITH WARNINGS      ' WS-ED-COUNT
           MOVE WS-REVIEW-CNT          TO WS-ED-COUNT
           DISPLAY '   MANUAL REVIEW      ' WS-ED-COUNT
           MOVE WS-FAIL-CNT            TO WS-ED-COUNT
           DISPLAY '   FAILED             ' WS-ED-COUNT
           MOVE WS-BAND-CNT(1)         TO WS-ED-COUNT
           DISPLAY '   BAND A             ' WS-ED-COUNT
           MOVE WS-BAND-CNT(2)         TO WS-ED-COUNT
           DISPLAY '   BAND B             ' WS-ED-COUNT
           MOVE WS-BAND-CNT(3)         TO WS-ED-COUNT
           DISPLAY '   BAND C             ' WS-ED-COUNT
           MOVE WS-BAND-CNT(4)         TO WS-ED-COUNT
           DISPLAY '   BAND X             ' WS-ED-COUNT
           DISPLAY '   CHECKPOINTS TAKEN  ' WS-COMMIT-CNT
           DISPLAY '   RESTART COUNT      ' CC-RESTART-CNT
           DISPLAY '******************************************'
      *
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12             TO RETURN-CODE
               WHEN WS-REVIEW-CNT > ZERO
                   MOVE 8              TO RETURN-CODE
               WHEN WS-WARN-CNT > ZERO
                   MOVE 4              TO RETURN-CODE
               WHEN OTHER
                   MOVE 0              TO RETURN-CODE
           END-EVALUATE
           .
      *
       9100-SQL-ERROR.
           MOVE SQLCODE                TO WS-SQL-DISP
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE 'Y'                    TO WS-FATAL-SW
      *
           DISPLAY 'CBPRT03  SQL ERROR PARA=' WS-PARAGRAPH
                   ' TABLE=' ER-SQL-TABLE
           DISPLAY '         OP=' ER-SQL-OPERATION
                   ' SQLCODE=' WS-SQL-DISP
                   ' PARTY=' DCL-PARTY-ID
           DISPLAY '         SQLERRMC=' SQLERRMC(1:44)
           .
      *
       9200-FILE-ERROR.
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE CC-KEY                 TO ER-VSAM-KEY
           DISPLAY 'CBPRT03  FILE ERROR ' ER-FILE-NAME
                   ' OP=' ER-SQL-OPERATION
                   ' STATUS=' ER-FILE-STATUS
                   ' KEY=' CC-KEY
           .
      *
      ******************************************************************
      * 9900 - U3131.  THE OPERATOR RESTARTS WITH PARM='RESTART' ONCE  *
      *        THE UNDERLYING PROBLEM IS CLEARED.                      *
      ******************************************************************
       9900-ABEND.
           MOVE 'U313'                 TO ER-ABEND-CODE
           MOVE 'Y'                    TO ER-ABEND-REQUESTED
           EXEC SQL
               ROLLBACK
           END-EXEC
           DISPLAY 'CBPRT03  ABEND U3131 PARA=' WS-PARAGRAPH
                   ' LAST PARTY=' DCL-PARTY-ID
           DISPLAY 'CBPRT03  RESTART WITH PARM=RESTART AFTER PARTY '
                   CC-LAST-KEY(1:11)
           MOVE 12                     TO RETURN-CODE
           STOP RUN
           .
