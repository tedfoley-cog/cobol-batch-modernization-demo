      ******************************************************************
      * CBCRD01 - NIGHTLY AUTHORISATION LOG EXTRACT                    *
      *                                                                *
      * STEP 1 OF THE CARDNITE CYCLE.  JOB CBCRD01J.                   *
      *                                                                *
      * READS THE AUTHORISATION LOG (VSAM ESDS, WRITTEN ALL DAY BY     *
      * THE ONLINE REGION) FROM THE START AND SELECTS EVERY RECORD     *
      * BELONGING TO THE CYCLE DATE SUPPLIED ON THE PARM.  SELECTED    *
      * RECORDS ARE WRITTEN TO THE EXTRACT GENERATION DATA SET.        *
      *                                                                *
      * THE CYCLE CONTROL RECORD ON CYCLCTL IS CREATED OR RESET FOR    *
      * THE CYCLE DATE.  EVERY LATER STEP OF THE CHAIN UPDATES IT.     *
      *                                                                *
      * PARM  - CCYYMMDD CYCLE DATE, OPTIONALLY FOLLOWED BY A COMMA    *
      *         AND THE EIGHT CHARACTER CYCLE ID.                      *
      *                                                                *
      * CALLED BY   - JCL ONLY                                         *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - AUTHLOG  VSAM ESDS  INPUT  SEQUENTIAL            *
      *             - AUTHEXTR QSAM       OUTPUT LRECL 300             *
      *             - CYCLCTL  VSAM KSDS  I-O                          *
      *                                                                *
      * RETURN CODE - 0000 EXTRACT COMPLETE                            *
      *               0004 EXTRACT COMPLETE, NO RECORDS SELECTED       *
      *               0012 FATAL - SEE U0101 / U0102                   *
      * USER ABEND  - U0101 CYCLE CONTROL RECORD UNREADABLE            *
      *               U0102 AUTHLOG OPEN FAILED                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD01.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1998-03-11.
      *
      * MAINTENANCE
      * 1998-03-11 CRD0114 ORIGINAL - NIGHTLY EXTRACT
      * 2001-09-04 CRD2288 CYCLE CONTROL RECORD ADDED
      * 2007-06-19 CRD5510 SIX DIGIT ACTIVATION DATE WINDOWING
      * 2014-11-02 CRD8842 HASH TOTAL CARRIED FORWARD TO CBCRD10
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS TOP-OF-PAGE.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT AUTHLOG-FILE  ASSIGN TO AUTHLOG
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS WS-AUTHLOG-STATUS.
      *
           SELECT EXTRACT-FILE  ASSIGN TO AUTHEXTR
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-EXTRACT-STATUS.
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
       FD  AUTHLOG-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  AUTHLOG-REC                     PIC X(200).
      *
       FD  EXTRACT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       01  EXTRACT-REC                     PIC X(300).
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD01 '.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
      *    FILE STATUS FIELDS
       01  WS-STATUS-FIELDS.
           05  WS-AUTHLOG-STATUS           PIC X(2)  VALUE '00'.
               88  WS-AUTHLOG-OK                     VALUE '00'.
               88  WS-AUTHLOG-EOF                    VALUE '10'.
           05  WS-EXTRACT-STATUS           PIC X(2)  VALUE '00'.
               88  WS-EXTRACT-OK                     VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
               88  WS-CYCLCTL-NOTFND                 VALUE '23'.
               88  WS-CYCLCTL-DUP                    VALUE '22'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-SELECT-SW                PIC X     VALUE 'N'.
               88  WS-SELECTED                       VALUE 'Y'.
           05  WS-CTL-EXISTS-SW            PIC X     VALUE 'N'.
               88  WS-CTL-EXISTS                     VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-WRITTEN-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-SKIP-DATE-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-SKIP-POSTED-CNT          PIC 9(9)  VALUE ZERO.
           05  WS-SKIP-STATUS-CNT          PIC 9(9)  VALUE ZERO.
           05  WS-PURCHASE-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-CASH-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-REFUND-CNT               PIC 9(9)  VALUE ZERO.
      *
       01  WS-TOTALS.
           05  WS-HASH-TOTAL               PIC S9(15) COMP-3
                                                     VALUE ZERO.
           05  WS-AMT-TOTAL                PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-WORK-AMT                 PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
      *
      *    PARM AREA - CCYYMMDD[,CYCLEID]
       01  WS-PARM-DATE                    PIC 9(8)  VALUE ZERO.
       01  WS-CYCLE-ID                     PIC X(8)  VALUE SPACES.
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYCLE-DATE-R REDEFINES WS-CYCLE-DATE.
           05  WS-CYCLE-CC                 PIC 9(2).
           05  WS-CYCLE-YY                 PIC 9(2).
           05  WS-CYCLE-MM                 PIC 9(2).
           05  WS-CYCLE-DD                 PIC 9(2).
      *
       01  WS-CURRENT-DATE.
           05  WS-CD-DATE                  PIC 9(8).
           05  WS-CD-TIME                  PIC 9(8).
           05  WS-CD-FILLER                PIC X(5).
      *
       01  WS-TIMESTAMP                    PIC X(26) VALUE SPACES.
      *
       01  WS-RETURN-CODE                  PIC 9(4)  VALUE ZERO.
       01  WS-ABEND-CODE                   PIC 9(4)  VALUE ZERO.
      *
      *    AUTHORISATION IMAGE AND ITS VARIANT OVERLAYS
           COPY CVAUTH01Y.
      *
      *    EXTRACT RECORD
           COPY CVAXTR01Y.
      *
      *    CYCLE CONTROL RECORD
           COPY CVCTRL01Y.
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
       01  WS-DISPLAY-AMT                  PIC ---,---,---,--9.99.
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
           PERFORM 2000-PROCESS-AUTHLOG
               UNTIL WS-EOF
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
           PERFORM 1100-EDIT-PARM
           PERFORM 1200-OPEN-FILES
           PERFORM 1300-START-CYCLE-CONTROL
      *
           DISPLAY 'CBCRD01 - CARDNITE AUTHORISATION EXTRACT'
           DISPLAY '          CYCLE DATE  ' WS-CYCLE-DATE
           DISPLAY '          CYCLE ID    ' WS-CYCLE-ID
           DISPLAY '          STARTED AT  ' WS-TIMESTAMP
           .
      *
       1100-EDIT-PARM.
           IF LK-PARM-LEN < 8
               MOVE 'PARM MUST SUPPLY CCYYMMDD CYCLE DATE'
                                           TO ER-MESSAGE
               MOVE 0101                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE LK-PARM-DATA(1:8)          TO WS-PARM-DATE
           MOVE WS-PARM-DATE               TO WS-CYCLE-DATE
      *
           IF WS-CYCLE-MM < 01 OR WS-CYCLE-MM > 12
           OR WS-CYCLE-DD < 01 OR WS-CYCLE-DD > 31
               MOVE 'CYCLE DATE ON PARM IS NOT A VALID DATE'
                                           TO ER-MESSAGE
               MOVE 0101                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF LK-PARM-LEN > 9
               MOVE LK-PARM-DATA(10:8)     TO WS-CYCLE-ID
           ELSE
               STRING 'N' WS-CYCLE-DATE(3:6)
                 DELIMITED BY SIZE INTO WS-CYCLE-ID
               END-STRING
           END-IF
           .
      *
       1200-OPEN-FILES.
           OPEN INPUT  AUTHLOG-FILE
           IF NOT WS-AUTHLOG-OK
               MOVE 'AUTHLOG'              TO ER-FILE-NAME
               MOVE WS-AUTHLOG-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF AUTHLOG FAILED'
                                           TO ER-MESSAGE
               MOVE 0102                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           OPEN OUTPUT EXTRACT-FILE
           IF NOT WS-EXTRACT-OK
               MOVE 'AUTHEXTR'             TO ER-FILE-NAME
               MOVE WS-EXTRACT-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF EXTRACT GDG FAILED'
                                           TO ER-MESSAGE
               MOVE 0102                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           OPEN I-O    CYCLCTL-FILE
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF CYCLE CONTROL FAILED'
                                           TO ER-MESSAGE
               MOVE 0101                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 1300 - CREATE OR RESET THE CYCLE CONTROL RECORD.               *
      *        A RECORD ALREADY PRESENT FOR THE CYCLE DATE MEANS THE   *
      *        CHAIN IS BEING RE-RUN - THE COUNTS ARE ZEROISED AND     *
      *        THE RESTART COUNTER IS BUMPED.                          *
      ******************************************************************
       1300-START-CYCLE-CONTROL.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
      *
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'N'                TO WS-CTL-EXISTS-SW
               NOT INVALID KEY
                   MOVE 'Y'                TO WS-CTL-EXISTS-SW
           END-READ
      *
           IF NOT WS-CTL-EXISTS
           AND NOT WS-CYCLCTL-NOTFND
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'READ OF CYCLE CONTROL FAILED'
                                           TO ER-MESSAGE
               MOVE 0101                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF WS-CTL-EXISTS
               ADD 1                       TO CC-RESTART-CNT
               MOVE 'S'                    TO CC-STATUS
               DISPLAY 'CBCRD01 - CYCLE CONTROL EXISTS, RESTART NBR '
                       CC-RESTART-CNT
           ELSE
               INITIALIZE CYCLE-CTRL-RECORD
               MOVE 'CARDNITE'             TO CC-CYCLE-TYPE
               MOVE WS-CYCLE-DATE          TO CC-CYCLE-DATE
               MOVE ZERO                   TO CC-RESTART-CNT
               MOVE 'R'                    TO CC-STATUS
           END-IF
      *
           MOVE WS-CYCLE-ID                TO CC-CYCLE-ID
           MOVE WS-STEP-NAME               TO CC-CURRENT-STEP
           MOVE SPACES                     TO CC-LAST-GOOD-STEP
           MOVE WS-TIMESTAMP               TO CC-START-TS
           MOVE SPACES                     TO CC-END-TS
           MOVE WS-COMMIT-FREQUENCY        TO CC-COMMIT-FREQ
           MOVE ZERO                       TO CC-RECS-READ
                                              CC-RECS-WRITTEN
                                              CC-RECS-REJECTED
                                              CC-TOTAL-DR-AMT
                                              CC-TOTAL-CR-AMT
                                              CC-HASH-TOTAL
           MOVE SPACES                     TO CC-LAST-KEY
           MOVE 'Y'                        TO CC-ONLINE-CLOSED-FLG
      *
           PERFORM 8100-WRITE-CYCLE-CONTROL
           .
      *
      ******************************************************************
      * 2000 - MAIN EXTRACT LOOP                                       *
      ******************************************************************
       2000-PROCESS-AUTHLOG.
           READ AUTHLOG-FILE INTO AUTH-RECORD
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
                   GO TO 2000-EXIT
           END-READ
      *
           IF NOT WS-AUTHLOG-OK
           AND NOT WS-AUTHLOG-EOF
               MOVE 'AUTHLOG '             TO ER-FILE-NAME
               MOVE WS-AUTHLOG-STATUS      TO ER-FILE-STATUS
               MOVE 'SEQUENTIAL READ OF AUTHLOG FAILED'
                                           TO ER-MESSAGE
               MOVE 0102                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           ADD 1                           TO WS-READ-CNT
           PERFORM 2100-SELECT-RECORD
      *
           IF WS-SELECTED
               PERFORM 2200-BUILD-EXTRACT
               PERFORM 2300-WRITE-EXTRACT
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2100 - SELECTION.  ONLY APPROVED OR REVERSED AUTHORISATIONS    *
      *        FOR THE CYCLE DATE THAT HAVE NOT ALREADY BEEN POSTED    *
      *        TAKE PART IN THE CYCLE.  DECLINES STAY ON THE LOG FOR   *
      *        THE FRAUD FEED AND ARE COUNTED ONLY.                    *
      ******************************************************************
       2100-SELECT-RECORD.
           MOVE 'N'                        TO WS-SELECT-SW
      *
           IF AUTH-DATE NOT = WS-CYCLE-DATE
               ADD 1                       TO WS-SKIP-DATE-CNT
               GO TO 2100-EXIT
           END-IF
      *
           IF AUTH-POSTED-FLG = 'Y'
               ADD 1                       TO WS-SKIP-POSTED-CNT
               GO TO 2100-EXIT
           END-IF
      *
           IF NOT AUTH-APPROVED
           AND NOT AUTH-REVERSED
               ADD 1                       TO WS-SKIP-STATUS-CNT
               GO TO 2100-EXIT
           END-IF
      *
           MOVE 'Y'                        TO WS-SELECT-SW
      *
      *    THE DETAIL AREA IS ONLY MEANINGFUL UNDER THE TYPE.  THE
      *    AMOUNT IS PICKED UP HERE FOR THE CONTROL TOTAL ONLY - THE
      *    FULL EDIT IS CBCRD02 WORK.
           EVALUATE TRUE
               WHEN AUTH-PURCHASE-TYPE
                   ADD 1                   TO WS-PURCHASE-CNT
                   MOVE AP-AMOUNT          TO WS-WORK-AMT
               WHEN AUTH-CASH-ADV-TYPE
                   ADD 1                   TO WS-CASH-CNT
                   MOVE AC-AMOUNT          TO WS-WORK-AMT
               WHEN AUTH-REFUND-TYPE
                   ADD 1                   TO WS-REFUND-CNT
                   COMPUTE WS-WORK-AMT = AR-AMOUNT * -1
               WHEN OTHER
      *            UNKNOWN TYPES ARE STILL EXTRACTED - CBCRD02 OWNS
      *            THE REJECT DECISION AND THE REASON CODE.
                   MOVE ZERO               TO WS-WORK-AMT
           END-EVALUATE
      *
           ADD WS-WORK-AMT                 TO WS-AMT-TOTAL
           ADD AUTH-SEQ-NUM                TO WS-HASH-TOTAL
           .
       2100-EXIT.
           EXIT
           .
      *
       2200-BUILD-EXTRACT.
           INITIALIZE AUTH-EXTRACT-REC
           MOVE WS-CYCLE-DATE              TO AX-CYCLE-DATE
           MOVE WS-CYCLE-ID                TO AX-CYCLE-ID
           ADD 1                           TO WS-WRITTEN-CNT
           MOVE WS-WRITTEN-CNT             TO AX-EXTRACT-SEQ
           MOVE WS-READ-CNT                TO AX-SOURCE-RBA
           MOVE AUTH-RECORD                TO AX-AUTH-IMAGE
           MOVE SPACE                      TO AX-EDIT-STATUS
           MOVE SPACES                     TO AX-EDIT-REASON
           MOVE ZERO                       TO AX-MCC
           MOVE SPACES                     TO AX-ACQUIRER-ID
                                              AX-SETTLE-ROUTE
                                              AX-MERCH-NAME
           MOVE 'N'                        TO AX-HIGH-RISK-FLG
           MOVE SPACE                      TO AX-ENRICH-STATUS
           .
      *
       2300-WRITE-EXTRACT.
           WRITE EXTRACT-REC FROM AUTH-EXTRACT-REC
           IF NOT WS-EXTRACT-OK
               MOVE 'AUTHEXTR'             TO ER-FILE-NAME
               MOVE WS-EXTRACT-STATUS      TO ER-FILE-STATUS
               MOVE 'WRITE TO EXTRACT GDG FAILED'
                                           TO ER-MESSAGE
               MOVE 0102                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 3000 - TERMINATION                                             *
      ******************************************************************
       3000-TERMINATE.
           MOVE WS-READ-CNT                TO CC-RECS-READ
           MOVE WS-WRITTEN-CNT             TO CC-RECS-WRITTEN
           MOVE ZERO                       TO CC-RECS-REJECTED
           MOVE WS-HASH-TOTAL              TO CC-HASH-TOTAL
           MOVE WS-AMT-TOTAL               TO CC-TOTAL-DR-AMT
           MOVE WS-STEP-NAME               TO CC-LAST-GOOD-STEP
           PERFORM 8100-WRITE-CYCLE-CONTROL
      *
           CLOSE AUTHLOG-FILE
                 EXTRACT-FILE
                 CYCLCTL-FILE
      *
           PERFORM 3100-PRINT-CONTROLS
      *
           IF WS-WRITTEN-CNT = ZERO
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
               DISPLAY 'CBCRD01 - WARNING, NO RECORDS SELECTED'
           END-IF
           .
      *
       3100-PRINT-CONTROLS.
           DISPLAY '----------------------------------------------'
           MOVE WS-READ-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD01 RECORDS READ        ' WS-DISPLAY-CNT
           MOVE WS-WRITTEN-CNT             TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD01 RECORDS EXTRACTED   ' WS-DISPLAY-CNT
           MOVE WS-SKIP-DATE-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD01 SKIPPED OTHER DATE  ' WS-DISPLAY-CNT
           MOVE WS-SKIP-POSTED-CNT         TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD01 SKIPPED POSTED      ' WS-DISPLAY-CNT
           MOVE WS-SKIP-STATUS-CNT         TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD01 SKIPPED NOT APPROVED' WS-DISPLAY-CNT
           MOVE WS-PURCHASE-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD01 PURCHASES           ' WS-DISPLAY-CNT
           MOVE WS-CASH-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD01 CASH ADVANCES       ' WS-DISPLAY-CNT
           MOVE WS-REFUND-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD01 REFUNDS             ' WS-DISPLAY-CNT
           MOVE WS-AMT-TOTAL               TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD01 AMOUNT TOTAL        ' WS-DISPLAY-AMT
           DISPLAY 'CBCRD01 HASH TOTAL          ' WS-HASH-TOTAL
           DISPLAY '----------------------------------------------'
           .
      *
      ******************************************************************
      * 8100 - WRITE OR REWRITE THE CYCLE CONTROL RECORD               *
      ******************************************************************
       8100-WRITE-CYCLE-CONTROL.
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
      *
           IF WS-CTL-EXISTS
               REWRITE CYCLCTL-REC
               IF NOT WS-CYCLCTL-OK
                   MOVE 'CYCLCTL '         TO ER-FILE-NAME
                   MOVE WS-CYCLCTL-STATUS  TO ER-FILE-STATUS
                   MOVE 'REWRITE OF CYCLE CONTROL FAILED'
                                           TO ER-MESSAGE
                   MOVE 0101               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
               END-IF
           ELSE
               WRITE CYCLCTL-REC
               IF WS-CYCLCTL-OK
                   MOVE 'Y'                TO WS-CTL-EXISTS-SW
               ELSE
                   MOVE 'CYCLCTL '         TO ER-FILE-NAME
                   MOVE WS-CYCLCTL-STATUS  TO ER-FILE-STATUS
                   MOVE 'WRITE OF CYCLE CONTROL FAILED'
                                           TO ER-MESSAGE
                   MOVE 0101               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 9500 - FATAL.  POPULATE THE ERROR AREA, HAND IT TO THE BATCH   *
      *        ERROR HANDLER AND ABEND WITH THE USER CODE.             *
      ******************************************************************
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'VSAM'                     TO ER-ERROR-TYPE
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
      *
           DISPLAY 'CBCRD01 FATAL ' ER-MESSAGE
           DISPLAY '        FILE=' ER-FILE-NAME
                   ' STATUS=' ER-FILE-STATUS
                   ' ABEND=U' WS-ABEND-CODE
      *
           CALL 'CBCRD91' USING ERROR-AREA
      *
           MOVE WS-RC-FATAL                TO WS-RETURN-CODE
           MOVE WS-RETURN-CODE             TO RETURN-CODE
           GOBACK
           .
