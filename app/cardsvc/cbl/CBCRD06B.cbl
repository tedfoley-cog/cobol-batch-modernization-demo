      ******************************************************************
      * CBCRD06B - DISPATCH THE PARTY EXPOSURE RECALCULATION           *
      *                                                                *
      * STEP030 OF JOB CBCRD06J.                                       *
      *                                                                *
      * FOR EVERY PARTY ON THE SORTED WORK LIST THIS PROGRAM FILLS IN  *
      * THE RISK COMMUNICATION AREA AND HANDS IT TO THE BATCH          *
      * DISPATCHER UNDER ROUTE TYPE XMOD, ROUTE KEY RSKRECAL.  THE     *
      * DISPATCHER RESOLVES THE ROUTE FROM CARDSVC.PGM_ROUTE AND       *
      * ISSUES THE DYNAMIC CALL.  THIS PROGRAM DELIBERATELY DOES NOT   *
      * KNOW THE NAME OF THE ANSWERING LOAD MODULE - THE ROUTE TABLE   *
      * OWNS IT AND THE STEPLIB IN THE JCL MAKES IT REACHABLE.         *
      *                                                                *
      * COMPILE WITH DYNAM.                                            *
      *                                                                *
      * PER PARTY RETURN CODE HANDLING                                 *
      *   0000  ACCEPT THE RETURNED EXPOSURE AND BAND                  *
      *   0004  ACCEPT, COUNT AN EXCEPTION AND CARRY THE WARNING       *
      *   0008  SERIOUS CONDITION.  THE PARTY IS WRITTEN TO THE        *
      *         MANUAL REVIEW FILE AND EXCLUDED FROM THE ACCEPTED      *
      *         FILE SO STEP040 NEVER APPLIES IT                       *
      *   0012  FATAL.  THE STEP STOPS WITH A USER ABEND               *
      *                                                                *
      * THE STEP RETURN CODE IS THE WORST SEEN, SO STEP040 CAN BE      *
      * GATED WITH IF RC <= 4 IN THE JCL.                              *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD90 (BATCH DISPATCHER, ROUTE TYPE XMOD)      *
      *             - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - PARTYSRT QSAM INPUT   LRECL 150                  *
      *             - PARTYACC QSAM OUTPUT  LRECL 150 ACCEPTED         *
      *             - PARTYRVW QSAM OUTPUT  LRECL 150 MANUAL REVIEW    *
      *             - RISKEXC  QSAM OUTPUT  LRECL 133 EXCEPTION LIST   *
      * TABLES      - CARDSVC.ROUTE_AUDIT   (INSERT)                   *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 ALL PARTIES ACCEPTED                        *
      *               0004 WARNINGS PRESENT, STEP040 STILL RUNS        *
      *               0008 ONE OR MORE PARTIES FOR MANUAL REVIEW       *
      *               0012 FATAL                                       *
      * USER ABEND  - U0602 FILE OPEN OR I/O FAILURE                   *
      *               U0605 DISPATCHER COULD NOT RESOLVE THE ROUTE     *
      *               U0606 CROSSING RETURNED A FATAL CONDITION        *
      *               U0607 COMMUNICATION AREA VERSION MISMATCH        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD06B.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  2001-04-24.
      *
      * MAINTENANCE
      * 2001-04-24 CRD2111 ORIGINAL - CALL WAS STATIC
      * 2005-02-28 CRD4501 STATIC CALL REPLACED BY A DISPATCHED CALL
      *                    SO THE TARGET CAN BE SWITCHED WITHOUT A
      *                    RECOMPILE OF THIS MODULE
      * 2012-03-19 CRD7811 MANUAL REVIEW FILE ADDED FOR RC 8
      * 2017-06-02 CRD9601 COMMAREA VERSION CHECKED ON RETURN
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORTED-FILE   ASSIGN TO PARTYSRT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-SORTED-STATUS.
      *
           SELECT ACCEPT-FILE   ASSIGN TO PARTYACC
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-ACCEPT-STATUS.
      *
           SELECT REVIEW-FILE   ASSIGN TO PARTYRVW
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REVIEW-STATUS.
      *
           SELECT EXCEPT-FILE   ASSIGN TO RISKEXC
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-EXCEPT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  SORTED-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       01  SORTED-REC                      PIC X(150).
      *
       FD  ACCEPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       01  ACCEPT-REC                      PIC X(150).
      *
       FD  REVIEW-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       01  REVIEW-REC                      PIC X(150).
      *
       FD  EXCEPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  EXCEPT-REC                      PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD06B'.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP030 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-SORTED-STATUS            PIC X(2)  VALUE '00'.
               88  WS-SORTED-OK                      VALUE '00'.
           05  WS-ACCEPT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-ACCEPT-OK                      VALUE '00'.
           05  WS-REVIEW-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REVIEW-OK                      VALUE '00'.
           05  WS-EXCEPT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-EXCEPT-OK                      VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-DISPATCH-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-ACCEPT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-WARN-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-REVIEW-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SANCTION-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-BAND-WORSE-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-AUDIT-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CYCLE-ID                     PIC X(8)  VALUE SPACES.
      *
       01  WS-CORREL-ID                    PIC X(16) VALUE SPACES.
       01  WS-CORREL-ID-R REDEFINES WS-CORREL-ID.
           05  WS-CI-PREFIX                PIC X(4).
           05  WS-CI-DATE                  PIC 9(6).
           05  WS-CI-SEQ                   PIC 9(6).
       01  WS-CORREL-SEQ                   PIC 9(6)  VALUE ZERO.
       01  WS-JOB-NAME                     PIC X(8)  VALUE 'CBCRD06J'.
       01  WS-MAX-REQ-AMT                  PIC S9(9)V99 COMP-3
                                                     VALUE 999999999.99.
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
       01  WS-BAND-RANK-INPUT              PIC X     VALUE SPACE.
       01  WS-BAND-RANK-OLD                PIC 9     VALUE ZERO.
       01  WS-BAND-RANK-NEW                PIC 9     VALUE ZERO.
       01  WS-BAND-RANK                    PIC 9     VALUE ZERO.
      *
       01  WS-RETURN-AREA.
           05  WS-RETURN-CD                PIC S9(4) COMP VALUE ZERO.
           05  WS-RETURN-PGM               PIC X(8)  VALUE SPACES.
           05  WS-RETURN-MSG               PIC X(60) VALUE SPACES.
      *
       01  WS-EXCEPT-LINE.
           05  EL-PARTY-ID                 PIC X(11).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  EL-ACCT-ID                  PIC 9(11).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  EL-RC                       PIC 9(4).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  EL-BAND-OLD                 PIC X.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  EL-BAND-NEW                 PIC X.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  EL-SANCTION                 PIC X.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  EL-EXPOSURE                 PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  EL-REASON                   PIC X(60).
           05  FILLER                      PIC X(12) VALUE SPACES.
      *
           COPY CVPWRK01Y.
           COPY CVRISK01Y.
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-AUDIT.
           05  DCL-CORREL-ID               PIC X(16).
           05  DCL-CALLER-PGM              PIC X(8).
           05  DCL-ROUTE-TYPE              PIC X(4).
           05  DCL-ROUTE-KEY               PIC X(8).
           05  DCL-SEQ-NBR                 PIC S9(4) COMP.
           05  DCL-RESOLVED-PGM            PIC X(8).
           05  DCL-RESOLVED-MOD            PIC X(8).
           05  DCL-CALL-TYPE               PIC X(1).
           05  DCL-USED-FALLBACK           PIC X(1).
           05  DCL-RC                      PIC S9(4) COMP.
           05  DCL-JOB-NAME                PIC X(8).
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
           PERFORM 2000-DISPATCH-PARTY
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
      *
           MOVE 'RCAL'                     TO WS-CI-PREFIX
           MOVE WS-CYCLE-DATE(3:6)         TO WS-CI-DATE
      *
           OPEN INPUT  SORTED-FILE
           OPEN OUTPUT ACCEPT-FILE
           OPEN OUTPUT REVIEW-FILE
           OPEN OUTPUT EXCEPT-FILE
      *
           IF NOT WS-SORTED-OK
               MOVE 'PARTYSRT'             TO ER-FILE-NAME
               MOVE WS-SORTED-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF SORTED WORK LIST FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF NOT WS-ACCEPT-OK OR NOT WS-REVIEW-OK
               MOVE 'PARTYACC'             TO ER-FILE-NAME
               MOVE WS-ACCEPT-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF OUTCOME FILES FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           DISPLAY 'CBCRD06B - EXPOSURE RECALCULATION DISPATCH'
           DISPLAY 'CBCRD06B - CYCLE ' WS-CYCLE-DATE
           PERFORM 1900-READ-PARTY
           .
      *
       1900-READ-PARTY.
           READ SORTED-FILE INTO PARTY-WORK-REC
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
               NOT AT END
                   ADD 1                   TO WS-READ-CNT
                   MOVE PW-CYCLE-ID        TO WS-CYCLE-ID
           END-READ
           .
      *
      ******************************************************************
      * 2000 - ONE PARTY                                               *
      ******************************************************************
       2000-DISPATCH-PARTY.
           PERFORM 2100-BUILD-RISK-AREA
           PERFORM 2200-CALL-DISPATCHER
           PERFORM 2300-CHECK-ROUTE-OUTCOME
           PERFORM 2400-HANDLE-PARTY-RC
           PERFORM 2800-AUDIT-CROSSING
      *
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQUENCY
               EXEC SQL COMMIT WORK END-EXEC
               MOVE ZERO                   TO WS-SINCE-COMMIT
           END-IF
      *
           PERFORM 1900-READ-PARTY
           .
      *
      ******************************************************************
      * 2100 - POPULATE THE SHARED 512 BYTE AREA.  IT IS CLEARED FOR   *
      *        EVERY PARTY - THE ANSWERING SIDE FILLS IN THE OUTPUT    *
      *        SECTION AND APPENDS ITSELF TO THE HOP TRACE.            *
      ******************************************************************
       2100-BUILD-RISK-AREA.
           INITIALIZE CV-RISK-AREA
      *
           MOVE 0003                       TO CV-RISK-VERSION
           MOVE WS-PROGRAM-ID              TO CV-RISK-CALLER-ID
           MOVE WS-MODULE-CARDSVC          TO CV-RISK-CALLER-MOD
      *
           ADD 1                           TO WS-CORREL-SEQ
           MOVE WS-CORREL-SEQ              TO WS-CI-SEQ
           MOVE WS-CORREL-ID               TO CV-RISK-CORREL-ID
      *
           MOVE WS-CD-DATE                 TO CV-RISK-REQ-DATE
           MOVE WS-CD-TIME(1:6)            TO CV-RISK-REQ-TIME
           MOVE 'B'                        TO CV-RISK-CHANNEL
      *
           MOVE PW-PARTY-ID                TO CV-RISK-PARTY-ID
           MOVE PW-CUST-ID                 TO CV-RISK-CUST-ID
           MOVE PW-ACCT-ID                 TO CV-RISK-ACCT-ID
           MOVE PW-CARD-NUM                TO CV-RISK-CARD-NUM
      *
      *    THE REQUEST AMOUNT IS ONLY S9(9)V99 ON THE SHARED AREA.
      *    A CORPORATE PARTY CAN CARRY MORE THAN THAT, SO IT IS
      *    CAPPED AND REPORTED RATHER THAN TRUNCATED SILENTLY.
           IF PW-EXPOSURE-AMT > WS-MAX-REQ-AMT
               MOVE WS-MAX-REQ-AMT         TO CV-RISK-REQ-AMT
               DISPLAY 'CBCRD06B - EXPOSURE CAPPED FOR PARTY '
                       PW-PARTY-ID
           ELSE
               MOVE PW-EXPOSURE-AMT        TO CV-RISK-REQ-AMT
           END-IF
           MOVE WS-CURRENCY-USD            TO CV-RISK-REQ-CURR
           MOVE WS-COUNTRY-USA             TO CV-RISK-COUNTRY
           MOVE 'RCAL'                     TO CV-RISK-REQ-TYPE
      *
           MOVE ZERO                       TO CV-RISK-RC
           MOVE ZERO                       TO CV-RISK-HOP-CNT
      *
           ADD 1                           TO CV-RISK-HOP-CNT
           MOVE WS-PROGRAM-ID          TO CV-RISK-HOP-PGM
                                          (CV-RISK-HOP-CNT)
           MOVE CV-RISK-RC             TO CV-RISK-HOP-RC
                                          (CV-RISK-HOP-CNT)
           .
      *
      ******************************************************************
      * 2200 - THE CROSSING.  THE ROUTE KEY IS THE ONLY THING THIS     *
      *        PROGRAM KNOWS ABOUT THE ANSWERING SERVICE.              *
      ******************************************************************
       2200-CALL-DISPATCHER.
           MOVE SPACES                     TO ROUTE-REQUEST
           MOVE 'XMOD'                     TO RQ-ROUTE-TYPE
           MOVE 'RSKRECAL'                 TO RQ-ROUTE-KEY
           MOVE 1                          TO RQ-SEQ-NBR
           MOVE ZERO                       TO RQ-RC
      *
           MOVE ZERO                       TO WS-RETURN-CD
           MOVE SPACES                     TO WS-RETURN-PGM
                                              WS-RETURN-MSG
      *
           CALL 'CBCRD90' USING ROUTE-REQUEST
                                CV-RISK-AREA
                                WS-RETURN-AREA
      *
           ADD 1                           TO WS-DISPATCH-CNT
                                              WS-SINCE-COMMIT
           .
      *
       2300-CHECK-ROUTE-OUTCOME.
           IF RQ-RC-NOT-FOUND OR RQ-RC-TABLE-ERROR
               MOVE 'ROUT'                 TO ER-ERROR-TYPE
               MOVE 'ROUTE XMOD/RSKRECAL COULD NOT BE RESOLVED'
                                           TO ER-MESSAGE
               MOVE 0605                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF CV-RISK-VER-OLD
               MOVE 'COMMAREA VERSION RETURNED IS DOWN LEVEL'
                                           TO ER-MESSAGE
               MOVE 0607                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
      *    THE ANSWERING SIDE SETS ITS OWN RETURN CODE IN THE SHARED
      *    AREA.  THE DISPATCHER RETURN AREA IS ONLY A TRANSPORT
      *    FAILURE INDICATOR, SO THE HIGHER OF THE TWO IS TAKEN.
           IF WS-RETURN-CD > CV-RISK-RC
               MOVE WS-RETURN-CD           TO CV-RISK-RC
           END-IF
           .
      *
      ******************************************************************
      * 2400 - PER PARTY OUTCOME                                       *
      ******************************************************************
       2400-HANDLE-PARTY-RC.
           MOVE CV-RISK-RC                 TO PW-RC
           MOVE CV-RISK-REASON-CD          TO PW-REASON-CD
           MOVE CV-RISK-ADVICE-CD          TO PW-ADVICE-CD
           MOVE CV-RISK-SANCTION-FLG       TO PW-SANCTION-FLG
           MOVE CV-RISK-SCORE              TO PW-SCORE
           MOVE WS-TIMESTAMP               TO PW-DISPATCH-TS
      *
           IF CV-RISK-BAND NOT = SPACES
               MOVE CV-RISK-BAND           TO PW-RISK-BAND-NEW
           END-IF
           IF CV-RISK-EXPOSURE-AMT NOT = ZERO
               MOVE CV-RISK-EXPOSURE-AMT   TO PW-EXPOSURE-AMT
           END-IF
      *
           PERFORM 2500-COMPARE-BANDS
      *
           EVALUATE TRUE
               WHEN CV-RISK-RC-OK
                   PERFORM 2600-WRITE-ACCEPTED
               WHEN CV-RISK-RC-WARN
                   ADD 1                   TO WS-WARN-CNT
                   IF WS-WORST-RC < WS-RC-WARNING
                       MOVE WS-RC-WARNING  TO WS-WORST-RC
                   END-IF
                   PERFORM 2600-WRITE-ACCEPTED
                   PERFORM 2700-WRITE-EXCEPTION
               WHEN CV-RISK-RC-ERROR
                   ADD 1                   TO WS-REVIEW-CNT
                   IF CV-RISK-SANCTION-HIT
                       ADD 1               TO WS-SANCTION-CNT
                   END-IF
                   IF WS-WORST-RC < WS-RC-ERROR
                       MOVE WS-RC-ERROR    TO WS-WORST-RC
                   END-IF
                   PERFORM 2650-WRITE-REVIEW
                   PERFORM 2700-WRITE-EXCEPTION
               WHEN OTHER
                   PERFORM 2700-WRITE-EXCEPTION
                   MOVE 'BUSN'             TO ER-ERROR-TYPE
                   MOVE 'CROSSING RETURNED A FATAL CONDITION'
                                           TO ER-MESSAGE
                   MOVE 0606               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-EVALUATE
           .
      *
       2500-COMPARE-BANDS.
           MOVE PW-RISK-BAND-OLD           TO WS-BAND-RANK-INPUT
           PERFORM 2550-RANK-BAND
           MOVE WS-BAND-RANK               TO WS-BAND-RANK-OLD
      *
           MOVE PW-RISK-BAND-NEW           TO WS-BAND-RANK-INPUT
           PERFORM 2550-RANK-BAND
           MOVE WS-BAND-RANK               TO WS-BAND-RANK-NEW
      *
           IF WS-BAND-RANK-NEW > WS-BAND-RANK-OLD
               ADD 1                       TO WS-BAND-WORSE-CNT
           END-IF
           .
      *
       2550-RANK-BAND.
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
       2600-WRITE-ACCEPTED.
           WRITE ACCEPT-REC FROM PARTY-WORK-REC
           IF NOT WS-ACCEPT-OK
               MOVE 'PARTYACC'             TO ER-FILE-NAME
               MOVE WS-ACCEPT-STATUS       TO ER-FILE-STATUS
               MOVE 'WRITE TO ACCEPTED FILE FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           ADD 1                           TO WS-ACCEPT-CNT
           .
      *
      ******************************************************************
      * 2650 - A PARTY ON THE MANUAL REVIEW FILE IS NEVER WRITTEN TO   *
      *        THE ACCEPTED FILE, SO STEP040 CANNOT APPLY IT.  THE     *
      *        SERVICING TEAM WORKS THE FILE THE FOLLOWING MORNING.    *
      ******************************************************************
       2650-WRITE-REVIEW.
           WRITE REVIEW-REC FROM PARTY-WORK-REC
           IF NOT WS-REVIEW-OK
               MOVE 'PARTYRVW'             TO ER-FILE-NAME
               MOVE WS-REVIEW-STATUS       TO ER-FILE-STATUS
               MOVE 'WRITE TO MANUAL REVIEW FILE FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       2700-WRITE-EXCEPTION.
           MOVE PW-PARTY-ID                TO EL-PARTY-ID
           MOVE PW-ACCT-ID                 TO EL-ACCT-ID
           MOVE PW-RC                      TO EL-RC
           MOVE PW-RISK-BAND-OLD           TO EL-BAND-OLD
           MOVE PW-RISK-BAND-NEW           TO EL-BAND-NEW
           MOVE PW-SANCTION-FLG            TO EL-SANCTION
           MOVE PW-EXPOSURE-AMT            TO EL-EXPOSURE
           MOVE CV-RISK-REASON-TXT         TO EL-REASON
           WRITE EXCEPT-REC FROM WS-EXCEPT-LINE
           .
      *
      ******************************************************************
      * 2800 - AUDIT THE CROSSING.  THE RESOLVED TARGET IS TAKEN FROM  *
      *        THE ROUTE REQUEST THE DISPATCHER FILLED IN - IT IS NOT  *
      *        KNOWN TO THIS PROGRAM UNTIL THE CALL HAS RETURNED.      *
      ******************************************************************
       2800-AUDIT-CROSSING.
           MOVE CV-RISK-CORREL-ID          TO DCL-CORREL-ID
           MOVE WS-PROGRAM-ID              TO DCL-CALLER-PGM
           MOVE RQ-ROUTE-TYPE              TO DCL-ROUTE-TYPE
           MOVE RQ-ROUTE-KEY               TO DCL-ROUTE-KEY
           MOVE RQ-SEQ-NBR                 TO DCL-SEQ-NBR
           MOVE RQ-RESOLVED-PGM            TO DCL-RESOLVED-PGM
           MOVE RQ-RESOLVED-MOD            TO DCL-RESOLVED-MOD
           MOVE RQ-RESOLVED-CALL           TO DCL-CALL-TYPE
           MOVE WS-JOB-NAME                TO DCL-JOB-NAME
           MOVE CV-RISK-RC                 TO DCL-RC
           IF RQ-USED-FALLBACK = SPACE
               MOVE 'N'                    TO DCL-USED-FALLBACK
           ELSE
               MOVE RQ-USED-FALLBACK       TO DCL-USED-FALLBACK
           END-IF
      *
           EXEC SQL
               INSERT INTO CARDSVC.ROUTE_AUDIT
                     (CORREL_ID
                    , CALLER_PGM
                    , ROUTE_TYPE
                    , ROUTE_KEY
                    , SEQ_NBR
                    , RESOLVED_PGM
                    , RESOLVED_MODULE
                    , CALL_TYPE
                    , USED_FALLBACK
                    , RETURN_CD
                    , JOB_NAME)
               VALUES (:DCL-CORREL-ID
                    , :DCL-CALLER-PGM
                    , :DCL-ROUTE-TYPE
                    , :DCL-ROUTE-KEY
                    , :DCL-SEQ-NBR
                    , :DCL-RESOLVED-PGM
                    , :DCL-RESOLVED-MOD
                    , :DCL-CALL-TYPE
                    , :DCL-USED-FALLBACK
                    , :DCL-RC
                    , :DCL-JOB-NAME)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-AUDIT-CNT
               WHEN -803
                   CONTINUE
               WHEN OTHER
      *            THE AUDIT TRAIL IS NOT WORTH FAILING THE CYCLE FOR.
                   DISPLAY 'CBCRD06B WARNING ROUTE AUDIT SQLCODE='
                           SQLCODE ' CORREL=' DCL-CORREL-ID
           END-EVALUATE
           .
      *
       3000-TERMINATE.
           EXEC SQL COMMIT WORK END-EXEC
      *
           CLOSE SORTED-FILE
                 ACCEPT-FILE
                 REVIEW-FILE
                 EXCEPT-FILE
      *
           MOVE WS-WORST-RC                TO WS-RETURN-CODE
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-READ-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B PARTIES READ      ' WS-DISPLAY-CNT
           MOVE WS-DISPATCH-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B CROSSINGS MADE    ' WS-DISPLAY-CNT
           MOVE WS-ACCEPT-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B ACCEPTED          ' WS-DISPLAY-CNT
           MOVE WS-WARN-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B ACCEPTED WITH WARN' WS-DISPLAY-CNT
           MOVE WS-REVIEW-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B MANUAL REVIEW     ' WS-DISPLAY-CNT
           MOVE WS-SANCTION-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B SANCTION FLAGGED  ' WS-DISPLAY-CNT
           MOVE WS-BAND-WORSE-CNT          TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B BANDS DETERIORATED' WS-DISPLAY-CNT
           MOVE WS-AUDIT-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B AUDIT ROWS        ' WS-DISPLAY-CNT
           DISPLAY 'CBCRD06B STEP RETURN CODE  ' WS-RETURN-CODE
           DISPLAY '----------------------------------------------'
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD06B FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           DISPLAY 'CBCRD06B PARTY=' PW-PARTY-ID
                   ' CORREL=' CV-RISK-CORREL-ID
                   ' RISK RC=' CV-RISK-RC
           DISPLAY 'CBCRD06B REASON=' CV-RISK-REASON-TXT
           DISPLAY 'CBCRD06B FAILING PGM=' CV-RISK-FAIL-PGM
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
