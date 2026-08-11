      ******************************************************************
      * CBREF04 - PGMROUT UNLOAD AND REBUILD VERIFICATION              *
      *                                                                *
      * PGMROUT IS THE VSAM FALLBACK COPY OF CARDSVC.PGM_ROUTE THAT    *
      * CACRD90 AND CBCRD90 READ WHEN THE DB2 LOAD OF THE ROUTE CACHE  *
      * FAILS.  BECAUSE THE DISPATCHERS READ THE CLUSTER DIRECTLY INTO *
      * ROUTE-RECORD (CVROUT01Y) THE UNLOAD RECORD IS THAT COPYBOOK -  *
      * NO REFORMATTING, NO REORDERED FIELDS.                          *
      *                                                                *
      *   PARM='UNLOAD'  STEP UNLDROUT - ACTIVE, IN DATE ROWS ONLY     *
      *   PARM='VERIFY'  STEP VERFROUT - COUNT THE REBUILT CLUSTER     *
      *                                                                *
      * CALLED BY   - CBREF04J (BOTH STEPS RUN UNDER IKJEFT01)         *
      * CALLS       - CBCRD91 ON A FATAL CONDITION                     *
      * READS       - CARDSVC.PGM_ROUTE, PGMROUT (KSDS), ROUTCTL       *
      * WRITES      - ROUTUNL (FB 97), ROUTCTL (FB 80)                 *
      *                                                                *
      * NOTE ON THE RECORD LENGTH - THE CLUSTER WAS DEFINED WITH       *
      * RECORDSIZE(97 97) WHEN THE DESCRIPTION WAS 39 BYTES.  THE      *
      * COPYBOOK DESCRIPTION IS NOW 40, SO THE UNLOAD WRITES THE       *
      * FIRST 97 BYTES OF ROUTE-RECORD AND THE LAST BYTE OF THE        *
      * DESCRIPTION IS NOT CARRIED IN THE FALLBACK COPY.  EVERY FIELD  *
      * THE DISPATCHERS ACTUALLY USE SITS IN THE FIRST 58 BYTES, SO    *
      * THE CLUSTER HAS NEVER BEEN REDEFINED.  DO NOT "FIX" THIS       *
      * WITHOUT REDEFINING THE CLUSTER IN THE SAME CHANGE.             *
      *                                                                *
      * A COUNT MISMATCH IS AN ABEND.  A SHORT PGMROUT WOULD LOOK      *
      * PERFECTLY HEALTHY UNTIL THE DAY DB2 IS DOWN AND A ROUTE THAT   *
      * NEVER MADE IT INTO THE CLUSTER IS ASKED FOR.                   *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - UNLOADED / COUNTS AGREE                                 *
      *   04 - UNLOADED, EXPIRED OR INACTIVE ROWS SKIPPED              *
      *   12 - FATAL, USER ABEND U931 THROUGH CBCRD91                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBREF04.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROUTUNL-FILE ASSIGN TO ROUTUNL
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-UNL-STATUS.
      *
           SELECT ROUTCTL-FILE ASSIGN TO ROUTCTL
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-CTL-STATUS.
      *
           SELECT PGMROUT-FILE ASSIGN TO PGMROUT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS PGR-KEY
                  FILE STATUS  IS WS-RTE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ROUTUNL-FILE
           RECORD CONTAINS 97 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  ROUTUNL-REC                     PIC X(97).
      *
       FD  ROUTCTL-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  ROUTCTL-REC.
           05  RC-LABEL                    PIC X(12).
           05  RC-COUNT                    PIC 9(9).
           05  RC-RUN-DATE                 PIC 9(8).
           05  FILLER                      PIC X(51).
      *
       FD  PGMROUT-FILE
           RECORD CONTAINS 97 CHARACTERS.
       01  PGMROUT-REC.
           05  PGR-KEY                     PIC X(16).
           05  FILLER                      PIC X(81).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBREF04 '.
      *
           COPY CVROUT01Y.
      *
       01  WS-STATUSES.
           05  WS-UNL-STATUS               PIC X(2)  VALUE '00'.
               88  WS-UNL-OK                         VALUE '00'.
           05  WS-CTL-STATUS               PIC X(2)  VALUE '00'.
               88  WS-CTL-OK                         VALUE '00'.
           05  WS-RTE-STATUS               PIC X(2)  VALUE '00'.
               88  WS-RTE-OK                         VALUE '00'.
               88  WS-RTE-EOF                        VALUE '10'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
      *
       01  WS-FUNCTION                     PIC X(8)  VALUE SPACES.
           88  WS-UNLOAD                             VALUE 'UNLOAD  '.
           88  WS-VERIFY                             VALUE 'VERIFY  '.
      *
       01  WS-COUNTS.
           05  WS-DB2-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-UNL-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-VSAM-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-CTL-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-SKIP-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-XMOD-CNT                 PIC 9(9)  VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-RETURN-CD                PIC S9(4) COMP VALUE ZERO.
           05  WS-MSG                      PIC X(78) VALUE SPACES.
           05  WS-PREV-KEY                 PIC X(16) VALUE LOW-VALUES.
      *
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-YYYY                  PIC 9(4).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-ROUTE.
           05  DCL-ROUTE-TYPE              PIC X(4).
           05  DCL-ROUTE-KEY               PIC X(8).
           05  DCL-SEQ-NBR                 PIC S9(4) COMP.
           05  DCL-PGM-NAME                PIC X(8).
           05  DCL-CALL-TYPE               PIC X.
           05  DCL-MODULE-ID               PIC X(8).
           05  DCL-EFF-DATE                PIC X(10).
           05  DCL-EXP-DATE                PIC X(10).
           05  DCL-ACTIVE-FLG              PIC X.
           05  DCL-FALLBACK-PGM            PIC X(8).
           05  DCL-DESCRIPTION             PIC X(40).
           05  DCL-RUN-DATE                PIC X(10).
           05  DCL-ROW-CNT                 PIC S9(9) COMP-3.
      *
       01  IND-VARS.
           05  IND-FALLBACK                PIC S9(4) COMP.
           05  IND-DESC                    PIC S9(4) COMP.
      *
      ******************************************************************
      * ONLY ROWS THAT ARE ACTIVE AND IN DATE ON THE RUN DATE GO INTO  *
      * THE FALLBACK COPY.  THE TABLE KEEPS EXPIRED GENERATIONS FOR    *
      * AUDIT AND THE DISPATCHERS FILTER THEM ON THE DB2 PATH, BUT     *
      * THEY DO NOT FILTER ON THE VSAM PATH - THEY TAKE THE FIRST      *
      * MATCHING KEY.  THE FILTER HAS TO HAPPEN HERE.                  *
      ******************************************************************
           EXEC SQL DECLARE ROUTCSR CURSOR FOR
               SELECT ROUTE_TYPE
                    , ROUTE_KEY
                    , SEQ_NBR
                    , PGM_NAME
                    , CALL_TYPE
                    , MODULE_ID
                    , CHAR(EFF_DATE, ISO)
                    , CHAR(EXP_DATE, ISO)
                    , ACTIVE_FLG
                    , FALLBACK_PGM
                    , DESCRIPTION
                 FROM CARDSVC.PGM_ROUTE
                WHERE ACTIVE_FLG = 'Y'
                  AND DATE(:DCL-RUN-DATE)
                      BETWEEN EFF_DATE AND EXP_DATE
                ORDER BY ROUTE_TYPE
                       , ROUTE_KEY
                       , SEQ_NBR
                WITH UR
           END-EXEC.
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
           IF WS-FATAL
               GO TO 0000-TERMINATE
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-UNLOAD
                   PERFORM 2000-UNLOAD-ROUTES
               WHEN WS-VERIFY
                   PERFORM 3000-VERIFY-REBUILD
               WHEN OTHER
                   MOVE 'PARM MUST BE UNLOAD OR VERIFY'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
           END-EVALUATE
           .
       0000-TERMINATE.
           IF WS-FATAL
               PERFORM 9500-ABEND
           END-IF
           MOVE WS-RETURN-CD               TO RETURN-CODE
           GOBACK
           .
      *
      ******************************************************************
      * PARM IS FUNCTION,CCYYMMDD - THE RUN DATE IS PASSED BECAUSE A   *
      * RERUN OF LAST WEEK'S CYCLE MUST REBUILD LAST WEEK'S ROUTES,    *
      * NOT TODAY'S.                                                   *
      ******************************************************************
       1000-INITIALISE.
           IF LK-PARM-LEN < 15
               MOVE 'PARM MUST BE FUNCTION,CCYYMMDD'
                                           TO WS-MSG
               PERFORM 9200-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           MOVE SPACES                     TO WS-FUNCTION
           UNSTRING LK-PARM-DATA(1:LK-PARM-LEN)
               DELIMITED BY ','
               INTO WS-FUNCTION
                  , WS-DATE-WORK
           END-UNSTRING
      *
           IF WS-DATE-WORK = ZERO
               MOVE 'RUN DATE IN THE PARM IS NOT NUMERIC'
                                           TO WS-MSG
               PERFORM 9200-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           MOVE SPACES                     TO DCL-RUN-DATE
           MOVE WS-DW-YYYY                 TO DCL-RUN-DATE(1:4)
           MOVE '-'                        TO DCL-RUN-DATE(5:1)
           MOVE WS-DW-MM                   TO DCL-RUN-DATE(6:2)
           MOVE '-'                        TO DCL-RUN-DATE(8:1)
           MOVE WS-DW-DD                   TO DCL-RUN-DATE(9:2)
      *
           DISPLAY 'CBREF04 FUNCTION ' WS-FUNCTION
                   ' RUN DATE ' DCL-RUN-DATE
           .
       1000-EXIT.
           EXIT
           .
      *
       2000-UNLOAD-ROUTES.
           OPEN OUTPUT ROUTUNL-FILE
           IF NOT WS-UNL-OK
               MOVE 'ROUTUNL OPEN FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2100-COUNT-DB2-ROWS
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           EXEC SQL
               OPEN ROUTCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'OPEN ROUTCSR FAILED'  TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM UNTIL WS-EOF OR WS-FATAL
               PERFORM 2200-FETCH-ROUTE
               IF NOT WS-EOF AND NOT WS-FATAL
                   PERFORM 2300-WRITE-ROUTE
               END-IF
           END-PERFORM
      *
           EXEC SQL
               CLOSE ROUTCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'CLOSE ROUTCSR FAILED' TO WS-MSG
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           PERFORM 2400-WRITE-CONTROL
           CLOSE ROUTUNL-FILE
      *
           DISPLAY 'CBREF04 UNLOAD DB2=' WS-DB2-CNT
                   ' WRITTEN=' WS-UNL-CNT
                   ' DUPLICATE KEYS SKIPPED=' WS-SKIP-CNT
                   ' CROSS MODULE=' WS-XMOD-CNT
      *
           EVALUATE TRUE
               WHEN WS-UNL-CNT = ZERO
                   MOVE 'NO ACTIVE ROUTES SELECTED FOR THE RUN DATE'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
               WHEN WS-SKIP-CNT > ZERO
                   MOVE 4                  TO WS-RETURN-CD
               WHEN OTHER
                   MOVE 0                  TO WS-RETURN-CD
           END-EVALUATE
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-COUNT-DB2-ROWS.
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-ROW-CNT
                 FROM CARDSVC.PGM_ROUTE
                WHERE ACTIVE_FLG = 'Y'
                  AND DATE(:DCL-RUN-DATE)
                      BETWEEN EFF_DATE AND EXP_DATE
                WITH UR
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'COUNT OF CARDSVC.PGM_ROUTE FAILED'
                                           TO WS-MSG
               PERFORM 9400-SQL-ERROR
           ELSE
               MOVE DCL-ROW-CNT            TO WS-DB2-CNT
           END-IF
           .
      *
       2200-FETCH-ROUTE.
           EXEC SQL
               FETCH ROUTCSR
                INTO :DCL-ROUTE-TYPE
                   , :DCL-ROUTE-KEY
                   , :DCL-SEQ-NBR
                   , :DCL-PGM-NAME
                   , :DCL-CALL-TYPE
                   , :DCL-MODULE-ID
                   , :DCL-EFF-DATE
                   , :DCL-EXP-DATE
                   , :DCL-ACTIVE-FLG
                   , :DCL-FALLBACK-PGM :IND-FALLBACK
                   , :DCL-DESCRIPTION :IND-DESC
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'                TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'FETCH ROUTCSR FAILED'
                                           TO WS-MSG
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2300 - THE TABLE KEY CARRIES EFF_DATE, THE CLUSTER KEY DOES    *
      *        NOT.  TWO GENERATIONS OF THE SAME ROUTE CAN BOTH BE     *
      *        ACTIVE AND IN DATE, AND ONLY THE FIRST ONE IN EFFECTIVE *
      *        DATE ORDER CAN BE LOADED.  THE SECOND IS COUNTED AND    *
      *        REPORTED SO THE REFERENCE TEAM CAN EXPIRE IT.           *
      ******************************************************************
       2300-WRITE-ROUTE.
           MOVE SPACES                     TO ROUTE-RECORD
           MOVE DCL-ROUTE-TYPE             TO RT-ROUTE-TYPE
           MOVE DCL-ROUTE-KEY              TO RT-ROUTE-KEY
           MOVE DCL-SEQ-NBR                TO RT-SEQ-NBR
           MOVE DCL-PGM-NAME               TO RT-PGM-NAME
           MOVE DCL-CALL-TYPE              TO RT-CALL-TYPE
           MOVE DCL-MODULE-ID              TO RT-MODULE-ID
           MOVE DCL-ACTIVE-FLG             TO RT-ACTIVE-FLG
      *
           IF IND-FALLBACK < ZERO
               MOVE SPACES                 TO RT-FALLBACK-PGM
           ELSE
               MOVE DCL-FALLBACK-PGM       TO RT-FALLBACK-PGM
           END-IF
      *
           IF IND-DESC < ZERO
               MOVE SPACES                 TO RT-DESCRIPTION
           ELSE
               MOVE DCL-DESCRIPTION        TO RT-DESCRIPTION
           END-IF
      *
           MOVE DCL-EFF-DATE(1:4)          TO WS-DW-YYYY
           MOVE DCL-EFF-DATE(6:2)          TO WS-DW-MM
           MOVE DCL-EFF-DATE(9:2)          TO WS-DW-DD
           MOVE WS-DATE-WORK               TO RT-EFF-DATE
      *
           MOVE DCL-EXP-DATE(1:4)          TO WS-DW-YYYY
           MOVE DCL-EXP-DATE(6:2)          TO WS-DW-MM
           MOVE DCL-EXP-DATE(9:2)          TO WS-DW-DD
           MOVE WS-DATE-WORK               TO RT-EXP-DATE
      *
           IF RT-KEY = WS-PREV-KEY
               ADD 1                       TO WS-SKIP-CNT
               DISPLAY 'CBREF04 OVERLAPPING ROUTE GENERATION - '
                       RT-KEY ' PGM ' RT-PGM-NAME
               GO TO 2300-EXIT
           END-IF
           MOVE RT-KEY                     TO WS-PREV-KEY
      *
           IF RT-MOD-PARTYRSK
               ADD 1                       TO WS-XMOD-CNT
           END-IF
      *
           MOVE ROUTE-RECORD(1:97)         TO ROUTUNL-REC
           WRITE ROUTUNL-REC
           IF NOT WS-UNL-OK
               MOVE 'ROUTUNL WRITE FAILED' TO WS-MSG
               PERFORM 9300-FILE-ERROR
           ELSE
               ADD 1                       TO WS-UNL-CNT
           END-IF
           .
       2300-EXIT.
           EXIT
           .
      *
       2400-WRITE-CONTROL.
           OPEN OUTPUT ROUTCTL-FILE
           IF NOT WS-CTL-OK
               MOVE 'ROUTCTL OPEN FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 2400-EXIT
           END-IF
      *
           MOVE 'PGMROUTCNT  '             TO RC-LABEL
           MOVE WS-UNL-CNT                 TO RC-COUNT
           MOVE WS-DATE-WORK               TO RC-RUN-DATE
           WRITE ROUTCTL-REC
           IF NOT WS-CTL-OK
               MOVE 'ROUTCTL WRITE FAILED' TO WS-MSG
               PERFORM 9300-FILE-ERROR
           END-IF
      *
           CLOSE ROUTCTL-FILE
           .
       2400-EXIT.
           EXIT
           .
      *
       3000-VERIFY-REBUILD.
           OPEN INPUT ROUTCTL-FILE
           IF NOT WS-CTL-OK
               MOVE 'ROUTCTL OPEN FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           READ ROUTCTL-FILE
               AT END
                   MOVE 'ROUTCTL IS EMPTY - UNLOAD DID NOT RUN'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
           END-READ
           MOVE RC-COUNT                   TO WS-CTL-CNT
           CLOSE ROUTCTL-FILE
      *
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 2100-COUNT-DB2-ROWS
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
      *
           OPEN INPUT PGMROUT-FILE
           IF NOT WS-RTE-OK
               MOVE 'PGMROUT OPEN FAILED - CLUSTER NOT REBUILT'
                                           TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               READ PGMROUT-FILE NEXT RECORD
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-VSAM-CNT
                       PERFORM 3100-CHECK-RECORD
               END-READ
      *
               IF NOT WS-RTE-OK
               AND NOT WS-RTE-EOF
                   MOVE 'PGMROUT READ FAILED'
                                           TO WS-MSG
                   PERFORM 9300-FILE-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE PGMROUT-FILE
      *
           DISPLAY 'CBREF04 VERIFY DB2=' WS-DB2-CNT
                   ' UNLOAD=' WS-CTL-CNT
                   ' VSAM=' WS-VSAM-CNT
      *
           EVALUATE TRUE
               WHEN WS-FATAL
                   CONTINUE
               WHEN WS-VSAM-CNT NOT = WS-CTL-CNT
                   MOVE 'PGMROUT COUNT DOES NOT MATCH THE UNLOAD'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
               WHEN OTHER
                   MOVE 0                  TO WS-RETURN-CD
                   DISPLAY 'CBREF04 PGMROUT REBUILD VERIFIED'
           END-EVALUATE
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3100 - A ROUTE WITH A BLANK PROGRAM NAME LOADS PERFECTLY WELL  *
      *        AND FAILS AT DISPATCH TIME, SO IT IS TREATED AS A FAILED*
      *        REBUILD RATHER THAN A WARNING.                          *
      ******************************************************************
       3100-CHECK-RECORD.
           MOVE PGMROUT-REC                TO ROUTE-RECORD(1:97)
      *
           IF RT-PGM-NAME = SPACES
               MOVE 'PGMROUT HOLDS A ROUTE WITH NO PROGRAM NAME'
                                           TO WS-MSG
               PERFORM 9200-FATAL
           END-IF
           .
      *
       9200-FATAL.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF04 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'ROUT'                     TO ER-ERROR-TYPE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U931'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF04 FATAL - ' WS-MSG
           .
      *
       9300-FILE-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF04 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'VSAM'                     TO ER-ERROR-TYPE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'PGMROUT '                 TO ER-FILE-NAME
           MOVE WS-RTE-STATUS              TO ER-FILE-STATUS
           MOVE 'U931'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF04 FILE ERROR - ' WS-MSG
                   ' UNL=' WS-UNL-STATUS
                   ' CTL=' WS-CTL-STATUS
                   ' RTE=' WS-RTE-STATUS
           .
      *
       9400-SQL-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF04 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           MOVE 'PGM_ROUTE         '       TO ER-SQL-TABLE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U932'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF04 SQL ERROR SQLCODE=' SQLCODE
                   ' - ' WS-MSG
           .
      *
       9500-ABEND.
           MOVE 12                         TO WS-RETURN-CD
           CALL 'CBCRD91' USING ERROR-AREA
           .
