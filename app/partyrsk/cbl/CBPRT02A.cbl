      ******************************************************************
      * CBPRT02A - VENDOR SANCTIONS FEED INGEST                        *
      *                                                                *
      * PART OF THE PARTYWK WEEKLY CYCLE, STEP ONE OF JOB CBPRT02J.    *
      *                                                                *
      * READS THE WEEKLY VENDOR SANCTIONS FEED AND APPLIES IT TO       *
      * PARTYRSK.PARTY_SANCTION.  THE FEED IS A FIXED LENGTH EBCDIC    *
      * FILE OF 200 BYTE RECORDS WITH A HEADER AND A TRAILER           *
      * CONTAINING THE VENDOR CONTROL COUNT.  THE COUNTS ARE BALANCED  *
      * BEFORE ANYTHING IS COMMITTED.                                  *
      *                                                                *
      * ACTION CODES ON THE FEED                                       *
      *   A  ADD OR REPLACE - ENTRY BECOMES ACTIVE                     *
      *   C  CHANGE - DETAIL ONLY, ACTIVE FLAG UNTOUCHED               *
      *   D  DELIST - DELISTED DATE SET, ACTIVE FLAG SET TO N          *
      *                                                                *
      * DELISTED ENTRIES ARE NEVER DELETED.  A SANCTIONS ENTRY THAT    *
      * ONCE EXISTED HAS TO STAY VISIBLE TO AN AUDITOR, SO IT IS       *
      * KEPT WITH ACTIVE_FLG = 'N' AND EXCLUDED BY THE SCREENING       *
      * PROGRAMS.                                                      *
      *                                                                *
      * A FEED THAT WOULD DELIST MORE THAN THE PERCENTAGE HELD IN      *
      * WS-DELIST-TOLERANCE IS REJECTED WHOLE - THAT PATTERN HAS       *
      * ALWAYS MEANT A TRUNCATED TRANSMISSION, NOT A REAL PURGE.       *
      *                                                                *
      * RUN BY     - CBPRT02J STEP LOADSANC                            *
      * FILES      - SANCFEED  VENDOR FEED, FB 200                     *
      *              SANCREJ   REJECTED RECORDS, FB 200                *
      * TABLES     - PARTYRSK.PARTY_SANCTION   SELECT/INSERT/UPDATE    *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - FEED APPLIED CLEAN                                      *
      *   04 - APPLIED WITH REJECTS - CHECK SANCREJ                    *
      *   08 - FEED REFUSED ON CONTROL TOTALS OR DELIST TOLERANCE      *
      *   12 - FAILED                                                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBPRT02A.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SANCFEED-FILE ASSIGN TO SANCFEED
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-FEED-STATUS.
      *
           SELECT SANCREJ-FILE  ASSIGN TO SANCREJ
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REJ-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SANCFEED-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  SANCFEED-REC                PIC X(200).
      *
       FD  SANCREJ-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  SANCREJ-REC                 PIC X(200).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CBPRT02A'.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
       01  WS-JOB-NAME                 PIC X(8)  VALUE 'CBPRT02J'.
      *
      ******************************************************************
      * FEED RECORD.  THE VENDOR LAYOUT HAS NOT CHANGED SINCE THE      *
      * 1998 CONTRACT EXCEPT FOR THE PROGRAM CODE, WHICH WAS ADDED     *
      * OVER THE OLD ALIAS TAIL.                                       *
      ******************************************************************
       01  WS-FEED-RECORD.
           05  SF-REC-TYPE             PIC X(2).
               88  SF-HEADER                     VALUE 'HD'.
               88  SF-DETAIL                     VALUE 'DT'.
               88  SF-TRAILER                    VALUE 'TR'.
           05  SF-BODY                 PIC X(198).
      *
       01  WS-FEED-DETAIL.
           05  FILLER                  PIC X(2).
           05  SF-LIST-CD              PIC X(8).
           05  SF-ENTRY-ID             PIC X(16).
           05  SF-ACTION               PIC X(1).
               88  SF-ACT-ADD                    VALUE 'A'.
               88  SF-ACT-CHANGE                 VALUE 'C'.
               88  SF-ACT-DELIST                 VALUE 'D'.
               88  SF-ACT-VALID                  VALUE 'A' 'C' 'D'.
           05  SF-ENTITY-NAME          PIC X(60).
           05  SF-ENTITY-TYPE          PIC X(1).
               88  SF-TYPE-VALID                 VALUE 'I' 'O' 'V'.
           05  SF-COUNTRY              PIC X(3).
           05  SF-DOB                  PIC 9(8).
           05  SF-DOB-R REDEFINES SF-DOB.
               10  SF-DOB-CCYY         PIC 9(4).
               10  SF-DOB-MM           PIC 9(2).
               10  SF-DOB-DD           PIC 9(2).
           05  SF-LISTED-YMD           PIC 9(6).
           05  SF-LISTED-R REDEFINES SF-LISTED-YMD.
               10  SF-LISTED-YY        PIC 9(2).
               10  SF-LISTED-MM        PIC 9(2).
               10  SF-LISTED-DD        PIC 9(2).
           05  SF-DELIST-YMD           PIC 9(6).
           05  SF-DELIST-R REDEFINES SF-DELIST-YMD.
               10  SF-DELIST-YY        PIC 9(2).
               10  SF-DELIST-MM        PIC 9(2).
               10  SF-DELIST-DD        PIC 9(2).
           05  SF-PROGRAM-CD           PIC X(12).
           05  SF-ALIAS-NAME           PIC X(60).
           05  FILLER                  PIC X(22).
      *
       01  WS-FEED-HEADER.
           05  FILLER                  PIC X(2).
           05  HD-VENDOR-CD            PIC X(8).
           05  HD-RUN-DATE             PIC 9(8).
           05  HD-FEED-SEQ             PIC 9(6).
           05  FILLER                  PIC X(176).
      *
       01  WS-FEED-TRAILER.
           05  FILLER                  PIC X(2).
           05  TR-DETAIL-CNT           PIC 9(9).
           05  TR-ADD-CNT              PIC 9(9).
           05  TR-CHG-CNT              PIC 9(9).
           05  TR-DEL-CNT              PIC 9(9).
           05  FILLER                  PIC X(162).
      *
       01  WS-STATUS-FIELDS.
           05  WS-FEED-STATUS          PIC X(2)  VALUE '00'.
           05  WS-REJ-STATUS           PIC X(2)  VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X     VALUE 'N'.
               88  WS-EOF                        VALUE 'Y'.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
           05  WS-REFUSED-SW           PIC X     VALUE 'N'.
               88  WS-REFUSED                    VALUE 'Y'.
           05  WS-HEADER-SEEN-SW       PIC X     VALUE 'N'.
               88  WS-HEADER-SEEN                VALUE 'Y'.
           05  WS-TRAILER-SEEN-SW      PIC X     VALUE 'N'.
               88  WS-TRAILER-SEEN               VALUE 'Y'.
           05  WS-VALID-SW             PIC X     VALUE 'Y'.
               88  WS-VALID                      VALUE 'Y'.
      *
       01  WS-TOTALS.
           05  WS-READ-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-DETAIL-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-ADD-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-UPDATE-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-DELIST-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-COMMIT-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT         PIC 9(9)  VALUE ZERO.
           05  WS-ACTIVE-BEFORE        PIC 9(9)  VALUE ZERO.
      *
       01  WS-CONTROL-VALUES.
           05  WS-DELIST-TOLERANCE     PIC 9(3)  VALUE 010.
           05  WS-DELIST-PCT           PIC S9(5)V99 COMP-3 VALUE ZERO.
      *
       01  WS-EDIT-FIELDS.
           05  WS-ED-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-ED-PCT               PIC ZZ9.99.
           05  WS-SQL-DISP             PIC -(9)9.
      *
       01  WS-REJECT-LINE.
           05  RJ-REASON               PIC X(30).
           05  FILLER                  PIC X     VALUE '|'.
           05  RJ-DATA                 PIC X(169).
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-DATE-R REDEFINES WS-CURR-DATE.
               10  WS-CURR-CCYY        PIC 9(4).
               10  WS-CURR-MMDD        PIC 9(4).
           05  WS-WORK-YY              PIC 9(2)  VALUE ZERO.
           05  WS-WORK-CCYY            PIC 9(4)  VALUE ZERO.
           05  WS-WORK-ISO             PIC X(10) VALUE SPACES.
      *
      *    HOST VARIABLES
       01  DCL-SANC.
           05  DCL-LIST-CD             PIC X(8).
           05  DCL-ENTRY-ID            PIC X(16).
           05  DCL-ENTITY-NAME         PIC X(60).
           05  DCL-ENTITY-TYPE         PIC X(1).
           05  DCL-COUNTRY-CD          PIC X(3).
           05  DCL-DOB                 PIC X(10).
           05  DCL-PROGRAM-CD          PIC X(12).
           05  DCL-LISTED-DATE         PIC X(10).
           05  DCL-DELISTED-DATE       PIC X(10).
           05  DCL-ACTIVE-FLG          PIC X(1).
           05  DCL-LOAD-JOB            PIC X(8).
           05  DCL-ROW-CNT             PIC S9(9) COMP.
      *
       01  DCL-IND.
           05  IND-DOB                 PIC S9(4) COMP.
           05  IND-DELISTED            PIC S9(4) COMP.
           05  IND-COUNTRY             PIC S9(4) COMP.
           05  IND-PROGRAM             PIC S9(4) COMP.
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
           PERFORM 0200-COUNT-ACTIVE
      *
           PERFORM 1000-READ-FEED
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               PERFORM 2000-PROCESS-RECORD
               PERFORM 1000-READ-FEED
           END-PERFORM
      *
           PERFORM 3000-BALANCE-CONTROLS
           PERFORM 4000-CLOSE-DOWN
           PERFORM 9000-REPORT-TOTALS
           GOBACK
           .
      *
       0100-INITIALISE.
           MOVE '0100-INITIALISE'      TO WS-PARAGRAPH
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
           MOVE WS-JOB-NAME            TO DCL-LOAD-JOB
      *
           OPEN INPUT  SANCFEED-FILE
           IF WS-FEED-STATUS NOT = '00'
               MOVE 'SANCFEED'         TO ER-FILE-NAME
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE WS-FEED-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           OPEN OUTPUT SANCREJ-FILE
           IF WS-REJ-STATUS NOT = '00'
               MOVE 'SANCREJ '         TO ER-FILE-NAME
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE WS-REJ-STATUS      TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           DISPLAY 'CBPRT02A SANCTIONS INGEST STARTED DATE='
                   WS-CURR-DATE
           .
      *
       0200-COUNT-ACTIVE.
           MOVE '0200-COUNT-ACTIVE'    TO WS-PARAGRAPH
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-ROW-CNT
                 FROM PARTYRSK.PARTY_SANCTION
                WHERE ACTIVE_FLG = 'Y'
                 WITH UR
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'PARTY_SANCTION   ' TO ER-SQL-TABLE
               MOVE 'SELCOUNT'         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           MOVE DCL-ROW-CNT            TO WS-ACTIVE-BEFORE
           MOVE WS-ACTIVE-BEFORE       TO WS-ED-COUNT
           DISPLAY 'CBPRT02A ACTIVE ENTRIES BEFORE LOAD ' WS-ED-COUNT
           .
      *
       1000-READ-FEED.
           READ SANCFEED-FILE INTO WS-FEED-RECORD
           EVALUATE WS-FEED-STATUS
               WHEN '00'
                   ADD 1               TO WS-READ-CNT
               WHEN '10'
                   MOVE 'Y'            TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'SANCFEED'     TO ER-FILE-NAME
                   MOVE 'READ    '     TO ER-SQL-OPERATION
                   MOVE WS-FEED-STATUS TO ER-FILE-STATUS
                   PERFORM 9200-FILE-ERROR
                   PERFORM 9900-ABEND
           END-EVALUATE
           .
      *
       2000-PROCESS-RECORD.
           MOVE '2000-PROCESS-RECORD'  TO WS-PARAGRAPH
      *
           EVALUATE TRUE
               WHEN SF-HEADER
                   PERFORM 2100-HANDLE-HEADER
               WHEN SF-TRAILER
                   PERFORM 2200-HANDLE-TRAILER
               WHEN SF-DETAIL
                   PERFORM 2300-HANDLE-DETAIL
               WHEN OTHER
                   MOVE 'UNKNOWN RECORD TYPE'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
           END-EVALUATE
           .
      *
       2100-HANDLE-HEADER.
           MOVE WS-FEED-RECORD         TO WS-FEED-HEADER
           MOVE 'Y'                    TO WS-HEADER-SEEN-SW
      *
           DISPLAY 'CBPRT02A FEED HEADER VENDOR=' HD-VENDOR-CD
                   ' RUN DATE=' HD-RUN-DATE
                   ' SEQUENCE=' HD-FEED-SEQ
      *
      *    A FEED OLDER THAN THE CURRENT WEEK IS ACCEPTED BUT CALLED
      *    OUT - THE VENDOR RESENDS AFTER AN OUTAGE AND OPERATIONS
      *    NEED TO SEE IT ON THE JOB LOG.
           IF HD-RUN-DATE < WS-CURR-DATE - 7
               DISPLAY 'CBPRT02A *** FEED IS STALE, RUN DATE '
                       HD-RUN-DATE ' TODAY ' WS-CURR-DATE
           END-IF
           .
      *
       2200-HANDLE-TRAILER.
           MOVE WS-FEED-RECORD         TO WS-FEED-TRAILER
           MOVE 'Y'                    TO WS-TRAILER-SEEN-SW
      *
           DISPLAY 'CBPRT02A FEED TRAILER DETAIL=' TR-DETAIL-CNT
                   ' ADD=' TR-ADD-CNT
                   ' CHG=' TR-CHG-CNT
                   ' DEL=' TR-DEL-CNT
           .
      *
       2300-HANDLE-DETAIL.
           MOVE WS-FEED-RECORD         TO WS-FEED-DETAIL
           ADD 1                       TO WS-DETAIL-CNT
      *
           IF NOT WS-HEADER-SEEN
               MOVE 'DETAIL BEFORE HEADER'
                                       TO RJ-REASON
               PERFORM 2900-REJECT-RECORD
               GO TO 2300-EXIT
           END-IF
      *
           PERFORM 2400-VALIDATE-DETAIL
           IF NOT WS-VALID
               GO TO 2300-EXIT
           END-IF
      *
           PERFORM 2500-CONVERT-DATES
           PERFORM 2600-APPLY-DETAIL
           PERFORM 2800-CHECKPOINT
           .
       2300-EXIT.
           EXIT
           .
      *
       2400-VALIDATE-DETAIL.
           MOVE 'Y'                    TO WS-VALID-SW
      *
           EVALUATE TRUE
               WHEN SF-LIST-CD = SPACES
                   MOVE 'LIST CODE MISSING'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               WHEN SF-ENTRY-ID = SPACES
                   MOVE 'ENTRY ID MISSING'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               WHEN SF-ENTITY-NAME = SPACES
                   MOVE 'ENTITY NAME MISSING'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               WHEN NOT SF-ACT-VALID
                   MOVE 'INVALID ACTION CODE'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               WHEN SF-LISTED-YMD = ZERO
                   MOVE 'LISTED DATE MISSING'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               WHEN SF-LISTED-MM < 01 OR SF-LISTED-MM > 12
                   MOVE 'LISTED DATE NOT VALID'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               WHEN SF-ACT-DELIST AND SF-DELIST-YMD = ZERO
                   MOVE 'DELIST WITHOUT DELIST DATE'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF WS-VALID AND NOT SF-TYPE-VALID
      *        THE VENDOR STILL SENDS THE OCCASIONAL BLANK TYPE ON
      *        VESSEL ENTRIES.  DEFAULT IT RATHER THAN LOSE THE ROW.
               MOVE 'O'                TO SF-ENTITY-TYPE
           END-IF
           .
      *
       2500-CONVERT-DATES.
      *    THE LISTED AND DELISTED DATES ARE SIX DIGIT AND HAVE TO BE
      *    WINDOWED AGAINST THE CENTURY PIVOT.
           MOVE SF-LISTED-YY           TO WS-WORK-YY
           PERFORM 2550-WINDOW-YEAR
           STRING WS-WORK-CCYY '-' SF-LISTED-MM '-' SF-LISTED-DD
                  DELIMITED BY SIZE INTO WS-WORK-ISO
           MOVE WS-WORK-ISO            TO DCL-LISTED-DATE
      *
           IF SF-DELIST-YMD = ZERO
               MOVE -1                 TO IND-DELISTED
               MOVE SPACES             TO DCL-DELISTED-DATE
           ELSE
               MOVE ZERO               TO IND-DELISTED
               MOVE SF-DELIST-YY       TO WS-WORK-YY
               PERFORM 2550-WINDOW-YEAR
               STRING WS-WORK-CCYY '-' SF-DELIST-MM '-' SF-DELIST-DD
                      DELIMITED BY SIZE INTO WS-WORK-ISO
               MOVE WS-WORK-ISO        TO DCL-DELISTED-DATE
           END-IF
      *
      *    THE DATE OF BIRTH IS EIGHT DIGIT AND OFTEN ABSENT.
           IF SF-DOB = ZERO OR SF-DOB-MM = ZERO
               MOVE -1                 TO IND-DOB
               MOVE SPACES             TO DCL-DOB
           ELSE
               MOVE ZERO               TO IND-DOB
               STRING SF-DOB-CCYY '-' SF-DOB-MM '-' SF-DOB-DD
                      DELIMITED BY SIZE INTO WS-WORK-ISO
               MOVE WS-WORK-ISO        TO DCL-DOB
           END-IF
      *
           IF SF-COUNTRY = SPACES
               MOVE -1                 TO IND-COUNTRY
           ELSE
               MOVE ZERO               TO IND-COUNTRY
           END-IF
           IF SF-PROGRAM-CD = SPACES
               MOVE -1                 TO IND-PROGRAM
           ELSE
               MOVE ZERO               TO IND-PROGRAM
           END-IF
      *
           MOVE SF-LIST-CD             TO DCL-LIST-CD
           MOVE SF-ENTRY-ID            TO DCL-ENTRY-ID
           MOVE SF-ENTITY-NAME         TO DCL-ENTITY-NAME
           MOVE SF-ENTITY-TYPE         TO DCL-ENTITY-TYPE
           MOVE SF-COUNTRY             TO DCL-COUNTRY-CD
           MOVE SF-PROGRAM-CD          TO DCL-PROGRAM-CD
           .
      *
       2550-WINDOW-YEAR.
           IF WS-WORK-YY > WS-CENTURY-PIVOT
               COMPUTE WS-WORK-CCYY = 1900 + WS-WORK-YY
           ELSE
               COMPUTE WS-WORK-CCYY = 2000 + WS-WORK-YY
           END-IF
           .
      *
       2600-APPLY-DETAIL.
           MOVE '2600-APPLY-DETAIL'    TO WS-PARAGRAPH
      *
           EVALUATE TRUE
               WHEN SF-ACT-DELIST
                   MOVE 'N'            TO DCL-ACTIVE-FLG
               WHEN SF-ACT-ADD
                   MOVE 'Y'            TO DCL-ACTIVE-FLG
               WHEN OTHER
                   MOVE SPACES         TO DCL-ACTIVE-FLG
           END-EVALUATE
      *
           PERFORM 2650-UPDATE-ENTRY
           IF SQLCODE = +100
               IF SF-ACT-DELIST
      *            A DELIST FOR AN ENTRY WE NEVER LOADED.  RECORD IT
      *            SO THE VENDOR CAN BE QUERIED, DO NOT INSERT.
                   MOVE 'DELIST FOR UNKNOWN ENTRY'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               ELSE
                   PERFORM 2700-INSERT-ENTRY
               END-IF
           END-IF
           .
      *
       2650-UPDATE-ENTRY.
           IF SF-ACT-CHANGE
               EXEC SQL
                   UPDATE PARTYRSK.PARTY_SANCTION
                      SET ENTITY_NAME   = :DCL-ENTITY-NAME
                        , ENTITY_TYPE   = :DCL-ENTITY-TYPE
                        , COUNTRY_CD    = :DCL-COUNTRY-CD
                                          :IND-COUNTRY
                        , DOB           = :DCL-DOB :IND-DOB
                        , PROGRAM_CD    = :DCL-PROGRAM-CD
                                          :IND-PROGRAM
                        , LISTED_DATE   = :DCL-LISTED-DATE
                        , LOAD_JOB      = :DCL-LOAD-JOB
                        , LOAD_TS       = CURRENT TIMESTAMP
                    WHERE LIST_CD  = :DCL-LIST-CD
                      AND ENTRY_ID = :DCL-ENTRY-ID
               END-EXEC
           ELSE
               EXEC SQL
                   UPDATE PARTYRSK.PARTY_SANCTION
                      SET ENTITY_NAME   = :DCL-ENTITY-NAME
                        , ENTITY_TYPE   = :DCL-ENTITY-TYPE
                        , COUNTRY_CD    = :DCL-COUNTRY-CD
                                          :IND-COUNTRY
                        , DOB           = :DCL-DOB :IND-DOB
                        , PROGRAM_CD    = :DCL-PROGRAM-CD
                                          :IND-PROGRAM
                        , LISTED_DATE   = :DCL-LISTED-DATE
                        , DELISTED_DATE = :DCL-DELISTED-DATE
                                          :IND-DELISTED
                        , ACTIVE_FLG    = :DCL-ACTIVE-FLG
                        , LOAD_JOB      = :DCL-LOAD-JOB
                        , LOAD_TS       = CURRENT TIMESTAMP
                    WHERE LIST_CD  = :DCL-LIST-CD
                      AND ENTRY_ID = :DCL-ENTRY-ID
               END-EXEC
           END-IF
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF SF-ACT-DELIST
                       ADD 1           TO WS-DELIST-CNT
                   ELSE
                       ADD 1           TO WS-UPDATE-CNT
                   END-IF
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE 'PARTY_SANCTION   ' TO ER-SQL-TABLE
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2700-INSERT-ENTRY.
           IF DCL-ACTIVE-FLG = SPACES
      *        A CHANGE FOR AN ENTRY WE DO NOT HOLD BECOMES AN ADD.
               MOVE 'Y'                TO DCL-ACTIVE-FLG
           END-IF
      *
           EXEC SQL
               INSERT INTO PARTYRSK.PARTY_SANCTION
                     (LIST_CD
                    , ENTRY_ID
                    , ENTITY_NAME
                    , ENTITY_TYPE
                    , COUNTRY_CD
                    , DOB
                    , PROGRAM_CD
                    , LISTED_DATE
                    , DELISTED_DATE
                    , ACTIVE_FLG
                    , LOAD_JOB)
               VALUES (:DCL-LIST-CD
                    , :DCL-ENTRY-ID
                    , :DCL-ENTITY-NAME
                    , :DCL-ENTITY-TYPE
                    , :DCL-COUNTRY-CD    :IND-COUNTRY
                    , :DCL-DOB           :IND-DOB
                    , :DCL-PROGRAM-CD    :IND-PROGRAM
                    , :DCL-LISTED-DATE
                    , :DCL-DELISTED-DATE :IND-DELISTED
                    , :DCL-ACTIVE-FLG
                    , :DCL-LOAD-JOB)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-ADD-CNT
               WHEN -803
                   MOVE 'DUPLICATE WITHIN FEED'
                                       TO RJ-REASON
                   PERFORM 2900-REJECT-RECORD
               WHEN OTHER
                   MOVE 'PARTY_SANCTION   ' TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2800-CHECKPOINT.
           ADD 1                       TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT < WS-COMMIT-FREQUENCY
               GO TO 2800-EXIT
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
           ADD 1                       TO WS-COMMIT-CNT
           MOVE ZERO                   TO WS-SINCE-COMMIT
           MOVE WS-DETAIL-CNT          TO WS-ED-COUNT
           DISPLAY 'CBPRT02A CHECKPOINT ' WS-COMMIT-CNT
                   ' DETAIL=' WS-ED-COUNT
                   ' LAST ENTRY=' DCL-ENTRY-ID
           .
       2800-EXIT.
           EXIT
           .
      *
       2900-REJECT-RECORD.
           MOVE 'N'                    TO WS-VALID-SW
           ADD 1                       TO WS-REJECT-CNT
           MOVE WS-FEED-RECORD(1:169)  TO RJ-DATA
      *
           WRITE SANCREJ-REC FROM WS-REJECT-LINE
           IF WS-REJ-STATUS NOT = '00'
               MOVE 'SANCREJ '         TO ER-FILE-NAME
               MOVE 'WRITE   '         TO ER-SQL-OPERATION
               MOVE WS-REJ-STATUS      TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           IF WS-REJECT-CNT < 26
               DISPLAY 'CBPRT02A REJECT ' RJ-REASON
                       ' LIST=' SF-LIST-CD
                       ' ENTRY=' SF-ENTRY-ID
           END-IF
           .
      *
      ******************************************************************
      * 3000 - CONTROL BALANCING                                       *
      *                                                                *
      * NOTHING IS BACKED OUT ONCE A CHECKPOINT HAS BEEN TAKEN, SO A   *
      * FAILURE HERE LEAVES THE TABLE PART LOADED.  THE OPERATOR       *
      * RESTORES THE IMAGE COPY AND RERUNS - SEE THE RUNBOOK.          *
      ******************************************************************
       3000-BALANCE-CONTROLS.
           MOVE '3000-BALANCE-CONTROLS' TO WS-PARAGRAPH
      *
           IF NOT WS-TRAILER-SEEN
               DISPLAY 'CBPRT02A *** NO TRAILER RECORD - FEED IS '
                       'TRUNCATED'
               MOVE 'Y'                TO WS-REFUSED-SW
               GO TO 3000-EXIT
           END-IF
      *
           IF TR-DETAIL-CNT NOT = WS-DETAIL-CNT
               MOVE WS-DETAIL-CNT      TO WS-ED-COUNT
               DISPLAY 'CBPRT02A *** CONTROL COUNT MISMATCH '
                       'TRAILER=' TR-DETAIL-CNT
                       ' READ=' WS-ED-COUNT
               MOVE 'Y'                TO WS-REFUSED-SW
           END-IF
      *
           IF WS-ACTIVE-BEFORE > ZERO
               COMPUTE WS-DELIST-PCT ROUNDED =
                       (WS-DELIST-CNT * 100) / WS-ACTIVE-BEFORE
               IF WS-DELIST-PCT > WS-DELIST-TOLERANCE
                   MOVE WS-DELIST-PCT  TO WS-ED-PCT
                   DISPLAY 'CBPRT02A *** DELIST VOLUME ' WS-ED-PCT
                           ' PCT EXCEEDS TOLERANCE '
                           WS-DELIST-TOLERANCE
                   MOVE 'Y'            TO WS-REFUSED-SW
               END-IF
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
       4000-CLOSE-DOWN.
           MOVE '4000-CLOSE-DOWN'      TO WS-PARAGRAPH
      *
           IF WS-FATAL OR WS-REFUSED
               EXEC SQL
                   ROLLBACK
               END-EXEC
               DISPLAY 'CBPRT02A UNCOMMITTED WORK BACKED OUT'
           ELSE
               EXEC SQL
                   COMMIT
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'COMMIT           ' TO ER-SQL-TABLE
                   MOVE 'COMMIT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
               END-IF
           END-IF
      *
           CLOSE SANCFEED-FILE
           IF WS-FEED-STATUS NOT = '00'
               MOVE 'SANCFEED'         TO ER-FILE-NAME
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE WS-FEED-STATUS     TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
           END-IF
      *
           CLOSE SANCREJ-FILE
           IF WS-REJ-STATUS NOT = '00'
               MOVE 'SANCREJ '         TO ER-FILE-NAME
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE WS-REJ-STATUS      TO ER-FILE-STATUS
               PERFORM 9200-FILE-ERROR
           END-IF
           .
      *
       9000-REPORT-TOTALS.
           DISPLAY '******************************************'
           DISPLAY 'CBPRT02A SANCTIONS FEED SUMMARY'
           MOVE WS-READ-CNT            TO WS-ED-COUNT
           DISPLAY '   RECORDS READ       ' WS-ED-COUNT
           MOVE WS-DETAIL-CNT          TO WS-ED-COUNT
           DISPLAY '   DETAIL RECORDS     ' WS-ED-COUNT
           MOVE WS-ADD-CNT             TO WS-ED-COUNT
           DISPLAY '   ENTRIES ADDED      ' WS-ED-COUNT
           MOVE WS-UPDATE-CNT          TO WS-ED-COUNT
           DISPLAY '   ENTRIES UPDATED    ' WS-ED-COUNT
           MOVE WS-DELIST-CNT          TO WS-ED-COUNT
           DISPLAY '   ENTRIES DELISTED   ' WS-ED-COUNT
           MOVE WS-REJECT-CNT          TO WS-ED-COUNT
           DISPLAY '   RECORDS REJECTED   ' WS-ED-COUNT
           DISPLAY '   COMMITS TAKEN      ' WS-COMMIT-CNT
           DISPLAY '******************************************'
      *
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12             TO RETURN-CODE
               WHEN WS-REFUSED
                   MOVE 8              TO RETURN-CODE
               WHEN WS-REJECT-CNT > ZERO
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
           DISPLAY 'CBPRT02A SQL ERROR PARA=' WS-PARAGRAPH
                   ' TABLE=' ER-SQL-TABLE
           DISPLAY '         OP=' ER-SQL-OPERATION
                   ' SQLCODE=' WS-SQL-DISP
           DISPLAY '         LIST=' DCL-LIST-CD
                   ' ENTRY=' DCL-ENTRY-ID
           DISPLAY '         SQLERRMC=' SQLERRMC(1:44)
           .
      *
       9200-FILE-ERROR.
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           DISPLAY 'CBPRT02A FILE ERROR ' ER-FILE-NAME
                   ' OP=' ER-SQL-OPERATION
                   ' STATUS=' ER-FILE-STATUS
           .
      *
      ******************************************************************
      * 9900 - U3121.  THE SCREENING FILE IS BUILT FROM THIS TABLE IN  *
      *        THE NEXT STEP, SO A HALF APPLIED FEED MUST STOP THE     *
      *        JOB BEFORE CBPRT02B RUNS.                               *
      ******************************************************************
       9900-ABEND.
           MOVE 'U312'                 TO ER-ABEND-CODE
           EXEC SQL
               ROLLBACK
           END-EXEC
           DISPLAY 'CBPRT02A ABEND U3121 PARA=' WS-PARAGRAPH
                   ' RECORD=' WS-READ-CNT
           MOVE 12                     TO RETURN-CODE
           STOP RUN
           .
