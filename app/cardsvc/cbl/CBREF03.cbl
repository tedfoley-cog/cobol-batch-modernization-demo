      ******************************************************************
      * CBREF03 - FRAUDRUL AND RSNCODE UNLOAD AND VERIFICATION         *
      *                                                                *
      * RUNS TWICE IN CBREF03J, EITHER SIDE OF THE SORT AND THE IDCAMS *
      * REBUILD, EXACTLY AS CBREF02 DOES FOR MERCHRTE -                *
      *                                                                *
      *   PARM='UNLOAD'  STEP UNLDREF - CURSOR OVER CARDSVC.FRAUD_RULE *
      *                  ONTO FRAUUNL, AND THE REASON CODE REFERENCE   *
      *                  FEED RSNFEED ONTO RSNUNL AFTER EDITING        *
      *   PARM='VERIFY'  STEP VERFREF - READ BOTH REBUILT CLUSTERS END *
      *                  TO END AND COMPARE THE COUNTS WITH THE        *
      *                  CONTROL RECORDS WRITTEN BY THE UNLOAD         *
      *                                                                *
      * THE REASON CODE REFERENCE IS NOT A DB2 TABLE - IT IS STILL THE *
      * FLAT MASTER THE REFERENCE DATA TEAM MAINTAINS ON TSO, WHICH IS *
      * WHY THE RSNCODE SIDE EDITS ITS INPUT AND THE FRAUD RULE SIDE   *
      * DOES NOT.                                                      *
      *                                                                *
      * CALLED BY   - CBREF03J (BOTH STEPS RUN UNDER IKJEFT01)         *
      * CALLS       - CBCRD91 ON A FATAL CONDITION                     *
      * READS       - CARDSVC.FRAUD_RULE, RSNFEED, FRAUDRUL, RSNCODE   *
      * WRITES      - FRAUUNL (FB 240), RSNUNL (FB 100), REFCTL (FB 80)*
      *                                                                *
      * A COUNT MISMATCH ON EITHER CLUSTER IS AN ABEND.  A SHORT       *
      * FRAUDRUL MEANS RULES SILENTLY STOP FIRING, WHICH IS WORSE      *
      * THAN A FAILED JOB.                                             *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - UNLOADED / COUNTS AGREE                                 *
      *   04 - UNLOADED WITH EDITED REASON CODES DROPPED               *
      *   12 - FATAL, USER ABEND U921 THROUGH CBCRD91                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBREF03.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FRAUUNL-FILE ASSIGN TO FRAUUNL
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-FRAUUNL-STATUS.
      *
           SELECT RSNFEED-FILE ASSIGN TO RSNFEED
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-RSNFEED-STATUS.
      *
           SELECT RSNUNL-FILE ASSIGN TO RSNUNL
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-RSNUNL-STATUS.
      *
           SELECT REFCTL-FILE ASSIGN TO REFCTL
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REFCTL-STATUS.
      *
           SELECT FRAUDRUL-FILE ASSIGN TO FRAUDRUL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FRV-KEY
                  FILE STATUS  IS WS-FRAUDRUL-STATUS.
      *
           SELECT RSNCODE-FILE ASSIGN TO RSNCODE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS RSV-KEY
                  FILE STATUS  IS WS-RSNCODE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  FRAUUNL-FILE
           RECORD CONTAINS 240 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  FRAUUNL-REC                     PIC X(240).
      *
       FD  RSNFEED-FILE
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  RSNFEED-REC                     PIC X(100).
      *
       FD  RSNUNL-FILE
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  RSNUNL-REC                      PIC X(100).
      *
       FD  REFCTL-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REFCTL-REC.
           05  RC-LABEL                    PIC X(12).
           05  RC-COUNT                    PIC 9(9).
           05  RC-RUN-DATE                 PIC 9(8).
           05  FILLER                      PIC X(51).
      *
       FD  FRAUDRUL-FILE
           RECORD CONTAINS 240 CHARACTERS.
       01  FRAUDRUL-REC.
           05  FRV-KEY                     PIC X(8).
           05  FILLER                      PIC X(232).
      *
       FD  RSNCODE-FILE
           RECORD CONTAINS 100 CHARACTERS.
       01  RSNCODE-REC.
           05  RSV-KEY                     PIC X(8).
           05  FILLER                      PIC X(92).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBREF03 '.
      *
      ******************************************************************
      * FRAUDRUL VSAM RECORD - KSDS, KEY 8, LRECL 240                  *
      ******************************************************************
       01  FRAUD-RULE-RECORD.
           05  FR-RULE-ID                  PIC X(8).
           05  FR-RULE-CLASS               PIC X(4).
           05  FR-RULE-SEQ                 PIC 9(4).
           05  FR-HANDLER-PGM              PIC X(8).
           05  FR-THRESHOLD-AMT            PIC S9(11)V99 COMP-3.
           05  FR-THRESHOLD-CNT            PIC 9(4).
           05  FR-THRESHOLD-PCT            PIC S9(3)V99 COMP-3.
           05  FR-SCORE-POINTS             PIC 9(4).
           05  FR-ACTION-CD                PIC X(4).
           05  FR-EFF-DATE                 PIC 9(8).
           05  FR-EXP-DATE                 PIC 9(8).
           05  FR-ACTIVE-FLG               PIC X.
           05  FR-DESCRIPTION              PIC X(60).
           05  FR-MCC-LIST                 PIC X(80).
           05  FR-COUNTRY-LIST             PIC X(36).
           05  FR-FILLER                   PIC X.
      *
      ******************************************************************
      * RSNCODE VSAM RECORD - KSDS, KEY 8, LRECL 100                   *
      * THE FEED CARRIES THE SAME LAYOUT SO THE EDIT CAN WORK IN PLACE *
      ******************************************************************
       01  REASON-CODE-RECORD.
           05  RS-KEY.
               10  RS-CODE-TYPE            PIC X(4).
               10  RS-REASON-CD            PIC X(4).
           05  RS-DESCRIPTION              PIC X(40).
           05  RS-ACTION-CD                PIC X(4).
           05  RS-SEVERITY                 PIC X.
           05  RS-DISPLAY-TEXT             PIC X(30).
           05  RS-ACTIVE-FLG               PIC X.
           05  RS-LAST-MAINT-DATE          PIC 9(8).
           05  RS-FILLER                   PIC X(8).
      *
       01  WS-STATUSES.
           05  WS-FRAUUNL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-FRAUUNL-OK                     VALUE '00'.
           05  WS-RSNFEED-STATUS           PIC X(2)  VALUE '00'.
               88  WS-RSNFEED-OK                     VALUE '00'.
               88  WS-RSNFEED-EOF                    VALUE '10'.
           05  WS-RSNUNL-STATUS            PIC X(2)  VALUE '00'.
               88  WS-RSNUNL-OK                      VALUE '00'.
           05  WS-REFCTL-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REFCTL-OK                      VALUE '00'.
           05  WS-FRAUDRUL-STATUS          PIC X(2)  VALUE '00'.
               88  WS-FRAUDRUL-OK                    VALUE '00'.
               88  WS-FRAUDRUL-EOF                   VALUE '10'.
           05  WS-RSNCODE-STATUS           PIC X(2)  VALUE '00'.
               88  WS-RSNCODE-OK                     VALUE '00'.
               88  WS-RSNCODE-EOF                    VALUE '10'.
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
           05  WS-FRAU-DB2-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-FRAU-UNL-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-FRAU-VSAM-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-FRAU-CTL-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-RSN-READ-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-RSN-UNL-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-RSN-VSAM-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-RSN-CTL-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-RSN-DROP-CNT             PIC 9(9)  VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-RETURN-CD                PIC S9(4) COMP VALUE ZERO.
           05  WS-MSG                      PIC X(78) VALUE SPACES.
           05  WS-EDIT-REASON              PIC X(30) VALUE SPACES.
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
       01  DCL-RULE.
           05  DCL-RULE-ID                 PIC X(8).
           05  DCL-RULE-CLASS              PIC X(4).
           05  DCL-RULE-SEQ                PIC S9(4) COMP.
           05  DCL-HANDLER-PGM             PIC X(8).
           05  DCL-THRESHOLD-AMT           PIC S9(11)V99 COMP-3.
           05  DCL-THRESHOLD-CNT           PIC S9(4) COMP.
           05  DCL-THRESHOLD-PCT           PIC S9(3)V99 COMP-3.
           05  DCL-SCORE-POINTS            PIC S9(4) COMP.
           05  DCL-ACTION-CD               PIC X(4).
           05  DCL-MCC-LIST                PIC X(200).
           05  DCL-COUNTRY-LIST            PIC X(120).
           05  DCL-EFF-DATE                PIC X(10).
           05  DCL-EXP-DATE                PIC X(10).
           05  DCL-ACTIVE-FLG              PIC X.
           05  DCL-DESCRIPTION             PIC X(60).
           05  DCL-ROW-CNT                 PIC S9(9) COMP-3.
      *
       01  IND-VARS.
           05  IND-AMT                     PIC S9(4) COMP.
           05  IND-CNT                     PIC S9(4) COMP.
           05  IND-PCT                     PIC S9(4) COMP.
           05  IND-MCC                     PIC S9(4) COMP.
           05  IND-COUNTRY                 PIC S9(4) COMP.
           05  IND-DESC                    PIC S9(4) COMP.
      *
           EXEC SQL DECLARE FRAUCSR CURSOR FOR
               SELECT RULE_ID
                    , RULE_CLASS
                    , RULE_SEQ
                    , HANDLER_PGM
                    , THRESHOLD_AMT
                    , THRESHOLD_CNT
                    , THRESHOLD_PCT
                    , SCORE_POINTS
                    , ACTION_CD
                    , MCC_LIST
                    , COUNTRY_LIST
                    , CHAR(EFF_DATE, ISO)
                    , CHAR(EXP_DATE, ISO)
                    , ACTIVE_FLG
                    , DESCRIPTION
                 FROM CARDSVC.FRAUD_RULE
                WHERE ACTIVE_FLG = 'Y'
                ORDER BY RULE_ID
                WITH UR
           END-EXEC.
      *
       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-PARM-LEN                 PIC S9(4) COMP.
           05  LK-PARM-DATA                PIC X(8).
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
                   PERFORM 2000-UNLOAD-RULES
                   IF NOT WS-FATAL
                       PERFORM 3000-UNLOAD-REASONS
                   END-IF
                   IF NOT WS-FATAL
                       PERFORM 4000-WRITE-CONTROL
                   END-IF
               WHEN WS-VERIFY
                   PERFORM 5000-VERIFY-REBUILD
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
       1000-INITIALISE.
           IF LK-PARM-LEN = ZERO
               MOVE 'NO PARM PASSED TO CBREF03'
                                           TO WS-MSG
               PERFORM 9200-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           MOVE SPACES                     TO WS-FUNCTION
           MOVE LK-PARM-DATA(1:LK-PARM-LEN)
                                           TO WS-FUNCTION
           DISPLAY 'CBREF03 FUNCTION ' WS-FUNCTION
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - FRAUD RULES OUT OF DB2                                  *
      ******************************************************************
       2000-UNLOAD-RULES.
           OPEN OUTPUT FRAUUNL-FILE
           IF NOT WS-FRAUUNL-OK
               MOVE 'FRAUUNL OPEN FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-ROW-CNT
                 FROM CARDSVC.FRAUD_RULE
                WHERE ACTIVE_FLG = 'Y'
                WITH UR
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'COUNT OF CARDSVC.FRAUD_RULE FAILED'
                                           TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 2000-EXIT
           END-IF
           MOVE DCL-ROW-CNT                TO WS-FRAU-DB2-CNT
      *
           EXEC SQL
               OPEN FRAUCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'OPEN FRAUCSR FAILED'  TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               PERFORM 2100-FETCH-RULE
               IF NOT WS-EOF AND NOT WS-FATAL
                   PERFORM 2200-WRITE-RULE
               END-IF
           END-PERFORM
      *
           EXEC SQL
               CLOSE FRAUCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'CLOSE FRAUCSR FAILED' TO WS-MSG
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           CLOSE FRAUUNL-FILE
      *
           IF WS-FRAU-UNL-CNT NOT = WS-FRAU-DB2-CNT
               MOVE 'FRAUD RULE UNLOAD COUNT DOES NOT MATCH DB2'
                                           TO WS-MSG
               PERFORM 9200-FATAL
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-FETCH-RULE.
           EXEC SQL
               FETCH FRAUCSR
                INTO :DCL-RULE-ID
                   , :DCL-RULE-CLASS
                   , :DCL-RULE-SEQ
                   , :DCL-HANDLER-PGM
                   , :DCL-THRESHOLD-AMT :IND-AMT
                   , :DCL-THRESHOLD-CNT :IND-CNT
                   , :DCL-THRESHOLD-PCT :IND-PCT
                   , :DCL-SCORE-POINTS
                   , :DCL-ACTION-CD
                   , :DCL-MCC-LIST :IND-MCC
                   , :DCL-COUNTRY-LIST :IND-COUNTRY
                   , :DCL-EFF-DATE
                   , :DCL-EXP-DATE
                   , :DCL-ACTIVE-FLG
                   , :DCL-DESCRIPTION :IND-DESC
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'                TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'FETCH FRAUCSR FAILED'
                                           TO WS-MSG
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2200 - THE MCC AND COUNTRY LISTS ARE VARCHAR IN DB2 AND FIXED  *
      *        IN THE CLUSTER.  ANYTHING PAST THE FIXED LENGTH IS      *
      *        DROPPED AND REPORTED - THE ONLINE RULE HANDLERS HAVE    *
      *        NEVER READ MORE THAN THAT.                              *
      ******************************************************************
       2200-WRITE-RULE.
           MOVE SPACES                     TO FRAUD-RULE-RECORD
           MOVE DCL-RULE-ID                TO FR-RULE-ID
           MOVE DCL-RULE-CLASS             TO FR-RULE-CLASS
           MOVE DCL-RULE-SEQ               TO FR-RULE-SEQ
           MOVE DCL-HANDLER-PGM            TO FR-HANDLER-PGM
           MOVE DCL-ACTION-CD              TO FR-ACTION-CD
           MOVE DCL-SCORE-POINTS           TO FR-SCORE-POINTS
           MOVE DCL-ACTIVE-FLG             TO FR-ACTIVE-FLG
      *
           IF IND-AMT < ZERO
               MOVE ZERO                   TO FR-THRESHOLD-AMT
           ELSE
               MOVE DCL-THRESHOLD-AMT      TO FR-THRESHOLD-AMT
           END-IF
      *
           IF IND-CNT < ZERO
               MOVE ZERO                   TO FR-THRESHOLD-CNT
           ELSE
               MOVE DCL-THRESHOLD-CNT      TO FR-THRESHOLD-CNT
           END-IF
      *
           IF IND-PCT < ZERO
               MOVE ZERO                   TO FR-THRESHOLD-PCT
           ELSE
               MOVE DCL-THRESHOLD-PCT      TO FR-THRESHOLD-PCT
           END-IF
      *
           IF IND-MCC < ZERO
               MOVE SPACES                 TO FR-MCC-LIST
           ELSE
               MOVE DCL-MCC-LIST(1:80)     TO FR-MCC-LIST
               IF DCL-MCC-LIST(81:120) NOT = SPACES
                   DISPLAY 'CBREF03 MCC LIST TRUNCATED RULE '
                           DCL-RULE-ID
               END-IF
           END-IF
      *
           IF IND-COUNTRY < ZERO
               MOVE SPACES                 TO FR-COUNTRY-LIST
           ELSE
               MOVE DCL-COUNTRY-LIST(1:36) TO FR-COUNTRY-LIST
               IF DCL-COUNTRY-LIST(37:84) NOT = SPACES
                   DISPLAY 'CBREF03 COUNTRY LIST TRUNCATED RULE '
                           DCL-RULE-ID
               END-IF
           END-IF
      *
           IF IND-DESC < ZERO
               MOVE SPACES                 TO FR-DESCRIPTION
           ELSE
               MOVE DCL-DESCRIPTION        TO FR-DESCRIPTION
           END-IF
      *
           MOVE DCL-EFF-DATE(1:4)          TO WS-DW-YYYY
           MOVE DCL-EFF-DATE(6:2)          TO WS-DW-MM
           MOVE DCL-EFF-DATE(9:2)          TO WS-DW-DD
           MOVE WS-DATE-WORK               TO FR-EFF-DATE
      *
           MOVE DCL-EXP-DATE(1:4)          TO WS-DW-YYYY
           MOVE DCL-EXP-DATE(6:2)          TO WS-DW-MM
           MOVE DCL-EXP-DATE(9:2)          TO WS-DW-DD
           MOVE WS-DATE-WORK               TO FR-EXP-DATE
      *
           MOVE FRAUD-RULE-RECORD          TO FRAUUNL-REC
           WRITE FRAUUNL-REC
           IF NOT WS-FRAUUNL-OK
               MOVE 'FRAUUNL WRITE FAILED' TO WS-MSG
               PERFORM 9300-FILE-ERROR
           ELSE
               ADD 1                       TO WS-FRAU-UNL-CNT
           END-IF
           .
      *
      ******************************************************************
      * 3000 - REASON CODES OFF THE FLAT MASTER                        *
      ******************************************************************
       3000-UNLOAD-REASONS.
           OPEN INPUT  RSNFEED-FILE
           IF NOT WS-RSNFEED-OK
               MOVE 'RSNFEED OPEN FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           OPEN OUTPUT RSNUNL-FILE
           IF NOT WS-RSNUNL-OK
               MOVE 'RSNUNL OPEN FAILED'   TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               READ RSNFEED-FILE INTO REASON-CODE-RECORD
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-RSN-READ-CNT
                       PERFORM 3100-EDIT-REASON
               END-READ
      *
               IF NOT WS-RSNFEED-OK
               AND NOT WS-RSNFEED-EOF
                   MOVE 'RSNFEED READ FAILED'
                                           TO WS-MSG
                   PERFORM 9300-FILE-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE RSNFEED-FILE
                 RSNUNL-FILE
      *
           IF WS-RSN-UNL-CNT = ZERO
               MOVE 'REASON CODE FEED PRODUCED NO RECORDS'
                                           TO WS-MSG
               PERFORM 9200-FATAL
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
       3100-EDIT-REASON.
           MOVE SPACES                     TO WS-EDIT-REASON
      *
           EVALUATE TRUE
               WHEN RS-CODE-TYPE = SPACES
                   MOVE 'CODE TYPE IS BLANK'
                                           TO WS-EDIT-REASON
               WHEN RS-REASON-CD = SPACES
                   MOVE 'REASON CODE IS BLANK'
                                           TO WS-EDIT-REASON
               WHEN RS-ACTIVE-FLG NOT = 'Y'
                   MOVE 'CODE IS NOT ACTIVE'
                                           TO WS-EDIT-REASON
               WHEN RS-DESCRIPTION = SPACES
                   MOVE 'DESCRIPTION IS BLANK'
                                           TO WS-EDIT-REASON
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF WS-EDIT-REASON NOT = SPACES
               ADD 1                       TO WS-RSN-DROP-CNT
               DISPLAY 'CBREF03 REASON CODE DROPPED - '
                       RS-KEY ' ' WS-EDIT-REASON
               GO TO 3100-EXIT
           END-IF
      *
           IF RS-DISPLAY-TEXT = SPACES
               MOVE RS-DESCRIPTION(1:30)   TO RS-DISPLAY-TEXT
           END-IF
      *
           MOVE REASON-CODE-RECORD         TO RSNUNL-REC
           WRITE RSNUNL-REC
           IF NOT WS-RSNUNL-OK
               MOVE 'RSNUNL WRITE FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
           ELSE
               ADD 1                       TO WS-RSN-UNL-CNT
           END-IF
           .
       3100-EXIT.
           EXIT
           .
      *
       4000-WRITE-CONTROL.
           OPEN OUTPUT REFCTL-FILE
           IF NOT WS-REFCTL-OK
               MOVE 'REFCTL OPEN FAILED'   TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 4000-EXIT
           END-IF
      *
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE-WORK
      *
           MOVE 'FRAUDRULCNT '             TO RC-LABEL
           MOVE WS-FRAU-UNL-CNT            TO RC-COUNT
           MOVE WS-DATE-WORK               TO RC-RUN-DATE
           WRITE REFCTL-REC
      *
           MOVE 'RSNCODECNT  '             TO RC-LABEL
           MOVE WS-RSN-UNL-CNT             TO RC-COUNT
           MOVE WS-DATE-WORK               TO RC-RUN-DATE
           WRITE REFCTL-REC
      *
           IF NOT WS-REFCTL-OK
               MOVE 'REFCTL WRITE FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
           END-IF
      *
           CLOSE REFCTL-FILE
      *
           DISPLAY 'CBREF03 UNLOAD FRAUD=' WS-FRAU-UNL-CNT
                   ' REASON READ=' WS-RSN-READ-CNT
                   ' WRITTEN=' WS-RSN-UNL-CNT
                   ' DROPPED=' WS-RSN-DROP-CNT
      *
           IF WS-RSN-DROP-CNT > ZERO
               MOVE 4                      TO WS-RETURN-CD
           END-IF
           .
       4000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5000 - VERIFY BOTH CLUSTERS                                    *
      ******************************************************************
       5000-VERIFY-REBUILD.
           PERFORM 5100-READ-CONTROL
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5200-COUNT-FRAUDRUL
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5300-COUNT-RSNCODE
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
      *
           DISPLAY 'CBREF03 VERIFY FRAUD CTL=' WS-FRAU-CTL-CNT
                   ' VSAM=' WS-FRAU-VSAM-CNT
                   ' REASON CTL=' WS-RSN-CTL-CNT
                   ' VSAM=' WS-RSN-VSAM-CNT
      *
           EVALUATE TRUE
               WHEN WS-FRAU-VSAM-CNT NOT = WS-FRAU-CTL-CNT
                   MOVE 'FRAUDRUL COUNT DOES NOT MATCH THE UNLOAD'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
               WHEN WS-RSN-VSAM-CNT NOT = WS-RSN-CTL-CNT
                   MOVE 'RSNCODE COUNT DOES NOT MATCH THE UNLOAD'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
               WHEN OTHER
                   MOVE 0                  TO WS-RETURN-CD
                   DISPLAY 'CBREF03 BOTH CLUSTERS VERIFIED'
           END-EVALUATE
           .
       5000-EXIT.
           EXIT
           .
      *
       5100-READ-CONTROL.
           OPEN INPUT REFCTL-FILE
           IF NOT WS-REFCTL-OK
               MOVE 'REFCTL OPEN FAILED'   TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 5100-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF
               READ REFCTL-FILE
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       EVALUATE RC-LABEL
                           WHEN 'FRAUDRULCNT '
                               MOVE RC-COUNT
                                        TO WS-FRAU-CTL-CNT
                           WHEN 'RSNCODECNT  '
                               MOVE RC-COUNT
                                        TO WS-RSN-CTL-CNT
                           WHEN OTHER
                               DISPLAY 'CBREF03 UNKNOWN CONTROL '
                                       RC-LABEL
                       END-EVALUATE
               END-READ
           END-PERFORM
      *
           CLOSE REFCTL-FILE
      *
           IF WS-FRAU-CTL-CNT = ZERO
               MOVE 'NO FRAUDRUL CONTROL RECORD - UNLOAD DID NOT RUN'
                                           TO WS-MSG
               PERFORM 9200-FATAL
           END-IF
           .
       5100-EXIT.
           EXIT
           .
      *
       5200-COUNT-FRAUDRUL.
           OPEN INPUT FRAUDRUL-FILE
           IF NOT WS-FRAUDRUL-OK
               MOVE 'FRAUDRUL OPEN FAILED - NOT REBUILT'
                                           TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 5200-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               READ FRAUDRUL-FILE NEXT RECORD
                    INTO FRAUD-RULE-RECORD
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-FRAU-VSAM-CNT
               END-READ
      *
               IF NOT WS-FRAUDRUL-OK
               AND NOT WS-FRAUDRUL-EOF
                   MOVE 'FRAUDRUL READ FAILED'
                                           TO WS-MSG
                   PERFORM 9300-FILE-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE FRAUDRUL-FILE
           .
       5200-EXIT.
           EXIT
           .
      *
       5300-COUNT-RSNCODE.
           OPEN INPUT RSNCODE-FILE
           IF NOT WS-RSNCODE-OK
               MOVE 'RSNCODE OPEN FAILED - NOT REBUILT'
                                           TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 5300-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               READ RSNCODE-FILE NEXT RECORD
                    INTO REASON-CODE-RECORD
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-RSN-VSAM-CNT
               END-READ
      *
               IF NOT WS-RSNCODE-OK
               AND NOT WS-RSNCODE-EOF
                   MOVE 'RSNCODE READ FAILED'
                                           TO WS-MSG
                   PERFORM 9300-FILE-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE RSNCODE-FILE
           .
       5300-EXIT.
           EXIT
           .
      *
       9200-FATAL.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF03 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'DATA'                     TO ER-ERROR-TYPE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U921'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF03 FATAL - ' WS-MSG
           .
      *
       9300-FILE-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF03 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'VSAM'                     TO ER-ERROR-TYPE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U921'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF03 FILE ERROR - ' WS-MSG
                   ' FRAUUNL=' WS-FRAUUNL-STATUS
                   ' RSNFEED=' WS-RSNFEED-STATUS
                   ' RSNUNL=' WS-RSNUNL-STATUS
                   ' FRAUDRUL=' WS-FRAUDRUL-STATUS
                   ' RSNCODE=' WS-RSNCODE-STATUS
           .
      *
       9400-SQL-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF03 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           MOVE 'FRAUD_RULE        '       TO ER-SQL-TABLE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U922'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF03 SQL ERROR SQLCODE=' SQLCODE
                   ' - ' WS-MSG
           .
      *
       9500-ABEND.
           MOVE 12                         TO WS-RETURN-CD
           CALL 'CBCRD91' USING ERROR-AREA
           .
