      ******************************************************************
      * CBPRT02B - WATCH LIST EXTRACT                                  *
      *                                                                *
      * PART OF THE PARTYWK WEEKLY CYCLE, STEP TWO OF JOB CBPRT02J.    *
      *                                                                *
      * UNLOADS THE ACTIVE ROWS OF PARTYRSK.PARTY_SANCTION INTO A      *
      * FLAT FILE IN WATCH LIST FORMAT (CVWATC1Y, 160 BYTES).  THE     *
      * FILE IS SORTED AND REPRO'D INTO PRTY.PROD.WATCHLST BY THE      *
      * LATER STEPS OF THE JOB, AND THE ALTERNATE INDEX IS REBUILT     *
      * WITH BLDINDEX.  NOTHING IN THIS PROGRAM WRITES TO VSAM         *
      * DIRECTLY - REBUILDING THROUGH IDCAMS IS FASTER AND LEAVES A    *
      * CLEAN CLUSTER BEHIND.                                          *
      *                                                                *
      * TWO SEARCH KEYS ARE DERIVED FOR EACH ENTRY                     *
      *   WL-SEARCH-NAME  25 BYTES, FOLDED AND STRIPPED OF NOISE       *
      *   WL-SOUNDEX      4 BYTE SOUNDEX OF THE FIRST WORD             *
      * THE ONLINE SCREENING PATH MATCHES ON THE SEARCH NAME FIRST     *
      * AND FALLS BACK TO THE SOUNDEX CODE.                            *
      *                                                                *
      * RUN BY     - CBPRT02J STEP EXTWATCH                            *
      * FILES      - WATCHOUT  EXTRACT, FB 160                         *
      * TABLES     - PARTYRSK.PARTY_SANCTION   SELECT                  *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - EXTRACT WRITTEN                                         *
      *   04 - EXTRACT WRITTEN, ENTRIES SKIPPED                        *
      *   08 - EXTRACT EMPTY OR BELOW THE MINIMUM VOLUME - THE JOB     *
      *        MUST NOT REPRO AN EMPTY WATCH LIST OVER A GOOD ONE      *
      *   12 - FAILED                                                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBPRT02B.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WATCHOUT-FILE ASSIGN TO WATCHOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-OUT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  WATCHOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 160 CHARACTERS.
       01  WATCHOUT-REC                PIC X(160).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CBPRT02B'.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
      *
       01  WS-OUT-STATUS               PIC X(2)  VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X     VALUE 'N'.
               88  WS-EOF                        VALUE 'Y'.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
      *
       01  WS-TOTALS.
           05  WS-FETCH-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-WRITE-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-SKIP-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-INDIV-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-ORG-CNT              PIC 9(9)  VALUE ZERO.
      *
       01  WS-MINIMUM-VOLUME           PIC 9(9)  VALUE 000001000.
      *
       01  WS-EDIT-FIELDS.
           05  WS-ED-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-SQL-DISP             PIC -(9)9.
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-DATE-R REDEFINES WS-CURR-DATE.
               10  WS-CURR-CC          PIC 9(2).
               10  WS-CURR-YYMMDD      PIC 9(6).
      *
      ******************************************************************
      * NAME FOLDING WORK AREAS                                        *
      ******************************************************************
       01  WS-NAME-WORK.
           05  WS-IN-NAME              PIC X(60) VALUE SPACES.
           05  WS-IN-CHAR REDEFINES WS-IN-NAME
                                       PIC X OCCURS 60 TIMES.
           05  WS-FOLD-NAME            PIC X(60) VALUE SPACES.
           05  WS-FOLD-CHAR REDEFINES WS-FOLD-NAME
                                       PIC X OCCURS 60 TIMES.
           05  WS-FIRST-WORD           PIC X(20) VALUE SPACES.
           05  WS-FIRST-CHAR REDEFINES WS-FIRST-WORD
                                       PIC X OCCURS 20 TIMES.
           05  WS-IN-SUB               PIC S9(4) COMP VALUE ZERO.
           05  WS-OUT-SUB              PIC S9(4) COMP VALUE ZERO.
           05  WS-WORD-LEN             PIC S9(4) COMP VALUE ZERO.
      *
      *    NOISE WORDS DROPPED BEFORE THE SEARCH NAME IS BUILT.  THE
      *    LIST GREW ONE ENTRY AT A TIME AS FALSE POSITIVES CAME BACK
      *    FROM THE OPERATIONS TEAM.
       01  WS-NOISE-TABLE.
           05  FILLER                  PIC X(10) VALUE 'THE       '.
           05  FILLER                  PIC X(10) VALUE 'AND       '.
           05  FILLER                  PIC X(10) VALUE 'OF        '.
           05  FILLER                  PIC X(10) VALUE 'LTD       '.
           05  FILLER                  PIC X(10) VALUE 'LIMITED   '.
           05  FILLER                  PIC X(10) VALUE 'INC       '.
           05  FILLER                  PIC X(10) VALUE 'CO        '.
           05  FILLER                  PIC X(10) VALUE 'COMPANY   '.
           05  FILLER                  PIC X(10) VALUE 'CORP      '.
           05  FILLER                  PIC X(10) VALUE 'SA        '.
           05  FILLER                  PIC X(10) VALUE 'GMBH      '.
           05  FILLER                  PIC X(10) VALUE 'PLC       '.
       01  WS-NOISE-LIST REDEFINES WS-NOISE-TABLE.
           05  WS-NOISE-WORD OCCURS 12 TIMES PIC X(10).
      *
       01  WS-SOUNDEX-WORK.
           05  WS-SDX-CODE             PIC X(4)  VALUE SPACES.
           05  WS-SDX-DIGIT            PIC X     VALUE SPACE.
           05  WS-SDX-LAST             PIC X     VALUE SPACE.
           05  WS-SDX-POS              PIC S9(4) COMP VALUE ZERO.
           05  WS-SDX-SUB              PIC S9(4) COMP VALUE ZERO.
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
      *
       01  DCL-IND.
           05  IND-COUNTRY             PIC S9(4) COMP.
           05  IND-DOB                 PIC S9(4) COMP.
           05  IND-PROGRAM             PIC S9(4) COMP.
           05  IND-DELISTED            PIC S9(4) COMP.
      *
       01  WS-ISO-SCRATCH              PIC X(10) VALUE SPACES.
       01  WS-ISO-PARTS REDEFINES WS-ISO-SCRATCH.
           05  WS-IS-CCYY              PIC 9(4).
           05  FILLER                  PIC X.
           05  WS-IS-MM                PIC 9(2).
           05  FILLER                  PIC X.
           05  WS-IS-DD                PIC 9(2).
      *
       01  WS-NUM-DATE.
           05  WS-ND-CCYY              PIC 9(4).
           05  WS-ND-MM                PIC 9(2).
           05  WS-ND-DD                PIC 9(2).
      *
           COPY CVWATC1Y.
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      ******************************************************************
      * ACTIVE ENTRIES ONLY.  A DELISTED ENTRY STAYS ON THE TABLE FOR  *
      * AUDIT BUT MUST NOT REACH THE SCREENING FILE.                   *
      ******************************************************************
           EXEC SQL DECLARE SANCCSR CURSOR FOR
               SELECT LIST_CD
                    , ENTRY_ID
                    , ENTITY_NAME
                    , ENTITY_TYPE
                    , COUNTRY_CD
                    , CHAR(DOB, ISO)
                    , PROGRAM_CD
                    , CHAR(LISTED_DATE, ISO)
                    , CHAR(DELISTED_DATE, ISO)
                 FROM PARTYRSK.PARTY_SANCTION
                WHERE ACTIVE_FLG = 'Y'
                ORDER BY LIST_CD, ENTRY_ID
                 WITH UR
           END-EXEC.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
           PERFORM 1000-OPEN-CURSOR
      *
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               PERFORM 2000-EXTRACT-ONE
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
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
      *
           OPEN OUTPUT WATCHOUT-FILE
           IF WS-OUT-STATUS NOT = '00'
               MOVE 'WATCHOUT'         TO ER-FILE-NAME
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           DISPLAY 'CBPRT02B WATCH LIST EXTRACT STARTED DATE='
                   WS-CURR-DATE
           .
      *
       1000-OPEN-CURSOR.
           MOVE '1000-OPEN-CURSOR'     TO WS-PARAGRAPH
           EXEC SQL
               OPEN SANCCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_SANCTION   ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               PERFORM 9900-ABEND
           END-IF
           .
      *
       2000-EXTRACT-ONE.
           MOVE '2000-EXTRACT-ONE'     TO WS-PARAGRAPH
      *
           EXEC SQL
               FETCH SANCCSR
                INTO :DCL-LIST-CD
                   , :DCL-ENTRY-ID
                   , :DCL-ENTITY-NAME
                   , :DCL-ENTITY-TYPE
                   , :DCL-COUNTRY-CD    :IND-COUNTRY
                   , :DCL-DOB           :IND-DOB
                   , :DCL-PROGRAM-CD    :IND-PROGRAM
                   , :DCL-LISTED-DATE
                   , :DCL-DELISTED-DATE :IND-DELISTED
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-FETCH-CNT
                   PERFORM 2100-BUILD-RECORD
               WHEN +100
                   MOVE 'Y'            TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'PARTY_SANCTION   ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2100-BUILD-RECORD.
           MOVE SPACES                 TO WATCH-LIST-RECORD
           MOVE DCL-LIST-CD            TO WL-LIST-CD
           MOVE DCL-ENTRY-ID           TO WL-ENTRY-ID
           MOVE DCL-ENTITY-NAME        TO WL-ENTITY-NAME
           MOVE DCL-ENTITY-TYPE        TO WL-ENTITY-TYPE
      *
           IF IND-COUNTRY < ZERO
               MOVE 'ZZZ'              TO WL-COUNTRY
           ELSE
               MOVE DCL-COUNTRY-CD     TO WL-COUNTRY
           END-IF
      *
           IF IND-PROGRAM < ZERO
               MOVE SPACES             TO WL-PROGRAM-CD
           ELSE
               MOVE DCL-PROGRAM-CD     TO WL-PROGRAM-CD
           END-IF
      *
           IF IND-DOB < ZERO
               MOVE ZERO               TO WL-DOB
           ELSE
               MOVE DCL-DOB            TO WS-ISO-SCRATCH
               PERFORM 2150-ISO-TO-NUM
               MOVE WS-NUM-DATE        TO WL-DOB
           END-IF
      *
           MOVE DCL-LISTED-DATE        TO WS-ISO-SCRATCH
           PERFORM 2150-ISO-TO-NUM
           MOVE WS-NUM-DATE            TO WL-LISTED-DATE
      *
           IF IND-DELISTED < ZERO
               MOVE ZERO               TO WL-DELISTED-DATE
           ELSE
               MOVE DCL-DELISTED-DATE  TO WS-ISO-SCRATCH
               PERFORM 2150-ISO-TO-NUM
               MOVE WS-NUM-DATE        TO WL-DELISTED-DATE
           END-IF
      *
           MOVE 'Y'                    TO WL-ACTIVE-FLG
           MOVE WS-CURR-YYMMDD         TO WL-LOAD-DATE
      *
           PERFORM 2200-FOLD-NAME
           MOVE WS-FOLD-NAME(1:25)     TO WL-SEARCH-NAME
      *
           PERFORM 2300-FIRST-WORD
           PERFORM 2400-SOUNDEX
           MOVE WS-SDX-CODE            TO WL-SOUNDEX
      *
           IF WL-SEARCH-NAME = SPACES
      *        NOTHING LEFT AFTER FOLDING - THE ENTRY CANNOT BE
      *        SCREENED ON AND IS LEFT OUT OF THE FILE.
               ADD 1                   TO WS-SKIP-CNT
               DISPLAY 'CBPRT02B NO SEARCHABLE NAME LIST='
                       WL-LIST-CD ' ENTRY=' WL-ENTRY-ID
               GO TO 2100-EXIT
           END-IF
      *
           IF WL-ENTITY-TYPE = 'I'
               ADD 1                   TO WS-INDIV-CNT
           ELSE
               ADD 1                   TO WS-ORG-CNT
           END-IF
      *
           WRITE WATCHOUT-REC FROM WATCH-LIST-RECORD
           IF WS-OUT-STATUS NOT = '00'
               MOVE 'WATCHOUT'         TO ER-FILE-NAME
               MOVE 'WRITE   '         TO ER-SQL-OPERATION
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
           ADD 1                       TO WS-WRITE-CNT
           .
       2100-EXIT.
           EXIT
           .
      *
       2150-ISO-TO-NUM.
           MOVE WS-IS-CCYY             TO WS-ND-CCYY
           MOVE WS-IS-MM               TO WS-ND-MM
           MOVE WS-IS-DD               TO WS-ND-DD
           .
      *
      ******************************************************************
      * 2200 - FOLD THE ENTITY NAME                                    *
      *        LETTERS AND DIGITS ONLY, SINGLE SPACES, NOISE WORDS     *
      *        DROPPED.                                                *
      ******************************************************************
       2200-FOLD-NAME.
           MOVE DCL-ENTITY-NAME        TO WS-IN-NAME
           MOVE SPACES                 TO WS-FOLD-NAME
           MOVE ZERO                   TO WS-OUT-SUB
      *
           PERFORM VARYING WS-IN-SUB FROM 1 BY 1
                     UNTIL WS-IN-SUB > 60
               EVALUATE TRUE
                   WHEN WS-IN-CHAR(WS-IN-SUB) IS ALPHABETIC
                    AND WS-IN-CHAR(WS-IN-SUB) NOT = SPACE
                       ADD 1           TO WS-OUT-SUB
                       MOVE WS-IN-CHAR(WS-IN-SUB)
                                       TO WS-FOLD-CHAR(WS-OUT-SUB)
                   WHEN WS-IN-CHAR(WS-IN-SUB) IS NUMERIC
                       ADD 1           TO WS-OUT-SUB
                       MOVE WS-IN-CHAR(WS-IN-SUB)
                                       TO WS-FOLD-CHAR(WS-OUT-SUB)
                   WHEN WS-OUT-SUB > ZERO
                    AND WS-FOLD-CHAR(WS-OUT-SUB) NOT = SPACE
                       ADD 1           TO WS-OUT-SUB
                       MOVE SPACE      TO WS-FOLD-CHAR(WS-OUT-SUB)
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
      *
           INSPECT WS-FOLD-NAME CONVERTING
                   'abcdefghijklmnopqrstuvwxyz' TO
                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      *
           PERFORM 2250-DROP-NOISE
           .
      *
       2250-DROP-NOISE.
           PERFORM VARYING WS-IN-SUB FROM 1 BY 1 UNTIL WS-IN-SUB > 12
               INSPECT WS-FOLD-NAME
                   REPLACING ALL WS-NOISE-WORD(WS-IN-SUB)
                             BY SPACES
           END-PERFORM
      *
      *    SQUEEZE OUT THE GAPS LEFT BEHIND.
           MOVE WS-FOLD-NAME           TO WS-IN-NAME
           MOVE SPACES                 TO WS-FOLD-NAME
           MOVE ZERO                   TO WS-OUT-SUB
           PERFORM VARYING WS-IN-SUB FROM 1 BY 1
                     UNTIL WS-IN-SUB > 60
               IF WS-IN-CHAR(WS-IN-SUB) NOT = SPACE
                   ADD 1               TO WS-OUT-SUB
                   MOVE WS-IN-CHAR(WS-IN-SUB)
                                       TO WS-FOLD-CHAR(WS-OUT-SUB)
               ELSE
                   IF WS-OUT-SUB > ZERO
                      AND WS-FOLD-CHAR(WS-OUT-SUB) NOT = SPACE
                       ADD 1           TO WS-OUT-SUB
                       MOVE SPACE      TO WS-FOLD-CHAR(WS-OUT-SUB)
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       2300-FIRST-WORD.
           MOVE SPACES                 TO WS-FIRST-WORD
           MOVE ZERO                   TO WS-WORD-LEN
      *
           PERFORM VARYING WS-IN-SUB FROM 1 BY 1
                     UNTIL WS-IN-SUB > 20
                        OR WS-FOLD-CHAR(WS-IN-SUB) = SPACE
               ADD 1                   TO WS-WORD-LEN
               MOVE WS-FOLD-CHAR(WS-IN-SUB)
                                       TO WS-FIRST-CHAR(WS-WORD-LEN)
           END-PERFORM
           .
      *
      ******************************************************************
      * 2400 - SOUNDEX                                                 *
      *        STANDARD RUSSELL CODING, FOUR BYTES, ZERO FILLED.       *
      ******************************************************************
       2400-SOUNDEX.
           MOVE SPACES                 TO WS-SDX-CODE
           IF WS-WORD-LEN = ZERO
               MOVE '0000'             TO WS-SDX-CODE
               GO TO 2400-EXIT
           END-IF
      *
           MOVE '0000'                 TO WS-SDX-CODE
           MOVE WS-FIRST-CHAR(1)       TO WS-SDX-CODE(1:1)
           MOVE 1                      TO WS-SDX-POS
           MOVE SPACE                  TO WS-SDX-LAST
      *
           PERFORM VARYING WS-SDX-SUB FROM 2 BY 1
                     UNTIL WS-SDX-SUB > WS-WORD-LEN
                        OR WS-SDX-POS > 3
               PERFORM 2450-SOUNDEX-DIGIT
               IF WS-SDX-DIGIT NOT = SPACE
                  AND WS-SDX-DIGIT NOT = WS-SDX-LAST
                   ADD 1               TO WS-SDX-POS
                   MOVE WS-SDX-DIGIT   TO WS-SDX-CODE(WS-SDX-POS:1)
               END-IF
               MOVE WS-SDX-DIGIT       TO WS-SDX-LAST
           END-PERFORM
           .
       2400-EXIT.
           EXIT
           .
      *
       2450-SOUNDEX-DIGIT.
           EVALUATE WS-FIRST-CHAR(WS-SDX-SUB)
               WHEN 'B' WHEN 'F' WHEN 'P' WHEN 'V'
                   MOVE '1'            TO WS-SDX-DIGIT
               WHEN 'C' WHEN 'G' WHEN 'J' WHEN 'K'
               WHEN 'Q' WHEN 'S' WHEN 'X' WHEN 'Z'
                   MOVE '2'            TO WS-SDX-DIGIT
               WHEN 'D' WHEN 'T'
                   MOVE '3'            TO WS-SDX-DIGIT
               WHEN 'L'
                   MOVE '4'            TO WS-SDX-DIGIT
               WHEN 'M' WHEN 'N'
                   MOVE '5'            TO WS-SDX-DIGIT
               WHEN 'R'
                   MOVE '6'            TO WS-SDX-DIGIT
               WHEN OTHER
                   MOVE SPACE          TO WS-SDX-DIGIT
           END-EVALUATE
           .
      *
       3000-CLOSE-DOWN.
           MOVE '3000-CLOSE-DOWN'      TO WS-PARAGRAPH
      *
           EXEC SQL
               CLOSE SANCCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_SANCTION   ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
      *
           CLOSE WATCHOUT-FILE
           IF WS-OUT-STATUS NOT = '00'
               MOVE 'WATCHOUT'         TO ER-FILE-NAME
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9200-FILE-ERROR
           END-IF
           .
      *
       9000-REPORT-TOTALS.
           DISPLAY '******************************************'
           DISPLAY 'CBPRT02B WATCH LIST EXTRACT SUMMARY'
           MOVE WS-FETCH-CNT           TO WS-ED-COUNT
           DISPLAY '   ROWS FETCHED       ' WS-ED-COUNT
           MOVE WS-WRITE-CNT           TO WS-ED-COUNT
           DISPLAY '   RECORDS WRITTEN    ' WS-ED-COUNT
           MOVE WS-INDIV-CNT           TO WS-ED-COUNT
           DISPLAY '   INDIVIDUALS        ' WS-ED-COUNT
           MOVE WS-ORG-CNT             TO WS-ED-COUNT
           DISPLAY '   ORGANISATIONS      ' WS-ED-COUNT
           MOVE WS-SKIP-CNT            TO WS-ED-COUNT
           DISPLAY '   ENTRIES SKIPPED    ' WS-ED-COUNT
           DISPLAY '******************************************'
      *
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12             TO RETURN-CODE
               WHEN WS-WRITE-CNT < WS-MINIMUM-VOLUME
                   MOVE WS-WRITE-CNT   TO WS-ED-COUNT
                   DISPLAY 'CBPRT02B *** EXTRACT VOLUME ' WS-ED-COUNT
                           ' BELOW MINIMUM - WATCHLST NOT REBUILT'
                   MOVE 8              TO RETURN-CODE
               WHEN WS-SKIP-CNT > ZERO
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
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE 'Y'                    TO WS-FATAL-SW
      *
           DISPLAY 'CBPRT02B SQL ERROR TABLE=' ER-SQL-TABLE
                   ' OP=' ER-SQL-OPERATION
                   ' SQLCODE=' WS-SQL-DISP
           DISPLAY '         LAST ENTRY=' DCL-LIST-CD
                   ' ' DCL-ENTRY-ID
           DISPLAY '         SQLERRMC=' SQLERRMC(1:44)
           .
      *
       9200-FILE-ERROR.
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE WS-OUT-STATUS          TO ER-FILE-STATUS
           MOVE 'F'                    TO ER-SEVERITY
           DISPLAY 'CBPRT02B FILE ERROR ' ER-FILE-NAME
                   ' OP=' ER-SQL-OPERATION
                   ' STATUS=' WS-OUT-STATUS
           .
      *
      ******************************************************************
      * 9900 - U3122.                                                  *
      ******************************************************************
       9900-ABEND.
           MOVE 'U312'                 TO ER-ABEND-CODE
           DISPLAY 'CBPRT02B ABEND U3122 WRITTEN=' WS-WRITE-CNT
           MOVE 12                     TO RETURN-CODE
           STOP RUN
           .
