      ******************************************************************
      * PRRSK05 - LATEST SCORE AND EXPOSURE SUMMARY FOR AN INQUIRY     *
      *                                                                *
      * THE BOTTOM OF THE INQUIRY CROSSING.  READS THE MOST RECENT     *
      * RISK SCORE AND THE CURRENT EXPOSURE TOTALS FOR THE PARTY AND   *
      * PUTS THEM IN THE CROSSING COMMAREA.                            *
      *                                                                *
      * NOTHING ON THE PARTY IS CHANGED HERE.  NO SCORE IS CALCULATED  *
      * AND NO SCORE ROW IS WRITTEN - AN INQUIRY MUST NEVER MOVE A     *
      * PARTY RISK POSITION.  THE ONE INSERT IN THIS PROGRAM IS THE    *
      * REGULATORY ACCESS RECORD ON THE AUDIT TABLE, WHICH COMPLIANCE  *
      * REQUIRE FOR EVERY LOOK AT A PARTY FROM OUTSIDE PARTYRSK.       *
      * LIKE THE AUTHORISATION CHAIN, THIS PROGRAM COMMITS NOTHING -   *
      * THE UNIT OF WORK BELONGS TO THE CALLING TASK.                  *
      *                                                                *
      * CALLED BY   - PRKYC04                                          *
      * CALLS       - PRERR01  PARTYRSK ERROR HANDLER                  *
      * TABLES      - PARTYRSK.PARTY_RISK_SCORE      (SELECT)          *
      *               PARTYRSK.PARTY_EXPOSURE        (SELECT)          *
      *               PARTYRSK.PARTY_RISK_AUDIT      (INSERT)          *
      * COMMAREA    - CV-RISK-AREA, 512 BYTES, CVRISK01Y               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRRSK05.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRRSK05 '.
       01  WS-ERROR-PGM                PIC X(8)  VALUE 'PRERR01 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-COMMAREA-LEN             PIC S9(4) COMP VALUE 512.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-SCORE-SW             PIC X     VALUE 'N'.
               88  WS-SCORE-FOUND                VALUE 'Y'.
           05  WS-EXPO-SW              PIC X     VALUE 'N'.
               88  WS-EXPO-FOUND                 VALUE 'Y'.
      *
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(8)  VALUE SPACES.
           05  WS-TIME-HMS             PIC X(6)  VALUE SPACES.
      *
       01  WS-TODAY                    PIC 9(8)  VALUE ZERO.
       01  WS-TODAY-R REDEFINES WS-TODAY.
           05  WS-TD-CC                PIC 9(2).
           05  WS-TD-YYMMDD            PIC 9(6).
      *
       01  WS-ISO-DATE                 PIC X(10) VALUE SPACES.
       01  WS-ISO-DATE-R REDEFINES WS-ISO-DATE.
           05  WS-ISO-CCYY             PIC X(4).
           05  FILLER                  PIC X.
           05  WS-ISO-MM               PIC X(2).
           05  FILLER                  PIC X.
           05  WS-ISO-DD               PIC X(2).
      *
       01  WS-WORK-DATE                PIC 9(8)  VALUE ZERO.
       01  WS-WORK-DATE-R REDEFINES WS-WORK-DATE.
           05  WS-WD-CCYY              PIC 9(4).
           05  WS-WD-MM                PIC 9(2).
           05  WS-WD-DD                PIC 9(2).
      *
       01  WS-AUDIT-ID.
           05  FILLER                  PIC X     VALUE 'Q'.
           05  WS-AU-DATE              PIC 9(6)  VALUE ZERO.
           05  WS-AU-TIME              PIC 9(6)  VALUE ZERO.
           05  WS-AU-TASK              PIC 9(7)  VALUE ZERO.
      *
       01  WS-HOP-WORK.
           05  WS-HOP-SUB              PIC S9(4) COMP VALUE 0.
           05  WS-HOP-POS              PIC S9(4) COMP VALUE 1.
           05  WS-HOP-TEXT             PIC X(120) VALUE SPACES.
      *
       01  WS-HOP-PAIR.
           05  WS-HP-PGM               PIC X(8).
           05  FILLER                  PIC X     VALUE '/'.
           05  WS-HP-RC                PIC 9(4).
           05  FILLER                  PIC X     VALUE SPACE.
      *
       01  WS-SCORE-AGE-DAYS           PIC S9(5) COMP-3 VALUE 0.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
       01  HV-KEY.
           05  HV-PARTY-ID             PIC X(11).
      *
       01  HV-SCORE.
           05  HV-RISK-SCORE           PIC S9(4) COMP.
           05  HV-RISK-BAND            PIC X(1).
           05  HV-MODEL-ID             PIC X(8).
           05  HV-ADVICE-CD            PIC X(4).
           05  HV-REASON-CD            PIC X(4).
           05  HV-SANCTION-FLG         PIC X(1).
           05  HV-SCORE-DATE           PIC X(10).
           05  HV-SCORE-DAYS           PIC S9(9) COMP.
      *
       01  HV-EXPO.
           05  HV-TOTAL-DRAWN          PIC S9(13)V99 COMP-3.
           05  HV-TOTAL-AVAIL          PIC S9(13)V99 COMP-3.
           05  HV-TOTAL-LIMIT          PIC S9(13)V99 COMP-3.
           05  HV-PAST-DUE             PIC S9(11)V99 COMP-3.
           05  HV-ROW-CNT              PIC S9(9) COMP.
           05  HV-STALE-CNT            PIC S9(9) COMP.
      *
       01  HV-AUDIT.
           05  HV-AUDIT-ID             PIC X(20).
           05  HV-EVENT-TYPE           PIC X(4).
           05  HV-REQ-MODULE           PIC X(8).
           05  HV-REQ-PGM              PIC X(8).
           05  HV-CORREL-ID            PIC X(16).
           05  HV-CHANNEL              PIC X(1).
           05  HV-KYC-STATUS           PIC X(2).
           05  HV-REASON-TXT           PIC X(60).
      *
       01  HV-HOP-TRACE.
           49  HV-HT-LEN               PIC S9(4) COMP.
           49  HV-HT-TEXT              PIC X(120).
      *
       01  HV-INDICATORS.
           05  IND-REASON-CD           PIC S9(4) COMP.
           05  IND-DRAWN               PIC S9(4) COMP.
           05  IND-AVAIL               PIC S9(4) COMP.
           05  IND-LIMIT               PIC S9(4) COMP.
           05  IND-PAST-DUE            PIC S9(4) COMP.
           05  IND-SCORE               PIC S9(4) COMP.
           05  IND-BAND                PIC S9(4) COMP.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
           COPY CVRISK01Y.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           IF EIBCALEN < WS-COMMAREA-LEN
               EXEC CICS ABEND ABCODE('PRR5') NODUMP END-EXEC
           END-IF
      *
           SET ADDRESS OF CV-RISK-AREA TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-INITIALISE
           PERFORM 2000-READ-LATEST-SCORE
           PERFORM 3000-READ-EXPOSURE
           PERFORM 4000-SUMMARISE
           PERFORM 5000-WRITE-ACCESS-AUDIT
           PERFORM 7000-ADD-HOP
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - INITIALISE                                              *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-SCORE-SW
           MOVE 'N'                    TO WS-EXPO-SW
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'N'                    TO ER-ABEND-REQUESTED
      *
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-CYMD)
                     TIME(WS-TIME-HMS)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE WS-DATE-CYMD           TO WS-TODAY
      *
           MOVE WS-TD-YYMMDD           TO WS-AU-DATE
           MOVE WS-TIME-HMS            TO WS-AU-TIME
           MOVE EIBTASKN               TO WS-AU-TASK
      *
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
      *
           MOVE ZERO                   TO HV-TOTAL-DRAWN
           MOVE ZERO                   TO HV-TOTAL-AVAIL
           MOVE ZERO                   TO HV-TOTAL-LIMIT
           MOVE ZERO                   TO HV-PAST-DUE
           .
      *
      ******************************************************************
      * 2000 - MOST RECENT SCORE                                       *
      *                                                                *
      * THE SCORE IS TAKEN AS IT STANDS.  IT IS NOT RECALCULATED FOR   *
      * AN INQUIRY EVEN WHEN IT IS OLD - THE AGE IS REPORTED INSTEAD   *
      * SO THE OPERATOR CAN SEE HOW MUCH TO TRUST IT.                  *
      ******************************************************************
       2000-READ-LATEST-SCORE.
           EXEC SQL
               SELECT RISK_SCORE
                    , RISK_BAND
                    , MODEL_ID
                    , ADVICE_CD
                    , REASON_CD
                    , SANCTION_FLG
                    , CHAR(DATE(SCORE_TS), ISO)
                    , DAYS(CURRENT DATE) - DAYS(DATE(SCORE_TS))
                 INTO :HV-RISK-SCORE
                    , :HV-RISK-BAND
                    , :HV-MODEL-ID
                    , :HV-ADVICE-CD
                    , :HV-REASON-CD :IND-REASON-CD
                    , :HV-SANCTION-FLG
                    , :HV-SCORE-DATE
                    , :HV-SCORE-DAYS
                 FROM PARTYRSK.PARTY_RISK_SCORE
                WHERE PARTY_ID = :HV-PARTY-ID
                ORDER BY SCORE_TS DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-SCORE-SW
                   MOVE HV-SCORE-DAYS  TO WS-SCORE-AGE-DAYS
               WHEN +100
      *            A PARTY THAT HAS NEVER BEEN THROUGH THE
      *            AUTHORISATION CHAIN HAS NO SCORE ON FILE.  THAT
      *            IS NORMAL FOR A NEW RELATIONSHIP.
                   MOVE ZERO           TO WS-SCORE-AGE-DAYS
               WHEN OTHER
                   MOVE 'PARTY_RISK_SCORE  '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '2000-READ-LATEST-SCORE'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - CURRENT EXPOSURE TOTALS                                 *
      *                                                                *
      * THE WHOLE PARTY POSITION AT THE LATEST WAREHOUSE DATE, ALL     *
      * PRODUCT SYSTEMS TOGETHER.  UNLIKE THE AUTHORISATION CHAIN NO   *
      * LIVE FALLBACK IS DONE HERE - AN INQUIRY SHOWS WHAT THE         *
      * WAREHOUSE HOLDS AND SAYS SO WHEN IT IS STALE.                  *
      ******************************************************************
       3000-READ-EXPOSURE.
           EXEC SQL
               SELECT COUNT(*)
                    , SUM(TOTAL_DRAWN)
                    , SUM(TOTAL_AVAILABLE)
                    , SUM(TOTAL_LIMIT)
                    , SUM(PAST_DUE_AMT)
                    , SUM(CASE WHEN STALE_FLG = 'Y'
                               THEN 1 ELSE 0 END)
                 INTO :HV-ROW-CNT
                    , :HV-TOTAL-DRAWN :IND-DRAWN
                    , :HV-TOTAL-AVAIL :IND-AVAIL
                    , :HV-TOTAL-LIMIT :IND-LIMIT
                    , :HV-PAST-DUE    :IND-PAST-DUE
                    , :HV-STALE-CNT
                 FROM PARTYRSK.PARTY_EXPOSURE
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND AS_OF_DATE =
                      (SELECT MAX(AS_OF_DATE)
                         FROM PARTYRSK.PARTY_EXPOSURE
                        WHERE PARTY_ID = :HV-PARTY-ID)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF HV-ROW-CNT > ZERO
                       MOVE 'Y'        TO WS-EXPO-SW
                   END-IF
                   IF IND-DRAWN < ZERO
                       MOVE ZERO       TO HV-TOTAL-DRAWN
                   END-IF
                   IF IND-AVAIL < ZERO
                       MOVE ZERO       TO HV-TOTAL-AVAIL
                   END-IF
                   IF IND-LIMIT < ZERO
                       MOVE ZERO       TO HV-TOTAL-LIMIT
                   END-IF
                   IF IND-PAST-DUE < ZERO
                       MOVE ZERO       TO HV-PAST-DUE
                   END-IF
               WHEN +100
                   MOVE ZERO           TO HV-ROW-CNT
                   MOVE ZERO           TO HV-STALE-CNT
               WHEN OTHER
                   MOVE 'PARTY_EXPOSURE    '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '3000-READ-EXPOSURE'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 4000 - PUBLISH THE SUMMARY                                     *
      ******************************************************************
       4000-SUMMARISE.
           IF WS-ERROR-FOUND
               GO TO 4000-EXIT
           END-IF
      *
           IF WS-SCORE-FOUND
               MOVE HV-RISK-SCORE      TO CV-RISK-SCORE
               MOVE HV-RISK-BAND       TO CV-RISK-BAND
               MOVE HV-MODEL-ID        TO CV-RISK-MODEL-ID
               MOVE HV-SANCTION-FLG    TO CV-RISK-SANCTION-FLG
               MOVE HV-SCORE-DATE      TO WS-ISO-DATE
               PERFORM 9200-ISO-TO-NUMERIC
               MOVE WS-WORK-DATE       TO CV-RISK-SCORE-DATE
           ELSE
               MOVE ZERO               TO CV-RISK-SCORE
               MOVE SPACE              TO CV-RISK-BAND
               MOVE SPACES             TO CV-RISK-MODEL-ID
               MOVE ZERO               TO CV-RISK-SCORE-DATE
           END-IF
      *
           MOVE HV-TOTAL-DRAWN         TO CV-RISK-EXPOSURE-AMT
           MOVE HV-TOTAL-AVAIL         TO CV-RISK-AVAIL-AMT
      *
      *    THE INQUIRY REASON TEXT IS THE ONE PLACE THE OPERATOR IS
      *    TOLD HOW GOOD THE PICTURE IS.  THE MOST SERIOUS CONDITION
      *    WINS - STALE DATA IS WORSE THAN AN OLD SCORE BECAUSE THE
      *    NUMBERS ON THE SCREEN ARE THEN WRONG RATHER THAN JUST OLD.
           EVALUATE TRUE
               WHEN NOT WS-EXPO-FOUND
                   MOVE 'NEXP'         TO CV-RISK-REASON-CD
                   MOVE 'NO EXPOSURE POSITION HELD FOR THIS PARTY'
                                       TO CV-RISK-REASON-TXT
                   IF CV-RISK-RC < 0004
                       MOVE 0004       TO CV-RISK-RC
                   END-IF
               WHEN HV-STALE-CNT > ZERO
                   MOVE 'STAL'         TO CV-RISK-REASON-CD
                   MOVE 'EXPOSURE FEED IS STALE - POSITION MAY BE OLD'
                                       TO CV-RISK-REASON-TXT
                   IF CV-RISK-RC < 0004
                       MOVE 0004       TO CV-RISK-RC
                   END-IF
               WHEN NOT WS-SCORE-FOUND
                   MOVE 'NSCR'         TO CV-RISK-REASON-CD
                   MOVE 'NO RISK SCORE HELD FOR THIS PARTY'
                                       TO CV-RISK-REASON-TXT
                   IF CV-RISK-RC < 0004
                       MOVE 0004       TO CV-RISK-RC
                   END-IF
               WHEN WS-SCORE-AGE-DAYS > 90
                   MOVE 'SAGE'         TO CV-RISK-REASON-CD
                   MOVE 'RISK SCORE IS MORE THAN 90 DAYS OLD'
                                       TO CV-RISK-REASON-TXT
                   IF CV-RISK-RC < 0004
                       MOVE 0004       TO CV-RISK-RC
                   END-IF
               WHEN OTHER
                   MOVE 'OK  '         TO CV-RISK-REASON-CD
           END-EVALUATE
           .
       4000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5000 - REGULATORY ACCESS RECORD                                *
      *                                                                *
      * WRITTEN WHATEVER THE OUTCOME.  A FAILED INQUIRY IS STILL AN    *
      * ACCESS TO THE PARTY RECORD AND HAS TO BE ON THE TRAIL.  THE    *
      * SCORE COLUMNS CARRY THE VALUES THAT WERE SHOWN, NOT NEW ONES - *
      * OLD AND NEW ARE THE SAME BECAUSE NOTHING MOVED.                *
      ******************************************************************
       5000-WRITE-ACCESS-AUDIT.
           PERFORM 5100-BUILD-HOP-TRACE
      *
           MOVE WS-AUDIT-ID            TO HV-AUDIT-ID
           MOVE 'INQY'                 TO HV-EVENT-TYPE
           MOVE CV-RISK-CALLER-MOD     TO HV-REQ-MODULE
           MOVE CV-RISK-CALLER-ID      TO HV-REQ-PGM
           MOVE CV-RISK-CORREL-ID      TO HV-CORREL-ID
           MOVE CV-RISK-CHANNEL        TO HV-CHANNEL
           MOVE CV-RISK-KYC-STATUS     TO HV-KYC-STATUS
           MOVE CV-RISK-REASON-TXT     TO HV-REASON-TXT
      *
           IF HV-REQ-MODULE = SPACES
               MOVE 'UNKNOWN '         TO HV-REQ-MODULE
           END-IF
           IF HV-REQ-PGM = SPACES
               MOVE 'UNKNOWN '         TO HV-REQ-PGM
           END-IF
           IF HV-CHANNEL = SPACE
               MOVE 'O'                TO HV-CHANNEL
           END-IF
      *
           IF WS-SCORE-FOUND
               MOVE ZERO               TO IND-SCORE
               MOVE ZERO               TO IND-BAND
           ELSE
               MOVE -1                 TO IND-SCORE
               MOVE -1                 TO IND-BAND
               MOVE ZERO               TO HV-RISK-SCORE
               MOVE SPACE              TO HV-RISK-BAND
           END-IF
      *
           EXEC SQL
               INSERT INTO PARTYRSK.PARTY_RISK_AUDIT
                     (AUDIT_ID
                    , PARTY_ID
                    , EVENT_TS
                    , EVENT_TYPE
                    , REQUESTING_MODULE
                    , REQUESTING_PGM
                    , CORREL_ID
                    , CHANNEL
                    , OLD_SCORE
                    , NEW_SCORE
                    , OLD_BAND
                    , NEW_BAND
                    , KYC_STATUS
                    , SANCTION_FLG
                    , ADVICE_CD
                    , REASON_TXT
                    , HOP_TRACE)
               VALUES (:HV-AUDIT-ID
                    , :HV-PARTY-ID
                    , CURRENT TIMESTAMP
                    , :HV-EVENT-TYPE
                    , :HV-REQ-MODULE
                    , :HV-REQ-PGM
                    , :HV-CORREL-ID
                    , :HV-CHANNEL
                    , :HV-RISK-SCORE :IND-SCORE
                    , :HV-RISK-SCORE :IND-SCORE
                    , :HV-RISK-BAND  :IND-BAND
                    , :HV-RISK-BAND  :IND-BAND
                    , :HV-KYC-STATUS
                    , :HV-SANCTION-FLG
                    , ' '
                    , :HV-REASON-TXT
                    , :HV-HOP-TRACE)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
      *            TWO INQUIRIES IN THE SAME TASK AND THE SAME
      *            SECOND.  THE SECOND ACCESS IS ALREADY COVERED BY
      *            THE FIRST ROW SO IT IS LET GO.
                   CONTINUE
               WHEN OTHER
                   MOVE 'PARTY_RISK_AUDIT  '
                                       TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE '5000-WRITE-ACCESS-AUDIT'
                                       TO ER-PARAGRAPH
                   MOVE 'F'            TO ER-SEVERITY
                   PERFORM 8000-SQL-ERROR
                   MOVE 'AUD2'         TO CV-RISK-REASON-CD
                   MOVE 'ACCESS TRAIL NOT WRITTEN - INQUIRY REFUSED'
                                       TO CV-RISK-REASON-TXT
           END-EVALUATE
           .
      *
       5100-BUILD-HOP-TRACE.
           MOVE SPACES                 TO WS-HOP-TEXT
           MOVE 1                      TO WS-HOP-POS
      *
           PERFORM VARYING WS-HOP-SUB FROM 1 BY 1
                     UNTIL WS-HOP-SUB > CV-RISK-HOP-CNT
                        OR WS-HOP-SUB > 8
                        OR WS-HOP-POS > 106
               MOVE CV-RISK-HOP-PGM(WS-HOP-SUB)
                                       TO WS-HP-PGM
               MOVE CV-RISK-HOP-RC(WS-HOP-SUB)
                                       TO WS-HP-RC
               MOVE WS-HOP-PAIR        TO WS-HOP-TEXT(WS-HOP-POS:14)
               ADD 14                  TO WS-HOP-POS
           END-PERFORM
      *
           IF WS-HOP-POS <= 106
               MOVE WS-PGM-ID          TO WS-HP-PGM
               MOVE CV-RISK-RC         TO WS-HP-RC
               MOVE WS-HOP-PAIR        TO WS-HOP-TEXT(WS-HOP-POS:14)
           END-IF
      *
           MOVE WS-HOP-TEXT            TO HV-HT-TEXT
           MOVE 120                    TO HV-HT-LEN
           .
      *
      ******************************************************************
      * 7000 - HOP TRACE                                               *
      ******************************************************************
       7000-ADD-HOP.
           IF CV-RISK-HOP-CNT < 8
               ADD 1                   TO CV-RISK-HOP-CNT
               MOVE WS-PGM-ID          TO
                                    CV-RISK-HOP-PGM(CV-RISK-HOP-CNT)
               MOVE CV-RISK-RC         TO
                                    CV-RISK-HOP-RC(CV-RISK-HOP-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 8000 / 9000 - DIAGNOSTICS                                      *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           IF ER-SEVERITY NOT = 'F'
               MOVE 'E'                TO ER-SEVERITY
           END-IF
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'INQUIRY SUMMARY READ OR AUDIT WRITE FAILED'
                                       TO ER-MESSAGE
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           IF CV-RISK-REASON-CD = SPACES
               MOVE 'SQL7'             TO CV-RISK-REASON-CD
           END-IF
           PERFORM 9000-REPORT-ERROR
           .
      *
       9000-REPORT-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
      ******************************************************************
      * 9200 - ISO DATE TO THE NUMERIC FORM THE COPYBOOKS USE          *
      ******************************************************************
       9200-ISO-TO-NUMERIC.
           IF WS-ISO-DATE = SPACES
               MOVE ZERO               TO WS-WORK-DATE
           ELSE
               MOVE WS-ISO-CCYY        TO WS-WD-CCYY
               MOVE WS-ISO-MM          TO WS-WD-MM
               MOVE WS-ISO-DD          TO WS-WD-DD
           END-IF
           .
