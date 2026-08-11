      ******************************************************************
      * CAFRD02 - FRAUD RULE HANDLER - GEOGRAPHY AND MCC               *
      *                                                                *
      * ROUTE FRAU / STANDARD SEQ 2 AND ROUTE FRAU / HIGHRISK SEQ 2.   *
      *                                                                *
      * CALLED BY  - CACRD07 BY DYNAMIC CALL, NAME RESOLVED FROM       *
      *              CARDSVC.PGM_ROUTE                                 *
      * CALLS      - NONE                                              *
      * TABLES     - CARDSVC.FRAUD_RULE     SELECT                     *
      *              CARDSVC.AUTHORIZATION  SELECT                     *
      *              CARDSVC.MERCHANT       SELECT                     *
      *                                                                *
      * PARAMETERS - AUTH-RECORD     CVAUTH01Y                         *
      *              FRAUD-WORK-AREA CVFRAU1Y                          *
      *                                                                *
      * THE COUNTRY AND MCC LISTS ARE HELD AS COMMA SEPARATED          *
      * VARCHARS ON THE RULE ROW.  THEY ARE SCANNED HERE - AN SQL      *
      * PREDICATE CANNOT BE BUILT AGAINST THEM WITHOUT DYNAMIC SQL     *
      * AND THIS PROGRAM IS BOUND STATIC.                              *
      *                                                                *
      * ORIGINAL 1999 VERSION.                                         *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAFRD02.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CAFRD02 '.
       01  WS-RULE-CLASS               PIC X(4)  VALUE 'GEOG'.
      *
       01  WS-SWITCHES.
           05  WS-END-SW               PIC X     VALUE 'N'.
               88  WS-END-OF-RULES               VALUE 'Y'.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-FIRED-SW             PIC X     VALUE 'N'.
               88  WS-RULE-FIRED                 VALUE 'Y'.
           05  WS-MATCH-SW             PIC X     VALUE 'N'.
               88  WS-ENTRY-MATCHED              VALUE 'Y'.
           05  WS-PRIOR-SW             PIC X     VALUE 'N'.
               88  WS-PRIOR-FOUND                VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-POINTS-ADDED         PIC 9(4)  VALUE ZERO.
           05  WS-RULES-READ           PIC 9(4)  VALUE ZERO.
           05  WS-LIST-POS             PIC S9(4) COMP VALUE 1.
           05  WS-LIST-LEN             PIC S9(4) COMP VALUE 0.
      *
      *    THE LIST SCAN.  ENTRIES ARE THREE OR FOUR BYTES SEPARATED
      *    BY COMMAS - 'USA,CAN,MEX' OR '5967,7995,6011'.
       01  WS-LIST-WORK.
           05  WS-LIST-TEXT            PIC X(200) VALUE SPACES.
           05  WS-LIST-ENTRY           PIC X(4)  VALUE SPACES.
           05  WS-LIST-TARGET          PIC X(4)  VALUE SPACES.
           05  WS-LIST-IDX             PIC S9(4) COMP VALUE 0.
           05  WS-ENTRY-LEN            PIC S9(4) COMP VALUE 0.
      *
      *    IMPOSSIBLE TRAVEL.  THE TEST IS CRUDE - IT LOOKS AT THE
      *    ELAPSED MINUTES BETWEEN TWO AUTHORISATIONS IN DIFFERENT
      *    COUNTRIES AND NOT AT THE DISTANCE BETWEEN THEM.
       01  WS-TRAVEL.
           05  WS-PRIOR-COUNTRY        PIC X(3)  VALUE SPACES.
           05  WS-PRIOR-TS             PIC X(26) VALUE SPACES.
           05  WS-ELAPSED-MIN          PIC S9(9) COMP-3 VALUE ZERO.
           05  WS-MIN-TRAVEL-MIN       PIC S9(4) COMP VALUE 240.
      *
       01  WS-SEVERITY-WORK.
           05  WS-OLD-RANK             PIC 9     VALUE ZERO.
           05  WS-NEW-RANK             PIC 9     VALUE ZERO.
      *
       01  WS-AUTH-COUNTRY             PIC X(3)  VALUE SPACES.
       01  WS-AUTH-MCC                 PIC X(4)  VALUE SPACES.
       01  WS-MCC-EDIT                 PIC 9(4)  VALUE ZERO.
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-RULE.
           05  DCL-RULE-ID             PIC X(8).
           05  DCL-RULE-CLASS          PIC X(4).
           05  DCL-SCORE-POINTS        PIC S9(4) COMP.
           05  DCL-ACTION-CD           PIC X(4).
           05  DCL-THRESH-CNT          PIC S9(4) COMP.
           05  DCL-DESCRIPTION         PIC X(60).
      *
       01  DCL-MCC-LIST.
           49  DCL-MCC-LEN             PIC S9(4) COMP.
           49  DCL-MCC-TEXT            PIC X(200).
      *
       01  DCL-CTRY-LIST.
           49  DCL-CTRY-LEN            PIC S9(4) COMP.
           49  DCL-CTRY-TEXT           PIC X(120).
      *
       01  IND-RULE.
           05  IND-MCC-LIST            PIC S9(4) COMP.
           05  IND-CTRY-LIST           PIC S9(4) COMP.
           05  IND-THRESH-CNT          PIC S9(4) COMP.
      *
       01  DCL-KEYS.
           05  DCL-CARD-NUM            PIC X(16).
           05  DCL-MERCHANT-ID         PIC X(15).
      *
       01  DCL-PRIOR.
           05  DCL-PRIOR-COUNTRY       PIC X(3).
           05  DCL-PRIOR-MINUTES       PIC S9(9) COMP.
      *
       01  DCL-MERCH.
           05  DCL-MER-COUNTRY         PIC X(3).
           05  DCL-MER-MCC             PIC X(4).
      *
       LINKAGE SECTION.
           COPY CVAUTH01Y.
           COPY CVFRAU1Y.
      *
      ******************************************************************
       PROCEDURE DIVISION USING AUTH-RECORD
                                FRAUD-WORK-AREA.
      *
       0000-MAIN-LINE.
           PERFORM 1000-INITIALISE     THRU 1000-EXIT
           IF WS-ERROR-FOUND
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 2000-READ-MERCHANT  THRU 2000-EXIT
           PERFORM 3000-READ-PRIOR     THRU 3000-EXIT
           PERFORM 4000-APPLY-RULES    THRU 4000-EXIT
           PERFORM 6000-POST-RESULT    THRU 6000-EXIT
           .
       0000-RETURN.
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - PICK THE COUNTRY AND MCC OUT OF THE VARIANT             *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-END-SW
           MOVE 'N'                    TO WS-FIRED-SW
           MOVE 'N'                    TO WS-PRIOR-SW
           MOVE ZERO                   TO WS-POINTS-ADDED
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
      *
           MOVE ZERO                   TO FW-SCORE-ADDED
           IF FW-WORST-ACTION = SPACES OR LOW-VALUES
               MOVE 'SCOR'             TO FW-WORST-ACTION
           END-IF
      *
           MOVE AUTH-CARD-NUM          TO DCL-CARD-NUM
           MOVE SPACES                 TO WS-AUTH-COUNTRY
           MOVE SPACES                 TO WS-AUTH-MCC
           MOVE SPACES                 TO DCL-MERCHANT-ID
      *
      *    THE DETAIL AREA HOLDS NO COUNTRY.  THE CURRENCY IS TAKEN AS
      *    A STAND IN UNTIL THE MERCHANT ROW IS READ AT 2000.
           EVALUATE AUTH-TYPE
               WHEN 'P'
                   MOVE AP-MERCHANT-ID TO DCL-MERCHANT-ID
                   MOVE AP-MCC         TO WS-MCC-EDIT
                   MOVE WS-MCC-EDIT    TO WS-AUTH-MCC
                   MOVE AP-CURRENCY    TO WS-AUTH-COUNTRY
               WHEN 'C'
      *            A CASH ADVANCE CARRIES NO MERCHANT.  THE ACQUIRER
      *            IS THE ONLY GEOGRAPHY WE HAVE AND IT IS NOT A
      *            COUNTRY CODE, SO ONLY THE MCC RULES CAN RUN.
                   MOVE '6011'         TO WS-AUTH-MCC
                   MOVE AC-CURRENCY    TO WS-AUTH-COUNTRY
               WHEN 'R'
                   MOVE AR-ORIG-MERCHANT
                                       TO DCL-MERCHANT-ID
               WHEN OTHER
                   MOVE 0004           TO FW-RC
                   MOVE 'AUTHORISATION TYPE NOT RECOGNISED'
                                       TO FW-MESSAGE
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
      *
      *    THE WORK AREA CARRIES THE COUNTRY WHEN CACRD07 KNOWS IT
           IF FW-COUNTRY NOT = SPACES
               MOVE FW-COUNTRY         TO WS-AUTH-COUNTRY
           END-IF
           IF FW-MCC > ZERO
               MOVE FW-MCC             TO WS-MCC-EDIT
               MOVE WS-MCC-EDIT        TO WS-AUTH-MCC
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - THE MERCHANT ROW IS AUTHORITATIVE FOR THE COUNTRY       *
      ******************************************************************
       2000-READ-MERCHANT.
           IF DCL-MERCHANT-ID = SPACES
               GO TO 2000-EXIT
           END-IF
      *
           EXEC SQL
               SELECT COUNTRY_CD
                    , MCC
                 INTO :DCL-MER-COUNTRY
                    , :DCL-MER-MCC
                 FROM CARDSVC.MERCHANT
                WHERE MERCHANT_ID = :DCL-MERCHANT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-MER-COUNTRY
                                       TO WS-AUTH-COUNTRY
                   IF WS-AUTH-MCC = SPACES OR WS-AUTH-MCC = '0000'
                       MOVE DCL-MER-MCC
                                       TO WS-AUTH-MCC
                   END-IF
               WHEN +100
      *            AN UNKNOWN MERCHANT IS ITSELF A SIGNAL
                   MOVE 'GEOUNKM '     TO DCL-RULE-ID
                   MOVE WS-RULE-CLASS  TO DCL-RULE-CLASS
                   MOVE 15             TO DCL-SCORE-POINTS
                   MOVE 'FLAG'         TO DCL-ACTION-CD
                   PERFORM 5000-FIRE-RULE
                                       THRU 5000-EXIT
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'MERCHANT          '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG
                                       THRU 9100-EXIT
           END-EVALUATE
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - THE PREVIOUS AUTHORISATION ON THE SAME CARD             *
      ******************************************************************
       3000-READ-PRIOR.
           MOVE SPACES                 TO WS-PRIOR-COUNTRY
           MOVE ZERO                   TO WS-ELAPSED-MIN
      *
      *    CACRD07 SUPPLIES THE PRIOR CONTEXT WHEN IT HAS IT IN CORE
           IF FW-PRIOR-COUNTRY NOT = SPACES
               MOVE FW-PRIOR-COUNTRY   TO WS-PRIOR-COUNTRY
               MOVE 'Y'                TO WS-PRIOR-SW
           END-IF
      *
           EXEC SQL
               SELECT M.COUNTRY_CD
                    , TIMESTAMPDIFF(4,
                        CHAR(CURRENT TIMESTAMP -
                             TIMESTAMP(A.AUTH_DATE, A.AUTH_TIME)))
                 INTO :DCL-PRIOR-COUNTRY
                    , :DCL-PRIOR-MINUTES
                 FROM CARDSVC.AUTHORIZATION A
                    , CARDSVC.MERCHANT      M
                WHERE A.CARD_NUM    = :DCL-CARD-NUM
                  AND A.AUTH_STATUS = 'A'
                  AND A.AUTH_TYPE   = 'P'
                  AND M.MERCHANT_ID = SUBSTR(A.AUTH_DETAIL, 1, 15)
                ORDER BY A.AUTH_DATE DESC
                       , A.AUTH_SEQ_NUM DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-PRIOR-COUNTRY
                                       TO WS-PRIOR-COUNTRY
                   MOVE DCL-PRIOR-MINUTES
                                       TO WS-ELAPSED-MIN
                   MOVE 'Y'            TO WS-PRIOR-SW
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG
                                       THRU 9100-EXIT
           END-EVALUATE
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - WALK THE GEOGRAPHY AND MCC RULES                        *
      ******************************************************************
       4000-APPLY-RULES.
           EXEC SQL DECLARE GEOCSR CURSOR FOR
               SELECT RULE_ID
                    , RULE_CLASS
                    , SCORE_POINTS
                    , ACTION_CD
                    , THRESHOLD_CNT
                    , MCC_LIST
                    , COUNTRY_LIST
                    , DESCRIPTION
                 FROM CARDSVC.FRAUD_RULE
                WHERE HANDLER_PGM = :WS-PGM-ID
                  AND ACTIVE_FLG  = 'Y'
                  AND EFF_DATE   <= CURRENT DATE
                  AND EXP_DATE    > CURRENT DATE
                ORDER BY RULE_SEQ
           END-EXEC
      *
           EXEC SQL OPEN GEOCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE 'FRAUD_RULE        '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 4000-EXIT
           END-IF
      *
           PERFORM 4100-NEXT-RULE      THRU 4100-EXIT
               UNTIL WS-END-OF-RULES
                  OR WS-ERROR-FOUND
      *
           EXEC SQL CLOSE GEOCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE 'FRAUD_RULE        '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
           END-IF
           .
       4000-EXIT.
           EXIT
           .
      *
       4100-NEXT-RULE.
           EXEC SQL
               FETCH GEOCSR
                INTO :DCL-RULE-ID
                   , :DCL-RULE-CLASS
                   , :DCL-SCORE-POINTS
                   , :DCL-ACTION-CD
                   , :DCL-THRESH-CNT :IND-THRESH-CNT
                   , :DCL-MCC-LIST   :IND-MCC-LIST
                   , :DCL-CTRY-LIST  :IND-CTRY-LIST
                   , :DCL-DESCRIPTION
           END-EXEC
      *
           IF SQLCODE = +100
               MOVE 'Y'                TO WS-END-SW
               GO TO 4100-EXIT
           END-IF
      *
           IF SQLCODE NOT = 0
               MOVE 'FETCH   '         TO ER-SQL-OPERATION
               MOVE 'FRAUD_RULE        '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 4100-EXIT
           END-IF
      *
           ADD 1                       TO WS-RULES-READ
           PERFORM 4200-TEST-RULE      THRU 4200-EXIT
           .
       4100-EXIT.
           EXIT
           .
      *
       4200-TEST-RULE.
           EVALUATE DCL-RULE-ID
               WHEN 'GEOCTRY '
                   PERFORM 4300-TEST-COUNTRY
                                       THRU 4300-EXIT
               WHEN 'GEOMCC  '
                   PERFORM 4400-TEST-MCC
                                       THRU 4400-EXIT
               WHEN 'GEOTRVL '
                   PERFORM 4500-TEST-TRAVEL
                                       THRU 4500-EXIT
               WHEN 'GEOCNP  '
      *            CARD NOT PRESENT OUTSIDE THE HOME COUNTRY
                   IF FW-CARD-PRESENT-FLG = 'N'
                      AND WS-AUTH-COUNTRY NOT = WS-COUNTRY-USA
                      AND WS-AUTH-COUNTRY NOT = SPACES
                       PERFORM 5000-FIRE-RULE
                                       THRU 5000-EXIT
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       4200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4300 - COUNTRY LIST.  THE LIST IS A BLACK LIST - A MATCH FIRES *
      * THE RULE.                                                      *
      ******************************************************************
       4300-TEST-COUNTRY.
           IF IND-CTRY-LIST < ZERO
               GO TO 4300-EXIT
           END-IF
           IF WS-AUTH-COUNTRY = SPACES
               GO TO 4300-EXIT
           END-IF
      *
           MOVE SPACES                 TO WS-LIST-TEXT
           MOVE DCL-CTRY-TEXT(1:DCL-CTRY-LEN)
                                       TO WS-LIST-TEXT(1:DCL-CTRY-LEN)
           MOVE DCL-CTRY-LEN           TO WS-LIST-LEN
           MOVE 3                      TO WS-ENTRY-LEN
           MOVE WS-AUTH-COUNTRY        TO WS-LIST-TARGET
      *
           PERFORM 4600-SCAN-LIST      THRU 4600-EXIT
      *
           IF WS-ENTRY-MATCHED
               PERFORM 5000-FIRE-RULE  THRU 5000-EXIT
           END-IF
           .
       4300-EXIT.
           EXIT
           .
      *
       4400-TEST-MCC.
           IF IND-MCC-LIST < ZERO
               GO TO 4400-EXIT
           END-IF
           IF WS-AUTH-MCC = SPACES
               GO TO 4400-EXIT
           END-IF
      *
           MOVE SPACES                 TO WS-LIST-TEXT
           MOVE DCL-MCC-TEXT(1:DCL-MCC-LEN)
                                       TO WS-LIST-TEXT(1:DCL-MCC-LEN)
           MOVE DCL-MCC-LEN            TO WS-LIST-LEN
           MOVE 4                      TO WS-ENTRY-LEN
           MOVE WS-AUTH-MCC            TO WS-LIST-TARGET
      *
           PERFORM 4600-SCAN-LIST      THRU 4600-EXIT
      *
           IF WS-ENTRY-MATCHED
               PERFORM 5000-FIRE-RULE  THRU 5000-EXIT
           END-IF
           .
       4400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4500 - IMPOSSIBLE TRAVEL                                       *
      ******************************************************************
       4500-TEST-TRAVEL.
           IF NOT WS-PRIOR-FOUND
               GO TO 4500-EXIT
           END-IF
           IF WS-PRIOR-COUNTRY = SPACES OR WS-AUTH-COUNTRY = SPACES
               GO TO 4500-EXIT
           END-IF
           IF WS-PRIOR-COUNTRY = WS-AUTH-COUNTRY
               GO TO 4500-EXIT
           END-IF
      *
      *    THE RULE ROW CARRIES THE MINIMUM CREDIBLE JOURNEY TIME IN
      *    THE COUNT COLUMN.  FOUR HOURS WHEN IT IS NOT SET.
           IF IND-THRESH-CNT NOT < ZERO AND DCL-THRESH-CNT > ZERO
               MOVE DCL-THRESH-CNT     TO WS-MIN-TRAVEL-MIN
           END-IF
      *
           IF WS-ELAPSED-MIN < WS-MIN-TRAVEL-MIN
               PERFORM 5000-FIRE-RULE  THRU 5000-EXIT
               MOVE 'TWO COUNTRIES IN LESS THAN THE MINIMUM TRAVEL TIME'
                                       TO FW-MESSAGE
           END-IF
           .
       4500-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4600 - SCAN A COMMA SEPARATED LIST FOR THE TARGET              *
      ******************************************************************
       4600-SCAN-LIST.
           MOVE 'N'                    TO WS-MATCH-SW
           MOVE 1                      TO WS-LIST-POS
      *
       4600-LOOP.
           IF WS-LIST-POS > WS-LIST-LEN
               GO TO 4600-EXIT
           END-IF
      *
           MOVE SPACES                 TO WS-LIST-ENTRY
           MOVE WS-LIST-TEXT(WS-LIST-POS:WS-ENTRY-LEN)
                                       TO WS-LIST-ENTRY
      *
           IF WS-LIST-ENTRY = WS-LIST-TARGET
               MOVE 'Y'                TO WS-MATCH-SW
               GO TO 4600-EXIT
           END-IF
      *
      *    STEP OVER THE ENTRY AND ITS SEPARATOR
           COMPUTE WS-LIST-POS = WS-LIST-POS + WS-ENTRY-LEN + 1
           GO TO 4600-LOOP
           .
       4600-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5000 - RECORD ONE FIRED RULE                                   *
      ******************************************************************
       5000-FIRE-RULE.
           MOVE 'Y'                    TO WS-FIRED-SW
           ADD DCL-SCORE-POINTS        TO WS-POINTS-ADDED
      *
           IF FW-RULE-FIRED-CNT < 20
               ADD 1                   TO FW-RULE-FIRED-CNT
               MOVE DCL-RULE-ID        TO
                            FW-RF-RULE-ID(FW-RULE-FIRED-CNT)
               MOVE DCL-RULE-CLASS     TO
                            FW-RF-RULE-CLASS(FW-RULE-FIRED-CNT)
               MOVE DCL-SCORE-POINTS   TO
                            FW-RF-POINTS(FW-RULE-FIRED-CNT)
               MOVE DCL-ACTION-CD      TO
                            FW-RF-ACTION(FW-RULE-FIRED-CNT)
           END-IF
      *
           PERFORM 5100-PROMOTE-ACTION THRU 5100-EXIT
      *
           IF FW-REASON-CD = SPACES OR LOW-VALUES
               MOVE DCL-RULE-ID(1:4)   TO FW-REASON-CD
           END-IF
           .
       5000-EXIT.
           EXIT
           .
      *
       5100-PROMOTE-ACTION.
           MOVE ZERO                   TO WS-OLD-RANK
           MOVE ZERO                   TO WS-NEW-RANK
      *
           EVALUATE FW-WORST-ACTION
               WHEN 'SCOR'  MOVE 1     TO WS-OLD-RANK
               WHEN 'FLAG'  MOVE 2     TO WS-OLD-RANK
               WHEN 'REFR'  MOVE 3     TO WS-OLD-RANK
               WHEN 'DECL'  MOVE 4     TO WS-OLD-RANK
               WHEN OTHER   MOVE 1     TO WS-OLD-RANK
           END-EVALUATE
      *
           EVALUATE DCL-ACTION-CD
               WHEN 'SCOR'  MOVE 1     TO WS-NEW-RANK
               WHEN 'FLAG'  MOVE 2     TO WS-NEW-RANK
               WHEN 'REFR'  MOVE 3     TO WS-NEW-RANK
               WHEN 'DECL'  MOVE 4     TO WS-NEW-RANK
               WHEN OTHER   MOVE 1     TO WS-NEW-RANK
           END-EVALUATE
      *
           IF WS-NEW-RANK > WS-OLD-RANK
               MOVE DCL-ACTION-CD      TO FW-WORST-ACTION
           END-IF
           .
       5100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 6000 - ACCUMULATE INTO THE PIPELINE RESULT                     *
      ******************************************************************
       6000-POST-RESULT.
           MOVE WS-POINTS-ADDED        TO FW-SCORE-ADDED
           ADD WS-POINTS-ADDED         TO FW-SCORE-TOTAL
      *
           IF FW-SCORE-TOTAL > 9999
               MOVE 9999               TO FW-SCORE-TOTAL
           END-IF
      *
           IF FW-HANDLER-CNT < 8
               ADD 1                   TO FW-HANDLER-CNT
               MOVE WS-PGM-ID          TO FW-HD-PGM(FW-HANDLER-CNT)
               MOVE FW-RC              TO FW-HD-RC(FW-HANDLER-CNT)
               MOVE WS-POINTS-ADDED    TO FW-HD-POINTS(FW-HANDLER-CNT)
           END-IF
      *
           IF WS-RULE-FIRED
               IF FW-MESSAGE = SPACES OR LOW-VALUES
                   MOVE 'GEOGRAPHY OR MERCHANT CATEGORY RULE FIRED'
                                       TO FW-MESSAGE
               END-IF
           END-IF
      *
           IF WS-AUTH-COUNTRY NOT = SPACES
               MOVE WS-AUTH-COUNTRY    TO FW-COUNTRY
           END-IF
           .
       6000-EXIT.
           EXIT
           .
      *
       9100-SQL-DIAG.
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE 'GEOGRAPHY RULE PROCESSING FAILED'
                                       TO ER-MESSAGE
           MOVE SQLCODE                TO FW-SQLCODE
           MOVE WS-PGM-ID              TO FW-FAIL-PGM
           IF FW-RC < 0004
               MOVE 0004               TO FW-RC
           END-IF
           .
       9100-EXIT.
           EXIT
           .
