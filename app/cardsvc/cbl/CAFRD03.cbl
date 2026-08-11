      ******************************************************************
      * CAFRD03 - FRAUD RULE HANDLER - AMOUNT PATTERNS                 *
      *                                                                *
      * ROUTE FRAU / STANDARD SEQ 3.  NOT REGISTERED ON THE HIGHRISK   *
      * PIPELINE - THOSE AUTHORISATIONS ARE ALREADY REFERRED BY THE    *
      * TIME THE AMOUNT PATTERNS WOULD MATTER.                         *
      *                                                                *
      * CALLED BY  - CACRD07 BY DYNAMIC CALL, NAME RESOLVED FROM       *
      *              CARDSVC.PGM_ROUTE                                 *
      * CALLS      - NONE                                              *
      * TABLES     - CARDSVC.FRAUD_RULE     SELECT                     *
      *              CARDSVC.AUTHORIZATION  SELECT                     *
      *                                                                *
      * PARAMETERS - AUTH-RECORD     CVAUTH01Y                         *
      *              FRAUD-WORK-AREA CVFRAU1Y                          *
      *                                                                *
      * DELIVERED 2021 WITH THE CARD NOT PRESENT PROGRAMME.  THE       *
      * PATTERNS LOOKED FOR ARE                                        *
      *   ROUND AMOUNT PROBING     - 1.00, 10.00, 100.00 TEST CHARGES  *
      *   STRUCTURING              - REPEATED AMOUNTS JUST UNDER A     *
      *                              FLOOR LIMIT                       *
      *   REPEATED IDENTICAL AMOUNT- THE SAME AMOUNT SEVERAL TIMES IN  *
      *                              THE SAME DAY                      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAFRD03.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CAFRD03 '.
       01  WS-RULE-CLASS               PIC X(4)  VALUE 'AMTP'.
      *
       01  WS-FLAGS.
           05  WS-EOF-FLAG             PIC X     VALUE 'N'.
               88  WS-END-OF-RULES               VALUE 'Y'.
               88  WS-MORE-RULES                 VALUE 'N'.
           05  WS-ERROR-FLAG           PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-FIRED-FLAG           PIC X     VALUE 'N'.
               88  WS-ANY-RULE-FIRED             VALUE 'Y'.
      *
       01  WS-ACCUMULATORS.
           05  WS-POINTS-ADDED         PIC 9(4)  VALUE ZERO.
           05  WS-RULES-READ           PIC 9(4)  VALUE ZERO.
      *
       01  WS-AMOUNT-ANALYSIS.
           05  WS-AUTH-AMT             PIC S9(9)V99 COMP-3 VALUE ZERO.
           05  WS-WHOLE-PART           PIC S9(9)  COMP-3 VALUE ZERO.
           05  WS-CENTS-PART           PIC S9(2)  COMP-3 VALUE ZERO.
           05  WS-QUOTIENT             PIC S9(9)  COMP-3 VALUE ZERO.
           05  WS-REMAINDER            PIC S9(9)  COMP-3 VALUE ZERO.
           05  WS-FLOOR-LIMIT          PIC S9(9)V99 COMP-3
                                                 VALUE 100.00.
           05  WS-GAP-TO-FLOOR         PIC S9(9)V99 COMP-3 VALUE ZERO.
           05  WS-SAME-AMT-CNT         PIC S9(4)  COMP VALUE ZERO.
           05  WS-NEAR-FLOOR-CNT       PIC S9(4)  COMP VALUE ZERO.
           05  WS-SMALL-AMT-CNT        PIC S9(4)  COMP VALUE ZERO.
      *
       01  WS-SEVERITY.
           05  WS-OLD-RANK             PIC 9     VALUE ZERO.
           05  WS-NEW-RANK             PIC 9     VALUE ZERO.
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-RULE-ROW.
           05  DCL-RULE-ID             PIC X(8).
           05  DCL-RULE-CLASS          PIC X(4).
           05  DCL-THRESHOLD-AMT       PIC S9(11)V99 COMP-3.
           05  DCL-THRESHOLD-CNT       PIC S9(4) COMP.
           05  DCL-THRESHOLD-PCT       PIC S9(3)V99 COMP-3.
           05  DCL-SCORE-POINTS        PIC S9(4) COMP.
           05  DCL-ACTION-CD           PIC X(4).
           05  DCL-DESCRIPTION         PIC X(60).
      *
       01  DCL-RULE-IND.
           05  IND-THRESHOLD-AMT       PIC S9(4) COMP.
           05  IND-THRESHOLD-CNT       PIC S9(4) COMP.
           05  IND-THRESHOLD-PCT       PIC S9(4) COMP.
      *
       01  DCL-KEYS.
           05  DCL-CARD-NUM            PIC X(16).
           05  DCL-DETAIL-PATTERN      PIC X(60).
           05  DCL-MATCH-CNT           PIC S9(9) COMP.
      *
       01  WS-DETAIL-IMAGE             PIC X(60) VALUE SPACES.
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
           PERFORM 1000-INITIALISE
           IF NOT WS-ERROR-FOUND
               PERFORM 2000-COUNT-PATTERNS
               PERFORM 3000-APPLY-RULES
               PERFORM 8000-POST-RESULT
           END-IF
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - PULL THE AMOUNT OUT OF THE VARIANT                      *
      ******************************************************************
       1000-INITIALISE.
           INITIALIZE WS-ACCUMULATORS
           MOVE 'N'                    TO WS-ERROR-FLAG
           MOVE 'N'                    TO WS-FIRED-FLAG
           MOVE 'N'                    TO WS-EOF-FLAG
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
      *
           MOVE ZERO                   TO FW-SCORE-ADDED
           IF FW-WORST-ACTION = SPACES OR LOW-VALUES
               MOVE 'SCOR'             TO FW-WORST-ACTION
           END-IF
      *
           MOVE AUTH-CARD-NUM          TO DCL-CARD-NUM
      *
           EVALUATE TRUE
               WHEN AUTH-PURCHASE-TYPE
                   MOVE AP-AMOUNT      TO WS-AUTH-AMT
               WHEN AUTH-CASH-ADV-TYPE
                   MOVE AC-AMOUNT      TO WS-AUTH-AMT
               WHEN AUTH-REFUND-TYPE
      *            REFUNDS ARE NOT SCORED FOR AMOUNT PATTERNS
                   MOVE ZERO           TO WS-AUTH-AMT
                   MOVE 'Y'            TO WS-EOF-FLAG
               WHEN OTHER
                   MOVE 'Y'            TO WS-ERROR-FLAG
                   MOVE 0004           TO FW-RC
                   MOVE 'AUTHORISATION TYPE NOT RECOGNISED'
                                       TO FW-MESSAGE
           END-EVALUATE
      *
           IF WS-AUTH-AMT < ZERO
               COMPUTE WS-AUTH-AMT = 0 - WS-AUTH-AMT
           END-IF
      *
           COMPUTE WS-WHOLE-PART = FUNCTION INTEGER-PART(WS-AUTH-AMT)
           COMPUTE WS-CENTS-PART =
               (WS-AUTH-AMT - WS-WHOLE-PART) * 100
      *
      *    THE FLOOR LIMIT USED FOR THE STRUCTURING TEST IS THE
      *    ACQUIRER FLOOR THE SCHEMES PUBLISH, NOT A CREDIT LIMIT.
           MOVE 100.00                 TO WS-FLOOR-LIMIT
           COMPUTE WS-GAP-TO-FLOOR = WS-FLOOR-LIMIT - WS-AUTH-AMT
           .
      *
      ******************************************************************
      * 2000 - HOW OFTEN HAS THIS SHAPE OF AMOUNT BEEN SEEN TODAY      *
      *                                                                *
      * THE AMOUNT IS PACKED INSIDE AUTH_DETAIL SO THE COMPARISON IS   *
      * DONE ON THE BYTE IMAGE OF THE FIELD.  THAT ONLY WORKS FOR AN   *
      * EXACT MATCH, WHICH IS ALL THE REPEAT TEST NEEDS.               *
      ******************************************************************
       2000-COUNT-PATTERNS.
           IF WS-END-OF-RULES
               EXIT PARAGRAPH
           END-IF
      *
           MOVE SPACES                 TO WS-DETAIL-IMAGE
           EVALUATE TRUE
               WHEN AUTH-PURCHASE-TYPE
                   MOVE AUTH-DETAIL(38:9)
                                       TO WS-DETAIL-IMAGE(1:9)
               WHEN AUTH-CASH-ADV-TYPE
                   MOVE AUTH-DETAIL(24:6)
                                       TO WS-DETAIL-IMAGE(1:6)
           END-EVALUATE
           MOVE WS-DETAIL-IMAGE        TO DCL-DETAIL-PATTERN
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-MATCH-CNT
                 FROM CARDSVC.AUTHORIZATION
                WHERE CARD_NUM  = :DCL-CARD-NUM
                  AND AUTH_DATE = CURRENT DATE
                  AND AUTH_TYPE = :AUTH-TYPE
                  AND SUBSTR(AUTH_DETAIL, 38, 9) =
                      SUBSTR(:DCL-DETAIL-PATTERN, 1, 9)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-MATCH-CNT  TO WS-SAME-AMT-CNT
               WHEN +100
                   MOVE ZERO           TO WS-SAME-AMT-CNT
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG
           END-EVALUATE
      *
           PERFORM 2100-COUNT-SMALL
           .
      *
      *    ROUND AMOUNT PROBING SHOWS UP AS SEVERAL SMALL AUTHS ON THE
      *    SAME CARD IN THE SAME DAY.  THE AMOUNTS CANNOT BE COMPARED
      *    IN SQL SO ONLY THE COUNT IS TAKEN HERE.
       2100-COUNT-SMALL.
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-MATCH-CNT
                 FROM CARDSVC.AUTHORIZATION
                WHERE CARD_NUM    = :DCL-CARD-NUM
                  AND AUTH_DATE   = CURRENT DATE
                  AND AUTH_STATUS IN ('A','D','F')
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-MATCH-CNT  TO WS-SMALL-AMT-CNT
                   MOVE DCL-MATCH-CNT  TO WS-NEAR-FLOOR-CNT
               WHEN +100
                   MOVE ZERO           TO WS-SMALL-AMT-CNT
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - APPLY THE AMOUNT PATTERN RULES                          *
      ******************************************************************
       3000-APPLY-RULES.
           IF WS-END-OF-RULES
               EXIT PARAGRAPH
           END-IF
      *
           EXEC SQL DECLARE AMTCSR CURSOR FOR
               SELECT RULE_ID
                    , RULE_CLASS
                    , THRESHOLD_AMT
                    , THRESHOLD_CNT
                    , THRESHOLD_PCT
                    , SCORE_POINTS
                    , ACTION_CD
                    , DESCRIPTION
                 FROM CARDSVC.FRAUD_RULE
                WHERE RULE_CLASS  = :WS-RULE-CLASS
                  AND HANDLER_PGM = :WS-PGM-ID
                  AND ACTIVE_FLG  = 'Y'
                  AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
                ORDER BY RULE_SEQ
           END-EXEC
      *
           EXEC SQL OPEN AMTCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE 'FRAUD_RULE        '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG
               MOVE 'Y'                TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
      *
           MOVE 'N'                    TO WS-EOF-FLAG
           PERFORM UNTIL WS-END-OF-RULES OR WS-ERROR-FOUND
               PERFORM 3100-FETCH-RULE
           END-PERFORM
      *
           EXEC SQL CLOSE AMTCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE 'FRAUD_RULE        '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG
           END-IF
           .
      *
       3100-FETCH-RULE.
           EXEC SQL
               FETCH AMTCSR
                INTO :DCL-RULE-ID
                   , :DCL-RULE-CLASS
                   , :DCL-THRESHOLD-AMT :IND-THRESHOLD-AMT
                   , :DCL-THRESHOLD-CNT :IND-THRESHOLD-CNT
                   , :DCL-THRESHOLD-PCT :IND-THRESHOLD-PCT
                   , :DCL-SCORE-POINTS
                   , :DCL-ACTION-CD
                   , :DCL-DESCRIPTION
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-RULES-READ
                   PERFORM 3200-EVALUATE-RULE
               WHEN +100
                   MOVE 'Y'            TO WS-EOF-FLAG
               WHEN OTHER
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   MOVE 'FRAUD_RULE        '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG
                   MOVE 'Y'            TO WS-ERROR-FLAG
           END-EVALUATE
           .
      *
       3200-EVALUATE-RULE.
           IF IND-THRESHOLD-CNT < ZERO
               MOVE ZERO               TO DCL-THRESHOLD-CNT
           END-IF
           IF IND-THRESHOLD-AMT < ZERO
               MOVE ZERO               TO DCL-THRESHOLD-AMT
           END-IF
      *
           EVALUATE DCL-RULE-ID
               WHEN 'AMTROUND'
                   PERFORM 3300-TEST-ROUND
               WHEN 'AMTUNDER'
                   PERFORM 3400-TEST-STRUCTURING
               WHEN 'AMTREPET'
                   PERFORM 3500-TEST-REPEAT
               WHEN 'AMTPROBE'
                   PERFORM 3600-TEST-PROBE
               WHEN OTHER
      *            AN UNKNOWN RULE ID BELONGS TO A LATER RELEASE
                   CONTINUE
           END-EVALUATE
           .
      *
      *    A ROUND AMOUNT IS ONE WITH NO CENTS AND A WHOLE PART THAT
      *    DIVIDES BY THE RULE STEP - TEN DOLLARS WHEN NOT SET.
       3300-TEST-ROUND.
           IF WS-CENTS-PART NOT = ZERO
               EXIT PARAGRAPH
           END-IF
      *
           IF DCL-THRESHOLD-CNT = ZERO
               MOVE 10                 TO DCL-THRESHOLD-CNT
           END-IF
      *
           DIVIDE WS-WHOLE-PART BY DCL-THRESHOLD-CNT
               GIVING WS-QUOTIENT
               REMAINDER WS-REMAINDER
           END-DIVIDE
      *
           IF WS-REMAINDER = ZERO AND WS-WHOLE-PART > ZERO
               PERFORM 7000-FIRE-RULE
           END-IF
           .
      *
      *    STRUCTURING - THE AMOUNT SITS JUST UNDER THE FLOOR LIMIT
      *    AND IT IS NOT THE FIRST TIME TODAY.
       3400-TEST-STRUCTURING.
           IF DCL-THRESHOLD-AMT > ZERO
               MOVE DCL-THRESHOLD-AMT  TO WS-FLOOR-LIMIT
               COMPUTE WS-GAP-TO-FLOOR = WS-FLOOR-LIMIT - WS-AUTH-AMT
           END-IF
      *
           IF WS-GAP-TO-FLOOR > ZERO
              AND WS-GAP-TO-FLOOR < 5.00
               IF DCL-THRESHOLD-CNT = ZERO
                   MOVE 2              TO DCL-THRESHOLD-CNT
               END-IF
               IF WS-NEAR-FLOOR-CNT NOT < DCL-THRESHOLD-CNT
                   PERFORM 7000-FIRE-RULE
                   MOVE 'AMOUNTS REPEATEDLY KEPT JUST UNDER THE FLOOR'
                                       TO FW-MESSAGE
               END-IF
           END-IF
           .
      *
       3500-TEST-REPEAT.
           IF DCL-THRESHOLD-CNT = ZERO
               MOVE 3                  TO DCL-THRESHOLD-CNT
           END-IF
      *
           IF WS-SAME-AMT-CNT NOT < DCL-THRESHOLD-CNT
               PERFORM 7000-FIRE-RULE
               MOVE 'THE SAME AMOUNT HAS BEEN SEEN SEVERAL TIMES TODAY'
                                       TO FW-MESSAGE
           END-IF
           .
      *
      *    PROBING - A VERY SMALL AMOUNT ON A CARD THAT HAS ALREADY
      *    BEEN USED TODAY.  CLASSIC CARD TESTING BEHAVIOUR.
       3600-TEST-PROBE.
           IF DCL-THRESHOLD-AMT = ZERO
               MOVE 2.00               TO DCL-THRESHOLD-AMT
           END-IF
      *
           IF WS-AUTH-AMT > ZERO
              AND WS-AUTH-AMT NOT > DCL-THRESHOLD-AMT
               IF WS-SMALL-AMT-CNT > 1
                   PERFORM 7000-FIRE-RULE
               ELSE
      *            A SINGLE PROBE IS ONLY WORTH A NOTE
                   COMPUTE DCL-SCORE-POINTS = DCL-SCORE-POINTS / 2
                   MOVE 'SCOR'         TO DCL-ACTION-CD
                   PERFORM 7000-FIRE-RULE
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 7000 - RECORD A FIRED RULE                                     *
      ******************************************************************
       7000-FIRE-RULE.
           MOVE 'Y'                    TO WS-FIRED-FLAG
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
           PERFORM 7100-PROMOTE-ACTION
      *
           IF FW-REASON-CD = SPACES OR LOW-VALUES
               MOVE DCL-RULE-ID(4:4)   TO FW-REASON-CD
           END-IF
           .
      *
       7100-PROMOTE-ACTION.
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
      *
      ******************************************************************
      * 8000 - ADD TO WHAT THE EARLIER HANDLERS FOUND                  *
      ******************************************************************
       8000-POST-RESULT.
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
           IF WS-ANY-RULE-FIRED
              AND (FW-MESSAGE = SPACES OR FW-MESSAGE = LOW-VALUES)
               MOVE 'AMOUNT PATTERN RULE FIRED'
                                       TO FW-MESSAGE
           END-IF
           .
      *
       9100-SQL-DIAG.
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE 'AMOUNT PATTERN RULE PROCESSING FAILED'
                                       TO ER-MESSAGE
           MOVE SQLCODE                TO FW-SQLCODE
           MOVE WS-PGM-ID              TO FW-FAIL-PGM
           IF FW-RC < 0004
               MOVE 0004               TO FW-RC
           END-IF
           .
