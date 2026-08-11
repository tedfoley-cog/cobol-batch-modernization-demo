      ******************************************************************
      * CAFRD04 - FRAUD RULE HANDLER - MERCHANT REPUTATION             *
      *                                                                *
      * ROUTE FRAU / STANDARD SEQ 4 AND ROUTE FRAU / HIGHRISK SEQ 3.   *
      * ON THE HIGHRISK PIPELINE IT RUNS DIRECTLY AFTER CAFRD02 - THE  *
      * AMOUNT PATTERN HANDLER IS NOT REGISTERED THERE.                *
      *                                                                *
      * CALLED BY  - CACRD07 BY DYNAMIC CALL, NAME RESOLVED FROM       *
      *              CARDSVC.PGM_ROUTE                                 *
      * CALLS      - NONE                                              *
      * TABLES     - CARDSVC.FRAUD_RULE     SELECT                     *
      *              CARDSVC.MERCHANT       SELECT                     *
      *              CARDSVC.AUTHORIZATION  SELECT                     *
      *                                                                *
      * PARAMETERS - AUTH-RECORD     CVAUTH01Y                         *
      *              FRAUD-WORK-AREA CVFRAU1Y                          *
      *                                                                *
      * DELIVERED 2022 AFTER THE ACQUIRER PORTFOLIO REVIEW.  SCORES    *
      * THE MERCHANT AND THE ACQUIRER BEHIND IT RATHER THAN THE        *
      * CARDHOLDER, SO IT IS THE ONLY HANDLER THAT CAN FIRE ON AN      *
      * OTHERWISE ORDINARY AUTHORISATION.                              *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAFRD04.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-CONSTANTS.
           05  WS-PGM-ID               PIC X(8)  VALUE 'CAFRD04 '.
           05  WS-RULE-CLASS           PIC X(4)  VALUE 'MERC'.
           05  WS-LOOKBACK-DAYS        PIC S9(4) COMP VALUE 90.
      *
       01  WS-FLAGS.
           05  WS-EOF-FLAG             PIC X     VALUE 'N'.
               88  WS-END-OF-RULES               VALUE 'Y'.
           05  WS-ERROR-FLAG           PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-FIRED-FLAG           PIC X     VALUE 'N'.
               88  WS-ANY-RULE-FIRED             VALUE 'Y'.
           05  WS-MERCHANT-FLAG        PIC X     VALUE 'N'.
               88  WS-MERCHANT-KNOWN             VALUE 'Y'.
               88  WS-MERCHANT-UNKNOWN           VALUE 'N'.
           05  WS-ACQUIRER-FLAG        PIC X     VALUE 'N'.
               88  WS-ACQUIRER-KNOWN             VALUE 'Y'.
      *
       01  WS-WORK.
           05  WS-POINTS-ADDED         PIC 9(4)  VALUE ZERO.
           05  WS-RULES-READ           PIC 9(4)  VALUE ZERO.
           05  WS-SCALED-POINTS        PIC S9(6)V99 COMP-3 VALUE ZERO.
           05  WS-OLD-RANK             PIC 9     VALUE ZERO.
           05  WS-NEW-RANK             PIC 9     VALUE ZERO.
      *
       01  WS-MERCHANT-VIEW.
           05  WS-MER-ID               PIC X(15) VALUE SPACES.
           05  WS-MER-NAME             PIC X(40) VALUE SPACES.
           05  WS-MER-COUNTRY          PIC X(3)  VALUE SPACES.
           05  WS-MER-STATUS           PIC X     VALUE SPACES.
               88  WS-MER-ACTIVE                 VALUE 'A'.
               88  WS-MER-SUSPENDED              VALUE 'S'.
               88  WS-MER-TERMINATED             VALUE 'T'.
           05  WS-MER-HIGH-RISK        PIC X     VALUE 'N'.
               88  WS-MER-IS-HIGH-RISK           VALUE 'Y'.
           05  WS-MER-CB-RATE          PIC S9(3)V99 COMP-3 VALUE ZERO.
           05  WS-MER-ONBOARD-DAYS     PIC S9(9) COMP VALUE ZERO.
           05  WS-ACQ-ID               PIC X(11) VALUE SPACES.
           05  WS-ACQ-MERCHANT-CNT     PIC S9(9) COMP VALUE ZERO.
           05  WS-ACQ-HIGH-RISK-CNT    PIC S9(9) COMP VALUE ZERO.
           05  WS-ACQ-HIGH-RISK-PCT    PIC S9(3)V99 COMP-3 VALUE ZERO.
           05  WS-MER-DECLINE-CNT      PIC S9(9) COMP VALUE ZERO.
           05  WS-MER-AUTH-CNT         PIC S9(9) COMP VALUE ZERO.
           05  WS-MER-DECLINE-PCT      PIC S9(3)V99 COMP-3 VALUE ZERO.
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
       01  DCL-MERCHANT-ROW.
           05  DCL-MERCHANT-ID         PIC X(15).
           05  DCL-MERCHANT-NAME       PIC X(40).
           05  DCL-ACQUIRER-ID         PIC X(11).
           05  DCL-COUNTRY-CD          PIC X(3).
           05  DCL-HIGH-RISK-FLG       PIC X(1).
           05  DCL-CHARGEBACK-RATE     PIC S9(3)V99 COMP-3.
           05  DCL-STATUS              PIC X(1).
           05  DCL-ONBOARD-DAYS        PIC S9(9) COMP.
      *
       01  DCL-MERCHANT-IND.
           05  IND-ACQUIRER-ID         PIC S9(4) COMP.
           05  IND-ONBOARD-DAYS        PIC S9(4) COMP.
      *
       01  DCL-COUNTS.
           05  DCL-COUNT-1             PIC S9(9) COMP.
           05  DCL-COUNT-2             PIC S9(9) COMP.
           05  DCL-LOOKBACK            PIC S9(4) COMP.
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
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 2000-READ-MERCHANT
               IF WS-MERCHANT-KNOWN
                   PERFORM 2500-READ-ACQUIRER
                   PERFORM 2700-READ-DECLINE-HISTORY
               END-IF
               PERFORM 3000-APPLY-RULES
               PERFORM 8000-POST-RESULT
           END-IF
      *
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - THE MERCHANT ID IS ONLY PRESENT ON SOME VARIANTS        *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'N'                    TO WS-ERROR-FLAG
           MOVE 'N'                    TO WS-FIRED-FLAG
           MOVE 'N'                    TO WS-EOF-FLAG
           MOVE 'N'                    TO WS-MERCHANT-FLAG
           MOVE 'N'                    TO WS-ACQUIRER-FLAG
           MOVE ZERO                   TO WS-POINTS-ADDED
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE WS-LOOKBACK-DAYS       TO DCL-LOOKBACK
      *
           MOVE ZERO                   TO FW-SCORE-ADDED
           IF FW-WORST-ACTION = SPACES OR LOW-VALUES
               MOVE 'SCOR'             TO FW-WORST-ACTION
           END-IF
      *
           MOVE SPACES                 TO WS-MER-ID
      *
           EVALUATE TRUE
               WHEN AUTH-PURCHASE-TYPE
                   MOVE AP-MERCHANT-ID TO WS-MER-ID
               WHEN AUTH-REFUND-TYPE
                   MOVE AR-ORIG-MERCHANT
                                       TO WS-MER-ID
               WHEN AUTH-CASH-ADV-TYPE
      *            AN ATM HAS NO MERCHANT ROW.  THE ACQUIRER IS STILL
      *            SCORED FROM THE CASH ADVANCE VARIANT.
                   MOVE AC-ACQUIRER-ID TO WS-ACQ-ID
                   MOVE 'Y'            TO WS-ACQUIRER-FLAG
               WHEN OTHER
                   MOVE 'Y'            TO WS-ERROR-FLAG
                   MOVE 0004           TO FW-RC
                   MOVE 'AUTHORISATION TYPE NOT RECOGNISED'
                                       TO FW-MESSAGE
           END-EVALUATE
      *
           IF WS-MER-ID = SPACES AND FW-MERCHANT-ID NOT = SPACES
               MOVE FW-MERCHANT-ID     TO WS-MER-ID
           END-IF
           .
      *
      ******************************************************************
      * 2000 - MERCHANT MASTER                                         *
      ******************************************************************
       2000-READ-MERCHANT.
           IF WS-MER-ID = SPACES
               EXIT PARAGRAPH
           END-IF
      *
           MOVE WS-MER-ID              TO DCL-MERCHANT-ID
      *
           EXEC SQL
               SELECT MERCHANT_NAME
                    , ACQUIRER_ID
                    , COUNTRY_CD
                    , HIGH_RISK_FLG
                    , CHARGEBACK_RATE
                    , STATUS
                    , COALESCE(DAYS(CURRENT DATE) -
                               DAYS(ONBOARD_DATE), 9999)
                 INTO :DCL-MERCHANT-NAME
                    , :DCL-ACQUIRER-ID :IND-ACQUIRER-ID
                    , :DCL-COUNTRY-CD
                    , :DCL-HIGH-RISK-FLG
                    , :DCL-CHARGEBACK-RATE
                    , :DCL-STATUS
                    , :DCL-ONBOARD-DAYS :IND-ONBOARD-DAYS
                 FROM CARDSVC.MERCHANT
                WHERE MERCHANT_ID = :DCL-MERCHANT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-MERCHANT-FLAG
                   MOVE DCL-MERCHANT-NAME
                                       TO WS-MER-NAME
                   MOVE DCL-COUNTRY-CD TO WS-MER-COUNTRY
                   MOVE DCL-HIGH-RISK-FLG
                                       TO WS-MER-HIGH-RISK
                   MOVE DCL-CHARGEBACK-RATE
                                       TO WS-MER-CB-RATE
                   MOVE DCL-STATUS     TO WS-MER-STATUS
                   MOVE DCL-ONBOARD-DAYS
                                       TO WS-MER-ONBOARD-DAYS
                   IF IND-ACQUIRER-ID NOT < ZERO
                       MOVE DCL-ACQUIRER-ID
                                       TO WS-ACQ-ID
                       MOVE 'Y'        TO WS-ACQUIRER-FLAG
                   END-IF
               WHEN +100
                   MOVE 'N'            TO WS-MERCHANT-FLAG
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'MERCHANT          '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2500 - ACQUIRER LEVEL PORTFOLIO                                *
      ******************************************************************
       2500-READ-ACQUIRER.
           IF NOT WS-ACQUIRER-KNOWN
               EXIT PARAGRAPH
           END-IF
      *
           MOVE WS-ACQ-ID              TO DCL-ACQUIRER-ID
           MOVE ZERO                   TO WS-ACQ-HIGH-RISK-PCT
      *
           EXEC SQL
               SELECT COUNT(*)
                    , SUM(CASE WHEN HIGH_RISK_FLG = 'Y'
                               THEN 1 ELSE 0 END)
                 INTO :DCL-COUNT-1
                    , :DCL-COUNT-2
                 FROM CARDSVC.MERCHANT
                WHERE ACQUIRER_ID = :DCL-ACQUIRER-ID
                  AND STATUS <> 'T'
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-COUNT-1    TO WS-ACQ-MERCHANT-CNT
                   MOVE DCL-COUNT-2    TO WS-ACQ-HIGH-RISK-CNT
                   IF WS-ACQ-MERCHANT-CNT > ZERO
                       COMPUTE WS-ACQ-HIGH-RISK-PCT ROUNDED =
                           (WS-ACQ-HIGH-RISK-CNT * 100)
                           / WS-ACQ-MERCHANT-CNT
                   END-IF
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'MERCHANT          '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2700 - DECLINE RATE SEEN AT THIS MERCHANT IN THE LOOKBACK      *
      ******************************************************************
       2700-READ-DECLINE-HISTORY.
           MOVE WS-MER-ID              TO DCL-MERCHANT-ID
           MOVE ZERO                   TO WS-MER-DECLINE-PCT
      *
           EXEC SQL
               SELECT COUNT(*)
                    , SUM(CASE WHEN AUTH_STATUS = 'D'
                               THEN 1 ELSE 0 END)
                 INTO :DCL-COUNT-1
                    , :DCL-COUNT-2
                 FROM CARDSVC.AUTHORIZATION
                WHERE SUBSTR(AUTH_DETAIL, 1, 15) = :DCL-MERCHANT-ID
                  AND AUTH_TYPE = 'P'
                  AND AUTH_DATE > CURRENT DATE - :DCL-LOOKBACK DAYS
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-COUNT-1    TO WS-MER-AUTH-CNT
                   MOVE DCL-COUNT-2    TO WS-MER-DECLINE-CNT
                   IF WS-MER-AUTH-CNT > ZERO
                       COMPUTE WS-MER-DECLINE-PCT ROUNDED =
                           (WS-MER-DECLINE-CNT * 100)
                           / WS-MER-AUTH-CNT
                   END-IF
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - APPLY THE MERCHANT REPUTATION RULES                     *
      ******************************************************************
       3000-APPLY-RULES.
           EXEC SQL DECLARE MRCCSR CURSOR FOR
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
           EXEC SQL OPEN MRCCSR END-EXEC
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
           PERFORM UNTIL WS-END-OF-RULES OR WS-ERROR-FOUND
               PERFORM 3100-FETCH-RULE
           END-PERFORM
      *
           EXEC SQL CLOSE MRCCSR END-EXEC
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
               FETCH MRCCSR
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
           IF IND-THRESHOLD-PCT < ZERO
               MOVE ZERO               TO DCL-THRESHOLD-PCT
           END-IF
           IF IND-THRESHOLD-CNT < ZERO
               MOVE ZERO               TO DCL-THRESHOLD-CNT
           END-IF
      *
           EVALUATE DCL-RULE-ID
               WHEN 'MERCBRAT'
                   PERFORM 3300-TEST-CHARGEBACK
               WHEN 'MERHIRSK'
                   PERFORM 3400-TEST-HIGH-RISK
               WHEN 'MERSTATU'
                   PERFORM 3500-TEST-STATUS
               WHEN 'MERNEWMR'
                   PERFORM 3600-TEST-NEW-MERCHANT
               WHEN 'MERUNKWN'
                   PERFORM 3700-TEST-UNKNOWN
               WHEN 'MERACQPF'
                   PERFORM 3800-TEST-ACQUIRER
               WHEN 'MERDECRT'
                   PERFORM 3900-TEST-DECLINE-RATE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3300 - CHARGEBACK RATE.  THE POINTS SCALE WITH HOW FAR OVER    *
      * THE THRESHOLD THE MERCHANT IS, CAPPED AT TWICE THE ROW VALUE.  *
      ******************************************************************
       3300-TEST-CHARGEBACK.
           IF NOT WS-MERCHANT-KNOWN
               EXIT PARAGRAPH
           END-IF
      *
           IF DCL-THRESHOLD-PCT = ZERO
               MOVE 1.00               TO DCL-THRESHOLD-PCT
           END-IF
      *
           IF WS-MER-CB-RATE > DCL-THRESHOLD-PCT
               COMPUTE WS-SCALED-POINTS ROUNDED =
                   DCL-SCORE-POINTS *
                   (WS-MER-CB-RATE / DCL-THRESHOLD-PCT)
      *
               IF WS-SCALED-POINTS > (DCL-SCORE-POINTS * 2)
                   COMPUTE WS-SCALED-POINTS = DCL-SCORE-POINTS * 2
               END-IF
      *
               MOVE WS-SCALED-POINTS   TO DCL-SCORE-POINTS
               PERFORM 7000-FIRE-RULE
               MOVE 'MERCHANT CHARGEBACK RATE ABOVE THE ACCEPTED LEVEL'
                                       TO FW-MESSAGE
           END-IF
           .
      *
       3400-TEST-HIGH-RISK.
           IF WS-MERCHANT-KNOWN AND WS-MER-IS-HIGH-RISK
               PERFORM 7000-FIRE-RULE
           END-IF
           .
      *
       3500-TEST-STATUS.
           IF NOT WS-MERCHANT-KNOWN
               EXIT PARAGRAPH
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-MER-TERMINATED
      *            A TERMINATED MERCHANT SHOULD NOT BE ACQUIRING AT
      *            ALL - THIS IS ALWAYS A DECLINE
                   MOVE 'DECL'         TO DCL-ACTION-CD
                   PERFORM 7000-FIRE-RULE
                   MOVE 'MERCHANT IS TERMINATED ON THE MERCHANT MASTER'
                                       TO FW-MESSAGE
               WHEN WS-MER-SUSPENDED
                   MOVE 'REFR'         TO DCL-ACTION-CD
                   PERFORM 7000-FIRE-RULE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
       3600-TEST-NEW-MERCHANT.
           IF NOT WS-MERCHANT-KNOWN
               EXIT PARAGRAPH
           END-IF
      *
           IF DCL-THRESHOLD-CNT = ZERO
               MOVE 30                 TO DCL-THRESHOLD-CNT
           END-IF
      *
           IF WS-MER-ONBOARD-DAYS < DCL-THRESHOLD-CNT
               PERFORM 7000-FIRE-RULE
           END-IF
           .
      *
       3700-TEST-UNKNOWN.
      *    A PURCHASE AGAINST A MERCHANT THAT IS NOT ON THE MASTER
      *    MEANS THE ACQUIRER FEED IS BEHIND OR THE ID IS FORGED.
           IF AUTH-PURCHASE-TYPE
              AND WS-MERCHANT-UNKNOWN
              AND WS-MER-ID NOT = SPACES
               PERFORM 7000-FIRE-RULE
               MOVE 'MERCHANT NOT PRESENT ON THE MERCHANT MASTER'
                                       TO FW-MESSAGE
           END-IF
           .
      *
       3800-TEST-ACQUIRER.
           IF NOT WS-ACQUIRER-KNOWN
               EXIT PARAGRAPH
           END-IF
      *
           IF DCL-THRESHOLD-PCT = ZERO
               MOVE 25.00              TO DCL-THRESHOLD-PCT
           END-IF
      *
           IF WS-ACQ-MERCHANT-CNT > 4
              AND WS-ACQ-HIGH-RISK-PCT > DCL-THRESHOLD-PCT
               PERFORM 7000-FIRE-RULE
           END-IF
           .
      *
       3900-TEST-DECLINE-RATE.
           IF NOT WS-MERCHANT-KNOWN
               EXIT PARAGRAPH
           END-IF
      *
           IF DCL-THRESHOLD-PCT = ZERO
               MOVE 40.00              TO DCL-THRESHOLD-PCT
           END-IF
      *
           IF WS-MER-AUTH-CNT > 20
              AND WS-MER-DECLINE-PCT > DCL-THRESHOLD-PCT
               PERFORM 7000-FIRE-RULE
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
      * 8000 - LAST HANDLER ON THE PIPELINE.  THE TOTAL LEAVING HERE   *
      * IS WHAT CACRD07 SCORES THE AUTHORISATION WITH.                 *
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
           IF WS-MERCHANT-KNOWN
               MOVE WS-MER-ID          TO FW-MERCHANT-ID
               IF WS-ACQUIRER-KNOWN
                   MOVE WS-ACQ-ID      TO FW-ACQUIRER-ID
               END-IF
           END-IF
      *
           IF WS-ANY-RULE-FIRED
              AND (FW-MESSAGE = SPACES OR FW-MESSAGE = LOW-VALUES)
               MOVE 'MERCHANT REPUTATION RULE FIRED'
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
           MOVE 'MERCHANT REPUTATION RULE PROCESSING FAILED'
                                       TO ER-MESSAGE
           MOVE SQLCODE                TO FW-SQLCODE
           MOVE WS-PGM-ID              TO FW-FAIL-PGM
           IF FW-RC < 0004
               MOVE 0004               TO FW-RC
           END-IF
           .
