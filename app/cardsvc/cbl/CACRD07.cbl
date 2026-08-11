      ******************************************************************
      * CACRD07 - FRAUD RULE EVALUATION                                *
      *                                                                *
      * FOURTH PROGRAM OF THE AUTHORIZATION CHAIN.                     *
      *                                                                *
      * THREE THINGS HAPPEN HERE -                                     *
      *                                                                *
      *   1. THE MERCHANT IS READ FROM CARDSVC.MERCHANT SO THE HIGH    *
      *      RISK FLAG AND THE COUNTRY ARE KNOWN.  THAT PLUS THE RISK  *
      *      BAND FROM THE RISK STEP CHOOSES THE RULE CLASS - STANDARD *
      *      OR HIGHRISK.                                              *
      *                                                                *
      *   2. THE RULE SET FOR THAT CLASS IS READ.  THE FAST PATH IS    *
      *      THE VSAM FILE FRAUDRUL, WHICH CBREF04J REBUILDS FROM DB2  *
      *      EVERY NIGHT.  IF THE FILE IS CLOSED OR THE RULE IS NOT ON *
      *      IT, THE ROW IS READ STRAIGHT FROM CARDSVC.FRAUD_RULE.     *
      *                                                                *
      *   3. THE HANDLER PIPELINE IS DRIVEN THROUGH THE DISPATCHER.    *
      *      SEQUENCE 1, 2, 3 ... ARE ASKED FOR IN TURN UNTIL THE      *
      *      DISPATCHER REPORTS THAT NO ROUTE EXISTS AT THAT SEQUENCE. *
      *      THE HANDLERS ARE OWNED BY THE FRAUD TEAM AND ARE NEVER    *
      *      NAMED HERE.                                               *
      *                                                                *
      * SCORE POINTS ACCUMULATE ACROSS THE HANDLERS.  THE STRONGEST    *
      * ACTION SEEN WINS - DECL BEATS REFR BEATS FLAG BEATS SCOR - AND *
      * A DECL STOPS THE PIPELINE IMMEDIATELY.                         *
      *                                                                *
      * CALLED BY   - CACRD06  XCTL                                    *
      * CALLS       - CACRD90  LINK, ROUTE FRAU/STANDARD OR FRAU/      *
      *                        HIGHRISK, SEQUENCE 1 UPWARDS            *
      *             - CACRD08  XCTL, LIMIT AND VELOCITY                *
      *             - CACRD09  XCTL WHEN A HANDLER DECLINES            *
      *             - CACRD91  ERROR HANDLER                           *
      * TABLES      - CARDSVC.MERCHANT, CARDSVC.FRAUD_RULE  (SELECT)   *
      * FILES       - FRAUDRUL  CARD.PROD.FRAUDRUL  (READ)             *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD07.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD07 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'CACRD08 '.
       01  WS-DECISION-PGM             PIC X(8)  VALUE 'CACRD09 '.
       01  WS-FRAUD-FILE               PIC X(8)  VALUE 'FRAUDRUL'.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-ROUTE-KEY                PIC X(8)  VALUE 'STANDARD'.
       01  WS-SEQ                      PIC 9(4)  VALUE ZERO.
       01  WS-MAX-SEQ                  PIC 9(4)  VALUE 0009.
       01  WS-HANDLER-CNT              PIC 9(2)  VALUE ZERO.
      *
       01  WS-PIPELINE-SW              PIC X     VALUE 'Y'.
           88  WS-PIPELINE-OPEN                  VALUE 'Y'.
           88  WS-PIPELINE-DONE                  VALUE 'N'.
      *
       01  WS-MERCH-FOUND-SW           PIC X     VALUE 'N'.
           88  WS-MERCH-FOUND                    VALUE 'Y'.
       01  WS-RULE-FOUND-SW            PIC X     VALUE 'N'.
           88  WS-RULE-FOUND                     VALUE 'Y'.
       01  WS-MORE-SW                  PIC X     VALUE 'Y'.
           88  WS-MORE-ROWS                      VALUE 'Y'.
      *
       01  WS-SCORE-ACC                PIC S9(5) COMP-3 VALUE ZERO.
       01  WS-ACTION-RANK              PIC 9     VALUE 0.
       01  WS-BEST-RANK                PIC 9     VALUE 0.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-YYYYMMDD            PIC 9(8)  VALUE ZERO.
      *
      *    ---------------------------------------------------------
      *    FRAUDRUL RECORD - 240 BYTES, KEYED ON THE RULE ID.
      *    LOADED FROM CARDSVC.FRAUD_RULE BY THE NIGHTLY REFRESH.
      *    ---------------------------------------------------------
       01  FRAUD-RULE-REC.
           05  FR-RULE-ID              PIC X(8).
           05  FR-RULE-CLASS           PIC X(4).
           05  FR-RULE-SEQ             PIC 9(2).
           05  FR-HANDLER-PGM          PIC X(8).
           05  FR-THRESHOLD-AMT        PIC S9(9)V99 COMP-3.
           05  FR-THRESHOLD-CNT        PIC 9(4).
           05  FR-THRESHOLD-PCT        PIC S9(3)V99 COMP-3.
           05  FR-SCORE-POINTS         PIC S9(3)   COMP-3.
           05  FR-ACTION-CD            PIC X(4).
               88  FR-ACT-SCORE                  VALUE 'SCOR'.
               88  FR-ACT-FLAG                   VALUE 'FLAG'.
               88  FR-ACT-REFER                  VALUE 'REFR'.
               88  FR-ACT-DECLINE                VALUE 'DECL'.
           05  FR-MCC-LIST             PIC X(80).
           05  FR-COUNTRY-LIST         PIC X(60).
           05  FR-ACTIVE-FLG           PIC X.
           05  FR-DESCRIPTION          PIC X(60).
           05  FR-FILLER               PIC X(6).
      *
       01  WS-RULE-LEN                 PIC S9(4) COMP VALUE 240.
       01  WS-RULE-KEY                 PIC X(8)  VALUE SPACES.
      *
       01  WS-MCC-CHAR                 PIC X(4)  VALUE SPACES.
       01  WS-LIST-POS                 PIC S9(4) COMP VALUE 0.
       01  WS-LIST-HIT-SW              PIC X     VALUE 'N'.
           88  WS-LIST-HIT                       VALUE 'Y'.
      *
       01  WS-DISPATCH-AREA.
           05  WS-DA-ROUTE             PIC X(38).
           05  WS-DA-COMMAREA          PIC X(512).
      *
           COPY CVAUTHW1Y.
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-MERCHANT-ID             PIC X(15).
       01  DCL-MERCH-NAME              PIC X(40).
       01  DCL-MERCH-MCC               PIC X(4).
       01  DCL-MERCH-COUNTRY           PIC X(3).
       01  DCL-HIGH-RISK-FLG           PIC X(1).
       01  DCL-CHARGEBACK-RATE         PIC S9(3)V99 COMP-3.
       01  DCL-MERCH-STATUS            PIC X(1).
      *
       01  DCL-RULE-CLASS              PIC X(4).
       01  DCL-RULE-ID                 PIC X(8).
       01  DCL-RULE-SEQ                PIC S9(4) COMP.
       01  DCL-HANDLER-PGM             PIC X(8).
       01  DCL-THRESHOLD-AMT           PIC S9(9)V99 COMP-3.
       01  DCL-THRESHOLD-CNT           PIC S9(4) COMP.
       01  DCL-SCORE-POINTS            PIC S9(4) COMP.
       01  DCL-ACTION-CD               PIC X(4).
       01  DCL-MCC-LIST                PIC X(200).
       01  DCL-COUNTRY-LIST            PIC X(120).
      *
       01  IND-THRESHOLD-AMT           PIC S9(4) COMP.
       01  IND-THRESHOLD-CNT           PIC S9(4) COMP.
       01  IND-MCC-LIST                PIC S9(4) COMP.
       01  IND-COUNTRY-LIST            PIC S9(4) COMP.
      *
           EXEC SQL DECLARE RULCSR CURSOR FOR
               SELECT RULE_ID
                    , RULE_SEQ
                    , HANDLER_PGM
                    , THRESHOLD_AMT
                    , THRESHOLD_CNT
                    , SCORE_POINTS
                    , ACTION_CD
                    , MCC_LIST
                    , COUNTRY_LIST
                 FROM CARDSVC.FRAUD_RULE
                WHERE RULE_CLASS = :DCL-RULE-CLASS
                  AND ACTIVE_FLG = 'Y'
                  AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
                ORDER BY RULE_SEQ
           END-EXEC.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           IF EIBCALEN = ZERO
               PERFORM 9100-NO-COMMAREA
               GO TO 0000-EXIT
           END-IF
      *
           MOVE DFHCOMMAREA            TO CA-WORK-AREA
           PERFORM 0100-INIT
      *
           PERFORM 1000-READ-MERCHANT
           PERFORM 1500-CHOOSE-RULE-CLASS
           PERFORM 2000-LOAD-RULE-SET
           PERFORM 3000-DRIVE-PIPELINE
           PERFORM 4000-SET-VERDICT
           PERFORM 6000-CONTINUE-CHAIN
           .
       0000-EXIT.
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
           MOVE ZERO                   TO WS-SCORE-ACC
                                          WS-HANDLER-CNT
                                          WS-BEST-RANK
           MOVE 'Y'                    TO WS-PIPELINE-SW
      *
           MOVE ZERO                   TO CAW-FRAUD-SCORE
                                          CAW-FRAUD-SEQ
           MOVE 'SCOR'                 TO CAW-FRAUD-ACTION
           MOVE SPACES                 TO CAW-FRAUD-RULE
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-YYYYMMDD)
                     RESP(WS-RESP)
           END-EXEC
      *
           IF CAW-TRAIL-CNT < 8
               ADD 1                   TO CAW-TRAIL-CNT
               MOVE WS-PGM-ID          TO CAW-TRAIL-PGM(CAW-TRAIL-CNT)
               MOVE ZERO               TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 1000 - THE MERCHANT                                            *
      ******************************************************************
       1000-READ-MERCHANT.
           MOVE 'N'                    TO WS-MERCH-FOUND-SW
           MOVE 'N'                    TO DCL-HIGH-RISK-FLG
           MOVE ZERO                   TO DCL-CHARGEBACK-RATE
           MOVE CAW-RQ-MERCH-ID        TO DCL-MERCHANT-ID
      *
      *    A CASH ADVANCE HAS NO MERCHANT - THE ACQUIRER STANDS IN
           IF CAW-RQ-AUTH-TYPE = 'C'
               MOVE CAW-RQ-MERCH-CTRY  TO DCL-MERCH-COUNTRY
               GO TO 1000-EXIT
           END-IF
      *
           EXEC SQL
               SELECT MERCHANT_NAME
                    , MCC
                    , COUNTRY_CD
                    , HIGH_RISK_FLG
                    , CHARGEBACK_RATE
                    , STATUS
                 INTO :DCL-MERCH-NAME
                    , :DCL-MERCH-MCC
                    , :DCL-MERCH-COUNTRY
                    , :DCL-HIGH-RISK-FLG
                    , :DCL-CHARGEBACK-RATE
                    , :DCL-MERCH-STATUS
                 FROM CARDSVC.MERCHANT
                WHERE MERCHANT_ID = :DCL-MERCHANT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-MERCH-FOUND-SW
               WHEN +100
      *            AN UNKNOWN MERCHANT IS TREATED AS HIGH RISK -
      *            THE ACQUIRER HAS NOT BEEN THROUGH ONBOARDING
                   MOVE 'Y'            TO DCL-HIGH-RISK-FLG
                   MOVE CAW-RQ-MERCH-CTRY
                                       TO DCL-MERCH-COUNTRY
                   ADD 20              TO WS-SCORE-ACC
               WHEN OTHER
                   MOVE '1000-READ-MERCHANT'
                                       TO ER-PARAGRAPH
                   MOVE 'MERCHANT         '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
                   MOVE 'Y'            TO DCL-HIGH-RISK-FLG
           END-EVALUATE
      *
      *    A SUSPENDED OR TERMINATED MERCHANT SCORES HEAVILY
           IF WS-MERCH-FOUND AND DCL-MERCH-STATUS NOT = 'A'
               ADD 40                  TO WS-SCORE-ACC
               MOVE 'Y'                TO DCL-HIGH-RISK-FLG
           END-IF
      *
      *    A CHARGEBACK RATE OVER TWO PER CENT PULLS THE MERCHANT
      *    INTO THE HIGH RISK PIPELINE WHATEVER THE FLAG SAYS
           IF DCL-CHARGEBACK-RATE > 2.00
               MOVE 'Y'                TO DCL-HIGH-RISK-FLG
               ADD 15                  TO WS-SCORE-ACC
           END-IF
      *
      *    THE MCC ON THE REQUEST SHOULD AGREE WITH THE ONE ON FILE
           IF WS-MERCH-FOUND
               IF DCL-MERCH-MCC NOT = CAW-RQ-MCC
                   ADD 10              TO WS-SCORE-ACC
               END-IF
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 1500 - WHICH RULE CLASS APPLIES                                *
      ******************************************************************
       1500-CHOOSE-RULE-CLASS.
           MOVE 'STANDARD'             TO WS-ROUTE-KEY
      *
           EVALUATE TRUE
               WHEN DCL-HIGH-RISK-FLG = 'Y'
                   MOVE 'HIGHRISK'     TO WS-ROUTE-KEY
               WHEN DCL-MERCH-COUNTRY NOT = WS-COUNTRY-USA
                   MOVE 'HIGHRISK'     TO WS-ROUTE-KEY
               WHEN CAW-RSK-BAND = 'C' OR 'X'
                   MOVE 'HIGHRISK'     TO WS-ROUTE-KEY
               WHEN CAW-RQ-ECOM AND CAW-RQ-AMT > 500.00
                   MOVE 'HIGHRISK'     TO WS-ROUTE-KEY
               WHEN CAW-RQ-KEYED
                   MOVE 'HIGHRISK'     TO WS-ROUTE-KEY
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           MOVE WS-ROUTE-KEY(1:4)      TO DCL-RULE-CLASS
      *
      *    THE RULE CLASS ON THE TABLE IS THE FIRST FOUR CHARACTERS
      *    OF THE ROUTE KEY - STAN OR HIGH.
           .
      *
      ******************************************************************
      * 2000 - THE RULE SET.  VSAM FIRST, DB2 AS THE BACK STOP.        *
      ******************************************************************
       2000-LOAD-RULE-SET.
           MOVE ZERO                   TO WS-MAX-SEQ
           MOVE 'Y'                    TO WS-MORE-SW
      *
           EXEC SQL OPEN RULCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2000-LOAD-RULE-SET'
                                       TO ER-PARAGRAPH
               MOVE 'FRAUD_RULE       ' TO ER-SQL-TABLE
               MOVE 'OPEN    '          TO ER-SQL-OPERATION
               PERFORM 8000-SQL-ERROR
      *        WITHOUT A RULE SET THE PIPELINE STILL RUNS - THE
      *        HANDLERS CARRY THEIR OWN DEFAULTS
               MOVE 4                  TO WS-MAX-SEQ
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM UNTIL NOT WS-MORE-ROWS
               EXEC SQL
                   FETCH RULCSR
                    INTO :DCL-RULE-ID
                       , :DCL-RULE-SEQ
                       , :DCL-HANDLER-PGM
                       , :DCL-THRESHOLD-AMT :IND-THRESHOLD-AMT
                       , :DCL-THRESHOLD-CNT :IND-THRESHOLD-CNT
                       , :DCL-SCORE-POINTS
                       , :DCL-ACTION-CD
                       , :DCL-MCC-LIST      :IND-MCC-LIST
                       , :DCL-COUNTRY-LIST  :IND-COUNTRY-LIST
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 2100-APPLY-RULE-ROW
                   WHEN +100
                       MOVE 'N'        TO WS-MORE-SW
                   WHEN OTHER
                       MOVE 'N'        TO WS-MORE-SW
                       MOVE '2000-LOAD-RULE-SET'
                                       TO ER-PARAGRAPH
                       MOVE 'FRAUD_RULE       '
                                       TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       PERFORM 8000-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL CLOSE RULCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '2000-LOAD-RULE-SET'
                                       TO ER-PARAGRAPH
               MOVE 'FRAUD_RULE       ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '          TO ER-SQL-OPERATION
               PERFORM 8000-SQL-ERROR
           END-IF
      *
           IF WS-MAX-SEQ = ZERO
               MOVE 4                  TO WS-MAX-SEQ
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2100 - ONE RULE ROW.  THE ROW ITSELF CAN SCORE THE REQUEST     *
      *        BEFORE ANY HANDLER RUNS - THAT IS HOW THE MCC AND       *
      *        COUNTRY BLACK LISTS ARE APPLIED.                        *
      ******************************************************************
       2100-APPLY-RULE-ROW.
           IF DCL-RULE-SEQ > WS-MAX-SEQ
               MOVE DCL-RULE-SEQ       TO WS-MAX-SEQ
           END-IF
      *
      *    PREFER THE VSAM IMAGE - IT CARRIES THE OVERNIGHT TUNING
           PERFORM 2200-READ-RULE-VSAM
           IF NOT WS-RULE-FOUND
               PERFORM 2300-RULE-FROM-DB2
           END-IF
      *
           MOVE 'N'                    TO WS-LIST-HIT-SW
      *
           IF FR-MCC-LIST NOT = SPACES
               MOVE CAW-RQ-MCC         TO WS-MCC-CHAR
               PERFORM 2400-SCAN-MCC-LIST
               IF WS-LIST-HIT
                   ADD FR-SCORE-POINTS TO WS-SCORE-ACC
                   PERFORM 2500-RANK-ACTION
               END-IF
           END-IF
      *
           IF FR-COUNTRY-LIST NOT = SPACES
               PERFORM 2450-SCAN-COUNTRY-LIST
               IF WS-LIST-HIT
                   ADD FR-SCORE-POINTS TO WS-SCORE-ACC
                   PERFORM 2500-RANK-ACTION
               END-IF
           END-IF
      *
      *    AN AMOUNT THRESHOLD ON THE RULE IS TESTED HERE TOO
           IF FR-THRESHOLD-AMT > ZERO
               IF CAW-RQ-AMT > FR-THRESHOLD-AMT
                   ADD FR-SCORE-POINTS TO WS-SCORE-ACC
                   PERFORM 2500-RANK-ACTION
               END-IF
           END-IF
           .
      *
       2200-READ-RULE-VSAM.
           MOVE 'N'                    TO WS-RULE-FOUND-SW
           MOVE DCL-RULE-ID            TO WS-RULE-KEY
      *
           EXEC CICS READ
                     FILE(WS-FRAUD-FILE)
                     INTO(FRAUD-RULE-REC)
                     LENGTH(WS-RULE-LEN)
                     RIDFLD(WS-RULE-KEY)
                     KEYLENGTH(8)
                     EQUAL
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   IF FR-ACTIVE-FLG = 'Y'
                       MOVE 'Y'        TO WS-RULE-FOUND-SW
                   END-IF
               WHEN DFHRESP(NOTFND)
                   CONTINUE
               WHEN DFHRESP(NOTOPEN)
      *            THE FILE IS CLOSED FOR THE NIGHTLY REBUILD -
      *            FALL BACK TO DB2 WITHOUT RAISING AN ERROR
                   CONTINUE
               WHEN OTHER
                   MOVE '2200-READ-RULE-VSAM'
                                       TO ER-PARAGRAPH
                   MOVE WS-FRAUD-FILE  TO ER-FILE-NAME
                   MOVE WS-RULE-KEY    TO ER-VSAM-KEY
                   PERFORM 8200-VSAM-ERROR
           END-EVALUATE
           .
      *
       2300-RULE-FROM-DB2.
           MOVE DCL-RULE-ID            TO FR-RULE-ID
           MOVE DCL-RULE-CLASS         TO FR-RULE-CLASS
           MOVE DCL-RULE-SEQ           TO FR-RULE-SEQ
           MOVE DCL-HANDLER-PGM        TO FR-HANDLER-PGM
           MOVE DCL-SCORE-POINTS       TO FR-SCORE-POINTS
           MOVE DCL-ACTION-CD          TO FR-ACTION-CD
           MOVE 'Y'                    TO FR-ACTIVE-FLG
      *
           IF IND-THRESHOLD-AMT < ZERO
               MOVE ZERO               TO FR-THRESHOLD-AMT
           ELSE
               MOVE DCL-THRESHOLD-AMT  TO FR-THRESHOLD-AMT
           END-IF
      *
           IF IND-THRESHOLD-CNT < ZERO
               MOVE ZERO               TO FR-THRESHOLD-CNT
           ELSE
               MOVE DCL-THRESHOLD-CNT  TO FR-THRESHOLD-CNT
           END-IF
      *
           IF IND-MCC-LIST < ZERO
               MOVE SPACES             TO FR-MCC-LIST
           ELSE
               MOVE DCL-MCC-LIST(1:80) TO FR-MCC-LIST
           END-IF
      *
           IF IND-COUNTRY-LIST < ZERO
               MOVE SPACES             TO FR-COUNTRY-LIST
           ELSE
               MOVE DCL-COUNTRY-LIST(1:60)
                                       TO FR-COUNTRY-LIST
           END-IF
           .
      *
      *    THE LISTS ARE COMMA SEPARATED TEXT - WALK THEM FOUR AND
      *    THREE CHARACTERS AT A TIME RATHER THAN PARSING PROPERLY.
       2400-SCAN-MCC-LIST.
           MOVE 'N'                    TO WS-LIST-HIT-SW
           PERFORM VARYING WS-LIST-POS FROM 1 BY 5
                     UNTIL WS-LIST-POS > 76 OR WS-LIST-HIT
               IF FR-MCC-LIST(WS-LIST-POS:4) = WS-MCC-CHAR
                   MOVE 'Y'            TO WS-LIST-HIT-SW
                   MOVE FR-RULE-ID     TO CAW-FRAUD-RULE
               END-IF
           END-PERFORM
           .
      *
       2450-SCAN-COUNTRY-LIST.
           MOVE 'N'                    TO WS-LIST-HIT-SW
           PERFORM VARYING WS-LIST-POS FROM 1 BY 4
                     UNTIL WS-LIST-POS > 57 OR WS-LIST-HIT
               IF FR-COUNTRY-LIST(WS-LIST-POS:3) = DCL-MERCH-COUNTRY
                   MOVE 'Y'            TO WS-LIST-HIT-SW
                   MOVE FR-RULE-ID     TO CAW-FRAUD-RULE
               END-IF
           END-PERFORM
           .
      *
       2500-RANK-ACTION.
           EVALUATE TRUE
               WHEN FR-ACT-DECLINE
                   MOVE 4              TO WS-ACTION-RANK
               WHEN FR-ACT-REFER
                   MOVE 3              TO WS-ACTION-RANK
               WHEN FR-ACT-FLAG
                   MOVE 2              TO WS-ACTION-RANK
               WHEN OTHER
                   MOVE 1              TO WS-ACTION-RANK
           END-EVALUATE
      *
           IF WS-ACTION-RANK > WS-BEST-RANK
               MOVE WS-ACTION-RANK     TO WS-BEST-RANK
               MOVE FR-ACTION-CD       TO CAW-FRAUD-ACTION
               MOVE FR-RULE-ID         TO CAW-FRAUD-RULE
           END-IF
           .
      *
      ******************************************************************
      * 3000 - DRIVE THE HANDLER PIPELINE                              *
      *                                                                *
      * SEQUENCE NUMBERS ARE ASKED FOR IN TURN.  THE DISPATCHER        *
      * ANSWERS RQ-RC 0008 WHEN NOTHING IS REGISTERED AT THAT          *
      * SEQUENCE, AND THAT IS THE END OF THE PIPELINE - IT IS NOT AN   *
      * ERROR.  THE HANDLER READS AND UPDATES THE CHAIN COMMAREA.      *
      ******************************************************************
       3000-DRIVE-PIPELINE.
           MOVE 1                      TO WS-SEQ
      *
           PERFORM UNTIL WS-PIPELINE-DONE
               PERFORM 3100-CALL-HANDLER
      *
               IF WS-PIPELINE-OPEN
                   ADD 1               TO WS-SEQ
                   IF WS-SEQ > WS-MAX-SEQ
                       MOVE 'N'        TO WS-PIPELINE-SW
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       3100-CALL-HANDLER.
           MOVE SPACES                 TO ROUTE-REQUEST
           MOVE 'FRAU'                 TO RQ-ROUTE-TYPE
           MOVE WS-ROUTE-KEY           TO RQ-ROUTE-KEY
           MOVE WS-SEQ                 TO RQ-SEQ-NBR
           MOVE ZERO                   TO RQ-RC
      *
      *    THE HANDLER IS GIVEN THE LIVE CHAIN COMMAREA - IT SCORES
      *    INTO CAW-FRAUD-SCORE AND MAY RAISE CAW-FRAUD-ACTION.
           MOVE WS-SEQ                 TO CAW-FRAUD-SEQ
           MOVE WS-SCORE-ACC           TO CAW-FRAUD-SCORE
      *
           MOVE ROUTE-REQUEST          TO WS-DA-ROUTE
           MOVE CA-WORK-AREA           TO WS-DA-COMMAREA
      *
           EXEC CICS LINK
                     PROGRAM(WS-DISPATCHER-ONLINE)
                     COMMAREA(WS-DISPATCH-AREA)
                     LENGTH(LENGTH OF WS-DISPATCH-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'N'                TO WS-PIPELINE-SW
               MOVE '3100-CALL-HANDLER'
                                       TO ER-PARAGRAPH
               MOVE 'ROUT'             TO ER-ERROR-TYPE
               MOVE 'FRAUD HANDLER DISPATCH FAILED'
                                       TO ER-MESSAGE
               PERFORM 8100-CICS-ERROR
               GO TO 3100-EXIT
           END-IF
      *
           MOVE WS-DA-ROUTE            TO ROUTE-REQUEST
      *
      *    NO ROUTE AT THIS SEQUENCE - THE PIPELINE IS EXHAUSTED AND
      *    THE COMMAREA COMING BACK IS THE ONE WE SENT, SO IT IS NOT
      *    COPIED IN.
           IF RQ-RC NOT = ZERO
               MOVE 'N'                TO WS-PIPELINE-SW
               GO TO 3100-EXIT
           END-IF
      *
           MOVE WS-DA-COMMAREA         TO CA-WORK-AREA
           ADD 1                       TO WS-HANDLER-CNT
      *
      *    THE HANDLER OWNS THE RUNNING SCORE WHILE IT HAS CONTROL
           MOVE CAW-FRAUD-SCORE        TO WS-SCORE-ACC
      *
           EVALUATE CAW-FRAUD-ACTION
               WHEN 'DECL'
      *            A DECLINE ENDS THE PIPELINE - NOTHING LATER CAN
      *            OVERTURN IT AND THE REMAINING HANDLERS WOULD ONLY
      *            ADD POINTS TO A LOST CAUSE
                   MOVE 4              TO WS-BEST-RANK
                   MOVE 'N'            TO WS-PIPELINE-SW
               WHEN 'REFR'
                   IF WS-BEST-RANK < 3
                       MOVE 3          TO WS-BEST-RANK
                   END-IF
               WHEN 'FLAG'
                   IF WS-BEST-RANK < 2
                       MOVE 2          TO WS-BEST-RANK
                   END-IF
               WHEN OTHER
                   IF WS-BEST-RANK < 1
                       MOVE 1          TO WS-BEST-RANK
                   END-IF
           END-EVALUATE
           .
       3100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - THE VERDICT                                             *
      ******************************************************************
       4000-SET-VERDICT.
      *    THE SCORE IS CARRIED AS THREE DIGITS ON THE COMMAREA
           IF WS-SCORE-ACC > 999
               MOVE 999                TO CAW-FRAUD-SCORE
           ELSE
               IF WS-SCORE-ACC < ZERO
                   MOVE ZERO           TO CAW-FRAUD-SCORE
               ELSE
                   MOVE WS-SCORE-ACC   TO CAW-FRAUD-SCORE
               END-IF
           END-IF
      *
           EVALUATE WS-BEST-RANK
               WHEN 4
                   MOVE 'DECL'         TO CAW-FRAUD-ACTION
               WHEN 3
                   MOVE 'REFR'         TO CAW-FRAUD-ACTION
               WHEN 2
                   MOVE 'FLAG'         TO CAW-FRAUD-ACTION
               WHEN OTHER
                   MOVE 'SCOR'         TO CAW-FRAUD-ACTION
           END-EVALUATE
      *
      *    A HIGH SCORE STANDS ON ITS OWN EVEN WHEN NO SINGLE RULE
      *    ASKED FOR MORE THAN A FLAG.  THESE CUT OFFS HAVE NOT BEEN
      *    RETUNED SINCE THE 2011 MODEL REFRESH.
           EVALUATE TRUE
               WHEN CAW-FRAUD-SCORE > 200
                   MOVE 'DECL'         TO CAW-FRAUD-ACTION
               WHEN CAW-FRAUD-SCORE > 120
                   IF CAW-FRAUD-ACTION NOT = 'DECL'
                       MOVE 'REFR'     TO CAW-FRAUD-ACTION
                   END-IF
               WHEN CAW-FRAUD-SCORE > 060
                   IF CAW-FRAUD-ACTION = 'SCOR'
                       MOVE 'FLAG'     TO CAW-FRAUD-ACTION
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF CAW-TRAIL-CNT > ZERO
               IF CAW-FRAUD-ACTION = 'DECL'
                   MOVE WS-RC-ERROR    TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
               ELSE
                   IF CAW-FRAUD-ACTION = 'REFR'
                       MOVE WS-RC-WARNING
                                       TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
                   END-IF
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 6000 - PASS CONTROL ON                                         *
      ******************************************************************
       6000-CONTINUE-CHAIN.
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
           IF CAW-FRAUD-ACTION = 'DECL'
               MOVE WS-DECISION-PGM    TO WS-NEXT-PGM
           END-IF
      *
           EXEC CICS XCTL
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           MOVE '6000-CONTINUE-CHAIN'  TO ER-PARAGRAPH
           PERFORM 8100-CICS-ERROR
           .
      *
      ******************************************************************
      * 8000 - ERROR HANDLING                                          *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'FRAUD RULE READ FAILED'
                                       TO ER-MESSAGE
           PERFORM 8900-LINK-ERROR-PGM
           .
      *
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           IF ER-ERROR-TYPE = SPACES
               MOVE 'CICS'             TO ER-ERROR-TYPE
           END-IF
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
           PERFORM 8900-LINK-ERROR-PGM
           .
      *
       8200-VSAM-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE 'W'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
                                          ER-VSAM-RC
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE 'FRAUD RULE FILE READ FAILED - USING DB2'
                                       TO ER-MESSAGE
           PERFORM 8900-LINK-ERROR-PGM
           .
      *
       8900-LINK-ERROR-PGM.
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE EIBTRMID               TO ER-TERM-ID
           MOVE 'N'                    TO ER-ABEND-REQUESTED
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM-ONLINE)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
       9100-NO-COMMAREA.
           EXEC CICS SEND
                     TEXT('CACRD07 IS A CHAIN STEP - START WITH CA00')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           .
