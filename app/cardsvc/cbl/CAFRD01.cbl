      ******************************************************************
      * CAFRD01 - FRAUD RULE HANDLER - VELOCITY                        *
      *                                                                *
      * FIRST HANDLER IN THE PIPELINE.  ROUTE FRAU / STANDARD SEQ 1    *
      * AND ROUTE FRAU / HIGHRISK SEQ 1.                               *
      *                                                                *
      * CALLED BY  - CACRD07 BY DYNAMIC CALL.  THE LOAD MODULE NAME    *
      *              IS RESOLVED FROM CARDSVC.PGM_ROUTE - THE CALLER   *
      *              DOES NOT NAME THIS PROGRAM IN A LITERAL.          *
      * CALLS      - NONE                                              *
      * TABLES     - CARDSVC.FRAUD_RULE     SELECT                     *
      *              CARDSVC.AUTHORIZATION  SELECT                     *
      *              CARDSVC.CARD_LIMIT     SELECT                     *
      *                                                                *
      * PARAMETERS - AUTH-RECORD     CVAUTH01Y                         *
      *              FRAUD-WORK-AREA CVFRAU1Y                          *
      *                                                                *
      * THE WORK AREA IS CUMULATIVE.  POINTS ARE ADDED TO WHATEVER     *
      * THE PIPELINE HAS ALREADY SCORED AND THE WORST ACTION IS ONLY   *
      * PROMOTED, NEVER LOWERED.                                       *
      *                                                                *
      * ORIGINAL 1999 VERSION.  RULE PARAMETERS WERE MOVED OUT TO      *
      * FRAUD_RULE IN 2003 - BEFORE THAT THEY WERE LITERALS BELOW.     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAFRD01.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CAFRD01 '.
       01  WS-RULE-CLASS               PIC X(4)  VALUE 'VELO'.
      *
       01  WS-SWITCHES.
           05  WS-END-SW               PIC X     VALUE 'N'.
               88  WS-END-OF-RULES               VALUE 'Y'.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-FIRED-SW             PIC X     VALUE 'N'.
               88  WS-RULE-FIRED                 VALUE 'Y'.
           05  WS-CURSOR-SW            PIC X     VALUE 'N'.
               88  WS-CURSOR-OPEN                VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-RULES-READ           PIC 9(4)  VALUE ZERO.
           05  WS-POINTS-ADDED         PIC 9(4)  VALUE ZERO.
           05  WS-WINDOW-MIN           PIC S9(4) COMP VALUE 60.
      *
       01  WS-MEASURES.
           05  WS-CARD-CNT             PIC S9(9) COMP-3 VALUE ZERO.
           05  WS-ACCT-CNT             PIC S9(9) COMP-3 VALUE ZERO.
           05  WS-CARD-AMT             PIC S9(11)V99 COMP-3 VALUE ZERO.
           05  WS-ACCT-AMT             PIC S9(11)V99 COMP-3 VALUE ZERO.
           05  WS-DECL-CNT             PIC S9(9) COMP-3 VALUE ZERO.
      *
      *    THE OLD HARD CODED LIMITS.  THEY ARE STILL USED WHEN THE
      *    RULE ROW CARRIES NO THRESHOLD OF ITS OWN.
       01  WS-DEFAULTS.
           05  WS-DEF-CNT-CARD         PIC S9(4) COMP VALUE 5.
           05  WS-DEF-CNT-ACCT         PIC S9(4) COMP VALUE 9.
           05  WS-DEF-AMT-CARD         PIC S9(9)V99 COMP-3
                                                 VALUE 2500.00.
           05  WS-DEF-AMT-ACCT         PIC S9(9)V99 COMP-3
                                                 VALUE 5000.00.
      *
       01  WS-SEVERITY-WORK.
           05  WS-OLD-RANK             PIC 9     VALUE ZERO.
           05  WS-NEW-RANK             PIC 9     VALUE ZERO.
      *
       01  WS-AMOUNT-WORK              PIC S9(9)V99 COMP-3 VALUE ZERO.
      *
      *    A LOCAL COPY OF THE 60 BYTE VARIANT AREA.  THE ROWS READ
      *    BACK OUT OF THE WINDOW ARE NOT THE AUTHORISATION BEING
      *    SCORED SO THEY CANNOT BE MAPPED THROUGH THE LINKAGE COPY.
       01  WS-DETAIL-AREA              PIC X(60) VALUE SPACES.
       01  WS-DA-PURCHASE REDEFINES WS-DETAIL-AREA.
           05  FILLER                  PIC X(49).
           05  WS-DA-PURCH-AMT         PIC S9(9)V99 COMP-3.
           05  FILLER                  PIC X(5).
       01  WS-DA-CASH REDEFINES WS-DETAIL-AREA.
           05  FILLER                  PIC X(23).
           05  WS-DA-CASH-AMT          PIC S9(9)V99 COMP-3.
           05  WS-DA-CASH-FEE          PIC S9(5)V99 COMP-3.
           05  FILLER                  PIC X(27).
       01  WS-DA-REFUND REDEFINES WS-DETAIL-AREA.
           05  FILLER                  PIC X(33).
           05  WS-DA-REFUND-AMT        PIC S9(9)V99 COMP-3.
           05  FILLER                  PIC X(21).
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-RULE.
           05  DCL-RULE-ID             PIC X(8).
           05  DCL-RULE-CLASS          PIC X(4).
           05  DCL-RULE-SEQ            PIC S9(4) COMP.
           05  DCL-THRESH-AMT          PIC S9(11)V99 COMP-3.
           05  DCL-THRESH-CNT          PIC S9(4) COMP.
           05  DCL-THRESH-PCT          PIC S9(3)V99 COMP-3.
           05  DCL-SCORE-POINTS        PIC S9(4) COMP.
           05  DCL-ACTION-CD           PIC X(4).
           05  DCL-DESCRIPTION         PIC X(60).
      *
       01  IND-RULE.
           05  IND-THRESH-AMT          PIC S9(4) COMP.
           05  IND-THRESH-CNT          PIC S9(4) COMP.
           05  IND-THRESH-PCT          PIC S9(4) COMP.
      *
       01  DCL-KEYS.
           05  DCL-CARD-NUM            PIC X(16).
           05  DCL-ACCT-ID             PIC S9(11) COMP-3.
           05  DCL-WINDOW-MIN          PIC S9(4) COMP.
      *
       01  DCL-RESULTS.
           05  DCL-AUTH-CNT            PIC S9(9) COMP.
           05  DCL-AUTH-AMT            PIC S9(13)V99 COMP-3.
      *
       01  IND-AUTH-AMT                PIC S9(4) COMP.
      *
       01  DCL-WINDOW-ROW.
           05  DCL-WIN-TYPE            PIC X(1).
           05  DCL-WIN-DETAIL          PIC X(60).
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
           PERFORM 2000-GET-WINDOW     THRU 2000-EXIT
           PERFORM 3000-MEASURE        THRU 3000-EXIT
           IF WS-ERROR-FOUND
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 4000-APPLY-RULES    THRU 4000-EXIT
           PERFORM 5000-POST-RESULT    THRU 5000-EXIT
           .
       0000-RETURN.
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - SET UP FROM THE AUTHORIZATION                           *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-END-SW
           MOVE 'N'                    TO WS-FIRED-SW
           MOVE ZERO                   TO WS-POINTS-ADDED
           MOVE ZERO                   TO WS-RULES-READ
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
      *
           MOVE ZERO                   TO FW-SCORE-ADDED
           IF FW-WORST-ACTION = SPACES OR LOW-VALUES
               MOVE 'SCOR'             TO FW-WORST-ACTION
           END-IF
      *
           MOVE AUTH-CARD-NUM          TO DCL-CARD-NUM
           MOVE AUTH-ACCT-ID           TO DCL-ACCT-ID
      *
      *    THE AMOUNT LIVES IN A DIFFERENT PLACE IN EACH VARIANT OF
      *    THE DETAIL AREA.
           MOVE ZERO                   TO WS-AMOUNT-WORK
           EVALUATE AUTH-TYPE
               WHEN 'P'
                   MOVE AP-AMOUNT      TO WS-AMOUNT-WORK
               WHEN 'C'
                   COMPUTE WS-AMOUNT-WORK = AC-AMOUNT + AC-FEE
               WHEN 'R'
                   MOVE AR-AMOUNT      TO WS-AMOUNT-WORK
               WHEN OTHER
                   MOVE 0004           TO FW-RC
                   MOVE 'AUTHORISATION TYPE NOT RECOGNISED'
                                       TO FW-MESSAGE
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
      *
           IF WS-AMOUNT-WORK < ZERO
               COMPUTE WS-AMOUNT-WORK = 0 - WS-AMOUNT-WORK
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - THE ROLLING WINDOW IS A PROPERTY OF THE CARD LIMIT ROW  *
      * AND FALLS BACK TO ONE HOUR.                                    *
      ******************************************************************
       2000-GET-WINDOW.
           MOVE 60                     TO WS-WINDOW-MIN
      *
           EXEC SQL
               SELECT VELOCITY_WINDOW_MIN
                 INTO :DCL-WINDOW-MIN
                 FROM CARDSVC.CARD_LIMIT
                WHERE CARD_NUM   = :DCL-CARD-NUM
                  AND LIMIT_TYPE = 'CRED'
                  AND EXP_DATE   = '9999-12-31'
           END-EXEC
      *
           IF SQLCODE = 0
               IF DCL-WINDOW-MIN > ZERO
                   MOVE DCL-WINDOW-MIN TO WS-WINDOW-MIN
               END-IF
           ELSE
               IF SQLCODE NOT = +100
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD_LIMIT        '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-DIAG THRU 9100-EXIT
               END-IF
           END-IF
      *
           MOVE WS-WINDOW-MIN          TO DCL-WINDOW-MIN
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - COUNT AND TOTAL THE WINDOW                              *
      ******************************************************************
       3000-MEASURE.
           PERFORM 3100-COUNT-CARD     THRU 3100-EXIT
           IF WS-ERROR-FOUND
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-COUNT-ACCOUNT  THRU 3200-EXIT
           IF WS-ERROR-FOUND
               GO TO 3000-EXIT
           END-IF
           PERFORM 3300-COUNT-DECLINES THRU 3300-EXIT
           .
       3000-EXIT.
           EXIT
           .
      *
      *    THE AUTHORISED AMOUNT IS PACKED INSIDE THE 60 BYTE DETAIL
      *    AREA SO IT CANNOT BE SUMMED IN SQL.  THE WINDOW IS READ
      *    ROW BY ROW AND TOTALLED HERE UNDER THE VARIANT.
       3100-COUNT-CARD.
           MOVE ZERO                   TO WS-CARD-CNT
           MOVE ZERO                   TO WS-CARD-AMT
      *
           EXEC SQL DECLARE CRDCSR CURSOR FOR
               SELECT AUTH_TYPE
                    , AUTH_DETAIL
                 FROM CARDSVC.AUTHORIZATION
                WHERE CARD_NUM    = :DCL-CARD-NUM
                  AND AUTH_STATUS IN ('A','F')
                  AND TIMESTAMP(AUTH_DATE, AUTH_TIME) >
                      CURRENT TIMESTAMP - :DCL-WINDOW-MIN MINUTES
           END-EXEC
      *
           EXEC SQL OPEN CRDCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 3100-EXIT
           END-IF
      *
           MOVE 'N'                    TO WS-END-SW
           PERFORM 3150-FETCH-WINDOW   THRU 3150-EXIT
               UNTIL WS-END-OF-RULES
                  OR WS-ERROR-FOUND
      *
           EXEC SQL CLOSE CRDCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
           END-IF
           MOVE 'N'                    TO WS-END-SW
      *
      *    THIS AUTHORISATION IS NOT ON THE TABLE YET
           ADD 1                       TO WS-CARD-CNT
           ADD WS-AMOUNT-WORK          TO WS-CARD-AMT
           .
       3100-EXIT.
           EXIT
           .
      *
       3150-FETCH-WINDOW.
           EXEC SQL
               FETCH CRDCSR
                INTO :DCL-WIN-TYPE
                   , :DCL-WIN-DETAIL
           END-EXEC
      *
           IF SQLCODE = +100
               MOVE 'Y'                TO WS-END-SW
               GO TO 3150-EXIT
           END-IF
      *
           IF SQLCODE NOT = 0
               MOVE 'FETCH   '         TO ER-SQL-OPERATION
               MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 3150-EXIT
           END-IF
      *
           ADD 1                       TO WS-CARD-CNT
           MOVE DCL-WIN-DETAIL         TO WS-DETAIL-AREA
      *
           EVALUATE DCL-WIN-TYPE
               WHEN 'P'
                   ADD WS-DA-PURCH-AMT TO WS-CARD-AMT
               WHEN 'C'
                   ADD WS-DA-CASH-AMT  TO WS-CARD-AMT
                   ADD WS-DA-CASH-FEE  TO WS-CARD-AMT
               WHEN 'R'
      *            A REFUND REDUCES THE EXPOSURE IN THE WINDOW
                   SUBTRACT WS-DA-REFUND-AMT FROM WS-CARD-AMT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       3150-EXIT.
           EXIT
           .
      *
       3200-COUNT-ACCOUNT.
           MOVE ZERO                   TO WS-ACCT-CNT
           MOVE ZERO                   TO WS-ACCT-AMT
      *
           EXEC SQL DECLARE ACCCSR CURSOR FOR
               SELECT AUTH_TYPE
                    , AUTH_DETAIL
                 FROM CARDSVC.AUTHORIZATION
                WHERE ACCT_ID     = :DCL-ACCT-ID
                  AND AUTH_STATUS IN ('A','F')
                  AND TIMESTAMP(AUTH_DATE, AUTH_TIME) >
                      CURRENT TIMESTAMP - :DCL-WINDOW-MIN MINUTES
           END-EXEC
      *
           EXEC SQL OPEN ACCCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 3200-EXIT
           END-IF
      *
           MOVE 'N'                    TO WS-END-SW
           PERFORM 3250-FETCH-ACCOUNT  THRU 3250-EXIT
               UNTIL WS-END-OF-RULES
                  OR WS-ERROR-FOUND
      *
           EXEC SQL CLOSE ACCCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
           END-IF
           MOVE 'N'                    TO WS-END-SW
      *
           ADD 1                       TO WS-ACCT-CNT
           ADD WS-AMOUNT-WORK          TO WS-ACCT-AMT
           .
       3200-EXIT.
           EXIT
           .
      *
       3250-FETCH-ACCOUNT.
           EXEC SQL
               FETCH ACCCSR
                INTO :DCL-WIN-TYPE
                   , :DCL-WIN-DETAIL
           END-EXEC
      *
           IF SQLCODE = +100
               MOVE 'Y'                TO WS-END-SW
               GO TO 3250-EXIT
           END-IF
      *
           IF SQLCODE NOT = 0
               MOVE 'FETCH   '         TO ER-SQL-OPERATION
               MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 3250-EXIT
           END-IF
      *
           ADD 1                       TO WS-ACCT-CNT
           MOVE DCL-WIN-DETAIL         TO WS-DETAIL-AREA
      *
           EVALUATE DCL-WIN-TYPE
               WHEN 'P'
                   ADD WS-DA-PURCH-AMT TO WS-ACCT-AMT
               WHEN 'C'
                   ADD WS-DA-CASH-AMT  TO WS-ACCT-AMT
                   ADD WS-DA-CASH-FEE  TO WS-ACCT-AMT
               WHEN 'R'
                   SUBTRACT WS-DA-REFUND-AMT FROM WS-ACCT-AMT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       3250-EXIT.
           EXIT
           .
      *
      *    A RUN OF DECLINES ON THE SAME CARD IS THE OLDEST SIGNAL WE
      *    HAVE AND IT IS SCORED WITHOUT A RULE ROW.
       3300-COUNT-DECLINES.
           MOVE ZERO                   TO WS-DECL-CNT
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-AUTH-CNT
                 FROM CARDSVC.AUTHORIZATION
                WHERE CARD_NUM    = :DCL-CARD-NUM
                  AND AUTH_STATUS = 'D'
                  AND TIMESTAMP(AUTH_DATE, AUTH_TIME) >
                      CURRENT TIMESTAMP - :DCL-WINDOW-MIN MINUTES
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'SELECT  '         TO ER-SQL-OPERATION
               MOVE 'AUTHORIZATION     '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
               GO TO 3300-EXIT
           END-IF
      *
           MOVE DCL-AUTH-CNT           TO WS-DECL-CNT
      *
           IF WS-DECL-CNT > 3
               MOVE 'VELDECL '         TO DCL-RULE-ID
               MOVE WS-RULE-CLASS      TO DCL-RULE-CLASS
               MOVE 25                 TO DCL-SCORE-POINTS
               MOVE 'REFR'             TO DCL-ACTION-CD
               PERFORM 4300-FIRE-RULE  THRU 4300-EXIT
           END-IF
           .
       3300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - WALK THE VELOCITY RULES                                 *
      ******************************************************************
       4000-APPLY-RULES.
           EXEC SQL DECLARE VELCSR CURSOR FOR
               SELECT RULE_ID
                    , RULE_CLASS
                    , RULE_SEQ
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
                  AND EFF_DATE   <= CURRENT DATE
                  AND EXP_DATE    > CURRENT DATE
                ORDER BY RULE_SEQ
           END-EXEC
      *
           EXEC SQL OPEN VELCSR END-EXEC
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
           MOVE 'Y'                    TO WS-CURSOR-SW
      *
           PERFORM 4100-NEXT-RULE      THRU 4100-EXIT
               UNTIL WS-END-OF-RULES
                  OR WS-ERROR-FOUND
      *
           EXEC SQL CLOSE VELCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE 'FRAUD_RULE        '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-DIAG   THRU 9100-EXIT
           END-IF
           MOVE 'N'                    TO WS-CURSOR-SW
           .
       4000-EXIT.
           EXIT
           .
      *
       4100-NEXT-RULE.
           EXEC SQL
               FETCH VELCSR
                INTO :DCL-RULE-ID
                   , :DCL-RULE-CLASS
                   , :DCL-RULE-SEQ
                   , :DCL-THRESH-AMT :IND-THRESH-AMT
                   , :DCL-THRESH-CNT :IND-THRESH-CNT
                   , :DCL-THRESH-PCT :IND-THRESH-PCT
                   , :DCL-SCORE-POINTS
                   , :DCL-ACTION-CD
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
      ******************************************************************
      * 4200 - EACH RULE ID IS A KNOWN TEST.  A RULE ID THIS PROGRAM   *
      * DOES NOT RECOGNISE IS IGNORED - THE ROW MAY BELONG TO A LATER  *
      * RELEASE THAT HAS NOT BEEN INSTALLED YET.                       *
      ******************************************************************
       4200-TEST-RULE.
           IF IND-THRESH-CNT < ZERO
               MOVE ZERO               TO DCL-THRESH-CNT
           END-IF
           IF IND-THRESH-AMT < ZERO
               MOVE ZERO               TO DCL-THRESH-AMT
           END-IF
      *
           EVALUATE DCL-RULE-ID
               WHEN 'VELCNTC '
                   IF DCL-THRESH-CNT = ZERO
                       MOVE WS-DEF-CNT-CARD
                                       TO DCL-THRESH-CNT
                   END-IF
                   IF WS-CARD-CNT > DCL-THRESH-CNT
                       PERFORM 4300-FIRE-RULE
                                       THRU 4300-EXIT
                   END-IF
               WHEN 'VELCNTA '
                   IF DCL-THRESH-CNT = ZERO
                       MOVE WS-DEF-CNT-ACCT
                                       TO DCL-THRESH-CNT
                   END-IF
                   IF WS-ACCT-CNT > DCL-THRESH-CNT
                       PERFORM 4300-FIRE-RULE
                                       THRU 4300-EXIT
                   END-IF
               WHEN 'VELAMTC '
                   IF DCL-THRESH-AMT = ZERO
                       MOVE WS-DEF-AMT-CARD
                                       TO DCL-THRESH-AMT
                   END-IF
                   IF WS-CARD-AMT > DCL-THRESH-AMT
                       PERFORM 4300-FIRE-RULE
                                       THRU 4300-EXIT
                   END-IF
               WHEN 'VELAMTA '
                   IF DCL-THRESH-AMT = ZERO
                       MOVE WS-DEF-AMT-ACCT
                                       TO DCL-THRESH-AMT
                   END-IF
                   IF WS-ACCT-AMT > DCL-THRESH-AMT
                       PERFORM 4300-FIRE-RULE
                                       THRU 4300-EXIT
                   END-IF
               WHEN 'VELCASH '
      *            CASH ADVANCES INSIDE THE WINDOW ARE SCORED HARDER
                   IF AUTH-CASH-ADV-TYPE
                       IF WS-CARD-CNT > 1
                           PERFORM 4300-FIRE-RULE
                                       THRU 4300-EXIT
                       END-IF
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
      * 4300 - RECORD ONE FIRED RULE                                   *
      ******************************************************************
       4300-FIRE-RULE.
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
           PERFORM 4400-PROMOTE-ACTION THRU 4400-EXIT
      *
           IF FW-REASON-CD = SPACES OR LOW-VALUES
               MOVE DCL-RULE-ID(1:4)   TO FW-REASON-CD
           END-IF
           .
       4300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4400 - SEVERITY IS ONLY EVER RAISED                            *
      ******************************************************************
       4400-PROMOTE-ACTION.
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
       4400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5000 - ADD THIS HANDLER'S CONTRIBUTION                         *
      ******************************************************************
       5000-POST-RESULT.
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
                   MOVE 'VELOCITY THRESHOLD EXCEEDED IN ROLLING WINDOW'
                                       TO FW-MESSAGE
               END-IF
           END-IF
      *
           IF WS-ERROR-FOUND
               MOVE 0008               TO FW-RC
               MOVE WS-PGM-ID          TO FW-FAIL-PGM
               MOVE SQLCODE            TO FW-SQLCODE
           END-IF
           .
       5000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 9100 - SQL DIAGNOSTIC.  THE HANDLER DOES NOT ABEND THE         *
      * AUTHORISATION - IT REPORTS AND LETS CACRD07 DECIDE.            *
      ******************************************************************
       9100-SQL-DIAG.
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE 'VELOCITY RULE PROCESSING FAILED'
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
