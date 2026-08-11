      ******************************************************************
      * CBFEE01 - ANNUAL FEE AND LATE FEE HANDLER                      *
      *                                                                *
      * REACHED DYNAMICALLY FROM THE NIGHTLY FEE STEP.  THE CALLER     *
      * MOVES 'FEEC' AND EITHER 'ANNU' OR 'LATE' INTO THE ROUTE        *
      * REQUEST AND CALLS CBCRD90, WHICH CALLS THIS PROGRAM WITH THE   *
      * FEE WORK AREA AND A RETURN AREA.                               *
      *                                                                *
      * CALLED BY   - CBCRD90 ON BEHALF OF THE NIGHTLY FEE STEP        *
      * CALLS       - NONE                                             *
      * READS       - CARDSVC.FEE_SCHEDULE, CARDSVC.TRANSACTION        *
      *               FEEPARM (CONTROL CARDS)                          *
      * UPDATES     - CARDSVC.TRANSACTION, CARDSVC.ACCOUNT             *
      *                                                                *
      * NO RATE, THRESHOLD OR CAP IS CODED IN THE PROCEDURE DIVISION.  *
      * AMOUNTS COME FROM FEE_SCHEDULE, EVERYTHING ELSE FROM THE       *
      * FEEPARM CARDS.  CBCRD90 CANCELS THIS PROGRAM AFTER EVERY CALL  *
      * SO THE CARDS ARE RE-READ EACH TIME - FEEPARM IS A FEW RECORDS  *
      * AND STAYS IN THE BUFFER POOL.                                  *
      *                                                                *
      * ANNUAL FEE - CHARGED IN THE ANNIVERSARY MONTH OF THE ACCOUNT   *
      * OPEN DATE.  WAIVED IN THE FIRST YEAR, WAIVED WHEN THE PRODUCT  *
      * WAIVER RULE SAYS SO, AND NEVER CHARGED TWICE IN TWELVE MONTHS. *
      *                                                                *
      * LATE FEE - CHARGED WHEN THE MINIMUM PAYMENT WAS NOT MET BY THE *
      * DUE DATE.  THE AMOUNT IS TIERED ON THE BALANCE, CAPPED BY THE  *
      * REGULATORY MAXIMUM ON THE CARD, AND SUPPRESSED WHEN A LATE FEE *
      * WAS ALREADY CHARGED IN THE PREVIOUS CYCLE.                     *
      *                                                                *
      * RETURN AREA - BR-RETURN-CD, BR-FEE-TOTAL, BR-FEE-COUNT         *
      *   00 - FEE ASSESSED                                            *
      *   04 - NOTHING TO ASSESS OR FEE WAIVED                         *
      *   08 - NO FEE SCHEDULE ROW / NOT MY FEE TYPE                   *
      *   12 - SQL FAILURE, THE CALLER MUST BACK OUT                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBFEE01.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FEEPARM-FILE ASSIGN TO FEEPARM
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-FEEPARM-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  FEEPARM-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  FEEPARM-REC.
           05  FP-KEYWORD                  PIC X(12).
           05  FILLER                      PIC X.
           05  FP-VALUE                    PIC X(20).
           05  FILLER                      PIC X(47).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBFEE01 '.
      *
       01  WS-FEEPARM-STATUS               PIC X(2)  VALUE '00'.
           88  WS-FEEPARM-OK                         VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
           05  WS-CHARGE-SW                PIC X     VALUE 'N'.
               88  WS-CHARGE-IT                      VALUE 'Y'.
           05  WS-WAIVED-SW                PIC X     VALUE 'N'.
               88  WS-WAIVED                         VALUE 'Y'.
           05  WS-NOSCHED-SW               PIC X     VALUE 'N'.
               88  WS-NO-SCHEDULE                    VALUE 'Y'.
      *
      ******************************************************************
      * FEEPARM CARDS                                                  *
      *   REG-LATE-CAP  NNNNN.NN  REGULATORY MAXIMUM LATE FEE          *
      *   LATE-TIER1    NNNNN.NN  BALANCE AT WHICH TIER 2 STARTS       *
      *   LATE-TIER2    NNNNN.NN  BALANCE AT WHICH TIER 3 STARTS       *
      *   TIER1-PCT     NN.NNNNN  PERCENT OF THE SCHEDULE FLAT AMOUNT  *
      *   TIER2-PCT     NN.NNNNN                                       *
      *   TIER3-PCT     NN.NNNNN                                       *
      *   ANNU-WAIVE-MM NN        MONTHS OF LIFE BEFORE THE FIRST FEE  *
      *   ANNU-REPEAT   NNN       DAYS THAT MUST PASS BEFORE THE NEXT  *
      *   MIN-PAY-TOL   NNNNN.NN  SHORTFALL IGNORED ON A LATE TEST     *
      ******************************************************************
       01  WS-PARMS.
           05  WS-REG-LATE-CAP             PIC S9(7)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-LATE-TIER1               PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-LATE-TIER2               PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-TIER1-PCT                PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-TIER2-PCT                PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-TIER3-PCT                PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-ANNU-WAIVE-MM            PIC 9(2)  VALUE ZERO.
           05  WS-ANNU-REPEAT              PIC 9(3)  VALUE ZERO.
           05  WS-MIN-PAY-TOL              PIC S9(7)V99 COMP-3
                                                     VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-FEE-AMT                  PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-PAID-AMT                 PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-SHORTFALL                PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-MONTHS-OPEN              PIC S9(5) COMP VALUE ZERO.
           05  WS-DAYS-SINCE               PIC S9(9) COMP VALUE ZERO.
           05  WS-INT-A                    PIC S9(9) COMP VALUE ZERO.
           05  WS-INT-B                    PIC S9(9) COMP VALUE ZERO.
           05  WS-TXN-ID                   PIC X(16) VALUE SPACES.
           05  WS-DESC                     PIC X(40) VALUE SPACES.
      *
       01  WS-TXN-ID-BUILD.
           05  WS-TB-PREFIX                PIC X(3)  VALUE 'FEE'.
           05  WS-TB-TYPE                  PIC X(4).
           05  WS-TB-ACCT                  PIC 9(9).
      *
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-YYYY                  PIC 9(4).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
      *
       01  WS-OPEN-DATE                    PIC 9(8)  VALUE ZERO.
       01  WS-OPEN-PARTS REDEFINES WS-OPEN-DATE.
           05  WS-OD-YYYY                  PIC 9(4).
           05  WS-OD-MM                    PIC 9(2).
           05  WS-OD-DD                    PIC 9(2).
      *
       01  WS-ISO-DATE.
           05  WS-ISO-YYYY                 PIC 9(4).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-MM                   PIC 9(2).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-DD                   PIC 9(2).
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-FEE.
           05  DCL-PRODUCT-CD              PIC X(4).
           05  DCL-FEE-TYPE                PIC X(4).
           05  DCL-CYCLE-DATE              PIC X(10).
           05  DCL-FLAT-AMT                PIC S9(9)V99 COMP-3.
           05  DCL-PCT-RATE                PIC S9(3)V9(5) COMP-3.
           05  DCL-MIN-AMT                 PIC S9(9)V99 COMP-3.
           05  DCL-MAX-AMT                 PIC S9(9)V99 COMP-3.
           05  DCL-WAIVER-RULE             PIC X(4).
           05  DCL-CURRENCY                PIC X(3).
           05  DCL-DESCRIPTION             PIC X(40).
      *
       01  DCL-POST.
           05  DCL-TXN-ID                  PIC X(16).
           05  DCL-ACCT-ID                 PIC S9(11) COMP-3.
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-AMOUNT                  PIC S9(11)V99 COMP-3.
           05  DCL-TXN-TYPE                PIC X(4).
           05  DCL-TXN-DESC                PIC X(40).
           05  DCL-CYCLE-ID                PIC X(8).
           05  DCL-DUE-DATE                PIC X(10).
           05  DCL-PAID-AMT                PIC S9(13)V99 COMP-3.
      *
       01  IND-VARS.
           05  IND-WAIVER                  PIC S9(4) COMP.
           05  IND-PAID                    PIC S9(4) COMP.
      *
       LINKAGE SECTION.
           COPY CVFEEW1Y.
           COPY CVBRTN1Y.
      *
      ******************************************************************
       PROCEDURE DIVISION USING FEE-WORK-AREA
                                BATCH-RETURN-AREA.
      *
       0000-MAIN-LINE.
           PERFORM 1000-INITIALISE
           IF WS-FATAL
           OR WS-NO-SCHEDULE
               GO TO 0000-TERMINATE
           END-IF
      *
           EVALUATE TRUE
               WHEN FW-FEE-ANNUAL
                   PERFORM 2000-ANNUAL-FEE
               WHEN FW-FEE-LATE
                   PERFORM 3000-LATE-FEE
               WHEN OTHER
                   MOVE 8                  TO BR-RETURN-CD
                   MOVE 'FEE TYPE NOT HANDLED BY CBFEE01'
                                           TO BR-RETURN-MSG
                   GO TO 0000-TERMINATE
           END-EVALUATE
      *
           IF WS-CHARGE-IT
           AND NOT WS-FATAL
               PERFORM 5000-POST-FEE
           END-IF
           .
       0000-TERMINATE.
           PERFORM 9900-SET-RETURN
           GOBACK
           .
      *
       1000-INITIALISE.
           MOVE 'CBFEE01 '                 TO BR-RETURN-PGM
           MOVE SPACES                     TO BR-RETURN-MSG
                                              BR-WAIVER-RULE-CD
                                              BR-REASON-CD
           MOVE ZERO                       TO BR-RETURN-CD
                                              BR-FEE-TOTAL
                                              BR-FEE-COUNT
           MOVE FW-FEE-TYPE                TO BR-FEE-TYPE-CD
           MOVE 'N'                        TO BR-FEE-WAIVED-FLG
                                              WS-CHARGE-SW
                                              WS-WAIVED-SW
                                              WS-FATAL-SW
                                              WS-NOSCHED-SW
                                              WS-EOF-SW
           MOVE ZERO                       TO WS-FEE-AMT
      *
           MOVE FW-CYCLE-DATE              TO WS-DATE-WORK
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           MOVE WS-ISO-DATE                TO DCL-CYCLE-DATE
      *
           PERFORM 1100-READ-PARMS
           IF WS-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           PERFORM 1200-READ-SCHEDULE
           .
       1000-EXIT.
           EXIT
           .
      *
       1100-READ-PARMS.
           OPEN INPUT FEEPARM-FILE
           IF NOT WS-FEEPARM-OK
               MOVE 'Y'                    TO WS-FATAL-SW
               MOVE 'FEEPARM OPEN FAILED'  TO BR-RETURN-MSG
               DISPLAY 'CBFEE01 FEEPARM OPEN STATUS '
                       WS-FEEPARM-STATUS
               GO TO 1100-EXIT
           END-IF
      *
           PERFORM UNTIL WS-EOF
               READ FEEPARM-FILE
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       PERFORM 1150-APPLY-CARD
               END-READ
           END-PERFORM
      *
           CLOSE FEEPARM-FILE
      *
           IF WS-REG-LATE-CAP = ZERO
           OR WS-ANNU-REPEAT  = ZERO
               MOVE 'Y'                    TO WS-FATAL-SW
               MOVE 'FEEPARM CARD MISSING' TO BR-RETURN-MSG
           END-IF
           .
       1100-EXIT.
           EXIT
           .
      *
       1150-APPLY-CARD.
           EVALUATE FP-KEYWORD
               WHEN 'REG-LATE-CAP'
                   COMPUTE WS-REG-LATE-CAP =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'LATE-TIER1  '
                   COMPUTE WS-LATE-TIER1 =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'LATE-TIER2  '
                   COMPUTE WS-LATE-TIER2 =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'TIER1-PCT   '
                   COMPUTE WS-TIER1-PCT =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'TIER2-PCT   '
                   COMPUTE WS-TIER2-PCT =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'TIER3-PCT   '
                   COMPUTE WS-TIER3-PCT =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'ANNU-WAIVE-M'
                   MOVE FP-VALUE(1:2)      TO WS-ANNU-WAIVE-MM
               WHEN 'ANNU-REPEAT '
                   MOVE FP-VALUE(1:3)      TO WS-ANNU-REPEAT
               WHEN 'MIN-PAY-TOL '
                   COMPUTE WS-MIN-PAY-TOL =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN '*           '
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'CBFEE01 UNKNOWN FEEPARM CARD - '
                           FP-KEYWORD
           END-EVALUATE
           .
      *
      ******************************************************************
      * 1200 - THE FEE SCHEDULE ROW.  A PRODUCT WITHOUT ITS OWN ROW    *
      *        FALLS BACK TO THE GENERIC PRODUCT '****'.               *
      ******************************************************************
       1200-READ-SCHEDULE.
           MOVE FW-PRODUCT-CD              TO DCL-PRODUCT-CD
           MOVE FW-FEE-TYPE                TO DCL-FEE-TYPE
           PERFORM 1250-SELECT-SCHEDULE
      *
           IF SQLCODE = +100
               MOVE '****'                 TO DCL-PRODUCT-CD
               PERFORM 1250-SELECT-SCHEDULE
           END-IF
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'NO FEE SCHEDULE ROW FOR PRODUCT AND TYPE'
                                           TO BR-RETURN-MSG
                   MOVE 'Y'                TO WS-NOSCHED-SW
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       1250-SELECT-SCHEDULE.
           EXEC SQL
               SELECT FLAT_AMT
                    , PCT_RATE
                    , MIN_AMT
                    , MAX_AMT
                    , WAIVER_RULE_CD
                    , CURRENCY_CD
                    , DESCRIPTION
                 INTO :DCL-FLAT-AMT
                    , :DCL-PCT-RATE
                    , :DCL-MIN-AMT
                    , :DCL-MAX-AMT
                    , :DCL-WAIVER-RULE :IND-WAIVER
                    , :DCL-CURRENCY
                    , :DCL-DESCRIPTION
                 FROM CARDSVC.FEE_SCHEDULE
                WHERE PRODUCT_CD = :DCL-PRODUCT-CD
                  AND FEE_TYPE   = :DCL-FEE-TYPE
                  AND DATE(:DCL-CYCLE-DATE)
                      BETWEEN EFF_DATE AND EXP_DATE
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           IF IND-WAIVER < ZERO
               MOVE SPACES                 TO DCL-WAIVER-RULE
           END-IF
           .
      *
      ******************************************************************
      * 2000 - ANNUAL FEE                                              *
      ******************************************************************
       2000-ANNUAL-FEE.
           MOVE FW-OPEN-DATE               TO WS-OPEN-DATE
           MOVE FW-CYCLE-DATE              TO WS-DATE-WORK
      *
      *    ONLY IN THE ANNIVERSARY MONTH
           IF WS-OD-MM NOT = WS-DW-MM
               MOVE 'NOT THE ANNIVERSARY MONTH'
                                           TO BR-RETURN-MSG
               GO TO 2000-EXIT
           END-IF
      *
      *    FIRST YEAR WAIVER - THE ACCOUNT MUST HAVE LIVED LONG ENOUGH
           COMPUTE WS-MONTHS-OPEN =
                   ((WS-DW-YYYY - WS-OD-YYYY) * 12)
                   + (WS-DW-MM - WS-OD-MM)
      *
           IF WS-MONTHS-OPEN < WS-ANNU-WAIVE-MM
               MOVE 'Y'                    TO WS-WAIVED-SW
               MOVE 'FYR1'                 TO BR-WAIVER-RULE-CD
               MOVE 'FIRST YEAR WAIVER'    TO BR-RETURN-MSG
               GO TO 2000-EXIT
           END-IF
      *
      *    NOT TWICE INSIDE THE REPEAT WINDOW
           IF FW-PRIOR-ANNU-FEE-DT > ZERO
               MOVE FW-PRIOR-ANNU-FEE-DT   TO WS-DATE-WORK
               COMPUTE WS-INT-A =
                       FUNCTION INTEGER-OF-DATE(WS-DATE-WORK)
               MOVE FW-CYCLE-DATE          TO WS-DATE-WORK
               COMPUTE WS-INT-B =
                       FUNCTION INTEGER-OF-DATE(WS-DATE-WORK)
               COMPUTE WS-DAYS-SINCE = WS-INT-B - WS-INT-A
               IF WS-DAYS-SINCE < WS-ANNU-REPEAT
                   MOVE 'ANNUAL FEE ALREADY CHARGED THIS YEAR'
                                           TO BR-RETURN-MSG
                   GO TO 2000-EXIT
               END-IF
           END-IF
      *
           PERFORM 2100-CHECK-WAIVER-RULE
           IF WS-WAIVED
               GO TO 2000-EXIT
           END-IF
      *
           MOVE DCL-FLAT-AMT               TO WS-FEE-AMT
           IF DCL-MAX-AMT > ZERO
           AND WS-FEE-AMT > DCL-MAX-AMT
               MOVE DCL-MAX-AMT            TO WS-FEE-AMT
           END-IF
      *
           IF WS-FEE-AMT > ZERO
               MOVE 'Y'                    TO WS-CHARGE-SW
               MOVE 'ANNU'                 TO WS-TB-TYPE
               MOVE 'ANNUAL MEMBERSHIP FEE'
                                           TO WS-DESC
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2100 - PRODUCT LEVEL WAIVER RULES.  THE RULE CODE COMES FROM   *
      *        THE FEE SCHEDULE ROW, NOT FROM THIS PROGRAM.            *
      *          VIPW - WAIVED FOR A RELATIONSHIP CUSTOMER             *
      *          BALW - WAIVED WHEN THE BALANCE CARRIED IS HIGH        *
      *          SPND - WAIVED ON QUALIFYING SPEND IN THE LAST YEAR    *
      *          MANU - WAIVED BY THE ACCOUNT LEVEL FLAG               *
      ******************************************************************
       2100-CHECK-WAIVER-RULE.
           EVALUATE DCL-WAIVER-RULE
               WHEN 'VIPW'
                   IF FW-VIP-FLG = 'Y'
                       MOVE 'Y'            TO WS-WAIVED-SW
                   END-IF
               WHEN 'BALW'
                   IF FW-CURR-BAL > DCL-MIN-AMT
                       MOVE 'Y'            TO WS-WAIVED-SW
                   END-IF
               WHEN 'SPND'
                   PERFORM 2200-QUALIFYING-SPEND
                   IF WS-PAID-AMT > DCL-MIN-AMT
                       MOVE 'Y'            TO WS-WAIVED-SW
                   END-IF
               WHEN 'MANU'
                   IF FW-ANNIV-WAIVE-FLG = 'Y'
                       MOVE 'Y'            TO WS-WAIVED-SW
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF WS-WAIVED
               MOVE DCL-WAIVER-RULE        TO BR-WAIVER-RULE-CD
               MOVE 'ANNUAL FEE WAIVED BY PRODUCT RULE'
                                           TO BR-RETURN-MSG
           END-IF
           .
      *
       2200-QUALIFYING-SPEND.
           MOVE FW-ACCT-ID                 TO DCL-ACCT-ID
           MOVE ZERO                       TO WS-PAID-AMT
      *
           EXEC SQL
               SELECT SUM(BILLING_AMT)
                 INTO :DCL-PAID-AMT :IND-PAID
                 FROM CARDSVC.TRANSACTION
                WHERE ACCT_ID     = :DCL-ACCT-ID
                  AND TXN_TYPE_CD IN ('PURC','RECU')
                  AND POST_DATE   >
                      DATE(:DCL-CYCLE-DATE) - 1 YEAR
                  AND POST_DATE  <= DATE(:DCL-CYCLE-DATE)
                WITH UR
           END-EXEC
      *
           EVALUATE TRUE
               WHEN SQLCODE = 0 AND IND-PAID NOT < ZERO
                   MOVE DCL-PAID-AMT       TO WS-PAID-AMT
               WHEN SQLCODE = 0
                   MOVE ZERO               TO WS-PAID-AMT
               WHEN SQLCODE = +100
                   MOVE ZERO               TO WS-PAID-AMT
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - LATE FEE                                                *
      ******************************************************************
       3000-LATE-FEE.
           IF FW-MIN-PAY-DUE NOT > ZERO
               MOVE 'NO MINIMUM PAYMENT WAS DUE'
                                           TO BR-RETURN-MSG
               GO TO 3000-EXIT
           END-IF
      *
           IF FW-PAY-DUE-DATE = ZERO
           OR FW-PAY-DUE-DATE > FW-CYCLE-DATE
               MOVE 'PAYMENT NOT YET DUE'  TO BR-RETURN-MSG
               GO TO 3000-EXIT
           END-IF
      *
      *    A LATE FEE IN THE PREVIOUS CYCLE SUPPRESSES THIS ONE - THE
      *    ACCOUNT GOES TO COLLECTIONS INSTEAD OF BEING FEED AGAIN.
           IF FW-PRIOR-LATE-FEE-FLG = 'Y'
               MOVE 'LATE FEE CHARGED IN THE PREVIOUS CYCLE'
                                           TO BR-RETURN-MSG
               MOVE 'SUPP'                 TO BR-REASON-CD
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 3100-PAYMENTS-RECEIVED
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
      *
           COMPUTE WS-SHORTFALL = FW-MIN-PAY-DUE - WS-PAID-AMT
           IF WS-SHORTFALL NOT > WS-MIN-PAY-TOL
               MOVE 'MINIMUM PAYMENT WAS MET'
                                           TO BR-RETURN-MSG
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 3200-TIER-THE-FEE
      *
           IF WS-FEE-AMT > ZERO
               MOVE 'Y'                    TO WS-CHARGE-SW
               MOVE 'LATE'                 TO WS-TB-TYPE
               MOVE 'LATE PAYMENT FEE'     TO WS-DESC
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3100 - WHAT THE CUSTOMER ACTUALLY PAID BETWEEN THE LAST CYCLE  *
      *        AND THE DUE DATE.  PAYMENTS POST AS NEGATIVE BILLING    *
      *        AMOUNTS SO THE SUM IS NEGATED.                          *
      ******************************************************************
       3100-PAYMENTS-RECEIVED.
           MOVE FW-ACCT-ID                 TO DCL-ACCT-ID
           MOVE FW-PAY-DUE-DATE            TO WS-DATE-WORK
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           MOVE WS-ISO-DATE                TO DCL-DUE-DATE
           MOVE ZERO                       TO WS-PAID-AMT
      *
           EXEC SQL
               SELECT SUM(BILLING_AMT)
                 INTO :DCL-PAID-AMT :IND-PAID
                 FROM CARDSVC.TRANSACTION
                WHERE ACCT_ID     = :DCL-ACCT-ID
                  AND TXN_TYPE_CD = 'PYMT'
                  AND POST_DATE  <= DATE(:DCL-DUE-DATE)
                  AND POST_DATE   >
                      DATE(:DCL-DUE-DATE) - 1 MONTH
                WITH UR
           END-EXEC
      *
           EVALUATE TRUE
               WHEN SQLCODE = 0 AND IND-PAID NOT < ZERO
                   COMPUTE WS-PAID-AMT = 0 - DCL-PAID-AMT
               WHEN SQLCODE = 0
                   MOVE ZERO               TO WS-PAID-AMT
               WHEN SQLCODE = +100
                   MOVE ZERO               TO WS-PAID-AMT
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
      *
           IF WS-PAID-AMT < ZERO
               MOVE ZERO                   TO WS-PAID-AMT
           END-IF
           .
      *
      ******************************************************************
      * 3200 - TIER AND CAP.  THE TIER PERCENTAGES ARE APPLIED TO THE  *
      *        SCHEDULE FLAT AMOUNT, THEN THE SCHEDULE MINIMUM AND     *
      *        MAXIMUM APPLY, THEN THE REGULATORY CAP ON THE CARD.     *
      ******************************************************************
       3200-TIER-THE-FEE.
           EVALUATE TRUE
               WHEN FW-CURR-BAL < WS-LATE-TIER1
                   COMPUTE WS-FEE-AMT ROUNDED =
                           DCL-FLAT-AMT * WS-TIER1-PCT / 100
               WHEN FW-CURR-BAL < WS-LATE-TIER2
                   COMPUTE WS-FEE-AMT ROUNDED =
                           DCL-FLAT-AMT * WS-TIER2-PCT / 100
               WHEN OTHER
                   COMPUTE WS-FEE-AMT ROUNDED =
                           DCL-FLAT-AMT * WS-TIER3-PCT / 100
           END-EVALUATE
      *
           IF DCL-MIN-AMT > ZERO
           AND WS-FEE-AMT < DCL-MIN-AMT
               MOVE DCL-MIN-AMT            TO WS-FEE-AMT
           END-IF
      *
           IF DCL-MAX-AMT > ZERO
           AND WS-FEE-AMT > DCL-MAX-AMT
               MOVE DCL-MAX-AMT            TO WS-FEE-AMT
           END-IF
      *
      *    THE FEE MAY NEVER EXCEED THE AMOUNT THE CUSTOMER FAILED TO
      *    PAY - THAT IS THE REGULATION, NOT A LOCAL RULE.
           IF WS-FEE-AMT > WS-SHORTFALL
               MOVE WS-SHORTFALL           TO WS-FEE-AMT
               MOVE 'SHRT'                 TO BR-REASON-CD
           END-IF
      *
           IF WS-FEE-AMT > WS-REG-LATE-CAP
               MOVE WS-REG-LATE-CAP        TO WS-FEE-AMT
               MOVE 'RCAP'                 TO BR-REASON-CD
           END-IF
           .
      *
      ******************************************************************
      * 5000 - POST THE FEE                                            *
      ******************************************************************
       5000-POST-FEE.
           IF FW-SIMULATE
               MOVE 'SIMULATED - NOTHING POSTED'
                                           TO BR-RETURN-MSG
               MOVE WS-FEE-AMT             TO BR-FEE-TOTAL
               MOVE 1                      TO BR-FEE-COUNT
               GO TO 5000-EXIT
           END-IF
      *
           MOVE FW-ACCT-ID(3:9)            TO WS-TB-ACCT
           MOVE WS-TXN-ID-BUILD            TO WS-TXN-ID
           MOVE WS-TXN-ID                  TO DCL-TXN-ID
           MOVE FW-ACCT-ID                 TO DCL-ACCT-ID
           MOVE FW-CARD-NUM                TO DCL-CARD-NUM
           MOVE WS-FEE-AMT                 TO DCL-AMOUNT
           MOVE WS-TB-TYPE                 TO DCL-TXN-TYPE
           MOVE WS-DESC                    TO DCL-TXN-DESC
           MOVE FW-CYCLE-ID                TO DCL-CYCLE-ID
      *
           EXEC SQL
               INSERT INTO CARDSVC.TRANSACTION
                     (TXN_ID
                    , POST_DATE
                    , ACCT_ID
                    , CARD_NUM
                    , TXN_TYPE_CD
                    , TXN_SOURCE
                    , TXN_AMT
                    , CURRENCY_CD
                    , BILLING_AMT
                    , FX_RATE
                    , TXN_DESC
                    , TXN_LEG_CNT
                    , TXN_LEG_DATA
                    , CYCLE_ID
                    , POSTED_BY)
               VALUES (:DCL-TXN-ID
                    , DATE(:DCL-CYCLE-DATE)
                    , :DCL-ACCT-ID
                    , :DCL-CARD-NUM
                    , :DCL-TXN-TYPE
                    , 'BF'
                    , :DCL-AMOUNT
                    , :DCL-CURRENCY
                    , :DCL-AMOUNT
                    , 1
                    , :DCL-TXN-DESC
                    , 1
                    , 'FEE ASSESSMENT'
                    , :DCL-CYCLE-ID
                    , 'CBFEE01 ')
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
      *            THE CYCLE HAS BEEN RE-DRIVEN - THE FEE IS THERE
                   MOVE 'FEE ALREADY POSTED FOR THIS CYCLE'
                                           TO BR-RETURN-MSG
                   MOVE 'DUPL'             TO BR-REASON-CD
                   MOVE 'N'                TO WS-CHARGE-SW
                   GO TO 5000-EXIT
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
                   GO TO 5000-EXIT
           END-EVALUATE
      *
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET CURR_BAL       = CURR_BAL + :DCL-AMOUNT
                    , LAST_MAINT_PGM = 'CBFEE01 '
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE ACCT_ID        = :DCL-ACCT-ID
           END-EXEC
      *
           IF SQLCODE NOT = 0
               PERFORM 9400-SQL-ERROR
               GO TO 5000-EXIT
           END-IF
      *
           MOVE WS-FEE-AMT                 TO BR-FEE-TOTAL
           MOVE 1                          TO BR-FEE-COUNT
           MOVE DCL-DESCRIPTION            TO BR-RETURN-MSG
           .
       5000-EXIT.
           EXIT
           .
      *
       9400-SQL-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBFEE01 SQL ERROR SQLCODE=' SQLCODE
                   ' ACCT=' FW-ACCT-ID
                   ' TYPE=' FW-FEE-TYPE
           MOVE 'SQL FAILURE IN CBFEE01'   TO BR-RETURN-MSG
           .
      *
       9900-SET-RETURN.
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO BR-RETURN-CD
               WHEN WS-WAIVED
                   MOVE 'Y'                TO BR-FEE-WAIVED-FLG
                   MOVE 4                  TO BR-RETURN-CD
               WHEN WS-NO-SCHEDULE
                   MOVE 8                  TO BR-RETURN-CD
               WHEN WS-CHARGE-IT
                   MOVE 0                  TO BR-RETURN-CD
               WHEN OTHER
                   MOVE 4                  TO BR-RETURN-CD
           END-EVALUATE
           .
