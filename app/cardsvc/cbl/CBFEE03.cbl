      ******************************************************************
      * CBFEE03 - FOREIGN TRANSACTION FEE HANDLER                      *
      *                                                                *
      * REACHED DYNAMICALLY.  THE CALLER MOVES 'FEEC' AND 'FRGN' INTO  *
      * THE ROUTE REQUEST AND CALLS CBCRD90.                           *
      *                                                                *
      * CALLED BY   - CBCRD90 ON BEHALF OF THE NIGHTLY FEE STEP        *
      * CALLS       - NONE                                             *
      * READS       - CARDSVC.FEE_SCHEDULE, CARDSVC.TRANSACTION        *
      *               CARDSVC.MERCHANT, FEEPARM (CONTROL CARDS)        *
      * UPDATES     - CARDSVC.TRANSACTION, CARDSVC.ACCOUNT             *
      *                                                                *
      * TWO ASSESSMENTS LIVE IN THIS PROGRAM -                         *
      *                                                                *
      *  1. CURRENCY CONVERSION.  THE TRANSACTION CURRENCY DIFFERS     *
      *     FROM THE ACCOUNT CURRENCY.  THE FEE IS A PERCENTAGE OF     *
      *     THE CONVERTED AMOUNT, THAT IS OF THE BILLING AMOUNT AFTER  *
      *     THE FX RATE HAS BEEN APPLIED, NEVER OF THE ORIGINAL        *
      *     CURRENCY AMOUNT.                                           *
      *                                                                *
      *  2. CROSS BORDER.  THE ACQUIRER COUNTRY DIFFERS FROM THE       *
      *     ACCOUNT COUNTRY EVEN THOUGH THE CURRENCY MATCHES - A       *
      *     DOMESTIC CURRENCY PURCHASE AT A FOREIGN ACQUIRER.  THE     *
      *     SCHEME STILL CHARGES THE ISSUER, SO THE CARDHOLDER IS      *
      *     ASSESSED AT THE CROSS BORDER RATE FROM THE FEEPARM CARD.   *
      *                                                                *
      * ROUNDING CONVENTION - ALL FEE ARITHMETIC IS CARRIED AT FIVE    *
      * DECIMAL PLACES AND ROUNDED HALF UP TO TWO DECIMAL PLACES ONCE, *
      * AT THE POINT THE FEE AMOUNT IS ESTABLISHED (COMPUTE ... ROUNDED*
      * IS HALF UP IN ENTERPRISE COBOL).  INTERMEDIATE RESULTS ARE     *
      * NEVER ROUNDED, AND THE CONVERTED AMOUNT IS NOT RE-DERIVED FROM *
      * THE ROUNDED FEE.  THIS MATCHES THE SCHEME SETTLEMENT RULE AND  *
      * WAS AGREED WITH FINANCE IN 2004 - DO NOT CHANGE IT WITHOUT     *
      * A REGRESSION AGAINST THE SETTLEMENT PROOF.                     *
      *                                                                *
      * RETURN AREA - BR-RETURN-CD, BR-FEE-TOTAL, BR-FEE-COUNT         *
      *   00 - FEE ASSESSED                                            *
      *   04 - NOT A FOREIGN TRANSACTION                               *
      *   08 - NO SCHEDULE ROW, NOT MY FEE TYPE, OR UNUSABLE BASIS     *
      *   12 - SQL FAILURE, THE CALLER MUST BACK OUT                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBFEE03.
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBFEE03 '.
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
           05  WS-REJECT-SW                PIC X     VALUE 'N'.
               88  WS-REJECTED                       VALUE 'Y'.
           05  WS-BASIS-SW                 PIC X     VALUE ' '.
               88  WS-BASIS-CONVERSION               VALUE 'C'.
               88  WS-BASIS-CROSS-BORDER             VALUE 'X'.
               88  WS-BASIS-NONE                     VALUE ' '.
      *
      ******************************************************************
      * FEEPARM CARDS                                                  *
      *   XBORDER-PCT   NN.NNNNN  CROSS BORDER RATE                    *
      *   XBORDER-MIN   NNNNN.NN  MINIMUM CROSS BORDER FEE             *
      *   FX-TOL-PCT    NN.NNNNN  TOLERANCE ON THE RATE SANITY CHECK   *
      *   HOME-COUNTRY  CCC       ISSUER COUNTRY FOR THE BORDER TEST   *
      *   MERCH-LOOKUP  Y/N       DERIVE THE COUNTRY FROM MERCHANT     *
      ******************************************************************
       01  WS-PARMS.
           05  WS-XBORDER-PCT              PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-XBORDER-MIN              PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-FX-TOL-PCT               PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-HOME-COUNTRY             PIC X(3)  VALUE SPACES.
           05  WS-MERCH-LOOKUP             PIC X     VALUE 'N'.
               88  WS-DO-MERCH-LOOKUP                VALUE 'Y'.
      *
       01  WS-WORK.
           05  WS-FEE-AMT                  PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-CONVERTED                PIC S9(11)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-RATE-USED                PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-RATE-DIFF                PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-ACQ-COUNTRY              PIC X(3)  VALUE SPACES.
           05  WS-DESC                     PIC X(40) VALUE SPACES.
      *
       01  WS-TXN-ID-BUILD.
           05  WS-TB-PREFIX                PIC X(3)  VALUE 'FEE'.
           05  WS-TB-TYPE                  PIC X(4)  VALUE 'FRGN'.
           05  WS-TB-ACCT                  PIC 9(9).
      *
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-YYYY                  PIC 9(4).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
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
           05  DCL-CURRENCY                PIC X(3).
           05  DCL-DESCRIPTION             PIC X(40).
      *
       01  DCL-TXN.
           05  DCL-TXN-ID                  PIC X(16).
           05  DCL-POST-DATE               PIC X(10).
           05  DCL-ACCT-ID                 PIC S9(11) COMP-3.
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-AMOUNT                  PIC S9(11)V99 COMP-3.
           05  DCL-BILLING-AMT             PIC S9(11)V99 COMP-3.
           05  DCL-FX-RATE                 PIC S9(3)V9(5) COMP-3.
           05  DCL-TXN-CURR                PIC X(3).
           05  DCL-TXN-TYPE                PIC X(4).
           05  DCL-TXN-DESC                PIC X(40).
           05  DCL-CYCLE-ID                PIC X(8).
           05  DCL-MERCHANT-ID             PIC X(15).
           05  DCL-COUNTRY-CD              PIC X(3).
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
           OR WS-REJECTED
               GO TO 0000-TERMINATE
           END-IF
      *
           IF NOT FW-FEE-FOREIGN
               MOVE 'Y'                    TO WS-REJECT-SW
               MOVE 'FEE TYPE NOT HANDLED BY CBFEE03'
                                           TO BR-RETURN-MSG
               GO TO 0000-TERMINATE
           END-IF
      *
           PERFORM 2000-CLASSIFY-BASIS
           IF WS-FATAL
           OR WS-REJECTED
               GO TO 0000-TERMINATE
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-BASIS-CONVERSION
                   PERFORM 3000-CONVERSION-FEE
               WHEN WS-BASIS-CROSS-BORDER
                   PERFORM 3500-CROSS-BORDER-FEE
               WHEN OTHER
                   MOVE 'DOMESTIC TRANSACTION - NO FOREIGN FEE'
                                           TO BR-RETURN-MSG
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
           MOVE 'CBFEE03 '                 TO BR-RETURN-PGM
           MOVE SPACES                     TO BR-RETURN-MSG
                                              BR-REASON-CD
                                              BR-WAIVER-RULE-CD
           MOVE ZERO                       TO BR-RETURN-CD
                                              BR-FEE-TOTAL
                                              BR-FEE-COUNT
           MOVE FW-FEE-TYPE                TO BR-FEE-TYPE-CD
           MOVE 'N'                        TO BR-FEE-WAIVED-FLG
                                              WS-CHARGE-SW
                                              WS-FATAL-SW
                                              WS-REJECT-SW
                                              WS-EOF-SW
           MOVE SPACE                      TO WS-BASIS-SW
           MOVE ZERO                       TO WS-FEE-AMT
      *
           MOVE FW-CYCLE-DATE              TO WS-DATE-WORK
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           MOVE WS-ISO-DATE                TO DCL-CYCLE-DATE
      *
           MOVE FW-TXN-POST-DATE           TO WS-DATE-WORK
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           MOVE WS-ISO-DATE                TO DCL-POST-DATE
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
               DISPLAY 'CBFEE03 FEEPARM OPEN STATUS '
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
           IF WS-HOME-COUNTRY = SPACES
               MOVE 'Y'                    TO WS-FATAL-SW
               MOVE 'NO HOME-COUNTRY CARD IN FEEPARM'
                                           TO BR-RETURN-MSG
           END-IF
           .
       1100-EXIT.
           EXIT
           .
      *
       1150-APPLY-CARD.
           EVALUATE FP-KEYWORD
               WHEN 'XBORDER-PCT '
                   COMPUTE WS-XBORDER-PCT =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'XBORDER-MIN '
                   COMPUTE WS-XBORDER-MIN =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'FX-TOL-PCT  '
                   COMPUTE WS-FX-TOL-PCT =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'HOME-COUNTRY'
                   MOVE FP-VALUE(1:3)      TO WS-HOME-COUNTRY
               WHEN 'MERCH-LOOKUP'
                   MOVE FP-VALUE(1:1)      TO WS-MERCH-LOOKUP
               WHEN '*           '
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'CBFEE03 UNKNOWN FEEPARM CARD - '
                           FP-KEYWORD
           END-EVALUATE
           .
      *
       1200-READ-SCHEDULE.
           MOVE FW-PRODUCT-CD              TO DCL-PRODUCT-CD
           MOVE 'FRGN'                     TO DCL-FEE-TYPE
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
                   MOVE 'Y'                TO WS-REJECT-SW
                   MOVE 'NO FRGN FEE SCHEDULE ROW FOR PRODUCT'
                                           TO BR-RETURN-MSG
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
                    , CURRENCY_CD
                    , DESCRIPTION
                 INTO :DCL-FLAT-AMT
                    , :DCL-PCT-RATE
                    , :DCL-MIN-AMT
                    , :DCL-MAX-AMT
                    , :DCL-CURRENCY
                    , :DCL-DESCRIPTION
                 FROM CARDSVC.FEE_SCHEDULE
                WHERE PRODUCT_CD = :DCL-PRODUCT-CD
                  AND FEE_TYPE   = 'FRGN'
                  AND DATE(:DCL-CYCLE-DATE)
                      BETWEEN EFF_DATE AND EXP_DATE
                FETCH FIRST 1 ROW ONLY
           END-EXEC
           .
      *
      ******************************************************************
      * 2000 - WHICH ASSESSMENT, IF ANY, APPLIES                       *
      ******************************************************************
       2000-CLASSIFY-BASIS.
           IF FW-BASIS-AMT = ZERO
               MOVE 'Y'                    TO WS-REJECT-SW
               MOVE 'NO TRANSACTION BASIS SUPPLIED'
                                           TO BR-RETURN-MSG
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2100-ACQUIRER-COUNTRY
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           EVALUATE TRUE
               WHEN FW-BASIS-CURR NOT = FW-CURRENCY
                   MOVE 'C'                TO WS-BASIS-SW
               WHEN WS-ACQ-COUNTRY NOT = SPACES
                AND WS-ACQ-COUNTRY NOT = WS-HOME-COUNTRY
                   MOVE 'X'                TO WS-BASIS-SW
               WHEN OTHER
                   MOVE SPACE              TO WS-BASIS-SW
           END-EVALUATE
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2100 - THE COUNTRY ON THE WORK AREA IS THE SETTLEMENT COUNTRY  *
      *        WHEN THE CLEARING FILE CARRIED ONE.  WHEN IT DID NOT,   *
      *        AND THE CARD ALLOWS IT, THE MERCHANT TABLE IS READ.     *
      ******************************************************************
       2100-ACQUIRER-COUNTRY.
           MOVE FW-BASIS-COUNTRY           TO WS-ACQ-COUNTRY
      *
           IF WS-ACQ-COUNTRY NOT = SPACES
               GO TO 2100-EXIT
           END-IF
      *
           IF NOT WS-DO-MERCH-LOOKUP
           OR FW-MERCHANT-ID = SPACES
               GO TO 2100-EXIT
           END-IF
      *
           MOVE FW-MERCHANT-ID             TO DCL-MERCHANT-ID
           EXEC SQL
               SELECT COUNTRY_CD
                 INTO :DCL-COUNTRY-CD
                 FROM CARDSVC.MERCHANT
                WHERE MERCHANT_ID = :DCL-MERCHANT-ID
                WITH UR
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-COUNTRY-CD     TO WS-ACQ-COUNTRY
               WHEN +100
                   DISPLAY 'CBFEE03 MERCHANT NOT ON FILE - '
                           FW-MERCHANT-ID
                   MOVE 'MNOF'             TO BR-REASON-CD
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
       2100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - CURRENCY CONVERSION FEE                                 *
      *                                                                *
      * FW-BASIS-AMT IS THE ORIGINAL CURRENCY AMOUNT AND FW-FX-RATE    *
      * THE RATE THE SCHEME SETTLED AT.  THE CONVERTED AMOUNT IS       *
      * CARRIED AT FIVE DECIMALS AND IS NOT ROUNDED - ONLY THE FEE IS. *
      ******************************************************************
       3000-CONVERSION-FEE.
           MOVE FW-FX-RATE                 TO WS-RATE-USED
      *
           IF WS-RATE-USED NOT > ZERO
               MOVE 'Y'                    TO WS-REJECT-SW
               MOVE 'FX RATE IS ZERO OR NEGATIVE'
                                           TO BR-RETURN-MSG
               DISPLAY 'CBFEE03 BAD FX RATE ACCT ' FW-ACCT-ID
                       ' TXN ' FW-TXN-ID
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 3100-RATE-SANITY
           IF WS-REJECTED
               GO TO 3000-EXIT
           END-IF
      *
           COMPUTE WS-CONVERTED = FW-BASIS-AMT * WS-RATE-USED
      *
           IF WS-CONVERTED < ZERO
               COMPUTE WS-CONVERTED = 0 - WS-CONVERTED
           END-IF
      *
           COMPUTE WS-FEE-AMT ROUNDED =
                   WS-CONVERTED * DCL-PCT-RATE / 100
      *
           PERFORM 4000-APPLY-BOUNDS
      *
           IF WS-FEE-AMT > ZERO
               MOVE 'Y'                    TO WS-CHARGE-SW
               MOVE 'FOREIGN CURRENCY TRANSACTION FEE'
                                           TO WS-DESC
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3100 - THE CLEARING FILE HAS BEEN KNOWN TO CARRY AN INVERTED   *
      *        RATE.  WHEN THE RATE IMPLIED BY THE BILLING AMOUNT ON   *
      *        THE TRANSACTION ROW DIFFERS FROM THE RATE ON THE WORK   *
      *        AREA BY MORE THAN THE TOLERANCE, THE TRANSACTION IS     *
      *        REJECTED FOR MANUAL PRICING RATHER THAN GUESSED AT.     *
      ******************************************************************
       3100-RATE-SANITY.
           MOVE FW-TXN-ID                  TO DCL-TXN-ID
      *
           EXEC SQL
               SELECT FX_RATE
                 INTO :DCL-FX-RATE
                 FROM CARDSVC.TRANSACTION
                WHERE TXN_ID    = :DCL-TXN-ID
                  AND POST_DATE = DATE(:DCL-POST-DATE)
                WITH UR
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
      *            THE FEE IS PRICED FROM THE WORK AREA ALONE
                   MOVE 'TNOF'             TO BR-REASON-CD
                   GO TO 3100-EXIT
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
                   GO TO 3100-EXIT
           END-EVALUATE
      *
           COMPUTE WS-RATE-DIFF = DCL-FX-RATE - WS-RATE-USED
           IF WS-RATE-DIFF < ZERO
               COMPUTE WS-RATE-DIFF = 0 - WS-RATE-DIFF
           END-IF
      *
           IF WS-RATE-DIFF > WS-FX-TOL-PCT
               MOVE 'Y'                    TO WS-REJECT-SW
               MOVE 'FX RATE DISAGREES WITH THE POSTED TRANSACTION'
                                           TO BR-RETURN-MSG
               MOVE 'FXDF'                 TO BR-REASON-CD
               DISPLAY 'CBFEE03 FX MISMATCH TXN ' FW-TXN-ID
                       ' WORK ' WS-RATE-USED
                       ' POSTED ' DCL-FX-RATE
           END-IF
           .
       3100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3500 - CROSS BORDER ASSESSMENT.  SAME CURRENCY, DIFFERENT      *
      *        ACQUIRER COUNTRY.  THE RATE IS THE FEEPARM RATE, NOT    *
      *        THE SCHEDULE RATE, BECAUSE THE SCHEME SETS IT AND IT    *
      *        CHANGES OUTSIDE THE PRODUCT PRICING CYCLE.              *
      ******************************************************************
       3500-CROSS-BORDER-FEE.
           COMPUTE WS-CONVERTED = FW-BASIS-AMT
           IF WS-CONVERTED < ZERO
               COMPUTE WS-CONVERTED = 0 - WS-CONVERTED
           END-IF
      *
           COMPUTE WS-FEE-AMT ROUNDED =
                   WS-CONVERTED * WS-XBORDER-PCT / 100
      *
           IF WS-FEE-AMT < WS-XBORDER-MIN
               MOVE WS-XBORDER-MIN         TO WS-FEE-AMT
               MOVE 'MINF'                 TO BR-REASON-CD
           END-IF
      *
           IF DCL-MAX-AMT > ZERO
           AND WS-FEE-AMT > DCL-MAX-AMT
               MOVE DCL-MAX-AMT            TO WS-FEE-AMT
           END-IF
      *
           IF WS-FEE-AMT > ZERO
               MOVE 'Y'                    TO WS-CHARGE-SW
               MOVE 'CROSS BORDER TRANSACTION FEE'
                                           TO WS-DESC
               MOVE 'XBOR'                 TO BR-REASON-CD
           END-IF
           .
      *
       4000-APPLY-BOUNDS.
           IF DCL-MIN-AMT > ZERO
           AND WS-FEE-AMT < DCL-MIN-AMT
               MOVE DCL-MIN-AMT            TO WS-FEE-AMT
               MOVE 'MINF'                 TO BR-REASON-CD
           END-IF
      *
           IF DCL-MAX-AMT > ZERO
           AND WS-FEE-AMT > DCL-MAX-AMT
               MOVE DCL-MAX-AMT            TO WS-FEE-AMT
               MOVE 'MAXF'                 TO BR-REASON-CD
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
           MOVE WS-TXN-ID-BUILD            TO DCL-TXN-ID
           MOVE FW-ACCT-ID                 TO DCL-ACCT-ID
           MOVE FW-CARD-NUM                TO DCL-CARD-NUM
           MOVE WS-FEE-AMT                 TO DCL-AMOUNT
                                              DCL-BILLING-AMT
           MOVE 'FRGN'                     TO DCL-TXN-TYPE
           MOVE WS-DESC                    TO DCL-TXN-DESC
           MOVE FW-CYCLE-ID                TO DCL-CYCLE-ID
           MOVE FW-CURRENCY                TO DCL-CURRENCY
           MOVE FW-MERCHANT-ID             TO DCL-MERCHANT-ID
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
                    , MERCHANT_ID
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
                    , :DCL-BILLING-AMT
                    , 1
                    , :DCL-MERCHANT-ID
                    , :DCL-TXN-DESC
                    , 1
                    , 'FEE ASSESSMENT'
                    , :DCL-CYCLE-ID
                    , 'CBFEE03 ')
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
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
                    , LAST_MAINT_PGM = 'CBFEE03 '
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
           DISPLAY 'CBFEE03 SQL ERROR SQLCODE=' SQLCODE
                   ' ACCT=' FW-ACCT-ID
                   ' TXN=' FW-TXN-ID
           MOVE 'SQL FAILURE IN CBFEE03'   TO BR-RETURN-MSG
           .
      *
       9900-SET-RETURN.
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO BR-RETURN-CD
               WHEN WS-REJECTED
                   MOVE 8                  TO BR-RETURN-CD
               WHEN WS-CHARGE-IT
                   MOVE 0                  TO BR-RETURN-CD
               WHEN OTHER
                   MOVE 4                  TO BR-RETURN-CD
           END-EVALUATE
           .
