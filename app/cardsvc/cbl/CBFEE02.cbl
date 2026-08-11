      ******************************************************************
      * CBFEE02 - OVER LIMIT FEE AND CASH ADVANCE FEE HANDLER          *
      *                                                                *
      * REACHED DYNAMICALLY.  THE CALLER MOVES 'FEEC' AND EITHER       *
      * 'OVLM' OR 'CASH' INTO THE ROUTE REQUEST AND CALLS CBCRD90.     *
      *                                                                *
      * CALLED BY   - CBCRD90 ON BEHALF OF THE NIGHTLY FEE STEP        *
      * CALLS       - NONE                                             *
      * READS       - CARDSVC.FEE_SCHEDULE, CARDSVC.TRANSACTION        *
      *               FEEPARM (CONTROL CARDS)                          *
      * UPDATES     - CARDSVC.TRANSACTION, CARDSVC.ACCOUNT             *
      *                                                                *
      * OVER LIMIT - ONLY WHEN THE ACCOUNT HAS OPTED IN.  WITHOUT THE  *
      * OPT IN THE BANK MAY NOT CHARGE, WHATEVER THE EXCESS IS.  ONE   *
      * OVER LIMIT FEE PER CYCLE - THE TRANSACTION TABLE IS CHECKED    *
      * BEFORE THE FEE IS POSTED BECAUSE THE NIGHTLY CYCLE CAN BE      *
      * RE-DRIVEN AFTER AN ABEND.                                      *
      *                                                                *
      * CASH ADVANCE - A PERCENTAGE OF THE ADVANCE WITH A MINIMUM,     *
      * TAKEN FROM THE AUTHORIZATION IMAGE CARRIED IN FW-AUTH-IMAGE.   *
      * THE IMAGE IS THE 60 BYTE DETAIL AREA FROM CVAUTH01Y AND IS     *
      * ADDRESSED THROUGH THE AUTH-CASH-ADV REDEFINES UNDER AN         *
      * EVALUATE AUTH-TYPE.  THE CASH VARIANT IS SHORTER THAN THE      *
      * AREA, SO THE TAIL BYTES HOLD WHATEVER THE PREVIOUS RECORD      *
      * LEFT THERE AND MUST NOT BE TRUSTED - THE CURRENCY IS           *
      * VALIDATED BEFORE IT IS USED.                                   *
      *                                                                *
      * RETURN AREA - BR-RETURN-CD, BR-FEE-TOTAL, BR-FEE-COUNT         *
      *   00 - FEE ASSESSED                                            *
      *   04 - NOTHING TO ASSESS                                       *
      *   08 - NO SCHEDULE ROW, NOT MY FEE TYPE, OR UNUSABLE IMAGE     *
      *   12 - SQL FAILURE, THE CALLER MUST BACK OUT                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBFEE02.
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBFEE02 '.
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
      *
      ******************************************************************
      * FEEPARM CARDS                                                  *
      *   OVLM-TOL      NNNNN.NN  EXCESS IGNORED BEFORE A FEE IS DUE   *
      *   OVLM-MAX-CYC  N         FEES ALLOWED IN ONE CYCLE            *
      *   CASH-SURCHG   Y/N       PASS THE ATM SURCHARGE ON            *
      *   CASH-MIN-ADV  NNNNN.NN  ADVANCE BELOW WHICH NO FEE IS DUE    *
      *   VALID-CURR    CCC       CURRENCIES THE ATM FEED MAY CARRY    *
      ******************************************************************
       01  WS-PARMS.
           05  WS-OVLM-TOL                 PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-OVLM-MAX-CYC             PIC 9      VALUE ZERO.
           05  WS-CASH-SURCHG              PIC X      VALUE 'N'.
               88  WS-PASS-SURCHARGE                 VALUE 'Y'.
           05  WS-CASH-MIN-ADV             PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-VALID-CURR-CNT           PIC 9(2)   VALUE ZERO.
           05  WS-VALID-CURR OCCURS 12 TIMES
                                           PIC X(3).
           05  WS-PARM-READ-SW             PIC X      VALUE 'N'.
      *
       01  WS-WORK.
           05  WS-FEE-AMT                  PIC S9(9)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-EXCESS                   PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-ADVANCE                  PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-SURCHARGE                PIC S9(5)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-IDX                      PIC S9(4) COMP VALUE ZERO.
           05  WS-CURR-OK-SW               PIC X     VALUE 'N'.
               88  WS-CURR-OK                        VALUE 'Y'.
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
       01  WS-ISO-DATE.
           05  WS-ISO-YYYY                 PIC 9(4).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-MM                   PIC 9(2).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-DD                   PIC 9(2).
      *
      *    THE AUTHORIZATION IMAGE IS COPIED INTO A LOCAL RECORD SO
      *    THE REDEFINES IN CVAUTH01Y CAN BE USED ON IT.
           COPY CVAUTH01Y.
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
       01  DCL-POST.
           05  DCL-TXN-ID                  PIC X(16).
           05  DCL-ACCT-ID                 PIC S9(11) COMP-3.
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-AMOUNT                  PIC S9(11)V99 COMP-3.
           05  DCL-TXN-TYPE                PIC X(4).
           05  DCL-TXN-DESC                PIC X(40).
           05  DCL-CYCLE-ID                PIC X(8).
           05  DCL-FEE-CNT                 PIC S9(9) COMP-3.
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
           EVALUATE TRUE
               WHEN FW-FEE-OVERLIMIT
                   PERFORM 2000-OVER-LIMIT-FEE
               WHEN FW-FEE-CASH-ADV
                   PERFORM 3000-CASH-ADVANCE-FEE
               WHEN OTHER
                   MOVE 'Y'                TO WS-REJECT-SW
                   MOVE 'FEE TYPE NOT HANDLED BY CBFEE02'
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
           MOVE 'CBFEE02 '                 TO BR-RETURN-PGM
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
           MOVE ZERO                       TO WS-VALID-CURR-CNT
           OPEN INPUT FEEPARM-FILE
           IF NOT WS-FEEPARM-OK
               MOVE 'Y'                    TO WS-FATAL-SW
               MOVE 'FEEPARM OPEN FAILED'  TO BR-RETURN-MSG
               DISPLAY 'CBFEE02 FEEPARM OPEN STATUS '
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
           IF WS-VALID-CURR-CNT = ZERO
               MOVE 'Y'                    TO WS-FATAL-SW
               MOVE 'NO VALID-CURR CARD IN FEEPARM'
                                           TO BR-RETURN-MSG
           END-IF
           .
       1100-EXIT.
           EXIT
           .
      *
       1150-APPLY-CARD.
           EVALUATE FP-KEYWORD
               WHEN 'OVLM-TOL    '
                   COMPUTE WS-OVLM-TOL =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'OVLM-MAX-CYC'
                   MOVE FP-VALUE(1:1)      TO WS-OVLM-MAX-CYC
               WHEN 'CASH-SURCHG '
                   MOVE FP-VALUE(1:1)      TO WS-CASH-SURCHG
               WHEN 'CASH-MIN-ADV'
                   COMPUTE WS-CASH-MIN-ADV =
                           FUNCTION NUMVAL(FP-VALUE)
               WHEN 'VALID-CURR  '
                   IF WS-VALID-CURR-CNT < 12
                       ADD 1               TO WS-VALID-CURR-CNT
                       MOVE FP-VALUE(1:3)  TO
                            WS-VALID-CURR(WS-VALID-CURR-CNT)
                   END-IF
               WHEN '*           '
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'CBFEE02 UNKNOWN FEEPARM CARD - '
                           FP-KEYWORD
           END-EVALUATE
           .
      *
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
                   MOVE 'Y'                TO WS-REJECT-SW
                   MOVE 'NO FEE SCHEDULE ROW FOR PRODUCT AND TYPE'
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
                  AND FEE_TYPE   = :DCL-FEE-TYPE
                  AND DATE(:DCL-CYCLE-DATE)
                      BETWEEN EFF_DATE AND EXP_DATE
                FETCH FIRST 1 ROW ONLY
           END-EXEC
           .
      *
      ******************************************************************
      * 2000 - OVER LIMIT FEE                                          *
      ******************************************************************
       2000-OVER-LIMIT-FEE.
           IF NOT FW-OVLM-OPTED-IN
               MOVE 'ACCOUNT HAS NOT OPTED IN TO OVER LIMIT'
                                           TO BR-RETURN-MSG
               MOVE 'NOPT'                 TO BR-REASON-CD
               GO TO 2000-EXIT
           END-IF
      *
           COMPUTE WS-EXCESS = FW-CURR-BAL - FW-CREDIT-LIMIT
           IF WS-EXCESS NOT > WS-OVLM-TOL
               MOVE 'ACCOUNT IS WITHIN ITS CREDIT LIMIT'
                                           TO BR-RETURN-MSG
               GO TO 2000-EXIT
           END-IF
      *
           IF FW-PRIOR-OVLM-FEE-FLG = 'Y'
               MOVE 'OVER LIMIT FEE ALREADY CHARGED THIS CYCLE'
                                           TO BR-RETURN-MSG
               MOVE 'ONCE'                 TO BR-REASON-CD
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2100-COUNT-CYCLE-FEES
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           IF DCL-FEE-CNT NOT < WS-OVLM-MAX-CYC
               MOVE 'OVER LIMIT FEE LIMIT FOR THE CYCLE REACHED'
                                           TO BR-RETURN-MSG
               MOVE 'ONCE'                 TO BR-REASON-CD
               GO TO 2000-EXIT
           END-IF
      *
           MOVE DCL-FLAT-AMT               TO WS-FEE-AMT
      *
      *    THE FEE MAY NOT EXCEED THE AMOUNT THE ACCOUNT IS OVER BY
           IF WS-FEE-AMT > WS-EXCESS
               MOVE WS-EXCESS              TO WS-FEE-AMT
               MOVE 'EXCS'                 TO BR-REASON-CD
           END-IF
      *
           IF DCL-MAX-AMT > ZERO
           AND WS-FEE-AMT > DCL-MAX-AMT
               MOVE DCL-MAX-AMT            TO WS-FEE-AMT
           END-IF
      *
           IF WS-FEE-AMT > ZERO
               MOVE 'Y'                    TO WS-CHARGE-SW
               MOVE 'OVLM'                 TO WS-TB-TYPE
               MOVE 'OVER CREDIT LIMIT FEE'
                                           TO WS-DESC
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-COUNT-CYCLE-FEES.
           MOVE FW-ACCT-ID                 TO DCL-ACCT-ID
           MOVE FW-CYCLE-ID                TO DCL-CYCLE-ID
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-FEE-CNT
                 FROM CARDSVC.TRANSACTION
                WHERE ACCT_ID     = :DCL-ACCT-ID
                  AND TXN_TYPE_CD = 'OVLM'
                  AND CYCLE_ID    = :DCL-CYCLE-ID
                WITH UR
           END-EXEC
      *
           IF SQLCODE NOT = 0
               PERFORM 9400-SQL-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 3000 - CASH ADVANCE FEE                                        *
      ******************************************************************
       3000-CASH-ADVANCE-FEE.
           MOVE FW-AUTH-IMAGE              TO AUTH-DETAIL
           MOVE 'C'                        TO AUTH-TYPE
      *
           EVALUATE AUTH-TYPE
               WHEN 'C'
                   PERFORM 3100-EDIT-CASH-IMAGE
               WHEN OTHER
                   MOVE 'Y'                TO WS-REJECT-SW
                   MOVE 'AUTHORIZATION IS NOT A CASH ADVANCE'
                                           TO BR-RETURN-MSG
           END-EVALUATE
      *
           IF WS-REJECTED
               GO TO 3000-EXIT
           END-IF
      *
           IF WS-ADVANCE NOT > WS-CASH-MIN-ADV
               MOVE 'ADVANCE BELOW THE FEE THRESHOLD'
                                           TO BR-RETURN-MSG
               GO TO 3000-EXIT
           END-IF
      *
      *    PERCENTAGE OF THE ADVANCE, THEN THE SCHEDULE MINIMUM
           COMPUTE WS-FEE-AMT ROUNDED =
                   WS-ADVANCE * DCL-PCT-RATE / 100
      *
           IF WS-FEE-AMT < DCL-MIN-AMT
               MOVE DCL-MIN-AMT            TO WS-FEE-AMT
               MOVE 'MINF'                 TO BR-REASON-CD
           END-IF
      *
           IF DCL-MAX-AMT > ZERO
           AND WS-FEE-AMT > DCL-MAX-AMT
               MOVE DCL-MAX-AMT            TO WS-FEE-AMT
           END-IF
      *
      *    THE ACQUIRER SURCHARGE IS PASSED THROUGH ONLY WHEN THE
      *    CARD SAYS SO - SOME SCHEMES FORBID IT.
           IF WS-PASS-SURCHARGE
           AND WS-SURCHARGE > ZERO
               ADD WS-SURCHARGE            TO WS-FEE-AMT
               MOVE 'SURC'                 TO BR-REASON-CD
           END-IF
      *
           IF WS-FEE-AMT > ZERO
               MOVE 'Y'                    TO WS-CHARGE-SW
               MOVE 'CASH'                 TO WS-TB-TYPE
               MOVE 'CASH ADVANCE FEE'     TO WS-DESC
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3100 - THE CASH VARIANT OCCUPIES 40 OF THE 60 DETAIL BYTES.    *
      *        THE REST IS RESIDUE, SO ONLY THE FIELDS BELOW ARE       *
      *        TOUCHED AND THE CURRENCY IS VALIDATED AGAINST THE       *
      *        VALID-CURR CARDS BEFORE THE AMOUNT IS BELIEVED.         *
      ******************************************************************
       3100-EDIT-CASH-IMAGE.
           MOVE 'N'                        TO WS-CURR-OK-SW
           PERFORM VARYING WS-IDX FROM 1 BY 1
                     UNTIL WS-IDX > WS-VALID-CURR-CNT
                        OR WS-CURR-OK
               IF AC-CURRENCY = WS-VALID-CURR(WS-IDX)
                   MOVE 'Y'                TO WS-CURR-OK-SW
               END-IF
           END-PERFORM
      *
           IF NOT WS-CURR-OK
               MOVE 'Y'                    TO WS-REJECT-SW
               MOVE 'CASH ADVANCE IMAGE CURRENCY NOT RECOGNISED'
                                           TO BR-RETURN-MSG
               DISPLAY 'CBFEE02 BAD CASH IMAGE ACCT '
                       FW-ACCT-ID
                       ' ATM ' AC-ATM-ID
               GO TO 3100-EXIT
           END-IF
      *
           IF AC-AMOUNT NOT > ZERO
               MOVE 'Y'                    TO WS-REJECT-SW
               MOVE 'CASH ADVANCE AMOUNT IS NOT POSITIVE'
                                           TO BR-RETURN-MSG
               GO TO 3100-EXIT
           END-IF
      *
           MOVE AC-AMOUNT                  TO WS-ADVANCE
           MOVE AC-FEE                     TO WS-SURCHARGE
           IF WS-SURCHARGE < ZERO
               MOVE ZERO                   TO WS-SURCHARGE
           END-IF
           .
       3100-EXIT.
           EXIT
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
                    , AUTH_SEQ_NUM
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
                    , NULL
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
                    , 'CBFEE02 ')
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
                    , LAST_MAINT_PGM = 'CBFEE02 '
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE ACCT_ID        = :DCL-ACCT-ID
           END-EXEC
      *
           IF SQLCODE NOT = 0
               PERFORM 9400-SQL-ERROR
               GO TO 5000-EXIT
           END-IF
      *
      *    A CASH ADVANCE FEE ALSO INCREASES THE CASH BALANCE, WHICH
      *    IS PRICED AT THE CASH APR AND NOT THE PURCHASE APR.
           IF FW-FEE-CASH-ADV
               EXEC SQL
                   UPDATE CARDSVC.ACCOUNT
                      SET CASH_BAL      = CASH_BAL + :DCL-AMOUNT
                        , LAST_MAINT_TS = CURRENT TIMESTAMP
                    WHERE ACCT_ID       = :DCL-ACCT-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 9400-SQL-ERROR
                   GO TO 5000-EXIT
               END-IF
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
           DISPLAY 'CBFEE02 SQL ERROR SQLCODE=' SQLCODE
                   ' ACCT=' FW-ACCT-ID
                   ' TYPE=' FW-FEE-TYPE
           MOVE 'SQL FAILURE IN CBFEE02'   TO BR-RETURN-MSG
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
