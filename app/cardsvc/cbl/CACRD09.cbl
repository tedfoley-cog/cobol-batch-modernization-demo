      ******************************************************************
      * CACRD09 - AUTHORIZATION DECISION                               *
      *                                                                *
      * SIXTH PROGRAM OF THE AUTHORIZATION CHAIN, AND THE ONE THAT     *
      * OWNS THE ANSWER.  EVERY EARLIER STEP ONLY RECORDS A VERDICT    *
      * IN THE COMMAREA - NOTHING BEFORE THIS PROGRAM APPROVES OR      *
      * DECLINES ANYTHING.                                             *
      *                                                                *
      * PRECEDENCE, HIGHEST FIRST -                                    *
      *                                                                *
      *   1  SANCTIONS HIT                     DECLINE  59 / SANC      *
      *   2  RISK SERVICE BUSINESS DECLINE     DECLINE  05             *
      *   3  CARD OR ACCOUNT STATUS FAILURE    DECLINE  MAPPED         *
      *   4  FRAUD ACTION DECL                 DECLINE  59 / FRDD      *
      *   5  LIMIT EXCEEDED                    DECLINE  51 / OVLM      *
      *   6  VELOCITY EXCEEDED                 DECLINE  65 / VELO      *
      *   7  KYC NOT COMPLETE                  REFER    01 / KYCP      *
      *   8  FRAUD ACTION REFR                 REFER    01 / FRDR      *
      *   9  RISK ADVICE REFR OR REFERRAL SW   REFER    01 / RSKR      *
      *  10  LIMIT TOLERATED                   REFER    01 / TOLR      *
      *  11  EVERYTHING ELSE                   APPROVE  00 / APPR      *
      *                                                                *
      * A REFERRAL IS AN ANSWER, NOT AN ERROR - THE OPERATOR CALLS     *
      * THE AUTHORISATION DESK AND KEYS THE OUTCOME AS A NEW REQUEST.  *
      *                                                                *
      * CALLED BY   - CACRD05, CACRD06, CACRD07, CACRD08  XCTL         *
      * CALLS       - CACRD10  XCTL, AUTHORIZATION RECORD WRITE        *
      *             - CACRD91  ERROR HANDLER                           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD09.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD09 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'CACRD10 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-YYYYMMDD            PIC 9(8)  VALUE ZERO.
       01  WS-TIME-HHMMSS              PIC 9(6)  VALUE ZERO.
      *
       01  WS-DECIDED-SW               PIC X     VALUE 'N'.
           88  WS-DECIDED                        VALUE 'Y'.
      *
       01  WS-COMBINED-SCORE           PIC S9(5) COMP-3 VALUE ZERO.
      *
      *    ---------------------------------------------------------
      *    RESPONSE CODES AS SENT TO THE ACQUIRER
      *    ---------------------------------------------------------
       01  WS-RESP-CODES.
           05  WS-RC-APPROVED          PIC X(2)  VALUE '00'.
           05  WS-RC-REFER             PIC X(2)  VALUE '01'.
           05  WS-RC-RISK-DECLINE      PIC X(2)  VALUE '05'.
           05  WS-RC-EXPIRED-CARD      PIC X(2)  VALUE '54'.
           05  WS-RC-INSUFFICIENT      PIC X(2)  VALUE '51'.
           05  WS-RC-SUSPECTED-FRAUD   PIC X(2)  VALUE '59'.
           05  WS-RC-PICK-UP           PIC X(2)  VALUE '04'.
           05  WS-RC-RESTRICTED        PIC X(2)  VALUE '62'.
           05  WS-RC-VELOCITY          PIC X(2)  VALUE '65'.
           05  WS-RC-INVALID-CARD      PIC X(2)  VALUE '14'.
           05  WS-RC-PIN-EXCEEDED      PIC X(2)  VALUE '75'.
      *
           COPY CVAUTHW1Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
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
           PERFORM 1000-COMBINE-SCORES
           PERFORM 2000-DECIDE
           PERFORM 3000-POST-DECISION-RULES
           PERFORM 4000-BUILD-MESSAGE
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
           MOVE 'N'                    TO WS-DECIDED-SW
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-YYYYMMDD)
                     TIME(WS-TIME-HHMMSS)
                     RESP(WS-RESP)
           END-EXEC
      *
           IF CAW-TRAIL-CNT < 8
               ADD 1                   TO CAW-TRAIL-CNT
               MOVE WS-PGM-ID          TO CAW-TRAIL-PGM(CAW-TRAIL-CNT)
               MOVE ZERO               TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
      *
      *    A STEP THAT WAS SKIPPED LEAVES ITS VERDICT BLANK.  BLANK IS
      *    READ AS PASSED SO A SHORT CUT INTO THIS PROGRAM DOES NOT
      *    DECLINE ON A CHECK THAT NEVER RAN.
           IF CAW-STAT-RESULT = SPACE OR LOW-VALUE
               MOVE 'P'                TO CAW-STAT-RESULT
           END-IF
           IF CAW-LIM-RESULT = SPACE OR LOW-VALUE
               MOVE 'P'                TO CAW-LIM-RESULT
           END-IF
           IF CAW-FRAUD-ACTION = SPACES OR LOW-VALUES
               MOVE 'SCOR'             TO CAW-FRAUD-ACTION
           END-IF
           IF CAW-RSK-ADVICE = SPACES OR LOW-VALUES
               MOVE 'REFR'             TO CAW-RSK-ADVICE
               MOVE 'RSKM'             TO CAW-RSK-REASON
           END-IF
           .
      *
      ******************************************************************
      * 1000 - ONE NUMBER OUT OF TWO                                   *
      *                                                                *
      * THE RISK SCORE AND THE FRAUD SCORE ARE DIFFERENT SCALES.  THE  *
      * RISK MODEL RUNS 0 TO 999 AND THE FRAUD RULES 0 TO 999, BUT     *
      * THE FRAUD SIDE IS WEIGHTED HIGHER BECAUSE IT IS LOOKING AT     *
      * THIS TRANSACTION RATHER THAN THE PARTY.                        *
      ******************************************************************
       1000-COMBINE-SCORES.
           COMPUTE WS-COMBINED-SCORE ROUNDED =
                   (CAW-RSK-SCORE * 4 + CAW-FRAUD-SCORE * 6) / 10
      *
      *    A REFERRAL WARNING FROM THE RISK STEP ADDS A FLAT PENALTY
           IF CAW-RSK-REFERRED
               ADD 25                  TO WS-COMBINED-SCORE
           END-IF
      *
           IF WS-COMBINED-SCORE > 999
               MOVE 999                TO WS-COMBINED-SCORE
           END-IF
           .
      *
      ******************************************************************
      * 2000 - THE PRECEDENCE LADDER                                   *
      ******************************************************************
       2000-DECIDE.
           EVALUATE TRUE
      *
      *        1 - SANCTIONS.  NOTHING OVERTURNS THIS.
               WHEN CAW-RSK-SANCTIONED
                   PERFORM 2900-DECLINE
                   MOVE WS-RC-SUSPECTED-FRAUD
                                       TO CAW-DEC-RESP
                   MOVE 'SANC'         TO CAW-DEC-REASON
      *
      *        2 - THE RISK SERVICE SAID NO
               WHEN CAW-RSK-ADVICE = 'DECL'
                   PERFORM 2900-DECLINE
                   MOVE WS-RC-RISK-DECLINE
                                       TO CAW-DEC-RESP
                   IF CAW-RSK-REASON NOT = SPACES
                       MOVE CAW-RSK-REASON
                                       TO CAW-DEC-REASON
                   ELSE
                       MOVE 'RISK'     TO CAW-DEC-REASON
                   END-IF
      *
      *        3 - CARD OR ACCOUNT STATUS
               WHEN CAW-STAT-FAILED
                   PERFORM 2100-STATUS-DECLINE
      *
      *        4 - THE FRAUD RULES SAID NO
               WHEN CAW-FRD-DECLINE
                   PERFORM 2900-DECLINE
                   MOVE WS-RC-SUSPECTED-FRAUD
                                       TO CAW-DEC-RESP
                   MOVE 'FRDD'         TO CAW-DEC-REASON
      *
      *        5 - NO ROOM ON THE LINE
               WHEN CAW-LIM-EXCEEDED
                   PERFORM 2900-DECLINE
                   MOVE WS-RC-INSUFFICIENT
                                       TO CAW-DEC-RESP
                   MOVE 'OVLM'         TO CAW-DEC-REASON
      *
      *        6 - TOO MANY IN THE WINDOW
               WHEN CAW-LIM-VELOCITY
                   PERFORM 2900-DECLINE
                   MOVE WS-RC-VELOCITY TO CAW-DEC-RESP
                   MOVE 'VELO'         TO CAW-DEC-REASON
      *
      *        7 - KYC NOT COMPLETE.  A REFERRAL, NOT A DECLINE -
      *            THE DESK CAN CLEAR IT WHILE THE CUSTOMER WAITS.
               WHEN CAW-RSK-KYC = 'PN' OR 'EX'
                   PERFORM 2800-REFER
                   MOVE 'KYCP'         TO CAW-DEC-REASON
      *
      *        8 - THE FRAUD RULES WANT A LOOK
               WHEN CAW-FRD-REFER
                   PERFORM 2800-REFER
                   MOVE 'FRDR'         TO CAW-DEC-REASON
      *
      *        9 - THE RISK SERVICE WANTS A LOOK
               WHEN CAW-RSK-ADVICE = 'REFR'
                   PERFORM 2800-REFER
                   MOVE 'RSKR'         TO CAW-DEC-REASON
               WHEN CAW-RSK-REFERRED
                   PERFORM 2800-REFER
                   MOVE 'RSKR'         TO CAW-DEC-REASON
      *
      *       10 - INSIDE THE OVER LIMIT TOLERANCE
               WHEN CAW-LIM-TOLERATED
                   PERFORM 2800-REFER
                   MOVE 'TOLR'         TO CAW-DEC-REASON
      *
      *       11 - NOTHING TO SAY NO ABOUT
               WHEN OTHER
                   PERFORM 2700-APPROVE
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2100 - MAP THE STATUS REASON ONTO A RESPONSE CODE              *
      ******************************************************************
       2100-STATUS-DECLINE.
           PERFORM 2900-DECLINE
           MOVE CAW-STAT-REASON        TO CAW-DEC-REASON
      *
           EVALUATE CAW-STAT-REASON
               WHEN 'CB01'
                   MOVE WS-RC-RESTRICTED
                                       TO CAW-DEC-RESP
               WHEN 'CB12'
      *            BLOCKED BY THE FRAUD TEAM - THE TERMINAL IS TOLD
      *            TO RETAIN THE CARD
                   MOVE WS-RC-PICK-UP  TO CAW-DEC-RESP
               WHEN 'CC02'
                   MOVE WS-RC-INVALID-CARD
                                       TO CAW-DEC-RESP
               WHEN 'CL03'
                   MOVE WS-RC-PICK-UP  TO CAW-DEC-RESP
               WHEN 'CE04'
                   MOVE WS-RC-EXPIRED-CARD
                                       TO CAW-DEC-RESP
               WHEN 'CE13'
                   MOVE WS-RC-EXPIRED-CARD
                                       TO CAW-DEC-RESP
               WHEN 'CN05'
                   MOVE WS-RC-RESTRICTED
                                       TO CAW-DEC-RESP
               WHEN 'CP06'
                   MOVE WS-RC-PIN-EXCEEDED
                                       TO CAW-DEC-RESP
               WHEN 'AC07'
                   MOVE WS-RC-INVALID-CARD
                                       TO CAW-DEC-RESP
               WHEN 'AS08'
                   MOVE WS-RC-RESTRICTED
                                       TO CAW-DEC-RESP
               WHEN 'AW09'
                   MOVE WS-RC-RESTRICTED
                                       TO CAW-DEC-RESP
               WHEN 'AD10'
                   MOVE WS-RC-RESTRICTED
                                       TO CAW-DEC-RESP
               WHEN 'AX12'
                   MOVE WS-RC-INVALID-CARD
                                       TO CAW-DEC-RESP
               WHEN 'CX00'
                   MOVE WS-RC-INVALID-CARD
                                       TO CAW-DEC-RESP
               WHEN OTHER
                   MOVE WS-RC-RISK-DECLINE
                                       TO CAW-DEC-RESP
           END-EVALUATE
           .
      *
       2700-APPROVE.
           MOVE 'A'                    TO CAW-DEC-STATUS
           MOVE WS-RC-APPROVED         TO CAW-DEC-RESP
           MOVE 'APPR'                 TO CAW-DEC-REASON
           MOVE 'Y'                    TO WS-DECIDED-SW
           IF CAW-TRAIL-CNT > ZERO
               MOVE WS-RC-OK           TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
       2800-REFER.
           MOVE 'F'                    TO CAW-DEC-STATUS
           MOVE WS-RC-REFER            TO CAW-DEC-RESP
           MOVE 'Y'                    TO WS-DECIDED-SW
           IF CAW-TRAIL-CNT > ZERO
               MOVE WS-RC-WARNING      TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
       2900-DECLINE.
           MOVE 'D'                    TO CAW-DEC-STATUS
           MOVE 'Y'                    TO WS-DECIDED-SW
           IF CAW-TRAIL-CNT > ZERO
               MOVE WS-RC-ERROR        TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 3000 - THE RULES THAT SIT ON TOP OF THE LADDER                 *
      *                                                                *
      * THESE ARE THE LATER ADDITIONS.  THEY CAN ONLY MAKE AN ANSWER   *
      * MORE CONSERVATIVE, NEVER LESS - A DECLINE IS NEVER TURNED      *
      * BACK INTO AN APPROVAL HERE.                                    *
      ******************************************************************
       3000-POST-DECISION-RULES.
           IF CAW-DEC-DECLINED
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 3100-COMBINED-SCORE-RULE
           PERFORM 3200-HIGH-VALUE-RULE
           PERFORM 3300-ENTRY-MODE-RULE
           PERFORM 3400-REFUND-RULE
           .
       3000-EXIT.
           EXIT
           .
      *
      *    A COMBINED SCORE ABOVE THE CUT OFF REFERS AN APPROVAL AND
      *    DECLINES A REFERRAL THAT WAS ALREADY MARGINAL.
       3100-COMBINED-SCORE-RULE.
           EVALUATE TRUE
               WHEN WS-COMBINED-SCORE > 250
                   PERFORM 2900-DECLINE
                   MOVE WS-RC-SUSPECTED-FRAUD
                                       TO CAW-DEC-RESP
                   MOVE 'SCRH'         TO CAW-DEC-REASON
               WHEN WS-COMBINED-SCORE > 150
                   IF CAW-DEC-APPROVED
                       PERFORM 2800-REFER
                       MOVE 'SCRM'     TO CAW-DEC-REASON
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
      *    ANYTHING AT OR OVER FIVE THOUSAND ON A CARD WHOSE BAND IS
      *    NOT A GOES TO THE DESK.  THE FIGURE HAS NOT BEEN INDEXED
      *    SINCE IT WAS SET.
       3200-HIGH-VALUE-RULE.
           IF CAW-RQ-AUTH-TYPE = 'R'
               GO TO 3200-EXIT
           END-IF
      *
           IF CAW-RQ-AMT >= 5000.00
               IF CAW-RSK-BAND NOT = 'A'
                   IF CAW-DEC-APPROVED
                       PERFORM 2800-REFER
                       MOVE 'HIVL'     TO CAW-DEC-REASON
                   END-IF
               END-IF
           END-IF
      *
      *    A CASH ADVANCE OVER A THOUSAND ALWAYS REFERS
           IF CAW-RQ-AUTH-TYPE = 'C' AND CAW-RQ-AMT > 1000.00
               IF CAW-DEC-APPROVED
                   PERFORM 2800-REFER
                   MOVE 'CASH'         TO CAW-DEC-REASON
               END-IF
           END-IF
           .
       3200-EXIT.
           EXIT
           .
      *
      *    A KEYED OR ELECTRONIC COMMERCE ENTRY ON A CARD THE RISK
      *    SERVICE COULD NOT SCORE IS NOT APPROVED WITHOUT A LOOK.
       3300-ENTRY-MODE-RULE.
           IF CAW-RSK-BAND NOT = 'X'
               GO TO 3300-EXIT
           END-IF
      *
           IF CAW-RQ-KEYED OR CAW-RQ-ECOM
               IF CAW-DEC-APPROVED
                   PERFORM 2800-REFER
                   MOVE 'ENTM'         TO CAW-DEC-REASON
               END-IF
           END-IF
           .
       3300-EXIT.
           EXIT
           .
      *
      *    A REFUND LARGER THAN THE ORIGINAL CANNOT BE PROVED HERE -
      *    THE ORIGINAL AUTHORIZATION IS ONLY CHECKED AT SETTLEMENT -
      *    SO A LARGE ONE IS REFERRED.
       3400-REFUND-RULE.
           IF CAW-RQ-AUTH-TYPE NOT = 'R'
               GO TO 3400-EXIT
           END-IF
      *
           IF CAW-RQ-ORIG-AUTH = SPACES
               PERFORM 2800-REFER
               MOVE 'RFND'             TO CAW-DEC-REASON
               GO TO 3400-EXIT
           END-IF
      *
           IF CAW-RQ-AMT > 2500.00
               IF CAW-DEC-APPROVED
                   PERFORM 2800-REFER
                   MOVE 'RFHI'         TO CAW-DEC-REASON
               END-IF
           END-IF
           .
       3400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - THE OPERATOR MESSAGE                                    *
      ******************************************************************
       4000-BUILD-MESSAGE.
           MOVE WS-COMBINED-SCORE      TO CAW-RSK-SCORE
      *
           EVALUATE TRUE
               WHEN CAW-DEC-APPROVED
                   MOVE 'APPROVED'     TO CAW-MSG
               WHEN CAW-DEC-REFERRED
                   MOVE 'REFER TO THE AUTHORIZATION DESK'
                                       TO CAW-MSG
               WHEN CAW-DEC-DECLINED
                   MOVE 'DECLINED'     TO CAW-MSG
               WHEN OTHER
      *            THE LADDER ALWAYS SETS A STATUS - THIS CANNOT
      *            HAPPEN, WHICH IS WHY IT IS WORTH TRAPPING
                   MOVE 'D'            TO CAW-DEC-STATUS
                   MOVE WS-RC-RISK-DECLINE
                                       TO CAW-DEC-RESP
                   MOVE 'UNKN'         TO CAW-DEC-REASON
                   MOVE 'DECLINED - DECISION NOT REACHED'
                                       TO CAW-MSG
           END-EVALUATE
      *
           MOVE WS-DATE-YYYYMMDD       TO CAW-AUTH-DATE
           MOVE WS-TIME-HHMMSS         TO CAW-AUTH-TIME
           .
      *
      ******************************************************************
      * 6000 - PASS CONTROL ON                                         *
      ******************************************************************
       6000-CONTINUE-CHAIN.
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
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
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
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
                     TEXT('CACRD09 IS A CHAIN STEP - START WITH CA00')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           .
