      ******************************************************************
      * CACRD08 - LIMIT AND VELOCITY CHECK                             *
      *                                                                *
      * FIFTH PROGRAM OF THE AUTHORIZATION CHAIN.                      *
      *                                                                *
      * THE LIMIT TYPE IS CHOSEN FROM THE REQUEST -                    *
      *   CRED  PURCHASES AND REFUNDS IN THE ACCOUNT CURRENCY          *
      *   CASH  CASH ADVANCES                                          *
      *   FRGN  ANYTHING IN A CURRENCY OTHER THAN THE ACCOUNT ONE      *
      *                                                                *
      * THE ROW USED IS THE ONE EFFECTIVE TODAY.  AVAILABLE IS THE     *
      * ROW AVAILABLE AMOUNT LESS THE AUTHORISATIONS ALREADY PENDING   *
      * ON THE ACCOUNT, BECAUSE THE PENDING FIGURE IS HELD AT ACCOUNT  *
      * LEVEL AND THE LIMIT ROW IS ONLY UPDATED AT SETTLEMENT.  THAT   *
      * DOUBLE COUNTS A LITTLE ON MULTI CARD ACCOUNTS.  IT HAS BEEN    *
      * THAT WAY SINCE THE 1998 CONVERSION AND THE TOLERANCE BAND      *
      * BELOW WAS WIDENED IN 2004 TO COMPENSATE.                       *
      *                                                                *
      * OVER LIMIT TOLERANCE - AN AUTHORIZATION THAT GOES OVER THE     *
      * LIMIT IS STILL ALLOWED WHEN ALL OF THESE HOLD -                *
      *   THE OVERSHOOT IS INSIDE THE TOLERANCE PERCENTAGE FOR THE     *
      *   PRODUCT, THE ACCOUNT IS NOT DELINQUENT, THE RISK BAND IS A   *
      *   OR B, AND THE ACCOUNT HAS NOT ALREADY USED ITS TOLERANCE     *
      *   THIS CYCLE.                                                  *
      *                                                                *
      * VELOCITY - THE COUNT OF AUTHORIZATIONS ON THE CARD IN THE      *
      * LAST VELOCITY_WINDOW_MIN MINUTES.  DECLINED ONES COUNT, SO A   *
      * CARD BEING TESTED BY A FRAUDSTER TRIPS THE COUNTER.            *
      *                                                                *
      * CALLED BY   - CACRD07  XCTL                                    *
      * CALLS       - CACRD09  XCTL, AUTHORIZATION DECISION            *
      *             - CACRD91  ERROR HANDLER                           *
      * TABLES      - CARDSVC.CARD_LIMIT, CARDSVC.ACCOUNT,             *
      *               CARDSVC.AUTHORIZATION  (SELECT)                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD08.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD08 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'CACRD09 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-LIMIT-FOUND-SW           PIC X     VALUE 'N'.
           88  WS-LIMIT-FOUND                    VALUE 'Y'.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-YYYYMMDD            PIC 9(8)  VALUE ZERO.
      *
      *    AMOUNTS ARE WORKED OUT IN A SINGLE SCRATCH AREA THAT IS
      *    REUSED BY EVERY PARAGRAPH IN THE 3000 RANGE
       01  WS-CALC-AREA.
           05  WS-AVAIL-AMT            PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-EFFECTIVE-AVAIL      PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-OVER-AMT             PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-TOLERANCE-AMT        PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-REQ-AMT              PIC S9(11)V99 COMP-3 VALUE 0.
      *
       01  WS-TOLERANCE-PCT            PIC S9(3)V99 COMP-3 VALUE 0.
       01  WS-VELOCITY-WINDOW          PIC S9(4) COMP VALUE 60.
       01  WS-VELOCITY-MAX             PIC S9(4) COMP VALUE 5.
      *
           COPY CVAUTHW1Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-CARD-NUM                PIC X(16).
       01  DCL-ACCT-ID                 PIC S9(11) COMP-3.
       01  DCL-LIMIT-TYPE              PIC X(4).
       01  DCL-LIMIT-AMT               PIC S9(11)V99 COMP-3.
       01  DCL-USED-AMT                PIC S9(11)V99 COMP-3.
       01  DCL-AVAIL-AMT               PIC S9(11)V99 COMP-3.
       01  DCL-DAILY-CNT-LIMIT         PIC S9(4) COMP.
       01  DCL-DAILY-CNT-USED          PIC S9(4) COMP.
       01  DCL-VELOCITY-WINDOW         PIC S9(4) COMP.
       01  DCL-VELOCITY-MAX            PIC S9(4) COMP.
       01  DCL-RISK-BAND               PIC X(1).
      *
       01  DCL-ACCT-CURR               PIC X(3).
       01  DCL-PENDING-AMT             PIC S9(11)V99 COMP-3.
       01  DCL-CURR-BAL                PIC S9(11)V99 COMP-3.
       01  DCL-DELQ-BUCKET             PIC S9(4) COMP.
       01  DCL-PRODUCT-CD              PIC X(4).
      *
       01  DCL-AUTH-CNT                PIC S9(9) COMP.
       01  DCL-WINDOW-MIN              PIC S9(4) COMP.
      *
       01  IND-RISK-BAND               PIC S9(4) COMP.
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
           PERFORM 1000-READ-ACCOUNT
           PERFORM 1500-CHOOSE-LIMIT-TYPE
           PERFORM 2000-READ-LIMIT
      *
           IF WS-LIMIT-FOUND
               PERFORM 3000-CHECK-AVAILABLE
           ELSE
               PERFORM 2900-NO-LIMIT-ROW
           END-IF
      *
           PERFORM 4000-CHECK-VELOCITY
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
           MOVE 'N'                    TO WS-LIMIT-FOUND-SW
      *
           MOVE 'P'                    TO CAW-LIM-RESULT
           MOVE SPACES                 TO CAW-LIM-TYPE
           MOVE ZERO                   TO CAW-LIM-AVAIL
                                          CAW-LIM-OVER-AMT
                                          CAW-VELOCITY-CNT
      *
           MOVE ZERO                   TO WS-AVAIL-AMT
                                          WS-EFFECTIVE-AVAIL
                                          WS-OVER-AMT
                                          WS-TOLERANCE-AMT
           MOVE CAW-RQ-AMT             TO WS-REQ-AMT
      *
           MOVE WS-VELOCITY-WINDOW-MINS
                                       TO WS-VELOCITY-WINDOW
           MOVE WS-VELOCITY-MAX-CNT    TO WS-VELOCITY-MAX
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
      * 1000 - THE ACCOUNT SIDE OF THE SUM                             *
      ******************************************************************
       1000-READ-ACCOUNT.
           MOVE CAW-ACCT-ID            TO DCL-ACCT-ID
           MOVE ZERO                   TO DCL-PENDING-AMT
                                          DCL-CURR-BAL
                                          DCL-DELQ-BUCKET
           MOVE WS-CURRENCY-USD        TO DCL-ACCT-CURR
      *
           EXEC SQL
               SELECT CURRENCY_CD
                    , PENDING_AUTH_AMT
                    , CURR_BAL
                    , DELQ_BUCKET
                    , PRODUCT_CD
                 INTO :DCL-ACCT-CURR
                    , :DCL-PENDING-AMT
                    , :DCL-CURR-BAL
                    , :DCL-DELQ-BUCKET
                    , :DCL-PRODUCT-CD
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
      *            THE STATUS STEP HAS ALREADY FAILED THIS REQUEST -
      *            CARRY ON WITH ZERO AVAILABLE SO THE DECISION STEP
      *            SEES A CONSISTENT PICTURE
                   MOVE 'X'            TO CAW-LIM-RESULT
                   MOVE ZERO           TO CAW-LIM-AVAIL
               WHEN OTHER
                   MOVE '1000-READ-ACCOUNT'
                                       TO ER-PARAGRAPH
                   MOVE 'ACCOUNT          '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 1500 - WHICH LIMIT APPLIES                                     *
      ******************************************************************
       1500-CHOOSE-LIMIT-TYPE.
           EVALUATE TRUE
               WHEN CAW-RQ-CURR NOT = DCL-ACCT-CURR
                   MOVE 'FRGN'         TO DCL-LIMIT-TYPE
               WHEN CAW-RQ-AUTH-TYPE = 'C'
                   MOVE 'CASH'         TO DCL-LIMIT-TYPE
               WHEN OTHER
                   MOVE 'CRED'         TO DCL-LIMIT-TYPE
           END-EVALUATE
      *
           MOVE DCL-LIMIT-TYPE         TO CAW-LIM-TYPE
           .
      *
      ******************************************************************
      * 2000 - THE EFFECTIVE LIMIT ROW                                 *
      ******************************************************************
       2000-READ-LIMIT.
           MOVE CAW-RQ-CARD-NUM        TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT LIMIT_AMT
                    , USED_AMT
                    , AVAIL_AMT
                    , DAILY_CNT_LIMIT
                    , DAILY_CNT_USED
                    , VELOCITY_WINDOW_MIN
                    , VELOCITY_MAX_CNT
                    , RISK_BAND
                 INTO :DCL-LIMIT-AMT
                    , :DCL-USED-AMT
                    , :DCL-AVAIL-AMT
                    , :DCL-DAILY-CNT-LIMIT
                    , :DCL-DAILY-CNT-USED
                    , :DCL-VELOCITY-WINDOW
                    , :DCL-VELOCITY-MAX
                    , :DCL-RISK-BAND :IND-RISK-BAND
                 FROM CARDSVC.CARD_LIMIT
                WHERE CARD_NUM   = :DCL-CARD-NUM
                  AND LIMIT_TYPE = :DCL-LIMIT-TYPE
                  AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-LIMIT-FOUND-SW
                   MOVE DCL-VELOCITY-WINDOW
                                       TO WS-VELOCITY-WINDOW
                   MOVE DCL-VELOCITY-MAX
                                       TO WS-VELOCITY-MAX
               WHEN +100
                   MOVE 'N'            TO WS-LIMIT-FOUND-SW
               WHEN -811
      *            MORE THAN ONE EFFECTIVE ROW - A MAINTENANCE FAULT
      *            THAT SHOWS UP WHEN A LIMIT CHANGE IS BACKDATED.
      *            THE FIRST ROW IS TAKEN AND OPERATIONS ARE TOLD.
                   MOVE 'Y'            TO WS-LIMIT-FOUND-SW
                   MOVE '2000-READ-LIMIT'
                                       TO ER-PARAGRAPH
                   MOVE 'CARD_LIMIT       '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'OVERLAPPING EFFECTIVE LIMIT ROWS'
                                       TO ER-MESSAGE
                   PERFORM 8000-SQL-ERROR
               WHEN OTHER
                   MOVE 'N'            TO WS-LIMIT-FOUND-SW
                   MOVE '2000-READ-LIMIT'
                                       TO ER-PARAGRAPH
                   MOVE 'CARD_LIMIT       '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      *    NO EFFECTIVE ROW MEANS THE CARD HAS NO LINE OF THAT KIND.
      *    A FOREIGN REQUEST FALLS BACK TO THE CREDIT LINE - THAT IS
      *    HOW THE PRODUCT WORKED BEFORE FRGN LIMITS EXISTED.  ANY
      *    OTHER MISSING ROW IS AN OUTRIGHT FAILURE.
       2900-NO-LIMIT-ROW.
           IF DCL-LIMIT-TYPE = 'FRGN'
               MOVE 'CRED'             TO DCL-LIMIT-TYPE
                                          CAW-LIM-TYPE
               PERFORM 2000-READ-LIMIT
               IF WS-LIMIT-FOUND
                   PERFORM 3000-CHECK-AVAILABLE
                   GO TO 2900-EXIT
               END-IF
           END-IF
      *
           MOVE 'X'                    TO CAW-LIM-RESULT
           MOVE ZERO                   TO CAW-LIM-AVAIL
           MOVE CAW-RQ-AMT             TO CAW-LIM-OVER-AMT
           .
       2900-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - IS THERE ROOM                                           *
      ******************************************************************
       3000-CHECK-AVAILABLE.
           MOVE DCL-AVAIL-AMT          TO WS-AVAIL-AMT
      *
      *    THE PENDING FIGURE IS ACCOUNT WIDE.  IT IS ONLY TAKEN OFF
      *    THE CREDIT AND FOREIGN LINES - THE CASH LINE IS TRACKED
      *    SEPARATELY BY THE ATM SETTLEMENT FEED.
           IF DCL-LIMIT-TYPE NOT = 'CASH'
               COMPUTE WS-EFFECTIVE-AVAIL =
                       WS-AVAIL-AMT - DCL-PENDING-AMT
           ELSE
               MOVE WS-AVAIL-AMT       TO WS-EFFECTIVE-AVAIL
           END-IF
      *
           IF WS-EFFECTIVE-AVAIL < ZERO
               MOVE ZERO               TO WS-EFFECTIVE-AVAIL
           END-IF
      *
           MOVE WS-EFFECTIVE-AVAIL     TO CAW-LIM-AVAIL
      *
      *    A REFUND PUTS MONEY BACK AND NEVER NEEDS HEADROOM
           IF CAW-RQ-AUTH-TYPE = 'R'
               MOVE 'P'                TO CAW-LIM-RESULT
               MOVE ZERO               TO CAW-LIM-OVER-AMT
               GO TO 3000-EXIT
           END-IF
      *
           IF WS-REQ-AMT NOT > WS-EFFECTIVE-AVAIL
               MOVE 'P'                TO CAW-LIM-RESULT
               MOVE ZERO               TO CAW-LIM-OVER-AMT
               GO TO 3000-EXIT
           END-IF
      *
           COMPUTE WS-OVER-AMT = WS-REQ-AMT - WS-EFFECTIVE-AVAIL
           MOVE WS-OVER-AMT            TO CAW-LIM-OVER-AMT
      *
           PERFORM 3500-TOLERANCE
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3500 - OVER LIMIT TOLERANCE                                    *
      *                                                                *
      * THE PERCENTAGES ARE HARD CODED.  THEY WERE MEANT TO MOVE ONTO  *
      * THE FEE SCHEDULE TABLE IN THE 2007 RELEASE AND NEVER DID, SO   *
      * A PRODUCT ADDED SINCE THEN GETS THE CLASSIC BAND.              *
      ******************************************************************
       3500-TOLERANCE.
           EVALUATE DCL-PRODUCT-CD
               WHEN 'PLAT'
                   MOVE 10.00          TO WS-TOLERANCE-PCT
               WHEN 'GOLD'
                   MOVE 07.50          TO WS-TOLERANCE-PCT
               WHEN 'BUSN'
                   MOVE 15.00          TO WS-TOLERANCE-PCT
               WHEN OTHER
                   MOVE 05.00          TO WS-TOLERANCE-PCT
           END-EVALUATE
      *
      *    THE CASH LINE HAS NEVER BEEN TOLERATED
           IF DCL-LIMIT-TYPE = 'CASH'
               MOVE ZERO               TO WS-TOLERANCE-PCT
           END-IF
      *
           COMPUTE WS-TOLERANCE-AMT ROUNDED =
                   DCL-LIMIT-AMT * WS-TOLERANCE-PCT / 100
      *
      *    A HUNDRED DOLLARS EITHER WAY IS NOT WORTH A DECLINE ON A
      *    LARGE LINE, BUT THE ABSOLUTE CAP STOPS A PLATINUM CARD
      *    RUNNING THOUSANDS OVER
           IF WS-TOLERANCE-AMT > 1000.00
               MOVE 1000.00            TO WS-TOLERANCE-AMT
           END-IF
           IF WS-TOLERANCE-AMT < 50.00 AND WS-TOLERANCE-PCT > ZERO
               MOVE 50.00              TO WS-TOLERANCE-AMT
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-TOLERANCE-AMT = ZERO
                   MOVE 'X'            TO CAW-LIM-RESULT
               WHEN WS-OVER-AMT > WS-TOLERANCE-AMT
                   MOVE 'X'            TO CAW-LIM-RESULT
               WHEN DCL-DELQ-BUCKET > ZERO
      *            A DELINQUENT ACCOUNT GETS NO INDULGENCE
                   MOVE 'X'            TO CAW-LIM-RESULT
               WHEN CAW-RSK-BAND = 'C' OR 'X'
                   MOVE 'X'            TO CAW-LIM-RESULT
               WHEN DCL-DAILY-CNT-USED > DCL-DAILY-CNT-LIMIT
      *            THE DAILY COUNT IS THE PROXY FOR TOLERANCE ALREADY
      *            USED THIS CYCLE - THE PROPER FLAG WAS NEVER ADDED
                   MOVE 'X'            TO CAW-LIM-RESULT
               WHEN OTHER
                   MOVE 'T'            TO CAW-LIM-RESULT
           END-EVALUATE
      *
           IF CAW-LIM-EXCEEDED
               IF CAW-TRAIL-CNT > ZERO
                   MOVE WS-RC-ERROR    TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 4000 - VELOCITY                                                *
      *                                                                *
      * THE WINDOW IS IN MINUTES AND THE TABLE IS PARTITIONED ON       *
      * AUTH_DATE, SO THE PREDICATE CARRIES THE DATE AS WELL AS THE    *
      * TIMESTAMP EXPRESSION.  A WINDOW THAT SPANS MIDNIGHT LOSES THE  *
      * EARLIER PART - KNOWN, ACCEPTED, RAISED AS DEFECT 4471.         *
      ******************************************************************
       4000-CHECK-VELOCITY.
           MOVE CAW-RQ-CARD-NUM        TO DCL-CARD-NUM
           MOVE WS-VELOCITY-WINDOW     TO DCL-WINDOW-MIN
           MOVE ZERO                   TO DCL-AUTH-CNT
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-AUTH-CNT
                 FROM CARDSVC.AUTHORIZATION
                WHERE CARD_NUM  = :DCL-CARD-NUM
                  AND AUTH_DATE = CURRENT DATE
                  AND AUTH_TIME >= CURRENT TIME
                      - :DCL-WINDOW-MIN MINUTES
                  AND AUTH_TYPE <> 'R'
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE ZERO           TO DCL-AUTH-CNT
               WHEN OTHER
                   MOVE '4000-CHECK-VELOCITY'
                                       TO ER-PARAGRAPH
                   MOVE 'AUTHORIZATION    '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
                   MOVE ZERO           TO DCL-AUTH-CNT
           END-EVALUATE
      *
           IF DCL-AUTH-CNT > 999
               MOVE 999                TO CAW-VELOCITY-CNT
           ELSE
               MOVE DCL-AUTH-CNT       TO CAW-VELOCITY-CNT
           END-IF
      *
      *    THE VELOCITY VERDICT DOES NOT OVERWRITE A LIMIT FAILURE -
      *    THE DECISION STEP WANTS THE MORE SPECIFIC REASON.
           IF CAW-VELOCITY-CNT > WS-VELOCITY-MAX
               IF CAW-LIM-OK OR CAW-LIM-TOLERATED
                   MOVE 'V'            TO CAW-LIM-RESULT
                   IF CAW-TRAIL-CNT > ZERO
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
           IF ER-MESSAGE = SPACES
               MOVE 'LIMIT OR VELOCITY READ FAILED'
                                       TO ER-MESSAGE
           END-IF
           PERFORM 8900-LINK-ERROR-PGM
           .
      *
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
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
                     TEXT('CACRD08 IS A CHAIN STEP - START WITH CA00')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           .
