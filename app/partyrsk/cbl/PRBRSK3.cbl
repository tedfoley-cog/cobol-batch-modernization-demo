      ******************************************************************
      * PRBRSK3 - BATCH EXPOSURE RECALCULATION                         *
      *                                                                *
      * THIRD PROGRAM OF THE BATCH RECALCULATION CHAIN AND THE HEAVY   *
      * ONE.  REBUILDS THE PARTY EXPOSURE POSITION ACROSS EVERY        *
      * PRODUCT SYSTEM THAT REPORTS INTO PARTYDB.                      *
      *                                                                *
      * SOURCES                                                        *
      *   PARTYRSK.PARTY_EXPOSURE   PRIOR POSITION, LATEST AS OF DATE  *
      *   CARDSVC.ACCOUNT           LIVE CARD BALANCES, CR-4471        *
      *   CARDSVC.CARD_LIMIT        LIVE CARD LIMITS, CR-4471          *
      *                                                                *
      * THE CROSS DATABASE READS ARE COVERED BY THE SELECT GRANTS      *
      * HELD BY PRTYBAT.  PRTYBAT CANNOT SEE THE CARD TABLE, SO A      *
      * LIMIT IS ONLY REFRESHED FOR A CARD THE CALLER NAMED.  ANY      *
      * OTHER LIMIT IS CARRIED FORWARD FROM THE PRIOR POSITION AND     *
      * THE ROW IS MARKED STALE UNTIL THE MONTHLY FEED CORRECTS IT.    *
      *                                                                *
      * THE RESULT IS BUILT IN EXPOSURE-RECORD AND HANDED DOWN.  IT    *
      * IS NOT WRITTEN HERE - PRBRSK5 OWNS ALL PERSISTENCE.            *
      *                                                                *
      * CALLED BY   - PRBRSK2                                          *
      * CALLS       - PRBRSK4                                          *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - EXPOSURE REBUILT FROM LIVE DATA                         *
      *   04 - REBUILT WITH CARRIED FORWARD OR STALE COMPONENTS        *
      *   08 - EXPOSURE OUT OF LINE - LIMIT BREACH DETECTED            *
      *   12 - FATAL - PARTYDB OR CARDDB UNAVAILABLE                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRBRSK3.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'PRBRSK3 '.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
      *
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
           05  WS-PRIOR-EOF-SW         PIC X     VALUE 'N'.
               88  WS-PRIOR-EOF                  VALUE 'Y'.
           05  WS-CARD-EOF-SW          PIC X     VALUE 'N'.
               88  WS-CARD-EOF                   VALUE 'Y'.
           05  WS-STALE-SW             PIC X     VALUE 'N'.
               88  WS-STALE                      VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-OWN-RC               PIC 9(4)  VALUE ZERO.
           05  WS-CHAIN-RC             PIC 9(4)  VALUE ZERO.
           05  WS-WORST-RC             PIC 9(4)  VALUE ZERO.
           05  WS-PRIOR-CNT            PIC 9(2)  VALUE ZERO.
           05  WS-PROD-SUB             PIC S9(4) COMP VALUE ZERO.
           05  WS-SUB                  PIC S9(4) COMP VALUE ZERO.
           05  WS-FOUND-SUB            PIC S9(4) COMP VALUE ZERO.
           05  WS-ABEND-CODE           PIC S9(4) COMP VALUE ZERO.
      *
       01  WS-AMOUNT-WORK.
           05  WS-LIMIT-WORK           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-DRAWN-WORK           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-AVAIL-WORK           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-UTIL-WORK            PIC S9(5)V99  COMP-3 VALUE ZERO.
           05  WS-DISP-AMT             PIC ---,---,---,--9.99.
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME-R REDEFINES WS-CURR-TIME.
               10  WS-CURR-HHMMSS      PIC 9(6).
               10  WS-CURR-HUND        PIC 9(2).
           05  WS-TIMESTAMP            PIC X(26) VALUE SPACES.
      *
      *    PRIOR POSITION AS READ FROM PARTY_EXPOSURE
       01  WS-PRIOR-TABLE.
           05  WS-PRIOR OCCURS 15 TIMES.
               10  WS-PR-SYSTEM        PIC X(8).
               10  WS-PR-PRODUCT       PIC X(4).
               10  WS-PR-ACCT-CNT      PIC 9(4).
               10  WS-PR-LIMIT         PIC S9(13)V99 COMP-3.
               10  WS-PR-DRAWN         PIC S9(13)V99 COMP-3.
               10  WS-PR-DELQ          PIC 9.
      *
       01  WS-SQL-DISP                 PIC -(9)9.
      *
      *    HOST VARIABLES - PARTYRSK.PARTY_EXPOSURE
       01  DCL-EXPOSURE.
           05  DCL-PARTY-ID            PIC X(11).
           05  DCL-AS-OF-DATE          PIC X(10).
           05  DCL-PROD-SYSTEM         PIC X(8).
           05  DCL-PRODUCT-CD          PIC X(4).
           05  DCL-CURRENCY-CD         PIC X(3).
           05  DCL-ACCT-CNT            PIC S9(9) COMP.
           05  DCL-TOTAL-LIMIT         PIC S9(13)V99 COMP-3.
           05  DCL-TOTAL-DRAWN         PIC S9(13)V99 COMP-3.
           05  DCL-PAST-DUE-AMT        PIC S9(11)V99 COMP-3.
           05  DCL-WRITTEN-OFF-AMT     PIC S9(11)V99 COMP-3.
           05  DCL-DELQ-BUCKET         PIC S9(4) COMP.
           05  DCL-SECURED-AMT         PIC S9(13)V99 COMP-3.
      *
      *    HOST VARIABLES - CARDSVC AGGREGATES
       01  DCL-CARD-AGG.
           05  DCL-CA-PRODUCT          PIC X(4).
           05  DCL-CA-CURRENCY         PIC X(3).
           05  DCL-CA-ACCT-CNT         PIC S9(9) COMP.
           05  DCL-CA-BALANCE          PIC S9(13)V99 COMP-3.
           05  DCL-CA-PENDING          PIC S9(13)V99 COMP-3.
           05  DCL-CA-DELQ-AMT         PIC S9(11)V99 COMP-3.
           05  DCL-CA-DELQ-MAX         PIC S9(4) COMP.
           05  DCL-CA-CASH-BAL         PIC S9(13)V99 COMP-3.
      *
       01  DCL-CARD-LIMIT.
           05  DCL-CL-CARD-NUM         PIC X(16).
           05  DCL-CL-LIMIT-AMT        PIC S9(13)V99 COMP-3.
           05  DCL-CL-USED-AMT         PIC S9(13)V99 COMP-3.
           05  DCL-CL-AVAIL-AMT        PIC S9(13)V99 COMP-3.
           05  DCL-CL-RISK-BAND        PIC X(1).
      *
       01  DCL-IND-AREA.
           05  IND-SECURED             PIC S9(4) COMP.
           05  IND-DELQ-MAX            PIC S9(4) COMP.
           05  IND-CL-BAND             PIC S9(4) COMP.
      *
           COPY CVEXPO01Y.
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      ******************************************************************
      * PRIOR POSITION.  THE LATEST AS OF DATE HELD FOR THE PARTY.     *
      ******************************************************************
           EXEC SQL DECLARE PRIORCSR CURSOR FOR
               SELECT PROD_SYSTEM
                    , PRODUCT_CD
                    , CURRENCY_CD
                    , ACCT_CNT
                    , TOTAL_LIMIT
                    , TOTAL_DRAWN
                    , SECURED_AMT
                    , DELQ_BUCKET
                 FROM PARTYRSK.PARTY_EXPOSURE
                WHERE PARTY_ID = :DCL-PARTY-ID
                  AND AS_OF_DATE =
                     (SELECT MAX(AS_OF_DATE)
                        FROM PARTYRSK.PARTY_EXPOSURE
                       WHERE PARTY_ID = :DCL-PARTY-ID)
                ORDER BY PROD_SYSTEM, PRODUCT_CD
           END-EXEC.
      *
      ******************************************************************
      * LIVE CARD POSITION.  CROSS DATABASE READ ADDED UNDER CR-4471   *
      * SO THAT A RECALCULATION DOES NOT HAVE TO WAIT FOR THE NIGHTLY  *
      * EXPOSURE FEED.                                                 *
      ******************************************************************
           EXEC SQL DECLARE CARDAGCSR CURSOR FOR
               SELECT PRODUCT_CD
                    , CURRENCY_CD
                    , COUNT(*)
                    , SUM(CURR_BAL)
                    , SUM(PENDING_AUTH_AMT)
                    , SUM(DELQ_AMT)
                    , MAX(DELQ_BUCKET)
                    , SUM(CASH_BAL)
                 FROM CARDSVC.ACCOUNT
                WHERE PARTY_ID = :DCL-PARTY-ID
                  AND ACCT_STATUS IN ('O','S')
                GROUP BY PRODUCT_CD, CURRENCY_CD
                ORDER BY PRODUCT_CD, CURRENCY_CD
           END-EXEC.
      *
       LINKAGE SECTION.
      *
           COPY CVRISK01Y.
      *
           COPY CVPARTY1Y.
      *
           COPY CVSANC01Y.
      *
       01  LK-RETURN-AREA.
           05  LK-RETURN-CD            PIC S9(4) COMP.
           05  LK-RETURN-PGM           PIC X(8).
           05  LK-RETURN-MSG           PIC X(60).
      *
      ******************************************************************
       PROCEDURE DIVISION USING CV-RISK-AREA
                                PARTY-RECORD
                                SANCTION-MATCH
                                LK-RETURN-AREA.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
      *
           PERFORM 1000-LOAD-PRIOR-POSITION
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 2000-AGGREGATE-CARD
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 3000-CARRY-FORWARD-OTHERS
           PERFORM 4000-COMPUTE-TOTALS
           PERFORM 5000-CALL-SCORER
           .
       0000-RETURN.
           PERFORM 8000-APPEND-HOP
           IF WS-WORST-RC > CV-RISK-RC
               MOVE WS-WORST-RC        TO CV-RISK-RC
           END-IF
           MOVE WS-WORST-RC            TO LK-RETURN-CD
           IF WS-OWN-RC >= WS-CHAIN-RC
               MOVE WS-PROGRAM-ID      TO LK-RETURN-PGM
           END-IF
           GOBACK
           .
      *
       0100-INITIALISE.
           MOVE '0100-INITIALISE'      TO WS-PARAGRAPH
           MOVE 'N'                    TO WS-FATAL-SW
           MOVE 'N'                    TO WS-STALE-SW
           MOVE ZERO                   TO WS-OWN-RC
           MOVE ZERO                   TO WS-CHAIN-RC
           MOVE ZERO                   TO WS-WORST-RC
           MOVE ZERO                   TO WS-PRIOR-CNT
           MOVE ZERO                   TO WS-PROD-SUB
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
      *
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           ACCEPT WS-CURR-TIME         FROM TIME
           MOVE PT-PARTY-ID            TO DCL-PARTY-ID
      *
           MOVE SPACES                 TO WS-PRIOR-TABLE
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 15
               MOVE ZERO               TO WS-PR-ACCT-CNT(WS-SUB)
               MOVE ZERO               TO WS-PR-LIMIT(WS-SUB)
               MOVE ZERO               TO WS-PR-DRAWN(WS-SUB)
               MOVE ZERO               TO WS-PR-DELQ(WS-SUB)
           END-PERFORM
      *
      *    EXPOSURE-RECORD IS BUILT FROM SCRATCH EACH TIME.  THE ODO
      *    COUNT IS SET BEFORE ANY SUBSCRIPTED MOVE.
           MOVE 1                      TO EX-PROD-CNT
           MOVE SPACES                 TO EX-PARTY-ID
           MOVE PT-PARTY-ID            TO EX-PARTY-ID
           MOVE WS-CURR-DATE           TO EX-AS-OF-DATE
           MOVE WS-CURRENCY-USD        TO EX-CURRENCY
           MOVE ZERO                   TO EX-TOTAL-LIMIT
           MOVE ZERO                   TO EX-TOTAL-DRAWN
           MOVE ZERO                   TO EX-TOTAL-AVAILABLE
           MOVE ZERO                   TO EX-UNSECURED-AMT
           MOVE ZERO                   TO EX-SECURED-AMT
           MOVE ZERO                   TO EX-PAST-DUE-AMT
           MOVE ZERO                   TO EX-WRITTEN-OFF-AMT
           MOVE ZERO                   TO EX-UTILISATION-PCT
           MOVE WS-PROGRAM-ID          TO EX-CALC-PGM
           MOVE 'N'                    TO EX-STALE-FLG
      *
           STRING WS-CURR-DATE(1:4)  '-'
                  WS-CURR-DATE(5:2)  '-'
                  WS-CURR-DATE(7:2)  '-'
                  WS-CURR-HHMMSS(1:2) '.'
                  WS-CURR-HHMMSS(3:2) '.'
                  WS-CURR-HHMMSS(5:2) '.'
                  WS-CURR-HUND '0000'
                  DELIMITED BY SIZE INTO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO EX-CALC-TS
           .
      *
      ******************************************************************
      * 1000 - PRIOR POSITION                                          *
      ******************************************************************
       1000-LOAD-PRIOR-POSITION.
           MOVE '1000-LOAD-PRIOR-POSITION' TO WS-PARAGRAPH
           MOVE 'N'                    TO WS-PRIOR-EOF-SW
      *
           EXEC SQL
               OPEN PRIORCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_EXPOSURE   ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
      *        A TABLE SPACE THE JOB CANNOT OPEN AT ALL WILL NOT COME
      *        BACK FOR THE NEXT PARTY EITHER.
               IF SQLCODE = -904 OR SQLCODE = -911 OR SQLCODE = -913
                   PERFORM 9900-ABEND
               END-IF
               GO TO 1000-EXIT
           END-IF
      *
           PERFORM UNTIL WS-PRIOR-EOF
                      OR WS-PRIOR-CNT >= 15
               EXEC SQL
                   FETCH PRIORCSR
                    INTO :DCL-PROD-SYSTEM
                       , :DCL-PRODUCT-CD
                       , :DCL-CURRENCY-CD
                       , :DCL-ACCT-CNT
                       , :DCL-TOTAL-LIMIT
                       , :DCL-TOTAL-DRAWN
                       , :DCL-SECURED-AMT    :IND-SECURED
                       , :DCL-DELQ-BUCKET
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1           TO WS-PRIOR-CNT
                       MOVE DCL-PROD-SYSTEM
                                       TO WS-PR-SYSTEM(WS-PRIOR-CNT)
                       MOVE DCL-PRODUCT-CD
                                       TO WS-PR-PRODUCT(WS-PRIOR-CNT)
                       MOVE DCL-ACCT-CNT
                                       TO WS-PR-ACCT-CNT(WS-PRIOR-CNT)
                       MOVE DCL-TOTAL-LIMIT
                                       TO WS-PR-LIMIT(WS-PRIOR-CNT)
                       MOVE DCL-TOTAL-DRAWN
                                       TO WS-PR-DRAWN(WS-PRIOR-CNT)
                       MOVE DCL-DELQ-BUCKET
                                       TO WS-PR-DELQ(WS-PRIOR-CNT)
                       IF IND-SECURED NOT < ZERO
                           ADD DCL-SECURED-AMT TO EX-SECURED-AMT
                       END-IF
                   WHEN +100
                       MOVE 'Y'        TO WS-PRIOR-EOF-SW
                   WHEN OTHER
                       MOVE 'PARTY_EXPOSURE   ' TO ER-SQL-TABLE
                       MOVE 'FETCH   '  TO ER-SQL-OPERATION
                       PERFORM 9100-SQL-ERROR
                       MOVE 'Y'        TO WS-PRIOR-EOF-SW
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE PRIORCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_EXPOSURE   ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
      *
           IF WS-PRIOR-CNT = ZERO
               DISPLAY 'PRBRSK3  NO PRIOR EXPOSURE PARTY='
                       DCL-PARTY-ID ' FIRST RECALCULATION'
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - LIVE CARD AGGREGATE                                     *
      ******************************************************************
       2000-AGGREGATE-CARD.
           MOVE '2000-AGGREGATE-CARD'  TO WS-PARAGRAPH
           MOVE 'N'                    TO WS-CARD-EOF-SW
      *
           EXEC SQL
               OPEN CARDAGCSR
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -551
               WHEN -904
      *            THE CARD DATABASE IS NOT REACHABLE FROM THIS JOB.
      *            CARRY THE WHOLE CARD POSITION FORWARD AND WARN.
                   MOVE 'Y'            TO WS-STALE-SW
                   MOVE 'Y'            TO WS-CARD-EOF-SW
                   MOVE 4              TO WS-OWN-RC
                   MOVE 'CARD POSITION UNAVAILABLE - CARRIED FORWARD'
                                       TO CV-RISK-REASON-TXT
                   DISPLAY 'PRBRSK3  CARDDB UNAVAILABLE SQLCODE='
                           SQLCODE ' PARTY=' DCL-PARTY-ID
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'ACCOUNT          ' TO ER-SQL-TABLE
                   MOVE 'OPEN    '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           PERFORM UNTIL WS-CARD-EOF
                      OR EX-PROD-CNT >= 15
               EXEC SQL
                   FETCH CARDAGCSR
                    INTO :DCL-CA-PRODUCT
                       , :DCL-CA-CURRENCY
                       , :DCL-CA-ACCT-CNT
                       , :DCL-CA-BALANCE
                       , :DCL-CA-PENDING
                       , :DCL-CA-DELQ-AMT
                       , :DCL-CA-DELQ-MAX  :IND-DELQ-MAX
                       , :DCL-CA-CASH-BAL
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 2100-ADD-CARD-PRODUCT
                   WHEN +100
                       MOVE 'Y'        TO WS-CARD-EOF-SW
                   WHEN OTHER
                       MOVE 'ACCOUNT          ' TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       PERFORM 9100-SQL-ERROR
                       MOVE 'Y'        TO WS-CARD-EOF-SW
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE CARDAGCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'ACCOUNT          ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-ADD-CARD-PRODUCT.
           MOVE '2100-ADD-CARD-PRODUCT' TO WS-PARAGRAPH
      *
           IF EX-PROD-CNT = 1 AND EX-PROD-SYSTEM(1) = SPACES
               MOVE 1                  TO WS-PROD-SUB
           ELSE
               ADD 1                   TO EX-PROD-CNT
               MOVE EX-PROD-CNT        TO WS-PROD-SUB
           END-IF
      *
           MOVE WS-MODULE-CARDSVC      TO EX-PROD-SYSTEM(WS-PROD-SUB)
           MOVE DCL-CA-PRODUCT         TO EX-PROD-CODE(WS-PROD-SUB)
           MOVE DCL-CA-ACCT-CNT        TO EX-PROD-ACCT-CNT(WS-PROD-SUB)
      *
           COMPUTE WS-DRAWN-WORK = DCL-CA-BALANCE + DCL-CA-PENDING
           MOVE WS-DRAWN-WORK          TO EX-PROD-DRAWN(WS-PROD-SUB)
      *
           IF IND-DELQ-MAX < ZERO
               MOVE ZERO               TO EX-PROD-DELQ-BUCKET
                                          (WS-PROD-SUB)
           ELSE
               MOVE DCL-CA-DELQ-MAX    TO EX-PROD-DELQ-BUCKET
                                          (WS-PROD-SUB)
           END-IF
      *
           ADD DCL-CA-DELQ-AMT         TO EX-PAST-DUE-AMT
      *
      *    CARD BALANCES ARE UNSECURED IN THEIR ENTIRETY.
           ADD WS-DRAWN-WORK           TO EX-UNSECURED-AMT
      *
           PERFORM 2200-REFRESH-LIMIT
      *
      *    A DRAWN POSITION ABOVE THE LIMIT IS REPORTED AS A BUSINESS
      *    CONDITION EVEN WHEN THE ACCOUNT ITSELF IS STILL OPEN.
           IF EX-PROD-DRAWN(WS-PROD-SUB)
              > EX-PROD-LIMIT(WS-PROD-SUB)
              AND EX-PROD-LIMIT(WS-PROD-SUB) > ZERO
               IF WS-OWN-RC < 8
                   MOVE 8              TO WS-OWN-RC
               END-IF
               MOVE 'OVLM'             TO CV-RISK-REASON-CD
               MOVE EX-PROD-DRAWN(WS-PROD-SUB) TO WS-DISP-AMT
               DISPLAY 'PRBRSK3  OVER LIMIT PARTY=' DCL-PARTY-ID
                       ' PROD=' EX-PROD-CODE(WS-PROD-SUB)
                       ' DRAWN=' WS-DISP-AMT
           END-IF
           .
      *
      ******************************************************************
      * 2200 - LIMIT REFRESH                                           *
      *                                                                *
      * ONLY THE CARD NAMED ON THE REQUEST CAN BE READ DIRECTLY.  FOR  *
      * EVERYTHING ELSE THE PRIOR LIMIT IS CARRIED FORWARD AND THE     *
      * POSITION IS MARKED STALE.                                      *
      ******************************************************************
       2200-REFRESH-LIMIT.
           MOVE '2200-REFRESH-LIMIT'   TO WS-PARAGRAPH
           MOVE ZERO                   TO WS-LIMIT-WORK
      *
           IF CV-RISK-CARD-NUM NOT = SPACES
               MOVE CV-RISK-CARD-NUM   TO DCL-CL-CARD-NUM
      *
               EXEC SQL
                   SELECT LIMIT_AMT
                        , USED_AMT
                        , AVAIL_AMT
                        , RISK_BAND
                     INTO :DCL-CL-LIMIT-AMT
                        , :DCL-CL-USED-AMT
                        , :DCL-CL-AVAIL-AMT
                        , :DCL-CL-RISK-BAND :IND-CL-BAND
                     FROM CARDSVC.CARD_LIMIT
                    WHERE CARD_NUM  = :DCL-CL-CARD-NUM
                      AND LIMIT_TYPE = 'CRED'
                      AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       MOVE DCL-CL-LIMIT-AMT TO WS-LIMIT-WORK
                   WHEN +100
                       MOVE 'Y'        TO WS-STALE-SW
                   WHEN OTHER
                       MOVE 'CARD_LIMIT       ' TO ER-SQL-TABLE
                       MOVE 'SELECT  ' TO ER-SQL-OPERATION
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           ELSE
               MOVE 'Y'                TO WS-STALE-SW
           END-IF
      *
           IF WS-LIMIT-WORK = ZERO
               PERFORM 2250-CARRY-FORWARD-LIMIT
           END-IF
      *
           MOVE WS-LIMIT-WORK          TO EX-PROD-LIMIT(WS-PROD-SUB)
           ADD  WS-LIMIT-WORK          TO EX-TOTAL-LIMIT
           .
      *
       2250-CARRY-FORWARD-LIMIT.
           MOVE ZERO                   TO WS-FOUND-SUB
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > WS-PRIOR-CNT
                        OR WS-FOUND-SUB > ZERO
               IF WS-PR-SYSTEM(WS-SUB) = WS-MODULE-CARDSVC
              AND WS-PR-PRODUCT(WS-SUB) = DCL-CA-PRODUCT
                   MOVE WS-SUB         TO WS-FOUND-SUB
               END-IF
           END-PERFORM
      *
           IF WS-FOUND-SUB > ZERO
               MOVE WS-PR-LIMIT(WS-FOUND-SUB) TO WS-LIMIT-WORK
               MOVE 'Y'                TO WS-STALE-SW
           ELSE
      *        NOTHING TO CARRY FORWARD.  THE DRAWN POSITION BECOMES
      *        THE WORKING LIMIT SO UTILISATION READS 100 PERCENT.
               MOVE WS-DRAWN-WORK      TO WS-LIMIT-WORK
               MOVE 'Y'                TO WS-STALE-SW
           END-IF
           .
      *
      ******************************************************************
      * 3000 - PRODUCT SYSTEMS THAT DO NOT FEED LIVE                   *
      *                                                                *
      * ANY PRIOR ROW FOR A SYSTEM OTHER THAN THE CARD SYSTEM IS       *
      * COPIED FORWARD UNCHANGED.  THOSE SYSTEMS DELIVER A MONTHLY     *
      * FEED AND ARE NOT RE-READ HERE.                                 *
      ******************************************************************
       3000-CARRY-FORWARD-OTHERS.
           MOVE '3000-CARRY-FORWARD-OTHERS' TO WS-PARAGRAPH
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > WS-PRIOR-CNT
                        OR EX-PROD-CNT >= 15
               IF WS-PR-SYSTEM(WS-SUB) NOT = WS-MODULE-CARDSVC
                   ADD 1               TO EX-PROD-CNT
                   MOVE EX-PROD-CNT    TO WS-PROD-SUB
                   MOVE WS-PR-SYSTEM(WS-SUB)
                                       TO EX-PROD-SYSTEM(WS-PROD-SUB)
                   MOVE WS-PR-PRODUCT(WS-SUB)
                                       TO EX-PROD-CODE(WS-PROD-SUB)
                   MOVE WS-PR-ACCT-CNT(WS-SUB)
                                       TO EX-PROD-ACCT-CNT
                                          (WS-PROD-SUB)
                   MOVE WS-PR-LIMIT(WS-SUB)
                                       TO EX-PROD-LIMIT(WS-PROD-SUB)
                   MOVE WS-PR-DRAWN(WS-SUB)
                                       TO EX-PROD-DRAWN(WS-PROD-SUB)
                   MOVE WS-PR-DELQ(WS-SUB)
                                       TO EX-PROD-DELQ-BUCKET
                                          (WS-PROD-SUB)
                   ADD WS-PR-LIMIT(WS-SUB) TO EX-TOTAL-LIMIT
               END-IF
           END-PERFORM
           .
      *
      ******************************************************************
      * 4000 - TOTALS AND UTILISATION                                  *
      ******************************************************************
       4000-COMPUTE-TOTALS.
           MOVE '4000-COMPUTE-TOTALS'  TO WS-PARAGRAPH
           MOVE ZERO                   TO EX-TOTAL-DRAWN
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > EX-PROD-CNT
               ADD EX-PROD-DRAWN(WS-SUB) TO EX-TOTAL-DRAWN
           END-PERFORM
      *
           COMPUTE EX-TOTAL-AVAILABLE =
                   EX-TOTAL-LIMIT - EX-TOTAL-DRAWN
           IF EX-TOTAL-AVAILABLE < ZERO
               MOVE ZERO               TO EX-TOTAL-AVAILABLE
           END-IF
      *
           COMPUTE EX-UNSECURED-AMT =
                   EX-TOTAL-DRAWN - EX-SECURED-AMT
           IF EX-UNSECURED-AMT < ZERO
               MOVE ZERO               TO EX-UNSECURED-AMT
           END-IF
      *
           IF EX-TOTAL-LIMIT > ZERO
               COMPUTE WS-UTIL-WORK ROUNDED =
                       (EX-TOTAL-DRAWN * 100) / EX-TOTAL-LIMIT
               ON SIZE ERROR
                   MOVE 999            TO WS-UTIL-WORK
               END-COMPUTE
           ELSE
               MOVE ZERO               TO WS-UTIL-WORK
           END-IF
      *
           IF WS-UTIL-WORK > 999.99
               MOVE 999.99             TO WS-UTIL-WORK
           END-IF
           MOVE WS-UTIL-WORK           TO EX-UTILISATION-PCT
      *
           IF WS-STALE
      *        THE ROW IS PUBLISHED ANYWAY.  PRBRSK5 CLEARS THE FLAG
      *        ONLY WHEN EVERY COMPONENT CAME FROM A LIVE READ.
               MOVE 'Y'                TO EX-STALE-FLG
               IF WS-OWN-RC < 4
                   MOVE 4              TO WS-OWN-RC
               END-IF
           ELSE
               MOVE 'N'                TO EX-STALE-FLG
           END-IF
      *
           MOVE EX-TOTAL-DRAWN         TO CV-RISK-EXPOSURE-AMT
           MOVE EX-TOTAL-AVAILABLE     TO CV-RISK-AVAIL-AMT
      *
           MOVE EX-TOTAL-DRAWN         TO WS-DISP-AMT
           DISPLAY 'PRBRSK3  EXPOSURE PARTY=' EX-PARTY-ID
                   ' PRODUCTS=' EX-PROD-CNT
                   ' DRAWN=' WS-DISP-AMT
                   ' UTIL=' EX-UTILISATION-PCT
      *
           MOVE WS-OWN-RC              TO WS-WORST-RC
           .
      *
       5000-CALL-SCORER.
           MOVE '5000-CALL-SCORER'     TO WS-PARAGRAPH
           MOVE ZERO                   TO LK-RETURN-CD
      *
           CALL 'PRBRSK4' USING CV-RISK-AREA
                                PARTY-RECORD
                                EXPOSURE-RECORD
                                LK-RETURN-AREA
      *
           MOVE LK-RETURN-CD           TO WS-CHAIN-RC
           IF WS-CHAIN-RC > WS-WORST-RC
               MOVE WS-CHAIN-RC        TO WS-WORST-RC
           END-IF
           .
      *
       8000-APPEND-HOP.
           IF CV-RISK-HOP-CNT < 8
               ADD 1                   TO CV-RISK-HOP-CNT
               MOVE WS-PROGRAM-ID      TO
                    CV-RISK-HOP-PGM(CV-RISK-HOP-CNT)
               MOVE WS-OWN-RC          TO
                    CV-RISK-HOP-RC(CV-RISK-HOP-CNT)
           END-IF
           .
      *
       9100-SQL-ERROR.
           MOVE SQLCODE                TO WS-SQL-DISP
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE WS-PROGRAM-ID          TO CV-RISK-FAIL-PGM
           MOVE 'EXPOSURE REBUILD FAILED'
                                       TO CV-RISK-REASON-TXT
           MOVE 12                     TO WS-OWN-RC
           MOVE 12                     TO WS-WORST-RC
           MOVE 'Y'                    TO WS-FATAL-SW
      *
           DISPLAY 'PRBRSK3  SQL ERROR PARA=' WS-PARAGRAPH
           DISPLAY '         TABLE=' ER-SQL-TABLE
                   ' OP=' ER-SQL-OPERATION
                   ' SQLCODE=' WS-SQL-DISP
           DISPLAY '         PARTY=' DCL-PARTY-ID
                   ' SQLERRMC=' SQLERRMC(1:30)
           .
      *
      ******************************************************************
      * 9900 - U3103.  ONLY DRIVEN WHEN THE EXPOSURE TABLE ITSELF IS   *
      *        UNREADABLE - CARRYING ON WOULD PUBLISH A ZERO POSITION. *
      ******************************************************************
       9900-ABEND.
           MOVE 'U310'                 TO ER-ABEND-CODE
           MOVE 'Y'                    TO ER-ABEND-REQUESTED
           DISPLAY 'PRBRSK3  ABEND U3103 PARTY=' DCL-PARTY-ID
                   ' PARA=' WS-PARAGRAPH
           MOVE 3103                   TO WS-ABEND-CODE
           CALL 'ILBOABN0' USING WS-ABEND-CODE
           .
