      ******************************************************************
      * PRRSK02 - PARTY EXPOSURE AGGREGATION                           *
      *                                                                *
      * BUILDS THE CONSOLIDATED EXPOSURE POSITION FOR THE PARTY FROM   *
      * PARTYRSK.PARTY_EXPOSURE AT THE LATEST AVAILABLE AS OF DATE,    *
      * ACROSS EVERY PRODUCT SYSTEM THAT FEEDS THE PARTY WAREHOUSE.    *
      *                                                                *
      * FALL BACK                                                      *
      *   THE CARD FEED IS BUILT BY THE NIGHTLY CYCLE.  IF THE CARD    *
      *   ROW IS ABSENT OR CARRIES STALE_FLG 'Y' THE CARD POSITION IS  *
      *   TAKEN LIVE FROM THE CARD ACCOUNT TABLE INSTEAD OF WAITING    *
      *   FOR THE NEXT FEED.  THE READ ONLY CROSS DATABASE GRANT THAT  *
      *   MAKES THIS POSSIBLE WAS ADDED IN 2017 UNDER CHANGE CR-4471   *
      *   AND IS THE ONLY PLACE PARTYRSK READS ANOTHER MODULES DATA.   *
      *   IT IS A SELECT ONLY - NOTHING IN PARTYRSK EVER UPDATES IT.   *
      *                                                                *
      * NOTE THAT THE ONLINE RACF GROUP HOLDS NO GRANT ON THE CARD     *
      * LIMIT TABLE, ONLY THE BATCH GROUP DOES, SO THE LIVE FALL BACK  *
      * CAN RECOVER DRAWN BALANCES BUT NOT LIMITS.  WHERE NO LIMIT IS  *
      * AVAILABLE THE STALE LIMIT IS CARRIED FORWARD AND THE RESULT IS *
      * MARKED AS A WARNING.                                           *
      *                                                                *
      * CALLED BY   - PRKYC02                                          *
      * CALLS       - PRRSK03  RISK SCORING                            *
      *               PRERR01  PARTYRSK ERROR HANDLER                  *
      * TABLES      - PARTYRSK.PARTY_EXPOSURE        (SELECT, CURSOR)  *
      *               CARDSVC.ACCOUNT                (SELECT)          *
      * COMMAREA    - CV-RISK-AREA, 512 BYTES, CVRISK01Y               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRRSK02.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRRSK02 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'PRRSK03 '.
       01  WS-ERROR-PGM                PIC X(8)  VALUE 'PRERR01 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-COMMAREA-LEN             PIC S9(4) COMP VALUE 512.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-CURSOR-SW            PIC X     VALUE 'N'.
               88  WS-CURSOR-OPEN                VALUE 'Y'.
           05  WS-CARD-ROW-SW          PIC X     VALUE 'N'.
               88  WS-CARD-ROW-GOOD              VALUE 'Y'.
           05  WS-FALLBACK-SW          PIC X     VALUE 'N'.
               88  WS-FALLBACK-USED              VALUE 'Y'.
      *
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(8)  VALUE SPACES.
      *
       01  WS-TODAY                    PIC 9(8)  VALUE ZERO.
      *
       01  WS-ISO-DATE                 PIC X(10) VALUE SPACES.
       01  WS-ISO-DATE-R REDEFINES WS-ISO-DATE.
           05  WS-ISO-CCYY             PIC X(4).
           05  FILLER                  PIC X.
           05  WS-ISO-MM               PIC X(2).
           05  FILLER                  PIC X.
           05  WS-ISO-DD               PIC X(2).
      *
       01  WS-CONV-DATE                PIC 9(8)  VALUE ZERO.
       01  WS-CONV-DATE-R REDEFINES WS-CONV-DATE.
           05  WS-CV-CCYY              PIC 9(4).
           05  WS-CV-MM                PIC 9(2).
           05  WS-CV-DD                PIC 9(2).
      *
       01  WS-CARD-SYSTEM              PIC X(8)  VALUE 'CARDSVC '.
       01  WS-SUB                      PIC S9(4) COMP VALUE 0.
       01  WS-CARD-OCCUR               PIC S9(4) COMP VALUE 0.
      *
       01  WS-WORK-AMTS.
           05  WS-CARD-LIMIT           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-CARD-DRAWN           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-CARD-AVAIL           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-CALC-UTIL            PIC S9(5)V99 COMP-3  VALUE 0.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
       01  HV-KEYS.
           05  HV-PARTY-ID             PIC X(11).
           05  HV-AS-OF-DATE           PIC X(10).
      *
       01  HV-EXPO.
           05  HV-PROD-SYSTEM          PIC X(8).
           05  HV-PRODUCT-CD           PIC X(4).
           05  HV-CURRENCY-CD          PIC X(3).
           05  HV-ACCT-CNT             PIC S9(9) COMP.
           05  HV-TOTAL-LIMIT          PIC S9(13)V99 COMP-3.
           05  HV-TOTAL-DRAWN          PIC S9(13)V99 COMP-3.
           05  HV-TOTAL-AVAIL          PIC S9(13)V99 COMP-3.
           05  HV-UNSECURED-AMT        PIC S9(13)V99 COMP-3.
           05  HV-SECURED-AMT          PIC S9(13)V99 COMP-3.
           05  HV-PAST-DUE-AMT         PIC S9(11)V99 COMP-3.
           05  HV-WRITTEN-OFF-AMT      PIC S9(11)V99 COMP-3.
           05  HV-DELQ-BUCKET          PIC S9(4) COMP.
           05  HV-UTILISATION          PIC S9(3)V99 COMP-3.
           05  HV-STALE-FLG            PIC X(1).
           05  HV-CALC-PGM             PIC X(8).
      *
       01  HV-CARD.
           05  HV-CD-ACCT-CNT          PIC S9(9) COMP.
           05  HV-CD-CURR-BAL          PIC S9(13)V99 COMP-3.
           05  HV-CD-PENDING           PIC S9(13)V99 COMP-3.
           05  HV-CD-DELQ-AMT          PIC S9(11)V99 COMP-3.
           05  HV-CD-DELQ-BUCKET       PIC S9(4) COMP.
           05  HV-CD-CURRENCY          PIC X(3).
      *
       01  HV-INDICATORS.
           05  IND-CALC-PGM            PIC S9(4) COMP.
           05  IND-CD-CURR-BAL         PIC S9(4) COMP.
           05  IND-CD-PENDING          PIC S9(4) COMP.
           05  IND-CD-DELQ-AMT         PIC S9(4) COMP.
           05  IND-CD-DELQ-BUCKET      PIC S9(4) COMP.
           05  IND-CD-CURRENCY         PIC S9(4) COMP.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL DECLARE EXPOCSR CURSOR FOR
               SELECT PROD_SYSTEM
                    , PRODUCT_CD
                    , CURRENCY_CD
                    , ACCT_CNT
                    , TOTAL_LIMIT
                    , TOTAL_DRAWN
                    , TOTAL_AVAILABLE
                    , UNSECURED_AMT
                    , SECURED_AMT
                    , PAST_DUE_AMT
                    , WRITTEN_OFF_AMT
                    , DELQ_BUCKET
                    , UTILISATION_PCT
                    , STALE_FLG
                    , CALC_PGM
                 FROM PARTYRSK.PARTY_EXPOSURE
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND AS_OF_DATE = DATE(:HV-AS-OF-DATE)
                ORDER BY PROD_SYSTEM, PRODUCT_CD
           END-EXEC.
      *
           COPY CVEXPO01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
           COPY CVRISK01Y.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           IF EIBCALEN < WS-COMMAREA-LEN
               EXEC CICS ABEND ABCODE('PRR2') NODUMP END-EXEC
           END-IF
      *
           SET ADDRESS OF CV-RISK-AREA TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-INITIALISE
           PERFORM 1500-FIND-AS-OF-DATE
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 2000-AGGREGATE-EXPOSURE
           END-IF
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 3000-CHECK-CARD-POSITION
           END-IF
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 4000-DERIVE-TOTALS
               PERFORM 5000-LINK-SCORING
           END-IF
      *
           PERFORM 7000-ADD-HOP
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - INITIALISE THE EXPOSURE RECORD                          *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-CURSOR-SW
           MOVE 'N'                    TO WS-CARD-ROW-SW
           MOVE 'N'                    TO WS-FALLBACK-SW
           MOVE ZERO                   TO WS-CARD-OCCUR
      *
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'N'                    TO ER-ABEND-REQUESTED
      *
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-CYMD)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE WS-DATE-CYMD           TO WS-TODAY
      *
      *    THE OCCURS DEPENDING ON COUNT IS SET BEFORE ANY OCCURRENCE
      *    IS TOUCHED - THE TRAILER FIELDS SIT IMMEDIATELY AFTER THE
      *    LAST ACTIVE ENTRY.
           MOVE 1                      TO EX-PROD-CNT
           MOVE CV-RISK-PARTY-ID       TO EX-PARTY-ID
           MOVE ZERO                   TO EX-AS-OF-DATE
           MOVE WS-CURRENCY-USD        TO EX-CURRENCY
           MOVE ZERO                   TO EX-TOTAL-LIMIT
           MOVE ZERO                   TO EX-TOTAL-DRAWN
           MOVE ZERO                   TO EX-TOTAL-AVAILABLE
           MOVE ZERO                   TO EX-UNSECURED-AMT
           MOVE ZERO                   TO EX-SECURED-AMT
           MOVE ZERO                   TO EX-PAST-DUE-AMT
           MOVE ZERO                   TO EX-WRITTEN-OFF-AMT
           MOVE ZERO                   TO EX-UTILISATION-PCT
           MOVE WS-PGM-ID              TO EX-CALC-PGM
           MOVE SPACES                 TO EX-CALC-TS
           MOVE 'N'                    TO EX-STALE-FLG
           MOVE ZERO                   TO EX-PROD-CNT
      *
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
           .
      *
      ******************************************************************
      * 1500 - LATEST AS OF DATE HELD FOR THE PARTY                    *
      *                                                                *
      * THE WAREHOUSE KEEPS THIRTEEN MONTHS OF POSITIONS.  THE CHAIN   *
      * ALWAYS WORKS ON THE MOST RECENT ONE.                           *
      ******************************************************************
       1500-FIND-AS-OF-DATE.
           EXEC SQL
               SELECT CHAR(MAX(AS_OF_DATE), ISO)
                 INTO :HV-AS-OF-DATE
                 FROM PARTYRSK.PARTY_EXPOSURE
                WHERE PARTY_ID = :HV-PARTY-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE SPACES         TO HV-AS-OF-DATE
               WHEN OTHER
                   MOVE 'PARTY_EXPOSURE    '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '1500-FIND-AS-OF-DATE'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
                   GO TO 1500-EXIT
           END-EVALUATE
      *
           IF HV-AS-OF-DATE = SPACES OR LOW-VALUES
      *        NO WAREHOUSE POSITION AT ALL.  EVERYTHING WILL COME
      *        FROM THE LIVE FALL BACK BELOW.
               MOVE ZERO               TO EX-AS-OF-DATE
               MOVE 'Y'                TO EX-STALE-FLG
           ELSE
               MOVE HV-AS-OF-DATE      TO WS-ISO-DATE
               MOVE WS-ISO-CCYY        TO WS-CV-CCYY
               MOVE WS-ISO-MM          TO WS-CV-MM
               MOVE WS-ISO-DD          TO WS-CV-DD
               MOVE WS-CONV-DATE       TO EX-AS-OF-DATE
           END-IF
           .
       1500-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - AGGREGATE EVERY PRODUCT SYSTEM AT THAT DATE             *
      ******************************************************************
       2000-AGGREGATE-EXPOSURE.
           IF EX-AS-OF-DATE = ZERO
               GO TO 2000-EXIT
           END-IF
      *
           EXEC SQL OPEN EXPOCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'PARTY_EXPOSURE    '
                                       TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE '2000-AGGREGATE-EXPOSURE'
                                       TO ER-PARAGRAPH
               PERFORM 8000-SQL-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           MOVE 'Y'                    TO WS-CURSOR-SW
      *
           PERFORM 2100-FETCH-EXPOSURE
               UNTIL SQLCODE = +100
                  OR WS-ERROR-FOUND
                  OR EX-PROD-CNT >= 15
      *
           IF WS-CURSOR-OPEN
               EXEC SQL CLOSE EXPOCSR END-EXEC
               MOVE 'N'                TO WS-CURSOR-SW
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-FETCH-EXPOSURE.
           EXEC SQL
               FETCH EXPOCSR
                INTO :HV-PROD-SYSTEM
                   , :HV-PRODUCT-CD
                   , :HV-CURRENCY-CD
                   , :HV-ACCT-CNT
                   , :HV-TOTAL-LIMIT
                   , :HV-TOTAL-DRAWN
                   , :HV-TOTAL-AVAIL
                   , :HV-UNSECURED-AMT
                   , :HV-SECURED-AMT
                   , :HV-PAST-DUE-AMT
                   , :HV-WRITTEN-OFF-AMT
                   , :HV-DELQ-BUCKET
                   , :HV-UTILISATION
                   , :HV-STALE-FLG
                   , :HV-CALC-PGM      :IND-CALC-PGM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   PERFORM 2200-POST-PRODUCT
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE 'PARTY_EXPOSURE    '
                                       TO ER-SQL-TABLE
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   MOVE '2100-FETCH-EXPOSURE'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2200 - ADD ONE PRODUCT SYSTEM ROW TO THE AGGREGATE             *
      ******************************************************************
       2200-POST-PRODUCT.
           ADD 1                       TO EX-PROD-CNT
           MOVE EX-PROD-CNT            TO WS-SUB
      *
           MOVE HV-PROD-SYSTEM         TO EX-PROD-SYSTEM(WS-SUB)
           MOVE HV-PRODUCT-CD          TO EX-PROD-CODE(WS-SUB)
           MOVE HV-ACCT-CNT            TO EX-PROD-ACCT-CNT(WS-SUB)
           MOVE HV-TOTAL-LIMIT         TO EX-PROD-LIMIT(WS-SUB)
           MOVE HV-TOTAL-DRAWN         TO EX-PROD-DRAWN(WS-SUB)
           MOVE HV-DELQ-BUCKET         TO EX-PROD-DELQ-BUCKET(WS-SUB)
      *
           ADD HV-TOTAL-LIMIT          TO EX-TOTAL-LIMIT
           ADD HV-TOTAL-DRAWN          TO EX-TOTAL-DRAWN
           ADD HV-TOTAL-AVAIL          TO EX-TOTAL-AVAILABLE
           ADD HV-UNSECURED-AMT        TO EX-UNSECURED-AMT
           ADD HV-SECURED-AMT          TO EX-SECURED-AMT
           ADD HV-PAST-DUE-AMT         TO EX-PAST-DUE-AMT
           ADD HV-WRITTEN-OFF-AMT      TO EX-WRITTEN-OFF-AMT
      *
      *    REMEMBER WHERE THE CARD ROW LANDED SO THE FALL BACK CAN
      *    OVERWRITE IT IN PLACE RATHER THAN ADDING A SECOND ONE.
           IF HV-PROD-SYSTEM = WS-CARD-SYSTEM
               MOVE WS-SUB             TO WS-CARD-OCCUR
               IF HV-STALE-FLG = 'Y'
                   MOVE 'N'            TO WS-CARD-ROW-SW
                   MOVE 'Y'            TO EX-STALE-FLG
               ELSE
                   MOVE 'Y'            TO WS-CARD-ROW-SW
               END-IF
           END-IF
      *
           IF HV-CURRENCY-CD NOT = WS-CURRENCY-USD
      *        MULTI CURRENCY PARTIES ARE AGGREGATED AT PARITY IN THIS
      *        RELEASE.  THE POSITION IS FLAGGED AS A WARNING SO THE
      *        CREDIT OFFICER KNOWS THE NUMBER IS INDICATIVE.
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
               MOVE 'CCY1'             TO CV-RISK-REASON-CD
               MOVE 'EXPOSURE AGGREGATED ACROSS CURRENCIES AT PARITY'
                                       TO CV-RISK-REASON-TXT
           END-IF
           .
      *
      ******************************************************************
      * 3000 - CARD POSITION FALL BACK                                 *
      ******************************************************************
       3000-CHECK-CARD-POSITION.
           IF WS-CARD-ROW-GOOD
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 3100-READ-LIVE-CARD
      *
           IF WS-ERROR-FOUND
               GO TO 3000-EXIT
           END-IF
      *
           IF NOT WS-FALLBACK-USED
               GO TO 3000-EXIT
           END-IF
      *
           IF WS-CARD-OCCUR > ZERO
               PERFORM 3200-REPLACE-CARD-ROW
           ELSE
               PERFORM 3300-ADD-CARD-ROW
           END-IF
      *
           MOVE 'CARD POSITION TAKEN LIVE - WAREHOUSE FEED STALE'
                                       TO CV-RISK-REASON-TXT
           MOVE 'EXP4'                 TO CV-RISK-REASON-CD
           IF CV-RISK-RC < 0004
               MOVE 0004               TO CV-RISK-RC
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3100 - LIVE READ OF THE CARD ACCOUNT POSITION                  *
      *        CROSS DATABASE SELECT, CR-4471.  READ ONLY.             *
      ******************************************************************
       3100-READ-LIVE-CARD.
           EXEC SQL
               SELECT COUNT(*)
                    , SUM(CURR_BAL)
                    , SUM(PENDING_AUTH_AMT)
                    , SUM(DELQ_AMT)
                    , MAX(DELQ_BUCKET)
                    , MIN(CURRENCY_CD)
                 INTO :HV-CD-ACCT-CNT
                    , :HV-CD-CURR-BAL    :IND-CD-CURR-BAL
                    , :HV-CD-PENDING     :IND-CD-PENDING
                    , :HV-CD-DELQ-AMT    :IND-CD-DELQ-AMT
                    , :HV-CD-DELQ-BUCKET :IND-CD-DELQ-BUCKET
                    , :HV-CD-CURRENCY    :IND-CD-CURRENCY
                 FROM CARDSVC.ACCOUNT
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND ACCT_STATUS IN ('O','S')
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE ZERO           TO HV-CD-ACCT-CNT
               WHEN -551
      *            THE GRANT HAS BEEN REVOKED OR THE PACKAGE WAS
      *            REBOUND WITHOUT IT.  THE ASSESSMENT CONTINUES ON
      *            THE STALE POSITION AND IS DOWNGRADED TO A WARNING.
                   MOVE 'W'            TO ER-SEVERITY
                   MOVE 'SQL '         TO ER-ERROR-TYPE
                   MOVE 'ACCOUNT           '
                                       TO ER-SQL-TABLE
                   MOVE 'EXP5'         TO ER-REASON-CD
                   MOVE '3100-READ-LIVE-CARD'
                                       TO ER-PARAGRAPH
                   MOVE 'CROSS DATABASE READ REFUSED - CHECK CR-4471'
                                       TO ER-MESSAGE
                   MOVE SQLCODE        TO ER-SQLCODE
                   PERFORM 9000-REPORT-ERROR
                   MOVE 0004           TO CV-RISK-RC
                   GO TO 3100-EXIT
               WHEN OTHER
                   MOVE 'ACCOUNT           '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '3100-READ-LIVE-CARD'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
                   GO TO 3100-EXIT
           END-EVALUATE
      *
           IF HV-CD-ACCT-CNT = ZERO
      *        NO OPEN CARD ACCOUNTS.  NOTHING TO FALL BACK TO AND
      *        THAT IS A PERFECTLY NORMAL POSITION FOR A NEW PARTY.
               GO TO 3100-EXIT
           END-IF
      *
           IF IND-CD-CURR-BAL < ZERO
               MOVE ZERO               TO HV-CD-CURR-BAL
           END-IF
           IF IND-CD-PENDING < ZERO
               MOVE ZERO               TO HV-CD-PENDING
           END-IF
           IF IND-CD-DELQ-AMT < ZERO
               MOVE ZERO               TO HV-CD-DELQ-AMT
           END-IF
           IF IND-CD-DELQ-BUCKET < ZERO
               MOVE ZERO               TO HV-CD-DELQ-BUCKET
           END-IF
      *
      *    DRAWN IS THE POSTED BALANCE PLUS ANYTHING AUTHORISED AND
      *    NOT YET PRESENTED.  A REQUEST IN FLIGHT IS NOT INCLUDED -
      *    THE SCORING PROGRAM ADDS IT SEPARATELY.
           COMPUTE WS-CARD-DRAWN =
                   HV-CD-CURR-BAL + HV-CD-PENDING
      *
           MOVE 'Y'                    TO WS-FALLBACK-SW
           .
       3100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3200 - OVERLAY THE STALE CARD OCCURRENCE                       *
      *                                                                *
      * THE LIMIT CANNOT BE REFRESHED ONLINE - THE CARD LIMIT TABLE IS *
      * GRANTED TO THE BATCH GROUP ONLY - SO THE STALE LIMIT IS KEPT   *
      * AND ONLY THE DRAWN SIDE IS REPLACED.                           *
      ******************************************************************
       3200-REPLACE-CARD-ROW.
           MOVE WS-CARD-OCCUR          TO WS-SUB
      *
           SUBTRACT EX-PROD-DRAWN(WS-SUB)
                                       FROM EX-TOTAL-DRAWN
           SUBTRACT EX-PROD-DRAWN(WS-SUB)
                                       FROM EX-UNSECURED-AMT
      *
           MOVE WS-CARD-DRAWN          TO EX-PROD-DRAWN(WS-SUB)
           MOVE HV-CD-ACCT-CNT         TO EX-PROD-ACCT-CNT(WS-SUB)
           MOVE HV-CD-DELQ-BUCKET      TO EX-PROD-DELQ-BUCKET(WS-SUB)
      *
           ADD WS-CARD-DRAWN           TO EX-TOTAL-DRAWN
           ADD WS-CARD-DRAWN           TO EX-UNSECURED-AMT
      *
           MOVE EX-PROD-LIMIT(WS-SUB)  TO WS-CARD-LIMIT
      *
           COMPUTE EX-TOTAL-AVAILABLE =
                   EX-TOTAL-LIMIT - EX-TOTAL-DRAWN
      *
           ADD HV-CD-DELQ-AMT          TO EX-PAST-DUE-AMT
           MOVE 'Y'                    TO EX-STALE-FLG
           .
      *
      ******************************************************************
      * 3300 - NO CARD ROW AT ALL IN THE WAREHOUSE                     *
      ******************************************************************
       3300-ADD-CARD-ROW.
           IF EX-PROD-CNT >= 15
      *        THE ARRAY IS FULL.  THE TOTALS STILL PICK THE POSITION
      *        UP, ONLY THE PRODUCT BREAKDOWN LOSES IT.
               ADD WS-CARD-DRAWN       TO EX-TOTAL-DRAWN
               ADD WS-CARD-DRAWN       TO EX-UNSECURED-AMT
               ADD HV-CD-DELQ-AMT      TO EX-PAST-DUE-AMT
               MOVE 0004               TO CV-RISK-RC
               GO TO 3300-EXIT
           END-IF
      *
           ADD 1                       TO EX-PROD-CNT
           MOVE EX-PROD-CNT            TO WS-SUB
      *
           MOVE WS-CARD-SYSTEM         TO EX-PROD-SYSTEM(WS-SUB)
           MOVE 'CARD'                 TO EX-PROD-CODE(WS-SUB)
           MOVE HV-CD-ACCT-CNT         TO EX-PROD-ACCT-CNT(WS-SUB)
           MOVE ZERO                   TO EX-PROD-LIMIT(WS-SUB)
           MOVE WS-CARD-DRAWN          TO EX-PROD-DRAWN(WS-SUB)
           MOVE HV-CD-DELQ-BUCKET      TO EX-PROD-DELQ-BUCKET(WS-SUB)
      *
           ADD WS-CARD-DRAWN           TO EX-TOTAL-DRAWN
           ADD WS-CARD-DRAWN           TO EX-UNSECURED-AMT
           ADD HV-CD-DELQ-AMT          TO EX-PAST-DUE-AMT
      *
           COMPUTE EX-TOTAL-AVAILABLE =
                   EX-TOTAL-LIMIT - EX-TOTAL-DRAWN
           MOVE 'Y'                    TO EX-STALE-FLG
           .
       3300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - DERIVE THE UTILISATION AND PUBLISH THE POSITION         *
      ******************************************************************
       4000-DERIVE-TOTALS.
           IF EX-TOTAL-LIMIT > ZERO
               COMPUTE WS-CALC-UTIL ROUNDED =
                       (EX-TOTAL-DRAWN * 100) / EX-TOTAL-LIMIT
               IF WS-CALC-UTIL > 999.99
                   MOVE 999.99         TO EX-UTILISATION-PCT
               ELSE
                   MOVE WS-CALC-UTIL   TO EX-UTILISATION-PCT
               END-IF
           ELSE
               MOVE ZERO               TO EX-UTILISATION-PCT
           END-IF
      *
           IF EX-TOTAL-AVAILABLE = ZERO
               COMPUTE EX-TOTAL-AVAILABLE =
                       EX-TOTAL-LIMIT - EX-TOTAL-DRAWN
           END-IF
      *
           IF EX-TOTAL-AVAILABLE < ZERO
               MOVE ZERO               TO EX-TOTAL-AVAILABLE
           END-IF
      *
           MOVE EX-TOTAL-DRAWN         TO CV-RISK-EXPOSURE-AMT
           MOVE EX-TOTAL-AVAILABLE     TO CV-RISK-AVAIL-AMT
      *
      *    A REQUEST BIGGER THAN THE HEADROOM IS NOT DECLINED HERE.
      *    IT IS THE SCORING PROGRAM THAT WEIGHS IT.
           IF EX-PROD-CNT = ZERO
               MOVE 'NO EXPOSURE POSITION HELD FOR THIS PARTY'
                                       TO CV-RISK-REASON-TXT
               MOVE 'EXP0'             TO CV-RISK-REASON-CD
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 5000 - CONTINUE INTO SCORING                                   *
      ******************************************************************
       5000-LINK-SCORING.
           EXEC CICS LINK
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CV-RISK-AREA)
                     LENGTH(WS-COMMAREA-LEN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP = DFHRESP(NORMAL)
               GO TO 5000-EXIT
           END-IF
      *
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'LNK4'                 TO CV-RISK-REASON-CD
           MOVE 'SCORING MODULE COULD NOT BE LINKED'
                                       TO CV-RISK-REASON-TXT
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE '5000-LINK-SCORING'    TO ER-PARAGRAPH
           MOVE CV-RISK-REASON-TXT     TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
       5000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 7000 - HOP TRACE                                               *
      ******************************************************************
       7000-ADD-HOP.
           IF CV-RISK-HOP-CNT < 8
               ADD 1                   TO CV-RISK-HOP-CNT
               MOVE WS-PGM-ID          TO
                                    CV-RISK-HOP-PGM(CV-RISK-HOP-CNT)
               MOVE CV-RISK-RC         TO
                                    CV-RISK-HOP-RC(CV-RISK-HOP-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 8000 / 9000 - DIAGNOSTICS                                      *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'EXPOSURE AGGREGATION FAILED'
                                       TO ER-MESSAGE
      *
           IF WS-CURSOR-OPEN
               EXEC SQL CLOSE EXPOCSR END-EXEC
               MOVE 'N'                TO WS-CURSOR-SW
           END-IF
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'SQL3'                 TO CV-RISK-REASON-CD
           MOVE 'EXPOSURE POSITION UNAVAILABLE - RISK NOT ASSESSED'
                                       TO CV-RISK-REASON-TXT
           PERFORM 9000-REPORT-ERROR
           .
      *
       9000-REPORT-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
