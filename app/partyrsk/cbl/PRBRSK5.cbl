      ******************************************************************
      * PRBRSK5 - BATCH RISK PERSIST                                   *
      *                                                                *
      * LAST PROGRAM OF THE BATCH RECALCULATION CHAIN.  EVERYTHING     *
      * THE CHAIN HAS WORKED OUT IS WRITTEN HERE AND NOWHERE ELSE.     *
      *                                                                *
      *   PARTYRSK.PARTY_RISK_SCORE   INSERT, ONE ROW PER RESCORE      *
      *   PARTYRSK.PARTY_EXPOSURE     UPDATE, INSERT WHEN NOT PRESENT  *
      *   PARTYRSK.PARTY_RISK_AUDIT   INSERT, EVENT TYPE RCAL          *
      *                                                                *
      * NO COMMIT IS TAKEN HERE.  PRBRSK1 OWNS THE UNIT OF WORK.       *
      *                                                                *
      * CALLED BY   - PRBRSK4                                          *
      * CALLS       - NONE                                             *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - ALL ROWS WRITTEN                                        *
      *   04 - WRITTEN, DUPLICATE SCORE ROW SUPPRESSED                 *
      *   12 - FATAL - NOTHING RELIABLE ON THE DATABASE, CALLER MUST   *
      *        BACK OUT                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRBRSK5.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'PRBRSK5 '.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
      *
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
           05  WS-OLD-FOUND-SW         PIC X     VALUE 'N'.
               88  WS-OLD-FOUND                  VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-OWN-RC               PIC 9(4)  VALUE ZERO.
           05  WS-WORST-RC             PIC 9(4)  VALUE ZERO.
           05  WS-SUB                  PIC S9(4) COMP VALUE ZERO.
           05  WS-EXPO-UPD             PIC 9(4)  VALUE ZERO.
           05  WS-EXPO-INS             PIC 9(4)  VALUE ZERO.
           05  WS-AUDIT-SEQ            PIC 9(4)  VALUE ZERO.
           05  WS-ABEND-CODE           PIC S9(4) COMP VALUE ZERO.
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME-R REDEFINES WS-CURR-TIME.
               10  WS-CURR-HHMMSS      PIC 9(6).
               10  WS-CURR-HUND        PIC 9(2).
      *
       01  WS-TRACE-WORK.
           05  WS-TRACE-TXT            PIC X(120) VALUE SPACES.
           05  WS-TRACE-POS            PIC S9(4) COMP VALUE 1.
           05  WS-TRACE-LEN            PIC S9(4) COMP VALUE ZERO.
           05  WS-HOP-RC-ED            PIC 9(4).
      *
       01  WS-COMP-WORK.
           05  WS-COMP-TXT             PIC X(400) VALUE SPACES.
           05  WS-COMP-POS             PIC S9(4) COMP VALUE 1.
           05  WS-CP-POINTS-ED         PIC -(5)9.99.
      *
       01  WS-SQL-DISP                 PIC -(9)9.
      *
      *    HOST VARIABLES
       01  DCL-SCORE-ROW.
           05  DCL-PARTY-ID            PIC X(11).
           05  DCL-SCORE-TS            PIC X(26).
           05  DCL-MODEL-ID            PIC X(8).
           05  DCL-MODEL-VERSION       PIC S9(4) COMP.
           05  DCL-RISK-SCORE          PIC S9(4) COMP.
           05  DCL-RISK-BAND           PIC X(1).
           05  DCL-PD-PCT              PIC S9(3)V9(5) COMP-3.
           05  DCL-EXPOSURE-AMT        PIC S9(13)V99 COMP-3.
           05  DCL-ADVICE-CD           PIC X(4).
           05  DCL-REASON-CD           PIC X(4).
           05  DCL-KYC-STATUS          PIC X(2).
           05  DCL-SANCTION-FLG        PIC X(1).
           05  DCL-SCORED-BY           PIC X(8).
           05  DCL-SOURCE-CHANNEL      PIC X(1).
      *
       01  DCL-COMP-DATA.
           49  DCL-COMP-LEN            PIC S9(4) COMP.
           49  DCL-COMP-TXT            PIC X(400).
      *
       01  DCL-EXPO-ROW.
           05  DCL-AS-OF-DATE          PIC X(10).
           05  DCL-PROD-SYSTEM         PIC X(8).
           05  DCL-PRODUCT-CD          PIC X(4).
           05  DCL-CURRENCY-CD         PIC X(3).
           05  DCL-ACCT-CNT            PIC S9(9) COMP.
           05  DCL-TOTAL-LIMIT         PIC S9(13)V99 COMP-3.
           05  DCL-TOTAL-DRAWN         PIC S9(13)V99 COMP-3.
           05  DCL-TOTAL-AVAIL         PIC S9(13)V99 COMP-3.
           05  DCL-UNSECURED-AMT       PIC S9(13)V99 COMP-3.
           05  DCL-SECURED-AMT         PIC S9(13)V99 COMP-3.
           05  DCL-PAST-DUE-AMT        PIC S9(11)V99 COMP-3.
           05  DCL-WRITTEN-OFF-AMT     PIC S9(11)V99 COMP-3.
           05  DCL-DELQ-BUCKET         PIC S9(4) COMP.
           05  DCL-UTIL-PCT            PIC S9(3)V99 COMP-3.
           05  DCL-STALE-FLG           PIC X(1).
           05  DCL-CALC-PGM            PIC X(8).
      *
       01  DCL-AUDIT-ROW.
           05  DCL-AUDIT-ID            PIC X(20).
           05  DCL-EVENT-TYPE          PIC X(4).
           05  DCL-REQ-MODULE          PIC X(8).
           05  DCL-REQ-PGM             PIC X(8).
           05  DCL-CORREL-ID           PIC X(16).
           05  DCL-CHANNEL             PIC X(1).
           05  DCL-OLD-SCORE           PIC S9(4) COMP.
           05  DCL-NEW-SCORE           PIC S9(4) COMP.
           05  DCL-OLD-BAND            PIC X(1).
           05  DCL-NEW-BAND            PIC X(1).
           05  DCL-REASON-TXT          PIC X(60).
      *
       01  DCL-HOP-TRACE.
           49  DCL-HOP-LEN             PIC S9(4) COMP.
           49  DCL-HOP-TXT             PIC X(120).
      *
       01  DCL-IND-AREA.
           05  IND-OLD-SCORE           PIC S9(4) COMP.
           05  IND-OLD-BAND            PIC S9(4) COMP.
           05  IND-REASON-CD           PIC S9(4) COMP.
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       LINKAGE SECTION.
      *
           COPY CVRISK01Y.
      *
           COPY CVEXPO01Y.
      *
           COPY CVSCOR01Y.
      *
       01  LK-RETURN-AREA.
           05  LK-RETURN-CD            PIC S9(4) COMP.
           05  LK-RETURN-PGM           PIC X(8).
           05  LK-RETURN-MSG           PIC X(60).
      *
      ******************************************************************
       PROCEDURE DIVISION USING CV-RISK-AREA
                                EXPOSURE-RECORD
                                SCORE-RECORD
                                LK-RETURN-AREA.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
           PERFORM 1000-READ-PRIOR-SCORE
      *
           PERFORM 2000-WRITE-SCORE
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 3000-WRITE-EXPOSURE
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 4000-WRITE-AUDIT
           .
       0000-RETURN.
           PERFORM 8000-APPEND-HOP
           IF WS-OWN-RC > CV-RISK-RC
               MOVE WS-OWN-RC          TO CV-RISK-RC
           END-IF
           MOVE WS-OWN-RC              TO WS-WORST-RC
           MOVE WS-WORST-RC            TO LK-RETURN-CD
           MOVE WS-PROGRAM-ID          TO LK-RETURN-PGM
           GOBACK
           .
      *
       0100-INITIALISE.
           MOVE '0100-INITIALISE'      TO WS-PARAGRAPH
           MOVE 'N'                    TO WS-FATAL-SW
           MOVE 'N'                    TO WS-OLD-FOUND-SW
           MOVE ZERO                   TO WS-OWN-RC
           MOVE ZERO                   TO WS-EXPO-UPD
           MOVE ZERO                   TO WS-EXPO-INS
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
      *
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           ACCEPT WS-CURR-TIME         FROM TIME
      *
           MOVE SC-PARTY-ID            TO DCL-PARTY-ID
      *
           STRING WS-CURR-DATE(1:4)  '-'
                  WS-CURR-DATE(5:2)  '-'
                  WS-CURR-DATE(7:2)  '-'
                  WS-CURR-HHMMSS(1:2) '.'
                  WS-CURR-HHMMSS(3:2) '.'
                  WS-CURR-HHMMSS(5:2) '.'
                  WS-CURR-HUND '0000'
                  DELIMITED BY SIZE INTO DCL-SCORE-TS
      *
           MOVE EX-AS-OF-DATE(1:4)     TO DCL-AS-OF-DATE(1:4)
           MOVE '-'                    TO DCL-AS-OF-DATE(5:1)
           MOVE EX-AS-OF-DATE(5:2)     TO DCL-AS-OF-DATE(6:2)
           MOVE '-'                    TO DCL-AS-OF-DATE(8:1)
           MOVE EX-AS-OF-DATE(7:2)     TO DCL-AS-OF-DATE(9:2)
      *
           PERFORM 0200-BUILD-TRACE
           PERFORM 0300-BUILD-COMPONENTS
           .
      *
      ******************************************************************
      * 0200 - FLATTEN THE HOP TRACE FOR THE AUDIT ROW                 *
      *        FORMAT  PGMNAME/RC PGMNAME/RC ...                       *
      ******************************************************************
       0200-BUILD-TRACE.
           MOVE SPACES                 TO WS-TRACE-TXT
           MOVE 1                      TO WS-TRACE-POS
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > CV-RISK-HOP-CNT
                        OR WS-SUB > 8
                        OR WS-TRACE-POS > 106
               MOVE CV-RISK-HOP-RC(WS-SUB) TO WS-HOP-RC-ED
               STRING CV-RISK-HOP-PGM(WS-SUB) DELIMITED BY SPACE
                      '/'                     DELIMITED BY SIZE
                      WS-HOP-RC-ED            DELIMITED BY SIZE
                      ' '                     DELIMITED BY SIZE
                      INTO WS-TRACE-TXT
                      WITH POINTER WS-TRACE-POS
               END-STRING
           END-PERFORM
      *
           COMPUTE WS-TRACE-LEN = WS-TRACE-POS - 1
           IF WS-TRACE-LEN < 1
               MOVE 1                  TO WS-TRACE-LEN
           END-IF
           MOVE WS-TRACE-TXT           TO DCL-HOP-TXT
           MOVE WS-TRACE-LEN           TO DCL-HOP-LEN
           .
      *
      ******************************************************************
      * 0300 - COMPONENT BREAKDOWN.  HELD AS TEXT ON THE SCORE ROW SO  *
      *        THAT A CHALLENGE CAN BE ANSWERED WITHOUT RE-RUNNING     *
      *        THE MODEL.                                              *
      ******************************************************************
       0300-BUILD-COMPONENTS.
           MOVE SPACES                 TO WS-COMP-TXT
           MOVE 1                      TO WS-COMP-POS
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > SC-COMPONENT-CNT
                        OR WS-SUB > 12
                        OR WS-COMP-POS > 380
               MOVE SC-CP-POINTS(WS-SUB) TO WS-CP-POINTS-ED
               STRING SC-CP-CODE(WS-SUB) DELIMITED BY SPACE
                      '='                DELIMITED BY SIZE
                      WS-CP-POINTS-ED    DELIMITED BY SIZE
                      ';'                DELIMITED BY SIZE
                      INTO WS-COMP-TXT
                      WITH POINTER WS-COMP-POS
               END-STRING
           END-PERFORM
      *
           MOVE WS-COMP-TXT            TO DCL-COMP-TXT
           COMPUTE DCL-COMP-LEN = WS-COMP-POS - 1
           IF DCL-COMP-LEN < 1
               MOVE 1                  TO DCL-COMP-LEN
           END-IF
           .
      *
      ******************************************************************
      * 1000 - PRIOR SCORE, FOR THE AUDIT TRAIL                        *
      ******************************************************************
       1000-READ-PRIOR-SCORE.
           MOVE '1000-READ-PRIOR-SCORE' TO WS-PARAGRAPH
      *
           EXEC SQL
               SELECT RISK_SCORE
                    , RISK_BAND
                 INTO :DCL-OLD-SCORE  :IND-OLD-SCORE
                    , :DCL-OLD-BAND   :IND-OLD-BAND
                 FROM PARTYRSK.PARTY_RISK_SCORE
                WHERE PARTY_ID = :DCL-PARTY-ID
                ORDER BY SCORE_TS DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-OLD-FOUND-SW
               WHEN +100
                   MOVE ZERO           TO DCL-OLD-SCORE
                   MOVE SPACES         TO DCL-OLD-BAND
                   MOVE -1             TO IND-OLD-SCORE
                   MOVE -1             TO IND-OLD-BAND
               WHEN OTHER
                   MOVE 'PARTY_RISK_SCORE ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2000 - SCORE ROW                                               *
      ******************************************************************
       2000-WRITE-SCORE.
           MOVE '2000-WRITE-SCORE'     TO WS-PARAGRAPH
      *
           MOVE SC-MODEL-ID            TO DCL-MODEL-ID
           MOVE SC-MODEL-VERSION       TO DCL-MODEL-VERSION
           MOVE SC-SCORE               TO DCL-RISK-SCORE
           MOVE SC-BAND                TO DCL-RISK-BAND
           MOVE SC-PD-PCT              TO DCL-PD-PCT
           MOVE EX-TOTAL-DRAWN         TO DCL-EXPOSURE-AMT
           MOVE CV-RISK-ADVICE-CD      TO DCL-ADVICE-CD
           MOVE CV-RISK-KYC-STATUS     TO DCL-KYC-STATUS
           MOVE CV-RISK-SANCTION-FLG   TO DCL-SANCTION-FLG
           MOVE SC-SCORED-BY           TO DCL-SCORED-BY
           MOVE CV-RISK-CHANNEL        TO DCL-SOURCE-CHANNEL
      *
           IF CV-RISK-REASON-CD = SPACES
               MOVE SPACES             TO DCL-REASON-CD
               MOVE -1                 TO IND-REASON-CD
           ELSE
               MOVE CV-RISK-REASON-CD  TO DCL-REASON-CD
               MOVE ZERO               TO IND-REASON-CD
           END-IF
      *
           EXEC SQL
               INSERT INTO PARTYRSK.PARTY_RISK_SCORE
                     (PARTY_ID
                    , SCORE_TS
                    , MODEL_ID
                    , MODEL_VERSION
                    , RISK_SCORE
                    , RISK_BAND
                    , PD_PCT
                    , EXPOSURE_AMT
                    , ADVICE_CD
                    , REASON_CD
                    , KYC_STATUS
                    , SANCTION_FLG
                    , COMPONENT_DATA
                    , OVERRIDE_FLG
                    , SCORED_BY
                    , SOURCE_CHANNEL)
               VALUES (:DCL-PARTY-ID
                    , :DCL-SCORE-TS
                    , :DCL-MODEL-ID
                    , :DCL-MODEL-VERSION
                    , :DCL-RISK-SCORE
                    , :DCL-RISK-BAND
                    , :DCL-PD-PCT
                    , :DCL-EXPOSURE-AMT
                    , :DCL-ADVICE-CD
                    , :DCL-REASON-CD :IND-REASON-CD
                    , :DCL-KYC-STATUS
                    , :DCL-SANCTION-FLG
                    , :DCL-COMP-DATA
                    , 'N'
                    , :DCL-SCORED-BY
                    , :DCL-SOURCE-CHANNEL)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
      *            THE SAME PARTY HAS ALREADY BEEN SCORED THIS
      *            HUNDREDTH OF A SECOND.  THE EARLIER ROW STANDS.
                   MOVE 4              TO WS-OWN-RC
                   DISPLAY 'PRBRSK5  DUPLICATE SCORE SUPPRESSED PARTY='
                           DCL-PARTY-ID ' TS=' DCL-SCORE-TS
               WHEN OTHER
                   MOVE 'PARTY_RISK_SCORE ' TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - EXPOSURE ROWS                                           *
      *                                                                *
      * ONE ROW PER PRODUCT SYSTEM AND PRODUCT.  UPDATE FIRST, INSERT  *
      * WHEN THE UPDATE FINDS NOTHING - THE TABLE IS KEYED ON THE AS   *
      * OF DATE SO A SECOND RUN ON THE SAME DAY REPLACES THE FIRST.    *
      ******************************************************************
       3000-WRITE-EXPOSURE.
           MOVE '3000-WRITE-EXPOSURE'  TO WS-PARAGRAPH
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > EX-PROD-CNT
                        OR WS-FATAL
               IF EX-PROD-SYSTEM(WS-SUB) NOT = SPACES
                   PERFORM 3100-SET-EXPOSURE-HOST
                   PERFORM 3200-UPDATE-EXPOSURE
                   IF SQLCODE = +100
                       PERFORM 3300-INSERT-EXPOSURE
                   END-IF
               END-IF
           END-PERFORM
      *
           DISPLAY 'PRBRSK5  EXPOSURE PARTY=' EX-PARTY-ID
                   ' UPDATED=' WS-EXPO-UPD
                   ' INSERTED=' WS-EXPO-INS
                   ' STALE=' EX-STALE-FLG
           .
      *
       3100-SET-EXPOSURE-HOST.
           MOVE EX-PROD-SYSTEM(WS-SUB) TO DCL-PROD-SYSTEM
           MOVE EX-PROD-CODE(WS-SUB)   TO DCL-PRODUCT-CD
           MOVE EX-CURRENCY            TO DCL-CURRENCY-CD
           MOVE EX-PROD-ACCT-CNT(WS-SUB) TO DCL-ACCT-CNT
           MOVE EX-PROD-LIMIT(WS-SUB)  TO DCL-TOTAL-LIMIT
           MOVE EX-PROD-DRAWN(WS-SUB)  TO DCL-TOTAL-DRAWN
           MOVE EX-PROD-DELQ-BUCKET(WS-SUB) TO DCL-DELQ-BUCKET
           MOVE EX-PAST-DUE-AMT        TO DCL-PAST-DUE-AMT
           MOVE EX-WRITTEN-OFF-AMT     TO DCL-WRITTEN-OFF-AMT
           MOVE EX-SECURED-AMT         TO DCL-SECURED-AMT
           MOVE EX-UTILISATION-PCT     TO DCL-UTIL-PCT
           MOVE EX-STALE-FLG           TO DCL-STALE-FLG
           MOVE EX-CALC-PGM            TO DCL-CALC-PGM
      *
           COMPUTE DCL-TOTAL-AVAIL =
                   DCL-TOTAL-LIMIT - DCL-TOTAL-DRAWN
           IF DCL-TOTAL-AVAIL < ZERO
               MOVE ZERO               TO DCL-TOTAL-AVAIL
           END-IF
      *
           COMPUTE DCL-UNSECURED-AMT =
                   DCL-TOTAL-DRAWN - DCL-SECURED-AMT
           IF DCL-UNSECURED-AMT < ZERO
               MOVE ZERO               TO DCL-UNSECURED-AMT
           END-IF
           .
      *
       3200-UPDATE-EXPOSURE.
           MOVE '3200-UPDATE-EXPOSURE' TO WS-PARAGRAPH
      *
           EXEC SQL
               UPDATE PARTYRSK.PARTY_EXPOSURE
                  SET ACCT_CNT        = :DCL-ACCT-CNT
                    , TOTAL_LIMIT     = :DCL-TOTAL-LIMIT
                    , TOTAL_DRAWN     = :DCL-TOTAL-DRAWN
                    , TOTAL_AVAILABLE = :DCL-TOTAL-AVAIL
                    , UNSECURED_AMT   = :DCL-UNSECURED-AMT
                    , SECURED_AMT     = :DCL-SECURED-AMT
                    , PAST_DUE_AMT    = :DCL-PAST-DUE-AMT
                    , WRITTEN_OFF_AMT = :DCL-WRITTEN-OFF-AMT
                    , DELQ_BUCKET     = :DCL-DELQ-BUCKET
                    , UTILISATION_PCT = :DCL-UTIL-PCT
                    , STALE_FLG       = :DCL-STALE-FLG
                    , CALC_PGM        = :DCL-CALC-PGM
                    , CALC_TS         = CURRENT TIMESTAMP
                WHERE PARTY_ID    = :DCL-PARTY-ID
                  AND AS_OF_DATE  = :DCL-AS-OF-DATE
                  AND PROD_SYSTEM = :DCL-PROD-SYSTEM
                  AND PRODUCT_CD  = :DCL-PRODUCT-CD
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-EXPO-UPD
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE 'PARTY_EXPOSURE   ' TO ER-SQL-TABLE
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       3300-INSERT-EXPOSURE.
           MOVE '3300-INSERT-EXPOSURE' TO WS-PARAGRAPH
      *
           EXEC SQL
               INSERT INTO PARTYRSK.PARTY_EXPOSURE
                     (PARTY_ID
                    , AS_OF_DATE
                    , PROD_SYSTEM
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
                    , CALC_PGM)
               VALUES (:DCL-PARTY-ID
                    , :DCL-AS-OF-DATE
                    , :DCL-PROD-SYSTEM
                    , :DCL-PRODUCT-CD
                    , :DCL-CURRENCY-CD
                    , :DCL-ACCT-CNT
                    , :DCL-TOTAL-LIMIT
                    , :DCL-TOTAL-DRAWN
                    , :DCL-TOTAL-AVAIL
                    , :DCL-UNSECURED-AMT
                    , :DCL-SECURED-AMT
                    , :DCL-PAST-DUE-AMT
                    , :DCL-WRITTEN-OFF-AMT
                    , :DCL-DELQ-BUCKET
                    , :DCL-UTIL-PCT
                    , :DCL-STALE-FLG
                    , :DCL-CALC-PGM)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-EXPO-INS
               WHEN -803
      *            ANOTHER TASK GOT THERE FIRST.  RE-DRIVE THE UPDATE.
                   PERFORM 3200-UPDATE-EXPOSURE
               WHEN OTHER
                   MOVE 'PARTY_EXPOSURE   ' TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 4000 - AUDIT TRAIL                                             *
      *                                                                *
      * APPEND ONLY.  THE HOP TRACE IS CARRIED SO THAT A REGULATOR     *
      * CAN SEE WHICH PROGRAMS TOUCHED THE DECISION.                   *
      ******************************************************************
       4000-WRITE-AUDIT.
           MOVE '4000-WRITE-AUDIT'     TO WS-PARAGRAPH
           ADD 1                       TO WS-AUDIT-SEQ
      *
           MOVE SPACES                 TO DCL-AUDIT-ID
           STRING CV-RISK-CORREL-ID    DELIMITED BY SIZE
                  WS-AUDIT-SEQ         DELIMITED BY SIZE
                  INTO DCL-AUDIT-ID
      *
           MOVE 'RCAL'                 TO DCL-EVENT-TYPE
           MOVE WS-MODULE-PARTYRSK     TO DCL-REQ-MODULE
           MOVE CV-RISK-CALLER-ID      TO DCL-REQ-PGM
           MOVE CV-RISK-CORREL-ID      TO DCL-CORREL-ID
           MOVE CV-RISK-CHANNEL        TO DCL-CHANNEL
           MOVE SC-SCORE               TO DCL-NEW-SCORE
           MOVE SC-BAND                TO DCL-NEW-BAND
           MOVE CV-RISK-REASON-TXT     TO DCL-REASON-TXT
      *
           EXEC SQL
               INSERT INTO PARTYRSK.PARTY_RISK_AUDIT
                     (AUDIT_ID
                    , PARTY_ID
                    , EVENT_TYPE
                    , REQUESTING_MODULE
                    , REQUESTING_PGM
                    , CORREL_ID
                    , CHANNEL
                    , OLD_SCORE
                    , NEW_SCORE
                    , OLD_BAND
                    , NEW_BAND
                    , KYC_STATUS
                    , SANCTION_FLG
                    , ADVICE_CD
                    , REASON_TXT
                    , HOP_TRACE)
               VALUES (:DCL-AUDIT-ID
                    , :DCL-PARTY-ID
                    , :DCL-EVENT-TYPE
                    , :DCL-REQ-MODULE
                    , :DCL-REQ-PGM
                    , :DCL-CORREL-ID
                    , :DCL-CHANNEL
                    , :DCL-OLD-SCORE :IND-OLD-SCORE
                    , :DCL-NEW-SCORE
                    , :DCL-OLD-BAND  :IND-OLD-BAND
                    , :DCL-NEW-BAND
                    , :DCL-KYC-STATUS
                    , :DCL-SANCTION-FLG
                    , :DCL-ADVICE-CD
                    , :DCL-REASON-TXT
                    , :DCL-HOP-TRACE)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
      *            THE AUDIT ID IS THE CORRELATION ID PLUS A SEQUENCE.
      *            A CLASH MEANS THE CALLER REUSED THE CORRELATION ID.
                   ADD 1               TO WS-AUDIT-SEQ
                   MOVE 4              TO WS-OWN-RC
                   DISPLAY 'PRBRSK5  DUPLICATE AUDIT ID='
                           DCL-AUDIT-ID ' PARTY=' DCL-PARTY-ID
               WHEN OTHER
                   MOVE 'PARTY_RISK_AUDIT ' TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
      *            THE SCORE ROW IS ALREADY ON THE UNIT OF WORK.
      *            LETTING THE STEP RUN ON WOULD LEAVE A SCORE WITH
      *            NO AUDIT TRAIL BEHIND IT.
                   PERFORM 9900-ABEND
           END-EVALUATE
      *
           IF WS-OLD-FOUND
              AND DCL-OLD-BAND NOT = DCL-NEW-BAND
               DISPLAY 'PRBRSK5  BAND CHANGE PARTY=' DCL-PARTY-ID
                       ' FROM=' DCL-OLD-BAND
                       ' TO=' DCL-NEW-BAND
                       ' SCORE=' DCL-OLD-SCORE '>' DCL-NEW-SCORE
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
           MOVE 'RISK RESULTS NOT WRITTEN - UNIT OF WORK BACKED OUT'
                                       TO CV-RISK-REASON-TXT
           MOVE 12                     TO WS-OWN-RC
           MOVE 'Y'                    TO WS-FATAL-SW
      *
           DISPLAY 'PRBRSK5  SQL ERROR PARA=' WS-PARAGRAPH
           DISPLAY '         TABLE=' ER-SQL-TABLE
                   ' OP=' ER-SQL-OPERATION
                   ' SQLCODE=' WS-SQL-DISP
           DISPLAY '         PARTY=' DCL-PARTY-ID
                   ' CORREL=' CV-RISK-CORREL-ID
           DISPLAY '         SQLERRMC=' SQLERRMC(1:44)
           .
      *
      ******************************************************************
      * 9900 - U3105.  RESERVED FOR A WRITE FAILURE THE CALLER CANNOT  *
      *        BACK OUT FROM.                                          *
      ******************************************************************
       9900-ABEND.
           MOVE 'U310'                 TO ER-ABEND-CODE
           MOVE 'Y'                    TO ER-ABEND-REQUESTED
           DISPLAY 'PRBRSK5  ABEND U3105 PARTY=' DCL-PARTY-ID
           MOVE 3105                   TO WS-ABEND-CODE
           CALL 'ILBOABN0' USING WS-ABEND-CODE
           .
