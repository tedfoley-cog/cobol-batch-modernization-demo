      ******************************************************************
      * CBPRT04 - WEEKLY REGULATORY RISK AUDIT REPORT                  *
      *                                                                *
      * PART OF THE PARTYWK WEEKLY CYCLE, STEP FOUR.                   *
      *                                                                *
      * PRINTS THE WEEKLY RISK AUDIT PACK FROM PARTYRSK.PARTY_RISK_    *
      * AUDIT AND ITS SUPPORTING TABLES.  THE PACK GOES TO THE         *
      * FINANCIAL CRIME COMMITTEE AND IS RETAINED FOR SEVEN YEARS,     *
      * SO THE HEADINGS AND THE COLUMN ORDER ARE FIXED BY THE 2011     *
      * SUBMISSION STANDARD AND MUST NOT BE REARRANGED.                *
      *                                                                *
      * SECTIONS                                                       *
      *   1  BAND MOVEMENT BY REQUESTING MODULE, CONTROL BREAK ON      *
      *      MODULE AND ON EVENT TYPE                                  *
      *   2  BANDED SCORE DISTRIBUTION                                 *
      *   3  SANCTIONS HIT SUMMARY                                     *
      *   4  OVERDUE KYC EXCEPTIONS                                    *
      *                                                                *
      * THE REPORT COVERS THE SEVEN DAYS UP TO AND INCLUDING THE RUN   *
      * DATE.  A RERUN PRINTS THE SAME NUMBERS.                        *
      *                                                                *
      * RUN BY     - CBPRT04J STEP RPTAUD                              *
      * FILES      - RISKRPT   REPORT, FBA 133                         *
      * TABLES     - PARTYRSK.PARTY_RISK_AUDIT   SELECT                *
      *              PARTYRSK.PARTY_RISK_SCORE   SELECT                *
      *              PARTYRSK.PARTY_KYC          SELECT                *
      *              PARTYRSK.CUSTOMER           SELECT                *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - REPORT PRINTED                                          *
      *   04 - REPORT PRINTED, NO AUDIT ACTIVITY IN THE PERIOD         *
      *   12 - FAILED                                                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBPRT04.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RISKRPT-FILE ASSIGN TO RISKRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-RPT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  RISKRPT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  RISKRPT-REC                 PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CBPRT04 '.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
      *
       01  WS-RPT-STATUS               PIC X(2)  VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X     VALUE 'N'.
               88  WS-EOF                        VALUE 'Y'.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
           05  WS-FIRST-SW             PIC X     VALUE 'Y'.
               88  WS-FIRST-ROW                  VALUE 'Y'.
      *
       01  WS-PAGE-CONTROL.
           05  WS-PAGE-NO              PIC 9(4)  VALUE ZERO.
           05  WS-LINE-NO              PIC 9(2)  VALUE 99.
           05  WS-LINES-PER-PAGE       PIC 9(2)  VALUE 55.
           05  WS-SECTION-TITLE        PIC X(40) VALUE SPACES.
      *
       01  WS-BREAK-KEYS.
           05  WS-CURR-MODULE          PIC X(8)  VALUE SPACES.
           05  WS-CURR-EVENT           PIC X(4)  VALUE SPACES.
           05  WS-PREV-MODULE          PIC X(8)  VALUE SPACES.
           05  WS-PREV-EVENT           PIC X(4)  VALUE SPACES.
      *
       01  WS-COUNTERS.
           05  WS-ROW-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-EVENT-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-MODULE-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-GRAND-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-EVENT-UP             PIC 9(9)  VALUE ZERO.
           05  WS-EVENT-DOWN           PIC 9(9)  VALUE ZERO.
           05  WS-MODULE-UP            PIC 9(9)  VALUE ZERO.
           05  WS-MODULE-DOWN          PIC 9(9)  VALUE ZERO.
           05  WS-GRAND-UP             PIC 9(9)  VALUE ZERO.
           05  WS-GRAND-DOWN           PIC 9(9)  VALUE ZERO.
           05  WS-SUB                  PIC S9(4) COMP VALUE ZERO.
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-DATE-R REDEFINES WS-CURR-DATE.
               10  WS-CURR-CCYY        PIC 9(4).
               10  WS-CURR-MM          PIC 9(2).
               10  WS-CURR-DD          PIC 9(2).
           05  WS-CURR-TIME            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME-R REDEFINES WS-CURR-TIME.
               10  WS-CURR-HH          PIC 9(2).
               10  WS-CURR-MI          PIC 9(2).
               10  WS-CURR-SS          PIC 9(2).
               10  WS-CURR-HUND        PIC 9(2).
           05  WS-RUN-DATE-ED          PIC X(10) VALUE SPACES.
      *
      ******************************************************************
      * REPORT LINES.  FIRST BYTE IS THE ASA CARRIAGE CONTROL.         *
      ******************************************************************
       01  WS-BLANK-LINE               PIC X(133) VALUE SPACES.
      *
       01  WS-HEAD-1.
           05  FILLER                  PIC X     VALUE '1'.
           05  FILLER                  PIC X(8)  VALUE 'CBPRT04 '.
           05  FILLER                  PIC X(6)  VALUE SPACES.
           05  FILLER                  PIC X(46) VALUE
               'PARTY RISK - WEEKLY REGULATORY AUDIT PACK     '.
           05  FILLER                  PIC X(12) VALUE 'RUN DATE  '.
           05  H1-RUN-DATE             PIC X(10).
           05  FILLER                  PIC X(8)  VALUE '   PAGE '.
           05  H1-PAGE-NO              PIC ZZZ9.
           05  FILLER                  PIC X(38) VALUE SPACES.
      *
       01  WS-HEAD-2.
           05  FILLER                  PIC X     VALUE ' '.
           05  FILLER                  PIC X(14) VALUE
               'PERIOD ENDING '.
           05  H2-PERIOD-END           PIC X(10).
           05  FILLER                  PIC X(14) VALUE
               '   SEVEN DAYS '.
           05  FILLER                  PIC X(10) VALUE 'AS AT     '.
           05  H2-RUN-TIME             PIC X(8).
           05  FILLER                  PIC X(76) VALUE SPACES.
      *
       01  WS-HEAD-3.
           05  FILLER                  PIC X     VALUE ' '.
           05  H3-SECTION              PIC X(40).
           05  FILLER                  PIC X(92) VALUE SPACES.
      *
       01  WS-RULE-LINE.
           05  FILLER                  PIC X     VALUE ' '.
           05  FILLER                  PIC X(132) VALUE ALL '-'.
      *
       01  WS-COL-HEAD-1.
           05  FILLER                  PIC X     VALUE ' '.
           05  FILLER                  PIC X(10) VALUE 'MODULE    '.
           05  FILLER                  PIC X(7)  VALUE 'EVENT  '.
           05  FILLER                  PIC X(13) VALUE 'PARTY        '.
           05  FILLER                  PIC X(21) VALUE
               'EVENT TIMESTAMP      '.
           05  FILLER                  PIC X(12) VALUE 'OLD    NEW  '.
           05  FILLER                  PIC X(12) VALUE 'BAND MOVE   '.
           05  FILLER                  PIC X(6)  VALUE 'SANC  '.
           05  FILLER                  PIC X(6)  VALUE 'KYC   '.
           05  FILLER                  PIC X(45) VALUE
               'REQUESTING PGM  ADVICE                       '.
      *
       01  WS-DETAIL-LINE.
           05  FILLER                  PIC X     VALUE ' '.
           05  DL-MODULE               PIC X(8).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-EVENT                PIC X(4).
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  DL-PARTY-ID             PIC X(11).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-EVENT-TS             PIC X(19).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-OLD-SCORE            PIC ZZ9.
           05  FILLER                  PIC X(4)  VALUE SPACES.
           05  DL-NEW-SCORE            PIC ZZ9.
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  DL-OLD-BAND             PIC X.
           05  FILLER                  PIC X(4)  VALUE ' TO '.
           05  DL-NEW-BAND             PIC X.
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  DL-MOVE-FLAG            PIC X(4).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-SANCTION             PIC X.
           05  FILLER                  PIC X(5)  VALUE SPACES.
           05  DL-KYC                  PIC X(2).
           05  FILLER                  PIC X(4)  VALUE SPACES.
           05  DL-REQ-PGM              PIC X(8).
           05  FILLER                  PIC X(4)  VALUE SPACES.
           05  DL-ADVICE               PIC X(4).
           05  FILLER                  PIC X(25) VALUE SPACES.
      *
       01  WS-BREAK-LINE.
           05  FILLER                  PIC X     VALUE ' '.
           05  BL-LABEL                PIC X(34).
           05  BL-COUNT                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(12) VALUE
               '   UPGRADED '.
           05  BL-UP                   PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(14) VALUE
               '   DOWNGRADED '.
           05  BL-DOWN                 PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(49) VALUE SPACES.
      *
       01  WS-BAND-HEAD.
           05  FILLER                  PIC X     VALUE ' '.
           05  FILLER                  PIC X(12) VALUE 'SCORE BAND  '.
           05  FILLER                  PIC X(14) VALUE 'RANGE         '.
           05  FILLER                  PIC X(14) VALUE 'PARTIES       '.
           05  FILLER                  PIC X(12) VALUE 'PERCENT     '.
           05  FILLER                  PIC X(20) VALUE
               'AVERAGE SCORE       '.
           05  FILLER                  PIC X(60) VALUE SPACES.
      *
       01  WS-BAND-LINE.
           05  FILLER                  PIC X     VALUE ' '.
           05  BD-BAND                 PIC X.
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  BD-DESC                 PIC X(20).
           05  BD-RANGE                PIC X(12).
           05  BD-COUNT                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(4)  VALUE SPACES.
           05  BD-PERCENT              PIC ZZ9.99.
           05  FILLER                  PIC X(8)  VALUE SPACES.
           05  BD-AVG-SCORE            PIC ZZ9.99.
           05  FILLER                  PIC X(60) VALUE SPACES.
      *
       01  WS-SANC-HEAD.
           05  FILLER                  PIC X     VALUE ' '.
           05  FILLER                  PIC X(14) VALUE 'PARTY         '.
           05  FILLER                  PIC X(32) VALUE
               'LEGAL NAME                      '.
           05  FILLER                  PIC X(12) VALUE 'HITS        '.
           05  FILLER                  PIC X(22) VALUE
               'LATEST HIT            '.
           05  FILLER                  PIC X(10) VALUE 'BAND      '.
           05  FILLER                  PIC X(42) VALUE SPACES.
      *
       01  WS-SANC-LINE.
           05  FILLER                  PIC X     VALUE ' '.
           05  SL-PARTY-ID             PIC X(11).
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  SL-LEGAL-NAME           PIC X(30).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  SL-HIT-CNT              PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(5)  VALUE SPACES.
           05  SL-LAST-HIT             PIC X(19).
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  SL-BAND                 PIC X.
           05  FILLER                  PIC X(58) VALUE SPACES.
      *
       01  WS-KYC-HEAD.
           05  FILLER                  PIC X     VALUE ' '.
           05  FILLER                  PIC X(14) VALUE 'PARTY         '.
           05  FILLER                  PIC X(32) VALUE
               'LEGAL NAME                      '.
           05  FILLER                  PIC X(8)  VALUE 'STATUS  '.
           05  FILLER                  PIC X(8)  VALUE 'LEVEL   '.
           05  FILLER                  PIC X(14) VALUE 'DUE DATE      '.
           05  FILLER                  PIC X(12) VALUE 'DAYS LATE   '.
           05  FILLER                  PIC X(44) VALUE SPACES.
      *
       01  WS-KYC-LINE.
           05  FILLER                  PIC X     VALUE ' '.
           05  KL-PARTY-ID             PIC X(11).
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  KL-LEGAL-NAME           PIC X(30).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  KL-STATUS               PIC X(2).
           05  FILLER                  PIC X(6)  VALUE SPACES.
           05  KL-LEVEL                PIC X(4).
           05  FILLER                  PIC X(4)  VALUE SPACES.
           05  KL-DUE-DATE             PIC X(10).
           05  FILLER                  PIC X(4)  VALUE SPACES.
           05  KL-DAYS-LATE            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(50) VALUE SPACES.
      *
       01  WS-TOTAL-LINE.
           05  FILLER                  PIC X     VALUE ' '.
           05  TL-LABEL                PIC X(40).
           05  TL-VALUE                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(82) VALUE SPACES.
      *
       01  WS-NONE-LINE.
           05  FILLER                  PIC X     VALUE ' '.
           05  FILLER                  PIC X(40) VALUE
               '*** NO ENTRIES IN THIS SECTION ***      '.
           05  FILLER                  PIC X(92) VALUE SPACES.
      *
       01  WS-WORK-FIELDS.
           05  WS-PCT-WORK             PIC S9(5)V99 COMP-3 VALUE ZERO.
           05  WS-SQL-DISP             PIC -(9)9.
           05  WS-SECTION-ROWS         PIC 9(9)  VALUE ZERO.
      *
      *    HOST VARIABLES
       01  DCL-AUDIT.
           05  DCL-REQ-MODULE          PIC X(8).
           05  DCL-EVENT-TYPE          PIC X(4).
           05  DCL-PARTY-ID            PIC X(11).
           05  DCL-EVENT-TS            PIC X(26).
           05  DCL-OLD-SCORE           PIC S9(4) COMP.
           05  DCL-NEW-SCORE           PIC S9(4) COMP.
           05  DCL-OLD-BAND            PIC X(1).
           05  DCL-NEW-BAND            PIC X(1).
           05  DCL-SANCTION-FLG        PIC X(1).
           05  DCL-KYC-STATUS          PIC X(2).
           05  DCL-REQ-PGM             PIC X(8).
           05  DCL-ADVICE-CD           PIC X(4).
      *
       01  DCL-BAND.
           05  DCL-BD-BAND             PIC X(1).
           05  DCL-BD-COUNT            PIC S9(9) COMP.
           05  DCL-BD-AVG              PIC S9(5)V99 COMP-3.
           05  DCL-BD-MIN              PIC S9(4) COMP.
           05  DCL-BD-MAX              PIC S9(4) COMP.
           05  DCL-TOTAL-SCORED        PIC S9(9) COMP.
      *
       01  DCL-SANC.
           05  DCL-SN-PARTY            PIC X(11).
           05  DCL-SN-NAME             PIC X(60).
           05  DCL-SN-HITS             PIC S9(9) COMP.
           05  DCL-SN-LAST             PIC X(26).
           05  DCL-SN-BAND             PIC X(1).
      *
       01  DCL-KYC.
           05  DCL-KY-PARTY            PIC X(11).
           05  DCL-KY-NAME             PIC X(60).
           05  DCL-KY-STATUS           PIC X(2).
           05  DCL-KY-LEVEL            PIC X(4).
           05  DCL-KY-DUE              PIC X(10).
           05  DCL-KY-LATE             PIC S9(9) COMP.
      *
       01  DCL-IND.
           05  IND-OLD-SCORE           PIC S9(4) COMP.
           05  IND-NEW-SCORE           PIC S9(4) COMP.
           05  IND-OLD-BAND            PIC S9(4) COMP.
           05  IND-NEW-BAND            PIC S9(4) COMP.
           05  IND-SANCTION            PIC S9(4) COMP.
           05  IND-KYC-STATUS          PIC S9(4) COMP.
           05  IND-ADVICE              PIC S9(4) COMP.
           05  IND-NAME                PIC S9(4) COMP.
           05  IND-BD-AVG              PIC S9(4) COMP.
           05  IND-SN-BAND             PIC S9(4) COMP.
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      ******************************************************************
      * SECTION 1 - AUDIT ACTIVITY, ORDERED FOR THE CONTROL BREAKS.    *
      ******************************************************************
           EXEC SQL DECLARE AUDITCSR CURSOR FOR
               SELECT REQUESTING_MODULE
                    , EVENT_TYPE
                    , PARTY_ID
                    , CHAR(EVENT_TS)
                    , OLD_SCORE
                    , NEW_SCORE
                    , OLD_BAND
                    , NEW_BAND
                    , SANCTION_FLG
                    , KYC_STATUS
                    , REQUESTING_PGM
                    , ADVICE_CD
                 FROM PARTYRSK.PARTY_RISK_AUDIT
                WHERE DATE(EVENT_TS)
                      BETWEEN CURRENT DATE - 6 DAYS
                          AND CURRENT DATE
                ORDER BY REQUESTING_MODULE
                       , EVENT_TYPE
                       , EVENT_TS
                 WITH UR
           END-EXEC.
      *
      ******************************************************************
      * SECTION 2 - BANDED DISTRIBUTION OF THE LATEST SCORE PER PARTY. *
      ******************************************************************
           EXEC SQL DECLARE BANDCSR CURSOR FOR
               SELECT RISK_BAND
                    , COUNT(*)
                    , AVG(DECIMAL(RISK_SCORE,7,2))
                    , MIN(RISK_SCORE)
                    , MAX(RISK_SCORE)
                 FROM PARTYRSK.PARTY_RISK_SCORE S
                WHERE S.SCORE_TS =
                     (SELECT MAX(S2.SCORE_TS)
                        FROM PARTYRSK.PARTY_RISK_SCORE S2
                       WHERE S2.PARTY_ID = S.PARTY_ID)
                GROUP BY RISK_BAND
                ORDER BY RISK_BAND
                 WITH UR
           END-EXEC.
      *
      ******************************************************************
      * SECTION 3 - SANCTIONS HITS RAISED IN THE PERIOD.               *
      ******************************************************************
           EXEC SQL DECLARE SANCCSR CURSOR FOR
               SELECT A.PARTY_ID
                    , MAX(C.LEGAL_NAME)
                    , COUNT(*)
                    , CHAR(MAX(A.EVENT_TS))
                    , MAX(A.NEW_BAND)
                 FROM PARTYRSK.PARTY_RISK_AUDIT A
                    , PARTYRSK.CUSTOMER C
                WHERE A.PARTY_ID = C.PARTY_ID
                  AND A.SANCTION_FLG = 'Y'
                  AND DATE(A.EVENT_TS)
                      BETWEEN CURRENT DATE - 6 DAYS
                          AND CURRENT DATE
                GROUP BY A.PARTY_ID
                ORDER BY 3 DESC, 1
                 WITH UR
           END-EXEC.
      *
      ******************************************************************
      * SECTION 4 - KYC REVIEWS STILL OUTSTANDING.                     *
      ******************************************************************
           EXEC SQL DECLARE KYCCSR CURSOR FOR
               SELECT K.PARTY_ID
                    , C.LEGAL_NAME
                    , K.KYC_STATUS
                    , K.KYC_LEVEL
                    , CHAR(K.NEXT_REVIEW_DATE, ISO)
                    , DAYS(CURRENT DATE) - DAYS(K.NEXT_REVIEW_DATE)
                 FROM PARTYRSK.PARTY_KYC K
                    , PARTYRSK.CUSTOMER C
                WHERE K.PARTY_ID = C.PARTY_ID
                  AND C.CUST_STATUS = 'A'
                  AND K.NEXT_REVIEW_DATE < CURRENT DATE
                  AND K.KYC_STATUS IN ('PN','EX','FL')
                  AND K.KYC_SEQ =
                     (SELECT MAX(K2.KYC_SEQ)
                        FROM PARTYRSK.PARTY_KYC K2
                       WHERE K2.PARTY_ID = K.PARTY_ID)
                ORDER BY 6 DESC
                FETCH FIRST 500 ROWS ONLY
                 WITH UR
           END-EXEC.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
           PERFORM 1000-SECTION-ACTIVITY
           PERFORM 2000-SECTION-DISTRIBUTION
           PERFORM 3000-SECTION-SANCTIONS
           PERFORM 4000-SECTION-KYC
           PERFORM 8000-CLOSE-DOWN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK
           .
      *
       0100-INITIALISE.
           MOVE '0100-INITIALISE'      TO WS-PARAGRAPH
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           ACCEPT WS-CURR-TIME         FROM TIME
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
      *
           STRING WS-CURR-CCYY '-' WS-CURR-MM '-' WS-CURR-DD
                  DELIMITED BY SIZE INTO WS-RUN-DATE-ED
           MOVE WS-RUN-DATE-ED         TO H1-RUN-DATE
           MOVE WS-RUN-DATE-ED         TO H2-PERIOD-END
           STRING WS-CURR-HH ':' WS-CURR-MI ':' WS-CURR-SS
                  DELIMITED BY SIZE INTO H2-RUN-TIME
      *
           OPEN OUTPUT RISKRPT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'RISKRPT '         TO ER-FILE-NAME
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           DISPLAY 'CBPRT04  AUDIT REPORT STARTED DATE=' WS-CURR-DATE
           .
      *
      ******************************************************************
      * 1000 - SECTION 1, ACTIVITY WITH CONTROL BREAKS                 *
      ******************************************************************
       1000-SECTION-ACTIVITY.
           MOVE '1000-SECTION-ACTIVITY' TO WS-PARAGRAPH
           MOVE 'SECTION 1 - RISK EVENT ACTIVITY'
                                       TO WS-SECTION-TITLE
           MOVE 99                     TO WS-LINE-NO
           MOVE ZERO                   TO WS-SECTION-ROWS
           MOVE 'N'                    TO WS-EOF-SW
           MOVE 'Y'                    TO WS-FIRST-SW
      *
           EXEC SQL
               OPEN AUDITCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_RISK_AUDIT ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           PERFORM 1100-FETCH-AUDIT
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               IF WS-FIRST-ROW
                   PERFORM 1200-START-BREAKS
                   MOVE 'N'            TO WS-FIRST-SW
               ELSE
                   IF DCL-REQ-MODULE NOT = WS-PREV-MODULE
                       PERFORM 1500-BREAK-EVENT
                       PERFORM 1600-BREAK-MODULE
                       PERFORM 1200-START-BREAKS
                   ELSE
                       IF DCL-EVENT-TYPE NOT = WS-PREV-EVENT
                           PERFORM 1500-BREAK-EVENT
                           MOVE DCL-EVENT-TYPE TO WS-PREV-EVENT
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 1300-PRINT-DETAIL
               PERFORM 1100-FETCH-AUDIT
           END-PERFORM
      *
           IF WS-SECTION-ROWS = ZERO
               PERFORM 1900-PRINT-NONE
           ELSE
               PERFORM 1500-BREAK-EVENT
               PERFORM 1600-BREAK-MODULE
               PERFORM 1700-BREAK-GRAND
           END-IF
      *
           EXEC SQL
               CLOSE AUDITCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_RISK_AUDIT ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *
       1100-FETCH-AUDIT.
           EXEC SQL
               FETCH AUDITCSR
                INTO :DCL-REQ-MODULE
                   , :DCL-EVENT-TYPE
                   , :DCL-PARTY-ID
                   , :DCL-EVENT-TS
                   , :DCL-OLD-SCORE   :IND-OLD-SCORE
                   , :DCL-NEW-SCORE   :IND-NEW-SCORE
                   , :DCL-OLD-BAND    :IND-OLD-BAND
                   , :DCL-NEW-BAND    :IND-NEW-BAND
                   , :DCL-SANCTION-FLG :IND-SANCTION
                   , :DCL-KYC-STATUS  :IND-KYC-STATUS
                   , :DCL-REQ-PGM
                   , :DCL-ADVICE-CD   :IND-ADVICE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-SECTION-ROWS
               WHEN +100
                   MOVE 'Y'            TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'PARTY_RISK_AUDIT ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       1200-START-BREAKS.
           MOVE DCL-REQ-MODULE         TO WS-PREV-MODULE
           MOVE DCL-EVENT-TYPE         TO WS-PREV-EVENT
           MOVE ZERO                   TO WS-EVENT-CNT
           MOVE ZERO                   TO WS-EVENT-UP
           MOVE ZERO                   TO WS-EVENT-DOWN
           MOVE ZERO                   TO WS-MODULE-CNT
           MOVE ZERO                   TO WS-MODULE-UP
           MOVE ZERO                   TO WS-MODULE-DOWN
           .
      *
       1300-PRINT-DETAIL.
           IF WS-LINE-NO > WS-LINES-PER-PAGE
               PERFORM 7000-PAGE-HEAD
               MOVE WS-COL-HEAD-1      TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
               MOVE WS-RULE-LINE       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
           END-IF
      *
           MOVE SPACES                 TO WS-DETAIL-LINE
           MOVE DCL-REQ-MODULE         TO DL-MODULE
           MOVE DCL-EVENT-TYPE         TO DL-EVENT
           MOVE DCL-PARTY-ID           TO DL-PARTY-ID
           MOVE DCL-EVENT-TS(1:19)     TO DL-EVENT-TS
      *
           IF IND-OLD-SCORE < ZERO
               MOVE ZERO               TO DL-OLD-SCORE
           ELSE
               MOVE DCL-OLD-SCORE      TO DL-OLD-SCORE
           END-IF
           IF IND-NEW-SCORE < ZERO
               MOVE ZERO               TO DL-NEW-SCORE
           ELSE
               MOVE DCL-NEW-SCORE      TO DL-NEW-SCORE
           END-IF
      *
           IF IND-OLD-BAND < ZERO
               MOVE '-'                TO DL-OLD-BAND
           ELSE
               MOVE DCL-OLD-BAND       TO DL-OLD-BAND
           END-IF
           IF IND-NEW-BAND < ZERO
               MOVE '-'                TO DL-NEW-BAND
           ELSE
               MOVE DCL-NEW-BAND       TO DL-NEW-BAND
           END-IF
      *
           PERFORM 1400-CLASSIFY-MOVE
      *
           IF IND-SANCTION < ZERO
               MOVE '-'                TO DL-SANCTION
           ELSE
               MOVE DCL-SANCTION-FLG   TO DL-SANCTION
           END-IF
           IF IND-KYC-STATUS < ZERO
               MOVE '--'               TO DL-KYC
           ELSE
               MOVE DCL-KYC-STATUS     TO DL-KYC
           END-IF
           MOVE DCL-REQ-PGM            TO DL-REQ-PGM
           IF IND-ADVICE < ZERO
               MOVE SPACES             TO DL-ADVICE
           ELSE
               MOVE DCL-ADVICE-CD      TO DL-ADVICE
           END-IF
      *
           MOVE WS-DETAIL-LINE         TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
      *
           ADD 1                       TO WS-EVENT-CNT
           ADD 1                       TO WS-MODULE-CNT
           ADD 1                       TO WS-GRAND-CNT
           .
      *
      ******************************************************************
      * 1400 - BAND MOVEMENT.  A HIGHER BAND LETTER IS A WORSE RISK,   *
      *        SO C TO B IS AN UPGRADE AND B TO C IS A DOWNGRADE.      *
      ******************************************************************
       1400-CLASSIFY-MOVE.
           EVALUATE TRUE
               WHEN IND-OLD-BAND < ZERO
                   MOVE 'NEW '         TO DL-MOVE-FLAG
               WHEN IND-NEW-BAND < ZERO
                   MOVE '    '         TO DL-MOVE-FLAG
               WHEN DCL-OLD-BAND = DCL-NEW-BAND
                   MOVE '    '         TO DL-MOVE-FLAG
               WHEN DCL-NEW-BAND > DCL-OLD-BAND
                   MOVE 'DOWN'         TO DL-MOVE-FLAG
                   ADD 1               TO WS-EVENT-DOWN
                   ADD 1               TO WS-MODULE-DOWN
                   ADD 1               TO WS-GRAND-DOWN
               WHEN OTHER
                   MOVE 'UP  '         TO DL-MOVE-FLAG
                   ADD 1               TO WS-EVENT-UP
                   ADD 1               TO WS-MODULE-UP
                   ADD 1               TO WS-GRAND-UP
           END-EVALUATE
           .
      *
       1500-BREAK-EVENT.
           MOVE SPACES                 TO WS-BREAK-LINE
           MOVE SPACES                 TO BL-LABEL
           STRING '  EVENT TOTAL ' WS-PREV-MODULE ' ' WS-PREV-EVENT
                  DELIMITED BY SIZE INTO BL-LABEL
           MOVE WS-EVENT-CNT           TO BL-COUNT
           MOVE WS-EVENT-UP            TO BL-UP
           MOVE WS-EVENT-DOWN          TO BL-DOWN
           MOVE WS-BREAK-LINE          TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
      *
           MOVE ZERO                   TO WS-EVENT-CNT
           MOVE ZERO                   TO WS-EVENT-UP
           MOVE ZERO                   TO WS-EVENT-DOWN
           .
      *
       1600-BREAK-MODULE.
           MOVE SPACES                 TO WS-BREAK-LINE
           MOVE SPACES                 TO BL-LABEL
           STRING ' MODULE TOTAL ' WS-PREV-MODULE
                  DELIMITED BY SIZE INTO BL-LABEL
           MOVE WS-MODULE-CNT          TO BL-COUNT
           MOVE WS-MODULE-UP           TO BL-UP
           MOVE WS-MODULE-DOWN         TO BL-DOWN
           MOVE WS-BREAK-LINE          TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
           MOVE WS-BLANK-LINE          TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
      *
           MOVE ZERO                   TO WS-MODULE-CNT
           MOVE ZERO                   TO WS-MODULE-UP
           MOVE ZERO                   TO WS-MODULE-DOWN
           .
      *
       1700-BREAK-GRAND.
           MOVE WS-RULE-LINE           TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
           MOVE SPACES                 TO WS-BREAK-LINE
           MOVE 'GRAND TOTAL - ALL MODULES'
                                       TO BL-LABEL
           MOVE WS-GRAND-CNT           TO BL-COUNT
           MOVE WS-GRAND-UP            TO BL-UP
           MOVE WS-GRAND-DOWN          TO BL-DOWN
           MOVE WS-BREAK-LINE          TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
           .
      *
       1900-PRINT-NONE.
           IF WS-LINE-NO > WS-LINES-PER-PAGE
               PERFORM 7000-PAGE-HEAD
           END-IF
           MOVE WS-NONE-LINE           TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
           .
      *
      ******************************************************************
      * 2000 - SECTION 2, BANDED DISTRIBUTION                          *
      ******************************************************************
       2000-SECTION-DISTRIBUTION.
           MOVE '2000-SECTION-DISTRIBUTION' TO WS-PARAGRAPH
           MOVE 'SECTION 2 - RISK BAND DISTRIBUTION'
                                       TO WS-SECTION-TITLE
           MOVE 99                     TO WS-LINE-NO
           MOVE ZERO                   TO WS-SECTION-ROWS
           MOVE 'N'                    TO WS-EOF-SW
      *
           PERFORM 2100-COUNT-SCORED
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           EXEC SQL
               OPEN BANDCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_RISK_SCORE ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               EXEC SQL
                   FETCH BANDCSR
                    INTO :DCL-BD-BAND
                       , :DCL-BD-COUNT
                       , :DCL-BD-AVG :IND-BD-AVG
                       , :DCL-BD-MIN
                       , :DCL-BD-MAX
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1           TO WS-SECTION-ROWS
                       PERFORM 2200-PRINT-BAND
                   WHEN +100
                       MOVE 'Y'        TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'PARTY_RISK_SCORE ' TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE BANDCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_RISK_SCORE ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
      *
           IF WS-SECTION-ROWS = ZERO
               PERFORM 1900-PRINT-NONE
           ELSE
               MOVE WS-RULE-LINE       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
               MOVE SPACES             TO WS-TOTAL-LINE
               MOVE 'PARTIES CARRYING A CURRENT SCORE'
                                       TO TL-LABEL
               MOVE DCL-TOTAL-SCORED   TO TL-VALUE
               MOVE WS-TOTAL-LINE      TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-COUNT-SCORED.
           EXEC SQL
               SELECT COUNT(DISTINCT PARTY_ID)
                 INTO :DCL-TOTAL-SCORED
                 FROM PARTYRSK.PARTY_RISK_SCORE
                 WITH UR
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = +100
               MOVE 'PARTY_RISK_SCORE ' TO ER-SQL-TABLE
               MOVE 'SELCOUNT'         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
           .
      *
       2200-PRINT-BAND.
           IF WS-LINE-NO > WS-LINES-PER-PAGE
               PERFORM 7000-PAGE-HEAD
               MOVE WS-BAND-HEAD       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
               MOVE WS-RULE-LINE       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
           END-IF
      *
           MOVE SPACES                 TO WS-BAND-LINE
           MOVE DCL-BD-BAND            TO BD-BAND
      *
           EVALUATE DCL-BD-BAND
               WHEN 'A'
                   MOVE 'LOW RISK'     TO BD-DESC
               WHEN 'B'
                   MOVE 'MEDIUM RISK'  TO BD-DESC
               WHEN 'C'
                   MOVE 'HIGH RISK'    TO BD-DESC
               WHEN OTHER
                   MOVE 'REFUSE / REVIEW' TO BD-DESC
           END-EVALUATE
      *
           MOVE SPACES                 TO BD-RANGE
           STRING DCL-BD-MIN ' - ' DCL-BD-MAX
                  DELIMITED BY SIZE INTO BD-RANGE
           MOVE DCL-BD-COUNT           TO BD-COUNT
      *
           IF DCL-TOTAL-SCORED > ZERO
               COMPUTE WS-PCT-WORK ROUNDED =
                       (DCL-BD-COUNT * 100) / DCL-TOTAL-SCORED
           ELSE
               MOVE ZERO               TO WS-PCT-WORK
           END-IF
           MOVE WS-PCT-WORK            TO BD-PERCENT
      *
           IF IND-BD-AVG < ZERO
               MOVE ZERO               TO BD-AVG-SCORE
           ELSE
               MOVE DCL-BD-AVG         TO BD-AVG-SCORE
           END-IF
      *
           MOVE WS-BAND-LINE           TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
           .
      *
      ******************************************************************
      * 3000 - SECTION 3, SANCTIONS HITS                               *
      ******************************************************************
       3000-SECTION-SANCTIONS.
           MOVE '3000-SECTION-SANCTIONS' TO WS-PARAGRAPH
           MOVE 'SECTION 3 - SANCTIONS HIT SUMMARY'
                                       TO WS-SECTION-TITLE
           MOVE 99                     TO WS-LINE-NO
           MOVE ZERO                   TO WS-SECTION-ROWS
           MOVE 'N'                    TO WS-EOF-SW
      *
           EXEC SQL
               OPEN SANCCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_RISK_AUDIT ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               EXEC SQL
                   FETCH SANCCSR
                    INTO :DCL-SN-PARTY
                       , :DCL-SN-NAME  :IND-NAME
                       , :DCL-SN-HITS
                       , :DCL-SN-LAST
                       , :DCL-SN-BAND  :IND-SN-BAND
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1           TO WS-SECTION-ROWS
                       PERFORM 3100-PRINT-SANCTION
                   WHEN +100
                       MOVE 'Y'        TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'PARTY_RISK_AUDIT ' TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE SANCCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_RISK_AUDIT ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
      *
           IF WS-SECTION-ROWS = ZERO
               PERFORM 1900-PRINT-NONE
           ELSE
               MOVE WS-RULE-LINE       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
               MOVE SPACES             TO WS-TOTAL-LINE
               MOVE 'PARTIES WITH A SANCTIONS HIT THIS PERIOD'
                                       TO TL-LABEL
               MOVE WS-SECTION-ROWS    TO TL-VALUE
               MOVE WS-TOTAL-LINE      TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
       3100-PRINT-SANCTION.
           IF WS-LINE-NO > WS-LINES-PER-PAGE
               PERFORM 7000-PAGE-HEAD
               MOVE WS-SANC-HEAD       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
               MOVE WS-RULE-LINE       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
           END-IF
      *
           MOVE SPACES                 TO WS-SANC-LINE
           MOVE DCL-SN-PARTY           TO SL-PARTY-ID
           IF IND-NAME < ZERO
               MOVE '*** NAME UNAVAILABLE ***'
                                       TO SL-LEGAL-NAME
           ELSE
               MOVE DCL-SN-NAME(1:30)  TO SL-LEGAL-NAME
           END-IF
           MOVE DCL-SN-HITS            TO SL-HIT-CNT
           MOVE DCL-SN-LAST(1:19)      TO SL-LAST-HIT
           IF IND-SN-BAND < ZERO
               MOVE '-'                TO SL-BAND
           ELSE
               MOVE DCL-SN-BAND        TO SL-BAND
           END-IF
      *
           MOVE WS-SANC-LINE           TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
           .
      *
      ******************************************************************
      * 4000 - SECTION 4, OVERDUE KYC                                  *
      ******************************************************************
       4000-SECTION-KYC.
           MOVE '4000-SECTION-KYC'     TO WS-PARAGRAPH
           MOVE 'SECTION 4 - OVERDUE KYC EXCEPTIONS'
                                       TO WS-SECTION-TITLE
           MOVE 99                     TO WS-LINE-NO
           MOVE ZERO                   TO WS-SECTION-ROWS
           MOVE 'N'                    TO WS-EOF-SW
      *
           EXEC SQL
               OPEN KYCCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               GO TO 4000-EXIT
           END-IF
      *
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               EXEC SQL
                   FETCH KYCCSR
                    INTO :DCL-KY-PARTY
                       , :DCL-KY-NAME :IND-NAME
                       , :DCL-KY-STATUS
                       , :DCL-KY-LEVEL
                       , :DCL-KY-DUE
                       , :DCL-KY-LATE
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1           TO WS-SECTION-ROWS
                       PERFORM 4100-PRINT-KYC
                   WHEN +100
                       MOVE 'Y'        TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       PERFORM 9100-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE KYCCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
      *
           IF WS-SECTION-ROWS = ZERO
               PERFORM 1900-PRINT-NONE
           ELSE
               MOVE WS-RULE-LINE       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
               MOVE SPACES             TO WS-TOTAL-LINE
               MOVE 'OVERDUE REVIEWS LISTED - WORST 500'
                                       TO TL-LABEL
               MOVE WS-SECTION-ROWS    TO TL-VALUE
               MOVE WS-TOTAL-LINE      TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
           END-IF
           .
       4000-EXIT.
           EXIT
           .
      *
       4100-PRINT-KYC.
           IF WS-LINE-NO > WS-LINES-PER-PAGE
               PERFORM 7000-PAGE-HEAD
               MOVE WS-KYC-HEAD        TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
               MOVE WS-RULE-LINE       TO RISKRPT-REC
               PERFORM 7100-WRITE-LINE
           END-IF
      *
           MOVE SPACES                 TO WS-KYC-LINE
           MOVE DCL-KY-PARTY           TO KL-PARTY-ID
           IF IND-NAME < ZERO
               MOVE '*** NAME UNAVAILABLE ***'
                                       TO KL-LEGAL-NAME
           ELSE
               MOVE DCL-KY-NAME(1:30)  TO KL-LEGAL-NAME
           END-IF
           MOVE DCL-KY-STATUS          TO KL-STATUS
           MOVE DCL-KY-LEVEL           TO KL-LEVEL
           MOVE DCL-KY-DUE             TO KL-DUE-DATE
           MOVE DCL-KY-LATE            TO KL-DAYS-LATE
      *
           MOVE WS-KYC-LINE            TO RISKRPT-REC
           PERFORM 7100-WRITE-LINE
           .
      *
      ******************************************************************
      * 7000 - PRINT CONTROL                                           *
      ******************************************************************
       7000-PAGE-HEAD.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO H1-PAGE-NO
           MOVE WS-SECTION-TITLE       TO H3-SECTION
      *
           WRITE RISKRPT-REC FROM WS-HEAD-1
           PERFORM 7200-CHECK-WRITE
           WRITE RISKRPT-REC FROM WS-HEAD-2
           PERFORM 7200-CHECK-WRITE
           WRITE RISKRPT-REC FROM WS-HEAD-3
           PERFORM 7200-CHECK-WRITE
           WRITE RISKRPT-REC FROM WS-BLANK-LINE
           PERFORM 7200-CHECK-WRITE
      *
           MOVE 4                      TO WS-LINE-NO
           .
      *
       7100-WRITE-LINE.
           WRITE RISKRPT-REC
           PERFORM 7200-CHECK-WRITE
           ADD 1                       TO WS-LINE-NO
           ADD 1                       TO WS-ROW-CNT
           .
      *
       7200-CHECK-WRITE.
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'RISKRPT '         TO ER-FILE-NAME
               MOVE 'WRITE   '         TO ER-SQL-OPERATION
               PERFORM 9200-FILE-ERROR
               PERFORM 9900-ABEND
           END-IF
           .
      *
       8000-CLOSE-DOWN.
           MOVE '8000-CLOSE-DOWN'      TO WS-PARAGRAPH
      *
           CLOSE RISKRPT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'RISKRPT '         TO ER-FILE-NAME
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9200-FILE-ERROR
           END-IF
      *
           EXEC SQL
               COMMIT
           END-EXEC
           .
      *
       9000-SET-RETURN-CODE.
           DISPLAY '******************************************'
           DISPLAY 'CBPRT04  AUDIT REPORT SUMMARY'
           DISPLAY '   PAGES PRINTED      ' WS-PAGE-NO
           DISPLAY '   LINES PRINTED      ' WS-ROW-CNT
           DISPLAY '   AUDIT EVENTS       ' WS-GRAND-CNT
           DISPLAY '   BAND UPGRADES      ' WS-GRAND-UP
           DISPLAY '   BAND DOWNGRADES    ' WS-GRAND-DOWN
           DISPLAY '******************************************'
      *
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12             TO RETURN-CODE
               WHEN WS-GRAND-CNT = ZERO
                   DISPLAY 'CBPRT04  *** NO AUDIT ACTIVITY IN THE '
                           'REPORTING PERIOD'
                   MOVE 4              TO RETURN-CODE
               WHEN OTHER
                   MOVE 0              TO RETURN-CODE
           END-EVALUATE
           .
      *
       9100-SQL-ERROR.
           MOVE SQLCODE                TO WS-SQL-DISP
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE 'Y'                    TO WS-FATAL-SW
      *
           DISPLAY 'CBPRT04  SQL ERROR PARA=' WS-PARAGRAPH
                   ' TABLE=' ER-SQL-TABLE
           DISPLAY '         OP=' ER-SQL-OPERATION
                   ' SQLCODE=' WS-SQL-DISP
           DISPLAY '         SQLERRMC=' SQLERRMC(1:44)
           .
      *
       9200-FILE-ERROR.
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE WS-RPT-STATUS          TO ER-FILE-STATUS
           MOVE 'F'                    TO ER-SEVERITY
           DISPLAY 'CBPRT04  FILE ERROR ' ER-FILE-NAME
                   ' OP=' ER-SQL-OPERATION
                   ' STATUS=' WS-RPT-STATUS
           .
      *
      ******************************************************************
      * 9900 - U3141.  A PART PRINTED PACK MUST NOT REACH THE          *
      *        COMMITTEE, SO THE STEP FAILS RATHER THAN TRUNCATE.      *
      ******************************************************************
       9900-ABEND.
           MOVE 'U314'                 TO ER-ABEND-CODE
           DISPLAY 'CBPRT04  ABEND U3141 PARA=' WS-PARAGRAPH
                   ' PAGE=' WS-PAGE-NO
           MOVE 12                     TO RETURN-CODE
           STOP RUN
           .
