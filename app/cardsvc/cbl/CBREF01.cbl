      ******************************************************************
      * CBREF01 - MERCHANT REFERENCE REFRESH                           *
      *                                                                *
      * FIRST STEP OF THE WEEKLY CARDREF CHAIN.  LOADS THE ACQUIRER    *
      * MERCHANT FEED INTO CARDSVC.MERCHANT.                           *
      *                                                                *
      * CALLED BY   - CBREF01J (STEP RUNMERCH, UNDER IKJEFT01)         *
      * CALLS       - CBCRD91 ON A FATAL CONDITION                     *
      * READS       - MERCHFD (FB 200, EBCDIC, GDG +0)                 *
      *               REFPARM (CONTROL CARDS)                          *
      * UPDATES     - CARDSVC.MERCHANT                                 *
      * WRITES      - MERCHRPT (FBA 133)                               *
      *                                                                *
      * THE FEED CARRIES A MAINTENANCE ACTION IN THE FIRST BYTE -      *
      *   A  ADD, INSERT AND FALL BACK TO UPDATE ON A DUPLICATE        *
      *   C  CHANGE, UPDATE AND FALL BACK TO INSERT WHEN NOT FOUND     *
      *   D  DELETE, LOGICAL - THE ROW IS MARKED STATUS 'D' SO THAT    *
      *      HISTORIC TRANSACTIONS STILL RESOLVE THEIR MERCHANT        *
      *   R  RISK ONLY, CHARGEBACK RATE AND HIGH RISK FLAG             *
      *                                                                *
      * THE HIGH RISK FLAG IS NOT TAKEN FROM THE FEED ALONE - A        *
      * MERCHANT WHOSE CHARGEBACK RATE IS AT OR ABOVE THE THRESHOLD    *
      * ON THE REFPARM CARD IS FLAGGED WHATEVER THE ACQUIRER SAYS.     *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - ALL RECORDS APPLIED                                     *
      *   04 - APPLIED WITH REJECTS BELOW THE TOLERANCE                *
      *   08 - REJECT TOLERANCE EXCEEDED, THE CHAIN MUST NOT CONTINUE  *
      *   12 - FATAL, USER ABEND U901 THROUGH CBCRD91                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBREF01.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MERCHFD-FILE ASSIGN TO MERCHFD
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-MERCHFD-STATUS.
      *
           SELECT REFPARM-FILE ASSIGN TO REFPARM
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REFPARM-STATUS.
      *
           SELECT MERCHRPT-FILE ASSIGN TO MERCHRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-MERCHRPT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  MERCHFD-FILE
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  MERCHFD-REC.
           05  MF-ACTION                   PIC X.
               88  MF-ADD                            VALUE 'A'.
               88  MF-CHANGE                         VALUE 'C'.
               88  MF-DELETE                         VALUE 'D'.
               88  MF-RISK-ONLY                      VALUE 'R'.
           05  MF-MERCHANT-ID              PIC X(15).
           05  MF-MERCHANT-NAME            PIC X(40).
           05  MF-MCC                      PIC X(4).
           05  MF-ACQUIRER-ID              PIC X(11).
           05  MF-COUNTRY-CD               PIC X(3).
           05  MF-CITY                     PIC X(25).
           05  MF-HIGH-RISK-FLG            PIC X.
           05  MF-CHARGEBACK-RATE          PIC S9(3)V99.
           05  MF-SETTLE-ROUTE-CD          PIC X(4).
           05  MF-STATUS                   PIC X.
           05  MF-ONBOARD-DATE             PIC 9(8).
           05  MF-FEED-SEQ                 PIC 9(9).
           05  FILLER                      PIC X(73).
      *
       FD  REFPARM-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REFPARM-REC.
           05  RP-KEYWORD                  PIC X(12).
           05  FILLER                      PIC X.
           05  RP-VALUE                    PIC X(20).
           05  FILLER                      PIC X(47).
      *
       FD  MERCHRPT-FILE
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  MERCHRPT-REC                    PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBREF01 '.
      *
       01  WS-STATUSES.
           05  WS-MERCHFD-STATUS           PIC X(2)  VALUE '00'.
               88  WS-MERCHFD-OK                     VALUE '00'.
               88  WS-MERCHFD-EOF                    VALUE '10'.
           05  WS-REFPARM-STATUS           PIC X(2)  VALUE '00'.
               88  WS-REFPARM-OK                     VALUE '00'.
           05  WS-MERCHRPT-STATUS          PIC X(2)  VALUE '00'.
               88  WS-MERCHRPT-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-PARM-EOF-SW              PIC X     VALUE 'N'.
               88  WS-PARM-EOF                       VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
      *
      ******************************************************************
      * REFPARM CARDS                                                  *
      *   RUN-DATE      CCYYMMDD                                       *
      *   COMMIT-FREQ   NNNNNN    ROWS BETWEEN COMMIT POINTS           *
      *   HIRISK-CBRT  NNN.NN     CHARGEBACK RATE FORCING HIGH RISK    *
      *   REJECT-TOL    NNNNN     REJECTS ALLOWED BEFORE RC 08         *
      ******************************************************************
       01  WS-PARMS.
           05  WS-RUN-DATE                 PIC 9(8)  VALUE ZERO.
           05  WS-COMMIT-FREQ              PIC 9(6)  VALUE ZERO.
           05  WS-HIRISK-CBRATE            PIC S9(3)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-REJECT-TOL               PIC 9(5)  VALUE ZERO.
      *
       01  WS-COUNTS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-INSERT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-UPDATE-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-DELETE-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-RISK-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-FLAGGED-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-REJECT-REASON            PIC X(30) VALUE SPACES.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE 99.
           05  WS-PAGE-CNT                 PIC 9(3)  VALUE ZERO.
           05  WS-RETURN-CD                PIC S9(4) COMP VALUE ZERO.
      *
       01  WS-ISO-DATE.
           05  WS-ISO-YYYY                 PIC 9(4).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-MM                   PIC 9(2).
           05  FILLER                      PIC X     VALUE '-'.
           05  WS-ISO-DD                   PIC 9(2).
      *
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-YYYY                  PIC 9(4).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
      *
      ******************************************************************
      * REPORT LINES                                                   *
      ******************************************************************
       01  RL-HEAD1.
           05  FILLER                      PIC X     VALUE '1'.
           05  FILLER                      PIC X(20) VALUE
               'CBREF01             '.
           05  FILLER                      PIC X(46) VALUE
               'MERCHANT REFERENCE REFRESH                    '.
           05  FILLER                      PIC X(10) VALUE
               'RUN DATE  '.
           05  RH1-DATE                    PIC X(10).
           05  FILLER                      PIC X(28) VALUE SPACES.
           05  FILLER                      PIC X(5)  VALUE 'PAGE '.
           05  RH1-PAGE                    PIC ZZ9.
           05  FILLER                      PIC X(10) VALUE SPACES.
      *
       01  RL-HEAD2.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(132) VALUE
               'ACT MERCHANT ID     NAME                        CTRY'.
      *
       01  RL-HEAD3.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(132) VALUE
               '--- --------------- --------------------------- ----'.
      *
       01  RL-DETAIL.
           05  FILLER                      PIC X     VALUE ' '.
           05  RD-ACTION                   PIC X(3).
           05  FILLER                      PIC X     VALUE SPACE.
           05  RD-MERCHANT-ID              PIC X(15).
           05  FILLER                      PIC X     VALUE SPACE.
           05  RD-NAME                     PIC X(27).
           05  FILLER                      PIC X     VALUE SPACE.
           05  RD-COUNTRY                  PIC X(4).
           05  FILLER                      PIC X     VALUE SPACE.
           05  RD-REASON                   PIC X(30).
           05  FILLER                      PIC X(49) VALUE SPACES.
      *
       01  RL-TOTAL.
           05  FILLER                      PIC X     VALUE ' '.
           05  RT-TEXT                     PIC X(34).
           05  RT-COUNT                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(88) VALUE SPACES.
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-MERCH.
           05  DCL-MERCHANT-ID             PIC X(15).
           05  DCL-MERCHANT-NAME           PIC X(40).
           05  DCL-MCC                     PIC X(4).
           05  DCL-ACQUIRER-ID             PIC X(11).
           05  DCL-COUNTRY-CD              PIC X(3).
           05  DCL-CITY                    PIC X(25).
           05  DCL-HIGH-RISK-FLG           PIC X.
           05  DCL-CHARGEBACK-RATE         PIC S9(3)V99 COMP-3.
           05  DCL-SETTLE-ROUTE-CD         PIC X(4).
           05  DCL-STATUS                  PIC X.
           05  DCL-ONBOARD-DATE            PIC X(10).
      *
       01  IND-ONBOARD                     PIC S9(4) COMP.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 1000-INITIALISE
           IF WS-FATAL
               GO TO 0000-TERMINATE
           END-IF
      *
           PERFORM UNTIL WS-EOF OR WS-FATAL
               PERFORM 2000-READ-FEED
               IF NOT WS-EOF
                   PERFORM 3000-APPLY-RECORD
               END-IF
           END-PERFORM
      *
           PERFORM 8000-FINALISE
           .
       0000-TERMINATE.
           IF WS-FATAL
               PERFORM 9500-ABEND
           END-IF
           MOVE WS-RETURN-CD               TO RETURN-CODE
           GOBACK
           .
      *
       1000-INITIALISE.
           OPEN INPUT  REFPARM-FILE
           IF NOT WS-REFPARM-OK
               MOVE 'REFPARM OPEN'         TO WS-REJECT-REASON
               PERFORM 9300-FILE-ERROR
               GO TO 1000-EXIT
           END-IF
      *
           PERFORM UNTIL WS-PARM-EOF
               READ REFPARM-FILE
                   AT END
                       MOVE 'Y'            TO WS-PARM-EOF-SW
                   NOT AT END
                       PERFORM 1100-APPLY-CARD
               END-READ
           END-PERFORM
           CLOSE REFPARM-FILE
      *
           IF WS-RUN-DATE = ZERO
           OR WS-COMMIT-FREQ = ZERO
               DISPLAY 'CBREF01 REQUIRED REFPARM CARD MISSING'
               MOVE 'Y'                    TO WS-FATAL-SW
               MOVE 'CBREF01 '             TO ER-PGM-NAME
               MOVE '1000-INITIALISE'      TO ER-PARAGRAPH
               MOVE 'F'                    TO ER-SEVERITY
               MOVE 'DATA'                 TO ER-ERROR-TYPE
               MOVE 'REQUIRED REFPARM CARD MISSING'
                                           TO ER-MESSAGE
               MOVE 'U901'                 TO ER-ABEND-CODE
               MOVE 'Y'                    TO ER-ABEND-REQUESTED
               GO TO 1000-EXIT
           END-IF
      *
           OPEN INPUT  MERCHFD-FILE
           IF NOT WS-MERCHFD-OK
               MOVE 'MERCHFD OPEN'         TO WS-REJECT-REASON
               PERFORM 9300-FILE-ERROR
               GO TO 1000-EXIT
           END-IF
      *
           OPEN OUTPUT MERCHRPT-FILE
           IF NOT WS-MERCHRPT-OK
               MOVE 'MERCHRPT OPEN'        TO WS-REJECT-REASON
               PERFORM 9300-FILE-ERROR
               GO TO 1000-EXIT
           END-IF
      *
           MOVE WS-RUN-DATE                TO WS-DATE-WORK
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           MOVE WS-ISO-DATE                TO RH1-DATE
           .
       1000-EXIT.
           EXIT
           .
      *
       1100-APPLY-CARD.
           EVALUATE RP-KEYWORD
               WHEN 'RUN-DATE    '
                   MOVE RP-VALUE(1:8)      TO WS-RUN-DATE
               WHEN 'COMMIT-FREQ '
                   MOVE RP-VALUE(1:6)      TO WS-COMMIT-FREQ
               WHEN 'HIRISK-CBRT '
                   COMPUTE WS-HIRISK-CBRATE =
                           FUNCTION NUMVAL(RP-VALUE)
               WHEN 'REJECT-TOL  '
                   MOVE RP-VALUE(1:5)      TO WS-REJECT-TOL
               WHEN '*           '
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'CBREF01 UNKNOWN REFPARM CARD - '
                           RP-KEYWORD
           END-EVALUATE
           .
      *
       2000-READ-FEED.
           READ MERCHFD-FILE
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
               NOT AT END
                   ADD 1                   TO WS-READ-CNT
           END-READ
      *
           IF NOT WS-MERCHFD-OK
           AND NOT WS-MERCHFD-EOF
               MOVE 'MERCHFD READ'         TO WS-REJECT-REASON
               PERFORM 9300-FILE-ERROR
           END-IF
           .
      *
       3000-APPLY-RECORD.
           MOVE SPACES                     TO WS-REJECT-REASON
           PERFORM 3100-EDIT-RECORD
           IF WS-REJECT-REASON NOT = SPACES
               PERFORM 7000-REJECT-LINE
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 3200-MOVE-HOST-VARS
      *
           EVALUATE TRUE
               WHEN MF-ADD
                   PERFORM 4000-INSERT-MERCHANT
               WHEN MF-CHANGE
                   PERFORM 4100-UPDATE-MERCHANT
               WHEN MF-DELETE
                   PERFORM 4200-DELETE-MERCHANT
               WHEN MF-RISK-ONLY
                   PERFORM 4300-UPDATE-RISK
               WHEN OTHER
                   MOVE 'UNKNOWN ACTION CODE'
                                           TO WS-REJECT-REASON
                   PERFORM 7000-REJECT-LINE
           END-EVALUATE
      *
           IF NOT WS-FATAL
               ADD 1                       TO WS-SINCE-COMMIT
               IF WS-SINCE-COMMIT NOT < WS-COMMIT-FREQ
                   PERFORM 6000-COMMIT
               END-IF
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
       3100-EDIT-RECORD.
           EVALUATE TRUE
               WHEN MF-MERCHANT-ID = SPACES
                   MOVE 'MERCHANT ID IS BLANK'
                                           TO WS-REJECT-REASON
               WHEN MF-COUNTRY-CD = SPACES
                AND NOT MF-RISK-ONLY
                   MOVE 'COUNTRY CODE IS BLANK'
                                           TO WS-REJECT-REASON
               WHEN MF-MCC NOT NUMERIC
                AND NOT MF-DELETE
                AND NOT MF-RISK-ONLY
                   MOVE 'MCC IS NOT NUMERIC'
                                           TO WS-REJECT-REASON
               WHEN MF-CHARGEBACK-RATE < ZERO
                   MOVE 'CHARGEBACK RATE IS NEGATIVE'
                                           TO WS-REJECT-REASON
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3200 - THE HIGH RISK FLAG IS THE ACQUIRER FLAG OR'D WITH THE   *
      *        LOCAL CHARGEBACK RULE.  ONCE FLAGGED BY THE RULE THE    *
      *        MERCHANT STAYS FLAGGED UNTIL THE RATE FALLS AGAIN.      *
      ******************************************************************
       3200-MOVE-HOST-VARS.
           MOVE MF-MERCHANT-ID             TO DCL-MERCHANT-ID
           MOVE MF-MERCHANT-NAME           TO DCL-MERCHANT-NAME
           MOVE MF-MCC                     TO DCL-MCC
           MOVE MF-ACQUIRER-ID             TO DCL-ACQUIRER-ID
           MOVE MF-COUNTRY-CD              TO DCL-COUNTRY-CD
           MOVE MF-CITY                    TO DCL-CITY
           MOVE MF-CHARGEBACK-RATE         TO DCL-CHARGEBACK-RATE
           MOVE MF-SETTLE-ROUTE-CD         TO DCL-SETTLE-ROUTE-CD
      *
           IF MF-STATUS = SPACE
               MOVE 'A'                    TO DCL-STATUS
           ELSE
               MOVE MF-STATUS              TO DCL-STATUS
           END-IF
      *
           IF MF-HIGH-RISK-FLG = 'Y'
           OR MF-CHARGEBACK-RATE NOT < WS-HIRISK-CBRATE
               MOVE 'Y'                    TO DCL-HIGH-RISK-FLG
               IF MF-HIGH-RISK-FLG NOT = 'Y'
                   ADD 1                   TO WS-FLAGGED-CNT
               END-IF
           ELSE
               MOVE 'N'                    TO DCL-HIGH-RISK-FLG
           END-IF
      *
           IF MF-ONBOARD-DATE > ZERO
               MOVE MF-ONBOARD-DATE        TO WS-DATE-WORK
               MOVE WS-DW-YYYY             TO WS-ISO-YYYY
               MOVE WS-DW-MM               TO WS-ISO-MM
               MOVE WS-DW-DD               TO WS-ISO-DD
               MOVE WS-ISO-DATE            TO DCL-ONBOARD-DATE
               MOVE ZERO                   TO IND-ONBOARD
           ELSE
               MOVE -1                     TO IND-ONBOARD
           END-IF
           .
      *
       4000-INSERT-MERCHANT.
           EXEC SQL
               INSERT INTO CARDSVC.MERCHANT
                     (MERCHANT_ID
                    , MERCHANT_NAME
                    , MCC
                    , ACQUIRER_ID
                    , COUNTRY_CD
                    , CITY
                    , HIGH_RISK_FLG
                    , CHARGEBACK_RATE
                    , SETTLE_ROUTE_CD
                    , STATUS
                    , ONBOARD_DATE
                    , LAST_MAINT_TS)
               VALUES (:DCL-MERCHANT-ID
                    , :DCL-MERCHANT-NAME
                    , :DCL-MCC
                    , :DCL-ACQUIRER-ID
                    , :DCL-COUNTRY-CD
                    , :DCL-CITY
                    , :DCL-HIGH-RISK-FLG
                    , :DCL-CHARGEBACK-RATE
                    , :DCL-SETTLE-ROUTE-CD
                    , :DCL-STATUS
                    , :DCL-ONBOARD-DATE :IND-ONBOARD
                    , CURRENT TIMESTAMP)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-INSERT-CNT
               WHEN -803
      *            THE ACQUIRER RE-SENDS ADDS AFTER A FEED RERUN
                   PERFORM 4100-UPDATE-MERCHANT
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       4100-UPDATE-MERCHANT.
           EXEC SQL
               UPDATE CARDSVC.MERCHANT
                  SET MERCHANT_NAME   = :DCL-MERCHANT-NAME
                    , MCC             = :DCL-MCC
                    , ACQUIRER_ID     = :DCL-ACQUIRER-ID
                    , COUNTRY_CD      = :DCL-COUNTRY-CD
                    , CITY            = :DCL-CITY
                    , HIGH_RISK_FLG   = :DCL-HIGH-RISK-FLG
                    , CHARGEBACK_RATE = :DCL-CHARGEBACK-RATE
                    , SETTLE_ROUTE_CD = :DCL-SETTLE-ROUTE-CD
                    , STATUS          = :DCL-STATUS
                    , LAST_MAINT_TS   = CURRENT TIMESTAMP
                WHERE MERCHANT_ID     = :DCL-MERCHANT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-UPDATE-CNT
               WHEN +100
                   IF MF-CHANGE
      *                A CHANGE FOR A MERCHANT WE NEVER SAW ADDED
                       PERFORM 4000-INSERT-MERCHANT
                   ELSE
                       MOVE 'MERCHANT NOT ON FILE'
                                           TO WS-REJECT-REASON
                       PERFORM 7000-REJECT-LINE
                   END-IF
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 4200 - LOGICAL DELETE ONLY.  A PHYSICAL DELETE WOULD ORPHAN    *
      *        EVERY TRANSACTION THAT EVER NAMED THE MERCHANT AND      *
      *        CBREF05 WOULD REPORT THEM WEEK AFTER WEEK.              *
      ******************************************************************
       4200-DELETE-MERCHANT.
           EXEC SQL
               UPDATE CARDSVC.MERCHANT
                  SET STATUS        = 'D'
                    , LAST_MAINT_TS = CURRENT TIMESTAMP
                WHERE MERCHANT_ID   = :DCL-MERCHANT-ID
                  AND STATUS       <> 'D'
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-DELETE-CNT
               WHEN +100
                   MOVE 'ALREADY DELETED OR NOT ON FILE'
                                           TO WS-REJECT-REASON
                   PERFORM 7000-REJECT-LINE
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       4300-UPDATE-RISK.
           EXEC SQL
               UPDATE CARDSVC.MERCHANT
                  SET HIGH_RISK_FLG   = :DCL-HIGH-RISK-FLG
                    , CHARGEBACK_RATE = :DCL-CHARGEBACK-RATE
                    , LAST_MAINT_TS   = CURRENT TIMESTAMP
                WHERE MERCHANT_ID     = :DCL-MERCHANT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-RISK-CNT
               WHEN +100
                   MOVE 'RISK UPDATE - NOT ON FILE'
                                           TO WS-REJECT-REASON
                   PERFORM 7000-REJECT-LINE
               WHEN OTHER
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       6000-COMMIT.
           EXEC SQL
               COMMIT
           END-EXEC
      *
           IF SQLCODE NOT = 0
               PERFORM 9400-SQL-ERROR
           ELSE
               MOVE ZERO                   TO WS-SINCE-COMMIT
               DISPLAY 'CBREF01 COMMIT AT FEED SEQ ' MF-FEED-SEQ
                       ' READ ' WS-READ-CNT
           END-IF
           .
      *
       7000-REJECT-LINE.
           ADD 1                           TO WS-REJECT-CNT
      *
           IF WS-LINE-CNT > 55
               PERFORM 7100-PAGE-HEAD
           END-IF
      *
           MOVE MF-ACTION                  TO RD-ACTION
           MOVE MF-MERCHANT-ID             TO RD-MERCHANT-ID
           MOVE MF-MERCHANT-NAME           TO RD-NAME
           MOVE MF-COUNTRY-CD              TO RD-COUNTRY
           MOVE WS-REJECT-REASON           TO RD-REASON
           MOVE RL-DETAIL                  TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
           .
      *
       7100-PAGE-HEAD.
           ADD 1                           TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT                TO RH1-PAGE
           MOVE RL-HEAD1                   TO MERCHRPT-REC
           WRITE MERCHRPT-REC
           MOVE RL-HEAD2                   TO MERCHRPT-REC
           WRITE MERCHRPT-REC
           MOVE RL-HEAD3                   TO MERCHRPT-REC
           WRITE MERCHRPT-REC
           MOVE 4                          TO WS-LINE-CNT
           .
      *
       7200-WRITE-LINE.
           WRITE MERCHRPT-REC
           IF NOT WS-MERCHRPT-OK
               MOVE 'MERCHRPT WRITE'       TO WS-REJECT-REASON
               PERFORM 9300-FILE-ERROR
           END-IF
           ADD 1                           TO WS-LINE-CNT
           .
      *
       8000-FINALISE.
           IF WS-LINE-CNT > 45
               PERFORM 7100-PAGE-HEAD
           END-IF
      *
           MOVE SPACES                     TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
      *
           MOVE 'FEED RECORDS READ'        TO RT-TEXT
           MOVE WS-READ-CNT                TO RT-COUNT
           MOVE RL-TOTAL                   TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
      *
           MOVE 'MERCHANTS INSERTED'       TO RT-TEXT
           MOVE WS-INSERT-CNT              TO RT-COUNT
           MOVE RL-TOTAL                   TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
      *
           MOVE 'MERCHANTS UPDATED'        TO RT-TEXT
           MOVE WS-UPDATE-CNT              TO RT-COUNT
           MOVE RL-TOTAL                   TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
      *
           MOVE 'MERCHANTS LOGICALLY DELETED'
                                           TO RT-TEXT
           MOVE WS-DELETE-CNT              TO RT-COUNT
           MOVE RL-TOTAL                   TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
      *
           MOVE 'RISK ONLY UPDATES'        TO RT-TEXT
           MOVE WS-RISK-CNT                TO RT-COUNT
           MOVE RL-TOTAL                   TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
      *
           MOVE 'FLAGGED HIGH RISK BY RATE RULE'
                                           TO RT-TEXT
           MOVE WS-FLAGGED-CNT             TO RT-COUNT
           MOVE RL-TOTAL                   TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
      *
           MOVE 'RECORDS REJECTED'         TO RT-TEXT
           MOVE WS-REJECT-CNT              TO RT-COUNT
           MOVE RL-TOTAL                   TO MERCHRPT-REC
           PERFORM 7200-WRITE-LINE
      *
           PERFORM 6000-COMMIT
      *
           CLOSE MERCHFD-FILE
                 MERCHRPT-FILE
      *
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO WS-RETURN-CD
               WHEN WS-REJECT-CNT > WS-REJECT-TOL
                   MOVE 8                  TO WS-RETURN-CD
                   DISPLAY 'CBREF01 REJECT TOLERANCE EXCEEDED - '
                           WS-REJECT-CNT
               WHEN WS-REJECT-CNT > ZERO
                   MOVE 4                  TO WS-RETURN-CD
               WHEN OTHER
                   MOVE 0                  TO WS-RETURN-CD
           END-EVALUATE
      *
           DISPLAY 'CBREF01 READ=' WS-READ-CNT
                   ' INS=' WS-INSERT-CNT
                   ' UPD=' WS-UPDATE-CNT
                   ' DEL=' WS-DELETE-CNT
                   ' RSK=' WS-RISK-CNT
                   ' REJ=' WS-REJECT-CNT
           .
      *
       9300-FILE-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF01 '                 TO ER-PGM-NAME
           MOVE '9300-FILE-ERROR'          TO ER-PARAGRAPH
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'VSAM'                     TO ER-ERROR-TYPE
           MOVE WS-REJECT-REASON           TO ER-MESSAGE
           MOVE 'MERCHFD '                 TO ER-FILE-NAME
           MOVE WS-MERCHFD-STATUS          TO ER-FILE-STATUS
           MOVE 'U902'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF01 FILE ERROR - ' WS-REJECT-REASON
                   ' MERCHFD=' WS-MERCHFD-STATUS
                   ' MERCHRPT=' WS-MERCHRPT-STATUS
           .
      *
       9400-SQL-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF01 '                 TO ER-PGM-NAME
           MOVE '9400-SQL-ERROR'           TO ER-PARAGRAPH
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           MOVE 'MERCHANT           '      TO ER-SQL-TABLE
           MOVE 'MERCHANT MAINTENANCE FAILED'
                                           TO ER-MESSAGE
           MOVE 'U903'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF01 SQL ERROR SQLCODE=' SQLCODE
                   ' MERCHANT=' MF-MERCHANT-ID
                   ' ACTION=' MF-ACTION
           EXEC SQL
               ROLLBACK
           END-EXEC
           .
      *
       9500-ABEND.
           MOVE 12                         TO WS-RETURN-CD
           CALL 'CBCRD91' USING ERROR-AREA
           .
