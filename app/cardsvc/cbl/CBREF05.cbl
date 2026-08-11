      ******************************************************************
      * CBREF05 - REFERENCE INTEGRITY AND DATA QUALITY REPORT          *
      *                                                                *
      * LAST STEP OF THE WEEKLY CARDREF CHAIN.  COUNTS AND VALIDATES   *
      * ROWS.  IT CHANGES NOTHING - EVERY FINDING IS A LINE ON THE     *
      * REPORT AND A CONTRIBUTION TO THE STEP RETURN CODE.             *
      *                                                                *
      * SECTIONS OF THE REPORT                                         *
      *   1  ORPHANS      - CARDS WITH NO ACCOUNT, LIMITS WITH NO      *
      *                     CARD, TRANSACTIONS WITH NO ACCOUNT,        *
      *                     TRANSACTIONS NAMING AN UNKNOWN MERCHANT    *
      *   2  ROUTE SANITY - ROUTES WITH NO PROGRAM NAME, ROUTES STILL  *
      *                     FLAGGED ACTIVE PAST THEIR EXPIRY DATE,     *
      *                     GAPS IN THE SEQUENCE NUMBERS WITHIN A      *
      *                     ROUTE KEY                                  *
      *   3  VSAM V DB2   - RECORD COUNTS IN MERCHRTE AND PGMROUT      *
      *                     AGAINST THE TABLES THEY WERE BUILT FROM    *
      *                                                                *
      * CALLED BY   - CBREF05J (STEP REFRPT, UNDER IKJEFT01)           *
      * CALLS       - CBCRD91 ON A FATAL CONDITION                     *
      * READS       - CARDSVC.CARD, CARD_LIMIT, TRANSACTION, ACCOUNT,  *
      *               MERCHANT, PGM_ROUTE, MERCHRTE, PGMROUT, REFPARM  *
      * WRITES      - REFRPT (FBA 133)                                 *
      *                                                                *
      * RETURN CODES - THE HIGHEST SEVERITY REACHED                    *
      *   00 - NOTHING FOUND                                           *
      *   04 - FINDINGS BELOW THE TOLERANCES ON THE REFPARM CARDS      *
      *   08 - TOLERANCE EXCEEDED, OR A VSAM COUNT DISAGREES WITH DB2  *
      *   12 - FATAL, USER ABEND U941 THROUGH CBCRD91                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBREF05.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REFPARM-FILE ASSIGN TO REFPARM
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-PARM-STATUS.
      *
           SELECT REFRPT-FILE ASSIGN TO REFRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-RPT-STATUS.
      *
           SELECT MERCHRTE-FILE ASSIGN TO MERCHRTE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS MRR-KEY
                  FILE STATUS  IS WS-MRT-STATUS.
      *
           SELECT PGMROUT-FILE ASSIGN TO PGMROUT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS PGR-KEY
                  FILE STATUS  IS WS-PGR-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  REFPARM-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REFPARM-REC.
           05  RP-KEYWORD                  PIC X(12).
           05  FILLER                      PIC X.
           05  RP-VALUE                    PIC X(20).
           05  FILLER                      PIC X(47).
      *
       FD  REFRPT-FILE
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REFRPT-REC                      PIC X(133).
      *
       FD  MERCHRTE-FILE
           RECORD CONTAINS 120 CHARACTERS.
       01  MERCHRTE-REC.
           05  MRR-KEY                     PIC X(15).
           05  FILLER                      PIC X(105).
      *
       FD  PGMROUT-FILE
           RECORD CONTAINS 97 CHARACTERS.
       01  PGMROUT-REC.
           05  PGR-KEY                     PIC X(16).
           05  FILLER                      PIC X(81).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBREF05 '.
      *
       01  WS-STATUSES.
           05  WS-PARM-STATUS              PIC X(2)  VALUE '00'.
               88  WS-PARM-OK                        VALUE '00'.
           05  WS-RPT-STATUS               PIC X(2)  VALUE '00'.
               88  WS-RPT-OK                         VALUE '00'.
           05  WS-MRT-STATUS               PIC X(2)  VALUE '00'.
               88  WS-MRT-OK                         VALUE '00'.
               88  WS-MRT-EOF                        VALUE '10'.
           05  WS-PGR-STATUS               PIC X(2)  VALUE '00'.
               88  WS-PGR-OK                         VALUE '00'.
               88  WS-PGR-EOF                        VALUE '10'.
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
      * REFPARM CARDS READ BY THIS STEP                                *
      *   RUN-DATE      CCYYMMDD                                       *
      *   ORPHAN-TOL    NNNNN   ORPHANS ALLOWED BEFORE SEVERITY 8      *
      *   ROUTE-TOL     NNNNN   ROUTE FINDINGS ALLOWED BEFORE 8        *
      *   LIST-LIMIT    NNNNN   DETAIL LINES PRINTED PER SECTION       *
      ******************************************************************
       01  WS-PARMS.
           05  WS-RUN-DATE                 PIC 9(8)  VALUE ZERO.
           05  WS-ORPHAN-TOL               PIC 9(5)  VALUE ZERO.
           05  WS-ROUTE-TOL                PIC 9(5)  VALUE ZERO.
           05  WS-LIST-LIMIT               PIC 9(5)  VALUE 50.
      *
       01  WS-FINDINGS.
           05  WS-CARD-ORPHANS             PIC 9(9)  VALUE ZERO.
           05  WS-LIMIT-ORPHANS            PIC 9(9)  VALUE ZERO.
           05  WS-TXN-ORPHANS              PIC 9(9)  VALUE ZERO.
           05  WS-TXN-MERCH-ORPHANS        PIC 9(9)  VALUE ZERO.
           05  WS-ROUTE-NO-PGM             PIC 9(9)  VALUE ZERO.
           05  WS-ROUTE-EXPIRED            PIC 9(9)  VALUE ZERO.
           05  WS-ROUTE-GAPS               PIC 9(9)  VALUE ZERO.
           05  WS-ORPHAN-TOTAL             PIC 9(9)  VALUE ZERO.
           05  WS-ROUTE-TOTAL              PIC 9(9)  VALUE ZERO.
      *
       01  WS-VSAM-COUNTS.
           05  WS-MERCH-DB2                PIC 9(9)  VALUE ZERO.
           05  WS-MERCH-VSAM               PIC 9(9)  VALUE ZERO.
           05  WS-ROUTE-DB2                PIC 9(9)  VALUE ZERO.
           05  WS-ROUTE-VSAM               PIC 9(9)  VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-SEVERITY                 PIC 9(2)  VALUE ZERO.
           05  WS-RETURN-CD                PIC S9(4) COMP VALUE ZERO.
           05  WS-MSG                      PIC X(78) VALUE SPACES.
           05  WS-LISTED                   PIC 9(5)  VALUE ZERO.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE 99.
           05  WS-PAGE-CNT                 PIC 9(3)  VALUE ZERO.
           05  WS-SECTION-TITLE            PIC X(40) VALUE SPACES.
           05  WS-EXPECTED-SEQ             PIC 9(4)  VALUE ZERO.
           05  WS-SEQ-DISP                 PIC 9(4)  VALUE ZERO.
           05  WS-PREV-ROUTE-KEY           PIC X(12) VALUE LOW-VALUES.
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
      ******************************************************************
      * REPORT LINES                                                   *
      ******************************************************************
       01  RL-HEAD1.
           05  FILLER                      PIC X     VALUE '1'.
           05  FILLER                      PIC X(9)  VALUE 'CBREF05  '.
           05  FILLER                      PIC X(44) VALUE
               'CARDSVC REFERENCE INTEGRITY REPORT'.
           05  FILLER                      PIC X(10) VALUE 'RUN DATE  '.
           05  RH1-DATE                    PIC X(10).
           05  FILLER                      PIC X(45) VALUE SPACES.
           05  FILLER                      PIC X(5)  VALUE 'PAGE '.
           05  RH1-PAGE                    PIC ZZ9.
           05  FILLER                      PIC X(6)  VALUE SPACES.
      *
       01  RL-SECTION.
           05  FILLER                      PIC X     VALUE '0'.
           05  RS-TITLE                    PIC X(40).
           05  FILLER                      PIC X(92) VALUE SPACES.
      *
       01  RL-DETAIL.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  RD-KEY                      PIC X(24).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-TEXT                     PIC X(60).
           05  FILLER                      PIC X(42) VALUE SPACES.
      *
       01  RL-COUNT.
           05  FILLER                      PIC X     VALUE ' '.
           05  RC-TEXT                     PIC X(46).
           05  RC-VALUE                    PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  RC-FLAG                     PIC X(12).
           05  FILLER                      PIC X(59) VALUE SPACES.
      *
       01  RL-COMPARE.
           05  FILLER                      PIC X     VALUE ' '.
           05  RM-NAME                     PIC X(12).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  FILLER                      PIC X(6)  VALUE 'DB2   '.
           05  RM-DB2                      PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(3)  VALUE SPACES.
           05  FILLER                      PIC X(6)  VALUE 'VSAM  '.
           05  RM-VSAM                     PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(3)  VALUE SPACES.
           05  RM-FLAG                     PIC X(20).
           05  FILLER                      PIC X(58) VALUE SPACES.
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-WORK.
           05  DCL-ROW-CNT                 PIC S9(9) COMP-3.
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-ACCT-ID                 PIC S9(11) COMP-3.
           05  DCL-LIMIT-TYPE              PIC X(4).
           05  DCL-TXN-ID                  PIC X(16).
           05  DCL-MERCHANT-ID             PIC X(15).
           05  DCL-ROUTE-TYPE              PIC X(4).
           05  DCL-ROUTE-KEY               PIC X(8).
           05  DCL-SEQ-NBR                 PIC S9(4) COMP.
           05  DCL-PGM-NAME                PIC X(8).
           05  DCL-EXP-DATE                PIC X(10).
           05  DCL-RUN-DATE                PIC X(10).
      *
      ******************************************************************
      * ORPHAN CURSORS.  ALL OF THEM ARE ANTI JOINS - THE OUTER TABLE  *
      * ROW EXISTS AND THE PARENT DOES NOT.  ISOLATION UR IS FINE,     *
      * THE REPORT IS A SNAPSHOT AND THE CHAIN HOLDS NO LOCKS.         *
      ******************************************************************
           EXEC SQL DECLARE CARDORPH CURSOR FOR
               SELECT C.CARD_NUM
                    , C.ACCT_ID
                 FROM CARDSVC.CARD C
                WHERE NOT EXISTS
                     (SELECT 1
                        FROM CARDSVC.ACCOUNT A
                       WHERE A.ACCT_ID = C.ACCT_ID)
                ORDER BY C.CARD_NUM
                WITH UR
           END-EXEC.
      *
           EXEC SQL DECLARE LIMTORPH CURSOR FOR
               SELECT L.CARD_NUM
                    , L.LIMIT_TYPE
                 FROM CARDSVC.CARD_LIMIT L
                WHERE NOT EXISTS
                     (SELECT 1
                        FROM CARDSVC.CARD C
                       WHERE C.CARD_NUM = L.CARD_NUM)
                ORDER BY L.CARD_NUM
                       , L.LIMIT_TYPE
                WITH UR
           END-EXEC.
      *
           EXEC SQL DECLARE TXNORPH CURSOR FOR
               SELECT T.TXN_ID
                    , T.ACCT_ID
                 FROM CARDSVC.TRANSACTION T
                WHERE NOT EXISTS
                     (SELECT 1
                        FROM CARDSVC.ACCOUNT A
                       WHERE A.ACCT_ID = T.ACCT_ID)
                ORDER BY T.TXN_ID
                WITH UR
           END-EXEC.
      *
           EXEC SQL DECLARE TXNMERCH CURSOR FOR
               SELECT T.TXN_ID
                    , T.MERCHANT_ID
                 FROM CARDSVC.TRANSACTION T
                WHERE T.MERCHANT_ID IS NOT NULL
                  AND T.MERCHANT_ID <> ' '
                  AND NOT EXISTS
                     (SELECT 1
                        FROM CARDSVC.MERCHANT M
                       WHERE M.MERCHANT_ID = T.MERCHANT_ID)
                ORDER BY T.TXN_ID
                WITH UR
           END-EXEC.
      *
      ******************************************************************
      * ROUTE SANITY CURSOR - EVERY ACTIVE ROUTE ROW IN KEY ORDER SO   *
      * THE SEQUENCE GAP TEST CAN BE DONE ON THE BREAK.                *
      ******************************************************************
           EXEC SQL DECLARE ROUTSANE CURSOR FOR
               SELECT ROUTE_TYPE
                    , ROUTE_KEY
                    , SEQ_NBR
                    , PGM_NAME
                    , CHAR(EXP_DATE, ISO)
                 FROM CARDSVC.PGM_ROUTE
                WHERE ACTIVE_FLG = 'Y'
                ORDER BY ROUTE_TYPE
                       , ROUTE_KEY
                       , SEQ_NBR
                WITH UR
           END-EXEC.
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
           PERFORM 2000-ORPHAN-SECTION
           IF NOT WS-FATAL
               PERFORM 3000-ROUTE-SECTION
           END-IF
           IF NOT WS-FATAL
               PERFORM 4000-VSAM-SECTION
           END-IF
      *
           PERFORM 8000-SUMMARY
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
           OPEN INPUT REFPARM-FILE
           IF NOT WS-PARM-OK
               MOVE 'REFPARM OPEN FAILED'  TO WS-MSG
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
               MOVE 'RUN-DATE CARD IS MISSING FROM REFPARM'
                                           TO WS-MSG
               PERFORM 9200-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           OPEN OUTPUT REFRPT-FILE
           IF NOT WS-RPT-OK
               MOVE 'REFRPT OPEN FAILED'   TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 1000-EXIT
           END-IF
      *
           MOVE WS-RUN-DATE                TO WS-DATE-WORK
           MOVE WS-DW-YYYY                 TO WS-ISO-YYYY
           MOVE WS-DW-MM                   TO WS-ISO-MM
           MOVE WS-DW-DD                   TO WS-ISO-DD
           MOVE WS-ISO-DATE                TO RH1-DATE
           MOVE WS-ISO-DATE                TO DCL-RUN-DATE
           .
       1000-EXIT.
           EXIT
           .
      *
       1100-APPLY-CARD.
           EVALUATE RP-KEYWORD
               WHEN 'RUN-DATE    '
                   MOVE RP-VALUE(1:8)      TO WS-RUN-DATE
               WHEN 'ORPHAN-TOL  '
                   MOVE RP-VALUE(1:5)      TO WS-ORPHAN-TOL
               WHEN 'ROUTE-TOL   '
                   MOVE RP-VALUE(1:5)      TO WS-ROUTE-TOL
               WHEN 'LIST-LIMIT  '
                   MOVE RP-VALUE(1:5)      TO WS-LIST-LIMIT
               WHEN '*           '
                   CONTINUE
               WHEN OTHER
      *            THE STEP SHARES ITS SYSIN WITH CBREF01, SO CARDS
      *            MEANT FOR THE LOAD ARE IGNORED HERE ON PURPOSE.
                   CONTINUE
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2000 - ORPHANS                                                 *
      ******************************************************************
       2000-ORPHAN-SECTION.
           MOVE 'SECTION 1 - ORPHANED ROWS'
                                           TO WS-SECTION-TITLE
           PERFORM 7100-SECTION-HEAD
      *
           PERFORM 2100-CARD-ORPHANS
           IF NOT WS-FATAL
               PERFORM 2200-LIMIT-ORPHANS
           END-IF
           IF NOT WS-FATAL
               PERFORM 2300-TXN-ORPHANS
           END-IF
           IF NOT WS-FATAL
               PERFORM 2400-TXN-MERCHANTS
           END-IF
      *
           COMPUTE WS-ORPHAN-TOTAL = WS-CARD-ORPHANS
                                   + WS-LIMIT-ORPHANS
                                   + WS-TXN-ORPHANS
                                   + WS-TXN-MERCH-ORPHANS
      *
           MOVE 'CARDS WITH NO ACCOUNT ROW'
                                           TO RC-TEXT
           MOVE WS-CARD-ORPHANS            TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           MOVE 'LIMITS WITH NO CARD ROW'  TO RC-TEXT
           MOVE WS-LIMIT-ORPHANS           TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           MOVE 'TRANSACTIONS WITH NO ACCOUNT ROW'
                                           TO RC-TEXT
           MOVE WS-TXN-ORPHANS             TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           MOVE 'TRANSACTIONS NAMING AN UNKNOWN MERCHANT'
                                           TO RC-TEXT
           MOVE WS-TXN-MERCH-ORPHANS       TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           IF WS-ORPHAN-TOTAL > WS-ORPHAN-TOL
               PERFORM 8100-RAISE-EIGHT
           ELSE
               IF WS-ORPHAN-TOTAL > ZERO
                   PERFORM 8200-RAISE-FOUR
               END-IF
           END-IF
           .
      *
       2100-CARD-ORPHANS.
           MOVE ZERO                       TO WS-LISTED
           EXEC SQL
               OPEN CARDORPH
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'OPEN CARDORPH FAILED' TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 2100-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               EXEC SQL
                   FETCH CARDORPH
                    INTO :DCL-CARD-NUM
                       , :DCL-ACCT-ID
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-CARD-ORPHANS
                       IF WS-LISTED < WS-LIST-LIMIT
                           ADD 1           TO WS-LISTED
                           MOVE DCL-CARD-NUM
                                           TO RD-KEY
                           MOVE 'CARD HAS NO ACCOUNT ROW'
                                           TO RD-TEXT
                           PERFORM 7200-DETAIL-LINE
                       END-IF
                   WHEN +100
                       MOVE 'Y'            TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'FETCH CARDORPH FAILED'
                                           TO WS-MSG
                       PERFORM 9400-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE CARDORPH
           END-EXEC
           .
       2100-EXIT.
           EXIT
           .
      *
       2200-LIMIT-ORPHANS.
           MOVE ZERO                       TO WS-LISTED
           EXEC SQL
               OPEN LIMTORPH
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'OPEN LIMTORPH FAILED' TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 2200-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               EXEC SQL
                   FETCH LIMTORPH
                    INTO :DCL-CARD-NUM
                       , :DCL-LIMIT-TYPE
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-LIMIT-ORPHANS
                       IF WS-LISTED < WS-LIST-LIMIT
                           ADD 1           TO WS-LISTED
                           MOVE SPACES     TO RD-KEY
                           MOVE DCL-CARD-NUM
                                           TO RD-KEY(1:16)
                           MOVE DCL-LIMIT-TYPE
                                           TO RD-KEY(18:4)
                           MOVE 'CARD LIMIT HAS NO CARD ROW'
                                           TO RD-TEXT
                           PERFORM 7200-DETAIL-LINE
                       END-IF
                   WHEN +100
                       MOVE 'Y'            TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'FETCH LIMTORPH FAILED'
                                           TO WS-MSG
                       PERFORM 9400-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE LIMTORPH
           END-EXEC
           .
       2200-EXIT.
           EXIT
           .
      *
       2300-TXN-ORPHANS.
           MOVE ZERO                       TO WS-LISTED
           EXEC SQL
               OPEN TXNORPH
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'OPEN TXNORPH FAILED'  TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 2300-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               EXEC SQL
                   FETCH TXNORPH
                    INTO :DCL-TXN-ID
                       , :DCL-ACCT-ID
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-TXN-ORPHANS
                       IF WS-LISTED < WS-LIST-LIMIT
                           ADD 1           TO WS-LISTED
                           MOVE DCL-TXN-ID TO RD-KEY
                           MOVE 'TRANSACTION HAS NO ACCOUNT ROW'
                                           TO RD-TEXT
                           PERFORM 7200-DETAIL-LINE
                       END-IF
                   WHEN +100
                       MOVE 'Y'            TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'FETCH TXNORPH FAILED'
                                           TO WS-MSG
                       PERFORM 9400-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE TXNORPH
           END-EXEC
           .
       2300-EXIT.
           EXIT
           .
      *
       2400-TXN-MERCHANTS.
           MOVE ZERO                       TO WS-LISTED
           EXEC SQL
               OPEN TXNMERCH
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'OPEN TXNMERCH FAILED' TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 2400-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               EXEC SQL
                   FETCH TXNMERCH
                    INTO :DCL-TXN-ID
                       , :DCL-MERCHANT-ID
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1               TO WS-TXN-MERCH-ORPHANS
                       IF WS-LISTED < WS-LIST-LIMIT
                           ADD 1           TO WS-LISTED
                           MOVE SPACES     TO RD-KEY
                           MOVE DCL-TXN-ID TO RD-KEY(1:16)
                           MOVE SPACES     TO RD-TEXT
                           STRING 'MERCHANT '
                                  DCL-MERCHANT-ID
                                  ' IS NOT ON THE MERCHANT TABLE'
                               DELIMITED BY SIZE
                               INTO RD-TEXT
                           END-STRING
                           PERFORM 7200-DETAIL-LINE
                       END-IF
                   WHEN +100
                       MOVE 'Y'            TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'FETCH TXNMERCH FAILED'
                                           TO WS-MSG
                       PERFORM 9400-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE TXNMERCH
           END-EXEC
           .
       2400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - ROUTE TABLE SANITY                                      *
      *                                                                *
      * THREE TESTS, ALL DONE ON THE ONE PASS -                        *
      *   A ROUTE WITH A BLANK PROGRAM NAME                            *
      *   A ROUTE PAST ITS EXPIRY DATE BUT STILL FLAGGED ACTIVE        *
      *   A SEQUENCE THAT DOES NOT START AT 1 OR SKIPS A NUMBER, WHICH *
      *   MATTERS BECAUSE THE MULTI STEP ROUTE TYPES ARE WALKED BY     *
      *   SEQUENCE NUMBER UNTIL A NOT FOUND STOPS THE WALK             *
      ******************************************************************
       3000-ROUTE-SECTION.
           MOVE 'SECTION 2 - ROUTE TABLE SANITY'
                                           TO WS-SECTION-TITLE
           PERFORM 7100-SECTION-HEAD
           MOVE ZERO                       TO WS-LISTED
      *
           EXEC SQL
               OPEN ROUTSANE
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'OPEN ROUTSANE FAILED' TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           MOVE LOW-VALUES                 TO WS-PREV-ROUTE-KEY
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               EXEC SQL
                   FETCH ROUTSANE
                    INTO :DCL-ROUTE-TYPE
                       , :DCL-ROUTE-KEY
                       , :DCL-SEQ-NBR
                       , :DCL-PGM-NAME
                       , :DCL-EXP-DATE
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 3100-CHECK-ROUTE
                   WHEN +100
                       MOVE 'Y'            TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'FETCH ROUTSANE FAILED'
                                           TO WS-MSG
                       PERFORM 9400-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE ROUTSANE
           END-EXEC
      *
           COMPUTE WS-ROUTE-TOTAL = WS-ROUTE-NO-PGM
                                  + WS-ROUTE-EXPIRED
                                  + WS-ROUTE-GAPS
      *
           MOVE 'ACTIVE ROUTES WITH NO PROGRAM NAME'
                                           TO RC-TEXT
           MOVE WS-ROUTE-NO-PGM            TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           MOVE 'EXPIRED ROUTES STILL FLAGGED ACTIVE'
                                           TO RC-TEXT
           MOVE WS-ROUTE-EXPIRED           TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           MOVE 'GAPS IN ROUTE SEQUENCE NUMBERS'
                                           TO RC-TEXT
           MOVE WS-ROUTE-GAPS              TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           IF WS-ROUTE-TOTAL > WS-ROUTE-TOL
               PERFORM 8100-RAISE-EIGHT
           ELSE
               IF WS-ROUTE-TOTAL > ZERO
                   PERFORM 8200-RAISE-FOUR
               END-IF
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
       3100-CHECK-ROUTE.
           IF DCL-ROUTE-TYPE NOT = WS-PREV-ROUTE-KEY(1:4)
           OR DCL-ROUTE-KEY  NOT = WS-PREV-ROUTE-KEY(5:8)
               MOVE DCL-ROUTE-TYPE         TO WS-PREV-ROUTE-KEY(1:4)
               MOVE DCL-ROUTE-KEY          TO WS-PREV-ROUTE-KEY(5:8)
               MOVE 1                      TO WS-EXPECTED-SEQ
           END-IF
      *
           IF DCL-SEQ-NBR NOT = WS-EXPECTED-SEQ
               ADD 1                       TO WS-ROUTE-GAPS
               IF WS-LISTED < WS-LIST-LIMIT
                   ADD 1                   TO WS-LISTED
                   PERFORM 3200-MOVE-KEY
                   MOVE SPACES             TO RD-TEXT
                   MOVE DCL-SEQ-NBR        TO WS-SEQ-DISP
                   STRING 'SEQUENCE '
                          WS-SEQ-DISP
                          ' FOUND WHERE '
                          WS-EXPECTED-SEQ
                          ' WAS EXPECTED'
                       DELIMITED BY SIZE
                       INTO RD-TEXT
                   END-STRING
                   PERFORM 7200-DETAIL-LINE
               END-IF
               MOVE DCL-SEQ-NBR            TO WS-EXPECTED-SEQ
           END-IF
           ADD 1                           TO WS-EXPECTED-SEQ
      *
           IF DCL-PGM-NAME = SPACES
               ADD 1                       TO WS-ROUTE-NO-PGM
               IF WS-LISTED < WS-LIST-LIMIT
                   ADD 1                   TO WS-LISTED
                   PERFORM 3200-MOVE-KEY
                   MOVE 'ACTIVE ROUTE HAS A BLANK PROGRAM NAME'
                                           TO RD-TEXT
                   PERFORM 7200-DETAIL-LINE
               END-IF
           END-IF
      *
           MOVE DCL-EXP-DATE(1:4)          TO WS-DW-YYYY
           MOVE DCL-EXP-DATE(6:2)          TO WS-DW-MM
           MOVE DCL-EXP-DATE(9:2)          TO WS-DW-DD
      *
           IF WS-DATE-WORK < WS-RUN-DATE
               ADD 1                       TO WS-ROUTE-EXPIRED
               IF WS-LISTED < WS-LIST-LIMIT
                   ADD 1                   TO WS-LISTED
                   PERFORM 3200-MOVE-KEY
                   MOVE SPACES             TO RD-TEXT
                   STRING 'EXPIRED '
                          DCL-EXP-DATE
                          ' AND STILL FLAGGED ACTIVE'
                       DELIMITED BY SIZE
                       INTO RD-TEXT
                   END-STRING
                   PERFORM 7200-DETAIL-LINE
               END-IF
           END-IF
           .
      *
       3200-MOVE-KEY.
           MOVE SPACES                     TO RD-KEY
           MOVE DCL-ROUTE-TYPE             TO RD-KEY(1:4)
           MOVE DCL-ROUTE-KEY              TO RD-KEY(6:8)
           MOVE DCL-SEQ-NBR                TO WS-SEQ-DISP
           MOVE WS-SEQ-DISP                TO RD-KEY(15:4)
           .
      *
      ******************************************************************
      * 4000 - VSAM AGAINST DB2                                        *
      *                                                                *
      * THE CLUSTERS WERE BUILT EARLIER IN THE SAME CHAIN AND VERIFIED *
      * AT THE TIME.  THIS PASS CATCHES THE CASE WHERE SOMEBODY HAS    *
      * MAINTAINED THE TABLE AFTERWARDS WITHOUT REBUILDING, WHICH THE  *
      * REBUILD STEPS THEMSELVES CANNOT SEE.                           *
      ******************************************************************
       4000-VSAM-SECTION.
           MOVE 'SECTION 3 - VSAM AGAINST DB2 COUNTS'
                                           TO WS-SECTION-TITLE
           PERFORM 7100-SECTION-HEAD
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-ROW-CNT
                 FROM CARDSVC.MERCHANT
                WHERE STATUS IN ('A','S')
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'COUNT OF CARDSVC.MERCHANT FAILED'
                                           TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 4000-EXIT
           END-IF
           MOVE DCL-ROW-CNT                TO WS-MERCH-DB2
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-ROW-CNT
                 FROM CARDSVC.PGM_ROUTE
                WHERE ACTIVE_FLG = 'Y'
                  AND DATE(:DCL-RUN-DATE)
                      BETWEEN EFF_DATE AND EXP_DATE
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'COUNT OF CARDSVC.PGM_ROUTE FAILED'
                                           TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 4000-EXIT
           END-IF
           MOVE DCL-ROW-CNT                TO WS-ROUTE-DB2
      *
           PERFORM 4100-COUNT-MERCHRTE
           IF NOT WS-FATAL
               PERFORM 4200-COUNT-PGMROUT
           END-IF
      *
           MOVE 'MERCHRTE    '             TO RM-NAME
           MOVE WS-MERCH-DB2               TO RM-DB2
           MOVE WS-MERCH-VSAM              TO RM-VSAM
           IF WS-MERCH-DB2 = WS-MERCH-VSAM
               MOVE 'IN STEP'              TO RM-FLAG
           ELSE
               MOVE 'OUT OF STEP'          TO RM-FLAG
               PERFORM 8100-RAISE-EIGHT
           END-IF
           MOVE RL-COMPARE                 TO REFRPT-REC
           PERFORM 7400-WRITE-LINE
      *
           MOVE 'PGMROUT     '             TO RM-NAME
           MOVE WS-ROUTE-DB2               TO RM-DB2
           MOVE WS-ROUTE-VSAM              TO RM-VSAM
           IF WS-ROUTE-DB2 = WS-ROUTE-VSAM
               MOVE 'IN STEP'              TO RM-FLAG
           ELSE
               MOVE 'OUT OF STEP'          TO RM-FLAG
               PERFORM 8100-RAISE-EIGHT
           END-IF
           MOVE RL-COMPARE                 TO REFRPT-REC
           PERFORM 7400-WRITE-LINE
           .
       4000-EXIT.
           EXIT
           .
      *
       4100-COUNT-MERCHRTE.
           OPEN INPUT MERCHRTE-FILE
           IF NOT WS-MRT-OK
               MOVE 'MERCHRTE OPEN FAILED' TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 4100-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               READ MERCHRTE-FILE NEXT RECORD
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-MERCH-VSAM
               END-READ
      *
               IF NOT WS-MRT-OK
               AND NOT WS-MRT-EOF
                   MOVE 'MERCHRTE READ FAILED'
                                           TO WS-MSG
                   PERFORM 9300-FILE-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE MERCHRTE-FILE
           .
       4100-EXIT.
           EXIT
           .
      *
       4200-COUNT-PGMROUT.
           OPEN INPUT PGMROUT-FILE
           IF NOT WS-PGR-OK
               MOVE 'PGMROUT OPEN FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 4200-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               READ PGMROUT-FILE NEXT RECORD
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-ROUTE-VSAM
               END-READ
      *
               IF NOT WS-PGR-OK
               AND NOT WS-PGR-EOF
                   MOVE 'PGMROUT READ FAILED'
                                           TO WS-MSG
                   PERFORM 9300-FILE-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE PGMROUT-FILE
           .
       4200-EXIT.
           EXIT
           .
      *
       7100-SECTION-HEAD.
           IF WS-LINE-CNT > 50
               PERFORM 7000-PAGE-HEAD
           END-IF
           MOVE WS-SECTION-TITLE           TO RS-TITLE
           MOVE RL-SECTION                 TO REFRPT-REC
           PERFORM 7400-WRITE-LINE
           .
      *
       7000-PAGE-HEAD.
           ADD 1                           TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT                TO RH1-PAGE
           MOVE RL-HEAD1                   TO REFRPT-REC
           WRITE REFRPT-REC
           MOVE 3                          TO WS-LINE-CNT
           .
      *
       7200-DETAIL-LINE.
           IF WS-LINE-CNT > 58
               PERFORM 7000-PAGE-HEAD
           END-IF
           MOVE RL-DETAIL                  TO REFRPT-REC
           PERFORM 7400-WRITE-LINE
           .
      *
       7300-COUNT-LINE.
           IF WS-LINE-CNT > 58
               PERFORM 7000-PAGE-HEAD
           END-IF
           IF RC-VALUE = ZERO
               MOVE SPACES                 TO RC-FLAG
           ELSE
               MOVE '** REVIEW **'         TO RC-FLAG
           END-IF
           MOVE RL-COUNT                   TO REFRPT-REC
           PERFORM 7400-WRITE-LINE
           .
      *
       7400-WRITE-LINE.
           WRITE REFRPT-REC
           IF NOT WS-RPT-OK
               MOVE 'REFRPT WRITE FAILED'  TO WS-MSG
               PERFORM 9300-FILE-ERROR
           END-IF
           ADD 1                           TO WS-LINE-CNT
           .
      *
       8000-SUMMARY.
           MOVE 'SECTION 4 - SUMMARY'      TO WS-SECTION-TITLE
           PERFORM 7100-SECTION-HEAD
      *
           MOVE 'TOTAL ORPHANED ROWS'      TO RC-TEXT
           MOVE WS-ORPHAN-TOTAL            TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           MOVE 'TOTAL ROUTE TABLE FINDINGS'
                                           TO RC-TEXT
           MOVE WS-ROUTE-TOTAL             TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           MOVE 'HIGHEST SEVERITY REACHED' TO RC-TEXT
           MOVE WS-SEVERITY                TO RC-VALUE
           PERFORM 7300-COUNT-LINE
      *
           CLOSE REFRPT-FILE
      *
           IF WS-FATAL
               MOVE 12                     TO WS-RETURN-CD
           ELSE
               MOVE WS-SEVERITY            TO WS-RETURN-CD
           END-IF
      *
           DISPLAY 'CBREF05 ORPHANS=' WS-ORPHAN-TOTAL
                   ' ROUTE FINDINGS=' WS-ROUTE-TOTAL
                   ' SEVERITY=' WS-SEVERITY
           .
      *
       8100-RAISE-EIGHT.
           IF WS-SEVERITY < 8
               MOVE 8                      TO WS-SEVERITY
           END-IF
           .
      *
       8200-RAISE-FOUR.
           IF WS-SEVERITY < 4
               MOVE 4                      TO WS-SEVERITY
           END-IF
           .
      *
       9200-FATAL.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF05 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'DATA'                     TO ER-ERROR-TYPE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U941'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF05 FATAL - ' WS-MSG
           .
      *
       9300-FILE-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF05 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'VSAM'                     TO ER-ERROR-TYPE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U941'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF05 FILE ERROR - ' WS-MSG
                   ' PARM=' WS-PARM-STATUS
                   ' RPT=' WS-RPT-STATUS
                   ' MERCHRTE=' WS-MRT-STATUS
                   ' PGMROUT=' WS-PGR-STATUS
           .
      *
       9400-SQL-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF05 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U942'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF05 SQL ERROR SQLCODE=' SQLCODE
                   ' - ' WS-MSG
           .
      *
       9500-ABEND.
           MOVE 12                         TO WS-RETURN-CD
           CALL 'CBCRD91' USING ERROR-AREA
           .
