      ******************************************************************
      * CBPRT01 - WEEKLY KYC REFRESH                                   *
      *                                                                *
      * PART OF THE PARTYWK WEEKLY CYCLE, STEP ONE.                    *
      *                                                                *
      * FINDS EVERY ACTIVE PARTY WHOSE KYC REVIEW HAS FALLEN DUE,      *
      * RAISES A PENDING REVIEW ROW ON PARTYRSK.PARTY_KYC AND WRITES   *
      * A WORK LIST FOR THE FINANCIAL CRIME OPERATIONS TEAM.           *
      *                                                                *
      * THE REVIEW CYCLE IS DRIVEN OFF THE KYC LEVEL                   *
      *   SIMP  SIMPLIFIED     60 MONTHS                               *
      *   STAN  STANDARD       36 MONTHS                               *
      *   ENHA  ENHANCED       12 MONTHS                               *
      * A PARTY FLAGGED FOR ENHANCED DUE DILIGENCE OR CARRYING THE     *
      * PEP MARKER IS ALWAYS TREATED AS ENHANCED WHATEVER THE LEVEL    *
      * ON THE ROW SAYS.                                               *
      *                                                                *
      * RUN BY     - CBPRT01J                                          *
      * FILES      - KYCWORK   WORK LIST, FB 133                       *
      * TABLES     - PARTYRSK.PARTY_KYC     SELECT / INSERT            *
      *              PARTYRSK.CUSTOMER      SELECT                     *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - COMPLETED, WORK LIST WRITTEN                            *
      *   04 - COMPLETED WITH REJECTS - SEE THE SYSOUT SUMMARY         *
      *   12 - FAILED - NOTHING COMMITTED SINCE THE LAST CHECKPOINT    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBPRT01.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KYCWORK-FILE ASSIGN TO KYCWORK
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-WORK-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KYCWORK-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  KYCWORK-REC                 PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CBPRT01 '.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
       01  WS-JOB-NAME                 PIC X(8)  VALUE 'CBPRT01J'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X     VALUE 'N'.
               88  WS-EOF                        VALUE 'Y'.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
           05  WS-WORK-OPEN-SW         PIC X     VALUE 'N'.
               88  WS-WORK-OPEN                  VALUE 'Y'.
      *
       01  WS-WORK-STATUS              PIC X(2)  VALUE '00'.
      *
       01  WS-TOTALS.
           05  WS-READ-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-RAISED-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-SKIPPED-CNT          PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-WRITTEN-CNT          PIC 9(9)  VALUE ZERO.
           05  WS-COMMIT-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT         PIC 9(9)  VALUE ZERO.
           05  WS-EDD-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-PEP-CNT              PIC 9(9)  VALUE ZERO.
      *
       01  WS-EDIT-FIELDS.
           05  WS-ED-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-ED-DAYS              PIC ---,--9.
           05  WS-SQL-DISP             PIC -(9)9.
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-DATE-R REDEFINES WS-CURR-DATE.
               10  WS-CURR-CCYY        PIC 9(4).
               10  WS-CURR-MM          PIC 9(2).
               10  WS-CURR-DD          PIC 9(2).
           05  WS-CURR-TIME            PIC 9(8)  VALUE ZERO.
           05  WS-DATE-ISO             PIC X(10) VALUE SPACES.
      *
       01  WS-REVIEW-WORK.
           05  WS-MONTHS-ADD           PIC S9(4) COMP VALUE ZERO.
           05  WS-NEW-SEQ              PIC S9(4) COMP VALUE ZERO.
           05  WS-EFF-LEVEL            PIC X(4)  VALUE SPACES.
           05  WS-REVIEW-TYPE          PIC X(4)  VALUE SPACES.
      *
      ******************************************************************
      * WORK LIST RECORD.  READ BY THE OPERATIONS CASE TOOL, SO THE    *
      * COLUMN POSITIONS MUST NOT MOVE.                                *
      ******************************************************************
       01  WS-WORK-LINE.
           05  WL-PARTY-ID             PIC X(11).
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-LEGAL-NAME           PIC X(60).
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-KYC-LEVEL            PIC X(4).
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-REVIEW-TYPE          PIC X(4).
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-DUE-DATE             PIC X(10).
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-DAYS-OVERDUE         PIC ZZZZ9.
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-RISK-RATING          PIC X.
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-PEP-FLG              PIC X.
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-EDD-FLG              PIC X.
           05  FILLER                  PIC X     VALUE SPACES.
           05  WL-NEW-SEQ              PIC ZZZZ9.
           05  FILLER                  PIC X(24) VALUE SPACES.
      *
      *    HOST VARIABLES
       01  DCL-KYC.
           05  DCL-PARTY-ID            PIC X(11).
           05  DCL-KYC-SEQ             PIC S9(4) COMP.
           05  DCL-KYC-STATUS          PIC X(2).
           05  DCL-KYC-LEVEL           PIC X(4).
           05  DCL-REVIEW-TYPE         PIC X(4).
           05  DCL-REVIEW-DATE         PIC X(10).
           05  DCL-NEXT-REVIEW         PIC X(10).
           05  DCL-EXPIRY-DATE         PIC X(10).
           05  DCL-ID-DOC-TYPE         PIC X(4).
           05  DCL-ID-DOC-REF          PIC X(20).
           05  DCL-RISK-RATING         PIC X(1).
           05  DCL-EDD-FLG             PIC X(1).
           05  DCL-ADDR-VERIFIED       PIC X(1).
           05  DCL-SOF-CD              PIC X(4).
           05  DCL-DAYS-OVERDUE        PIC S9(9) COMP.
           05  DCL-LEGAL-NAME          PIC X(60).
           05  DCL-PEP-FLG             PIC X(1).
           05  DCL-MAX-SEQ             PIC S9(4) COMP.
           05  DCL-NEW-NEXT-REVIEW     PIC X(10).
           05  DCL-REVIEW-NOTES        PIC X(200).
      *
       01  DCL-IND.
           05  IND-EXPIRY              PIC S9(4) COMP.
           05  IND-RISK-RATING         PIC S9(4) COMP.
           05  IND-DOC-TYPE            PIC S9(4) COMP.
           05  IND-DOC-REF             PIC S9(4) COMP.
           05  IND-SOF                 PIC S9(4) COMP.
           05  IND-LEGAL-NAME          PIC S9(4) COMP.
           05  IND-MAX-SEQ             PIC S9(4) COMP.
      *
       01  DCL-NOTES.
           49  DCL-NOTES-LEN           PIC S9(4) COMP.
           49  DCL-NOTES-TXT           PIC X(200).
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      ******************************************************************
      * DUE REVIEWS.  THE LATEST KYC ROW PER PARTY, WHERE THE NEXT     *
      * REVIEW DATE HAS PASSED AND NO REFRESH IS ALREADY PENDING.      *
      ******************************************************************
           EXEC SQL DECLARE KYCDUECSR CURSOR FOR
               SELECT K.PARTY_ID
                    , K.KYC_SEQ
                    , K.KYC_STATUS
                    , K.KYC_LEVEL
                    , K.REVIEW_TYPE
                    , CHAR(K.REVIEW_DATE, ISO)
                    , CHAR(K.NEXT_REVIEW_DATE, ISO)
                    , CHAR(K.EXPIRY_DATE, ISO)
                    , K.ID_DOC_TYPE
                    , K.ID_DOC_REF
                    , K.RISK_RATING
                    , K.ENHANCED_DD_FLG
                    , K.ADDR_VERIFIED_FLG
                    , K.SOURCE_OF_FUNDS_CD
                    , DAYS(CURRENT DATE) - DAYS(K.NEXT_REVIEW_DATE)
                    , C.LEGAL_NAME
                    , C.PEP_FLG
                 FROM PARTYRSK.PARTY_KYC K
                    , PARTYRSK.CUSTOMER  C
                WHERE K.PARTY_ID = C.PARTY_ID
                  AND C.CUST_STATUS = 'A'
                  AND K.NEXT_REVIEW_DATE < CURRENT DATE
                  AND K.KYC_STATUS IN ('OK','EX')
                  AND K.KYC_SEQ =
                     (SELECT MAX(K2.KYC_SEQ)
                        FROM PARTYRSK.PARTY_KYC K2
                       WHERE K2.PARTY_ID = K.PARTY_ID)
                  AND C.CUST_ID =
                     (SELECT MIN(C2.CUST_ID)
                        FROM PARTYRSK.CUSTOMER C2
                       WHERE C2.PARTY_ID = K.PARTY_ID)
                ORDER BY K.PARTY_ID
                WITH UR
           END-EXEC.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
           PERFORM 1000-OPEN-CURSOR
      *
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               PERFORM 2000-PROCESS-ONE-PARTY
           END-PERFORM
      *
           PERFORM 3000-CLOSE-DOWN
           PERFORM 9000-REPORT-TOTALS
           GOBACK
           .
      *
       0100-INITIALISE.
           MOVE '0100-INITIALISE'      TO WS-PARAGRAPH
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           ACCEPT WS-CURR-TIME         FROM TIME
      *
           STRING WS-CURR-CCYY '-' WS-CURR-MM '-' WS-CURR-DD
                  DELIMITED BY SIZE INTO WS-DATE-ISO
      *
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
      *
           OPEN OUTPUT KYCWORK-FILE
           IF WS-WORK-STATUS NOT = '00'
               MOVE 'KYCWORK '         TO ER-FILE-NAME
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9200-VSAM-ERROR
               PERFORM 9900-ABEND
           END-IF
           MOVE 'Y'                    TO WS-WORK-OPEN-SW
      *
           DISPLAY 'CBPRT01  KYC REFRESH STARTED  DATE=' WS-DATE-ISO
           DISPLAY 'CBPRT01  COMMIT FREQUENCY=' WS-COMMIT-FREQUENCY
           .
      *
       1000-OPEN-CURSOR.
           MOVE '1000-OPEN-CURSOR'     TO WS-PARAGRAPH
      *
           EXEC SQL
               OPEN KYCDUECSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               PERFORM 9900-ABEND
           END-IF
           .
      *
       2000-PROCESS-ONE-PARTY.
           MOVE '2000-PROCESS-ONE-PARTY' TO WS-PARAGRAPH
      *
           PERFORM 2100-FETCH-DUE
           IF WS-EOF OR WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           ADD 1                       TO WS-READ-CNT
      *
           PERFORM 2200-DETERMINE-LEVEL
           PERFORM 2300-NEXT-SEQUENCE
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           IF WS-NEW-SEQ > 999
      *        THE PARTY HAS BEEN REVIEWED 999 TIMES.  THAT IS BAD
      *        DATA, NOT A REAL REVIEW HISTORY.
               ADD 1                   TO WS-REJECT-CNT
               DISPLAY 'CBPRT01  SEQUENCE EXHAUSTED PARTY='
                       DCL-PARTY-ID ' SEQ=' WS-NEW-SEQ
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2400-RAISE-PENDING
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2500-WRITE-WORK-LINE
           PERFORM 2900-CHECKPOINT
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-FETCH-DUE.
           EXEC SQL
               FETCH KYCDUECSR
                INTO :DCL-PARTY-ID
                   , :DCL-KYC-SEQ
                   , :DCL-KYC-STATUS
                   , :DCL-KYC-LEVEL
                   , :DCL-REVIEW-TYPE
                   , :DCL-REVIEW-DATE
                   , :DCL-NEXT-REVIEW
                   , :DCL-EXPIRY-DATE  :IND-EXPIRY
                   , :DCL-ID-DOC-TYPE  :IND-DOC-TYPE
                   , :DCL-ID-DOC-REF   :IND-DOC-REF
                   , :DCL-RISK-RATING  :IND-RISK-RATING
                   , :DCL-EDD-FLG
                   , :DCL-ADDR-VERIFIED
                   , :DCL-SOF-CD       :IND-SOF
                   , :DCL-DAYS-OVERDUE
                   , :DCL-LEGAL-NAME   :IND-LEGAL-NAME
                   , :DCL-PEP-FLG
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2200-DETERMINE-LEVEL.
           MOVE DCL-KYC-LEVEL          TO WS-EFF-LEVEL
      *
           IF DCL-EDD-FLG = 'Y'
               MOVE 'ENHA'             TO WS-EFF-LEVEL
               ADD 1                   TO WS-EDD-CNT
           END-IF
           IF DCL-PEP-FLG = 'Y'
               MOVE 'ENHA'             TO WS-EFF-LEVEL
               ADD 1                   TO WS-PEP-CNT
           END-IF
           IF DCL-RISK-RATING = 'C' OR DCL-RISK-RATING = 'X'
               MOVE 'ENHA'             TO WS-EFF-LEVEL
           END-IF
      *
           EVALUATE WS-EFF-LEVEL
               WHEN 'SIMP'
                   MOVE 60             TO WS-MONTHS-ADD
                   MOVE 'PERI'         TO WS-REVIEW-TYPE
               WHEN 'ENHA'
                   MOVE 12             TO WS-MONTHS-ADD
                   MOVE 'EDDR'         TO WS-REVIEW-TYPE
               WHEN OTHER
                   MOVE 36             TO WS-MONTHS-ADD
                   MOVE 'PERI'         TO WS-REVIEW-TYPE
           END-EVALUATE
      *
      *    A REVIEW THAT IS MORE THAN TWO YEARS LATE IS TREATED AS A
      *    REMEDIATION CASE AND GETS THE SHORT CYCLE WHATEVER LEVEL
      *    THE PARTY SITS AT.
           IF DCL-DAYS-OVERDUE > 730
               MOVE 12                 TO WS-MONTHS-ADD
               MOVE 'REMD'             TO WS-REVIEW-TYPE
           END-IF
           .
      *
       2300-NEXT-SEQUENCE.
           EXEC SQL
               SELECT MAX(KYC_SEQ)
                 INTO :DCL-MAX-SEQ :IND-MAX-SEQ
                 FROM PARTYRSK.PARTY_KYC
                WHERE PARTY_ID = :DCL-PARTY-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF IND-MAX-SEQ < ZERO
                       MOVE 1          TO WS-NEW-SEQ
                   ELSE
                       COMPUTE WS-NEW-SEQ = DCL-MAX-SEQ + 1
                   END-IF
               WHEN +100
                   MOVE 1              TO WS-NEW-SEQ
               WHEN OTHER
                   MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
                   MOVE 'SELMAX  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2400-RAISE-PENDING.
           MOVE '2400-RAISE-PENDING'   TO WS-PARAGRAPH
      *
           MOVE SPACES                 TO DCL-NOTES-TXT
           STRING 'RAISED BY ' WS-JOB-NAME
                  ' ON ' WS-DATE-ISO
                  ' PRIOR SEQ ' DCL-KYC-SEQ
                  ' OVERDUE DAYS ' DCL-DAYS-OVERDUE
                  DELIMITED BY SIZE INTO DCL-NOTES-TXT
           MOVE 80                     TO DCL-NOTES-LEN
      *
           EXEC SQL
               INSERT INTO PARTYRSK.PARTY_KYC
                     (PARTY_ID
                    , KYC_SEQ
                    , KYC_STATUS
                    , KYC_LEVEL
                    , REVIEW_TYPE
                    , REVIEW_DATE
                    , NEXT_REVIEW_DATE
                    , EXPIRY_DATE
                    , ID_DOC_TYPE
                    , ID_DOC_REF
                    , ADDR_VERIFIED_FLG
                    , SOURCE_OF_FUNDS_CD
                    , RISK_RATING
                    , ENHANCED_DD_FLG
                    , REVIEWED_BY
                    , REVIEW_NOTES)
               VALUES (:DCL-PARTY-ID
                    , :WS-NEW-SEQ
                    , 'PN'
                    , :WS-EFF-LEVEL
                    , :WS-REVIEW-TYPE
                    , CURRENT DATE
                    , CURRENT DATE + :WS-MONTHS-ADD MONTHS
                    , :DCL-EXPIRY-DATE  :IND-EXPIRY
                    , :DCL-ID-DOC-TYPE  :IND-DOC-TYPE
                    , :DCL-ID-DOC-REF   :IND-DOC-REF
                    , :DCL-ADDR-VERIFIED
                    , :DCL-SOF-CD       :IND-SOF
                    , :DCL-RISK-RATING  :IND-RISK-RATING
                    , :DCL-EDD-FLG
                    , :WS-JOB-NAME
                    , :DCL-NOTES)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-RAISED-CNT
               WHEN -803
      *            ANOTHER RUN OF THE CYCLE ALREADY RAISED IT.
                   ADD 1               TO WS-SKIPPED-CNT
                   DISPLAY 'CBPRT01  REVIEW ALREADY RAISED PARTY='
                           DCL-PARTY-ID ' SEQ=' WS-NEW-SEQ
               WHEN -530
      *            THE CUSTOMER ROW WENT AWAY UNDER US.
                   ADD 1               TO WS-REJECT-CNT
                   DISPLAY 'CBPRT01  PARENT MISSING PARTY='
                           DCL-PARTY-ID
               WHEN OTHER
                   MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2500-WRITE-WORK-LINE.
           MOVE '2500-WRITE-WORK-LINE' TO WS-PARAGRAPH
           MOVE SPACES                 TO WS-WORK-LINE
      *
           MOVE DCL-PARTY-ID           TO WL-PARTY-ID
           IF IND-LEGAL-NAME < ZERO
               MOVE '*** NAME NOT ON CUSTOMER ROW ***'
                                       TO WL-LEGAL-NAME
           ELSE
               MOVE DCL-LEGAL-NAME     TO WL-LEGAL-NAME
           END-IF
           MOVE WS-EFF-LEVEL           TO WL-KYC-LEVEL
           MOVE WS-REVIEW-TYPE         TO WL-REVIEW-TYPE
           MOVE DCL-NEXT-REVIEW        TO WL-DUE-DATE
           MOVE DCL-DAYS-OVERDUE       TO WL-DAYS-OVERDUE
           IF IND-RISK-RATING < ZERO
               MOVE '?'                TO WL-RISK-RATING
           ELSE
               MOVE DCL-RISK-RATING    TO WL-RISK-RATING
           END-IF
           MOVE DCL-PEP-FLG            TO WL-PEP-FLG
           MOVE DCL-EDD-FLG            TO WL-EDD-FLG
           MOVE WS-NEW-SEQ             TO WL-NEW-SEQ
      *
           WRITE KYCWORK-REC FROM WS-WORK-LINE
           IF WS-WORK-STATUS NOT = '00'
               MOVE 'KYCWORK '         TO ER-FILE-NAME
               MOVE 'WRITE   '         TO ER-SQL-OPERATION
               PERFORM 9200-VSAM-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           ADD 1                       TO WS-WRITTEN-CNT
           .
      *
       2900-CHECKPOINT.
           ADD 1                       TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT < WS-COMMIT-FREQUENCY
               GO TO 2900-EXIT
           END-IF
      *
      *    THE CURSOR IS DECLARED WITH HOLD IN THE BIND SO THE COMMIT
      *    DOES NOT LOSE POSITION.
           EXEC SQL
               COMMIT
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'COMMIT           ' TO ER-SQL-TABLE
               MOVE 'COMMIT  '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           ADD 1                       TO WS-COMMIT-CNT
           MOVE ZERO                   TO WS-SINCE-COMMIT
           MOVE WS-READ-CNT            TO WS-ED-COUNT
           DISPLAY 'CBPRT01  CHECKPOINT ' WS-COMMIT-CNT
                   ' READ=' WS-ED-COUNT
                   ' LAST PARTY=' DCL-PARTY-ID
           .
       2900-EXIT.
           EXIT
           .
      *
       3000-CLOSE-DOWN.
           MOVE '3000-CLOSE-DOWN'      TO WS-PARAGRAPH
      *
           EXEC SQL
               CLOSE KYCDUECSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
           END-IF
      *
           IF WS-FATAL
               EXEC SQL
                   ROLLBACK
               END-EXEC
           ELSE
               EXEC SQL
                   COMMIT
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'COMMIT           ' TO ER-SQL-TABLE
                   MOVE 'COMMIT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
               END-IF
           END-IF
      *
           IF WS-WORK-OPEN
               CLOSE KYCWORK-FILE
               IF WS-WORK-STATUS NOT = '00'
                   MOVE 'KYCWORK '     TO ER-FILE-NAME
                   MOVE 'CLOSE   '     TO ER-SQL-OPERATION
                   PERFORM 9200-VSAM-ERROR
               END-IF
           END-IF
           .
      *
       9000-REPORT-TOTALS.
           DISPLAY '******************************************'
           DISPLAY 'CBPRT01  KYC REFRESH SUMMARY'
           MOVE WS-READ-CNT            TO WS-ED-COUNT
           DISPLAY '   REVIEWS DUE        ' WS-ED-COUNT
           MOVE WS-RAISED-CNT          TO WS-ED-COUNT
           DISPLAY '   PENDING ROWS ADDED ' WS-ED-COUNT
           MOVE WS-WRITTEN-CNT         TO WS-ED-COUNT
           DISPLAY '   WORK LIST LINES    ' WS-ED-COUNT
           MOVE WS-SKIPPED-CNT         TO WS-ED-COUNT
           DISPLAY '   ALREADY RAISED     ' WS-ED-COUNT
           MOVE WS-REJECT-CNT          TO WS-ED-COUNT
           DISPLAY '   REJECTED           ' WS-ED-COUNT
           MOVE WS-EDD-CNT             TO WS-ED-COUNT
           DISPLAY '   ENHANCED DD CASES  ' WS-ED-COUNT
           MOVE WS-PEP-CNT             TO WS-ED-COUNT
           DISPLAY '   PEP CASES          ' WS-ED-COUNT
           DISPLAY '   COMMITS TAKEN      ' WS-COMMIT-CNT
           DISPLAY '******************************************'
      *
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12             TO RETURN-CODE
               WHEN WS-REJECT-CNT > ZERO
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
           DISPLAY 'CBPRT01  SQL ERROR PARA=' WS-PARAGRAPH
                   ' TABLE=' ER-SQL-TABLE
           DISPLAY '         OP=' ER-SQL-OPERATION
                   ' SQLCODE=' WS-SQL-DISP
                   ' PARTY=' DCL-PARTY-ID
           DISPLAY '         SQLERRMC=' SQLERRMC(1:44)
           .
      *
       9200-VSAM-ERROR.
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE WS-WORK-STATUS         TO ER-FILE-STATUS
           MOVE 'F'                    TO ER-SEVERITY
           DISPLAY 'CBPRT01  FILE ERROR ' ER-FILE-NAME
                   ' OP=' ER-SQL-OPERATION
                   ' STATUS=' WS-WORK-STATUS
           .
      *
      ******************************************************************
      * 9900 - U3111.  THE WORK LIST IS AN INPUT TO THE OPERATIONS     *
      *        CASE TOOL, SO A PARTIAL FILE IS WORSE THAN NO FILE.     *
      ******************************************************************
       9900-ABEND.
           MOVE 'U311'                 TO ER-ABEND-CODE
           EXEC SQL
               ROLLBACK
           END-EXEC
           DISPLAY 'CBPRT01  ABEND U3111 PARA=' WS-PARAGRAPH
                   ' PARTY=' DCL-PARTY-ID
           MOVE 12                     TO RETURN-CODE
           STOP RUN
           .
