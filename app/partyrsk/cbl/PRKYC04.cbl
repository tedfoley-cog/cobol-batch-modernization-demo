      ******************************************************************
      * PRKYC04 - PARTY DETAIL AND KYC HISTORY ASSEMBLY                *
      *                                                                *
      * BUILDS THE PARTY RECORD FOR THE INQUIRY CROSSING - THE PARTY   *
      * ITSELF, EVERY CUSTOMER NUMBER LINKED TO IT ACROSS THE PRODUCT  *
      * SYSTEMS, THE NAME VARIATIONS THOSE CUSTOMER RECORDS CARRY, AND *
      * THE LAST FIVE KYC REVIEWS.                                     *
      *                                                                *
      * THE PARTY RECORD IS NOT RETURNED TO THE CALLER - THE CROSSING  *
      * COMMAREA IS ONLY 512 BYTES AND CANNOT HOLD IT.  WHAT GOES BACK *
      * IS THE SUMMARY IN CV-RISK-OUT.  THE FULL RECORD IS BUILT HERE  *
      * BECAUSE THE SUMMARY CANNOT BE DERIVED WITHOUT IT.              *
      *                                                                *
      * CALLED BY   - PRKYC03                                          *
      * CALLS       - PRRSK05  SCORE AND EXPOSURE SUMMARY              *
      *               PRERR01  PARTYRSK ERROR HANDLER                  *
      * TABLES      - PARTYRSK.CUSTOMER              (SELECT, CURSOR)  *
      *               PARTYRSK.PARTY_KYC             (CURSOR)          *
      * COMMAREA    - CV-RISK-AREA, 512 BYTES, CVRISK01Y               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRKYC04.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRKYC04 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'PRRSK05 '.
       01  WS-ERROR-PGM                PIC X(8)  VALUE 'PRERR01 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-COMMAREA-LEN             PIC S9(4) COMP VALUE 512.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-LINK-CSR-SW          PIC X     VALUE 'N'.
               88  WS-LINK-CSR-OPEN              VALUE 'Y'.
           05  WS-KYC-CSR-SW           PIC X     VALUE 'N'.
               88  WS-KYC-CSR-OPEN               VALUE 'Y'.
           05  WS-END-SW               PIC X     VALUE 'N'.
               88  WS-END-OF-CURSOR              VALUE 'Y'.
      *
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(8)  VALUE SPACES.
           05  WS-TIME-HMS             PIC X(6)  VALUE SPACES.
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
       01  WS-WORK-DATE                PIC 9(8)  VALUE ZERO.
       01  WS-WORK-DATE-R REDEFINES WS-WORK-DATE.
           05  WS-WD-CCYY              PIC 9(4).
           05  WS-WD-MM                PIC 9(2).
           05  WS-WD-DD                PIC 9(2).
      *
      ******************************************************************
      * THE PRODUCT SYSTEM IS NOT HELD ON THE CUSTOMER ROW.  IT IS     *
      * TAKEN FROM THE CUSTOMER NUMBER RANGE, WHICH IS THE CONVENTION  *
      * THE PARTY FILE HAS USED SINCE THE 1996 MERGER.  THE RANGES ARE *
      * OWNED BY THE CUSTOMER NUMBER STANDARDS GROUP AND HAVE NOT      *
      * MOVED SINCE - A NEW PRODUCT SYSTEM NEEDS A CODE CHANGE HERE.   *
      ******************************************************************
       01  WS-RANGE-CONST.
           05  FILLER                  PIC X(26)
               VALUE '100000000199999999DEPOSIT '.
           05  FILLER                  PIC X(26)
               VALUE '200000000299999999LOANS   '.
           05  FILLER                  PIC X(26)
               VALUE '300000000399999999MORTGAGE'.
           05  FILLER                  PIC X(26)
               VALUE '700000000799999999CARDS   '.
           05  FILLER                  PIC X(26)
               VALUE '800000000899999999WEALTH  '.
       01  WS-RANGE-TABLE REDEFINES WS-RANGE-CONST.
           05  WS-RANGE-ENTRY OCCURS 5 TIMES
                              INDEXED BY WS-RX.
               10  WS-RG-LOW           PIC 9(9).
               10  WS-RG-HIGH          PIC 9(9).
               10  WS-RG-SYSTEM        PIC X(8).
       01  WS-RANGE-UNKNOWN            PIC X(8)  VALUE 'UNKNOWN '.
      *
      ******************************************************************
      * KYC HISTORY.  ONLY THE LAST FIVE REVIEWS ARE KEPT - THAT IS    *
      * WHAT THE INQUIRY SCREEN SHOWS AND WHAT THE SUMMARY NEEDS.      *
      ******************************************************************
       01  WS-KYC-HISTORY.
           05  WS-KH-CNT               PIC S9(4) COMP VALUE 0.
           05  WS-KH-ENTRY OCCURS 5 TIMES
                           INDEXED BY WS-KX.
               10  WS-KH-SEQ           PIC S9(4) COMP.
               10  WS-KH-STATUS        PIC X(2).
               10  WS-KH-LEVEL         PIC X(4).
               10  WS-KH-REVIEW-TYPE   PIC X(4).
               10  WS-KH-REVIEW-DATE   PIC 9(8).
               10  WS-KH-NEXT-DATE     PIC 9(8).
               10  WS-KH-EXPIRY-DATE   PIC 9(8).
               10  WS-KH-REVIEWED-BY   PIC X(8).
      *
       01  WS-CURRENT-KYC.
           05  WS-CK-STATUS            PIC X(2)  VALUE SPACES.
           05  WS-CK-NEXT-DATE         PIC 9(8)  VALUE ZERO.
           05  WS-CK-EXPIRY-DATE       PIC 9(8)  VALUE ZERO.
      *
       01  WS-SUB                      PIC S9(4) COMP VALUE 0.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
       01  HV-PARTY.
           05  HV-PARTY-ID             PIC X(11).
           05  HV-PARTY-TYPE           PIC X(1).
           05  HV-LEGAL-NAME           PIC X(60).
           05  HV-LAST-NAME            PIC X(25).
           05  HV-FIRST-NAME           PIC X(20).
           05  HV-DOB                  PIC X(10).
           05  HV-NATIONAL-ID          PIC X(11).
           05  HV-TAX-ID               PIC X(15).
           05  HV-DOMICILE-CTRY        PIC X(3).
           05  HV-COUNTRY-CD           PIC X(3).
           05  HV-CITIZENSHIP          PIC X(3).
           05  HV-PEP-FLG              PIC X(1).
           05  HV-CUST-STATUS          PIC X(1).
           05  HV-ONBOARD-DATE         PIC X(10).
           05  HV-CUST-ID              PIC S9(9) COMP-3.
      *
       01  HV-KYC.
           05  HV-KYC-SEQ              PIC S9(4) COMP.
           05  HV-KYC-STATUS           PIC X(2).
           05  HV-KYC-LEVEL            PIC X(4).
           05  HV-REVIEW-TYPE          PIC X(4).
           05  HV-REVIEW-DATE          PIC X(10).
           05  HV-NEXT-REVIEW          PIC X(10).
           05  HV-EXPIRY-DATE          PIC X(10).
           05  HV-REVIEWED-BY          PIC X(8).
      *
       01  HV-INDICATORS.
           05  IND-LEGAL-NAME          PIC S9(4) COMP.
           05  IND-FIRST-NAME          PIC S9(4) COMP.
           05  IND-DOB                 PIC S9(4) COMP.
           05  IND-NATIONAL-ID         PIC S9(4) COMP.
           05  IND-TAX-ID              PIC S9(4) COMP.
           05  IND-DOMICILE            PIC S9(4) COMP.
           05  IND-CITIZENSHIP         PIC S9(4) COMP.
           05  IND-NEXT-REVIEW         PIC S9(4) COMP.
           05  IND-EXPIRY              PIC S9(4) COMP.
           05  IND-REVIEWED-BY         PIC S9(4) COMP.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           COPY CVPARTY1Y.
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
               EXEC CICS ABEND ABCODE('PRC4') NODUMP END-EXEC
           END-IF
      *
           SET ADDRESS OF CV-RISK-AREA TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-INITIALISE
           PERFORM 2000-READ-PARTY
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 2500-BUILD-LINKS
               PERFORM 2800-BUILD-ALIASES
               PERFORM 3000-READ-KYC-HISTORY
               PERFORM 3500-PUBLISH-KYC
           END-IF
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 5000-LINK-SUMMARY
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
      * 1000 - INITIALISE                                              *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-END-SW
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
                     TIME(WS-TIME-HMS)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE WS-DATE-CYMD           TO WS-TODAY
      *
      *    THE ODO COUNTS MUST BE SET BEFORE ANYTHING IS MOVED INTO
      *    THE VARIABLE PART OF THE RECORD.
           MOVE 1                      TO PT-LINK-CNT
           MOVE ZERO                   TO PT-ALIAS-CNT
           MOVE ZERO                   TO WS-KH-CNT
      *
           MOVE SPACES                 TO PT-PARTY-ID
           MOVE SPACES                 TO PT-LEGAL-NAME
           MOVE SPACES                 TO PT-SHORT-NAME
           MOVE ZERO                   TO PT-DOB-INCORP
           MOVE ZERO                   TO PT-ONBOARD-DATE
      *
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
           .
      *
      ******************************************************************
      * 2000 - THE PARTY ITSELF                                        *
      *                                                                *
      * ONE PARTY MAY HOLD SEVERAL CUSTOMER ROWS.  THE OLDEST ACTIVE   *
      * ONE IS THE MASTER - THAT IS THE ROW THE PARTY ATTRIBUTES ARE   *
      * TAKEN FROM AND IT IS WHAT THE COMPLIANCE REVIEWERS SEE.        *
      ******************************************************************
       2000-READ-PARTY.
           EXEC SQL
               SELECT PARTY_TYPE
                    , LEGAL_NAME
                    , LAST_NAME
                    , FIRST_NAME
                    , CHAR(DOB, ISO)
                    , NATIONAL_ID
                    , TAX_ID
                    , DOMICILE_CTRY
                    , COUNTRY_CD
                    , CITIZENSHIP_CTRY
                    , PEP_FLG
                    , CUST_STATUS
                    , CHAR(ONBOARD_DATE, ISO)
                 INTO :HV-PARTY-TYPE
                    , :HV-LEGAL-NAME    :IND-LEGAL-NAME
                    , :HV-LAST-NAME
                    , :HV-FIRST-NAME    :IND-FIRST-NAME
                    , :HV-DOB           :IND-DOB
                    , :HV-NATIONAL-ID   :IND-NATIONAL-ID
                    , :HV-TAX-ID        :IND-TAX-ID
                    , :HV-DOMICILE-CTRY :IND-DOMICILE
                    , :HV-COUNTRY-CD
                    , :HV-CITIZENSHIP   :IND-CITIZENSHIP
                    , :HV-PEP-FLG
                    , :HV-CUST-STATUS
                    , :HV-ONBOARD-DATE
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :HV-PARTY-ID
                ORDER BY CUST_STATUS, ONBOARD_DATE
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 0012           TO CV-RISK-RC
                   MOVE WS-PGM-ID      TO CV-RISK-FAIL-PGM
                   MOVE 'PTY3'         TO CV-RISK-REASON-CD
                   MOVE 'NO CUSTOMER ROW HELD FOR THIS PARTY'
                                       TO CV-RISK-REASON-TXT
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '2000-READ-PARTY'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           MOVE CV-RISK-PARTY-ID       TO PT-PARTY-ID
           MOVE HV-PARTY-TYPE          TO PT-PARTY-TYPE
      *
           IF IND-LEGAL-NAME < ZERO
              OR HV-LEGAL-NAME = SPACES
      *        AN INDIVIDUAL HAS NO LEGAL NAME ON FILE - IT IS MADE
      *        UP FROM THE NAME PARTS THE WAY THE STATEMENTS DO IT.
               MOVE SPACES             TO PT-LEGAL-NAME
               STRING HV-FIRST-NAME    DELIMITED BY '  '
                      SPACE            DELIMITED BY SIZE
                      HV-LAST-NAME     DELIMITED BY '  '
                 INTO PT-LEGAL-NAME
               END-STRING
           ELSE
               MOVE HV-LEGAL-NAME      TO PT-LEGAL-NAME
           END-IF
      *
           MOVE PT-LEGAL-NAME(1:25)    TO PT-SHORT-NAME
      *
           IF IND-DOB < ZERO
               MOVE ZERO               TO PT-DOB-INCORP
           ELSE
               MOVE HV-DOB             TO WS-ISO-DATE
               PERFORM 9200-ISO-TO-NUMERIC
               MOVE WS-WORK-DATE       TO PT-DOB-INCORP
           END-IF
      *
           MOVE HV-ONBOARD-DATE        TO WS-ISO-DATE
           PERFORM 9200-ISO-TO-NUMERIC
           MOVE WS-WORK-DATE           TO PT-ONBOARD-DATE
      *
           IF IND-NATIONAL-ID < ZERO
               MOVE SPACES             TO PT-NATIONAL-ID
           ELSE
               MOVE HV-NATIONAL-ID     TO PT-NATIONAL-ID
           END-IF
      *
           IF IND-TAX-ID < ZERO
               MOVE SPACES             TO PT-TAX-ID
           ELSE
               MOVE HV-TAX-ID          TO PT-TAX-ID
           END-IF
      *
           IF IND-DOMICILE < ZERO
               MOVE HV-COUNTRY-CD      TO PT-DOMICILE-CTRY
           ELSE
               MOVE HV-DOMICILE-CTRY   TO PT-DOMICILE-CTRY
           END-IF
      *
           MOVE HV-COUNTRY-CD          TO PT-RESIDENCE-CTRY
      *
           IF IND-CITIZENSHIP < ZERO
               MOVE PT-DOMICILE-CTRY   TO PT-CITIZENSHIP
           ELSE
               MOVE HV-CITIZENSHIP     TO PT-CITIZENSHIP
           END-IF
      *
           MOVE HV-PEP-FLG             TO PT-PEP-FLG
           MOVE HV-CUST-STATUS         TO PT-STATUS
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2500 - LINKED CUSTOMER NUMBERS                                 *
      ******************************************************************
       2500-BUILD-LINKS.
           EXEC SQL DECLARE LINKCSR CURSOR FOR
               SELECT CUST_ID
                    , CUST_STATUS
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :HV-PARTY-ID
                ORDER BY CUST_ID
           END-EXEC
      *
           EXEC SQL OPEN LINKCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE '2500-BUILD-LINKS' TO ER-PARAGRAPH
               PERFORM 8000-SQL-ERROR
               GO TO 2500-EXIT
           END-IF
      *
           MOVE 'Y'                    TO WS-LINK-CSR-SW
           MOVE 'N'                    TO WS-END-SW
           MOVE ZERO                   TO WS-SUB
      *
           PERFORM UNTIL WS-END-OF-CURSOR
                      OR WS-SUB >= 20
               EXEC SQL
                   FETCH LINKCSR
                    INTO :HV-CUST-ID
                       , :HV-CUST-STATUS
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1           TO WS-SUB
                       MOVE WS-SUB     TO PT-LINK-CNT
                       MOVE HV-CUST-ID TO PT-LINK-CUST-ID(WS-SUB)
                       MOVE HV-CUST-STATUS
                                       TO PT-LINK-STATUS(WS-SUB)
                       PERFORM 2600-DERIVE-SYSTEM
                   WHEN +100
                       MOVE 'Y'        TO WS-END-SW
                   WHEN OTHER
                       MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       MOVE '2500-BUILD-LINKS'
                                       TO ER-PARAGRAPH
                       PERFORM 8000-SQL-ERROR
                       MOVE 'Y'        TO WS-END-SW
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL CLOSE LINKCSR END-EXEC
           MOVE 'N'                    TO WS-LINK-CSR-SW
      *
      *    THE ODO MUST NEVER BE LEFT AT ZERO - THE MINIMUM ON THE
      *    LAYOUT IS ONE OCCURRENCE.
           IF WS-SUB = ZERO
               MOVE 1                  TO PT-LINK-CNT
               MOVE CV-RISK-CUST-ID    TO PT-LINK-CUST-ID(1)
               MOVE WS-RANGE-UNKNOWN   TO PT-LINK-SYSTEM(1)
               MOVE 'A'                TO PT-LINK-STATUS(1)
           END-IF
           .
       2500-EXIT.
           EXIT
           .
      *
       2600-DERIVE-SYSTEM.
           MOVE WS-RANGE-UNKNOWN       TO PT-LINK-SYSTEM(WS-SUB)
      *
           PERFORM VARYING WS-RX FROM 1 BY 1
                     UNTIL WS-RX > 5
               IF HV-CUST-ID NOT < WS-RG-LOW(WS-RX)
                  AND HV-CUST-ID NOT > WS-RG-HIGH(WS-RX)
                   MOVE WS-RG-SYSTEM(WS-RX)
                                       TO PT-LINK-SYSTEM(WS-SUB)
                   MOVE 6              TO WS-RX
               END-IF
           END-PERFORM
           .
      *
      ******************************************************************
      * 2800 - ALIASES                                                 *
      *                                                                *
      * THERE IS NO ALIAS TABLE.  THE ALIASES ARE THE NAME VARIATIONS  *
      * THE LINKED CUSTOMER ROWS CARRY - THE PRODUCT SYSTEMS WERE      *
      * LOADED FROM DIFFERENT MERGERS AND SPELL THE SAME PERSON        *
      * DIFFERENTLY.  COMPLIANCE SCREEN EVERY VARIATION, NOT JUST THE  *
      * MASTER NAME, SO THEY ARE COLLECTED HERE.                       *
      ******************************************************************
       2800-BUILD-ALIASES.
           EXEC SQL DECLARE ALIACSR CURSOR FOR
               SELECT DISTINCT COALESCE(LEGAL_NAME, LAST_NAME)
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :HV-PARTY-ID
           END-EXEC
      *
           EXEC SQL OPEN ALIACSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE '2800-BUILD-ALIASES'
                                       TO ER-PARAGRAPH
               PERFORM 8000-SQL-ERROR
               GO TO 2800-EXIT
           END-IF
      *
           MOVE 'N'                    TO WS-END-SW
           MOVE ZERO                   TO WS-SUB
      *
           PERFORM UNTIL WS-END-OF-CURSOR
                      OR WS-SUB >= 10
               EXEC SQL
                   FETCH ALIACSR
                    INTO :HV-LEGAL-NAME :IND-LEGAL-NAME
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       IF IND-LEGAL-NAME NOT < ZERO
                          AND HV-LEGAL-NAME NOT = PT-LEGAL-NAME
                           ADD 1       TO WS-SUB
                           MOVE WS-SUB TO PT-ALIAS-CNT
                           MOVE HV-LEGAL-NAME
                                       TO PT-ALIAS-NAME(WS-SUB)
                           MOVE 'ALSO' TO PT-ALIAS-TYPE(WS-SUB)
                       END-IF
                   WHEN +100
                       MOVE 'Y'        TO WS-END-SW
                   WHEN OTHER
                       MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       MOVE '2800-BUILD-ALIASES'
                                       TO ER-PARAGRAPH
                       PERFORM 8000-SQL-ERROR
                       MOVE 'Y'        TO WS-END-SW
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL CLOSE ALIACSR END-EXEC
           .
       2800-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - KYC HISTORY, MOST RECENT REVIEW FIRST                   *
      ******************************************************************
       3000-READ-KYC-HISTORY.
           EXEC SQL DECLARE KYCHCSR CURSOR FOR
               SELECT KYC_SEQ
                    , KYC_STATUS
                    , KYC_LEVEL
                    , REVIEW_TYPE
                    , CHAR(REVIEW_DATE, ISO)
                    , CHAR(NEXT_REVIEW_DATE, ISO)
                    , CHAR(EXPIRY_DATE, ISO)
                    , REVIEWED_BY
                 FROM PARTYRSK.PARTY_KYC
                WHERE PARTY_ID = :HV-PARTY-ID
                ORDER BY KYC_SEQ DESC
           END-EXEC
      *
           EXEC SQL OPEN KYCHCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'PARTY_KYC         '
                                       TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE '3000-READ-KYC-HISTORY'
                                       TO ER-PARAGRAPH
               PERFORM 8000-SQL-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           MOVE 'Y'                    TO WS-KYC-CSR-SW
           MOVE 'N'                    TO WS-END-SW
           MOVE ZERO                   TO WS-KH-CNT
      *
           PERFORM UNTIL WS-END-OF-CURSOR
                      OR WS-KH-CNT >= 5
               EXEC SQL
                   FETCH KYCHCSR
                    INTO :HV-KYC-SEQ
                       , :HV-KYC-STATUS
                       , :HV-KYC-LEVEL
                       , :HV-REVIEW-TYPE
                       , :HV-REVIEW-DATE
                       , :HV-NEXT-REVIEW :IND-NEXT-REVIEW
                       , :HV-EXPIRY-DATE :IND-EXPIRY
                       , :HV-REVIEWED-BY :IND-REVIEWED-BY
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       PERFORM 3100-STORE-KYC-ROW
                   WHEN +100
                       MOVE 'Y'        TO WS-END-SW
                   WHEN OTHER
                       MOVE 'PARTY_KYC         '
                                       TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       MOVE '3000-READ-KYC-HISTORY'
                                       TO ER-PARAGRAPH
                       PERFORM 8000-SQL-ERROR
                       MOVE 'Y'        TO WS-END-SW
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL CLOSE KYCHCSR END-EXEC
           MOVE 'N'                    TO WS-KYC-CSR-SW
           .
       3000-EXIT.
           EXIT
           .
      *
       3100-STORE-KYC-ROW.
           ADD 1                       TO WS-KH-CNT
           SET WS-KX                   TO WS-KH-CNT
      *
           MOVE HV-KYC-SEQ             TO WS-KH-SEQ(WS-KX)
           MOVE HV-KYC-STATUS          TO WS-KH-STATUS(WS-KX)
           MOVE HV-KYC-LEVEL           TO WS-KH-LEVEL(WS-KX)
           MOVE HV-REVIEW-TYPE         TO WS-KH-REVIEW-TYPE(WS-KX)
      *
           MOVE HV-REVIEW-DATE         TO WS-ISO-DATE
           PERFORM 9200-ISO-TO-NUMERIC
           MOVE WS-WORK-DATE           TO WS-KH-REVIEW-DATE(WS-KX)
      *
           IF IND-NEXT-REVIEW < ZERO
               MOVE ZERO               TO WS-KH-NEXT-DATE(WS-KX)
           ELSE
               MOVE HV-NEXT-REVIEW     TO WS-ISO-DATE
               PERFORM 9200-ISO-TO-NUMERIC
               MOVE WS-WORK-DATE       TO WS-KH-NEXT-DATE(WS-KX)
           END-IF
      *
           IF IND-EXPIRY < ZERO
               MOVE ZERO               TO WS-KH-EXPIRY-DATE(WS-KX)
           ELSE
               MOVE HV-EXPIRY-DATE     TO WS-ISO-DATE
               PERFORM 9200-ISO-TO-NUMERIC
               MOVE WS-WORK-DATE       TO WS-KH-EXPIRY-DATE(WS-KX)
           END-IF
      *
           IF IND-REVIEWED-BY < ZERO
               MOVE SPACES             TO WS-KH-REVIEWED-BY(WS-KX)
           ELSE
               MOVE HV-REVIEWED-BY     TO WS-KH-REVIEWED-BY(WS-KX)
           END-IF
      *
      *    THE FIRST ROW BACK IS THE CURRENT REVIEW.
           IF WS-KH-CNT = 1
               MOVE WS-KH-STATUS(WS-KX)
                                       TO WS-CK-STATUS
               MOVE WS-KH-NEXT-DATE(WS-KX)
                                       TO WS-CK-NEXT-DATE
               MOVE WS-KH-EXPIRY-DATE(WS-KX)
                                       TO WS-CK-EXPIRY-DATE
           END-IF
           .
      *
      ******************************************************************
      * 3500 - PUBLISH THE KYC STANDING                                *
      *                                                                *
      * AN OVERDUE REVIEW OR AN EXPIRED DOCUMENT DOWNGRADES A STATUS   *
      * THAT STILL SAYS OK ON THE TABLE - THE WEEKLY REVIEW JOB IS THE *
      * ONLY THING THAT MAINTAINS THE COLUMN AND IT RUNS BEHIND.       *
      ******************************************************************
       3500-PUBLISH-KYC.
           IF WS-KH-CNT = ZERO
               MOVE 'PN'               TO CV-RISK-KYC-STATUS
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
               GO TO 3500-EXIT
           END-IF
      *
           MOVE WS-CK-STATUS           TO CV-RISK-KYC-STATUS
      *
           IF CV-RISK-KYC-OK
               IF WS-CK-EXPIRY-DATE NOT = ZERO
                  AND WS-CK-EXPIRY-DATE < WS-TODAY
                   MOVE 'EX'           TO CV-RISK-KYC-STATUS
               ELSE
                   IF WS-CK-NEXT-DATE NOT = ZERO
                      AND WS-CK-NEXT-DATE < WS-TODAY
                       MOVE 'EX'       TO CV-RISK-KYC-STATUS
                   END-IF
               END-IF
           END-IF
      *
           IF NOT CV-RISK-KYC-OK
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
           END-IF
      *
           IF PT-IS-PEP
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
           END-IF
           .
       3500-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5000 - SCORE AND EXPOSURE SUMMARY                              *
      ******************************************************************
       5000-LINK-SUMMARY.
           EXEC CICS LINK
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CV-RISK-AREA)
                     LENGTH(WS-COMMAREA-LEN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 0012               TO CV-RISK-RC
               MOVE WS-PGM-ID          TO CV-RISK-FAIL-PGM
               MOVE 'LNK7'             TO CV-RISK-REASON-CD
               MOVE 'SUMMARY MODULE COULD NOT BE LINKED'
                                       TO CV-RISK-REASON-TXT
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE 'F'                TO ER-SEVERITY
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE WS-RESP2           TO ER-EIBRESP2
               MOVE '5000-LINK-SUMMARY'
                                       TO ER-PARAGRAPH
               MOVE CV-RISK-REASON-TXT TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
           END-IF
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
           MOVE 'PARTY DETAIL ASSEMBLY FAILED'
                                       TO ER-MESSAGE
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'SQL6'                 TO CV-RISK-REASON-CD
           MOVE 'PARTY DETAIL COULD NOT BE ASSEMBLED'
                                       TO CV-RISK-REASON-TXT
      *
           IF WS-LINK-CSR-OPEN
               EXEC SQL CLOSE LINKCSR END-EXEC
               MOVE 'N'                TO WS-LINK-CSR-SW
           END-IF
      *
           IF WS-KYC-CSR-OPEN
               EXEC SQL CLOSE KYCHCSR END-EXEC
               MOVE 'N'                TO WS-KYC-CSR-SW
           END-IF
      *
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
      *
      ******************************************************************
      * 9200 - ISO DATE TO THE NUMERIC FORM THE COPYBOOKS USE          *
      ******************************************************************
       9200-ISO-TO-NUMERIC.
           IF WS-ISO-DATE = SPACES
               MOVE ZERO               TO WS-WORK-DATE
           ELSE
               MOVE WS-ISO-CCYY        TO WS-WD-CCYY
               MOVE WS-ISO-MM          TO WS-WD-MM
               MOVE WS-ISO-DD          TO WS-WD-DD
           END-IF
           .
