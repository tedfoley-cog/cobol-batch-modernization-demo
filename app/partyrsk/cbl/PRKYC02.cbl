      ******************************************************************
      * PRKYC02 - KYC STATUS AND SANCTIONS SCREENING                   *
      *                                                                *
      * ORIGINALLY WRITTEN 1998 FOR THE KYC REWRITE AND CONVERTED TO   *
      * COMMAND LEVEL IN 2001.  THE NAME SCREENING SECTION IS STILL    *
      * THE ORIGINAL CODE APART FROM THE ALTERNATE INDEX CHANGE MADE   *
      * WHEN THE WATCH LIST OUTGREW A SEQUENTIAL SCAN.                 *
      *                                                                *
      * READS THE LATEST KYC ROW FOR THE PARTY, SETS THE KYC STATUS    *
      * IN THE RISK COMMAREA, CHECKS THE REVIEW EXPIRY AGAINST TODAY   *
      * AND SCREENS THE PARTY NAME AGAINST THE CONSOLIDATED WATCH      *
      * LIST THROUGH THE ENTITY NAME PATH.                             *
      *                                                                *
      * A CONFIRMED HIT STOPS THE CHAIN HERE WITH RETURN CODE 8.       *
      * THE EXPOSURE PROGRAM IS NOT LINKED IN THAT CASE - A SANCTIONED *
      * PARTY IS NOT SCORED AND NOT AGGREGATED.                        *
      *                                                                *
      * CALLED BY   - PRKYC01                                          *
      * CALLS       - PRRSK02  EXPOSURE AGGREGATION                    *
      *               PRERR01  PARTYRSK ERROR HANDLER                  *
      * TABLES      - PARTYRSK.PARTY_KYC             (SELECT)          *
      *               PARTYRSK.CUSTOMER              (SELECT)          *
      * FILES       - WATCHNM  PATH PRTY.PROD.WATCHLST.PATH1 OVER      *
      *                        THE ENTITY NAME ALTERNATE INDEX         *
      * COMMAREA    - CV-RISK-AREA, 512 BYTES, CVRISK01Y               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRKYC02.
       AUTHOR.        PARTY AND RISK.
       DATE-WRITTEN.  1998-03-11.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRKYC02 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'PRRSK02 '.
       01  WS-ERROR-PGM                PIC X(8)  VALUE 'PRERR01 '.
       01  WS-WATCH-FILE               PIC X(8)  VALUE 'WATCHNM '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-COMMAREA-LEN             PIC S9(4) COMP VALUE 512.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-KYC-FOUND-SW         PIC X     VALUE 'N'.
               88  WS-KYC-FOUND                  VALUE 'Y'.
           05  WS-BROWSE-SW            PIC X     VALUE 'N'.
               88  WS-BROWSE-STARTED             VALUE 'Y'.
           05  WS-EOF-SW               PIC X     VALUE 'N'.
               88  WS-END-OF-BROWSE              VALUE 'Y'.
           05  WS-STOP-SW              PIC X     VALUE 'N'.
               88  WS-STOP-CHAIN                 VALUE 'Y'.
      *
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(8)  VALUE SPACES.
      *
       01  WS-TODAY                    PIC 9(8)  VALUE ZERO.
       01  WS-TODAY-R REDEFINES WS-TODAY.
           05  WS-TD-CC                PIC 9(2).
           05  WS-TD-YY                PIC 9(2).
           05  WS-TD-MM                PIC 9(2).
           05  WS-TD-DD                PIC 9(2).
      *
      ******************************************************************
      * REVIEW DATES ARRIVE FROM DB2 AS ISO CHARACTER STRINGS.  THEY   *
      * ARE UNPICKED THROUGH A REDEFINES - THE ORIGINAL CODE DID THIS  *
      * WITH REFERENCE MODIFICATION AND IT WAS UNREADABLE.             *
      ******************************************************************
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
       01  WS-NEXT-REVIEW              PIC 9(8)  VALUE ZERO.
       01  WS-EXPIRY-DATE              PIC 9(8)  VALUE ZERO.
      *
      ******************************************************************
      * SCREENING WORK AREAS                                           *
      ******************************************************************
       01  WS-SCREEN-NAME              PIC X(60) VALUE SPACES.
       01  WS-CAND-NAME                PIC X(60) VALUE SPACES.
       01  WS-BROWSE-KEY               PIC X(60) VALUE SPACES.
      *
       01  WS-WATCH-BUFFER             PIC X(160) VALUE SPACES.
       01  WS-WATCH-LENGTH             PIC S9(4) COMP VALUE 160.
      *
       01  WS-MATCH-SCORE              PIC 9(3)  VALUE ZERO.
       01  WS-CHAR-HITS                PIC 9(3)  VALUE ZERO.
       01  WS-CHAR-SEEN                PIC 9(3)  VALUE ZERO.
       01  WS-SUB                      PIC S9(4) COMP VALUE 0.
       01  WS-IDX                      PIC S9(4) COMP VALUE 0.
       01  WS-READ-CNT                 PIC 9(4)  VALUE ZERO.
      *
      *    THE BROWSE IS CAPPED.  A COMMON SURNAME ON THE INTERNAL
      *    LIST CAN RETURN SEVERAL HUNDRED ROWS AND THE ONLINE
      *    RESPONSE BUDGET FOR THE WHOLE CROSSING IS 300 MS.
       01  WS-MAX-READS                PIC 9(4)  VALUE 0250.
      *
       01  WS-SCORE-CONFIRM            PIC 9(3)  VALUE 090.
       01  WS-SCORE-POSSIBLE           PIC 9(3)  VALUE 070.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
       01  HV-KYC.
           05  HV-PARTY-ID             PIC X(11).
           05  HV-KYC-SEQ              PIC S9(4) COMP.
           05  HV-KYC-STATUS           PIC X(2).
           05  HV-KYC-LEVEL            PIC X(4).
           05  HV-REVIEW-TYPE          PIC X(4).
           05  HV-REVIEW-DATE          PIC X(10).
           05  HV-NEXT-REVIEW          PIC X(10).
           05  HV-EXPIRY-DATE          PIC X(10).
           05  HV-RISK-RATING          PIC X(1).
           05  HV-ENHANCED-DD          PIC X(1).
           05  HV-ADDR-VERIFIED        PIC X(1).
      *
       01  HV-PARTY.
           05  HV-LEGAL-NAME           PIC X(60).
           05  HV-FIRST-NAME           PIC X(20).
           05  HV-LAST-NAME            PIC X(25).
           05  HV-PEP-FLG              PIC X(1).
           05  HV-PARTY-TYPE           PIC X(1).
      *
       01  HV-INDICATORS.
           05  IND-NEXT-REVIEW         PIC S9(4) COMP.
           05  IND-EXPIRY              PIC S9(4) COMP.
           05  IND-RISK-RATING         PIC S9(4) COMP.
           05  IND-LEGAL-NAME          PIC S9(4) COMP.
           05  IND-FIRST-NAME          PIC S9(4) COMP.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           COPY CVSANC01Y.
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
               EXEC CICS ABEND ABCODE('PRC2') NODUMP END-EXEC
           END-IF
      *
           SET ADDRESS OF CV-RISK-AREA TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-INIT           THRU 1000-EXIT
           PERFORM 2000-READ-KYC       THRU 2000-EXIT
           IF WS-ERROR-FOUND
               GO TO 0000-FINISH
           END-IF
      *
           PERFORM 2500-READ-PARTY-NAME
                                       THRU 2500-EXIT
           IF WS-ERROR-FOUND
               GO TO 0000-FINISH
           END-IF
      *
           PERFORM 3000-SCREEN-NAME    THRU 3000-EXIT
           IF WS-ERROR-FOUND
               GO TO 0000-FINISH
           END-IF
      *
           PERFORM 4000-EVALUATE-KYC   THRU 4000-EXIT
      *
           IF WS-STOP-CHAIN
               GO TO 0000-FINISH
           END-IF
      *
           PERFORM 5000-LINK-EXPOSURE  THRU 5000-EXIT
           .
       0000-FINISH.
           PERFORM 7000-ADD-HOP        THRU 7000-EXIT
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - INITIALISE                                              *
      ******************************************************************
       1000-INIT.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-KYC-FOUND-SW
           MOVE 'N'                    TO WS-STOP-SW
           MOVE 'N'                    TO WS-BROWSE-SW
           MOVE 'N'                    TO WS-EOF-SW
           MOVE ZERO                   TO WS-READ-CNT
      *
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'N'                    TO ER-ABEND-REQUESTED
      *
           MOVE SPACES                 TO SANCTION-MATCH
           MOVE CV-RISK-PARTY-ID       TO SM-PARTY-ID
           MOVE ZERO                   TO SM-MATCH-CNT
           MOVE ZERO                   TO SM-BEST-SCORE
           MOVE 'N'                    TO SM-HIT-FLG
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
               MOVE SPACES             TO SM-MT-LIST-CD(WS-SUB)
               MOVE SPACES             TO SM-MT-ENTRY-ID(WS-SUB)
               MOVE ZERO               TO SM-MT-SCORE(WS-SUB)
               MOVE SPACES             TO SM-MT-DISPOSITION(WS-SUB)
           END-PERFORM
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
           MOVE WS-TODAY               TO SM-SCREEN-DATE
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - LATEST KYC ROW FOR THE PARTY                            *
      *                                                                *
      * THE HIGHEST SEQUENCE NUMBER IS THE CURRENT REVIEW.  EARLIER    *
      * SEQUENCES ARE KEPT FOR SEVEN YEARS FOR THE REGULATOR AND ARE   *
      * READ ONLY BY THE INQUIRY CHAIN.                                *
      ******************************************************************
       2000-READ-KYC.
           EXEC SQL
               SELECT KYC_SEQ
                    , KYC_STATUS
                    , KYC_LEVEL
                    , REVIEW_TYPE
                    , CHAR(REVIEW_DATE, ISO)
                    , CHAR(NEXT_REVIEW_DATE, ISO)
                    , CHAR(EXPIRY_DATE, ISO)
                    , RISK_RATING
                    , ENHANCED_DD_FLG
                    , ADDR_VERIFIED_FLG
                 INTO :HV-KYC-SEQ
                    , :HV-KYC-STATUS
                    , :HV-KYC-LEVEL
                    , :HV-REVIEW-TYPE
                    , :HV-REVIEW-DATE
                    , :HV-NEXT-REVIEW  :IND-NEXT-REVIEW
                    , :HV-EXPIRY-DATE  :IND-EXPIRY
                    , :HV-RISK-RATING  :IND-RISK-RATING
                    , :HV-ENHANCED-DD
                    , :HV-ADDR-VERIFIED
                 FROM PARTYRSK.PARTY_KYC
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND KYC_SEQ =
                      (SELECT MAX(KYC_SEQ)
                         FROM PARTYRSK.PARTY_KYC
                        WHERE PARTY_ID = :HV-PARTY-ID)
           END-EXEC
      *
           IF SQLCODE = 0
               MOVE 'Y'                TO WS-KYC-FOUND-SW
               MOVE HV-KYC-STATUS      TO CV-RISK-KYC-STATUS
               GO TO 2000-CONVERT
           END-IF
      *
           IF SQLCODE = +100
      *        NO KYC RECORD AT ALL.  THAT IS NOT A SYSTEM FAILURE -
      *        IT IS AN UNONBOARDED PARTY AND IT IS TREATED AS
      *        PENDING SO THE CARD SIDE REFERS IT.
               MOVE 'PN'               TO CV-RISK-KYC-STATUS
               MOVE 'KYC0'             TO CV-RISK-REASON-CD
               MOVE 'NO KYC REVIEW ON FILE FOR THIS PARTY'
                                       TO CV-RISK-REASON-TXT
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
               GO TO 2000-EXIT
           END-IF
      *
           MOVE 'PARTY_KYC         '   TO ER-SQL-TABLE
           MOVE 'SELECT  '             TO ER-SQL-OPERATION
           MOVE '2000-READ-KYC'        TO ER-PARAGRAPH
           PERFORM 8000-SQL-ERROR      THRU 8000-EXIT
           GO TO 2000-EXIT
           .
       2000-CONVERT.
           MOVE ZERO                   TO WS-NEXT-REVIEW
           MOVE ZERO                   TO WS-EXPIRY-DATE
      *
           IF IND-NEXT-REVIEW NOT < ZERO
               MOVE HV-NEXT-REVIEW     TO WS-ISO-DATE
               PERFORM 2100-CONVERT-ISO
                                       THRU 2100-EXIT
               MOVE WS-CONV-DATE       TO WS-NEXT-REVIEW
           END-IF
      *
           IF IND-EXPIRY NOT < ZERO
               MOVE HV-EXPIRY-DATE     TO WS-ISO-DATE
               PERFORM 2100-CONVERT-ISO
                                       THRU 2100-EXIT
               MOVE WS-CONV-DATE       TO WS-EXPIRY-DATE
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-CONVERT-ISO.
           MOVE ZERO                   TO WS-CONV-DATE
           IF WS-ISO-DATE = SPACES
               GO TO 2100-EXIT
           END-IF
           MOVE WS-ISO-CCYY            TO WS-CV-CCYY
           MOVE WS-ISO-MM              TO WS-CV-MM
           MOVE WS-ISO-DD              TO WS-CV-DD
           .
       2100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2500 - NAME TO SCREEN                                          *
      *                                                                *
      * AN ORGANISATION IS SCREENED ON ITS LEGAL NAME.  AN INDIVIDUAL  *
      * IS SCREENED ON SURNAME FOLLOWED BY FORENAME BECAUSE THAT IS    *
      * THE ORDER THE WATCH LIST LOADER BUILDS THE ALTERNATE KEY IN.   *
      ******************************************************************
       2500-READ-PARTY-NAME.
           EXEC SQL
               SELECT LEGAL_NAME
                    , FIRST_NAME
                    , LAST_NAME
                    , PEP_FLG
                    , PARTY_TYPE
                 INTO :HV-LEGAL-NAME  :IND-LEGAL-NAME
                    , :HV-FIRST-NAME  :IND-FIRST-NAME
                    , :HV-LAST-NAME
                    , :HV-PEP-FLG
                    , :HV-PARTY-TYPE
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND CUST_STATUS <> 'X'
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           IF SQLCODE = +100
               MOVE 'PARTY NAME NOT AVAILABLE - SCREENING SKIPPED'
                                       TO CV-RISK-REASON-TXT
               MOVE 'SCR0'             TO CV-RISK-REASON-CD
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
               MOVE SPACES             TO WS-SCREEN-NAME
               GO TO 2500-EXIT
           END-IF
      *
           IF SQLCODE NOT = 0
               MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
               MOVE 'SELECT  '         TO ER-SQL-OPERATION
               MOVE '2500-READ-PARTY-NAME'
                                       TO ER-PARAGRAPH
               PERFORM 8000-SQL-ERROR  THRU 8000-EXIT
               GO TO 2500-EXIT
           END-IF
      *
           MOVE SPACES                 TO WS-SCREEN-NAME
      *
           IF HV-PARTY-TYPE = 'I'
               MOVE HV-LAST-NAME       TO WS-SCREEN-NAME(1:25)
               IF IND-FIRST-NAME NOT < ZERO
                   MOVE HV-FIRST-NAME  TO WS-SCREEN-NAME(27:20)
               END-IF
               GO TO 2500-EXIT
           END-IF
      *
           IF IND-LEGAL-NAME < ZERO
               MOVE HV-LAST-NAME       TO WS-SCREEN-NAME(1:25)
           ELSE
               MOVE HV-LEGAL-NAME      TO WS-SCREEN-NAME
           END-IF
           .
       2500-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - WATCH LIST SCREENING                                    *
      *                                                                *
      * BROWSE THE ENTITY NAME PATH FROM THE SCREENING NAME AND SCORE  *
      * EVERY CANDIDATE UNTIL THE FIRST TWELVE CHARACTERS NO LONGER    *
      * AGREE.  THE LIST IS NOT UNIQUE ON NAME SO SEVERAL ENTRIES CAN  *
      * SHARE ONE KEY.                                                 *
      ******************************************************************
       3000-SCREEN-NAME.
           IF WS-SCREEN-NAME = SPACES
               GO TO 3000-EXIT
           END-IF
      *
           MOVE WS-SCREEN-NAME         TO WS-BROWSE-KEY
      *
           EXEC CICS STARTBR
                     FILE(WS-WATCH-FILE)
                     RIDFLD(WS-BROWSE-KEY)
                     KEYLENGTH(60)
                     GTEQ
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP = DFHRESP(NOTFND)
      *        NOTHING AT OR ABOVE THE NAME.  CLEAN.
               GO TO 3000-EXIT
           END-IF
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '3000-SCREEN-NAME' TO ER-PARAGRAPH
               MOVE 'STARTBR '         TO ER-SQL-OPERATION
               PERFORM 8200-VSAM-ERROR THRU 8200-EXIT
               GO TO 3000-EXIT
           END-IF
      *
           MOVE 'Y'                    TO WS-BROWSE-SW
           .
       3000-LOOP.
           IF WS-END-OF-BROWSE
               GO TO 3000-ENDBR
           END-IF
      *
           IF WS-READ-CNT >= WS-MAX-READS
      *        THE CAP WAS HIT.  THE RESULT IS PARTIAL AND MUST BE
      *        FLAGGED - COMPLIANCE REVIEW THE PRER QUEUE DAILY FOR
      *        THIS CONDITION.
               MOVE 'W'                TO ER-SEVERITY
               MOVE 'BUSN'             TO ER-ERROR-TYPE
               MOVE 'SCRC'             TO ER-REASON-CD
               MOVE '3000-SCREEN-NAME' TO ER-PARAGRAPH
               MOVE 'WATCH LIST BROWSE CAP REACHED - PARTIAL SCREEN'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
                                       THRU 9000-EXIT
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
               GO TO 3000-ENDBR
           END-IF
      *
           MOVE 160                    TO WS-WATCH-LENGTH
           EXEC CICS READNEXT
                     FILE(WS-WATCH-FILE)
                     INTO(WS-WATCH-BUFFER)
                     LENGTH(WS-WATCH-LENGTH)
                     RIDFLD(WS-BROWSE-KEY)
                     KEYLENGTH(60)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP = DFHRESP(ENDFILE)
               MOVE 'Y'                TO WS-EOF-SW
               GO TO 3000-LOOP
           END-IF
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '3000-SCREEN-NAME' TO ER-PARAGRAPH
               MOVE 'READNEXT'         TO ER-SQL-OPERATION
               PERFORM 8200-VSAM-ERROR THRU 8200-EXIT
               GO TO 3000-ENDBR
           END-IF
      *
           ADD 1                       TO WS-READ-CNT
           MOVE WS-WATCH-BUFFER        TO SANCTION-RECORD
           MOVE SN-ENTITY-NAME         TO WS-CAND-NAME
      *
      *    ONCE THE LEADING TWELVE CHARACTERS DIVERGE THERE IS NO
      *    POINT READING ON - THE PATH IS IN NAME ORDER.
           IF WS-CAND-NAME(1:12) NOT = WS-SCREEN-NAME(1:12)
               GO TO 3000-ENDBR
           END-IF
      *
           IF SN-ACTIVE-FLG NOT = 'Y'
               GO TO 3000-LOOP
           END-IF
      *
           IF SN-DELISTED-DATE NOT = ZERO
              AND SN-DELISTED-DATE < WS-TODAY
               GO TO 3000-LOOP
           END-IF
      *
           PERFORM 3100-SCORE-CANDIDATE
                                       THRU 3100-EXIT
           PERFORM 3200-POST-MATCH     THRU 3200-EXIT
      *
           GO TO 3000-LOOP
           .
       3000-ENDBR.
           IF WS-BROWSE-STARTED
               EXEC CICS ENDBR
                         FILE(WS-WATCH-FILE)
                         RESP(WS-RESP)
               END-EXEC
               MOVE 'N'                TO WS-BROWSE-SW
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3100 - SCORE ONE CANDIDATE                                     *
      *                                                                *
      * CHARACTER AGREEMENT OVER THE SIGNIFICANT LENGTH OF THE NAME,   *
      * ADJUSTED FOR COUNTRY AND ENTITY TYPE.  THIS IS THE 1998        *
      * ALGORITHM AND IS DELIBERATELY CRUDE - THE STRATEGIC MATCHER    *
      * RUNS OVERNIGHT IN THE PARTY WEEKLY CYCLE.                      *
      ******************************************************************
       3100-SCORE-CANDIDATE.
           MOVE ZERO                   TO WS-CHAR-HITS
           MOVE ZERO                   TO WS-CHAR-SEEN
           MOVE ZERO                   TO WS-MATCH-SCORE
      *
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 60
               IF WS-SCREEN-NAME(WS-IDX:1) NOT = SPACE
                  OR WS-CAND-NAME(WS-IDX:1) NOT = SPACE
                   ADD 1               TO WS-CHAR-SEEN
                   IF WS-SCREEN-NAME(WS-IDX:1) =
                      WS-CAND-NAME(WS-IDX:1)
                       ADD 1           TO WS-CHAR-HITS
                   END-IF
               END-IF
           END-PERFORM
      *
           IF WS-CHAR-SEEN = ZERO
               GO TO 3100-EXIT
           END-IF
      *
           COMPUTE WS-MATCH-SCORE =
                   (WS-CHAR-HITS * 100) / WS-CHAR-SEEN
      *
      *    SAME COUNTRY LIFTS THE SCORE, A DIFFERENT DECLARED COUNTRY
      *    PULLS IT DOWN.  ENTITY TYPE MISMATCH IS A HARD DISCOUNT.
           IF SN-COUNTRY = CV-RISK-COUNTRY
               ADD 5                   TO WS-MATCH-SCORE
           ELSE
               IF SN-COUNTRY NOT = SPACES
                   SUBTRACT 3          FROM WS-MATCH-SCORE
               END-IF
           END-IF
      *
           IF SN-ENTITY-TYPE NOT = HV-PARTY-TYPE
               SUBTRACT 15             FROM WS-MATCH-SCORE
           END-IF
      *
           IF HV-PEP-FLG = 'Y'
               ADD 3                   TO WS-MATCH-SCORE
           END-IF
      *
           IF WS-MATCH-SCORE > 999
               MOVE 999                TO WS-MATCH-SCORE
           END-IF
           .
       3100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3200 - RECORD THE MATCH IF IT IS WORTH RECORDING               *
      ******************************************************************
       3200-POST-MATCH.
           IF WS-MATCH-SCORE < WS-SCORE-POSSIBLE
               GO TO 3200-EXIT
           END-IF
      *
           IF WS-MATCH-SCORE > SM-BEST-SCORE
               MOVE WS-MATCH-SCORE     TO SM-BEST-SCORE
           END-IF
      *
           IF SM-MATCH-CNT < 5
               ADD 1                   TO SM-MATCH-CNT
               MOVE SN-LIST-CD         TO SM-MT-LIST-CD(SM-MATCH-CNT)
               MOVE SN-ENTRY-ID        TO SM-MT-ENTRY-ID(SM-MATCH-CNT)
               MOVE WS-MATCH-SCORE     TO SM-MT-SCORE(SM-MATCH-CNT)
               IF WS-MATCH-SCORE >= WS-SCORE-CONFIRM
                   MOVE 'HIT '         TO
                                    SM-MT-DISPOSITION(SM-MATCH-CNT)
               ELSE
                   MOVE 'POSS'         TO
                                    SM-MT-DISPOSITION(SM-MATCH-CNT)
               END-IF
           END-IF
      *
           IF WS-MATCH-SCORE >= WS-SCORE-CONFIRM
               MOVE 'Y'                TO SM-HIT-FLG
           ELSE
               IF SM-HIT-FLG NOT = 'Y'
                   MOVE 'P'            TO SM-HIT-FLG
               END-IF
           END-IF
           .
       3200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - APPLY THE KYC AND SCREENING OUTCOME                     *
      ******************************************************************
       4000-EVALUATE-KYC.
           IF SM-HIT
               PERFORM 4100-SANCTION-HIT
                                       THRU 4100-EXIT
               GO TO 4000-EXIT
           END-IF
      *
           IF SM-POSSIBLE
      *        A POSSIBLE MATCH DOES NOT STOP THE AUTHORISATION BUT IT
      *        IS CARRIED DOWN THE CHAIN AND SCORED AGAINST.
               MOVE 'P'                TO CV-RISK-SANCTION-FLG
               MOVE 'SANP'             TO CV-RISK-REASON-CD
               MOVE 'POSSIBLE WATCH LIST MATCH - REVIEW REQUIRED'
                                       TO CV-RISK-REASON-TXT
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
           ELSE
               MOVE 'N'                TO CV-RISK-SANCTION-FLG
           END-IF
      *
           IF NOT WS-KYC-FOUND
               GO TO 4000-EXIT
           END-IF
      *
           IF HV-KYC-STATUS = 'FL'
      *        A FAILED REVIEW IS A BUSINESS DECLINE IN ITS OWN RIGHT
      *        BUT THE PARTY IS STILL SCORED SO THE CARD SIDE HAS A
      *        NUMBER TO SHOW THE OPERATOR.
               MOVE 'FL'               TO CV-RISK-KYC-STATUS
               MOVE 'KYCF'             TO CV-RISK-REASON-CD
               MOVE 'KYC REVIEW FAILED - PARTY NOT IN GOOD STANDING'
                                       TO CV-RISK-REASON-TXT
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
               GO TO 4000-EXIT
           END-IF
      *
           IF WS-EXPIRY-DATE NOT = ZERO
              AND WS-EXPIRY-DATE < WS-TODAY
               MOVE 'EX'               TO CV-RISK-KYC-STATUS
               MOVE 'KYCX'             TO CV-RISK-REASON-CD
               MOVE 'KYC DOCUMENTATION EXPIRED'
                                       TO CV-RISK-REASON-TXT
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
               GO TO 4000-EXIT
           END-IF
      *
           IF WS-NEXT-REVIEW NOT = ZERO
              AND WS-NEXT-REVIEW < WS-TODAY
               MOVE 'EX'               TO CV-RISK-KYC-STATUS
               MOVE 'KYCR'             TO CV-RISK-REASON-CD
               MOVE 'KYC PERIODIC REVIEW OVERDUE'
                                       TO CV-RISK-REASON-TXT
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
           END-IF
           .
       4000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4100 - CONFIRMED HIT.  STOP THE CHAIN.                         *
      ******************************************************************
       4100-SANCTION-HIT.
           MOVE 'Y'                    TO CV-RISK-SANCTION-FLG
           MOVE 0008                   TO CV-RISK-RC
           MOVE 'SANC'                 TO CV-RISK-REASON-CD
           MOVE 'CONFIRMED WATCH LIST MATCH - AUTHORISATION STOPPED'
                                       TO CV-RISK-REASON-TXT
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'X'                    TO CV-RISK-BAND
           MOVE 999                    TO CV-RISK-SCORE
           MOVE 'DECL'                 TO CV-RISK-ADVICE-CD
           MOVE 'Y'                    TO WS-STOP-SW
      *
      *    THE HIT IS ALWAYS REPORTED EVEN THOUGH IT IS NOT AN ERROR.
      *    THE PRER QUEUE IS THE FEED THE SANCTIONS DESK WORKS FROM.
           MOVE 'I'                    TO ER-SEVERITY
           MOVE 'BUSN'                 TO ER-ERROR-TYPE
           MOVE 'SANC'                 TO ER-REASON-CD
           MOVE '4100-SANCTION-HIT'    TO ER-PARAGRAPH
           MOVE 'CONFIRMED WATCH LIST MATCH ON AUTHORISATION SCREEN'
                                       TO ER-MESSAGE
           MOVE CV-RISK-PARTY-ID       TO ER-VSAM-KEY(1:11)
           MOVE SM-MT-ENTRY-ID(1)      TO ER-VSAM-KEY(13:16)
           PERFORM 9000-REPORT-ERROR   THRU 9000-EXIT
           .
       4100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5000 - CARRY ON DOWN THE CHAIN                                 *
      ******************************************************************
       5000-LINK-EXPOSURE.
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
           MOVE 'LNK3'                 TO CV-RISK-REASON-CD
           MOVE 'EXPOSURE MODULE COULD NOT BE LINKED'
                                       TO CV-RISK-REASON-TXT
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE '5000-LINK-EXPOSURE'   TO ER-PARAGRAPH
           MOVE CV-RISK-REASON-TXT     TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR   THRU 9000-EXIT
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
       7000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 8000 / 8200 / 9000 - DIAGNOSTICS                               *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'KYC READ FAILED ON THE PARTY DATABASE'
                                       TO ER-MESSAGE
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'SQL2'                 TO CV-RISK-REASON-CD
           MOVE 'KYC STATUS COULD NOT BE ESTABLISHED'
                                       TO CV-RISK-REASON-TXT
           PERFORM 9000-REPORT-ERROR   THRU 9000-EXIT
           .
       8000-EXIT.
           EXIT
           .
      *
       8200-VSAM-ERROR.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-WATCH-FILE          TO ER-FILE-NAME
           MOVE WS-RESP                TO ER-VSAM-RC
           MOVE WS-BROWSE-KEY(1:32)    TO ER-VSAM-KEY
           MOVE 'WATCH LIST SCREENING FAILED - LIST NOT AVAILABLE'
                                       TO ER-MESSAGE
      *
      *    A SCREENING FAILURE MUST NEVER LOOK LIKE A CLEAN SCREEN.
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'SCRF'                 TO CV-RISK-REASON-CD
           MOVE 'SANCTIONS SCREENING UNAVAILABLE - REFER MANUALLY'
                                       TO CV-RISK-REASON-TXT
           PERFORM 9000-REPORT-ERROR   THRU 9000-EXIT
           .
       8200-EXIT.
           EXIT
           .
      *
       9000-REPORT-ERROR.
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
       9000-EXIT.
           EXIT
           .
