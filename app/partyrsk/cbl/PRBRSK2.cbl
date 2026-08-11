      ******************************************************************
      * PRBRSK2 - BATCH KYC AND SANCTIONS REFRESH                      *
      *                                                                *
      * SECOND PROGRAM OF THE BATCH RECALCULATION CHAIN.               *
      *                                                                *
      * DECIDES WHETHER THE PARTY KYC REVIEW HAS FALLEN DUE AND        *
      * RE-SCREENS THE PARTY AGAINST THE WATCH LIST.                   *
      *                                                                *
      * THE ONLINE SCREENER USES THE CICS FILE CONTROL BROWSE AGAINST  *
      * THE NAME PATH.  IN BATCH WE OPEN THE PATH OURSELVES, START     *
      * BROWSE ON THE SEARCH NAME AND CLOSE AGAIN, BECAUSE THE JOB     *
      * MAY BE RUNNING WHILE THE ONLINE REGION IS DOWN.                *
      *                                                                *
      * CALLED BY   - PRBRSK1                                          *
      * CALLS       - PRBRSK3                                          *
      * FILES       - WATCHNAM  PRTY.PROD.WATCHLST.PATH1  INPUT        *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - KYC IN DATE, NO WATCH LIST MATCH                        *
      *   04 - KYC REVIEW OVERDUE OR POSSIBLE MATCH RAISED             *
      *   08 - CONFIRMED SANCTIONS HIT OR KYC STATUS FAILED            *
      *   12 - FATAL - WATCH LIST OR PARTYDB UNAVAILABLE               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRBRSK2.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WATCHNAM-FILE ASSIGN TO WATCHNAM
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS WLF-ENTITY-NAME
                  WITH DUPLICATES
                  FILE STATUS  IS WS-WATCH-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  WATCHNAM-FILE
           RECORD CONTAINS 160 CHARACTERS.
       01  WATCHNAM-REC.
           05  FILLER                  PIC X(24).
           05  WLF-ENTITY-NAME         PIC X(60).
           05  FILLER                  PIC X(76).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'PRBRSK2 '.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
      *
       01  WS-SWITCHES.
           05  WS-WATCH-OPEN-SW        PIC X     VALUE 'N'.
               88  WS-WATCH-OPEN                 VALUE 'Y'.
           05  WS-BROWSE-EOF-SW        PIC X     VALUE 'N'.
               88  WS-BROWSE-EOF                 VALUE 'Y'.
           05  WS-KYC-FOUND-SW         PIC X     VALUE 'N'.
               88  WS-KYC-FOUND                  VALUE 'Y'.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
      *
       01  WS-WATCH-STATUS             PIC X(2)  VALUE '00'.
           88  WS-WATCH-OK                       VALUE '00'.
           88  WS-WATCH-DUP-KEY                  VALUE '02'.
           88  WS-WATCH-EOF                      VALUE '10'.
           88  WS-WATCH-NOT-FOUND                VALUE '23'.
      *
       01  WS-COUNTERS.
           05  WS-OWN-RC               PIC 9(4)  VALUE ZERO.
           05  WS-CHAIN-RC             PIC 9(4)  VALUE ZERO.
           05  WS-WORST-RC             PIC 9(4)  VALUE ZERO.
           05  WS-READ-CNT             PIC 9(7)  VALUE ZERO.
           05  WS-MATCH-SUB            PIC S9(4) COMP VALUE ZERO.
           05  WS-SUB                  PIC S9(4) COMP VALUE ZERO.
           05  WS-SCORE-WORK           PIC 9(3)  VALUE ZERO.
           05  WS-CHAR-HITS            PIC 9(3)  VALUE ZERO.
           05  WS-ABEND-CODE           PIC S9(4) COMP VALUE ZERO.
      *
       01  WS-NAME-WORK.
           05  WS-SEARCH-NAME          PIC X(60) VALUE SPACES.
           05  WS-BROWSE-NAME          PIC X(60) VALUE SPACES.
           05  WS-CAND-NAME            PIC X(60) VALUE SPACES.
           05  WS-NAME-LEN             PIC S9(4) COMP VALUE ZERO.
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME-R REDEFINES WS-CURR-TIME.
               10  WS-CURR-HHMMSS      PIC 9(6).
               10  WS-CURR-HUND        PIC 9(2).
      *
      *    LAST SCREENING DATE IS STILL HELD AS A SIX DIGIT DATE ON
      *    THE WATCH LIST TRAILER.  WINDOW IT BEFORE COMPARING.
           05  WS-SIX-DATE             PIC 9(6)  VALUE ZERO.
           05  WS-SIX-DATE-R REDEFINES WS-SIX-DATE.
               10  WS-SIX-YY           PIC 9(2).
               10  WS-SIX-MM           PIC 9(2).
               10  WS-SIX-DD           PIC 9(2).
           05  WS-FULL-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-FULL-DATE-R REDEFINES WS-FULL-DATE.
               10  WS-FULL-CC          PIC 9(2).
               10  WS-FULL-YYMMDD      PIC 9(6).
      *
       01  WS-KYC-HOST.
           05  DCL-PARTY-ID            PIC X(11).
           05  DCL-KYC-SEQ             PIC S9(4) COMP.
           05  DCL-KYC-STATUS          PIC X(2).
           05  DCL-KYC-LEVEL           PIC X(4).
           05  DCL-REVIEW-TYPE         PIC X(4).
           05  DCL-REVIEW-DATE         PIC X(10).
           05  DCL-NEXT-REVIEW         PIC X(10).
           05  DCL-EXPIRY-DATE         PIC X(10).
           05  DCL-RISK-RATING         PIC X(1).
           05  DCL-EDD-FLG             PIC X(1).
           05  DCL-DAYS-OVERDUE        PIC S9(9) COMP.
      *
       01  WS-KYC-IND.
           05  IND-NEXT-REVIEW         PIC S9(4) COMP.
           05  IND-EXPIRY              PIC S9(4) COMP.
           05  IND-RISK-RATING         PIC S9(4) COMP.
      *
       01  WS-SQL-DISP                 PIC -(9)9.
      *
           COPY CVWATC1Y.
      *
           COPY CVSANC01Y.
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
           COPY CVPARTY1Y.
      *
       01  LK-RETURN-AREA.
           05  LK-RETURN-CD            PIC S9(4) COMP.
           05  LK-RETURN-PGM           PIC X(8).
           05  LK-RETURN-MSG           PIC X(60).
      *
      ******************************************************************
       PROCEDURE DIVISION USING CV-RISK-AREA
                                PARTY-RECORD
                                LK-RETURN-AREA.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
      *
           PERFORM 1000-CHECK-KYC
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 2000-SCREEN-WATCHLIST
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 3000-POST-SCREEN-DECISION
           PERFORM 4000-CALL-EXPOSURE
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
           MOVE 'N'                    TO WS-KYC-FOUND-SW
           MOVE ZERO                   TO WS-OWN-RC
           MOVE ZERO                   TO WS-CHAIN-RC
           MOVE ZERO                   TO WS-WORST-RC
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
      *
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           ACCEPT WS-CURR-TIME         FROM TIME
      *
           MOVE PT-PARTY-ID            TO DCL-PARTY-ID
           MOVE SPACES                 TO SANCTION-MATCH
           MOVE PT-PARTY-ID            TO SM-PARTY-ID
           MOVE WS-CURR-DATE           TO SM-SCREEN-DATE
           MOVE ZERO                   TO SM-MATCH-CNT
           MOVE ZERO                   TO SM-BEST-SCORE
           MOVE 'N'                    TO SM-HIT-FLG
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
               MOVE SPACES             TO SM-MT-LIST-CD(WS-SUB)
               MOVE SPACES             TO SM-MT-ENTRY-ID(WS-SUB)
               MOVE ZERO               TO SM-MT-SCORE(WS-SUB)
               MOVE SPACES             TO SM-MT-DISPOSITION(WS-SUB)
           END-PERFORM
           .
      *
      ******************************************************************
      * 1000 - KYC REVIEW STATUS                                       *
      *                                                                *
      * THE LATEST KYC ROW WINS.  A REVIEW THAT HAS FALLEN DUE MOVES   *
      * THE PARTY TO PN AND IS A WARNING, NOT A DECLINE.  A ROW        *
      * ALREADY CARRYING FL IS A HARD CONDITION.                       *
      ******************************************************************
       1000-CHECK-KYC.
           MOVE '1000-CHECK-KYC'       TO WS-PARAGRAPH
      *
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
                    , COALESCE(DAYS(CURRENT DATE)
                             - DAYS(NEXT_REVIEW_DATE), 0)
                 INTO :DCL-KYC-SEQ
                    , :DCL-KYC-STATUS
                    , :DCL-KYC-LEVEL
                    , :DCL-REVIEW-TYPE
                    , :DCL-REVIEW-DATE
                    , :DCL-NEXT-REVIEW  :IND-NEXT-REVIEW
                    , :DCL-EXPIRY-DATE  :IND-EXPIRY
                    , :DCL-RISK-RATING  :IND-RISK-RATING
                    , :DCL-EDD-FLG
                    , :DCL-DAYS-OVERDUE
                 FROM PARTYRSK.PARTY_KYC
                WHERE PARTY_ID = :DCL-PARTY-ID
                ORDER BY KYC_SEQ DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-KYC-FOUND-SW
                   PERFORM 1100-EVALUATE-KYC
               WHEN +100
      *            NO KYC RECORD AT ALL.  THE PARTY WAS ONBOARDED
      *            BEFORE THE 2003 REMEDIATION AND NEVER PICKED UP.
                   MOVE 'EX'           TO CV-RISK-KYC-STATUS
                   MOVE 4              TO WS-OWN-RC
                   MOVE 'NO KYC RECORD - PARTY PREDATES REMEDIATION'
                                       TO CV-RISK-REASON-TXT
                   DISPLAY 'PRBRSK2  NO KYC ROW PARTY=' DCL-PARTY-ID
               WHEN OTHER
                   MOVE 'PARTY_KYC        ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       1100-EVALUATE-KYC.
           MOVE DCL-KYC-STATUS         TO CV-RISK-KYC-STATUS
      *
           EVALUATE TRUE
               WHEN DCL-KYC-STATUS = 'FL'
                   MOVE 8              TO WS-OWN-RC
                   MOVE 'KYC FAILED - PARTY MUST NOT BE ONBOARDED'
                                       TO CV-RISK-REASON-TXT
                   DISPLAY 'PRBRSK2  KYC FAILED PARTY=' DCL-PARTY-ID
                           ' SEQ=' DCL-KYC-SEQ
      *
               WHEN IND-NEXT-REVIEW < ZERO
                   MOVE 'EX'           TO CV-RISK-KYC-STATUS
                   MOVE 4              TO WS-OWN-RC
                   MOVE 'NO NEXT REVIEW DATE SET ON LATEST KYC ROW'
                                       TO CV-RISK-REASON-TXT
      *
               WHEN DCL-DAYS-OVERDUE > 90
                   MOVE 'EX'           TO CV-RISK-KYC-STATUS
                   MOVE 4              TO WS-OWN-RC
                   MOVE 'KYC REVIEW MORE THAN 90 DAYS OVERDUE'
                                       TO CV-RISK-REASON-TXT
                   DISPLAY 'PRBRSK2  KYC EXPIRED PARTY=' DCL-PARTY-ID
                           ' DAYS OVERDUE=' DCL-DAYS-OVERDUE
      *
               WHEN DCL-DAYS-OVERDUE > 0
                   MOVE 'PN'           TO CV-RISK-KYC-STATUS
                   MOVE 4              TO WS-OWN-RC
                   MOVE 'KYC REVIEW OVERDUE - PENDING REFRESH'
                                       TO CV-RISK-REASON-TXT
      *
               WHEN OTHER
                   MOVE 'OK'           TO CV-RISK-KYC-STATUS
           END-EVALUATE
      *
      *    ENHANCED DUE DILIGENCE AND PEP PARTIES ARE ALWAYS SCREENED
      *    AGAINST THE FULL LIST, EVEN WHEN THE REVIEW IS IN DATE.
           IF DCL-EDD-FLG = 'Y' OR PT-IS-PEP
               MOVE 'EDD '             TO CV-RISK-REASON-CD
           END-IF
           .
      *
      ******************************************************************
      * 2000 - WATCH LIST SCREENING                                    *
      ******************************************************************
       2000-SCREEN-WATCHLIST.
           MOVE '2000-SCREEN-WATCHLIST' TO WS-PARAGRAPH
      *
           PERFORM 2100-OPEN-WATCHLIST
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2200-BUILD-SEARCH-NAME
           PERFORM 2300-BROWSE-MATCHES
           PERFORM 2900-CLOSE-WATCHLIST
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-OPEN-WATCHLIST.
           MOVE '2100-OPEN-WATCHLIST'  TO WS-PARAGRAPH
           MOVE 'N'                    TO WS-WATCH-OPEN-SW
      *
           OPEN INPUT WATCHNAM-FILE
      *
           EVALUATE WS-WATCH-STATUS
               WHEN '00'
               WHEN '97'
                   MOVE 'Y'            TO WS-WATCH-OPEN-SW
               WHEN OTHER
                   MOVE 'WATCHNAM'     TO ER-FILE-NAME
                   MOVE 'OPEN'         TO ER-SQL-OPERATION
                   PERFORM 9200-VSAM-ERROR
                   MOVE 12             TO WS-OWN-RC
                   MOVE 'Y'            TO WS-FATAL-SW
                   MOVE 'WATCH LIST NOT AVAILABLE - SCREENING FAILED'
                                       TO CV-RISK-REASON-TXT
      *            A 35 IS A MISSING FILE AND THE CALLER CAN DECIDE
      *            WHAT TO DO WITH IT.  ANYTHING ELSE IS A BROKEN
      *            CLUSTER AND EVERY PARTY AFTER THIS ONE WOULD FAIL
      *            THE SAME WAY, SO THE STEP GOES DOWN NOW.
                   IF WS-WATCH-STATUS NOT = '35'
                       PERFORM 9900-ABEND
                   END-IF
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2200 - BUILD THE SEARCH NAME                                   *
      *                                                                *
      * THE LIST IS HELD IN UPPER CASE WITH PUNCTUATION REMOVED.  WE   *
      * REPRODUCE THE SAME NORMALISATION ON THE PARTY NAME SO THAT     *
      * THE GENERIC BROWSE LINES UP.                                   *
      ******************************************************************
       2200-BUILD-SEARCH-NAME.
           MOVE '2200-BUILD-SEARCH-NAME' TO WS-PARAGRAPH
           MOVE SPACES                 TO WS-SEARCH-NAME
           MOVE PT-LEGAL-NAME          TO WS-SEARCH-NAME
      *
           INSPECT WS-SEARCH-NAME
               REPLACING ALL ','  BY ' '
                         ALL '.'  BY ' '
                         ALL '-'  BY ' '
                         ALL '''' BY ' '
      *
           MOVE ZERO                   TO WS-NAME-LEN
           PERFORM VARYING WS-SUB FROM 60 BY -1
                     UNTIL WS-SUB < 1
                        OR WS-NAME-LEN > ZERO
               IF WS-SEARCH-NAME(WS-SUB:1) NOT = SPACE
                   MOVE WS-SUB         TO WS-NAME-LEN
               END-IF
           END-PERFORM
      *
           IF WS-NAME-LEN < 4
      *        TOO SHORT TO SCREEN ON.  FLAG IT AND LET THE ANALYST
      *        PICK IT UP FROM THE EXCEPTION REPORT.
               MOVE 4                  TO WS-OWN-RC
               MOVE 'NAME TOO SHORT TO SCREEN - MANUAL CHECK NEEDED'
                                       TO CV-RISK-REASON-TXT
           END-IF
      *
           MOVE WS-SEARCH-NAME         TO WS-BROWSE-NAME
           .
      *
      ******************************************************************
      * 2300 - GENERIC BROWSE ON THE NAME PATH                         *
      *                                                                *
      * READ FORWARD WHILE THE FIRST WS-NAME-LEN CHARACTERS STILL      *
      * MATCH.  UP TO FIVE CANDIDATES ARE KEPT.                        *
      ******************************************************************
       2300-BROWSE-MATCHES.
           MOVE '2300-BROWSE-MATCHES'  TO WS-PARAGRAPH
           MOVE 'N'                    TO WS-BROWSE-EOF-SW
           MOVE ZERO                   TO WS-READ-CNT
      *
           IF WS-NAME-LEN < 4
               GO TO 2300-EXIT
           END-IF
      *
           MOVE WS-BROWSE-NAME         TO WLF-ENTITY-NAME
      *
           START WATCHNAM-FILE KEY NOT LESS THAN WLF-ENTITY-NAME
           EVALUATE WS-WATCH-STATUS
               WHEN '00'
                   CONTINUE
               WHEN '23'
               WHEN '10'
                   MOVE 'Y'            TO WS-BROWSE-EOF-SW
               WHEN OTHER
                   MOVE 'WATCHNAM'     TO ER-FILE-NAME
                   MOVE 'START   '     TO ER-SQL-OPERATION
                   PERFORM 9200-VSAM-ERROR
                   MOVE 'Y'            TO WS-BROWSE-EOF-SW
                   MOVE 12             TO WS-OWN-RC
                   MOVE 'Y'            TO WS-FATAL-SW
           END-EVALUATE
      *
           PERFORM UNTIL WS-BROWSE-EOF
                      OR SM-MATCH-CNT >= 5
               READ WATCHNAM-FILE NEXT RECORD INTO WATCH-LIST-RECORD
               EVALUATE WS-WATCH-STATUS
                   WHEN '00'
                   WHEN '02'
                       ADD 1           TO WS-READ-CNT
                       PERFORM 2400-TEST-CANDIDATE
                   WHEN '10'
                       MOVE 'Y'        TO WS-BROWSE-EOF-SW
                   WHEN OTHER
                       MOVE 'WATCHNAM' TO ER-FILE-NAME
                       MOVE 'READNEXT' TO ER-SQL-OPERATION
                       PERFORM 9200-VSAM-ERROR
                       MOVE 'Y'        TO WS-BROWSE-EOF-SW
                       MOVE 12         TO WS-OWN-RC
                       MOVE 'Y'        TO WS-FATAL-SW
               END-EVALUATE
      *
      *        MORE THAN 500 CANDIDATES MEANS THE NAME IS FAR TOO
      *        COMMON TO SCREEN AUTOMATICALLY.
               IF WS-READ-CNT > 500
                   MOVE 'Y'            TO WS-BROWSE-EOF-SW
                   MOVE 4              TO WS-OWN-RC
                   MOVE 'SCREENING ABANDONED - NAME TOO COMMON'
                                       TO CV-RISK-REASON-TXT
                   DISPLAY 'PRBRSK2  BROWSE ABANDONED PARTY='
                           PT-PARTY-ID ' READS=' WS-READ-CNT
               END-IF
           END-PERFORM
           .
       2300-EXIT.
           EXIT
           .
      *
       2400-TEST-CANDIDATE.
           MOVE WL-ENTITY-NAME         TO WS-CAND-NAME
      *
           IF WS-CAND-NAME(1:WS-NAME-LEN)
              NOT = WS-BROWSE-NAME(1:WS-NAME-LEN)
               MOVE 'Y'                TO WS-BROWSE-EOF-SW
               GO TO 2400-EXIT
           END-IF
      *
      *    DELISTED ENTRIES STAY ON THE FILE FOR AUDIT.  WINDOW THE
      *    SIX DIGIT LOAD DATE BEFORE DECIDING HOW STALE IT IS.
           IF WL-DELISTED
               MOVE WL-LOAD-DATE       TO WS-SIX-DATE
               PERFORM 2500-WINDOW-DATE
               IF WS-FULL-DATE > ZERO
                   CONTINUE
               END-IF
               GO TO 2400-EXIT
           END-IF
      *
           PERFORM 2600-SCORE-CANDIDATE
      *
           IF WS-SCORE-WORK >= 70
               ADD 1                   TO SM-MATCH-CNT
               MOVE SM-MATCH-CNT       TO WS-MATCH-SUB
               MOVE WL-LIST-CD         TO SM-MT-LIST-CD(WS-MATCH-SUB)
               MOVE WL-ENTRY-ID        TO SM-MT-ENTRY-ID(WS-MATCH-SUB)
               MOVE WS-SCORE-WORK      TO SM-MT-SCORE(WS-MATCH-SUB)
      *
               IF WS-SCORE-WORK >= 95
                   MOVE 'HIT '         TO
                        SM-MT-DISPOSITION(WS-MATCH-SUB)
                   MOVE 'Y'            TO SM-HIT-FLG
               ELSE
                   MOVE 'POSS'         TO
                        SM-MT-DISPOSITION(WS-MATCH-SUB)
                   IF SM-NO-HIT
                       MOVE 'P'        TO SM-HIT-FLG
                   END-IF
               END-IF
      *
               IF WS-SCORE-WORK > SM-BEST-SCORE
                   MOVE WS-SCORE-WORK  TO SM-BEST-SCORE
               END-IF
           END-IF
           .
       2400-EXIT.
           EXIT
           .
      *
       2500-WINDOW-DATE.
           MOVE ZERO                   TO WS-FULL-DATE
           IF WS-SIX-DATE = ZERO
               GO TO 2500-EXIT
           END-IF
           IF WS-SIX-YY > WS-CENTURY-PIVOT
               MOVE WS-CENTURY-19      TO WS-FULL-CC
           ELSE
               MOVE WS-CENTURY-20      TO WS-FULL-CC
           END-IF
           MOVE WS-SIX-DATE            TO WS-FULL-YYMMDD
           .
       2500-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2600 - CANDIDATE SCORING                                       *
      *                                                                *
      * BASE SCORE FROM THE LENGTH OF THE COMMON PREFIX, ADJUSTED FOR  *
      * COUNTRY AND DATE OF BIRTH.  THE ONLINE SCREENER HAS ITS OWN    *
      * VERSION OF THIS.                                               *
      ******************************************************************
       2600-SCORE-CANDIDATE.
           MOVE ZERO                   TO WS-SCORE-WORK
           MOVE ZERO                   TO WS-CHAR-HITS
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 60
               IF WS-CAND-NAME(WS-SUB:1) = WS-SEARCH-NAME(WS-SUB:1)
                   ADD 1               TO WS-CHAR-HITS
               END-IF
           END-PERFORM
      *
           COMPUTE WS-SCORE-WORK = (WS-CHAR-HITS * 100) / 60
      *
           IF WL-COUNTRY = PT-DOMICILE-CTRY
              OR WL-COUNTRY = PT-CITIZENSHIP
               ADD 10                  TO WS-SCORE-WORK
           END-IF
      *
           IF WL-DOB NOT = ZERO
              AND PT-DOB-INCORP NOT = ZERO
               IF WL-DOB = PT-DOB-INCORP
                   ADD 15              TO WS-SCORE-WORK
               ELSE
                   SUBTRACT 25       FROM WS-SCORE-WORK
               END-IF
           END-IF
      *
           IF PT-ORGANISATION AND WL-TYPE-INDIVIDUAL
               SUBTRACT 20             FROM WS-SCORE-WORK
           END-IF
           IF PT-INDIVIDUAL AND WL-TYPE-ORGANISATION
               SUBTRACT 20             FROM WS-SCORE-WORK
           END-IF
      *
           IF WS-SCORE-WORK > 999
               MOVE 999                TO WS-SCORE-WORK
           END-IF
           .
      *
       2900-CLOSE-WATCHLIST.
           MOVE '2900-CLOSE-WATCHLIST' TO WS-PARAGRAPH
           IF NOT WS-WATCH-OPEN
               GO TO 2900-EXIT
           END-IF
      *
           CLOSE WATCHNAM-FILE
           IF WS-WATCH-STATUS NOT = '00'
               MOVE 'WATCHNAM'         TO ER-FILE-NAME
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9200-VSAM-ERROR
           END-IF
           MOVE 'N'                    TO WS-WATCH-OPEN-SW
           .
       2900-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - SCREENING OUTCOME                                       *
      ******************************************************************
       3000-POST-SCREEN-DECISION.
           MOVE '3000-POST-SCREEN-DECISION' TO WS-PARAGRAPH
      *
           EVALUATE TRUE
               WHEN SM-HIT
                   MOVE 'Y'            TO CV-RISK-SANCTION-FLG
                   MOVE 8              TO WS-OWN-RC
                   MOVE 'SANC'         TO CV-RISK-REASON-CD
                   MOVE 'CONFIRMED WATCH LIST MATCH'
                                       TO CV-RISK-REASON-TXT
                   DISPLAY 'PRBRSK2  WATCH LIST HIT PARTY='
                           PT-PARTY-ID
                           ' LIST=' SM-MT-LIST-CD(1)
                           ' ENTRY=' SM-MT-ENTRY-ID(1)
                           ' SCORE=' SM-BEST-SCORE
      *
               WHEN SM-POSSIBLE
                   MOVE 'N'            TO CV-RISK-SANCTION-FLG
                   IF WS-OWN-RC < 4
                       MOVE 4          TO WS-OWN-RC
                   END-IF
                   MOVE 'POSS'         TO CV-RISK-REASON-CD
                   MOVE 'POSSIBLE WATCH LIST MATCH - REFER TO ANALYST'
                                       TO CV-RISK-REASON-TXT
                   DISPLAY 'PRBRSK2  POSSIBLE MATCH PARTY='
                           PT-PARTY-ID
                           ' CANDIDATES=' SM-MATCH-CNT
                           ' BEST=' SM-BEST-SCORE
      *
               WHEN OTHER
                   MOVE 'N'            TO CV-RISK-SANCTION-FLG
           END-EVALUATE
      *
           MOVE WS-OWN-RC              TO WS-WORST-RC
           .
      *
      ******************************************************************
      * 4000 - EXPOSURE REBUILD                                        *
      *                                                                *
      * A CONFIRMED HIT STILL GOES DOWN THE CHAIN.  THE REGULATOR      *
      * EXPECTS THE EXPOSURE AND THE SCORE TO BE CURRENT ON A PARTY    *
      * THAT HAS BEEN STOPPED.                                         *
      ******************************************************************
       4000-CALL-EXPOSURE.
           MOVE '4000-CALL-EXPOSURE'   TO WS-PARAGRAPH
           MOVE ZERO                   TO LK-RETURN-CD
      *
           CALL 'PRBRSK3' USING CV-RISK-AREA
                                PARTY-RECORD
                                SANCTION-MATCH
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
           MOVE 12                     TO WS-OWN-RC
           MOVE 'Y'                    TO WS-FATAL-SW
      *
           DISPLAY 'PRBRSK2  SQL ERROR PARA=' WS-PARAGRAPH
                   ' TABLE=' ER-SQL-TABLE
           DISPLAY '         SQLCODE=' WS-SQL-DISP
                   ' PARTY=' DCL-PARTY-ID
           .
      *
       9200-VSAM-ERROR.
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE WS-WATCH-STATUS        TO ER-FILE-STATUS
           MOVE 'E'                    TO ER-SEVERITY
           MOVE PT-PARTY-ID            TO ER-VSAM-KEY
      *
           DISPLAY 'PRBRSK2  VSAM ERROR FILE=' ER-FILE-NAME
                   ' OP=' ER-SQL-OPERATION
                   ' STATUS=' WS-WATCH-STATUS
           DISPLAY '         PARTY=' PT-PARTY-ID
                   ' NAME=' WS-BROWSE-NAME(1:30)
           .
      *
      ******************************************************************
      * 9900 - WATCH LIST UNUSABLE.  SCREENING CANNOT BE SKIPPED SO    *
      *        THE STEP IS FAILED WITH U3102.                          *
      ******************************************************************
       9900-ABEND.
           MOVE 'U310'                 TO ER-ABEND-CODE
           MOVE 'Y'                    TO ER-ABEND-REQUESTED
           DISPLAY 'PRBRSK2  ABEND U3102 PARTY=' PT-PARTY-ID
                   ' STATUS=' WS-WATCH-STATUS
           MOVE 3102                   TO WS-ABEND-CODE
           CALL 'ILBOABN0' USING WS-ABEND-CODE
           .
