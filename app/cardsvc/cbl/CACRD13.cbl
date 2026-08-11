      ******************************************************************
      * CACRD13 - DISPUTE / CHARGEBACK ENTRY                           *
      *                                                                *
      * MENU OPTION 7.  BROWSES RECENT POSTED TRANSACTIONS FOR AN      *
      * ACCOUNT, LETS THE OPERATOR SELECT ONE AND RAISES A DISPUTE     *
      * AGAINST IT.  WHERE THE REASON CODE AND THE AMOUNT QUALIFY A    *
      * PROVISIONAL CREDIT IS POSTED AT THE SAME TIME.                 *
      *                                                                *
      * CALLED BY  - CACRD90 BY XCTL, ROUTE MENU / OPT07               *
      * RETURNS TO - CACRD00 BY XCTL                                   *
      * CALLS      - CACRD91 BY LINK FOR ERROR DISPLAY                 *
      * MAPSET     - CARDST2, MAPS CRD13A AND CRD13B                   *
      * FILES      - RSNCODE   KSDS  READ                              *
      * TABLES     - CARDSVC.TRANSACTION   SELECT UPDATE INSERT        *
      *              CARDSVC.ACCOUNT       SELECT UPDATE               *
      *              CARDSVC.DISPUTE       SELECT INSERT               *
      *              CARDSVC.MERCHANT      SELECT                      *
      *                                                                *
      * ORIGINAL - 1999.  PROVISIONAL CREDIT ADDED 2005 FOR REG Z.     *
      *                                                                *
      * NOTE - THE FILING WINDOW IS STILL CALCULATED FROM THE SIX      *
      * DIGIT POST DATE THE OLD TRANSACTION EXTRACT SUPPLIED.  THE     *
      * COLUMN IS A REAL DATE NOW BUT THE CALCULATION WAS NEVER        *
      * CHANGED, SO THE DATE IS WINDOWED ON THE PIVOT IN CVCONSTY.     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD13.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD13 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDST2 '.
       01  WS-MAP-LIST                 PIC X(8)  VALUE 'CRD13A  '.
       01  WS-MAP-DTL                  PIC X(8)  VALUE 'CRD13B  '.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CACRD00 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE 0.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-EOF-SW               PIC X     VALUE 'N'.
               88  WS-END-OF-CURSOR              VALUE 'Y'.
           05  WS-SELECT-SW            PIC X     VALUE 'N'.
               88  WS-ROW-SELECTED               VALUE 'Y'.
           05  WS-PROV-SW              PIC X     VALUE 'N'.
               88  WS-PROV-CREDIT-DUE            VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-IDX                  PIC S9(4) COMP VALUE 0.
           05  WS-ROW-CNT              PIC S9(4) COMP VALUE 0.
           05  WS-LEG-LEN              PIC S9(4) COMP VALUE 0.
           05  WS-DAYS-ELAPSED         PIC S9(5) COMP-3 VALUE 0.
           05  WS-INT-TODAY            PIC S9(9) COMP VALUE 0.
           05  WS-INT-POST             PIC S9(9) COMP VALUE 0.
      *
      *    THE LEG TABLE STARTS AT OFFSET 150 IN TXN-RECORD AND EACH
      *    OCCURRENCE IS 24 BYTES.  THE IMAGE WRITTEN TO
      *    TRANSACTION.TXN_LEG_DATA IS THAT SLICE ONLY.
       01  WS-LEG-CONSTANTS.
           05  WS-LEG-OFFSET           PIC S9(4) COMP VALUE 150.
           05  WS-LEG-SIZE             PIC S9(4) COMP VALUE 24.
      *
       01  WS-WORK-FIELDS.
           05  WS-EDIT-AMT             PIC ZZZ,ZZZ,ZZ9.99-.
           05  WS-EDIT-LINE            PIC ZZ,ZZZ,ZZ9.99-.
           05  WS-INPUT-AMT            PIC X(13) VALUE SPACES.
           05  WS-NUMERIC-IN           PIC 9(11)V99 VALUE ZERO.
           05  WS-DISPLAY-DATE         PIC X(10) VALUE SPACES.
           05  WS-WINDOW-TEXT          PIC X(40) VALUE SPACES.
           05  WS-DUP-COUNT            PIC S9(4) COMP VALUE 0.
      *
      ******************************************************************
      * DATE AREAS.  THE SIX DIGIT POST DATE IS WINDOWED ON THE PIVOT  *
      * HELD IN CVCONSTY - YY BELOW THE PIVOT IS 20XX, AT OR ABOVE IT  *
      * IS 19XX.                                                       *
      ******************************************************************
       01  WS-DATE-AREAS.
           05  WS-TODAY-YYYYMMDD       PIC 9(8)  VALUE ZERO.
           05  WS-TODAY-R REDEFINES WS-TODAY-YYYYMMDD.
               10  WS-TODAY-CC         PIC 9(2).
               10  WS-TODAY-YY         PIC 9(2).
               10  WS-TODAY-MM         PIC 9(2).
               10  WS-TODAY-DD         PIC 9(2).
           05  WS-SIX-DATE             PIC 9(6)  VALUE ZERO.
           05  WS-SIX-DATE-R REDEFINES WS-SIX-DATE.
               10  WS-SIX-YY           PIC 9(2).
               10  WS-SIX-MM           PIC 9(2).
               10  WS-SIX-DD           PIC 9(2).
           05  WS-FULL-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-FULL-DATE-R REDEFINES WS-FULL-DATE.
               10  WS-FULL-CC          PIC 9(2).
               10  WS-FULL-YY          PIC 9(2).
               10  WS-FULL-MM          PIC 9(2).
               10  WS-FULL-DD          PIC 9(2).
           05  WS-ISO-DATE             PIC X(10) VALUE SPACES.
           05  WS-ISO-R REDEFINES WS-ISO-DATE.
               10  WS-ISO-YYYY         PIC X(4).
               10  WS-ISO-SEP1         PIC X.
               10  WS-ISO-MM           PIC X(2).
               10  WS-ISO-SEP2         PIC X.
               10  WS-ISO-DD           PIC X(2).
      *
       01  WS-JULIAN.
           05  WS-JUL-YY               PIC 9(2)  VALUE ZERO.
           05  WS-JUL-DDD              PIC 9(3)  VALUE ZERO.
      *
       01  WS-MESSAGES.
           05  WS-MSG-ACCT-REQD        PIC X(78) VALUE
               'ACCOUNT NUMBER MUST BE ENTERED'.
           05  WS-MSG-ACCT-BAD         PIC X(78) VALUE
               'ACCOUNT NUMBER MUST BE NUMERIC'.
           05  WS-MSG-NO-ACCT          PIC X(78) VALUE
               'ACCOUNT NOT ON FILE'.
           05  WS-MSG-NO-ROWS          PIC X(78) VALUE
               'NO TRANSACTIONS FOUND FOR THAT ACCOUNT AND DATE RANGE'.
           05  WS-MSG-EOF              PIC X(78) VALUE
               'END OF TRANSACTION LIST - PF7 TO PAGE BACK'.
           05  WS-MSG-BOF              PIC X(78) VALUE
               'START OF TRANSACTION LIST'.
           05  WS-MSG-SELECT           PIC X(78) VALUE
               'MARK ONE TRANSACTION WITH S AND PRESS ENTER'.
           05  WS-MSG-ONE-ONLY         PIC X(78) VALUE
               'ONLY ONE TRANSACTION MAY BE SELECTED AT A TIME'.
           05  WS-MSG-ALREADY-DISP     PIC X(78) VALUE
               'TRANSACTION IS ALREADY UNDER DISPUTE'.
           05  WS-MSG-RSN-REQD         PIC X(78) VALUE
               'DISPUTE REASON CODE MUST BE ENTERED'.
           05  WS-MSG-RSN-BAD          PIC X(78) VALUE
               'REASON CODE NOT FOUND FOR CATEGORY DISP'.
           05  WS-MSG-RSN-INACT        PIC X(78) VALUE
               'REASON CODE IS NO LONGER IN USE - SEE NETWORK GUIDE'.
           05  WS-MSG-AMT-BAD          PIC X(78) VALUE
               'DISPUTE AMOUNT MUST BE NUMERIC AND GREATER THAN ZERO'.
           05  WS-MSG-AMT-HIGH         PIC X(78) VALUE
               'DISPUTE AMOUNT CANNOT EXCEED THE TRANSACTION AMOUNT'.
           05  WS-MSG-WINDOW           PIC X(78) VALUE
               'FILING WINDOW HAS EXPIRED FOR THIS REASON CODE'.
           05  WS-MSG-RAISED           PIC X(78) VALUE
               'DISPUTE RAISED'.
           05  WS-MSG-RAISED-PC        PIC X(78) VALUE
               'DISPUTE RAISED AND PROVISIONAL CREDIT POSTED'.
           05  WS-MSG-DUP              PIC X(78) VALUE
               'A DISPUTE ALREADY EXISTS AGAINST THIS TRANSACTION'.
      *
      ******************************************************************
      * PROGRAM COMMAREA - THE BROWSE POSITION IS CARRIED HERE         *
      * BETWEEN PSEUDO CONVERSATIONAL TURNS.                           *
      ******************************************************************
       01  WS-COMMAREA.
           05  CA-PGM-ID               PIC X(8).
           05  CA-FROM-PGM             PIC X(8).
           05  CA-STEP                 PIC X.
               88  CA-STEP-LIST                VALUE '1'.
               88  CA-STEP-DETAIL              VALUE '2'.
           05  CA-ACCT-ID              PIC 9(11).
           05  CA-FROM-DATE            PIC 9(8).
           05  CA-PAGE-NBR             PIC 9(3).
      *    KEY OF THE FIRST AND LAST ROW ON THE PAGE JUST SENT
           05  CA-TOP-KEY.
               10  CA-TOP-POST-DATE    PIC X(10).
               10  CA-TOP-TXN-ID       PIC X(16).
           05  CA-BOT-KEY.
               10  CA-BOT-POST-DATE    PIC X(10).
               10  CA-BOT-TXN-ID       PIC X(16).
           05  CA-DIRECTION            PIC X.
               88  CA-FORWARD                  VALUE 'F'.
               88  CA-BACKWARD                 VALUE 'B'.
           05  CA-EOF-FLG              PIC X.
      *    THE SELECTED TRANSACTION
           05  CA-SEL-TXN-ID           PIC X(16).
           05  CA-SEL-POST-DATE        PIC X(10).
           05  CA-SEL-POST-SIX         PIC 9(6).
           05  CA-SEL-CARD-NUM         PIC X(16).
           05  CA-SEL-AMT              PIC S9(11)V99 COMP-3.
           05  CA-SEL-CURR             PIC X(3).
           05  CA-SEL-MERCH-ID         PIC X(15).
           05  CA-SEL-MERCH-NAME       PIC X(40).
           05  CA-SEL-MCC              PIC X(4).
           05  CA-DISPUTE-ID           PIC X(12).
           05  CA-OPER-ID              PIC X(8).
           05  CA-FILLER               PIC X(288).
      *
           COPY CVTRAN01Y.
           COPY CVRSNC1Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      *    BMS SYMBOLIC MAPS FOR MAPSET CARDST2
           COPY CARDST2.
      *
      ******************************************************************
      * SCREEN LINE TABLE - THE PAGE JUST BUILT                        *
      ******************************************************************
       01  WS-PAGE-TABLE.
           05  WS-PAGE-ROW OCCURS 8 TIMES.
               10  WS-PR-TXN-ID        PIC X(16).
               10  WS-PR-POST-DATE     PIC X(10).
               10  WS-PR-AMT           PIC S9(11)V99 COMP-3.
               10  WS-PR-CURR          PIC X(3).
               10  WS-PR-MERCH-ID      PIC X(15).
               10  WS-PR-MERCH-NAME    PIC X(40).
               10  WS-PR-MCC           PIC X(4).
               10  WS-PR-DISP-FLG      PIC X.
               10  WS-PR-CARD-NUM      PIC X(16).
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-ACCT-ID                 PIC S9(11) COMP-3.
       01  DCL-ACCT-STATUS             PIC X(1).
       01  DCL-CURR-BAL                PIC S9(13)V99 COMP-3.
       01  DCL-CURRENCY-CD             PIC X(3).
      *
       01  DCL-TXN.
           05  DCL-TXN-ID              PIC X(16).
           05  DCL-POST-DATE           PIC X(10).
           05  DCL-POST-SIX            PIC S9(6) COMP-3.
           05  DCL-CARD-NUM            PIC X(16).
           05  DCL-TXN-TYPE-CD         PIC X(4).
           05  DCL-TXN-SOURCE          PIC X(2).
           05  DCL-TXN-AMT             PIC S9(13)V99 COMP-3.
           05  DCL-BILLING-AMT         PIC S9(13)V99 COMP-3.
           05  DCL-TXN-CURR            PIC X(3).
           05  DCL-MERCHANT-ID         PIC X(15).
           05  DCL-MCC                 PIC X(4).
           05  DCL-TXN-DESC            PIC X(40).
           05  DCL-DISPUTE-FLG         PIC X(1).
           05  DCL-AUTH-SEQ-NUM        PIC S9(9) COMP-3.
      *
       01  DCL-MERCH-NAME              PIC X(40).
      *
       01  DCL-DISPUTE.
           05  DCL-DISPUTE-ID          PIC X(12).
           05  DCL-REASON-CD           PIC X(4).
           05  DCL-DISPUTE-AMT         PIC S9(13)V99 COMP-3.
           05  DCL-PROV-FLG            PIC X(1).
           05  DCL-PROV-AMT            PIC S9(13)V99 COMP-3.
           05  DCL-NARRATIVE           PIC X(40).
      *
       01  DCL-CREDIT.
           05  DCL-CR-TXN-ID           PIC X(16).
           05  DCL-CR-AMT              PIC S9(13)V99 COMP-3.
           05  DCL-CR-LEG-CNT          PIC S9(4) COMP.
           05  DCL-CR-LEG-DATA.
               49  DCL-CR-LEG-LEN      PIC S9(4) COMP.
               49  DCL-CR-LEG-TEXT     PIC X(400).
      *
       01  DCL-EXIST-CNT               PIC S9(9) COMP.
      *
      *    BROWSE POSITIONING KEY - THE PAGE STARTS IMMEDIATELY BELOW
      *    THIS ROW IN DESCENDING KEY ORDER
       01  DCL-TOP-DATE                PIC X(10).
       01  DCL-TOP-TXN                 PIC X(16).
      *
       01  IND-MERCHANT                PIC S9(4) COMP.
       01  IND-MCC                     PIC S9(4) COMP.
       01  IND-DESC                    PIC S9(4) COMP.
       01  IND-AUTH-SEQ                PIC S9(4) COMP.
      *
      ******************************************************************
      * THE BROWSE CURSOR.  ROWS ARE RETURNED NEWEST FIRST.  PAGING    *
      * IS BY KEY RANGE - CICS HOLDS NO CURSOR ACROSS A PSEUDO         *
      * CONVERSATIONAL TURN SO IT IS REOPENED ON EVERY PASS.           *
      ******************************************************************
           EXEC SQL DECLARE TXNCSR CURSOR FOR
               SELECT TXN_ID
                    , CHAR(POST_DATE, ISO)
                    , DECIMAL(SUBSTR(CHAR(POST_DATE, ISO),3,2) ||
                              SUBSTR(CHAR(POST_DATE, ISO),6,2) ||
                              SUBSTR(CHAR(POST_DATE, ISO),9,2), 6, 0)
                    , CARD_NUM
                    , TXN_TYPE_CD
                    , TXN_AMT
                    , CURRENCY_CD
                    , MERCHANT_ID
                    , MCC
                    , TXN_DESC
                    , DISPUTE_FLG
                 FROM CARDSVC.TRANSACTION
                WHERE ACCT_ID   = :DCL-ACCT-ID
                  AND POST_DATE >= DATE(:DCL-POST-DATE)
                  AND (POST_DATE < DATE(:DCL-TOP-DATE)
                   OR (POST_DATE = DATE(:DCL-TOP-DATE)
                  AND  TXN_ID    < :DCL-TOP-TXN))
                  AND TXN_TYPE_CD IN ('PURC','CASH','FEE ','INTR')
                ORDER BY POST_DATE DESC
                       , TXN_ID    DESC
               FETCH FIRST 9 ROWS ONLY
           END-EXEC
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE LOW-VALUES             TO CRD13AO
           MOVE LOW-VALUES             TO CRD13BO
      *
           IF EIBCALEN = ZERO
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE 'CACRD13 ENTERED WITH NO COMMAREA'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               PERFORM 0900-RETURN-MENU
               GO TO 0000-EXIT
           END-IF
      *
           MOVE DFHCOMMAREA            TO WS-COMMAREA
           PERFORM 8100-GET-DATE
      *
           IF CA-PGM-ID NOT = WS-PGM-ID
               PERFORM 1000-FIRST-TIME
               GO TO 0000-EXIT
           END-IF
      *
           EVALUATE EIBAID
               WHEN DFHPF3
                   PERFORM 0900-RETURN-MENU
                   GO TO 0000-EXIT
               WHEN DFHPF12
                   PERFORM 9600-EXIT-SESSION
                   GO TO 0000-EXIT
               WHEN DFHCLEAR
                   PERFORM 1000-FIRST-TIME
                   GO TO 0000-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF CA-STEP-DETAIL
               PERFORM 5000-DETAIL-PASS
           ELSE
               PERFORM 2000-LIST-PASS
           END-IF
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
       0900-RETURN-MENU.
           MOVE SPACES                 TO WS-COMMAREA
           MOVE WS-PGM-ID              TO CA-FROM-PGM
           EXEC CICS XCTL
                     PROGRAM(WS-MENU-PGM)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'XCTL TO MENU FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
           END-IF
           .
      *
       1000-FIRST-TIME.
           MOVE SPACES                 TO WS-COMMAREA
           MOVE WS-PGM-ID              TO CA-PGM-ID
           MOVE '1'                    TO CA-STEP
           MOVE EIBTRMID               TO CA-OPER-ID
           MOVE ZERO                   TO CA-ACCT-ID
           MOVE ZERO                   TO CA-FROM-DATE
           MOVE ZERO                   TO CA-PAGE-NBR
           MOVE ZERO                   TO CA-SEL-AMT
           MOVE ZERO                   TO CA-SEL-POST-SIX
           MOVE 'F'                    TO CA-DIRECTION
           MOVE 'N'                    TO CA-EOF-FLG
           MOVE WS-MSG-SELECT          TO M13MSGO
           PERFORM 8200-SEND-LIST
           .
      *
      ******************************************************************
      * 2000 - TRANSACTION LIST PASS                                   *
      ******************************************************************
       2000-LIST-PASS.
           MOVE 'N'                    TO WS-ERROR-SW
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-LIST)
                     MAPSET(WS-MAPSET)
                     INTO(CRD13AI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE WS-MSG-ACCT-REQD
                                       TO M13MSGO
                   PERFORM 8200-SEND-LIST
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE 'RECEIVE MAP CRD13A FAILED'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           PERFORM 2100-EDIT-LIST-INPUT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-LIST
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2200-CHECK-ACCOUNT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-LIST
               GO TO 2000-EXIT
           END-IF
      *
      *    A SELECTION TAKES PRIORITY OVER PAGING
           PERFORM 2300-FIND-SELECTION
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-LIST
               GO TO 2000-EXIT
           END-IF
      *
           IF WS-ROW-SELECTED
               MOVE '2'                TO CA-STEP
               PERFORM 4000-PAINT-DETAIL
               PERFORM 8300-SEND-DETAIL
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2400-SET-BROWSE-KEY
           PERFORM 3000-BUILD-PAGE
      *
           IF WS-ROW-CNT = ZERO
               IF CA-PAGE-NBR = ZERO
                   MOVE WS-MSG-NO-ROWS TO M13MSGO
               ELSE
                   MOVE WS-MSG-EOF     TO M13MSGO
               END-IF
           ELSE
               MOVE WS-MSG-SELECT      TO M13MSGO
           END-IF
      *
           PERFORM 8200-SEND-LIST
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-EDIT-LIST-INPUT.
           IF M13ACCTL = ZERO OR M13ACCTI = SPACES
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ACCT-REQD   TO M13MSGO
               GO TO 2100-EXIT
           END-IF
      *
           IF M13ACCTI IS NOT NUMERIC
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ACCT-BAD    TO M13MSGO
               GO TO 2100-EXIT
           END-IF
      *
           IF M13ACCTI NOT = CA-ACCT-ID
      *        A NEW ACCOUNT RESTARTS THE BROWSE
               MOVE ZERO               TO CA-PAGE-NBR
               MOVE SPACES             TO CA-TOP-KEY
               MOVE SPACES             TO CA-BOT-KEY
               MOVE 'N'                TO CA-EOF-FLG
           END-IF
           MOVE M13ACCTI               TO CA-ACCT-ID
      *
      *    THE FROM DATE MAY BE KEYED AS YYMMDD OR YYYYMMDD.  SIX
      *    DIGIT ENTRY IS WINDOWED THE SAME WAY THE EXTRACT DATES ARE.
           IF M13FROML > ZERO AND M13FROMI NOT = SPACES
               IF M13FROMI(7:2) = SPACES
                   MOVE M13FROMI(1:6)  TO WS-SIX-DATE
                   PERFORM 7100-WINDOW-SIX-DIGIT
                   MOVE WS-FULL-DATE   TO CA-FROM-DATE
               ELSE
                   MOVE M13FROMI(1:8)  TO CA-FROM-DATE
               END-IF
           ELSE
               IF CA-FROM-DATE = ZERO
      *            DEFAULT IS THE LAST TWO YEARS
                   COMPUTE CA-FROM-DATE =
                       WS-TODAY-YYYYMMDD - 20000
               END-IF
           END-IF
           .
       2100-EXIT.
           EXIT
           .
      *
       2200-CHECK-ACCOUNT.
           MOVE CA-ACCT-ID             TO DCL-ACCT-ID
      *
           EXEC SQL
               SELECT ACCT_STATUS
                    , CURR_BAL
                    , CURRENCY_CD
                 INTO :DCL-ACCT-STATUS
                    , :DCL-CURR-BAL
                    , :DCL-CURRENCY-CD
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NO-ACCT TO M13MSGO
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'ACCOUNT           '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       2300-FIND-SELECTION.
           MOVE 'N'                    TO WS-SELECT-SW
           MOVE ZERO                   TO WS-DUP-COUNT
      *
           IF EIBAID = DFHPF7 OR EIBAID = DFHPF8
               GO TO 2300-EXIT
           END-IF
      *
           IF M13SEL1I = 'S' OR 'X'
               MOVE 1                  TO WS-IDX
               ADD 1                   TO WS-DUP-COUNT
           END-IF
           IF M13SEL2I = 'S' OR 'X'
               MOVE 2                  TO WS-IDX
               ADD 1                   TO WS-DUP-COUNT
           END-IF
           IF M13SEL3I = 'S' OR 'X'
               MOVE 3                  TO WS-IDX
               ADD 1                   TO WS-DUP-COUNT
           END-IF
           IF M13SEL4I = 'S' OR 'X'
               MOVE 4                  TO WS-IDX
               ADD 1                   TO WS-DUP-COUNT
           END-IF
           IF M13SEL5I = 'S' OR 'X'
               MOVE 5                  TO WS-IDX
               ADD 1                   TO WS-DUP-COUNT
           END-IF
           IF M13SEL6I = 'S' OR 'X'
               MOVE 6                  TO WS-IDX
               ADD 1                   TO WS-DUP-COUNT
           END-IF
           IF M13SEL7I = 'S' OR 'X'
               MOVE 7                  TO WS-IDX
               ADD 1                   TO WS-DUP-COUNT
           END-IF
           IF M13SEL8I = 'S' OR 'X'
               MOVE 8                  TO WS-IDX
               ADD 1                   TO WS-DUP-COUNT
           END-IF
      *
           IF WS-DUP-COUNT = ZERO
               GO TO 2300-EXIT
           END-IF
      *
           IF WS-DUP-COUNT > 1
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ONE-ONLY    TO M13MSGO
               GO TO 2300-EXIT
           END-IF
      *
      *    THE PAGE TABLE IS REBUILT BEFORE THE SELECTION IS RESOLVED
      *    BECAUSE NOTHING BUT THE KEYS SURVIVES THE PSEUDO
      *    CONVERSATIONAL TURN.
           PERFORM 2400-SET-BROWSE-KEY
           PERFORM 3000-BUILD-PAGE
      *
           IF WS-IDX > WS-ROW-CNT
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE 'NOTHING ON THAT LINE - RESELECT'
                                       TO M13MSGO
               GO TO 2300-EXIT
           END-IF
      *
           IF WS-PR-DISP-FLG(WS-IDX) = 'Y'
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ALREADY-DISP
                                       TO M13MSGO
               GO TO 2300-EXIT
           END-IF
      *
           MOVE WS-PR-TXN-ID(WS-IDX)   TO CA-SEL-TXN-ID
           MOVE WS-PR-POST-DATE(WS-IDX)
                                       TO CA-SEL-POST-DATE
           MOVE WS-PR-AMT(WS-IDX)      TO CA-SEL-AMT
           MOVE WS-PR-CURR(WS-IDX)     TO CA-SEL-CURR
           MOVE WS-PR-MERCH-ID(WS-IDX) TO CA-SEL-MERCH-ID
           MOVE WS-PR-MERCH-NAME(WS-IDX)
                                       TO CA-SEL-MERCH-NAME
           MOVE WS-PR-MCC(WS-IDX)      TO CA-SEL-MCC
           MOVE WS-PR-CARD-NUM(WS-IDX) TO CA-SEL-CARD-NUM
      *
      *    KEEP THE SIX DIGIT FORM OF THE POST DATE - THE FILING
      *    WINDOW IS CALCULATED FROM IT
           MOVE CA-SEL-POST-DATE       TO WS-ISO-DATE
           MOVE WS-ISO-YYYY(3:2)       TO WS-SIX-YY
           MOVE WS-ISO-MM              TO WS-SIX-MM
           MOVE WS-ISO-DD              TO WS-SIX-DD
           MOVE WS-SIX-DATE            TO CA-SEL-POST-SIX
      *
           MOVE 'Y'                    TO WS-SELECT-SW
           .
       2300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2400 - WHERE DOES THE NEXT PAGE START                          *
      ******************************************************************
       2400-SET-BROWSE-KEY.
           EVALUATE EIBAID
               WHEN DFHPF8
                   IF CA-EOF-FLG = 'Y'
                       MOVE WS-MSG-EOF TO M13MSGO
                   ELSE
                       MOVE CA-BOT-POST-DATE
                                       TO DCL-TOP-DATE
                       MOVE CA-BOT-TXN-ID
                                       TO DCL-TOP-TXN
                       ADD 1           TO CA-PAGE-NBR
                   END-IF
               WHEN DFHPF7
      *            PAGING BACK RESTARTS FROM THE TOP OF THE PREVIOUS
      *            PAGE.  THE BROWSE IS CHEAP AND THE OPERATORS NEVER
      *            GO BACK MORE THAN A PAGE OR TWO.
                   IF CA-PAGE-NBR > 1
                       SUBTRACT 1      FROM CA-PAGE-NBR
                   ELSE
                       MOVE ZERO       TO CA-PAGE-NBR
                       MOVE WS-MSG-BOF TO M13MSGO
                   END-IF
                   MOVE '9999-12-31'   TO DCL-TOP-DATE
                   MOVE HIGH-VALUES    TO DCL-TOP-TXN
                   PERFORM 2500-SKIP-FORWARD
               WHEN OTHER
                   IF CA-PAGE-NBR = ZERO
                       MOVE '9999-12-31'
                                       TO DCL-TOP-DATE
                       MOVE HIGH-VALUES
                                       TO DCL-TOP-TXN
                   ELSE
                       MOVE '9999-12-31'
                                       TO DCL-TOP-DATE
                       MOVE HIGH-VALUES
                                       TO DCL-TOP-TXN
                       PERFORM 2500-SKIP-FORWARD
                   END-IF
           END-EVALUATE
      *
           MOVE CA-FROM-DATE(1:4)      TO WS-ISO-YYYY
           MOVE '-'                    TO WS-ISO-SEP1
           MOVE CA-FROM-DATE(5:2)      TO WS-ISO-MM
           MOVE '-'                    TO WS-ISO-SEP2
           MOVE CA-FROM-DATE(7:2)      TO WS-ISO-DD
           MOVE WS-ISO-DATE            TO DCL-POST-DATE
           .
      *
      *    POSITION AT THE START OF PAGE CA-PAGE-NBR BY READING AND
      *    DISCARDING THE PRECEDING PAGES.  CRUDE BUT THE VOLUMES ARE
      *    SMALL AND IT SURVIVES A LOST CURSOR.
       2500-SKIP-FORWARD.
           MOVE ZERO                   TO WS-IDX
           PERFORM UNTIL WS-IDX NOT < CA-PAGE-NBR
               PERFORM 3000-BUILD-PAGE
               IF WS-ROW-CNT = ZERO
                   MOVE CA-PAGE-NBR    TO WS-IDX
               ELSE
                   MOVE WS-PR-POST-DATE(WS-ROW-CNT)
                                       TO DCL-TOP-DATE
                   MOVE WS-PR-TXN-ID(WS-ROW-CNT)
                                       TO DCL-TOP-TXN
                   ADD 1               TO WS-IDX
               END-IF
           END-PERFORM
           .
      *
      ******************************************************************
      * 3000 - FETCH ONE PAGE                                          *
      ******************************************************************
       3000-BUILD-PAGE.
           MOVE ZERO                   TO WS-ROW-CNT
           MOVE 'N'                    TO WS-EOF-SW
           MOVE 'N'                    TO CA-EOF-FLG
           MOVE SPACES                 TO WS-PAGE-TABLE
           MOVE CA-ACCT-ID             TO DCL-ACCT-ID
      *
           EXEC SQL OPEN TXNCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM UNTIL WS-END-OF-CURSOR
                      OR WS-ROW-CNT NOT < 8
               PERFORM 3100-FETCH-ROW
           END-PERFORM
      *
      *    ONE EXTRA ROW IS FETCHED TO SEE WHETHER A NEXT PAGE EXISTS
           IF NOT WS-END-OF-CURSOR
               PERFORM 3200-PEEK-AHEAD
           ELSE
               MOVE 'Y'                TO CA-EOF-FLG
           END-IF
      *
           EXEC SQL CLOSE TXNCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-ERROR
           END-IF
      *
           IF WS-ROW-CNT > ZERO
               MOVE WS-PR-POST-DATE(1) TO CA-TOP-POST-DATE
               MOVE WS-PR-TXN-ID(1)    TO CA-TOP-TXN-ID
               MOVE WS-PR-POST-DATE(WS-ROW-CNT)
                                       TO CA-BOT-POST-DATE
               MOVE WS-PR-TXN-ID(WS-ROW-CNT)
                                       TO CA-BOT-TXN-ID
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
       3100-FETCH-ROW.
           EXEC SQL
               FETCH TXNCSR
                INTO :DCL-TXN-ID
                   , :DCL-POST-DATE
                   , :DCL-POST-SIX
                   , :DCL-CARD-NUM
                   , :DCL-TXN-TYPE-CD
                   , :DCL-TXN-AMT
                   , :DCL-TXN-CURR
                   , :DCL-MERCHANT-ID  :IND-MERCHANT
                   , :DCL-MCC          :IND-MCC
                   , :DCL-TXN-DESC     :IND-DESC
                   , :DCL-DISPUTE-FLG
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-ROW-CNT
                   PERFORM 3150-STORE-ROW
               WHEN +100
                   MOVE 'Y'            TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-EOF-SW
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       3150-STORE-ROW.
           MOVE DCL-TXN-ID             TO WS-PR-TXN-ID(WS-ROW-CNT)
           MOVE DCL-POST-DATE          TO WS-PR-POST-DATE(WS-ROW-CNT)
           MOVE DCL-TXN-AMT            TO WS-PR-AMT(WS-ROW-CNT)
           MOVE DCL-TXN-CURR           TO WS-PR-CURR(WS-ROW-CNT)
           MOVE DCL-CARD-NUM           TO WS-PR-CARD-NUM(WS-ROW-CNT)
           MOVE DCL-DISPUTE-FLG        TO WS-PR-DISP-FLG(WS-ROW-CNT)
      *
           IF IND-MERCHANT < ZERO
               MOVE SPACES             TO WS-PR-MERCH-ID(WS-ROW-CNT)
           ELSE
               MOVE DCL-MERCHANT-ID    TO WS-PR-MERCH-ID(WS-ROW-CNT)
           END-IF
      *
           IF IND-MCC < ZERO
               MOVE SPACES             TO WS-PR-MCC(WS-ROW-CNT)
           ELSE
               MOVE DCL-MCC            TO WS-PR-MCC(WS-ROW-CNT)
           END-IF
      *
      *    THE MERCHANT NAME IS ONLY LOOKED UP FOR ROWS THAT CARRY AN
      *    ACQUIRED MERCHANT.  FEES AND INTEREST DO NOT.
           IF WS-PR-MERCH-ID(WS-ROW-CNT) = SPACES
               IF IND-DESC < ZERO
                   MOVE SPACES         TO WS-PR-MERCH-NAME(WS-ROW-CNT)
               ELSE
                   MOVE DCL-TXN-DESC   TO WS-PR-MERCH-NAME(WS-ROW-CNT)
               END-IF
           ELSE
               PERFORM 3180-READ-MERCHANT
               MOVE DCL-MERCH-NAME     TO WS-PR-MERCH-NAME(WS-ROW-CNT)
           END-IF
           .
      *
       3180-READ-MERCHANT.
           MOVE WS-PR-MERCH-ID(WS-ROW-CNT)
                                       TO DCL-MERCHANT-ID
           MOVE SPACES                 TO DCL-MERCH-NAME
      *
           EXEC SQL
               SELECT MERCHANT_NAME
                 INTO :DCL-MERCH-NAME
                 FROM CARDSVC.MERCHANT
                WHERE MERCHANT_ID = :DCL-MERCHANT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'MERCHANT NOT ON FILE'
                                       TO DCL-MERCH-NAME
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'MERCHANT          '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE SPACES         TO DCL-MERCH-NAME
           END-EVALUATE
           .
      *
       3200-PEEK-AHEAD.
           EXEC SQL
               FETCH TXNCSR
                INTO :DCL-TXN-ID
                   , :DCL-POST-DATE
                   , :DCL-POST-SIX
                   , :DCL-CARD-NUM
                   , :DCL-TXN-TYPE-CD
                   , :DCL-TXN-AMT
                   , :DCL-TXN-CURR
                   , :DCL-MERCHANT-ID  :IND-MERCHANT
                   , :DCL-MCC          :IND-MCC
                   , :DCL-TXN-DESC     :IND-DESC
                   , :DCL-DISPUTE-FLG
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'N'            TO CA-EOF-FLG
               WHEN +100
                   MOVE 'Y'            TO CA-EOF-FLG
               WHEN OTHER
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 4000 - DISPUTE DETAIL SCREEN                                   *
      ******************************************************************
       4000-PAINT-DETAIL.
           MOVE CA-SEL-TXN-ID          TO M13BTIDO
           MOVE CA-SEL-POST-DATE       TO M13BPDTO
           MOVE CA-SEL-AMT             TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M13BTAMO
           MOVE CA-SEL-MERCH-NAME      TO M13BMERO
           MOVE CA-SEL-AMT             TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M13BDAMO
           MOVE SPACES                 TO M13BDIDO
           MOVE SPACES                 TO M13BPRVO
           MOVE 'KEY THE REASON CODE AND PRESS ENTER'
                                       TO M13BMSGO
           .
      *
      ******************************************************************
      * 5000 - DISPUTE DETAIL PASS                                     *
      ******************************************************************
       5000-DETAIL-PASS.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-PROV-SW
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-DTL)
                     MAPSET(WS-MAPSET)
                     INTO(CRD13BI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE WS-MSG-RSN-REQD
                                       TO M13BMSGO
                   PERFORM 8300-SEND-DETAIL
                   GO TO 5000-EXIT
               WHEN OTHER
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE 'RECEIVE MAP CRD13B FAILED'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   GO TO 5000-EXIT
           END-EVALUATE
      *
           IF EIBAID = DFHPF7
               MOVE '1'                TO CA-STEP
               MOVE WS-MSG-SELECT      TO M13MSGO
               PERFORM 2400-SET-BROWSE-KEY
               PERFORM 3000-BUILD-PAGE
               PERFORM 8200-SEND-LIST
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5100-EDIT-DETAIL
           IF WS-ERROR-FOUND
               PERFORM 8300-SEND-DETAIL
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5200-CHECK-WINDOW
           IF WS-ERROR-FOUND
               PERFORM 8300-SEND-DETAIL
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5300-CHECK-DUPLICATE
           IF WS-ERROR-FOUND
               PERFORM 8300-SEND-DETAIL
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 6000-RAISE-DISPUTE
      *
           IF NOT WS-ERROR-FOUND
               MOVE CA-DISPUTE-ID      TO M13BDIDO
               IF WS-PROV-CREDIT-DUE
                   MOVE DCL-PROV-AMT   TO WS-EDIT-AMT
                   MOVE WS-EDIT-AMT    TO M13BPRVO
                   MOVE WS-MSG-RAISED-PC
                                       TO M13BMSGO
               ELSE
                   MOVE 'NONE'         TO M13BPRVO
                   MOVE WS-MSG-RAISED  TO M13BMSGO
               END-IF
           END-IF
      *
           PERFORM 8300-SEND-DETAIL
           .
       5000-EXIT.
           EXIT
           .
      *
       5100-EDIT-DETAIL.
           IF M13BRSNL = ZERO OR M13BRSNI = SPACES
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-RSN-REQD    TO M13BMSGO
               GO TO 5100-EXIT
           END-IF
           MOVE M13BRSNI               TO DCL-REASON-CD
      *
           PERFORM 5150-READ-REASON
           IF WS-ERROR-FOUND
               GO TO 5100-EXIT
           END-IF
           MOVE RSN-LONG-DESC(1:40)    TO M13BRDSO
      *
           IF M13BDAML = ZERO OR M13BDAMI = SPACES
               MOVE CA-SEL-AMT         TO DCL-DISPUTE-AMT
           ELSE
               MOVE M13BDAMI           TO WS-INPUT-AMT
               PERFORM 8500-CONVERT-AMOUNT
               IF WS-ERROR-FOUND
                   MOVE WS-MSG-AMT-BAD TO M13BMSGO
                   GO TO 5100-EXIT
               END-IF
               MOVE WS-NUMERIC-IN      TO DCL-DISPUTE-AMT
           END-IF
      *
           IF DCL-DISPUTE-AMT > CA-SEL-AMT
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-AMT-HIGH    TO M13BMSGO
               GO TO 5100-EXIT
           END-IF
      *
           MOVE M13BNARI               TO DCL-NARRATIVE
           IF DCL-NARRATIVE = SPACES OR LOW-VALUES
               MOVE RSN-SHORT-DESC     TO DCL-NARRATIVE
           END-IF
           .
       5100-EXIT.
           EXIT
           .
      *
       5150-READ-REASON.
           MOVE SPACES                 TO RSN-CODE-RECORD
           MOVE 'DISP'                 TO RSN-CATEGORY
           MOVE DCL-REASON-CD          TO RSN-CODE
      *
           EXEC CICS READ
                     FILE('RSNCODE ')
                     INTO(RSN-CODE-RECORD)
                     RIDFLD(RSN-KEY)
                     KEYLENGTH(8)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   IF NOT RSN-ACTIVE
                       MOVE 'Y'        TO WS-ERROR-SW
                       MOVE WS-MSG-RSN-INACT
                                       TO M13BMSGO
                   END-IF
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-RSN-BAD TO M13BMSGO
               WHEN OTHER
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE 'RSNCODE '     TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE RSN-KEY        TO ER-VSAM-KEY
                   MOVE 'READ FAILED ON REASON CODE FILE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      ******************************************************************
      * 5200 - FILING WINDOW                                           *
      *        RSN-FILING-DAYS COMES FROM THE NETWORK RULES ON THE     *
      *        REASON CODE FILE.  THE ELAPSED DAYS ARE MEASURED FROM   *
      *        THE WINDOWED SIX DIGIT POST DATE.                       *
      ******************************************************************
       5200-CHECK-WINDOW.
           MOVE CA-SEL-POST-SIX        TO WS-SIX-DATE
           PERFORM 7100-WINDOW-SIX-DIGIT
      *
           COMPUTE WS-INT-POST =
               FUNCTION INTEGER-OF-DATE (WS-FULL-DATE)
           COMPUTE WS-INT-TODAY =
               FUNCTION INTEGER-OF-DATE (WS-TODAY-YYYYMMDD)
           COMPUTE WS-DAYS-ELAPSED = WS-INT-TODAY - WS-INT-POST
      *
           MOVE SPACES                 TO WS-WINDOW-TEXT
           STRING WS-DAYS-ELAPSED DELIMITED BY SIZE
                  ' DAYS ELAPSED OF '  DELIMITED BY SIZE
                  RSN-FILING-DAYS      DELIMITED BY SIZE
                  ' ALLOWED'           DELIMITED BY SIZE
             INTO WS-WINDOW-TEXT
           END-STRING
           MOVE WS-WINDOW-TEXT         TO M13BWINO
      *
           IF WS-DAYS-ELAPSED > RSN-FILING-DAYS
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-WINDOW      TO M13BMSGO
               GO TO 5200-EXIT
           END-IF
      *
      *    PROVISIONAL CREDIT IS ONLY GIVEN WHERE THE NETWORK RULES
      *    ALLOW IT AND THE AMOUNT IS INSIDE THE BRANCH LIMIT.
           IF RSN-PROV-CREDIT-FLG = 'Y'
               IF DCL-DISPUTE-AMT NOT > 2500.00
                   MOVE 'Y'            TO WS-PROV-SW
               END-IF
           END-IF
           .
       5200-EXIT.
           EXIT
           .
      *
       5300-CHECK-DUPLICATE.
           MOVE CA-SEL-TXN-ID          TO DCL-TXN-ID
           MOVE ZERO                   TO DCL-EXIST-CNT
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-EXIST-CNT
                 FROM CARDSVC.DISPUTE
                WHERE TXN_ID = :DCL-TXN-ID
                  AND STATUS IN ('OP','IN','CB','RP')
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF DCL-EXIST-CNT > ZERO
                       MOVE 'Y'        TO WS-ERROR-SW
                       MOVE WS-MSG-DUP TO M13BMSGO
                   END-IF
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'DISPUTE           '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      ******************************************************************
      * 6000 - RAISE THE DISPUTE                                       *
      ******************************************************************
       6000-RAISE-DISPUTE.
           PERFORM 6100-BUILD-DISPUTE-ID
      *
           IF WS-PROV-CREDIT-DUE
               MOVE 'Y'                TO DCL-PROV-FLG
               MOVE DCL-DISPUTE-AMT    TO DCL-PROV-AMT
           ELSE
               MOVE 'N'                TO DCL-PROV-FLG
               MOVE ZERO               TO DCL-PROV-AMT
           END-IF
      *
           PERFORM 6200-INSERT-DISPUTE
           IF WS-ERROR-FOUND
               GO TO 6000-BACKOUT
           END-IF
      *
           PERFORM 6300-FLAG-TRANSACTION
           IF WS-ERROR-FOUND
               GO TO 6000-BACKOUT
           END-IF
      *
           IF WS-PROV-CREDIT-DUE
               PERFORM 6400-BUILD-CREDIT-LEGS
               PERFORM 6500-INSERT-CREDIT
               IF WS-ERROR-FOUND
                   GO TO 6000-BACKOUT
               END-IF
               PERFORM 6600-UPDATE-BALANCE
               IF WS-ERROR-FOUND
                   GO TO 6000-BACKOUT
               END-IF
           END-IF
      *
           EXEC CICS SYNCPOINT RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SYNCPOINT FAILED ON DISPUTE ENTRY'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           GO TO 6000-EXIT
           .
       6000-BACKOUT.
           EXEC CICS SYNCPOINT ROLLBACK RESP(WS-RESP) END-EXEC
           .
       6000-EXIT.
           EXIT
           .
      *
      *    DISPUTE ID IS D + JULIAN YYDDD + THE TASK NUMBER.  IT HAS
      *    BEEN UNIQUE ENOUGH SINCE 1999.
       6100-BUILD-DISPUTE-ID.
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYDDD(WS-JULIAN)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE SPACES                 TO CA-DISPUTE-ID
           MOVE 'D'                    TO CA-DISPUTE-ID(1:1)
           MOVE WS-JUL-YY              TO CA-DISPUTE-ID(2:2)
           MOVE WS-JUL-DDD             TO CA-DISPUTE-ID(4:3)
           MOVE EIBTASKN               TO CA-DISPUTE-ID(7:6)
           MOVE CA-DISPUTE-ID          TO DCL-DISPUTE-ID
           .
      *
       6200-INSERT-DISPUTE.
           MOVE CA-SEL-TXN-ID          TO DCL-TXN-ID
           MOVE CA-SEL-POST-DATE       TO DCL-POST-DATE
           MOVE CA-ACCT-ID             TO DCL-ACCT-ID
           MOVE CA-SEL-CARD-NUM        TO DCL-CARD-NUM
           MOVE CA-SEL-CURR            TO DCL-CURRENCY-CD
      *
           EXEC SQL
               INSERT INTO CARDSVC.DISPUTE
                     (DISPUTE_ID
                    , TXN_ID
                    , POST_DATE
                    , ACCT_ID
                    , CARD_NUM
                    , RAISED_DATE
                    , REASON_CD
                    , DISPUTE_AMT
                    , CURRENCY_CD
                    , STATUS
                    , PROV_CREDIT_FLG
                    , PROV_CREDIT_AMT
                    , REPRESENTMENT_FLG
                    , RAISED_BY
                    , LAST_MAINT_TS)
               VALUES (:DCL-DISPUTE-ID
                    , :DCL-TXN-ID
                    , DATE(:DCL-POST-DATE)
                    , :DCL-ACCT-ID
                    , :DCL-CARD-NUM
                    , CURRENT DATE
                    , :DCL-REASON-CD
                    , :DCL-DISPUTE-AMT
                    , :DCL-CURRENCY-CD
                    , 'OP'
                    , :DCL-PROV-FLG
                    , :DCL-PROV-AMT
                    , 'N'
                    , :CA-OPER-ID
                    , CURRENT TIMESTAMP)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'DISPUTE ID CLASH - PRESS ENTER TO RETRY'
                                       TO M13BMSGO
               WHEN OTHER
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE 'DISPUTE           '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       6300-FLAG-TRANSACTION.
           EXEC SQL
               UPDATE CARDSVC.TRANSACTION
                  SET DISPUTE_FLG = 'Y'
                WHERE TXN_ID      = :DCL-TXN-ID
                  AND POST_DATE   = DATE(:DCL-POST-DATE)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'TRANSACTION ROW NOT FOUND FOR UPDATE'
                                       TO M13BMSGO
               WHEN OTHER
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      ******************************************************************
      * 6400 - BUILD THE LEG TABLE FOR THE CREDIT TRANSACTION          *
      *                                                                *
      * TWO LEGS WHERE THE NETWORK RULE ALSO REVERSES THE FEE, ONE     *
      * OTHERWISE.  THE TRAILER MOVES WITH THE OCCURRENCE COUNT SO IT  *
      * IS SET AFTER TXN-LEG-CNT.                                      *
      ******************************************************************
       6400-BUILD-CREDIT-LEGS.
           MOVE SPACES                 TO TXN-RECORD
           MOVE 1                      TO TXN-LEG-CNT
      *
           IF RSN-ACTION-CD = 'FULL'
               MOVE 2                  TO TXN-LEG-CNT
           END-IF
      *
           MOVE 1                      TO TXN-LEG-SEQ(1)
           MOVE 'PRIN'                 TO TXN-LEG-TYPE(1)
           COMPUTE TXN-LEG-AMT(1) = 0 - DCL-PROV-AMT
           MOVE '2101000100'           TO TXN-LEG-GL-ACCT(1)
           MOVE 'N'                    TO TXN-LEG-REVERSED(1)
      *
           IF TXN-LEG-CNT = 2
               MOVE 2                  TO TXN-LEG-SEQ(2)
               MOVE 'FEE '             TO TXN-LEG-TYPE(2)
               MOVE ZERO               TO TXN-LEG-AMT(2)
               MOVE '4404000100'       TO TXN-LEG-GL-ACCT(2)
               MOVE 'N'                TO TXN-LEG-REVERSED(2)
           END-IF
      *
           MOVE CA-OPER-ID             TO TXN-POSTED-BY
           MOVE SPACES                 TO TXN-POSTED-TS
           MOVE SPACES                 TO TXN-CYCLE-ID
           MOVE 'N'                    TO TXN-GL-POSTED-FLG
           MOVE 'Y'                    TO TXN-DISPUTE-FLG
      *
      *    ONLY THE LEG SLICE GOES TO TXN_LEG_DATA
           COMPUTE WS-LEG-LEN = TXN-LEG-CNT * WS-LEG-SIZE
           MOVE SPACES                 TO DCL-CR-LEG-TEXT
           MOVE TXN-RECORD(WS-LEG-OFFSET:WS-LEG-LEN)
                                       TO DCL-CR-LEG-TEXT(1:WS-LEG-LEN)
           MOVE WS-LEG-LEN             TO DCL-CR-LEG-LEN
           MOVE TXN-LEG-CNT            TO DCL-CR-LEG-CNT
           .
      *
       6500-INSERT-CREDIT.
      *    THE CREDIT CARRIES THE DISPUTE ID AS ITS TRANSACTION ID SO
      *    THE CHARGEBACK CLERK CAN TIE THE TWO TOGETHER.
           MOVE SPACES                 TO DCL-CR-TXN-ID
           MOVE 'PC'                   TO DCL-CR-TXN-ID(1:2)
           MOVE CA-DISPUTE-ID          TO DCL-CR-TXN-ID(3:12)
           COMPUTE DCL-CR-AMT = 0 - DCL-PROV-AMT
           MOVE CA-SEL-CARD-NUM        TO DCL-CARD-NUM
           MOVE CA-ACCT-ID             TO DCL-ACCT-ID
      *
           EXEC SQL
               INSERT INTO CARDSVC.TRANSACTION
                     (TXN_ID
                    , POST_DATE
                    , ACCT_ID
                    , CARD_NUM
                    , TXN_TYPE_CD
                    , TXN_SOURCE
                    , TXN_AMT
                    , CURRENCY_CD
                    , BILLING_AMT
                    , FX_RATE
                    , TXN_DESC
                    , TXN_LEG_CNT
                    , TXN_LEG_DATA
                    , GL_POSTED_FLG
                    , DISPUTE_FLG
                    , POSTED_BY
                    , POSTED_TS)
               VALUES (:DCL-CR-TXN-ID
                    , CURRENT DATE
                    , :DCL-ACCT-ID
                    , :DCL-CARD-NUM
                    , 'PCRD'
                    , 'ON'
                    , :DCL-CR-AMT
                    , :DCL-CURRENCY-CD
                    , :DCL-CR-AMT
                    , 1
                    , :DCL-NARRATIVE
                    , :DCL-CR-LEG-CNT
                    , :DCL-CR-LEG-DATA
                    , 'N'
                    , 'Y'
                    , :CA-OPER-ID
                    , CURRENT TIMESTAMP)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'PROVISIONAL CREDIT ALREADY POSTED TODAY'
                                       TO M13BMSGO
               WHEN OTHER
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       6600-UPDATE-BALANCE.
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET CURR_BAL       = CURR_BAL - :DCL-PROV-AMT
                    , LAST_MAINT_PGM = 'CACRD13 '
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE ACCT_ID        = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'ACCOUNT ROW NOT FOUND FOR BALANCE UPDATE'
                                       TO M13BMSGO
               WHEN OTHER
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   MOVE 'ACCOUNT           '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      ******************************************************************
      * 7100 - SIX DIGIT DATE WINDOWING                                *
      ******************************************************************
       7100-WINDOW-SIX-DIGIT.
           IF WS-SIX-YY < WS-CENTURY-PIVOT
               MOVE WS-CENTURY-20      TO WS-FULL-CC
           ELSE
               MOVE WS-CENTURY-19      TO WS-FULL-CC
           END-IF
           MOVE WS-SIX-YY              TO WS-FULL-YY
           MOVE WS-SIX-MM              TO WS-FULL-MM
           MOVE WS-SIX-DD              TO WS-FULL-DD
           .
      *
      ******************************************************************
      * 8000 - COMMON ROUTINES                                         *
      ******************************************************************
       8100-GET-DATE.
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-YYYYMMDD)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE WS-TODAY-YYYYMMDD      TO WS-DISPLAY-DATE(1:8)
           .
      *
       8200-SEND-LIST.
           MOVE WS-DISPLAY-DATE        TO M13DATEO
           MOVE CA-ACCT-ID             TO M13ACCTO
           MOVE CA-PAGE-NBR            TO M13PAGEO
           PERFORM 8250-FILL-ROWS
      *
           EXEC CICS SEND
                     MAP(WS-MAP-LIST)
                     MAPSET(WS-MAPSET)
                     FROM(CRD13AO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN
                     TRANSID(WS-TRAN-CARD)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
       8250-FILL-ROWS.
           MOVE WS-PR-TXN-ID(1)        TO M13TID1O
           MOVE WS-PR-POST-DATE(1)     TO M13DAT1O
           MOVE WS-PR-AMT(1)           TO WS-EDIT-LINE
           MOVE WS-EDIT-LINE           TO M13AMT1O
           MOVE WS-PR-MERCH-NAME(1)    TO M13MER1O
           MOVE WS-PR-MCC(1)           TO M13MCC1O
           MOVE WS-PR-DISP-FLG(1)      TO M13DSP1O
      *
           MOVE WS-PR-TXN-ID(2)        TO M13TID2O
           MOVE WS-PR-POST-DATE(2)     TO M13DAT2O
           MOVE WS-PR-AMT(2)           TO WS-EDIT-LINE
           MOVE WS-EDIT-LINE           TO M13AMT2O
           MOVE WS-PR-MERCH-NAME(2)    TO M13MER2O
           MOVE WS-PR-MCC(2)           TO M13MCC2O
           MOVE WS-PR-DISP-FLG(2)      TO M13DSP2O
      *
           MOVE WS-PR-TXN-ID(3)        TO M13TID3O
           MOVE WS-PR-POST-DATE(3)     TO M13DAT3O
           MOVE WS-PR-AMT(3)           TO WS-EDIT-LINE
           MOVE WS-EDIT-LINE           TO M13AMT3O
           MOVE WS-PR-MERCH-NAME(3)    TO M13MER3O
           MOVE WS-PR-MCC(3)           TO M13MCC3O
           MOVE WS-PR-DISP-FLG(3)      TO M13DSP3O
      *
           MOVE WS-PR-TXN-ID(4)        TO M13TID4O
           MOVE WS-PR-POST-DATE(4)     TO M13DAT4O
           MOVE WS-PR-AMT(4)           TO WS-EDIT-LINE
           MOVE WS-EDIT-LINE           TO M13AMT4O
           MOVE WS-PR-MERCH-NAME(4)    TO M13MER4O
           MOVE WS-PR-MCC(4)           TO M13MCC4O
           MOVE WS-PR-DISP-FLG(4)      TO M13DSP4O
      *
           MOVE WS-PR-TXN-ID(5)        TO M13TID5O
           MOVE WS-PR-POST-DATE(5)     TO M13DAT5O
           MOVE WS-PR-AMT(5)           TO WS-EDIT-LINE
           MOVE WS-EDIT-LINE           TO M13AMT5O
           MOVE WS-PR-MERCH-NAME(5)    TO M13MER5O
           MOVE WS-PR-MCC(5)           TO M13MCC5O
           MOVE WS-PR-DISP-FLG(5)      TO M13DSP5O
      *
           MOVE WS-PR-TXN-ID(6)        TO M13TID6O
           MOVE WS-PR-POST-DATE(6)     TO M13DAT6O
           MOVE WS-PR-AMT(6)           TO WS-EDIT-LINE
           MOVE WS-EDIT-LINE           TO M13AMT6O
           MOVE WS-PR-MERCH-NAME(6)    TO M13MER6O
           MOVE WS-PR-MCC(6)           TO M13MCC6O
           MOVE WS-PR-DISP-FLG(6)      TO M13DSP6O
      *
           MOVE WS-PR-TXN-ID(7)        TO M13TID7O
           MOVE WS-PR-POST-DATE(7)     TO M13DAT7O
           MOVE WS-PR-AMT(7)           TO WS-EDIT-LINE
           MOVE WS-EDIT-LINE           TO M13AMT7O
           MOVE WS-PR-MERCH-NAME(7)    TO M13MER7O
           MOVE WS-PR-MCC(7)           TO M13MCC7O
           MOVE WS-PR-DISP-FLG(7)      TO M13DSP7O
      *
           MOVE WS-PR-TXN-ID(8)        TO M13TID8O
           MOVE WS-PR-POST-DATE(8)     TO M13DAT8O
           MOVE WS-PR-AMT(8)           TO WS-EDIT-LINE
           MOVE WS-EDIT-LINE           TO M13AMT8O
           MOVE WS-PR-MERCH-NAME(8)    TO M13MER8O
           MOVE WS-PR-MCC(8)           TO M13MCC8O
           MOVE WS-PR-DISP-FLG(8)      TO M13DSP8O
           .
      *
       8300-SEND-DETAIL.
           MOVE WS-DISPLAY-DATE        TO M13BDTEO
      *
           EXEC CICS SEND
                     MAP(WS-MAP-DTL)
                     MAPSET(WS-MAPSET)
                     FROM(CRD13BO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN
                     TRANSID(WS-TRAN-CARD)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
       8500-CONVERT-AMOUNT.
           MOVE ZERO                   TO WS-NUMERIC-IN
           MOVE 'N'                    TO WS-ERROR-SW
           INSPECT WS-INPUT-AMT REPLACING ALL ',' BY ' '
      *
           COMPUTE WS-NUMERIC-IN = FUNCTION NUMVAL (WS-INPUT-AMT)
      *
           IF WS-NUMERIC-IN NOT > ZERO
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           .
      *
      ******************************************************************
      * 9000 - ERROR HANDLING                                          *
      ******************************************************************
       9000-REPORT-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'E'                    TO ER-SEVERITY
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE EIBTRMID               TO ER-TERM-ID
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM-ONLINE)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
       9100-SQL-ERROR.
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE 'SQL FAILURE IN DISPUTE ENTRY'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
       9600-EXIT-SESSION.
           EXEC CICS SEND TEXT
                     FROM(WS-MSG-RAISED)
                     LENGTH(78)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           .
