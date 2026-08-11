      ******************************************************************
      * CACRD15 - CUSTOMER / KYC INQUIRY                               *
      *                                                                *
      * MENU OPTION 9.  READ ONLY.  SHOWS THE CARD SIDE VIEW OF THE    *
      * CUSTOMER TOGETHER WITH THE RISK AND KYC OPINION OBTAINED       *
      * THROUGH THE DISPATCHER ON ROUTE XMOD / KYCINQ.                 *
      *                                                                *
      * CALLED BY  - CACRD90 BY XCTL, ROUTE MENU / OPT09               *
      * RETURNS TO - CACRD00 BY XCTL                                   *
      * CALLS      - CACRD90 BY LINK  ROUTE XMOD / KYCINQ              *
      *              CACRD91 BY LINK  ERROR DISPLAY                    *
      * MAPSET     - CARDST2, MAP CRD15A                               *
      * FILES      - CUSTPREF  KSDS  READ                              *
      * TABLES     - CARDSVC.ACCOUNT     SELECT                        *
      *              CARDSVC.CARD        SELECT                        *
      *              CARDSVC.CARD_LIMIT  SELECT                        *
      *                                                                *
      * NO UPDATES ARE MADE BY THIS PROGRAM.  IT MUST NOT ISSUE A      *
      * SYNCPOINT - THE OPERATOR MAY BE MID WAY THROUGH A MAINTENANCE  *
      * CONVERSATION ON ANOTHER SCREEN.                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD15.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD15 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDST2 '.
       01  WS-MAP-INQ                  PIC X(8)  VALUE 'CRD15A  '.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CACRD00 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE 0.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-SUPPRESS-SW          PIC X     VALUE 'N'.
               88  WS-SUPPRESS-DETAIL            VALUE 'Y'.
           05  WS-PREF-SW              PIC X     VALUE 'N'.
               88  WS-PREF-FOUND                 VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-IDX                  PIC S9(4) COMP VALUE 0.
           05  WS-EDIT-AMT             PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EDIT-SCORE           PIC ZZ9.
           05  WS-NAME-WORK            PIC X(40) VALUE SPACES.
           05  WS-STATE-ZIP            PIC X(13) VALUE SPACES.
           05  WS-EMBOSS-WORK          PIC X(26) VALUE SPACES.
           05  WS-EMBOSS-1             PIC X(26) VALUE SPACES.
           05  WS-EMBOSS-2             PIC X(26) VALUE SPACES.
      *
       01  WS-DATE-AREAS.
           05  WS-TODAY-YYYYMMDD       PIC 9(8)  VALUE ZERO.
           05  WS-DISPLAY-DATE         PIC X(10) VALUE SPACES.
           05  WS-TIME-HHMMSS          PIC 9(6)  VALUE ZERO.
           05  WS-SIX-DATE             PIC 9(6)  VALUE ZERO.
           05  WS-SIX-R REDEFINES WS-SIX-DATE.
               10  WS-SIX-YY           PIC 9(2).
               10  WS-SIX-MM           PIC 9(2).
               10  WS-SIX-DD           PIC 9(2).
           05  WS-FULL-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-FULL-R REDEFINES WS-FULL-DATE.
               10  WS-FULL-CC          PIC 9(2).
               10  WS-FULL-YY          PIC 9(2).
               10  WS-FULL-MM          PIC 9(2).
               10  WS-FULL-DD          PIC 9(2).
           05  WS-EDIT-DATE            PIC X(10) VALUE SPACES.
           05  WS-EDIT-R REDEFINES WS-EDIT-DATE.
               10  WS-ED-YYYY          PIC X(4).
               10  WS-ED-SEP1          PIC X.
               10  WS-ED-MM            PIC X(2).
               10  WS-ED-SEP2          PIC X.
               10  WS-ED-DD            PIC X(2).
      *
       01  WS-MESSAGES.
           05  WS-MSG-ENTER            PIC X(78) VALUE
               'KEY A CUSTOMER NUMBER OR A CARD NUMBER AND PRESS ENTER'.
           05  WS-MSG-KEY-REQD         PIC X(78) VALUE
               'CUSTOMER NUMBER OR CARD NUMBER IS REQUIRED'.
           05  WS-MSG-CUST-BAD         PIC X(78) VALUE
               'CUSTOMER NUMBER MUST BE NUMERIC'.
           05  WS-MSG-NO-CUST          PIC X(78) VALUE
               'NO CARD ACCOUNT FOUND FOR THAT CUSTOMER'.
           05  WS-MSG-NO-CARD          PIC X(78) VALUE
               'CARD NUMBER NOT ON FILE'.
           05  WS-MSG-OK               PIC X(78) VALUE
               'INQUIRY COMPLETE'.
           05  WS-MSG-ROUTE-BAD        PIC X(78) VALUE
               'RISK ENQUIRY COULD NOT BE ROUTED - TRY AGAIN LATER'.
           05  WS-MSG-DECLINED         PIC X(78) VALUE
               'CUSTOMER DETAIL SUPPRESSED - REFER TO FINANCIAL CRIME'.
           05  WS-MSG-PREF-MISSING     PIC X(78) VALUE
               'NO SERVICING PREFERENCES HELD - ADDRESS FROM CARD ONLY'.
      *
       01  WS-KYC-TEXT                 PIC X(14) VALUE SPACES.
      *
      ******************************************************************
      * PROGRAM COMMAREA                                               *
      ******************************************************************
       01  WS-COMMAREA.
           05  CA-PGM-ID               PIC X(8).
           05  CA-FROM-PGM             PIC X(8).
           05  CA-STEP                 PIC X.
           05  CA-CUST-ID              PIC 9(9).
           05  CA-PARTY-ID             PIC X(11).
           05  CA-ACCT-ID              PIC 9(11).
           05  CA-CARD-NUM             PIC X(16).
           05  CA-LAST-RC              PIC 9(4).
           05  CA-OPER-ID              PIC X(8).
           05  CA-FILLER               PIC X(435).
      *
           COPY CVRISK01Y.
           COPY CVROUT01Y.
           COPY CVCUST01Y.
           COPY CVCPRF1Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      ******************************************************************
      * THE DISPATCHER COMMAREA - THE 38 BYTE ROUTE REQUEST FOLLOWED   *
      * BY THE CALLER COMMAREA.  ON A CROSS MODULE CALL THAT COMMAREA  *
      * IS THE RISK AREA ITSELF.                                       *
      ******************************************************************
       01  WS-DISPATCH-AREA.
           05  WS-DISP-REQUEST         PIC X(38).
           05  WS-DISP-COMMAREA        PIC X(512).
      *
      *    BMS SYMBOLIC MAPS FOR MAPSET CARDST2
           COPY CARDST2.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-KEYS.
           05  DCL-CUST-ID             PIC S9(9)  COMP-3.
           05  DCL-ACCT-ID             PIC S9(11) COMP-3.
           05  DCL-CARD-NUM            PIC X(16).
           05  DCL-PARTY-ID            PIC X(11).
      *
       01  DCL-CARD-DATA.
           05  DCL-EMBOSSED-NAME       PIC X(26).
           05  DCL-CARD-STATUS         PIC X(1).
           05  DCL-PRODUCT-CD          PIC X(4).
           05  DCL-EXPIRY-YYMM         PIC X(4).
      *
       01  DCL-ACCT-DATA.
           05  DCL-ACCT-STATUS         PIC X(1).
           05  DCL-CURR-BAL            PIC S9(13)V99 COMP-3.
           05  DCL-OPEN-DATE           PIC X(10).
           05  DCL-BRANCH-CD           PIC X(5).
      *
       01  DCL-LIMIT-DATA.
           05  DCL-LIMIT-AMT           PIC S9(13)V99 COMP-3.
           05  DCL-AVAIL-AMT           PIC S9(13)V99 COMP-3.
      *
       01  IND-BRANCH                  PIC S9(4) COMP.
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
           MOVE LOW-VALUES             TO CRD15AO
      *
           IF EIBCALEN = ZERO
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE 'CACRD15 ENTERED WITH NO COMMAREA'
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
           PERFORM 2000-INQUIRY-PASS
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
           MOVE ZERO                   TO CA-CUST-ID
           MOVE ZERO                   TO CA-ACCT-ID
           MOVE ZERO                   TO CA-LAST-RC
           MOVE WS-MSG-ENTER           TO M15MSGO
           PERFORM 8200-SEND-MAP
           .
      *
      ******************************************************************
      * 2000 - ONE INQUIRY                                             *
      ******************************************************************
       2000-INQUIRY-PASS.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-SUPPRESS-SW
           MOVE 'N'                    TO WS-PREF-SW
           MOVE SPACES                 TO CUST-RECORD
           MOVE ZERO                   TO CUST-ID
           MOVE ZERO                   TO CUST-DOB
           MOVE ZERO                   TO CUST-OPEN-DATE
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-INQ)
                     MAPSET(WS-MAPSET)
                     INTO(CRD15AI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE WS-MSG-KEY-REQD
                                       TO M15MSGO
                   PERFORM 8200-SEND-MAP
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE 'RECEIVE MAP CRD15A FAILED'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           PERFORM 2100-EDIT-INPUT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2200-RESOLVE-CUSTOMER
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2400-READ-PREFERENCES
           PERFORM 2500-BUILD-LOCAL-VIEW
           PERFORM 3000-GET-RISK-OPINION
      *
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 4000-EVALUATE-REPLY
           PERFORM 8200-SEND-MAP
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-EDIT-INPUT.
           MOVE ZERO                   TO CA-CUST-ID
           MOVE SPACES                 TO CA-CARD-NUM
      *
           IF M15CUSTL > ZERO AND M15CUSTI NOT = SPACES
               IF M15CUSTI IS NOT NUMERIC
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-CUST-BAD
                                       TO M15MSGO
                   GO TO 2100-EXIT
               END-IF
               MOVE M15CUSTI           TO CA-CUST-ID
           END-IF
      *
           IF M15CARDL > ZERO AND M15CARDI NOT = SPACES
               MOVE M15CARDI           TO CA-CARD-NUM
           END-IF
      *
           IF CA-CUST-ID = ZERO AND CA-CARD-NUM = SPACES
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-KEY-REQD    TO M15MSGO
           END-IF
           .
       2100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2200 - THE CARD SIDE HAS NO CUSTOMER MASTER OF ITS OWN.  THE   *
      * IDENTIFIERS COME OFF THE ACCOUNT AND THE NAME OFF THE PLASTIC. *
      ******************************************************************
       2200-RESOLVE-CUSTOMER.
           IF CA-CARD-NUM NOT = SPACES
               PERFORM 2250-BY-CARD
           ELSE
               PERFORM 2300-BY-CUSTOMER
           END-IF
           .
      *
       2250-BY-CARD.
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT C.CUST_ID
                    , C.ACCT_ID
                    , C.EMBOSSED_NAME
                    , C.CARD_STATUS
                    , C.PRODUCT_CD
                    , C.EXPIRY_YYMM
                    , A.PARTY_ID
                    , A.ACCT_STATUS
                    , A.CURR_BAL
                    , CHAR(A.OPEN_DATE, ISO)
                    , A.BRANCH_CD
                 INTO :DCL-CUST-ID
                    , :DCL-ACCT-ID
                    , :DCL-EMBOSSED-NAME
                    , :DCL-CARD-STATUS
                    , :DCL-PRODUCT-CD
                    , :DCL-EXPIRY-YYMM
                    , :DCL-PARTY-ID
                    , :DCL-ACCT-STATUS
                    , :DCL-CURR-BAL
                    , :DCL-OPEN-DATE
                    , :DCL-BRANCH-CD :IND-BRANCH
                 FROM CARDSVC.CARD    C
                    , CARDSVC.ACCOUNT A
                WHERE C.CARD_NUM = :DCL-CARD-NUM
                  AND C.ACCT_ID  = A.ACCT_ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-CUST-ID    TO CA-CUST-ID
                   MOVE DCL-ACCT-ID    TO CA-ACCT-ID
                   MOVE DCL-PARTY-ID   TO CA-PARTY-ID
                   PERFORM 2350-READ-LIMIT
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NO-CARD TO M15MSGO
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD              '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      *    NO CARD KEYED - TAKE THE OLDEST OPEN ACCOUNT AND THE CARD
      *    THE STATEMENTS GO TO.
       2300-BY-CUSTOMER.
           MOVE CA-CUST-ID             TO DCL-CUST-ID
      *
           EXEC SQL
               SELECT C.CARD_NUM
                    , C.ACCT_ID
                    , C.EMBOSSED_NAME
                    , C.CARD_STATUS
                    , C.PRODUCT_CD
                    , C.EXPIRY_YYMM
                    , A.PARTY_ID
                    , A.ACCT_STATUS
                    , A.CURR_BAL
                    , CHAR(A.OPEN_DATE, ISO)
                    , A.BRANCH_CD
                 INTO :DCL-CARD-NUM
                    , :DCL-ACCT-ID
                    , :DCL-EMBOSSED-NAME
                    , :DCL-CARD-STATUS
                    , :DCL-PRODUCT-CD
                    , :DCL-EXPIRY-YYMM
                    , :DCL-PARTY-ID
                    , :DCL-ACCT-STATUS
                    , :DCL-CURR-BAL
                    , :DCL-OPEN-DATE
                    , :DCL-BRANCH-CD :IND-BRANCH
                 FROM CARDSVC.CARD    C
                    , CARDSVC.ACCOUNT A
                WHERE C.CUST_ID = :DCL-CUST-ID
                  AND C.ACCT_ID = A.ACCT_ID
                ORDER BY A.OPEN_DATE ASC
                       , C.ISSUE_DATE ASC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-CARD-NUM   TO CA-CARD-NUM
                   MOVE DCL-ACCT-ID    TO CA-ACCT-ID
                   MOVE DCL-PARTY-ID   TO CA-PARTY-ID
                   PERFORM 2350-READ-LIMIT
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NO-CUST TO M15MSGO
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD              '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       2350-READ-LIMIT.
           MOVE ZERO                   TO DCL-LIMIT-AMT
           MOVE ZERO                   TO DCL-AVAIL-AMT
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT LIMIT_AMT
                    , AVAIL_AMT
                 INTO :DCL-LIMIT-AMT
                    , :DCL-AVAIL-AMT
                 FROM CARDSVC.CARD_LIMIT
                WHERE CARD_NUM   = :DCL-CARD-NUM
                  AND LIMIT_TYPE = 'CRED'
                  AND EXP_DATE   = '9999-12-31'
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD_LIMIT        '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2400 - SERVICING PREFERENCES                                   *
      ******************************************************************
       2400-READ-PREFERENCES.
           MOVE SPACES                 TO CUST-PREF-RECORD
           MOVE CA-CUST-ID             TO CPRF-CUST-ID
      *
           EXEC CICS READ
                     FILE('CUSTPREF')
                     INTO(CUST-PREF-RECORD)
                     RIDFLD(CPRF-CUST-ID)
                     KEYLENGTH(9)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 'Y'            TO WS-PREF-SW
               WHEN DFHRESP(NOTFND)
                   MOVE WS-MSG-PREF-MISSING
                                       TO M15WARNO
               WHEN OTHER
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE 'CUSTPREF'     TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE CA-CUST-ID     TO ER-VSAM-KEY
                   MOVE 'READ FAILED ON CUSTOMER PREFERENCE FILE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2500 - BUILD THE LOCAL CUSTOMER VIEW                           *
      *                                                                *
      * THE NAME COMES OFF THE PLASTIC AND THE ADDRESS OFF THE         *
      * PREFERENCE FILE.  THE EMBOSSING LINE IS 26 BYTES OF INITIAL    *
      * AND SURNAME SO THE FIRST NAME AND THE MIDDLE INITIAL CANNOT    *
      * ALWAYS BE RECOVERED.  DATE OF BIRTH AND THE NATIONAL ID ARE    *
      * NOT HELD ON THIS SIDE AT ALL.                                  *
      ******************************************************************
       2500-BUILD-LOCAL-VIEW.
           MOVE CA-CUST-ID             TO CUST-ID
           MOVE CA-PARTY-ID            TO CUST-PARTY-ID
      *
           MOVE DCL-EMBOSSED-NAME      TO WS-EMBOSS-WORK
           MOVE SPACES                 TO WS-EMBOSS-1
           MOVE SPACES                 TO WS-EMBOSS-2
      *
           UNSTRING WS-EMBOSS-WORK
               DELIMITED BY ALL SPACES
               INTO WS-EMBOSS-1
                  , WS-EMBOSS-2
           END-UNSTRING
      *
           IF WS-EMBOSS-2 = SPACES
      *        ONE WORD ONLY - TREAT IT AS THE SURNAME
               MOVE WS-EMBOSS-1        TO CUST-LAST-NAME
               MOVE SPACES             TO CUST-FIRST-NAME
           ELSE
               MOVE WS-EMBOSS-1        TO CUST-FIRST-NAME
               MOVE WS-EMBOSS-2        TO CUST-LAST-NAME
           END-IF
           MOVE SPACES                 TO CUST-TITLE
           MOVE SPACES                 TO CUST-MIDDLE-INIT
      *
           IF WS-PREF-FOUND
               MOVE CPRF-MAIL-ADDR1    TO CUST-ADDR-LINE1
               MOVE CPRF-MAIL-CITY     TO CUST-CITY
               MOVE CPRF-MAIL-ZIP      TO CUST-ZIP
               MOVE CPRF-DAY-PHONE     TO CUST-PHONE-HOME
               IF CPRF-PREF-NAME NOT = SPACES
                   MOVE CPRF-PREF-NAME TO WS-NAME-WORK
               END-IF
           END-IF
      *
      *    THE PREFERENCE FILE HOLDS ONE ADDRESS LINE, NO STATE AND
      *    NO COUNTRY.  THEY STAY BLANK ON THE SCREEN.
           MOVE SPACES                 TO CUST-ADDR-LINE2
           MOVE SPACES                 TO CUST-STATE
           MOVE SPACES                 TO CUST-COUNTRY
           MOVE SPACES                 TO CUST-NATIONAL-ID
           MOVE SPACES                 TO CUST-EMAIL
           MOVE DCL-PRODUCT-CD         TO CUST-SEGMENT-CD
           MOVE DCL-ACCT-STATUS        TO CUST-STATUS
      *
           IF WS-NAME-WORK = SPACES
               STRING CUST-FIRST-NAME  DELIMITED BY '  '
                      ' '              DELIMITED BY SIZE
                      CUST-LAST-NAME   DELIMITED BY '  '
                 INTO WS-NAME-WORK
               END-STRING
           END-IF
           .
      *
      ******************************************************************
      * 3000 - CROSS MODULE RISK AND KYC ENQUIRY                       *
      *                                                                *
      * THE TARGET PROGRAM IS NOT NAMED HERE.  THE DISPATCHER RESOLVES *
      * ROUTE XMOD / KYCINQ FROM THE ROUTE TABLE AND ISSUES THE LINK.  *
      ******************************************************************
       3000-GET-RISK-OPINION.
           MOVE SPACES                 TO ROUTE-REQUEST
           MOVE SPACES                 TO CV-RISK-AREA
      *
           MOVE 0003                   TO CV-RISK-VERSION
           MOVE WS-PGM-ID              TO CV-RISK-CALLER-ID
           MOVE WS-MODULE-CARDSVC      TO CV-RISK-CALLER-MOD
           MOVE SPACES                 TO CV-RISK-CORREL-ID
           MOVE EIBTRNID               TO CV-RISK-CORREL-ID(1:4)
           MOVE EIBTASKN               TO CV-RISK-CORREL-ID(5:7)
           MOVE EIBTRMID               TO CV-RISK-CORREL-ID(12:4)
           MOVE WS-TODAY-YYYYMMDD      TO CV-RISK-REQ-DATE
           MOVE WS-TIME-HHMMSS         TO CV-RISK-REQ-TIME
           MOVE 'O'                    TO CV-RISK-CHANNEL
      *
           MOVE CA-PARTY-ID            TO CV-RISK-PARTY-ID
           MOVE CA-CUST-ID             TO CV-RISK-CUST-ID
           MOVE CA-ACCT-ID             TO CV-RISK-ACCT-ID
           MOVE CA-CARD-NUM            TO CV-RISK-CARD-NUM
           MOVE ZERO                   TO CV-RISK-REQ-AMT
           MOVE WS-CURRENCY-USD        TO CV-RISK-REQ-CURR
           MOVE WS-COUNTRY-USA         TO CV-RISK-COUNTRY
           MOVE 'INQY'                 TO CV-RISK-REQ-TYPE
      *
           MOVE ZERO                   TO CV-RISK-RC
           MOVE ZERO                   TO CV-RISK-HOP-CNT
           ADD 1                       TO CV-RISK-HOP-CNT
           MOVE WS-PGM-ID              TO
                                CV-RISK-HOP-PGM(CV-RISK-HOP-CNT)
           MOVE CV-RISK-RC             TO
                                CV-RISK-HOP-RC(CV-RISK-HOP-CNT)
      *
           MOVE 'XMOD'                 TO RQ-ROUTE-TYPE
           MOVE WS-ROUTE-KYCINQ        TO RQ-ROUTE-KEY
           MOVE 1                      TO RQ-SEQ-NBR
           MOVE SPACES                 TO RQ-RESOLVED-PGM
           MOVE SPACES                 TO RQ-RESOLVED-MOD
           MOVE 'N'                    TO RQ-USED-FALLBACK
           MOVE ZERO                   TO RQ-RC
      *
           MOVE ROUTE-REQUEST          TO WS-DISP-REQUEST
           MOVE CV-RISK-AREA           TO WS-DISP-COMMAREA
      *
           EXEC CICS LINK
                     PROGRAM(WS-DISPATCHER-ONLINE)
                     COMMAREA(WS-DISPATCH-AREA)
                     LENGTH(LENGTH OF WS-DISPATCH-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           MOVE WS-DISP-REQUEST        TO ROUTE-REQUEST
           MOVE WS-DISP-COMMAREA       TO CV-RISK-AREA
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE WS-RESP2           TO ER-EIBRESP2
               MOVE 'LINK TO ONLINE DISPATCHER FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ROUTE-BAD   TO M15MSGO
               GO TO 3000-EXIT
           END-IF
      *
           IF RQ-RC NOT = ZERO
               MOVE 'ROUT'             TO ER-ERROR-TYPE
               MOVE RQ-ROUTE-KEY       TO ER-REASON-CD
               MOVE 'ROUTE XMOD KYCINQ COULD NOT BE RESOLVED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ROUTE-BAD   TO M15MSGO
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - WHAT CAME BACK                                          *
      ******************************************************************
       4000-EVALUATE-REPLY.
           MOVE CV-RISK-RC             TO CA-LAST-RC
      *
           EVALUATE CV-RISK-RC
               WHEN 0000
                   MOVE WS-MSG-OK      TO M15MSGO
               WHEN 0004
                   MOVE WS-MSG-OK      TO M15MSGO
                   MOVE CV-RISK-REASON-TXT
                                       TO M15WARNO
               WHEN 0008
                   MOVE 'Y'            TO WS-SUPPRESS-SW
                   MOVE WS-MSG-DECLINED
                                       TO M15MSGO
                   MOVE CV-RISK-REASON-TXT
                                       TO M15WARNO
               WHEN 0012
                   MOVE 'SQL '         TO ER-ERROR-TYPE
                   MOVE CV-RISK-SQLCODE
                                       TO ER-SQLCODE
                   MOVE CV-RISK-FAIL-PGM
                                       TO ER-REASON-CD
                   MOVE 'F'            TO ER-SEVERITY
                   MOVE CV-RISK-REASON-TXT
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   MOVE 'Y'            TO WS-SUPPRESS-SW
                   MOVE 'RISK SERVICE FAILED - SEE THE ERROR DISPLAY'
                                       TO M15MSGO
               WHEN OTHER
                   MOVE 'UNEXPECTED RETURN CODE FROM RISK SERVICE'
                                       TO M15MSGO
                   MOVE 'Y'            TO WS-SUPPRESS-SW
           END-EVALUATE
      *
           PERFORM 4100-PAINT-LOCAL
      *
           IF WS-SUPPRESS-DETAIL
               PERFORM 4300-SUPPRESS-DETAIL
           ELSE
               PERFORM 4200-PAINT-RISK
           END-IF
           .
      *
       4100-PAINT-LOCAL.
           MOVE CUST-ID                TO M15CUSTO
           MOVE CA-CARD-NUM            TO M15CARDO
           MOVE WS-NAME-WORK           TO M15NAMEO
           MOVE CUST-PARTY-ID          TO M15PTYO
           MOVE CUST-ADDR-LINE1        TO M15AD1O
           MOVE CUST-ADDR-LINE2        TO M15AD2O
           MOVE CUST-CITY              TO M15CITYO
      *
           MOVE SPACES                 TO WS-STATE-ZIP
           STRING CUST-STATE           DELIMITED BY SIZE
                  ' '                  DELIMITED BY SIZE
                  CUST-ZIP             DELIMITED BY SIZE
             INTO WS-STATE-ZIP
           END-STRING
           MOVE WS-STATE-ZIP           TO M15STZPO
           MOVE CUST-COUNTRY           TO M15CTRYO
           MOVE CUST-SEGMENT-CD        TO M15SEGO
      *
      *    THE VIP MARKER IS A CUSTOMER MASTER FIELD.  ON THIS SIDE
      *    PREMIUM SERVICING IS THE NEAREST THING WE HOLD.
           IF WS-PREF-FOUND AND CPRF-SERVICE-PREMIUM
               MOVE 'Y'                TO CUST-VIP-FLG
           ELSE
               MOVE 'N'                TO CUST-VIP-FLG
           END-IF
           MOVE CUST-VIP-FLG           TO M15VIPO
      *
           IF WS-PREF-FOUND
               MOVE CPRF-SERVICE-LEVEL TO M15SVCLO
               MOVE CPRF-STMT-PREF     TO M15SPRFO
               MOVE CPRF-CONTACT-METHOD
                                       TO M15CMTHO
           ELSE
               MOVE '??'               TO M15SVCLO
               MOVE 'PAPR'             TO M15SPRFO
               MOVE 'M'                TO M15CMTHO
           END-IF
      *
      *    DATE OF BIRTH IS NOT CARRIED ON THE CARD SIDE
           MOVE 'NOT HELD'             TO M15DOBO
           .
      *
       4200-PAINT-RISK.
           MOVE CV-RISK-BAND           TO M15BANDO
           MOVE CV-RISK-SCORE          TO WS-EDIT-SCORE
           MOVE WS-EDIT-SCORE          TO M15SCORO
           MOVE CV-RISK-KYC-STATUS     TO M15KYCO
      *
           EVALUATE CV-RISK-KYC-STATUS
               WHEN 'OK'
                   MOVE 'VERIFIED'     TO WS-KYC-TEXT
               WHEN 'PN'
                   MOVE 'PENDING'      TO WS-KYC-TEXT
               WHEN 'EX'
                   MOVE 'EXPIRED'      TO WS-KYC-TEXT
               WHEN 'FL'
                   MOVE 'FAILED'       TO WS-KYC-TEXT
               WHEN OTHER
                   MOVE 'UNKNOWN'      TO WS-KYC-TEXT
           END-EVALUATE
           MOVE WS-KYC-TEXT            TO M15KYCTO
      *
           MOVE CV-RISK-SANCTION-FLG   TO M15SANCO
           MOVE CV-RISK-EXPOSURE-AMT   TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M15EXPOO
           MOVE CV-RISK-AVAIL-AMT      TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M15AVALO
           MOVE CV-RISK-MODEL-ID       TO M15MODLO
      *
           IF CV-RISK-SCORE-DATE > ZERO
               MOVE CV-RISK-SCORE-DATE(1:4)
                                       TO WS-ED-YYYY
               MOVE '-'                TO WS-ED-SEP1
               MOVE CV-RISK-SCORE-DATE(5:2)
                                       TO WS-ED-MM
               MOVE '-'                TO WS-ED-SEP2
               MOVE CV-RISK-SCORE-DATE(7:2)
                                       TO WS-ED-DD
               MOVE WS-EDIT-DATE       TO M15SCDTO
           ELSE
               MOVE 'NOT SCORED'       TO M15SCDTO
           END-IF
      *
      *    THE CARD SIDE AVAILABLE CREDIT IS SHOWN WHERE THE RISK
      *    SERVICE DID NOT RETURN AN EXPOSURE FIGURE
           IF CV-RISK-AVAIL-AMT = ZERO AND DCL-AVAIL-AMT NOT = ZERO
               MOVE DCL-AVAIL-AMT      TO WS-EDIT-AMT
               MOVE WS-EDIT-AMT        TO M15AVALO
           END-IF
           .
      *
       4300-SUPPRESS-DETAIL.
           MOVE SPACES                 TO M15BANDO
           MOVE SPACES                 TO M15SCORO
           MOVE CV-RISK-KYC-STATUS     TO M15KYCO
           MOVE 'REFER'                TO M15KYCTO
           MOVE CV-RISK-SANCTION-FLG   TO M15SANCO
           MOVE SPACES                 TO M15EXPOO
           MOVE SPACES                 TO M15AVALO
           MOVE SPACES                 TO M15MODLO
           MOVE SPACES                 TO M15SCDTO
           MOVE SPACES                 TO M15AD1O
           MOVE SPACES                 TO M15AD2O
           MOVE SPACES                 TO M15CITYO
           MOVE SPACES                 TO M15STZPO
           MOVE SPACES                 TO M15CTRYO
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
                     TIME(WS-TIME-HHMMSS)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE WS-TODAY-YYYYMMDD      TO WS-DISPLAY-DATE(1:8)
           .
      *
       8200-SEND-MAP.
           MOVE WS-DISPLAY-DATE        TO M15DATEO
      *
           EXEC CICS SEND
                     MAP(WS-MAP-INQ)
                     MAPSET(WS-MAPSET)
                     FROM(CRD15AO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SEND MAP CRD15A FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
           END-IF
      *
           EXEC CICS RETURN
                     TRANSID(WS-TRAN-CARD)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
      ******************************************************************
      * 9000 - ERROR HANDLING                                          *
      ******************************************************************
       9000-REPORT-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           IF ER-SEVERITY = SPACES OR LOW-VALUES
               MOVE 'E'                TO ER-SEVERITY
           END-IF
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
           MOVE 'SQL FAILURE IN CUSTOMER INQUIRY'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
       9600-EXIT-SESSION.
           EXEC CICS SEND TEXT
                     FROM(WS-MSG-OK)
                     LENGTH(78)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           .
