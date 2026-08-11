      ******************************************************************
      * CACRD02 - CARD LIST / CROSS REFERENCE                          *
      *                                                                *
      * MENU OPTION 2.  BROWSES THE CARDXREF ALTERNATE INDEX PATH      *
      * CARD.PROD.CARDXREF.PATH1 (FILE CARDXRP1), WHICH IS KEYED ON    *
      * ACCOUNT NUMBER AND CARRIES ONE RECORD PER PLASTIC.             *
      *                                                                *
      * TEN CARDS PER PAGE.  THE BROWSE POSITION IS HELD IN THE        *
      * COMMAREA (CAW-BROWSE) BECAUSE THE BROWSE IS CLOSED AT THE END  *
      * OF EVERY PASS - CICS DOES NOT HOLD A BROWSE ACROSS A PSEUDO    *
      * CONVERSATIONAL RETURN.                                         *
      *                                                                *
      * CALLED BY   - CACRD90 XCTL, ROUTE MENU/OPT02                   *
      * CALLS       - CACRD00 ON PF3, CACRD90 ON PF12                  *
      *             - CACRD91  ERROR HANDLER                           *
      * FILES       - CARDXRP1  PATH OVER CARD.PROD.CARDXREF.AIX1      *
      * MAPSET      - CARDSET   MAP CARDLST                            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD02.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD02 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDSET '.
       01  WS-MAP                      PIC X(8)  VALUE 'CARDLST '.
       01  WS-XREF-PATH                PIC X(8)  VALUE 'CARDXRP1'.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-FILE-STATUS              PIC X(2)  VALUE '00'.
      *
       01  WS-SUB                      PIC S9(4) COMP VALUE 0.
       01  WS-LINE-CNT                 PIC S9(4) COMP VALUE 0.
       01  WS-HALF-CNT                 PIC S9(4) COMP VALUE 0.
       01  WS-BROWSE-OPEN-SW           PIC X     VALUE 'N'.
           88  WS-BROWSE-OPEN                    VALUE 'Y'.
       01  WS-END-SW                   PIC X     VALUE 'N'.
           88  WS-END-OF-BROWSE                  VALUE 'Y'.
       01  WS-ERASE-SW                 PIC X     VALUE 'Y'.
           88  WS-ERASE                          VALUE 'Y'.
       01  WS-DIRECTION                PIC X     VALUE 'F'.
           88  WS-FORWARD                        VALUE 'F'.
           88  WS-BACKWARD                       VALUE 'B'.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-OUT                 PIC X(8)  VALUE SPACES.
      *
       01  WS-ACCT-KEY                 PIC X(11) VALUE SPACES.
       01  WS-ACCT-NUM                 PIC 9(11) VALUE ZERO.
       01  WS-EXPIRY-EDIT              PIC X(6)  VALUE SPACES.
      *
      *    ---------------------------------------------------------
      *    CARDXREF RECORD - 128 BYTES.  PRIMARY KEY IS THE CARD
      *    NUMBER, THE AIX KEY IS THE ACCOUNT NUMBER AT OFFSET 16.
      *    ---------------------------------------------------------
       01  XREF-RECORD.
           05  XR-CARD-NUM             PIC X(16).
           05  XR-ACCT-ID              PIC X(11).
           05  XR-CUST-ID              PIC X(9).
           05  XR-CARD-STATUS          PIC X.
               88  XR-ACTIVE                     VALUE 'A'.
               88  XR-BLOCKED                    VALUE 'B'.
               88  XR-CLOSED                     VALUE 'C'.
               88  XR-LOST-STOLEN                VALUE 'L'.
               88  XR-EXPIRED                    VALUE 'E'.
               88  XR-NOT-ACTIVATED              VALUE 'N'.
           05  XR-PRODUCT-CD           PIC X(4).
           05  XR-EXPIRY-YYMM          PIC 9(4).
           05  XR-EMBOSSED-NAME        PIC X(26).
           05  XR-PRIMARY-FLG          PIC X.
           05  XR-ISSUE-DATE           PIC 9(8).
      *    LEGACY 6 DIGIT DATE - PIVOT WINDOWED WHEN DISPLAYED
           05  XR-LAST-USED-YYMMDD     PIC 9(6).
           05  XR-BRANCH-CD            PIC X(5).
           05  XR-FILLER               PIC X(37).
      *
       01  WS-XREF-LEN                 PIC S9(4) COMP VALUE 128.
      *
       01  WS-PAGE-TABLE.
           05  WS-PAGE-ENTRY OCCURS 10 TIMES.
               10  WS-PG-CARD          PIC X(16).
               10  WS-PG-STATUS        PIC X(2).
               10  WS-PG-PRODUCT       PIC X(4).
               10  WS-PG-EXPIRY        PIC X(6).
               10  WS-PG-NAME          PIC X(26).
      *
      *    SCRATCH LINE USED WHEN A BACKWARD PAGE IS FLIPPED
       01  WS-SWAP-ENTRY.
           05  WS-SW-CARD              PIC X(16).
           05  WS-SW-STATUS            PIC X(2).
           05  WS-SW-PRODUCT           PIC X(4).
           05  WS-SW-EXPIRY            PIC X(6).
           05  WS-SW-NAME              PIC X(26).
      *
       01  WS-OTHER-SUB                PIC S9(4) COMP VALUE 0.
      *
       01  WS-MSG-NO-CARDS             PIC X(60) VALUE
           'NO CARDS ON FILE FOR THIS ACCOUNT'.
       01  WS-MSG-KEY-REQD             PIC X(60) VALUE
           'ENTER AN ACCOUNT NUMBER AND PRESS ENTER'.
       01  WS-MSG-EOF                  PIC X(60) VALUE
           'LAST PAGE - NO MORE CARDS FOR THIS ACCOUNT'.
       01  WS-MSG-BOF                  PIC X(60) VALUE
           'FIRST PAGE'.
       01  WS-MSG-OK                   PIC X(60) VALUE
           'SELECT WITH S FOR CARD DETAIL, PF7 / PF8 TO PAGE'.
      *
       01  WS-DISPATCH-AREA.
           05  WS-DA-ROUTE             PIC X(38).
           05  WS-DA-COMMAREA          PIC X(512).
      *
           COPY CVAUTHW1Y.
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
           COPY CARDSET.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           IF EIBCALEN = ZERO
               PERFORM 9100-NO-COMMAREA
               GO TO 0000-RETURN
           END-IF
      *
           MOVE DFHCOMMAREA            TO CA-WORK-AREA
           PERFORM 0100-INIT
      *
           IF CAW-SCREEN-ID NOT = 'CARDLST '
               PERFORM 1000-FIRST-DISPLAY
               GO TO 0000-RETURN
           END-IF
      *
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2000-PROCESS-INPUT
               WHEN DFHPF7
                   PERFORM 2200-PAGE-BACKWARD
               WHEN DFHPF8
                   PERFORM 2100-PAGE-FORWARD
               WHEN DFHPF3
                   PERFORM 7000-BACK-TO-MENU
               WHEN DFHPF12
                   PERFORM 7100-EXIT-SESSION
               WHEN DFHCLEAR
                   PERFORM 7100-EXIT-SESSION
               WHEN OTHER
                   MOVE 'KEY NOT ACTIVE - PF7, PF8, PF3 OR PF12'
                                       TO CAW-MSG
                   PERFORM 5000-SEND-SCREEN
           END-EVALUATE
           .
       0000-RETURN.
           EXEC CICS RETURN
                     TRANSID(WS-TRAN-CARD)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
           END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           MOVE SPACES                 TO ERROR-AREA
           MOVE LOW-VALUES             TO CARDLSTO
           MOVE SPACES                 TO WS-PAGE-TABLE
           MOVE ZERO                   TO WS-LINE-CNT
           MOVE 'N'                    TO WS-END-SW
                                          WS-BROWSE-OPEN-SW
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT)
                     DATESEP('/')
                     RESP(WS-RESP)
           END-EXEC
      *
           IF CAW-TRAIL-CNT < 8
               ADD 1                   TO CAW-TRAIL-CNT
               MOVE WS-PGM-ID          TO CAW-TRAIL-PGM(CAW-TRAIL-CNT)
               MOVE ZERO               TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 1000 - FIRST DISPLAY                                           *
      ******************************************************************
       1000-FIRST-DISPLAY.
           MOVE 'Y'                    TO WS-ERASE-SW
           MOVE 'CARDLST '             TO CAW-SCREEN-ID
           MOVE SPACES                 TO CAW-BROWSE
           MOVE ZERO                   TO CAW-BR-PAGE-NBR
      *
           IF CAW-ACCT-ID > ZERO
               PERFORM 1100-START-NEW-LIST
           ELSE
               MOVE WS-MSG-KEY-REQD    TO CAW-MSG
           END-IF
      *
           PERFORM 5000-SEND-SCREEN
           .
      *
       1100-START-NEW-LIST.
           MOVE CAW-ACCT-ID            TO WS-ACCT-NUM
           MOVE WS-ACCT-NUM            TO WS-ACCT-KEY
           MOVE WS-ACCT-KEY            TO CAW-BR-ACCT-KEY
           MOVE LOW-VALUES             TO CAW-BR-FIRST-CARD
                                          CAW-BR-LAST-CARD
           MOVE 'N'                    TO CAW-BR-EOF-SW
           MOVE 'Y'                    TO CAW-BR-BOF-SW
           MOVE 1                      TO CAW-BR-PAGE-NBR
           MOVE 'F'                    TO WS-DIRECTION
      *
           PERFORM 3000-BUILD-PAGE
           .
      *
      ******************************************************************
      * 2000 - RE-ENTRY HANDLING                                       *
      ******************************************************************
       2000-PROCESS-INPUT.
           PERFORM 2900-RECEIVE
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE WS-MSG-KEY-REQD    TO CAW-MSG
               PERFORM 5000-SEND-SCREEN
               GO TO 2000-EXIT
           END-IF
      *
      *    A NEW ACCOUNT NUMBER RESTARTS THE BROWSE
           IF CLACCTL > ZERO
               IF CLACCTI IS NOT NUMERIC
                   MOVE 'ACCOUNT NUMBER MUST BE NUMERIC'
                                       TO CAW-MSG
                   PERFORM 5000-SEND-SCREEN
                   GO TO 2000-EXIT
               END-IF
               IF CLACCTI NOT = CAW-BR-ACCT-KEY
                   MOVE CLACCTI        TO CAW-ACCT-ID
                   PERFORM 1100-START-NEW-LIST
                   PERFORM 5000-SEND-SCREEN
                   GO TO 2000-EXIT
               END-IF
           END-IF
      *
           PERFORM 2300-CHECK-SELECTION
           IF CAW-CARD-NUM NOT = SPACES AND CAW-CARD-NUM NOT =
              LOW-VALUES
               PERFORM 7200-GO-TO-DETAIL
           END-IF
      *
           MOVE 'F'                    TO WS-DIRECTION
           PERFORM 3000-BUILD-PAGE
           PERFORM 5000-SEND-SCREEN
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-PAGE-FORWARD.
           PERFORM 2900-RECEIVE
      *
           IF CAW-BR-EOF
               MOVE WS-MSG-EOF         TO CAW-MSG
               PERFORM 5000-SEND-SCREEN
               GO TO 2100-EXIT
           END-IF
      *
           MOVE 'F'                    TO WS-DIRECTION
           ADD 1                       TO CAW-BR-PAGE-NBR
           MOVE 'N'                    TO CAW-BR-BOF-SW
           PERFORM 3000-BUILD-PAGE
           PERFORM 5000-SEND-SCREEN
           .
       2100-EXIT.
           EXIT
           .
      *
       2200-PAGE-BACKWARD.
           PERFORM 2900-RECEIVE
      *
           IF CAW-BR-PAGE-NBR <= 1
               MOVE 'Y'                TO CAW-BR-BOF-SW
               MOVE WS-MSG-BOF         TO CAW-MSG
               PERFORM 5000-SEND-SCREEN
               GO TO 2200-EXIT
           END-IF
      *
           MOVE 'B'                    TO WS-DIRECTION
           SUBTRACT 1                FROM CAW-BR-PAGE-NBR
           MOVE 'N'                    TO CAW-BR-EOF-SW
           PERFORM 3000-BUILD-PAGE
           PERFORM 5000-SEND-SCREEN
           .
       2200-EXIT.
           EXIT
           .
      *
       2300-CHECK-SELECTION.
           MOVE SPACES                 TO CAW-CARD-NUM
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 10
               IF CLSELL(WS-SUB) > ZERO
                   IF CLSELI(WS-SUB) = 'S' OR 's'
                       IF CLCARDO(WS-SUB) NOT = SPACES
                           MOVE CLCARDO(WS-SUB)
                                       TO CAW-CARD-NUM
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       2900-RECEIVE.
           EXEC CICS RECEIVE
                     MAP(WS-MAP)
                     MAPSET(WS-MAPSET)
                     INTO(CARDLSTI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
              AND WS-RESP NOT = DFHRESP(MAPFAIL)
               MOVE '2900-RECEIVE'     TO ER-PARAGRAPH
               PERFORM 8100-CICS-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 3000 - BUILD ONE PAGE FROM THE ALTERNATE INDEX PATH            *
      *        FORWARD PAGING RESUMES AFTER THE LAST CARD SHOWN,       *
      *        BACKWARD PAGING RESUMES BEFORE THE FIRST ONE.           *
      ******************************************************************
       3000-BUILD-PAGE.
           MOVE SPACES                 TO WS-PAGE-TABLE
           MOVE ZERO                   TO WS-LINE-CNT
           MOVE 'N'                    TO WS-END-SW
      *
           PERFORM 3100-START-BROWSE
           IF NOT WS-BROWSE-OPEN
               GO TO 3000-EXIT
           END-IF
      *
           IF WS-FORWARD
               PERFORM 3200-READ-FORWARD
                   UNTIL WS-END-OF-BROWSE OR WS-LINE-CNT = 10
           ELSE
               PERFORM 3300-READ-BACKWARD
                   UNTIL WS-END-OF-BROWSE OR WS-LINE-CNT = 10
               PERFORM 3400-REVERSE-PAGE
           END-IF
      *
           PERFORM 3500-END-BROWSE
      *
           IF WS-LINE-CNT = ZERO
               MOVE WS-MSG-NO-CARDS    TO CAW-MSG
               MOVE 'Y'                TO CAW-BR-EOF-SW
           ELSE
               MOVE WS-PG-CARD(1)      TO CAW-BR-FIRST-CARD
               MOVE WS-PG-CARD(WS-LINE-CNT)
                                       TO CAW-BR-LAST-CARD
               MOVE WS-MSG-OK          TO CAW-MSG
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3100 - POSITION THE BROWSE.  THE PATH KEY IS THE ACCOUNT       *
      *        NUMBER ONLY, SO THE RIDFLD IS 11 BYTES AND THE BROWSE   *
      *        IS RESTRICTED WITH KEYLENGTH / GENERIC.                 *
      ******************************************************************
       3100-START-BROWSE.
           MOVE CAW-BR-ACCT-KEY        TO WS-ACCT-KEY
      *
           EXEC CICS STARTBR
                     FILE(WS-XREF-PATH)
                     RIDFLD(WS-ACCT-KEY)
                     KEYLENGTH(11)
                     GENERIC
                     GTEQ
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 'Y'            TO WS-BROWSE-OPEN-SW
               WHEN DFHRESP(NOTFND)
                   MOVE 'N'            TO WS-BROWSE-OPEN-SW
                   MOVE WS-MSG-NO-CARDS
                                       TO CAW-MSG
               WHEN OTHER
                   MOVE 'N'            TO WS-BROWSE-OPEN-SW
                   MOVE '3100-START-BROWSE'
                                       TO ER-PARAGRAPH
                   MOVE WS-XREF-PATH   TO ER-FILE-NAME
                   MOVE WS-ACCT-KEY    TO ER-VSAM-KEY
                   PERFORM 8200-VSAM-ERROR
           END-EVALUATE
      *
           IF NOT WS-BROWSE-OPEN
               GO TO 3100-EXIT
           END-IF
      *
      *    SKIP THE CARDS ALREADY SHOWN ON EARLIER PAGES
           IF WS-FORWARD
               AND CAW-BR-LAST-CARD NOT = LOW-VALUES
               AND CAW-BR-LAST-CARD NOT = SPACES
               PERFORM 3150-SKIP-TO-RESUME
           END-IF
      *
           IF WS-BACKWARD
               PERFORM 3160-POSITION-BACKWARD
           END-IF
           .
       3100-EXIT.
           EXIT
           .
      *
       3150-SKIP-TO-RESUME.
           PERFORM UNTIL WS-END-OF-BROWSE
               EXEC CICS READNEXT
                         FILE(WS-XREF-PATH)
                         INTO(XREF-RECORD)
                         LENGTH(WS-XREF-LEN)
                         RIDFLD(WS-ACCT-KEY)
                         KEYLENGTH(11)
                         RESP(WS-RESP)
               END-EXEC
      *
               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'Y'        TO WS-END-SW
                       MOVE 'Y'        TO CAW-BR-EOF-SW
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'Y'        TO WS-END-SW
                       MOVE '3150-SKIP-TO-RESUME'
                                       TO ER-PARAGRAPH
                       MOVE WS-XREF-PATH
                                       TO ER-FILE-NAME
                       PERFORM 8200-VSAM-ERROR
                   WHEN XR-ACCT-ID NOT = CAW-BR-ACCT-KEY
                       MOVE 'Y'        TO WS-END-SW
                       MOVE 'Y'        TO CAW-BR-EOF-SW
                   WHEN XR-CARD-NUM = CAW-BR-LAST-CARD
                       MOVE 'Y'        TO WS-END-SW
               END-EVALUATE
           END-PERFORM
      *
           MOVE 'N'                    TO WS-END-SW
           .
      *
       3160-POSITION-BACKWARD.
      *    READ FORWARD TO THE FIRST CARD OF THE PAGE JUST SHOWN, THEN
      *    LET 3300 WALK BACKWARDS FROM THERE.
           PERFORM UNTIL WS-END-OF-BROWSE
               EXEC CICS READNEXT
                         FILE(WS-XREF-PATH)
                         INTO(XREF-RECORD)
                         LENGTH(WS-XREF-LEN)
                         RIDFLD(WS-ACCT-KEY)
                         KEYLENGTH(11)
                         RESP(WS-RESP)
               END-EXEC
      *
               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'Y'        TO WS-END-SW
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'Y'        TO WS-END-SW
                   WHEN XR-CARD-NUM = CAW-BR-FIRST-CARD
                       MOVE 'Y'        TO WS-END-SW
               END-EVALUATE
           END-PERFORM
      *
           MOVE 'N'                    TO WS-END-SW
           .
      *
       3200-READ-FORWARD.
           EXEC CICS READNEXT
                     FILE(WS-XREF-PATH)
                     INTO(XREF-RECORD)
                     LENGTH(WS-XREF-LEN)
                     RIDFLD(WS-ACCT-KEY)
                     KEYLENGTH(11)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(ENDFILE)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE 'Y'            TO CAW-BR-EOF-SW
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE '3200-READ-FORWARD'
                                       TO ER-PARAGRAPH
                   MOVE WS-XREF-PATH   TO ER-FILE-NAME
                   PERFORM 8200-VSAM-ERROR
               WHEN XR-ACCT-ID NOT = CAW-BR-ACCT-KEY
                   MOVE 'Y'            TO WS-END-SW
                   MOVE 'Y'            TO CAW-BR-EOF-SW
               WHEN OTHER
                   PERFORM 3900-ADD-LINE
           END-EVALUATE
           .
      *
       3300-READ-BACKWARD.
           EXEC CICS READPREV
                     FILE(WS-XREF-PATH)
                     INTO(XREF-RECORD)
                     LENGTH(WS-XREF-LEN)
                     RIDFLD(WS-ACCT-KEY)
                     KEYLENGTH(11)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(ENDFILE)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE 'Y'            TO CAW-BR-BOF-SW
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE '3300-READ-BACKWARD'
                                       TO ER-PARAGRAPH
                   MOVE WS-XREF-PATH   TO ER-FILE-NAME
                   PERFORM 8200-VSAM-ERROR
               WHEN XR-ACCT-ID NOT = CAW-BR-ACCT-KEY
                   MOVE 'Y'            TO WS-END-SW
                   MOVE 'Y'            TO CAW-BR-BOF-SW
               WHEN OTHER
                   PERFORM 3900-ADD-LINE
           END-EVALUATE
           .
      *
      *    THE BACKWARD PAGE IS BUILT BOTTOM UP - FLIP IT.
       3400-REVERSE-PAGE.
           DIVIDE WS-LINE-CNT BY 2 GIVING WS-HALF-CNT
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > WS-HALF-CNT
               COMPUTE WS-OTHER-SUB = WS-LINE-CNT - WS-SUB + 1
               MOVE WS-PAGE-ENTRY(WS-SUB)
                                       TO WS-SWAP-ENTRY
               MOVE WS-PAGE-ENTRY(WS-OTHER-SUB)
                                       TO WS-PAGE-ENTRY(WS-SUB)
               MOVE WS-SWAP-ENTRY      TO WS-PAGE-ENTRY(WS-OTHER-SUB)
           END-PERFORM
           .
      *
       3500-END-BROWSE.
           IF WS-BROWSE-OPEN
               EXEC CICS ENDBR
                         FILE(WS-XREF-PATH)
                         RESP(WS-RESP)
               END-EXEC
               MOVE 'N'                TO WS-BROWSE-OPEN-SW
           END-IF
           .
      *
       3900-ADD-LINE.
           ADD 1                       TO WS-LINE-CNT
           MOVE XR-CARD-NUM            TO WS-PG-CARD(WS-LINE-CNT)
           MOVE XR-PRODUCT-CD          TO WS-PG-PRODUCT(WS-LINE-CNT)
           MOVE XR-EMBOSSED-NAME       TO WS-PG-NAME(WS-LINE-CNT)
      *
           MOVE SPACES                 TO WS-EXPIRY-EDIT
           MOVE XR-EXPIRY-YYMM(1:2)    TO WS-EXPIRY-EDIT(1:2)
           MOVE '/'                    TO WS-EXPIRY-EDIT(3:1)
           MOVE XR-EXPIRY-YYMM(3:2)    TO WS-EXPIRY-EDIT(4:2)
           MOVE WS-EXPIRY-EDIT         TO WS-PG-EXPIRY(WS-LINE-CNT)
      *
           EVALUATE TRUE
               WHEN XR-ACTIVE
                   MOVE 'AC'           TO WS-PG-STATUS(WS-LINE-CNT)
               WHEN XR-BLOCKED
                   MOVE 'BL'           TO WS-PG-STATUS(WS-LINE-CNT)
               WHEN XR-CLOSED
                   MOVE 'CL'           TO WS-PG-STATUS(WS-LINE-CNT)
               WHEN XR-LOST-STOLEN
                   MOVE 'LS'           TO WS-PG-STATUS(WS-LINE-CNT)
               WHEN XR-EXPIRED
                   MOVE 'EX'           TO WS-PG-STATUS(WS-LINE-CNT)
               WHEN XR-NOT-ACTIVATED
                   MOVE 'NA'           TO WS-PG-STATUS(WS-LINE-CNT)
               WHEN OTHER
                   MOVE '??'           TO WS-PG-STATUS(WS-LINE-CNT)
           END-EVALUATE
           .
      *
      ******************************************************************
      * 5000 - PAINT THE LIST                                          *
      ******************************************************************
       5000-SEND-SCREEN.
           MOVE WS-DATE-OUT            TO CLDATEO
           MOVE CAW-MSG                TO CLMSGO
           MOVE CAW-BR-PAGE-NBR        TO CLPAGEO
           MOVE WS-LINE-CNT            TO CLCNTO
           MOVE 'CARDLST '             TO CAW-SCREEN-ID
      *
           IF CAW-ACCT-ID > ZERO
               MOVE CAW-ACCT-ID        TO CLACCTO
           END-IF
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 10
               MOVE WS-PG-CARD(WS-SUB)    TO CLCARDO(WS-SUB)
               MOVE WS-PG-STATUS(WS-SUB)  TO CLSTATO(WS-SUB)
               MOVE WS-PG-PRODUCT(WS-SUB) TO CLPRODO(WS-SUB)
               MOVE WS-PG-EXPIRY(WS-SUB)  TO CLEXPO(WS-SUB)
               MOVE WS-PG-NAME(WS-SUB)    TO CLNAMEO(WS-SUB)
               MOVE SPACE                 TO CLSELO(WS-SUB)
           END-PERFORM
      *
           IF WS-ERASE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDLSTO)
                         ERASE
                         CURSOR
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
               MOVE 'N'                TO WS-ERASE-SW
           ELSE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDLSTO)
                         DATAONLY
                         CURSOR
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           .
      *
      ******************************************************************
      * 7000 - NAVIGATION                                              *
      ******************************************************************
       7000-BACK-TO-MENU.
           MOVE SPACES                 TO CAW-SCREEN-ID
                                          CAW-MSG
      *
           EXEC CICS XCTL
                     PROGRAM('CACRD00 ')
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE '7000-BACK-TO-MENU'    TO ER-PARAGRAPH
           PERFORM 8100-CICS-ERROR
           .
      *
       7100-EXIT-SESSION.
           MOVE 'X '                   TO CAW-OPTION
           MOVE 'OPTX    '             TO CAW-ROUTE-KEY
           PERFORM 7300-DISPATCH
           .
      *
      *    CARD SELECTED FROM THE LIST - HAND OVER TO THE DETAIL SCREEN
      *    THROUGH THE MENU ROUTE SO THIS PROGRAM STAYS FREE OF TARGET
      *    PROGRAM NAMES.
       7200-GO-TO-DETAIL.
           MOVE '03'                   TO CAW-OPTION
           MOVE 'OPT03   '             TO CAW-ROUTE-KEY
           MOVE SPACES                 TO CAW-SCREEN-ID
           PERFORM 7300-DISPATCH
           .
      *
       7300-DISPATCH.
           MOVE SPACES                 TO ROUTE-REQUEST
           MOVE 'MENU'                 TO RQ-ROUTE-TYPE
           MOVE CAW-ROUTE-KEY          TO RQ-ROUTE-KEY
           MOVE 1                      TO RQ-SEQ-NBR
           MOVE ROUTE-REQUEST          TO WS-DA-ROUTE
           MOVE CA-WORK-AREA           TO WS-DA-COMMAREA
      *
           EXEC CICS LINK
                     PROGRAM(WS-DISPATCHER-ONLINE)
                     COMMAREA(WS-DISPATCH-AREA)
                     LENGTH(LENGTH OF WS-DISPATCH-AREA)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE 'FUNCTION UNAVAILABLE - ROUTE NOT RESOLVED'
                                       TO CAW-MSG
           MOVE 'CARDLST '             TO CAW-SCREEN-ID
           PERFORM 5000-SEND-SCREEN
           .
      *
      ******************************************************************
      * 8000 - ERROR HANDLING                                          *
      ******************************************************************
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
           PERFORM 8900-LINK-ERROR-PGM
           .
      *
       8200-VSAM-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
                                          ER-VSAM-RC
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE 'CARD LIST BROWSE FAILED'
                                       TO ER-MESSAGE
           PERFORM 8900-LINK-ERROR-PGM
           .
      *
       8900-LINK-ERROR-PGM.
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE EIBTRMID               TO ER-TERM-ID
           MOVE 'N'                    TO ER-ABEND-REQUESTED
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM-ONLINE)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
       9100-NO-COMMAREA.
           EXEC CICS SEND
                     TEXT('CACRD02 MUST BE STARTED FROM THE CA00 MENU')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           EXEC CICS RETURN END-EXEC
           .
