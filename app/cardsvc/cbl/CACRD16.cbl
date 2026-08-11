      ******************************************************************
      * CACRD16 - REFERENCE / REASON CODE LOOKUP                       *
      *                                                                *
      * MENU OPTION 10.  TWO ENQUIRIES ON ONE TRANSACTION -            *
      *   CRD16A  REASON CODE BROWSE OVER THE RSNCODE KSDS             *
      *   CRD16B  MERCHANT LOOKUP OVER CARDSVC.MERCHANT                *
      * PF5 SWAPS BETWEEN THEM, PF7 AND PF8 PAGE.                      *
      *                                                                *
      * CALLED BY  - CACRD90 BY XCTL, ROUTE MENU / OPT10               *
      * RETURNS TO - CACRD00 BY XCTL                                   *
      * CALLS      - CACRD91 BY LINK FOR ERROR DISPLAY                 *
      * MAPSET     - CARDST2, MAPS CRD16A AND CRD16B                   *
      * FILES      - RSNCODE  KSDS  STARTBR READNEXT READPREV ENDBR    *
      * TABLES     - CARDSVC.MERCHANT  SELECT                          *
      *                                                                *
      * THE BROWSE IS NOT HELD ACROSS A TERMINAL WAIT.  EACH PASS      *
      * REPOSITIONS FROM THE KEY SAVED IN THE COMMAREA AND ENDS THE    *
      * BROWSE BEFORE RETURNING.                                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD16.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD16 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDST2 '.
       01  WS-MAP-RSN                  PIC X(8)  VALUE 'CRD16A  '.
       01  WS-MAP-MER                  PIC X(8)  VALUE 'CRD16B  '.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CACRD00 '.
       01  WS-FILE-RSN                 PIC X(8)  VALUE 'RSNCODE '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE 0.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-END-SW               PIC X     VALUE 'N'.
               88  WS-END-OF-FILE                VALUE 'Y'.
           05  WS-BROWSE-SW            PIC X     VALUE 'N'.
               88  WS-BROWSE-OPEN                VALUE 'Y'.
           05  WS-EMPTY-SW             PIC X     VALUE 'N'.
               88  WS-PAGE-EMPTY                 VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-ROW-CNT              PIC S9(4) COMP VALUE 0.
           05  WS-IDX                  PIC S9(4) COMP VALUE 0.
           05  WS-MAX-ROWS             PIC S9(4) COMP VALUE 10.
           05  WS-MAX-MERCH            PIC S9(4) COMP VALUE 8.
           05  WS-KEYLEN               PIC S9(4) COMP VALUE 8.
      *
       01  WS-BROWSE-KEY.
           05  WS-BR-CATEGORY          PIC X(4)  VALUE SPACES.
           05  WS-BR-CODE              PIC X(4)  VALUE SPACES.
      *
      *    THE PAGE HOLDING AREA.  READPREV FILLS IT BACKWARDS SO THE
      *    ROWS ARE TURNED ROUND BEFORE THEY ARE PAINTED.
       01  WS-PAGE-TABLE.
           05  WS-PAGE-ROW OCCURS 10 TIMES.
               10  WS-PG-CATEGORY      PIC X(4).
               10  WS-PG-CODE          PIC X(4).
               10  WS-PG-DESC          PIC X(20).
               10  WS-PG-ACTION        PIC X(4).
               10  WS-PG-PROV          PIC X.
               10  WS-PG-DAYS          PIC X(3).
               10  WS-PG-NETWORK       PIC X(4).
      *
       01  WS-MERCH-TABLE.
           05  WS-MER-ROW OCCURS 8 TIMES.
               10  WS-MR-ID            PIC X(15).
               10  WS-MR-NAME          PIC X(24).
               10  WS-MR-MCC           PIC X(4).
               10  WS-MR-COUNTRY       PIC X(3).
               10  WS-MR-RATE          PIC X(6).
               10  WS-MR-HIGH-RISK     PIC X.
               10  WS-MR-STATUS        PIC X.
      *
       01  WS-EDIT-FIELDS.
           05  WS-EDIT-DAYS            PIC ZZ9.
           05  WS-EDIT-RATE            PIC ZZ9.99.
           05  WS-EDIT-PAGE            PIC ZZ9.
      *
       01  WS-DATE-AREAS.
           05  WS-TODAY-YYYYMMDD       PIC 9(8)  VALUE ZERO.
           05  WS-DISPLAY-DATE         PIC X(10) VALUE SPACES.
      *
       01  WS-MESSAGES.
           05  WS-MSG-ENTER            PIC X(78) VALUE
               'KEY A CATEGORY OR A CODE AND PRESS ENTER'.
           05  WS-MSG-MER-ENTER        PIC X(78) VALUE
               'KEY A MERCHANT ID OR AN MCC AND PRESS ENTER'.
           05  WS-MSG-CAT-BAD          PIC X(78) VALUE
               'CATEGORY MUST BE DISP CHBK BLCK OR FEES'.
           05  WS-MSG-NOT-FOUND        PIC X(78) VALUE
               'NO REASON CODE ON FILE FOR THAT KEY'.
           05  WS-MSG-EOF              PIC X(78) VALUE
               'END OF FILE - NO FURTHER CODES'.
           05  WS-MSG-TOP              PIC X(78) VALUE
               'TOP OF FILE - ALREADY ON THE FIRST PAGE'.
           05  WS-MSG-PAGED            PIC X(78) VALUE
               'PF7 PREVIOUS PAGE  PF8 NEXT PAGE  PF5 MERCHANT LOOKUP'.
           05  WS-MSG-MER-NONE         PIC X(78) VALUE
               'NO MERCHANT MATCHES THAT SELECTION'.
           05  WS-MSG-MER-KEY          PIC X(78) VALUE
               'MERCHANT ID OR MCC MUST BE ENTERED'.
           05  WS-MSG-MCC-BAD          PIC X(78) VALUE
               'MCC MUST BE FOUR NUMERIC DIGITS'.
           05  WS-MSG-MER-PAGED        PIC X(78) VALUE
               'PF8 NEXT PAGE  PF5 REASON CODES  PF3 MENU'.
           05  WS-MSG-BYE              PIC X(78) VALUE
               'CARD SERVICING SESSION ENDED'.
      *
      ******************************************************************
      * PROGRAM COMMAREA                                               *
      ******************************************************************
       01  WS-COMMAREA.
           05  CA-PGM-ID               PIC X(8).
           05  CA-FROM-PGM             PIC X(8).
           05  CA-SCREEN               PIC X.
               88  CA-SCR-REASON               VALUE 'A'.
               88  CA-SCR-MERCHANT             VALUE 'B'.
           05  CA-CATEGORY             PIC X(4).
           05  CA-CODE                 PIC X(4).
           05  CA-PAGE-NBR             PIC 9(3).
           05  CA-TOP-KEY              PIC X(8).
           05  CA-BOT-KEY              PIC X(8).
           05  CA-EOF-FLG              PIC X.
           05  CA-MER-ID               PIC X(15).
           05  CA-MER-MCC              PIC X(4).
           05  CA-MER-PAGE             PIC 9(3).
           05  CA-MER-LAST-ID          PIC X(15).
           05  CA-MER-EOF              PIC X.
           05  CA-OPER-ID              PIC X(8).
           05  CA-FILLER               PIC X(425).
      *
           COPY CVRSNC1Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      *    BMS SYMBOLIC MAPS FOR MAPSET CARDST2
           COPY CARDST2.
      *
      ******************************************************************
      * DB2 HOST VARIABLES - MERCHANT LOOKUP                           *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-MERCHANT.
           05  DCL-MER-ID              PIC X(15).
           05  DCL-MER-NAME            PIC X(40).
           05  DCL-MER-MCC             PIC X(4).
           05  DCL-MER-ACQUIRER        PIC X(11).
           05  DCL-MER-COUNTRY         PIC X(3).
           05  DCL-MER-CITY            PIC X(25).
           05  DCL-MER-HIGH-RISK       PIC X(1).
           05  DCL-MER-CB-RATE         PIC S9(3)V99 COMP-3.
           05  DCL-MER-STATUS          PIC X(1).
      *
       01  DCL-MER-KEYS.
           05  DCL-KEY-ID              PIC X(15).
           05  DCL-KEY-MCC             PIC X(4).
           05  DCL-LAST-ID             PIC X(15).
      *
       01  IND-MER-CITY                PIC S9(4) COMP.
       01  IND-MER-ACQ                 PIC S9(4) COMP.
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
      *
           IF EIBCALEN = ZERO
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE 'CACRD16 ENTERED WITH NO COMMAREA'
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
           IF CA-SCR-MERCHANT
               PERFORM 5000-MERCHANT-PASS
           ELSE
               PERFORM 2000-REASON-PASS
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
           MOVE 'A'                    TO CA-SCREEN
           MOVE SPACES                 TO CA-CATEGORY
           MOVE SPACES                 TO CA-CODE
           MOVE 1                      TO CA-PAGE-NBR
           MOVE LOW-VALUES             TO CA-TOP-KEY
           MOVE LOW-VALUES             TO CA-BOT-KEY
           MOVE 'N'                    TO CA-EOF-FLG
           MOVE 'N'                    TO CA-MER-EOF
           MOVE 1                      TO CA-MER-PAGE
           MOVE EIBTRMID               TO CA-OPER-ID
      *
           MOVE LOW-VALUES             TO CRD16AO
           MOVE WS-MSG-ENTER           TO M16MSGO
           PERFORM 8200-SEND-REASON
           .
      *
      ******************************************************************
      * 2000 - REASON CODE BROWSE                                      *
      ******************************************************************
       2000-REASON-PASS.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-END-SW
           MOVE 'N'                    TO WS-EMPTY-SW
           MOVE LOW-VALUES             TO CRD16AO
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-RSN)
                     MAPSET(WS-MAPSET)
                     INTO(CRD16AI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
      *            NO FIELDS CHANGED - A PAGING KEY ON ITS OWN
                   CONTINUE
               WHEN OTHER
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE 'RECEIVE MAP CRD16A FAILED'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           IF EIBAID = DFHPF5
               MOVE 'B'                TO CA-SCREEN
               MOVE 1                  TO CA-MER-PAGE
               MOVE SPACES             TO CA-MER-LAST-ID
               MOVE 'N'                TO CA-MER-EOF
               MOVE LOW-VALUES         TO CRD16BO
               MOVE WS-MSG-MER-ENTER   TO M16BMSGO
               PERFORM 8300-SEND-MERCH
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2100-EDIT-SELECTION
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-REASON
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2200-SET-POSITION
           PERFORM 3000-BUILD-PAGE
      *
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-REASON
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 4000-PAINT-PAGE
           PERFORM 8200-SEND-REASON
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-EDIT-SELECTION.
           IF M16CATL > ZERO
               MOVE M16CATI            TO CA-CATEGORY
               INSPECT CA-CATEGORY CONVERTING
                   'abcdefghijklmnopqrstuvwxyz' TO
                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      *        A NEW SELECTION ALWAYS RESTARTS THE BROWSE
               MOVE LOW-VALUES         TO CA-TOP-KEY
               MOVE LOW-VALUES         TO CA-BOT-KEY
               MOVE 1                  TO CA-PAGE-NBR
               MOVE 'N'                TO CA-EOF-FLG
           END-IF
      *
           IF M16CODEL > ZERO
               MOVE M16CODEI           TO CA-CODE
               INSPECT CA-CODE CONVERTING
                   'abcdefghijklmnopqrstuvwxyz' TO
                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               MOVE LOW-VALUES         TO CA-TOP-KEY
               MOVE LOW-VALUES         TO CA-BOT-KEY
               MOVE 1                  TO CA-PAGE-NBR
               MOVE 'N'                TO CA-EOF-FLG
           END-IF
      *
           IF CA-CATEGORY NOT = SPACES
               IF CA-CATEGORY NOT = 'DISP' AND
                  CA-CATEGORY NOT = 'CHBK' AND
                  CA-CATEGORY NOT = 'BLCK' AND
                  CA-CATEGORY NOT = 'FEES'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-CAT-BAD TO M16MSGO
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 2200 - GENERIC KEY POSITIONING                                 *
      *                                                                *
      * A CATEGORY ON ITS OWN IS A GENERIC START - THE FIRST FOUR      *
      * BYTES OF THE KEY ONLY.  A CATEGORY AND A CODE IS A FULL KEY.   *
      * A CODE WITHOUT A CATEGORY CANNOT POSITION THE KSDS SO THE      *
      * BROWSE RUNS FROM THE FRONT AND THE CODE FILTERS.               *
      ******************************************************************
       2200-SET-POSITION.
           EVALUATE TRUE
               WHEN EIBAID = DFHPF8 AND CA-BOT-KEY NOT = LOW-VALUES
                   MOVE CA-BOT-KEY     TO WS-BROWSE-KEY
                   MOVE 8              TO WS-KEYLEN
                   ADD 1               TO CA-PAGE-NBR
               WHEN EIBAID = DFHPF7 AND CA-TOP-KEY NOT = LOW-VALUES
                   MOVE CA-TOP-KEY     TO WS-BROWSE-KEY
                   MOVE 8              TO WS-KEYLEN
                   IF CA-PAGE-NBR > 1
                       SUBTRACT 1 FROM CA-PAGE-NBR
                   END-IF
               WHEN CA-CATEGORY NOT = SPACES AND CA-CODE NOT = SPACES
                   MOVE CA-CATEGORY    TO WS-BR-CATEGORY
                   MOVE CA-CODE        TO WS-BR-CODE
                   MOVE 8              TO WS-KEYLEN
                   MOVE 1              TO CA-PAGE-NBR
               WHEN CA-CATEGORY NOT = SPACES
                   MOVE CA-CATEGORY    TO WS-BR-CATEGORY
                   MOVE SPACES         TO WS-BR-CODE
                   MOVE 4              TO WS-KEYLEN
                   MOVE 1              TO CA-PAGE-NBR
               WHEN OTHER
                   MOVE LOW-VALUES     TO WS-BROWSE-KEY
                   MOVE 8              TO WS-KEYLEN
                   MOVE 1              TO CA-PAGE-NBR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - FILL ONE PAGE                                           *
      ******************************************************************
       3000-BUILD-PAGE.
           MOVE SPACES                 TO WS-PAGE-TABLE
           MOVE ZERO                   TO WS-ROW-CNT
      *
           PERFORM 3100-START-BROWSE
           IF WS-ERROR-FOUND OR WS-END-OF-FILE
               GO TO 3000-CLOSE
           END-IF
      *
           IF EIBAID = DFHPF7
               PERFORM 3300-READ-BACKWARD
                   UNTIL WS-ROW-CNT NOT < WS-MAX-ROWS
                      OR WS-END-OF-FILE
                      OR WS-ERROR-FOUND
               PERFORM 3400-REVERSE-PAGE
           ELSE
               PERFORM 3200-READ-FORWARD
                   UNTIL WS-ROW-CNT NOT < WS-MAX-ROWS
                      OR WS-END-OF-FILE
                      OR WS-ERROR-FOUND
           END-IF
           .
       3000-CLOSE.
           IF WS-BROWSE-OPEN
               EXEC CICS ENDBR
                         FILE(WS-FILE-RSN)
                         RESP(WS-RESP)
                         RESP2(WS-RESP2)
               END-EXEC
               MOVE 'N'                TO WS-BROWSE-SW
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE WS-FILE-RSN    TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE 'ENDBR FAILED ON RSNCODE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
               END-IF
           END-IF
      *
           IF WS-ROW-CNT = ZERO
               MOVE 'Y'                TO WS-EMPTY-SW
           END-IF
           .
      *
       3100-START-BROWSE.
           EXEC CICS STARTBR
                     FILE(WS-FILE-RSN)
                     RIDFLD(WS-BROWSE-KEY)
                     KEYLENGTH(WS-KEYLEN)
                     GENERIC
                     GTEQ
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 'Y'            TO WS-BROWSE-SW
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE WS-MSG-NOT-FOUND
                                       TO M16MSGO
               WHEN DFHRESP(ENDFILE)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE WS-MSG-EOF     TO M16MSGO
               WHEN OTHER
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE WS-FILE-RSN    TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE WS-BROWSE-KEY  TO ER-VSAM-KEY
                   MOVE 'STARTBR FAILED ON RSNCODE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       3200-READ-FORWARD.
           EXEC CICS READNEXT
                     FILE(WS-FILE-RSN)
                     INTO(RSN-CODE-RECORD)
                     RIDFLD(WS-BROWSE-KEY)
                     KEYLENGTH(8)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 3500-FILTER-AND-STORE
               WHEN DFHRESP(ENDFILE)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE 'Y'            TO CA-EOF-FLG
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE 'Y'            TO CA-EOF-FLG
               WHEN OTHER
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE WS-FILE-RSN    TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE WS-BROWSE-KEY  TO ER-VSAM-KEY
                   MOVE 'READNEXT FAILED ON RSNCODE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       3300-READ-BACKWARD.
           EXEC CICS READPREV
                     FILE(WS-FILE-RSN)
                     INTO(RSN-CODE-RECORD)
                     RIDFLD(WS-BROWSE-KEY)
                     KEYLENGTH(8)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 3500-FILTER-AND-STORE
               WHEN DFHRESP(ENDFILE)
                   MOVE 'Y'            TO WS-END-SW
                   MOVE WS-MSG-TOP     TO M16MSGO
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'            TO WS-END-SW
               WHEN OTHER
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE WS-FILE-RSN    TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE WS-BROWSE-KEY  TO ER-VSAM-KEY
                   MOVE 'READPREV FAILED ON RSNCODE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3500 - A RECORD HAS BEEN READ.  THE CATEGORY FILTER IS APPLIED *
      * HERE RATHER THAN BY THE BROWSE BECAUSE A CODE MAY HAVE BEEN    *
      * KEYED WITHOUT A CATEGORY.                                      *
      ******************************************************************
       3500-FILTER-AND-STORE.
           IF CA-CATEGORY NOT = SPACES
               IF RSN-CATEGORY NOT = CA-CATEGORY
      *            THE GENERIC BROWSE HAS RUN OFF THE CATEGORY
                   MOVE 'Y'            TO WS-END-SW
                   MOVE 'Y'            TO CA-EOF-FLG
                   GO TO 3500-EXIT
               END-IF
           END-IF
      *
           IF CA-CODE NOT = SPACES
               IF RSN-CODE NOT = CA-CODE
                   GO TO 3500-EXIT
               END-IF
           END-IF
      *
      *    SUPERSEDED CODES ARE STILL ON THE FILE FOR OLD DISPUTES
           IF RSN-ACTIVE-FLG = 'N' AND CA-CODE = SPACES
               GO TO 3500-EXIT
           END-IF
      *
           ADD 1                       TO WS-ROW-CNT
           MOVE RSN-CATEGORY           TO WS-PG-CATEGORY(WS-ROW-CNT)
           MOVE RSN-CODE               TO WS-PG-CODE(WS-ROW-CNT)
           MOVE RSN-SHORT-DESC         TO WS-PG-DESC(WS-ROW-CNT)
           MOVE RSN-ACTION-CD          TO WS-PG-ACTION(WS-ROW-CNT)
           MOVE RSN-PROV-CREDIT-FLG    TO WS-PG-PROV(WS-ROW-CNT)
           MOVE RSN-FILING-DAYS        TO WS-EDIT-DAYS
           MOVE WS-EDIT-DAYS           TO WS-PG-DAYS(WS-ROW-CNT)
           MOVE RSN-NETWORK-CD         TO WS-PG-NETWORK(WS-ROW-CNT)
      *
           IF WS-ROW-CNT = 1
               MOVE RSN-KEY            TO CA-TOP-KEY
           END-IF
           MOVE RSN-KEY                TO CA-BOT-KEY
           .
       3500-EXIT.
           EXIT
           .
      *
      *    READPREV BUILT THE PAGE FROM THE BOTTOM UP
       3400-REVERSE-PAGE.
           MOVE CA-BOT-KEY             TO WS-BROWSE-KEY
           MOVE CA-TOP-KEY             TO CA-BOT-KEY
           MOVE WS-BROWSE-KEY          TO CA-TOP-KEY
           MOVE 'N'                    TO CA-EOF-FLG
           .
      *
      ******************************************************************
      * 4000 - PAINT THE REASON CODE PAGE                              *
      ******************************************************************
       4000-PAINT-PAGE.
           MOVE CA-CATEGORY            TO M16CATO
           MOVE CA-CODE                TO M16CODEO
           MOVE CA-PAGE-NBR            TO WS-EDIT-PAGE
           MOVE WS-EDIT-PAGE           TO M16PAGEO
      *
           IF WS-PAGE-EMPTY
               MOVE WS-MSG-NOT-FOUND   TO M16MSGO
               GO TO 4000-EXIT
           END-IF
      *
           IF WS-ROW-CNT > 0
               MOVE WS-PG-CATEGORY(1)  TO M16CT1O
               MOVE WS-PG-CODE(1)      TO M16CD1O
               MOVE WS-PG-DESC(1)      TO M16DS1O
               MOVE WS-PG-ACTION(1)    TO M16AC1O
               MOVE WS-PG-PROV(1)      TO M16PV1O
               MOVE WS-PG-DAYS(1)      TO M16DY1O
               MOVE WS-PG-NETWORK(1)   TO M16NW1O
           END-IF
           IF WS-ROW-CNT > 1
               MOVE WS-PG-CATEGORY(2)  TO M16CT2O
               MOVE WS-PG-CODE(2)      TO M16CD2O
               MOVE WS-PG-DESC(2)      TO M16DS2O
               MOVE WS-PG-ACTION(2)    TO M16AC2O
               MOVE WS-PG-PROV(2)      TO M16PV2O
               MOVE WS-PG-DAYS(2)      TO M16DY2O
               MOVE WS-PG-NETWORK(2)   TO M16NW2O
           END-IF
           IF WS-ROW-CNT > 2
               MOVE WS-PG-CATEGORY(3)  TO M16CT3O
               MOVE WS-PG-CODE(3)      TO M16CD3O
               MOVE WS-PG-DESC(3)      TO M16DS3O
               MOVE WS-PG-ACTION(3)    TO M16AC3O
               MOVE WS-PG-PROV(3)      TO M16PV3O
               MOVE WS-PG-DAYS(3)      TO M16DY3O
               MOVE WS-PG-NETWORK(3)   TO M16NW3O
           END-IF
           IF WS-ROW-CNT > 3
               MOVE WS-PG-CATEGORY(4)  TO M16CT4O
               MOVE WS-PG-CODE(4)      TO M16CD4O
               MOVE WS-PG-DESC(4)      TO M16DS4O
               MOVE WS-PG-ACTION(4)    TO M16AC4O
               MOVE WS-PG-PROV(4)      TO M16PV4O
               MOVE WS-PG-DAYS(4)      TO M16DY4O
               MOVE WS-PG-NETWORK(4)   TO M16NW4O
           END-IF
           IF WS-ROW-CNT > 4
               MOVE WS-PG-CATEGORY(5)  TO M16CT5O
               MOVE WS-PG-CODE(5)      TO M16CD5O
               MOVE WS-PG-DESC(5)      TO M16DS5O
               MOVE WS-PG-ACTION(5)    TO M16AC5O
               MOVE WS-PG-PROV(5)      TO M16PV5O
               MOVE WS-PG-DAYS(5)      TO M16DY5O
               MOVE WS-PG-NETWORK(5)   TO M16NW5O
           END-IF
           IF WS-ROW-CNT > 5
               MOVE WS-PG-CATEGORY(6)  TO M16CT6O
               MOVE WS-PG-CODE(6)      TO M16CD6O
               MOVE WS-PG-DESC(6)      TO M16DS6O
               MOVE WS-PG-ACTION(6)    TO M16AC6O
               MOVE WS-PG-PROV(6)      TO M16PV6O
               MOVE WS-PG-DAYS(6)      TO M16DY6O
               MOVE WS-PG-NETWORK(6)   TO M16NW6O
           END-IF
           IF WS-ROW-CNT > 6
               MOVE WS-PG-CATEGORY(7)  TO M16CT7O
               MOVE WS-PG-CODE(7)      TO M16CD7O
               MOVE WS-PG-DESC(7)      TO M16DS7O
               MOVE WS-PG-ACTION(7)    TO M16AC7O
               MOVE WS-PG-PROV(7)      TO M16PV7O
               MOVE WS-PG-DAYS(7)      TO M16DY7O
               MOVE WS-PG-NETWORK(7)   TO M16NW7O
           END-IF
           IF WS-ROW-CNT > 7
               MOVE WS-PG-CATEGORY(8)  TO M16CT8O
               MOVE WS-PG-CODE(8)      TO M16CD8O
               MOVE WS-PG-DESC(8)      TO M16DS8O
               MOVE WS-PG-ACTION(8)    TO M16AC8O
               MOVE WS-PG-PROV(8)      TO M16PV8O
               MOVE WS-PG-DAYS(8)      TO M16DY8O
               MOVE WS-PG-NETWORK(8)   TO M16NW8O
           END-IF
           IF WS-ROW-CNT > 8
               MOVE WS-PG-CATEGORY(9)  TO M16CT9O
               MOVE WS-PG-CODE(9)      TO M16CD9O
               MOVE WS-PG-DESC(9)      TO M16DS9O
               MOVE WS-PG-ACTION(9)    TO M16AC9O
               MOVE WS-PG-PROV(9)      TO M16PV9O
               MOVE WS-PG-DAYS(9)      TO M16DY9O
               MOVE WS-PG-NETWORK(9)   TO M16NW9O
           END-IF
           IF WS-ROW-CNT > 9
               MOVE WS-PG-CATEGORY(10) TO M16CTAO
               MOVE WS-PG-CODE(10)     TO M16CDAO
               MOVE WS-PG-DESC(10)     TO M16DSAO
               MOVE WS-PG-ACTION(10)   TO M16ACAO
               MOVE WS-PG-PROV(10)     TO M16PVAO
               MOVE WS-PG-DAYS(10)     TO M16DYAO
               MOVE WS-PG-NETWORK(10)  TO M16NWAO
           END-IF
      *
           IF CA-EOF-FLG = 'Y'
               MOVE WS-MSG-EOF         TO M16MSGO
           ELSE
               MOVE WS-MSG-PAGED       TO M16MSGO
           END-IF
           .
       4000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5000 - MERCHANT LOOKUP                                         *
      ******************************************************************
       5000-MERCHANT-PASS.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-END-SW
           MOVE LOW-VALUES             TO CRD16BO
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-MER)
                     MAPSET(WS-MAPSET)
                     INTO(CRD16BI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   CONTINUE
               WHEN OTHER
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE 'RECEIVE MAP CRD16B FAILED'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   GO TO 5000-EXIT
           END-EVALUATE
      *
           IF EIBAID = DFHPF5
               MOVE 'A'                TO CA-SCREEN
               MOVE LOW-VALUES         TO CRD16AO
               MOVE WS-MSG-ENTER       TO M16MSGO
               PERFORM 8200-SEND-REASON
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5100-EDIT-MERCHANT
           IF WS-ERROR-FOUND
               PERFORM 8300-SEND-MERCH
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5200-FETCH-MERCHANTS
           IF WS-ERROR-FOUND
               PERFORM 8300-SEND-MERCH
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5400-PAINT-MERCHANTS
           PERFORM 8300-SEND-MERCH
           .
       5000-EXIT.
           EXIT
           .
      *
       5100-EDIT-MERCHANT.
           IF M16BMIDL > ZERO
               MOVE M16BMIDI           TO CA-MER-ID
               MOVE SPACES             TO CA-MER-LAST-ID
               MOVE 1                  TO CA-MER-PAGE
               MOVE 'N'                TO CA-MER-EOF
           END-IF
      *
           IF M16BMCCL > ZERO
               MOVE M16BMCCI           TO CA-MER-MCC
               MOVE SPACES             TO CA-MER-LAST-ID
               MOVE 1                  TO CA-MER-PAGE
               MOVE 'N'                TO CA-MER-EOF
           END-IF
      *
           IF CA-MER-ID = SPACES AND CA-MER-MCC = SPACES
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-MER-KEY     TO M16BMSGO
               GO TO 5100-EXIT
           END-IF
      *
           IF CA-MER-MCC NOT = SPACES
               IF CA-MER-MCC IS NOT NUMERIC
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-MCC-BAD TO M16BMSGO
               END-IF
           END-IF
           .
       5100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5200 - THE MERCHANT CURSOR.  PAGING IS DONE ON THE MERCHANT ID *
      * HELD IN THE COMMAREA - THE CURSOR IS NOT KEPT OPEN ACROSS A    *
      * TERMINAL WAIT.                                                 *
      ******************************************************************
       5200-FETCH-MERCHANTS.
           MOVE SPACES                 TO WS-MERCH-TABLE
           MOVE ZERO                   TO WS-ROW-CNT
      *
           IF CA-MER-ID NOT = SPACES
               MOVE CA-MER-ID          TO DCL-KEY-ID
           ELSE
               MOVE SPACES             TO DCL-KEY-ID
           END-IF
           MOVE CA-MER-MCC             TO DCL-KEY-MCC
      *
           IF EIBAID = DFHPF8
               ADD 1                   TO CA-MER-PAGE
               MOVE CA-MER-LAST-ID     TO DCL-LAST-ID
           ELSE
               MOVE SPACES             TO CA-MER-LAST-ID
               MOVE LOW-VALUES         TO DCL-LAST-ID
           END-IF
      *
           EXEC SQL DECLARE MERCSR CURSOR FOR
               SELECT MERCHANT_ID
                    , MERCHANT_NAME
                    , MCC
                    , ACQUIRER_ID
                    , COUNTRY_CD
                    , CITY
                    , HIGH_RISK_FLG
                    , CHARGEBACK_RATE
                    , STATUS
                 FROM CARDSVC.MERCHANT
                WHERE MERCHANT_ID > :DCL-LAST-ID
                  AND (:DCL-KEY-ID = ' '
                   OR  MERCHANT_ID LIKE :DCL-KEY-ID)
                  AND (:DCL-KEY-MCC = ' '
                   OR  MCC = :DCL-KEY-MCC)
                ORDER BY MERCHANT_ID
               FETCH FIRST 8 ROWS ONLY
           END-EXEC
      *
           EXEC SQL OPEN MERCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               MOVE 'MERCHANT          '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 5200-EXIT
           END-IF
      *
           PERFORM 5300-FETCH-ONE
               UNTIL WS-END-OF-FILE
                  OR WS-ERROR-FOUND
                  OR WS-ROW-CNT NOT < WS-MAX-MERCH
      *
           EXEC SQL CLOSE MERCSR END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               MOVE 'MERCHANT          '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-ERROR
           END-IF
           .
       5200-EXIT.
           EXIT
           .
      *
       5300-FETCH-ONE.
           EXEC SQL
               FETCH MERCSR
                INTO :DCL-MER-ID
                   , :DCL-MER-NAME
                   , :DCL-MER-MCC
                   , :DCL-MER-ACQUIRER :IND-MER-ACQ
                   , :DCL-MER-COUNTRY
                   , :DCL-MER-CITY :IND-MER-CITY
                   , :DCL-MER-HIGH-RISK
                   , :DCL-MER-CB-RATE
                   , :DCL-MER-STATUS
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1               TO WS-ROW-CNT
                   MOVE DCL-MER-ID     TO WS-MR-ID(WS-ROW-CNT)
                   MOVE DCL-MER-NAME   TO WS-MR-NAME(WS-ROW-CNT)
                   MOVE DCL-MER-MCC    TO WS-MR-MCC(WS-ROW-CNT)
                   MOVE DCL-MER-COUNTRY
                                       TO WS-MR-COUNTRY(WS-ROW-CNT)
                   MOVE DCL-MER-CB-RATE
                                       TO WS-EDIT-RATE
                   MOVE WS-EDIT-RATE   TO WS-MR-RATE(WS-ROW-CNT)
                   MOVE DCL-MER-HIGH-RISK
                                       TO WS-MR-HIGH-RISK(WS-ROW-CNT)
                   MOVE DCL-MER-STATUS TO WS-MR-STATUS(WS-ROW-CNT)
                   MOVE DCL-MER-ID     TO CA-MER-LAST-ID
               WHEN +100
                   MOVE 'Y'            TO WS-END-SW
                   MOVE 'Y'            TO CA-MER-EOF
               WHEN OTHER
                   MOVE 'FETCH   '     TO ER-SQL-OPERATION
                   MOVE 'MERCHANT          '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       5400-PAINT-MERCHANTS.
           MOVE CA-MER-ID              TO M16BMIDO
           MOVE CA-MER-MCC             TO M16BMCCO
           MOVE CA-MER-PAGE            TO WS-EDIT-PAGE
           MOVE WS-EDIT-PAGE           TO M16BPGEO
      *
           IF WS-ROW-CNT = ZERO
               MOVE WS-MSG-MER-NONE    TO M16BMSGO
               GO TO 5400-EXIT
           END-IF
      *
           IF WS-ROW-CNT > 0
               MOVE WS-MR-ID(1)        TO M16BI1O
               MOVE WS-MR-NAME(1)      TO M16BN1O
               MOVE WS-MR-MCC(1)       TO M16BC1O
               MOVE WS-MR-COUNTRY(1)   TO M16BY1O
               MOVE WS-MR-RATE(1)      TO M16BR1O
               MOVE WS-MR-HIGH-RISK(1) TO M16BH1O
               MOVE WS-MR-STATUS(1)    TO M16BS1O
           END-IF
           IF WS-ROW-CNT > 1
               MOVE WS-MR-ID(2)        TO M16BI2O
               MOVE WS-MR-NAME(2)      TO M16BN2O
               MOVE WS-MR-MCC(2)       TO M16BC2O
               MOVE WS-MR-COUNTRY(2)   TO M16BY2O
               MOVE WS-MR-RATE(2)      TO M16BR2O
               MOVE WS-MR-HIGH-RISK(2) TO M16BH2O
               MOVE WS-MR-STATUS(2)    TO M16BS2O
           END-IF
           IF WS-ROW-CNT > 2
               MOVE WS-MR-ID(3)        TO M16BI3O
               MOVE WS-MR-NAME(3)      TO M16BN3O
               MOVE WS-MR-MCC(3)       TO M16BC3O
               MOVE WS-MR-COUNTRY(3)   TO M16BY3O
               MOVE WS-MR-RATE(3)      TO M16BR3O
               MOVE WS-MR-HIGH-RISK(3) TO M16BH3O
               MOVE WS-MR-STATUS(3)    TO M16BS3O
           END-IF
           IF WS-ROW-CNT > 3
               MOVE WS-MR-ID(4)        TO M16BI4O
               MOVE WS-MR-NAME(4)      TO M16BN4O
               MOVE WS-MR-MCC(4)       TO M16BC4O
               MOVE WS-MR-COUNTRY(4)   TO M16BY4O
               MOVE WS-MR-RATE(4)      TO M16BR4O
               MOVE WS-MR-HIGH-RISK(4) TO M16BH4O
               MOVE WS-MR-STATUS(4)    TO M16BS4O
           END-IF
           IF WS-ROW-CNT > 4
               MOVE WS-MR-ID(5)        TO M16BI5O
               MOVE WS-MR-NAME(5)      TO M16BN5O
               MOVE WS-MR-MCC(5)       TO M16BC5O
               MOVE WS-MR-COUNTRY(5)   TO M16BY5O
               MOVE WS-MR-RATE(5)      TO M16BR5O
               MOVE WS-MR-HIGH-RISK(5) TO M16BH5O
               MOVE WS-MR-STATUS(5)    TO M16BS5O
           END-IF
           IF WS-ROW-CNT > 5
               MOVE WS-MR-ID(6)        TO M16BI6O
               MOVE WS-MR-NAME(6)      TO M16BN6O
               MOVE WS-MR-MCC(6)       TO M16BC6O
               MOVE WS-MR-COUNTRY(6)   TO M16BY6O
               MOVE WS-MR-RATE(6)      TO M16BR6O
               MOVE WS-MR-HIGH-RISK(6) TO M16BH6O
               MOVE WS-MR-STATUS(6)    TO M16BS6O
           END-IF
           IF WS-ROW-CNT > 6
               MOVE WS-MR-ID(7)        TO M16BI7O
               MOVE WS-MR-NAME(7)      TO M16BN7O
               MOVE WS-MR-MCC(7)       TO M16BC7O
               MOVE WS-MR-COUNTRY(7)   TO M16BY7O
               MOVE WS-MR-RATE(7)      TO M16BR7O
               MOVE WS-MR-HIGH-RISK(7) TO M16BH7O
               MOVE WS-MR-STATUS(7)    TO M16BS7O
           END-IF
           IF WS-ROW-CNT > 7
               MOVE WS-MR-ID(8)        TO M16BI8O
               MOVE WS-MR-NAME(8)      TO M16BN8O
               MOVE WS-MR-MCC(8)       TO M16BC8O
               MOVE WS-MR-COUNTRY(8)   TO M16BY8O
               MOVE WS-MR-RATE(8)      TO M16BR8O
               MOVE WS-MR-HIGH-RISK(8) TO M16BH8O
               MOVE WS-MR-STATUS(8)    TO M16BS8O
           END-IF
      *
           IF CA-MER-EOF = 'Y'
               MOVE WS-MSG-EOF         TO M16BMSGO
           ELSE
               MOVE WS-MSG-MER-PAGED   TO M16BMSGO
           END-IF
           .
       5400-EXIT.
           EXIT
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
       8200-SEND-REASON.
           MOVE WS-DISPLAY-DATE        TO M16DATEO
      *
           EXEC CICS SEND
                     MAP(WS-MAP-RSN)
                     MAPSET(WS-MAPSET)
                     FROM(CRD16AO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SEND MAP CRD16A FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
           END-IF
      *
           PERFORM 8400-RETURN-TRAN
           .
      *
       8300-SEND-MERCH.
           MOVE WS-DISPLAY-DATE        TO M16BDTEO
      *
           EXEC CICS SEND
                     MAP(WS-MAP-MER)
                     MAPSET(WS-MAPSET)
                     FROM(CRD16BO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SEND MAP CRD16B FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
           END-IF
      *
           PERFORM 8400-RETURN-TRAN
           .
      *
       8400-RETURN-TRAN.
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
           MOVE 'E'                    TO ER-SEVERITY
           MOVE 'SQL FAILURE IN MERCHANT LOOKUP'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
       9600-EXIT-SESSION.
           EXEC CICS SEND TEXT
                     FROM(WS-MSG-BYE)
                     LENGTH(78)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           .
