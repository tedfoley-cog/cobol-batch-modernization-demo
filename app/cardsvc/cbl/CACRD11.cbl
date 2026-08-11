      ******************************************************************
      * CACRD11 - CARD MAINTENANCE                                     *
      *                                                                *
      * MENU OPTION 5.  BLOCK, UNBLOCK, REPLACE AND REISSUE OF A       *
      * PLASTIC.  MAINTAINS CARDSVC.CARD AND THE CARDXREF VSAM FILE.   *
      *                                                                *
      * CALLED BY  - CACRD90 BY XCTL, ROUTE MENU / OPT05               *
      * RETURNS TO - CACRD00 BY XCTL                                   *
      * CALLS      - CACRD91 BY LINK FOR ERROR DISPLAY                 *
      * MAPSET     - CARDST2, MAPS CRD11A AND CRD11B                   *
      * FILES      - CARDXREF  KSDS  READ UPDATE REWRITE WRITE         *
      *              RSNCODE   KSDS  READ                              *
      * TABLES     - CARDSVC.CARD          SELECT UPDATE INSERT        *
      *              CARDSVC.ACCOUNT       SELECT                      *
      *                                                                *
      * ORIGINAL - 1997.  REISSUE SUPPORT ADDED 2002.  THE CONFIRM     *
      * SCREEN WAS ADDED 2004 AFTER THE BLOCK IN ERROR INCIDENT.       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD11.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD11 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDST2 '.
       01  WS-MAP-ENTRY                PIC X(8)  VALUE 'CRD11A  '.
       01  WS-MAP-CONF                 PIC X(8)  VALUE 'CRD11B  '.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CACRD00 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-CARD-FOUND-SW        PIC X     VALUE 'N'.
               88  WS-CARD-FOUND                 VALUE 'Y'.
           05  WS-XREF-FOUND-SW        PIC X     VALUE 'N'.
               88  WS-XREF-FOUND                 VALUE 'Y'.
           05  WS-REISSUE-SW           PIC X     VALUE 'N'.
               88  WS-REISSUE-WANTED             VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-SUB                  PIC S9(4) COMP VALUE 0.
           05  WS-DIGIT                PIC 9     VALUE 0.
           05  WS-DOUBLED              PIC 99    VALUE 0.
           05  WS-CHECK-SUM            PIC 9(4)  VALUE 0.
           05  WS-CHECK-DIGIT          PIC 9     VALUE 0.
           05  WS-SEQ-PART             PIC 9(9)  VALUE 0.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE 0.
           05  WS-NEW-CARD             PIC X(16) VALUE SPACES.
           05  WS-NEW-CARD-R REDEFINES WS-NEW-CARD.
               10  WS-NC-DIGIT         PIC 9 OCCURS 16 TIMES.
           05  WS-DISPLAY-AMT          PIC ZZZ,ZZZ,ZZ9.99-.
           05  WS-DISPLAY-DATE         PIC X(10) VALUE SPACES.
      *
      *    DATE DECOMPOSITION - THE SIX DIGIT ACTIVATION DATE IS
      *    WINDOWED AGAINST THE PIVOT IN CVCONSTY
       01  WS-DATE-AREAS.
           05  WS-TODAY-YYYYMMDD       PIC 9(8)  VALUE ZERO.
           05  WS-TODAY-R REDEFINES WS-TODAY-YYYYMMDD.
               10  WS-TODAY-CC         PIC 9(2).
               10  WS-TODAY-YY         PIC 9(2).
               10  WS-TODAY-MM         PIC 9(2).
               10  WS-TODAY-DD         PIC 9(2).
           05  WS-SIX-DIGIT            PIC 9(6)  VALUE ZERO.
           05  WS-SIX-DIGIT-R REDEFINES WS-SIX-DIGIT.
               10  WS-SIX-YY           PIC 9(2).
               10  WS-SIX-MM           PIC 9(2).
               10  WS-SIX-DD           PIC 9(2).
           05  WS-EXPANDED-DATE        PIC 9(8)  VALUE ZERO.
           05  WS-EXPANDED-R REDEFINES WS-EXPANDED-DATE.
               10  WS-EXP-CC           PIC 9(2).
               10  WS-EXP-YY           PIC 9(2).
               10  WS-EXP-MM           PIC 9(2).
               10  WS-EXP-DD           PIC 9(2).
      *
       01  WS-STATUS-TEXT              PIC X(14) VALUE SPACES.
       01  WS-ACTION-TEXT              PIC X(20) VALUE SPACES.
      *
       01  WS-MESSAGES.
           05  WS-MSG-NO-CARD          PIC X(78) VALUE
               'CARD NUMBER NOT ON FILE - CHECK AND RE-ENTER'.
           05  WS-MSG-CARD-REQD        PIC X(78) VALUE
               'CARD NUMBER MUST BE ENTERED'.
           05  WS-MSG-ACTN-REQD        PIC X(78) VALUE
               'ACTION MUST BE ONE OF BLCK UNBL REPL REIS'.
           05  WS-MSG-RSN-REQD         PIC X(78) VALUE
               'BLOCK REASON CODE IS REQUIRED FOR A BLOCK REQUEST'.
           05  WS-MSG-RSN-BAD          PIC X(78) VALUE
               'BLOCK REASON CODE NOT FOUND ON THE REASON CODE FILE'.
           05  WS-MSG-ALREADY-BLK      PIC X(78) VALUE
               'CARD IS ALREADY BLOCKED - NO ACTION TAKEN'.
           05  WS-MSG-CLOSED-BLK       PIC X(78) VALUE
               'CARD IS CLOSED - A CLOSED CARD CANNOT BE BLOCKED'.
           05  WS-MSG-LOST-BLK         PIC X(78) VALUE
               'CARD IS FLAGGED LOST OR STOLEN - BLOCK NOT REQUIRED'.
           05  WS-MSG-EXP-BLK          PIC X(78) VALUE
               'CARD HAS EXPIRED - BLOCK NOT PERMITTED, USE REISSUE'.
           05  WS-MSG-NOT-BLOCKED      PIC X(78) VALUE
               'CARD IS NOT BLOCKED - UNBLOCK IS NOT APPLICABLE'.
           05  WS-MSG-LOST-UNBLK       PIC X(78) VALUE
               'LOST OR STOLEN CARD CANNOT BE UNBLOCKED - REPLACE IT'.
           05  WS-MSG-CLOSED-UNBLK     PIC X(78) VALUE
               'CLOSED CARD CANNOT BE UNBLOCKED'.
           05  WS-MSG-EXP-UNBLK        PIC X(78) VALUE
               'EXPIRED CARD CANNOT BE UNBLOCKED - REISSUE REQUIRED'.
           05  WS-MSG-NEW-UNBLK        PIC X(78) VALUE
               'CARD HAS NEVER BEEN ACTIVATED - NOTHING TO UNBLOCK'.
           05  WS-MSG-CLOSED-REPL      PIC X(78) VALUE
               'CLOSED CARD CANNOT BE REPLACED - OPEN A NEW ACCOUNT'.
           05  WS-MSG-NEW-REPL         PIC X(78) VALUE
               'CARD NOT YET ACTIVATED - CANCEL AND RE-EMBOSS INSTEAD'.
           05  WS-MSG-BLK-REIS         PIC X(78) VALUE
               'BLOCKED CARD MUST BE UNBLOCKED BEFORE REISSUE'.
           05  WS-MSG-LOST-REIS        PIC X(78) VALUE
               'LOST OR STOLEN CARD MUST BE REPLACED, NOT REISSUED'.
           05  WS-MSG-CLOSED-REIS      PIC X(78) VALUE
               'CLOSED CARD CANNOT BE REISSUED'.
           05  WS-MSG-NEW-REIS         PIC X(78) VALUE
               'CARD NOT ACTIVATED - REISSUE WOULD ORPHAN THE PLASTIC'.
           05  WS-MSG-ACCT-CLOSED      PIC X(78) VALUE
               'ACCOUNT IS NOT OPEN - MAINTENANCE REFUSED'.
           05  WS-MSG-CONFIRM          PIC X(78) VALUE
               'CHECK THE DETAIL AND ENTER Y TO COMMIT THE CHANGE'.
           05  WS-MSG-ABANDONED        PIC X(78) VALUE
               'REQUEST ABANDONED - NO CHANGE HAS BEEN MADE'.
           05  WS-MSG-CONF-BAD         PIC X(78) VALUE
               'ENTER Y TO COMMIT OR N TO ABANDON'.
           05  WS-MSG-DONE-BLK         PIC X(78) VALUE
               'CARD HAS BEEN BLOCKED'.
           05  WS-MSG-DONE-UNBLK       PIC X(78) VALUE
               'CARD HAS BEEN UNBLOCKED AND IS ACTIVE'.
           05  WS-MSG-DONE-REPL        PIC X(78) VALUE
               'REPLACEMENT CARD RAISED - OLD PLASTIC CLOSED'.
           05  WS-MSG-DONE-REIS        PIC X(78) VALUE
               'CARD REISSUED - NEW PLASTIC WILL BE EMBOSSED TONIGHT'.
           05  WS-MSG-XREF-LOCK        PIC X(78) VALUE
               'CROSS REFERENCE RECORD IN USE - RETRY IN A MOMENT'.
      *
      ******************************************************************
      * PROGRAM COMMAREA - CARVED OUT OF THE STANDARD 512 BYTE AREA    *
      ******************************************************************
       01  WS-COMMAREA.
           05  CA-PGM-ID               PIC X(8).
           05  CA-FROM-PGM             PIC X(8).
           05  CA-STEP                 PIC X.
               88  CA-STEP-ENTRY               VALUE '1'.
               88  CA-STEP-CONFIRM             VALUE '2'.
           05  CA-CARD-NUM             PIC X(16).
           05  CA-NEW-CARD-NUM         PIC X(16).
           05  CA-ACTION               PIC X(4).
               88  CA-ACT-BLOCK                VALUE 'BLCK'.
               88  CA-ACT-UNBLOCK              VALUE 'UNBL'.
               88  CA-ACT-REPLACE              VALUE 'REPL'.
               88  CA-ACT-REISSUE              VALUE 'REIS'.
           05  CA-BLOCK-REASON         PIC X(4).
           05  CA-OLD-STATUS           PIC X.
           05  CA-NEW-STATUS           PIC X.
           05  CA-ACCT-ID              PIC 9(11).
           05  CA-CUST-ID              PIC 9(9).
           05  CA-REISSUE-CNT          PIC 9(2).
           05  CA-OPER-ID              PIC X(8).
           05  CA-FILLER               PIC X(410).
      *
           COPY CVCARD01Y.
           COPY CVXREF1Y.
           COPY CVRSNC1Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      *    BMS SYMBOLIC MAPS FOR MAPSET CARDST2
           COPY CARDST2.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-CARD.
           05  DCL-CARD-NUM            PIC X(16).
           05  DCL-ACCT-ID             PIC S9(11) COMP-3.
           05  DCL-CUST-ID             PIC S9(9)  COMP-3.
           05  DCL-EMBOSSED-NAME       PIC X(26).
           05  DCL-PRODUCT-CD          PIC X(4).
           05  DCL-CARD-STATUS         PIC X(1).
           05  DCL-EXPIRY-YYMM         PIC X(4).
           05  DCL-ISSUE-DATE          PIC X(10).
           05  DCL-ACTIVATION-DATE     PIC X(10).
           05  DCL-LAST-USED-DATE      PIC X(10).
           05  DCL-REISSUE-CNT         PIC S9(4) COMP.
           05  DCL-PREV-CARD-NUM       PIC X(16).
           05  DCL-BLOCK-REASON        PIC X(4).
           05  DCL-BLOCK-DATE          PIC X(10).
      *
       01  DCL-ACCT-STATUS             PIC X(1).
       01  DCL-PARTY-ID                PIC X(11).
       01  DCL-BRANCH-CD               PIC X(5).
      *
       01  IND-VARS.
           05  IND-ACTIVATION          PIC S9(4) COMP.
           05  IND-LAST-USED           PIC S9(4) COMP.
           05  IND-PREV-CARD           PIC S9(4) COMP.
           05  IND-BLOCK-REASON        PIC S9(4) COMP.
           05  IND-BLOCK-DATE          PIC S9(4) COMP.
           05  IND-BRANCH              PIC S9(4) COMP.
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
           MOVE LOW-VALUES             TO CRD11AO
           MOVE LOW-VALUES             TO CRD11BO
      *
           IF EIBCALEN = ZERO
               PERFORM 9500-NO-COMMAREA THRU 9500-EXIT
               GO TO 0000-RETURN-MENU
           END-IF
      *
           MOVE DFHCOMMAREA            TO WS-COMMAREA
      *
           IF CA-PGM-ID NOT = WS-PGM-ID
               PERFORM 1000-FIRST-TIME THRU 1000-EXIT
               GO TO 0000-EXIT
           END-IF
      *
           EVALUATE EIBAID
               WHEN DFHPF3
                   GO TO 0000-RETURN-MENU
               WHEN DFHPF12
                   PERFORM 9600-EXIT-SESSION THRU 9600-EXIT
                   GO TO 0000-EXIT
               WHEN DFHCLEAR
                   PERFORM 1000-FIRST-TIME THRU 1000-EXIT
                   GO TO 0000-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF CA-STEP-CONFIRM
               PERFORM 5000-CONFIRM-PASS THRU 5000-EXIT
           ELSE
               PERFORM 2000-ENTRY-PASS THRU 2000-EXIT
           END-IF
           GO TO 0000-EXIT
           .
       0000-RETURN-MENU.
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
               PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
           END-IF
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - FIRST ENTRY FROM THE DISPATCHER                         *
      ******************************************************************
       1000-FIRST-TIME.
           MOVE SPACES                 TO WS-COMMAREA
           MOVE WS-PGM-ID              TO CA-PGM-ID
           MOVE '1'                    TO CA-STEP
           MOVE EIBTRMID               TO CA-OPER-ID
           MOVE ZERO                   TO CA-ACCT-ID
           MOVE ZERO                   TO CA-CUST-ID
           MOVE ZERO                   TO CA-REISSUE-CNT
      *
           PERFORM 8100-GET-DATE THRU 8100-EXIT
           MOVE WS-DISPLAY-DATE        TO M11DATEO
           MOVE SPACES                 TO M11MSGO
           PERFORM 8200-SEND-ENTRY THRU 8200-EXIT
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - ENTRY SCREEN PASS                                       *
      ******************************************************************
       2000-ENTRY-PASS.
           MOVE 'N'                    TO WS-ERROR-SW
           PERFORM 8100-GET-DATE THRU 8100-EXIT
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-ENTRY)
                     MAPSET(WS-MAPSET)
                     INTO(CRD11AI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE 'NO DATA ENTERED - KEY THE CARD NUMBER'
                                       TO M11MSGO
                   PERFORM 8200-SEND-ENTRY THRU 8200-EXIT
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE 'RECEIVE MAP CRD11A FAILED'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           PERFORM 2100-EDIT-INPUT THRU 2100-EXIT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-ENTRY THRU 8200-EXIT
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2200-READ-CARD THRU 2200-EXIT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-ENTRY THRU 8200-EXIT
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2300-SHOW-CARD THRU 2300-EXIT
      *
           PERFORM 2400-CHECK-ACCOUNT THRU 2400-EXIT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-ENTRY THRU 8200-EXIT
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 3000-TEST-TRANSITION THRU 3000-EXIT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-ENTRY THRU 8200-EXIT
               GO TO 2000-EXIT
           END-IF
      *
           IF WS-REISSUE-WANTED
               PERFORM 4200-BUILD-NEW-NUMBER THRU 4200-EXIT
           ELSE
               MOVE SPACES             TO CA-NEW-CARD-NUM
           END-IF
      *
           MOVE '2'                    TO CA-STEP
           PERFORM 8300-SEND-CONFIRM THRU 8300-EXIT
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-EDIT-INPUT.
           IF M11CARDL = ZERO OR M11CARDI = SPACES
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-CARD-REQD   TO M11MSGO
               GO TO 2100-EXIT
           END-IF
           MOVE M11CARDI               TO CA-CARD-NUM
      *
           MOVE M11ACTNI               TO CA-ACTION
           IF NOT CA-ACT-BLOCK
              AND NOT CA-ACT-UNBLOCK
              AND NOT CA-ACT-REPLACE
              AND NOT CA-ACT-REISSUE
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ACTN-REQD   TO M11MSGO
               GO TO 2100-EXIT
           END-IF
      *
           MOVE M11BRSNI               TO CA-BLOCK-REASON
           IF CA-ACT-BLOCK
               IF CA-BLOCK-REASON = SPACES OR LOW-VALUES
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-RSN-REQD
                                       TO M11MSGO
                   GO TO 2100-EXIT
               END-IF
               PERFORM 2150-READ-REASON THRU 2150-EXIT
           END-IF
           .
       2100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2150 - VALIDATE THE BLOCK REASON AGAINST THE RSNCODE FILE      *
      ******************************************************************
       2150-READ-REASON.
           MOVE SPACES                 TO RSN-CODE-RECORD
           MOVE 'BLOK'                 TO RSN-CATEGORY
           MOVE CA-BLOCK-REASON        TO RSN-CODE
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
                       MOVE WS-MSG-RSN-BAD
                                       TO M11MSGO
                   END-IF
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-RSN-BAD TO M11MSGO
               WHEN OTHER
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE 'RSNCODE '     TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE RSN-KEY        TO ER-VSAM-KEY
                   MOVE 'READ FAILED ON REASON CODE FILE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
       2150-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2200 - READ THE CARD ROW                                       *
      ******************************************************************
       2200-READ-CARD.
           MOVE 'N'                    TO WS-CARD-FOUND-SW
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT CARD_NUM
                    , ACCT_ID
                    , CUST_ID
                    , EMBOSSED_NAME
                    , PRODUCT_CD
                    , CARD_STATUS
                    , EXPIRY_YYMM
                    , CHAR(ISSUE_DATE, ISO)
                    , CHAR(ACTIVATION_DATE, ISO)
                    , CHAR(LAST_USED_DATE, ISO)
                    , REISSUE_CNT
                    , PREV_CARD_NUM
                    , BLOCK_REASON
                    , CHAR(BLOCK_DATE, ISO)
                 INTO :DCL-CARD-NUM
                    , :DCL-ACCT-ID
                    , :DCL-CUST-ID
                    , :DCL-EMBOSSED-NAME
                    , :DCL-PRODUCT-CD
                    , :DCL-CARD-STATUS
                    , :DCL-EXPIRY-YYMM
                    , :DCL-ISSUE-DATE
                    , :DCL-ACTIVATION-DATE  :IND-ACTIVATION
                    , :DCL-LAST-USED-DATE   :IND-LAST-USED
                    , :DCL-REISSUE-CNT
                    , :DCL-PREV-CARD-NUM    :IND-PREV-CARD
                    , :DCL-BLOCK-REASON     :IND-BLOCK-REASON
                    , :DCL-BLOCK-DATE       :IND-BLOCK-DATE
                 FROM CARDSVC.CARD
                WHERE CARD_NUM = :DCL-CARD-NUM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-CARD-FOUND-SW
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NO-CARD TO M11MSGO
                   GO TO 2200-EXIT
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD              '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR THRU 9100-EXIT
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 2200-EXIT
           END-EVALUATE
      *
           MOVE SPACES                 TO CARD-RECORD
           MOVE DCL-CARD-NUM           TO CARD-NUM
           MOVE DCL-ACCT-ID            TO CARD-ACCT-ID
           MOVE DCL-CUST-ID            TO CARD-CUST-ID
           MOVE DCL-EMBOSSED-NAME      TO CARD-EMBOSSED-NAME
           MOVE DCL-PRODUCT-CD         TO CARD-PRODUCT-CD
           MOVE DCL-CARD-STATUS        TO CARD-STATUS
           MOVE DCL-EXPIRY-YYMM        TO CARD-EXPIRY-YYMM
           MOVE DCL-REISSUE-CNT        TO CARD-REISSUE-CNT
      *
           IF IND-PREV-CARD < ZERO
               MOVE SPACES             TO CARD-PREV-CARD-NUM
           ELSE
               MOVE DCL-PREV-CARD-NUM  TO CARD-PREV-CARD-NUM
           END-IF
      *
           IF IND-BLOCK-REASON < ZERO
               MOVE SPACES             TO CARD-BLOCK-REASON
           ELSE
               MOVE DCL-BLOCK-REASON   TO CARD-BLOCK-REASON
           END-IF
      *
           MOVE DCL-ACCT-ID            TO CA-ACCT-ID
           MOVE DCL-CUST-ID            TO CA-CUST-ID
           MOVE DCL-CARD-STATUS        TO CA-OLD-STATUS
           MOVE DCL-REISSUE-CNT        TO CA-REISSUE-CNT
           .
       2200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2300 - PAINT THE CARD DETAIL                                   *
      ******************************************************************
       2300-SHOW-CARD.
           MOVE CARD-NUM               TO M11CARDO
           MOVE CARD-STATUS            TO M11STATO
           MOVE CARD-ACCT-ID           TO M11ACCTO
           MOVE CARD-EMBOSSED-NAME     TO M11NAMEO
           MOVE CARD-PRODUCT-CD        TO M11PRODO
           MOVE CARD-EXPIRY-YYMM       TO M11EXPYO
           MOVE CARD-REISSUE-CNT       TO M11RCNTO
           MOVE CARD-PREV-CARD-NUM     TO M11PREVO
           MOVE CARD-BLOCK-REASON      TO M11BRSDO
           MOVE DCL-ISSUE-DATE         TO M11ISSDO
      *
           IF IND-BLOCK-DATE < ZERO
               MOVE SPACES             TO M11BDATO
           ELSE
               MOVE DCL-BLOCK-DATE     TO M11BDATO
           END-IF
      *
      *    THE ACTIVATION DATE ON THE OLDEST ROWS IS STILL A SIX
      *    DIGIT DATE ON THE EXTRACT SIDE - WINDOW IT FOR DISPLAY
           IF IND-ACTIVATION < ZERO
               MOVE 'NOT ACTIVATED'    TO M11ACTVO
           ELSE
               MOVE DCL-ACTIVATION-DATE(1:4)
                                       TO WS-EXPANDED-DATE(1:4)
               MOVE DCL-ACTIVATION-DATE
                                       TO M11ACTVO
           END-IF
      *
           MOVE CARD-STATUS            TO WS-STATUS-TEXT(1:1)
           PERFORM 8400-STATUS-TEXT THRU 8400-EXIT
           MOVE WS-STATUS-TEXT         TO M11STXTO
           .
       2300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2400 - THE ACCOUNT MUST BE OPEN FOR ANY MAINTENANCE            *
      ******************************************************************
       2400-CHECK-ACCOUNT.
           MOVE CA-ACCT-ID             TO DCL-ACCT-ID
      *
           EXEC SQL
               SELECT ACCT_STATUS
                    , PARTY_ID
                    , BRANCH_CD
                 INTO :DCL-ACCT-STATUS
                    , :DCL-PARTY-ID
                    , :DCL-BRANCH-CD :IND-BRANCH
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'ACCOUNT ROW MISSING FOR THIS CARD - REFER'
                                       TO M11MSGO
                   GO TO 2400-EXIT
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'ACCOUNT           '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR THRU 9100-EXIT
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 2400-EXIT
           END-EVALUATE
      *
           IF IND-BRANCH < ZERO
               MOVE SPACES             TO DCL-BRANCH-CD
           END-IF
      *
      *    UNBLOCK AND REISSUE ARE REFUSED ON A CLOSED OR WRITTEN OFF
      *    ACCOUNT.  A BLOCK IS ALWAYS ALLOWED.
           IF DCL-ACCT-STATUS NOT = 'O'
               IF CA-ACT-BLOCK
                   CONTINUE
               ELSE
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-ACCT-CLOSED
                                       TO M11MSGO
               END-IF
           END-IF
           .
       2400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - LEGAL STATUS TRANSITIONS                                *
      *                                                                *
      *        A ACTIVE   B BLOCKED  C CLOSED                          *
      *        L LOST     E EXPIRED  N NOT ACTIVATED                   *
      ******************************************************************
       3000-TEST-TRANSITION.
           MOVE 'N'                    TO WS-REISSUE-SW
           MOVE SPACES                 TO CA-NEW-STATUS
      *
           IF CA-ACT-BLOCK
               GO TO 3100-TEST-BLOCK
           END-IF
           IF CA-ACT-UNBLOCK
               GO TO 3200-TEST-UNBLOCK
           END-IF
           IF CA-ACT-REPLACE
               GO TO 3300-TEST-REPLACE
           END-IF
           GO TO 3400-TEST-REISSUE
           .
      *
       3100-TEST-BLOCK.
           EVALUATE CARD-STATUS
               WHEN 'A'
                   MOVE 'B'            TO CA-NEW-STATUS
                   MOVE 'BLOCK CARD'   TO WS-ACTION-TEXT
               WHEN 'N'
                   MOVE 'B'            TO CA-NEW-STATUS
                   MOVE 'BLOCK UNUSED CARD'
                                       TO WS-ACTION-TEXT
               WHEN 'B'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-ALREADY-BLK
                                       TO M11MSGO
               WHEN 'C'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-CLOSED-BLK
                                       TO M11MSGO
               WHEN 'L'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-LOST-BLK
                                       TO M11MSGO
               WHEN 'E'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-EXP-BLK TO M11MSGO
               WHEN OTHER
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'CARD STATUS ON FILE IS NOT A KNOWN VALUE'
                                       TO M11MSGO
           END-EVALUATE
           GO TO 3000-EXIT
           .
      *
       3200-TEST-UNBLOCK.
           EVALUATE CARD-STATUS
               WHEN 'B'
                   MOVE 'A'            TO CA-NEW-STATUS
                   MOVE 'UNBLOCK CARD' TO WS-ACTION-TEXT
               WHEN 'A'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NOT-BLOCKED
                                       TO M11MSGO
               WHEN 'L'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-LOST-UNBLK
                                       TO M11MSGO
               WHEN 'C'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-CLOSED-UNBLK
                                       TO M11MSGO
               WHEN 'E'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-EXP-UNBLK
                                       TO M11MSGO
               WHEN 'N'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NEW-UNBLK
                                       TO M11MSGO
               WHEN OTHER
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'CARD STATUS ON FILE IS NOT A KNOWN VALUE'
                                       TO M11MSGO
           END-EVALUATE
           GO TO 3000-EXIT
           .
      *
      *    REPLACE - THE OLD PLASTIC IS CLOSED AND A SUCCESSOR IS
      *    RAISED.  USED FOR LOST, STOLEN AND DAMAGED CARDS.
       3300-TEST-REPLACE.
           EVALUATE CARD-STATUS
               WHEN 'A'
                   MOVE 'C'            TO CA-NEW-STATUS
                   MOVE 'REPLACE CARD' TO WS-ACTION-TEXT
                   MOVE 'Y'            TO WS-REISSUE-SW
               WHEN 'B'
                   MOVE 'C'            TO CA-NEW-STATUS
                   MOVE 'REPLACE BLOCKED CRD'
                                       TO WS-ACTION-TEXT
                   MOVE 'Y'            TO WS-REISSUE-SW
               WHEN 'L'
                   MOVE 'C'            TO CA-NEW-STATUS
                   MOVE 'REPLACE LOST CARD'
                                       TO WS-ACTION-TEXT
                   MOVE 'Y'            TO WS-REISSUE-SW
               WHEN 'E'
                   MOVE 'C'            TO CA-NEW-STATUS
                   MOVE 'REPLACE EXPIRED CRD'
                                       TO WS-ACTION-TEXT
                   MOVE 'Y'            TO WS-REISSUE-SW
               WHEN 'C'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-CLOSED-REPL
                                       TO M11MSGO
               WHEN 'N'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NEW-REPL
                                       TO M11MSGO
               WHEN OTHER
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'CARD STATUS ON FILE IS NOT A KNOWN VALUE'
                                       TO M11MSGO
           END-EVALUATE
           GO TO 3000-EXIT
           .
      *
      *    REISSUE - SCHEDULED RENEWAL.  THE OLD PLASTIC EXPIRES
      *    NATURALLY SO IT IS LEFT ON E AND NOT CLOSED.
       3400-TEST-REISSUE.
           EVALUATE CARD-STATUS
               WHEN 'A'
                   MOVE 'E'            TO CA-NEW-STATUS
                   MOVE 'REISSUE ON RENEWAL'
                                       TO WS-ACTION-TEXT
                   MOVE 'Y'            TO WS-REISSUE-SW
               WHEN 'E'
                   MOVE 'E'            TO CA-NEW-STATUS
                   MOVE 'REISSUE EXPIRED CRD'
                                       TO WS-ACTION-TEXT
                   MOVE 'Y'            TO WS-REISSUE-SW
               WHEN 'B'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-BLK-REIS
                                       TO M11MSGO
               WHEN 'L'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-LOST-REIS
                                       TO M11MSGO
               WHEN 'C'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-CLOSED-REIS
                                       TO M11MSGO
               WHEN 'N'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NEW-REIS
                                       TO M11MSGO
               WHEN OTHER
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'CARD STATUS ON FILE IS NOT A KNOWN VALUE'
                                       TO M11MSGO
           END-EVALUATE
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - APPLY THE CHANGE                                        *
      *        EVERYTHING IN THIS RANGE IS IN THE SAME UNIT OF WORK.   *
      ******************************************************************
       4000-APPLY-CHANGE.
           MOVE 'N'                    TO WS-ERROR-SW
      *
           PERFORM 4100-UPDATE-CARD THRU 4100-EXIT
           IF WS-ERROR-FOUND
               GO TO 4000-BACKOUT
           END-IF
      *
           IF CA-NEW-CARD-NUM NOT = SPACES
               PERFORM 4300-INSERT-NEW-CARD THRU 4300-EXIT
               IF WS-ERROR-FOUND
                   GO TO 4000-BACKOUT
               END-IF
               PERFORM 4400-WRITE-NEW-XREF THRU 4400-EXIT
               IF WS-ERROR-FOUND
                   GO TO 4000-BACKOUT
               END-IF
           END-IF
      *
           PERFORM 4500-MARK-OLD-XREF THRU 4500-EXIT
           IF WS-ERROR-FOUND
               GO TO 4000-BACKOUT
           END-IF
      *
           EXEC CICS SYNCPOINT RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SYNCPOINT FAILED ON CARD MAINTENANCE'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           GO TO 4000-EXIT
           .
       4000-BACKOUT.
           EXEC CICS SYNCPOINT ROLLBACK RESP(WS-RESP) END-EXEC
           .
       4000-EXIT.
           EXIT
           .
      *
       4100-UPDATE-CARD.
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
           MOVE CA-NEW-STATUS          TO DCL-CARD-STATUS
           MOVE CA-BLOCK-REASON        TO DCL-BLOCK-REASON
      *
           IF CA-ACT-BLOCK
               EXEC SQL
                   UPDATE CARDSVC.CARD
                      SET CARD_STATUS    = :DCL-CARD-STATUS
                        , BLOCK_REASON   = :DCL-BLOCK-REASON
                        , BLOCK_DATE     = CURRENT DATE
                        , LAST_MAINT_PGM = 'CACRD11 '
                        , LAST_MAINT_TS  = CURRENT TIMESTAMP
                    WHERE CARD_NUM       = :DCL-CARD-NUM
               END-EXEC
           ELSE
               IF CA-ACT-UNBLOCK
                   EXEC SQL
                       UPDATE CARDSVC.CARD
                          SET CARD_STATUS    = :DCL-CARD-STATUS
                            , BLOCK_REASON   = NULL
                            , BLOCK_DATE     = NULL
                            , PIN_TRIES      = 0
                            , LAST_MAINT_PGM = 'CACRD11 '
                            , LAST_MAINT_TS  = CURRENT TIMESTAMP
                        WHERE CARD_NUM       = :DCL-CARD-NUM
                   END-EXEC
               ELSE
                   EXEC SQL
                       UPDATE CARDSVC.CARD
                          SET CARD_STATUS    = :DCL-CARD-STATUS
                            , LAST_MAINT_PGM = 'CACRD11 '
                            , LAST_MAINT_TS  = CURRENT TIMESTAMP
                        WHERE CARD_NUM       = :DCL-CARD-NUM
                   END-EXEC
               END-IF
           END-IF
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'CARD ROW DISAPPEARED DURING UPDATE - RETRY'
                                       TO M11BMSGO
               WHEN OTHER
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   MOVE 'CARD              '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR THRU 9100-EXIT
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
       4100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4200 - SUCCESSOR CARD NUMBER                                   *
      *                                                                *
      *        THE FIRST SIX DIGITS ARE THE PRODUCT BIN AND ARE KEPT.  *
      *        DIGITS 7 TO 15 ARE THE ACCOUNT NUMBER WITH THE REISSUE  *
      *        COUNT FOLDED IN.  DIGIT 16 IS THE MOD 10 CHECK DIGIT.   *
      ******************************************************************
       4200-BUILD-NEW-NUMBER.
           MOVE SPACES                 TO WS-NEW-CARD
           MOVE CA-CARD-NUM(1:6)       TO WS-NEW-CARD(1:6)
      *
           COMPUTE WS-SEQ-PART = FUNCTION MOD (CA-ACCT-ID, 100000000)
           ADD 1                       TO CA-REISSUE-CNT
           COMPUTE WS-SEQ-PART = WS-SEQ-PART
                               + (CA-REISSUE-CNT * 100000000)
           MOVE WS-SEQ-PART            TO WS-NEW-CARD(7:9)
           MOVE ZERO                   TO WS-NEW-CARD(16:1)
      *
           PERFORM 4250-CHECK-DIGIT THRU 4250-EXIT
           MOVE WS-CHECK-DIGIT         TO WS-NC-DIGIT(16)
           MOVE WS-NEW-CARD            TO CA-NEW-CARD-NUM
           .
       4200-EXIT.
           EXIT
           .
      *
      *    MOD 10 - DOUBLE EVERY SECOND DIGIT FROM THE RIGHT OF THE
      *    FIRST FIFTEEN AND CAST OUT NINES.
       4250-CHECK-DIGIT.
           MOVE ZERO                   TO WS-CHECK-SUM
           MOVE 1                      TO WS-SUB
           .
       4250-LOOP.
           IF WS-SUB > 15
               GO TO 4250-FINISH
           END-IF
           MOVE WS-NC-DIGIT(WS-SUB)    TO WS-DIGIT
           IF FUNCTION MOD (WS-SUB, 2) = 1
               COMPUTE WS-DOUBLED = WS-DIGIT * 2
               IF WS-DOUBLED > 9
                   SUBTRACT 9 FROM WS-DOUBLED
               END-IF
               ADD WS-DOUBLED          TO WS-CHECK-SUM
           ELSE
               ADD WS-DIGIT            TO WS-CHECK-SUM
           END-IF
           ADD 1                       TO WS-SUB
           GO TO 4250-LOOP
           .
       4250-FINISH.
           COMPUTE WS-CHECK-DIGIT =
               FUNCTION MOD ((10 - FUNCTION MOD (WS-CHECK-SUM, 10)), 10)
           .
       4250-EXIT.
           EXIT
           .
      *
       4300-INSERT-NEW-CARD.
           MOVE CA-NEW-CARD-NUM        TO DCL-CARD-NUM
           MOVE CA-CARD-NUM            TO DCL-PREV-CARD-NUM
           MOVE CA-ACCT-ID             TO DCL-ACCT-ID
           MOVE CA-CUST-ID             TO DCL-CUST-ID
           MOVE CA-REISSUE-CNT         TO DCL-REISSUE-CNT
      *
      *    THE NEW PLASTIC EXPIRES THREE YEARS OUT.  THE CARD PLANT
      *    EXTRACT CBCRD08J PICKS IT UP ON STATUS N.
           EXEC SQL
               INSERT INTO CARDSVC.CARD
                     (CARD_NUM
                    , ACCT_ID
                    , CUST_ID
                    , EMBOSSED_NAME
                    , PRODUCT_CD
                    , CARD_STATUS
                    , EXPIRY_YYMM
                    , ISSUE_DATE
                    , CVV_IND
                    , PIN_TRIES
                    , REISSUE_CNT
                    , PREV_CARD_NUM
                    , LAST_MAINT_PGM
                    , LAST_MAINT_TS)
               VALUES (:DCL-CARD-NUM
                    , :DCL-ACCT-ID
                    , :DCL-CUST-ID
                    , :DCL-EMBOSSED-NAME
                    , :DCL-PRODUCT-CD
                    , 'N'
                    , SUBSTR(CHAR(CURRENT DATE + 3 YEARS, ISO),3,2) ||
                      SUBSTR(CHAR(CURRENT DATE + 3 YEARS, ISO),6,2)
                    , CURRENT DATE
                    , 'Y'
                    , 0
                    , :DCL-REISSUE-CNT
                    , :DCL-PREV-CARD-NUM
                    , 'CACRD11 '
                    , CURRENT TIMESTAMP)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'GENERATED CARD NUMBER ALREADY EXISTS - REFER'
                                       TO M11BMSGO
               WHEN OTHER
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD              '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR THRU 9100-EXIT
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
       4300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4400 - THE NEW CROSS REFERENCE RECORD                          *
      ******************************************************************
       4400-WRITE-NEW-XREF.
           MOVE SPACES                 TO CARD-XREF-RECORD
           MOVE CA-NEW-CARD-NUM        TO XREF-CARD-NUM
           MOVE CA-ACCT-ID             TO XREF-ACCT-ID
           MOVE CA-CUST-ID             TO XREF-CUST-ID
           MOVE DCL-PARTY-ID           TO XREF-PARTY-ID
           MOVE 'N'                    TO XREF-CARD-STATUS
           MOVE CARD-PRODUCT-CD        TO XREF-PRODUCT-CD
           MOVE ZERO                   TO XREF-EXPIRY-YYMM
           MOVE WS-TODAY-YYYYMMDD      TO XREF-ISSUE-DATE
           MOVE CA-CARD-NUM            TO XREF-PREV-CARD-NUM
           MOVE 'N'                    TO XREF-SUPERSEDED-FLG
           MOVE ZERO                   TO XREF-SUPERSEDE-DT
           MOVE CA-REISSUE-CNT         TO XREF-REISSUE-CNT
           MOVE DCL-BRANCH-CD          TO XREF-BRANCH-CD
           MOVE WS-PGM-ID              TO XREF-LAST-MAINT-PGM
           MOVE WS-TODAY-YYYYMMDD      TO XREF-LAST-MAINT-DT
      *
           EXEC CICS WRITE
                     FILE('CARDXREF')
                     FROM(CARD-XREF-RECORD)
                     RIDFLD(XREF-CARD-NUM)
                     LENGTH(LENGTH OF CARD-XREF-RECORD)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(DUPREC)
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'CROSS REFERENCE ALREADY HOLDS THE NEW NUMBER'
                                       TO M11BMSGO
               WHEN OTHER
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE 'CARDXREF'     TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE XREF-CARD-NUM  TO ER-VSAM-KEY
                   MOVE 'WRITE FAILED ON CARD CROSS REFERENCE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
       4400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4500 - MARK THE OLD CROSS REFERENCE RECORD                     *
      ******************************************************************
       4500-MARK-OLD-XREF.
           MOVE 'N'                    TO WS-XREF-FOUND-SW
           MOVE CA-CARD-NUM            TO XREF-CARD-NUM
      *
           EXEC CICS READ
                     FILE('CARDXREF')
                     INTO(CARD-XREF-RECORD)
                     RIDFLD(XREF-CARD-NUM)
                     KEYLENGTH(16)
                     UPDATE
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 'Y'            TO WS-XREF-FOUND-SW
               WHEN DFHRESP(NOTFND)
      *            THE XREF IS REBUILT NIGHTLY - A MISSING RECORD IS
      *            NOT FATAL, THE DB2 ROW IS THE MASTER
                   GO TO 4500-EXIT
               WHEN DFHRESP(DUPKEY)
                   MOVE 'Y'            TO WS-XREF-FOUND-SW
               WHEN OTHER
                   MOVE 'VSAM'         TO ER-ERROR-TYPE
                   MOVE 'CARDXREF'     TO ER-FILE-NAME
                   MOVE WS-RESP        TO ER-VSAM-RC
                   MOVE XREF-CARD-NUM  TO ER-VSAM-KEY
                   MOVE 'READ FOR UPDATE FAILED ON CROSS REFERENCE'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 4500-EXIT
           END-EVALUATE
      *
           MOVE CA-NEW-STATUS          TO XREF-CARD-STATUS
           MOVE WS-PGM-ID              TO XREF-LAST-MAINT-PGM
           MOVE WS-TODAY-YYYYMMDD      TO XREF-LAST-MAINT-DT
      *
           IF CA-NEW-CARD-NUM NOT = SPACES
               MOVE 'Y'                TO XREF-SUPERSEDED-FLG
               MOVE WS-TODAY-YY        TO WS-SIX-YY
               MOVE WS-TODAY-MM        TO WS-SIX-MM
               MOVE WS-TODAY-DD        TO WS-SIX-DD
               MOVE WS-SIX-DIGIT       TO XREF-SUPERSEDE-DT
           END-IF
      *
           EXEC CICS REWRITE
                     FILE('CARDXREF')
                     FROM(CARD-XREF-RECORD)
                     LENGTH(LENGTH OF CARD-XREF-RECORD)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'VSAM'             TO ER-ERROR-TYPE
               MOVE 'CARDXREF'         TO ER-FILE-NAME
               MOVE WS-RESP            TO ER-VSAM-RC
               MOVE XREF-CARD-NUM      TO ER-VSAM-KEY
               MOVE 'REWRITE FAILED ON CARD CROSS REFERENCE'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           .
       4500-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 5000 - CONFIRMATION SCREEN PASS                                *
      ******************************************************************
       5000-CONFIRM-PASS.
           PERFORM 8100-GET-DATE THRU 8100-EXIT
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-CONF)
                     MAPSET(WS-MAPSET)
                     INTO(CRD11BI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE WS-MSG-CONF-BAD    TO M11BMSGO
               PERFORM 8300-SEND-CONFIRM THRU 8300-EXIT
               GO TO 5000-EXIT
           END-IF
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'RECEIVE MAP CRD11B FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
      *
           EVALUATE M11BCNFI
               WHEN 'Y'
                   PERFORM 4000-APPLY-CHANGE THRU 4000-EXIT
                   IF WS-ERROR-FOUND
                       PERFORM 8300-SEND-CONFIRM THRU 8300-EXIT
                   ELSE
                       PERFORM 5100-DONE-MESSAGE THRU 5100-EXIT
                       MOVE '1'        TO CA-STEP
                       PERFORM 8200-SEND-ENTRY THRU 8200-EXIT
                   END-IF
               WHEN 'N'
                   MOVE WS-MSG-ABANDONED
                                       TO M11MSGO
                   MOVE '1'            TO CA-STEP
                   PERFORM 8200-SEND-ENTRY THRU 8200-EXIT
               WHEN OTHER
                   MOVE WS-MSG-CONF-BAD
                                       TO M11BMSGO
                   PERFORM 8300-SEND-CONFIRM THRU 8300-EXIT
           END-EVALUATE
           .
       5000-EXIT.
           EXIT
           .
      *
       5100-DONE-MESSAGE.
           EVALUATE TRUE
               WHEN CA-ACT-BLOCK
                   MOVE WS-MSG-DONE-BLK
                                       TO M11MSGO
               WHEN CA-ACT-UNBLOCK
                   MOVE WS-MSG-DONE-UNBLK
                                       TO M11MSGO
               WHEN CA-ACT-REPLACE
                   MOVE WS-MSG-DONE-REPL
                                       TO M11MSGO
               WHEN OTHER
                   MOVE WS-MSG-DONE-REIS
                                       TO M11MSGO
           END-EVALUATE
      *
           MOVE CA-NEW-CARD-NUM        TO M11PREVO
           MOVE SPACES                 TO CA-NEW-CARD-NUM
           MOVE SPACES                 TO CA-ACTION
           MOVE SPACES                 TO CA-BLOCK-REASON
           .
       5100-EXIT.
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
                     DATESEP('-')
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE WS-TODAY-YYYYMMDD      TO WS-DISPLAY-DATE(1:8)
           .
       8100-EXIT.
           EXIT
           .
      *
       8200-SEND-ENTRY.
           MOVE WS-DISPLAY-DATE        TO M11DATEO
           EXEC CICS SEND
                     MAP(WS-MAP-ENTRY)
                     MAPSET(WS-MAPSET)
                     FROM(CRD11AO)
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
       8200-EXIT.
           EXIT
           .
      *
       8300-SEND-CONFIRM.
           MOVE WS-DISPLAY-DATE        TO M11BDTEO
           MOVE CA-CARD-NUM            TO M11BCRDO
           MOVE WS-ACTION-TEXT         TO M11BACTO
           MOVE CA-NEW-CARD-NUM        TO M11BNEWO
      *
           MOVE CA-OLD-STATUS          TO WS-STATUS-TEXT(1:1)
           PERFORM 8400-STATUS-TEXT THRU 8400-EXIT
           MOVE WS-STATUS-TEXT         TO M11BOSTO
      *
           MOVE CA-NEW-STATUS          TO WS-STATUS-TEXT(1:1)
           PERFORM 8400-STATUS-TEXT THRU 8400-EXIT
           MOVE WS-STATUS-TEXT         TO M11BNSTO
      *
           IF M11BMSGO = SPACES OR LOW-VALUES
               MOVE WS-MSG-CONFIRM     TO M11BMSGO
           END-IF
      *
           EXEC CICS SEND
                     MAP(WS-MAP-CONF)
                     MAPSET(WS-MAPSET)
                     FROM(CRD11BO)
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
       8300-EXIT.
           EXIT
           .
      *
       8400-STATUS-TEXT.
           EVALUATE WS-STATUS-TEXT(1:1)
               WHEN 'A'
                   MOVE 'ACTIVE'       TO WS-STATUS-TEXT
               WHEN 'B'
                   MOVE 'BLOCKED'      TO WS-STATUS-TEXT
               WHEN 'C'
                   MOVE 'CLOSED'       TO WS-STATUS-TEXT
               WHEN 'L'
                   MOVE 'LOST/STOLEN'  TO WS-STATUS-TEXT
               WHEN 'E'
                   MOVE 'EXPIRED'      TO WS-STATUS-TEXT
               WHEN 'N'
                   MOVE 'NOT ACTIVATED'
                                       TO WS-STATUS-TEXT
               WHEN OTHER
                   MOVE 'UNKNOWN'      TO WS-STATUS-TEXT
           END-EVALUATE
           .
       8400-EXIT.
           EXIT
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
       9000-EXIT.
           EXIT
           .
      *
       9100-SQL-ERROR.
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE 'SQL FAILURE IN CARD MAINTENANCE'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
           .
       9100-EXIT.
           EXIT
           .
      *
       9500-NO-COMMAREA.
           MOVE 'DATA'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE 'CACRD11 ENTERED WITH NO COMMAREA'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR THRU 9000-EXIT
           .
       9500-EXIT.
           EXIT
           .
      *
       9600-EXIT-SESSION.
           EXEC CICS SEND TEXT
                     FROM(WS-MSG-ABANDONED)
                     LENGTH(78)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           .
       9600-EXIT.
           EXIT
           .
