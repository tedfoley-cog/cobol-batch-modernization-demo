      ******************************************************************
      * CACRD12 - CREDIT LIMIT CHANGE REQUEST                          *
      *                                                                *
      * MENU OPTION 6.  CAPTURES A REQUESTED LIMIT AGAINST             *
      * CARDSVC.CARD_LIMIT AND APPLIES THE APPROVAL POLICY.            *
      *                                                                *
      * SMALL INCREASES INSIDE THE DELEGATED TOLERANCE ARE APPLIED     *
      * IMMEDIATELY BY EXPIRING THE CURRENT EFFECTIVE DATED ROW AND    *
      * INSERTING A NEW ONE.  ANYTHING LARGER NEEDS A RISK OPINION,    *
      * WHICH IS OBTAINED THROUGH THE DISPATCHER ON ROUTE XMOD/KYCINQ. *
      *                                                                *
      * CALLED BY  - CACRD90 BY XCTL, ROUTE MENU / OPT06               *
      * RETURNS TO - CACRD00 BY XCTL                                   *
      * CALLS      - CACRD90 BY LINK FOR THE RISK OPINION              *
      *              CACRD91 BY LINK FOR ERROR DISPLAY                 *
      * MAPSET     - CARDST2, MAP CRD12A                               *
      * TABLES     - CARDSVC.CARD          SELECT                      *
      *              CARDSVC.CARD_LIMIT    SELECT UPDATE INSERT        *
      *              CARDSVC.ACCOUNT       SELECT                      *
      *                                                                *
      * ORIGINAL - 1998.  THE RISK REFERRAL WAS ADDED IN 2003 WHEN     *
      * DELEGATED AUTHORITY WAS CAPPED BY CREDIT POLICY.               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD12.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD12 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDST2 '.
       01  WS-MAP-LIMIT                PIC X(8)  VALUE 'CRD12A  '.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CACRD00 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE 0.
      *
      *    DELEGATED AUTHORITY - CREDIT POLICY CP-114.  AN INCREASE
      *    IS AUTOMATIC IF IT IS INSIDE BOTH OF THESE.
       01  WS-POLICY-LIMITS.
           05  WS-TOLERANCE-PCT        PIC S9(3)V9(5) COMP-3
                                                 VALUE 20.00000.
           05  WS-TOLERANCE-MAX        PIC S9(9)V99 COMP-3
                                                 VALUE 5000.00.
           05  WS-ABSOLUTE-CEILING     PIC S9(11)V99 COMP-3
                                                 VALUE 250000.00.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-NEED-RISK-SW         PIC X     VALUE 'N'.
               88  WS-RISK-NEEDED                VALUE 'Y'.
           05  WS-APPLY-SW             PIC X     VALUE 'N'.
               88  WS-CAN-APPLY                  VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-DELTA-AMT            PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-TOLERANCE-AMT        PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-NEW-AVAIL            PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-EDIT-AMT             PIC ZZZ,ZZZ,ZZ9.99-.
           05  WS-NUMERIC-IN           PIC 9(11)V99 VALUE ZERO.
           05  WS-INPUT-AMT            PIC X(13) VALUE SPACES.
           05  WS-DISPLAY-DATE         PIC X(10) VALUE SPACES.
           05  WS-TODAY-YYYYMMDD       PIC 9(8)  VALUE ZERO.
      *
       01  WS-MESSAGES.
           05  WS-MSG-CARD-REQD        PIC X(78) VALUE
               'CARD NUMBER MUST BE ENTERED'.
           05  WS-MSG-NO-CARD          PIC X(78) VALUE
               'CARD NUMBER NOT ON FILE'.
           05  WS-MSG-NO-LIMIT         PIC X(78) VALUE
               'NO CURRENT LIMIT ROW FOR THIS CARD AND LIMIT TYPE'.
           05  WS-MSG-BAD-TYPE         PIC X(78) VALUE
               'LIMIT TYPE MUST BE CRED CASH DAIL OR FRGN'.
           05  WS-MSG-BAD-AMT          PIC X(78) VALUE
               'NEW LIMIT MUST BE NUMERIC AND GREATER THAN ZERO'.
           05  WS-MSG-SAME-AMT         PIC X(78) VALUE
               'NEW LIMIT IS THE SAME AS THE CURRENT LIMIT'.
           05  WS-MSG-CEILING          PIC X(78) VALUE
               'REQUESTED LIMIT EXCEEDS THE PRODUCT CEILING - REFER'.
           05  WS-MSG-BELOW-USED       PIC X(78) VALUE
               'NEW LIMIT IS BELOW THE AMOUNT ALREADY DRAWN'.
           05  WS-MSG-CARD-STATUS      PIC X(78) VALUE
               'LIMIT CHANGE ALLOWED ON ACTIVE CARDS ONLY'.
           05  WS-MSG-NEED-RISK        PIC X(78) VALUE
               'INCREASE EXCEEDS DELEGATED AUTHORITY - PRESS PF5'.
           05  WS-MSG-APPLIED          PIC X(78) VALUE
               'LIMIT CHANGE APPLIED WITH EFFECT FROM TODAY'.
           05  WS-MSG-RISK-OK          PIC X(78) VALUE
               'RISK OPINION OBTAINED - PRESS ENTER TO APPLY'.
           05  WS-MSG-RISK-BAND        PIC X(78) VALUE
               'RISK BAND DOES NOT SUPPORT AN INCREASE - REFUSED'.
           05  WS-MSG-RISK-KYC         PIC X(78) VALUE
               'KYC IS NOT SATISFACTORY - INCREASE REFUSED'.
           05  WS-MSG-RISK-SANC        PIC X(78) VALUE
               'PARTY HAS A SANCTION CONDITION - REFER TO FINANCIAL CR'.
           05  WS-MSG-RISK-WARN        PIC X(78) VALUE
               'RISK OPINION RETURNED A WARNING - REVIEW BEFORE APPLY'.
           05  WS-MSG-RISK-FAIL        PIC X(78) VALUE
               'RISK OPINION UNAVAILABLE - CHANGE CANNOT BE APPLIED'.
           05  WS-MSG-RISK-STALE       PIC X(78) VALUE
               'RISK OPINION IS FOR A DIFFERENT AMOUNT - REPEAT PF5'.
      *
       01  WS-DECISION-TEXT            PIC X(40) VALUE SPACES.
      *
      ******************************************************************
      * PROGRAM COMMAREA                                               *
      ******************************************************************
       01  WS-COMMAREA.
           05  CA-PGM-ID               PIC X(8).
           05  CA-FROM-PGM             PIC X(8).
           05  CA-CARD-NUM             PIC X(16).
           05  CA-LIMIT-TYPE           PIC X(4).
           05  CA-ACCT-ID              PIC 9(11).
           05  CA-CUST-ID              PIC 9(9).
           05  CA-PARTY-ID             PIC X(11).
           05  CA-CURRENT-LIMIT        PIC S9(11)V99 COMP-3.
           05  CA-REQUESTED-LIMIT      PIC S9(11)V99 COMP-3.
           05  CA-USED-AMT             PIC S9(11)V99 COMP-3.
           05  CA-REASON-CD            PIC X(4).
           05  CA-RISK-HELD-FLG        PIC X.
               88  CA-RISK-HELD                VALUE 'Y'.
           05  CA-RISK-AMT             PIC S9(11)V99 COMP-3.
           05  CA-RISK-BAND            PIC X.
           05  CA-RISK-SCORE           PIC 9(3).
           05  CA-RISK-KYC             PIC X(2).
           05  CA-RISK-SANCTION        PIC X.
           05  CA-RISK-EXPOSURE        PIC S9(11)V99 COMP-3.
           05  CA-RISK-ADVICE          PIC X(4).
           05  CA-RISK-RC              PIC 9(4).
           05  CA-OPER-ID              PIC X(8).
           05  CA-FILLER               PIC X(376).
      *
           COPY CVRISK01Y.
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      *    BMS SYMBOLIC MAPS FOR MAPSET CARDST2
           COPY CARDST2.
      *
      ******************************************************************
      * THE DISPATCHER TAKES THE ROUTE REQUEST FOLLOWED BY THE 512     *
      * BYTE CALLER COMMAREA.  FOR A CROSS MODULE CALL THAT COMMAREA   *
      * IS THE RISK AREA ITSELF.                                       *
      ******************************************************************
       01  WS-DISPATCH-AREA.
           05  WS-DISP-REQUEST         PIC X(38).
           05  WS-DISP-COMMAREA        PIC X(512).
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-CARD-NUM                PIC X(16).
       01  DCL-CARD-STATUS             PIC X(1).
       01  DCL-ACCT-ID                 PIC S9(11) COMP-3.
       01  DCL-CUST-ID                 PIC S9(9)  COMP-3.
       01  DCL-PARTY-ID                PIC X(11).
       01  DCL-PRODUCT-CD              PIC X(4).
      *
       01  DCL-LIMIT.
           05  DCL-LIMIT-TYPE          PIC X(4).
           05  DCL-LIMIT-AMT           PIC S9(13)V99 COMP-3.
           05  DCL-USED-AMT            PIC S9(13)V99 COMP-3.
           05  DCL-AVAIL-AMT           PIC S9(13)V99 COMP-3.
           05  DCL-APR-PCT             PIC S9(3)V9(5) COMP-3.
           05  DCL-CASH-APR-PCT        PIC S9(3)V9(5) COMP-3.
           05  DCL-DAILY-CNT-LIMIT     PIC S9(4) COMP.
           05  DCL-VELOCITY-WINDOW     PIC S9(4) COMP.
           05  DCL-VELOCITY-MAX-CNT    PIC S9(4) COMP.
           05  DCL-RISK-BAND           PIC X(1).
           05  DCL-EFF-DATE            PIC X(10).
      *
       01  DCL-NEW-LIMIT-AMT           PIC S9(13)V99 COMP-3.
       01  DCL-NEW-AVAIL-AMT           PIC S9(13)V99 COMP-3.
      *
       01  IND-RISK-BAND               PIC S9(4) COMP.
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
           MOVE LOW-VALUES             TO CRD12AO
      *
           IF EIBCALEN = ZERO
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE 'CACRD12 ENTERED WITH NO COMMAREA'
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
           PERFORM 2000-PROCESS-SCREEN
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
      ******************************************************************
      * 1000 - FIRST ENTRY                                             *
      ******************************************************************
       1000-FIRST-TIME.
           MOVE SPACES                 TO WS-COMMAREA
           MOVE WS-PGM-ID              TO CA-PGM-ID
           MOVE 'CRED'                 TO CA-LIMIT-TYPE
           MOVE EIBTRMID               TO CA-OPER-ID
           MOVE ZERO                   TO CA-ACCT-ID
           MOVE ZERO                   TO CA-CUST-ID
           MOVE ZERO                   TO CA-CURRENT-LIMIT
           MOVE ZERO                   TO CA-REQUESTED-LIMIT
           MOVE ZERO                   TO CA-USED-AMT
           MOVE ZERO                   TO CA-RISK-AMT
           MOVE ZERO                   TO CA-RISK-EXPOSURE
           MOVE ZERO                   TO CA-RISK-SCORE
           MOVE ZERO                   TO CA-RISK-RC
           MOVE 'N'                    TO CA-RISK-HELD-FLG
           MOVE 'CRED'                 TO M12LTYPO
           PERFORM 8200-SEND-MAP
           .
      *
      ******************************************************************
      * 2000 - SCREEN PASS                                             *
      ******************************************************************
       2000-PROCESS-SCREEN.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-APPLY-SW
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-LIMIT)
                     MAPSET(WS-MAPSET)
                     INTO(CRD12AI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE 'NO DATA ENTERED - KEY THE CARD NUMBER'
                                       TO M12MSGO
                   PERFORM 8200-SEND-MAP
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE 'RECEIVE MAP CRD12A FAILED'
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
           PERFORM 2200-READ-CARD
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2300-READ-LIMIT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2400-PAINT-CURRENT
      *
      *    A BARE ENQUIRY - NO NEW LIMIT KEYED
           IF CA-REQUESTED-LIMIT = ZERO
               MOVE 'CURRENT POSITION DISPLAYED - KEY A NEW LIMIT'
                                       TO M12MSGO
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 3000-CHECK-POLICY
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           IF EIBAID = DFHPF5
               PERFORM 5000-GET-RISK-OPINION
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           IF WS-RISK-NEEDED
               PERFORM 4000-TEST-RISK-HELD
           ELSE
               MOVE 'Y'                TO WS-APPLY-SW
               MOVE 'WITHIN DELEGATED AUTHORITY'
                                       TO WS-DECISION-TEXT
           END-IF
      *
           IF WS-CAN-APPLY
               PERFORM 6000-APPLY-LIMIT
               IF NOT WS-ERROR-FOUND
                   MOVE WS-MSG-APPLIED TO M12MSGO
                   MOVE CA-REQUESTED-LIMIT
                                       TO CA-CURRENT-LIMIT
                   MOVE ZERO           TO CA-REQUESTED-LIMIT
                   MOVE 'N'            TO CA-RISK-HELD-FLG
                   PERFORM 2400-PAINT-CURRENT
               END-IF
           END-IF
      *
           MOVE WS-DECISION-TEXT       TO M12DECNO
           PERFORM 8200-SEND-MAP
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-EDIT-INPUT.
           IF M12CARDL = ZERO OR M12CARDI = SPACES
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-CARD-REQD   TO M12MSGO
               GO TO 2100-EXIT
           END-IF
      *
      *    A CHANGE OF CARD INVALIDATES ANY RISK OPINION WE HOLD
           IF M12CARDI NOT = CA-CARD-NUM
               MOVE 'N'                TO CA-RISK-HELD-FLG
               MOVE ZERO               TO CA-RISK-AMT
           END-IF
           MOVE M12CARDI               TO CA-CARD-NUM
      *
           IF M12LTYPL = ZERO OR M12LTYPI = SPACES
               MOVE 'CRED'             TO CA-LIMIT-TYPE
           ELSE
               MOVE M12LTYPI           TO CA-LIMIT-TYPE
           END-IF
      *
           IF CA-LIMIT-TYPE NOT = 'CRED'
              AND CA-LIMIT-TYPE NOT = 'CASH'
              AND CA-LIMIT-TYPE NOT = 'DAIL'
              AND CA-LIMIT-TYPE NOT = 'FRGN'
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-BAD-TYPE    TO M12MSGO
               GO TO 2100-EXIT
           END-IF
      *
           MOVE M12RSNI                TO CA-REASON-CD
      *
           MOVE ZERO                   TO CA-REQUESTED-LIMIT
           IF M12NLIML > ZERO AND M12NLIMI NOT = SPACES
               MOVE M12NLIMI           TO WS-INPUT-AMT
               PERFORM 8500-CONVERT-AMOUNT
               IF WS-ERROR-FOUND
                   MOVE WS-MSG-BAD-AMT TO M12MSGO
                   GO TO 2100-EXIT
               END-IF
               MOVE WS-NUMERIC-IN      TO CA-REQUESTED-LIMIT
           END-IF
           .
       2100-EXIT.
           EXIT
           .
      *
       2200-READ-CARD.
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT C.CARD_STATUS
                    , C.ACCT_ID
                    , C.CUST_ID
                    , C.PRODUCT_CD
                    , A.PARTY_ID
                 INTO :DCL-CARD-STATUS
                    , :DCL-ACCT-ID
                    , :DCL-CUST-ID
                    , :DCL-PRODUCT-CD
                    , :DCL-PARTY-ID
                 FROM CARDSVC.CARD C
                    , CARDSVC.ACCOUNT A
                WHERE C.CARD_NUM = :DCL-CARD-NUM
                  AND A.ACCT_ID  = C.ACCT_ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NO-CARD TO M12MSGO
                   GO TO 2200-EXIT
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD              '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 2200-EXIT
           END-EVALUATE
      *
           MOVE DCL-ACCT-ID            TO CA-ACCT-ID
           MOVE DCL-CUST-ID            TO CA-CUST-ID
           MOVE DCL-PARTY-ID           TO CA-PARTY-ID
      *
           IF DCL-CARD-STATUS NOT = 'A'
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-CARD-STATUS TO M12MSGO
           END-IF
           .
       2200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2300 - THE CURRENT EFFECTIVE DATED LIMIT ROW                   *
      ******************************************************************
       2300-READ-LIMIT.
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
           MOVE CA-LIMIT-TYPE          TO DCL-LIMIT-TYPE
      *
           EXEC SQL
               SELECT LIMIT_AMT
                    , USED_AMT
                    , AVAIL_AMT
                    , APR_PCT
                    , CASH_APR_PCT
                    , DAILY_CNT_LIMIT
                    , VELOCITY_WINDOW_MIN
                    , VELOCITY_MAX_CNT
                    , RISK_BAND
                    , CHAR(EFF_DATE, ISO)
                 INTO :DCL-LIMIT-AMT
                    , :DCL-USED-AMT
                    , :DCL-AVAIL-AMT
                    , :DCL-APR-PCT
                    , :DCL-CASH-APR-PCT
                    , :DCL-DAILY-CNT-LIMIT
                    , :DCL-VELOCITY-WINDOW
                    , :DCL-VELOCITY-MAX-CNT
                    , :DCL-RISK-BAND :IND-RISK-BAND
                    , :DCL-EFF-DATE
                 FROM CARDSVC.CARD_LIMIT
                WHERE CARD_NUM   = :DCL-CARD-NUM
                  AND LIMIT_TYPE = :DCL-LIMIT-TYPE
                  AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NO-LIMIT
                                       TO M12MSGO
                   GO TO 2300-EXIT
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD_LIMIT        '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 2300-EXIT
           END-EVALUATE
      *
           IF IND-RISK-BAND < ZERO
               MOVE SPACE              TO DCL-RISK-BAND
           END-IF
      *
           MOVE DCL-LIMIT-AMT          TO CA-CURRENT-LIMIT
           MOVE DCL-USED-AMT           TO CA-USED-AMT
           .
       2300-EXIT.
           EXIT
           .
      *
       2400-PAINT-CURRENT.
           MOVE CA-CARD-NUM            TO M12CARDO
           MOVE CA-LIMIT-TYPE          TO M12LTYPO
           MOVE CA-CURRENT-LIMIT       TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M12CLIMO
           MOVE DCL-USED-AMT           TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M12USEDO
           MOVE DCL-AVAIL-AMT          TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M12AVALO
           MOVE DCL-EFF-DATE           TO M12EFFDO
      *
           IF CA-RISK-HELD
               MOVE CA-RISK-BAND       TO M12BANDO
               MOVE CA-RISK-SCORE      TO M12SCORO
               MOVE CA-RISK-KYC        TO M12KYCO
               MOVE CA-RISK-ADVICE     TO M12ADVCO
               MOVE CA-RISK-EXPOSURE   TO WS-EDIT-AMT
               MOVE WS-EDIT-AMT        TO M12EXPOO
           END-IF
           .
      *
      ******************************************************************
      * 3000 - APPROVAL POLICY                                         *
      ******************************************************************
       3000-CHECK-POLICY.
           MOVE 'N'                    TO WS-NEED-RISK-SW
           MOVE SPACES                 TO WS-DECISION-TEXT
      *
           IF CA-REQUESTED-LIMIT = CA-CURRENT-LIMIT
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-SAME-AMT    TO M12MSGO
               GO TO 3000-EXIT
           END-IF
      *
           IF CA-REQUESTED-LIMIT > WS-ABSOLUTE-CEILING
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-CEILING     TO M12MSGO
               GO TO 3000-EXIT
           END-IF
      *
           IF CA-REQUESTED-LIMIT < CA-USED-AMT
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-BELOW-USED  TO M12MSGO
               GO TO 3000-EXIT
           END-IF
      *
           COMPUTE WS-DELTA-AMT = CA-REQUESTED-LIMIT
                                - CA-CURRENT-LIMIT
      *
      *    A DECREASE IS ALWAYS INSIDE AUTHORITY
           IF WS-DELTA-AMT < ZERO
               MOVE 'DECREASE - NO RISK OPINION NEEDED'
                                       TO WS-DECISION-TEXT
               GO TO 3000-EXIT
           END-IF
      *
           COMPUTE WS-TOLERANCE-AMT ROUNDED =
               (CA-CURRENT-LIMIT * WS-TOLERANCE-PCT) / 100
      *
           IF WS-TOLERANCE-AMT > WS-TOLERANCE-MAX
               MOVE WS-TOLERANCE-MAX   TO WS-TOLERANCE-AMT
           END-IF
      *
           IF WS-DELTA-AMT > WS-TOLERANCE-AMT
               MOVE 'Y'                TO WS-NEED-RISK-SW
               MOVE 'RISK OPINION REQUIRED'
                                       TO WS-DECISION-TEXT
           ELSE
               MOVE 'WITHIN DELEGATED AUTHORITY'
                                       TO WS-DECISION-TEXT
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - DO WE ALREADY HOLD A USABLE RISK OPINION                *
      ******************************************************************
       4000-TEST-RISK-HELD.
           IF NOT CA-RISK-HELD
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-NEED-RISK   TO M12MSGO
               GO TO 4000-EXIT
           END-IF
      *
           IF CA-RISK-AMT NOT = CA-REQUESTED-LIMIT
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-RISK-STALE  TO M12MSGO
               MOVE 'N'                TO CA-RISK-HELD-FLG
               GO TO 4000-EXIT
           END-IF
      *
           PERFORM 4100-EVALUATE-OPINION
           .
       4000-EXIT.
           EXIT
           .
      *
       4100-EVALUATE-OPINION.
           EVALUATE TRUE
               WHEN CA-RISK-SANCTION = 'Y'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-RISK-SANC
                                       TO M12MSGO
                   MOVE 'REFUSED - SANCTION CONDITION'
                                       TO WS-DECISION-TEXT
               WHEN CA-RISK-BAND = 'C'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-RISK-BAND
                                       TO M12MSGO
                   MOVE 'REFUSED - RISK BAND C'
                                       TO WS-DECISION-TEXT
               WHEN CA-RISK-BAND = 'X'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-RISK-BAND
                                       TO M12MSGO
                   MOVE 'REFUSED - RISK BAND X'
                                       TO WS-DECISION-TEXT
               WHEN CA-RISK-KYC NOT = 'OK'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-RISK-KYC
                                       TO M12MSGO
                   MOVE 'REFUSED - KYC NOT SATISFACTORY'
                                       TO WS-DECISION-TEXT
               WHEN OTHER
                   MOVE 'Y'            TO WS-APPLY-SW
                   MOVE 'APPROVED ON RISK OPINION'
                                       TO WS-DECISION-TEXT
           END-EVALUATE
           .
      *
      ******************************************************************
      * 5000 - RISK OPINION THROUGH THE DISPATCHER                     *
      *                                                                *
      *        ROUTE XMOD / KYCINQ.  THE DISPATCHER RESOLVES AND       *
      *        LINKS THE TARGET - THIS PROGRAM DOES NOT KNOW IT.       *
      ******************************************************************
       5000-GET-RISK-OPINION.
           MOVE 'N'                    TO CA-RISK-HELD-FLG
           MOVE LOW-VALUES             TO CV-RISK-AREA
      *
           MOVE 0003                   TO CV-RISK-VERSION
           MOVE WS-PGM-ID              TO CV-RISK-CALLER-ID
           MOVE WS-MODULE-CARDSVC      TO CV-RISK-CALLER-MOD
           MOVE SPACES                 TO CV-RISK-CORREL-ID
           MOVE EIBTRNID               TO CV-RISK-CORREL-ID(1:4)
           MOVE EIBTASKN               TO CV-RISK-CORREL-ID(5:7)
           MOVE WS-TODAY-YYYYMMDD      TO CV-RISK-REQ-DATE
           MOVE ZERO                   TO CV-RISK-REQ-TIME
           MOVE 'O'                    TO CV-RISK-CHANNEL
      *
           MOVE CA-PARTY-ID            TO CV-RISK-PARTY-ID
           MOVE CA-CUST-ID             TO CV-RISK-CUST-ID
           MOVE CA-ACCT-ID             TO CV-RISK-ACCT-ID
           MOVE CA-CARD-NUM            TO CV-RISK-CARD-NUM
           MOVE CA-REQUESTED-LIMIT     TO CV-RISK-REQ-AMT
           MOVE WS-CURRENCY-USD        TO CV-RISK-REQ-CURR
           MOVE ZERO                   TO CV-RISK-MCC
           MOVE SPACES                 TO CV-RISK-MERCH-ID
           MOVE WS-COUNTRY-USA         TO CV-RISK-COUNTRY
           MOVE 'INQY'                 TO CV-RISK-REQ-TYPE
      *
           MOVE ZERO                   TO CV-RISK-SCORE
           MOVE ZERO                   TO CV-RISK-RC
           MOVE ZERO                   TO CV-RISK-HOP-CNT
      *
           ADD 1                       TO CV-RISK-HOP-CNT
           MOVE WS-PGM-ID              TO
                                CV-RISK-HOP-PGM(CV-RISK-HOP-CNT)
           MOVE CV-RISK-RC             TO
                                CV-RISK-HOP-RC(CV-RISK-HOP-CNT)
      *
           MOVE SPACES                 TO ROUTE-REQUEST
           MOVE 'XMOD'                 TO RQ-ROUTE-TYPE
           MOVE WS-ROUTE-KYCINQ        TO RQ-ROUTE-KEY
           MOVE 1                      TO RQ-SEQ-NBR
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
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ROUT'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'LINK TO ONLINE DISPATCHER FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               MOVE WS-MSG-RISK-FAIL   TO M12MSGO
               GO TO 5000-EXIT
           END-IF
      *
           MOVE WS-DISP-REQUEST        TO ROUTE-REQUEST
           MOVE WS-DISP-COMMAREA       TO CV-RISK-AREA
      *
           IF NOT RQ-RC-OK
               MOVE 'ROUT'             TO ER-ERROR-TYPE
               MOVE RQ-ROUTE-KEY       TO ER-REASON-CD
               MOVE 'RISK ROUTE COULD NOT BE RESOLVED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               MOVE WS-MSG-RISK-FAIL   TO M12MSGO
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5100-STORE-OPINION
           .
       5000-EXIT.
           EXIT
           .
      *
       5100-STORE-OPINION.
           MOVE CV-RISK-RC             TO CA-RISK-RC
           MOVE CV-RISK-BAND           TO CA-RISK-BAND
           MOVE CV-RISK-SCORE          TO CA-RISK-SCORE
           MOVE CV-RISK-KYC-STATUS     TO CA-RISK-KYC
           MOVE CV-RISK-SANCTION-FLG   TO CA-RISK-SANCTION
           MOVE CV-RISK-EXPOSURE-AMT   TO CA-RISK-EXPOSURE
           MOVE CV-RISK-ADVICE-CD      TO CA-RISK-ADVICE
           MOVE CA-REQUESTED-LIMIT     TO CA-RISK-AMT
      *
           EVALUATE CV-RISK-RC
               WHEN 0000
                   MOVE 'Y'            TO CA-RISK-HELD-FLG
                   MOVE WS-MSG-RISK-OK TO M12MSGO
               WHEN 0004
                   MOVE 'Y'            TO CA-RISK-HELD-FLG
                   MOVE WS-MSG-RISK-WARN
                                       TO M12MSGO
               WHEN 0008
                   MOVE 'Y'            TO CA-RISK-HELD-FLG
                   MOVE CV-RISK-REASON-TXT
                                       TO M12MSGO
               WHEN OTHER
                   MOVE 'N'            TO CA-RISK-HELD-FLG
                   MOVE 'BUSN'         TO ER-ERROR-TYPE
                   MOVE CV-RISK-REASON-CD
                                       TO ER-REASON-CD
                   MOVE CV-RISK-REASON-TXT
                                       TO ER-MESSAGE(1:60)
                   PERFORM 9000-REPORT-ERROR
                   MOVE WS-MSG-RISK-FAIL
                                       TO M12MSGO
           END-EVALUATE
      *
           PERFORM 2400-PAINT-CURRENT
           .
      *
      ******************************************************************
      * 6000 - APPLY THE CHANGE                                        *
      *        EXPIRE THE CURRENT ROW AND INSERT THE SUCCESSOR.        *
      ******************************************************************
       6000-APPLY-LIMIT.
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
           MOVE CA-LIMIT-TYPE          TO DCL-LIMIT-TYPE
           MOVE CA-REQUESTED-LIMIT     TO DCL-NEW-LIMIT-AMT
      *
           COMPUTE WS-NEW-AVAIL = CA-REQUESTED-LIMIT - CA-USED-AMT
           MOVE WS-NEW-AVAIL           TO DCL-NEW-AVAIL-AMT
      *
           EXEC SQL
               UPDATE CARDSVC.CARD_LIMIT
                  SET EXP_DATE      = CURRENT DATE - 1 DAY
                    , LAST_MAINT_TS = CURRENT TIMESTAMP
                WHERE CARD_NUM      = :DCL-CARD-NUM
                  AND LIMIT_TYPE    = :DCL-LIMIT-TYPE
                  AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'UPDATE  '         TO ER-SQL-OPERATION
               MOVE 'CARD_LIMIT        '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 6000-BACKOUT
           END-IF
      *
           EXEC SQL
               INSERT INTO CARDSVC.CARD_LIMIT
                     (CARD_NUM
                    , LIMIT_TYPE
                    , LIMIT_AMT
                    , USED_AMT
                    , AVAIL_AMT
                    , DAILY_CNT_LIMIT
                    , DAILY_CNT_USED
                    , VELOCITY_WINDOW_MIN
                    , VELOCITY_MAX_CNT
                    , APR_PCT
                    , CASH_APR_PCT
                    , RISK_BAND
                    , LAST_REVIEW_DATE
                    , EFF_DATE
                    , EXP_DATE
                    , LAST_MAINT_TS)
               VALUES (:DCL-CARD-NUM
                    , :DCL-LIMIT-TYPE
                    , :DCL-NEW-LIMIT-AMT
                    , :DCL-USED-AMT
                    , :DCL-NEW-AVAIL-AMT
                    , :DCL-DAILY-CNT-LIMIT
                    , 0
                    , :DCL-VELOCITY-WINDOW
                    , :DCL-VELOCITY-MAX-CNT
                    , :DCL-APR-PCT
                    , :DCL-CASH-APR-PCT
                    , :DCL-RISK-BAND
                    , CURRENT DATE
                    , CURRENT DATE
                    , '9999-12-31'
                    , CURRENT TIMESTAMP)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'A LIMIT ROW ALREADY EXISTS FOR TODAY'
                                       TO M12MSGO
                   GO TO 6000-BACKOUT
               WHEN OTHER
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD_LIMIT        '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 6000-BACKOUT
           END-EVALUATE
      *
           EXEC CICS SYNCPOINT RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SYNCPOINT FAILED ON LIMIT CHANGE'
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
       8200-SEND-MAP.
           MOVE WS-DISPLAY-DATE        TO M12DATEO
      *
           EXEC CICS SEND
                     MAP(WS-MAP-LIMIT)
                     MAPSET(WS-MAPSET)
                     FROM(CRD12AO)
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
      ******************************************************************
      * 8500 - SCREEN AMOUNT TO PACKED                                 *
      *        THE OPERATOR MAY KEY A DECIMAL POINT OR NOT.            *
      ******************************************************************
       8500-CONVERT-AMOUNT.
           MOVE ZERO                   TO WS-NUMERIC-IN
           MOVE 'N'                    TO WS-ERROR-SW
      *
           INSPECT WS-INPUT-AMT REPLACING ALL ',' BY ' '
      *
           IF WS-INPUT-AMT(1:11) IS NOT NUMERIC
               IF WS-INPUT-AMT IS NOT NUMERIC
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 8500-EXIT
               END-IF
           END-IF
      *
           COMPUTE WS-NUMERIC-IN = FUNCTION NUMVAL (WS-INPUT-AMT)
      *
           IF WS-NUMERIC-IN NOT > ZERO
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           .
       8500-EXIT.
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
      *
       9100-SQL-ERROR.
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE 'SQL FAILURE IN LIMIT MAINTENANCE'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
       9600-EXIT-SESSION.
           EXEC CICS SEND TEXT
                     FROM(WS-MSG-APPLIED)
                     LENGTH(78)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           .
