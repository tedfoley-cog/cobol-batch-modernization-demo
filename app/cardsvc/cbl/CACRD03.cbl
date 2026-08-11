      ******************************************************************
      * CACRD03 - CARD DETAIL AND STATUS                               *
      *                                                                *
      * MENU OPTION 3.  READS CARDSVC.CARD FOR THE PLASTIC AND JOINS   *
      * CARDSVC.CARD_LIMIT FOR THE ROWS EFFECTIVE TODAY, ONE PER       *
      * LIMIT TYPE.  DISPLAYS STATUS, EXPIRY, BLOCK REASON, LIMITS     *
      * AND AVAILABLE AMOUNTS.                                         *
      *                                                                *
      * THE CARD ACTIVATION DATE IS A LEGACY 6 DIGIT YYMMDD ON THE     *
      * MASTER RECORD - IT IS WINDOWED AGAINST WS-CENTURY-PIVOT.       *
      *                                                                *
      * CALLED BY   - CACRD90 XCTL, ROUTE MENU/OPT03                   *
      * CALLS       - CACRD00 ON PF3, CACRD90 ON PF12                  *
      *             - CACRD91  ERROR HANDLER                           *
      * TABLES      - CARDSVC.CARD, CARDSVC.CARD_LIMIT  (SELECT)       *
      * MAPSET      - CARDSET   MAP CARDDTL                            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD03.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD03 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDSET '.
       01  WS-MAP                      PIC X(8)  VALUE 'CARDDTL '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-FOUND-SW                 PIC X     VALUE 'N'.
           88  WS-FOUND                          VALUE 'Y'.
       01  WS-ERASE-SW                 PIC X     VALUE 'Y'.
           88  WS-ERASE                          VALUE 'Y'.
       01  WS-MORE-SW                  PIC X     VALUE 'Y'.
           88  WS-MORE-ROWS                      VALUE 'Y'.
      *
       01  WS-SUB                      PIC S9(4) COMP VALUE 0.
       01  WS-LIMIT-CNT                PIC S9(4) COMP VALUE 0.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-OUT                 PIC X(8)  VALUE SPACES.
      *
       01  WS-EDIT-AMT                 PIC ---,---,---,--9.99.
       01  WS-EDIT-RATE                PIC ---9.99999.
       01  WS-STATUS-DESC              PIC X(14) VALUE SPACES.
      *
      *    ACTIVATION DATE DECOMPOSITION - 6 DIGIT LEGACY VALUE
       01  WS-LEGACY-DATE              PIC 9(6)  VALUE ZERO.
       01  WS-LEGACY-PARTS REDEFINES WS-LEGACY-DATE.
           05  WS-LEG-YY               PIC 9(2).
           05  WS-LEG-MM               PIC 9(2).
           05  WS-LEG-DD               PIC 9(2).
       01  WS-FULL-DATE                PIC X(10) VALUE SPACES.
      *
       01  WS-MSG-NOT-FOUND            PIC X(60) VALUE
           'CARD NOT FOUND ON THE CARD MASTER'.
       01  WS-MSG-KEY-REQD             PIC X(60) VALUE
           'ENTER A CARD NUMBER AND PRESS ENTER'.
       01  WS-MSG-OK                   PIC X(60) VALUE
           'CARD DISPLAYED'.
       01  WS-MSG-NO-LIMIT             PIC X(60) VALUE
           'CARD DISPLAYED - NO EFFECTIVE LIMIT ROWS FOUND'.
      *
       01  WS-DISPATCH-AREA.
           05  WS-DA-ROUTE             PIC X(38).
           05  WS-DA-COMMAREA          PIC X(512).
      *
           COPY CVAUTHW1Y.
           COPY CVCARD01Y.
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
           COPY CARDSET.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-CARD.
           05  DCL-CARD-NUM            PIC X(16).
           05  DCL-ACCT-ID             PIC S9(11)   COMP-3.
           05  DCL-CUST-ID             PIC S9(9)    COMP-3.
           05  DCL-EMBOSSED-NAME       PIC X(26).
           05  DCL-PRODUCT-CD          PIC X(4).
           05  DCL-CARD-STATUS         PIC X(1).
           05  DCL-EXPIRY-YYMM         PIC X(4).
           05  DCL-ISSUE-DATE          PIC X(10).
           05  DCL-ACTIVATION-DT       PIC X(10).
           05  DCL-LAST-USED-DT        PIC X(10).
           05  DCL-PIN-TRIES           PIC S9(4)    COMP.
           05  DCL-BLOCK-REASON        PIC X(4).
           05  DCL-BLOCK-DATE          PIC X(10).
      *
       01  DCL-LIMIT.
           05  DCL-LIMIT-TYPE          PIC X(4).
           05  DCL-LIMIT-AMT           PIC S9(11)V99 COMP-3.
           05  DCL-USED-AMT            PIC S9(11)V99 COMP-3.
           05  DCL-AVAIL-AMT           PIC S9(11)V99 COMP-3.
           05  DCL-APR-PCT             PIC S9(3)V9(5) COMP-3.
           05  DCL-CASH-APR-PCT        PIC S9(3)V9(5) COMP-3.
           05  DCL-RISK-BAND           PIC X(1).
      *
       01  IND-CARD.
           05  IND-ACTIVATION-DT       PIC S9(4) COMP.
           05  IND-LAST-USED-DT        PIC S9(4) COMP.
           05  IND-BLOCK-REASON        PIC S9(4) COMP.
           05  IND-BLOCK-DATE          PIC S9(4) COMP.
       01  IND-RISK-BAND               PIC S9(4) COMP.
      *
           EXEC SQL DECLARE LIMCSR CURSOR FOR
               SELECT LIMIT_TYPE
                    , LIMIT_AMT
                    , USED_AMT
                    , AVAIL_AMT
                    , APR_PCT
                    , CASH_APR_PCT
                    , RISK_BAND
                 FROM CARDSVC.CARD_LIMIT
                WHERE CARD_NUM = :DCL-CARD-NUM
                  AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
                ORDER BY LIMIT_TYPE
           END-EXEC.
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
           IF CAW-SCREEN-ID NOT = 'CARDDTL '
               PERFORM 1000-FIRST-DISPLAY
               GO TO 0000-RETURN
           END-IF
      *
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2000-PROCESS-INPUT
               WHEN DFHPF3
                   PERFORM 7000-BACK-TO-MENU
               WHEN DFHPF12
                   PERFORM 7100-EXIT-SESSION
               WHEN DFHCLEAR
                   PERFORM 7100-EXIT-SESSION
               WHEN OTHER
                   MOVE 'KEY NOT ACTIVE - USE ENTER, PF3 OR PF12'
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
           MOVE LOW-VALUES             TO CARDDTLO
           INITIALIZE CARD-RECORD
           MOVE 'N'                    TO WS-FOUND-SW
           MOVE ZERO                   TO WS-LIMIT-CNT
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
       1000-FIRST-DISPLAY.
           MOVE 'Y'                    TO WS-ERASE-SW
           MOVE 'CARDDTL '             TO CAW-SCREEN-ID
      *
           IF CAW-CARD-NUM NOT = SPACES AND CAW-CARD-NUM NOT =
              LOW-VALUES
               PERFORM 3000-READ-CARD
               IF WS-FOUND
                   PERFORM 3500-READ-LIMITS
                   PERFORM 4000-FORMAT-DETAIL
               ELSE
                   MOVE WS-MSG-NOT-FOUND
                                       TO CAW-MSG
               END-IF
           ELSE
               MOVE WS-MSG-KEY-REQD    TO CAW-MSG
           END-IF
      *
           PERFORM 5000-SEND-SCREEN
           .
      *
       2000-PROCESS-INPUT.
           EXEC CICS RECEIVE
                     MAP(WS-MAP)
                     MAPSET(WS-MAPSET)
                     INTO(CARDDTLI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE WS-MSG-KEY-REQD
                                       TO CAW-MSG
                   PERFORM 5000-SEND-SCREEN
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE '2000-PROCESS-INPUT'
                                       TO ER-PARAGRAPH
                   PERFORM 8100-CICS-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           IF CDCARDL = ZERO
               MOVE WS-MSG-KEY-REQD    TO CAW-MSG
               PERFORM 5000-SEND-SCREEN
               GO TO 2000-EXIT
           END-IF
      *
           MOVE CDCARDI                TO CAW-CARD-NUM
           PERFORM 3000-READ-CARD
      *
           IF WS-FOUND
               PERFORM 3500-READ-LIMITS
               PERFORM 4000-FORMAT-DETAIL
           ELSE
               MOVE WS-MSG-NOT-FOUND   TO CAW-MSG
           END-IF
      *
           PERFORM 5000-SEND-SCREEN
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - CARD MASTER                                             *
      ******************************************************************
       3000-READ-CARD.
           MOVE 'N'                    TO WS-FOUND-SW
           MOVE CAW-CARD-NUM           TO DCL-CARD-NUM
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
                    , PIN_TRIES
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
                    , :DCL-ACTIVATION-DT :IND-ACTIVATION-DT
                    , :DCL-LAST-USED-DT  :IND-LAST-USED-DT
                    , :DCL-PIN-TRIES
                    , :DCL-BLOCK-REASON  :IND-BLOCK-REASON
                    , :DCL-BLOCK-DATE    :IND-BLOCK-DATE
                 FROM CARDSVC.CARD
                WHERE CARD_NUM = :DCL-CARD-NUM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-FOUND-SW
                   PERFORM 3100-MOVE-TO-RECORD
               WHEN +100
                   MOVE 'N'            TO WS-FOUND-SW
               WHEN OTHER
                   MOVE '3000-READ-CARD'
                                       TO ER-PARAGRAPH
                   MOVE 'CARD             '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
       3100-MOVE-TO-RECORD.
           MOVE DCL-CARD-NUM           TO CARD-NUM
           MOVE DCL-ACCT-ID            TO CARD-ACCT-ID
           MOVE DCL-CUST-ID            TO CARD-CUST-ID
           MOVE DCL-EMBOSSED-NAME      TO CARD-EMBOSSED-NAME
           MOVE DCL-PRODUCT-CD         TO CARD-PRODUCT-CD
           MOVE DCL-CARD-STATUS        TO CARD-STATUS
           MOVE DCL-EXPIRY-YYMM        TO CARD-EXPIRY-YYMM
           MOVE DCL-PIN-TRIES          TO CARD-PIN-TRIES
      *
           IF IND-BLOCK-REASON < ZERO
               MOVE SPACES             TO CARD-BLOCK-REASON
           ELSE
               MOVE DCL-BLOCK-REASON   TO CARD-BLOCK-REASON
           END-IF
      *
           MOVE DCL-ACCT-ID            TO CAW-ACCT-ID
           MOVE DCL-CUST-ID            TO CAW-CUST-ID
           .
      *
      ******************************************************************
      * 3500 - THE EFFECTIVE LIMIT ROWS                                *
      ******************************************************************
       3500-READ-LIMITS.
           MOVE ZERO                   TO WS-LIMIT-CNT
           MOVE 'Y'                    TO WS-MORE-SW
      *
           EXEC SQL OPEN LIMCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE '3500-READ-LIMITS'  TO ER-PARAGRAPH
               MOVE 'CARD_LIMIT       ' TO ER-SQL-TABLE
               MOVE 'OPEN    '          TO ER-SQL-OPERATION
               PERFORM 8000-SQL-ERROR
               GO TO 3500-EXIT
           END-IF
      *
           PERFORM UNTIL NOT WS-MORE-ROWS OR WS-LIMIT-CNT = 4
               EXEC SQL
                   FETCH LIMCSR
                    INTO :DCL-LIMIT-TYPE
                       , :DCL-LIMIT-AMT
                       , :DCL-USED-AMT
                       , :DCL-AVAIL-AMT
                       , :DCL-APR-PCT
                       , :DCL-CASH-APR-PCT
                       , :DCL-RISK-BAND :IND-RISK-BAND
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1           TO WS-LIMIT-CNT
                       PERFORM 3600-FORMAT-LIMIT-LINE
                   WHEN +100
                       MOVE 'N'        TO WS-MORE-SW
                   WHEN OTHER
                       MOVE 'N'        TO WS-MORE-SW
                       MOVE '3500-READ-LIMITS'
                                       TO ER-PARAGRAPH
                       MOVE 'CARD_LIMIT       '
                                       TO ER-SQL-TABLE
                       MOVE 'FETCH   ' TO ER-SQL-OPERATION
                       PERFORM 8000-SQL-ERROR
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL CLOSE LIMCSR END-EXEC
           IF SQLCODE NOT = 0
      *        THE LIMIT LINES ARE ALREADY ON THE SCREEN.  A CLOSE
      *        FAILURE IS LOGGED BUT THE OPERATOR IS NOT STOPPED.
               MOVE '3500-CLOSE-LIMIT-CURSOR'
                                       TO ER-PARAGRAPH
               MOVE 'CARD_LIMIT       ' TO ER-SQL-TABLE
               MOVE 'CLOSE   '          TO ER-SQL-OPERATION
               MOVE 'W'                TO ER-SEVERITY
               MOVE 'LIMIT CURSOR CLOSE FAILED'
                                       TO ER-MESSAGE
               PERFORM 8000-SQL-ERROR
           END-IF
           .
       3500-EXIT.
           EXIT
           .
      *
       3600-FORMAT-LIMIT-LINE.
           EVALUATE DCL-LIMIT-TYPE
               WHEN 'CRED'
                   MOVE 'CREDIT      ' TO CDLTYPO(WS-LIMIT-CNT)
               WHEN 'CASH'
                   MOVE 'CASH ADVANCE' TO CDLTYPO(WS-LIMIT-CNT)
               WHEN 'DAIL'
                   MOVE 'DAILY       ' TO CDLTYPO(WS-LIMIT-CNT)
               WHEN 'FRGN'
                   MOVE 'FOREIGN     ' TO CDLTYPO(WS-LIMIT-CNT)
               WHEN OTHER
                   MOVE DCL-LIMIT-TYPE TO CDLTYPO(WS-LIMIT-CNT)
           END-EVALUATE
      *
           MOVE DCL-LIMIT-AMT          TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO CDLLIMO(WS-LIMIT-CNT)
           MOVE DCL-USED-AMT           TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO CDLUSDO(WS-LIMIT-CNT)
           MOVE DCL-AVAIL-AMT          TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO CDLAVLO(WS-LIMIT-CNT)
      *
      *    THE CREDIT ROW CARRIES THE RATES AND THE BAND FOR THE CARD
           IF DCL-LIMIT-TYPE = 'CRED'
               MOVE DCL-APR-PCT        TO WS-EDIT-RATE
               MOVE WS-EDIT-RATE       TO CDAPRO
               MOVE DCL-CASH-APR-PCT   TO WS-EDIT-RATE
               MOVE WS-EDIT-RATE       TO CDCAPRO
               IF IND-RISK-BAND < ZERO
                   MOVE '-'            TO CDBANDO
               ELSE
                   MOVE DCL-RISK-BAND  TO CDBANDO
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 4000 - FORMAT THE CARD DETAIL                                  *
      ******************************************************************
       4000-FORMAT-DETAIL.
           MOVE CARD-NUM               TO CDCARDO
           MOVE CARD-ACCT-ID           TO CDACCTO
           MOVE CARD-CUST-ID           TO CDCUSTO
           MOVE CARD-EMBOSSED-NAME     TO CDNAMEO
           MOVE CARD-PRODUCT-CD        TO CDPRODO
           MOVE CARD-STATUS            TO CDSTATO
           MOVE CARD-EXPIRY-YYMM       TO CDEXPO
           MOVE CARD-PIN-TRIES         TO CDPINO
           MOVE DCL-ISSUE-DATE         TO CDISSO
      *
           EVALUATE TRUE
               WHEN CARD-ACTIVE
                   MOVE 'ACTIVE        ' TO WS-STATUS-DESC
               WHEN CARD-BLOCKED
                   MOVE 'BLOCKED       ' TO WS-STATUS-DESC
               WHEN CARD-CLOSED
                   MOVE 'CLOSED        ' TO WS-STATUS-DESC
               WHEN CARD-LOST-STOLEN
                   MOVE 'LOST / STOLEN ' TO WS-STATUS-DESC
               WHEN CARD-EXPIRED
                   MOVE 'EXPIRED       ' TO WS-STATUS-DESC
               WHEN CARD-NOT-ACTIVATED
                   MOVE 'NOT ACTIVATED ' TO WS-STATUS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN       ' TO WS-STATUS-DESC
           END-EVALUATE
           MOVE WS-STATUS-DESC         TO CDSTATDO
      *
           IF IND-ACTIVATION-DT < ZERO
      *        NULL ON DB2 - THE OLDER PLASTICS STILL CARRY THE 6 DIGIT
      *        ACTIVATION DATE BROUGHT ACROSS ON THE VSAM CONVERSION
               IF CARD-ACTIVATION-DT > ZERO
                   MOVE CARD-ACTIVATION-DT
                                       TO WS-LEGACY-DATE
                   PERFORM 4500-WINDOW-LEGACY-DATE
                   MOVE WS-FULL-DATE   TO CDACTO
               ELSE
                   MOVE 'NOT ACTIV '   TO CDACTO
               END-IF
           ELSE
               MOVE DCL-ACTIVATION-DT  TO CDACTO
           END-IF
      *
           IF IND-LAST-USED-DT < ZERO
               MOVE 'NEVER USED'       TO CDLUSEO
           ELSE
               MOVE DCL-LAST-USED-DT   TO CDLUSEO
           END-IF
      *
           IF IND-BLOCK-REASON < ZERO
               MOVE SPACES             TO CDBLKRO
           ELSE
               MOVE DCL-BLOCK-REASON   TO CDBLKRO
           END-IF
      *
           IF IND-BLOCK-DATE < ZERO
               MOVE SPACES             TO CDBLKDO
           ELSE
               MOVE DCL-BLOCK-DATE     TO CDBLKDO
           END-IF
      *
           IF WS-LIMIT-CNT = ZERO
               MOVE WS-MSG-NO-LIMIT    TO CAW-MSG
           ELSE
               MOVE WS-MSG-OK          TO CAW-MSG
           END-IF
           .
      *
      ******************************************************************
      * 4500 - PIVOT WINDOW A 6 DIGIT YYMMDD.  RETAINED FOR THE OLDER  *
      *        CARDS WHOSE ACTIVATION DATE STILL COMES FROM THE VSAM   *
      *        EXTRACT RATHER THAN DB2.                                *
      ******************************************************************
       4500-WINDOW-LEGACY-DATE.
           IF WS-LEG-YY > WS-CENTURY-PIVOT
               MOVE WS-CENTURY-19      TO WS-FULL-DATE(1:2)
           ELSE
               MOVE WS-CENTURY-20      TO WS-FULL-DATE(1:2)
           END-IF
           MOVE WS-LEG-YY              TO WS-FULL-DATE(3:2)
           MOVE '-'                    TO WS-FULL-DATE(5:1)
           MOVE WS-LEG-MM              TO WS-FULL-DATE(6:2)
           MOVE '-'                    TO WS-FULL-DATE(8:1)
           MOVE WS-LEG-DD              TO WS-FULL-DATE(9:2)
           .
      *
      ******************************************************************
      * 5000 - SEND                                                    *
      ******************************************************************
       5000-SEND-SCREEN.
           MOVE WS-DATE-OUT            TO CDDATEO
           MOVE CAW-MSG                TO CDMSGO
           MOVE 'CARDDTL '             TO CAW-SCREEN-ID
      *
           IF CAW-CARD-NUM NOT = LOW-VALUES
               MOVE CAW-CARD-NUM       TO CDCARDO
           END-IF
      *
           IF WS-ERASE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDDTLO)
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
                         FROM(CARDDTLO)
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
      *
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
           PERFORM 5000-SEND-SCREEN
           .
      *
      ******************************************************************
      * 8000 - ERROR HANDLING                                          *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
      *    A PARAGRAPH THAT CAN CARRY ON SETS ITS OWN SEVERITY AND
      *    ITS OWN TEXT BEFORE IT GETS HERE
           IF ER-SEVERITY NOT = 'W'
               MOVE 'E'                TO ER-SEVERITY
               MOVE 'CARD DETAIL INQUIRY FAILED'
                                       TO ER-MESSAGE
           END-IF
           PERFORM 8900-LINK-ERROR-PGM
           MOVE SPACES                 TO ER-SEVERITY
           .
      *
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
                     TEXT('CACRD03 MUST BE STARTED FROM THE CA00 MENU')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           EXEC CICS RETURN END-EXEC
           .
