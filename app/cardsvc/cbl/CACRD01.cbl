      ******************************************************************
      * CACRD01 - ACCOUNT INQUIRY                                      *
      *                                                                *
      * MENU OPTION 1.  READS CARDSVC.ACCOUNT BY ACCOUNT ID AND        *
      * DISPLAYS BALANCES, CYCLE DATES, DELINQUENCY AND MINIMUM        *
      * PAYMENT.  PSEUDO CONVERSATIONAL ON TRANSACTION CA00.           *
      *                                                                *
      * CALLED BY   - CACRD90 XCTL, ROUTE MENU/OPT01                   *
      * CALLS       - CACRD90  ON PF3 (ROUTE MENU/OPTX AND THE MENU)   *
      *             - CACRD91  ERROR HANDLER                           *
      * TABLES      - CARDSVC.ACCOUNT  (SELECT)                        *
      * MAPSET      - CARDSET   MAP CARDACC                            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD01.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD01 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDSET '.
       01  WS-MAP                      PIC X(8)  VALUE 'CARDACC '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-FOUND-SW                 PIC X     VALUE 'N'.
           88  WS-FOUND                          VALUE 'Y'.
       01  WS-ERASE-SW                 PIC X     VALUE 'Y'.
           88  WS-ERASE                          VALUE 'Y'.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-OUT                 PIC X(8)  VALUE SPACES.
      *
       01  WS-EDIT-AMT                 PIC ---,---,---,--9.99.
       01  WS-EDIT-DATE                PIC X(10) VALUE SPACES.
      *
       01  WS-STATUS-DESC              PIC X(12) VALUE SPACES.
       01  WS-DELQ-DESC                PIC X(14) VALUE SPACES.
      *
       01  WS-MSG-NOT-FOUND            PIC X(60) VALUE
           'ACCOUNT NOT FOUND ON THE ACCOUNT MASTER'.
       01  WS-MSG-KEY-REQD             PIC X(60) VALUE
           'ENTER AN ACCOUNT NUMBER AND PRESS ENTER'.
       01  WS-MSG-OK                   PIC X(60) VALUE
           'ACCOUNT DISPLAYED'.
       01  WS-MSG-NUMERIC              PIC X(60) VALUE
           'ACCOUNT NUMBER MUST BE NUMERIC'.
      *
       01  WS-DISPATCH-AREA.
           05  WS-DA-ROUTE             PIC X(38).
           05  WS-DA-COMMAREA          PIC X(512).
      *
           COPY CVAUTHW1Y.
           COPY CVACCT01Y.
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
       01  DCL-ACCOUNT.
           05  DCL-ACCT-ID             PIC S9(11)   COMP-3.
           05  DCL-CUST-ID             PIC S9(9)    COMP-3.
           05  DCL-PARTY-ID            PIC X(11).
           05  DCL-PRODUCT-CD          PIC X(4).
           05  DCL-ACCT-STATUS         PIC X(1).
           05  DCL-CURRENCY-CD         PIC X(3).
           05  DCL-CURR-BAL            PIC S9(11)V99 COMP-3.
           05  DCL-STMT-BAL            PIC S9(11)V99 COMP-3.
           05  DCL-PENDING-AUTH        PIC S9(11)V99 COMP-3.
           05  DCL-CASH-BAL            PIC S9(11)V99 COMP-3.
           05  DCL-MIN-PAY-DUE         PIC S9(9)V99  COMP-3.
           05  DCL-LAST-PAY-AMT        PIC S9(9)V99  COMP-3.
           05  DCL-LAST-PAY-DATE       PIC X(10).
           05  DCL-CYCLE-DAY           PIC S9(4)    COMP.
           05  DCL-LAST-CYCLE-DT       PIC X(10).
           05  DCL-NEXT-CYCLE-DT       PIC X(10).
           05  DCL-PAY-DUE-DATE        PIC X(10).
           05  DCL-DELQ-BUCKET         PIC S9(4)    COMP.
           05  DCL-DELQ-AMT            PIC S9(9)V99  COMP-3.
           05  DCL-BRANCH-CD           PIC X(5).
      *
       01  IND-ARRAY.
           05  IND-LAST-PAY-DATE       PIC S9(4) COMP.
           05  IND-LAST-CYCLE-DT       PIC S9(4) COMP.
           05  IND-NEXT-CYCLE-DT       PIC S9(4) COMP.
           05  IND-PAY-DUE-DATE        PIC S9(4) COMP.
           05  IND-BRANCH-CD           PIC S9(4) COMP.
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
      *    ARRIVAL FROM THE MENU - THE SCREEN IS NOT YET ON THE TUBE
           IF CAW-SCREEN-ID NOT = 'CARDACC '
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
           MOVE LOW-VALUES             TO CARDACCO
           MOVE 'N'                    TO WS-FOUND-SW
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
      * 1000 - FIRST DISPLAY.  IF THE MENU PASSED AN ACCOUNT NUMBER    *
      *        THE INQUIRY IS RUN STRAIGHT AWAY.                       *
      ******************************************************************
       1000-FIRST-DISPLAY.
           MOVE 'Y'                    TO WS-ERASE-SW
           MOVE 'CARDACC '             TO CAW-SCREEN-ID
      *
           IF CAW-ACCT-ID > ZERO
               PERFORM 3000-READ-ACCOUNT
               IF WS-FOUND
                   PERFORM 4000-FORMAT-DETAIL
                   MOVE WS-MSG-OK      TO CAW-MSG
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
      ******************************************************************
      * 2000 - RE-ENTRY.  READ THE KEYED ACCOUNT NUMBER AND INQUIRE.   *
      ******************************************************************
       2000-PROCESS-INPUT.
           EXEC CICS RECEIVE
                     MAP(WS-MAP)
                     MAPSET(WS-MAPSET)
                     INTO(CARDACCI)
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
           IF AIACCTL = ZERO
               MOVE WS-MSG-KEY-REQD    TO CAW-MSG
               PERFORM 5000-SEND-SCREEN
               GO TO 2000-EXIT
           END-IF
      *
           IF AIACCTI IS NOT NUMERIC
               MOVE WS-MSG-NUMERIC     TO CAW-MSG
               PERFORM 5000-SEND-SCREEN
               GO TO 2000-EXIT
           END-IF
      *
           MOVE AIACCTI                TO CAW-ACCT-ID
           PERFORM 3000-READ-ACCOUNT
      *
           IF WS-FOUND
               PERFORM 4000-FORMAT-DETAIL
               MOVE WS-MSG-OK          TO CAW-MSG
           ELSE
               MOVE WS-MSG-NOT-FOUND   TO CAW-MSG
               MOVE ZERO               TO CAW-CUST-ID
               MOVE SPACES             TO CAW-PARTY-ID
           END-IF
      *
           PERFORM 5000-SEND-SCREEN
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - ACCOUNT MASTER READ                                     *
      ******************************************************************
       3000-READ-ACCOUNT.
           MOVE 'N'                    TO WS-FOUND-SW
           MOVE CAW-ACCT-ID            TO DCL-ACCT-ID
      *
           EXEC SQL
               SELECT ACCT_ID
                    , CUST_ID
                    , PARTY_ID
                    , PRODUCT_CD
                    , ACCT_STATUS
                    , CURRENCY_CD
                    , CURR_BAL
                    , STMT_BAL
                    , PENDING_AUTH_AMT
                    , CASH_BAL
                    , MIN_PAY_DUE
                    , LAST_PAY_AMT
                    , CHAR(LAST_PAY_DATE, ISO)
                    , CYCLE_DAY
                    , CHAR(LAST_CYCLE_DATE, ISO)
                    , CHAR(NEXT_CYCLE_DATE, ISO)
                    , CHAR(PAY_DUE_DATE, ISO)
                    , DELQ_BUCKET
                    , DELQ_AMT
                    , BRANCH_CD
                 INTO :DCL-ACCT-ID
                    , :DCL-CUST-ID
                    , :DCL-PARTY-ID
                    , :DCL-PRODUCT-CD
                    , :DCL-ACCT-STATUS
                    , :DCL-CURRENCY-CD
                    , :DCL-CURR-BAL
                    , :DCL-STMT-BAL
                    , :DCL-PENDING-AUTH
                    , :DCL-CASH-BAL
                    , :DCL-MIN-PAY-DUE
                    , :DCL-LAST-PAY-AMT
                    , :DCL-LAST-PAY-DATE  :IND-LAST-PAY-DATE
                    , :DCL-CYCLE-DAY
                    , :DCL-LAST-CYCLE-DT  :IND-LAST-CYCLE-DT
                    , :DCL-NEXT-CYCLE-DT  :IND-NEXT-CYCLE-DT
                    , :DCL-PAY-DUE-DATE   :IND-PAY-DUE-DATE
                    , :DCL-DELQ-BUCKET
                    , :DCL-DELQ-AMT
                    , :DCL-BRANCH-CD      :IND-BRANCH-CD
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-FOUND-SW
                   PERFORM 3100-MOVE-TO-RECORD
               WHEN +100
                   MOVE 'N'            TO WS-FOUND-SW
               WHEN OTHER
                   MOVE '3000-READ-ACCOUNT'
                                       TO ER-PARAGRAPH
                   MOVE 'ACCOUNT          '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
       3100-MOVE-TO-RECORD.
           MOVE DCL-ACCT-ID            TO ACCT-ID
           MOVE DCL-CUST-ID            TO ACCT-CUST-ID
           MOVE DCL-PARTY-ID           TO ACCT-PARTY-ID
           MOVE DCL-PRODUCT-CD         TO ACCT-PRODUCT-CD
           MOVE DCL-ACCT-STATUS        TO ACCT-STATUS
           MOVE DCL-CURRENCY-CD        TO ACCT-CURRENCY
           MOVE DCL-CURR-BAL           TO ACCT-CURR-BAL
           MOVE DCL-STMT-BAL           TO ACCT-STMT-BAL
           MOVE DCL-PENDING-AUTH       TO ACCT-PENDING-AUTH-AMT
           MOVE DCL-CASH-BAL           TO ACCT-CASH-BAL
           MOVE DCL-MIN-PAY-DUE        TO ACCT-MIN-PAY-DUE
           MOVE DCL-LAST-PAY-AMT       TO ACCT-LAST-PAY-AMT
           MOVE DCL-CYCLE-DAY          TO ACCT-CYCLE-DAY
           MOVE DCL-DELQ-BUCKET        TO ACCT-DELQ-BUCKET
           MOVE DCL-DELQ-AMT           TO ACCT-DELQ-AMT
      *
           IF IND-BRANCH-CD < ZERO
               MOVE SPACES             TO ACCT-BRANCH-CD
           ELSE
               MOVE DCL-BRANCH-CD      TO ACCT-BRANCH-CD
           END-IF
      *
           MOVE DCL-CUST-ID            TO CAW-CUST-ID
           MOVE DCL-PARTY-ID           TO CAW-PARTY-ID
           .
      *
      ******************************************************************
      * 4000 - FORMAT THE DETAIL LINES                                 *
      ******************************************************************
       4000-FORMAT-DETAIL.
           MOVE ACCT-ID                TO AIACCTO
           MOVE ACCT-CUST-ID           TO AICUSTO
           MOVE ACCT-STATUS            TO AISTATO
           MOVE ACCT-PRODUCT-CD        TO AIPRODO
           MOVE ACCT-CURRENCY          TO AICURRO
           MOVE ACCT-BRANCH-CD         TO AIBRCHO
      *
           EVALUATE TRUE
               WHEN ACCT-OPEN
                   MOVE 'OPEN        ' TO WS-STATUS-DESC
               WHEN ACCT-CLOSED
                   MOVE 'CLOSED      ' TO WS-STATUS-DESC
               WHEN ACCT-SUSPENDED
                   MOVE 'SUSPENDED   ' TO WS-STATUS-DESC
               WHEN ACCT-WRITTEN-OFF
                   MOVE 'WRITTEN OFF ' TO WS-STATUS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN     ' TO WS-STATUS-DESC
           END-EVALUATE
           MOVE WS-STATUS-DESC         TO AISTATDO
      *
           MOVE ACCT-CURR-BAL          TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO AICBALO
           MOVE ACCT-STMT-BAL          TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO AISBALO
           MOVE ACCT-PENDING-AUTH-AMT  TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO AIPENDO
           MOVE ACCT-CASH-BAL          TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO AICASHO
           MOVE ACCT-MIN-PAY-DUE       TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO AIMINPO
           MOVE ACCT-LAST-PAY-AMT      TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO AILPAYO
           MOVE ACCT-DELQ-AMT          TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO AIDLQAO
      *
           MOVE DCL-LAST-PAY-DATE      TO WS-EDIT-DATE
           IF IND-LAST-PAY-DATE < ZERO
               MOVE 'NONE      '       TO WS-EDIT-DATE
           END-IF
           MOVE WS-EDIT-DATE           TO AILPDTO
      *
           MOVE DCL-LAST-CYCLE-DT      TO WS-EDIT-DATE
           IF IND-LAST-CYCLE-DT < ZERO
               MOVE 'NOT CYCLED'       TO WS-EDIT-DATE
           END-IF
           MOVE WS-EDIT-DATE           TO AILCYCO
      *
           MOVE DCL-NEXT-CYCLE-DT      TO WS-EDIT-DATE
           IF IND-NEXT-CYCLE-DT < ZERO
               MOVE 'UNSET     '       TO WS-EDIT-DATE
           END-IF
           MOVE WS-EDIT-DATE           TO AINCYCO
      *
           MOVE DCL-PAY-DUE-DATE       TO WS-EDIT-DATE
           IF IND-PAY-DUE-DATE < ZERO
               MOVE 'UNSET     '       TO WS-EDIT-DATE
           END-IF
           MOVE WS-EDIT-DATE           TO AIDUEO
      *
           MOVE ACCT-CYCLE-DAY         TO AICYCDO
           MOVE ACCT-DELQ-BUCKET       TO AIDELQO
      *
           EVALUATE TRUE
               WHEN ACCT-CURRENT
                   MOVE 'CURRENT       ' TO WS-DELQ-DESC
               WHEN ACCT-DELQ-30
                   MOVE '30 DAYS       ' TO WS-DELQ-DESC
               WHEN ACCT-DELQ-60
                   MOVE '60 DAYS       ' TO WS-DELQ-DESC
               WHEN ACCT-DELQ-90
                   MOVE '90 DAYS       ' TO WS-DELQ-DESC
               WHEN ACCT-DELQ-120-PLUS
                   MOVE '120 DAYS PLUS ' TO WS-DELQ-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN       ' TO WS-DELQ-DESC
           END-EVALUATE
           MOVE WS-DELQ-DESC           TO AIDELQDO
           .
      *
      ******************************************************************
      * 5000 - SEND THE INQUIRY SCREEN                                 *
      ******************************************************************
       5000-SEND-SCREEN.
           MOVE WS-DATE-OUT            TO AIDATEO
           MOVE CAW-MSG                TO AIMSGO
           MOVE 'CARDACC '             TO CAW-SCREEN-ID
      *
           IF CAW-ACCT-ID > ZERO
               MOVE CAW-ACCT-ID        TO AIACCTO
           END-IF
      *
           IF WS-ERASE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDACCO)
                         ERASE
                         CURSOR
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDACCO)
                         DATAONLY
                         CURSOR
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
           END-IF
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '5000-SEND-SCREEN' TO ER-PARAGRAPH
               PERFORM 8100-CICS-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 7000 - NAVIGATION                                              *
      ******************************************************************
       7000-BACK-TO-MENU.
           MOVE SPACES                 TO CAW-MSG
           MOVE SPACES                 TO CAW-SCREEN-ID
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
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
           PERFORM 7200-DISPATCH
           .
      *
       7200-DISPATCH.
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
      * 8000 / 9000 - ERROR HANDLING                                   *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'ACCOUNT INQUIRY FAILED - SEE DIAGNOSTIC'
                                       TO ER-MESSAGE
           PERFORM 8900-LINK-ERROR-PGM
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
                     TEXT('CACRD01 MUST BE STARTED FROM THE CA00 MENU')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN END-EXEC
           .
