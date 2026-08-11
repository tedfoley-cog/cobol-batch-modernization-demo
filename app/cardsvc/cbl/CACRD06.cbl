      ******************************************************************
      * CACRD06 - CARD AND ACCOUNT STATUS VALIDATION                   *
      *                                                                *
      * THIRD PROGRAM OF THE AUTHORIZATION CHAIN.  READS THE CARD AND  *
      * THE OWNING ACCOUNT AND APPLIES THE STATUS RULES THAT DO NOT    *
      * DEPEND ON AMOUNTS -                                            *
      *                                                                *
      *   CARD STATUS   A ACTIVE       PASS                            *
      *                 B BLOCKED      CARD BLOCKED       CB01         *
      *                 C CLOSED       CARD CLOSED        CC02         *
      *                 L LOST STOLEN  PICK UP CARD       CL03         *
      *                 E EXPIRED      CARD EXPIRED       CE04         *
      *                 N NOT ACTIVE   CARD NOT ACTIVATED CN05         *
      *   EXPIRY YYMM   BEFORE THE CURRENT MONTH          CE04         *
      *   PIN TRIES     THREE OR MORE CONSECUTIVE FAILS   CP06         *
      *   ACCOUNT       C CLOSED / S SUSPENDED / W W-OFF  AC07/AS08/   *
      *                                                   AW09         *
      *   DELINQUENCY   BUCKET 3 AND ABOVE                AD10         *
      *                 BUCKET 2 REFERS RATHER THAN FAILS AD11         *
      *                                                                *
      * A FAILURE HERE DOES NOT END THE CHAIN.  THE DECISION STEP      *
      * OWNS THE OUTCOME - THIS PROGRAM ONLY RECORDS THE VERDICT.      *
      *                                                                *
      * CALLED BY   - CACRD05  XCTL                                    *
      * CALLS       - CACRD07  XCTL, FRAUD EVALUATION                  *
      *             - CACRD09  XCTL WHEN THE CARD CANNOT BE READ       *
      *             - CACRD91  ERROR HANDLER                           *
      * TABLES      - CARDSVC.CARD, CARDSVC.ACCOUNT  (SELECT)          *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD06.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD06 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'CACRD07 '.
       01  WS-DECISION-PGM             PIC X(8)  VALUE 'CACRD09 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-YYYYMMDD            PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-YYYYMMDD.
           05  WS-TODAY-CC             PIC 9(2).
           05  WS-TODAY-YY             PIC 9(2).
           05  WS-TODAY-MM             PIC 9(2).
           05  WS-TODAY-DD             PIC 9(2).
       01  WS-TODAY-YYMM               PIC 9(4)  VALUE ZERO.
      *
       01  WS-CARD-FOUND-SW            PIC X     VALUE 'N'.
           88  WS-CARD-FOUND                     VALUE 'Y'.
       01  WS-ACCT-FOUND-SW            PIC X     VALUE 'N'.
           88  WS-ACCT-FOUND                     VALUE 'Y'.
      *
           COPY CVAUTHW1Y.
           COPY CVCARD01Y.
           COPY CVACCT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-CARD-NUM                PIC X(16).
       01  DCL-CARD-STATUS             PIC X(1).
       01  DCL-EXPIRY-YYMM             PIC X(4).
       01  DCL-PIN-TRIES               PIC S9(4) COMP.
       01  DCL-BLOCK-REASON            PIC X(4).
       01  DCL-CARD-ACCT-ID            PIC S9(11) COMP-3.
       01  DCL-CARD-CUST-ID            PIC S9(9)  COMP-3.
       01  DCL-PRODUCT-CD              PIC X(4).
      *
       01  DCL-ACCT-ID                 PIC S9(11) COMP-3.
       01  DCL-ACCT-STATUS             PIC X(1).
       01  DCL-ACCT-CURR               PIC X(3).
       01  DCL-PARTY-ID                PIC X(11).
       01  DCL-DELQ-BUCKET             PIC S9(4)  COMP.
       01  DCL-DELQ-AMT                PIC S9(9)V99 COMP-3.
       01  DCL-CURR-BAL                PIC S9(11)V99 COMP-3.
       01  DCL-PENDING-AMT             PIC S9(11)V99 COMP-3.
      *
       01  IND-BLOCK-REASON            PIC S9(4) COMP.
       01  IND-PARTY-ID                PIC S9(4) COMP.
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
               GO TO 0000-EXIT
           END-IF
      *
           MOVE DFHCOMMAREA            TO CA-WORK-AREA
           PERFORM 0100-INIT
      *
           PERFORM 1000-READ-CARD
           IF NOT WS-CARD-FOUND
               PERFORM 1900-CARD-MISSING
               PERFORM 6000-CONTINUE-CHAIN
               GO TO 0000-EXIT
           END-IF
      *
           PERFORM 2000-READ-ACCOUNT
      *
           PERFORM 3000-CHECK-CARD-STATUS
           IF CAW-STAT-OK
               PERFORM 3200-CHECK-EXPIRY
           END-IF
           IF CAW-STAT-OK
               PERFORM 3300-CHECK-PIN-TRIES
           END-IF
           IF CAW-STAT-OK
               PERFORM 3400-CHECK-ACCT-STATUS
           END-IF
           IF CAW-STAT-OK
               PERFORM 3500-CHECK-DELINQUENCY
           END-IF
      *
           PERFORM 6000-CONTINUE-CHAIN
           .
       0000-EXIT.
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           MOVE SPACES                 TO ERROR-AREA
           INITIALIZE CARD-RECORD
           INITIALIZE ACCT-RECORD
           MOVE 'N'                    TO WS-CARD-FOUND-SW
                                          WS-ACCT-FOUND-SW
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
      *    THE STATUS BLOCK STARTS CLEAN ON EVERY REQUEST
           MOVE 'P'                    TO CAW-STAT-RESULT
           MOVE SPACES                 TO CAW-STAT-REASON
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-YYYYMMDD)
                     RESP(WS-RESP)
           END-EXEC
      *
           COMPUTE WS-TODAY-YYMM = WS-TODAY-YY * 100 + WS-TODAY-MM
      *
           IF CAW-TRAIL-CNT < 8
               ADD 1                   TO CAW-TRAIL-CNT
               MOVE WS-PGM-ID          TO CAW-TRAIL-PGM(CAW-TRAIL-CNT)
               MOVE ZERO               TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 1000 - CARD MASTER                                             *
      ******************************************************************
       1000-READ-CARD.
           MOVE CAW-RQ-CARD-NUM        TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT CARD_STATUS
                    , EXPIRY_YYMM
                    , PIN_TRIES
                    , BLOCK_REASON
                    , ACCT_ID
                    , CUST_ID
                    , PRODUCT_CD
                 INTO :DCL-CARD-STATUS
                    , :DCL-EXPIRY-YYMM
                    , :DCL-PIN-TRIES
                    , :DCL-BLOCK-REASON :IND-BLOCK-REASON
                    , :DCL-CARD-ACCT-ID
                    , :DCL-CARD-CUST-ID
                    , :DCL-PRODUCT-CD
                 FROM CARDSVC.CARD
                WHERE CARD_NUM = :DCL-CARD-NUM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-CARD-FOUND-SW
                   MOVE DCL-CARD-STATUS
                                       TO CARD-STATUS
                   MOVE DCL-EXPIRY-YYMM
                                       TO CARD-EXPIRY-YYMM
                   MOVE DCL-PIN-TRIES  TO CARD-PIN-TRIES
                   MOVE DCL-CARD-ACCT-ID
                                       TO CARD-ACCT-ID
                                          CAW-ACCT-ID
                   MOVE DCL-CARD-CUST-ID
                                       TO CARD-CUST-ID
                                          CAW-CUST-ID
                   MOVE DCL-PRODUCT-CD TO CARD-PRODUCT-CD
                   IF IND-BLOCK-REASON < ZERO
                       MOVE SPACES     TO CARD-BLOCK-REASON
                   ELSE
                       MOVE DCL-BLOCK-REASON
                                       TO CARD-BLOCK-REASON
                   END-IF
               WHEN +100
                   MOVE 'N'            TO WS-CARD-FOUND-SW
               WHEN OTHER
                   MOVE 'N'            TO WS-CARD-FOUND-SW
                   MOVE '1000-READ-CARD'
                                       TO ER-PARAGRAPH
                   MOVE 'CARD             '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
       1900-CARD-MISSING.
           MOVE 'F'                    TO CAW-STAT-RESULT
           MOVE 'CX00'                 TO CAW-STAT-REASON
           MOVE WS-DECISION-PGM        TO WS-NEXT-PGM
           IF CAW-TRAIL-CNT > ZERO
               MOVE WS-RC-ERROR        TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 2000 - OWNING ACCOUNT                                          *
      ******************************************************************
       2000-READ-ACCOUNT.
           MOVE CAW-ACCT-ID            TO DCL-ACCT-ID
      *
           EXEC SQL
               SELECT ACCT_STATUS
                    , CURRENCY_CD
                    , PARTY_ID
                    , DELQ_BUCKET
                    , DELQ_AMT
                    , CURR_BAL
                    , PENDING_AUTH_AMT
                 INTO :DCL-ACCT-STATUS
                    , :DCL-ACCT-CURR
                    , :DCL-PARTY-ID :IND-PARTY-ID
                    , :DCL-DELQ-BUCKET
                    , :DCL-DELQ-AMT
                    , :DCL-CURR-BAL
                    , :DCL-PENDING-AMT
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-ACCT-FOUND-SW
                   MOVE DCL-ACCT-STATUS
                                       TO ACCT-STATUS
                   MOVE DCL-ACCT-CURR  TO ACCT-CURRENCY
                   MOVE DCL-DELQ-BUCKET
                                       TO ACCT-DELQ-BUCKET
                   MOVE DCL-DELQ-AMT   TO ACCT-DELQ-AMT
                   MOVE DCL-CURR-BAL   TO ACCT-CURR-BAL
                   MOVE DCL-PENDING-AMT
                                       TO ACCT-PENDING-AUTH-AMT
                   IF IND-PARTY-ID NOT < ZERO
                       MOVE DCL-PARTY-ID
                                       TO ACCT-PARTY-ID
                                          CAW-PARTY-ID
                   END-IF
               WHEN +100
                   MOVE 'N'            TO WS-ACCT-FOUND-SW
                   MOVE 'F'            TO CAW-STAT-RESULT
                   MOVE 'AX12'         TO CAW-STAT-REASON
               WHEN OTHER
                   MOVE 'N'            TO WS-ACCT-FOUND-SW
                   MOVE '2000-READ-ACCOUNT'
                                       TO ER-PARAGRAPH
                   MOVE 'ACCOUNT          '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - THE STATUS RULES                                        *
      ******************************************************************
       3000-CHECK-CARD-STATUS.
           EVALUATE TRUE
               WHEN CARD-ACTIVE
                   CONTINUE
               WHEN CARD-BLOCKED
                   PERFORM 3900-FAIL-CARD
                   MOVE 'CB01'         TO CAW-STAT-REASON
      *            A BLOCK PLACED BY THE FRAUD TEAM IS REPORTED
      *            SEPARATELY SO THE OPERATOR DOES NOT TELL THE
      *            CARDHOLDER TO CALL THE CARD DESK
                   IF CARD-BLOCK-REASON = 'FRAU'
                       MOVE 'CB12'     TO CAW-STAT-REASON
                   END-IF
               WHEN CARD-CLOSED
                   PERFORM 3900-FAIL-CARD
                   MOVE 'CC02'         TO CAW-STAT-REASON
               WHEN CARD-LOST-STOLEN
                   PERFORM 3900-FAIL-CARD
                   MOVE 'CL03'         TO CAW-STAT-REASON
               WHEN CARD-EXPIRED
                   PERFORM 3900-FAIL-CARD
                   MOVE 'CE04'         TO CAW-STAT-REASON
               WHEN CARD-NOT-ACTIVATED
                   PERFORM 3900-FAIL-CARD
                   MOVE 'CN05'         TO CAW-STAT-REASON
               WHEN OTHER
                   PERFORM 3900-FAIL-CARD
                   MOVE 'CU99'         TO CAW-STAT-REASON
           END-EVALUATE
           .
      *
      *    THE EXPIRY IS A YYMM ON THE MASTER.  A CARD IS GOOD TO THE
      *    LAST DAY OF THE MONTH PRINTED ON IT.
       3200-CHECK-EXPIRY.
           IF CARD-EXPIRY-YYMM < WS-TODAY-YYMM
               PERFORM 3900-FAIL-CARD
               MOVE 'CE04'             TO CAW-STAT-REASON
           END-IF
      *
      *    A CARD EXPIRING THIS MONTH IS STILL GOOD BUT IS FLAGGED
      *    FOR THE DECISION STEP TO CONSIDER ALONGSIDE THE RISK BAND
           IF CARD-EXPIRY-YYMM = WS-TODAY-YYMM
               IF CAW-RSK-BAND = 'C' OR 'X'
                   PERFORM 3900-FAIL-CARD
                   MOVE 'CE13'         TO CAW-STAT-REASON
               END-IF
           END-IF
           .
      *
       3300-CHECK-PIN-TRIES.
           IF CARD-PIN-TRIES > 2
               PERFORM 3900-FAIL-CARD
               MOVE 'CP06'             TO CAW-STAT-REASON
           END-IF
           .
      *
       3400-CHECK-ACCT-STATUS.
           IF NOT WS-ACCT-FOUND
               GO TO 3400-EXIT
           END-IF
      *
           EVALUATE TRUE
               WHEN ACCT-OPEN
                   CONTINUE
               WHEN ACCT-CLOSED
                   PERFORM 3900-FAIL-CARD
                   MOVE 'AC07'         TO CAW-STAT-REASON
               WHEN ACCT-SUSPENDED
                   PERFORM 3900-FAIL-CARD
                   MOVE 'AS08'         TO CAW-STAT-REASON
               WHEN ACCT-WRITTEN-OFF
                   PERFORM 3900-FAIL-CARD
                   MOVE 'AW09'         TO CAW-STAT-REASON
               WHEN OTHER
                   PERFORM 3900-FAIL-CARD
                   MOVE 'AU98'         TO CAW-STAT-REASON
           END-EVALUATE
           .
       3400-EXIT.
           EXIT
           .
      *
      *    A REFUND IS ALLOWED ON A DELINQUENT ACCOUNT - MONEY IS
      *    COMING BACK IN.  EVERYTHING ELSE IS TESTED.
       3500-CHECK-DELINQUENCY.
           IF CAW-RQ-AUTH-TYPE = 'R'
               GO TO 3500-EXIT
           END-IF
      *
           EVALUATE TRUE
               WHEN ACCT-DELQ-120-PLUS
                   PERFORM 3900-FAIL-CARD
                   MOVE 'AD10'         TO CAW-STAT-REASON
               WHEN ACCT-DELQ-90
                   PERFORM 3900-FAIL-CARD
                   MOVE 'AD10'         TO CAW-STAT-REASON
               WHEN ACCT-DELQ-60
      *            SIXTY DAYS DOWN IS A REFERRAL, NOT A DECLINE -
      *            THE DECISION STEP DOWNGRADES THE OUTCOME
                   MOVE 'Y'            TO CAW-RSK-REFERRAL-SW
                   MOVE 'AD11'         TO CAW-STAT-REASON
               WHEN ACCT-DELQ-30
      *            THIRTY DAYS DOWN ONLY MATTERS ON A CASH ADVANCE
                   IF CAW-RQ-AUTH-TYPE = 'C'
                       MOVE 'Y'        TO CAW-RSK-REFERRAL-SW
                       MOVE 'AD11'     TO CAW-STAT-REASON
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       3500-EXIT.
           EXIT
           .
      *
       3900-FAIL-CARD.
           MOVE 'F'                    TO CAW-STAT-RESULT
           IF CAW-TRAIL-CNT > ZERO
               MOVE WS-RC-ERROR        TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 6000 - PASS CONTROL ON                                         *
      ******************************************************************
       6000-CONTINUE-CHAIN.
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
      *    A HARD STATUS FAILURE MAKES THE FRAUD AND LIMIT WORK
      *    POINTLESS - GO STRAIGHT TO THE DECISION.
           IF CAW-STAT-FAILED
               MOVE WS-DECISION-PGM    TO WS-NEXT-PGM
           END-IF
      *
           EXEC CICS XCTL
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           MOVE '6000-CONTINUE-CHAIN'  TO ER-PARAGRAPH
           PERFORM 8100-CICS-ERROR
           .
      *
      ******************************************************************
      * 8000 - ERROR HANDLING                                          *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'STATUS VALIDATION READ FAILED'
                                       TO ER-MESSAGE
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
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
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
                     TEXT('CACRD06 IS A CHAIN STEP - START WITH CA00')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           .
