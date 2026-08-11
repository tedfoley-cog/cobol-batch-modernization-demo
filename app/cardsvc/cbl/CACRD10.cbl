      ******************************************************************
      * CACRD10 - AUTHORIZATION RECORD WRITE                           *
      *                                                                *
      * SEVENTH AND LAST WORKING PROGRAM OF THE AUTHORIZATION CHAIN.   *
      * THE DECISION IS ALREADY MADE - THIS PROGRAM ONLY RECORDS IT.   *
      *                                                                *
      * WORK DONE, IN ORDER -                                          *
      *   1. THE AUTH RECORD SKELETON IS RECOVERED FROM THE TSQ THAT   *
      *      CACRD04 WROTE.  IT CARRIES THE 60 BYTE VARIANT IMAGE.     *
      *   2. A SEQUENCE NUMBER IS TAKEN FOR THE CARD AND DATE.         *
      *   3. THE ROW IS INSERTED INTO CARDSVC.AUTHORIZATION.           *
      *   4. ON AN APPROVAL ONLY - ACCOUNT.PENDING_AUTH_AMT AND THE    *
      *      CARD_LIMIT USED AND AVAILABLE AMOUNTS ARE UPDATED.        *
      *   5. THE RAW IMAGE IS WRITTEN TO THE ESDS AUTHLOG, WHICH THE   *
      *      NIGHTLY CYCLE READS.                                      *
      *   6. CARD.LAST_USED_DATE IS STAMPED.                           *
      *                                                                *
      * A REFERRAL OR A DECLINE IS STILL RECORDED AND STILL LOGGED -   *
      * THE FRAUD TEAM AND THE DECLINE ANALYTICS BOTH NEED THEM.       *
      *                                                                *
      * NOTHING IS COMMITTED HERE.  THE SYNCPOINT IS TAKEN BY CICS AT  *
      * TASK END, SO THE ESDS WRITE AND THE DB2 CHANGES STAND OR FALL  *
      * TOGETHER.                                                      *
      *                                                                *
      * CALLED BY   - CACRD09  XCTL                                    *
      * CALLS       - CACRD17  XCTL, EXIT AND SESSION AUDIT            *
      *             - CACRD91  ERROR HANDLER                           *
      * TABLES      - CARDSVC.AUTHORIZATION  (SELECT, INSERT)          *
      *               CARDSVC.ACCOUNT        (UPDATE)                  *
      *               CARDSVC.CARD_LIMIT     (UPDATE)                  *
      *               CARDSVC.CARD           (UPDATE)                  *
      * FILES       - AUTHLOG   CARD.PROD.AUTHLOG  (WRITE)             *
      * MAPSET      - CARDSET   MAP CARDRSP                            *
      * TSQ         - CARDAUTQ  AUTH RECORD SKELETON                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD10.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD10 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'CACRD17 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDSET '.
       01  WS-MAP-RSP                  PIC X(8)  VALUE 'CARDRSP '.
       01  WS-AUTH-TSQ                 PIC X(8)  VALUE 'CARDAUTQ'.
       01  WS-LOG-FILE                 PIC X(8)  VALUE 'AUTHLOG '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-TSQ-ITEM                 PIC S9(4) COMP VALUE 1.
       01  WS-TSQ-LEN                  PIC S9(4) COMP VALUE 200.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-YYYYMMDD            PIC 9(8)  VALUE ZERO.
       01  WS-TIME-HHMMSS              PIC 9(6)  VALUE ZERO.
       01  WS-TIMESTAMP                PIC X(26) VALUE SPACES.
      *
       01  WS-UPDATE-OK-SW             PIC X     VALUE 'Y'.
           88  WS-UPDATE-OK                      VALUE 'Y'.
           88  WS-UPDATE-BAD                     VALUE 'N'.
      *
       01  WS-RETRY-CNT                PIC 9(2)  VALUE ZERO.
       01  WS-MAX-RETRY                PIC 9(2)  VALUE 05.
      *
      *    ---------------------------------------------------------
      *    AUTHLOG RECORD - 200 BYTE ESDS IMAGE.  THE FIRST 140
      *    BYTES ARE THE AUTH RECORD AS IT STANDS, THE REST IS THE
      *    ONLINE CONTEXT THE BATCH RECONCILIATION NEEDS.
      *    ---------------------------------------------------------
       01  AUTHLOG-RECORD.
           05  AL-RECORD-TYPE          PIC X(2)  VALUE 'AU'.
           05  AL-AUTH-IMAGE           PIC X(140).
           05  AL-DEC-STATUS           PIC X.
           05  AL-RESP-CODE            PIC X(2).
           05  AL-REASON-CD            PIC X(4).
           05  AL-RISK-SCORE           PIC 9(3).
           05  AL-RISK-BAND            PIC X.
           05  AL-FRAUD-SCORE          PIC 9(3).
           05  AL-FRAUD-ACTION         PIC X(4).
           05  AL-FRAUD-RULE           PIC X(8).
           05  AL-LIM-TYPE             PIC X(4).
           05  AL-VELOCITY-CNT         PIC 9(3).
           05  AL-TRAN-ID              PIC X(4).
           05  AL-TERM-ID              PIC X(4).
           05  AL-OPER-ID              PIC X(8).
           05  AL-LOG-TIMESTAMP        PIC X(8).
           05  AL-FILLER               PIC X(1).
      *
       01  WS-LOG-LEN                  PIC S9(4) COMP VALUE 200.
       01  WS-LOG-RBA                  PIC S9(8) COMP VALUE 0.
      *
       01  WS-EDIT-AMT                 PIC ZZZ,ZZZ,ZZ9.99-.
       01  WS-EDIT-SEQ                 PIC 9(9).
      *
           COPY CVAUTHW1Y.
           COPY CVAUTH01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
           COPY CARDSET.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-CARD-NUM                PIC X(16).
       01  DCL-AUTH-DATE               PIC X(10).
       01  DCL-AUTH-TIME               PIC X(8).
       01  DCL-AUTH-SEQ-NUM            PIC S9(9) COMP-3.
       01  DCL-ACCT-ID                 PIC S9(11) COMP-3.
       01  DCL-CUST-ID                 PIC S9(9) COMP-3.
       01  DCL-AUTH-TYPE               PIC X(1).
       01  DCL-AUTH-STATUS             PIC X(1).
       01  DCL-RESP-CODE               PIC X(2).
       01  DCL-REASON-CD               PIC X(4).
       01  DCL-RISK-SCORE              PIC S9(4) COMP.
       01  DCL-RISK-BAND               PIC X(1).
       01  DCL-AUTH-DETAIL             PIC X(60).
       01  DCL-ORIG-PGM                PIC X(8).
       01  DCL-TERM-ID                 PIC X(4).
       01  DCL-OPER-ID                 PIC X(8).
      *
       01  DCL-LIMIT-TYPE              PIC X(4).
       01  DCL-POST-AMT                PIC S9(11)V99 COMP-3.
      *
       01  IND-MAX-SEQ                 PIC S9(4) COMP.
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
           PERFORM 1000-RECOVER-SKELETON
           PERFORM 1500-NEXT-SEQ-NBR
           PERFORM 2000-COMPLETE-RECORD
           PERFORM 3000-INSERT-AUTH
      *
           IF WS-UPDATE-OK
               IF CAW-DEC-APPROVED
                   PERFORM 4000-UPDATE-BALANCES
               END-IF
               PERFORM 5000-WRITE-LOG
               PERFORM 5500-STAMP-CARD
           END-IF
      *
           PERFORM 5900-DELETE-TSQ
           PERFORM 7000-SEND-OUTCOME
           PERFORM 8500-CONTINUE-CHAIN
           .
       0000-EXIT.
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
           MOVE 'Y'                    TO WS-UPDATE-OK-SW
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-YYYYMMDD)
                     TIME(WS-TIME-HHMMSS)
                     RESP(WS-RESP)
           END-EXEC
      *
           IF CAW-AUTH-DATE = ZERO
               MOVE WS-DATE-YYYYMMDD   TO CAW-AUTH-DATE
           END-IF
           IF CAW-AUTH-TIME = ZERO
               MOVE WS-TIME-HHMMSS     TO CAW-AUTH-TIME
           END-IF
      *
           IF CAW-TRAIL-CNT < 8
               ADD 1                   TO CAW-TRAIL-CNT
               MOVE WS-PGM-ID          TO CAW-TRAIL-PGM(CAW-TRAIL-CNT)
               MOVE ZERO               TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 1000 - THE SKELETON CACRD04 PARKED                             *
      *                                                                *
      * THE COMMAREA IS ONLY 512 BYTES AND THE VARIANT IMAGE WOULD     *
      * NOT FIT ALONGSIDE THE DECISION STATE, SO IT TRAVELS IN A TSQ   *
      * KEYED ON THE TERMINAL.  IF IT HAS GONE - A PURGE, A RESTART -  *
      * THE RECORD IS REBUILT FROM THE REQUEST FIELDS INSTEAD, WHICH   *
      * LOSES THE MERCHANT NAME AND THE CASH ADVANCE FEE.              *
      ******************************************************************
       1000-RECOVER-SKELETON.
           MOVE LOW-VALUES             TO AUTH-RECORD
           MOVE 200                    TO WS-TSQ-LEN
           MOVE 1                      TO WS-TSQ-ITEM
      *
           EXEC CICS READQ TS
                     QUEUE(WS-AUTH-TSQ)
                     INTO(AUTH-RECORD)
                     LENGTH(WS-TSQ-LEN)
                     ITEM(WS-TSQ-ITEM)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(QIDERR)
                   PERFORM 1100-REBUILD-SKELETON
               WHEN DFHRESP(ITEMERR)
                   PERFORM 1100-REBUILD-SKELETON
               WHEN OTHER
                   MOVE '1000-RECOVER-SKELETON'
                                       TO ER-PARAGRAPH
                   MOVE 'TSQ '         TO ER-ERROR-TYPE
                   MOVE 'AUTH SKELETON QUEUE READ FAILED'
                                       TO ER-MESSAGE
                   PERFORM 8100-CICS-ERROR
                   PERFORM 1100-REBUILD-SKELETON
           END-EVALUATE
           .
      *
       1100-REBUILD-SKELETON.
           MOVE SPACES                 TO AUTH-RECORD
           MOVE CAW-RQ-CARD-NUM        TO AUTH-CARD-NUM
           MOVE CAW-ACCT-ID            TO AUTH-ACCT-ID
           MOVE CAW-CUST-ID            TO AUTH-CUST-ID
           MOVE CAW-RQ-AUTH-TYPE       TO AUTH-TYPE
      *
           MOVE SPACES                 TO AUTH-DETAIL
      *
      *    THE OVERLAY IS FILLED THE SAME WAY CACRD04 WOULD HAVE DONE
      *    IT.  ONLY THE FIELDS STILL ON THE COMMAREA ARE AVAILABLE,
      *    AND THE TAIL OF A SHORT VARIANT IS LEFT AS IT LIES.
           EVALUATE AUTH-TYPE
               WHEN 'P'
                   MOVE CAW-RQ-MERCH-ID
                                       TO AP-MERCHANT-ID
                   MOVE CAW-RQ-MCC     TO AP-MCC
                   MOVE CAW-RQ-TERMINAL
                                       TO AP-TERMINAL-ID
                   MOVE CAW-RQ-AMT     TO AP-AMOUNT
                   MOVE CAW-RQ-CURR    TO AP-CURRENCY
                   MOVE CAW-RQ-ENTRY-MODE
                                       TO AP-ENTRY-MODE
               WHEN 'C'
                   MOVE CAW-RQ-ATM-ID  TO AC-ATM-ID
                   MOVE CAW-RQ-NETWORK TO AC-NETWORK
                   MOVE CAW-RQ-AMT     TO AC-AMOUNT
                   MOVE ZERO           TO AC-FEE
                   MOVE CAW-RQ-CURR    TO AC-CURRENCY
               WHEN 'R'
                   MOVE CAW-RQ-ORIG-AUTH
                                       TO AR-ORIG-AUTH-ID
                   MOVE CAW-RQ-ORIG-DATE
                                       TO AR-ORIG-DATE
                   MOVE CAW-RQ-MERCH-ID
                                       TO AR-ORIG-MERCHANT
                   MOVE CAW-RQ-AMT     TO AR-AMOUNT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
      ******************************************************************
      * 1500 - THE NEXT SEQUENCE NUMBER FOR THE CARD AND DATE          *
      *                                                                *
      * THERE IS NO SEQUENCE OBJECT ON THIS TABLE - IT PREDATES THEM.  *
      * THE MAXIMUM IS READ AND ONE IS ADDED, AND A DUPLICATE KEY ON   *
      * THE INSERT IS RETRIED.  TWO TERMINALS ON THE SAME CARD IN THE  *
      * SAME SECOND IS RARE BUT IT HAPPENS AT PETROL STATIONS.         *
      ******************************************************************
       1500-NEXT-SEQ-NBR.
           MOVE CAW-RQ-CARD-NUM        TO DCL-CARD-NUM
           MOVE ZERO                   TO DCL-AUTH-SEQ-NUM
      *
           EXEC SQL
               SELECT MAX(AUTH_SEQ_NUM)
                 INTO :DCL-AUTH-SEQ-NUM :IND-MAX-SEQ
                 FROM CARDSVC.AUTHORIZATION
                WHERE CARD_NUM  = :DCL-CARD-NUM
                  AND AUTH_DATE = CURRENT DATE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF IND-MAX-SEQ < ZERO
                       MOVE ZERO       TO DCL-AUTH-SEQ-NUM
                   END-IF
               WHEN +100
                   MOVE ZERO           TO DCL-AUTH-SEQ-NUM
               WHEN OTHER
                   MOVE '1500-NEXT-SEQ-NBR'
                                       TO ER-PARAGRAPH
                   MOVE 'AUTHORIZATION    '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
                   MOVE ZERO           TO DCL-AUTH-SEQ-NUM
           END-EVALUATE
      *
           ADD 1                       TO DCL-AUTH-SEQ-NUM
           MOVE DCL-AUTH-SEQ-NUM       TO CAW-AUTH-SEQ-NUM
                                          AUTH-SEQ-NUM
           .
      *
      ******************************************************************
      * 2000 - FINISH THE RECORD OFF                                   *
      ******************************************************************
       2000-COMPLETE-RECORD.
           MOVE CAW-RQ-CARD-NUM        TO AUTH-CARD-NUM
           MOVE CAW-AUTH-DATE          TO AUTH-DATE
           MOVE CAW-AUTH-TIME          TO AUTH-TIME
           MOVE CAW-ACCT-ID            TO AUTH-ACCT-ID
           MOVE CAW-CUST-ID            TO AUTH-CUST-ID
           MOVE CAW-DEC-STATUS         TO AUTH-STATUS
           MOVE CAW-DEC-RESP           TO AUTH-RESP-CODE
           MOVE CAW-DEC-REASON         TO AUTH-REASON-CD
           MOVE CAW-RSK-SCORE          TO AUTH-RISK-SCORE
           MOVE CAW-RSK-BAND           TO AUTH-RISK-BAND
           MOVE 'N'                    TO AUTH-SETTLED-FLG
                                          AUTH-POSTED-FLG
      *
           MOVE WS-PGM-ID              TO AUTH-ORIG-PGM
           MOVE CAW-TERM-ID            TO AUTH-TERM-ID
           MOVE CAW-OPER-ID            TO AUTH-OPER-ID
      *
           MOVE SPACES                 TO WS-TIMESTAMP
           STRING WS-DATE-YYYYMMDD     DELIMITED BY SIZE
                  '-'                  DELIMITED BY SIZE
                  WS-TIME-HHMMSS       DELIMITED BY SIZE
                  INTO WS-TIMESTAMP
           END-STRING
           MOVE WS-TIMESTAMP           TO AUTH-TIMESTAMP
           .
      *
      ******************************************************************
      * 3000 - THE ROW                                                 *
      ******************************************************************
       3000-INSERT-AUTH.
           MOVE CAW-RQ-CARD-NUM        TO DCL-CARD-NUM
           MOVE CAW-ACCT-ID            TO DCL-ACCT-ID
           MOVE CAW-CUST-ID            TO DCL-CUST-ID
           MOVE CAW-RQ-AUTH-TYPE       TO DCL-AUTH-TYPE
           MOVE CAW-DEC-STATUS         TO DCL-AUTH-STATUS
           MOVE CAW-DEC-RESP           TO DCL-RESP-CODE
           MOVE CAW-DEC-REASON         TO DCL-REASON-CD
           MOVE CAW-RSK-SCORE          TO DCL-RISK-SCORE
           MOVE CAW-RSK-BAND           TO DCL-RISK-BAND
           MOVE AUTH-DETAIL            TO DCL-AUTH-DETAIL
           MOVE WS-PGM-ID              TO DCL-ORIG-PGM
           MOVE CAW-TERM-ID            TO DCL-TERM-ID
           MOVE CAW-OPER-ID            TO DCL-OPER-ID
      *
           MOVE ZERO                   TO WS-RETRY-CNT
      *
           PERFORM 3100-INSERT-ATTEMPT
           PERFORM UNTIL SQLCODE NOT = -803
                      OR WS-RETRY-CNT >= WS-MAX-RETRY
               ADD 1                   TO WS-RETRY-CNT
               ADD 1                   TO DCL-AUTH-SEQ-NUM
               MOVE DCL-AUTH-SEQ-NUM   TO CAW-AUTH-SEQ-NUM
                                          AUTH-SEQ-NUM
               PERFORM 3100-INSERT-ATTEMPT
           END-PERFORM
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
                   MOVE 'N'            TO WS-UPDATE-OK-SW
                   MOVE '3000-INSERT-AUTH'
                                       TO ER-PARAGRAPH
                   MOVE 'AUTHORIZATION    '
                                       TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE 'AUTH SEQUENCE EXHAUSTED AFTER RETRIES'
                                       TO ER-MESSAGE
                   PERFORM 8000-SQL-ERROR
               WHEN OTHER
                   MOVE 'N'            TO WS-UPDATE-OK-SW
                   MOVE '3000-INSERT-AUTH'
                                       TO ER-PARAGRAPH
                   MOVE 'AUTHORIZATION    '
                                       TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
      *
           IF WS-UPDATE-BAD
               IF CAW-TRAIL-CNT > ZERO
                   MOVE WS-RC-FATAL    TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
               END-IF
           END-IF
           .
      *
       3100-INSERT-ATTEMPT.
           EXEC SQL
               INSERT INTO CARDSVC.AUTHORIZATION
                    ( CARD_NUM
                    , AUTH_DATE
                    , AUTH_SEQ_NUM
                    , ACCT_ID
                    , CUST_ID
                    , AUTH_TIME
                    , AUTH_TYPE
                    , AUTH_STATUS
                    , RESP_CODE
                    , REASON_CD
                    , RISK_SCORE
                    , RISK_BAND
                    , SETTLED_FLG
                    , POSTED_FLG
                    , AUTH_DETAIL
                    , ORIG_PGM
                    , TERM_ID
                    , OPER_ID )
               VALUES
                    ( :DCL-CARD-NUM
                    , CURRENT DATE
                    , :DCL-AUTH-SEQ-NUM
                    , :DCL-ACCT-ID
                    , :DCL-CUST-ID
                    , CURRENT TIME
                    , :DCL-AUTH-TYPE
                    , :DCL-AUTH-STATUS
                    , :DCL-RESP-CODE
                    , :DCL-REASON-CD
                    , :DCL-RISK-SCORE
                    , :DCL-RISK-BAND
                    , 'N'
                    , 'N'
                    , :DCL-AUTH-DETAIL
                    , :DCL-ORIG-PGM
                    , :DCL-TERM-ID
                    , :DCL-OPER-ID )
           END-EXEC
           .
      *
      ******************************************************************
      * 4000 - THE BALANCES.  APPROVALS ONLY.                          *
      *                                                                *
      * A REFUND MOVES THE OTHER WAY - IT RELEASES EXPOSURE RATHER     *
      * THAN TAKING IT.  THE PENDING FIGURE IS NOT ALLOWED TO GO       *
      * NEGATIVE BECAUSE THE SETTLEMENT FEED WOULD REJECT IT.          *
      ******************************************************************
       4000-UPDATE-BALANCES.
           MOVE CAW-RQ-AMT             TO DCL-POST-AMT
           IF CAW-RQ-AUTH-TYPE = 'R'
               COMPUTE DCL-POST-AMT = CAW-RQ-AMT * -1
           END-IF
      *
           PERFORM 4100-UPDATE-ACCOUNT
           IF WS-UPDATE-OK
               PERFORM 4200-UPDATE-LIMIT
           END-IF
           .
      *
       4100-UPDATE-ACCOUNT.
           MOVE CAW-ACCT-ID            TO DCL-ACCT-ID
      *
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET PENDING_AUTH_AMT =
                      CASE WHEN PENDING_AUTH_AMT + :DCL-POST-AMT < 0
                           THEN 0
                           ELSE PENDING_AUTH_AMT + :DCL-POST-AMT
                      END
                    , LAST_MAINT_TS = CURRENT TIMESTAMP
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
      *            THE ACCOUNT WENT AWAY BETWEEN THE STATUS STEP AND
      *            NOW - THE AUTH ROW STANDS, OPERATIONS RECONCILE
                   MOVE '4100-UPDATE-ACCOUNT'
                                       TO ER-PARAGRAPH
                   MOVE 'ACCOUNT          '
                                       TO ER-SQL-TABLE
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   MOVE 'ACCOUNT ROW NOT FOUND ON PENDING UPDATE'
                                       TO ER-MESSAGE
                   PERFORM 8000-SQL-ERROR
               WHEN OTHER
                   MOVE 'N'            TO WS-UPDATE-OK-SW
                   MOVE '4100-UPDATE-ACCOUNT'
                                       TO ER-PARAGRAPH
                   MOVE 'ACCOUNT          '
                                       TO ER-SQL-TABLE
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
       4200-UPDATE-LIMIT.
           MOVE CAW-RQ-CARD-NUM        TO DCL-CARD-NUM
           MOVE CAW-LIM-TYPE           TO DCL-LIMIT-TYPE
           IF DCL-LIMIT-TYPE = SPACES
               MOVE 'CRED'             TO DCL-LIMIT-TYPE
           END-IF
      *
           EXEC SQL
               UPDATE CARDSVC.CARD_LIMIT
                  SET USED_AMT  = USED_AMT  + :DCL-POST-AMT
                    , AVAIL_AMT = AVAIL_AMT - :DCL-POST-AMT
                    , DAILY_CNT_USED = DAILY_CNT_USED + 1
                    , LAST_MAINT_TS = CURRENT TIMESTAMP
                WHERE CARD_NUM   = :DCL-CARD-NUM
                  AND LIMIT_TYPE = :DCL-LIMIT-TYPE
                  AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
      *            NO EFFECTIVE ROW - THE LIMIT STEP ALREADY SAID SO
      *            AND THE DECISION WAS TAKEN WITHOUT ONE
                   CONTINUE
               WHEN OTHER
                   MOVE 'N'            TO WS-UPDATE-OK-SW
                   MOVE '4200-UPDATE-LIMIT'
                                       TO ER-PARAGRAPH
                   MOVE 'CARD_LIMIT       '
                                       TO ER-SQL-TABLE
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 5000 - THE ESDS LOG                                            *
      ******************************************************************
       5000-WRITE-LOG.
           MOVE SPACES                 TO AUTHLOG-RECORD
           MOVE 'AU'                   TO AL-RECORD-TYPE
           MOVE AUTH-RECORD(1:140)     TO AL-AUTH-IMAGE
           MOVE CAW-DEC-STATUS         TO AL-DEC-STATUS
           MOVE CAW-DEC-RESP           TO AL-RESP-CODE
           MOVE CAW-DEC-REASON         TO AL-REASON-CD
           MOVE CAW-RSK-SCORE          TO AL-RISK-SCORE
           MOVE CAW-RSK-BAND           TO AL-RISK-BAND
           MOVE CAW-FRAUD-SCORE        TO AL-FRAUD-SCORE
           MOVE CAW-FRAUD-ACTION       TO AL-FRAUD-ACTION
           MOVE CAW-FRAUD-RULE         TO AL-FRAUD-RULE
           MOVE CAW-LIM-TYPE           TO AL-LIM-TYPE
           MOVE CAW-VELOCITY-CNT       TO AL-VELOCITY-CNT
           MOVE CAW-TRAN-ID            TO AL-TRAN-ID
           MOVE CAW-TERM-ID            TO AL-TERM-ID
           MOVE CAW-OPER-ID            TO AL-OPER-ID
           MOVE WS-TIME-HHMMSS         TO AL-LOG-TIMESTAMP
      *
           MOVE 200                    TO WS-LOG-LEN
      *
           EXEC CICS WRITE
                     FILE(WS-LOG-FILE)
                     FROM(AUTHLOG-RECORD)
                     LENGTH(WS-LOG-LEN)
                     RIDFLD(WS-LOG-RBA)
                     RBA
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOSPACE)
                   MOVE '5000-WRITE-LOG'
                                       TO ER-PARAGRAPH
                   MOVE WS-LOG-FILE    TO ER-FILE-NAME
                   MOVE 'AUTHLOG IS FULL - CALL OPERATIONS'
                                       TO ER-MESSAGE
                   MOVE 'E'            TO ER-SEVERITY
                   PERFORM 8200-VSAM-ERROR
               WHEN DFHRESP(NOTOPEN)
                   MOVE '5000-WRITE-LOG'
                                       TO ER-PARAGRAPH
                   MOVE WS-LOG-FILE    TO ER-FILE-NAME
                   MOVE 'AUTHLOG IS CLOSED - AUTH NOT LOGGED'
                                       TO ER-MESSAGE
                   MOVE 'E'            TO ER-SEVERITY
                   PERFORM 8200-VSAM-ERROR
               WHEN OTHER
                   MOVE '5000-WRITE-LOG'
                                       TO ER-PARAGRAPH
                   MOVE WS-LOG-FILE    TO ER-FILE-NAME
                   MOVE 'AUTHLOG WRITE FAILED'
                                       TO ER-MESSAGE
                   MOVE 'E'            TO ER-SEVERITY
                   PERFORM 8200-VSAM-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 5500 - LAST USED DATE.  APPROVALS AND REFERRALS ONLY - A       *
      *        DECLINE IS NOT A USE OF THE CARD.                       *
      ******************************************************************
       5500-STAMP-CARD.
           IF CAW-DEC-DECLINED
               GO TO 5500-EXIT
           END-IF
      *
           MOVE CAW-RQ-CARD-NUM        TO DCL-CARD-NUM
      *
           EXEC SQL
               UPDATE CARDSVC.CARD
                  SET LAST_USED_DATE = CURRENT DATE
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE CARD_NUM = :DCL-CARD-NUM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE '5500-STAMP-CARD'
                                       TO ER-PARAGRAPH
                   MOVE 'CARD             '
                                       TO ER-SQL-TABLE
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
       5500-EXIT.
           EXIT
           .
      *
       5900-DELETE-TSQ.
           EXEC CICS DELETEQ TS
                     QUEUE(WS-AUTH-TSQ)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               AND WS-RESP NOT = DFHRESP(QIDERR)
               MOVE '5900-DELETE-TSQ'  TO ER-PARAGRAPH
               MOVE 'TSQ '             TO ER-ERROR-TYPE
               MOVE 'AUTH SKELETON QUEUE NOT DELETED'
                                       TO ER-MESSAGE
               PERFORM 8100-CICS-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 7000 - THE OUTCOME SCREEN                                      *
      ******************************************************************
       7000-SEND-OUTCOME.
           MOVE LOW-VALUES             TO CARDRSPO
      *
           MOVE CAW-AUTH-DATE          TO ARDATEO
           MOVE CAW-RQ-CARD-NUM        TO ARCARDO
           MOVE CAW-ACCT-ID            TO ARACCTO
      *
           MOVE CAW-RQ-AMT             TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO ARAMTO
           MOVE CAW-RQ-CURR            TO ARCURRO
      *
           MOVE CAW-DEC-STATUS         TO ARDECO
           EVALUATE TRUE
               WHEN CAW-DEC-APPROVED
                   MOVE 'APPROVED  '   TO ARDECDO
               WHEN CAW-DEC-REFERRED
                   MOVE 'REFERRED  '   TO ARDECDO
               WHEN OTHER
                   MOVE 'DECLINED  '   TO ARDECDO
           END-EVALUATE
      *
           MOVE CAW-DEC-RESP           TO ARRESPO
           MOVE CAW-DEC-REASON         TO ARRSNO
           MOVE CAW-MSG                TO ARRSNDO
      *
           MOVE CAW-AUTH-SEQ-NUM       TO WS-EDIT-SEQ
           MOVE WS-EDIT-SEQ            TO ARSEQO
      *
           MOVE CAW-RSK-SCORE          TO ARSCORO
           MOVE CAW-RSK-BAND           TO ARBANDO
           MOVE CAW-RSK-ADVICE         TO ARADVO
      *
           MOVE CAW-FRAUD-SCORE        TO ARFSCRO
           MOVE CAW-FRAUD-ACTION       TO ARFACTO
           MOVE CAW-FRAUD-RULE         TO ARFRULO
      *
           MOVE CAW-LIM-TYPE           TO ARLTYPO
           MOVE CAW-LIM-AVAIL          TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO ARLAVLO
           MOVE CAW-VELOCITY-CNT       TO ARVELO
      *
           IF WS-UPDATE-BAD
               MOVE 'DECISION NOT RECORDED - CALL OPERATIONS'
                                       TO ARMSGO
           ELSE
               MOVE CAW-MSG            TO ARMSGO
           END-IF
      *
           EXEC CICS SEND
                     MAP(WS-MAP-RSP)
                     MAPSET(WS-MAPSET)
                     FROM(CARDRSPO)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '7000-SEND-OUTCOME'
                                       TO ER-PARAGRAPH
               PERFORM 8100-CICS-ERROR
           END-IF
      *
           MOVE 'CARDRSP '             TO CAW-SCREEN-ID
           .
      *
      ******************************************************************
      * 8500 - PASS CONTROL ON                                         *
      ******************************************************************
       8500-CONTINUE-CHAIN.
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
           EXEC CICS XCTL
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           MOVE '8500-CONTINUE-CHAIN'  TO ER-PARAGRAPH
           PERFORM 8100-CICS-ERROR
           .
      *
      ******************************************************************
      * 8000 - ERROR HANDLING                                          *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           IF ER-SEVERITY = SPACE
               MOVE 'E'                TO ER-SEVERITY
           END-IF
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           IF ER-MESSAGE = SPACES
               MOVE 'AUTHORIZATION WRITE FAILED'
                                       TO ER-MESSAGE
           END-IF
           PERFORM 8900-LINK-ERROR-PGM
           .
      *
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           IF ER-ERROR-TYPE = SPACES
               MOVE 'CICS'             TO ER-ERROR-TYPE
           END-IF
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
           MOVE WS-RESP                TO ER-EIBRESP
                                          ER-VSAM-RC
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE AUTH-CARD-NUM          TO ER-VSAM-KEY
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
      *
           MOVE SPACES                 TO ER-MESSAGE
                                          ER-ERROR-TYPE
           .
      *
       9100-NO-COMMAREA.
           EXEC CICS SEND
                     TEXT('CACRD10 IS A CHAIN STEP - START WITH CA00')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           .
