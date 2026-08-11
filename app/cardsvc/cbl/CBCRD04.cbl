      ******************************************************************
      * CBCRD04 - NIGHTLY TRANSACTION POSTING ENGINE                   *
      *                                                                *
      * STEP 4 OF THE CARDNITE CYCLE.  JOB CBCRD04J.                   *
      *                                                                *
      * THE ENRICHED AUTHORISATION FILE IS POSTED INTO                 *
      * CARDSVC.TRANSACTION, THE ACCOUNT BALANCES ARE MOVED AND THE    *
      * SOURCE AUTHORISATION IS STAMPED POSTED.  EVERY POSTING BUILDS  *
      * A TXN-RECORD FROM CVTRAN01Y - THE LEG TABLE IS AN OCCURS       *
      * DEPENDING ON, SO THE TRAILER IS MOVED INTO PLACE BY REFERENCE  *
      * MODIFICATION AFTER TXN-LEG-CNT IS KNOWN.                       *
      *                                                                *
      *   PURCHASE      ONE PRINCIPAL LEG, PLUS AN FX LEG WHEN THE     *
      *                 AUTHORISATION CURRENCY IS NOT THE BILLING      *
      *                 CURRENCY OF THE ACCOUNT                        *
      *   CASH ADVANCE  PRINCIPAL LEG PLUS A CASH FEE LEG              *
      *   REFUND        A SINGLE REVERSING PRINCIPAL LEG               *
      *                                                                *
      * CHECKPOINT / RESTART                                           *
      *   A COMMIT IS TAKEN EVERY CC-COMMIT-FREQ POSTINGS.  THE KEY    *
      *   OF THE LAST COMMITTED AUTHORISATION IS HELD IN CC-LAST-KEY   *
      *   ON THE CYCLE CONTROL RECORD, WHICH IS REWRITTEN IN THE SAME  *
      *   CHECKPOINT WINDOW.  ON A RESTART THE INPUT FILE IS READ      *
      *   FORWARD AND SKIPPED UNTIL THAT KEY IS PASSED.  RUNNING THE   *
      *   STEP WITH PARM RESTART=N IGNORES THE SAVED KEY AND STARTS    *
      *   FROM THE TOP - ONLY VALID AFTER THE OPERATOR HAS BACKED OUT  *
      *   THE CYCLE'S POSTINGS.                                        *
      *                                                                *
      * PARM  - CCYYMMDD[,RESTART=Y|N]                                 *
      *                                                                *
      * CALLED BY   - JCL ONLY (IKJEFT01 / DSN RUN)                    *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - AUTHENR  QSAM INPUT   LRECL 300                  *
      *             - AUTHREJ  QSAM OUTPUT  LRECL 300 (POST REJECTS)   *
      *             - CYCLCTL  VSAM KSDS I-O                           *
      * TABLES      - CARDSVC.TRANSACTION   (INSERT)                   *
      *             - CARDSVC.ACCOUNT       (SELECT / UPDATE)          *
      *             - CARDSVC.CARD_LIMIT    (SELECT / UPDATE)          *
      *             - CARDSVC.AUTHORIZATION (UPDATE)                   *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 ALL POSTED                                  *
      *               0004 POSTED WITH REJECTS                         *
      *               0012 FATAL                                       *
      * USER ABEND  - U0401 CYCLE CONTROL UNUSABLE                     *
      *               U0402 FILE OPEN OR I/O FAILURE                   *
      *               U0403 UNRECOVERABLE SQL ERROR                    *
      *               U0404 RESTART KEY NOT FOUND ON THE INPUT FILE    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD04.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1998-05-19.
      *
      * MAINTENANCE
      * 1998-05-19 CRD0130 ORIGINAL
      * 2000-11-27 CRD1904 CHECKPOINT RESTART ADDED AFTER THE OCT 2000
      *                    CYCLE HAD TO BE RE-RUN FROM THE START
      * 2004-03-08 CRD3912 FX LEG SPLIT OUT OF THE PRINCIPAL LEG
      * 2010-07-14 CRD6903 CASH FEE LEG NO LONGER POSTED WHEN THE
      *                    ACQUIRER FEE IS ZERO
      * 2016-02-29 CRD9401 PENDING AUTHORISATION AMOUNT RELEASED AT
      *                    POSTING TIME
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENRICH-FILE   ASSIGN TO AUTHENR
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-ENRICH-STATUS.
      *
           SELECT REJECT-FILE   ASSIGN TO AUTHREJ
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REJECT-STATUS.
      *
           SELECT CYCLCTL-FILE  ASSIGN TO CYCLCTL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CTL-KEY
                  FILE STATUS  IS WS-CYCLCTL-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  ENRICH-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       01  ENRICH-REC                      PIC X(300).
      *
       FD  REJECT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       01  REJECT-REC                      PIC X(300).
      *
       FD  CYCLCTL-FILE
           RECORD CONTAINS 256 CHARACTERS.
       01  CYCLCTL-REC.
           05  CTL-KEY.
               10  CTL-CYCLE-TYPE          PIC X(8).
               10  CTL-CYCLE-DATE          PIC 9(8).
           05  CTL-REST                    PIC X(240).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD04 '.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-ENRICH-STATUS            PIC X(2)  VALUE '00'.
               88  WS-ENRICH-OK                      VALUE '00'.
           05  WS-REJECT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REJECT-OK                      VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-RESTART-SW               PIC X     VALUE 'N'.
               88  WS-RESTARTING                     VALUE 'Y'.
           05  WS-SKIPPING-SW              PIC X     VALUE 'N'.
               88  WS-SKIPPING                       VALUE 'Y'.
           05  WS-REJECT-SW                PIC X     VALUE 'N'.
               88  WS-POST-REJECTED                  VALUE 'Y'.
           05  WS-ACCT-FOUND-SW            PIC X     VALUE 'N'.
               88  WS-ACCT-FOUND                     VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-SKIPPED-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-POSTED-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-LEG-CNT-TOT              PIC 9(9)  VALUE ZERO.
           05  WS-COMMIT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-SINCE-COMMIT             PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-SEQ               PIC 9(9)  VALUE ZERO.
           05  WS-ACCT-UPD-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-AUTH-UPD-CNT             PIC 9(9)  VALUE ZERO.
      *
       01  WS-TOTALS.
           05  WS-DEBIT-TOTAL              PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-CREDIT-TOTAL             PIC S9(13)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-HASH-TOTAL               PIC S9(15) COMP-3
                                                     VALUE ZERO.
      *
       01  WS-PARM-FIELDS.
           05  WS-CYCLE-DATE               PIC 9(8)  VALUE ZERO.
           05  WS-RESTART-REQ              PIC X     VALUE 'Y'.
      *
       01  WS-RESTART-KEY                  PIC X(32) VALUE SPACES.
       01  WS-CURRENT-KEY                  PIC X(32) VALUE SPACES.
       01  WS-CURRENT-KEY-R REDEFINES WS-CURRENT-KEY.
           05  WS-CK-CARD-NUM              PIC X(16).
           05  WS-CK-SEQ-NUM               PIC 9(9).
           05  WS-CK-FILLER                PIC X(7).
      *
       01  WS-WORK-FIELDS.
           05  WS-TXN-ID                   PIC X(16) VALUE SPACES.
           05  WS-TXN-ID-R REDEFINES WS-TXN-ID.
               10  WS-TXN-PREFIX           PIC X(2).
               10  WS-TXN-DATE-PART        PIC 9(6).
               10  WS-TXN-SERIAL           PIC 9(8).
           05  WS-TXN-SERIAL-NBR           PIC 9(8)  VALUE ZERO.
           05  WS-LEG-SUB                  PIC S9(4) COMP VALUE ZERO.
           05  WS-LEG-LEN                  PIC S9(4) COMP VALUE ZERO.
           05  WS-TRAILER-OFF              PIC S9(4) COMP VALUE ZERO.
           05  WS-LEG-DATA-LEN             PIC S9(4) COMP VALUE ZERO.
           05  WS-PRINCIPAL-AMT            PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-FEE-AMT                  PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-FX-AMT                   PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-BILLING-AMT              PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-FX-RATE                  PIC S9(3)V9(5) COMP-3
                                                     VALUE 1.00000.
           05  WS-SIGN-MULT                PIC S9(1) COMP-3 VALUE +1.
      *
       01  WS-CURRENT-DATE.
           05  WS-CD-DATE                  PIC 9(8).
           05  WS-CD-TIME                  PIC 9(8).
           05  WS-CD-FILLER                PIC X(5).
       01  WS-TIMESTAMP                    PIC X(26) VALUE SPACES.
      *
       01  WS-RETURN-CODE                  PIC 9(4)  VALUE ZERO.
       01  WS-ABEND-CODE                   PIC 9(4)  VALUE ZERO.
      *
           COPY CVAXTR01Y.
           COPY CVRJCT01Y.
           COPY CVAUTH01Y.
           COPY CVTRAN01Y.
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      *    THE LEG TABLE IMAGE HANDED TO DB2 AS A VARCHAR.  ONE LEG IS
      *    2 + 4 + 7 + 10 + 1 = 24 BYTES, TWELVE LEGS MAXIMUM.
       01  WS-LEG-DATA.
           05  WS-LEG-DATA-LN              PIC S9(4) COMP.
           05  WS-LEG-DATA-TX              PIC X(400).
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-ACCOUNT.
           05  DCL-ACCT-ID                 PIC S9(11)    COMP-3.
           05  DCL-CURR-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-CASH-BAL                PIC S9(11)V99 COMP-3.
           05  DCL-PENDING-AUTH            PIC S9(11)V99 COMP-3.
           05  DCL-CURRENCY-CD             PIC X(3).
           05  DCL-ACCT-STATUS             PIC X(1).
           05  DCL-PRODUCT-CD              PIC X(4).
           05  DCL-PARTY-ID                PIC X(11).
      *
       01  DCL-TXN.
           05  DCL-TXN-ID                  PIC X(16).
           05  DCL-POST-DATE               PIC X(10).
           05  DCL-CARD-NUM                PIC X(16).
           05  DCL-AUTH-SEQ                PIC S9(9)     COMP-3.
           05  DCL-TXN-TYPE-CD             PIC X(4).
           05  DCL-TXN-SOURCE              PIC X(2).
           05  DCL-TXN-AMT                 PIC S9(11)V99 COMP-3.
           05  DCL-CURRENCY                PIC X(3).
           05  DCL-BILLING-AMT             PIC S9(11)V99 COMP-3.
           05  DCL-FX-RATE                 PIC S9(3)V9(5) COMP-3.
           05  DCL-MERCHANT-ID             PIC X(15).
           05  DCL-MCC                     PIC X(4).
           05  DCL-TXN-DESC                PIC X(40).
           05  DCL-LEG-CNT                 PIC S9(4) COMP.
           05  DCL-CYCLE-ID                PIC X(8).
           05  DCL-POSTED-BY               PIC X(8).
      *
       01  DCL-AUTH-KEY.
           05  DCL-AUTH-DATE               PIC X(10).
      *
       01  WS-DISPLAY-CNT                  PIC ZZZ,ZZZ,ZZ9.
       01  WS-DISPLAY-AMT                  PIC ---,---,---,--9.99.
      *
       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-PARM-LEN                 PIC S9(4) COMP.
           05  LK-PARM-DATA                PIC X(20).
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-PARM.
      *
       0000-MAIN-LINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-POST-RECORD
               UNTIL WS-EOF
           PERFORM 3000-TERMINATE
           MOVE WS-RETURN-CODE             TO RETURN-CODE
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - INITIALISATION                                          *
      ******************************************************************
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           STRING WS-CD-DATE(1:4) '-' WS-CD-DATE(5:2) '-'
                  WS-CD-DATE(7:2) '-' WS-CD-TIME(1:2) '.'
                  WS-CD-TIME(3:2) '.' WS-CD-TIME(5:2) '.000000'
             DELIMITED BY SIZE INTO WS-TIMESTAMP
           END-STRING
      *
           PERFORM 1100-EDIT-PARM
           PERFORM 1200-OPEN-FILES
           PERFORM 1300-READ-CYCLE-CONTROL
           PERFORM 1400-SET-RESTART-POSITION
      *
           MOVE 'PS'                       TO WS-TXN-PREFIX
           MOVE WS-CYCLE-DATE(3:6)         TO WS-TXN-DATE-PART
      *
           DISPLAY 'CBCRD04 - POSTING ENGINE, CYCLE ' WS-CYCLE-DATE
           DISPLAY '          COMMIT FREQUENCY ' CC-COMMIT-FREQ
           IF WS-RESTARTING
               DISPLAY '          RESTART FROM KEY ' WS-RESTART-KEY
           END-IF
           .
      *
       1100-EDIT-PARM.
           IF LK-PARM-LEN < 8
               MOVE 'PARM MUST SUPPLY CCYYMMDD CYCLE DATE'
                                           TO ER-MESSAGE
               MOVE 0401                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
      *
           IF LK-PARM-LEN > 17
               IF LK-PARM-DATA(10:8) = 'RESTART='
                   MOVE LK-PARM-DATA(18:1) TO WS-RESTART-REQ
               END-IF
           END-IF
           .
      *
       1200-OPEN-FILES.
           OPEN INPUT  ENRICH-FILE
           OPEN EXTEND REJECT-FILE
           OPEN I-O    CYCLCTL-FILE
      *
           IF NOT WS-ENRICH-OK
               MOVE 'AUTHENR '             TO ER-FILE-NAME
               MOVE WS-ENRICH-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF ENRICHED FILE FAILED' TO ER-MESSAGE
               MOVE 0402                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0401                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       1300-READ-CYCLE-CONTROL.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 0401               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
      *
           MOVE WS-STEP-NAME               TO CC-CURRENT-STEP
           IF CC-COMMIT-FREQ = ZERO
               MOVE WS-COMMIT-FREQUENCY    TO CC-COMMIT-FREQ
           END-IF
           .
      *
      ******************************************************************
      * 1400 - RESTART POSITIONING                                     *
      *                                                                *
      * A NON BLANK CC-LAST-KEY MEANS A PREVIOUS RUN COMMITTED PART OF *
      * THE FILE.  THE INPUT IS READ FORWARD AND DISCARDED UNTIL THAT  *
      * KEY HAS BEEN PASSED.  DB2 HAS ALREADY COMMITTED THOSE ROWS SO  *
      * NOTHING IS BACKED OUT HERE.                                    *
      ******************************************************************
       1400-SET-RESTART-POSITION.
           MOVE CC-LAST-KEY                TO WS-RESTART-KEY
      *
           IF WS-RESTART-REQ = 'N'
               MOVE SPACES                 TO WS-RESTART-KEY
               DISPLAY 'CBCRD04 - RESTART SUPPRESSED BY PARM, '
                       'POSTING FROM TOP OF FILE'
               GO TO 1400-EXIT
           END-IF
      *
           IF WS-RESTART-KEY = SPACES
               GO TO 1400-EXIT
           END-IF
      *
           MOVE 'Y'                        TO WS-RESTART-SW
           MOVE 'Y'                        TO WS-SKIPPING-SW
           MOVE 'S'                        TO CC-STATUS
           .
       1400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - POST ONE AUTHORISATION                                  *
      ******************************************************************
       2000-POST-RECORD.
           READ ENRICH-FILE INTO AUTH-EXTRACT-REC
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
                   GO TO 2000-EXIT
           END-READ
      *
           ADD 1                           TO WS-READ-CNT
           MOVE AX-AUTH-IMAGE              TO AUTH-RECORD
           MOVE SPACES                     TO WS-CURRENT-KEY
           MOVE AUTH-CARD-NUM              TO WS-CK-CARD-NUM
           MOVE AUTH-SEQ-NUM               TO WS-CK-SEQ-NUM
      *
           IF WS-SKIPPING
               PERFORM 2050-CHECK-RESTART-KEY
               GO TO 2000-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-REJECT-SW
           PERFORM 2100-READ-ACCOUNT
      *
           IF NOT WS-POST-REJECTED
               PERFORM 2200-BUILD-TRANSACTION
           END-IF
      *
           IF NOT WS-POST-REJECTED
               PERFORM 2300-INSERT-TRANSACTION
           END-IF
      *
           IF NOT WS-POST-REJECTED
               PERFORM 2400-UPDATE-ACCOUNT
               PERFORM 2500-UPDATE-AUTHORISATION
           END-IF
      *
           IF WS-POST-REJECTED
               PERFORM 2700-WRITE-POST-REJECT
           ELSE
               ADD 1                       TO WS-POSTED-CNT
                                              WS-SINCE-COMMIT
               ADD AUTH-SEQ-NUM            TO WS-HASH-TOTAL
           END-IF
      *
           IF WS-SINCE-COMMIT >= CC-COMMIT-FREQ
               PERFORM 2800-TAKE-CHECKPOINT
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2050 - SKIP FORWARD ON A RESTART.  THE KEY IS THE CARD NUMBER  *
      *        AND AUTHORISATION SEQUENCE OF THE LAST COMMITTED ROW.   *
      ******************************************************************
       2050-CHECK-RESTART-KEY.
           ADD 1                           TO WS-SKIPPED-CNT
           IF WS-CURRENT-KEY = WS-RESTART-KEY
               MOVE 'N'                    TO WS-SKIPPING-SW
               DISPLAY 'CBCRD04 - RESTART POSITION REACHED AFTER '
                       WS-SKIPPED-CNT ' RECORDS'
           END-IF
           .
      *
      ******************************************************************
      * 2100 - ACCOUNT LOOK UP.  A MISSING OR CLOSED ACCOUNT IS A      *
      *        POSTING REJECT, NOT AN ABEND - THE CYCLE MUST FINISH.   *
      ******************************************************************
       2100-READ-ACCOUNT.
           MOVE 'N'                        TO WS-ACCT-FOUND-SW
           MOVE AUTH-ACCT-ID               TO DCL-ACCT-ID
      *
           EXEC SQL
               SELECT CURR_BAL
                    , CASH_BAL
                    , PENDING_AUTH_AMT
                    , CURRENCY_CD
                    , ACCT_STATUS
                    , PRODUCT_CD
                    , PARTY_ID
                 INTO :DCL-CURR-BAL
                    , :DCL-CASH-BAL
                    , :DCL-PENDING-AUTH
                    , :DCL-CURRENCY-CD
                    , :DCL-ACCT-STATUS
                    , :DCL-PRODUCT-CD
                    , :DCL-PARTY-ID
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_ID = :DCL-ACCT-ID
                  FOR UPDATE OF CURR_BAL
                              , CASH_BAL
                              , PENDING_AUTH_AMT
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'                TO WS-ACCT-FOUND-SW
               WHEN 100
                   MOVE 'R007'             TO AX-EDIT-REASON
                   MOVE 'Y'                TO WS-REJECT-SW
               WHEN OTHER
                   MOVE 'ACCOUNT          ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '          TO ER-SQL-OPERATION
                   MOVE 'SELECT ON CARDSVC.ACCOUNT FAILED'
                                            TO ER-MESSAGE
                   MOVE 0403                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
      *
           IF WS-ACCT-FOUND
           AND DCL-ACCT-STATUS = 'C' OR 'W'
               MOVE 'R007'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
           END-IF
           .
      *
      ******************************************************************
      * 2200 - BUILD THE TXN-RECORD                                    *
      ******************************************************************
       2200-BUILD-TRANSACTION.
           INITIALIZE TXN-RECORD
           MOVE 1                          TO TXN-LEG-CNT
           MOVE ZERO                       TO WS-PRINCIPAL-AMT
                                              WS-FEE-AMT
                                              WS-FX-AMT
           MOVE 1.00000                    TO WS-FX-RATE
           MOVE +1                         TO WS-SIGN-MULT
      *
           PERFORM 2210-GENERATE-TXN-ID
      *
           MOVE WS-TXN-ID                  TO TXN-ID
           MOVE WS-CYCLE-DATE              TO TXN-POST-DATE
           MOVE AUTH-ACCT-ID               TO TXN-ACCT-ID
           MOVE AUTH-CARD-NUM              TO TXN-CARD-NUM
           MOVE AUTH-SEQ-NUM               TO TXN-AUTH-SEQ-NUM
           MOVE 'BT'                       TO TXN-SOURCE
           MOVE AX-MCC                     TO TXN-MCC
      *
           EVALUATE TRUE
               WHEN AUTH-PURCHASE-TYPE
                   PERFORM 2220-BUILD-PURCHASE
               WHEN AUTH-CASH-ADV-TYPE
                   PERFORM 2230-BUILD-CASH-ADV
               WHEN AUTH-REFUND-TYPE
                   PERFORM 2240-BUILD-REFUND
               WHEN OTHER
                   MOVE 'R001'             TO AX-EDIT-REASON
                   MOVE 'Y'                TO WS-REJECT-SW
                   GO TO 2200-EXIT
           END-EVALUATE
      *
           PERFORM 2280-BUILD-TRAILER
           PERFORM 2290-FLATTEN-LEGS
           ADD TXN-LEG-CNT                 TO WS-LEG-CNT-TOT
           .
       2200-EXIT.
           EXIT
           .
      *
       2210-GENERATE-TXN-ID.
           ADD 1                           TO WS-TXN-SERIAL-NBR
           MOVE WS-TXN-SERIAL-NBR          TO WS-TXN-SERIAL
           .
      *
      ******************************************************************
      * 2220 - PURCHASE.  ONE PRINCIPAL LEG.  WHEN THE AUTHORISATION   *
      *        CURRENCY DIFFERS FROM THE ACCOUNT BILLING CURRENCY THE  *
      *        DIFFERENCE IS CARRIED AS A SEPARATE FX LEG SO THE       *
      *        STATEMENT CAN SHOW IT - SEE CRD3912.                    *
      ******************************************************************
       2220-BUILD-PURCHASE.
           MOVE AP-AMOUNT                  TO WS-PRINCIPAL-AMT
           MOVE 'PURC'                     TO TXN-TYPE-CD
           MOVE AP-CURRENCY                TO TXN-CURRENCY
           MOVE AP-MERCHANT-ID             TO TXN-MERCHANT-ID
           MOVE AX-MERCH-NAME              TO TXN-DESCRIPTION
      *
           IF AP-CURRENCY = DCL-CURRENCY-CD
               MOVE WS-PRINCIPAL-AMT       TO WS-BILLING-AMT
               MOVE 1.00000                TO WS-FX-RATE
           ELSE
               PERFORM 2250-CONVERT-CURRENCY
           END-IF
      *
           MOVE WS-PRINCIPAL-AMT           TO TXN-AMOUNT
           MOVE WS-BILLING-AMT             TO TXN-BILLING-AMT
           MOVE WS-FX-RATE                 TO TXN-FX-RATE
      *
           MOVE 1                          TO TXN-LEG-CNT
           MOVE 01                         TO TXN-LEG-SEQ(1)
           MOVE 'PRIN'                     TO TXN-LEG-TYPE(1)
           MOVE WS-BILLING-AMT             TO TXN-LEG-AMT(1)
           MOVE '1100100100'               TO TXN-LEG-GL-ACCT(1)
           MOVE 'N'                        TO TXN-LEG-REVERSED(1)
      *
           IF WS-FX-AMT NOT = ZERO
               ADD 1                       TO TXN-LEG-CNT
               MOVE 02                     TO TXN-LEG-SEQ(2)
               MOVE 'FXAD'                 TO TXN-LEG-TYPE(2)
               MOVE WS-FX-AMT              TO TXN-LEG-AMT(2)
               MOVE '1100100900'           TO TXN-LEG-GL-ACCT(2)
               MOVE 'N'                    TO TXN-LEG-REVERSED(2)
           END-IF
      *
           ADD WS-BILLING-AMT              TO WS-DEBIT-TOTAL
           .
      *
      ******************************************************************
      * 2230 - CASH ADVANCE.  PRINCIPAL PLUS THE ACQUIRER FEE.  THE    *
      *        FEE LEG IS SUPPRESSED WHEN THE FEE IS ZERO - CRD6903.   *
      ******************************************************************
       2230-BUILD-CASH-ADV.
           MOVE AC-AMOUNT                  TO WS-PRINCIPAL-AMT
           MOVE AC-FEE                     TO WS-FEE-AMT
           MOVE 'CASH'                     TO TXN-TYPE-CD
           MOVE AC-CURRENCY                TO TXN-CURRENCY
           MOVE SPACES                     TO TXN-MERCHANT-ID
           STRING 'ATM CASH ADVANCE ' AC-ATM-ID
             DELIMITED BY SIZE INTO TXN-DESCRIPTION
           END-STRING
      *
           IF AC-CURRENCY = DCL-CURRENCY-CD
               MOVE WS-PRINCIPAL-AMT       TO WS-BILLING-AMT
               MOVE 1.00000                TO WS-FX-RATE
           ELSE
               PERFORM 2250-CONVERT-CURRENCY
           END-IF
      *
           MOVE WS-PRINCIPAL-AMT           TO TXN-AMOUNT
           MOVE WS-BILLING-AMT             TO TXN-BILLING-AMT
           MOVE WS-FX-RATE                 TO TXN-FX-RATE
      *
           MOVE 1                          TO TXN-LEG-CNT
           MOVE 01                         TO TXN-LEG-SEQ(1)
           MOVE 'PRIN'                     TO TXN-LEG-TYPE(1)
           MOVE WS-BILLING-AMT             TO TXN-LEG-AMT(1)
           MOVE '1100200100'               TO TXN-LEG-GL-ACCT(1)
           MOVE 'N'                        TO TXN-LEG-REVERSED(1)
      *
           IF WS-FEE-AMT > ZERO
               ADD 1                       TO TXN-LEG-CNT
               MOVE 02                     TO TXN-LEG-SEQ(2)
               MOVE 'FEE '                 TO TXN-LEG-TYPE(2)
               MOVE WS-FEE-AMT             TO TXN-LEG-AMT(2)
               MOVE '4100300100'           TO TXN-LEG-GL-ACCT(2)
               MOVE 'N'                    TO TXN-LEG-REVERSED(2)
           END-IF
      *
           IF WS-FX-AMT NOT = ZERO
               ADD 1                       TO TXN-LEG-CNT
               MOVE TXN-LEG-CNT            TO TXN-LEG-SEQ(TXN-LEG-CNT)
               MOVE 'FXAD'                 TO TXN-LEG-TYPE(TXN-LEG-CNT)
               MOVE WS-FX-AMT              TO TXN-LEG-AMT(TXN-LEG-CNT)
               MOVE '1100200900'
                                       TO TXN-LEG-GL-ACCT(TXN-LEG-CNT)
               MOVE 'N'
                                      TO TXN-LEG-REVERSED(TXN-LEG-CNT)
           END-IF
      *
           ADD WS-BILLING-AMT              TO WS-DEBIT-TOTAL
           ADD WS-FEE-AMT                  TO WS-DEBIT-TOTAL
           .
      *
      ******************************************************************
      * 2240 - REFUND.  A SINGLE REVERSING PRINCIPAL LEG.  THE SIGN    *
      *        IS CARRIED ON THE AMOUNT, NOT ON THE LEG TYPE.          *
      ******************************************************************
       2240-BUILD-REFUND.
           MOVE -1                         TO WS-SIGN-MULT
           COMPUTE WS-PRINCIPAL-AMT = AR-AMOUNT * WS-SIGN-MULT
           MOVE 'RFND'                     TO TXN-TYPE-CD
           MOVE DCL-CURRENCY-CD            TO TXN-CURRENCY
           MOVE AR-ORIG-MERCHANT           TO TXN-MERCHANT-ID
           STRING 'REFUND OF ' AR-ORIG-AUTH-ID
             DELIMITED BY SIZE INTO TXN-DESCRIPTION
           END-STRING
      *
           MOVE WS-PRINCIPAL-AMT           TO WS-BILLING-AMT
           MOVE 1.00000                    TO WS-FX-RATE
           MOVE WS-PRINCIPAL-AMT           TO TXN-AMOUNT
           MOVE WS-BILLING-AMT             TO TXN-BILLING-AMT
           MOVE WS-FX-RATE                 TO TXN-FX-RATE
      *
           MOVE 1                          TO TXN-LEG-CNT
           MOVE 01                         TO TXN-LEG-SEQ(1)
           MOVE 'PRIN'                     TO TXN-LEG-TYPE(1)
           MOVE WS-BILLING-AMT             TO TXN-LEG-AMT(1)
           MOVE '1100100100'               TO TXN-LEG-GL-ACCT(1)
           MOVE 'Y'                        TO TXN-LEG-REVERSED(1)
      *
           COMPUTE WS-CREDIT-TOTAL =
                   WS-CREDIT-TOTAL + (WS-BILLING-AMT * -1)
           .
      *
      ******************************************************************
      * 2250 - CROSS CURRENCY.  THE RATE COMES FROM THE ENRICHMENT     *
      *        STEP WHERE ONE WAS AVAILABLE, OTHERWISE THE STANDING    *
      *        RATE OF ONE IS USED AND THE DIFFERENCE IS ZERO.         *
      ******************************************************************
       2250-CONVERT-CURRENCY.
           MOVE 1.00000                    TO WS-FX-RATE
           COMPUTE WS-BILLING-AMT ROUNDED =
                   WS-PRINCIPAL-AMT * WS-FX-RATE
           COMPUTE WS-FX-AMT =
                   WS-BILLING-AMT - WS-PRINCIPAL-AMT
           .
      *
       2280-BUILD-TRAILER.
           MOVE WS-PROGRAM-ID              TO TXN-POSTED-BY
           MOVE WS-TIMESTAMP               TO TXN-POSTED-TS
           MOVE AX-CYCLE-ID                TO TXN-CYCLE-ID
           MOVE 'N'                        TO TXN-GL-POSTED-FLG
           MOVE 'N'                        TO TXN-DISPUTE-FLG
           MOVE SPACES                     TO TXN-TRAILER-FILLER
           .
      *
      ******************************************************************
      * 2290 - FLATTEN THE ODO LEG TABLE INTO THE VARCHAR HANDED TO    *
      *        DB2.  THE OCCURRENCE LENGTH IS FIXED AT 24 BYTES SO     *
      *        THE LENGTH IS SIMPLY THE COUNT TIMES TWENTY FOUR.       *
      ******************************************************************
       2290-FLATTEN-LEGS.
           MOVE SPACES                     TO WS-LEG-DATA-TX
           COMPUTE WS-LEG-DATA-LEN = TXN-LEG-CNT * 24
           MOVE WS-LEG-DATA-LEN            TO WS-LEG-DATA-LN
      *
           PERFORM VARYING WS-LEG-SUB FROM 1 BY 1
                     UNTIL WS-LEG-SUB > TXN-LEG-CNT
               COMPUTE WS-TRAILER-OFF = ((WS-LEG-SUB - 1) * 24) + 1
               MOVE TXN-LEG(WS-LEG-SUB)
                                  TO WS-LEG-DATA-TX(WS-TRAILER-OFF:24)
           END-PERFORM
           .
      *
      ******************************************************************
      * 2300 - INSERT THE POSTED TRANSACTION                           *
      ******************************************************************
       2300-INSERT-TRANSACTION.
           MOVE TXN-ID                     TO DCL-TXN-ID
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO DCL-POST-DATE
           END-STRING
           MOVE TXN-CARD-NUM               TO DCL-CARD-NUM
           MOVE TXN-AUTH-SEQ-NUM           TO DCL-AUTH-SEQ
           MOVE TXN-TYPE-CD                TO DCL-TXN-TYPE-CD
           MOVE TXN-SOURCE                 TO DCL-TXN-SOURCE
           MOVE TXN-AMOUNT                 TO DCL-TXN-AMT
           MOVE TXN-CURRENCY               TO DCL-CURRENCY
           MOVE TXN-BILLING-AMT            TO DCL-BILLING-AMT
           MOVE TXN-FX-RATE                TO DCL-FX-RATE
           MOVE TXN-MERCHANT-ID            TO DCL-MERCHANT-ID
           MOVE TXN-MCC                    TO DCL-MCC
           MOVE TXN-DESCRIPTION            TO DCL-TXN-DESC
           MOVE TXN-LEG-CNT                TO DCL-LEG-CNT
           MOVE TXN-CYCLE-ID               TO DCL-CYCLE-ID
           MOVE WS-PROGRAM-ID              TO DCL-POSTED-BY
      *
           EXEC SQL
               INSERT INTO CARDSVC.TRANSACTION
                     (TXN_ID
                    , POST_DATE
                    , ACCT_ID
                    , CARD_NUM
                    , AUTH_SEQ_NUM
                    , TXN_TYPE_CD
                    , TXN_SOURCE
                    , TXN_AMT
                    , CURRENCY_CD
                    , BILLING_AMT
                    , FX_RATE
                    , MERCHANT_ID
                    , MCC
                    , TXN_DESC
                    , TXN_LEG_CNT
                    , TXN_LEG_DATA
                    , CYCLE_ID
                    , GL_POSTED_FLG
                    , DISPUTE_FLG
                    , POSTED_BY)
               VALUES (:DCL-TXN-ID
                    , DATE(:DCL-POST-DATE)
                    , :DCL-ACCT-ID
                    , :DCL-CARD-NUM
                    , :DCL-AUTH-SEQ
                    , :DCL-TXN-TYPE-CD
                    , :DCL-TXN-SOURCE
                    , :DCL-TXN-AMT
                    , :DCL-CURRENCY
                    , :DCL-BILLING-AMT
                    , :DCL-FX-RATE
                    , :DCL-MERCHANT-ID
                    , :DCL-MCC
                    , :DCL-TXN-DESC
                    , :DCL-LEG-CNT
                    , :WS-LEG-DATA
                    , :DCL-CYCLE-ID
                    , 'N'
                    , 'N'
                    , :DCL-POSTED-BY)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
      *            A DUPLICATE MEANS THIS AUTHORISATION WAS POSTED BY
      *            A PREVIOUS RUN THAT FAILED AFTER THE INSERT BUT
      *            BEFORE THE CHECKPOINT.  TREAT IT AS ALREADY DONE.
                   MOVE 'R008'             TO AX-EDIT-REASON
                   MOVE 'Y'                TO WS-REJECT-SW
                   DISPLAY 'CBCRD04 DUPLICATE TXN SKIPPED '
                           TXN-ID ' AUTH SEQ ' AUTH-SEQ-NUM
               WHEN OTHER
                   MOVE 'TRANSACTION      ' TO ER-SQL-TABLE
                   MOVE 'INSERT  '          TO ER-SQL-OPERATION
                   MOVE 'INSERT INTO CARDSVC.TRANSACTION FAILED'
                                            TO ER-MESSAGE
                   MOVE 0403                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2400 - MOVE THE ACCOUNT BALANCES.  THE PENDING AUTHORISATION   *
      *        AMOUNT IS RELEASED BY THE POSTED AMOUNT - CRD9401.      *
      ******************************************************************
       2400-UPDATE-ACCOUNT.
           IF AUTH-CASH-ADV-TYPE
               COMPUTE DCL-CASH-BAL = DCL-CASH-BAL
                                    + WS-BILLING-AMT
                                    + WS-FEE-AMT
           END-IF
      *
           COMPUTE DCL-CURR-BAL = DCL-CURR-BAL
                                + WS-BILLING-AMT
                                + WS-FEE-AMT
      *
           COMPUTE DCL-PENDING-AUTH = DCL-PENDING-AUTH
                                    - WS-BILLING-AMT
           IF DCL-PENDING-AUTH < ZERO
               MOVE ZERO                   TO DCL-PENDING-AUTH
           END-IF
      *
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET CURR_BAL         = :DCL-CURR-BAL
                    , CASH_BAL         = :DCL-CASH-BAL
                    , PENDING_AUTH_AMT = :DCL-PENDING-AUTH
                    , LAST_MAINT_PGM   = :WS-PROGRAM-ID
                    , LAST_MAINT_TS    = CURRENT TIMESTAMP
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-ACCT-UPD-CNT
               WHEN 100
                   MOVE 'R007'             TO AX-EDIT-REASON
                   MOVE 'Y'                TO WS-REJECT-SW
               WHEN OTHER
                   MOVE 'ACCOUNT          ' TO ER-SQL-TABLE
                   MOVE 'UPDATE  '          TO ER-SQL-OPERATION
                   MOVE 'UPDATE OF CARDSVC.ACCOUNT FAILED'
                                            TO ER-MESSAGE
                   MOVE 0403                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       2500-UPDATE-AUTHORISATION.
           STRING WS-CYCLE-DATE(1:4) '-' WS-CYCLE-DATE(5:2) '-'
                  WS-CYCLE-DATE(7:2)
             DELIMITED BY SIZE INTO DCL-AUTH-DATE
           END-STRING
           MOVE AUTH-CARD-NUM              TO DCL-CARD-NUM
           MOVE AUTH-SEQ-NUM               TO DCL-AUTH-SEQ
      *
           EXEC SQL
               UPDATE CARDSVC.AUTHORIZATION
                  SET POSTED_FLG  = 'Y'
                    , SETTLED_FLG = 'Y'
                WHERE CARD_NUM     = :DCL-CARD-NUM
                  AND AUTH_DATE    = DATE(:DCL-AUTH-DATE)
                  AND AUTH_SEQ_NUM = :DCL-AUTH-SEQ
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1                   TO WS-AUTH-UPD-CNT
               WHEN 100
      *            THE AUTHORISATION LOG AND DB2 HAVE DRIFTED.  THE
      *            POSTING STANDS, THE CONDITION IS REPORTED.
                   DISPLAY 'CBCRD04 WARNING AUTH ROW NOT FOUND CARD='
                           AUTH-CARD-NUM ' SEQ=' AUTH-SEQ-NUM
               WHEN OTHER
                   MOVE 'AUTHORIZATION    ' TO ER-SQL-TABLE
                   MOVE 'UPDATE  '          TO ER-SQL-OPERATION
                   MOVE 'UPDATE OF CARDSVC.AUTHORIZATION FAILED'
                                            TO ER-MESSAGE
                   MOVE 0403                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       2700-WRITE-POST-REJECT.
           ADD 1                           TO WS-REJECT-CNT
                                              WS-REJECT-SEQ
           INITIALIZE CYCLE-REJECT-REC
           MOVE AX-CYCLE-DATE              TO RJ-CYCLE-DATE
           MOVE AX-CYCLE-ID                TO RJ-CYCLE-ID
           MOVE WS-REJECT-SEQ              TO RJ-REJECT-SEQ
           MOVE AX-EDIT-REASON             TO RJ-REASON-CD
           MOVE 'POSTING REJECTED - SEE CBCRD04 LOG'
                                           TO RJ-REASON-TXT
           MOVE WS-PROGRAM-ID              TO RJ-DETECT-PGM
           MOVE WS-STEP-NAME               TO RJ-DETECT-STEP
           MOVE AX-AUTH-IMAGE              TO RJ-AUTH-IMAGE
           WRITE REJECT-REC FROM CYCLE-REJECT-REC
           .
      *
      ******************************************************************
      * 2800 - CHECKPOINT                                              *
      *                                                                *
      * THE CYCLE CONTROL RECORD IS REWRITTEN FIRST AND THE DB2 UNIT   *
      * OF WORK IS COMMITTED IMMEDIATELY AFTERWARDS.  A FAILURE        *
      * BETWEEN THE TWO LEAVES THE SAVED KEY BEHIND THE COMMITTED      *
      * WORK, SO THE RESTART REPLAYS A FEW POSTINGS - THE -803 PATH    *
      * IN 2300 ABSORBS THEM.  THE OTHER ORDER WOULD LOSE POSTINGS.    *
      ******************************************************************
       2800-TAKE-CHECKPOINT.
           MOVE WS-CURRENT-KEY             TO CC-LAST-KEY
           MOVE WS-READ-CNT                TO CC-RECS-READ
           MOVE WS-POSTED-CNT              TO CC-RECS-WRITTEN
           MOVE WS-REJECT-CNT              TO CC-RECS-REJECTED
           MOVE WS-DEBIT-TOTAL             TO CC-TOTAL-DR-AMT
           MOVE WS-CREDIT-TOTAL            TO CC-TOTAL-CR-AMT
           MOVE WS-HASH-TOTAL              TO CC-HASH-TOTAL
           MOVE 'R'                        TO CC-STATUS
      *
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'CHECKPOINT REWRITE FAILED' TO ER-MESSAGE
               MOVE 0401                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           EXEC SQL COMMIT WORK END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'COMMIT  '             TO ER-SQL-OPERATION
               MOVE 'COMMIT AT CHECKPOINT FAILED' TO ER-MESSAGE
               MOVE 0403                   TO WS-ABEND-CODE
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           ADD 1                           TO WS-COMMIT-CNT
           MOVE ZERO                       TO WS-SINCE-COMMIT
      *
           DISPLAY 'CBCRD04 CHECKPOINT ' WS-COMMIT-CNT
                   ' POSTED ' WS-POSTED-CNT
                   ' KEY ' WS-CURRENT-KEY(1:25)
           .
      *
      ******************************************************************
      * 3000 - TERMINATION                                             *
      ******************************************************************
       3000-TERMINATE.
           IF WS-SKIPPING
               MOVE 'RESTART KEY NEVER MATCHED ON THE INPUT FILE'
                                           TO ER-MESSAGE
               MOVE 0404                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE WS-READ-CNT                TO CC-RECS-READ
           MOVE WS-POSTED-CNT              TO CC-RECS-WRITTEN
           MOVE WS-REJECT-CNT              TO CC-RECS-REJECTED
           MOVE WS-DEBIT-TOTAL             TO CC-TOTAL-DR-AMT
           MOVE WS-CREDIT-TOTAL            TO CC-TOTAL-CR-AMT
           MOVE WS-HASH-TOTAL              TO CC-HASH-TOTAL
           MOVE SPACES                     TO CC-LAST-KEY
           MOVE WS-STEP-NAME               TO CC-LAST-GOOD-STEP
           MOVE 'R'                        TO CC-STATUS
      *
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
      *
           EXEC SQL COMMIT WORK END-EXEC
           ADD 1                           TO WS-COMMIT-CNT
      *
           CLOSE ENRICH-FILE
                 REJECT-FILE
                 CYCLCTL-FILE
      *
           IF WS-REJECT-CNT > ZERO
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
           END-IF
      *
           PERFORM 3100-PRINT-CONTROLS
           .
      *
       3100-PRINT-CONTROLS.
           DISPLAY '----------------------------------------------'
           MOVE WS-READ-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD04 RECORDS READ       ' WS-DISPLAY-CNT
           MOVE WS-SKIPPED-CNT             TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD04 SKIPPED ON RESTART ' WS-DISPLAY-CNT
           MOVE WS-POSTED-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD04 TRANSACTIONS POSTED' WS-DISPLAY-CNT
           MOVE WS-LEG-CNT-TOT             TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD04 LEGS POSTED        ' WS-DISPLAY-CNT
           MOVE WS-REJECT-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD04 POSTING REJECTS    ' WS-DISPLAY-CNT
           MOVE WS-ACCT-UPD-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD04 ACCOUNTS UPDATED   ' WS-DISPLAY-CNT
           MOVE WS-AUTH-UPD-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD04 AUTHS STAMPED      ' WS-DISPLAY-CNT
           MOVE WS-COMMIT-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD04 COMMITS TAKEN      ' WS-DISPLAY-CNT
           MOVE WS-DEBIT-TOTAL             TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD04 DEBIT TOTAL        ' WS-DISPLAY-AMT
           MOVE WS-CREDIT-TOTAL            TO WS-DISPLAY-AMT
           DISPLAY 'CBCRD04 CREDIT TOTAL       ' WS-DISPLAY-AMT
           DISPLAY 'CBCRD04 HASH TOTAL         ' WS-HASH-TOTAL
           DISPLAY '----------------------------------------------'
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD04 SQL ERROR SQLCODE=' SQLCODE
                   ' OPERATION=' ER-SQL-OPERATION
                   ' TABLE=' ER-SQL-TABLE
           DISPLAY '        CARD=' AUTH-CARD-NUM
                   ' SEQ=' AUTH-SEQ-NUM
                   ' ACCT=' AUTH-ACCT-ID
           PERFORM 9500-FATAL-ERROR
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
      *
           DISPLAY 'CBCRD04 FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           DISPLAY 'CBCRD04 LAST COMMITTED KEY ' CC-LAST-KEY
      *
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
