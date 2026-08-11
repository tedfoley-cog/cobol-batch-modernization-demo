      ******************************************************************
      * CBCRD03 - MERCHANT ENRICHMENT OF THE SORTED CLEAN FILE         *
      *                                                                *
      * STEP 3 OF THE CARDNITE CYCLE.  JOB CBCRD03J STEP020.           *
      * STEP010 OF THE SAME JOB IS A DFSORT STEP - THIS PROGRAM        *
      * ASSUMES THE INPUT ARRIVES IN MERCHANT-ID WITHIN CARD-NUMBER    *
      * ORDER AND USES THAT TO AVOID RE-READING THE SAME MERCHANT.     *
      *                                                                *
      * FOR EACH CLEAN AUTHORISATION THE MERCHANT IS RESOLVED FROM     *
      * TWO PLACES - THE VSAM SETTLEMENT ROUTING FILE MERCHRTE, WHICH  *
      * THE ACQUIRER FEED REFRESHES DAILY, AND THE DB2 MERCHANT TABLE  *
      * WHICH HOLDS THE STANDING DATA.  VSAM WINS ON THE SETTLEMENT    *
      * ROUTE, DB2 WINS ON MCC AND NAME.                               *
      *                                                                *
      * AN UNMATCHED MERCHANT IS NOT A REJECT - IT TAKES THE DEFAULT   *
      * ROUTE FROM THE SYSIN CONTROL CARD AND IS COUNTED AS A WARNING. *
      *                                                                *
      * CALLED BY   - JCL ONLY                                         *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - AUTHSRT  QSAM INPUT   LRECL 300 (SORTED CLEAN)   *
      *             - AUTHENR  QSAM OUTPUT  LRECL 300                  *
      *             - MERCHRTE VSAM KSDS INPUT, RANDOM                 *
      *             - CYCLCTL  VSAM KSDS I-O                           *
      *             - SYSIN    CONTROL CARDS - DEFAULT ROUTE           *
      * TABLES      - CARDSVC.MERCHANT (SELECT)                        *
      * PLAN        - CARDNITP                                         *
      *                                                                *
      * RETURN CODE - 0000 ALL MERCHANTS MATCHED                       *
      *               0004 ONE OR MORE DEFAULTED ROUTES                *
      *               0012 FATAL                                       *
      * USER ABEND  - U0301 CYCLE CONTROL UNUSABLE                     *
      *               U0302 FILE OPEN OR I/O FAILURE                   *
      *               U0303 UNRECOVERABLE SQL ERROR                    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD03.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1998-04-08.
      *
      * MAINTENANCE
      * 1998-04-08 CRD0121 ORIGINAL
      * 2005-05-16 CRD4407 MERCHRTE OVERRIDES DB2 SETTLEMENT ROUTE
      * 2012-01-23 CRD7719 HIGH RISK MERCHANT FLAG CARRIED FORWARD
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORTED-FILE   ASSIGN TO AUTHSRT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-SORTED-STATUS.
      *
           SELECT ENRICH-FILE   ASSIGN TO AUTHENR
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-ENRICH-STATUS.
      *
           SELECT MERCHRTE-FILE ASSIGN TO MERCHRTE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS MR-MERCHANT-ID
                  FILE STATUS  IS WS-MERCHRTE-STATUS.
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
       FD  SORTED-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       01  SORTED-REC                      PIC X(300).
      *
       FD  ENRICH-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       01  ENRICH-REC                      PIC X(300).
      *
       FD  MERCHRTE-FILE
           RECORD CONTAINS 120 CHARACTERS.
       01  MERCHRTE-REC.
           05  MR-MERCHANT-ID              PIC X(15).
           05  MR-ACQUIRER-ID              PIC X(11).
           05  MR-SETTLE-ROUTE             PIC X(4).
           05  MR-SETTLE-CUTOFF            PIC 9(6).
           05  MR-NETWORK-CD               PIC X(4).
           05  MR-COUNTRY-CD               PIC X(3).
           05  MR-STATUS                   PIC X.
               88  MR-ACTIVE               VALUE 'A'.
               88  MR-SUSPENDED            VALUE 'S'.
           05  MR-LAST-FEED-DT             PIC 9(8).
           05  MR-FILLER                   PIC X(68).
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD03 '.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP020 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-SORTED-STATUS            PIC X(2)  VALUE '00'.
               88  WS-SORTED-OK                      VALUE '00'.
           05  WS-ENRICH-STATUS            PIC X(2)  VALUE '00'.
               88  WS-ENRICH-OK                      VALUE '00'.
           05  WS-MERCHRTE-STATUS          PIC X(2)  VALUE '00'.
               88  WS-MERCHRTE-OK                    VALUE '00'.
               88  WS-MERCHRTE-NOTFND                VALUE '23'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-VSAM-HIT-SW              PIC X     VALUE 'N'.
               88  WS-VSAM-HIT                       VALUE 'Y'.
           05  WS-DB2-HIT-SW               PIC X     VALUE 'N'.
               88  WS-DB2-HIT                        VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-WRITTEN-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-MATCHED-CNT              PIC 9(9)  VALUE ZERO.
           05  WS-DEFAULTED-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-NO-MERCHANT-CNT          PIC 9(9)  VALUE ZERO.
           05  WS-SUSPENDED-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-HIGH-RISK-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-VSAM-READ-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-DB2-READ-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-CACHE-HIT-CNT            PIC 9(9)  VALUE ZERO.
      *
      *    LAST MERCHANT SEEN - THE FILE IS IN MERCHANT ORDER SO A
      *    SINGLE ENTRY CACHE REMOVES MOST OF THE I/O.
       01  WS-LAST-MERCHANT.
           05  WS-LM-MERCHANT-ID           PIC X(15) VALUE HIGH-VALUES.
           05  WS-LM-MCC                   PIC 9(4)  VALUE ZERO.
           05  WS-LM-NAME                  PIC X(22) VALUE SPACES.
           05  WS-LM-ACQUIRER              PIC X(11) VALUE SPACES.
           05  WS-LM-ROUTE                 PIC X(4)  VALUE SPACES.
           05  WS-LM-HIGH-RISK             PIC X     VALUE 'N'.
           05  WS-LM-STATUS                PIC X     VALUE SPACE.
      *
       01  WS-CONTROL-CARD                 PIC X(80) VALUE SPACES.
       01  WS-DEFAULT-ROUTE                PIC X(4)  VALUE 'DFLT'.
       01  WS-DEFAULT-ACQUIRER             PIC X(11)
                                           VALUE 'UNKNOWN    '.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-CURRENT-DATE.
           05  WS-CD-DATE                  PIC 9(8).
           05  WS-CD-TIME                  PIC 9(8).
           05  WS-CD-FILLER                PIC X(5).
       01  WS-TIMESTAMP                    PIC X(26) VALUE SPACES.
      *
       01  WS-RETURN-CODE                  PIC 9(4)  VALUE ZERO.
       01  WS-ABEND-CODE                   PIC 9(4)  VALUE ZERO.
       01  WS-WORK-MERCHANT                PIC X(15) VALUE SPACES.
      *
           COPY CVAXTR01Y.
           COPY CVAUTH01Y.
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-MERCHANT.
           05  DCL-MERCHANT-ID             PIC X(15).
           05  DCL-MERCHANT-NAME           PIC X(40).
           05  DCL-MCC                     PIC X(4).
           05  DCL-ACQUIRER-ID             PIC X(11).
           05  DCL-COUNTRY-CD              PIC X(3).
           05  DCL-HIGH-RISK-FLG           PIC X(1).
           05  DCL-SETTLE-ROUTE-CD         PIC X(4).
           05  DCL-STATUS                  PIC X(1).
      *
       01  IND-VARS.
           05  IND-ACQUIRER                PIC S9(4) COMP.
           05  IND-ROUTE                   PIC S9(4) COMP.
      *
       01  WS-DISPLAY-CNT                  PIC ZZZ,ZZZ,ZZ9.
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
           PERFORM 2000-ENRICH-RECORD
               UNTIL WS-EOF
           PERFORM 3000-TERMINATE
           MOVE WS-RETURN-CODE             TO RETURN-CODE
           GOBACK
           .
      *
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
      *
           PERFORM 1100-READ-CONTROL-CARDS
      *
           OPEN INPUT  SORTED-FILE
                       MERCHRTE-FILE
           IF NOT WS-SORTED-OK OR NOT WS-MERCHRTE-OK
               MOVE 'AUTHSRT '             TO ER-FILE-NAME
               MOVE WS-SORTED-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF INPUT FILES FAILED' TO ER-MESSAGE
               MOVE 0302                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           OPEN OUTPUT ENRICH-FILE
           OPEN I-O    CYCLCTL-FILE
           IF NOT WS-ENRICH-OK OR NOT WS-CYCLCTL-OK
               MOVE 'AUTHENR '             TO ER-FILE-NAME
               MOVE WS-ENRICH-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF OUTPUT FILES FAILED' TO ER-MESSAGE
               MOVE 0302                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 0301               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE WS-STEP-NAME               TO CC-CURRENT-STEP
      *
           DISPLAY 'CBCRD03 - MERCHANT ENRICHMENT, CYCLE '
                   WS-CYCLE-DATE
           DISPLAY '          DEFAULT ROUTE ' WS-DEFAULT-ROUTE
           .
      *
      ******************************************************************
      * 1100 - CONTROL CARDS.  THE DEFAULT SETTLEMENT ROUTE IS A       *
      *        BUSINESS PARAMETER AND LIVES IN SYSIN, NOT IN THE CODE. *
      *        CARD FORMAT - DEFAULT-ROUTE=XXXX                        *
      ******************************************************************
       1100-READ-CONTROL-CARDS.
           ACCEPT WS-CONTROL-CARD FROM SYSIN
           PERFORM UNTIL WS-CONTROL-CARD = SPACES
                      OR WS-CONTROL-CARD(1:3) = 'END'
               EVALUATE WS-CONTROL-CARD(1:14)
                   WHEN 'DEFAULT-ROUTE='
                       MOVE WS-CONTROL-CARD(15:4)
                                           TO WS-DEFAULT-ROUTE
                   WHEN 'DEFAULT-ACQID='
                       MOVE WS-CONTROL-CARD(15:11)
                                           TO WS-DEFAULT-ACQUIRER
                   WHEN OTHER
                       IF WS-CONTROL-CARD(1:1) NOT = '*'
                           DISPLAY 'CBCRD03 - CARD IGNORED '
                                   WS-CONTROL-CARD(1:40)
                       END-IF
               END-EVALUATE
               MOVE SPACES                 TO WS-CONTROL-CARD
               ACCEPT WS-CONTROL-CARD FROM SYSIN
           END-PERFORM
           .
      *
      ******************************************************************
      * 2000 - ENRICH ONE RECORD                                       *
      ******************************************************************
       2000-ENRICH-RECORD.
           READ SORTED-FILE INTO AUTH-EXTRACT-REC
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
                   GO TO 2000-EXIT
           END-READ
      *
           ADD 1                           TO WS-READ-CNT
           MOVE AX-AUTH-IMAGE              TO AUTH-RECORD
      *
      *    ONLY PURCHASES CARRY A MERCHANT.  CASH ADVANCES ROUTE BY
      *    ACQUIRER AND REFUNDS INHERIT THE ORIGINAL MERCHANT.
           EVALUATE TRUE
               WHEN AUTH-PURCHASE-TYPE
                   MOVE AP-MERCHANT-ID     TO WS-WORK-MERCHANT
                   PERFORM 2100-RESOLVE-MERCHANT
               WHEN AUTH-CASH-ADV-TYPE
                   PERFORM 2200-RESOLVE-ACQUIRER
               WHEN AUTH-REFUND-TYPE
                   MOVE AR-ORIG-MERCHANT   TO WS-WORK-MERCHANT
                   PERFORM 2100-RESOLVE-MERCHANT
               WHEN OTHER
      *            CBCRD02 SHOULD HAVE REMOVED THESE.  BE DEFENSIVE.
                   MOVE 'N'                TO AX-ENRICH-STATUS
                   ADD 1                   TO WS-NO-MERCHANT-CNT
           END-EVALUATE
      *
           PERFORM 2900-WRITE-ENRICHED
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-RESOLVE-MERCHANT.
           IF WS-WORK-MERCHANT = SPACES
               PERFORM 2800-APPLY-DEFAULT
               GO TO 2100-EXIT
           END-IF
      *
           IF WS-WORK-MERCHANT = WS-LM-MERCHANT-ID
               ADD 1                       TO WS-CACHE-HIT-CNT
               PERFORM 2700-APPLY-CACHE
               GO TO 2100-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-VSAM-HIT-SW
                                              WS-DB2-HIT-SW
           PERFORM 2110-READ-MERCHRTE
           PERFORM 2120-SELECT-MERCHANT
      *
           IF WS-VSAM-HIT OR WS-DB2-HIT
               MOVE WS-WORK-MERCHANT       TO WS-LM-MERCHANT-ID
               PERFORM 2700-APPLY-CACHE
               ADD 1                       TO WS-MATCHED-CNT
           ELSE
               PERFORM 2800-APPLY-DEFAULT
           END-IF
           .
       2100-EXIT.
           EXIT
           .
      *
       2110-READ-MERCHRTE.
           MOVE WS-WORK-MERCHANT           TO MR-MERCHANT-ID
           READ MERCHRTE-FILE
               INVALID KEY
                   CONTINUE
           END-READ
           ADD 1                           TO WS-VSAM-READ-CNT
      *
           EVALUATE WS-MERCHRTE-STATUS
               WHEN '00'
                   MOVE 'Y'                TO WS-VSAM-HIT-SW
                   MOVE MR-ACQUIRER-ID     TO WS-LM-ACQUIRER
                   MOVE MR-SETTLE-ROUTE    TO WS-LM-ROUTE
                   MOVE MR-STATUS          TO WS-LM-STATUS
                   IF MR-SUSPENDED
                       ADD 1               TO WS-SUSPENDED-CNT
                   END-IF
               WHEN '23'
                   CONTINUE
               WHEN OTHER
                   MOVE 'MERCHRTE'         TO ER-FILE-NAME
                   MOVE WS-MERCHRTE-STATUS TO ER-FILE-STATUS
                   MOVE WS-WORK-MERCHANT   TO ER-VSAM-KEY
                   MOVE 'READ OF MERCHRTE FAILED' TO ER-MESSAGE
                   MOVE 0302               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-EVALUATE
           .
      *
       2120-SELECT-MERCHANT.
           MOVE WS-WORK-MERCHANT           TO DCL-MERCHANT-ID
      *
           EXEC SQL
               SELECT MERCHANT_NAME
                    , MCC
                    , ACQUIRER_ID
                    , COUNTRY_CD
                    , HIGH_RISK_FLG
                    , SETTLE_ROUTE_CD
                    , STATUS
                 INTO :DCL-MERCHANT-NAME
                    , :DCL-MCC
                    , :DCL-ACQUIRER-ID   :IND-ACQUIRER
                    , :DCL-COUNTRY-CD
                    , :DCL-HIGH-RISK-FLG
                    , :DCL-SETTLE-ROUTE-CD :IND-ROUTE
                    , :DCL-STATUS
                 FROM CARDSVC.MERCHANT
                WHERE MERCHANT_ID = :DCL-MERCHANT-ID
           END-EXEC
      *
           ADD 1                           TO WS-DB2-READ-CNT
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'                TO WS-DB2-HIT-SW
                   MOVE DCL-MERCHANT-NAME(1:22)
                                           TO WS-LM-NAME
                   MOVE DCL-MCC            TO WS-LM-MCC
                   MOVE DCL-HIGH-RISK-FLG  TO WS-LM-HIGH-RISK
                   IF DCL-HIGH-RISK-FLG = 'Y'
                       ADD 1               TO WS-HIGH-RISK-CNT
                   END-IF
      *            VSAM IS THE DAILY FEED SO IT OVERRIDES DB2 ON THE
      *            SETTLEMENT ROUTE.  DB2 ONLY FILLS THE GAP.
                   IF NOT WS-VSAM-HIT
                       IF IND-ROUTE < 0
                           MOVE WS-DEFAULT-ROUTE
                                           TO WS-LM-ROUTE
                       ELSE
                           MOVE DCL-SETTLE-ROUTE-CD
                                           TO WS-LM-ROUTE
                       END-IF
                       IF IND-ACQUIRER < 0
                           MOVE WS-DEFAULT-ACQUIRER
                                           TO WS-LM-ACQUIRER
                       ELSE
                           MOVE DCL-ACQUIRER-ID
                                           TO WS-LM-ACQUIRER
                       END-IF
                   END-IF
               WHEN 100
                   CONTINUE
               WHEN OTHER
                   MOVE 'MERCHANT         ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '          TO ER-SQL-OPERATION
                   MOVE 'SELECT ON CARDSVC.MERCHANT FAILED'
                                            TO ER-MESSAGE
                   MOVE 0303                TO WS-ABEND-CODE
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 2200 - CASH ADVANCE.  NO MERCHANT - THE ACQUIRER IN THE        *
      *        DETAIL AREA IS THE ROUTING KEY AND THE NETWORK CODE     *
      *        BECOMES THE SETTLEMENT ROUTE.                           *
      ******************************************************************
       2200-RESOLVE-ACQUIRER.
           MOVE ZERO                       TO AX-MCC
           MOVE AC-ACQUIRER-ID             TO AX-ACQUIRER-ID
           MOVE SPACES                     TO AX-MERCH-NAME
           MOVE 'N'                        TO AX-HIGH-RISK-FLG
      *
           IF AC-NETWORK = SPACES
               MOVE WS-DEFAULT-ROUTE       TO AX-SETTLE-ROUTE
               MOVE 'D'                    TO AX-ENRICH-STATUS
               ADD 1                       TO WS-DEFAULTED-CNT
           ELSE
               MOVE AC-NETWORK             TO AX-SETTLE-ROUTE
               MOVE 'M'                    TO AX-ENRICH-STATUS
               ADD 1                       TO WS-MATCHED-CNT
           END-IF
           .
      *
       2700-APPLY-CACHE.
           MOVE WS-LM-MCC                  TO AX-MCC
           MOVE WS-LM-ACQUIRER             TO AX-ACQUIRER-ID
           MOVE WS-LM-ROUTE                TO AX-SETTLE-ROUTE
           MOVE WS-LM-NAME                 TO AX-MERCH-NAME
           MOVE WS-LM-HIGH-RISK            TO AX-HIGH-RISK-FLG
           MOVE 'M'                        TO AX-ENRICH-STATUS
           .
      *
       2800-APPLY-DEFAULT.
           MOVE ZERO                       TO AX-MCC
           MOVE WS-DEFAULT-ACQUIRER        TO AX-ACQUIRER-ID
           MOVE WS-DEFAULT-ROUTE           TO AX-SETTLE-ROUTE
           MOVE 'UNKNOWN MERCHANT     '    TO AX-MERCH-NAME
           MOVE 'N'                        TO AX-HIGH-RISK-FLG
           MOVE 'D'                        TO AX-ENRICH-STATUS
           ADD 1                           TO WS-DEFAULTED-CNT
      *
           DISPLAY 'CBCRD03 WARNING MERCHANT NOT FOUND '
                   WS-WORK-MERCHANT
                   ' CARD=' AUTH-CARD-NUM
                   ' SEQ='  AUTH-SEQ-NUM
           .
      *
       2900-WRITE-ENRICHED.
           WRITE ENRICH-REC FROM AUTH-EXTRACT-REC
           IF NOT WS-ENRICH-OK
               MOVE 'AUTHENR '             TO ER-FILE-NAME
               MOVE WS-ENRICH-STATUS       TO ER-FILE-STATUS
               MOVE 'WRITE OF ENRICHED FILE FAILED' TO ER-MESSAGE
               MOVE 0302                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           ADD 1                           TO WS-WRITTEN-CNT
           .
      *
       3000-TERMINATE.
           IF WS-DEFAULTED-CNT > ZERO
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
           END-IF
      *
           MOVE WS-STEP-NAME               TO CC-LAST-GOOD-STEP
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'REWRITE OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0301                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           CLOSE SORTED-FILE
                 ENRICH-FILE
                 MERCHRTE-FILE
                 CYCLCTL-FILE
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-READ-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 RECORDS READ       ' WS-DISPLAY-CNT
           MOVE WS-WRITTEN-CNT             TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 RECORDS WRITTEN    ' WS-DISPLAY-CNT
           MOVE WS-MATCHED-CNT             TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 MERCHANTS MATCHED  ' WS-DISPLAY-CNT
           MOVE WS-DEFAULTED-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 ROUTES DEFAULTED   ' WS-DISPLAY-CNT
           MOVE WS-SUSPENDED-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 SUSPENDED MERCHANT ' WS-DISPLAY-CNT
           MOVE WS-HIGH-RISK-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 HIGH RISK MERCHANT ' WS-DISPLAY-CNT
           MOVE WS-CACHE-HIT-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 CACHE HITS         ' WS-DISPLAY-CNT
           MOVE WS-VSAM-READ-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 MERCHRTE READS     ' WS-DISPLAY-CNT
           MOVE WS-DB2-READ-CNT            TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD03 DB2 SELECTS        ' WS-DISPLAY-CNT
           DISPLAY '----------------------------------------------'
           .
      *
       9400-SQL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           DISPLAY 'CBCRD03 SQL ERROR SQLCODE=' SQLCODE
                   ' TABLE=' ER-SQL-TABLE
                   ' KEY=' WS-WORK-MERCHANT
           PERFORM 9500-FATAL-ERROR
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD03 FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
