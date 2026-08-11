      ******************************************************************
      * CBREF02 - MERCHRTE UNLOAD AND REBUILD VERIFICATION             *
      *                                                                *
      * RUNS TWICE IN CBREF02J.  THE FUNCTION IS TAKEN FROM THE JCL    *
      * PARM BECAUSE THE TWO PASSES SIT EITHER SIDE OF THE SORT AND    *
      * THE IDCAMS REBUILD -                                           *
      *                                                                *
      *   PARM='UNLOAD'  STEP UNLDMRCH - CURSOR OVER CARDSVC.MERCHANT, *
      *                  WRITE THE MERCHRTE IMAGE TO MERCHUNL AND THE  *
      *                  ROW COUNT TO MERCHCTL                         *
      *   PARM='VERIFY'  STEP VERFMRCH - READ THE REBUILT VSAM CLUSTER *
      *                  END TO END, COMPARE THE COUNT WITH MERCHCTL   *
      *                  AND WITH A FRESH COUNT FROM DB2, ABEND WHEN   *
      *                  THEY DISAGREE                                 *
      *                                                                *
      * CALLED BY   - CBREF02J (BOTH STEPS RUN UNDER IKJEFT01)         *
      * CALLS       - CBCRD91 ON A FATAL CONDITION                     *
      * READS       - CARDSVC.MERCHANT, MERCHRTE (KSDS), MERCHCTL      *
      * WRITES      - MERCHUNL (FB 120), MERCHCTL (FB 80)              *
      *                                                                *
      * A COUNT MISMATCH IS ALWAYS AN ABEND.  THE OLD CLUSTER HAS      *
      * ALREADY BEEN DELETED BY THE TIME THIS RUNS, SO LEAVING A       *
      * SHORT CLUSTER IN PLACE WOULD SILENTLY DECLINE AUTHORIZATIONS   *
      * ON EVERY MERCHANT THAT DID NOT MAKE IT INTO THE REBUILD.       *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - UNLOAD WRITTEN / COUNTS AGREE                           *
      *   04 - UNLOAD WRITTEN, MERCHANTS SUPPRESSED (SEE SYSOUT)       *
      *   12 - FATAL, USER ABEND U911 THROUGH CBCRD91                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBREF02.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MERCHUNL-FILE ASSIGN TO MERCHUNL
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-UNL-STATUS.
      *
           SELECT MERCHCTL-FILE ASSIGN TO MERCHCTL
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-CTL-STATUS.
      *
           SELECT MERCHRTE-FILE ASSIGN TO MERCHRTE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS MRR-KEY
                  FILE STATUS  IS WS-RTE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  MERCHUNL-FILE
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  MERCHUNL-REC                    PIC X(120).
      *
       FD  MERCHCTL-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  MERCHCTL-REC.
           05  MC-LABEL                    PIC X(12).
           05  MC-COUNT                    PIC 9(9).
           05  MC-RUN-DATE                 PIC 9(8).
           05  MC-CYCLE-ID                 PIC X(8).
           05  FILLER                      PIC X(43).
      *
       FD  MERCHRTE-FILE
           RECORD CONTAINS 120 CHARACTERS.
       01  MERCHRTE-REC.
           05  MRR-KEY                     PIC X(15).
           05  FILLER                      PIC X(105).
      *
       WORKING-STORAGE SECTION.
      *
           COPY CVMRTE1Y.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBREF02 '.
      *
       01  WS-STATUSES.
           05  WS-UNL-STATUS               PIC X(2)  VALUE '00'.
               88  WS-UNL-OK                         VALUE '00'.
           05  WS-CTL-STATUS               PIC X(2)  VALUE '00'.
               88  WS-CTL-OK                         VALUE '00'.
           05  WS-RTE-STATUS               PIC X(2)  VALUE '00'.
               88  WS-RTE-OK                         VALUE '00'.
               88  WS-RTE-EOF                        VALUE '10'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
      *
       01  WS-FUNCTION                     PIC X(8)  VALUE SPACES.
           88  WS-UNLOAD                             VALUE 'UNLOAD  '.
           88  WS-VERIFY                             VALUE 'VERIFY  '.
      *
       01  WS-COUNTS.
           05  WS-DB2-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-UNL-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-VSAM-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-CTL-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-SUPPRESS-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-HIGH-RISK-CNT            PIC 9(9)  VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-RETURN-CD                PIC S9(4) COMP VALUE ZERO.
           05  WS-MSG                      PIC X(78) VALUE SPACES.
      *
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-YYYY                  PIC 9(4).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-MERCH.
           05  DCL-MERCHANT-ID             PIC X(15).
           05  DCL-MERCHANT-NAME           PIC X(40).
           05  DCL-MCC                     PIC X(4).
           05  DCL-ACQUIRER-ID             PIC X(11).
           05  DCL-COUNTRY-CD              PIC X(3).
           05  DCL-CITY                    PIC X(25).
           05  DCL-HIGH-RISK-FLG           PIC X.
           05  DCL-CHARGEBACK-RATE         PIC S9(3)V99 COMP-3.
           05  DCL-SETTLE-ROUTE-CD         PIC X(4).
           05  DCL-STATUS                  PIC X.
           05  DCL-MAINT-DATE              PIC X(10).
           05  DCL-ROW-CNT                 PIC S9(9) COMP-3.
      *
       01  IND-VARS.
           05  IND-ACQUIRER                PIC S9(4) COMP.
           05  IND-CITY                    PIC S9(4) COMP.
           05  IND-ROUTE                   PIC S9(4) COMP.
      *
      ******************************************************************
      * THE UNLOAD SELECTS ACTIVE AND SUSPENDED MERCHANTS.  LOGICALLY  *
      * DELETED ROWS ARE LEFT OUT OF THE VSAM COPY - THE ONLINE PATH   *
      * TREATS AN ABSENT MERCHANT AND A DELETED ONE THE SAME WAY.      *
      ******************************************************************
           EXEC SQL DECLARE MERCHCSR CURSOR FOR
               SELECT MERCHANT_ID
                    , MERCHANT_NAME
                    , MCC
                    , ACQUIRER_ID
                    , COUNTRY_CD
                    , CITY
                    , HIGH_RISK_FLG
                    , CHARGEBACK_RATE
                    , SETTLE_ROUTE_CD
                    , STATUS
                    , CHAR(DATE(LAST_MAINT_TS), ISO)
                 FROM CARDSVC.MERCHANT
                WHERE STATUS IN ('A','S')
                ORDER BY MERCHANT_ID
                WITH UR
           END-EXEC.
      *
       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-PARM-LEN                 PIC S9(4) COMP.
           05  LK-PARM-DATA                PIC X(8).
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-PARM.
      *
       0000-MAIN-LINE.
           PERFORM 1000-INITIALISE
           IF WS-FATAL
               GO TO 0000-TERMINATE
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-UNLOAD
                   PERFORM 2000-UNLOAD-MERCHANTS
               WHEN WS-VERIFY
                   PERFORM 3000-VERIFY-REBUILD
               WHEN OTHER
                   MOVE 'PARM MUST BE UNLOAD OR VERIFY'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
           END-EVALUATE
           .
       0000-TERMINATE.
           IF WS-FATAL
               PERFORM 9500-ABEND
           END-IF
           MOVE WS-RETURN-CD               TO RETURN-CODE
           GOBACK
           .
      *
       1000-INITIALISE.
           IF LK-PARM-LEN = ZERO
               MOVE 'NO PARM PASSED TO CBREF02'
                                           TO WS-MSG
               PERFORM 9200-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           MOVE SPACES                     TO WS-FUNCTION
           MOVE LK-PARM-DATA(1:LK-PARM-LEN)
                                           TO WS-FUNCTION
           DISPLAY 'CBREF02 FUNCTION ' WS-FUNCTION
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - UNLOAD                                                  *
      ******************************************************************
       2000-UNLOAD-MERCHANTS.
           OPEN OUTPUT MERCHUNL-FILE
           IF NOT WS-UNL-OK
               MOVE 'MERCHUNL OPEN FAILED' TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           OPEN OUTPUT MERCHCTL-FILE
           IF NOT WS-CTL-OK
               MOVE 'MERCHCTL OPEN FAILED' TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2100-COUNT-DB2-ROWS
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           EXEC SQL
               OPEN MERCHCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'OPEN MERCHCSR FAILED' TO WS-MSG
               PERFORM 9400-SQL-ERROR
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM UNTIL WS-EOF OR WS-FATAL
               PERFORM 2200-FETCH-MERCHANT
               IF NOT WS-EOF AND NOT WS-FATAL
                   PERFORM 2300-WRITE-UNLOAD
               END-IF
           END-PERFORM
      *
           EXEC SQL
               CLOSE MERCHCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'CLOSE MERCHCSR FAILED'
                                           TO WS-MSG
               PERFORM 9400-SQL-ERROR
           END-IF
      *
           PERFORM 2400-WRITE-CONTROL
      *
           CLOSE MERCHUNL-FILE
                 MERCHCTL-FILE
      *
           DISPLAY 'CBREF02 UNLOAD DB2=' WS-DB2-CNT
                   ' WRITTEN=' WS-UNL-CNT
                   ' HIGH RISK=' WS-HIGH-RISK-CNT
      *
      *    THE COUNT ON THE CURSOR AND THE COUNT FROM COUNT(*) MUST
      *    AGREE - THEY ARE TAKEN IN THE SAME UNIT OF WORK.
           IF WS-UNL-CNT NOT = WS-DB2-CNT
               MOVE 'UNLOAD COUNT DOES NOT MATCH THE TABLE COUNT'
                                           TO WS-MSG
               PERFORM 9200-FATAL
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-COUNT-DB2-ROWS.
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-ROW-CNT
                 FROM CARDSVC.MERCHANT
                WHERE STATUS IN ('A','S')
                WITH UR
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'COUNT OF CARDSVC.MERCHANT FAILED'
                                           TO WS-MSG
               PERFORM 9400-SQL-ERROR
           ELSE
               MOVE DCL-ROW-CNT            TO WS-DB2-CNT
           END-IF
           .
      *
       2200-FETCH-MERCHANT.
           EXEC SQL
               FETCH MERCHCSR
                INTO :DCL-MERCHANT-ID
                   , :DCL-MERCHANT-NAME
                   , :DCL-MCC
                   , :DCL-ACQUIRER-ID :IND-ACQUIRER
                   , :DCL-COUNTRY-CD
                   , :DCL-CITY :IND-CITY
                   , :DCL-HIGH-RISK-FLG
                   , :DCL-CHARGEBACK-RATE
                   , :DCL-SETTLE-ROUTE-CD :IND-ROUTE
                   , :DCL-STATUS
                   , :DCL-MAINT-DATE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'                TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'FETCH MERCHCSR FAILED'
                                           TO WS-MSG
                   PERFORM 9400-SQL-ERROR
           END-EVALUATE
           .
      *
       2300-WRITE-UNLOAD.
           MOVE SPACES                     TO MERCHANT-RTE-RECORD
           MOVE DCL-MERCHANT-ID            TO MR-MERCHANT-ID
           MOVE DCL-MERCHANT-NAME          TO MR-MERCHANT-NAME
           MOVE DCL-MCC                    TO MR-MCC
           MOVE DCL-COUNTRY-CD             TO MR-COUNTRY-CD
           MOVE DCL-HIGH-RISK-FLG          TO MR-HIGH-RISK-FLG
           MOVE DCL-CHARGEBACK-RATE        TO MR-CHARGEBACK-RATE
           MOVE DCL-STATUS                 TO MR-STATUS
      *
           IF IND-ACQUIRER < ZERO
               MOVE SPACES                 TO MR-ACQUIRER-ID
           ELSE
               MOVE DCL-ACQUIRER-ID        TO MR-ACQUIRER-ID
           END-IF
      *
           IF IND-CITY < ZERO
               MOVE SPACES                 TO MR-CITY
           ELSE
               MOVE DCL-CITY               TO MR-CITY
           END-IF
      *
           IF IND-ROUTE < ZERO
               MOVE SPACES                 TO MR-SETTLE-ROUTE-CD
           ELSE
               MOVE DCL-SETTLE-ROUTE-CD    TO MR-SETTLE-ROUTE-CD
           END-IF
      *
           MOVE DCL-MAINT-DATE(1:4)        TO WS-DW-YYYY
           MOVE DCL-MAINT-DATE(6:2)        TO WS-DW-MM
           MOVE DCL-MAINT-DATE(9:2)        TO WS-DW-DD
           MOVE WS-DATE-WORK               TO MR-LAST-MAINT-DATE
      *
           IF MR-HIGH-RISK
               ADD 1                       TO WS-HIGH-RISK-CNT
           END-IF
      *
           MOVE MERCHANT-RTE-RECORD        TO MERCHUNL-REC
           WRITE MERCHUNL-REC
           IF NOT WS-UNL-OK
               MOVE 'MERCHUNL WRITE FAILED'
                                           TO WS-MSG
               PERFORM 9300-FILE-ERROR
           ELSE
               ADD 1                       TO WS-UNL-CNT
           END-IF
           .
      *
       2400-WRITE-CONTROL.
           MOVE 'MERCHRTECNT '             TO MC-LABEL
           MOVE WS-UNL-CNT                 TO MC-COUNT
           MOVE FUNCTION CURRENT-DATE(1:8) TO MC-RUN-DATE
           MOVE 'CARDREF '                 TO MC-CYCLE-ID
           MOVE SPACES                     TO MERCHCTL-REC(38:43)
           WRITE MERCHCTL-REC
           IF NOT WS-CTL-OK
               MOVE 'MERCHCTL WRITE FAILED'
                                           TO WS-MSG
               PERFORM 9300-FILE-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 3000 - VERIFY                                                  *
      ******************************************************************
       3000-VERIFY-REBUILD.
           OPEN INPUT MERCHCTL-FILE
           IF NOT WS-CTL-OK
               MOVE 'MERCHCTL OPEN FAILED' TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           READ MERCHCTL-FILE
               AT END
                   MOVE 'MERCHCTL IS EMPTY - UNLOAD DID NOT RUN'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
           END-READ
           MOVE MC-COUNT                   TO WS-CTL-CNT
           CLOSE MERCHCTL-FILE
      *
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
      *
           PERFORM 2100-COUNT-DB2-ROWS
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
      *
           OPEN INPUT MERCHRTE-FILE
           IF NOT WS-RTE-OK
               MOVE 'MERCHRTE OPEN FAILED - CLUSTER NOT REBUILT'
                                           TO WS-MSG
               PERFORM 9300-FILE-ERROR
               GO TO 3000-EXIT
           END-IF
      *
           MOVE 'N'                        TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR WS-FATAL
               READ MERCHRTE-FILE NEXT RECORD
                    INTO MERCHANT-RTE-RECORD
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-VSAM-CNT
                       IF MR-MERCHANT-ID = SPACES
                           ADD 1           TO WS-SUPPRESS-CNT
                       END-IF
               END-READ
      *
               IF NOT WS-RTE-OK
               AND NOT WS-RTE-EOF
                   MOVE 'MERCHRTE READ FAILED'
                                           TO WS-MSG
                   PERFORM 9300-FILE-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE MERCHRTE-FILE
      *
           DISPLAY 'CBREF02 VERIFY DB2=' WS-DB2-CNT
                   ' UNLOAD=' WS-CTL-CNT
                   ' VSAM=' WS-VSAM-CNT
      *
           EVALUATE TRUE
               WHEN WS-FATAL
                   CONTINUE
               WHEN WS-VSAM-CNT NOT = WS-CTL-CNT
                   MOVE 'VSAM COUNT DOES NOT MATCH THE UNLOAD COUNT'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
               WHEN WS-VSAM-CNT NOT = WS-DB2-CNT
                   MOVE 'VSAM COUNT DOES NOT MATCH THE DB2 COUNT'
                                           TO WS-MSG
                   PERFORM 9200-FATAL
               WHEN WS-SUPPRESS-CNT > ZERO
                   MOVE 4                  TO WS-RETURN-CD
                   DISPLAY 'CBREF02 RECORDS WITH A BLANK KEY - '
                           WS-SUPPRESS-CNT
               WHEN OTHER
                   MOVE 0                  TO WS-RETURN-CD
                   DISPLAY 'CBREF02 MERCHRTE REBUILD VERIFIED'
           END-EVALUATE
           .
       3000-EXIT.
           EXIT
           .
      *
       9200-FATAL.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF02 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'DATA'                     TO ER-ERROR-TYPE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U911'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF02 FATAL - ' WS-MSG
           .
      *
       9300-FILE-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF02 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'VSAM'                     TO ER-ERROR-TYPE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'MERCHRTE'                 TO ER-FILE-NAME
           MOVE WS-RTE-STATUS              TO ER-FILE-STATUS
           MOVE 'U911'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF02 FILE ERROR - ' WS-MSG
                   ' UNL=' WS-UNL-STATUS
                   ' CTL=' WS-CTL-STATUS
                   ' RTE=' WS-RTE-STATUS
           .
      *
       9400-SQL-ERROR.
           MOVE 'Y'                        TO WS-FATAL-SW
           MOVE 'CBREF02 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'SQL '                     TO ER-ERROR-TYPE
           MOVE SQLCODE                    TO ER-SQLCODE
           MOVE SQLSTATE                   TO ER-SQLSTATE
           MOVE 'MERCHANT          '       TO ER-SQL-TABLE
           MOVE WS-MSG                     TO ER-MESSAGE
           MOVE 'U912'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           DISPLAY 'CBREF02 SQL ERROR SQLCODE=' SQLCODE
                   ' - ' WS-MSG
           .
      *
       9500-ABEND.
           MOVE 12                         TO WS-RETURN-CD
           CALL 'CBCRD91' USING ERROR-AREA
           .
