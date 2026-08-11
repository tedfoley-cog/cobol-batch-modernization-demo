      ******************************************************************
      * CBCRD06X - SORT AND DE-DUPLICATE THE PARTY WORK LIST           *
      *                                                                *
      * STEP020 OF JOB CBCRD06J.                                       *
      *                                                                *
      * CBCRD06A WRITES ONE RECORD PER ACCOUNT.  THE RISK SERVICE IS   *
      * DRIVEN ONCE PER PARTY, NOT ONCE PER ACCOUNT, SO THIS STEP      *
      * SORTS ON PARTY THEN ACCOUNT AND COLLAPSES EACH PARTY GROUP     *
      * INTO A SINGLE RECORD - THE EXPOSURE, POSTED AMOUNT, LIMIT AND  *
      * TRANSACTION COUNT ARE SUMMED AND THE WORST RISK BAND IN THE    *
      * GROUP IS CARRIED FORWARD.  THE LOWEST ACCOUNT NUMBER IN THE    *
      * GROUP IS KEPT AS THE REPRESENTATIVE ACCOUNT SO CBCRD06C HAS    *
      * SOMETHING TO UPDATE AGAINST.                                   *
      *                                                                *
      * THE SORT IS AN INTERNAL COBOL SORT RATHER THAN A DFSORT STEP   *
      * BECAUSE THE SUMMARISING CANNOT BE EXPRESSED IN SORT CONTROL    *
      * CARDS - THE BAND PRECEDENCE IS NOT A COLLATING ORDER.          *
      *                                                                *
      * CALLED BY   - JCL ONLY                                         *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - PARTYWK  QSAM INPUT   LRECL 150                  *
      *             - PARTYSRT QSAM OUTPUT  LRECL 150                  *
      *             - SORTWK   INTERNAL SORT WORK                      *
      *                                                                *
      * RETURN CODE - 0000 NORMAL                                      *
      *               0004 NOTHING TO DISPATCH                         *
      *               0012 FATAL                                       *
      * USER ABEND  - U0602 FILE OPEN OR I/O FAILURE                   *
      *               U0604 SORT FAILED                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD06X.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  2001-04-19.
      *
      * MAINTENANCE
      * 2001-04-19 CRD2110 ORIGINAL
      * 2006-06-12 CRD5001 WORST BAND IN THE GROUP CARRIED FORWARD
      *                    INSTEAD OF THE FIRST ONE READ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORK-FILE     ASSIGN TO PARTYWK
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-WORK-STATUS.
      *
           SELECT SORTED-FILE   ASSIGN TO PARTYSRT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-SORTED-STATUS.
      *
           SELECT SORT-FILE     ASSIGN TO SORTWK.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  WORK-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       01  WORK-REC                        PIC X(150).
      *
       FD  SORTED-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       01  SORTED-REC                      PIC X(150).
      *
       SD  SORT-FILE
           RECORD CONTAINS 150 CHARACTERS.
       01  SORT-REC.
           05  SR-PARTY-ID                 PIC X(11).
           05  SR-ACCT-ID                  PIC 9(11).
           05  SR-REST                     PIC X(128).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD06X'.
      *
       01  WS-STATUS-FIELDS.
           05  WS-WORK-STATUS              PIC X(2)  VALUE '00'.
               88  WS-WORK-OK                        VALUE '00'.
           05  WS-SORTED-STATUS            PIC X(2)  VALUE '00'.
               88  WS-SORTED-OK                      VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-FIRST-SW                 PIC X     VALUE 'Y'.
               88  WS-FIRST-RECORD                   VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-IN-CNT                   PIC 9(9)  VALUE ZERO.
           05  WS-OUT-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-COLLAPSED-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-MULTI-ACCT-CNT           PIC 9(9)  VALUE ZERO.
      *
       01  WS-HOLD-PARTY                   PIC X(11) VALUE SPACES.
       01  WS-GROUP-ACCT-CNT               PIC 9(4)  VALUE ZERO.
      *
       01  WS-BAND-RANK-INPUT              PIC X     VALUE SPACE.
       01  WS-BAND-RANK                    PIC 9     VALUE ZERO.
       01  WS-HOLD-BAND-RANK               PIC 9     VALUE ZERO.
      *
       01  WS-RETURN-CODE                  PIC 9(4)  VALUE ZERO.
       01  WS-ABEND-CODE                   PIC 9(4)  VALUE ZERO.
      *
       01  WS-CURRENT-DATE.
           05  WS-CD-DATE                  PIC 9(8).
           05  WS-CD-TIME                  PIC 9(8).
           05  WS-CD-FILLER                PIC X(5).
       01  WS-TIMESTAMP                    PIC X(26) VALUE SPACES.
      *
           COPY CVPWRK01Y.
      *
      *    THE ACCUMULATED GROUP RECORD.  SAME 150 BYTE LAYOUT AS
      *    CVPWRK01Y - KEEP THE TWO IN STEP IF THE COPYBOOK CHANGES.
       01  PARTY-HOLD-REC.
           05  PH-KEY.
               10  PH-PARTY-ID             PIC X(11).
               10  PH-ACCT-ID              PIC 9(11).
           05  PH-CUST-ID                  PIC 9(9).
           05  PH-CARD-NUM                 PIC X(16).
           05  PH-CYCLE-DATE               PIC 9(8).
           05  PH-CYCLE-ID                 PIC X(8).
           05  PH-PRODUCT-CD               PIC X(4).
           05  PH-POSTED-AMT               PIC S9(11)V99 COMP-3.
           05  PH-TXN-CNT                  PIC 9(5).
           05  PH-CURR-BAL                 PIC S9(11)V99 COMP-3.
           05  PH-CREDIT-LIMIT             PIC S9(11)V99 COMP-3.
           05  PH-EXPOSURE-AMT             PIC S9(11)V99 COMP-3.
           05  PH-RISK-BAND-OLD            PIC X.
           05  PH-RISK-BAND-NEW            PIC X.
           05  PH-OUTCOME.
               10  PH-RC                   PIC 9(4).
               10  PH-REASON-CD            PIC X(4).
               10  PH-ADVICE-CD            PIC X(4).
               10  PH-SANCTION-FLG         PIC X.
               10  PH-SCORE                PIC 9(3).
               10  PH-DISPATCH-TS          PIC X(26).
           05  PH-FILLER                   PIC X(6).
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
       01  WS-DISPLAY-CNT                  PIC ZZZ,ZZZ,ZZ9.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           STRING WS-CD-DATE(1:4) '-' WS-CD-DATE(5:2) '-'
                  WS-CD-DATE(7:2) '-' WS-CD-TIME(1:2) '.'
                  WS-CD-TIME(3:2) '.' WS-CD-TIME(5:2) '.000000'
             DELIMITED BY SIZE INTO WS-TIMESTAMP
           END-STRING
      *
           SORT SORT-FILE
                ON ASCENDING KEY SR-PARTY-ID
                                 SR-ACCT-ID
                USING WORK-FILE
                OUTPUT PROCEDURE IS 2000-COLLAPSE-SECTION
      *
           IF SORT-RETURN NOT = ZERO
               MOVE 'INTERNAL SORT OF THE WORK LIST FAILED'
                                           TO ER-MESSAGE
               MOVE 0604                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM 3000-PRINT-CONTROLS
           MOVE WS-RETURN-CODE             TO RETURN-CODE
           GOBACK
           .
      *
      ******************************************************************
      * 2000 - OUTPUT PROCEDURE.  ONE PASS, CONTROL BREAK ON PARTY.    *
      ******************************************************************
       2000-COLLAPSE-SECTION SECTION.
      *
       2000-COLLAPSE-START.
           OPEN OUTPUT SORTED-FILE
           IF NOT WS-SORTED-OK
               MOVE 'PARTYSRT'             TO ER-FILE-NAME
               MOVE WS-SORTED-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF SORTED WORK LIST FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM 2100-RETURN-RECORD
           PERFORM UNTIL WS-EOF
               IF WS-FIRST-RECORD
                   PERFORM 2200-START-GROUP
                   MOVE 'N'                TO WS-FIRST-SW
               ELSE
                   IF PW-PARTY-ID = WS-HOLD-PARTY
                       PERFORM 2300-ACCUMULATE
                   ELSE
                       PERFORM 2400-WRITE-GROUP
                       PERFORM 2200-START-GROUP
                   END-IF
               END-IF
               PERFORM 2100-RETURN-RECORD
           END-PERFORM
      *
           IF WS-IN-CNT > ZERO
               PERFORM 2400-WRITE-GROUP
           END-IF
      *
           CLOSE SORTED-FILE
           .
      *
       2100-RETURN-RECORD.
           RETURN SORT-FILE INTO PARTY-WORK-REC
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
               NOT AT END
                   ADD 1                   TO WS-IN-CNT
           END-RETURN
           .
      *
       2200-START-GROUP.
           MOVE PARTY-WORK-REC             TO PARTY-HOLD-REC
           MOVE PW-PARTY-ID                TO WS-HOLD-PARTY
           MOVE 1                          TO WS-GROUP-ACCT-CNT
           MOVE PW-RISK-BAND-OLD           TO WS-BAND-RANK-INPUT
           PERFORM 2500-RANK-BAND
           MOVE WS-BAND-RANK               TO WS-HOLD-BAND-RANK
           .
      *
       2300-ACCUMULATE.
           ADD 1                           TO WS-GROUP-ACCT-CNT
           ADD PW-POSTED-AMT               TO PH-POSTED-AMT
           ADD PW-TXN-CNT                  TO PH-TXN-CNT
           ADD PW-CURR-BAL                 TO PH-CURR-BAL
           ADD PW-CREDIT-LIMIT             TO PH-CREDIT-LIMIT
           ADD PW-EXPOSURE-AMT             TO PH-EXPOSURE-AMT
      *
           MOVE PW-RISK-BAND-OLD           TO WS-BAND-RANK-INPUT
           PERFORM 2500-RANK-BAND
           IF WS-BAND-RANK > WS-HOLD-BAND-RANK
               MOVE WS-BAND-RANK           TO WS-HOLD-BAND-RANK
               MOVE PW-RISK-BAND-OLD       TO PH-RISK-BAND-OLD
               MOVE PW-RISK-BAND-OLD       TO PH-RISK-BAND-NEW
           END-IF
           .
      *
       2400-WRITE-GROUP.
           IF WS-GROUP-ACCT-CNT > 1
               ADD 1                       TO WS-MULTI-ACCT-CNT
               COMPUTE WS-COLLAPSED-CNT =
                       WS-COLLAPSED-CNT + WS-GROUP-ACCT-CNT - 1
           END-IF
      *
           WRITE SORTED-REC FROM PARTY-HOLD-REC
           IF NOT WS-SORTED-OK
               MOVE 'PARTYSRT'             TO ER-FILE-NAME
               MOVE WS-SORTED-STATUS       TO ER-FILE-STATUS
               MOVE 'WRITE TO SORTED WORK LIST FAILED' TO ER-MESSAGE
               MOVE 0602                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           ADD 1                           TO WS-OUT-CNT
           .
      *
      ******************************************************************
      * 2500 - BAND PRECEDENCE.  X REFUSE IS WORST, THEN C, B, A.      *
      *        AN UNKNOWN BAND RANKS BELOW A SO IT NEVER MASKS A REAL  *
      *        ONE ON A MULTI ACCOUNT PARTY.                           *
      ******************************************************************
       2500-RANK-BAND.
           EVALUATE WS-BAND-RANK-INPUT
               WHEN 'X'
                   MOVE 4                  TO WS-BAND-RANK
               WHEN 'C'
                   MOVE 3                  TO WS-BAND-RANK
               WHEN 'B'
                   MOVE 2                  TO WS-BAND-RANK
               WHEN 'A'
                   MOVE 1                  TO WS-BAND-RANK
               WHEN OTHER
                   MOVE 0                  TO WS-BAND-RANK
           END-EVALUATE
           .
      *
       2900-COLLAPSE-EXIT.
           EXIT
           .
      *
       3000-CONTROL-SECTION SECTION.
      *
       3000-PRINT-CONTROLS.
           IF WS-OUT-CNT = ZERO
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
           END-IF
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-IN-CNT                  TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06X RECORDS SORTED    ' WS-DISPLAY-CNT
           MOVE WS-OUT-CNT                 TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06X PARTIES WRITTEN   ' WS-DISPLAY-CNT
           MOVE WS-MULTI-ACCT-CNT          TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06X MULTI ACCT PARTIES' WS-DISPLAY-CNT
           MOVE WS-COLLAPSED-CNT           TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD06X RECORDS COLLAPSED ' WS-DISPLAY-CNT
           DISPLAY '----------------------------------------------'
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'VSAM'                     TO ER-ERROR-TYPE
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD06X FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
