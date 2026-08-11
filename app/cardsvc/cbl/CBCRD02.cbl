      ******************************************************************
      * CBCRD02 - NIGHTLY EXTRACT VALIDATION AND REJECT SPLIT          *
      *                                                                *
      * STEP 2 OF THE CARDNITE CYCLE.  JOB CBCRD02J.                   *
      *                                                                *
      * EVERY EXTRACT RECORD IS EDITED.  THE 60 BYTE DETAIL AREA IS    *
      * ONLY MEANINGFUL UNDER THE AUTHORISATION TYPE, SO ALL OVERLAY   *
      * FIELDS ARE REFERENCED INSIDE AN EVALUATE AUTH-TYPE.  A TYPE    *
      * THE EDIT DOES NOT RECOGNISE IS REJECTED WITH REASON R001 AND   *
      * ITS DETAIL AREA IS NEVER LOOKED AT - THE SHORT VARIANTS LEAVE  *
      * WHATEVER THE PREVIOUS RECORD WROTE IN THE TAIL BYTES.          *
      *                                                                *
      * PARM  - CCYYMMDD CYCLE DATE, COMMA, REJECT TOLERANCE AS A      *
      *         WHOLE NUMBER OF PERCENT (E.G. 20250114,02).            *
      *         THE TOLERANCE IS A BUSINESS RULE AND IS DELIBERATELY   *
      *         NOT CODED HERE.                                        *
      *                                                                *
      * CALLED BY   - JCL ONLY                                         *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - AUTHEXTR QSAM INPUT   LRECL 300  (GDG 0)         *
      *             - AUTHCLN  QSAM OUTPUT  LRECL 300                  *
      *             - AUTHREJ  QSAM OUTPUT  LRECL 300                  *
      *             - CYCLCTL  VSAM KSDS I-O                           *
      *                                                                *
      * RETURN CODE - 0000 NO REJECTS                                  *
      *               0004 REJECTS WITHIN TOLERANCE                    *
      *               0008 REJECT TOLERANCE EXCEEDED - CHAIN HELD      *
      *               0012 FATAL                                       *
      * USER ABEND  - U0201 CYCLE CONTROL UNUSABLE                     *
      *               U0202 FILE OPEN OR I/O FAILURE                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD02.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1998-03-24.
      *
      * MAINTENANCE
      * 1998-03-24 CRD0117 ORIGINAL
      * 2003-02-17 CRD3301 REJECT TOLERANCE MOVED OUT TO THE PARM
      * 2009-08-30 CRD6620 REFUND ORIGINAL DATE WINDOWED ON PIVOT
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXTRACT-FILE  ASSIGN TO AUTHEXTR
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-EXTRACT-STATUS.
      *
           SELECT CLEAN-FILE    ASSIGN TO AUTHCLN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-CLEAN-STATUS.
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
       FD  EXTRACT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       01  EXTRACT-REC                     PIC X(300).
      *
       FD  CLEAN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       01  CLEAN-REC                       PIC X(300).
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD02 '.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP010 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-EXTRACT-STATUS           PIC X(2)  VALUE '00'.
               88  WS-EXTRACT-OK                     VALUE '00'.
               88  WS-EXTRACT-EOF                    VALUE '10'.
           05  WS-CLEAN-STATUS             PIC X(2)  VALUE '00'.
               88  WS-CLEAN-OK                       VALUE '00'.
           05  WS-REJECT-STATUS            PIC X(2)  VALUE '00'.
               88  WS-REJECT-OK                      VALUE '00'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
               88  WS-CYCLCTL-NOTFND                 VALUE '23'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-REJECT-SW                PIC X     VALUE 'N'.
               88  WS-REC-REJECTED                   VALUE 'Y'.
           05  WS-WARN-SW                  PIC X     VALUE 'N'.
               88  WS-REC-WARNED                     VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-CLEAN-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-WARN-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-REJECT-SEQ               PIC 9(9)  VALUE ZERO.
      *
      *    REJECT ANALYSIS BY REASON - DRIVEN OFF THE REASON TABLE
       01  WS-REASON-TABLE.
           05  WS-REASON-ENTRY OCCURS 10 TIMES
                       INDEXED BY WS-RSN-IDX.
               10  WS-RSN-CODE             PIC X(4).
               10  WS-RSN-COUNT            PIC 9(9).
               10  WS-RSN-TEXT             PIC X(40).
      *
       01  WS-REASON-SEED.
           05  FILLER PIC X(4)  VALUE 'R001'.
           05  FILLER PIC X(40) VALUE 'UNRECOGNISED AUTHORISATION TYPE'.
           05  FILLER PIC X(4)  VALUE 'R002'.
           05  FILLER PIC X(40) VALUE 'AMOUNT OUT OF RANGE'.
           05  FILLER PIC X(4)  VALUE 'R003'.
           05  FILLER PIC X(40) VALUE 'DATE INVALID OR OUT OF CYCLE'.
           05  FILLER PIC X(4)  VALUE 'R004'.
           05  FILLER PIC X(40) VALUE 'STATUS NOT POSTABLE'.
           05  FILLER PIC X(4)  VALUE 'R005'.
           05  FILLER PIC X(40) VALUE 'CURRENCY CODE NOT NUMERIC/BLANK'.
           05  FILLER PIC X(4)  VALUE 'R006'.
           05  FILLER PIC X(40) VALUE 'MERCHANT CATEGORY CODE INVALID'.
           05  FILLER PIC X(4)  VALUE 'R007'.
           05  FILLER PIC X(40) VALUE 'ACCOUNT OR CUSTOMER ID INVALID'.
           05  FILLER PIC X(4)  VALUE 'R008'.
           05  FILLER PIC X(40) VALUE 'AUTHORISATION ALREADY POSTED'.
           05  FILLER PIC X(4)  VALUE 'R009'.
           05  FILLER PIC X(40) VALUE 'CARD NUMBER NOT NUMERIC'.
           05  FILLER PIC X(4)  VALUE 'R010'.
           05  FILLER PIC X(40) VALUE 'POSTING REJECTED DOWNSTREAM'.
       01  WS-REASON-SEED-R REDEFINES WS-REASON-SEED.
           05  WS-SEED-ENTRY OCCURS 10 TIMES.
               10  WS-SEED-CODE            PIC X(4).
               10  WS-SEED-TEXT            PIC X(40).
      *
       01  WS-PARM-FIELDS.
           05  WS-CYCLE-DATE               PIC 9(8)  VALUE ZERO.
           05  WS-CYCLE-DATE-R REDEFINES WS-CYCLE-DATE.
               10  WS-CYCLE-CC             PIC 9(2).
               10  WS-CYCLE-YY             PIC 9(2).
               10  WS-CYCLE-MM             PIC 9(2).
               10  WS-CYCLE-DD             PIC 9(2).
           05  WS-TOLERANCE-PCT            PIC 9(2)  VALUE ZERO.
      *
       01  WS-WORK-FIELDS.
           05  WS-WORK-AMT                 PIC S9(11)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-REJECT-PCT               PIC S9(5)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-ORIG-DATE-6              PIC 9(6)  VALUE ZERO.
           05  WS-ORIG-DATE-6-R REDEFINES WS-ORIG-DATE-6.
               10  WS-ORIG-YY              PIC 9(2).
               10  WS-ORIG-MM              PIC 9(2).
               10  WS-ORIG-DD              PIC 9(2).
           05  WS-ORIG-DATE-8              PIC 9(8)  VALUE ZERO.
           05  WS-NUMERIC-TEST             PIC X(16) VALUE SPACES.
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
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
       01  WS-DISPLAY-CNT                  PIC ZZZ,ZZZ,ZZ9.
       01  WS-DISPLAY-PCT                  PIC ZZ9.99.
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
           PERFORM 2000-EDIT-RECORD
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
           PERFORM 1200-LOAD-REASON-TABLE
           PERFORM 1300-OPEN-FILES
           PERFORM 1400-READ-CYCLE-CONTROL
      *
           DISPLAY 'CBCRD02 - EXTRACT EDIT, CYCLE ' WS-CYCLE-DATE
           DISPLAY '          REJECT TOLERANCE ' WS-TOLERANCE-PCT
                   ' PERCENT'
           .
      *
       1100-EDIT-PARM.
           IF LK-PARM-LEN < 11
               MOVE 'PARM MUST BE CCYYMMDD,NN' TO ER-MESSAGE
               MOVE 0201                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
           MOVE LK-PARM-DATA(10:2)         TO WS-TOLERANCE-PCT
           .
      *
       1200-LOAD-REASON-TABLE.
           PERFORM VARYING WS-RSN-IDX FROM 1 BY 1
                     UNTIL WS-RSN-IDX > 10
               MOVE WS-SEED-CODE(WS-RSN-IDX)
                                           TO WS-RSN-CODE(WS-RSN-IDX)
               MOVE WS-SEED-TEXT(WS-RSN-IDX)
                                           TO WS-RSN-TEXT(WS-RSN-IDX)
               MOVE ZERO                   TO WS-RSN-COUNT(WS-RSN-IDX)
           END-PERFORM
           .
      *
       1300-OPEN-FILES.
           OPEN INPUT  EXTRACT-FILE
           IF NOT WS-EXTRACT-OK
               MOVE 'AUTHEXTR'             TO ER-FILE-NAME
               MOVE WS-EXTRACT-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF EXTRACT FAILED' TO ER-MESSAGE
               MOVE 0202                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           OPEN OUTPUT CLEAN-FILE
                       REJECT-FILE
           IF NOT WS-CLEAN-OK OR NOT WS-REJECT-OK
               MOVE 'AUTHCLN '             TO ER-FILE-NAME
               MOVE WS-CLEAN-STATUS        TO ER-FILE-STATUS
               MOVE 'OPEN OF OUTPUT FILES FAILED' TO ER-MESSAGE
               MOVE 0202                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           OPEN I-O    CYCLCTL-FILE
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0201                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       1400-READ-CYCLE-CONTROL.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD - RUN CBCRD01J FIRST'
                                           TO ER-MESSAGE
                   MOVE 0201               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE WS-STEP-NAME               TO CC-CURRENT-STEP
           .
      *
      ******************************************************************
      * 2000 - EDIT ONE RECORD                                         *
      ******************************************************************
       2000-EDIT-RECORD.
           READ EXTRACT-FILE INTO AUTH-EXTRACT-REC
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
                   GO TO 2000-EXIT
           END-READ
      *
           ADD 1                           TO WS-READ-CNT
           MOVE AX-AUTH-IMAGE              TO AUTH-RECORD
           MOVE 'N'                        TO WS-REJECT-SW
                                              WS-WARN-SW
           MOVE SPACES                     TO AX-EDIT-REASON
      *
           PERFORM 2100-EDIT-COMMON
           IF NOT WS-REC-REJECTED
               PERFORM 2200-EDIT-DETAIL
           END-IF
      *
           IF WS-REC-REJECTED
               PERFORM 2800-WRITE-REJECT
           ELSE
               PERFORM 2900-WRITE-CLEAN
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2100 - EDITS THAT APPLY TO EVERY TYPE                          *
      ******************************************************************
       2100-EDIT-COMMON.
           MOVE AUTH-CARD-NUM              TO WS-NUMERIC-TEST
           IF WS-NUMERIC-TEST NOT NUMERIC
               MOVE 'R009'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2100-EXIT
           END-IF
      *
           IF AUTH-ACCT-ID NOT NUMERIC
           OR AUTH-ACCT-ID = ZERO
           OR AUTH-CUST-ID NOT NUMERIC
               MOVE 'R007'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2100-EXIT
           END-IF
      *
           IF AUTH-DATE NOT = AX-CYCLE-DATE
               MOVE 'R003'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2100-EXIT
           END-IF
      *
           IF AUTH-POSTED-FLG = 'Y'
               MOVE 'R008'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2100-EXIT
           END-IF
      *
           IF NOT AUTH-APPROVED
           AND NOT AUTH-REVERSED
               MOVE 'R004'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
           END-IF
           .
       2100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2200 - DETAIL EDIT.                                            *
      *                                                                *
      * THE OVERLAY FIELDS ARE ONLY ADDRESSED UNDER THE MATCHING       *
      * AUTH-TYPE.  A CASH ADVANCE IS 33 BYTES SHORTER THAN A          *
      * PURCHASE, SO REFERENCING AP-ENTRY-MODE ON A CASH RECORD WOULD  *
      * PICK UP THE PREVIOUS RECORD'S RESIDUE.                         *
      ******************************************************************
       2200-EDIT-DETAIL.
           EVALUATE TRUE
               WHEN AUTH-PURCHASE-TYPE
                   PERFORM 2210-EDIT-PURCHASE
               WHEN AUTH-CASH-ADV-TYPE
                   PERFORM 2220-EDIT-CASH-ADV
               WHEN AUTH-REFUND-TYPE
                   PERFORM 2230-EDIT-REFUND
               WHEN OTHER
                   MOVE 'R001'             TO AX-EDIT-REASON
                   MOVE 'Y'                TO WS-REJECT-SW
           END-EVALUATE
           .
      *
       2210-EDIT-PURCHASE.
           IF AP-AMOUNT NOT NUMERIC
           OR AP-AMOUNT <= ZERO
           OR AP-AMOUNT > WS-MAX-AUTH-AMT
               MOVE 'R002'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2210-EXIT
           END-IF
      *
           IF AP-CURRENCY = SPACES OR LOW-VALUES
               MOVE 'R005'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2210-EXIT
           END-IF
      *
           IF AP-MCC NOT NUMERIC
           OR AP-MCC = ZERO
               MOVE 'R006'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2210-EXIT
           END-IF
      *
      *    A BLANK MERCHANT ID IS TOLERATED - CBCRD03 WILL DEFAULT THE
      *    SETTLEMENT ROUTE - BUT IT IS COUNTED AS A WARNING.
           IF AP-MERCHANT-ID = SPACES
               MOVE 'Y'                    TO WS-WARN-SW
           END-IF
      *
           MOVE AP-MCC                     TO AX-MCC
           MOVE AP-MERCH-NAME              TO AX-MERCH-NAME
           MOVE AP-AMOUNT                  TO WS-WORK-AMT
           .
       2210-EXIT.
           EXIT
           .
      *
       2220-EDIT-CASH-ADV.
           IF AC-AMOUNT NOT NUMERIC
           OR AC-AMOUNT <= ZERO
           OR AC-AMOUNT > WS-MAX-AUTH-AMT
               MOVE 'R002'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2220-EXIT
           END-IF
      *
           IF AC-FEE NOT NUMERIC
           OR AC-FEE < ZERO
               MOVE 'R002'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2220-EXIT
           END-IF
      *
           IF AC-CURRENCY = SPACES OR LOW-VALUES
               MOVE 'R005'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2220-EXIT
           END-IF
      *
      *    AN ATM WITHDRAWAL HAS NO MERCHANT - THE ACQUIRER STANDS IN.
           IF AC-ACQUIRER-ID = SPACES
               MOVE 'Y'                    TO WS-WARN-SW
           END-IF
      *
           MOVE ZERO                       TO AX-MCC
           MOVE AC-ACQUIRER-ID             TO AX-ACQUIRER-ID
           MOVE AC-AMOUNT                  TO WS-WORK-AMT
           .
       2220-EXIT.
           EXIT
           .
      *
       2230-EDIT-REFUND.
           IF AR-AMOUNT NOT NUMERIC
           OR AR-AMOUNT <= ZERO
               MOVE 'R002'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2230-EXIT
           END-IF
      *
           IF AR-ORIG-AUTH-ID = SPACES
               MOVE 'R003'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2230-EXIT
           END-IF
      *
      *    ORIGINAL DATE IS A LEGACY SIX DIGIT DATE.  WINDOW IT ON THE
      *    CENTURY PIVOT BEFORE COMPARING WITH THE CYCLE DATE.
           MOVE AR-ORIG-DATE               TO WS-ORIG-DATE-6
           PERFORM 2240-WINDOW-DATE
           IF WS-ORIG-DATE-8 > AX-CYCLE-DATE
               MOVE 'R003'                 TO AX-EDIT-REASON
               MOVE 'Y'                    TO WS-REJECT-SW
               GO TO 2230-EXIT
           END-IF
      *
           MOVE ZERO                       TO AX-MCC
           COMPUTE WS-WORK-AMT = AR-AMOUNT * -1
           .
       2230-EXIT.
           EXIT
           .
      *
       2240-WINDOW-DATE.
           IF WS-ORIG-MM < 01 OR WS-ORIG-MM > 12
           OR WS-ORIG-DD < 01 OR WS-ORIG-DD > 31
               MOVE 99999999               TO WS-ORIG-DATE-8
               GO TO 2240-EXIT
           END-IF
      *
           IF WS-ORIG-YY > WS-CENTURY-PIVOT
               COMPUTE WS-ORIG-DATE-8 =
                       (WS-CENTURY-19 * 1000000) + WS-ORIG-DATE-6
           ELSE
               COMPUTE WS-ORIG-DATE-8 =
                       (WS-CENTURY-20 * 1000000) + WS-ORIG-DATE-6
           END-IF
           .
       2240-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2800 - REJECT                                                  *
      ******************************************************************
       2800-WRITE-REJECT.
           ADD 1                           TO WS-REJECT-CNT
                                              WS-REJECT-SEQ
           PERFORM 2810-COUNT-REASON
      *
           INITIALIZE CYCLE-REJECT-REC
           MOVE AX-CYCLE-DATE              TO RJ-CYCLE-DATE
           MOVE AX-CYCLE-ID                TO RJ-CYCLE-ID
           MOVE WS-REJECT-SEQ              TO RJ-REJECT-SEQ
           MOVE AX-EDIT-REASON             TO RJ-REASON-CD
           MOVE WS-RSN-TEXT(WS-RSN-IDX)    TO RJ-REASON-TXT
           MOVE WS-PROGRAM-ID              TO RJ-DETECT-PGM
           MOVE WS-STEP-NAME               TO RJ-DETECT-STEP
           MOVE AX-AUTH-IMAGE              TO RJ-AUTH-IMAGE
      *
           WRITE REJECT-REC FROM CYCLE-REJECT-REC
           IF NOT WS-REJECT-OK
               MOVE 'AUTHREJ '             TO ER-FILE-NAME
               MOVE WS-REJECT-STATUS       TO ER-FILE-STATUS
               MOVE 'WRITE TO REJECT FILE FAILED' TO ER-MESSAGE
               MOVE 0202                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       2810-COUNT-REASON.
           SET WS-RSN-IDX TO 1
           SEARCH WS-REASON-ENTRY
               AT END
                   SET WS-RSN-IDX TO 1
               WHEN WS-RSN-CODE(WS-RSN-IDX) = AX-EDIT-REASON
                   ADD 1                   TO WS-RSN-COUNT(WS-RSN-IDX)
           END-SEARCH
           .
      *
       2900-WRITE-CLEAN.
           ADD 1                           TO WS-CLEAN-CNT
           IF WS-REC-WARNED
               ADD 1                       TO WS-WARN-CNT
               MOVE 'W'                    TO AX-EDIT-STATUS
           ELSE
               MOVE 'C'                    TO AX-EDIT-STATUS
           END-IF
      *
           WRITE CLEAN-REC FROM AUTH-EXTRACT-REC
           IF NOT WS-CLEAN-OK
               MOVE 'AUTHCLN '             TO ER-FILE-NAME
               MOVE WS-CLEAN-STATUS        TO ER-FILE-STATUS
               MOVE 'WRITE TO CLEAN FILE FAILED' TO ER-MESSAGE
               MOVE 0202                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 3000 - TERMINATION AND RETURN CODE                             *
      ******************************************************************
       3000-TERMINATE.
           PERFORM 3100-SET-RETURN-CODE
           PERFORM 3200-UPDATE-CONTROL
           PERFORM 3300-PRINT-CONTROLS
      *
           CLOSE EXTRACT-FILE
                 CLEAN-FILE
                 REJECT-FILE
                 CYCLCTL-FILE
           .
      *
       3100-SET-RETURN-CODE.
           IF WS-READ-CNT = ZERO
               MOVE ZERO                   TO WS-REJECT-PCT
           ELSE
               COMPUTE WS-REJECT-PCT ROUNDED =
                       (WS-REJECT-CNT * 100) / WS-READ-CNT
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-REJECT-CNT = ZERO
                   MOVE WS-RC-OK           TO WS-RETURN-CODE
               WHEN WS-REJECT-PCT > WS-TOLERANCE-PCT
                   MOVE WS-RC-ERROR        TO WS-RETURN-CODE
                   DISPLAY 'CBCRD02 - REJECT TOLERANCE EXCEEDED'
               WHEN OTHER
                   MOVE WS-RC-WARNING      TO WS-RETURN-CODE
           END-EVALUATE
           .
      *
       3200-UPDATE-CONTROL.
           ADD WS-CLEAN-CNT                TO CC-RECS-WRITTEN
           MOVE WS-REJECT-CNT              TO CC-RECS-REJECTED
           IF WS-RETURN-CODE > WS-RC-WARNING
               MOVE 'F'                    TO CC-STATUS
           ELSE
               MOVE WS-STEP-NAME           TO CC-LAST-GOOD-STEP
           END-IF
      *
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'REWRITE OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0201                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       3300-PRINT-CONTROLS.
           DISPLAY '----------------------------------------------'
           MOVE WS-READ-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD02 RECORDS READ     ' WS-DISPLAY-CNT
           MOVE WS-CLEAN-CNT               TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD02 CLEAN            ' WS-DISPLAY-CNT
           MOVE WS-WARN-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD02 CLEAN WITH WARN  ' WS-DISPLAY-CNT
           MOVE WS-REJECT-CNT              TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD02 REJECTED         ' WS-DISPLAY-CNT
           MOVE WS-REJECT-PCT              TO WS-DISPLAY-PCT
           DISPLAY 'CBCRD02 REJECT PERCENT   ' WS-DISPLAY-PCT
           DISPLAY 'CBCRD02 REJECT ANALYSIS'
           PERFORM VARYING WS-RSN-IDX FROM 1 BY 1
                     UNTIL WS-RSN-IDX > 10
               IF WS-RSN-COUNT(WS-RSN-IDX) > ZERO
                   MOVE WS-RSN-COUNT(WS-RSN-IDX)
                                           TO WS-DISPLAY-CNT
                   DISPLAY '        ' WS-RSN-CODE(WS-RSN-IDX) ' '
                           WS-RSN-TEXT(WS-RSN-IDX) ' '
                           WS-DISPLAY-CNT
               END-IF
           END-PERFORM
           DISPLAY '----------------------------------------------'
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'DATA'                     TO ER-ERROR-TYPE
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD02 FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
