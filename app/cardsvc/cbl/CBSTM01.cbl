      ******************************************************************
      * CBSTM01 - PAPER STATEMENT PRINT IMAGE                          *
      *                                                                *
      * REACHED DYNAMICALLY.  THE CALLER MOVES 'STMT' AND 'PAPR' INTO  *
      * THE ROUTE REQUEST AND CALLS CBCRD90, WHICH CALLS THIS PROGRAM  *
      * WITH THE STATEMENT RECORD AND A RETURN AREA.                   *
      *                                                                *
      * CALLED BY   - CBCRD90 ON BEHALF OF CBBIL04                     *
      * CALLS       - NONE                                             *
      * WRITES      - STMTPRT (FBA 133, CARRIAGE CONTROL COLUMN 1)     *
      *                                                                *
      * CBCRD90 CANCELS THE RESOLVED MODULE AFTER EVERY CALL, SO THIS  *
      * PROGRAM CANNOT HOLD THE PRINT FILE OPEN BETWEEN STATEMENTS.    *
      * THE FILE IS OPENED EXTEND, WRITTEN AND CLOSED FOR EACH         *
      * STATEMENT.  THE COST WAS RAISED UNDER CR-4471 AND NOT FUNDED.  *
      * THE JOB MUST ALLOCATE STMTPRT WITH DISP=MOD.                   *
      *                                                                *
      * A CALL WITH STMT-ACCT-ID ZERO AND FORMAT CODE 'CLOS' IS THE    *
      * END OF RUN SIGNAL AND PRINTS THE END OF REPORT BANNER ONLY.    *
      *                                                                *
      * THE LINE ARRAY IS PRINTED IN CATEGORY ORDER, NOT ARRAY ORDER.  *
      * A CATEGORY THAT DOES NOT FIT ON THE CURRENT PAGE SPILLS ONTO A *
      * CONTINUATION PAGE CARRYING THE WORD CONTINUED.                 *
      *                                                                *
      * RETURN AREA - BR-RETURN-CD, BR-LINES-OUT, BR-PAGES-OUT         *
      *   00 - PRINTED                                                 *
      *   04 - PRINTED WITH A DATA WARNING                             *
      *   12 - PRINT FILE FAILURE                                      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBSTM01.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STMTPRT-FILE ASSIGN TO STMTPRT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTPRT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  STMTPRT-FILE
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  STMTPRT-REC                     PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBSTM01 '.
      *
       01  WS-STMTPRT-STATUS               PIC X(2)  VALUE '00'.
           88  WS-STMTPRT-OK                         VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW                 PIC X     VALUE 'N'.
               88  WS-ERROR                          VALUE 'Y'.
           05  WS-WARNING-SW               PIC X     VALUE 'N'.
               88  WS-WARNING                        VALUE 'Y'.
           05  WS-CONTINUED-SW             PIC X     VALUE 'N'.
               88  WS-CONTINUED                      VALUE 'Y'.
      *
       01  WS-WORK.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE ZERO.
           05  WS-PAGE-NBR                 PIC 9(3)  VALUE ZERO.
           05  WS-LINES-OUT                PIC 9(6)  VALUE ZERO.
           05  WS-LINES-PER-PAGE           PIC 9(3)  VALUE 052.
           05  WS-IDX                      PIC S9(4) COMP VALUE ZERO.
           05  WS-CAT-IDX                  PIC S9(4) COMP VALUE ZERO.
           05  WS-CAT-LINES                PIC S9(4) COMP VALUE ZERO.
           05  WS-CAT-TOTAL                PIC S9(11)V99 COMP-3.
           05  WS-LINE-CATEGORY            PIC 9     VALUE ZERO.
      *
      ******************************************************************
      * CATEGORIES ARE PRINTED IN THIS ORDER.  A LINE IS PLACED IN A   *
      * CATEGORY FROM ITS DESCRIPTION AND ITS DEBIT / CREDIT FLAG -    *
      * THE STATEMENT LINE ITSELF CARRIES NO TRANSACTION TYPE.         *
      ******************************************************************
       01  WS-CATEGORY-TABLE.
           05  FILLER                      PIC X(28) VALUE
               'PAYMENTS AND CREDITS        '.
           05  FILLER                      PIC X(28) VALUE
               'PURCHASES AND ADJUSTMENTS   '.
           05  FILLER                      PIC X(28) VALUE
               'CASH ADVANCES               '.
           05  FILLER                      PIC X(28) VALUE
               'FEES CHARGED                '.
           05  FILLER                      PIC X(28) VALUE
               'INTEREST CHARGED            '.
           05  FILLER                      PIC X(28) VALUE
               'ITEMS UNDER DISPUTE         '.
       01  WS-CATEGORY-LIST REDEFINES WS-CATEGORY-TABLE.
           05  WS-CATEGORY-NAME OCCURS 6 TIMES
                                           PIC X(28).
      *
       01  WS-DATE-EDIT.
           05  WS-DE-MM                    PIC 9(2).
           05  FILLER                      PIC X     VALUE '/'.
           05  WS-DE-DD                    PIC 9(2).
           05  FILLER                      PIC X     VALUE '/'.
           05  WS-DE-YY                    PIC 9(2).
      *
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-YYYY                  PIC 9(4).
           05  WS-DW-YY REDEFINES WS-DW-YYYY.
               10  WS-DW-CC                PIC 9(2).
               10  WS-DW-YEAR              PIC 9(2).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
      *
       01  WS-DESC-UPPER                   PIC X(40) VALUE SPACES.
      *
      ******************************************************************
      * PRINT LINES.  CARRIAGE CONTROL IN COLUMN 1 -                   *
      *   1 SKIP TO CHANNEL 1   - SPACE ONE   0 SPACE TWO   + OVERPRINT*
      ******************************************************************
       01  PL-HEAD1.
           05  FILLER                      PIC X     VALUE '1'.
           05  FILLER                      PIC X(24) VALUE
               'FIRST CONTINENTAL BANK  '.
           05  FILLER                      PIC X(28) VALUE
               'CREDIT CARD STATEMENT       '.
           05  FILLER                      PIC X(14) VALUE
               'STATEMENT DATE'.
           05  PL1-CYCLE-DATE              PIC X(8).
           05  FILLER                      PIC X(9)  VALUE SPACES.
           05  FILLER                      PIC X(5)  VALUE 'PAGE '.
           05  PL1-PAGE                    PIC ZZ9.
           05  FILLER                      PIC X(11) VALUE SPACES.
           05  PL1-CONTINUED               PIC X(31).
      *
       01  PL-HEAD2.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(16) VALUE
               'ACCOUNT NUMBER  '.
           05  PL2-ACCT-ID                 PIC 9(11).
           05  FILLER                      PIC X(6)  VALUE SPACES.
           05  FILLER                      PIC X(16) VALUE
               'STATEMENT NUMBER'.
           05  PL2-STMT-NBR                PIC 9(6).
           05  FILLER                      PIC X(6)  VALUE SPACES.
           05  FILLER                      PIC X(16) VALUE
               'BILLING PERIOD  '.
           05  PL2-FROM-DATE               PIC X(8).
           05  FILLER                      PIC X(3)  VALUE ' - '.
           05  PL2-TO-DATE                 PIC X(8).
           05  FILLER                      PIC X(36) VALUE SPACES.
      *
       01  PL-CAT-HEAD.
           05  FILLER                      PIC X     VALUE '0'.
           05  PLC-CATEGORY                PIC X(28).
           05  FILLER                      PIC X(104) VALUE SPACES.
      *
       01  PL-COL-HEAD.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(132) VALUE
               '  TRANS DATE  POST DATE  DESCRIPTION                 '
            &  '                          REFERENCE            AMOUNT'
            &  '                                               '.
      *
       01  PL-DETAIL.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  PLD-TRAN-DATE               PIC X(8).
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  PLD-POST-DATE               PIC X(8).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  PLD-DESC                    PIC X(40).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  PLD-REF                     PIC X(16).
           05  PLD-AMOUNT                  PIC ---,---,--9.99.
           05  PLD-DR-CR                   PIC X(2).
           05  FILLER                      PIC X(34) VALUE SPACES.
      *
       01  PL-CAT-TOTAL.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(64) VALUE SPACES.
           05  FILLER                      PIC X(18) VALUE
               'CATEGORY TOTAL    '.
           05  PLT-AMOUNT                  PIC ---,---,--9.99.
           05  FILLER                      PIC X(37) VALUE SPACES.
      *
       01  PL-BOX-TOP.
           05  FILLER                      PIC X     VALUE '0'.
           05  FILLER                      PIC X(52) VALUE
               '+--------------------------------------------------+'.
           05  FILLER                      PIC X(80) VALUE SPACES.
      *
       01  PL-BOX-LINE.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X     VALUE '|'.
           05  PLB-TEXT                    PIC X(30).
           05  PLB-AMOUNT                  PIC ---,---,--9.99.
           05  FILLER                      PIC X(6)  VALUE SPACES.
           05  FILLER                      PIC X     VALUE '|'.
           05  FILLER                      PIC X(80) VALUE SPACES.
      *
       01  PL-BOX-BOTTOM.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(52) VALUE
               '+--------------------------------------------------+'.
           05  FILLER                      PIC X(80) VALUE SPACES.
      *
       01  PL-WARN1.
           05  FILLER                      PIC X     VALUE '0'.
           05  FILLER                      PIC X(132) VALUE
               'MINIMUM PAYMENT WARNING - IF YOU MAKE ONLY THE MINIMU'
            &  'M PAYMENT EACH PERIOD YOU WILL PAY MORE IN INTEREST  '
            &  'AND IT WILL TAKE LONGER TO PAY    '.
      *
       01  PL-WARN2.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(132) VALUE
               'OFF YOUR BALANCE.  A LATE PAYMENT FEE AND A HIGHER PE'
            &  'NALTY RATE MAY APPLY IF WE DO NOT RECEIVE YOUR MINIMU'
            &  'M PAYMENT BY THE DUE DATE.        '.
      *
       01  PL-DUE.
           05  FILLER                      PIC X     VALUE '0'.
           05  FILLER                      PIC X(20) VALUE
               'PAYMENT DUE DATE    '.
           05  PLU-DUE-DATE                PIC X(8).
           05  FILLER                      PIC X(8)  VALUE SPACES.
           05  FILLER                      PIC X(20) VALUE
               'MINIMUM PAYMENT DUE '.
           05  PLU-MIN-PAY                 PIC ---,---,--9.99.
           05  FILLER                      PIC X(63) VALUE SPACES.
      *
       01  PL-ENDBANNER.
           05  FILLER                      PIC X     VALUE '1'.
           05  FILLER                      PIC X(132) VALUE
               '*** END OF STATEMENT PRINT RUN ***                   '
            &  '                                                     '
            &  '                                  '.
      *
       01  PL-BLANK.
           05  FILLER                      PIC X     VALUE ' '.
           05  FILLER                      PIC X(132) VALUE SPACES.
      *
           COPY CVCONSTY.
      *
       LINKAGE SECTION.
           COPY CVSTMT01Y.
           COPY CVBRTN1Y.
      *
      ******************************************************************
       PROCEDURE DIVISION USING STMT-RECORD
                                BATCH-RETURN-AREA.
      *
       0000-MAIN-LINE.
           MOVE ZERO                       TO BR-RETURN-CD
                                              BR-LINES-OUT
                                              BR-PAGES-OUT
           MOVE 'CBSTM01 '                 TO BR-RETURN-PGM
           MOVE SPACES                     TO BR-RETURN-MSG
           MOVE 'N'                        TO WS-ERROR-SW
                                              WS-WARNING-SW
           MOVE ZERO                       TO WS-LINE-CNT
                                              WS-PAGE-NBR
                                              WS-LINES-OUT
      *
           PERFORM 1000-OPEN-PRINT
           IF WS-ERROR
               GO TO 0000-TERMINATE
           END-IF
      *
           IF STMT-FORMAT-CD = 'CLOS'
               PERFORM 7000-END-BANNER
           ELSE
               PERFORM 2000-PRINT-STATEMENT
           END-IF
      *
           PERFORM 8000-CLOSE-PRINT
           .
       0000-TERMINATE.
           MOVE WS-LINES-OUT               TO BR-LINES-OUT
           MOVE WS-PAGE-NBR                TO BR-PAGES-OUT
           EVALUATE TRUE
               WHEN WS-ERROR
                   MOVE 12                 TO BR-RETURN-CD
               WHEN WS-WARNING
                   MOVE 4                  TO BR-RETURN-CD
               WHEN OTHER
                   MOVE 0                  TO BR-RETURN-CD
           END-EVALUATE
           GOBACK
           .
      *
       1000-OPEN-PRINT.
           OPEN EXTEND STMTPRT-FILE
           IF NOT WS-STMTPRT-OK
               MOVE 'Y'                    TO WS-ERROR-SW
               MOVE 'STMTPRT OPEN FAILED'  TO BR-RETURN-MSG
               DISPLAY 'CBSTM01 STMTPRT OPEN STATUS '
                       WS-STMTPRT-STATUS
           END-IF
           .
      *
       2000-PRINT-STATEMENT.
           IF STMT-LINE-CNT = ZERO
           OR STMT-LINE-CNT > 300
               MOVE 'Y'                    TO WS-WARNING-SW
               MOVE 'LINE COUNT OUT OF RANGE - HEADER ONLY PRINTED'
                                           TO BR-RETURN-MSG
               PERFORM 3000-PAGE-HEAD
               PERFORM 5000-SUMMARY-BOX
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 3000-PAGE-HEAD
      *
           PERFORM VARYING WS-CAT-IDX FROM 1 BY 1
                     UNTIL WS-CAT-IDX > 6
                        OR WS-ERROR
               PERFORM 4000-PRINT-CATEGORY
           END-PERFORM
      *
           IF NOT WS-ERROR
               PERFORM 5000-SUMMARY-BOX
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - PAGE HEADING.  EVERY PAGE AFTER THE FIRST CARRIES THE   *
      *        CONTINUED LEGEND.                                       *
      ******************************************************************
       3000-PAGE-HEAD.
           ADD 1                           TO WS-PAGE-NBR
           MOVE WS-PAGE-NBR                TO PL1-PAGE
      *
           MOVE STMT-CYCLE-DATE            TO WS-DATE-WORK
           PERFORM 6500-EDIT-DATE
           MOVE WS-DATE-EDIT               TO PL1-CYCLE-DATE
      *
           IF WS-PAGE-NBR > 1
               MOVE '  ... STATEMENT CONTINUED ...  '
                                           TO PL1-CONTINUED
               MOVE 'Y'                    TO WS-CONTINUED-SW
           ELSE
               MOVE SPACES                 TO PL1-CONTINUED
           END-IF
      *
           MOVE STMT-ACCT-ID               TO PL2-ACCT-ID
           MOVE STMT-NUMBER                TO PL2-STMT-NBR
           MOVE STMT-PERIOD-FROM           TO WS-DATE-WORK
           PERFORM 6500-EDIT-DATE
           MOVE WS-DATE-EDIT               TO PL2-FROM-DATE
           MOVE STMT-PERIOD-TO             TO WS-DATE-WORK
           PERFORM 6500-EDIT-DATE
           MOVE WS-DATE-EDIT               TO PL2-TO-DATE
      *
           PERFORM 6000-WRITE-LINE-HEAD1
           MOVE PL-HEAD2                   TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           MOVE PL-BLANK                   TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           MOVE 3                          TO WS-LINE-CNT
           .
      *
       6000-WRITE-LINE-HEAD1.
           MOVE PL-HEAD1                   TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           .
      *
      ******************************************************************
      * 4000 - ONE CATEGORY.  THE ARRAY IS SCANNED ONCE PER CATEGORY - *
      *        300 LINES BY SIX PASSES IS CHEAPER THAN A SORT.         *
      ******************************************************************
       4000-PRINT-CATEGORY.
           MOVE ZERO                       TO WS-CAT-LINES
                                              WS-CAT-TOTAL
      *
      *    FIRST PASS - IS THERE ANYTHING IN THIS CATEGORY AT ALL
           PERFORM VARYING WS-IDX FROM 1 BY 1
                     UNTIL WS-IDX > STMT-LINE-CNT
               PERFORM 4500-CLASSIFY-LINE
               IF WS-LINE-CATEGORY = WS-CAT-IDX
                   ADD 1                   TO WS-CAT-LINES
               END-IF
           END-PERFORM
      *
           IF WS-CAT-LINES = ZERO
               GO TO 4000-EXIT
           END-IF
      *
           PERFORM 4100-CATEGORY-HEAD
      *
           PERFORM VARYING WS-IDX FROM 1 BY 1
                     UNTIL WS-IDX > STMT-LINE-CNT
                        OR WS-ERROR
               PERFORM 4500-CLASSIFY-LINE
               IF WS-LINE-CATEGORY = WS-CAT-IDX
                   PERFORM 4200-PRINT-LINE
               END-IF
           END-PERFORM
      *
           MOVE WS-CAT-TOTAL               TO PLT-AMOUNT
           MOVE PL-CAT-TOTAL               TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           ADD 1                           TO WS-LINE-CNT
           .
       4000-EXIT.
           EXIT
           .
      *
       4100-CATEGORY-HEAD.
      *    A CATEGORY HEADING NEEDS THREE LINES BEHIND IT OR IT GOES
      *    OVER THE PAGE ON ITS OWN.
           IF WS-LINE-CNT + 4 > WS-LINES-PER-PAGE
               PERFORM 3000-PAGE-HEAD
           END-IF
      *
           MOVE WS-CATEGORY-NAME(WS-CAT-IDX)
                                           TO PLC-CATEGORY
           MOVE PL-CAT-HEAD                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           MOVE PL-COL-HEAD                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           ADD 3                           TO WS-LINE-CNT
           .
      *
       4200-PRINT-LINE.
           IF WS-LINE-CNT + 1 > WS-LINES-PER-PAGE
               PERFORM 3000-PAGE-HEAD
               MOVE WS-CATEGORY-NAME(WS-CAT-IDX)
                                           TO PLC-CATEGORY
               MOVE ' CONTINUED'           TO PLC-CATEGORY(19:10)
               MOVE PL-CAT-HEAD            TO STMTPRT-REC
               PERFORM 6100-WRITE-LINE
               MOVE PL-COL-HEAD            TO STMTPRT-REC
               PERFORM 6100-WRITE-LINE
               ADD 3                       TO WS-LINE-CNT
           END-IF
      *
           MOVE STMT-LN-DATE(WS-IDX)       TO WS-DATE-WORK
           PERFORM 6500-EDIT-DATE
           MOVE WS-DATE-EDIT               TO PLD-TRAN-DATE
      *
           MOVE STMT-LN-POST-DATE(WS-IDX)  TO WS-DATE-WORK
           PERFORM 6500-EDIT-DATE
           MOVE WS-DATE-EDIT               TO PLD-POST-DATE
      *
           MOVE STMT-LN-DESC(WS-IDX)       TO PLD-DESC
           MOVE STMT-LN-REF(WS-IDX)        TO PLD-REF
           MOVE STMT-LN-AMT(WS-IDX)        TO PLD-AMOUNT
      *
           IF STMT-LN-DR-CR(WS-IDX) = 'C'
               MOVE 'CR'                   TO PLD-DR-CR
           ELSE
               MOVE '  '                   TO PLD-DR-CR
           END-IF
      *
           MOVE PL-DETAIL                  TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           ADD 1                           TO WS-LINE-CNT
           ADD STMT-LN-AMT(WS-IDX)         TO WS-CAT-TOTAL
           .
      *
      ******************************************************************
      * 4500 - CATEGORY OF ONE LINE                                    *
      *   1 PAYMENTS AND CREDITS    2 PURCHASES    3 CASH ADVANCES     *
      *   4 FEES                    5 INTEREST     6 DISPUTED          *
      ******************************************************************
       4500-CLASSIFY-LINE.
           MOVE STMT-LN-DESC(WS-IDX)       TO WS-DESC-UPPER
           MOVE 2                          TO WS-LINE-CATEGORY
      *
           EVALUATE TRUE
               WHEN WS-DESC-UPPER(31:10) = '*DISPUTED*'
                   MOVE 6                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:8) = 'INTEREST'
                   MOVE 5                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:11) = 'FINANCE CHG'
                   MOVE 5                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:3) = 'FEE'
                   MOVE 4                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:10) = 'ANNUAL FEE'
                   MOVE 4                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:8) = 'LATE FEE'
                   MOVE 4                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:10) = 'OVER LIMIT'
                   MOVE 4                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:7) = 'FOREIGN'
                   MOVE 4                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:12) = 'CASH ADVANCE'
                   MOVE 3                  TO WS-LINE-CATEGORY
               WHEN WS-DESC-UPPER(1:12) = 'ATM WITHDRAW'
                   MOVE 3                  TO WS-LINE-CATEGORY
               WHEN STMT-LN-DR-CR(WS-IDX) = 'C'
                   MOVE 1                  TO WS-LINE-CATEGORY
               WHEN OTHER
                   MOVE 2                  TO WS-LINE-CATEGORY
           END-EVALUATE
           .
      *
      ******************************************************************
      * 5000 - SUMMARY BOX, PAYMENT DUE AND REGULATORY WARNING         *
      ******************************************************************
       5000-SUMMARY-BOX.
           IF WS-LINE-CNT + 16 > WS-LINES-PER-PAGE
               PERFORM 3000-PAGE-HEAD
           END-IF
      *
           MOVE PL-BOX-TOP                 TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' PREVIOUS BALANCE            '  TO PLB-TEXT
           MOVE STMT-OPEN-BAL              TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' PAYMENTS AND CREDITS        '  TO PLB-TEXT
           MOVE STMT-PAYMENTS              TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' PURCHASES                   '  TO PLB-TEXT
           MOVE STMT-PURCHASES             TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' CASH ADVANCES               '  TO PLB-TEXT
           MOVE STMT-CASH-ADV              TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' FEES CHARGED                '  TO PLB-TEXT
           MOVE STMT-FEES                  TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' INTEREST CHARGED            '  TO PLB-TEXT
           MOVE STMT-INTEREST              TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' NEW BALANCE                 '  TO PLB-TEXT
           MOVE STMT-CLOSE-BAL             TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' CREDIT LIMIT                '  TO PLB-TEXT
           MOVE STMT-CREDIT-LIMIT          TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE ' AVAILABLE CREDIT            '  TO PLB-TEXT
           MOVE STMT-AVAIL-CREDIT          TO PLB-AMOUNT
           MOVE PL-BOX-LINE                TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE PL-BOX-BOTTOM              TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE STMT-DUE-DATE              TO WS-DATE-WORK
           PERFORM 6500-EDIT-DATE
           MOVE WS-DATE-EDIT               TO PLU-DUE-DATE
           MOVE STMT-MIN-PAY               TO PLU-MIN-PAY
           MOVE PL-DUE                     TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           MOVE PL-WARN1                   TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           MOVE PL-WARN2                   TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
      *
           ADD 15                          TO WS-LINE-CNT
           .
      *
       6100-WRITE-LINE.
           WRITE STMTPRT-REC
           IF NOT WS-STMTPRT-OK
               MOVE 'Y'                    TO WS-ERROR-SW
               MOVE 'STMTPRT WRITE FAILED' TO BR-RETURN-MSG
               DISPLAY 'CBSTM01 STMTPRT WRITE STATUS '
                       WS-STMTPRT-STATUS
                       ' ACCT ' STMT-ACCT-ID
           ELSE
               ADD 1                       TO WS-LINES-OUT
           END-IF
           .
      *
      ******************************************************************
      * 6500 - MM/DD/YY FROM AN EIGHT DIGIT DATE.  A SIX DIGIT DATE    *
      *        THAT REACHED THE STATEMENT UNCONVERTED IS WINDOWED      *
      *        AGAINST WS-CENTURY-PIVOT RATHER THAN PRINTED AS 0019.   *
      ******************************************************************
       6500-EDIT-DATE.
           IF WS-DW-CC = ZERO
               IF WS-DW-YEAR < WS-CENTURY-PIVOT
                   MOVE WS-CENTURY-20      TO WS-DW-CC
               ELSE
                   MOVE WS-CENTURY-19      TO WS-DW-CC
               END-IF
               MOVE 'Y'                    TO WS-WARNING-SW
           END-IF
      *
           MOVE WS-DW-MM                   TO WS-DE-MM
           MOVE WS-DW-DD                   TO WS-DE-DD
           MOVE WS-DW-YEAR                 TO WS-DE-YY
           .
      *
       7000-END-BANNER.
           MOVE PL-ENDBANNER               TO STMTPRT-REC
           PERFORM 6100-WRITE-LINE
           MOVE 'PRINT RUN CLOSED'         TO BR-RETURN-MSG
           .
      *
       8000-CLOSE-PRINT.
           CLOSE STMTPRT-FILE
           IF NOT WS-STMTPRT-OK
               MOVE 'Y'                    TO WS-ERROR-SW
               MOVE 'STMTPRT CLOSE FAILED' TO BR-RETURN-MSG
           END-IF
           .
