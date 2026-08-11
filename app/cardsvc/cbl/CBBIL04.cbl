      ******************************************************************
      * CBBIL04 - CARDBILL STEP 04 - STATEMENT RENDERING               *
      *                                                                *
      * READS THE DELIVERY PREFERENCE OF THE ACCOUNT HOLDER FROM VSAM  *
      * CUSTPREF, TURNS IT INTO A ROUTE KEY AND HANDS THE STATEMENT    *
      * TO THE BATCH DISPATCHER.  THE FORMATTER THAT ACTUALLY RENDERS  *
      * THE STATEMENT IS NOT NAMED HERE - IT IS WHATEVER THE ROUTE     *
      * TABLE SAYS IT IS.                                              *
      *                                                                *
      * A DISPATCH WITH STMT-ACCT-ID ZERO AND FORMAT CODE 'CLOS' IS    *
      * THE END OF RUN SIGNAL TO EACH FORMATTER THAT WAS USED.         *
      *                                                                *
      * CALLED BY   - JOB CBBIL04J, STEP RENDER                        *
      * CALLS       - CBCRD90 (BATCH DISPATCHER, ROUTE TYPE 'STMT')    *
      *             - CBCRD91 (FATAL ERROR / ABEND HANDLER)            *
      * READS       - STMTWK2, VSAM CUSTPREF                           *
      * WRITES      - STMTWK3 (STMT-RECORD, VB 24208)                  *
      *                                                                *
      * COMPILE WITH DYNAM - THE DISPATCHER AND EVERYTHING BELOW IT    *
      * IS RESOLVED AT RUN TIME.                                       *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - ALL STATEMENTS RENDERED                                 *
      *   04 - SOME STATEMENTS FELL BACK TO THE DEFAULT PREFERENCE     *
      *   08 - ONE OR MORE FORMATTER FAILURES                          *
      *   12 - FATAL - ROUTE TABLE UNUSABLE                            *
      * ABEND U0804 - UNRECOVERABLE FILE CONDITION                     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBBIL04.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STMTWK2-FILE ASSIGN TO STMTWK2
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTWK2-STATUS.
      *
           SELECT STMTWK3-FILE ASSIGN TO STMTWK3
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTWK3-STATUS.
      *
           SELECT CUSTPREF-FILE ASSIGN TO CUSTPREF
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CP-CUST-ID
                  FILE STATUS  IS WS-CUSTPREF-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  STMTWK2-FILE
           RECORD IS VARYING IN SIZE FROM 284 TO 24204 CHARACTERS
               DEPENDING ON WS-IN-LEN
           BLOCK CONTAINS 0 RECORDS.
       01  STMTWK2-REC                     PIC X(24204).
      *
       FD  STMTWK3-FILE
           RECORD IS VARYING IN SIZE FROM 284 TO 24204 CHARACTERS
               DEPENDING ON WS-OUT-LEN
           BLOCK CONTAINS 0 RECORDS.
       01  STMTWK3-REC                     PIC X(24204).
      *
      ******************************************************************
      * CUSTPREF - CUSTOMER SERVICING PREFERENCES.  THE FILE IS OWNED  *
      * BY THE ONLINE MAINTENANCE TRANSACTION AND ONLY THE DELIVERY    *
      * FIELDS ARE OF INTEREST HERE.                                   *
      ******************************************************************
       FD  CUSTPREF-FILE
           RECORD CONTAINS 180 CHARACTERS.
       01  CUSTPREF-REC.
           05  CP-CUST-ID                  PIC 9(9).
           05  CP-STMT-DELIVERY            PIC X(4).
               88  CP-DELIV-PAPER          VALUE 'MAIL'.
               88  CP-DELIV-EMAIL          VALUE 'EMAL'.
               88  CP-DELIV-PORTAL         VALUE 'PORT'.
               88  CP-DELIV-BOTH           VALUE 'BOTH'.
           05  CP-EMAIL-ADDR               PIC X(50).
           05  CP-EMAIL-VERIFIED           PIC X.
           05  CP-LANGUAGE-CD              PIC X(2).
           05  CP-LARGE-PRINT-FLG          PIC X.
           05  CP-SUPPRESS-MKTG-FLG        PIC X.
           05  CP-PAPERLESS-DATE           PIC 9(8).
           05  CP-STMT-COPIES              PIC 9(2).
           05  CP-LAST-MAINT-PGM           PIC X(8).
           05  CP-LAST-MAINT-TS            PIC X(26).
           05  CP-FILLER                   PIC X(68).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBBIL04 '.
      *
       01  WS-FILE-STATUS-AREA.
           05  WS-STMTWK2-STATUS           PIC X(2)  VALUE '00'.
               88  WS-STMTWK2-OK                     VALUE '00'.
           05  WS-STMTWK3-STATUS           PIC X(2)  VALUE '00'.
               88  WS-STMTWK3-OK                     VALUE '00'.
           05  WS-CUSTPREF-STATUS          PIC X(2)  VALUE '00'.
               88  WS-CUSTPREF-OK                    VALUE '00'.
               88  WS-CUSTPREF-NOTFND                VALUE '23'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-SYSIN-EOF-SW             PIC X     VALUE 'N'.
               88  WS-SYSIN-EOF                      VALUE 'Y'.
           05  WS-FATAL-SW                 PIC X     VALUE 'N'.
               88  WS-FATAL                          VALUE 'Y'.
           05  WS-PAPER-USED-SW            PIC X     VALUE 'N'.
               88  WS-PAPER-USED                     VALUE 'Y'.
           05  WS-ELEC-USED-SW             PIC X     VALUE 'N'.
               88  WS-ELEC-USED                      VALUE 'Y'.
      *
       01  WS-CONTROL-CARDS.
           05  WS-CYCLE-DATE               PIC 9(8)  VALUE ZERO.
           05  WS-CYCLE-ID                 PIC X(8)  VALUE SPACES.
           05  WS-DEFAULT-DELIVERY         PIC X(4)  VALUE SPACES.
           05  WS-ELEC-CUTOVER-DT          PIC 9(8)  VALUE ZERO.
      *
       01  WS-CARD-IMAGE.
           05  WS-CARD-KEYWORD             PIC X(12).
           05  FILLER                      PIC X.
           05  WS-CARD-VALUE               PIC X(20).
           05  FILLER                      PIC X(47).
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-PAPER-CNT                PIC 9(9)  VALUE ZERO.
           05  WS-ELEC-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-DEFAULTED-CNT            PIC 9(9)  VALUE ZERO.
           05  WS-FAILED-CNT               PIC 9(9)  VALUE ZERO.
           05  WS-PAGES-CNT                PIC 9(9)  VALUE ZERO.
      *
       01  WS-IN-LEN                       PIC S9(8) COMP VALUE 284.
       01  WS-OUT-LEN                      PIC S9(8) COMP VALUE 284.
      *
       01  WS-DELIVERY-KEY                 PIC X(8)  VALUE SPACES.
       01  WS-DISP-CNT                     PIC ZZZ,ZZZ,ZZ9.
      *
           COPY CVCONSTY.
           COPY CVSTMT01Y.
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVBRTN1Y.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 1000-INITIALISE
           IF WS-FATAL
               GO TO 0000-TERMINATE
           END-IF
      *
           PERFORM 2000-READ-STATEMENT
           PERFORM UNTIL WS-EOF
                      OR WS-FATAL
               ADD 1                       TO WS-READ-CNT
               PERFORM 3000-RENDER-STATEMENT
               PERFORM 2000-READ-STATEMENT
           END-PERFORM
      *
           PERFORM 7500-CLOSE-FORMATTERS
           PERFORM 8000-CLOSE-DOWN
           PERFORM 8500-REPORT-TOTALS
           .
       0000-TERMINATE.
           PERFORM 9900-SET-RETURN-CODE
           GOBACK
           .
      *
       1000-INITIALISE.
           MOVE 'CBBIL04 '                 TO ER-PGM-NAME
           MOVE '1000-INITIALISE'          TO ER-PARAGRAPH
      *
           PERFORM 1100-READ-SYSIN
           IF WS-FATAL
               GO TO 1000-EXIT
           END-IF
      *
           OPEN INPUT  STMTWK2-FILE
                       CUSTPREF-FILE
           OPEN OUTPUT STMTWK3-FILE
      *
           IF NOT WS-STMTWK2-OK
               MOVE 'STMTWK2 '             TO ER-FILE-NAME
               MOVE WS-STMTWK2-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STMTWK2 OPEN FAILED'  TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF NOT WS-CUSTPREF-OK
               MOVE 'CUSTPREF'             TO ER-FILE-NAME
               MOVE WS-CUSTPREF-STATUS     TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'CUSTPREF OPEN FAILED' TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF NOT WS-STMTWK3-OK
               MOVE 'STMTWK3 '             TO ER-FILE-NAME
               MOVE WS-STMTWK3-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STMTWK3 OPEN FAILED'  TO ER-MESSAGE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
       1100-READ-SYSIN.
           PERFORM UNTIL WS-SYSIN-EOF
               ACCEPT WS-CARD-IMAGE FROM SYSIN
                   ON EXCEPTION
                       MOVE 'Y'            TO WS-SYSIN-EOF-SW
               END-ACCEPT
               IF NOT WS-SYSIN-EOF
                   EVALUATE WS-CARD-KEYWORD
                       WHEN 'CYCLE-DATE '
                           MOVE WS-CARD-VALUE(1:8)
                                           TO WS-CYCLE-DATE
                       WHEN 'CYCLE-ID   '
                           MOVE WS-CARD-VALUE(1:8)
                                           TO WS-CYCLE-ID
                       WHEN 'DFLT-DELIV '
                           MOVE WS-CARD-VALUE(1:4)
                                           TO WS-DEFAULT-DELIVERY
                       WHEN 'ELEC-CUTOVR'
                           MOVE WS-CARD-VALUE(1:8)
                                           TO WS-ELEC-CUTOVER-DT
                       WHEN '*          '
                           CONTINUE
                       WHEN OTHER
                           DISPLAY 'CBBIL04 BAD CONTROL CARD - '
                                   WS-CARD-IMAGE
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           IF WS-DEFAULT-DELIVERY = SPACES
               MOVE 'PAPR'                 TO WS-DEFAULT-DELIVERY
           END-IF
           .
      *
       2000-READ-STATEMENT.
           READ STMTWK2-FILE INTO STMT-RECORD
               AT END
                   MOVE 'Y'                TO WS-EOF-SW
           END-READ
           .
      *
      ******************************************************************
      * 3000 - ONE STATEMENT                                           *
      ******************************************************************
       3000-RENDER-STATEMENT.
           MOVE '3000-RENDER-STATEMENT'    TO ER-PARAGRAPH
           PERFORM 3100-READ-PREFERENCE
           PERFORM 3200-MAP-ROUTE-KEY
           PERFORM 3300-DISPATCH-FORMATTER
           PERFORM 3400-WRITE-STATEMENT
           .
      *
       3100-READ-PREFERENCE.
           MOVE SPACES                     TO CUSTPREF-REC
           MOVE STMT-CUST-ID               TO CP-CUST-ID
      *
           READ CUSTPREF-FILE
               INVALID KEY
                   CONTINUE
           END-READ
      *
           EVALUATE TRUE
               WHEN WS-CUSTPREF-OK
                   CONTINUE
               WHEN WS-CUSTPREF-NOTFND
                   ADD 1                   TO WS-DEFAULTED-CNT
                   MOVE SPACES             TO CUSTPREF-REC
                   MOVE WS-DEFAULT-DELIVERY
                                           TO CP-STMT-DELIVERY
                   DISPLAY 'CBBIL04 NO PREFERENCE FOR CUST '
                           STMT-CUST-ID ' - DEFAULTED'
               WHEN OTHER
                   MOVE 'CUSTPREF'         TO ER-FILE-NAME
                   MOVE WS-CUSTPREF-STATUS TO ER-FILE-STATUS
                   MOVE 'VSAM'             TO ER-ERROR-TYPE
                   MOVE 'CUSTPREF READ FAILED'
                                           TO ER-MESSAGE
                   PERFORM 9500-ABEND
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3200 - PREFERENCE TO ROUTE KEY                                 *
      *                                                                *
      * ELECTRONIC DELIVERY NEEDS A VERIFIED ADDRESS AND A PAPERLESS   *
      * DATE ON OR AFTER THE CUTOVER GIVEN ON THE CONTROL CARD.  AN    *
      * UNVERIFIED ADDRESS FALLS BACK TO PAPER - THAT RULE CAME OUT    *
      * OF THE 2020 PAPERLESS PROGRAMME.                               *
      ******************************************************************
       3200-MAP-ROUTE-KEY.
           EVALUATE TRUE
               WHEN CP-STMT-DELIVERY = 'EMAL'
                 OR CP-STMT-DELIVERY = 'PORT'
                   IF CP-EMAIL-VERIFIED = 'Y'
                  AND CP-PAPERLESS-DATE NOT < WS-ELEC-CUTOVER-DT
                       MOVE 'ELEC'         TO WS-DELIVERY-KEY(1:4)
                   ELSE
                       MOVE 'PAPR'         TO WS-DELIVERY-KEY(1:4)
                       ADD 1               TO WS-DEFAULTED-CNT
                   END-IF
               WHEN CP-STMT-DELIVERY = 'BOTH'
      *            BOTH MEANS THE PAPER COPY IS THE RECORD COPY
                   MOVE 'PAPR'             TO WS-DELIVERY-KEY(1:4)
               WHEN OTHER
                   MOVE 'PAPR'             TO WS-DELIVERY-KEY(1:4)
           END-EVALUATE
      *
           MOVE SPACES                     TO WS-DELIVERY-KEY(5:4)
           MOVE WS-DELIVERY-KEY(1:4)       TO STMT-FORMAT-CD
      *
           IF STMT-FMT-ELECTRONIC
               ADD 1                       TO WS-ELEC-CNT
               MOVE 'Y'                    TO WS-ELEC-USED-SW
           ELSE
               ADD 1                       TO WS-PAPER-CNT
               MOVE 'Y'                    TO WS-PAPER-USED-SW
           END-IF
           .
      *
      ******************************************************************
      * 3300 - DISPATCH.  THE FORMATTER NAME IS NEVER CODED HERE.      *
      ******************************************************************
       3300-DISPATCH-FORMATTER.
           MOVE SPACES                     TO BATCH-RETURN-AREA
           MOVE ZERO                       TO BR-RETURN-CD
                                              BR-LINES-OUT
                                              BR-PAGES-OUT
                                              BR-HASH-TOTAL
                                              BR-FEE-TOTAL
                                              BR-FEE-COUNT
      *
           MOVE 'STMT'                     TO RQ-ROUTE-TYPE
           MOVE WS-DELIVERY-KEY            TO RQ-ROUTE-KEY
           MOVE 1                          TO RQ-SEQ-NBR
      *
           CALL 'CBCRD90' USING ROUTE-REQUEST
                                STMT-RECORD
                                BATCH-RETURN-AREA
      *
           EVALUATE TRUE
               WHEN RQ-RC-TABLE-ERROR
                   MOVE 'ROUT'             TO ER-ERROR-TYPE
                   MOVE 'ROUTE TABLE UNAVAILABLE'
                                           TO ER-MESSAGE
                   PERFORM 9500-FATAL-ERROR
               WHEN RQ-RC-NOT-FOUND
                   ADD 1                   TO WS-FAILED-CNT
                   MOVE 'ROUT'             TO ER-ERROR-TYPE
                   DISPLAY 'CBBIL04 NO STMT ROUTE FOR KEY '
                           WS-DELIVERY-KEY ' ACCT ' STMT-ACCT-ID
               WHEN BR-RETURN-CD > 4
                   ADD 1                   TO WS-FAILED-CNT
                   DISPLAY 'CBBIL04 FORMATTER RC=' BR-RETURN-CD
                           ' ACCT=' STMT-ACCT-ID
                           ' MSG=' BR-RETURN-MSG
               WHEN OTHER
                   MOVE BR-PAGES-OUT       TO STMT-PAGE-CNT
                   ADD BR-PAGES-OUT        TO WS-PAGES-CNT
           END-EVALUATE
      *
           IF RQ-USED-FALLBACK = 'Y'
               DISPLAY 'CBBIL04 FALLBACK FORMATTER USED FOR ACCT '
                       STMT-ACCT-ID
           END-IF
           .
      *
       3400-WRITE-STATEMENT.
           COMPUTE WS-OUT-LEN = 204 + (80 * STMT-LINE-CNT)
      *
           WRITE STMTWK3-REC FROM STMT-RECORD
           IF NOT WS-STMTWK3-OK
               MOVE 'STMTWK3 '             TO ER-FILE-NAME
               MOVE WS-STMTWK3-STATUS      TO ER-FILE-STATUS
               MOVE 'VSAM'                 TO ER-ERROR-TYPE
               MOVE 'STMTWK3 WRITE FAILED' TO ER-MESSAGE
               PERFORM 9500-ABEND
           END-IF
           .
      *
      ******************************************************************
      * 7500 - END OF RUN SIGNAL                                       *
      *                                                                *
      * ONLY THE FORMATTERS THAT WERE ACTUALLY USED ARE SIGNALLED, SO  *
      * AN UNUSED FORMATTER NEVER CREATES AN EMPTY OUTPUT DATASET.     *
      ******************************************************************
       7500-CLOSE-FORMATTERS.
           MOVE SPACES                     TO STMT-RECORD
           MOVE ZERO                       TO STMT-ACCT-ID
                                              STMT-CYCLE-DATE
                                              STMT-NUMBER
                                              STMT-CUST-ID
           MOVE 1                          TO STMT-LINE-CNT
           MOVE 'CLOS'                     TO STMT-FORMAT-CD
      *
           IF WS-PAPER-USED
               MOVE 'PAPR'                 TO WS-DELIVERY-KEY(1:4)
               MOVE SPACES                 TO WS-DELIVERY-KEY(5:4)
               PERFORM 7550-SIGNAL-FORMATTER
           END-IF
      *
           IF WS-ELEC-USED
               MOVE 'ELEC'                 TO WS-DELIVERY-KEY(1:4)
               MOVE SPACES                 TO WS-DELIVERY-KEY(5:4)
               PERFORM 7550-SIGNAL-FORMATTER
           END-IF
           .
      *
       7550-SIGNAL-FORMATTER.
           MOVE SPACES                     TO BATCH-RETURN-AREA
           MOVE ZERO                       TO BR-RETURN-CD
           MOVE 'STMT'                     TO RQ-ROUTE-TYPE
           MOVE WS-DELIVERY-KEY            TO RQ-ROUTE-KEY
           MOVE 1                          TO RQ-SEQ-NBR
      *
           CALL 'CBCRD90' USING ROUTE-REQUEST
                                STMT-RECORD
                                BATCH-RETURN-AREA
      *
           IF BR-RETURN-CD > 4
               ADD 1                       TO WS-FAILED-CNT
               DISPLAY 'CBBIL04 FORMATTER CLOSE RC=' BR-RETURN-CD
                       ' KEY=' WS-DELIVERY-KEY
                       ' MSG=' BR-RETURN-MSG
           END-IF
           .
      *
       8000-CLOSE-DOWN.
           CLOSE STMTWK2-FILE
                 STMTWK3-FILE
                 CUSTPREF-FILE
           .
      *
       8500-REPORT-TOTALS.
           DISPLAY '*---------------------------------------------*'
           DISPLAY '* CBBIL04 STATEMENT RENDERING                 *'
           DISPLAY '*---------------------------------------------*'
           DISPLAY '  CYCLE ID          ' WS-CYCLE-ID
           DISPLAY '  CYCLE DATE        ' WS-CYCLE-DATE
           MOVE WS-READ-CNT                TO WS-DISP-CNT
           DISPLAY '  STATEMENTS READ   ' WS-DISP-CNT
           MOVE WS-PAPER-CNT               TO WS-DISP-CNT
           DISPLAY '  ROUTED PAPER      ' WS-DISP-CNT
           MOVE WS-ELEC-CNT                TO WS-DISP-CNT
           DISPLAY '  ROUTED ELECTRONIC ' WS-DISP-CNT
           MOVE WS-DEFAULTED-CNT           TO WS-DISP-CNT
           DISPLAY '  DEFAULTED PREFS   ' WS-DISP-CNT
           MOVE WS-PAGES-CNT               TO WS-DISP-CNT
           DISPLAY '  PRINT PAGES       ' WS-DISP-CNT
           MOVE WS-FAILED-CNT              TO WS-DISP-CNT
           DISPLAY '  FORMATTER FAILURES' WS-DISP-CNT
           .
      *
       9500-FATAL-ERROR.
           MOVE 'CBBIL04 '                 TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL04 FATAL - ' ER-MESSAGE
           .
      *
       9500-ABEND.
           MOVE 'CBBIL04 '                 TO ER-PGM-NAME
           MOVE 'U804'                     TO ER-ABEND-CODE
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE 'Y'                        TO WS-FATAL-SW
           DISPLAY 'CBBIL04 ABEND U0804 - ' ER-MESSAGE
           CALL 'CBCRD91' USING ERROR-AREA
           .
      *
       9900-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-FATAL
                   MOVE 12                 TO RETURN-CODE
               WHEN WS-FAILED-CNT > ZERO
                   MOVE 8                  TO RETURN-CODE
               WHEN WS-DEFAULTED-CNT > ZERO
                   MOVE 4                  TO RETURN-CODE
               WHEN OTHER
                   MOVE 0                  TO RETURN-CODE
           END-EVALUATE
           .
