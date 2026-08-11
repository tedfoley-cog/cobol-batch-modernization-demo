      ******************************************************************
      * CBSTM02 - ELECTRONIC STATEMENT EXTRACT                         *
      *                                                                *
      * REACHED DYNAMICALLY.  THE CALLER MOVES 'STMT' AND 'ELEC' INTO  *
      * THE ROUTE REQUEST AND CALLS CBCRD90, WHICH CALLS THIS PROGRAM  *
      * WITH THE STATEMENT RECORD AND A RETURN AREA.                   *
      *                                                                *
      * CALLED BY   - CBCRD90 ON BEHALF OF CBBIL04                     *
      * CALLS       - NONE                                             *
      * WRITES      - STMTEXT (VB 32004, EBCDIC, PIPE DELIMITED)       *
      *                                                                *
      * THE VENDOR CONTRACT (SCHEDULE 4B, 2011) FIXES THE FORMAT -     *
      *                                                                *
      *   HDR|VERSION|ACCT|CYCLE|STMTNBR|CUST|FROM|TO|DUE              *
      *   BAL|OPEN|CLOSE|PURCH|CASH|PAY|FEES|INT|MINPAY|LIMIT|AVAIL    *
      *   APR|APR-RATE|POINTS                                          *
      *   DTL|SEQ|TRANDATE|POSTDATE|DESC|REF|AMOUNT|DRCR   (REPEATED)  *
      *   TRL|LINES|HASH                                               *
      *                                                                *
      * ALL AMOUNTS ARE UNPACKED FROM COMP-3 INTO DISPLAY WITH AN      *
      * EXPLICIT LEADING SIGN AND AN EXPLICIT DECIMAL POINT - THE      *
      * VENDOR WILL NOT ACCEPT AN IMPLIED SIGN OR AN IMPLIED POINT.    *
      * DATES ARE EIGHT DIGIT CCYYMMDD.  A LEGACY SIX DIGIT DATE IS    *
      * WINDOWED AGAINST WS-CENTURY-PIVOT IN CVCONSTY BEFORE IT GOES   *
      * OUT.                                                           *
      *                                                                *
      * THE TRAILER HASH IS THE SUM OF THE UNSIGNED LINE AMOUNTS IN    *
      * CENTS PLUS THE ACCOUNT NUMBER.  THE VENDOR RECOMPUTES IT AND   *
      * REJECTS THE RECORD IF IT DIFFERS.                              *
      *                                                                *
      * CBCRD90 CANCELS THE RESOLVED MODULE AFTER EVERY CALL, SO THE   *
      * EXTRACT FILE IS OPENED EXTEND FOR EACH STATEMENT.  THE JOB     *
      * MUST ALLOCATE STMTEXT WITH DISP=MOD.                           *
      *                                                                *
      * A CALL WITH STMT-ACCT-ID ZERO AND FORMAT CODE 'CLOS' WRITES    *
      * THE FILE TRAILER AND NOTHING ELSE.                             *
      *                                                                *
      * RETURN AREA - BR-RETURN-CD, BR-LINES-OUT, BR-HASH-TOTAL        *
      *   00 - EXTRACTED                                               *
      *   04 - EXTRACTED WITH A DATA WARNING                           *
      *   12 - EXTRACT FILE FAILURE                                    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBSTM02.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STMTEXT-FILE ASSIGN TO STMTEXT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-STMTEXT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  STMTEXT-FILE
           RECORD IS VARYING IN SIZE FROM 20 TO 32000 CHARACTERS
               DEPENDING ON WS-OUT-LEN
           BLOCK CONTAINS 0 RECORDS.
       01  STMTEXT-REC                     PIC X(32000).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBSTM02 '.
       01  WS-FORMAT-VERSION               PIC X(4)  VALUE '04B '.
      *
       01  WS-STMTEXT-STATUS               PIC X(2)  VALUE '00'.
           88  WS-STMTEXT-OK                         VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW                 PIC X     VALUE 'N'.
               88  WS-ERROR                          VALUE 'Y'.
           05  WS-WARNING-SW               PIC X     VALUE 'N'.
               88  WS-WARNING                        VALUE 'Y'.
      *
       01  WS-WORK.
           05  WS-OUT-LEN                  PIC S9(8) COMP VALUE 20.
           05  WS-PTR                      PIC S9(8) COMP VALUE 1.
           05  WS-IDX                      PIC S9(4) COMP VALUE ZERO.
           05  WS-RECS-OUT                 PIC 9(6)  VALUE ZERO.
           05  WS-SEQ-NBR                  PIC 9(4)  VALUE ZERO.
      *
       01  WS-BUFFER                       PIC X(32000) VALUE SPACES.
      *
       01  WS-HASH-AREA.
           05  WS-HASH-TOTAL               PIC S9(15) COMP-3
                                                     VALUE ZERO.
           05  WS-HASH-CENTS               PIC S9(15) COMP-3
                                                     VALUE ZERO.
           05  WS-HASH-EDIT                PIC 9(15).
      *
      ******************************************************************
      * UNPACKING AREAS.  THE SIGN IS SEPARATE AND LEADING SO IT CAN   *
      * BE MOVED STRAIGHT INTO THE DELIMITED RECORD.                   *
      ******************************************************************
       01  WS-AMT-13                       PIC S9(11)V99
                                           SIGN LEADING SEPARATE.
       01  WS-AMT-13-EDIT.
           05  WS-A13-SIGN                 PIC X.
           05  WS-A13-INT                  PIC 9(11).
           05  WS-A13-DEC                  PIC 9(2).
       01  WS-AMT-OUT.
           05  WS-AO-SIGN                  PIC X.
           05  WS-AO-INT                   PIC 9(11).
           05  FILLER                      PIC X     VALUE '.'.
           05  WS-AO-DEC                   PIC 9(2).
      *
       01  WS-RATE-8                       PIC S9(3)V9(5)
                                           SIGN LEADING SEPARATE.
       01  WS-RATE-EDIT REDEFINES WS-RATE-8.
           05  WS-RE-SIGN                  PIC X.
           05  WS-RE-INT                   PIC 9(3).
           05  WS-RE-DEC                   PIC 9(5).
       01  WS-RATE-OUT.
           05  WS-RO-SIGN                  PIC X.
           05  WS-RO-INT                   PIC 9(3).
           05  FILLER                      PIC X     VALUE '.'.
           05  WS-RO-DEC                   PIC 9(5).
      *
       01  WS-DATE-WORK                    PIC 9(8)  VALUE ZERO.
       01  WS-DATE-PARTS REDEFINES WS-DATE-WORK.
           05  WS-DW-CCYY                  PIC 9(4).
           05  WS-DW-YY REDEFINES WS-DW-CCYY.
               10  WS-DW-CC                PIC 9(2).
               10  WS-DW-YEAR              PIC 9(2).
           05  WS-DW-MM                    PIC 9(2).
           05  WS-DW-DD                    PIC 9(2).
      *
       01  WS-DATE-OUT                     PIC 9(8)  VALUE ZERO.
      *
       01  WS-ACCT-OUT                     PIC 9(11) VALUE ZERO.
       01  WS-CUST-OUT                     PIC 9(9)  VALUE ZERO.
       01  WS-STMT-OUT                     PIC 9(6)  VALUE ZERO.
       01  WS-CNT-OUT                      PIC 9(4)  VALUE ZERO.
       01  WS-SEQ-OUT                      PIC 9(4)  VALUE ZERO.
       01  WS-PTS-OUT                      PIC 9(9)  VALUE ZERO.
       01  WS-DESC-OUT                     PIC X(40) VALUE SPACES.
       01  WS-REF-OUT                      PIC X(16) VALUE SPACES.
       01  WS-DESC-LEN                     PIC S9(4) COMP VALUE ZERO.
       01  WS-REF-LEN                      PIC S9(4) COMP VALUE ZERO.
      *
       01  WS-DELIM                        PIC X     VALUE '|'.
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
                                              BR-HASH-TOTAL
           MOVE 'CBSTM02 '                 TO BR-RETURN-PGM
           MOVE SPACES                     TO BR-RETURN-MSG
           MOVE 'N'                        TO WS-ERROR-SW
                                              WS-WARNING-SW
           MOVE ZERO                       TO WS-HASH-TOTAL
                                              WS-RECS-OUT
      *
           PERFORM 1000-OPEN-EXTRACT
           IF WS-ERROR
               GO TO 0000-TERMINATE
           END-IF
      *
           IF STMT-FORMAT-CD = 'CLOS'
               PERFORM 7000-FILE-TRAILER
           ELSE
               PERFORM 2000-BUILD-EXTRACT
           END-IF
      *
           PERFORM 8000-CLOSE-EXTRACT
           .
       0000-TERMINATE.
           MOVE WS-RECS-OUT                TO BR-LINES-OUT
           MOVE WS-HASH-TOTAL              TO BR-HASH-TOTAL
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
       1000-OPEN-EXTRACT.
           OPEN EXTEND STMTEXT-FILE
           IF NOT WS-STMTEXT-OK
               MOVE 'Y'                    TO WS-ERROR-SW
               MOVE 'STMTEXT OPEN FAILED'  TO BR-RETURN-MSG
               DISPLAY 'CBSTM02 STMTEXT OPEN STATUS '
                       WS-STMTEXT-STATUS
           END-IF
           .
      *
       2000-BUILD-EXTRACT.
           IF STMT-LINE-CNT = ZERO
           OR STMT-LINE-CNT > 300
               MOVE 'Y'                    TO WS-WARNING-SW
               MOVE 'LINE COUNT OUT OF RANGE - HEADER ONLY EXTRACTED'
                                           TO BR-RETURN-MSG
           END-IF
      *
           PERFORM 3000-HEADER-SEGMENT
           PERFORM 3500-BALANCE-SEGMENT
      *
           IF NOT WS-WARNING
               PERFORM VARYING WS-IDX FROM 1 BY 1
                         UNTIL WS-IDX > STMT-LINE-CNT
                            OR WS-ERROR
                   PERFORM 4000-DETAIL-SEGMENT
               END-PERFORM
           END-IF
      *
           PERFORM 5000-RECORD-TRAILER
           .
      *
      ******************************************************************
      * 3000 - HDR SEGMENT                                             *
      ******************************************************************
       3000-HEADER-SEGMENT.
           MOVE SPACES                     TO WS-BUFFER
           MOVE 1                          TO WS-PTR
      *
           MOVE STMT-ACCT-ID               TO WS-ACCT-OUT
           MOVE STMT-CUST-ID               TO WS-CUST-OUT
           MOVE STMT-NUMBER                TO WS-STMT-OUT
      *
           STRING 'HDR'                    DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-FORMAT-VERSION(1:3)   DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-ACCT-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           MOVE STMT-CYCLE-DATE            TO WS-DATE-WORK
           PERFORM 6500-NORMALISE-DATE
           STRING WS-DATE-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-STMT-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-CUST-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           MOVE STMT-PERIOD-FROM           TO WS-DATE-WORK
           PERFORM 6500-NORMALISE-DATE
           STRING WS-DATE-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           MOVE STMT-PERIOD-TO             TO WS-DATE-WORK
           PERFORM 6500-NORMALISE-DATE
           STRING WS-DATE-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           MOVE STMT-DUE-DATE              TO WS-DATE-WORK
           PERFORM 6500-NORMALISE-DATE
           STRING WS-DATE-OUT              DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           PERFORM 6000-WRITE-SEGMENT
           ADD STMT-ACCT-ID                TO WS-HASH-TOTAL
           .
      *
      ******************************************************************
      * 3500 - BAL AND APR SEGMENTS                                    *
      ******************************************************************
       3500-BALANCE-SEGMENT.
           MOVE SPACES                     TO WS-BUFFER
           MOVE 1                          TO WS-PTR
      *
           STRING 'BAL'                    DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           MOVE STMT-OPEN-BAL              TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-CLOSE-BAL             TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-PURCHASES             TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-CASH-ADV              TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-PAYMENTS              TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-FEES                  TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-INTEREST              TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-MIN-PAY               TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-CREDIT-LIMIT          TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
           MOVE STMT-AVAIL-CREDIT          TO WS-AMT-13
           PERFORM 6200-UNPACK-AMOUNT
      *
           MOVE STMT-APR                   TO WS-RATE-8
           MOVE WS-RE-SIGN                 TO WS-RO-SIGN
           MOVE WS-RE-INT                  TO WS-RO-INT
           MOVE WS-RE-DEC                  TO WS-RO-DEC
           MOVE STMT-REWARD-PTS            TO WS-PTS-OUT
      *
           STRING WS-RATE-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-PTS-OUT               DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           PERFORM 6000-WRITE-SEGMENT
           .
      *
      ******************************************************************
      * 4000 - ONE DTL SEGMENT                                         *
      ******************************************************************
       4000-DETAIL-SEGMENT.
           MOVE SPACES                     TO WS-BUFFER
           MOVE 1                          TO WS-PTR
           MOVE WS-IDX                     TO WS-SEQ-OUT
      *
           STRING 'DTL'                    DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-SEQ-OUT               DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           MOVE STMT-LN-DATE(WS-IDX)       TO WS-DATE-WORK
           PERFORM 6500-NORMALISE-DATE
           STRING WS-DATE-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           MOVE STMT-LN-POST-DATE(WS-IDX)  TO WS-DATE-WORK
           PERFORM 6500-NORMALISE-DATE
           STRING WS-DATE-OUT              DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
      *    TRAILING BLANKS ARE STRIPPED - THE VENDOR PAYS BY VOLUME.
           MOVE STMT-LN-DESC(WS-IDX)       TO WS-DESC-OUT
           PERFORM 6300-STRIP-DESC
           MOVE STMT-LN-REF(WS-IDX)        TO WS-REF-OUT
           PERFORM 6400-STRIP-REF
      *
           STRING WS-DESC-OUT(1:WS-DESC-LEN)
                                           DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-REF-OUT(1:WS-REF-LEN) DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           MOVE STMT-LN-AMT(WS-IDX)        TO WS-AMT-13
           PERFORM 6250-FORMAT-AMOUNT
           STRING WS-AMT-OUT               DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  STMT-LN-DR-CR(WS-IDX)    DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           PERFORM 6000-WRITE-SEGMENT
      *
      *    HASH IN CENTS, SIGN IGNORED
           COMPUTE WS-HASH-CENTS =
                   FUNCTION ABS(STMT-LN-AMT(WS-IDX)) * 100
           ADD WS-HASH-CENTS               TO WS-HASH-TOTAL
           .
      *
      ******************************************************************
      * 5000 - TRL SEGMENT FOR THIS STATEMENT                          *
      ******************************************************************
       5000-RECORD-TRAILER.
           MOVE SPACES                     TO WS-BUFFER
           MOVE 1                          TO WS-PTR
           MOVE STMT-LINE-CNT              TO WS-CNT-OUT
           MOVE WS-HASH-TOTAL              TO WS-HASH-EDIT
      *
           STRING 'TRL'                    DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-CNT-OUT               DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-HASH-EDIT             DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           PERFORM 6000-WRITE-SEGMENT
           .
      *
       6000-WRITE-SEGMENT.
           COMPUTE WS-OUT-LEN = WS-PTR - 1
           IF WS-OUT-LEN < 20
               MOVE 20                     TO WS-OUT-LEN
           END-IF
      *
           MOVE WS-BUFFER                  TO STMTEXT-REC
           WRITE STMTEXT-REC
           IF NOT WS-STMTEXT-OK
               MOVE 'Y'                    TO WS-ERROR-SW
               MOVE 'STMTEXT WRITE FAILED' TO BR-RETURN-MSG
               DISPLAY 'CBSTM02 STMTEXT WRITE STATUS '
                       WS-STMTEXT-STATUS
                       ' ACCT ' STMT-ACCT-ID
           ELSE
               ADD 1                       TO WS-RECS-OUT
           END-IF
           .
      *
      ******************************************************************
      * 6200 - UNPACK COMP-3 AND APPEND WITH A TRAILING DELIMITER      *
      ******************************************************************
       6200-UNPACK-AMOUNT.
           PERFORM 6250-FORMAT-AMOUNT
           STRING WS-AMT-OUT               DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
           .
      *
      ******************************************************************
      * 6250 - THE MOVE TO WS-AMT-13 DOES THE UNPACKING.  THE SIGN IS  *
      *        SEPARATE AND LEADING SO IT IS ALWAYS + OR - AND NEVER   *
      *        AN OVERPUNCH THE VENDOR CANNOT READ.                    *
      ******************************************************************
       6250-FORMAT-AMOUNT.
           MOVE WS-AMT-13                  TO WS-AMT-13-EDIT
           IF WS-A13-SIGN = '-'
               MOVE '-'                    TO WS-AO-SIGN
           ELSE
               MOVE '+'                    TO WS-AO-SIGN
           END-IF
           MOVE WS-A13-INT                 TO WS-AO-INT
           MOVE WS-A13-DEC                 TO WS-AO-DEC
           .
      *
       6300-STRIP-DESC.
           MOVE 40                         TO WS-DESC-LEN
           PERFORM UNTIL WS-DESC-LEN = 1
                      OR WS-DESC-OUT(WS-DESC-LEN:1) NOT = SPACE
               SUBTRACT 1                FROM WS-DESC-LEN
           END-PERFORM
      *
      *    A DELIMITER INSIDE A DESCRIPTION WOULD SPLIT THE FIELD
           INSPECT WS-DESC-OUT REPLACING ALL '|' BY ' '
           .
      *
       6400-STRIP-REF.
           MOVE 16                         TO WS-REF-LEN
           PERFORM UNTIL WS-REF-LEN = 1
                      OR WS-REF-OUT(WS-REF-LEN:1) NOT = SPACE
               SUBTRACT 1                FROM WS-REF-LEN
           END-PERFORM
           INSPECT WS-REF-OUT REPLACING ALL '|' BY ' '
           .
      *
      ******************************************************************
      * 6500 - SIX DIGIT DATES.  A DATE WHOSE FIRST TWO DIGITS ARE     *
      *        ZERO CAME OFF A LEGACY FEED AS YYMMDD AND IS WINDOWED   *
      *        AGAINST WS-CENTURY-PIVOT FROM CVCONSTY.                 *
      ******************************************************************
       6500-NORMALISE-DATE.
           IF WS-DW-CC = ZERO
               IF WS-DW-YEAR < WS-CENTURY-PIVOT
                   MOVE WS-CENTURY-20      TO WS-DW-CC
               ELSE
                   MOVE WS-CENTURY-19      TO WS-DW-CC
               END-IF
               MOVE 'Y'                    TO WS-WARNING-SW
           END-IF
      *
           MOVE WS-DATE-WORK               TO WS-DATE-OUT
           .
      *
       7000-FILE-TRAILER.
           MOVE SPACES                     TO WS-BUFFER
           MOVE 1                          TO WS-PTR
           STRING 'EOF'                    DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  WS-FORMAT-VERSION(1:3)   DELIMITED BY SIZE
                  WS-DELIM                 DELIMITED BY SIZE
                  'CBSTM02'                DELIMITED BY SIZE
             INTO WS-BUFFER
             WITH POINTER WS-PTR
           END-STRING
      *
           PERFORM 6000-WRITE-SEGMENT
           MOVE 'EXTRACT CLOSED'           TO BR-RETURN-MSG
           .
      *
       8000-CLOSE-EXTRACT.
           CLOSE STMTEXT-FILE
           IF NOT WS-STMTEXT-OK
               MOVE 'Y'                    TO WS-ERROR-SW
               MOVE 'STMTEXT CLOSE FAILED' TO BR-RETURN-MSG
           END-IF
           .
