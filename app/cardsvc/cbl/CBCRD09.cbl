      ******************************************************************
      * CBCRD09 - BACKUP VERIFICATION AND GENERATION REGISTER          *
      *                                                                *
      * JOB CBCRD09J STEP070.  RUNS AFTER THE IDCAMS STEPS THAT REPRO  *
      * CARDXREF, CYCLCTL AND AUTHLOG TO NEW GDG GENERATIONS AND       *
      * RE-DEFINE AUTHLOG FOR THE NEXT DAY.                            *
      *                                                                *
      * IDCAMS WILL HAPPILY REPRO ZERO RECORDS AND SET A ZERO RETURN   *
      * CODE, WHICH IS HOW THE 1999 RESTORE FOUND AN EMPTY GENERATION. *
      * THIS PROGRAM READS EACH BACKUP GENERATION IT HAS JUST TAKEN,   *
      * COUNTS AND HASHES IT, COMPARES THE COUNTS AGAINST THE CYCLE    *
      * CONTROL RECORD AND WRITES A BACKUP REGISTER LINE THAT THE      *
      * OPERATIONS TEAM KEEP FOR THE RETENTION PERIOD.  IT ALSO PROVES *
      * THAT THE RE-DEFINED AUTHLOG IS EMPTY AND OPENABLE BEFORE THE   *
      * ONLINE REGION IS ALLOWED BACK UP BY CBCRD10.                   *
      *                                                                *
      * CALLED BY   - JCL ONLY                                         *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - BKPXREF  SEQUENTIAL BACKUP OF CARDXREF   (-0)    *
      *             - BKPCTL   SEQUENTIAL BACKUP OF CYCLCTL    (-0)    *
      *             - BKPAUTH  SEQUENTIAL BACKUP OF AUTHLOG    (-0)    *
      *             - AUTHLOG  VSAM ESDS, RE-DEFINED, MUST BE EMPTY    *
      *             - CYCLCTL  VSAM KSDS UPDATE                        *
      *             - BKPREG   SYSOUT LRECL 133 BACKUP REGISTER        *
      *                                                                *
      * RETURN CODE - 0000 ALL GENERATIONS VERIFIED                    *
      *               0004 A GENERATION IS SMALLER THAN EXPECTED       *
      *               0012 FATAL                                       *
      * USER ABEND  - U0901 A BACKUP GENERATION IS EMPTY               *
      *               U0902 FILE OPEN OR I/O FAILURE                   *
      *               U0903 RE-DEFINED AUTHLOG IS NOT EMPTY            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD09.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  1999-12-06.
      *
      * MAINTENANCE
      * 1999-12-06 CRD1801 ORIGINAL - WRITTEN AFTER THE NOVEMBER 1999
      *                    RESTORE FOUND AN EMPTY GENERATION
      * 2010-05-17 CRD6802 HASH TOTAL ADDED TO THE REGISTER LINE
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BKPXREF-FILE  ASSIGN TO BKPXREF
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-BKPXREF-STATUS.
      *
           SELECT BKPCTL-FILE   ASSIGN TO BKPCTL
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-BKPCTL-STATUS.
      *
           SELECT BKPAUTH-FILE  ASSIGN TO BKPAUTH
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-BKPAUTH-STATUS.
      *
           SELECT AUTHLOG-FILE  ASSIGN TO AUTHLOG
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-AUTHLOG-STATUS.
      *
           SELECT CYCLCTL-FILE  ASSIGN TO CYCLCTL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CTL-KEY
                  FILE STATUS  IS WS-CYCLCTL-STATUS.
      *
           SELECT REGISTER-FILE ASSIGN TO BKPREG
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS  IS WS-REGISTER-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  BKPXREF-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 128 CHARACTERS.
       01  BKPXREF-REC                     PIC X(128).
      *
       FD  BKPCTL-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 256 CHARACTERS.
       01  BKPCTL-REC                      PIC X(256).
      *
       FD  BKPAUTH-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  BKPAUTH-REC.
           05  BA-CARD-NUM                 PIC X(16).
           05  BA-REST                     PIC X(184).
      *
       FD  AUTHLOG-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  AUTHLOG-REC                     PIC X(200).
      *
       FD  CYCLCTL-FILE
           RECORD CONTAINS 256 CHARACTERS.
       01  CYCLCTL-REC.
           05  CTL-KEY.
               10  CTL-CYCLE-TYPE          PIC X(8).
               10  CTL-CYCLE-DATE          PIC 9(8).
           05  CTL-REST                    PIC X(240).
      *
       FD  REGISTER-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  REGISTER-REC                    PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD09 '.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP070 '.
      *
       01  WS-STATUS-FIELDS.
           05  WS-BKPXREF-STATUS           PIC X(2)  VALUE '00'.
               88  WS-BKPXREF-OK                     VALUE '00'.
               88  WS-BKPXREF-EOF                    VALUE '10'.
           05  WS-BKPCTL-STATUS            PIC X(2)  VALUE '00'.
               88  WS-BKPCTL-OK                      VALUE '00'.
               88  WS-BKPCTL-EOF                     VALUE '10'.
           05  WS-BKPAUTH-STATUS           PIC X(2)  VALUE '00'.
               88  WS-BKPAUTH-OK                     VALUE '00'.
               88  WS-BKPAUTH-EOF                    VALUE '10'.
           05  WS-AUTHLOG-STATUS           PIC X(2)  VALUE '00'.
               88  WS-AUTHLOG-OK                     VALUE '00'.
               88  WS-AUTHLOG-EOF                    VALUE '10'.
           05  WS-CYCLCTL-STATUS           PIC X(2)  VALUE '00'.
               88  WS-CYCLCTL-OK                     VALUE '00'.
           05  WS-REGISTER-STATUS          PIC X(2)  VALUE '00'.
               88  WS-REGISTER-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                   PIC X     VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-XREF-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-CTL-CNT                  PIC 9(9)  VALUE ZERO.
           05  WS-AUTH-CNT                 PIC 9(9)  VALUE ZERO.
           05  WS-RESIDUAL-CNT             PIC 9(9)  VALUE ZERO.
           05  WS-LINE-CNT                 PIC 9(3)  VALUE 99.
           05  WS-PAGE-CNT                 PIC 9(4)  VALUE ZERO.
      *
       01  WS-HASH-TOTAL                   PIC S9(15) COMP-3
                                                     VALUE ZERO.
       01  WS-HASH-WORK                    PIC S9(15) COMP-3
                                                     VALUE ZERO.
       01  WS-CARD-DIGITS                  PIC 9(16) VALUE ZERO.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-EXPECTED-AUTH                PIC 9(9)  VALUE ZERO.
       01  WS-SHORTFALL-PCT                PIC S9(3)V99 COMP-3
                                                     VALUE ZERO.
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
      ******************************************************************
      * BACKUP REGISTER LINES                                          *
      ******************************************************************
       01  REG-HEAD-1.
           05  FILLER                      PIC X(9)  VALUE 'CBCRD09  '.
           05  FILLER                      PIC X(42) VALUE
               'CARD SERVICES - NIGHTLY BACKUP REGISTER   '.
           05  FILLER                      PIC X(11) VALUE
               'CYCLE DATE '.
           05  RH1-CYCLE-DATE              PIC 9(8).
           05  FILLER                      PIC X(48) VALUE SPACES.
           05  FILLER                      PIC X(5)  VALUE 'PAGE '.
           05  RH1-PAGE                    PIC ZZZ9.
           05  FILLER                      PIC X(6)  VALUE SPACES.
      *
       01  REG-HEAD-2.
           05  FILLER                      PIC X(10) VALUE 'DD NAME   '.
           05  FILLER                      PIC X(22) VALUE
               'CLUSTER               '.
           05  FILLER                      PIC X(15) VALUE
               '       RECORDS '.
           05  FILLER                      PIC X(22) VALUE
               '           HASH TOTAL '.
           05  FILLER                      PIC X(12) VALUE
               'VERIFIED AT '.
           05  FILLER                      PIC X(52) VALUE SPACES.
      *
       01  REG-DETAIL.
           05  RD-DD-NAME                  PIC X(8).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-CLUSTER                  PIC X(20).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-RECORDS                  PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  RD-HASH                     PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  RD-VERIFIED-TS              PIC X(19).
           05  FILLER                      PIC X(46) VALUE SPACES.
      *
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
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
           PERFORM 2000-VERIFY-XREF
           PERFORM 3000-VERIFY-CYCLCTL
           PERFORM 4000-VERIFY-AUTHLOG
           PERFORM 5000-PROVE-RESET
           PERFORM 6000-TERMINATE
           MOVE WS-RETURN-CODE             TO RETURN-CODE
           GOBACK
           .
      *
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           STRING WS-CD-DATE(1:4) '-' WS-CD-DATE(5:2) '-'
                  WS-CD-DATE(7:2) '-' WS-CD-TIME(1:2) '.'
                  WS-CD-TIME(3:2) '.' WS-CD-TIME(5:2) '.000000'
             DELIMITED BY SIZE INTO WS-TIMESTAMP
           END-STRING
      *
           IF LK-PARM-LEN < 8
               MOVE 'PARM MUST SUPPLY CCYYMMDD CYCLE DATE'
                                           TO ER-MESSAGE
               MOVE 0902                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
           MOVE WS-CYCLE-DATE              TO RH1-CYCLE-DATE
      *
           OPEN OUTPUT REGISTER-FILE
           OPEN I-O    CYCLCTL-FILE
           IF NOT WS-REGISTER-OK
               MOVE 'BKPREG  '             TO ER-FILE-NAME
               MOVE WS-REGISTER-STATUS     TO ER-FILE-STATUS
               MOVE 'OPEN OF BACKUP REGISTER FAILED' TO ER-MESSAGE
               MOVE 0902                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE 0902               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE CC-RECS-READ               TO WS-EXPECTED-AUTH
      *
           PERFORM 8000-PRINT-HEADINGS
           .
      *
      ******************************************************************
      * 2000 - CARD CROSS REFERENCE BACKUP                             *
      ******************************************************************
       2000-VERIFY-XREF.
           MOVE 'N'                        TO WS-EOF-SW
           OPEN INPUT BKPXREF-FILE
           IF NOT WS-BKPXREF-OK
               MOVE 'BKPXREF '             TO ER-FILE-NAME
               MOVE WS-BKPXREF-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF CARDXREF BACKUP FAILED' TO ER-MESSAGE
               MOVE 0902                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM UNTIL WS-EOF
               READ BKPXREF-FILE
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-XREF-CNT
               END-READ
               IF NOT WS-BKPXREF-OK AND NOT WS-BKPXREF-EOF
                   MOVE 'BKPXREF '         TO ER-FILE-NAME
                   MOVE WS-BKPXREF-STATUS  TO ER-FILE-STATUS
                   MOVE 'READ OF CARDXREF BACKUP FAILED' TO ER-MESSAGE
                   MOVE 0902               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE BKPXREF-FILE
      *
           IF WS-XREF-CNT = ZERO
               MOVE 'CARDXREF BACKUP GENERATION IS EMPTY'
                                           TO ER-MESSAGE
               MOVE 0901                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE 'BKPXREF '                 TO RD-DD-NAME
           MOVE 'CARD.PROD.CARDXREF '      TO RD-CLUSTER
           MOVE WS-XREF-CNT                TO RD-RECORDS
           MOVE ZERO                       TO RD-HASH
           PERFORM 8100-WRITE-REGISTER
           .
      *
      ******************************************************************
      * 3000 - CYCLE CONTROL BACKUP                                    *
      ******************************************************************
       3000-VERIFY-CYCLCTL.
           MOVE 'N'                        TO WS-EOF-SW
           OPEN INPUT BKPCTL-FILE
           IF NOT WS-BKPCTL-OK
               MOVE 'BKPCTL  '             TO ER-FILE-NAME
               MOVE WS-BKPCTL-STATUS       TO ER-FILE-STATUS
               MOVE 'OPEN OF CYCLCTL BACKUP FAILED' TO ER-MESSAGE
               MOVE 0902                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM UNTIL WS-EOF
               READ BKPCTL-FILE
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-CTL-CNT
               END-READ
           END-PERFORM
      *
           CLOSE BKPCTL-FILE
      *
           IF WS-CTL-CNT = ZERO
               MOVE 'CYCLCTL BACKUP GENERATION IS EMPTY'
                                           TO ER-MESSAGE
               MOVE 0901                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE 'BKPCTL  '                 TO RD-DD-NAME
           MOVE 'CARD.PROD.CYCLCTL  '      TO RD-CLUSTER
           MOVE WS-CTL-CNT                 TO RD-RECORDS
           MOVE ZERO                       TO RD-HASH
           PERFORM 8100-WRITE-REGISTER
           .
      *
      ******************************************************************
      * 4000 - AUTHORIZATION LOG BACKUP.  THIS IS THE ONE THAT MATTERS *
      *        - THE LIVE CLUSTER HAS ALREADY BEEN DELETED AND         *
      *        REDEFINED BY THE TIME THIS STEP RUNS, SO THE BACKUP IS  *
      *        THE ONLY COPY OF THE NIGHT'S AUTHORISATIONS.            *
      ******************************************************************
       4000-VERIFY-AUTHLOG.
           MOVE 'N'                        TO WS-EOF-SW
           OPEN INPUT BKPAUTH-FILE
           IF NOT WS-BKPAUTH-OK
               MOVE 'BKPAUTH '             TO ER-FILE-NAME
               MOVE WS-BKPAUTH-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF AUTHLOG BACKUP FAILED' TO ER-MESSAGE
               MOVE 0902                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM UNTIL WS-EOF
               READ BKPAUTH-FILE
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-AUTH-CNT
                       PERFORM 4100-ACCUMULATE-HASH
               END-READ
               IF NOT WS-BKPAUTH-OK AND NOT WS-BKPAUTH-EOF
                   MOVE 'BKPAUTH '         TO ER-FILE-NAME
                   MOVE WS-BKPAUTH-STATUS  TO ER-FILE-STATUS
                   MOVE 'READ OF AUTHLOG BACKUP FAILED' TO ER-MESSAGE
                   MOVE 0902               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
               END-IF
           END-PERFORM
      *
           CLOSE BKPAUTH-FILE
      *
           IF WS-AUTH-CNT = ZERO
               MOVE 'AUTHLOG BACKUP GENERATION IS EMPTY'
                                           TO ER-MESSAGE
               MOVE 0901                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
      *    THE EXTRACT ONLY SELECTS THE CYCLE'S OWN RECORDS, SO THE
      *    BACKUP IS NORMALLY THE LARGER OF THE TWO.  IF IT IS SMALLER
      *    THAN WHAT CBCRD01 READ, SOMETHING WAS LOST BETWEEN THE TWO
      *    JOBS AND OPERATIONS MUST BE TOLD BEFORE THE RESET IS TRUSTED.
           IF WS-AUTH-CNT < WS-EXPECTED-AUTH
               MOVE WS-RC-WARNING          TO WS-RETURN-CODE
               DISPLAY 'CBCRD09 - WARNING, AUTHLOG BACKUP HOLDS '
                       WS-AUTH-CNT ' RECORDS BUT CBCRD01 READ '
                       WS-EXPECTED-AUTH
           END-IF
      *
           MOVE 'BKPAUTH '                 TO RD-DD-NAME
           MOVE 'CARD.PROD.AUTHLOG  '      TO RD-CLUSTER
           MOVE WS-AUTH-CNT                TO RD-RECORDS
           MOVE WS-HASH-TOTAL              TO RD-HASH
           PERFORM 8100-WRITE-REGISTER
           .
      *
      ******************************************************************
      * 4100 - HASH THE CARD NUMBER.  THE SAME ALGORITHM AS CBCRD01 SO *
      *        THE TWO TOTALS CAN BE COMPARED BY CBCRD10.  NON NUMERIC *
      *        CARD NUMBERS ARE SKIPPED RATHER THAN ABENDING - THE     *
      *        LOG HOLDS THE OCCASIONAL TEST RECORD.                   *
      ******************************************************************
       4100-ACCUMULATE-HASH.
           IF BA-CARD-NUM IS NUMERIC
               MOVE BA-CARD-NUM            TO WS-CARD-DIGITS
               COMPUTE WS-HASH-WORK = FUNCTION MOD
                       (WS-CARD-DIGITS 999999999)
               ADD WS-HASH-WORK            TO WS-HASH-TOTAL
           END-IF
           .
      *
      ******************************************************************
      * 5000 - PROVE THE RE-DEFINED CLUSTER IS USABLE AND EMPTY        *
      ******************************************************************
       5000-PROVE-RESET.
           MOVE 'N'                        TO WS-EOF-SW
           OPEN INPUT AUTHLOG-FILE
           IF NOT WS-AUTHLOG-OK
               MOVE 'AUTHLOG '             TO ER-FILE-NAME
               MOVE WS-AUTHLOG-STATUS      TO ER-FILE-STATUS
               MOVE 'RE-DEFINED AUTHLOG WILL NOT OPEN' TO ER-MESSAGE
               MOVE 0903                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           PERFORM UNTIL WS-EOF
               READ AUTHLOG-FILE
                   AT END
                       MOVE 'Y'            TO WS-EOF-SW
                   NOT AT END
                       ADD 1               TO WS-RESIDUAL-CNT
               END-READ
           END-PERFORM
      *
           CLOSE AUTHLOG-FILE
      *
           IF WS-RESIDUAL-CNT NOT = ZERO
               DISPLAY 'CBCRD09 - RESET AUTHLOG STILL HOLDS '
                       WS-RESIDUAL-CNT ' RECORDS'
               MOVE 'RE-DEFINED AUTHLOG IS NOT EMPTY'
                                           TO ER-MESSAGE
               MOVE 0903                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           DISPLAY 'CBCRD09 - AUTHLOG RESET VERIFIED EMPTY'
           .
      *
       6000-TERMINATE.
           PERFORM 6100-UPDATE-CYCLE-CONTROL
      *
           CLOSE REGISTER-FILE
                 CYCLCTL-FILE
      *
           DISPLAY '----------------------------------------------'
           MOVE WS-XREF-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD09 CARDXREF BACKED UP ' WS-DISPLAY-CNT
           MOVE WS-CTL-CNT                 TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD09 CYCLCTL BACKED UP  ' WS-DISPLAY-CNT
           MOVE WS-AUTH-CNT                TO WS-DISPLAY-CNT
           DISPLAY 'CBCRD09 AUTHLOG BACKED UP  ' WS-DISPLAY-CNT
           DISPLAY 'CBCRD09 AUTHLOG HASH TOTAL ' WS-HASH-TOTAL
           DISPLAY '----------------------------------------------'
           .
      *
       6100-UPDATE-CYCLE-CONTROL.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'CYCLE CONTROL RECORD DISAPPEARED MID STEP'
                                           TO ER-MESSAGE
                   MOVE 0902               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
           MOVE 'CBCRD09 '                 TO CC-CURRENT-STEP
                                              CC-LAST-GOOD-STEP
           MOVE CYCLE-CTRL-RECORD          TO CYCLCTL-REC
           REWRITE CYCLCTL-REC
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'REWRITE OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0902                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           .
      *
       8000-PRINT-HEADINGS.
           ADD 1                           TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT                TO RH1-PAGE
           WRITE REGISTER-REC FROM REG-HEAD-1
           WRITE REGISTER-REC FROM REG-HEAD-2
           MOVE SPACES                     TO REGISTER-REC
           WRITE REGISTER-REC
           MOVE 4                          TO WS-LINE-CNT
           .
      *
       8100-WRITE-REGISTER.
           MOVE WS-TIMESTAMP(1:19)         TO RD-VERIFIED-TS
           WRITE REGISTER-REC FROM REG-DETAIL
           IF NOT WS-REGISTER-OK
               MOVE 'BKPREG  '             TO ER-FILE-NAME
               MOVE WS-REGISTER-STATUS     TO ER-FILE-STATUS
               MOVE 'WRITE TO BACKUP REGISTER FAILED' TO ER-MESSAGE
               MOVE 0902                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           ADD 1                           TO WS-LINE-CNT
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'VSAM'                     TO ER-ERROR-TYPE
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD09 FATAL ' ER-MESSAGE
                   ' ABEND=U' WS-ABEND-CODE
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
