      ******************************************************************
      * CBCRD06W - BRANCH COMPLETION WAIT / VERIFY                     *
      *                                                                *
      * STEP005 OF JOB CBCRD06J.  THE FIRST THING THE JOIN DOES.       *
      *                                                                *
      * CBCRD05AJ AND CBCRD05BJ RUN IN PARALLEL AND EACH POSTS ITS     *
      * OWN COMPLETION FLAG INTO THE CYCLE CONTROL RECORD ON VSAM      *
      * CYCLCTL AS ITS LAST STEP                                       *
      *                                                                *
      *   CC-FILLER(1:1) = 'A'  FEE BRANCH COMPLETED                   *
      *   CC-FILLER(2:1) = 'B'  INTEREST BRANCH COMPLETED              *
      *   EITHER SET TO   'F'   THAT BRANCH ENDED IN ERROR             *
      *                                                                *
      * THE SCHEDULER DEPENDENCY IS DECLARED SEPARATELY, BUT AN        *
      * OPERATOR CAN RELEASE A JOB BY HAND AND THE 2004 CYCLE PROVED   *
      * IT HAPPENS.  THIS STEP MAKES THE JOB SELF DEFENDING - IF       *
      * EITHER FLAG IS MISSING OR FAILED FOR THE CURRENT CYCLE DATE    *
      * THE JOB ABENDS BEFORE ANY EXPOSURE IS RECALCULATED, RATHER     *
      * THAN QUIETLY RECALCULATING FROM HALF A CYCLE.                  *
      *                                                                *
      * THE PROGRAM CAN OPTIONALLY POLL - PARM CCYYMMDD,WAIT=NNN       *
      * RE-READS THE RECORD UP TO NNN TIMES.  THE STANDARD SCHEDULE    *
      * PASSES WAIT=000, WHICH IS A SINGLE CHECK WITH NO WAIT.         *
      *                                                                *
      * CALLED BY   - JCL ONLY                                         *
      * CALLS       - CBCRD91 (BATCH ERROR HANDLER, FATAL ONLY)        *
      * FILES       - CYCLCTL  VSAM KSDS INPUT                         *
      *                                                                *
      * RETURN CODE - 0000 BOTH BRANCHES COMPLETED                     *
      * USER ABEND  - U0601 CYCLE CONTROL UNUSABLE                     *
      *               U0610 FEE BRANCH NOT COMPLETE                    *
      *               U0611 INTEREST BRANCH NOT COMPLETE               *
      *               U0612 A BRANCH ENDED IN ERROR                    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD06W.
       AUTHOR.        CARD SYSTEMS.
       DATE-WRITTEN.  2004-11-30.
      *
      * MAINTENANCE
      * 2004-11-30 CRD4207 ORIGINAL - WRITTEN AFTER THE NOVEMBER
      *                    CYCLE RECALCULATED EXPOSURE BEFORE THE
      *                    FEE BRANCH HAD POSTED
      * 2009-09-02 CRD6011 OPTIONAL POLL ADDED FOR THE MONTH END
      *                    CYCLE WHERE THE BRANCHES OVERRUN
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CYCLCTL-FILE  ASSIGN TO CYCLCTL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS CTL-KEY
                  FILE STATUS  IS WS-CYCLCTL-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
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
       01  WS-PROGRAM-ID                   PIC X(8)  VALUE 'CBCRD06W'.
       01  WS-STEP-NAME                    PIC X(8)  VALUE 'STEP005 '.
      *
       01  WS-CYCLCTL-STATUS               PIC X(2)  VALUE '00'.
           88  WS-CYCLCTL-OK                         VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-BOTH-SW                  PIC X     VALUE 'N'.
               88  WS-BOTH-COMPLETE                  VALUE 'Y'.
      *
       01  WS-CYCLE-DATE                   PIC 9(8)  VALUE ZERO.
       01  WS-WAIT-LIMIT                   PIC 9(3)  VALUE ZERO.
       01  WS-POLL-CNT                     PIC 9(3)  VALUE ZERO.
       01  WS-DELAY-SUB                    PIC 9(9)  VALUE ZERO.
       01  WS-DELAY-WORK                   PIC 9(9)  VALUE ZERO.
      *
       01  WS-FEE-FLAG                     PIC X     VALUE SPACE.
           88  WS-FEE-DONE                           VALUE 'A'.
           88  WS-FEE-FAILED                         VALUE 'F'.
       01  WS-INT-FLAG                     PIC X     VALUE SPACE.
           88  WS-INT-DONE                           VALUE 'B'.
           88  WS-INT-FAILED                         VALUE 'F'.
      *
       01  WS-CURRENT-DATE.
           05  WS-CD-DATE                  PIC 9(8).
           05  WS-CD-TIME                  PIC 9(8).
           05  WS-CD-FILLER                PIC X(5).
       01  WS-TIMESTAMP                    PIC X(26) VALUE SPACES.
      *
       01  WS-ABEND-CODE                   PIC 9(4)  VALUE ZERO.
      *
           COPY CVCTRL01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
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
           PERFORM 2000-CHECK-BRANCHES
               UNTIL WS-BOTH-COMPLETE
                  OR WS-POLL-CNT > WS-WAIT-LIMIT
           PERFORM 3000-TERMINATE
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
               MOVE 0601                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
           MOVE LK-PARM-DATA(1:8)          TO WS-CYCLE-DATE
      *
           IF LK-PARM-LEN > 16
               IF LK-PARM-DATA(10:5) = 'WAIT='
                   MOVE LK-PARM-DATA(15:3) TO WS-WAIT-LIMIT
               END-IF
           END-IF
      *
           OPEN INPUT CYCLCTL-FILE
           IF NOT WS-CYCLCTL-OK
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'OPEN OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0601                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           DISPLAY 'CBCRD06W - BRANCH COMPLETION CHECK, CYCLE '
                   WS-CYCLE-DATE
           DISPLAY 'CBCRD06W - POLL LIMIT ' WS-WAIT-LIMIT
           .
      *
       2000-CHECK-BRANCHES.
           MOVE 'CARDNITE'                 TO CTL-CYCLE-TYPE
           MOVE WS-CYCLE-DATE              TO CTL-CYCLE-DATE
      *
           READ CYCLCTL-FILE INTO CYCLE-CTRL-RECORD
               INVALID KEY
                   MOVE 'NO CYCLE CONTROL RECORD FOR CYCLE DATE'
                                           TO ER-MESSAGE
                   MOVE WS-CYCLE-DATE      TO ER-VSAM-KEY(1:8)
                   MOVE 0601               TO WS-ABEND-CODE
                   PERFORM 9500-FATAL-ERROR
           END-READ
      *
           IF NOT WS-CYCLCTL-OK
           AND WS-CYCLCTL-STATUS NOT = '23'
               MOVE 'CYCLCTL '             TO ER-FILE-NAME
               MOVE WS-CYCLCTL-STATUS      TO ER-FILE-STATUS
               MOVE 'READ OF CYCLE CONTROL FAILED' TO ER-MESSAGE
               MOVE 0601                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           MOVE CC-FILLER(1:1)             TO WS-FEE-FLAG
           MOVE CC-FILLER(2:1)             TO WS-INT-FLAG
      *
           IF WS-FEE-DONE AND WS-INT-DONE
               MOVE 'Y'                    TO WS-BOTH-SW
               GO TO 2000-EXIT
           END-IF
      *
           IF WS-FEE-FAILED OR WS-INT-FAILED
               GO TO 2000-EXIT
           END-IF
      *
           ADD 1                           TO WS-POLL-CNT
           IF WS-POLL-CNT <= WS-WAIT-LIMIT
               DISPLAY 'CBCRD06W - BRANCHES INCOMPLETE, POLL '
                       WS-POLL-CNT ' FEE=' WS-FEE-FLAG
                       ' INT=' WS-INT-FLAG
               PERFORM 2100-DELAY
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2100 - CRUDE DELAY BETWEEN POLLS.  THERE IS NO WAIT SERVICE    *
      *        AVAILABLE TO A PLAIN BATCH PROGRAM AND THE OPERATIONS   *
      *        STANDARD FORBIDS AN ASSEMBLER STUB IN THIS CHAIN.       *
      ******************************************************************
       2100-DELAY.
           PERFORM VARYING WS-DELAY-SUB FROM 1 BY 1
                     UNTIL WS-DELAY-SUB > 500000
               COMPUTE WS-DELAY-WORK = WS-DELAY-SUB + 1
           END-PERFORM
           .
      *
       3000-TERMINATE.
           CLOSE CYCLCTL-FILE
      *
           IF WS-FEE-FAILED
               MOVE 'FEE BRANCH CBCRD05AJ ENDED IN ERROR'
                                           TO ER-MESSAGE
               MOVE 0612                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF WS-INT-FAILED
               MOVE 'INTEREST BRANCH CBCRD05BJ ENDED IN ERROR'
                                           TO ER-MESSAGE
               MOVE 0612                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF NOT WS-FEE-DONE
               MOVE 'FEE BRANCH HAS NOT POSTED ITS COMPLETION FLAG'
                                           TO ER-MESSAGE
               MOVE 0610                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           IF NOT WS-INT-DONE
               MOVE 'INTEREST BRANCH HAS NOT POSTED ITS FLAG'
                                           TO ER-MESSAGE
               MOVE 0611                   TO WS-ABEND-CODE
               PERFORM 9500-FATAL-ERROR
           END-IF
      *
           DISPLAY 'CBCRD06W - BOTH BRANCHES COMPLETE FOR CYCLE '
                   WS-CYCLE-DATE ' - JOIN MAY PROCEED'
           MOVE WS-RC-OK                   TO RETURN-CODE
           .
      *
       9500-FATAL-ERROR.
           MOVE WS-PROGRAM-ID              TO ER-PGM-NAME
           MOVE 'F'                        TO ER-SEVERITY
           MOVE 'BUSN'                     TO ER-ERROR-TYPE
           MOVE WS-TIMESTAMP               TO ER-TIMESTAMP
           MOVE 'Y'                        TO ER-ABEND-REQUESTED
           MOVE WS-ABEND-CODE              TO ER-ABEND-CODE
           DISPLAY 'CBCRD06W FATAL ' ER-MESSAGE
           DISPLAY 'CBCRD06W FEE FLAG=' WS-FEE-FLAG
                   ' INTEREST FLAG=' WS-INT-FLAG
                   ' ABEND=U' WS-ABEND-CODE
           CALL 'CBCRD91' USING ERROR-AREA
           MOVE WS-RC-FATAL                TO RETURN-CODE
           GOBACK
           .
