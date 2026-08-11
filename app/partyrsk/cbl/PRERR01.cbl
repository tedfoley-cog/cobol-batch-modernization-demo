      ******************************************************************
      * PRERR01 - PARTYRSK ONLINE ERROR HANDLER                        *
      *                                                                *
      * PARTY AND RISK RUNS UNDER ITS OWN RACF GROUP AND MUST NOT      *
      * DEPEND ON THE CARD SYSTEMS ERROR HANDLER.  THIS MODULE IS THE  *
      * PARTYRSK EQUIVALENT AND IS LINKED TO BY EVERY PARTYRSK ONLINE  *
      * PROGRAM WHEN A SQL, VSAM OR CICS CONDITION IS DETECTED.        *
      *                                                                *
      * IT FORMATS THE DIAGNOSTIC, WRITES IT TO THE PARTYRSK ERROR     *
      * TRANSIENT DATA QUEUE (PRER) AND TO A RECOVERABLE TEMPORARY     *
      * STORAGE QUEUE KEYED BY TASK NUMBER SO THE ON CALL TEAM CAN     *
      * BROWSE IT WITH CEBR AFTER THE TASK HAS ENDED.                  *
      *                                                                *
      * IF ER-ABEND-REQUESTED IS 'Y' THE TASK IS ABENDED WITH THE      *
      * CODE IN ER-ABEND-CODE SO THAT THE CICS UNIT OF WORK IS BACKED  *
      * OUT.  OTHERWISE CONTROL IS RETURNED TO THE CALLER WHICH IS     *
      * EXPECTED TO SET ITS OWN RETURN CODE.                           *
      *                                                                *
      * CALLED BY   - PRKYC01 PRKYC02 PRKYC03 PRKYC04                  *
      *               PRRSK02 PRRSK03 PRRSK04 PRRSK05                  *
      * CALLS       - NONE                                             *
      * PARAMETERS  - ERROR-AREA (CVERRS01Y)                           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRERR01.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRERR01 '.
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-TDQ-NAME                 PIC X(4)  VALUE 'PRER'.
       01  WS-TSQ-NAME                 PIC X(8)  VALUE SPACES.
       01  WS-TSQ-LENGTH               PIC S9(4) COMP VALUE 0.
       01  WS-ABEND-CODE               PIC X(4)  VALUE SPACES.
      *
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(10) VALUE SPACES.
           05  WS-TIME-HMS             PIC X(8)  VALUE SPACES.
      *
      ******************************************************************
      * THE ERROR LINE IS 132 BYTES SO IT PRINTS CLEANLY WHEN THE      *
      * PRER QUEUE IS SPOOLED BY THE PARTYWK CLEANUP STEP.             *
      ******************************************************************
       01  WS-ERROR-LINE.
           05  WS-EL-STAMP             PIC X(19).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-EL-MODULE            PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-EL-PGM               PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-EL-PARA              PIC X(30).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-EL-SEV               PIC X.
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-EL-TYPE              PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-EL-REASON            PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-EL-TEXT              PIC X(52).
      *
       01  WS-DETAIL-LINE.
           05  WS-DL-TAG               PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-DL-TEXT              PIC X(123).
      *
       01  WS-SQL-DISPLAY.
           05  FILLER                  PIC X(9)  VALUE 'SQLCODE= '.
           05  WS-SD-SQLCODE           PIC -(9)9.
           05  FILLER                  PIC X(11) VALUE ' SQLSTATE= '.
           05  WS-SD-SQLSTATE          PIC X(5).
           05  FILLER                  PIC X(8)  VALUE ' TABLE= '.
           05  WS-SD-TABLE             PIC X(18).
           05  FILLER                  PIC X(6)  VALUE ' OPER='.
           05  WS-SD-OPER              PIC X(8).
      *
       01  WS-VSAM-DISPLAY.
           05  FILLER                  PIC X(7)  VALUE 'FILE=  '.
           05  WS-VD-FILE              PIC X(8).
           05  FILLER                  PIC X(9)  VALUE ' STATUS= '.
           05  WS-VD-STATUS            PIC X(2).
           05  FILLER                  PIC X(6)  VALUE ' KEY= '.
           05  WS-VD-KEY               PIC X(32).
      *
       01  WS-CICS-DISPLAY.
           05  FILLER                  PIC X(8)  VALUE 'EIBRESP='.
           05  WS-CD-RESP              PIC ZZZZ9.
           05  FILLER                  PIC X(10) VALUE ' EIBRESP2='.
           05  WS-CD-RESP2             PIC ZZZZ9.
           05  FILLER                  PIC X(7)  VALUE ' TRAN= '.
           05  WS-CD-TRAN              PIC X(4).
           05  FILLER                  PIC X(7)  VALUE ' TERM= '.
           05  WS-CD-TERM              PIC X(4).
      *
       01  WS-TASK-DISPLAY             PIC 9(7)  VALUE ZERO.
      *
           COPY CVCONSTY.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(275).
      *
           COPY CVERRS01Y.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           IF EIBCALEN = ZERO
               EXEC CICS ABEND ABCODE('PRE1') NODUMP END-EXEC
           END-IF
      *
           SET ADDRESS OF ERROR-AREA   TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-STAMP-TIME
           PERFORM 2000-BUILD-HEADER
           PERFORM 3000-WRITE-TDQ
           PERFORM 4000-WRITE-TSQ
           PERFORM 5000-CHECK-ABEND
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - TIME STAMP FOR THE DIAGNOSTIC                           *
      ******************************************************************
       1000-STAMP-TIME.
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-CYMD)
                     DATESEP('-')
                     TIME(WS-TIME-HMS)
                     TIMESEP(':')
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE SPACES                 TO WS-EL-STAMP
           MOVE WS-DATE-CYMD           TO WS-EL-STAMP(1:10)
           MOVE WS-TIME-HMS            TO WS-EL-STAMP(12:8)
           .
      *
      ******************************************************************
      * 2000 - BUILD THE ONE LINE SUMMARY                              *
      ******************************************************************
       2000-BUILD-HEADER.
           MOVE WS-MODULE-PARTYRSK     TO WS-EL-MODULE
           MOVE ER-PGM-NAME            TO WS-EL-PGM
           MOVE ER-PARAGRAPH           TO WS-EL-PARA
           MOVE ER-SEVERITY            TO WS-EL-SEV
           MOVE ER-ERROR-TYPE          TO WS-EL-TYPE
           MOVE ER-REASON-CD           TO WS-EL-REASON
           MOVE ER-MESSAGE             TO WS-EL-TEXT
      *
           IF ER-PGM-NAME = SPACES
               MOVE 'UNKNOWN '         TO WS-EL-PGM
           END-IF
           .
      *
      ******************************************************************
      * 3000 - WRITE THE SUMMARY AND THE TYPE SPECIFIC DETAIL TO THE   *
      *        PARTYRSK ERROR TRANSIENT DATA QUEUE                     *
      ******************************************************************
       3000-WRITE-TDQ.
           EXEC CICS WRITEQ TD
                     QUEUE(WS-TDQ-NAME)
                     FROM(WS-ERROR-LINE)
                     LENGTH(LENGTH OF WS-ERROR-LINE)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
      *    A FAILING ERROR QUEUE MUST NEVER LOOP BACK INTO THE ERROR
      *    HANDLER.  THE DIAGNOSTIC IS STILL WRITTEN TO TEMPORARY
      *    STORAGE BELOW.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               CONTINUE
           END-IF
      *
           EVALUATE TRUE
               WHEN ER-TYPE-SQL
                   PERFORM 3100-DETAIL-SQL
               WHEN ER-TYPE-VSAM
                   PERFORM 3200-DETAIL-VSAM
               WHEN ER-TYPE-CICS
                   PERFORM 3300-DETAIL-CICS
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
       3100-DETAIL-SQL.
           MOVE ER-SQLCODE             TO WS-SD-SQLCODE
           MOVE ER-SQLSTATE            TO WS-SD-SQLSTATE
           MOVE ER-SQL-TABLE           TO WS-SD-TABLE
           MOVE ER-SQL-OPERATION       TO WS-SD-OPER
           MOVE 'SQL     '             TO WS-DL-TAG
           MOVE WS-SQL-DISPLAY         TO WS-DL-TEXT
           PERFORM 3900-WRITE-DETAIL
           .
      *
       3200-DETAIL-VSAM.
           MOVE ER-FILE-NAME           TO WS-VD-FILE
           MOVE ER-FILE-STATUS         TO WS-VD-STATUS
           MOVE ER-VSAM-KEY            TO WS-VD-KEY
           MOVE 'VSAM    '             TO WS-DL-TAG
           MOVE WS-VSAM-DISPLAY        TO WS-DL-TEXT
           PERFORM 3900-WRITE-DETAIL
           .
      *
       3300-DETAIL-CICS.
           MOVE ER-EIBRESP             TO WS-CD-RESP
           MOVE ER-EIBRESP2            TO WS-CD-RESP2
           MOVE ER-TRAN-ID             TO WS-CD-TRAN
           MOVE ER-TERM-ID             TO WS-CD-TERM
           MOVE 'CICS    '             TO WS-DL-TAG
           MOVE WS-CICS-DISPLAY        TO WS-DL-TEXT
           PERFORM 3900-WRITE-DETAIL
           .
      *
       3900-WRITE-DETAIL.
           EXEC CICS WRITEQ TD
                     QUEUE(WS-TDQ-NAME)
                     FROM(WS-DETAIL-LINE)
                     LENGTH(LENGTH OF WS-DETAIL-LINE)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
      ******************************************************************
      * 4000 - KEEP A COPY IN TEMPORARY STORAGE KEYED BY TASK NUMBER   *
      *        QUEUE NAME IS PRE PLUS THE LOW ORDER TASK NUMBER        *
      ******************************************************************
       4000-WRITE-TSQ.
           MOVE EIBTASKN               TO WS-TASK-DISPLAY
           MOVE SPACES                 TO WS-TSQ-NAME
           MOVE 'PRE'                  TO WS-TSQ-NAME(1:3)
           MOVE WS-TASK-DISPLAY(3:5)   TO WS-TSQ-NAME(4:5)
      *
           MOVE LENGTH OF WS-ERROR-LINE
                                       TO WS-TSQ-LENGTH
           EXEC CICS WRITEQ TS
                     QUEUE(WS-TSQ-NAME)
                     FROM(WS-ERROR-LINE)
                     LENGTH(WS-TSQ-LENGTH)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           MOVE LENGTH OF ERROR-AREA   TO WS-TSQ-LENGTH
           EXEC CICS WRITEQ TS
                     QUEUE(WS-TSQ-NAME)
                     FROM(ERROR-AREA)
                     LENGTH(WS-TSQ-LENGTH)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
      ******************************************************************
      * 5000 - ABEND IF THE CALLER ASKED FOR ONE                       *
      *        THE DEFAULT CODE IS PRXX WHEN NONE WAS SUPPLIED.        *
      ******************************************************************
       5000-CHECK-ABEND.
           IF ER-ABEND-REQUESTED NOT = 'Y'
               GO TO 5000-EXIT
           END-IF
      *
           IF ER-ABEND-CODE = SPACES OR LOW-VALUES
               MOVE 'PRXX'             TO WS-ABEND-CODE
           ELSE
               MOVE ER-ABEND-CODE      TO WS-ABEND-CODE
           END-IF
      *
           EXEC CICS ABEND
                     ABCODE(WS-ABEND-CODE)
           END-EXEC
           .
       5000-EXIT.
           EXIT
           .
