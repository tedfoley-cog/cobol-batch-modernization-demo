      ******************************************************************
      * CACRD17 - CA00 EXIT AND SESSION AUDIT                          *
      *                                                                *
      * THE END OF EVERY CA00 SESSION.  REACHED TWO WAYS -             *
      *   FROM THE MENU, WHEN THE OPERATOR KEYS X OR PRESSES PF12.     *
      *     CACRD00 ASKS THE DISPATCHER FOR MENU/OPTX AND THE          *
      *     DISPATCHER TRANSFERS CONTROL HERE.                         *
      *   FROM CACRD10, AT THE FOOT OF THE AUTHORIZATION CHAIN.        *
      *                                                                *
      * WHAT IT DOES -                                                 *
      *   WRITES ONE AUDIT LINE PER SESSION TO THE TRANSIENT DATA      *
      *     QUEUE CAUD, WHICH THE OVERNIGHT JOB CBREF07J PRINTS.       *
      *     WHEN THE SESSION RAN AN AUTHORIZATION THE PROGRAM TRAIL    *
      *     GOES OUT AS A SECOND LINE - THAT IS WHAT THE AUDITORS      *
      *     ASKED FOR AFTER THE 2009 REVIEW.                           *
      *   DELETES THE ROUTE CACHE QUEUE CARDRTQ THAT CACRD90 BUILT,    *
      *     SO A NEW SESSION RESOLVES ITS ROUTES AGAIN AND PICKS UP    *
      *     ANY CHANGE MADE TO PGM_ROUTE DURING THE DAY.               *
      *   CLEARS THE COMMAREA, SENDS THE GOODBYE SCREEN AND RETURNS    *
      *     WITHOUT A TRANSID SO THE PSEUDO CONVERSATION ENDS.         *
      *                                                                *
      * CALLED BY   - CACRD90  XCTL, ROUTE MENU/OPTX                   *
      *             - CACRD10  XCTL                                    *
      * CALLS       - CACRD91  ERROR HANDLER                           *
      * MAPSET      - CARDSET   MAP CARDBYE                            *
      * TDQ         - CAUD      SESSION AUDIT                          *
      * TSQ         - CARDRTQ   ROUTE CACHE, DELETED HERE              *
      *             - CARDAUTQ  AUTH SKELETON, DELETED IF LEFT OVER    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD17.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD17 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDSET '.
       01  WS-MAP-BYE                  PIC X(8)  VALUE 'CARDBYE '.
       01  WS-ROUTE-TSQ                PIC X(8)  VALUE 'CARDRTQ '.
       01  WS-AUTH-TSQ                 PIC X(8)  VALUE 'CARDAUTQ'.
       01  WS-AUDIT-TDQ                PIC X(4)  VALUE 'CAUD'.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-DISP                PIC X(10) VALUE SPACES.
       01  WS-TIME-DISP                PIC X(8)  VALUE SPACES.
      *
       01  WS-SUB                      PIC S9(4) COMP VALUE 0.
       01  WS-TRAIL-POS                PIC S9(4) COMP VALUE 1.
      *
      *    ---------------------------------------------------------
      *    THE AUDIT LINE.  FIXED COLUMNS - CBREF07J READS IT BY
      *    POSITION, SO NOTHING MAY BE INSERTED IN THE MIDDLE.
      *    ---------------------------------------------------------
       01  AUDIT-LINE.
           05  AU-REC-TYPE             PIC X(4)  VALUE 'SESS'.
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-DATE                 PIC X(10).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-TIME                 PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-TRAN-ID              PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-TERM-ID              PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-OPER-ID              PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-LAST-SCREEN          PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-ROUTE-KEY            PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-ACCT-ID              PIC X(11).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-CARD-NUM             PIC X(16).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-DEC-STATUS           PIC X.
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-RESP-CODE            PIC X(2).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-REASON-CD            PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AU-AUTH-SEQ             PIC X(9).
           05  FILLER                  PIC X(10) VALUE SPACES.
      *
       01  AUDIT-TRAIL-LINE.
           05  AT-REC-TYPE             PIC X(4)  VALUE 'TRAL'.
           05  FILLER                  PIC X     VALUE SPACE.
           05  AT-TERM-ID              PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AT-CNT                  PIC 9(2).
           05  FILLER                  PIC X     VALUE SPACE.
           05  AT-TEXT                 PIC X(104) VALUE SPACES.
      *
       01  WS-TRAIL-ENTRY.
           05  WS-TE-PGM               PIC X(8).
           05  WS-TE-SEP               PIC X     VALUE '/'.
           05  WS-TE-RC                PIC 9(4).
           05  WS-TE-GAP               PIC X     VALUE SPACE.
      *
       01  WS-AUDIT-LEN                PIC S9(4) COMP VALUE 120.
      *
       01  WS-CARD-MASKED              PIC X(16) VALUE SPACES.
      *
           COPY CVAUTHW1Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
           COPY CARDSET.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INIT
      *
           IF EIBCALEN > ZERO
               MOVE DFHCOMMAREA        TO CA-WORK-AREA
           ELSE
      *        A DIRECT START WITH NO COMMAREA STILL GETS A CLEAN
      *        GOODBYE SCREEN - THE OPERATOR SEES THE SESSION END
               INITIALIZE CA-WORK-AREA
               MOVE EIBTRNID           TO CAW-TRAN-ID
               MOVE EIBTRMID           TO CAW-TERM-ID
           END-IF
      *
           PERFORM 1000-WRITE-AUDIT
           PERFORM 2000-DELETE-QUEUES
           PERFORM 3000-SEND-GOODBYE
           PERFORM 4000-CLEAR-COMMAREA
      *
      *    NO TRANSID - THE PSEUDO CONVERSATION IS OVER
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           MOVE SPACES                 TO ERROR-AREA
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-DISP)
                     DATESEP('-')
                     TIME(WS-TIME-DISP)
                     TIMESEP(':')
                     RESP(WS-RESP)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE SPACES             TO WS-TIME-DISP
                                          WS-DATE-DISP
           END-IF
           .
      *
      ******************************************************************
      * 1000 - THE AUDIT TRAIL                                         *
      ******************************************************************
       1000-WRITE-AUDIT.
           MOVE 'SESS'                 TO AU-REC-TYPE
           MOVE WS-DATE-DISP           TO AU-DATE
           MOVE WS-TIME-DISP           TO AU-TIME
           MOVE CAW-TRAN-ID            TO AU-TRAN-ID
           MOVE CAW-TERM-ID            TO AU-TERM-ID
           MOVE CAW-OPER-ID            TO AU-OPER-ID
           MOVE CAW-SCREEN-ID          TO AU-LAST-SCREEN
           MOVE CAW-ROUTE-KEY          TO AU-ROUTE-KEY
           MOVE CAW-ACCT-ID            TO AU-ACCT-ID
      *
           PERFORM 1100-MASK-CARD
           MOVE WS-CARD-MASKED         TO AU-CARD-NUM
      *
           MOVE CAW-DEC-STATUS         TO AU-DEC-STATUS
           MOVE CAW-DEC-RESP           TO AU-RESP-CODE
           MOVE CAW-DEC-REASON         TO AU-REASON-CD
           MOVE CAW-AUTH-SEQ-NUM       TO AU-AUTH-SEQ
      *
           MOVE 120                    TO WS-AUDIT-LEN
      *
           EXEC CICS WRITEQ TD
                     QUEUE(WS-AUDIT-TDQ)
                     FROM(AUDIT-LINE)
                     LENGTH(WS-AUDIT-LEN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '1000-WRITE-AUDIT' TO ER-PARAGRAPH
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE 'W'                TO ER-SEVERITY
               MOVE 'SESSION AUDIT QUEUE WRITE FAILED'
                                       TO ER-MESSAGE
               PERFORM 8100-CICS-ERROR
      *        THE SESSION STILL ENDS CLEANLY - AN AUDIT LINE IS NOT
      *        WORTH FAILING THE OPERATOR FOR
               GO TO 1000-EXIT
           END-IF
      *
           IF CAW-TRAIL-CNT > ZERO
               PERFORM 1200-WRITE-TRAIL
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
      *    ONLY THE FIRST SIX AND LAST FOUR DIGITS MAY BE PRINTED
       1100-MASK-CARD.
           MOVE SPACES                 TO WS-CARD-MASKED
      *
           IF CAW-RQ-CARD-NUM NOT = SPACES
               MOVE CAW-RQ-CARD-NUM    TO WS-CARD-MASKED
           ELSE
               MOVE CAW-CARD-NUM       TO WS-CARD-MASKED
           END-IF
      *
           IF WS-CARD-MASKED NOT = SPACES
               MOVE '******'           TO WS-CARD-MASKED(7:6)
           END-IF
           .
      *
       1200-WRITE-TRAIL.
           MOVE 'TRAL'                 TO AT-REC-TYPE
           MOVE CAW-TERM-ID            TO AT-TERM-ID
           MOVE CAW-TRAIL-CNT          TO AT-CNT
           MOVE SPACES                 TO AT-TEXT
           MOVE 1                      TO WS-TRAIL-POS
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > CAW-TRAIL-CNT
                        OR WS-SUB > 8
               MOVE CAW-TRAIL-PGM(WS-SUB)
                                       TO WS-TE-PGM
               MOVE CAW-TRAIL-RC(WS-SUB)
                                       TO WS-TE-RC
               IF WS-TRAIL-POS < 91
                   MOVE WS-TRAIL-ENTRY TO AT-TEXT(WS-TRAIL-POS:14)
                   ADD 14              TO WS-TRAIL-POS
               END-IF
           END-PERFORM
      *
           MOVE 120                    TO WS-AUDIT-LEN
      *
           EXEC CICS WRITEQ TD
                     QUEUE(WS-AUDIT-TDQ)
                     FROM(AUDIT-TRAIL-LINE)
                     LENGTH(WS-AUDIT-LEN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '1200-WRITE-TRAIL' TO ER-PARAGRAPH
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE 'W'                TO ER-SEVERITY
               MOVE 'SESSION TRAIL QUEUE WRITE FAILED'
                                       TO ER-MESSAGE
               PERFORM 8100-CICS-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 2000 - TIDY THE TEMPORARY STORAGE UP                           *
      *                                                                *
      * QIDERR IS THE NORMAL ANSWER WHEN THE SESSION NEVER DISPATCHED  *
      * ANYTHING OR NEVER RAN AN AUTHORIZATION.  ANYTHING ELSE IS      *
      * LOGGED - A QUEUE LEFT BEHIND ON A TERMINAL WOULD BE PICKED UP  *
      * BY THE NEXT OPERATOR.                                          *
      ******************************************************************
       2000-DELETE-QUEUES.
           EXEC CICS DELETEQ TS
                     QUEUE(WS-ROUTE-TSQ)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(QIDERR)
                   CONTINUE
               WHEN OTHER
                   MOVE '2000-DELETE-QUEUES'
                                       TO ER-PARAGRAPH
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE 'W'            TO ER-SEVERITY
                   MOVE 'ROUTE CACHE QUEUE NOT DELETED'
                                       TO ER-MESSAGE
                   PERFORM 8100-CICS-ERROR
           END-EVALUATE
      *
           EXEC CICS DELETEQ TS
                     QUEUE(WS-AUTH-TSQ)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(QIDERR)
                   CONTINUE
               WHEN OTHER
                   MOVE '2000-DELETE-QUEUES'
                                       TO ER-PARAGRAPH
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE 'W'            TO ER-SEVERITY
                   MOVE 'AUTH SKELETON QUEUE NOT DELETED'
                                       TO ER-MESSAGE
                   PERFORM 8100-CICS-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - THE GOODBYE SCREEN                                      *
      ******************************************************************
       3000-SEND-GOODBYE.
           MOVE LOW-VALUES             TO CARDBYEO
      *
           MOVE CAW-OPER-ID            TO BYOPERO
           MOVE CAW-TERM-ID            TO BYTERMO
           MOVE CAW-TRAIL-CNT          TO BYCNTO
           MOVE WS-TIME-DISP           TO BYTIMEO
      *
           EVALUATE TRUE
               WHEN CAW-DEC-APPROVED
                   MOVE 'AUTHORIZATION APPROVED - SESSION ENDED'
                                       TO BYMSGO
               WHEN CAW-DEC-REFERRED
                   MOVE 'AUTHORIZATION REFERRED - SESSION ENDED'
                                       TO BYMSGO
               WHEN CAW-DEC-DECLINED
                   MOVE 'AUTHORIZATION DECLINED - SESSION ENDED'
                                       TO BYMSGO
               WHEN OTHER
                   MOVE 'CA00 SESSION ENDED - THANK YOU'
                                       TO BYMSGO
           END-EVALUATE
      *
           EXEC CICS SEND
                     MAP(WS-MAP-BYE)
                     MAPSET(WS-MAPSET)
                     FROM(CARDBYEO)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
      *        THE TERMINAL MAY HAVE GONE.  THERE IS NOTHING LEFT TO
      *        SEND AN ERROR SCREEN TO, SO IT IS ONLY RECORDED.
               MOVE '3000-SEND-GOODBYE'
                                       TO ER-PARAGRAPH
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE 'W'                TO ER-SEVERITY
               MOVE 'GOODBYE SCREEN NOT SENT'
                                       TO ER-MESSAGE
               PERFORM 8100-CICS-ERROR
           END-IF
           .
      *
       4000-CLEAR-COMMAREA.
           INITIALIZE CA-WORK-AREA
           IF EIBCALEN > ZERO
               MOVE CA-WORK-AREA       TO DFHCOMMAREA
           END-IF
           .
      *
      ******************************************************************
      * 8100 - ERROR HANDLING                                          *
      ******************************************************************
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE EIBTRMID               TO ER-TERM-ID
           MOVE 'N'                    TO ER-ABEND-REQUESTED
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM-ONLINE)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE SPACES                 TO ER-MESSAGE
           .
