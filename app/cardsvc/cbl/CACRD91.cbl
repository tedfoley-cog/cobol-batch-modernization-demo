      ******************************************************************
      * CACRD91 - CARDSVC ONLINE ERROR HANDLER                         *
      *                                                                *
      * LINKED BY EVERY ONLINE CARDSVC PROGRAM WITH ERROR-AREA         *
      * (CVERRS01Y) AS THE COMMAREA.  THE CALLER FILLS IN WHAT IT      *
      * KNOWS - PROGRAM, PARAGRAPH, SEVERITY, AND EITHER THE SQL       *
      * DETAIL, THE VSAM DETAIL OR THE CICS DETAIL.                    *
      *                                                                *
      * WHAT HAPPENS DEPENDS ON ER-SEVERITY -                          *
      *                                                                *
      *   I  INFORMATION.  LOGGED TO THE OPERATIONS QUEUE ONLY.        *
      *      CONTROL GOES STRAIGHT BACK TO THE CALLER.                 *
      *   W  WARNING.  LOGGED.  CONTROL GOES BACK TO THE CALLER,       *
      *      WHICH CARRIES ON AND USUALLY TELLS THE OPERATOR ITSELF.   *
      *   E  ERROR.  LOGGED, THE DIAGNOSTIC SCREEN IS SENT AND THE     *
      *      TASK IS ENDED WITH THE MENU TRANSACTION SET UP, SO THE    *
      *      NEXT ENTER PUTS THE OPERATOR BACK ON THE MENU.            *
      *   F  FATAL.  LOGGED, THE SCREEN IS SENT AND A USER ABEND IS    *
      *      ISSUED SO CICS BACKS THE UNIT OF WORK OUT.  THE ABEND     *
      *      CODE IS THE ONE THE CALLER ASKED FOR, OR AECD IF IT DID   *
      *      NOT ASK FOR ONE.                                          *
      *                                                                *
      * A CALLER MAY ALSO FORCE AN ABEND ON A LOWER SEVERITY BY        *
      * SETTING ER-ABEND-REQUESTED TO Y.                               *
      *                                                                *
      * CALLED BY   - EVERY CARDSVC ONLINE PROGRAM  LINK               *
      * CALLS       - NOTHING                                          *
      * MAPSET      - CARDSET   MAP CARDERR                            *
      * TDQ         - CERR      OPERATIONS ERROR LOG                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD91.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD91 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDSET '.
       01  WS-MAP-ERR                  PIC X(8)  VALUE 'CARDERR '.
       01  WS-ERROR-TDQ                PIC X(4)  VALUE 'CERR'.
       01  WS-MENU-TRAN                PIC X(4)  VALUE 'CA00'.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-ABEND-CODE               PIC X(4)  VALUE 'AECD'.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-DISP                PIC X(10) VALUE SPACES.
       01  WS-TIME-DISP                PIC X(8)  VALUE SPACES.
       01  WS-STAMP                    PIC X(26) VALUE SPACES.
      *
       01  WS-EDIT-SQLCODE             PIC -9(9).
       01  WS-EDIT-RESP                PIC -9(9).
      *
       01  WS-SEND-SW                  PIC X     VALUE 'N'.
           88  WS-SEND-SCREEN                    VALUE 'Y'.
       01  WS-ABEND-SW                 PIC X     VALUE 'N'.
           88  WS-ABEND-WANTED                   VALUE 'Y'.
       01  WS-RETURN-MENU-SW           PIC X     VALUE 'N'.
           88  WS-RETURN-MENU                    VALUE 'Y'.
      *
      *    ---------------------------------------------------------
      *    THE OPERATIONS LOG LINE.  ONE LINE PER FAILURE, READ BY
      *    THE SHIFT REPORT CBREF08J AND BY THE ON CALL ANALYST.
      *    ---------------------------------------------------------
       01  LOG-LINE.
           05  LG-STAMP                PIC X(19).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-SEVERITY             PIC X.
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-TYPE                 PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-PGM                  PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-PARAGRAPH            PIC X(30).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-TRAN                 PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-TERM                 PIC X(4).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-TASK                 PIC 9(7).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-MESSAGE              PIC X(78).
      *
       01  LOG-DETAIL-LINE.
           05  LD-STAMP                PIC X(19).
           05  FILLER                  PIC X(2)  VALUE ' *'.
           05  LD-TEXT                 PIC X(140) VALUE SPACES.
      *
       01  WS-SQL-TEXT.
           05  FILLER                  PIC X(9)  VALUE 'SQLCODE= '.
           05  WS-LD-SQLCODE           PIC -9(9).
           05  FILLER                  PIC X(11) VALUE ' SQLSTATE= '.
           05  WS-LD-SQLSTATE          PIC X(5).
           05  FILLER                  PIC X(8)  VALUE ' TABLE= '.
           05  WS-LD-TABLE             PIC X(18).
           05  FILLER                  PIC X(6)  VALUE ' OPER='.
           05  WS-LD-OPER              PIC X(8).
      *
       01  WS-VSAM-TEXT.
           05  FILLER                  PIC X(7)  VALUE 'FILE=  '.
           05  WS-LD-FILE              PIC X(8).
           05  FILLER                  PIC X(9)  VALUE ' STATUS= '.
           05  WS-LD-STATUS            PIC X(2).
           05  FILLER                  PIC X(6)  VALUE ' KEY= '.
           05  WS-LD-KEY               PIC X(32).
      *
       01  WS-CICS-TEXT.
           05  FILLER                  PIC X(9)  VALUE 'EIBRESP= '.
           05  WS-LD-RESP              PIC -9(9).
           05  FILLER                  PIC X(11) VALUE ' EIBRESP2= '.
           05  WS-LD-RESP2             PIC -9(9).
           05  FILLER                  PIC X(8)  VALUE ' EIBFN= '.
           05  WS-LD-EIBFN             PIC X(2).
      *
       01  WS-LOG-LEN                  PIC S9(4) COMP VALUE 175.
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
           COPY CARDSET.
      *
       LINKAGE SECTION.
      *    THE COMMAREA IS AN ERROR-AREA.  IT IS COPIED INTO WORKING
      *    STORAGE RATHER THAN ADDRESSED IN PLACE BECAUSE A FATAL
      *    CALL ABENDS THE TASK AND THE CALLER'S STORAGE GOES WITH IT.
       01  DFHCOMMAREA                 PIC X(275).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INIT
      *
           IF EIBCALEN = ZERO
               PERFORM 0900-NO-COMMAREA
               GO TO 0000-RETURN
           END-IF
      *
           MOVE SPACES                 TO ERROR-AREA
           IF EIBCALEN < LENGTH OF ERROR-AREA
      *        AN OLDER CALLER WITH A SHORT AREA - TAKE WHAT THERE IS
               MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO ERROR-AREA(1:EIBCALEN)
           ELSE
               MOVE DFHCOMMAREA        TO ERROR-AREA
           END-IF
      *
           PERFORM 1000-CLASSIFY
           PERFORM 2000-WRITE-LOG
      *
           IF WS-SEND-SCREEN
               PERFORM 3000-SEND-DIAGNOSTIC
           END-IF
      *
           IF WS-ABEND-WANTED
               PERFORM 4000-ABEND-TASK
           END-IF
      *
           IF WS-RETURN-MENU
               PERFORM 5000-RETURN-TO-MENU
           END-IF
           .
       0000-RETURN.
      *    A WARNING OR AN INFORMATION CALL COMES BACK HERE AND THE
      *    CALLER CARRIES ON WHERE IT LEFT OFF
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           MOVE 'N'                    TO WS-SEND-SW
                                          WS-ABEND-SW
                                          WS-RETURN-MENU-SW
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
               MOVE SPACES             TO WS-DATE-DISP
                                          WS-TIME-DISP
           END-IF
      *
           MOVE SPACES                 TO WS-STAMP
           STRING WS-DATE-DISP         DELIMITED BY SIZE
                  '-'                  DELIMITED BY SIZE
                  WS-TIME-DISP         DELIMITED BY SIZE
                  INTO WS-STAMP
           END-STRING
           .
      *
      ******************************************************************
      * 1000 - WHAT KIND OF FAILURE IS THIS                            *
      ******************************************************************
       1000-CLASSIFY.
           IF ER-PGM-NAME = SPACES OR LOW-VALUES
               MOVE 'UNKNOWN '         TO ER-PGM-NAME
           END-IF
           IF ER-ERROR-TYPE = SPACES OR LOW-VALUES
               MOVE 'CICS'             TO ER-ERROR-TYPE
           END-IF
           IF ER-MESSAGE = SPACES OR LOW-VALUES
               MOVE 'NO DIAGNOSTIC TEXT SUPPLIED BY THE CALLER'
                                       TO ER-MESSAGE
           END-IF
      *
           MOVE WS-STAMP               TO ER-TIMESTAMP
      *
           EVALUATE TRUE
               WHEN ER-SEV-INFO
                   CONTINUE
               WHEN ER-SEV-WARNING
                   CONTINUE
               WHEN ER-SEV-ERROR
                   MOVE 'Y'            TO WS-SEND-SW
                   MOVE 'Y'            TO WS-RETURN-MENU-SW
               WHEN ER-SEV-FATAL
                   MOVE 'Y'            TO WS-SEND-SW
                   MOVE 'Y'            TO WS-ABEND-SW
               WHEN OTHER
      *            A SEVERITY THAT IS NOT ONE OF THE FOUR IS TREATED
      *            AS AN ERROR - IT IS SAFER TO STOP THAN TO GUESS
                   MOVE 'E'            TO ER-SEVERITY
                   MOVE 'Y'            TO WS-SEND-SW
                   MOVE 'Y'            TO WS-RETURN-MENU-SW
           END-EVALUATE
      *
           IF ER-ABEND-REQUESTED = 'Y'
               MOVE 'Y'                TO WS-SEND-SW
               MOVE 'Y'                TO WS-ABEND-SW
               MOVE 'N'                TO WS-RETURN-MENU-SW
           END-IF
      *
           IF ER-ABEND-CODE NOT = SPACES AND ER-ABEND-CODE NOT =
              LOW-VALUES
               MOVE ER-ABEND-CODE      TO WS-ABEND-CODE
           ELSE
               MOVE WS-ABEND-CODE      TO ER-ABEND-CODE
           END-IF
           .
      *
      ******************************************************************
      * 2000 - THE OPERATIONS LOG                                      *
      *                                                                *
      * THE HEADER LINE ALWAYS GOES OUT.  A SECOND LINE CARRIES THE    *
      * DETAIL FOR WHICHEVER KIND OF FAILURE IT WAS - THE ANALYST      *
      * READS THE PAIR TOGETHER.                                       *
      ******************************************************************
       2000-WRITE-LOG.
           MOVE WS-STAMP(1:19)         TO LG-STAMP
                                          LD-STAMP
           MOVE ER-SEVERITY            TO LG-SEVERITY
           MOVE ER-ERROR-TYPE          TO LG-TYPE
           MOVE ER-PGM-NAME            TO LG-PGM
           MOVE ER-PARAGRAPH           TO LG-PARAGRAPH
           MOVE ER-TRAN-ID             TO LG-TRAN
           MOVE ER-TERM-ID             TO LG-TERM
           MOVE EIBTASKN               TO LG-TASK
           MOVE ER-MESSAGE             TO LG-MESSAGE
      *
           MOVE 175                    TO WS-LOG-LEN
      *
           EXEC CICS WRITEQ TD
                     QUEUE(WS-ERROR-TDQ)
                     FROM(LOG-LINE)
                     LENGTH(WS-LOG-LEN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
      *        THERE IS NOWHERE LEFT TO REPORT A FAILURE OF THE
      *        REPORTING QUEUE ITSELF.  THE SCREEN STILL GOES OUT.
               CONTINUE
           END-IF
      *
           PERFORM 2100-WRITE-DETAIL
           .
      *
       2100-WRITE-DETAIL.
           MOVE SPACES                 TO LD-TEXT
      *
           EVALUATE TRUE
               WHEN ER-TYPE-SQL
                   MOVE ER-SQLCODE     TO WS-LD-SQLCODE
                   MOVE ER-SQLSTATE    TO WS-LD-SQLSTATE
                   MOVE ER-SQL-TABLE   TO WS-LD-TABLE
                   MOVE ER-SQL-OPERATION
                                       TO WS-LD-OPER
                   MOVE WS-SQL-TEXT    TO LD-TEXT
               WHEN ER-TYPE-VSAM
                   MOVE ER-FILE-NAME   TO WS-LD-FILE
                   MOVE ER-FILE-STATUS TO WS-LD-STATUS
                   MOVE ER-VSAM-KEY    TO WS-LD-KEY
                   MOVE WS-VSAM-TEXT   TO LD-TEXT
               WHEN OTHER
                   MOVE ER-EIBRESP     TO WS-LD-RESP
                   MOVE ER-EIBRESP2    TO WS-LD-RESP2
                   MOVE ER-EIBFN       TO WS-LD-EIBFN
                   MOVE WS-CICS-TEXT   TO LD-TEXT
           END-EVALUATE
      *
           MOVE 161                    TO WS-LOG-LEN
      *
           EXEC CICS WRITEQ TD
                     QUEUE(WS-ERROR-TDQ)
                     FROM(LOG-DETAIL-LINE)
                     LENGTH(WS-LOG-LEN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               CONTINUE
           END-IF
           .
      *
      ******************************************************************
      * 3000 - THE DIAGNOSTIC SCREEN                                   *
      ******************************************************************
       3000-SEND-DIAGNOSTIC.
           MOVE LOW-VALUES             TO CARDERRO
      *
           MOVE WS-DATE-DISP           TO ERDATEO
           MOVE ER-PGM-NAME            TO ERPGMO
           MOVE ER-SEVERITY            TO ERSEVO
           MOVE ER-PARAGRAPH           TO ERPARAO
           MOVE ER-ERROR-TYPE          TO ERTYPEO
           MOVE ER-REASON-CD           TO ERRSNO
      *
           MOVE ER-SQLCODE             TO WS-EDIT-SQLCODE
           MOVE WS-EDIT-SQLCODE        TO ERSQLCO
           MOVE ER-SQLSTATE            TO ERSQLSO
           MOVE ER-SQL-TABLE           TO ERTABO
      *
           MOVE ER-EIBRESP             TO WS-EDIT-RESP
           MOVE WS-EDIT-RESP           TO ERRESPO
           MOVE ER-EIBRESP2            TO WS-EDIT-RESP
           MOVE WS-EDIT-RESP           TO ERRES2O
      *
           MOVE ER-FILE-NAME           TO ERFILEO
           MOVE ER-FILE-STATUS         TO ERFSTAO
           MOVE ER-VSAM-KEY            TO ERKEYO
      *
           MOVE ER-MESSAGE             TO ERMSGO
           MOVE ER-TIMESTAMP           TO ERTIMEO
           MOVE ER-TRAN-ID             TO ERTRANO
           MOVE ER-TERM-ID             TO ERTERMO
      *
           IF ER-SEV-FATAL
               MOVE 'TASK ENDED - QUOTE THIS SCREEN TO THE HELP DESK'
                                       TO ERINSTO
           ELSE
               MOVE 'PRESS ENTER TO RETURN TO THE CA00 MENU'
                                       TO ERINSTO
           END-IF
      *
           EXEC CICS SEND
                     MAP(WS-MAP-ERR)
                     MAPSET(WS-MAPSET)
                     FROM(CARDERRO)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
      *        THE TERMINAL IS GONE OR THE MAPSET IS NOT LOADED.
      *        THE LOG LINE IS ALREADY OUT, WHICH IS THE PART THAT
      *        MATTERS, SO A PLAIN TEXT MESSAGE IS TRIED INSTEAD.
               EXEC CICS SEND
                         TEXT('CARDSVC ERROR - SEE THE CERR QUEUE')
                         ERASE
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           .
      *
      ******************************************************************
      * 4000 - THE USER ABEND                                          *
      *                                                                *
      * CANCEL IS SPECIFIED SO NO HANDLE ABEND EXIT ANYWHERE UP THE    *
      * CHAIN CAN SWALLOW IT.  THE UNIT OF WORK IS BACKED OUT BY CICS. *
      ******************************************************************
       4000-ABEND-TASK.
           EXEC CICS ABEND
                     ABCODE(WS-ABEND-CODE)
                     CANCEL
                     RESP(WS-RESP)
           END-EXEC
      *
      *    CONTROL DOES NOT COME BACK.  IF IT SOMEHOW DOES, THE TASK
      *    IS ENDED THE ORDINARY WAY RATHER THAN LOOPING.
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           .
      *
      ******************************************************************
      * 5000 - BACK TO THE MENU                                        *
      *                                                                *
      * THE NEXT ATTENTION KEY STARTS CA00 AGAIN WITH NO COMMAREA, SO  *
      * CACRD00 SEES EIBCALEN ZERO AND PAINTS A FRESH MENU.  THE       *
      * CALLER DOES NOT GET CONTROL BACK.                              *
      ******************************************************************
       5000-RETURN-TO-MENU.
           EXEC CICS RETURN
                     TRANSID(WS-MENU-TRAN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
           .
      *
       0900-NO-COMMAREA.
           EXEC CICS SEND
                     TEXT('CACRD91 REQUIRES AN ERROR AREA COMMAREA')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           .
