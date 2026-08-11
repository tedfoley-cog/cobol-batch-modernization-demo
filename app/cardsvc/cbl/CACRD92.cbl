      ******************************************************************
      * CACRD92 - CROSS MODULE ROUTE FALLBACK                          *
      *                                                                *
      * REGISTERED AS FALLBACK_PGM ON THE XMOD ROUTES IN               *
      * CARDSVC.PGM_ROUTE.  CACRD90 LINKS HERE WHEN THE RESOLVED       *
      * TARGET CANNOT BE LOADED - THE LOAD MODULE IS NOT IN THE        *
      * LIBRARY, THE PROGRAM IS DISABLED, OR THE OWNING REGION IS      *
      * DOWN AND THE DEFINITION HAS BEEN REMOVED.                      *
      *                                                                *
      * THE COMMAREA IS THE CALLER'S OWN AREA, WHICH ON AN XMOD ROUTE  *
      * IS CV-RISK-AREA.  THE ANSWER GIVEN BACK IS DELIBERATELY        *
      * CONSERVATIVE -                                                 *
      *                                                                *
      *   BAND     X     NOT SCORED                                    *
      *   ADVICE   REFR  SEND IT TO A HUMAN                            *
      *   TEXT     RISK SERVICE UNAVAILABLE                            *
      *   RC       0004  WARNING, NOT A FAILURE                        *
      *                                                                *
      * RC 4 MATTERS.  AN 8 WOULD MAKE EVERY AUTHORIZATION DECLINE     *
      * WHILE THE RISK SERVICE IS DOWN AND A 12 WOULD PUT AN ERROR     *
      * SCREEN IN FRONT OF THE OPERATOR.  A 4 LETS THE CHAIN CARRY ON  *
      * AND THE DECISION STEP TURN IT INTO A REFERRAL, WHICH IS WHAT   *
      * THE AUTHORISATION DESK IS THERE FOR.                           *
      *                                                                *
      * THE SANCTION FLAG IS LEFT AT N.  THAT IS A KNOWN AND ACCEPTED  *
      * GAP - NO SANCTION SCREENING HAPPENS WHILE THE SERVICE IS       *
      * DOWN, WHICH IS WHY THE OUTAGE IS ALSO WRITTEN TO THE           *
      * OPERATIONS QUEUE FOR THE COMPLIANCE RECORD.                    *
      *                                                                *
      * CALLED BY   - CACRD90  LINK, WHEN AN XMOD TARGET WILL NOT LOAD *
      * CALLS       - NOTHING.  IT MUST NOT FAIL, SO IT DOES NOT TOUCH *
      *               DB2 AND DOES NOT LINK THE ERROR HANDLER.         *
      * TDQ         - CERR     OPERATIONS ERROR LOG                    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD92.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD92 '.
       01  WS-ERROR-TDQ                PIC X(4)  VALUE 'CERR'.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-YYYYMMDD            PIC 9(8)  VALUE ZERO.
       01  WS-TIME-HHMMSS              PIC 9(6)  VALUE ZERO.
       01  WS-DATE-DISP                PIC X(10) VALUE SPACES.
       01  WS-TIME-DISP                PIC X(8)  VALUE SPACES.
      *
       01  WS-DEGRADED-SCORE           PIC 9(3)  VALUE 500.
      *
       01  LOG-LINE.
           05  LG-STAMP                PIC X(19).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-SEVERITY             PIC X     VALUE 'W'.
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-TYPE                 PIC X(4)  VALUE 'ROUT'.
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-PGM                  PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-CALLER               PIC X(8).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-CORREL               PIC X(16).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-CARD                 PIC X(16).
           05  FILLER                  PIC X     VALUE SPACE.
           05  LG-MESSAGE              PIC X(60).
      *
       01  WS-LOG-LEN                  PIC S9(4) COMP VALUE 138.
      *
           COPY CVCONSTY.
      *
       LINKAGE SECTION.
           COPY CVRISK01Y.
      *
      ******************************************************************
       PROCEDURE DIVISION USING CV-RISK-AREA.
      *
       0000-MAIN-LINE.
           IF EIBCALEN = ZERO
      *        NOTHING TO ANSWER INTO.  THE DISPATCHER ALWAYS PASSES
      *        THE CALLER AREA, SO THIS ONLY HAPPENS IF THE PROGRAM
      *        IS STARTED BY HAND FROM A TERMINAL.
               PERFORM 9000-NO-COMMAREA
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 0100-INIT
           PERFORM 1000-BUILD-DEGRADED-ANSWER
           PERFORM 2000-APPEND-HOP
           PERFORM 3000-LOG-OUTAGE
           .
       0000-RETURN.
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-YYYYMMDD)
                     TIME(WS-TIME-HHMMSS)
                     RESP(WS-RESP)
           END-EXEC
      *
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
           .
      *
      ******************************************************************
      * 1000 - THE DEGRADED ANSWER                                     *
      *                                                                *
      * ONLY THE OUTPUT AND STATUS SECTIONS ARE TOUCHED.  THE HEADER   *
      * AND THE INPUT STAY AS THE CALLER BUILT THEM SO THE REQUEST     *
      * CAN BE REPLAYED FROM THE LOG WHEN THE SERVICE IS BACK.         *
      ******************************************************************
       1000-BUILD-DEGRADED-ANSWER.
           MOVE WS-DEGRADED-SCORE      TO CV-RISK-SCORE
           MOVE 'X'                    TO CV-RISK-BAND
      *
      *    THE KYC STATUS IS NOT KNOWN.  PENDING IS THE HONEST ANSWER
      *    AND IT KEEPS THE DECISION STEP AWAY FROM AN APPROVAL.
           MOVE 'PN'                   TO CV-RISK-KYC-STATUS
           MOVE 'N'                    TO CV-RISK-SANCTION-FLG
      *
      *    NO EXPOSURE FIGURE IS AVAILABLE.  ZERO AVAILABLE IS SAFER
      *    THAN A STALE ONE - THE LIMIT STEP HAS ITS OWN FIGURES.
           MOVE ZERO                   TO CV-RISK-EXPOSURE-AMT
                                          CV-RISK-AVAIL-AMT
      *
           MOVE 'REFR'                 TO CV-RISK-ADVICE-CD
           MOVE 'RSVU'                 TO CV-RISK-REASON-CD
           MOVE WS-DATE-YYYYMMDD       TO CV-RISK-SCORE-DATE
           MOVE 'FALLBACK'             TO CV-RISK-MODEL-ID
      *
           MOVE WS-RC-WARNING          TO CV-RISK-RC
           MOVE 'RISK SERVICE UNAVAILABLE'
                                       TO CV-RISK-REASON-TXT
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE ZERO                   TO CV-RISK-SQLCODE
           .
      *
      ******************************************************************
      * 2000 - THE HOP TRACE                                           *
      *                                                                *
      * THE FALLBACK COUNTS AS A HOP.  WITHOUT IT THE TRACE WOULD SHOW *
      * THE CALLER TALKING TO NOTHING AND THE OUTAGE WOULD BE INVISIBLE*
      * TO ANYONE READING THE TRACE AFTERWARDS.                        *
      ******************************************************************
       2000-APPEND-HOP.
           IF CV-RISK-HOP-CNT < 8
               ADD 1                   TO CV-RISK-HOP-CNT
               MOVE WS-PGM-ID          TO CV-RISK-HOP-PGM
                                          (CV-RISK-HOP-CNT)
               MOVE CV-RISK-RC         TO CV-RISK-HOP-RC
                                          (CV-RISK-HOP-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 3000 - TELL OPERATIONS                                         *
      *                                                                *
      * WRITEQ TD IS THE ONLY OUTSIDE CALL THIS PROGRAM MAKES.  IF IT  *
      * FAILS THERE IS NOTHING SENSIBLE LEFT TO DO - THE DEGRADED      *
      * ANSWER IS STILL RETURNED, WHICH IS THE WHOLE POINT.            *
      ******************************************************************
       3000-LOG-OUTAGE.
           MOVE SPACES                 TO LOG-LINE
           MOVE 'W'                    TO LG-SEVERITY
           MOVE 'ROUT'                 TO LG-TYPE
           MOVE WS-PGM-ID              TO LG-PGM
           MOVE CV-RISK-CALLER-ID      TO LG-CALLER
           MOVE CV-RISK-CORREL-ID      TO LG-CORREL
           MOVE CV-RISK-CARD-NUM       TO LG-CARD
           MOVE 'XMOD TARGET NOT LOADED - DEGRADED ANSWER RETURNED'
                                       TO LG-MESSAGE
      *
           MOVE SPACES                 TO LG-STAMP
           STRING WS-DATE-DISP         DELIMITED BY SIZE
                  '-'                  DELIMITED BY SIZE
                  WS-TIME-DISP         DELIMITED BY SIZE
                  INTO LG-STAMP
           END-STRING
      *
      *    THE CARD NUMBER IS MASKED BEFORE IT REACHES THE LOG
           IF LG-CARD NOT = SPACES
               MOVE '******'           TO LG-CARD(7:6)
           END-IF
      *
           MOVE 138                    TO WS-LOG-LEN
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
      *        THE OUTAGE COULD NOT BE LOGGED.  THE RETURN CODE IS
      *        RAISED SO AT LEAST THE CALLER SEES SOMETHING WAS WRONG
      *        WITH THE LOGGING AS WELL AS THE SERVICE.
               MOVE 'RISK SERVICE UNAVAILABLE - NOT LOGGED'
                                       TO CV-RISK-REASON-TXT
           END-IF
           .
      *
       9000-NO-COMMAREA.
           EXEC CICS SEND
                     TEXT('CACRD92 IS A ROUTE FALLBACK - NOT A TRAN')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           .
