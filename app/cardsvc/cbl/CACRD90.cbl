      ******************************************************************
      * CACRD90 - ONLINE PROGRAM DISPATCHER                            *
      *                                                                *
      * RESOLVES A LOGICAL ROUTE KEY TO A LOAD MODULE NAME USING THE   *
      * CARDSVC.PGM_ROUTE CONTROL TABLE AND ISSUES THE LINK OR XCTL.   *
      *                                                                *
      * THE ROUTE TABLE IS READ ONCE PER TASK AND HELD IN A CICS       *
      * TEMPORARY STORAGE QUEUE SO REPEATED DISPATCHES IN THE SAME     *
      * PSEUDO CONVERSATION DO NOT RE-READ DB2.                        *
      *                                                                *
      * IF DB2 IS UNAVAILABLE THE VSAM PGMROUT FILE IS USED INSTEAD.   *
      *                                                                *
      * CALLED BY   - CACRD00 AND ANY PROGRAM NEEDING A ROUTED TARGET  *
      * PARAMETERS  - ROUTE-REQUEST FOLLOWED BY THE CALLERS COMMAREA   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD90.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-NAME                 PIC X(8)  VALUE SPACES.
       01  WS-TSQ-NAME                 PIC X(8)  VALUE 'CARDRTQ '.
       01  WS-TSQ-ITEM                 PIC S9(4) COMP VALUE 1.
       01  WS-TSQ-LENGTH               PIC S9(4) COMP VALUE 0.
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-SUB                      PIC S9(4) COMP VALUE 0.
       01  WS-FOUND-SW                 PIC X     VALUE 'N'.
           88  WS-FOUND                          VALUE 'Y'.
       01  WS-RETRY-SW                 PIC X     VALUE 'N'.
           88  WS-RETRY-DONE                     VALUE 'Y'.
       01  WS-CURRENT-DATE             PIC X(10) VALUE SPACES.
       01  WS-CORREL-ID                PIC X(16) VALUE SPACES.
      *
       01  WS-VSAM-KEY.
           05  WS-VK-ROUTE-TYPE        PIC X(4).
           05  WS-VK-ROUTE-KEY         PIC X(8).
           05  WS-VK-SEQ-NBR           PIC 9(4).
      *
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      ******************************************************************
      * DB2 CURSOR OVER THE ACTIVE ROUTE SET                           *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL DECLARE CARDSVC.PGM_ROUTE TABLE
              (ROUTE_TYPE    CHAR(4)       NOT NULL,
               ROUTE_KEY     CHAR(8)       NOT NULL,
               SEQ_NBR       SMALLINT      NOT NULL,
               PGM_NAME      CHAR(8)       NOT NULL,
               CALL_TYPE     CHAR(1)       NOT NULL,
               MODULE_ID     CHAR(8)       NOT NULL,
               EFF_DATE      DATE          NOT NULL,
               EXP_DATE      DATE          NOT NULL,
               ACTIVE_FLG    CHAR(1)       NOT NULL,
               FALLBACK_PGM  CHAR(8),
               DESCRIPTION   CHAR(40),
               LAST_MAINT_BY CHAR(8),
               LAST_MAINT_TS TIMESTAMP     NOT NULL)
           END-EXEC.
      *
       01  DCL-ROUTE.
           05  DCL-ROUTE-TYPE          PIC X(4).
           05  DCL-ROUTE-KEY           PIC X(8).
           05  DCL-SEQ-NBR             PIC S9(4) COMP.
           05  DCL-PGM-NAME            PIC X(8).
           05  DCL-CALL-TYPE           PIC X(1).
           05  DCL-MODULE-ID           PIC X(8).
           05  DCL-FALLBACK-PGM        PIC X(8).
      *
       01  IND-FALLBACK                PIC S9(4) COMP.
      *
           EXEC SQL DECLARE ROUTECSR CURSOR FOR
               SELECT ROUTE_TYPE
                    , ROUTE_KEY
                    , SEQ_NBR
                    , PGM_NAME
                    , CALL_TYPE
                    , MODULE_ID
                    , FALLBACK_PGM
                 FROM CARDSVC.PGM_ROUTE
                WHERE ACTIVE_FLG = 'Y'
                  AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE
                ORDER BY ROUTE_TYPE, ROUTE_KEY, SEQ_NBR
           END-EXEC.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  LK-ROUTE-REQUEST        PIC X(38).
           05  LK-CALLER-COMMAREA      PIC X(512).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           MOVE LK-ROUTE-REQUEST       TO ROUTE-REQUEST
           MOVE EIBTRNID               TO WS-CORREL-ID(1:4)
           MOVE EIBTASKN               TO WS-CORREL-ID(5:7)
           MOVE ZERO                   TO RQ-RC
           MOVE 'N'                    TO RQ-USED-FALLBACK
      *
           PERFORM 1000-LOAD-ROUTES
           IF RQ-RC-TABLE-ERROR
               PERFORM 9000-ROUTE-FAILURE
               GO TO 0000-EXIT
           END-IF
      *
           PERFORM 2000-RESOLVE-ROUTE
           IF NOT WS-FOUND
               MOVE 0008               TO RQ-RC
               PERFORM 9000-ROUTE-FAILURE
               GO TO 0000-EXIT
           END-IF
      *
           PERFORM 3000-DISPATCH
           PERFORM 4000-AUDIT-DISPATCH
      *
           MOVE ROUTE-REQUEST          TO LK-ROUTE-REQUEST
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - LOAD THE ROUTE CACHE                                    *
      *        TSQ FIRST, THEN DB2, THEN THE VSAM FALLBACK FILE.       *
      ******************************************************************
       1000-LOAD-ROUTES.
           IF RC-LOADED
               GO TO 1000-EXIT
           END-IF
      *
           PERFORM 1100-READ-TSQ
           IF RC-LOADED
               GO TO 1000-EXIT
           END-IF
      *
           PERFORM 1200-READ-DB2
           IF RC-LOADED
               PERFORM 1400-WRITE-TSQ
               GO TO 1000-EXIT
           END-IF
      *
           PERFORM 1300-READ-VSAM
           IF RC-LOADED
               PERFORM 1400-WRITE-TSQ
           ELSE
               MOVE 0012               TO RQ-RC
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
       1100-READ-TSQ.
           MOVE LENGTH OF ROUTE-CACHE  TO WS-TSQ-LENGTH
           EXEC CICS READQ TS
                     QUEUE(WS-TSQ-NAME)
                     INTO(ROUTE-CACHE)
                     LENGTH(WS-TSQ-LENGTH)
                     ITEM(WS-TSQ-ITEM)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'N'                TO RC-LOADED-FLG
               MOVE ZERO               TO RC-ENTRY-CNT
           END-IF
           .
      *
       1200-READ-DB2.
           MOVE ZERO                   TO RC-ENTRY-CNT
      *
           EXEC SQL OPEN ROUTECSR END-EXEC
           IF SQLCODE NOT = 0
               PERFORM 8000-SQL-ERROR
               GO TO 1200-EXIT
           END-IF
      *
           PERFORM UNTIL SQLCODE = 100
                      OR RC-ENTRY-CNT >= 200
               EXEC SQL
                   FETCH ROUTECSR
                    INTO :DCL-ROUTE-TYPE
                       , :DCL-ROUTE-KEY
                       , :DCL-SEQ-NBR
                       , :DCL-PGM-NAME
                       , :DCL-CALL-TYPE
                       , :DCL-MODULE-ID
                       , :DCL-FALLBACK-PGM :IND-FALLBACK
               END-EXEC
      *
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1           TO RC-ENTRY-CNT
                       MOVE DCL-ROUTE-TYPE
                                       TO RC-ROUTE-TYPE(RC-ENTRY-CNT)
                       MOVE DCL-ROUTE-KEY
                                       TO RC-ROUTE-KEY(RC-ENTRY-CNT)
                       MOVE DCL-SEQ-NBR
                                       TO RC-SEQ-NBR(RC-ENTRY-CNT)
                       MOVE DCL-PGM-NAME
                                       TO RC-PGM-NAME(RC-ENTRY-CNT)
                       MOVE DCL-CALL-TYPE
                                       TO RC-CALL-TYPE(RC-ENTRY-CNT)
                       MOVE DCL-MODULE-ID
                                       TO RC-MODULE-ID(RC-ENTRY-CNT)
                       IF IND-FALLBACK < 0
                           MOVE SPACES TO RC-FALLBACK-PGM(RC-ENTRY-CNT)
                       ELSE
                           MOVE DCL-FALLBACK-PGM
                                       TO RC-FALLBACK-PGM(RC-ENTRY-CNT)
                       END-IF
                   WHEN 100
                       CONTINUE
                   WHEN OTHER
                       PERFORM 8000-SQL-ERROR
                       EXIT PERFORM
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL CLOSE ROUTECSR END-EXEC
      *
           IF RC-ENTRY-CNT > ZERO
               MOVE 'Y'                TO RC-LOADED-FLG
               MOVE 'D'                TO RC-SOURCE
           END-IF
           .
       1200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 1300 - VSAM FALLBACK.  PGMROUT IS REBUILT WEEKLY BY CBREF03J   *
      *        FROM THE SAME DB2 ROWS.                                 *
      ******************************************************************
       1300-READ-VSAM.
           MOVE ZERO                   TO RC-ENTRY-CNT
           MOVE LOW-VALUES             TO WS-VSAM-KEY
      *
           EXEC CICS STARTBR
                     FILE('PGMROUT ')
                     RIDFLD(WS-VSAM-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'VSAM'             TO ER-ERROR-TYPE
               MOVE 'PGMROUT '         TO ER-FILE-NAME
               GO TO 1300-EXIT
           END-IF
      *
           PERFORM UNTIL WS-RESP NOT = DFHRESP(NORMAL)
                      OR RC-ENTRY-CNT >= 200
               EXEC CICS READNEXT
                         FILE('PGMROUT ')
                         INTO(ROUTE-RECORD)
                         RIDFLD(WS-VSAM-KEY)
                         RESP(WS-RESP)
               END-EXEC
      *
               IF WS-RESP = DFHRESP(NORMAL)
                   IF RT-ACTIVE
                       ADD 1           TO RC-ENTRY-CNT
                       MOVE RT-ROUTE-TYPE
                                       TO RC-ROUTE-TYPE(RC-ENTRY-CNT)
                       MOVE RT-ROUTE-KEY
                                       TO RC-ROUTE-KEY(RC-ENTRY-CNT)
                       MOVE RT-SEQ-NBR TO RC-SEQ-NBR(RC-ENTRY-CNT)
                       MOVE RT-PGM-NAME
                                       TO RC-PGM-NAME(RC-ENTRY-CNT)
                       MOVE RT-CALL-TYPE
                                       TO RC-CALL-TYPE(RC-ENTRY-CNT)
                       MOVE RT-MODULE-ID
                                       TO RC-MODULE-ID(RC-ENTRY-CNT)
                       MOVE RT-FALLBACK-PGM
                                       TO RC-FALLBACK-PGM(RC-ENTRY-CNT)
                   END-IF
               END-IF
           END-PERFORM
      *
           EXEC CICS ENDBR FILE('PGMROUT ') RESP(WS-RESP) END-EXEC
      *
           IF RC-ENTRY-CNT > ZERO
               MOVE 'Y'                TO RC-LOADED-FLG
               MOVE 'V'                TO RC-SOURCE
           END-IF
           .
       1300-EXIT.
           EXIT
           .
      *
       1400-WRITE-TSQ.
           MOVE LENGTH OF ROUTE-CACHE  TO WS-TSQ-LENGTH
           EXEC CICS WRITEQ TS
                     QUEUE(WS-TSQ-NAME)
                     FROM(ROUTE-CACHE)
                     LENGTH(WS-TSQ-LENGTH)
                     ITEM(WS-TSQ-ITEM)
                     MAIN
                     RESP(WS-RESP)
           END-EXEC
           .
      *
      ******************************************************************
      * 2000 - RESOLVE THE REQUESTED ROUTE FROM THE CACHE              *
      ******************************************************************
       2000-RESOLVE-ROUTE.
           MOVE 'N'                    TO WS-FOUND-SW
           MOVE SPACES                 TO RQ-RESOLVED-PGM
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > RC-ENTRY-CNT
                        OR WS-FOUND
               IF RC-ROUTE-TYPE(WS-SUB) = RQ-ROUTE-TYPE
              AND RC-ROUTE-KEY(WS-SUB)  = RQ-ROUTE-KEY
              AND RC-SEQ-NBR(WS-SUB)    = RQ-SEQ-NBR
                   MOVE 'Y'            TO WS-FOUND-SW
                   MOVE RC-PGM-NAME(WS-SUB)
                                       TO RQ-RESOLVED-PGM
                   MOVE RC-CALL-TYPE(WS-SUB)
                                       TO RQ-RESOLVED-CALL
                   MOVE RC-MODULE-ID(WS-SUB)
                                       TO RQ-RESOLVED-MOD
               END-IF
           END-PERFORM
           .
      *
      ******************************************************************
      * 3000 - DISPATCH                                                *
      *        PGMIDERR IS RETRIED ONCE AGAINST THE FALLBACK PROGRAM.  *
      ******************************************************************
       3000-DISPATCH.
           MOVE RQ-RESOLVED-PGM        TO WS-PGM-NAME
           MOVE 'N'                    TO WS-RETRY-SW
      *
           EVALUATE RQ-RESOLVED-CALL
               WHEN 'L'
                   PERFORM 3100-DO-LINK
               WHEN 'X'
                   PERFORM 3200-DO-XCTL
               WHEN OTHER
                   MOVE 0008           TO RQ-RC
                   MOVE 'ROUT'         TO ER-ERROR-TYPE
                   MOVE 'CALL TYPE NOT VALID FOR ONLINE DISPATCH'
                                       TO ER-MESSAGE
           END-EVALUATE
           .
      *
       3100-DO-LINK.
           EXEC CICS LINK
                     PROGRAM(WS-PGM-NAME)
                     COMMAREA(LK-CALLER-COMMAREA)
                     LENGTH(LENGTH OF LK-CALLER-COMMAREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE ZERO           TO RQ-RC
               WHEN DFHRESP(PGMIDERR)
                   IF NOT WS-RETRY-DONE
                       PERFORM 3300-SWITCH-TO-FALLBACK
                       IF WS-PGM-NAME NOT = SPACES
                           PERFORM 3100-DO-LINK
                       END-IF
                   ELSE
                       MOVE 0008       TO RQ-RC
                   END-IF
               WHEN OTHER
                   MOVE 0008           TO RQ-RC
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE WS-RESP2       TO ER-EIBRESP2
           END-EVALUATE
           .
      *
       3200-DO-XCTL.
           EXEC CICS XCTL
                     PROGRAM(WS-PGM-NAME)
                     COMMAREA(LK-CALLER-COMMAREA)
                     LENGTH(LENGTH OF LK-CALLER-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
      *
      *    CONTROL ONLY RETURNS HERE IF THE XCTL FAILED
           IF WS-RESP = DFHRESP(PGMIDERR)
               IF NOT WS-RETRY-DONE
                   PERFORM 3300-SWITCH-TO-FALLBACK
                   IF WS-PGM-NAME NOT = SPACES
                       PERFORM 3200-DO-XCTL
                   END-IF
               END-IF
           END-IF
           MOVE 0008                   TO RQ-RC
           .
      *
       3300-SWITCH-TO-FALLBACK.
           MOVE 'Y'                    TO WS-RETRY-SW
           MOVE 'Y'                    TO RQ-USED-FALLBACK
           MOVE SPACES                 TO WS-PGM-NAME
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > RC-ENTRY-CNT
               IF RC-ROUTE-TYPE(WS-SUB) = RQ-ROUTE-TYPE
              AND RC-ROUTE-KEY(WS-SUB)  = RQ-ROUTE-KEY
              AND RC-SEQ-NBR(WS-SUB)    = RQ-SEQ-NBR
                   MOVE RC-FALLBACK-PGM(WS-SUB)
                                       TO WS-PGM-NAME
               END-IF
           END-PERFORM
           .
      *
      ******************************************************************
      * 4000 - AUDIT WHAT WAS ACTUALLY DISPATCHED                      *
      ******************************************************************
       4000-AUDIT-DISPATCH.
           EXEC SQL
               INSERT INTO CARDSVC.ROUTE_AUDIT
                     (CORREL_ID
                    , CALLER_PGM
                    , ROUTE_TYPE
                    , ROUTE_KEY
                    , SEQ_NBR
                    , RESOLVED_PGM
                    , RESOLVED_MODULE
                    , CALL_TYPE
                    , USED_FALLBACK
                    , ROUTE_SOURCE
                    , RETURN_CD
                    , TRAN_ID)
               VALUES (:WS-CORREL-ID
                    , 'CACRD90 '
                    , :RQ-ROUTE-TYPE
                    , :RQ-ROUTE-KEY
                    , :RQ-SEQ-NBR
                    , :RQ-RESOLVED-PGM
                    , :RQ-RESOLVED-MOD
                    , :RQ-RESOLVED-CALL
                    , :RQ-USED-FALLBACK
                    , :RC-SOURCE
                    , :RQ-RC
                    , 'CA00')
           END-EXEC
      *
      *    AUDIT FAILURE MUST NOT FAIL THE TRANSACTION
           IF SQLCODE NOT = 0
               CONTINUE
           END-IF
           .
      *
      ******************************************************************
      * 8000 / 9000 - ERROR HANDLING                                   *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE 'CACRD90 '             TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE 'PGM_ROUTE        '    TO ER-SQL-TABLE
           MOVE 'E'                    TO ER-SEVERITY
           .
      *
       9000-ROUTE-FAILURE.
           MOVE 'CACRD90 '             TO ER-PGM-NAME
           MOVE 'ROUT'                 TO ER-ERROR-TYPE
           MOVE RQ-ROUTE-TYPE          TO ER-REASON-CD
           MOVE 'F'                    TO ER-SEVERITY
      *
           EXEC CICS LINK
                     PROGRAM('CACRD91 ')
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
