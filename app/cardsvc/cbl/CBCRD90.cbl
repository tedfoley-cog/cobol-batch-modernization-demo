      ******************************************************************
      * CBCRD90 - BATCH PROGRAM DISPATCHER                             *
      *                                                                *
      * RESOLVES A LOGICAL ROUTE KEY TO A LOAD MODULE NAME FROM        *
      * CARDSVC.PGM_ROUTE AND PERFORMS A DYNAMIC CALL.                 *
      *                                                                *
      * THE RESOLVED MODULE IS CANCELLED AFTER EACH CALL SO THAT A     *
      * STEP RE-DRIVEN AFTER A ROUTE CHANGE PICKS UP THE NEW TARGET.   *
      * COMPILE WITH DYNAM.                                            *
      *                                                                *
      * CALLED BY   - CBCRD05A, CBCRD06B, CBBIL04, CBREF04             *
      * PARAMETERS  - ROUTE-REQUEST                                    *
      *             - CALLER PARAMETER AREA PASSED THROUGH UNCHANGED   *
      *             - RETURN AREA                                      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBCRD90.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PGMROUT-FILE ASSIGN TO PGMROUT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS PR-KEY
                  FILE STATUS  IS WS-PGMROUT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PGMROUT-FILE
           RECORD CONTAINS 97 CHARACTERS.
       01  PGMROUT-REC.
           05  PR-KEY.
               10  PR-ROUTE-TYPE       PIC X(4).
               10  PR-ROUTE-KEY        PIC X(8).
               10  PR-SEQ-NBR          PIC 9(4).
           05  PR-REST                 PIC X(81).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-NAME                 PIC X(8)  VALUE SPACES.
       01  WS-PGMROUT-STATUS           PIC X(2)  VALUE '00'.
       01  WS-SUB                      PIC S9(4) COMP VALUE 0.
       01  WS-FOUND-SW                 PIC X     VALUE 'N'.
           88  WS-FOUND                          VALUE 'Y'.
       01  WS-EOF-SW                   PIC X     VALUE 'N'.
           88  WS-EOF                            VALUE 'Y'.
       01  WS-JOB-NAME                 PIC X(8)  VALUE SPACES.
       01  WS-CALL-COUNT               PIC 9(9)  VALUE ZERO.
       01  WS-CORREL-ID                PIC X(16) VALUE SPACES.
      *
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
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
           EXEC SQL DECLARE BROUTCSR CURSOR FOR
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
       01  LK-ROUTE-REQUEST            PIC X(38).
       01  LK-PARM-AREA                PIC X(512).
       01  LK-RETURN-AREA.
           05  LK-RETURN-CD            PIC S9(4) COMP.
           05  LK-RETURN-PGM           PIC X(8).
           05  LK-RETURN-MSG           PIC X(60).
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-ROUTE-REQUEST
                                LK-PARM-AREA
                                LK-RETURN-AREA.
      *
       0000-MAIN-LINE.
           MOVE LK-ROUTE-REQUEST       TO ROUTE-REQUEST
           MOVE ZERO                   TO RQ-RC
           MOVE 'N'                    TO RQ-USED-FALLBACK
           MOVE ZERO                   TO LK-RETURN-CD
           IF WS-CORREL-ID = SPACES
               MOVE RQ-ROUTE-KEY       TO WS-CORREL-ID(1:8)
               MOVE RQ-ROUTE-TYPE      TO WS-CORREL-ID(9:4)
           END-IF
      *
           PERFORM 1000-LOAD-ROUTES
           IF RQ-RC-TABLE-ERROR
               MOVE 12                 TO LK-RETURN-CD
               MOVE 'ROUTE TABLE UNAVAILABLE'
                                       TO LK-RETURN-MSG
               GO TO 0000-EXIT
           END-IF
      *
           PERFORM 2000-RESOLVE-ROUTE
           IF NOT WS-FOUND
               MOVE 8                  TO LK-RETURN-CD
               MOVE 'ROUTE NOT FOUND'  TO LK-RETURN-MSG
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
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - LOAD ROUTE CACHE.  DB2 FIRST, VSAM PGMROUT AS FALLBACK. *
      *        THE CACHE PERSISTS FOR THE LIFE OF THE RUN UNIT.        *
      ******************************************************************
       1000-LOAD-ROUTES.
           IF RC-LOADED
               GO TO 1000-EXIT
           END-IF
      *
           PERFORM 1100-READ-DB2
           IF RC-LOADED
               GO TO 1000-EXIT
           END-IF
      *
           PERFORM 1200-READ-VSAM
           IF NOT RC-LOADED
               MOVE 0012               TO RQ-RC
           END-IF
           .
       1000-EXIT.
           EXIT
           .
      *
       1100-READ-DB2.
           MOVE ZERO                   TO RC-ENTRY-CNT
      *
           EXEC SQL OPEN BROUTCSR END-EXEC
           IF SQLCODE NOT = 0
               PERFORM 8000-SQL-ERROR
               GO TO 1100-EXIT
           END-IF
      *
           PERFORM UNTIL SQLCODE = 100
                      OR RC-ENTRY-CNT >= 200
               EXEC SQL
                   FETCH BROUTCSR
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
           EXEC SQL CLOSE BROUTCSR END-EXEC
      *
           IF RC-ENTRY-CNT > ZERO
               MOVE 'Y'                TO RC-LOADED-FLG
               MOVE 'D'                TO RC-SOURCE
           END-IF
           .
       1100-EXIT.
           EXIT
           .
      *
       1200-READ-VSAM.
           MOVE ZERO                   TO RC-ENTRY-CNT
           MOVE 'N'                    TO WS-EOF-SW
      *
           OPEN INPUT PGMROUT-FILE
           IF WS-PGMROUT-STATUS NOT = '00'
               MOVE 'VSAM'             TO ER-ERROR-TYPE
               MOVE 'PGMROUT '         TO ER-FILE-NAME
               MOVE WS-PGMROUT-STATUS  TO ER-FILE-STATUS
               GO TO 1200-EXIT
           END-IF
      *
           PERFORM UNTIL WS-EOF
                      OR RC-ENTRY-CNT >= 200
               READ PGMROUT-FILE INTO ROUTE-RECORD
                   AT END
                       MOVE 'Y'        TO WS-EOF-SW
                   NOT AT END
                       IF RT-ACTIVE
                           ADD 1       TO RC-ENTRY-CNT
                           MOVE RT-ROUTE-TYPE
                                       TO RC-ROUTE-TYPE(RC-ENTRY-CNT)
                           MOVE RT-ROUTE-KEY
                                       TO RC-ROUTE-KEY(RC-ENTRY-CNT)
                           MOVE RT-SEQ-NBR
                                       TO RC-SEQ-NBR(RC-ENTRY-CNT)
                           MOVE RT-PGM-NAME
                                       TO RC-PGM-NAME(RC-ENTRY-CNT)
                           MOVE RT-CALL-TYPE
                                       TO RC-CALL-TYPE(RC-ENTRY-CNT)
                           MOVE RT-MODULE-ID
                                       TO RC-MODULE-ID(RC-ENTRY-CNT)
                           MOVE RT-FALLBACK-PGM
                                       TO RC-FALLBACK-PGM(RC-ENTRY-CNT)
                       END-IF
               END-READ
           END-PERFORM
      *
           CLOSE PGMROUT-FILE
      *
           IF RC-ENTRY-CNT > ZERO
               MOVE 'Y'                TO RC-LOADED-FLG
               MOVE 'V'                TO RC-SOURCE
           END-IF
           .
       1200-EXIT.
           EXIT
           .
      *
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
      * 3000 - DYNAMIC CALL                                            *
      *                                                                *
      * THE TARGET MAY BELONG TO EITHER MODULE.  PARTYRSK MODULES ARE  *
      * RESOLVED FROM THE PARTYRSK LOAD LIBRARY CONCATENATED INTO      *
      * STEPLIB BY THE JOB.                                            *
      ******************************************************************
       3000-DISPATCH.
           MOVE RQ-RESOLVED-PGM        TO WS-PGM-NAME
      *
           CALL WS-PGM-NAME USING LK-PARM-AREA
                                  LK-RETURN-AREA
               ON EXCEPTION
                   PERFORM 3100-TRY-FALLBACK
           END-CALL
      *
           ADD 1                       TO WS-CALL-COUNT
      *
      *    CANCEL SO A CHANGED ROUTE TAKES EFFECT ON RE-DRIVE
           CANCEL WS-PGM-NAME
      *
           MOVE LK-RETURN-CD           TO RQ-RC
           .
      *
       3100-TRY-FALLBACK.
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
      *
           IF WS-PGM-NAME = SPACES
               MOVE 12                 TO LK-RETURN-CD
               MOVE 'TARGET NOT LOADABLE AND NO FALLBACK'
                                       TO LK-RETURN-MSG
               PERFORM 9000-ROUTE-FAILURE
           ELSE
               CALL WS-PGM-NAME USING LK-PARM-AREA
                                      LK-RETURN-AREA
                   ON EXCEPTION
                       MOVE 12         TO LK-RETURN-CD
                       MOVE 'FALLBACK TARGET NOT LOADABLE'
                                       TO LK-RETURN-MSG
                       PERFORM 9000-ROUTE-FAILURE
               END-CALL
               CANCEL WS-PGM-NAME
           END-IF
           .
      *
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
                    , JOB_NAME)
               VALUES (:WS-CORREL-ID
                    , 'CBCRD90 '
                    , :RQ-ROUTE-TYPE
                    , :RQ-ROUTE-KEY
                    , :RQ-SEQ-NBR
                    , :RQ-RESOLVED-PGM
                    , :RQ-RESOLVED-MOD
                    , :RQ-RESOLVED-CALL
                    , :RQ-USED-FALLBACK
                    , :RC-SOURCE
                    , :RQ-RC
                    , :WS-JOB-NAME)
           END-EXEC
      *
           IF SQLCODE NOT = 0
               CONTINUE
           END-IF
           .
      *
       8000-SQL-ERROR.
           MOVE 'CBCRD90 '             TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE 'PGM_ROUTE        '    TO ER-SQL-TABLE
           MOVE 'E'                    TO ER-SEVERITY
           .
      *
       9000-ROUTE-FAILURE.
           MOVE 'CBCRD90 '             TO ER-PGM-NAME
           MOVE 'ROUT'                 TO ER-ERROR-TYPE
           MOVE RQ-ROUTE-TYPE          TO ER-REASON-CD
           MOVE 'F'                    TO ER-SEVERITY
           MOVE RQ-RESOLVED-PGM        TO LK-RETURN-PGM
      *
           DISPLAY 'CBCRD90 ROUTE FAILURE TYPE=' RQ-ROUTE-TYPE
                   ' KEY=' RQ-ROUTE-KEY
                   ' PGM=' RQ-RESOLVED-PGM
           .
