      ******************************************************************
      * CACRD00 - CA00 MAIN MENU                                       *
      *                                                                *
      * ENTRY PROGRAM FOR CICS TRANSACTION CA00.  PSEUDO               *
      * CONVERSATIONAL - EACH PASS ENDS WITH RETURN TRANSID('CA00').   *
      *                                                                *
      * THE MENU DOES NOT KNOW WHICH PROGRAM SERVICES AN OPTION.  THE  *
      * SELECTED OPTION IS TURNED INTO A ROUTE KEY (OPT01 - OPT10,     *
      * OPTX) AND CACRD90 IS LINKED TO RESOLVE AND TRANSFER.           *
      *                                                                *
      * CALLED BY   - CICS TRANSACTION CA00 (TERMINAL INPUT)           *
      *             - ANY CA00 SCREEN RETURNING ON PF3                 *
      * CALLS       - CACRD90  ROUTE DISPATCHER                        *
      *             - CACRD91  ERROR HANDLER                           *
      * MAPSET      - CARDSET   MAP CARDMNU                            *
      * COMMAREA    - CVAUTHW1Y  512 BYTES                             *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD00.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD00 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDSET '.
       01  WS-MAP                      PIC X(8)  VALUE 'CARDMNU '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-SEND-ERASE-SW            PIC X     VALUE 'Y'.
           88  WS-SEND-ERASE                     VALUE 'Y'.
      *
       01  WS-OPTION-N                 PIC 9(2)  VALUE ZERO.
       01  WS-OPTION-X                 PIC X(2)  VALUE SPACES.
       01  WS-VALID-SW                 PIC X     VALUE 'Y'.
           88  WS-VALID                          VALUE 'Y'.
           88  WS-NOT-VALID                      VALUE 'N'.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-OUT                 PIC X(8)  VALUE SPACES.
       01  WS-TIME-OUT                 PIC X(8)  VALUE SPACES.
      *
      *    OPTION TO ROUTE KEY TRANSLATION.  THE MENU HOLDS ROUTE KEYS
      *    ONLY - LOAD MODULE NAMES LIVE IN CARDSVC.PGM_ROUTE.
       01  WS-ROUTE-TABLE.
           05  FILLER                  PIC X(8)  VALUE 'OPT01   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT02   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT03   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT04   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT05   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT06   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT07   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT08   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT09   '.
           05  FILLER                  PIC X(8)  VALUE 'OPT10   '.
       01  WS-ROUTE-KEYS REDEFINES WS-ROUTE-TABLE.
           05  WS-ROUTE-ENTRY          PIC X(8) OCCURS 10 TIMES.
      *
       01  WS-MSG-INVALID-OPT          PIC X(60) VALUE
           'OPTION NOT VALID - ENTER 1 THROUGH 10 OR X TO EXIT'.
       01  WS-MSG-INVALID-KEY          PIC X(60) VALUE
           'KEY NOT ACTIVE ON THIS SCREEN - USE ENTER, PF3 OR PF12'.
       01  WS-MSG-NO-ACCT              PIC X(60) VALUE
           'ACCOUNT NUMBER IS REQUIRED FOR THE SELECTED OPTION'.
       01  WS-MSG-NO-CARD              PIC X(60) VALUE
           'CARD NUMBER IS REQUIRED FOR THE SELECTED OPTION'.
       01  WS-MSG-ROUTE-FAIL           PIC X(60) VALUE
           'REQUESTED FUNCTION IS NOT AVAILABLE - CALL SUPPORT'.
       01  WS-MSG-WELCOME              PIC X(60) VALUE
           'ENTER AN OPTION AND PRESS ENTER'.
      *
      *    ROUTE REQUEST FOLLOWED BY THE 512 BYTE CALLER COMMAREA.
      *    CACRD90 EXPECTS THE TWO CONTIGUOUS.
       01  WS-DISPATCH-AREA.
           05  WS-DA-ROUTE             PIC X(38).
           05  WS-DA-COMMAREA          PIC X(512).
      *
           COPY CVAUTHW1Y.
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      *    BMS SYMBOLIC MAP - GENERATED FROM CARDMAP BY DFHMSD DSECT
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
           IF EIBCALEN = ZERO
               PERFORM 1000-FIRST-PASS
               GO TO 0000-RETURN
           END-IF
      *
           MOVE DFHCOMMAREA            TO CA-WORK-AREA
           MOVE 'N'                    TO CAW-FIRST-PASS-SW
           PERFORM 0200-CAPTURE-EIB
      *
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2000-PROCESS-INPUT
               WHEN DFHPF3
                   PERFORM 4000-EXIT-REQUEST
               WHEN DFHPF12
                   PERFORM 4000-EXIT-REQUEST
               WHEN DFHCLEAR
                   PERFORM 4000-EXIT-REQUEST
               WHEN OTHER
                   MOVE WS-MSG-INVALID-KEY
                                       TO CAW-MSG
                   PERFORM 3000-SEND-MENU
           END-EVALUATE
           .
       0000-RETURN.
           EXEC CICS RETURN
                     TRANSID(WS-TRAN-CARD)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
           END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 0100 - HOUSEKEEPING                                            *
      ******************************************************************
       0100-INIT.
           MOVE SPACES                 TO ERROR-AREA
           MOVE LOW-VALUES             TO CARDMNUO
           MOVE 'Y'                    TO WS-VALID-SW
           MOVE ZERO                   TO WS-RESP
                                          WS-RESP2
      *
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT)
                     DATESEP('/')
                     TIME(WS-TIME-OUT)
                     TIMESEP(':')
                     RESP(WS-RESP)
           END-EXEC
           .
      *
       0200-CAPTURE-EIB.
           MOVE EIBTRNID               TO CAW-TRAN-ID
           MOVE EIBTRMID               TO CAW-TERM-ID
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
           IF CAW-OPER-ID = SPACES OR LOW-VALUES
               EXEC CICS ASSIGN
                         USERID(CAW-OPER-ID)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'UNKNOWN '     TO CAW-OPER-ID
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 1000 - FIRST PASS.  NO COMMAREA - BUILD ONE AND PAINT THE MENU *
      ******************************************************************
       1000-FIRST-PASS.
           MOVE LOW-VALUES             TO CA-WORK-AREA
           MOVE SPACES                 TO CAW-HDR
                                          CAW-CTX
                                          CAW-BROWSE
                                          CAW-AUTH-REQ
                                          CAW-RISK
                                          CAW-DECISION
           MOVE ZERO                   TO CAW-ACCT-ID
                                          CAW-CUST-ID
                                          CAW-TRAIL-CNT
                                          CAW-BR-PAGE-NBR
           MOVE 'Y'                    TO CAW-FIRST-PASS-SW
           MOVE 'CARDMNU '             TO CAW-SCREEN-ID
           PERFORM 0200-CAPTURE-EIB
      *
           MOVE WS-MSG-WELCOME         TO CAW-MSG
           MOVE 'Y'                    TO WS-SEND-ERASE-SW
           PERFORM 3000-SEND-MENU
           .
      *
      ******************************************************************
      * 2000 - RECEIVE AND EDIT THE MENU SELECTION                     *
      ******************************************************************
       2000-PROCESS-INPUT.
           EXEC CICS RECEIVE
                     MAP(WS-MAP)
                     MAPSET(WS-MAPSET)
                     INTO(CARDMNUI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE WS-MSG-WELCOME TO CAW-MSG
                   PERFORM 3000-SEND-MENU
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE '2000-PROCESS-INPUT'
                                       TO ER-PARAGRAPH
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE 'E'            TO ER-SEVERITY
                   PERFORM 8000-CICS-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           PERFORM 2100-EDIT-OPTION
           IF WS-NOT-VALID
               PERFORM 3000-SEND-MENU
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2200-CAPTURE-CONTEXT
           IF WS-NOT-VALID
               PERFORM 3000-SEND-MENU
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 5000-DISPATCH-OPTION
      *
      *    CONTROL REACHES HERE ONLY IF THE XCTL DID NOT HAPPEN
           MOVE WS-MSG-ROUTE-FAIL      TO CAW-MSG
           PERFORM 3000-SEND-MENU
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-EDIT-OPTION.
           MOVE 'Y'                    TO WS-VALID-SW
           MOVE SPACES                 TO CAW-ROUTE-KEY
      *
           IF MNOPTL = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-INVALID-OPT TO CAW-MSG
               GO TO 2100-EXIT
           END-IF
      *
           MOVE MNOPTI                 TO WS-OPTION-X
           INSPECT WS-OPTION-X REPLACING ALL LOW-VALUES BY SPACE
      *
           IF WS-OPTION-X = 'X ' OR 'x '
               MOVE 'X '               TO CAW-OPTION
               MOVE 'OPTX    '         TO CAW-ROUTE-KEY
               GO TO 2100-EXIT
           END-IF
      *
      *    A SINGLE DIGIT MAY BE KEYED IN EITHER POSITION
           IF WS-OPTION-X(2:1) = SPACE
               MOVE WS-OPTION-X(1:1)   TO WS-OPTION-X(2:1)
               MOVE '0'                TO WS-OPTION-X(1:1)
           END-IF
      *
           IF WS-OPTION-X IS NOT NUMERIC
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-INVALID-OPT TO CAW-MSG
               GO TO 2100-EXIT
           END-IF
      *
           MOVE WS-OPTION-X            TO WS-OPTION-N
           IF WS-OPTION-N < 1 OR WS-OPTION-N > 10
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-INVALID-OPT TO CAW-MSG
               GO TO 2100-EXIT
           END-IF
      *
           MOVE WS-OPTION-X            TO CAW-OPTION
           MOVE WS-ROUTE-ENTRY(WS-OPTION-N)
                                       TO CAW-ROUTE-KEY
           .
       2100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2200 - CARRY THE KEYED ACCOUNT AND CARD FORWARD.  OPTIONS THAT *
      *        NEED A KEY ARE REJECTED HERE RATHER THAN BY THE TARGET  *
      *        SCREEN.                                                 *
      ******************************************************************
       2200-CAPTURE-CONTEXT.
           IF MNACCTL > ZERO
               IF MNACCTI IS NUMERIC
                   MOVE MNACCTI        TO CAW-ACCT-ID
               ELSE
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'ACCOUNT NUMBER MUST BE NUMERIC'
                                       TO CAW-MSG
                   GO TO 2200-EXIT
               END-IF
           END-IF
      *
           IF MNCARDL > ZERO
               MOVE MNCARDI            TO CAW-CARD-NUM
           END-IF
      *
           EVALUATE CAW-OPTION
               WHEN '01'
                   IF CAW-ACCT-ID = ZERO
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE WS-MSG-NO-ACCT
                                       TO CAW-MSG
                   END-IF
               WHEN '02'
                   IF CAW-ACCT-ID = ZERO
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE WS-MSG-NO-ACCT
                                       TO CAW-MSG
                   END-IF
               WHEN '03'
                   IF CAW-CARD-NUM = SPACES
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE WS-MSG-NO-CARD
                                       TO CAW-MSG
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       2200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - PAINT THE MENU                                          *
      ******************************************************************
       3000-SEND-MENU.
           MOVE WS-DATE-OUT            TO MNDATEO
           MOVE WS-TIME-OUT            TO MNTIMEO
           MOVE CAW-MSG                TO MNMSGO
           MOVE CAW-OPER-ID            TO MNOPERO
           MOVE CAW-TERM-ID            TO MNTERMO
      *
           IF CAW-ACCT-ID > ZERO
               MOVE CAW-ACCT-ID        TO MNACCTO
           END-IF
           IF CAW-CARD-NUM NOT = SPACES
               MOVE CAW-CARD-NUM       TO MNCARDO
           END-IF
      *
           MOVE 'CARDMNU '             TO CAW-SCREEN-ID
      *
           IF WS-SEND-ERASE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDMNUO)
                         ERASE
                         CURSOR
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
               MOVE 'N'                TO WS-SEND-ERASE-SW
           ELSE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDMNUO)
                         DATAONLY
                         CURSOR
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
           END-IF
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '3000-SEND-MENU'   TO ER-PARAGRAPH
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE 'F'                TO ER-SEVERITY
               PERFORM 8000-CICS-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 4000 - PF3 / PF12 / CLEAR ALL END THE SESSION THROUGH THE      *
      *        EXIT ROUTE.  THE MENU DOES NOT KNOW THE EXIT PROGRAM.   *
      ******************************************************************
       4000-EXIT-REQUEST.
           MOVE 'X '                   TO CAW-OPTION
           MOVE 'OPTX    '             TO CAW-ROUTE-KEY
           MOVE 'SESSION ENDED FROM MENU'
                                       TO CAW-MSG
           PERFORM 5000-DISPATCH-OPTION
      *
           MOVE WS-MSG-ROUTE-FAIL      TO CAW-MSG
           PERFORM 3000-SEND-MENU
           .
      *
      ******************************************************************
      * 5000 - DISPATCH.  CACRD90 RESOLVES THE ROUTE KEY AND ISSUES    *
      *        THE XCTL, SO A NORMAL DISPATCH DOES NOT COME BACK.      *
      ******************************************************************
       5000-DISPATCH-OPTION.
           PERFORM 5100-TRAIL
      *
           MOVE SPACES                 TO ROUTE-REQUEST
           MOVE 'MENU'                 TO RQ-ROUTE-TYPE
           MOVE CAW-ROUTE-KEY          TO RQ-ROUTE-KEY
           MOVE 1                      TO RQ-SEQ-NBR
           MOVE ZERO                   TO RQ-RC
           MOVE 'N'                    TO RQ-USED-FALLBACK
      *
           MOVE ROUTE-REQUEST          TO WS-DA-ROUTE
           MOVE CA-WORK-AREA           TO WS-DA-COMMAREA
      *
           EXEC CICS LINK
                     PROGRAM(WS-DISPATCHER-ONLINE)
                     COMMAREA(WS-DISPATCH-AREA)
                     LENGTH(LENGTH OF WS-DISPATCH-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           MOVE WS-DA-ROUTE            TO ROUTE-REQUEST
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '5000-DISPATCH-OPTION'
                                       TO ER-PARAGRAPH
               MOVE 'ROUT'             TO ER-ERROR-TYPE
               MOVE 'E'                TO ER-SEVERITY
               MOVE CAW-ROUTE-KEY(1:4) TO ER-REASON-CD
               PERFORM 8000-CICS-ERROR
           END-IF
           .
      *
       5100-TRAIL.
           IF CAW-TRAIL-CNT < 8
               ADD 1                   TO CAW-TRAIL-CNT
               MOVE WS-PGM-ID          TO CAW-TRAIL-PGM(CAW-TRAIL-CNT)
               MOVE ZERO               TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 8000 - HAND THE CONDITION TO THE ONLINE ERROR HANDLER          *
      ******************************************************************
       8000-CICS-ERROR.
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
           .
