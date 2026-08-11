      ******************************************************************
      * CACRD05 - RISK ASSESSMENT STEP                                 *
      *                                                                *
      * SECOND PROGRAM OF THE AUTHORIZATION CHAIN.  BUILDS THE RISK    *
      * COMMUNICATION AREA CV-RISK-AREA AND ASKS THE DISPATCHER FOR    *
      * ROUTE XMOD / RISKSVC.  THE DISPATCHER ISSUES THE LINK, SO      *
      * CONTROL COMES BACK HERE WITH THE ANSWER IN PLACE.  THIS        *
      * PROGRAM DOES NOT KNOW - AND MUST NOT KNOW - WHICH LOAD MODULE  *
      * ANSWERS THE ROUTE.                                             *
      *                                                                *
      * RETURN CODE HANDLING                                           *
      *   0000  ANSWER ACCEPTED, CARRY ON TO THE STATUS CHECKS         *
      *   0004  PARTIAL ANSWER, SET THE REFERRAL WARNING AND CARRY ON  *
      *   0008  BUSINESS DECLINE - JUMP STRAIGHT TO THE DECISION STEP  *
      *   0012  FATAL - DIAGNOSTIC SCREEN THROUGH THE ERROR HANDLER    *
      *                                                                *
      * CALLED BY   - CACRD04  XCTL                                    *
      * CALLS       - CACRD90  LINK, ROUTE XMOD/RISKSVC                *
      *             - CACRD06  XCTL, NORMAL PATH                       *
      *             - CACRD09  XCTL, RISK DECLINE SHORT CUT            *
      *             - CACRD91  ERROR HANDLER                           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD05.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD05 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'CACRD06 '.
       01  WS-DECISION-PGM             PIC X(8)  VALUE 'CACRD09 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-YYYYMMDD            PIC 9(8)  VALUE ZERO.
       01  WS-TIME-HHMMSS              PIC 9(6)  VALUE ZERO.
      *
       01  WS-CORREL-ID.
           05  WS-CORREL-TRAN          PIC X(4)  VALUE SPACES.
           05  WS-CORREL-TERM          PIC X(4)  VALUE SPACES.
           05  WS-CORREL-TASK          PIC 9(7)  VALUE ZERO.
           05  WS-CORREL-FILL          PIC X     VALUE SPACE.
      *
       01  WS-TASK-NUM                 PIC 9(7)  VALUE ZERO.
      *
       01  WS-DISPATCH-AREA.
           05  WS-DA-ROUTE             PIC X(38).
           05  WS-DA-COMMAREA          PIC X(512).
      *
           COPY CVAUTHW1Y.
           COPY CVRISK01Y.
           COPY CVROUT01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           IF EIBCALEN = ZERO
               PERFORM 9100-NO-COMMAREA
               GO TO 0000-EXIT
           END-IF
      *
           MOVE DFHCOMMAREA            TO CA-WORK-AREA
           PERFORM 0100-INIT
      *
           PERFORM 1000-BUILD-RISK-AREA
           PERFORM 2000-DISPATCH-RISK
           PERFORM 3000-EVALUATE-ANSWER
           PERFORM 6000-CONTINUE-CHAIN
           .
       0000-EXIT.
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-YYYYMMDD)
                     TIME(WS-TIME-HHMMSS)
                     RESP(WS-RESP)
           END-EXEC
      *
           IF CAW-TRAIL-CNT < 8
               ADD 1                   TO CAW-TRAIL-CNT
               MOVE WS-PGM-ID          TO CAW-TRAIL-PGM(CAW-TRAIL-CNT)
               MOVE ZERO               TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 1000 - BUILD THE CROSS MODULE AREA                             *
      ******************************************************************
       1000-BUILD-RISK-AREA.
           INITIALIZE CV-RISK-AREA
           MOVE 0003                   TO CV-RISK-VERSION
           MOVE WS-PGM-ID              TO CV-RISK-CALLER-ID
           MOVE WS-MODULE-CARDSVC      TO CV-RISK-CALLER-MOD
      *
           MOVE EIBTRNID               TO WS-CORREL-TRAN
           MOVE EIBTRMID               TO WS-CORREL-TERM
           MOVE EIBTASKN               TO WS-TASK-NUM
           MOVE WS-TASK-NUM            TO WS-CORREL-TASK
           MOVE SPACE                  TO WS-CORREL-FILL
           MOVE WS-CORREL-ID           TO CV-RISK-CORREL-ID
      *
           MOVE WS-DATE-YYYYMMDD       TO CV-RISK-REQ-DATE
           MOVE WS-TIME-HHMMSS         TO CV-RISK-REQ-TIME
           SET CV-RISK-CHNL-ONLINE     TO TRUE
           SET CV-RISK-AUTH            TO TRUE
      *
           MOVE CAW-PARTY-ID           TO CV-RISK-PARTY-ID
           MOVE CAW-CUST-ID            TO CV-RISK-CUST-ID
           MOVE CAW-ACCT-ID            TO CV-RISK-ACCT-ID
           MOVE CAW-RQ-CARD-NUM        TO CV-RISK-CARD-NUM
           MOVE CAW-RQ-AMT             TO CV-RISK-REQ-AMT
           MOVE CAW-RQ-CURR            TO CV-RISK-REQ-CURR
           MOVE CAW-RQ-MCC             TO CV-RISK-MCC
           MOVE CAW-RQ-MERCH-ID        TO CV-RISK-MERCH-ID
           MOVE CAW-RQ-MERCH-CTRY      TO CV-RISK-COUNTRY
      *
           MOVE ZERO                   TO CV-RISK-RC
                                          CV-RISK-HOP-CNT
      *
      *    EVERY PROGRAM IN A CROSS MODULE CHAIN APPENDS ITSELF
           ADD 1                       TO CV-RISK-HOP-CNT
           MOVE WS-PGM-ID              TO CV-RISK-HOP-PGM
                                          (CV-RISK-HOP-CNT)
           MOVE CV-RISK-RC             TO CV-RISK-HOP-RC
                                          (CV-RISK-HOP-CNT)
           .
      *
      ******************************************************************
      * 2000 - ASK THE DISPATCHER FOR THE RISK SERVICE                 *
      *        THE CALLER COMMAREA ON THIS ROUTE IS CV-RISK-AREA, NOT  *
      *        THE CHAIN WORK AREA - THE TARGET IS OUTSIDE CARDSVC.    *
      ******************************************************************
       2000-DISPATCH-RISK.
           MOVE SPACES                 TO ROUTE-REQUEST
           MOVE 'XMOD'                 TO RQ-ROUTE-TYPE
           MOVE WS-ROUTE-RISKSVC       TO RQ-ROUTE-KEY
           MOVE 1                      TO RQ-SEQ-NBR
           MOVE ZERO                   TO RQ-RC
      *
           MOVE ROUTE-REQUEST          TO WS-DA-ROUTE
           MOVE CV-RISK-AREA           TO WS-DA-COMMAREA
      *
           EXEC CICS LINK
                     PROGRAM(WS-DISPATCHER-ONLINE)
                     COMMAREA(WS-DISPATCH-AREA)
                     LENGTH(LENGTH OF WS-DISPATCH-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '2000-DISPATCH-RISK'
                                       TO ER-PARAGRAPH
               MOVE 'RISK SERVICE DISPATCH FAILED'
                                       TO ER-MESSAGE
               MOVE 'ROUT'             TO ER-ERROR-TYPE
               PERFORM 8100-CICS-ERROR
      *        TREAT AS A FATAL RISK CONDITION
               MOVE WS-RC-FATAL        TO CV-RISK-RC
               GO TO 2000-EXIT
           END-IF
      *
           MOVE WS-DA-ROUTE            TO ROUTE-REQUEST
           MOVE WS-DA-COMMAREA         TO CV-RISK-AREA
      *
      *    A ROUTE THAT CANNOT BE RESOLVED AT ALL IS FATAL.  A ROUTE
      *    THAT RAN THE FALLBACK PROGRAM IS NOT - THE FALLBACK RETURNS
      *    A CONSERVATIVE ANSWER OF ITS OWN.
           IF RQ-RC NOT = ZERO
               MOVE '2000-DISPATCH-RISK'
                                       TO ER-PARAGRAPH
               MOVE 'ROUT'             TO ER-ERROR-TYPE
               MOVE 'NO ACTIVE ROUTE FOR THE RISK SERVICE'
                                       TO ER-MESSAGE
               PERFORM 8100-CICS-ERROR
               MOVE WS-RC-FATAL        TO CV-RISK-RC
           END-IF
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - COPY THE ANSWER AND ACT ON THE RETURN CODE              *
      ******************************************************************
       3000-EVALUATE-ANSWER.
           PERFORM 3100-COPY-ANSWER
      *
           IF CAW-TRAIL-CNT > ZERO
               MOVE CV-RISK-RC         TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
      *
           EVALUATE CV-RISK-RC
               WHEN 0000
                   PERFORM 3200-RC-OK
               WHEN 0004
                   PERFORM 3300-RC-WARNING
               WHEN 0008
                   PERFORM 3400-RC-DECLINE
               WHEN 0012
                   PERFORM 3500-RC-FATAL
               WHEN OTHER
      *            AN UNKNOWN CODE IS TREATED AS FATAL
                   PERFORM 3500-RC-FATAL
           END-EVALUATE
           .
      *
       3100-COPY-ANSWER.
           MOVE CV-RISK-SCORE          TO CAW-RSK-SCORE
           MOVE CV-RISK-BAND           TO CAW-RSK-BAND
           MOVE CV-RISK-KYC-STATUS     TO CAW-RSK-KYC
           MOVE CV-RISK-SANCTION-FLG   TO CAW-RSK-SANCTION
           MOVE CV-RISK-EXPOSURE-AMT   TO CAW-RSK-EXPOSURE
           MOVE CV-RISK-AVAIL-AMT      TO CAW-RSK-AVAIL
           MOVE CV-RISK-ADVICE-CD      TO CAW-RSK-ADVICE
           MOVE CV-RISK-REASON-CD      TO CAW-RSK-REASON
           MOVE CV-RISK-RC             TO CAW-RSK-RC
           MOVE CV-RISK-MODEL-ID       TO CAW-RSK-MODEL
      *
      *    A PARTY THE RISK SERVICE COULD NOT SCORE COMES BACK WITH A
      *    ZERO SCORE AND A BLANK BAND - TREAT IT AS THE WORST BAND SO
      *    THE DECISION STEP CANNOT READ IT AS A CLEAN LOW RISK.
           IF CAW-RSK-BAND = SPACE OR LOW-VALUE
               MOVE 'X'                TO CAW-RSK-BAND
           END-IF
      *
           IF CAW-RSK-ADVICE = SPACES OR LOW-VALUES
               MOVE 'REFR'             TO CAW-RSK-ADVICE
           END-IF
           .
      *
       3200-RC-OK.
           MOVE 'N'                    TO CAW-RSK-REFERRAL-SW
      *
      *    A SANCTIONS HIT IS NEVER SOFT, WHATEVER THE RETURN CODE
           IF CAW-RSK-SANCTIONED
               MOVE 'DECL'             TO CAW-RSK-ADVICE
               MOVE 'SANC'             TO CAW-RSK-REASON
               MOVE 'Y'                TO CAW-RSK-REFERRAL-SW
               MOVE WS-DECISION-PGM    TO WS-NEXT-PGM
           END-IF
           .
      *
       3300-RC-WARNING.
      *    PARTIAL ANSWER - THE CHAIN CONTINUES BUT THE DECISION STEP
      *    IS TOLD THE SCORE CANNOT BE FULLY TRUSTED.
           MOVE 'Y'                    TO CAW-RSK-REFERRAL-SW
      *
           IF CAW-RSK-REASON = SPACES
               MOVE 'RPAR'             TO CAW-RSK-REASON
           END-IF
      *
           IF CAW-RSK-ADVICE = 'APPR'
               MOVE 'REFR'             TO CAW-RSK-ADVICE
           END-IF
      *
           IF CAW-RSK-SANCTIONED
               MOVE 'DECL'             TO CAW-RSK-ADVICE
               MOVE 'SANC'             TO CAW-RSK-REASON
               MOVE WS-DECISION-PGM    TO WS-NEXT-PGM
           END-IF
           .
      *
       3400-RC-DECLINE.
      *    BUSINESS DECLINE FROM THE RISK SERVICE - SANCTIONS HIT,
      *    KYC FAILURE OR AN EXPOSURE BREACH.  THE REMAINING STATUS,
      *    FRAUD AND LIMIT STEPS CANNOT OVERTURN IT, SO THEY ARE
      *    SKIPPED AND THE DECISION STEP IS ENTERED DIRECTLY.
           MOVE 'DECL'                 TO CAW-RSK-ADVICE
           MOVE 'Y'                    TO CAW-RSK-REFERRAL-SW
      *
           EVALUATE TRUE
               WHEN CAW-RSK-SANCTIONED
                   MOVE 'SANC'         TO CAW-RSK-REASON
               WHEN CAW-RSK-KYC = 'FL'
                   MOVE 'KYCF'         TO CAW-RSK-REASON
               WHEN CAW-RSK-KYC = 'EX'
                   MOVE 'KYCX'         TO CAW-RSK-REASON
               WHEN CAW-RSK-REASON = SPACES
                   MOVE 'RISK'         TO CAW-RSK-REASON
           END-EVALUATE
      *
           MOVE 'F'                    TO CAW-STAT-RESULT
           MOVE CAW-RSK-REASON         TO CAW-STAT-REASON
           MOVE WS-DECISION-PGM        TO WS-NEXT-PGM
           .
      *
       3500-RC-FATAL.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE '3500-RC-FATAL'        TO ER-PARAGRAPH
           MOVE 'BUSN'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE CV-RISK-REASON-TXT(1:60)
                                       TO ER-MESSAGE(1:60)
           MOVE CV-RISK-SQLCODE        TO ER-SQLCODE
           MOVE 'RSKF'                 TO ER-REASON-CD
           MOVE 'Y'                    TO ER-ABEND-REQUESTED
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE EIBTRMID               TO ER-TERM-ID
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM-ONLINE)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
      *    THE ERROR HANDLER OWNS THE SCREEN FROM HERE
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           .
      *
      ******************************************************************
      * 6000 - PASS CONTROL ON                                         *
      ******************************************************************
       6000-CONTINUE-CHAIN.
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
           EXEC CICS XCTL
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           MOVE '6000-CONTINUE-CHAIN'  TO ER-PARAGRAPH
           PERFORM 8100-CICS-ERROR
           .
      *
      ******************************************************************
      * 8000 - ERROR HANDLING                                          *
      ******************************************************************
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           IF ER-ERROR-TYPE = SPACES
               MOVE 'CICS'             TO ER-ERROR-TYPE
           END-IF
           MOVE 'E'                    TO ER-SEVERITY
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
      *
       9100-NO-COMMAREA.
           EXEC CICS SEND
                     TEXT('CACRD05 IS A CHAIN STEP - START WITH CA00')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           .
