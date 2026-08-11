      ******************************************************************
      * PRBRSK1 - BATCH RISK RECALCULATION - ENTRY POINT               *
      *                                                                *
      * PARTY AND RISK MODULE.  DRIVES A FULL RECALCULATION OF ONE     *
      * PARTY - KYC AND SANCTIONS REFRESH, EXPOSURE REBUILD, RESCORE   *
      * AND PERSIST.                                                   *
      *                                                                *
      * ENTERED FROM BATCH ONLY.  THE COMMAREA MUST CARRY REQUEST      *
      * TYPE 'RCAL' AND CHANNEL 'B'.  ONLINE CALLERS ARE REJECTED      *
      * WITH RETURN CODE 12 - THE ONLINE ENTRY POINTS DO NOT COME      *
      * THROUGH HERE.                                                  *
      *                                                                *
      * THIS PROGRAM OWNS THE DB2 UNIT OF WORK FOR THE CHAIN.  NONE    *
      * OF THE CALLED PROGRAMS COMMIT.  WHEN DRIVEN IN BULK BY THE     *
      * WEEKLY RESCORE THE COMMIT IS TAKEN EVERY WS-COMMIT-FREQUENCY   *
      * PARTIES.  A SINGLE PARTY REQUEST COMMITS ON THE WAY OUT.       *
      *                                                                *
      * CALLED BY   - THE BATCH DISPATCHER, ROUTE XMOD / RSKRECAL      *
      *             - CBPRT03 (WEEKLY FULL PORTFOLIO RESCORE)          *
      * CALLS       - PRBRSK2                                          *
      * PARAMETERS  - CV-RISK-AREA      (CVRISK01Y, 512 BYTES)         *
      *             - LK-RETURN-AREA                                   *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - RECALCULATION COMPLETE                                  *
      *   04 - COMPLETE WITH WARNINGS, SEE CV-RISK-REASON-TXT          *
      *   08 - BUSINESS CONDITION.  SANCTIONS HIT OR KYC FAILURE.      *
      *        THE CALLING JOB MUST ROUTE THE PARTY TO MANUAL REVIEW.  *
      *        THIS IS NOT AN ABEND CONDITION.                         *
      *   12 - FATAL.  DATA OR RESOURCE FAILURE, NOTHING COMMITTED.    *
      *                                                                *
      * MAINTENANCE                                                    *
      *   1998-11-02 PARTY AND RISK   INITIAL VERSION                  *
      *   2004-06-14 PARTY AND RISK   COMMIT FREQUENCY FROM CVCONSTY   *
      *   2017-03-09 PARTY AND RISK   CR-4471 LIVE CARD EXPOSURE       *
      *   2021-08-30 PARTY AND RISK   COMMAREA VERSION 0003            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRBRSK1.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'PRBRSK1 '.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
      *
      *    RUN UNIT COUNTERS.  THESE SURVIVE BETWEEN CALLS BECAUSE THE
      *    DRIVER DOES NOT CANCEL US BETWEEN PARTIES.
       01  WS-RUN-COUNTERS.
           05  WS-PARTIES-DONE         PIC 9(9)  VALUE ZERO.
           05  WS-PARTIES-SINCE-CMT    PIC 9(9)  VALUE ZERO.
           05  WS-COMMIT-COUNT         PIC 9(9)  VALUE ZERO.
           05  WS-HIT-COUNT            PIC 9(9)  VALUE ZERO.
           05  WS-WARN-COUNT           PIC 9(9)  VALUE ZERO.
      *
       01  WS-SWITCHES.
           05  WS-FIRST-CALL-SW        PIC X     VALUE 'Y'.
               88  WS-FIRST-CALL                 VALUE 'Y'.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
           05  WS-PARTY-FOUND-SW       PIC X     VALUE 'N'.
               88  WS-PARTY-FOUND                VALUE 'Y'.
           05  WS-LINK-EOF-SW          PIC X     VALUE 'N'.
               88  WS-LINK-EOF                   VALUE 'Y'.
      *
       01  WS-WORK-FIELDS.
           05  WS-WORST-RC             PIC 9(4)  VALUE ZERO.
           05  WS-CHAIN-RC             PIC 9(4)  VALUE ZERO.
           05  WS-SUB                  PIC S9(4) COMP VALUE ZERO.
           05  WS-ABEND-CODE           PIC S9(4) COMP VALUE ZERO.
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
      *
       01  WS-CURRENT-DATE-AREA.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-DATE-R REDEFINES WS-CURR-DATE.
               10  WS-CURR-CC          PIC 9(2).
               10  WS-CURR-YY          PIC 9(2).
               10  WS-CURR-MM          PIC 9(2).
               10  WS-CURR-DD          PIC 9(2).
           05  WS-CURR-TIME            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME-R REDEFINES WS-CURR-TIME.
               10  WS-CURR-HHMMSS      PIC 9(6).
               10  WS-CURR-HUND        PIC 9(2).
      *
       01  WS-SQL-WORK.
           05  WS-SAVE-SQLCODE         PIC S9(9) COMP VALUE ZERO.
           05  WS-SQL-DISP             PIC -(9)9.
      *
      *    HOST VARIABLES - PARTYRSK.CUSTOMER
       01  DCL-CUSTOMER.
           05  DCL-CUST-ID             PIC S9(9)V9(0) COMP-3.
           05  DCL-PARTY-ID            PIC X(11).
           05  DCL-PARTY-TYPE          PIC X(1).
           05  DCL-LEGAL-NAME          PIC X(60).
           05  DCL-LAST-NAME           PIC X(25).
           05  DCL-DOB                 PIC X(10).
           05  DCL-NATIONAL-ID         PIC X(11).
           05  DCL-TAX-ID              PIC X(15).
           05  DCL-COUNTRY-CD          PIC X(3).
           05  DCL-DOMICILE-CTRY       PIC X(3).
           05  DCL-CITIZENSHIP         PIC X(3).
           05  DCL-PEP-FLG             PIC X(1).
           05  DCL-CUST-STATUS         PIC X(1).
           05  DCL-ONBOARD-DATE        PIC X(10).
           05  DCL-SEGMENT-CD          PIC X(4).
      *
       01  DCL-IND-AREA.
           05  IND-DOB                 PIC S9(4) COMP.
           05  IND-NATIONAL-ID         PIC S9(4) COMP.
           05  IND-TAX-ID              PIC S9(4) COMP.
           05  IND-DOMICILE            PIC S9(4) COMP.
           05  IND-CITIZENSHIP         PIC S9(4) COMP.
           05  IND-LEGAL-NAME          PIC S9(4) COMP.
           05  IND-SEGMENT             PIC S9(4) COMP.
      *
       01  WS-DATE-EDIT.
           05  WS-DE-YYYY              PIC 9(4).
           05  FILLER                  PIC X     VALUE '-'.
           05  WS-DE-MM                PIC 9(2).
           05  FILLER                  PIC X     VALUE '-'.
           05  WS-DE-DD                PIC 9(2).
      *
           COPY CVPARTY1Y.
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      ******************************************************************
      * ONE PARTY MAY HOLD SEVERAL CUSTOMER NUMBERS.  THE CURSOR       *
      * BUILDS THE LINK TABLE IN PT-LINK.                              *
      ******************************************************************
           EXEC SQL DECLARE PLINKCSR CURSOR FOR
               SELECT CUST_ID
                    , CUST_STATUS
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :DCL-PARTY-ID
                ORDER BY CUST_ID
           END-EXEC.
      *
       LINKAGE SECTION.
      *
           COPY CVRISK01Y.
      *
       01  LK-RETURN-AREA.
           05  LK-RETURN-CD            PIC S9(4) COMP.
           05  LK-RETURN-PGM           PIC X(8).
           05  LK-RETURN-MSG           PIC X(60).
      *
      ******************************************************************
       PROCEDURE DIVISION USING CV-RISK-AREA
                                LK-RETURN-AREA.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
      *
           PERFORM 1000-VALIDATE-REQUEST
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 2000-RESOLVE-PARTY
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 3000-DRIVE-CHAIN
           PERFORM 4000-SET-OUTCOME
           PERFORM 5000-COMMIT-POINT
           .
       0000-RETURN.
           PERFORM 8000-APPEND-HOP
           MOVE WS-WORST-RC            TO CV-RISK-RC
           MOVE WS-WORST-RC            TO LK-RETURN-CD
           MOVE WS-PROGRAM-ID          TO LK-RETURN-PGM
           IF LK-RETURN-MSG = SPACES
               MOVE CV-RISK-REASON-TXT TO LK-RETURN-MSG
           END-IF
           GOBACK
           .
      *
      ******************************************************************
      * 0100 - INITIALISE.  RUN UNIT WORK IS DONE ONCE ONLY.           *
      ******************************************************************
       0100-INITIALISE.
           MOVE '0100-INITIALISE'      TO WS-PARAGRAPH
           MOVE 'N'                    TO WS-FATAL-SW
           MOVE 'N'                    TO WS-PARTY-FOUND-SW
           MOVE ZERO                   TO WS-WORST-RC
           MOVE ZERO                   TO WS-CHAIN-RC
           MOVE SPACES                 TO LK-RETURN-MSG
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
      *
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           ACCEPT WS-CURR-TIME         FROM TIME
      *
           IF WS-FIRST-CALL
               MOVE 'N'                TO WS-FIRST-CALL-SW
               DISPLAY 'PRBRSK1  RECALC RUN UNIT STARTED  DATE='
                       WS-CURR-DATE ' TIME=' WS-CURR-HHMMSS
               DISPLAY 'PRBRSK1  COMMIT FREQUENCY = '
                       WS-COMMIT-FREQUENCY
           END-IF
           .
      *
      ******************************************************************
      * 1000 - VALIDATE THE COMMAREA HANDED TO US                      *
      ******************************************************************
       1000-VALIDATE-REQUEST.
           MOVE '1000-VALIDATE-REQUEST' TO WS-PARAGRAPH
      *
           IF NOT CV-RISK-VER-CURRENT
               IF CV-RISK-VER-OLD
      *            DOWN LEVEL CALLER.  THE OLD LAYOUTS HAVE NO TRACE
      *            ARRAY SO WE CANNOT SAFELY WRITE INTO IT.
                   MOVE 'DOWN LEVEL COMMAREA VERSION - REBIND CALLER'
                                       TO CV-RISK-REASON-TXT
               ELSE
                   MOVE 'UNKNOWN COMMAREA VERSION'
                                       TO CV-RISK-REASON-TXT
               END-IF
               MOVE 12                 TO WS-WORST-RC
               MOVE 'Y'                TO WS-FATAL-SW
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE 'VERS'             TO ER-REASON-CD
               MOVE 'F'                TO ER-SEVERITY
               DISPLAY 'PRBRSK1  COMMAREA VERSION REJECTED VER='
                       CV-RISK-VERSION ' CALLER=' CV-RISK-CALLER-ID
               GO TO 1000-EXIT
           END-IF
      *
           IF NOT CV-RISK-RCAL
               MOVE 12                 TO WS-WORST-RC
               MOVE 'Y'                TO WS-FATAL-SW
               MOVE 'REQUEST TYPE NOT VALID FOR BATCH RECALCULATION'
                                       TO CV-RISK-REASON-TXT
               MOVE 'BUSN'             TO ER-ERROR-TYPE
               MOVE 'RQTY'             TO ER-REASON-CD
               DISPLAY 'PRBRSK1  INVALID REQUEST TYPE='
                       CV-RISK-REQ-TYPE ' CALLER=' CV-RISK-CALLER-ID
               GO TO 1000-EXIT
           END-IF
      *
           IF NOT CV-RISK-CHNL-BATCH
               MOVE 12                 TO WS-WORST-RC
               MOVE 'Y'                TO WS-FATAL-SW
               MOVE 'ONLINE CHANNEL NOT SUPPORTED BY THIS ENTRY POINT'
                                       TO CV-RISK-REASON-TXT
               MOVE 'BUSN'             TO ER-ERROR-TYPE
               MOVE 'CHNL'             TO ER-REASON-CD
               DISPLAY 'PRBRSK1  INVALID CHANNEL=' CV-RISK-CHANNEL
               GO TO 1000-EXIT
           END-IF
      *
           IF CV-RISK-PARTY-ID = SPACES
              AND CV-RISK-CUST-ID = ZERO
               MOVE 12                 TO WS-WORST-RC
               MOVE 'Y'                TO WS-FATAL-SW
               MOVE 'NO PARTY ID OR CUSTOMER ID SUPPLIED'
                                       TO CV-RISK-REASON-TXT
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE 'KEY '             TO ER-REASON-CD
               DISPLAY 'PRBRSK1  NO KEY SUPPLIED CORREL='
                       CV-RISK-CORREL-ID
           END-IF
      *
      *    THE CALLER OWNS THE CORRELATION ID.  IF IT ARRIVES EMPTY WE
      *    MANUFACTURE ONE SO THE AUDIT ROW CAN STILL BE TIED BACK.
           IF CV-RISK-CORREL-ID = SPACES
               MOVE SPACES             TO CV-RISK-CORREL-ID
               MOVE WS-CURR-DATE       TO CV-RISK-CORREL-ID(1:8)
               MOVE WS-CURR-HHMMSS     TO CV-RISK-CORREL-ID(9:6)
               MOVE 'BR'               TO CV-RISK-CORREL-ID(15:2)
           END-IF
      *
           MOVE ZERO                   TO CV-RISK-SQLCODE
           MOVE SPACES                 TO CV-RISK-FAIL-PGM
           MOVE WS-CURR-DATE           TO CV-RISK-REQ-DATE
           MOVE WS-CURR-HHMMSS         TO CV-RISK-REQ-TIME
           .
       1000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - RESOLVE THE PARTY                                       *
      *                                                                *
      * THE CALLER MAY SUPPLY EITHER THE PARTY ID OR A PRODUCT SYSTEM  *
      * CUSTOMER NUMBER.  A CUSTOMER NUMBER IS RESOLVED TO ITS PARTY   *
      * FIRST, THEN THE PARTY IS READ IN ITS OWN RIGHT.                *
      ******************************************************************
       2000-RESOLVE-PARTY.
           MOVE '2000-RESOLVE-PARTY'   TO WS-PARAGRAPH
           MOVE SPACES                 TO PARTY-RECORD
           MOVE 1                      TO PT-LINK-CNT
           MOVE ZERO                   TO PT-ALIAS-CNT
      *
           IF CV-RISK-PARTY-ID = SPACES
               PERFORM 2100-PARTY-FROM-CUST
               IF WS-FATAL
                   GO TO 2000-EXIT
               END-IF
           ELSE
               MOVE CV-RISK-PARTY-ID   TO DCL-PARTY-ID
           END-IF
      *
           PERFORM 2200-READ-PARTY
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2300-BUILD-LINKS
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-PARTY-FROM-CUST.
           MOVE '2100-PARTY-FROM-CUST' TO WS-PARAGRAPH
           MOVE CV-RISK-CUST-ID        TO DCL-CUST-ID
      *
           EXEC SQL
               SELECT PARTY_ID
                 INTO :DCL-PARTY-ID
                 FROM PARTYRSK.CUSTOMER
                WHERE CUST_ID = :DCL-CUST-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-PARTY-ID   TO CV-RISK-PARTY-ID
               WHEN +100
                   MOVE 12             TO WS-WORST-RC
                   MOVE 'Y'            TO WS-FATAL-SW
                   MOVE 'CUSTOMER NUMBER NOT KNOWN TO PARTY SYSTEM'
                                       TO CV-RISK-REASON-TXT
                   MOVE 'CUST'         TO CV-RISK-REASON-CD
                   DISPLAY 'PRBRSK1  CUST NOT FOUND CUST='
                           CV-RISK-CUST-ID
               WHEN OTHER
                   MOVE 'CUSTOMER         ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '      TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2200-READ-PARTY.
           MOVE '2200-READ-PARTY'      TO WS-PARAGRAPH
      *
           EXEC SQL
               SELECT CUST_ID
                    , PARTY_TYPE
                    , LEGAL_NAME
                    , LAST_NAME
                    , CHAR(DOB, ISO)
                    , NATIONAL_ID
                    , TAX_ID
                    , COUNTRY_CD
                    , DOMICILE_CTRY
                    , CITIZENSHIP_CTRY
                    , PEP_FLG
                    , CUST_STATUS
                    , CHAR(ONBOARD_DATE, ISO)
                    , SEGMENT_CD
                 INTO :DCL-CUST-ID
                    , :DCL-PARTY-TYPE
                    , :DCL-LEGAL-NAME    :IND-LEGAL-NAME
                    , :DCL-LAST-NAME
                    , :DCL-DOB           :IND-DOB
                    , :DCL-NATIONAL-ID   :IND-NATIONAL-ID
                    , :DCL-TAX-ID        :IND-TAX-ID
                    , :DCL-COUNTRY-CD
                    , :DCL-DOMICILE-CTRY :IND-DOMICILE
                    , :DCL-CITIZENSHIP   :IND-CITIZENSHIP
                    , :DCL-PEP-FLG
                    , :DCL-CUST-STATUS
                    , :DCL-ONBOARD-DATE
                    , :DCL-SEGMENT-CD    :IND-SEGMENT
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :DCL-PARTY-ID
                ORDER BY CUST_ID
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-PARTY-FOUND-SW
                   PERFORM 2250-MAP-PARTY
               WHEN +100
                   MOVE 12             TO WS-WORST-RC
                   MOVE 'Y'            TO WS-FATAL-SW
                   MOVE 'PARTY NOT FOUND ON PARTYDB'
                                       TO CV-RISK-REASON-TXT
                   MOVE 'PRTY'         TO CV-RISK-REASON-CD
                   DISPLAY 'PRBRSK1  PARTY NOT FOUND PARTY='
                           DCL-PARTY-ID
               WHEN OTHER
                   MOVE 'CUSTOMER         ' TO ER-SQL-TABLE
                   MOVE 'SELECT  '      TO ER-SQL-OPERATION
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       2250-MAP-PARTY.
           MOVE DCL-PARTY-ID           TO PT-PARTY-ID
           MOVE DCL-PARTY-TYPE         TO PT-PARTY-TYPE
           IF IND-LEGAL-NAME < ZERO
               MOVE DCL-LAST-NAME      TO PT-LEGAL-NAME
           ELSE
               MOVE DCL-LEGAL-NAME     TO PT-LEGAL-NAME
           END-IF
           MOVE PT-LEGAL-NAME(1:25)    TO PT-SHORT-NAME
      *
           IF IND-DOB < ZERO
               MOVE ZERO               TO PT-DOB-INCORP
           ELSE
               MOVE DCL-DOB            TO WS-DATE-EDIT
               MOVE WS-DE-YYYY         TO PT-DOB-INCORP(1:4)
               MOVE WS-DE-MM           TO PT-DOB-INCORP(5:2)
               MOVE WS-DE-DD           TO PT-DOB-INCORP(7:2)
           END-IF
      *
           IF IND-NATIONAL-ID < ZERO
               MOVE SPACES             TO PT-NATIONAL-ID
           ELSE
               MOVE DCL-NATIONAL-ID    TO PT-NATIONAL-ID
           END-IF
           IF IND-TAX-ID < ZERO
               MOVE SPACES             TO PT-TAX-ID
           ELSE
               MOVE DCL-TAX-ID         TO PT-TAX-ID
           END-IF
           IF IND-DOMICILE < ZERO
               MOVE DCL-COUNTRY-CD     TO PT-DOMICILE-CTRY
           ELSE
               MOVE DCL-DOMICILE-CTRY  TO PT-DOMICILE-CTRY
           END-IF
           MOVE DCL-COUNTRY-CD         TO PT-RESIDENCE-CTRY
           IF IND-CITIZENSHIP < ZERO
               MOVE DCL-COUNTRY-CD     TO PT-CITIZENSHIP
           ELSE
               MOVE DCL-CITIZENSHIP    TO PT-CITIZENSHIP
           END-IF
           MOVE DCL-PEP-FLG            TO PT-PEP-FLG
           MOVE DCL-CUST-STATUS        TO PT-STATUS
           MOVE DCL-ONBOARD-DATE       TO WS-DATE-EDIT
           MOVE WS-DE-YYYY             TO PT-ONBOARD-DATE(1:4)
           MOVE WS-DE-MM               TO PT-ONBOARD-DATE(5:2)
           MOVE WS-DE-DD               TO PT-ONBOARD-DATE(7:2)
      *
           IF PT-STATUS-EXITED
               MOVE 4                  TO WS-WORST-RC
               MOVE 'PARTY IS EXITED - SCORE HELD FOR RECORD ONLY'
                                       TO CV-RISK-REASON-TXT
               ADD 1                   TO WS-WARN-COUNT
           END-IF
           .
      *
       2300-BUILD-LINKS.
           MOVE '2300-BUILD-LINKS'     TO WS-PARAGRAPH
           MOVE ZERO                   TO WS-SUB
           MOVE 'N'                    TO WS-LINK-EOF-SW
      *
           EXEC SQL
               OPEN PLINKCSR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'CUSTOMER         ' TO ER-SQL-TABLE
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               GO TO 2300-EXIT
           END-IF
      *
           PERFORM UNTIL WS-LINK-EOF
                      OR WS-SUB >= 20
               EXEC SQL
                   FETCH PLINKCSR
                    INTO :DCL-CUST-ID
                       , :DCL-CUST-STATUS
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1           TO WS-SUB
                       MOVE WS-MODULE-CARDSVC
                                       TO PT-LINK-SYSTEM(WS-SUB)
                       MOVE DCL-CUST-ID
                                       TO PT-LINK-CUST-ID(WS-SUB)
                       MOVE DCL-CUST-STATUS
                                       TO PT-LINK-STATUS(WS-SUB)
                   WHEN +100
                       MOVE 'Y'        TO WS-LINK-EOF-SW
                   WHEN OTHER
                       MOVE 'CUSTOMER         ' TO ER-SQL-TABLE
                       MOVE 'FETCH   '  TO ER-SQL-OPERATION
                       PERFORM 9100-SQL-ERROR
                       MOVE 'Y'        TO WS-LINK-EOF-SW
               END-EVALUATE
           END-PERFORM
      *
           EXEC SQL
               CLOSE PLINKCSR
           END-EXEC
      *
           IF WS-SUB = ZERO
               MOVE 1                  TO PT-LINK-CNT
               MOVE WS-MODULE-PARTYRSK TO PT-LINK-SYSTEM(1)
               MOVE CV-RISK-CUST-ID    TO PT-LINK-CUST-ID(1)
               MOVE PT-STATUS          TO PT-LINK-STATUS(1)
           ELSE
               MOVE WS-SUB             TO PT-LINK-CNT
           END-IF
      *
           MOVE PT-LINK-CUST-ID(1)     TO CV-RISK-CUST-ID
           .
       2300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - DRIVE THE RECALCULATION CHAIN                           *
      *                                                                *
      * PRBRSK2 CARRIES ON DOWN THE CHAIN.  IT RETURNS THE WORST       *
      * CONDITION FOUND ANYWHERE BELOW IT.                             *
      ******************************************************************
       3000-DRIVE-CHAIN.
           MOVE '3000-DRIVE-CHAIN'     TO WS-PARAGRAPH
           MOVE ZERO                   TO LK-RETURN-CD
           MOVE SPACES                 TO LK-RETURN-PGM
      *
           CALL 'PRBRSK2' USING CV-RISK-AREA
                                PARTY-RECORD
                                LK-RETURN-AREA
      *
           MOVE LK-RETURN-CD           TO WS-CHAIN-RC
           IF WS-CHAIN-RC > WS-WORST-RC
               MOVE WS-CHAIN-RC        TO WS-WORST-RC
           END-IF
      *
           IF WS-CHAIN-RC NOT = ZERO
               DISPLAY 'PRBRSK1  CHAIN RC=' WS-CHAIN-RC
                       ' PGM=' LK-RETURN-PGM
                       ' PARTY=' PT-PARTY-ID
           END-IF
           .
      *
      ******************************************************************
      * 4000 - SET THE OUTCOME FOR THE CALLER                          *
      *                                                                *
      * A SANCTIONS HIT IS RETURNED AS 08 AND IS A NORMAL BUSINESS     *
      * OUTCOME.  THE CALLING JOB PARKS THE PARTY FOR MANUAL REVIEW.   *
      ******************************************************************
       4000-SET-OUTCOME.
           MOVE '4000-SET-OUTCOME'     TO WS-PARAGRAPH
           ADD 1                       TO WS-PARTIES-DONE
           ADD 1                       TO WS-PARTIES-SINCE-CMT
      *
           IF CV-RISK-SANCTION-HIT
               ADD 1                   TO WS-HIT-COUNT
               IF WS-WORST-RC < 8
                   MOVE 8              TO WS-WORST-RC
               END-IF
               MOVE 'SANC'             TO CV-RISK-REASON-CD
               MOVE 'SANCTIONS HIT - REFER TO FINANCIAL CRIME TEAM'
                                       TO CV-RISK-REASON-TXT
               DISPLAY 'PRBRSK1  SANCTIONS HIT PARTY=' PT-PARTY-ID
                       ' SCORE=' CV-RISK-SCORE
                       ' CORREL=' CV-RISK-CORREL-ID
           END-IF
      *
           IF CV-RISK-KYC-FAILED
               IF WS-WORST-RC < 8
                   MOVE 8              TO WS-WORST-RC
               END-IF
               MOVE 'KYCF'             TO CV-RISK-REASON-CD
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-WORST-RC = 0
                   MOVE 'APPR'         TO CV-RISK-ADVICE-CD
               WHEN WS-WORST-RC = 4
                   MOVE 'APPR'         TO CV-RISK-ADVICE-CD
                   ADD 1               TO WS-WARN-COUNT
               WHEN WS-WORST-RC = 8
                   MOVE 'REFR'         TO CV-RISK-ADVICE-CD
               WHEN OTHER
                   MOVE 'DECL'         TO CV-RISK-ADVICE-CD
           END-EVALUATE
      *
           IF CV-RISK-BAND-REFUSE
               MOVE 'DECL'             TO CV-RISK-ADVICE-CD
           END-IF
           .
      *
      ******************************************************************
      * 5000 - UNIT OF WORK                                            *
      *                                                                *
      * NOTHING BELOW US COMMITS.  A FATAL CONDITION ROLLS THE WHOLE   *
      * PARTY BACK SO THE SCORE AND THE EXPOSURE STAY IN STEP.         *
      ******************************************************************
       5000-COMMIT-POINT.
           MOVE '5000-COMMIT-POINT'    TO WS-PARAGRAPH
      *
           IF WS-WORST-RC >= 12
               PERFORM 5200-ROLLBACK
               GO TO 5000-EXIT
           END-IF
      *
           IF WS-PARTIES-SINCE-CMT >= WS-COMMIT-FREQUENCY
               PERFORM 5100-COMMIT
           END-IF
           .
       5000-EXIT.
           EXIT
           .
      *
       5100-COMMIT.
           EXEC SQL
               COMMIT
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'COMMIT           ' TO ER-SQL-TABLE
               MOVE 'COMMIT  '         TO ER-SQL-OPERATION
               PERFORM 9100-SQL-ERROR
               PERFORM 9900-ABEND
           END-IF
      *
           ADD 1                       TO WS-COMMIT-COUNT
           MOVE ZERO                   TO WS-PARTIES-SINCE-CMT
           MOVE WS-PARTIES-DONE        TO WS-DISP-COUNT
           DISPLAY 'PRBRSK1  COMMIT TAKEN NBR=' WS-COMMIT-COUNT
                   ' PARTIES=' WS-DISP-COUNT
                   ' LAST PARTY=' PT-PARTY-ID
           .
      *
       5200-ROLLBACK.
           EXEC SQL
               ROLLBACK
           END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY 'PRBRSK1  ROLLBACK FAILED SQLCODE=' SQLCODE
               PERFORM 9900-ABEND
           END-IF
      *
           MOVE ZERO                   TO WS-PARTIES-SINCE-CMT
           DISPLAY 'PRBRSK1  UNIT OF WORK BACKED OUT PARTY='
                   PT-PARTY-ID ' RC=' WS-WORST-RC
           .
      *
      ******************************************************************
      * 8000 - HOP TRACE                                               *
      ******************************************************************
       8000-APPEND-HOP.
           IF CV-RISK-HOP-CNT NOT NUMERIC
               MOVE ZERO               TO CV-RISK-HOP-CNT
           END-IF
           IF CV-RISK-HOP-CNT < 8
               ADD 1                   TO CV-RISK-HOP-CNT
               MOVE WS-PROGRAM-ID      TO
                    CV-RISK-HOP-PGM(CV-RISK-HOP-CNT)
               MOVE WS-WORST-RC        TO
                    CV-RISK-HOP-RC(CV-RISK-HOP-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 9100 - SQL FAILURE.  ANYTHING WE DID NOT EXPECT IS FATAL.      *
      ******************************************************************
       9100-SQL-ERROR.
           MOVE SQLCODE                TO WS-SAVE-SQLCODE
           MOVE SQLCODE                TO WS-SQL-DISP
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE WS-PROGRAM-ID          TO CV-RISK-FAIL-PGM
           MOVE 'SQL FAILURE IN PARTY RESOLUTION'
                                       TO CV-RISK-REASON-TXT
           MOVE 12                     TO WS-WORST-RC
           MOVE 'Y'                    TO WS-FATAL-SW
      *
           DISPLAY 'PRBRSK1  SQL ERROR PARA=' WS-PARAGRAPH
           DISPLAY '         TABLE=' ER-SQL-TABLE
                   ' OP=' ER-SQL-OPERATION
                   ' SQLCODE=' WS-SQL-DISP
           DISPLAY '         SQLERRMC=' SQLERRMC(1:44)
           DISPLAY '         PARTY=' DCL-PARTY-ID
                   ' CORREL=' CV-RISK-CORREL-ID
           .
      *
      ******************************************************************
      * 9900 - UNRECOVERABLE.  BACK OUT AND ABEND SO THE OPERATOR      *
      *        SEES IT.  U3101 IS THE PARTYRSK RECALC ABEND.           *
      ******************************************************************
       9900-ABEND.
           MOVE 'U310'                 TO ER-ABEND-CODE
           MOVE 'Y'                    TO ER-ABEND-REQUESTED
           DISPLAY 'PRBRSK1  ABEND U3101 PARTY=' PT-PARTY-ID
                   ' PARA=' WS-PARAGRAPH
           DISPLAY 'PRBRSK1  PARTIES PROCESSED=' WS-PARTIES-DONE
                   ' COMMITS=' WS-COMMIT-COUNT
                   ' HITS=' WS-HIT-COUNT
      *
           MOVE 3101                   TO WS-ABEND-CODE
           CALL 'ILBOABN0' USING WS-ABEND-CODE
           .
