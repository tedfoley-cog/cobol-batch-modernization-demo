      ******************************************************************
      * PRKYC01 - RISK SERVICE ENTRY POINT (AUTHORIZATION ASSESSMENT)  *
      *                                                                *
      * THE PARTYRSK SIDE OF THE ONLINE RISK CROSSING.  CARD SERVICING *
      * LINKS TO THIS PROGRAM THROUGH THE ROUTE TABLE - IT NEVER NAMES *
      * IT DIRECTLY - PASSING CV-RISK-AREA (CVRISK01Y) AS THE COMMAREA *
      * WITH CV-RISK-REQ-TYPE OF 'AUTH'.                               *
      *                                                                *
      * RESPONSIBILITIES                                               *
      *   - VALIDATE THE INBOUND COMMAREA AND ITS VERSION              *
      *   - RESOLVE THE PARTY IDENTIFIER WHEN ONLY A CUSTOMER NUMBER   *
      *     WAS SUPPLIED BY THE CALLING PRODUCT SYSTEM                 *
      *   - DRIVE THE ASSESSMENT CHAIN AND ROLL UP THE RETURN CODE     *
      *   - MAP THE ACCUMULATED RESULT INTO CV-RISK-OUT                *
      *                                                                *
      * THE UNIT OF WORK BELONGS TO THE CALLING TASK.  NO SYNCPOINT IS *
      * ISSUED ANYWHERE IN THIS CHAIN.                                 *
      *                                                                *
      * CALLED BY   - THE ONLINE DISPATCHER ON ROUTE XMOD / RISKSVC    *
      * CALLS       - PRKYC02  KYC AND SANCTIONS SCREENING             *
      *               PRERR01  PARTYRSK ERROR HANDLER                  *
      * TABLES      - PARTYRSK.CUSTOMER            (SELECT)            *
      * COMMAREA    - CV-RISK-AREA, 512 BYTES, CVRISK01Y               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRKYC01.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRKYC01 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'PRKYC02 '.
       01  WS-ERROR-PGM                PIC X(8)  VALUE 'PRERR01 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-COMMAREA-LEN             PIC S9(4) COMP VALUE 512.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-PARTY-SW             PIC X     VALUE 'N'.
               88  WS-PARTY-RESOLVED             VALUE 'Y'.
           05  WS-ABEND-SW             PIC X     VALUE 'N'.
               88  WS-ABEND-WANTED               VALUE 'Y'.
      *
      ******************************************************************
      * CURRENT DATE AND TIME.  THE DATE IS HELD BOTH AS A FULL EIGHT  *
      * DIGIT VALUE AND DECOMPOSED THROUGH A REDEFINES BECAUSE THE     *
      * OLDER PARTS OF THE CHAIN STILL EXPECT YYMMDD.                  *
      ******************************************************************
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(8)  VALUE SPACES.
           05  WS-TIME-HMS             PIC X(6)  VALUE SPACES.
      *
       01  WS-WORK-DATE                PIC 9(8)  VALUE ZERO.
       01  WS-WORK-DATE-R REDEFINES WS-WORK-DATE.
           05  WS-WD-CC                PIC 9(2).
           05  WS-WD-YY                PIC 9(2).
           05  WS-WD-MM                PIC 9(2).
           05  WS-WD-DD                PIC 9(2).
      *
       01  WS-WORK-TIME                PIC 9(6)  VALUE ZERO.
      *
      ******************************************************************
      * SAVED COPY OF THE INBOUND HEADER.  IF A DOWNSTREAM PROGRAM     *
      * CORRUPTS THE COMMAREA THE AUDIT TRAIL STILL SHOWS WHO ASKED.   *
      ******************************************************************
       01  WS-SAVE-HDR.
           05  WS-SV-CALLER-ID         PIC X(8)  VALUE SPACES.
           05  WS-SV-CALLER-MOD        PIC X(8)  VALUE SPACES.
           05  WS-SV-CORREL-ID         PIC X(16) VALUE SPACES.
           05  WS-SV-REQ-TYPE          PIC X(4)  VALUE SPACES.
      *
       01  WS-REASON-TEXT              PIC X(60) VALUE SPACES.
       01  WS-DOWNSTREAM-RC            PIC 9(4)  VALUE ZERO.
      *
      ******************************************************************
      * DB2 HOST VARIABLES - PARTYRSK.CUSTOMER                         *
      ******************************************************************
       01  HV-CUSTOMER.
           05  HV-CUST-ID              PIC S9(9)V9(0) COMP-3.
           05  HV-PARTY-ID             PIC X(11).
           05  HV-CUST-STATUS          PIC X(1).
           05  HV-PARTY-TYPE           PIC X(1).
           05  HV-COUNTRY-CD           PIC X(3).
           05  HV-LAST-NAME            PIC X(25).
      *
       01  HV-ROW-CNT                  PIC S9(9) COMP VALUE ZERO.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL DECLARE PARTYRSK.CUSTOMER TABLE
              (CUST_ID          DECIMAL(9,0)  NOT NULL,
               PARTY_ID         CHAR(11)      NOT NULL,
               PARTY_TYPE       CHAR(1)       NOT NULL,
               LAST_NAME        CHAR(25)      NOT NULL,
               COUNTRY_CD       CHAR(3)       NOT NULL,
               CUST_STATUS      CHAR(1)       NOT NULL,
               ONBOARD_DATE     DATE          NOT NULL,
               LAST_MAINT_TS    TIMESTAMP     NOT NULL)
           END-EXEC.
      *
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
           COPY CVRISK01Y.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           PERFORM 0100-ESTABLISH-HANDLERS
      *
           IF EIBCALEN < WS-COMMAREA-LEN
      *        NOTHING TO REPLY INTO - THE CALLER PASSED A SHORT OR
      *        MISSING COMMAREA.  THIS IS ALWAYS A BIND OR ROUTE TABLE
      *        ERROR AND THE TASK IS ABENDED SO IT IS INVESTIGATED.
               MOVE 'PRC1'             TO ER-ABEND-CODE
               MOVE 'Y'                TO ER-ABEND-REQUESTED
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE '0000-MAIN-LINE'   TO ER-PARAGRAPH
               MOVE 'COMMAREA MISSING OR TOO SHORT ON RISK ENTRY'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               GO TO 0000-EXIT
           END-IF
      *
           SET ADDRESS OF CV-RISK-AREA TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-INITIALISE
           PERFORM 1100-VALIDATE-REQUEST
           IF WS-ERROR-FOUND
               PERFORM 7000-ADD-HOP
               GO TO 0000-EXIT
           END-IF
      *
           PERFORM 1300-RESOLVE-PARTY
           IF WS-ERROR-FOUND
               PERFORM 7000-ADD-HOP
               GO TO 0000-EXIT
           END-IF
      *
           PERFORM 7000-ADD-HOP
           PERFORM 3000-DRIVE-CHAIN
           PERFORM 4000-MAP-RESULT
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 0100 - CONDITION HANDLING DISCIPLINE FOR THE WHOLE CHAIN       *
      *                                                                *
      * EVERY COMMAND IN PARTYRSK CARRIES RESP SO THE ONLY CONDITIONS  *
      * THAT REACH A HANDLER ARE THE ONES NOBODY CAN CODE FOR.  THOSE  *
      * GO TO 9500 WHICH REPORTS AND RETURNS A FATAL CODE RATHER THAN  *
      * LETTING CICS ABEND THE CARD SERVICING TASK.                    *
      ******************************************************************
       0100-ESTABLISH-HANDLERS.
           EXEC CICS HANDLE CONDITION
                     ERROR(9500-UNEXPECTED)
                     LENGERR(9500-UNEXPECTED)
           END-EXEC
      *
           EXEC CICS HANDLE ABEND
                     LABEL(9500-UNEXPECTED)
           END-EXEC
           .
      *
      ******************************************************************
      * 1000 - INITIALISE                                              *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-PARTY-SW
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE ZERO                   TO ER-SQLCODE
           MOVE ZERO                   TO ER-EIBRESP
           MOVE ZERO                   TO ER-EIBRESP2
           MOVE 'N'                    TO ER-ABEND-REQUESTED
      *
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-CYMD)
                     TIME(WS-TIME-HMS)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE WS-DATE-CYMD           TO WS-WORK-DATE
           MOVE WS-TIME-HMS            TO WS-WORK-TIME
      *
           MOVE CV-RISK-CALLER-ID      TO WS-SV-CALLER-ID
           MOVE CV-RISK-CALLER-MOD     TO WS-SV-CALLER-MOD
           MOVE CV-RISK-CORREL-ID      TO WS-SV-CORREL-ID
           MOVE CV-RISK-REQ-TYPE       TO WS-SV-REQ-TYPE
      *
      *    A CALLER THAT DID NOT STAMP THE REQUEST GETS OUR CLOCK.
           IF CV-RISK-REQ-DATE = ZERO
               MOVE WS-WORK-DATE       TO CV-RISK-REQ-DATE
           END-IF
           IF CV-RISK-REQ-TIME = ZERO
               MOVE WS-WORK-TIME       TO CV-RISK-REQ-TIME
           END-IF
           IF CV-RISK-CORREL-ID = SPACES
               MOVE EIBTRNID           TO CV-RISK-CORREL-ID(1:4)
               MOVE EIBTASKN           TO CV-RISK-CORREL-ID(5:7)
               MOVE WS-WORK-TIME       TO CV-RISK-CORREL-ID(12:5)
           END-IF
      *
           MOVE ZERO                   TO CV-RISK-RC
           MOVE ZERO                   TO CV-RISK-SQLCODE
           MOVE SPACES                 TO CV-RISK-REASON-TXT
           MOVE SPACES                 TO CV-RISK-FAIL-PGM
           MOVE ZERO                   TO WS-DOWNSTREAM-RC
      *
      *    THE OUTPUT AREA IS CLEARED HERE SO A PARTIAL CHAIN CANNOT
      *    RETURN VALUES LEFT BY AN EARLIER REQUEST IN THE SAME
      *    PSEUDO CONVERSATION.
           MOVE ZERO                   TO CV-RISK-SCORE
           MOVE SPACES                 TO CV-RISK-BAND
           MOVE SPACES                 TO CV-RISK-KYC-STATUS
           MOVE 'N'                    TO CV-RISK-SANCTION-FLG
           MOVE ZERO                   TO CV-RISK-EXPOSURE-AMT
           MOVE ZERO                   TO CV-RISK-AVAIL-AMT
           MOVE SPACES                 TO CV-RISK-ADVICE-CD
           MOVE SPACES                 TO CV-RISK-REASON-CD
           MOVE ZERO                   TO CV-RISK-SCORE-DATE
           MOVE SPACES                 TO CV-RISK-MODEL-ID
      *
           IF CV-RISK-HOP-CNT NOT NUMERIC
               MOVE ZERO               TO CV-RISK-HOP-CNT
           END-IF
           .
      *
      ******************************************************************
      * 1100 - VALIDATE THE REQUEST                                    *
      *                                                                *
      * THE VERSION CHECK IS THE CONTRACT BETWEEN THE TWO MODULES.     *
      * A DOWN LEVEL CALLER IS REJECTED RATHER THAN GUESSED AT - THE   *
      * FIELDS ADDED AT VERSION 3 SIT WHERE VERSION 2 HELD FILLER.     *
      ******************************************************************
       1100-VALIDATE-REQUEST.
           IF CV-RISK-VERSION NOT NUMERIC
               MOVE 'RISK COMMAREA VERSION IS NOT NUMERIC'
                                       TO WS-REASON-TEXT
               MOVE 'VER0'             TO ER-REASON-CD
               PERFORM 1900-REJECT-FATAL
               GO TO 1100-EXIT
           END-IF
      *
           IF NOT CV-RISK-VER-CURRENT
               MOVE 'RISK COMMAREA VERSION DOWN LEVEL - REBIND CALLER'
                                       TO WS-REASON-TEXT
               MOVE 'VER1'             TO ER-REASON-CD
               PERFORM 1900-REJECT-FATAL
               GO TO 1100-EXIT
           END-IF
      *
           IF NOT CV-RISK-AUTH
               MOVE 'REQUEST TYPE NOT VALID FOR AUTH RISK ENTRY POINT'
                                       TO WS-REASON-TEXT
               MOVE 'REQ1'             TO ER-REASON-CD
               PERFORM 1900-REJECT-FATAL
               GO TO 1100-EXIT
           END-IF
      *
           IF NOT CV-RISK-CHNL-ONLINE
      *        A BATCH FLAGGED REQUEST ARRIVING OVER A CICS LINK IS
      *        ACCEPTED BUT NOTED - THE RECALC DRIVER SOMETIMES REUSES
      *        AN AREA IT BUILT EARLIER IN THE SAME RUN.
               MOVE 'O'                TO CV-RISK-CHANNEL
           END-IF
      *
           IF CV-RISK-PARTY-ID = SPACES
              AND CV-RISK-CUST-ID = ZERO
               MOVE 'NEITHER PARTY ID NOR CUSTOMER ID SUPPLIED'
                                       TO WS-REASON-TEXT
               MOVE 'KEY1'             TO ER-REASON-CD
               PERFORM 1900-REJECT-FATAL
               GO TO 1100-EXIT
           END-IF
      *
           IF CV-RISK-CALLER-MOD = SPACES
               MOVE 'UNKNOWN '         TO CV-RISK-CALLER-MOD
           END-IF
           .
       1100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 1300 - RESOLVE THE PARTY                                       *
      *                                                                *
      * PRODUCT SYSTEMS KNOW THEIR OWN CUSTOMER NUMBER.  PARTYRSK IS   *
      * PARTY CENTRIC, SO THE CUSTOMER NUMBER IS TRANSLATED HERE ONCE  *
      * AND EVERY PROGRAM BELOW WORKS ON THE PARTY ID.                 *
      ******************************************************************
       1300-RESOLVE-PARTY.
           IF CV-RISK-PARTY-ID NOT = SPACES
               PERFORM 1400-VERIFY-PARTY
               GO TO 1300-EXIT
           END-IF
      *
           MOVE CV-RISK-CUST-ID        TO HV-CUST-ID
      *
           EXEC SQL
               SELECT PARTY_ID
                    , PARTY_TYPE
                    , CUST_STATUS
                    , COUNTRY_CD
                    , LAST_NAME
                 INTO :HV-PARTY-ID
                    , :HV-PARTY-TYPE
                    , :HV-CUST-STATUS
                    , :HV-COUNTRY-CD
                    , :HV-LAST-NAME
                 FROM PARTYRSK.CUSTOMER
                WHERE CUST_ID = :HV-CUST-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE HV-PARTY-ID    TO CV-RISK-PARTY-ID
                   MOVE 'Y'            TO WS-PARTY-SW
               WHEN +100
                   MOVE 'CUSTOMER NUMBER NOT KNOWN TO PARTY SYSTEM'
                                       TO WS-REASON-TEXT
                   MOVE 'CUS1'         TO ER-REASON-CD
                   PERFORM 1900-REJECT-FATAL
               WHEN OTHER
                   MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '1300-RESOLVE-PARTY'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
      *
           IF WS-PARTY-RESOLVED
               IF HV-CUST-STATUS = 'X'
                   MOVE 'PARTY HAS BEEN EXITED - REFER TO COMPLIANCE'
                                       TO CV-RISK-REASON-TXT
                   MOVE 0004           TO CV-RISK-RC
               END-IF
      *        THE COUNTRY ON THE REQUEST WINS WHEN THE ACQUIRER SENT
      *        ONE.  OTHERWISE THE PARTY DOMICILE IS USED DOWNSTREAM.
               IF CV-RISK-COUNTRY = SPACES
                   MOVE HV-COUNTRY-CD  TO CV-RISK-COUNTRY
               END-IF
           END-IF
           .
       1300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 1400 - THE CALLER SUPPLIED A PARTY ID.  CONFIRM IT EXISTS AND  *
      *        PICK UP THE CUSTOMER NUMBER FOR THE AUDIT TRAIL.        *
      ******************************************************************
       1400-VERIFY-PARTY.
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
      *
           EXEC SQL
               SELECT MIN(CUST_ID)
                    , COUNT(*)
                 INTO :HV-CUST-ID
                    , :HV-ROW-CNT
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND CUST_STATUS <> 'X'
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF HV-ROW-CNT = ZERO
                       MOVE 'PARTY ID HAS NO ACTIVE CUSTOMER RECORD'
                                       TO WS-REASON-TEXT
                       MOVE 'PTY1'     TO ER-REASON-CD
                       PERFORM 1900-REJECT-FATAL
                   ELSE
                       MOVE 'Y'        TO WS-PARTY-SW
                       IF CV-RISK-CUST-ID = ZERO
                           MOVE HV-CUST-ID
                                       TO CV-RISK-CUST-ID
                       END-IF
                   END-IF
               WHEN +100
                   MOVE 'PARTY ID NOT FOUND ON THE PARTY SYSTEM'
                                       TO WS-REASON-TEXT
                   MOVE 'PTY2'         TO ER-REASON-CD
                   PERFORM 1900-REJECT-FATAL
               WHEN OTHER
                   MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '1400-VERIFY-PARTY'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 1900 - REJECT WITH A FATAL RETURN CODE AND A REASON THE CARD   *
      *        SIDE CAN PUT ON THE SCREEN.                             *
      ******************************************************************
       1900-REJECT-FATAL.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-REASON-TEXT         TO CV-RISK-REASON-TXT
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'DECL'                 TO CV-RISK-ADVICE-CD
           MOVE ER-REASON-CD           TO CV-RISK-REASON-CD
      *
           MOVE 'BUSN'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-REASON-TEXT         TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
      ******************************************************************
      * 3000 - DRIVE THE ASSESSMENT CHAIN                              *
      *                                                                *
      * ONE LINK ONLY.  EVERYTHING BELOW HANGS OFF THE KYC PROGRAM AND *
      * THE SAME COMMAREA IS THREADED THROUGH THE WHOLE DESCENT.       *
      ******************************************************************
       3000-DRIVE-CHAIN.
           EXEC CICS LINK
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CV-RISK-AREA)
                     LENGTH(WS-COMMAREA-LEN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE CV-RISK-RC     TO WS-DOWNSTREAM-RC
               WHEN DFHRESP(PGMIDERR)
                   MOVE 'KYC SCREENING MODULE NOT AVAILABLE IN REGION'
                                       TO CV-RISK-REASON-TXT
                   MOVE 'LNK1'         TO CV-RISK-REASON-CD
                   MOVE 0012           TO CV-RISK-RC
                   MOVE WS-PGM-ID      TO CV-RISK-FAIL-PGM
                   MOVE '3000-DRIVE-CHAIN'
                                       TO ER-PARAGRAPH
                   PERFORM 8500-CICS-ERROR
               WHEN OTHER
                   MOVE 'RISK CHAIN LINK FAILED - SEE PRER QUEUE'
                                       TO CV-RISK-REASON-TXT
                   MOVE 'LNK2'         TO CV-RISK-REASON-CD
                   MOVE 0012           TO CV-RISK-RC
                   MOVE WS-PGM-ID      TO CV-RISK-FAIL-PGM
                   MOVE '3000-DRIVE-CHAIN'
                                       TO ER-PARAGRAPH
                   PERFORM 8500-CICS-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 4000 - MAP THE ACCUMULATED RESULT                              *
      *                                                                *
      * THE PROGRAMS BELOW HAVE ALREADY WRITTEN THEIR OWN FIELDS INTO  *
      * CV-RISK-OUT.  THIS PARAGRAPH OWNS THE ROLL UP - IT DECIDES     *
      * WHAT THE CARD SIDE SEES WHEN THE CHAIN STOPPED EARLY.          *
      ******************************************************************
       4000-MAP-RESULT.
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
      *
           EVALUATE TRUE
               WHEN CV-RISK-RC = 0012
                   PERFORM 4100-FATAL-RESULT
               WHEN CV-RISK-SANCTION-HIT
                   PERFORM 4200-SANCTION-RESULT
               WHEN CV-RISK-RC = 0008
                   PERFORM 4300-DECLINE-RESULT
               WHEN CV-RISK-ADVICE-CD = SPACES
                   PERFORM 4400-NO-ADVICE-RESULT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF CV-RISK-SCORE-DATE = ZERO
               MOVE WS-WORK-DATE       TO CV-RISK-SCORE-DATE
           END-IF
      *
      *    RESTORE THE HEADER FIELDS THE CALLER GAVE US.  THE CHAIN IS
      *    NOT PERMITTED TO CHANGE THEM AND ONE OF THE OLDER PROGRAMS
      *    USED TO OVERWRITE THE CORRELATION ID.
           MOVE WS-SV-CALLER-ID        TO CV-RISK-CALLER-ID
           MOVE WS-SV-CALLER-MOD       TO CV-RISK-CALLER-MOD
           MOVE WS-SV-CORREL-ID        TO CV-RISK-CORREL-ID
           MOVE WS-SV-REQ-TYPE         TO CV-RISK-REQ-TYPE
           .
      *
       4100-FATAL-RESULT.
           MOVE ZERO                   TO CV-RISK-SCORE
           MOVE 'X'                    TO CV-RISK-BAND
           MOVE 'DECL'                 TO CV-RISK-ADVICE-CD
           IF CV-RISK-REASON-CD = SPACES
               MOVE 'SYS0'             TO CV-RISK-REASON-CD
           END-IF
           IF CV-RISK-REASON-TXT = SPACES
               MOVE 'RISK ASSESSMENT COULD NOT BE COMPLETED'
                                       TO CV-RISK-REASON-TXT
           END-IF
           .
      *
       4200-SANCTION-RESULT.
      *    A CONFIRMED WATCH LIST HIT IS NEVER SCORED.  THE BAND IS
      *    FORCED TO X AND THE CARD SIDE MUST DECLINE AND REFER.
           MOVE 'X'                    TO CV-RISK-BAND
           MOVE 999                    TO CV-RISK-SCORE
           MOVE 'DECL'                 TO CV-RISK-ADVICE-CD
           IF CV-RISK-REASON-CD = SPACES
               MOVE 'SANC'             TO CV-RISK-REASON-CD
           END-IF
           MOVE 0008                   TO CV-RISK-RC
           .
      *
       4300-DECLINE-RESULT.
           IF CV-RISK-ADVICE-CD = SPACES
               MOVE 'DECL'             TO CV-RISK-ADVICE-CD
           END-IF
           IF CV-RISK-BAND = SPACES
               MOVE 'C'                TO CV-RISK-BAND
           END-IF
           .
      *
       4400-NO-ADVICE-RESULT.
      *    THE CHAIN RAN TO THE END BUT NOBODY SET AN ADVICE CODE.
      *    THAT IS A CODING FAULT DOWNSTREAM, NOT A BUSINESS DECLINE,
      *    SO IT IS REPORTED AS A WARNING AND REFERRED.
           MOVE 'REFR'                 TO CV-RISK-ADVICE-CD
           MOVE 'ADV0'                 TO CV-RISK-REASON-CD
           MOVE 'NO ADVICE RETURNED BY THE RISK CHAIN - REFERRED'
                                       TO CV-RISK-REASON-TXT
           MOVE 0004                   TO CV-RISK-RC
           MOVE 'W'                    TO ER-SEVERITY
           MOVE 'BUSN'                 TO ER-ERROR-TYPE
           MOVE '4400-NO-ADVICE-RESULT'
                                       TO ER-PARAGRAPH
           MOVE 'CHAIN COMPLETED WITHOUT AN ADVICE CODE'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
      ******************************************************************
      * 7000 - HOP TRACE                                               *
      ******************************************************************
       7000-ADD-HOP.
           IF CV-RISK-HOP-CNT < 8
               ADD 1                   TO CV-RISK-HOP-CNT
               MOVE WS-PGM-ID          TO
                                    CV-RISK-HOP-PGM(CV-RISK-HOP-CNT)
               MOVE CV-RISK-RC         TO
                                    CV-RISK-HOP-RC(CV-RISK-HOP-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 8000 / 8500 / 9000 - DIAGNOSTICS                               *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'PARTY LOOKUP FAILED ON THE PARTY DATABASE'
                                       TO ER-MESSAGE
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'SQL0'                 TO CV-RISK-REASON-CD
           MOVE 'PARTY DATABASE UNAVAILABLE - RISK NOT ASSESSED'
                                       TO CV-RISK-REASON-TXT
      *
           PERFORM 9000-REPORT-ERROR
           .
      *
       8500-CICS-ERROR.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE EIBTRMID               TO ER-TERM-ID
           MOVE CV-RISK-REASON-TXT     TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
       9000-REPORT-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
      ******************************************************************
      * 9500 - LAST RESORT.  REACHED ONLY FROM HANDLE CONDITION.       *
      ******************************************************************
       9500-UNEXPECTED.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE '9500-UNEXPECTED'      TO ER-PARAGRAPH
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE EIBRESP                TO ER-EIBRESP
           MOVE EIBRESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
           MOVE 'UNHANDLED CONDITION IN THE RISK ENTRY POINT'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
      *
           IF EIBCALEN >= WS-COMMAREA-LEN
               MOVE 0012               TO CV-RISK-RC
               MOVE WS-PGM-ID          TO CV-RISK-FAIL-PGM
               MOVE 'SYS9'             TO CV-RISK-REASON-CD
               MOVE 'UNEXPECTED CONDITION - RISK NOT ASSESSED'
                                       TO CV-RISK-REASON-TXT
           END-IF
      *
           GO TO 0000-EXIT
           .
