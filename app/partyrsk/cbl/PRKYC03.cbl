      ******************************************************************
      * PRKYC03 - PARTY AND KYC INQUIRY ENTRY POINT                    *
      *                                                                *
      * THE READ ONLY CROSSING.  CARD SERVICING LINKS TO THIS PROGRAM  *
      * THROUGH THE ROUTE TABLE WHEN AN OPERATOR ASKS FOR THE PARTY    *
      * AND KYC PICTURE BEHIND A CARD ACCOUNT.  THE COMMAREA IS THE    *
      * SAME CV-RISK-AREA USED BY THE AUTHORISATION CROSSING BUT WITH  *
      * CV-RISK-REQ-TYPE OF 'INQY'.                                    *
      *                                                                *
      * NOTHING IN THIS CHAIN UPDATES PARTY DATA.  THE ONLY ROW THAT   *
      * IS WRITTEN IS THE INQUIRY AUDIT ROW AT THE BOTTOM, WHICH IS A  *
      * REGULATORY REQUIREMENT - EVERY LOOK AT A PARTY RECORD FROM     *
      * OUTSIDE PARTYRSK IS RECORDED.                                  *
      *                                                                *
      * CALLED BY   - THE ONLINE DISPATCHER ON ROUTE XMOD / KYCINQ     *
      * CALLS       - PRKYC04  PARTY DETAIL AND KYC HISTORY            *
      *               PRERR01  PARTYRSK ERROR HANDLER                  *
      * TABLES      - PARTYRSK.CUSTOMER              (SELECT)          *
      * COMMAREA    - CV-RISK-AREA, 512 BYTES, CVRISK01Y               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRKYC03.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRKYC03 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'PRKYC04 '.
       01  WS-ERROR-PGM                PIC X(8)  VALUE 'PRERR01 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-COMMAREA-LEN             PIC S9(4) COMP VALUE 512.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
      *
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(8)  VALUE SPACES.
           05  WS-TIME-HMS             PIC X(6)  VALUE SPACES.
      *
       01  WS-TODAY                    PIC 9(8)  VALUE ZERO.
       01  WS-NOW                      PIC 9(6)  VALUE ZERO.
       01  WS-REASON-TEXT              PIC X(60) VALUE SPACES.
      *
       01  WS-SAVE-HDR.
           05  WS-SV-CALLER-ID         PIC X(8)  VALUE SPACES.
           05  WS-SV-CALLER-MOD        PIC X(8)  VALUE SPACES.
           05  WS-SV-CORREL-ID         PIC X(16) VALUE SPACES.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
       01  HV-AREA.
           05  HV-CUST-ID              PIC S9(9) COMP-3.
           05  HV-PARTY-ID             PIC X(11).
           05  HV-CUST-STATUS          PIC X(1).
           05  HV-ROW-CNT              PIC S9(9) COMP.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
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
           EXEC CICS HANDLE CONDITION
                     ERROR(9500-UNEXPECTED)
           END-EXEC
      *
           IF EIBCALEN < WS-COMMAREA-LEN
               MOVE 'PRC3'             TO ER-ABEND-CODE
               MOVE 'Y'                TO ER-ABEND-REQUESTED
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE '0000-MAIN-LINE'   TO ER-PARAGRAPH
               MOVE 'COMMAREA MISSING OR TOO SHORT ON INQUIRY ENTRY'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               GO TO 0000-EXIT
           END-IF
      *
           SET ADDRESS OF CV-RISK-AREA TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-INITIALISE
           PERFORM 1100-VALIDATE-REQUEST
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 1300-RESOLVE-PARTY
           END-IF
      *
           PERFORM 7000-ADD-HOP
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 3000-DRIVE-CHAIN
               PERFORM 4000-FORMAT-SUMMARY
           END-IF
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
      ******************************************************************
      * 1000 - INITIALISE                                              *
      ******************************************************************
       1000-INITIALISE.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
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
           MOVE WS-DATE-CYMD           TO WS-TODAY
           MOVE WS-TIME-HMS            TO WS-NOW
      *
           MOVE CV-RISK-CALLER-ID      TO WS-SV-CALLER-ID
           MOVE CV-RISK-CALLER-MOD     TO WS-SV-CALLER-MOD
           MOVE CV-RISK-CORREL-ID      TO WS-SV-CORREL-ID
      *
           IF CV-RISK-CORREL-ID = SPACES
               MOVE EIBTRNID           TO CV-RISK-CORREL-ID(1:4)
               MOVE EIBTASKN           TO CV-RISK-CORREL-ID(5:7)
               MOVE WS-NOW             TO CV-RISK-CORREL-ID(12:5)
               MOVE CV-RISK-CORREL-ID  TO WS-SV-CORREL-ID
           END-IF
      *
           IF CV-RISK-REQ-DATE = ZERO
               MOVE WS-TODAY           TO CV-RISK-REQ-DATE
           END-IF
           IF CV-RISK-REQ-TIME = ZERO
               MOVE WS-NOW             TO CV-RISK-REQ-TIME
           END-IF
      *
           MOVE ZERO                   TO CV-RISK-RC
           MOVE ZERO                   TO CV-RISK-SQLCODE
           MOVE SPACES                 TO CV-RISK-REASON-TXT
           MOVE SPACES                 TO CV-RISK-FAIL-PGM
      *
           MOVE ZERO                   TO CV-RISK-SCORE
           MOVE SPACES                 TO CV-RISK-BAND
           MOVE SPACES                 TO CV-RISK-KYC-STATUS
           MOVE 'N'                    TO CV-RISK-SANCTION-FLG
           MOVE ZERO                   TO CV-RISK-EXPOSURE-AMT
           MOVE ZERO                   TO CV-RISK-AVAIL-AMT
           MOVE SPACES                 TO CV-RISK-ADVICE-CD
           MOVE SPACES                 TO CV-RISK-REASON-CD
      *
           IF CV-RISK-HOP-CNT NOT NUMERIC
               MOVE ZERO               TO CV-RISK-HOP-CNT
           END-IF
           .
      *
      ******************************************************************
      * 1100 - VALIDATE                                                *
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
           IF NOT CV-RISK-INQY
               MOVE 'REQUEST TYPE NOT VALID FOR THE INQUIRY ENTRY'
                                       TO WS-REASON-TEXT
               MOVE 'REQ2'             TO ER-REASON-CD
               PERFORM 1900-REJECT-FATAL
               GO TO 1100-EXIT
           END-IF
      *
           IF CV-RISK-PARTY-ID = SPACES
              AND CV-RISK-CUST-ID = ZERO
               MOVE 'NEITHER PARTY ID NOR CUSTOMER ID SUPPLIED'
                                       TO WS-REASON-TEXT
               MOVE 'KEY1'             TO ER-REASON-CD
               PERFORM 1900-REJECT-FATAL
           END-IF
           .
       1100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 1300 - RESOLVE THE PARTY                                       *
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
                    , CUST_STATUS
                 INTO :HV-PARTY-ID
                    , :HV-CUST-STATUS
                 FROM PARTYRSK.CUSTOMER
                WHERE CUST_ID = :HV-CUST-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE HV-PARTY-ID    TO CV-RISK-PARTY-ID
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
           .
       1300-EXIT.
           EXIT
           .
      *
       1400-VERIFY-PARTY.
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :HV-ROW-CNT
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :HV-PARTY-ID
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
               MOVE 'SELECT  '         TO ER-SQL-OPERATION
               MOVE '1400-VERIFY-PARTY'
                                       TO ER-PARAGRAPH
               PERFORM 8000-SQL-ERROR
               GO TO 1400-EXIT
           END-IF
      *
           IF HV-ROW-CNT = ZERO
               MOVE 'PARTY ID NOT FOUND ON THE PARTY SYSTEM'
                                       TO WS-REASON-TEXT
               MOVE 'PTY2'             TO ER-REASON-CD
               PERFORM 1900-REJECT-FATAL
           END-IF
           .
       1400-EXIT.
           EXIT
           .
      *
       1900-REJECT-FATAL.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-REASON-TEXT         TO CV-RISK-REASON-TXT
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE ER-REASON-CD           TO CV-RISK-REASON-CD
      *
           MOVE 'BUSN'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-REASON-TEXT         TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
      ******************************************************************
      * 3000 - DRIVE THE INQUIRY CHAIN                                 *
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
           IF WS-RESP = DFHRESP(NORMAL)
               GO TO 3000-EXIT
           END-IF
      *
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'LNK6'                 TO CV-RISK-REASON-CD
           MOVE 'PARTY DETAIL MODULE COULD NOT BE LINKED'
                                       TO CV-RISK-REASON-TXT
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE '3000-DRIVE-CHAIN'     TO ER-PARAGRAPH
           MOVE CV-RISK-REASON-TXT     TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
       3000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - FORMAT THE SUMMARY FOR THE CALLING SCREEN               *
      *                                                                *
      * THE INQUIRY NEVER RETURNS AN ADVICE CODE - THERE IS NOTHING    *
      * BEING DECIDED - SO THE FIELD IS BLANKED AND THE REASON TEXT    *
      * CARRIES THE STANDING OF THE PARTY INSTEAD.                     *
      ******************************************************************
       4000-FORMAT-SUMMARY.
           MOVE SPACES                 TO CV-RISK-ADVICE-CD
      *
           IF CV-RISK-SCORE-DATE = ZERO
               MOVE 'NSCR'             TO CV-RISK-REASON-CD
               MOVE 'NO RISK SCORE HELD FOR THIS PARTY'
                                       TO CV-RISK-REASON-TXT
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
           END-IF
      *
           IF CV-RISK-KYC-STATUS = SPACES
               MOVE 'PN'               TO CV-RISK-KYC-STATUS
           END-IF
      *
           IF CV-RISK-REASON-TXT = SPACES
               EVALUATE TRUE
                   WHEN CV-RISK-KYC-OK
                       MOVE 'KYC IN GOOD STANDING'
                                       TO CV-RISK-REASON-TXT
                   WHEN CV-RISK-KYC-EXPIRED
                       MOVE 'KYC REVIEW OVERDUE OR DOCUMENTS EXPIRED'
                                       TO CV-RISK-REASON-TXT
                   WHEN CV-RISK-KYC-FAILED
                       MOVE 'KYC REVIEW FAILED'
                                       TO CV-RISK-REASON-TXT
                   WHEN OTHER
                       MOVE 'KYC REVIEW PENDING'
                                       TO CV-RISK-REASON-TXT
               END-EVALUATE
           END-IF
      *
           MOVE WS-SV-CALLER-ID        TO CV-RISK-CALLER-ID
           MOVE WS-SV-CALLER-MOD       TO CV-RISK-CALLER-MOD
           MOVE WS-SV-CORREL-ID        TO CV-RISK-CORREL-ID
           MOVE 'INQY'                 TO CV-RISK-REQ-TYPE
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
      * 8000 / 9000 - DIAGNOSTICS                                      *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'PARTY LOOKUP FAILED ON THE INQUIRY CHAIN'
                                       TO ER-MESSAGE
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'SQL5'                 TO CV-RISK-REASON-CD
           MOVE 'PARTY DATABASE UNAVAILABLE - INQUIRY FAILED'
                                       TO CV-RISK-REASON-TXT
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
      * 9500 - LAST RESORT                                             *
      ******************************************************************
       9500-UNEXPECTED.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE '9500-UNEXPECTED'      TO ER-PARAGRAPH
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE EIBRESP                TO ER-EIBRESP
           MOVE EIBRESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
           MOVE 'UNHANDLED CONDITION IN THE INQUIRY ENTRY POINT'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
      *
           IF EIBCALEN >= WS-COMMAREA-LEN
               MOVE 0012               TO CV-RISK-RC
               MOVE WS-PGM-ID          TO CV-RISK-FAIL-PGM
               MOVE 'SYS9'             TO CV-RISK-REASON-CD
           END-IF
      *
           GO TO 0000-EXIT
           .
