      ******************************************************************
      * PRRSK04 - RISK DECISION AND REGULATORY AUDIT                   *
      *                                                                *
      * TURNS THE SCORE, THE BAND, THE KYC STANDING AND THE SANCTIONS  *
      * FLAG INTO THE ADVICE THE CARD SIDE ACTS ON, WRITES THE SCORE   *
      * ROW AND WRITES THE REGULATORY AUDIT TRAIL ROW.                 *
      *                                                                *
      * THIS PROGRAM COMMITS NOTHING.  THE UNIT OF WORK BELONGS TO THE *
      * CALLING CICS TASK IN THE CARD SERVICING REGION - IF THAT TASK  *
      * BACKS OUT, THE SCORE AND THE AUDIT ROW GO WITH IT.  THAT IS    *
      * DELIBERATE AND WAS AGREED WITH COMPLIANCE AT IMPLEMENTATION:   *
      * AN AUTHORISATION THAT NEVER HAPPENED MUST NOT LEAVE A SCORE    *
      * BEHIND.  DO NOT ADD A SYNCPOINT HERE.                          *
      *                                                                *
      * THE AUDIT TABLE IS APPEND ONLY.  NOTHING IN THE ESTATE UPDATES *
      * OR DELETES FROM IT - THE RETENTION JOB IN THE PARTY WEEKLY     *
      * CYCLE UNLOADS AND PRUNES IT UNDER DUAL CONTROL.                *
      *                                                                *
      * CALLED BY   - PRRSK03                                          *
      * CALLS       - PRERR01  PARTYRSK ERROR HANDLER                  *
      * TABLES      - PARTYRSK.PARTY_RISK_SCORE      (SELECT, INSERT)  *
      *               PARTYRSK.PARTY_RISK_AUDIT      (INSERT)          *
      * COMMAREA    - CV-RISK-AREA, 512 BYTES, CVRISK01Y               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRRSK04.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRRSK04 '.
       01  WS-ERROR-PGM                PIC X(8)  VALUE 'PRERR01 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-COMMAREA-LEN             PIC S9(4) COMP VALUE 512.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-PRIOR-SW             PIC X     VALUE 'N'.
               88  WS-PRIOR-FOUND                VALUE 'Y'.
      *
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(8)  VALUE SPACES.
           05  WS-TIME-HMS             PIC X(6)  VALUE SPACES.
      *
       01  WS-TODAY                    PIC 9(8)  VALUE ZERO.
       01  WS-TODAY-R REDEFINES WS-TODAY.
           05  WS-TD-CC                PIC 9(2).
           05  WS-TD-YYMMDD            PIC 9(6).
      *
       01  WS-TASK-NBR                 PIC 9(7)  VALUE ZERO.
      *
      ******************************************************************
      * THE AUDIT KEY IS BUILT LOCALLY RATHER THAN FROM A SEQUENCE     *
      * OBJECT - THE TABLE PREDATES IDENTITY COLUMNS ON THIS SUBSYSTEM *
      * AND THE UNIQUENESS COMES FROM THE TASK NUMBER PLUS THE CLOCK.  *
      ******************************************************************
       01  WS-AUDIT-ID.
           05  FILLER                  PIC X     VALUE 'A'.
           05  WS-AU-DATE              PIC 9(6)  VALUE ZERO.
           05  WS-AU-TIME              PIC 9(6)  VALUE ZERO.
           05  WS-AU-TASK              PIC 9(7)  VALUE ZERO.
      *
      ******************************************************************
      * THE HOP TRACE IS SERIALISED AS PROGRAM AND RETURN CODE PAIRS   *
      * SEPARATED BY A SOLIDUS - PRKYC01/0000 PRKYC02/0000 ...         *
      ******************************************************************
       01  WS-HOP-WORK.
           05  WS-HOP-SUB              PIC S9(4) COMP VALUE 0.
           05  WS-HOP-POS              PIC S9(4) COMP VALUE 1.
           05  WS-HOP-TEXT             PIC X(120) VALUE SPACES.
      *
       01  WS-HOP-PAIR.
           05  WS-HP-PGM               PIC X(8).
           05  FILLER                  PIC X     VALUE '/'.
           05  WS-HP-RC                PIC 9(4).
           05  FILLER                  PIC X     VALUE SPACE.
      *
       01  WS-COMPONENT-TEXT           PIC X(400) VALUE SPACES.
      *
       01  WS-DECISION.
           05  WS-ADVICE-CD            PIC X(4)  VALUE SPACES.
           05  WS-REASON-CD            PIC X(4)  VALUE SPACES.
           05  WS-REASON-TXT           PIC X(60) VALUE SPACES.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
       01  HV-SCORE.
           05  HV-PARTY-ID             PIC X(11).
           05  HV-MODEL-ID             PIC X(8).
           05  HV-MODEL-VERSION        PIC S9(4) COMP.
           05  HV-RISK-SCORE           PIC S9(4) COMP.
           05  HV-RISK-BAND            PIC X(1).
           05  HV-PD-PCT               PIC S9(3)V9(5) COMP-3.
           05  HV-EXPOSURE-AMT         PIC S9(13)V99 COMP-3.
           05  HV-ADVICE-CD            PIC X(4).
           05  HV-REASON-CD            PIC X(4).
           05  HV-KYC-STATUS           PIC X(2).
           05  HV-SANCTION-FLG         PIC X(1).
           05  HV-SCORED-BY            PIC X(8).
           05  HV-SOURCE-CHANNEL       PIC X(1).
      *
       01  HV-PRIOR.
           05  HV-OLD-SCORE            PIC S9(4) COMP.
           05  HV-OLD-BAND             PIC X(1).
      *
       01  HV-AUDIT.
           05  HV-AUDIT-ID             PIC X(20).
           05  HV-EVENT-TYPE           PIC X(4).
           05  HV-REQ-MODULE           PIC X(8).
           05  HV-REQ-PGM              PIC X(8).
           05  HV-CORREL-ID            PIC X(16).
           05  HV-CHANNEL              PIC X(1).
           05  HV-REASON-TXT           PIC X(60).
      *
       01  HV-COMPONENT-DATA.
           49  HV-CD-LEN               PIC S9(4) COMP.
           49  HV-CD-TEXT              PIC X(400).
      *
       01  HV-HOP-TRACE.
           49  HV-HT-LEN               PIC S9(4) COMP.
           49  HV-HT-TEXT              PIC X(120).
      *
       01  IND-OLD-SCORE               PIC S9(4) COMP.
       01  IND-OLD-BAND                PIC S9(4) COMP.
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
           IF EIBCALEN < WS-COMMAREA-LEN
               EXEC CICS ABEND ABCODE('PRR4') NODUMP END-EXEC
           END-IF
      *
           SET ADDRESS OF CV-RISK-AREA TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-INITIALISE
           PERFORM 2000-READ-PRIOR-SCORE
           PERFORM 3000-DECIDE
           PERFORM 4000-BUILD-COMPONENT-TEXT
           PERFORM 4500-BUILD-HOP-TRACE
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 5000-INSERT-SCORE
           END-IF
      *
      *    THE AUDIT ROW IS WRITTEN EVEN WHEN THE SCORE INSERT FAILED.
      *    A FAILED ASSESSMENT IS STILL A REGULATORY EVENT.
           PERFORM 6000-INSERT-AUDIT
      *
           PERFORM 7000-ADD-HOP
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
           MOVE 'N'                    TO WS-PRIOR-SW
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
           MOVE EIBTASKN               TO WS-TASK-NBR
      *
           MOVE WS-TD-YYMMDD           TO WS-AU-DATE
           MOVE WS-TIME-HMS            TO WS-AU-TIME
           MOVE WS-TASK-NBR            TO WS-AU-TASK
      *
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
           MOVE CV-RISK-MODEL-ID       TO HV-MODEL-ID
           MOVE CV-RISK-SCORE          TO HV-RISK-SCORE
           MOVE CV-RISK-BAND           TO HV-RISK-BAND
           MOVE CV-RISK-EXPOSURE-AMT   TO HV-EXPOSURE-AMT
           MOVE CV-RISK-KYC-STATUS     TO HV-KYC-STATUS
           MOVE CV-RISK-CHANNEL        TO HV-SOURCE-CHANNEL
           MOVE WS-PGM-ID              TO HV-SCORED-BY
           MOVE 1                      TO HV-MODEL-VERSION
      *
      *    ONLY 'Y' AND 'N' ARE VALID ON THE TABLE.  A POSSIBLE MATCH
      *    IS RECORDED AS 'N' ON THE SCORE ROW AND CARRIED IN THE
      *    REASON CODE INSTEAD.
           IF CV-RISK-SANCTION-HIT
               MOVE 'Y'                TO HV-SANCTION-FLG
           ELSE
               MOVE 'N'                TO HV-SANCTION-FLG
           END-IF
      *
           IF HV-MODEL-ID = SPACES
               MOVE 'PRSCR003'         TO HV-MODEL-ID
           END-IF
      *
           IF CV-RISK-BAND = SPACES
               MOVE 'X'                TO HV-RISK-BAND
           END-IF
      *
      *    THE PD IS NOT CARRIED IN THE COMMAREA.  IT IS RECOMPUTED
      *    FROM THE PUBLISHED SCORE ON THE SAME LINEAR CURVE THE
      *    SCORING PROGRAM USES.
           COMPUTE HV-PD-PCT ROUNDED =
                   (CV-RISK-SCORE * 0.02500) / 100
           .
      *
      ******************************************************************
      * 2000 - PRIOR SCORE FOR THE AUDIT TRAIL                         *
      ******************************************************************
       2000-READ-PRIOR-SCORE.
           EXEC SQL
               SELECT RISK_SCORE
                    , RISK_BAND
                 INTO :HV-OLD-SCORE :IND-OLD-SCORE
                    , :HV-OLD-BAND  :IND-OLD-BAND
                 FROM PARTYRSK.PARTY_RISK_SCORE
                WHERE PARTY_ID = :HV-PARTY-ID
                ORDER BY SCORE_TS DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'Y'            TO WS-PRIOR-SW
               WHEN +100
                   MOVE ZERO           TO HV-OLD-SCORE
                   MOVE SPACE          TO HV-OLD-BAND
                   MOVE -1             TO IND-OLD-SCORE
                   MOVE -1             TO IND-OLD-BAND
               WHEN OTHER
      *            A MISSING PRIOR SCORE MUST NOT STOP THE DECISION.
      *            IT IS REPORTED AND THE AUDIT ROW CARRIES NULLS.
                   MOVE 'W'            TO ER-SEVERITY
                   MOVE 'SQL '         TO ER-ERROR-TYPE
                   MOVE 'PARTY_RISK_SCORE  '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '2000-READ-PRIOR-SCORE'
                                       TO ER-PARAGRAPH
                   MOVE SQLCODE        TO ER-SQLCODE
                   MOVE 'PRIOR SCORE READ FAILED - AUDIT WILL BE THIN'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   MOVE -1             TO IND-OLD-SCORE
                   MOVE -1             TO IND-OLD-BAND
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - THE DECISION                                            *
      *                                                                *
      * ORDER MATTERS.  A SANCTIONS HIT OUTRANKS EVERYTHING, THEN A    *
      * FAILED KYC REVIEW, THEN THE BAND, THEN THE HEADROOM.           *
      ******************************************************************
       3000-DECIDE.
           MOVE SPACES                 TO WS-ADVICE-CD
           MOVE SPACES                 TO WS-REASON-CD
           MOVE SPACES                 TO WS-REASON-TXT
      *
           EVALUATE TRUE
               WHEN CV-RISK-SANCTION-HIT
                   MOVE 'DECL'         TO WS-ADVICE-CD
                   MOVE 'SANC'         TO WS-REASON-CD
                   MOVE 'SANCTIONS MATCH CONFIRMED - DO NOT AUTHORISE'
                                       TO WS-REASON-TXT
               WHEN CV-RISK-KYC-FAILED
                   MOVE 'DECL'         TO WS-ADVICE-CD
                   MOVE 'KYCF'         TO WS-REASON-CD
                   MOVE 'KYC REVIEW FAILED - REFER TO COMPLIANCE'
                                       TO WS-REASON-TXT
               WHEN CV-RISK-BAND-REFUSE
                   MOVE 'DECL'         TO WS-ADVICE-CD
                   MOVE 'BNDX'         TO WS-REASON-CD
                   MOVE 'RISK BAND X - OUTSIDE APPETITE'
                                       TO WS-REASON-TXT
               WHEN CV-RISK-KYC-EXPIRED
                   MOVE 'REFR'         TO WS-ADVICE-CD
                   MOVE 'KYCX'         TO WS-REASON-CD
                   MOVE 'KYC REVIEW OVERDUE - REFER FOR REVIEW'
                                       TO WS-REASON-TXT
               WHEN CV-RISK-SANCTION-FLG = 'P'
                   MOVE 'REFR'         TO WS-ADVICE-CD
                   MOVE 'SANP'         TO WS-REASON-CD
                   MOVE 'POSSIBLE SANCTIONS MATCH - REFER FOR REVIEW'
                                       TO WS-REASON-TXT
               WHEN CV-RISK-BAND-HIGH
                   PERFORM 3100-DECIDE-BAND-C
               WHEN CV-RISK-KYC-PENDING
                   MOVE 'REFR'         TO WS-ADVICE-CD
                   MOVE 'KYCP'         TO WS-REASON-CD
                   MOVE 'KYC REVIEW PENDING - REFER FOR REVIEW'
                                       TO WS-REASON-TXT
               WHEN OTHER
                   PERFORM 3200-DECIDE-HEADROOM
           END-EVALUATE
      *
           MOVE WS-ADVICE-CD           TO CV-RISK-ADVICE-CD
           MOVE WS-ADVICE-CD           TO HV-ADVICE-CD
           MOVE WS-REASON-CD           TO CV-RISK-REASON-CD
           MOVE WS-REASON-CD           TO HV-REASON-CD
           MOVE WS-REASON-TXT          TO CV-RISK-REASON-TXT
           MOVE WS-REASON-TXT          TO HV-REASON-TXT
      *
           EVALUATE WS-ADVICE-CD
               WHEN 'DECL'
                   IF CV-RISK-RC < 0008
                       MOVE 0008       TO CV-RISK-RC
                   END-IF
               WHEN 'REFR'
                   IF CV-RISK-RC < 0004
                       MOVE 0004       TO CV-RISK-RC
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3100 - BAND C.  THE REQUEST IS ONLY APPROVED WHEN IT IS SMALL  *
      *        AND THE PARTY STILL HAS HEADROOM.                       *
      ******************************************************************
       3100-DECIDE-BAND-C.
           IF CV-RISK-REQ-AMT > CV-RISK-AVAIL-AMT
               MOVE 'DECL'             TO WS-ADVICE-CD
               MOVE 'HDRM'             TO WS-REASON-CD
               MOVE 'HIGH RISK BAND AND INSUFFICIENT HEADROOM'
                                       TO WS-REASON-TXT
           ELSE
               MOVE 'REFR'             TO WS-ADVICE-CD
               MOVE 'BNDC'             TO WS-REASON-CD
               MOVE 'HIGH RISK BAND - REFER FOR MANUAL DECISION'
                                       TO WS-REASON-TXT
           END-IF
           .
      *
      ******************************************************************
      * 3200 - BANDS A AND B                                           *
      ******************************************************************
       3200-DECIDE-HEADROOM.
           IF CV-RISK-REQ-AMT > CV-RISK-AVAIL-AMT
               MOVE 'REFR'             TO WS-ADVICE-CD
               MOVE 'HDRM'             TO WS-REASON-CD
               MOVE 'REQUEST EXCEEDS AVAILABLE HEADROOM'
                                       TO WS-REASON-TXT
           ELSE
               MOVE 'APPR'             TO WS-ADVICE-CD
               MOVE 'OK  '             TO WS-REASON-CD
               MOVE 'WITHIN APPETITE AND WITHIN HEADROOM'
                                       TO WS-REASON-TXT
           END-IF
           .
      *
      ******************************************************************
      * 4000 - COMPONENT DATA FOR THE MODEL RISK TEAM                  *
      *                                                                *
      * THE SCORING COMPONENTS THEMSELVES STAY IN THE SCORING PROGRAM. *
      * WHAT IS KEPT HERE IS THE INPUT SET THAT PRODUCED THE DECISION  *
      * SO A CHALLENGE CAN BE RECONSTRUCTED FROM THE ROW ALONE.        *
      ******************************************************************
       4000-BUILD-COMPONENT-TEXT.
           MOVE SPACES                 TO WS-COMPONENT-TEXT
           STRING 'SCORE='             DELIMITED BY SIZE
                  CV-RISK-SCORE        DELIMITED BY SIZE
                  ' BAND='             DELIMITED BY SIZE
                  CV-RISK-BAND         DELIMITED BY SIZE
                  ' KYC='              DELIMITED BY SIZE
                  CV-RISK-KYC-STATUS   DELIMITED BY SIZE
                  ' SANC='             DELIMITED BY SIZE
                  CV-RISK-SANCTION-FLG DELIMITED BY SIZE
                  ' MODEL='            DELIMITED BY SIZE
                  CV-RISK-MODEL-ID     DELIMITED BY SIZE
                  ' MCC='              DELIMITED BY SIZE
                  CV-RISK-MCC          DELIMITED BY SIZE
                  ' CTRY='             DELIMITED BY SIZE
                  CV-RISK-COUNTRY      DELIMITED BY SIZE
                  ' CURR='             DELIMITED BY SIZE
                  CV-RISK-REQ-CURR     DELIMITED BY SIZE
              INTO WS-COMPONENT-TEXT
           END-STRING
      *
           MOVE WS-COMPONENT-TEXT      TO HV-CD-TEXT
           MOVE 400                    TO HV-CD-LEN
           .
      *
      ******************************************************************
      * 4500 - SERIALISE THE HOP TRACE                                 *
      ******************************************************************
       4500-BUILD-HOP-TRACE.
           MOVE SPACES                 TO WS-HOP-TEXT
           MOVE 1                      TO WS-HOP-POS
      *
           PERFORM VARYING WS-HOP-SUB FROM 1 BY 1
                     UNTIL WS-HOP-SUB > CV-RISK-HOP-CNT
                        OR WS-HOP-SUB > 8
                        OR WS-HOP-POS > 106
               MOVE CV-RISK-HOP-PGM(WS-HOP-SUB)
                                       TO WS-HP-PGM
               MOVE CV-RISK-HOP-RC(WS-HOP-SUB)
                                       TO WS-HP-RC
               MOVE WS-HOP-PAIR        TO WS-HOP-TEXT(WS-HOP-POS:14)
               ADD 14                  TO WS-HOP-POS
           END-PERFORM
      *
      *    THIS PROGRAM IS NOT IN THE TRACE YET - IT ADDS ITSELF ON
      *    THE WAY OUT - SO IT IS APPENDED BY HAND FOR THE AUDIT ROW.
           IF WS-HOP-POS <= 106
               MOVE WS-PGM-ID          TO WS-HP-PGM
               MOVE CV-RISK-RC         TO WS-HP-RC
               MOVE WS-HOP-PAIR        TO WS-HOP-TEXT(WS-HOP-POS:14)
           END-IF
      *
           MOVE WS-HOP-TEXT            TO HV-HT-TEXT
           MOVE 120                    TO HV-HT-LEN
           .
      *
      ******************************************************************
      * 5000 - WRITE THE SCORE ROW                                     *
      ******************************************************************
       5000-INSERT-SCORE.
           EXEC SQL
               INSERT INTO PARTYRSK.PARTY_RISK_SCORE
                     (PARTY_ID
                    , SCORE_TS
                    , MODEL_ID
                    , MODEL_VERSION
                    , RISK_SCORE
                    , RISK_BAND
                    , PD_PCT
                    , EXPOSURE_AMT
                    , ADVICE_CD
                    , REASON_CD
                    , KYC_STATUS
                    , SANCTION_FLG
                    , COMPONENT_DATA
                    , OVERRIDE_FLG
                    , SCORED_BY
                    , SOURCE_CHANNEL)
               VALUES (:HV-PARTY-ID
                    , CURRENT TIMESTAMP
                    , :HV-MODEL-ID
                    , :HV-MODEL-VERSION
                    , :HV-RISK-SCORE
                    , :HV-RISK-BAND
                    , :HV-PD-PCT
                    , :HV-EXPOSURE-AMT
                    , :HV-ADVICE-CD
                    , :HV-REASON-CD
                    , :HV-KYC-STATUS
                    , :HV-SANCTION-FLG
                    , :HV-COMPONENT-DATA
                    , 'N'
                    , :HV-SCORED-BY
                    , :HV-SOURCE-CHANNEL)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
      *            TWO TASKS SCORED THE SAME PARTY IN THE SAME
      *            MICROSECOND.  THE DECISION STANDS, ONLY THE ROW
      *            IS LOST, SO IT IS A WARNING AND NOT A FAILURE.
                   MOVE 'W'            TO ER-SEVERITY
                   MOVE 'SQL '         TO ER-ERROR-TYPE
                   MOVE 'PARTY_RISK_SCORE  '
                                       TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE '5000-INSERT-SCORE'
                                       TO ER-PARAGRAPH
                   MOVE SQLCODE        TO ER-SQLCODE
                   MOVE 'DUPLICATE SCORE TIMESTAMP - ROW NOT WRITTEN'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   IF CV-RISK-RC < 0004
                       MOVE 0004       TO CV-RISK-RC
                   END-IF
               WHEN OTHER
                   MOVE 'PARTY_RISK_SCORE  '
                                       TO ER-SQL-TABLE
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE '5000-INSERT-SCORE'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 6000 - WRITE THE REGULATORY AUDIT ROW                          *
      ******************************************************************
       6000-INSERT-AUDIT.
           MOVE WS-AUDIT-ID            TO HV-AUDIT-ID
           MOVE 'AUTH'                 TO HV-EVENT-TYPE
           MOVE CV-RISK-CALLER-MOD     TO HV-REQ-MODULE
           MOVE CV-RISK-CALLER-ID      TO HV-REQ-PGM
           MOVE CV-RISK-CORREL-ID      TO HV-CORREL-ID
           MOVE CV-RISK-CHANNEL        TO HV-CHANNEL
      *
           IF HV-REQ-MODULE = SPACES
               MOVE 'UNKNOWN '         TO HV-REQ-MODULE
           END-IF
           IF HV-REQ-PGM = SPACES
               MOVE 'UNKNOWN '         TO HV-REQ-PGM
           END-IF
      *
           EXEC SQL
               INSERT INTO PARTYRSK.PARTY_RISK_AUDIT
                     (AUDIT_ID
                    , PARTY_ID
                    , EVENT_TS
                    , EVENT_TYPE
                    , REQUESTING_MODULE
                    , REQUESTING_PGM
                    , CORREL_ID
                    , CHANNEL
                    , OLD_SCORE
                    , NEW_SCORE
                    , OLD_BAND
                    , NEW_BAND
                    , KYC_STATUS
                    , SANCTION_FLG
                    , ADVICE_CD
                    , REASON_TXT
                    , HOP_TRACE)
               VALUES (:HV-AUDIT-ID
                    , :HV-PARTY-ID
                    , CURRENT TIMESTAMP
                    , :HV-EVENT-TYPE
                    , :HV-REQ-MODULE
                    , :HV-REQ-PGM
                    , :HV-CORREL-ID
                    , :HV-CHANNEL
                    , :HV-OLD-SCORE :IND-OLD-SCORE
                    , :HV-RISK-SCORE
                    , :HV-OLD-BAND  :IND-OLD-BAND
                    , :HV-RISK-BAND
                    , :HV-KYC-STATUS
                    , :HV-SANCTION-FLG
                    , :HV-ADVICE-CD
                    , :HV-REASON-TXT
                    , :HV-HOP-TRACE)
           END-EXEC
      *
           IF SQLCODE = 0
               GO TO 6000-EXIT
           END-IF
      *
      *    THE AUDIT TRAIL IS A REGULATORY OBLIGATION.  IF IT CANNOT
      *    BE WRITTEN THE ASSESSMENT IS NOT ALLOWED TO STAND - THE
      *    ADVICE IS DOWNGRADED TO A REFERRAL AND THE FAILURE IS
      *    REPORTED AS FATAL SO THE TASK IS INVESTIGATED.
           MOVE 'PARTY_RISK_AUDIT  '   TO ER-SQL-TABLE
           MOVE 'INSERT  '             TO ER-SQL-OPERATION
           MOVE '6000-INSERT-AUDIT'    TO ER-PARAGRAPH
           MOVE 'F'                    TO ER-SEVERITY
           PERFORM 8000-SQL-ERROR
      *
           MOVE 'REFR'                 TO CV-RISK-ADVICE-CD
           MOVE 'AUD1'                 TO CV-RISK-REASON-CD
           MOVE 'AUDIT TRAIL NOT WRITTEN - REFER AND RETRY'
                                       TO CV-RISK-REASON-TXT
           .
       6000-EXIT.
           EXIT
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
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           IF ER-SEVERITY NOT = 'F'
               MOVE 'E'                TO ER-SEVERITY
           END-IF
           MOVE 'RISK DECISION COULD NOT BE RECORDED'
                                       TO ER-MESSAGE
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
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
