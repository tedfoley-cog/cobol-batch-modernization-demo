      ******************************************************************
      * PRRSK03 - RISK SCORING                                         *
      *                                                                *
      * REWRITTEN 2014 FOR THE BASEL RESCORE PROGRAMME.  THE MODEL     *
      * PARAMETERS ARE NO LONGER COMPILED INTO THE LOAD MODULE - THEY  *
      * ARE HELD ON THE RSKPARM KSDS AND MAINTAINED BY THE MODEL RISK  *
      * TEAM THROUGH THE PARTY WEEKLY CYCLE.                           *
      *                                                                *
      * THE PARAMETER LOAD IS DONE ONCE PER LOAD MODULE RESIDENCY AND  *
      * HELD ACROSS TASKS IN WORKING STORAGE.  THE PROGRAM MUST STAY   *
      * NON REENTRANT FOR THAT REASON - IT IS DEFINED TO CICS WITH     *
      * CONCURRENCY(QUASIRENT) AND MUST NOT BE MADE THREADSAFE WITHOUT *
      * MOVING THE CACHE TO A CICS SHARED DATA TABLE.                  *
      *                                                                *
      * SIX WEIGHTED COMPONENTS PRODUCE A 0 TO 999 SCORE AND AN        *
      * A / B / C / X BAND.  ALL THE ARITHMETIC IS PACKED DECIMAL.     *
      *                                                                *
      * CALLED BY   - PRRSK02                                          *
      * CALLS       - PRRSK04  DECISION AND REGULATORY AUDIT           *
      *               PRERR01  PARTYRSK ERROR HANDLER                  *
      * TABLES      - PARTYRSK.CUSTOMER              (SELECT)          *
      *               PARTYRSK.PARTY_KYC             (SELECT)          *
      * FILES       - RSKPARM  PRTY.PROD.RSKPARM KSDS                  *
      * COMMAREA    - CV-RISK-AREA, 512 BYTES, CVRISK01Y               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRRSK03.
       AUTHOR.        PARTY AND RISK.
       DATE-WRITTEN.  2014-05-02.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'PRRSK03 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'PRRSK04 '.
       01  WS-ERROR-PGM                PIC X(8)  VALUE 'PRERR01 '.
       01  WS-PARM-FILE                PIC X(8)  VALUE 'RSKPARM '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-COMMAREA-LEN             PIC S9(4) COMP VALUE 512.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-BROWSE-SW            PIC X     VALUE 'N'.
               88  WS-BROWSE-STARTED             VALUE 'Y'.
      *
      ******************************************************************
      * FIRST TIME SWITCH.  SURVIVES ACROSS TASKS FOR AS LONG AS THE   *
      * PROGRAM STAYS RESIDENT - A NEWCOPY RELOADS THE PARAMETERS.     *
      ******************************************************************
       01  WS-PARM-CACHE-FLG           PIC X     VALUE 'N'.
           88  WS-PARMS-LOADED                   VALUE 'Y'.
      *
       01  WS-MODEL-ID                 PIC X(8)  VALUE 'PRSCR003'.
       01  WS-MODEL-VERSION            PIC 9(4)  VALUE 0001.
      *
       01  WS-PARM-TABLE.
           05  WS-PARM-CNT             PIC S9(4) COMP VALUE 0.
           05  WS-PARM-ENTRY OCCURS 60 TIMES
                             INDEXED BY WS-PX.
               10  WS-PE-CODE          PIC X(8).
               10  WS-PE-TYPE          PIC X(4).
               10  WS-PE-NUM           PIC S9(9)V9(5) COMP-3.
               10  WS-PE-CHAR          PIC X(20).
      *
       01  WS-MAX-PARMS                PIC S9(4) COMP VALUE 60.
      *
       01  WS-PARM-BUFFER              PIC X(120) VALUE SPACES.
       01  WS-PARM-LENGTH              PIC S9(4) COMP VALUE 120.
      *
       01  WS-BROWSE-KEY.
           05  WS-BK-MODEL-ID          PIC X(8)  VALUE SPACES.
           05  WS-BK-PARM-CODE         PIC X(8)  VALUE SPACES.
      *
      ******************************************************************
      * WEIGHTS AND CUT OFFS.  THE VALUES BELOW ARE THE FALL BACK SET  *
      * USED ONLY WHEN RSKPARM CANNOT BE READ - THEY ARE THE VALUES    *
      * THAT WERE EFFECTIVE AT THE 2014 IMPLEMENTATION.                *
      ******************************************************************
       01  WS-WEIGHTS.
           05  WS-WT-UTIL              PIC S9(3)V9(5) COMP-3
                                                 VALUE 0.28000.
           05  WS-WT-DELQ              PIC S9(3)V9(5) COMP-3
                                                 VALUE 0.30000.
           05  WS-WT-EXPO              PIC S9(3)V9(5) COMP-3
                                                 VALUE 0.15000.
           05  WS-WT-KYC               PIC S9(3)V9(5) COMP-3
                                                 VALUE 0.12000.
           05  WS-WT-CTRY              PIC S9(3)V9(5) COMP-3
                                                 VALUE 0.08000.
           05  WS-WT-TENURE            PIC S9(3)V9(5) COMP-3
                                                 VALUE 0.07000.
      *
       01  WS-CUTOFFS.
           05  WS-CUT-BAND-A           PIC S9(4)V99 COMP-3
                                                 VALUE 0250.00.
           05  WS-CUT-BAND-B           PIC S9(4)V99 COMP-3
                                                 VALUE 0500.00.
           05  WS-CUT-BAND-C           PIC S9(4)V99 COMP-3
                                                 VALUE 0800.00.
           05  WS-SCALE-FACTOR         PIC S9(3)V9(5) COMP-3
                                                 VALUE 1.00000.
      *
      ******************************************************************
      * COMPONENT WORK AREAS - EVERY INTERMEDIATE IS PACKED            *
      ******************************************************************
       01  WS-CALC.
           05  WS-UTIL-PCT             PIC S9(5)V99 COMP-3 VALUE 0.
           05  WS-LIMIT-AMT            PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-DRAWN-AMT            PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-AVAIL-AMT            PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-REQ-AMT              PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-POST-DRAWN           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-RAW-POINTS           PIC S9(6)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-POINTS         PIC S9(6)V99 COMP-3 VALUE 0.
           05  WS-SCORE-WORK           PIC S9(6)V99 COMP-3 VALUE 0.
           05  WS-PD-WORK              PIC S9(3)V9(5) COMP-3 VALUE 0.
           05  WS-TENURE-MONTHS        PIC S9(5) COMP-3 VALUE 0.
           05  WS-DELQ-BUCKET          PIC S9(3) COMP-3 VALUE 0.
           05  WS-WRITTEN-OFF          PIC S9(11)V99 COMP-3 VALUE 0.
      *
      *    THE COMPONENT BEING POSTED BY 3900.  EVERY COMPONENT
      *    PARAGRAPH FILLS THESE THREE FIELDS AND WS-RAW-POINTS.
           05  WS-CP-CODE              PIC X(8)  VALUE SPACES.
           05  WS-CP-RAW               PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-CP-WEIGHT            PIC S9(3)V9(5) COMP-3 VALUE 0.
      *
       01  WS-TIME-AREA.
           05  WS-ABSTIME              PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-DATE-CYMD            PIC X(8)  VALUE SPACES.
           05  WS-TIME-HMS             PIC X(6)  VALUE SPACES.
      *
       01  WS-TODAY                    PIC 9(8)  VALUE ZERO.
       01  WS-TODAY-R REDEFINES WS-TODAY.
           05  WS-TD-CCYY              PIC 9(4).
           05  WS-TD-MM                PIC 9(2).
           05  WS-TD-DD                PIC 9(2).
      *
       01  WS-ONBOARD                  PIC 9(8)  VALUE ZERO.
       01  WS-ONBOARD-R REDEFINES WS-ONBOARD.
           05  WS-OB-CCYY              PIC 9(4).
           05  WS-OB-MM                PIC 9(2).
           05  WS-OB-DD                PIC 9(2).
      *
       01  WS-ISO-DATE                 PIC X(10) VALUE SPACES.
       01  WS-ISO-DATE-R REDEFINES WS-ISO-DATE.
           05  WS-ISO-CCYY             PIC X(4).
           05  FILLER                  PIC X.
           05  WS-ISO-MM               PIC X(2).
           05  FILLER                  PIC X.
           05  WS-ISO-DD               PIC X(2).
      *
       01  WS-COUNTRY-RISK             PIC 9(3)  VALUE ZERO.
       01  WS-CTRY-USED                PIC X(3)  VALUE SPACES.
      *
      ******************************************************************
      * COUNTRY RISK IS A SMALL COMPILED TABLE.  ANYTHING NOT LISTED   *
      * TAKES THE DEFAULT.  THE FULL LIST LIVES ON RSKPARM AS CHARACTER*
      * PARAMETERS AND OVERRIDES THESE WHEN IT IS LOADED.              *
      ******************************************************************
       01  WS-CTRY-CONST.
           05  FILLER                  PIC X(6)  VALUE 'USA010'.
           05  FILLER                  PIC X(6)  VALUE 'GBR010'.
           05  FILLER                  PIC X(6)  VALUE 'CAN015'.
           05  FILLER                  PIC X(6)  VALUE 'DEU015'.
           05  FILLER                  PIC X(6)  VALUE 'FRA015'.
           05  FILLER                  PIC X(6)  VALUE 'MEX045'.
           05  FILLER                  PIC X(6)  VALUE 'BRA050'.
           05  FILLER                  PIC X(6)  VALUE 'NGA080'.
           05  FILLER                  PIC X(6)  VALUE 'RUS095'.
           05  FILLER                  PIC X(6)  VALUE 'IRN099'.
       01  WS-CTRY-TABLE REDEFINES WS-CTRY-CONST.
           05  WS-CTRY-ENTRY OCCURS 10 TIMES
                             INDEXED BY WS-CX.
               10  WS-CT-CODE          PIC X(3).
               10  WS-CT-RISK          PIC 9(3).
       01  WS-CTRY-DEFAULT             PIC 9(3)  VALUE 040.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
       01  HV-AREA.
           05  HV-PARTY-ID             PIC X(11).
           05  HV-ONBOARD-DATE         PIC X(10).
           05  HV-DOMICILE-CTRY        PIC X(3).
           05  HV-PEP-FLG              PIC X(1).
           05  HV-KYC-LEVEL            PIC X(4).
           05  HV-RISK-RATING          PIC X(1).
           05  HV-ENHANCED-DD          PIC X(1).
           05  HV-DELQ-BUCKET          PIC S9(4) COMP.
           05  HV-WRITTEN-OFF          PIC S9(11)V99 COMP-3.
      *
       01  HV-INDICATORS.
           05  IND-DOMICILE            PIC S9(4) COMP.
           05  IND-RISK-RATING         PIC S9(4) COMP.
           05  IND-DELQ-BUCKET         PIC S9(4) COMP.
           05  IND-WRITTEN-OFF         PIC S9(4) COMP.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           COPY CVSCOR01Y.
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
               EXEC CICS ABEND ABCODE('PRR3') NODUMP END-EXEC
           END-IF
      *
           SET ADDRESS OF CV-RISK-AREA TO ADDRESS OF DFHCOMMAREA
      *
           PERFORM 1000-INITIALISE
      *
           IF NOT WS-PARMS-LOADED
               PERFORM 1500-LOAD-PARAMETERS
           END-IF
      *
           PERFORM 2000-READ-PARTY-ATTRIBUTES
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 3000-SCORE-COMPONENTS
               PERFORM 4000-DERIVE-SCORE
               PERFORM 4500-PUBLISH-SCORE
           END-IF
      *
           IF NOT WS-ERROR-FOUND
               PERFORM 5000-LINK-DECISION
           END-IF
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
           MOVE 'N'                    TO WS-BROWSE-SW
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
      *
           INITIALIZE SCORE-RECORD
           MOVE CV-RISK-PARTY-ID       TO SC-PARTY-ID
           MOVE WS-TODAY               TO SC-SCORE-DATE
           MOVE WS-TIME-HMS            TO SC-SCORE-TIME
           MOVE WS-MODEL-ID            TO SC-MODEL-ID
           MOVE WS-MODEL-VERSION       TO SC-MODEL-VERSION
           MOVE ZERO                   TO SC-COMPONENT-CNT
           MOVE WS-PGM-ID              TO SC-SCORED-BY
           MOVE 'N'                    TO SC-OVERRIDE-FLG
      *
           MOVE CV-RISK-PARTY-ID       TO HV-PARTY-ID
      *
           MOVE CV-RISK-EXPOSURE-AMT   TO WS-DRAWN-AMT
           MOVE CV-RISK-AVAIL-AMT      TO WS-AVAIL-AMT
           MOVE CV-RISK-REQ-AMT        TO WS-REQ-AMT
           COMPUTE WS-LIMIT-AMT = WS-DRAWN-AMT + WS-AVAIL-AMT
           COMPUTE WS-POST-DRAWN = WS-DRAWN-AMT + WS-REQ-AMT
           .
      *
      ******************************************************************
      * 1500 - LOAD THE MODEL PARAMETERS                               *
      *                                                                *
      * GENERIC BROWSE ON THE MODEL ID.  ONLY PARAMETERS EFFECTIVE     *
      * TODAY ARE TAKEN - THE FILE CARRIES FUTURE DATED ROWS FROM THE  *
      * MOMENT THE MODEL RISK TEAM APPROVE THE NEXT CALIBRATION.       *
      ******************************************************************
       1500-LOAD-PARAMETERS.
           MOVE ZERO                   TO WS-PARM-CNT
           MOVE WS-MODEL-ID            TO WS-BK-MODEL-ID
           MOVE LOW-VALUES             TO WS-BK-PARM-CODE
      *
           EXEC CICS STARTBR
                     FILE(WS-PARM-FILE)
                     RIDFLD(WS-BROWSE-KEY)
                     KEYLENGTH(8)
                     GENERIC
                     GTEQ
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 'Y'            TO WS-BROWSE-SW
               WHEN DFHRESP(NOTFND)
      *            NO CALIBRATION ON FILE.  THE COMPILED FALL BACK
      *            WEIGHTS ARE USED AND THE RESULT IS A WARNING.
                   PERFORM 1900-PARM-FALLBACK
                   GO TO 1500-EXIT
               WHEN OTHER
                   MOVE '1500-LOAD-PARAMETERS'
                                       TO ER-PARAGRAPH
                   PERFORM 8200-VSAM-ERROR
                   PERFORM 1900-PARM-FALLBACK
                   GO TO 1500-EXIT
           END-EVALUATE
      *
           PERFORM UNTIL WS-RESP NOT = DFHRESP(NORMAL)
                      OR WS-PARM-CNT >= WS-MAX-PARMS
               MOVE 120                TO WS-PARM-LENGTH
               EXEC CICS READNEXT
                         FILE(WS-PARM-FILE)
                         INTO(WS-PARM-BUFFER)
                         LENGTH(WS-PARM-LENGTH)
                         RIDFLD(WS-BROWSE-KEY)
                         KEYLENGTH(16)
                         RESP(WS-RESP)
                         RESP2(WS-RESP2)
               END-EXEC
      *
               IF WS-RESP = DFHRESP(NORMAL)
                   MOVE WS-PARM-BUFFER TO MODEL-PARM-RECORD
                   IF MP-MODEL-ID NOT = WS-MODEL-ID
                       MOVE DFHRESP(ENDFILE)
                                       TO WS-RESP
                   ELSE
                       PERFORM 1600-ACCEPT-PARAMETER
                   END-IF
               END-IF
           END-PERFORM
      *
           IF WS-BROWSE-STARTED
               EXEC CICS ENDBR
                         FILE(WS-PARM-FILE)
                         RESP(WS-RESP)
               END-EXEC
               MOVE 'N'                TO WS-BROWSE-SW
           END-IF
      *
           IF WS-PARM-CNT = ZERO
               PERFORM 1900-PARM-FALLBACK
           ELSE
               PERFORM 1700-APPLY-PARAMETERS
               MOVE 'Y'                TO WS-PARM-CACHE-FLG
           END-IF
           .
       1500-EXIT.
           EXIT
           .
      *
       1600-ACCEPT-PARAMETER.
           IF MP-EFF-DATE > WS-TODAY
               GO TO 1600-EXIT
           END-IF
      *
           IF MP-EXP-DATE NOT = ZERO
              AND MP-EXP-DATE < WS-TODAY
               GO TO 1600-EXIT
           END-IF
      *
           ADD 1                       TO WS-PARM-CNT
           SET WS-PX                   TO WS-PARM-CNT
           MOVE MP-PARM-CODE           TO WS-PE-CODE(WS-PX)
           MOVE MP-PARM-TYPE           TO WS-PE-TYPE(WS-PX)
           MOVE MP-NUM-VALUE           TO WS-PE-NUM(WS-PX)
           MOVE MP-CHAR-VALUE          TO WS-PE-CHAR(WS-PX)
           .
       1600-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 1700 - MOVE THE LOADED PARAMETERS INTO THE WEIGHT SET          *
      ******************************************************************
       1700-APPLY-PARAMETERS.
           PERFORM VARYING WS-PX FROM 1 BY 1
                     UNTIL WS-PX > WS-PARM-CNT
               EVALUATE WS-PE-CODE(WS-PX)
                   WHEN 'WGTUTIL '
                       MOVE WS-PE-NUM(WS-PX)   TO WS-WT-UTIL
                   WHEN 'WGTDELQ '
                       MOVE WS-PE-NUM(WS-PX)   TO WS-WT-DELQ
                   WHEN 'WGTEXPO '
                       MOVE WS-PE-NUM(WS-PX)   TO WS-WT-EXPO
                   WHEN 'WGTKYC  '
                       MOVE WS-PE-NUM(WS-PX)   TO WS-WT-KYC
                   WHEN 'WGTCTRY '
                       MOVE WS-PE-NUM(WS-PX)   TO WS-WT-CTRY
                   WHEN 'WGTTENR '
                       MOVE WS-PE-NUM(WS-PX)   TO WS-WT-TENURE
                   WHEN 'CUTBANDA'
                       MOVE WS-PE-NUM(WS-PX)   TO WS-CUT-BAND-A
                   WHEN 'CUTBANDB'
                       MOVE WS-PE-NUM(WS-PX)   TO WS-CUT-BAND-B
                   WHEN 'CUTBANDC'
                       MOVE WS-PE-NUM(WS-PX)   TO WS-CUT-BAND-C
                   WHEN 'SCALE   '
                       MOVE WS-PE-NUM(WS-PX)   TO WS-SCALE-FACTOR
                   WHEN 'MODLVERS'
                       MOVE WS-PE-NUM(WS-PX)   TO WS-MODEL-VERSION
                       MOVE WS-MODEL-VERSION   TO SC-MODEL-VERSION
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
      *
           IF WS-SCALE-FACTOR = ZERO
               MOVE 1.00000            TO WS-SCALE-FACTOR
           END-IF
           .
      *
       1900-PARM-FALLBACK.
           MOVE 'W'                    TO ER-SEVERITY
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE 'MDL1'                 TO ER-REASON-CD
           MOVE WS-PARM-FILE           TO ER-FILE-NAME
           MOVE '1900-PARM-FALLBACK'   TO ER-PARAGRAPH
           MOVE 'MODEL PARAMETERS NOT LOADED - COMPILED SET IN USE'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
      *
           IF CV-RISK-RC < 0004
               MOVE 0004               TO CV-RISK-RC
           END-IF
           .
      *
      ******************************************************************
      * 2000 - PARTY ATTRIBUTES NEEDED BY THE MODEL                    *
      ******************************************************************
       2000-READ-PARTY-ATTRIBUTES.
           EXEC SQL
               SELECT CHAR(ONBOARD_DATE, ISO)
                    , DOMICILE_CTRY
                    , PEP_FLG
                 INTO :HV-ONBOARD-DATE
                    , :HV-DOMICILE-CTRY :IND-DOMICILE
                    , :HV-PEP-FLG
                 FROM PARTYRSK.CUSTOMER
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND CUST_STATUS <> 'X'
                ORDER BY ONBOARD_DATE
                FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE HV-ONBOARD-DATE
                                       TO WS-ISO-DATE
                   MOVE WS-ISO-CCYY    TO WS-OB-CCYY
                   MOVE WS-ISO-MM      TO WS-OB-MM
                   MOVE WS-ISO-DD      TO WS-OB-DD
               WHEN +100
                   MOVE ZERO           TO WS-ONBOARD
                   MOVE SPACES         TO HV-DOMICILE-CTRY
                   MOVE 'N'            TO HV-PEP-FLG
               WHEN OTHER
                   MOVE 'CUSTOMER          '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '2000-READ-PARTY-ATTRIBUTES'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           EXEC SQL
               SELECT KYC_LEVEL
                    , RISK_RATING
                    , ENHANCED_DD_FLG
                 INTO :HV-KYC-LEVEL
                    , :HV-RISK-RATING  :IND-RISK-RATING
                    , :HV-ENHANCED-DD
                 FROM PARTYRSK.PARTY_KYC
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND KYC_SEQ =
                      (SELECT MAX(KYC_SEQ)
                         FROM PARTYRSK.PARTY_KYC
                        WHERE PARTY_ID = :HV-PARTY-ID)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF IND-RISK-RATING < ZERO
                       MOVE SPACE      TO HV-RISK-RATING
                   END-IF
               WHEN +100
                   MOVE SPACES         TO HV-KYC-LEVEL
                   MOVE SPACE          TO HV-RISK-RATING
                   MOVE 'N'            TO HV-ENHANCED-DD
               WHEN OTHER
                   MOVE 'PARTY_KYC         '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '2000-READ-PARTY-ATTRIBUTES'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 3000 - THE SIX COMPONENTS                                      *
      ******************************************************************
       3000-SCORE-COMPONENTS.
           MOVE ZERO                   TO WS-TOTAL-POINTS
      *
           PERFORM 3100-COMPONENT-UTILISATION
           PERFORM 3200-COMPONENT-DELINQUENCY
           PERFORM 3300-COMPONENT-EXPOSURE
           PERFORM 3400-COMPONENT-KYC
           PERFORM 3500-COMPONENT-COUNTRY
           PERFORM 3600-COMPONENT-TENURE
           .
      *
      ******************************************************************
      * 3100 - UTILISATION AFTER THE REQUESTED AMOUNT                  *
      ******************************************************************
       3100-COMPONENT-UTILISATION.
           IF WS-LIMIT-AMT > ZERO
               COMPUTE WS-UTIL-PCT ROUNDED =
                       (WS-POST-DRAWN * 100) / WS-LIMIT-AMT
           ELSE
      *        NO LIMIT ON FILE.  AN UNLIMITED PARTY IS TREATED AS
      *        FULLY DRAWN RATHER THAN AS UNUSED HEADROOM.
               MOVE 100.00             TO WS-UTIL-PCT
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-UTIL-PCT <= 30.00
                   MOVE 050.00         TO WS-RAW-POINTS
               WHEN WS-UTIL-PCT <= 60.00
                   MOVE 200.00         TO WS-RAW-POINTS
               WHEN WS-UTIL-PCT <= 85.00
                   MOVE 450.00         TO WS-RAW-POINTS
               WHEN WS-UTIL-PCT <= 100.00
                   MOVE 700.00         TO WS-RAW-POINTS
               WHEN OTHER
                   MOVE 950.00         TO WS-RAW-POINTS
           END-EVALUATE
      *
           MOVE 'UTILPCT '             TO WS-CP-CODE
           MOVE WS-UTIL-PCT            TO WS-CP-RAW
           MOVE WS-WT-UTIL             TO WS-CP-WEIGHT
           PERFORM 3900-POST-COMPONENT
           .
      *
      ******************************************************************
      * 3200 - DELINQUENCY                                             *
      *        THE WORST BUCKET ACROSS THE PRODUCT SYSTEMS IS THE      *
      *        SINGLE STRONGEST PREDICTOR IN THE MODEL.                *
      ******************************************************************
       3200-COMPONENT-DELINQUENCY.
           PERFORM 3250-READ-DELINQUENCY
      *
           EVALUATE WS-DELQ-BUCKET
               WHEN 0
                   MOVE 000.00         TO WS-RAW-POINTS
               WHEN 1
                   MOVE 250.00         TO WS-RAW-POINTS
               WHEN 2
                   MOVE 480.00         TO WS-RAW-POINTS
               WHEN 3
                   MOVE 700.00         TO WS-RAW-POINTS
               WHEN 4
                   MOVE 850.00         TO WS-RAW-POINTS
               WHEN OTHER
                   MOVE 999.00         TO WS-RAW-POINTS
           END-EVALUATE
      *
      *    ANY WRITE OFF HISTORY FLOORS THE COMPONENT AT THE FOUR
      *    BUCKET LEVEL WHATEVER THE CURRENT POSITION LOOKS LIKE.
           IF WS-WRITTEN-OFF > ZERO
               IF WS-RAW-POINTS < 850.00
                   MOVE 850.00         TO WS-RAW-POINTS
               END-IF
           END-IF
      *
           MOVE 'DELQBKT '             TO WS-CP-CODE
           MOVE WS-DELQ-BUCKET         TO WS-CP-RAW
           MOVE WS-WT-DELQ             TO WS-CP-WEIGHT
           PERFORM 3900-POST-COMPONENT
           .
      *
       3250-READ-DELINQUENCY.
           MOVE ZERO                   TO WS-DELQ-BUCKET
           MOVE ZERO                   TO WS-WRITTEN-OFF
      *
           EXEC SQL
               SELECT MAX(DELQ_BUCKET)
                    , SUM(WRITTEN_OFF_AMT)
                 INTO :HV-DELQ-BUCKET :IND-DELQ-BUCKET
                    , :HV-WRITTEN-OFF :IND-WRITTEN-OFF
                 FROM PARTYRSK.PARTY_EXPOSURE
                WHERE PARTY_ID = :HV-PARTY-ID
                  AND AS_OF_DATE =
                      (SELECT MAX(AS_OF_DATE)
                         FROM PARTYRSK.PARTY_EXPOSURE
                        WHERE PARTY_ID = :HV-PARTY-ID)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF IND-DELQ-BUCKET NOT < ZERO
                       MOVE HV-DELQ-BUCKET
                                       TO WS-DELQ-BUCKET
                   END-IF
                   IF IND-WRITTEN-OFF NOT < ZERO
                       MOVE HV-WRITTEN-OFF
                                       TO WS-WRITTEN-OFF
                   END-IF
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   MOVE 'PARTY_EXPOSURE    '
                                       TO ER-SQL-TABLE
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE '3250-READ-DELINQUENCY'
                                       TO ER-PARAGRAPH
                   PERFORM 8000-SQL-ERROR
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3300 - EXPOSURE AGAINST THE LIMIT AND AGAINST THE REQUEST      *
      ******************************************************************
       3300-COMPONENT-EXPOSURE.
           EVALUATE TRUE
               WHEN WS-LIMIT-AMT = ZERO
                   MOVE 600.00         TO WS-RAW-POINTS
               WHEN WS-POST-DRAWN <= WS-LIMIT-AMT
                   COMPUTE WS-SCORE-WORK ROUNDED =
                           (WS-POST-DRAWN * 500) / WS-LIMIT-AMT
                   MOVE WS-SCORE-WORK  TO WS-RAW-POINTS
               WHEN OTHER
      *            OVER LIMIT ONCE THE REQUEST IS INCLUDED.  THE
      *            OVERSHOOT IS SCALED SO A SMALL BREACH DOES NOT
      *            SCORE THE SAME AS A LARGE ONE.
                   COMPUTE WS-SCORE-WORK ROUNDED =
                           500 + (((WS-POST-DRAWN - WS-LIMIT-AMT)
                                   * 400) / WS-LIMIT-AMT)
                   IF WS-SCORE-WORK > 999.00
                       MOVE 999.00     TO WS-RAW-POINTS
                   ELSE
                       MOVE WS-SCORE-WORK
                                       TO WS-RAW-POINTS
                   END-IF
           END-EVALUATE
      *
           MOVE 'EXPOLIM '             TO WS-CP-CODE
           MOVE WS-POST-DRAWN          TO WS-CP-RAW
           MOVE WS-WT-EXPO             TO WS-CP-WEIGHT
           PERFORM 3900-POST-COMPONENT
           .
      *
      ******************************************************************
      * 3400 - KYC STANDING                                            *
      ******************************************************************
       3400-COMPONENT-KYC.
           EVALUATE TRUE
               WHEN CV-RISK-KYC-FAILED
                   MOVE 999.00         TO WS-RAW-POINTS
               WHEN CV-RISK-KYC-EXPIRED
                   MOVE 600.00         TO WS-RAW-POINTS
               WHEN CV-RISK-KYC-PENDING
                   MOVE 400.00         TO WS-RAW-POINTS
               WHEN CV-RISK-KYC-OK
                   MOVE 050.00         TO WS-RAW-POINTS
               WHEN OTHER
                   MOVE 300.00         TO WS-RAW-POINTS
           END-EVALUATE
      *
      *    SIMPLIFIED DUE DILIGENCE CARRIES LESS COMFORT THAN A FULL
      *    OR ENHANCED REVIEW, EVEN WHEN THE STATUS IS SATISFACTORY.
           EVALUATE HV-KYC-LEVEL
               WHEN 'SDD '
                   ADD 100.00          TO WS-RAW-POINTS
               WHEN 'EDD '
                   SUBTRACT 50.00      FROM WS-RAW-POINTS
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF HV-PEP-FLG = 'Y'
               ADD 120.00              TO WS-RAW-POINTS
           END-IF
      *
           IF CV-RISK-SANCTION-FLG = 'P'
               ADD 200.00              TO WS-RAW-POINTS
           END-IF
      *
           IF WS-RAW-POINTS > 999.00
               MOVE 999.00             TO WS-RAW-POINTS
           END-IF
           IF WS-RAW-POINTS < ZERO
               MOVE ZERO               TO WS-RAW-POINTS
           END-IF
      *
           MOVE 'KYCSTAT '             TO WS-CP-CODE
           MOVE WS-RAW-POINTS          TO WS-CP-RAW
           MOVE WS-WT-KYC              TO WS-CP-WEIGHT
           PERFORM 3900-POST-COMPONENT
           .
      *
      ******************************************************************
      * 3500 - COUNTRY RISK                                            *
      *        THE TRANSACTION COUNTRY IS USED WHEN THE ACQUIRER SENT  *
      *        ONE, OTHERWISE THE PARTY DOMICILE.                      *
      ******************************************************************
       3500-COMPONENT-COUNTRY.
           MOVE WS-CTRY-DEFAULT        TO WS-COUNTRY-RISK
           MOVE SPACES                 TO WS-CTRY-USED
      *
           IF CV-RISK-COUNTRY NOT = SPACES
               MOVE CV-RISK-COUNTRY    TO WS-CTRY-USED
           ELSE
               IF IND-DOMICILE NOT < ZERO
                   MOVE HV-DOMICILE-CTRY
                                       TO WS-CTRY-USED
               END-IF
           END-IF
      *
           SET WS-CX                   TO 1
           SEARCH WS-CTRY-ENTRY
               AT END
                   MOVE WS-CTRY-DEFAULT
                                       TO WS-COUNTRY-RISK
               WHEN WS-CT-CODE(WS-CX) = WS-CTRY-USED
                   MOVE WS-CT-RISK(WS-CX)
                                       TO WS-COUNTRY-RISK
           END-SEARCH
      *
           COMPUTE WS-RAW-POINTS = WS-COUNTRY-RISK * 10
      *
           MOVE 'CTRYRSK '             TO WS-CP-CODE
           MOVE WS-COUNTRY-RISK        TO WS-CP-RAW
           MOVE WS-WT-CTRY             TO WS-CP-WEIGHT
           PERFORM 3900-POST-COMPONENT
           .
      *
      ******************************************************************
      * 3600 - TENURE                                                  *
      *        MONTHS SINCE ONBOARDING.  A LONG RELATIONSHIP IS THE    *
      *        ONLY COMPONENT THAT CAN PULL THE SCORE DOWN HARD.       *
      ******************************************************************
       3600-COMPONENT-TENURE.
           IF WS-ONBOARD = ZERO
               MOVE ZERO               TO WS-TENURE-MONTHS
           ELSE
               COMPUTE WS-TENURE-MONTHS =
                       ((WS-TD-CCYY - WS-OB-CCYY) * 12)
                     + (WS-TD-MM - WS-OB-MM)
           END-IF
      *
           IF WS-TENURE-MONTHS < ZERO
               MOVE ZERO               TO WS-TENURE-MONTHS
           END-IF
      *
           EVALUATE TRUE
               WHEN WS-TENURE-MONTHS < 6
                   MOVE 800.00         TO WS-RAW-POINTS
               WHEN WS-TENURE-MONTHS < 24
                   MOVE 500.00         TO WS-RAW-POINTS
               WHEN WS-TENURE-MONTHS < 60
                   MOVE 250.00         TO WS-RAW-POINTS
               WHEN WS-TENURE-MONTHS < 120
                   MOVE 120.00         TO WS-RAW-POINTS
               WHEN OTHER
                   MOVE 040.00         TO WS-RAW-POINTS
           END-EVALUATE
      *
           MOVE 'TENURE  '             TO WS-CP-CODE
           MOVE WS-TENURE-MONTHS       TO WS-CP-RAW
           MOVE WS-WT-TENURE           TO WS-CP-WEIGHT
           PERFORM 3900-POST-COMPONENT
           .
      *
      ******************************************************************
      * 3900 - POST ONE COMPONENT INTO THE SCORE RECORD                *
      ******************************************************************
       3900-POST-COMPONENT.
           IF SC-COMPONENT-CNT >= 12
               GO TO 3900-EXIT
           END-IF
      *
           ADD 1                       TO SC-COMPONENT-CNT
           MOVE WS-CP-CODE             TO SC-CP-CODE(SC-COMPONENT-CNT)
           MOVE WS-CP-RAW              TO
                                   SC-CP-RAW-VALUE(SC-COMPONENT-CNT)
           MOVE WS-CP-WEIGHT           TO
                                   SC-CP-WEIGHT(SC-COMPONENT-CNT)
      *
           COMPUTE SC-CP-POINTS(SC-COMPONENT-CNT) ROUNDED =
                   WS-RAW-POINTS * WS-CP-WEIGHT
      *
           ADD SC-CP-POINTS(SC-COMPONENT-CNT)
                                       TO WS-TOTAL-POINTS
           .
       3900-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 4000 - DERIVE THE SCORE AND THE BAND                           *
      ******************************************************************
       4000-DERIVE-SCORE.
           COMPUTE WS-SCORE-WORK ROUNDED =
                   WS-TOTAL-POINTS * WS-SCALE-FACTOR
      *
           IF WS-SCORE-WORK < ZERO
               MOVE ZERO               TO WS-SCORE-WORK
           END-IF
           IF WS-SCORE-WORK > 999.00
               MOVE 999.00             TO WS-SCORE-WORK
           END-IF
      *
           MOVE WS-SCORE-WORK          TO SC-SCORE
      *
           EVALUATE TRUE
               WHEN WS-SCORE-WORK <= WS-CUT-BAND-A
                   MOVE 'A'            TO SC-BAND
               WHEN WS-SCORE-WORK <= WS-CUT-BAND-B
                   MOVE 'B'            TO SC-BAND
               WHEN WS-SCORE-WORK <= WS-CUT-BAND-C
                   MOVE 'C'            TO SC-BAND
               WHEN OTHER
                   MOVE 'X'            TO SC-BAND
           END-EVALUATE
      *
      *    A FAILED KYC REVIEW CANNOT SIT IN THE TWO BEST BANDS
      *    WHATEVER THE ARITHMETIC PRODUCED.
           IF CV-RISK-KYC-FAILED
               IF SC-BAND = 'A' OR SC-BAND = 'B'
                   MOVE 'C'            TO SC-BAND
               END-IF
           END-IF
      *
      *    PROBABILITY OF DEFAULT IS PUBLISHED FOR THE CAPITAL FEED.
      *    THE CURVE IS LINEAR IN THIS MODEL VERSION.
           COMPUTE WS-PD-WORK ROUNDED =
                   (WS-SCORE-WORK * 0.02500) / 100
           MOVE WS-PD-WORK             TO SC-PD-PCT
           .
      *
      ******************************************************************
      * 4500 - PUBLISH INTO THE CROSS MODULE AREA                      *
      ******************************************************************
       4500-PUBLISH-SCORE.
           MOVE SC-SCORE               TO CV-RISK-SCORE
           MOVE SC-BAND                TO CV-RISK-BAND
           MOVE SC-MODEL-ID            TO CV-RISK-MODEL-ID
           MOVE SC-SCORE-DATE          TO CV-RISK-SCORE-DATE
      *
           IF SC-BAND = 'X'
               IF CV-RISK-RC < 0004
                   MOVE 0004           TO CV-RISK-RC
               END-IF
           END-IF
           .
      *
      ******************************************************************
      * 5000 - CONTINUE INTO THE DECISION PROGRAM                      *
      *                                                                *
      * THE SCORE RECORD ITSELF STAYS IN THIS PROGRAM.  THE DECISION   *
      * PROGRAM REBUILDS WHAT IT NEEDS FROM THE COMMAREA - THE TWO     *
      * WERE SPLIT SO THE SCORING LOGIC COULD BE RECOMPILED WITHOUT    *
      * REBINDING THE AUDIT INSERTS.                                   *
      ******************************************************************
       5000-LINK-DECISION.
           EXEC CICS LINK
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CV-RISK-AREA)
                     LENGTH(WS-COMMAREA-LEN)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 0012               TO CV-RISK-RC
               MOVE WS-PGM-ID          TO CV-RISK-FAIL-PGM
               MOVE 'LNK5'             TO CV-RISK-REASON-CD
               MOVE 'DECISION MODULE COULD NOT BE LINKED'
                                       TO CV-RISK-REASON-TXT
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE 'F'                TO ER-SEVERITY
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE WS-RESP2           TO ER-EIBRESP2
               MOVE '5000-LINK-DECISION'
                                       TO ER-PARAGRAPH
               MOVE CV-RISK-REASON-TXT TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
           END-IF
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
      * 8000 / 8200 / 9000 - DIAGNOSTICS                               *
      ******************************************************************
       8000-SQL-ERROR.
           MOVE 'Y'                    TO WS-ERROR-SW
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'SCORING INPUT READ FAILED'
                                       TO ER-MESSAGE
      *
           MOVE SQLCODE                TO CV-RISK-SQLCODE
           MOVE 0012                   TO CV-RISK-RC
           MOVE WS-PGM-ID              TO CV-RISK-FAIL-PGM
           MOVE 'SQL4'                 TO CV-RISK-REASON-CD
           MOVE 'RISK SCORE COULD NOT BE CALCULATED'
                                       TO CV-RISK-REASON-TXT
           PERFORM 9000-REPORT-ERROR
           .
      *
       8200-VSAM-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE 'W'                    TO ER-SEVERITY
           MOVE WS-PARM-FILE           TO ER-FILE-NAME
           MOVE WS-RESP                TO ER-VSAM-RC
           MOVE WS-BROWSE-KEY          TO ER-VSAM-KEY(1:16)
           MOVE 'MODEL PARAMETER FILE COULD NOT BE BROWSED'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
      *
           IF WS-BROWSE-STARTED
               EXEC CICS ENDBR
                         FILE(WS-PARM-FILE)
                         RESP(WS-RESP)
               END-EXEC
               MOVE 'N'                TO WS-BROWSE-SW
           END-IF
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
