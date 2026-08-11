      ******************************************************************
      * PRBRSK4 - BATCH RISK RESCORE                                   *
      *                                                                *
      * FOURTH PROGRAM OF THE BATCH RECALCULATION CHAIN.               *
      *                                                                *
      * RESCORES THE PARTY AGAINST THE CURRENT MODEL AND FILLS         *
      * SCORE-RECORD.  THE MODEL PARAMETERS ARE READ FROM VSAM         *
      * RSKPARM ONCE PER RUN UNIT AND HELD IN A TABLE - A WEEKLY       *
      * PORTFOLIO RESCORE OF SEVERAL MILLION PARTIES CANNOT AFFORD     *
      * TO RE-READ THEM PER PARTY.  THE PARAMETER SET IS THEREFORE     *
      * FIXED FOR THE LIFE OF THE STEP AND A MID RUN PARAMETER         *
      * CHANGE IS NOT PICKED UP UNTIL THE NEXT RESTART.                *
      *                                                                *
      * CALLED BY   - PRBRSK3                                          *
      * CALLS       - PRBRSK5                                          *
      * FILES       - RSKPARM   PRTY.PROD.RSKPARM   INPUT              *
      *                                                                *
      * RETURN CODES                                                   *
      *   00 - SCORED                                                  *
      *   04 - SCORED ON DEFAULT PARAMETERS OR PARTIAL COMPONENT SET   *
      *   08 - BAND X - REFUSE                                         *
      *   12 - FATAL - MODEL PARAMETERS UNUSABLE                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRBRSK4.
       AUTHOR.        PARTY AND RISK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RSKPARM-FILE ASSIGN TO RSKPARM
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS MPF-KEY
                  FILE STATUS  IS WS-PARM-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  RSKPARM-FILE
           RECORD CONTAINS 120 CHARACTERS.
       01  RSKPARM-REC.
           05  MPF-KEY.
               10  MPF-MODEL-ID        PIC X(8).
               10  MPF-PARM-CODE       PIC X(8).
           05  MPF-REST                PIC X(104).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'PRBRSK4 '.
       01  WS-PARAGRAPH                PIC X(30) VALUE SPACES.
      *
       01  WS-SWITCHES.
           05  WS-PARMS-LOADED-SW      PIC X     VALUE 'N'.
               88  WS-PARMS-LOADED               VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X     VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-FATAL-SW             PIC X     VALUE 'N'.
               88  WS-FATAL                      VALUE 'Y'.
           05  WS-DEFAULTED-SW         PIC X     VALUE 'N'.
               88  WS-DEFAULTED                  VALUE 'Y'.
      *
       01  WS-PARM-STATUS              PIC X(2)  VALUE '00'.
      *
       01  WS-COUNTERS.
           05  WS-OWN-RC               PIC 9(4)  VALUE ZERO.
           05  WS-CHAIN-RC             PIC 9(4)  VALUE ZERO.
           05  WS-WORST-RC             PIC 9(4)  VALUE ZERO.
           05  WS-SUB                  PIC S9(4) COMP VALUE ZERO.
           05  WS-CP                   PIC S9(4) COMP VALUE ZERO.
           05  WS-PARM-CNT             PIC S9(4) COMP VALUE ZERO.
           05  WS-SCORED-CNT           PIC 9(9)  VALUE ZERO.
           05  WS-ABEND-CODE           PIC S9(4) COMP VALUE ZERO.
      *
      ******************************************************************
      * MODEL PARAMETER TABLE.  LOADED ONCE, KEYED BY PARM CODE.       *
      ******************************************************************
       01  WS-MODEL-CONTROL.
           05  WS-MODEL-ID             PIC X(8)  VALUE 'PRSKB004'.
           05  WS-MODEL-VERSION        PIC 9(4)  VALUE 0004.
           05  WS-PARM-TAB-MAX         PIC S9(4) COMP VALUE 40.
      *
       01  WS-PARM-TABLE.
           05  WS-PARM OCCURS 40 TIMES.
               10  WS-PM-CODE          PIC X(8).
               10  WS-PM-TYPE          PIC X(4).
               10  WS-PM-NUM           PIC S9(9)V9(5) COMP-3.
               10  WS-PM-CHAR          PIC X(20).
      *
      ******************************************************************
      * WEIGHTS AND CUTOFFS.  DEFAULTS ARE THE 2016 MODEL AND ARE      *
      * ONLY USED IF THE PARAMETER FILE IS SHORT.                      *
      ******************************************************************
       01  WS-WEIGHTS.
           05  WS-WT-UTIL              PIC S9(3)V9(5) COMP-3
                                                     VALUE 1.75000.
           05  WS-WT-DELQ              PIC S9(3)V9(5) COMP-3
                                                     VALUE 12.50000.
           05  WS-WT-PASTDUE           PIC S9(3)V9(5) COMP-3
                                                     VALUE 0.00450.
           05  WS-WT-KYC               PIC S9(3)V9(5) COMP-3
                                                     VALUE 25.00000.
           05  WS-WT-PEP               PIC S9(3)V9(5) COMP-3
                                                     VALUE 40.00000.
           05  WS-WT-TENURE            PIC S9(3)V9(5) COMP-3
                                                     VALUE 0.75000.
           05  WS-WT-PRODCNT           PIC S9(3)V9(5) COMP-3
                                                     VALUE 5.00000.
           05  WS-WT-COUNTRY           PIC S9(3)V9(5) COMP-3
                                                     VALUE 30.00000.
      *
       01  WS-CUTOFFS.
           05  WS-CUT-BAND-A           PIC 9(3)  VALUE 200.
           05  WS-CUT-BAND-B           PIC 9(3)  VALUE 450.
           05  WS-CUT-BAND-C           PIC 9(3)  VALUE 700.
           05  WS-BASE-SCORE           PIC 9(3)  VALUE 120.
           05  WS-SCALE-FACTOR         PIC S9(3)V9(5) COMP-3
                                                     VALUE 1.00000.
      *
       01  WS-SCORE-WORK.
           05  WS-RAW-POINTS           PIC S9(7)V99 COMP-3 VALUE ZERO.
           05  WS-POINTS               PIC S9(4)V99 COMP-3 VALUE ZERO.
           05  WS-TOTAL-POINTS         PIC S9(7)V99 COMP-3 VALUE ZERO.
           05  WS-SCALED-SCORE         PIC S9(7)V99 COMP-3 VALUE ZERO.
           05  WS-FINAL-SCORE          PIC 9(3)  VALUE ZERO.
           05  WS-PD-WORK              PIC S9(3)V9(5) COMP-3
                                                     VALUE ZERO.
           05  WS-UTIL-PCT             PIC S9(5)V99 COMP-3 VALUE ZERO.
           05  WS-MAX-DELQ             PIC 9(1)  VALUE ZERO.
           05  WS-TENURE-YEARS         PIC S9(3)  VALUE ZERO.
      *
       01  WS-DATE-WORK.
           05  WS-CURR-DATE            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-DATE-R REDEFINES WS-CURR-DATE.
               10  WS-CURR-CCYY        PIC 9(4).
               10  WS-CURR-MMDD        PIC 9(4).
           05  WS-CURR-TIME            PIC 9(8)  VALUE ZERO.
           05  WS-CURR-TIME-R REDEFINES WS-CURR-TIME.
               10  WS-CURR-HHMMSS      PIC 9(6).
               10  WS-CURR-HUND        PIC 9(2).
           05  WS-ONBOARD-CCYY         PIC 9(4)  VALUE ZERO.
      *
      *    HIGH RISK DOMICILES.  MAINTAINED THROUGH RSKPARM ENTRY
      *    CTRYLIST BUT DEFAULTED HERE FOR A COLD START.
       01  WS-COUNTRY-LIST.
           05  FILLER                  PIC X(24)
               VALUE 'IRNPRKSYRCUBMMRAFGYEMSDN'.
       01  WS-COUNTRY-TAB REDEFINES WS-COUNTRY-LIST.
           05  WS-HR-COUNTRY OCCURS 8 TIMES PIC X(3).
      *
           COPY CVSCOR01Y.
      *
           COPY CVERRS01Y.
      *
           COPY CVCONSTY.
      *
       LINKAGE SECTION.
      *
           COPY CVRISK01Y.
      *
           COPY CVPARTY1Y.
      *
           COPY CVEXPO01Y.
      *
       01  LK-RETURN-AREA.
           05  LK-RETURN-CD            PIC S9(4) COMP.
           05  LK-RETURN-PGM           PIC X(8).
           05  LK-RETURN-MSG           PIC X(60).
      *
      ******************************************************************
       PROCEDURE DIVISION USING CV-RISK-AREA
                                PARTY-RECORD
                                EXPOSURE-RECORD
                                LK-RETURN-AREA.
      *
       0000-MAIN-LINE.
           PERFORM 0100-INITIALISE
      *
           IF NOT WS-PARMS-LOADED
               PERFORM 1000-LOAD-PARAMETERS
           END-IF
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
      *
           PERFORM 2000-BUILD-COMPONENTS
           PERFORM 3000-COMPUTE-SCORE
           PERFORM 4000-ASSIGN-BAND
           PERFORM 5000-CALL-PERSIST
           .
       0000-RETURN.
           PERFORM 8000-APPEND-HOP
           IF WS-WORST-RC > CV-RISK-RC
               MOVE WS-WORST-RC        TO CV-RISK-RC
           END-IF
           MOVE WS-WORST-RC            TO LK-RETURN-CD
           IF WS-OWN-RC >= WS-CHAIN-RC
               MOVE WS-PROGRAM-ID      TO LK-RETURN-PGM
           END-IF
           GOBACK
           .
      *
       0100-INITIALISE.
           MOVE '0100-INITIALISE'      TO WS-PARAGRAPH
           MOVE 'N'                    TO WS-FATAL-SW
           MOVE ZERO                   TO WS-OWN-RC
           MOVE ZERO                   TO WS-CHAIN-RC
           MOVE ZERO                   TO WS-WORST-RC
           MOVE ZERO                   TO WS-TOTAL-POINTS
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
      *
           ACCEPT WS-CURR-DATE         FROM DATE YYYYMMDD
           ACCEPT WS-CURR-TIME         FROM TIME
      *
           MOVE SPACES                 TO SCORE-RECORD
           MOVE PT-PARTY-ID            TO SC-PARTY-ID
           MOVE WS-CURR-DATE           TO SC-SCORE-DATE
           MOVE WS-CURR-HHMMSS         TO SC-SCORE-TIME
           MOVE WS-MODEL-ID            TO SC-MODEL-ID
           MOVE WS-MODEL-VERSION       TO SC-MODEL-VERSION
           MOVE ZERO                   TO SC-SCORE
           MOVE ZERO                   TO SC-PD-PCT
           MOVE ZERO                   TO SC-COMPONENT-CNT
           MOVE WS-PROGRAM-ID          TO SC-SCORED-BY
           MOVE 'N'                    TO SC-OVERRIDE-FLG
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 12
               MOVE SPACES             TO SC-CP-CODE(WS-SUB)
               MOVE ZERO               TO SC-CP-RAW-VALUE(WS-SUB)
               MOVE ZERO               TO SC-CP-WEIGHT(WS-SUB)
               MOVE ZERO               TO SC-CP-POINTS(WS-SUB)
           END-PERFORM
           .
      *
      ******************************************************************
      * 1000 - LOAD MODEL PARAMETERS                                   *
      ******************************************************************
       1000-LOAD-PARAMETERS.
           MOVE '1000-LOAD-PARAMETERS' TO WS-PARAGRAPH
           MOVE ZERO                   TO WS-PARM-CNT
           MOVE 'N'                    TO WS-PARM-EOF-SW
      *
           OPEN INPUT RSKPARM-FILE
           IF WS-PARM-STATUS NOT = '00'
               MOVE 'RSKPARM '         TO ER-FILE-NAME
               MOVE 'OPEN    '         TO ER-SQL-OPERATION
               PERFORM 9200-VSAM-ERROR
      *        A 35 MEANS THE PARAMETER FILE WAS NOT ALLOCATED AND THE
      *        HARD CODED DEFAULTS STAND IN.  A PHYSICAL ERROR MEANS
      *        THE FILE IS THERE BUT UNREADABLE, AND SCORING A WHOLE
      *        PORTFOLIO ON DEFAULTS IS NOT PERMITTED.
               IF WS-PARM-STATUS NOT = '35'
                   PERFORM 9900-ABEND
               END-IF
               MOVE 'Y'                TO WS-DEFAULTED-SW
               MOVE 4                  TO WS-OWN-RC
               MOVE 'Y'                TO WS-PARMS-LOADED-SW
               DISPLAY 'PRBRSK4  RSKPARM UNAVAILABLE - MODEL '
                       WS-MODEL-ID ' DEFAULTS IN USE'
               GO TO 1000-EXIT
           END-IF
      *
           MOVE WS-MODEL-ID            TO MPF-MODEL-ID
           MOVE LOW-VALUES             TO MPF-PARM-CODE
           START RSKPARM-FILE KEY NOT LESS THAN MPF-KEY
           IF WS-PARM-STATUS NOT = '00'
               MOVE 'Y'                TO WS-PARM-EOF-SW
               MOVE 'Y'                TO WS-DEFAULTED-SW
           END-IF
      *
           PERFORM UNTIL WS-PARM-EOF
                      OR WS-PARM-CNT >= WS-PARM-TAB-MAX
               READ RSKPARM-FILE NEXT RECORD INTO MODEL-PARM-RECORD
               EVALUATE WS-PARM-STATUS
                   WHEN '00'
                       IF MP-MODEL-ID NOT = WS-MODEL-ID
                           MOVE 'Y'    TO WS-PARM-EOF-SW
                       ELSE
                           PERFORM 1100-STORE-PARAMETER
                       END-IF
                   WHEN '10'
                       MOVE 'Y'        TO WS-PARM-EOF-SW
                   WHEN OTHER
                       MOVE 'RSKPARM '  TO ER-FILE-NAME
                       MOVE 'READNEXT' TO ER-SQL-OPERATION
                       PERFORM 9200-VSAM-ERROR
                       MOVE 'Y'        TO WS-PARM-EOF-SW
               END-EVALUATE
           END-PERFORM
      *
           CLOSE RSKPARM-FILE
           IF WS-PARM-STATUS NOT = '00'
               MOVE 'RSKPARM '         TO ER-FILE-NAME
               MOVE 'CLOSE   '         TO ER-SQL-OPERATION
               PERFORM 9200-VSAM-ERROR
           END-IF
      *
           PERFORM 1200-APPLY-PARAMETERS
           MOVE 'Y'                    TO WS-PARMS-LOADED-SW
      *
           DISPLAY 'PRBRSK4  MODEL=' WS-MODEL-ID
                   ' VERSION=' WS-MODEL-VERSION
                   ' PARMS LOADED=' WS-PARM-CNT
           .
       1000-EXIT.
           EXIT
           .
      *
       1100-STORE-PARAMETER.
      *    EFFECTIVE DATED.  SUPERSEDED ROWS STAY ON THE FILE.
           IF MP-EFF-DATE > WS-CURR-DATE
               GO TO 1100-EXIT
           END-IF
           IF MP-EXP-DATE NOT = ZERO
              AND MP-EXP-DATE < WS-CURR-DATE
               GO TO 1100-EXIT
           END-IF
      *
           ADD 1                       TO WS-PARM-CNT
           MOVE MP-PARM-CODE           TO WS-PM-CODE(WS-PARM-CNT)
           MOVE MP-PARM-TYPE           TO WS-PM-TYPE(WS-PARM-CNT)
           MOVE MP-NUM-VALUE           TO WS-PM-NUM(WS-PARM-CNT)
           MOVE MP-CHAR-VALUE          TO WS-PM-CHAR(WS-PARM-CNT)
           .
       1100-EXIT.
           EXIT
           .
      *
       1200-APPLY-PARAMETERS.
           IF WS-PARM-CNT = ZERO
               MOVE 'Y'                TO WS-DEFAULTED-SW
               MOVE 4                  TO WS-OWN-RC
               DISPLAY 'PRBRSK4  NO PARAMETERS FOUND FOR MODEL '
                       WS-MODEL-ID ' - DEFAULTS IN USE'
               GO TO 1200-EXIT
           END-IF
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > WS-PARM-CNT
               EVALUATE WS-PM-CODE(WS-SUB)
                   WHEN 'WTUTIL  '
                       MOVE WS-PM-NUM(WS-SUB) TO WS-WT-UTIL
                   WHEN 'WTDELQ  '
                       MOVE WS-PM-NUM(WS-SUB) TO WS-WT-DELQ
                   WHEN 'WTPASTDU'
                       MOVE WS-PM-NUM(WS-SUB) TO WS-WT-PASTDUE
                   WHEN 'WTKYC   '
                       MOVE WS-PM-NUM(WS-SUB) TO WS-WT-KYC
                   WHEN 'WTPEP   '
                       MOVE WS-PM-NUM(WS-SUB) TO WS-WT-PEP
                   WHEN 'WTTENURE'
                       MOVE WS-PM-NUM(WS-SUB) TO WS-WT-TENURE
                   WHEN 'WTPRODCT'
                       MOVE WS-PM-NUM(WS-SUB) TO WS-WT-PRODCNT
                   WHEN 'WTCTRY  '
                       MOVE WS-PM-NUM(WS-SUB) TO WS-WT-COUNTRY
                   WHEN 'CUTBANDA'
                       MOVE WS-PM-NUM(WS-SUB) TO WS-CUT-BAND-A
                   WHEN 'CUTBANDB'
                       MOVE WS-PM-NUM(WS-SUB) TO WS-CUT-BAND-B
                   WHEN 'CUTBANDC'
                       MOVE WS-PM-NUM(WS-SUB) TO WS-CUT-BAND-C
                   WHEN 'BASESCOR'
                       MOVE WS-PM-NUM(WS-SUB) TO WS-BASE-SCORE
                   WHEN 'SCALE   '
                       MOVE WS-PM-NUM(WS-SUB) TO WS-SCALE-FACTOR
                   WHEN 'CTRYLIST'
                       MOVE WS-PM-CHAR(WS-SUB)
                                       TO WS-COUNTRY-LIST(1:20)
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           .
       1200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2000 - SCORE COMPONENTS                                        *
      *                                                                *
      * EIGHT COMPONENTS ARE BUILT.  EACH CARRIES ITS RAW VALUE, THE   *
      * WEIGHT USED AND THE POINTS AWARDED SO THAT THE COMPONENT       *
      * BREAKDOWN CAN BE RECONSTRUCTED FROM THE SCORE ROW.             *
      ******************************************************************
       2000-BUILD-COMPONENTS.
           MOVE '2000-BUILD-COMPONENTS' TO WS-PARAGRAPH
           MOVE ZERO                   TO WS-CP
           MOVE ZERO                   TO WS-TOTAL-POINTS
      *
           PERFORM 2100-COMP-UTILISATION
           PERFORM 2200-COMP-DELINQUENCY
           PERFORM 2300-COMP-PAST-DUE
           PERFORM 2400-COMP-KYC
           PERFORM 2500-COMP-PEP
           PERFORM 2600-COMP-TENURE
           PERFORM 2700-COMP-PRODUCT-SPREAD
           PERFORM 2800-COMP-COUNTRY
      *
           MOVE WS-CP                  TO SC-COMPONENT-CNT
           .
      *
       2100-COMP-UTILISATION.
           ADD 1                       TO WS-CP
           MOVE 'EXPUTIL '             TO SC-CP-CODE(WS-CP)
           MOVE EX-UTILISATION-PCT     TO WS-UTIL-PCT
           MOVE WS-UTIL-PCT            TO SC-CP-RAW-VALUE(WS-CP)
           MOVE WS-WT-UTIL             TO SC-CP-WEIGHT(WS-CP)
      *
           COMPUTE WS-POINTS ROUNDED = WS-UTIL-PCT * WS-WT-UTIL
               ON SIZE ERROR
                   MOVE 9999.99        TO WS-POINTS
           END-COMPUTE
      *
      *    UTILISATION ABOVE THE LIMIT IS PENALISED TWICE OVER.
           IF WS-UTIL-PCT > 100
               COMPUTE WS-POINTS = WS-POINTS
                     + ((WS-UTIL-PCT - 100) * WS-WT-UTIL)
           END-IF
      *
           MOVE WS-POINTS              TO SC-CP-POINTS(WS-CP)
           ADD WS-POINTS               TO WS-TOTAL-POINTS
           .
      *
       2200-COMP-DELINQUENCY.
           ADD 1                       TO WS-CP
           MOVE 'DELQBUCK'             TO SC-CP-CODE(WS-CP)
      *
           MOVE ZERO                   TO WS-MAX-DELQ
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > EX-PROD-CNT
               IF EX-PROD-DELQ-BUCKET(WS-SUB) > WS-MAX-DELQ
                   MOVE EX-PROD-DELQ-BUCKET(WS-SUB) TO WS-MAX-DELQ
               END-IF
           END-PERFORM
      *
           MOVE WS-MAX-DELQ            TO SC-CP-RAW-VALUE(WS-CP)
           MOVE WS-WT-DELQ             TO SC-CP-WEIGHT(WS-CP)
      *
      *    THE BUCKET SCALE IS NOT LINEAR - THREE CYCLES DOWN IS FAR
      *    WORSE THAN ONE.
           EVALUATE WS-MAX-DELQ
               WHEN 0
                   MOVE ZERO           TO WS-POINTS
               WHEN 1
                   COMPUTE WS-POINTS = WS-WT-DELQ * 1
               WHEN 2
                   COMPUTE WS-POINTS = WS-WT-DELQ * 3
               WHEN 3
                   COMPUTE WS-POINTS = WS-WT-DELQ * 6
               WHEN OTHER
                   COMPUTE WS-POINTS = WS-WT-DELQ * 10
           END-EVALUATE
      *
           MOVE WS-POINTS              TO SC-CP-POINTS(WS-CP)
           ADD WS-POINTS               TO WS-TOTAL-POINTS
           .
      *
       2300-COMP-PAST-DUE.
           ADD 1                       TO WS-CP
           MOVE 'PASTDUE '             TO SC-CP-CODE(WS-CP)
           MOVE EX-PAST-DUE-AMT        TO SC-CP-RAW-VALUE(WS-CP)
           MOVE WS-WT-PASTDUE          TO SC-CP-WEIGHT(WS-CP)
      *
           COMPUTE WS-POINTS ROUNDED =
                   EX-PAST-DUE-AMT * WS-WT-PASTDUE
               ON SIZE ERROR
                   MOVE 9999.99        TO WS-POINTS
           END-COMPUTE
      *
           MOVE WS-POINTS              TO SC-CP-POINTS(WS-CP)
           ADD WS-POINTS               TO WS-TOTAL-POINTS
           .
      *
       2400-COMP-KYC.
           ADD 1                       TO WS-CP
           MOVE 'KYCSTAT '             TO SC-CP-CODE(WS-CP)
           MOVE WS-WT-KYC              TO SC-CP-WEIGHT(WS-CP)
      *
           EVALUATE TRUE
               WHEN CV-RISK-KYC-OK
                   MOVE ZERO           TO WS-POINTS
                   MOVE 0              TO SC-CP-RAW-VALUE(WS-CP)
               WHEN CV-RISK-KYC-PENDING
                   COMPUTE WS-POINTS = WS-WT-KYC * 1
                   MOVE 1              TO SC-CP-RAW-VALUE(WS-CP)
               WHEN CV-RISK-KYC-EXPIRED
                   COMPUTE WS-POINTS = WS-WT-KYC * 2
                   MOVE 2              TO SC-CP-RAW-VALUE(WS-CP)
               WHEN OTHER
                   COMPUTE WS-POINTS = WS-WT-KYC * 4
                   MOVE 4              TO SC-CP-RAW-VALUE(WS-CP)
           END-EVALUATE
      *
           MOVE WS-POINTS              TO SC-CP-POINTS(WS-CP)
           ADD WS-POINTS               TO WS-TOTAL-POINTS
           .
      *
       2500-COMP-PEP.
           ADD 1                       TO WS-CP
           MOVE 'PEPSANC '             TO SC-CP-CODE(WS-CP)
           MOVE WS-WT-PEP              TO SC-CP-WEIGHT(WS-CP)
           MOVE ZERO                   TO WS-POINTS
      *
           IF PT-IS-PEP
               COMPUTE WS-POINTS = WS-POINTS + WS-WT-PEP
               MOVE 1                  TO SC-CP-RAW-VALUE(WS-CP)
           END-IF
      *
           IF CV-RISK-SANCTION-HIT
               COMPUTE WS-POINTS = WS-POINTS + (WS-WT-PEP * 5)
               MOVE 5                  TO SC-CP-RAW-VALUE(WS-CP)
           END-IF
      *
           MOVE WS-POINTS              TO SC-CP-POINTS(WS-CP)
           ADD WS-POINTS               TO WS-TOTAL-POINTS
           .
      *
       2600-COMP-TENURE.
           ADD 1                       TO WS-CP
           MOVE 'TENURE  '             TO SC-CP-CODE(WS-CP)
           MOVE WS-WT-TENURE           TO SC-CP-WEIGHT(WS-CP)
      *
           MOVE PT-ONBOARD-DATE(1:4)   TO WS-ONBOARD-CCYY
           IF WS-ONBOARD-CCYY = ZERO
               MOVE ZERO               TO WS-TENURE-YEARS
           ELSE
               COMPUTE WS-TENURE-YEARS =
                       WS-CURR-CCYY - WS-ONBOARD-CCYY
           END-IF
           IF WS-TENURE-YEARS < ZERO
               MOVE ZERO               TO WS-TENURE-YEARS
           END-IF
           IF WS-TENURE-YEARS > 25
               MOVE 25                 TO WS-TENURE-YEARS
           END-IF
      *
           MOVE WS-TENURE-YEARS        TO SC-CP-RAW-VALUE(WS-CP)
      *    TENURE REDUCES THE SCORE.
           COMPUTE WS-POINTS ROUNDED =
                   0 - (WS-TENURE-YEARS * WS-WT-TENURE)
      *
           MOVE WS-POINTS              TO SC-CP-POINTS(WS-CP)
           ADD WS-POINTS               TO WS-TOTAL-POINTS
           .
      *
       2700-COMP-PRODUCT-SPREAD.
           ADD 1                       TO WS-CP
           MOVE 'PRODCNT '             TO SC-CP-CODE(WS-CP)
           MOVE WS-WT-PRODCNT          TO SC-CP-WEIGHT(WS-CP)
           MOVE EX-PROD-CNT            TO SC-CP-RAW-VALUE(WS-CP)
      *
           IF EX-PROD-CNT > 3
               COMPUTE WS-POINTS =
                       (EX-PROD-CNT - 3) * WS-WT-PRODCNT
           ELSE
               MOVE ZERO               TO WS-POINTS
           END-IF
      *
           MOVE WS-POINTS              TO SC-CP-POINTS(WS-CP)
           ADD WS-POINTS               TO WS-TOTAL-POINTS
           .
      *
       2800-COMP-COUNTRY.
           ADD 1                       TO WS-CP
           MOVE 'COUNTRY '             TO SC-CP-CODE(WS-CP)
           MOVE WS-WT-COUNTRY          TO SC-CP-WEIGHT(WS-CP)
           MOVE ZERO                   TO WS-POINTS
           MOVE ZERO                   TO SC-CP-RAW-VALUE(WS-CP)
      *
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 8
               IF WS-HR-COUNTRY(WS-SUB) = PT-DOMICILE-CTRY
                  OR WS-HR-COUNTRY(WS-SUB) = PT-CITIZENSHIP
                   MOVE WS-WT-COUNTRY  TO WS-POINTS
                   MOVE 1              TO SC-CP-RAW-VALUE(WS-CP)
                   MOVE 8              TO WS-SUB
               END-IF
           END-PERFORM
      *
           MOVE WS-POINTS              TO SC-CP-POINTS(WS-CP)
           ADD WS-POINTS               TO WS-TOTAL-POINTS
           .
      *
      ******************************************************************
      * 3000 - FINAL SCORE                                             *
      ******************************************************************
       3000-COMPUTE-SCORE.
           MOVE '3000-COMPUTE-SCORE'   TO WS-PARAGRAPH
      *
           COMPUTE WS-SCALED-SCORE ROUNDED =
                   (WS-BASE-SCORE + WS-TOTAL-POINTS) * WS-SCALE-FACTOR
               ON SIZE ERROR
                   MOVE 999            TO WS-SCALED-SCORE
           END-COMPUTE
      *
           IF WS-SCALED-SCORE < ZERO
               MOVE ZERO               TO WS-SCALED-SCORE
           END-IF
           IF WS-SCALED-SCORE > 999
               MOVE 999                TO WS-SCALED-SCORE
           END-IF
      *
           MOVE WS-SCALED-SCORE        TO WS-FINAL-SCORE
           MOVE WS-FINAL-SCORE         TO SC-SCORE
           MOVE WS-FINAL-SCORE         TO CV-RISK-SCORE
           MOVE WS-CURR-DATE           TO CV-RISK-SCORE-DATE
           MOVE WS-MODEL-ID            TO CV-RISK-MODEL-ID
      *
      *    PROBABILITY OF DEFAULT, EXPRESSED AS A PERCENTAGE.  THE
      *    CURVE IS APPROXIMATED IN THREE STRAIGHT SEGMENTS - THE
      *    EXPONENTIAL FORM WAS DROPPED WHEN THE MODEL MOVED TO BATCH.
           EVALUATE TRUE
               WHEN WS-FINAL-SCORE < 200
                   COMPUTE WS-PD-WORK ROUNDED =
                           0.10000 + (WS-FINAL-SCORE * 0.00250)
               WHEN WS-FINAL-SCORE < 600
                   COMPUTE WS-PD-WORK ROUNDED =
                           0.60000 + ((WS-FINAL-SCORE - 200) * 0.01500)
               WHEN OTHER
                   COMPUTE WS-PD-WORK ROUNDED =
                           6.60000 + ((WS-FINAL-SCORE - 600) * 0.05000)
           END-EVALUATE
      *
           IF WS-PD-WORK > 99.99999
               MOVE 99.99999           TO WS-PD-WORK
           END-IF
           MOVE WS-PD-WORK             TO SC-PD-PCT
           .
      *
      ******************************************************************
      * 4000 - BAND AND ADVICE                                         *
      ******************************************************************
       4000-ASSIGN-BAND.
           MOVE '4000-ASSIGN-BAND'     TO WS-PARAGRAPH
      *
           EVALUATE TRUE
               WHEN CV-RISK-SANCTION-HIT
                   MOVE 'X'            TO SC-BAND
               WHEN WS-FINAL-SCORE <= WS-CUT-BAND-A
                   MOVE 'A'            TO SC-BAND
               WHEN WS-FINAL-SCORE <= WS-CUT-BAND-B
                   MOVE 'B'            TO SC-BAND
               WHEN WS-FINAL-SCORE <= WS-CUT-BAND-C
                   MOVE 'C'            TO SC-BAND
               WHEN OTHER
                   MOVE 'X'            TO SC-BAND
           END-EVALUATE
      *
           MOVE SC-BAND                TO CV-RISK-BAND
      *
           EVALUATE SC-BAND
               WHEN 'A'
               WHEN 'B'
                   MOVE 'APPR'         TO CV-RISK-ADVICE-CD
               WHEN 'C'
                   MOVE 'REFR'         TO CV-RISK-ADVICE-CD
                   IF WS-OWN-RC < 4
                       MOVE 4          TO WS-OWN-RC
                   END-IF
               WHEN OTHER
                   MOVE 'DECL'         TO CV-RISK-ADVICE-CD
                   IF WS-OWN-RC < 8
                       MOVE 8          TO WS-OWN-RC
                   END-IF
           END-EVALUATE
      *
           IF WS-DEFAULTED
               IF WS-OWN-RC < 4
                   MOVE 4              TO WS-OWN-RC
               END-IF
               MOVE 'MDLD'             TO CV-RISK-REASON-CD
           END-IF
      *
           ADD 1                       TO WS-SCORED-CNT
           MOVE WS-OWN-RC              TO WS-WORST-RC
      *
           DISPLAY 'PRBRSK4  SCORED PARTY=' SC-PARTY-ID
                   ' SCORE=' SC-SCORE
                   ' BAND=' SC-BAND
                   ' MODEL=' SC-MODEL-ID
                   ' COMPONENTS=' SC-COMPONENT-CNT
           .
      *
       5000-CALL-PERSIST.
           MOVE '5000-CALL-PERSIST'    TO WS-PARAGRAPH
           MOVE ZERO                   TO LK-RETURN-CD
      *
           CALL 'PRBRSK5' USING CV-RISK-AREA
                                EXPOSURE-RECORD
                                SCORE-RECORD
                                LK-RETURN-AREA
      *
           MOVE LK-RETURN-CD           TO WS-CHAIN-RC
           IF WS-CHAIN-RC > WS-WORST-RC
               MOVE WS-CHAIN-RC        TO WS-WORST-RC
           END-IF
           .
      *
       8000-APPEND-HOP.
           IF CV-RISK-HOP-CNT < 8
               ADD 1                   TO CV-RISK-HOP-CNT
               MOVE WS-PROGRAM-ID      TO
                    CV-RISK-HOP-PGM(CV-RISK-HOP-CNT)
               MOVE WS-OWN-RC          TO
                    CV-RISK-HOP-RC(CV-RISK-HOP-CNT)
           END-IF
           .
      *
       9200-VSAM-ERROR.
           MOVE WS-PROGRAM-ID          TO ER-PGM-NAME
           MOVE WS-PARAGRAPH           TO ER-PARAGRAPH
           MOVE 'VSAM'                 TO ER-ERROR-TYPE
           MOVE WS-PARM-STATUS         TO ER-FILE-STATUS
           MOVE 'E'                    TO ER-SEVERITY
           MOVE MPF-KEY                TO ER-VSAM-KEY
      *
           DISPLAY 'PRBRSK4  VSAM ERROR FILE=' ER-FILE-NAME
                   ' OP=' ER-SQL-OPERATION
                   ' STATUS=' WS-PARM-STATUS
                   ' KEY=' MPF-KEY
           .
      *
      ******************************************************************
      * 9900 - U3104.  SCORING WITHOUT ANY USABLE PARAMETER SET IS     *
      *        NOT PERMITTED WHEN THE MODEL IS UNDER REGULATORY        *
      *        APPROVAL.                                               *
      ******************************************************************
       9900-ABEND.
           MOVE 'U310'                 TO ER-ABEND-CODE
           MOVE 'Y'                    TO ER-ABEND-REQUESTED
           DISPLAY 'PRBRSK4  ABEND U3104 MODEL=' WS-MODEL-ID
                   ' PARTY=' SC-PARTY-ID
           MOVE 3104                   TO WS-ABEND-CODE
           CALL 'ILBOABN0' USING WS-ABEND-CODE
           .
