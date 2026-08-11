      ******************************************************************
      * CVAUTHW1Y - CA00 ONLINE WORKING COMMAREA                       *
      *                                                                *
      * 512 BYTES.  PASSED FROM CACRD00 THROUGH EVERY CA00 SCREEN AND  *
      * DOWN THE AUTHORIZATION CHAIN CACRD04 - CACRD10 - CACRD17.      *
      * ALSO PASSED AS THE CALLER COMMAREA ON EVERY CACRD90 DISPATCH.  *
      *                                                                *
      * THE AREA CARRIES FOUR THINGS -                                 *
      *   1. THE PSEUDO CONVERSATION HEADER (WHO / WHERE / WHAT NEXT)  *
      *   2. THE AUTHORIZATION REQUEST AS KEYED ON THE ENTRY SCREEN    *
      *   3. THE ACCUMULATED DECISION STATE, INCLUDING THE ANSWER      *
      *      COPIED OUT OF CV-RISK-AREA AFTER THE PARTYRSK CROSSING    *
      *   4. THE PROGRAM TRAIL - ONE ENTRY PER CHAIN PROGRAM RUN       *
      *                                                                *
      * ADDED FOR THE 2003 AUTH REWRITE.  DO NOT EXTEND PAST 512 -     *
      * TAKE NEW FIELDS OUT OF CAW-RESERVED OR THE SECTION FILLERS.    *
      ******************************************************************
       01  CA-WORK-AREA.
      *
      *    ---------------------------------------------------------
      *    HEADER - 95 BYTES
      *    ---------------------------------------------------------
           05  CAW-HDR.
               10  CAW-TRAN-ID             PIC X(4).
               10  CAW-TERM-ID             PIC X(4).
               10  CAW-OPER-ID             PIC X(8).
               10  CAW-FROM-PGM            PIC X(8).
      *        ROUTE KEY - NEVER A LOAD MODULE NAME
               10  CAW-ROUTE-KEY           PIC X(8).
               10  CAW-OPTION              PIC X(2).
                   88  CAW-OPT-EXIT        VALUE 'X '.
               10  CAW-FIRST-PASS-SW       PIC X.
                   88  CAW-FIRST-PASS      VALUE 'Y'.
                   88  CAW-RE-ENTRY        VALUE 'N'.
               10  CAW-SCREEN-ID           PIC X(8).
               10  CAW-MSG                 PIC X(50).
               10  CAW-HDR-FILLER          PIC X(2).
      *
      *    ---------------------------------------------------------
      *    ENTITY CONTEXT - 50 BYTES
      *    ---------------------------------------------------------
           05  CAW-CTX.
               10  CAW-ACCT-ID             PIC 9(11).
               10  CAW-CARD-NUM            PIC X(16).
               10  CAW-CUST-ID             PIC 9(9).
               10  CAW-PARTY-ID            PIC X(11).
               10  CAW-CTX-FILLER          PIC X(3).
      *
      *    ---------------------------------------------------------
      *    BROWSE POSITION FOR THE CARDXREF PAGED LIST - 50 BYTES
      *    ---------------------------------------------------------
           05  CAW-BROWSE.
               10  CAW-BR-ACCT-KEY         PIC X(11).
               10  CAW-BR-FIRST-CARD       PIC X(16).
               10  CAW-BR-LAST-CARD        PIC X(16).
               10  CAW-BR-PAGE-NBR         PIC 9(3).
               10  CAW-BR-EOF-SW           PIC X.
                   88  CAW-BR-EOF          VALUE 'Y'.
               10  CAW-BR-BOF-SW           PIC X.
                   88  CAW-BR-BOF          VALUE 'Y'.
               10  CAW-BR-FILLER           PIC X(2).
      *
      *    ---------------------------------------------------------
      *    AUTHORIZATION REQUEST AS KEYED - 90 BYTES
      *    ---------------------------------------------------------
           05  CAW-AUTH-REQ.
               10  CAW-RQ-CARD-NUM         PIC X(16).
               10  CAW-RQ-AMT              PIC S9(9)V99 COMP-3.
               10  CAW-RQ-CURR             PIC X(3).
               10  CAW-RQ-MCC              PIC 9(4).
               10  CAW-RQ-MERCH-ID         PIC X(15).
               10  CAW-RQ-MERCH-CTRY       PIC X(3).
               10  CAW-RQ-TERMINAL         PIC X(8).
               10  CAW-RQ-ENTRY-MODE       PIC X.
                   88  CAW-RQ-SWIPED       VALUE 'S'.
                   88  CAW-RQ-CHIP         VALUE 'C'.
                   88  CAW-RQ-KEYED        VALUE 'K'.
                   88  CAW-RQ-ECOM         VALUE 'E'.
                   88  CAW-RQ-CONTACTLESS  VALUE 'T'.
               10  CAW-RQ-AUTH-TYPE        PIC X.
                   88  CAW-RQ-PURCHASE     VALUE 'P'.
                   88  CAW-RQ-CASH-ADV     VALUE 'C'.
                   88  CAW-RQ-REFUND       VALUE 'R'.
               10  CAW-RQ-ATM-ID           PIC X(8).
               10  CAW-RQ-NETWORK          PIC X(4).
               10  CAW-RQ-ORIG-AUTH        PIC X(12).
               10  CAW-RQ-ORIG-DATE        PIC 9(6).
               10  CAW-RQ-FILLER           PIC X(3).
      *
      *    ---------------------------------------------------------
      *    RISK ANSWER COPIED OUT OF CV-RISK-AREA - 45 BYTES
      *    ---------------------------------------------------------
           05  CAW-RISK.
               10  CAW-RSK-SCORE           PIC 9(3).
               10  CAW-RSK-BAND            PIC X.
                   88  CAW-RSK-BAND-LOW    VALUE 'A'.
                   88  CAW-RSK-BAND-MED    VALUE 'B'.
                   88  CAW-RSK-BAND-HIGH   VALUE 'C'.
                   88  CAW-RSK-BAND-REFUSE VALUE 'X'.
               10  CAW-RSK-KYC             PIC X(2).
               10  CAW-RSK-SANCTION        PIC X.
                   88  CAW-RSK-SANCTIONED  VALUE 'Y'.
               10  CAW-RSK-EXPOSURE        PIC S9(11)V99 COMP-3.
               10  CAW-RSK-AVAIL           PIC S9(11)V99 COMP-3.
               10  CAW-RSK-ADVICE          PIC X(4).
                   88  CAW-RSK-APPROVE     VALUE 'APPR'.
                   88  CAW-RSK-REFER       VALUE 'REFR'.
                   88  CAW-RSK-DECLINE     VALUE 'DECL'.
               10  CAW-RSK-REASON          PIC X(4).
               10  CAW-RSK-RC              PIC 9(4).
               10  CAW-RSK-MODEL           PIC X(8).
               10  CAW-RSK-REFERRAL-SW     PIC X.
                   88  CAW-RSK-REFERRED    VALUE 'Y'.
               10  CAW-RSK-FILLER          PIC X(3).
      *
      *    ---------------------------------------------------------
      *    ACCUMULATED DECISION STATE - 80 BYTES
      *    ---------------------------------------------------------
           05  CAW-DECISION.
               10  CAW-STAT-RESULT         PIC X.
                   88  CAW-STAT-OK         VALUE 'P'.
                   88  CAW-STAT-FAILED     VALUE 'F'.
               10  CAW-STAT-REASON         PIC X(4).
               10  CAW-FRAUD-SCORE         PIC 9(3).
               10  CAW-FRAUD-ACTION        PIC X(4).
                   88  CAW-FRD-SCORE-ONLY  VALUE 'SCOR'.
                   88  CAW-FRD-FLAG        VALUE 'FLAG'.
                   88  CAW-FRD-REFER       VALUE 'REFR'.
                   88  CAW-FRD-DECLINE     VALUE 'DECL'.
               10  CAW-FRAUD-RULE          PIC X(8).
               10  CAW-FRAUD-SEQ           PIC 9(2).
               10  CAW-LIM-RESULT          PIC X.
                   88  CAW-LIM-OK          VALUE 'P'.
                   88  CAW-LIM-TOLERATED   VALUE 'T'.
                   88  CAW-LIM-EXCEEDED    VALUE 'X'.
                   88  CAW-LIM-VELOCITY    VALUE 'V'.
               10  CAW-LIM-TYPE            PIC X(4).
               10  CAW-LIM-AVAIL           PIC S9(11)V99 COMP-3.
               10  CAW-LIM-OVER-AMT        PIC S9(11)V99 COMP-3.
               10  CAW-VELOCITY-CNT        PIC 9(3).
               10  CAW-DEC-STATUS          PIC X.
                   88  CAW-DEC-APPROVED    VALUE 'A'.
                   88  CAW-DEC-DECLINED    VALUE 'D'.
                   88  CAW-DEC-REFERRED    VALUE 'F'.
               10  CAW-DEC-RESP            PIC X(2).
               10  CAW-DEC-REASON          PIC X(4).
               10  CAW-AUTH-SEQ-NUM        PIC 9(9).
               10  CAW-AUTH-DATE           PIC 9(8).
               10  CAW-AUTH-TIME           PIC 9(6).
               10  CAW-DEC-FILLER          PIC X(6).
      *
      *    ---------------------------------------------------------
      *    PROGRAM TRAIL - APPENDED BY EVERY CHAIN PROGRAM - 98 BYTES
      *    ---------------------------------------------------------
           05  CAW-TRAIL.
               10  CAW-TRAIL-CNT           PIC 9(2).
               10  CAW-TRAIL-ENT OCCURS 8 TIMES.
                   15  CAW-TRAIL-PGM       PIC X(8).
                   15  CAW-TRAIL-RC        PIC 9(4).
      *
           05  CAW-RESERVED                PIC X(4).
