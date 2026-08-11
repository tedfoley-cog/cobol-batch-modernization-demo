      ******************************************************************
      * CVRISK01Y - RISK SERVICE COMMUNICATION AREA                    *
      *                                                                *
      * SHARED BETWEEN CARDSVC AND PARTYRSK.  512 BYTES.               *
      * USED AS CICS COMMAREA ON LINK AND AS THE FIRST PARAMETER ON    *
      * BATCH CALLS INTO THE RISK SERVICE.                             *
      *                                                                *
      * ANY CHANGE TO THIS LAYOUT REQUIRES A COORDINATED REBIND OF     *
      * BOTH MODULES.  BUMP CV-RISK-VERSION WHEN FIELDS ARE ADDED.     *
      ******************************************************************
       01  CV-RISK-AREA.
           05  CV-RISK-HDR.
               10  CV-RISK-VERSION         PIC 9(4).
                   88  CV-RISK-VER-CURRENT VALUE 0003.
                   88  CV-RISK-VER-OLD     VALUE 0001 0002.
               10  CV-RISK-CALLER-ID       PIC X(8).
               10  CV-RISK-CALLER-MOD      PIC X(8).
               10  CV-RISK-CORREL-ID       PIC X(16).
               10  CV-RISK-REQ-DATE        PIC 9(8).
               10  CV-RISK-REQ-TIME        PIC 9(6).
               10  CV-RISK-CHANNEL         PIC X.
                   88  CV-RISK-CHNL-ONLINE VALUE 'O'.
                   88  CV-RISK-CHNL-BATCH  VALUE 'B'.
               10  CV-RISK-HDR-FILLER      PIC X(9).
      *
           05  CV-RISK-IN.
               10  CV-RISK-PARTY-ID        PIC X(11).
               10  CV-RISK-CUST-ID         PIC 9(9).
               10  CV-RISK-ACCT-ID         PIC 9(11).
               10  CV-RISK-CARD-NUM        PIC X(16).
               10  CV-RISK-REQ-AMT         PIC S9(9)V99 COMP-3.
               10  CV-RISK-REQ-CURR        PIC X(3).
               10  CV-RISK-MCC             PIC 9(4).
               10  CV-RISK-MERCH-ID        PIC X(15).
               10  CV-RISK-COUNTRY         PIC X(3).
               10  CV-RISK-REQ-TYPE        PIC X(4).
                   88  CV-RISK-AUTH        VALUE 'AUTH'.
                   88  CV-RISK-INQY        VALUE 'INQY'.
                   88  CV-RISK-RCAL        VALUE 'RCAL'.
               10  CV-RISK-IN-FILLER       PIC X(20).
      *
           05  CV-RISK-OUT.
               10  CV-RISK-SCORE           PIC 9(3).
               10  CV-RISK-BAND            PIC X.
                   88  CV-RISK-BAND-LOW    VALUE 'A'.
                   88  CV-RISK-BAND-MED    VALUE 'B'.
                   88  CV-RISK-BAND-HIGH   VALUE 'C'.
                   88  CV-RISK-BAND-REFUSE VALUE 'X'.
               10  CV-RISK-KYC-STATUS      PIC X(2).
                   88  CV-RISK-KYC-OK      VALUE 'OK'.
                   88  CV-RISK-KYC-PENDING VALUE 'PN'.
                   88  CV-RISK-KYC-EXPIRED VALUE 'EX'.
                   88  CV-RISK-KYC-FAILED  VALUE 'FL'.
               10  CV-RISK-SANCTION-FLG    PIC X.
                   88  CV-RISK-SANCTION-HIT VALUE 'Y'.
               10  CV-RISK-EXPOSURE-AMT    PIC S9(11)V99 COMP-3.
               10  CV-RISK-AVAIL-AMT       PIC S9(11)V99 COMP-3.
               10  CV-RISK-ADVICE-CD       PIC X(4).
                   88  CV-RISK-ADV-APPROVE VALUE 'APPR'.
                   88  CV-RISK-ADV-REFER   VALUE 'REFR'.
                   88  CV-RISK-ADV-DECLINE VALUE 'DECL'.
               10  CV-RISK-REASON-CD       PIC X(4).
               10  CV-RISK-SCORE-DATE      PIC 9(8).
               10  CV-RISK-MODEL-ID        PIC X(8).
               10  CV-RISK-OUT-FILLER      PIC X(40).
      *
           05  CV-RISK-STATUS.
               10  CV-RISK-RC              PIC 9(4).
                   88  CV-RISK-RC-OK       VALUE 0000.
                   88  CV-RISK-RC-WARN     VALUE 0004.
                   88  CV-RISK-RC-ERROR    VALUE 0008.
                   88  CV-RISK-RC-FATAL    VALUE 0012.
               10  CV-RISK-REASON-TXT      PIC X(60).
               10  CV-RISK-FAIL-PGM        PIC X(8).
               10  CV-RISK-SQLCODE         PIC S9(9) COMP.
               10  CV-RISK-STAT-FILLER     PIC X(30).
      *
           05  CV-RISK-TRACE.
               10  CV-RISK-HOP-CNT         PIC 9(2).
               10  CV-RISK-HOP OCCURS 8 TIMES.
                   15  CV-RISK-HOP-PGM     PIC X(8).
                   15  CV-RISK-HOP-RC      PIC 9(4).
      *
      *    RESERVED SO THE AREA REMAINS 512 BYTES.  NEW FIELDS ARE
      *    CARVED OUT OF HERE - DO NOT EXTEND THE AREA.
           05  CV-RISK-RESERVED            PIC X(61).
