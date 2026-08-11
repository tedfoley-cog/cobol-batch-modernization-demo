      ******************************************************************
      * CVFRAU1Y - FRAUD SCORING WORK AREA                             *
      *                                                                *
      * SECOND PARAMETER ON EVERY CALL TO A FRAUD RULE HANDLER.  THE   *
      * FIRST PARAMETER IS THE AUTHORIZATION RECORD (CVAUTH01Y).       *
      *                                                                *
      * THE HANDLERS ARE CALLED IN SEQUENCE NUMBER ORDER AND EACH ONE  *
      * ADDS ITS OWN POINTS TO FW-SCORE-TOTAL AND PROMOTES             *
      * FW-WORST-ACTION IF ITS OWN ACTION IS MORE SEVERE.  THE AREA    *
      * IS NEVER CLEARED BETWEEN HANDLERS.                             *
      *                                                                *
      * ACTION SEVERITY ORDER - SCOR, FLAG, REFR, DECL.                *
      ******************************************************************
       01  FRAUD-WORK-AREA.
           05  FW-HDR.
               10  FW-VERSION              PIC 9(4).
                   88  FW-VER-CURRENT      VALUE 0002.
               10  FW-CALLER-PGM           PIC X(8).
               10  FW-ROUTE-KEY            PIC X(8).
                   88  FW-ROUTE-STANDARD   VALUE 'STANDARD'.
                   88  FW-ROUTE-HIGHRISK   VALUE 'HIGHRISK'.
               10  FW-SEQ-NBR              PIC 9(4).
               10  FW-REQ-DATE             PIC 9(8).
               10  FW-REQ-TIME             PIC 9(6).
               10  FW-CORREL-ID            PIC X(16).
      *
           05  FW-CONTEXT.
               10  FW-CARD-NUM             PIC X(16).
               10  FW-ACCT-ID              PIC 9(11).
               10  FW-CUST-ID              PIC 9(9).
               10  FW-MERCHANT-ID          PIC X(15).
               10  FW-ACQUIRER-ID          PIC X(11).
               10  FW-MCC                  PIC 9(4).
               10  FW-COUNTRY              PIC X(3).
               10  FW-AMOUNT               PIC S9(9)V99 COMP-3.
               10  FW-CURRENCY             PIC X(3).
               10  FW-ENTRY-MODE           PIC X.
               10  FW-CARD-PRESENT-FLG     PIC X.
               10  FW-PRIOR-COUNTRY        PIC X(3).
               10  FW-PRIOR-AUTH-DATE      PIC 9(8).
               10  FW-PRIOR-AUTH-TIME      PIC 9(6).
      *
           05  FW-RESULT.
               10  FW-SCORE-TOTAL          PIC 9(4).
               10  FW-SCORE-ADDED          PIC 9(4).
               10  FW-WORST-ACTION         PIC X(4).
                   88  FW-ACT-SCORE        VALUE 'SCOR'.
                   88  FW-ACT-FLAG         VALUE 'FLAG'.
                   88  FW-ACT-REFER        VALUE 'REFR'.
                   88  FW-ACT-DECLINE      VALUE 'DECL'.
               10  FW-REASON-CD            PIC X(4).
               10  FW-MESSAGE              PIC X(60).
      *
           05  FW-RULE-FIRED-CNT           PIC 9(2).
           05  FW-RULE-FIRED OCCURS 20 TIMES.
               10  FW-RF-RULE-ID           PIC X(8).
               10  FW-RF-RULE-CLASS        PIC X(4).
               10  FW-RF-POINTS            PIC 9(4).
               10  FW-RF-ACTION            PIC X(4).
      *
           05  FW-HANDLER-CNT              PIC 9(2).
           05  FW-HANDLER OCCURS 8 TIMES.
               10  FW-HD-PGM               PIC X(8).
               10  FW-HD-RC                PIC 9(4).
               10  FW-HD-POINTS            PIC 9(4).
      *
           05  FW-RC                       PIC 9(4).
               88  FW-RC-OK                VALUE 0000.
               88  FW-RC-WARN              VALUE 0004.
               88  FW-RC-ERROR             VALUE 0008.
               88  FW-RC-FATAL             VALUE 0012.
           05  FW-FAIL-PGM                 PIC X(8).
           05  FW-SQLCODE                  PIC S9(9) COMP.
           05  FW-FILLER                   PIC X(30).
