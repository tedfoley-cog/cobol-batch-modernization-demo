      ******************************************************************
      * CVPWRK01Y - EXPOSURE RECALCULATION WORK LIST RECORD            *
      *                                                                *
      * CARDSVC PRIVATE.  BUILT BY CBCRD06A, SORTED AND DE-DUPLICATED  *
      * BY CBCRD06X, DISPATCHED BY CBCRD06B AND APPLIED BY CBCRD06C.   *
      * THE MANUAL REVIEW FILE CARRIES THE SAME LAYOUT.                *
      * FIXED 150 BYTES, KEYED ON PARTY THEN ACCOUNT.                  *
      ******************************************************************
       01  PARTY-WORK-REC.
           05  PW-KEY.
               10  PW-PARTY-ID             PIC X(11).
               10  PW-ACCT-ID              PIC 9(11).
           05  PW-CUST-ID                  PIC 9(9).
           05  PW-CARD-NUM                 PIC X(16).
           05  PW-CYCLE-DATE               PIC 9(8).
           05  PW-CYCLE-ID                 PIC X(8).
           05  PW-PRODUCT-CD               PIC X(4).
           05  PW-POSTED-AMT               PIC S9(11)V99 COMP-3.
           05  PW-TXN-CNT                  PIC 9(5).
           05  PW-CURR-BAL                 PIC S9(11)V99 COMP-3.
           05  PW-CREDIT-LIMIT             PIC S9(11)V99 COMP-3.
           05  PW-EXPOSURE-AMT             PIC S9(11)V99 COMP-3.
           05  PW-RISK-BAND-OLD            PIC X.
           05  PW-RISK-BAND-NEW            PIC X.
           05  PW-OUTCOME.
               10  PW-RC                   PIC 9(4).
                   88  PW-RC-OK            VALUE 0000.
                   88  PW-RC-WARN          VALUE 0004.
                   88  PW-RC-REVIEW        VALUE 0008.
                   88  PW-RC-FATAL         VALUE 0012.
               10  PW-REASON-CD            PIC X(4).
               10  PW-ADVICE-CD            PIC X(4).
               10  PW-SANCTION-FLG         PIC X.
               10  PW-SCORE                PIC 9(3).
               10  PW-DISPATCH-TS          PIC X(26).
           05  PW-FILLER                   PIC X(6).
