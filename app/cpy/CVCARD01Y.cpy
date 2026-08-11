      ******************************************************************
      * CVCARD01Y - CARD MASTER RECORD                                 *
      * DB2 TABLE CARDDB.CARD AND VSAM CARDXREF ALTERNATE VIEW.        *
      ******************************************************************
       01  CARD-RECORD.
           05  CARD-NUM                    PIC X(16).
           05  CARD-ACCT-ID                PIC 9(11).
           05  CARD-CUST-ID                PIC 9(9).
           05  CARD-EMBOSSED-NAME          PIC X(26).
           05  CARD-PRODUCT-CD             PIC X(4).
               88  CARD-PROD-CLASSIC       VALUE 'CLAS'.
               88  CARD-PROD-GOLD          VALUE 'GOLD'.
               88  CARD-PROD-PLAT          VALUE 'PLAT'.
               88  CARD-PROD-BUSINESS      VALUE 'BUSN'.
           05  CARD-STATUS                 PIC X.
               88  CARD-ACTIVE             VALUE 'A'.
               88  CARD-BLOCKED            VALUE 'B'.
               88  CARD-CLOSED             VALUE 'C'.
               88  CARD-LOST-STOLEN        VALUE 'L'.
               88  CARD-EXPIRED            VALUE 'E'.
               88  CARD-NOT-ACTIVATED      VALUE 'N'.
           05  CARD-EXPIRY-YYMM            PIC 9(4).
           05  CARD-ISSUE-DATE             PIC 9(8).
           05  CARD-LAST-USED-DT           PIC 9(8).
      *    LEGACY 6 DIGIT DATE - WINDOWED, PIVOT 50
           05  CARD-ACTIVATION-DT          PIC 9(6).
           05  CARD-CVV-IND                PIC X.
           05  CARD-PIN-TRIES              PIC 9.
           05  CARD-REISSUE-CNT            PIC 9(2).
           05  CARD-PREV-CARD-NUM          PIC X(16).
           05  CARD-BLOCK-REASON           PIC X(4).
           05  CARD-BLOCK-DATE             PIC 9(8).
           05  CARD-LAST-MAINT-PGM         PIC X(8).
           05  CARD-LAST-MAINT-TS          PIC X(26).
           05  CARD-FILLER                 PIC X(20).
