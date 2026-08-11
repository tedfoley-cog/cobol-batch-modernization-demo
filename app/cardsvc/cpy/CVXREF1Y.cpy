      ******************************************************************
      * CVXREF1Y - CARDXREF VSAM RECORD (KSDS, KEY 16, LRECL 128)      *
      *                                                                *
      * THE SERVICING VIEW OF A PLASTIC.  ONE RECORD PER CARD NUMBER   *
      * INCLUDING SUPERSEDED PLASTICS - A REISSUE LEAVES THE OLD       *
      * RECORD IN PLACE WITH XREF-SUPERSEDED-FLG SET TO 'Y' SO THE     *
      * INTERCHANGE FEED CAN STILL RESOLVE THE OLD NUMBER.             *
      *                                                                *
      * WRITTEN BY CACRD11.  READ BY CACRD11, CACRD13, CACRD14.        *
      * ALTERNATE INDEX AIX1 IS BY XREF-ACCT-ID.                       *
      ******************************************************************
       01  CARD-XREF-RECORD.
           05  XREF-CARD-NUM               PIC X(16).
           05  XREF-ACCT-ID                PIC 9(11).
           05  XREF-CUST-ID                PIC 9(9).
           05  XREF-PARTY-ID               PIC X(11).
           05  XREF-CARD-STATUS            PIC X.
               88  XREF-ACTIVE             VALUE 'A'.
               88  XREF-BLOCKED            VALUE 'B'.
               88  XREF-CLOSED             VALUE 'C'.
               88  XREF-LOST-STOLEN        VALUE 'L'.
               88  XREF-EXPIRED            VALUE 'E'.
               88  XREF-NOT-ACTIVATED      VALUE 'N'.
           05  XREF-PRODUCT-CD             PIC X(4).
           05  XREF-EXPIRY-YYMM            PIC 9(4).
           05  XREF-ISSUE-DATE             PIC 9(8).
           05  XREF-PREV-CARD-NUM          PIC X(16).
           05  XREF-SUPERSEDED-FLG         PIC X.
               88  XREF-SUPERSEDED         VALUE 'Y'.
      *    LEGACY 6 DIGIT DATE - WINDOWED, PIVOT 50
           05  XREF-SUPERSEDE-DT           PIC 9(6).
           05  XREF-REISSUE-CNT            PIC 9(2).
           05  XREF-BRANCH-CD              PIC X(5).
           05  XREF-LAST-MAINT-PGM         PIC X(8).
           05  XREF-LAST-MAINT-DT          PIC 9(8).
           05  XREF-FILLER                 PIC X(18).
