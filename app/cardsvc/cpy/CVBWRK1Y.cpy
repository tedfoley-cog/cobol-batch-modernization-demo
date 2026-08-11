      ******************************************************************
      * CVBWRK1Y - CARDBILL CYCLE WORK RECORD                          *
      *                                                                *
      * WRITTEN BY CBBIL01 (CYCLE SELECTION).  READ BY CBBIL02 AND     *
      * CBBIL06.  FIXED 200 BYTES, SORTED ON BW-KEY.                   *
      *                                                                *
      * CARDSVC PRIVATE - NOT SHARED WITH PARTYRSK.                    *
      ******************************************************************
       01  BILL-WORK-RECORD.
           05  BW-KEY.
               10  BW-ACCT-ID              PIC 9(11).
               10  BW-CYCLE-DATE           PIC 9(8).
           05  BW-CUST-ID                  PIC 9(9).
           05  BW-PARTY-ID                 PIC X(11).
           05  BW-PRODUCT-CD               PIC X(4).
           05  BW-CURRENCY                 PIC X(3).
           05  BW-CYCLE-DAY                PIC 9(2).
           05  BW-ACCT-STATUS              PIC X.
               88  BW-ACCT-OPEN            VALUE 'O'.
               88  BW-ACCT-CLOSED          VALUE 'C'.
               88  BW-ACCT-SUSPENDED       VALUE 'S'.
               88  BW-ACCT-WRITTEN-OFF     VALUE 'W'.
           05  BW-PERIOD-FROM              PIC 9(8).
           05  BW-PERIOD-TO                PIC 9(8).
           05  BW-PREV-CYCLE-DT            PIC 9(8).
           05  BW-OPEN-BAL                 PIC S9(11)V99 COMP-3.
           05  BW-CURR-BAL                 PIC S9(11)V99 COMP-3.
           05  BW-CASH-BAL                 PIC S9(11)V99 COMP-3.
           05  BW-DELQ-BUCKET              PIC 9.
           05  BW-DELQ-AMT                 PIC S9(9)V99 COMP-3.
           05  BW-CREDIT-LIMIT             PIC S9(11)V99 COMP-3.
           05  BW-APR-PCT                  PIC S9(3)V9(5) COMP-3.
           05  BW-CASH-APR-PCT             PIC S9(3)V9(5) COMP-3.
           05  BW-STMT-COUNT               PIC 9(4).
           05  BW-STMT-NUMBER              PIC 9(6).
      *    DELIVERY CODE IS RESOLVED LATE - CBBIL01 LEAVES IT SPACES
      *    AND CBBIL04 SETS IT FROM CUSTPREF.
           05  BW-DELIVERY-CD              PIC X(4).
           05  BW-SELECT-TS                PIC X(26).
           05  BW-SELECT-PGM               PIC X(8).
           05  BW-FILLER                   PIC X(34).
