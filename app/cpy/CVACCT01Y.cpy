      ******************************************************************
      * CVACCT01Y - ACCOUNT MASTER RECORD                              *
      * DB2 TABLE CARDDB.ACCOUNT.                                      *
      ******************************************************************
       01  ACCT-RECORD.
           05  ACCT-ID                     PIC 9(11).
           05  ACCT-CUST-ID                PIC 9(9).
           05  ACCT-PARTY-ID               PIC X(11).
           05  ACCT-PRODUCT-CD             PIC X(4).
           05  ACCT-STATUS                 PIC X.
               88  ACCT-OPEN               VALUE 'O'.
               88  ACCT-CLOSED             VALUE 'C'.
               88  ACCT-SUSPENDED          VALUE 'S'.
               88  ACCT-WRITTEN-OFF        VALUE 'W'.
           05  ACCT-OPEN-DATE              PIC 9(8).
           05  ACCT-CLOSE-DATE             PIC 9(8).
           05  ACCT-CURRENCY               PIC X(3).
           05  ACCT-BALANCES.
               10  ACCT-CURR-BAL           PIC S9(11)V99 COMP-3.
               10  ACCT-STMT-BAL           PIC S9(11)V99 COMP-3.
               10  ACCT-PENDING-AUTH-AMT   PIC S9(11)V99 COMP-3.
               10  ACCT-CASH-BAL           PIC S9(11)V99 COMP-3.
               10  ACCT-MIN-PAY-DUE        PIC S9(9)V99 COMP-3.
               10  ACCT-LAST-PAY-AMT       PIC S9(9)V99 COMP-3.
           05  ACCT-CYCLE-DAY              PIC 9(2).
           05  ACCT-LAST-CYCLE-DT          PIC 9(8).
           05  ACCT-NEXT-CYCLE-DT          PIC 9(8).
           05  ACCT-PAY-DUE-DATE           PIC 9(8).
           05  ACCT-LAST-PAY-DATE          PIC 9(8).
           05  ACCT-DELQ-BUCKET            PIC 9.
               88  ACCT-CURRENT            VALUE 0.
               88  ACCT-DELQ-30            VALUE 1.
               88  ACCT-DELQ-60            VALUE 2.
               88  ACCT-DELQ-90            VALUE 3.
               88  ACCT-DELQ-120-PLUS      VALUE 4 5 6.
           05  ACCT-DELQ-AMT               PIC S9(9)V99 COMP-3.
           05  ACCT-STMT-COUNT             PIC 9(4).
           05  ACCT-BRANCH-CD              PIC X(5).
           05  ACCT-FILLER                 PIC X(24).
