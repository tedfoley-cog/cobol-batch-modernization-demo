      ******************************************************************
      * CVSTMT01Y - STATEMENT RECORD                                   *
      * VARIABLE LENGTH - STATEMENT LINES ARE AN ODO.                  *
      ******************************************************************
       01  STMT-RECORD.
           05  STMT-KEY.
               10  STMT-ACCT-ID            PIC 9(11).
               10  STMT-CYCLE-DATE         PIC 9(8).
           05  STMT-NUMBER                 PIC 9(6).
           05  STMT-CUST-ID                PIC 9(9).
           05  STMT-FORMAT-CD              PIC X(4).
               88  STMT-FMT-PAPER          VALUE 'PAPR'.
               88  STMT-FMT-ELECTRONIC     VALUE 'ELEC'.
           05  STMT-PERIOD-FROM            PIC 9(8).
           05  STMT-PERIOD-TO              PIC 9(8).
           05  STMT-DUE-DATE               PIC 9(8).
           05  STMT-AMOUNTS.
               10  STMT-OPEN-BAL           PIC S9(11)V99 COMP-3.
               10  STMT-CLOSE-BAL          PIC S9(11)V99 COMP-3.
               10  STMT-PURCHASES          PIC S9(11)V99 COMP-3.
               10  STMT-CASH-ADV           PIC S9(11)V99 COMP-3.
               10  STMT-PAYMENTS           PIC S9(11)V99 COMP-3.
               10  STMT-FEES               PIC S9(9)V99 COMP-3.
               10  STMT-INTEREST           PIC S9(9)V99 COMP-3.
               10  STMT-MIN-PAY            PIC S9(9)V99 COMP-3.
               10  STMT-CREDIT-LIMIT       PIC S9(11)V99 COMP-3.
               10  STMT-AVAIL-CREDIT       PIC S9(11)V99 COMP-3.
           05  STMT-APR                    PIC S9(3)V9(5) COMP-3.
           05  STMT-REWARD-PTS             PIC 9(9).
      *
           05  STMT-LINE-CNT               PIC 9(4).
           05  STMT-LINE OCCURS 1 TO 300 TIMES
                        DEPENDING ON STMT-LINE-CNT.
               10  STMT-LN-DATE            PIC 9(8).
               10  STMT-LN-POST-DATE       PIC 9(8).
               10  STMT-LN-DESC            PIC X(40).
               10  STMT-LN-REF             PIC X(16).
               10  STMT-LN-AMT             PIC S9(11)V99 COMP-3.
               10  STMT-LN-DR-CR           PIC X.
      *
           05  STMT-TRAILER.
               10  STMT-GEN-PGM            PIC X(8).
               10  STMT-GEN-TS             PIC X(26).
               10  STMT-PAGE-CNT           PIC 9(3).
               10  STMT-TRAILER-FILLER     PIC X(20).
