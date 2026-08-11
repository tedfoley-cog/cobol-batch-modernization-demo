      ******************************************************************
      * CVEXPO01Y - PARTY EXPOSURE AGGREGATE (PARTYRSK INTERNAL)       *
      ******************************************************************
       01  EXPOSURE-RECORD.
           05  EX-PARTY-ID                 PIC X(11).
           05  EX-AS-OF-DATE               PIC 9(8).
           05  EX-CURRENCY                 PIC X(3).
           05  EX-TOTALS.
               10  EX-TOTAL-LIMIT          PIC S9(13)V99 COMP-3.
               10  EX-TOTAL-DRAWN          PIC S9(13)V99 COMP-3.
               10  EX-TOTAL-AVAILABLE      PIC S9(13)V99 COMP-3.
               10  EX-UNSECURED-AMT        PIC S9(13)V99 COMP-3.
               10  EX-SECURED-AMT          PIC S9(13)V99 COMP-3.
               10  EX-PAST-DUE-AMT         PIC S9(11)V99 COMP-3.
               10  EX-WRITTEN-OFF-AMT      PIC S9(11)V99 COMP-3.
      *
           05  EX-PROD-CNT                 PIC 9(2).
           05  EX-PRODUCT OCCURS 1 TO 15 TIMES
                          DEPENDING ON EX-PROD-CNT.
               10  EX-PROD-SYSTEM          PIC X(8).
               10  EX-PROD-CODE            PIC X(4).
               10  EX-PROD-ACCT-CNT        PIC 9(4).
               10  EX-PROD-LIMIT           PIC S9(13)V99 COMP-3.
               10  EX-PROD-DRAWN           PIC S9(13)V99 COMP-3.
               10  EX-PROD-DELQ-BUCKET     PIC 9.
      *
           05  EX-UTILISATION-PCT          PIC S9(3)V99 COMP-3.
           05  EX-CALC-PGM                 PIC X(8).
           05  EX-CALC-TS                  PIC X(26).
           05  EX-STALE-FLG                PIC X.
