      ******************************************************************
      * CVTRAN01Y - POSTED TRANSACTION RECORD                          *
      *                                                                *
      * VARIABLE LENGTH.  THE LEG TABLE IS AN ODO - TRAILER FIELDS     *
      * FOLLOW IT, SO OFFSETS AFTER TXN-LEG DEPEND ON TXN-LEG-CNT.     *
      ******************************************************************
       01  TXN-RECORD.
           05  TXN-KEY.
               10  TXN-ID                  PIC X(16).
               10  TXN-POST-DATE           PIC 9(8).
           05  TXN-ACCT-ID                 PIC 9(11).
           05  TXN-CARD-NUM                PIC X(16).
           05  TXN-AUTH-SEQ-NUM            PIC 9(9).
           05  TXN-TYPE-CD                 PIC X(4).
           05  TXN-SOURCE                  PIC X(2).
               88  TXN-SRC-ONLINE          VALUE 'ON'.
               88  TXN-SRC-BATCH           VALUE 'BT'.
               88  TXN-SRC-INTERCHANGE     VALUE 'IC'.
           05  TXN-AMOUNT                  PIC S9(11)V99 COMP-3.
           05  TXN-CURRENCY                PIC X(3).
           05  TXN-BILLING-AMT             PIC S9(11)V99 COMP-3.
           05  TXN-FX-RATE                 PIC S9(3)V9(5) COMP-3.
           05  TXN-MERCHANT-ID             PIC X(15).
           05  TXN-MCC                     PIC 9(4).
           05  TXN-DESCRIPTION             PIC X(40).
      *
           05  TXN-LEG-CNT                 PIC 9(2).
           05  TXN-LEG OCCURS 1 TO 12 TIMES
                       DEPENDING ON TXN-LEG-CNT.
               10  TXN-LEG-SEQ             PIC 9(2).
               10  TXN-LEG-TYPE            PIC X(4).
                   88  TXN-LEG-PRINCIPAL   VALUE 'PRIN'.
                   88  TXN-LEG-FEE         VALUE 'FEE '.
                   88  TXN-LEG-INTEREST    VALUE 'INTR'.
                   88  TXN-LEG-REWARD      VALUE 'RWRD'.
                   88  TXN-LEG-FX          VALUE 'FXAD'.
               10  TXN-LEG-AMT             PIC S9(11)V99 COMP-3.
               10  TXN-LEG-GL-ACCT         PIC X(10).
               10  TXN-LEG-REVERSED        PIC X.
      *
      *    TRAILER - POSITION DEPENDS ON TXN-LEG-CNT
           05  TXN-TRAILER.
               10  TXN-POSTED-BY           PIC X(8).
               10  TXN-POSTED-TS           PIC X(26).
               10  TXN-CYCLE-ID            PIC X(8).
               10  TXN-GL-POSTED-FLG       PIC X.
               10  TXN-DISPUTE-FLG         PIC X.
               10  TXN-TRAILER-FILLER      PIC X(16).
