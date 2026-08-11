      ******************************************************************
      * CVAUTH01Y - AUTHORIZATION RECORD                               *
      *                                                                *
      * WRITTEN BY CACRD08.  READ BY CBCRD02J, CBCRD04J, CBCRD07J.     *
      * CBCRD03J SORTS ON THE FULL RECORD IMAGE.                       *
      *                                                                *
      * THE DETAIL AREA IS INTERPRETED ACCORDING TO AUTH-TYPE.  THE    *
      * VARIANTS DO NOT ALL FILL THE AREA - TRAILING BYTES CARRY       *
      * WHATEVER WAS LAST IN THE BUFFER.                               *
      ******************************************************************
       01  AUTH-RECORD.
           05  AUTH-KEY.
               10  AUTH-CARD-NUM           PIC X(16).
               10  AUTH-DATE               PIC 9(8).
               10  AUTH-SEQ-NUM            PIC 9(9).
           05  AUTH-ACCT-ID                PIC 9(11).
           05  AUTH-CUST-ID                PIC 9(9).
           05  AUTH-TIME                   PIC 9(6).
           05  AUTH-TYPE                   PIC X.
               88  AUTH-PURCHASE-TYPE      VALUE 'P'.
               88  AUTH-CASH-ADV-TYPE      VALUE 'C'.
               88  AUTH-REFUND-TYPE        VALUE 'R'.
           05  AUTH-STATUS                 PIC X.
               88  AUTH-APPROVED           VALUE 'A'.
               88  AUTH-DECLINED           VALUE 'D'.
               88  AUTH-REFERRED           VALUE 'F'.
               88  AUTH-REVERSED           VALUE 'V'.
           05  AUTH-RESP-CODE              PIC X(2).
           05  AUTH-REASON-CD              PIC X(4).
           05  AUTH-RISK-SCORE             PIC 9(3).
           05  AUTH-RISK-BAND              PIC X.
           05  AUTH-SETTLED-FLG            PIC X.
           05  AUTH-POSTED-FLG             PIC X.
      *
      *    ---------------------------------------------------------
      *    DETAIL AREA - 60 BYTES, THREE LAYOUTS
      *    ---------------------------------------------------------
           05  AUTH-DETAIL                 PIC X(60).
      *
           05  AUTH-PURCHASE REDEFINES AUTH-DETAIL.
               10  AP-MERCHANT-ID          PIC X(15).
               10  AP-MERCH-NAME           PIC X(22).
               10  AP-MCC                  PIC 9(4).
               10  AP-TERMINAL-ID          PIC X(8).
               10  AP-AMOUNT               PIC S9(9)V99 COMP-3.
               10  AP-CURRENCY             PIC X(3).
               10  AP-ENTRY-MODE           PIC X.
               10  AP-FILLER               PIC X(1).
      *
           05  AUTH-CASH-ADV REDEFINES AUTH-DETAIL.
               10  AC-ATM-ID               PIC X(8).
               10  AC-NETWORK              PIC X(4).
               10  AC-ACQUIRER-ID          PIC X(11).
               10  AC-AMOUNT               PIC S9(9)V99 COMP-3.
               10  AC-FEE                  PIC S9(5)V99 COMP-3.
               10  AC-CURRENCY             PIC X(3).
      *
           05  AUTH-REFUND REDEFINES AUTH-DETAIL.
               10  AR-ORIG-AUTH-ID         PIC X(12).
               10  AR-ORIG-DATE            PIC 9(6).
               10  AR-ORIG-MERCHANT        PIC X(15).
               10  AR-AMOUNT               PIC S9(9)V99 COMP-3.
      *
           05  AUTH-AUDIT.
               10  AUTH-ORIG-PGM           PIC X(8).
               10  AUTH-TERM-ID            PIC X(4).
               10  AUTH-OPER-ID            PIC X(8).
               10  AUTH-TIMESTAMP          PIC X(26).
