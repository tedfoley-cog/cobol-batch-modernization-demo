      ******************************************************************
      * CVFEEW1Y - FEE HANDLER ACCOUNT WORK AREA                       *
      *                                                                *
      * SECOND PARAMETER ON A FEEC DISPATCH THROUGH CBCRD90.  THE      *
      * AREA IS 512 BYTES BECAUSE THE DISPATCHER PASSES ITS PARAMETER  *
      * THROUGH AS PIC X(512).                                         *
      *                                                                *
      * THE FEE BASIS AREA IS FILLED ONLY FOR THE FEE TYPES THAT NEED  *
      * IT.  FOR CASH ADVANCE FEES THE AUTHORIZATION IMAGE IS CARRIED  *
      * IN FW-AUTH-IMAGE AND IS INTERPRETED WITH CVAUTH01Y.            *
      *                                                                *
      * CARDSVC PRIVATE.  USED BY CBFEE01, CBFEE02, CBFEE03.           *
      ******************************************************************
       01  FEE-WORK-AREA.
           05  FW-HDR.
               10  FW-CALLER-PGM           PIC X(8).
               10  FW-RUN-DATE             PIC 9(8).
               10  FW-CYCLE-ID             PIC X(8).
               10  FW-CYCLE-DATE           PIC 9(8).
               10  FW-FEE-TYPE             PIC X(4).
                   88  FW-FEE-ANNUAL       VALUE 'ANNU'.
                   88  FW-FEE-LATE         VALUE 'LATE'.
                   88  FW-FEE-OVERLIMIT    VALUE 'OVLM'.
                   88  FW-FEE-CASH-ADV     VALUE 'CASH'.
                   88  FW-FEE-FOREIGN      VALUE 'FRGN'.
               10  FW-SIMULATE-FLG         PIC X.
                   88  FW-SIMULATE         VALUE 'Y'.
      *
           05  FW-ACCT.
               10  FW-ACCT-ID              PIC 9(11).
               10  FW-CUST-ID              PIC 9(9).
               10  FW-CARD-NUM             PIC X(16).
               10  FW-PRODUCT-CD           PIC X(4).
               10  FW-ACCT-STATUS          PIC X.
               10  FW-CURRENCY             PIC X(3).
               10  FW-OPEN-DATE            PIC 9(8).
               10  FW-CURR-BAL             PIC S9(11)V99 COMP-3.
               10  FW-CASH-BAL             PIC S9(11)V99 COMP-3.
               10  FW-CREDIT-LIMIT         PIC S9(11)V99 COMP-3.
               10  FW-MIN-PAY-DUE          PIC S9(9)V99 COMP-3.
               10  FW-LAST-PAY-AMT         PIC S9(9)V99 COMP-3.
               10  FW-LAST-PAY-DATE        PIC 9(8).
               10  FW-PAY-DUE-DATE         PIC 9(8).
               10  FW-LAST-CYCLE-DT        PIC 9(8).
               10  FW-DELQ-BUCKET          PIC 9.
               10  FW-DELQ-AMT             PIC S9(9)V99 COMP-3.
               10  FW-VIP-FLG              PIC X.
               10  FW-OVLM-OPTIN-FLG       PIC X.
                   88  FW-OVLM-OPTED-IN    VALUE 'Y'.
               10  FW-ANNIV-WAIVE-FLG      PIC X.
               10  FW-COUNTRY-CD           PIC X(3).
      *
           05  FW-BASIS.
               10  FW-BASIS-AMT            PIC S9(11)V99 COMP-3.
               10  FW-BASIS-CURR           PIC X(3).
               10  FW-BASIS-COUNTRY        PIC X(3).
               10  FW-FX-RATE              PIC S9(3)V9(5) COMP-3.
               10  FW-TXN-ID               PIC X(16).
               10  FW-TXN-POST-DATE        PIC 9(8).
               10  FW-MERCHANT-ID          PIC X(15).
               10  FW-MCC                  PIC 9(4).
               10  FW-AUTH-SEQ-NUM         PIC 9(9).
               10  FW-AUTH-IMAGE           PIC X(60).
      *
           05  FW-PRIOR-CYCLE.
               10  FW-PRIOR-LATE-FEE-FLG   PIC X.
               10  FW-PRIOR-OVLM-FEE-FLG   PIC X.
               10  FW-PRIOR-ANNU-FEE-DT    PIC 9(8).
               10  FW-CYCLE-FEE-CNT        PIC 9(4).
      *
      *    KEEPS THE AREA AT 512 BYTES - NEW FIELDS ARE CARVED FROM
      *    HERE, THE AREA IS NEVER EXTENDED.
           05  FW-RESERVED                 PIC X(209).
