      ******************************************************************
      * CVMRTE1Y - MERCHRTE VSAM RECORD (KSDS, KEY 15, LRECL 120)      *
      *                                                                *
      * THE VSAM COPY OF CARDSVC.MERCHANT USED BY THE AUTHORIZATION    *
      * PATH WHEN DB2 IS NOT AVAILABLE.  REBUILT WEEKLY BY CBREF02J.   *
      * THE UNLOAD RECORD AND THE VSAM RECORD ARE THE SAME LAYOUT SO   *
      * IDCAMS REPRO CAN LOAD THE SORTED UNLOAD WITHOUT REFORMATTING.  *
      *                                                                *
      * CARDSVC PRIVATE.  USED BY CBREF02 AND CBREF05.                 *
      ******************************************************************
       01  MERCHANT-RTE-RECORD.
           05  MR-KEY.
               10  MR-MERCHANT-ID          PIC X(15).
           05  MR-MERCHANT-NAME            PIC X(40).
           05  MR-MCC                      PIC X(4).
           05  MR-ACQUIRER-ID              PIC X(11).
           05  MR-COUNTRY-CD               PIC X(3).
           05  MR-SETTLE-ROUTE-CD          PIC X(4).
           05  MR-HIGH-RISK-FLG            PIC X.
               88  MR-HIGH-RISK            VALUE 'Y'.
           05  MR-CHARGEBACK-RATE          PIC S9(3)V99 COMP-3.
           05  MR-STATUS                   PIC X.
               88  MR-ACTIVE               VALUE 'A'.
               88  MR-SUSPENDED            VALUE 'S'.
               88  MR-DELETED              VALUE 'D'.
           05  MR-CITY                     PIC X(25).
           05  MR-LAST-MAINT-DATE          PIC 9(8).
           05  MR-FILLER                   PIC X(5).
