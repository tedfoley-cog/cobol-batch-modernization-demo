      ******************************************************************
      * CVCPRF1Y - CUSTPREF VSAM RECORD (KSDS, KEY 9, LRECL 180)       *
      *                                                                *
      * SERVICING PREFERENCES HELD LOCALLY BY CARDSVC.  THE MAILING    *
      * NAME AND ADDRESS HERE ARE THE ONES THE CARD PLANT USES AND     *
      * ARE NOT NECESSARILY THE ONES ON THE CUSTOMER MASTER.           *
      *                                                                *
      * READ BY CACRD15.  MAINTAINED BY CACRD03 AND CBREF02J.          *
      ******************************************************************
       01  CUST-PREF-RECORD.
           05  CPRF-CUST-ID                PIC 9(9).
           05  CPRF-STMT-PREF              PIC X(4).
               88  CPRF-STMT-PAPER         VALUE 'PAPR'.
               88  CPRF-STMT-ELEC          VALUE 'ELEC'.
           05  CPRF-LANGUAGE-CD            PIC X(3).
           05  CPRF-CONTACT-METHOD         PIC X.
               88  CPRF-CONTACT-PHONE      VALUE 'P'.
               88  CPRF-CONTACT-MAIL       VALUE 'M'.
               88  CPRF-CONTACT-EMAIL      VALUE 'E'.
           05  CPRF-MARKETING-OPT          PIC X.
           05  CPRF-PAPERLESS-FLG          PIC X.
           05  CPRF-ALERT-FLAGS.
               10  CPRF-ALERT-AUTH         PIC X.
               10  CPRF-ALERT-DECLINE      PIC X.
               10  CPRF-ALERT-OVLIMIT      PIC X.
               10  CPRF-ALERT-PAYMENT      PIC X.
               10  CPRF-ALERT-FRAUD        PIC X.
               10  CPRF-ALERT-FILLER       PIC X(3).
           05  CPRF-PREF-NAME              PIC X(30).
           05  CPRF-MAIL-ADDR1             PIC X(30).
           05  CPRF-MAIL-CITY              PIC X(25).
           05  CPRF-MAIL-ZIP               PIC X(10).
           05  CPRF-DAY-PHONE              PIC X(15).
      *    LEGACY 6 DIGIT DATE - WINDOWED, PIVOT 50
           05  CPRF-LAST-CONTACT-DT        PIC 9(6).
           05  CPRF-SERVICE-LEVEL          PIC X(2).
               88  CPRF-SERVICE-PREMIUM    VALUE 'PR'.
               88  CPRF-SERVICE-STANDARD   VALUE 'ST'.
           05  CPRF-LAST-MAINT-PGM         PIC X(8).
           05  CPRF-FILLER                 PIC X(27).
