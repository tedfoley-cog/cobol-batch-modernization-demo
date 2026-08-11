      ******************************************************************
      * CVCUST01Y - CUSTOMER RECORD AS SEEN BY CARDSVC                 *
      *                                                                *
      * NOTE - PARTYRSK MAINTAINS ITS OWN VIEW OF THE PARTY IN         *
      * CVPARTY1Y.  THE TWO LAYOUTS ARE NOT IDENTICAL.                 *
      ******************************************************************
       01  CUST-RECORD.
           05  CUST-ID                     PIC 9(9).
           05  CUST-PARTY-ID               PIC X(11).
           05  CUST-NAME.
               10  CUST-TITLE              PIC X(4).
               10  CUST-FIRST-NAME         PIC X(20).
               10  CUST-MIDDLE-INIT        PIC X.
               10  CUST-LAST-NAME          PIC X(25).
           05  CUST-DOB                    PIC 9(8).
           05  CUST-NATIONAL-ID            PIC X(11).
           05  CUST-ADDRESS.
               10  CUST-ADDR-LINE1         PIC X(30).
               10  CUST-ADDR-LINE2         PIC X(30).
               10  CUST-CITY               PIC X(25).
               10  CUST-STATE              PIC X(2).
               10  CUST-ZIP                PIC X(10).
               10  CUST-COUNTRY            PIC X(3).
           05  CUST-PHONE-HOME             PIC X(15).
           05  CUST-PHONE-MOBILE           PIC X(15).
           05  CUST-EMAIL                  PIC X(50).
           05  CUST-SEGMENT-CD             PIC X(4).
           05  CUST-OPEN-DATE              PIC 9(8).
           05  CUST-STATUS                 PIC X.
           05  CUST-VIP-FLG                PIC X.
           05  CUST-FILLER                 PIC X(20).
