      ******************************************************************
      * CVPARTY1Y - PARTY RECORD (PARTYRSK VIEW)                       *
      *                                                                *
      * PARTYRSK INTERNAL.  NOT SHARED WITH CARDSVC.                   *
      * PARTYRSK IS PARTY CENTRIC - ONE PARTY MAY HOLD MANY CUSTOMER   *
      * NUMBERS ACROSS PRODUCT SYSTEMS.                                *
      ******************************************************************
       01  PARTY-RECORD.
           05  PT-PARTY-ID                 PIC X(11).
           05  PT-PARTY-TYPE               PIC X.
               88  PT-INDIVIDUAL           VALUE 'I'.
               88  PT-ORGANISATION         VALUE 'O'.
               88  PT-TRUST                VALUE 'T'.
           05  PT-LEGAL-NAME               PIC X(60).
           05  PT-SHORT-NAME               PIC X(25).
           05  PT-DOB-INCORP               PIC 9(8).
           05  PT-NATIONAL-ID              PIC X(11).
           05  PT-TAX-ID                   PIC X(15).
           05  PT-DOMICILE-CTRY            PIC X(3).
           05  PT-RESIDENCE-CTRY           PIC X(3).
           05  PT-CITIZENSHIP              PIC X(3).
           05  PT-PEP-FLG                  PIC X.
               88  PT-IS-PEP               VALUE 'Y'.
           05  PT-STATUS                   PIC X.
               88  PT-STATUS-ACTIVE        VALUE 'A'.
               88  PT-STATUS-DORMANT       VALUE 'D'.
               88  PT-STATUS-EXITED        VALUE 'X'.
           05  PT-ONBOARD-DATE             PIC 9(8).
      *
      *    LINKED CUSTOMER NUMBERS FROM THE PRODUCT SYSTEMS
           05  PT-LINK-CNT                 PIC 9(2).
           05  PT-LINK OCCURS 1 TO 20 TIMES
                       DEPENDING ON PT-LINK-CNT.
               10  PT-LINK-SYSTEM          PIC X(8).
               10  PT-LINK-CUST-ID         PIC 9(9).
               10  PT-LINK-STATUS          PIC X.
      *
           05  PT-ALIAS-CNT                PIC 9(2).
           05  PT-ALIAS OCCURS 0 TO 10 TIMES
                        DEPENDING ON PT-ALIAS-CNT.
               10  PT-ALIAS-NAME           PIC X(60).
               10  PT-ALIAS-TYPE           PIC X(4).
