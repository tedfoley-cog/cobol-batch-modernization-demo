      ******************************************************************
      * CVRSNC1Y - RSNCODE VSAM RECORD (KSDS, KEY 8, LRECL 100)        *
      *                                                                *
      * REASON AND RESPONSE CODE REFERENCE.  THE KEY IS CATEGORY       *
      * FOLLOWED BY CODE SO A GENERIC STARTBR ON THE CATEGORY GIVES    *
      * EVERY CODE IN THAT CATEGORY IN CODE ORDER.                     *
      *                                                                *
      * READ BY CACRD13 AND CACRD16.  LOADED BY CBREF01J.              *
      ******************************************************************
       01  RSN-CODE-RECORD.
           05  RSN-KEY.
               10  RSN-CATEGORY            PIC X(4).
                   88  RSN-CAT-DISPUTE     VALUE 'DISP'.
                   88  RSN-CAT-BLOCK       VALUE 'BLOK'.
                   88  RSN-CAT-DECLINE     VALUE 'DECL'.
                   88  RSN-CAT-FRAUD       VALUE 'FRAU'.
                   88  RSN-CAT-ADJUST      VALUE 'ADJT'.
               10  RSN-CODE                PIC X(4).
           05  RSN-SHORT-DESC              PIC X(20).
           05  RSN-LONG-DESC               PIC X(40).
           05  RSN-ACTION-CD               PIC X(4).
           05  RSN-PROV-CREDIT-FLG         PIC X.
               88  RSN-PROV-CREDIT-ALLOWED VALUE 'Y'.
           05  RSN-FILING-DAYS             PIC 9(3).
           05  RSN-NETWORK-CD              PIC X(4).
           05  RSN-ACTIVE-FLG              PIC X.
               88  RSN-ACTIVE              VALUE 'Y'.
           05  RSN-EFF-DATE                PIC 9(8).
           05  RSN-LAST-MAINT-PGM          PIC X(8).
           05  RSN-FILLER                  PIC X(3).
