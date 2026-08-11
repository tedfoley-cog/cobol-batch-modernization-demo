      ******************************************************************
      * CVAXTR01Y - NIGHTLY AUTHORISATION EXTRACT RECORD               *
      *                                                                *
      * CARDSVC PRIVATE.  WRITTEN BY CBCRD01, EDITED BY CBCRD02,       *
      * ENRICHED BY CBCRD03, CONSUMED BY CBCRD04.                      *
      *                                                                *
      * FIXED 300 BYTES SO THE FILE CAN BE SORTED WITH DFSORT ON A     *
      * FIXED DISPLACEMENT.  THE AUTHORISATION IMAGE IS CARRIED AS AN  *
      * UNINTERPRETED 179 BYTE STRING - REDEFINE IT WITH CVAUTH01Y IN  *
      * WORKING STORAGE BEFORE LOOKING AT THE DETAIL VARIANTS.         *
      ******************************************************************
       01  AUTH-EXTRACT-REC.
           05  AX-HEADER.
               10  AX-CYCLE-DATE           PIC 9(8).
               10  AX-CYCLE-ID             PIC X(8).
               10  AX-EXTRACT-SEQ          PIC 9(9).
               10  AX-SOURCE-RBA           PIC 9(9).
      *
      *    179 BYTE IMAGE OF AUTH-RECORD (CVAUTH01Y)
           05  AX-AUTH-IMAGE               PIC X(179).
      *
      *    SET BY CBCRD02 - EDIT OUTCOME
           05  AX-EDIT.
               10  AX-EDIT-STATUS          PIC X.
                   88  AX-EDIT-CLEAN       VALUE 'C'.
                   88  AX-EDIT-WARNED      VALUE 'W'.
                   88  AX-EDIT-REJECTED    VALUE 'R'.
               10  AX-EDIT-REASON          PIC X(4).
      *
      *    SET BY CBCRD03 - MERCHANT ENRICHMENT
           05  AX-ENRICH.
               10  AX-MCC                  PIC 9(4).
               10  AX-ACQUIRER-ID          PIC X(11).
               10  AX-SETTLE-ROUTE         PIC X(4).
               10  AX-MERCH-NAME           PIC X(22).
               10  AX-HIGH-RISK-FLG        PIC X.
               10  AX-ENRICH-STATUS        PIC X.
                   88  AX-ENRICH-MATCHED   VALUE 'M'.
                   88  AX-ENRICH-DEFAULTED VALUE 'D'.
                   88  AX-ENRICH-NA        VALUE 'N'.
      *
           05  AX-FILLER                   PIC X(39).
