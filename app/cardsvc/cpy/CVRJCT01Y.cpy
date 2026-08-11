      ******************************************************************
      * CVRJCT01Y - NIGHTLY CYCLE REJECT RECORD                        *
      *                                                                *
      * CARDSVC PRIVATE.  WRITTEN BY CBCRD02, CBCRD03 AND CBCRD04.     *
      * READ BY CBCRD10 WHEN THE CONTROL REPORT EXCEPTION SUMMARY IS   *
      * PRODUCED.  FIXED 300 BYTES.                                    *
      ******************************************************************
       01  CYCLE-REJECT-REC.
           05  RJ-CYCLE-DATE               PIC 9(8).
           05  RJ-CYCLE-ID                 PIC X(8).
           05  RJ-REJECT-SEQ               PIC 9(9).
           05  RJ-REASON-CD                PIC X(4).
               88  RJ-BAD-AUTH-TYPE        VALUE 'R001'.
               88  RJ-BAD-AMOUNT           VALUE 'R002'.
               88  RJ-BAD-DATE             VALUE 'R003'.
               88  RJ-BAD-STATUS           VALUE 'R004'.
               88  RJ-BAD-CURRENCY         VALUE 'R005'.
               88  RJ-BAD-MCC              VALUE 'R006'.
               88  RJ-BAD-ACCOUNT          VALUE 'R007'.
               88  RJ-ALREADY-POSTED       VALUE 'R008'.
               88  RJ-NO-CARD              VALUE 'R009'.
               88  RJ-POSTING-FAILED       VALUE 'R010'.
           05  RJ-REASON-TXT               PIC X(40).
           05  RJ-DETECT-PGM               PIC X(8).
           05  RJ-DETECT-STEP              PIC X(8).
           05  RJ-AUTH-IMAGE               PIC X(179).
           05  RJ-FILLER                   PIC X(36).
