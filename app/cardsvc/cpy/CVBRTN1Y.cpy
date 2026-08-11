      ******************************************************************
      * CVBRTN1Y - BATCH DISPATCHED CALL RETURN AREA                   *
      *                                                                *
      * THIRD PARAMETER ON A CBCRD90 DISPATCH.  THE FIRST 70 BYTES     *
      * ARE THE AREA CBCRD90 ITSELF ADDRESSES - THE EXTENSION IS SEEN  *
      * ONLY BY THE CALLER AND THE HANDLER, SO NEW COUNTERS GO IN      *
      * BR-EXTENSION AND NEVER AHEAD OF IT.                            *
      *                                                                *
      * CARDSVC PRIVATE.  USED BY CBBIL04, CBSTM01, CBSTM02,           *
      * CBFEE01, CBFEE02, CBFEE03.                                     *
      ******************************************************************
       01  BATCH-RETURN-AREA.
           05  BR-RETURN-CD                PIC S9(4) COMP.
               88  BR-RC-OK                VALUE 0.
               88  BR-RC-WARN              VALUE 4.
               88  BR-RC-BUSINESS          VALUE 8.
               88  BR-RC-FATAL             VALUE 12.
           05  BR-RETURN-PGM               PIC X(8).
           05  BR-RETURN-MSG               PIC X(60).
      *
           05  BR-EXTENSION.
               10  BR-FEE-TOTAL            PIC S9(9)V99 COMP-3.
               10  BR-FEE-COUNT            PIC 9(4).
               10  BR-FEE-TYPE-CD          PIC X(4).
               10  BR-FEE-WAIVED-FLG       PIC X.
                   88  BR-FEE-WAIVED       VALUE 'Y'.
               10  BR-WAIVER-RULE-CD       PIC X(4).
               10  BR-LINES-OUT            PIC 9(6).
               10  BR-PAGES-OUT            PIC 9(3).
               10  BR-HASH-TOTAL           PIC S9(15) COMP-3.
               10  BR-REASON-CD            PIC X(4).
               10  BR-EXT-FILLER           PIC X(20).
