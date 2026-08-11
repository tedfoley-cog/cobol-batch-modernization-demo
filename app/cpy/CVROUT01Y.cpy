      ******************************************************************
      * CVROUT01Y - PROGRAM ROUTE TABLE AREA                           *
      *                                                                *
      * USED BY THE DISPATCHERS CACRD90 (ONLINE) AND CBCRD90 (BATCH).  *
      * MIRRORS CARDDB.PGM_ROUTE AND THE VSAM PGMROUT FALLBACK FILE.   *
      ******************************************************************
       01  ROUTE-RECORD.
           05  RT-KEY.
               10  RT-ROUTE-TYPE           PIC X(4).
                   88  RT-TYPE-MENU        VALUE 'MENU'.
                   88  RT-TYPE-FRAUD       VALUE 'FRAU'.
                   88  RT-TYPE-FEE         VALUE 'FEEC'.
                   88  RT-TYPE-XMODULE     VALUE 'XMOD'.
                   88  RT-TYPE-STMT        VALUE 'STMT'.
               10  RT-ROUTE-KEY            PIC X(8).
               10  RT-SEQ-NBR              PIC 9(4).
           05  RT-PGM-NAME                 PIC X(8).
           05  RT-CALL-TYPE                PIC X.
               88  RT-CALL-LINK            VALUE 'L'.
               88  RT-CALL-XCTL            VALUE 'X'.
               88  RT-CALL-STATIC          VALUE 'C'.
               88  RT-CALL-DYNAMIC         VALUE 'D'.
           05  RT-MODULE-ID                PIC X(8).
               88  RT-MOD-CARDSVC          VALUE 'CARDSVC '.
               88  RT-MOD-PARTYRSK         VALUE 'PARTYRSK'.
           05  RT-EFF-DATE                 PIC 9(8).
           05  RT-EXP-DATE                 PIC 9(8).
           05  RT-ACTIVE-FLG               PIC X.
               88  RT-ACTIVE               VALUE 'Y'.
           05  RT-FALLBACK-PGM             PIC X(8).
           05  RT-DESCRIPTION              PIC X(40).
      *
      ******************************************************************
      * DISPATCHER WORK AREA - CACHED ROUTES FOR THE LIFE OF THE TASK  *
      ******************************************************************
       01  ROUTE-CACHE.
           05  RC-LOADED-FLG               PIC X VALUE 'N'.
               88  RC-LOADED               VALUE 'Y'.
           05  RC-SOURCE                   PIC X.
               88  RC-FROM-DB2             VALUE 'D'.
               88  RC-FROM-VSAM            VALUE 'V'.
           05  RC-ENTRY-CNT                PIC 9(4) VALUE ZERO.
           05  RC-ENTRY OCCURS 200 TIMES
                       INDEXED BY RC-IDX.
               10  RC-ROUTE-TYPE           PIC X(4).
               10  RC-ROUTE-KEY            PIC X(8).
               10  RC-SEQ-NBR              PIC 9(4).
               10  RC-PGM-NAME             PIC X(8).
               10  RC-CALL-TYPE            PIC X.
               10  RC-MODULE-ID            PIC X(8).
               10  RC-FALLBACK-PGM         PIC X(8).
      *
       01  ROUTE-REQUEST.
           05  RQ-ROUTE-TYPE               PIC X(4).
           05  RQ-ROUTE-KEY                PIC X(8).
           05  RQ-SEQ-NBR                  PIC 9(4).
           05  RQ-RESOLVED-PGM             PIC X(8).
           05  RQ-RESOLVED-CALL            PIC X.
           05  RQ-RESOLVED-MOD             PIC X(8).
           05  RQ-USED-FALLBACK            PIC X.
           05  RQ-RC                       PIC 9(4).
               88  RQ-RC-OK                VALUE 0000.
               88  RQ-RC-NOT-FOUND         VALUE 0008.
               88  RQ-RC-TABLE-ERROR       VALUE 0012.
