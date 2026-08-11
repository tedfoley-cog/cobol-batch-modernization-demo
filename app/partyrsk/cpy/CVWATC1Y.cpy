      ******************************************************************
      * CVWATC1Y - WATCHLST VSAM FILE RECORD (PARTYRSK INTERNAL)       *
      *                                                                *
      * PRTY.PROD.WATCHLST KSDS.  160 BYTE FIXED RECORD.               *
      * PRIMARY KEY  - WL-KEY, OFFSET 0 LENGTH 24                      *
      * ALTERNATE    - WL-ENTITY-NAME, OFFSET 24 LENGTH 60             *
      *                                                                *
      * THE FIRST 125 BYTES MATCH CVSANC01Y SO THAT A DB2 ROW CAN BE   *
      * MOVED STRAIGHT ONTO THE FILE RECORD.  THE TRAILER CARRIES THE  *
      * SCREENING KEYS BUILT BY THE WEEKLY REBUILD AND IS NOT HELD ON  *
      * PARTYDB.PARTY_SANCTION.                                        *
      ******************************************************************
       01  WATCH-LIST-RECORD.
           05  WL-KEY.
               10  WL-LIST-CD              PIC X(8).
                   88  WL-LIST-OFAC        VALUE 'OFAC    '.
                   88  WL-LIST-UN          VALUE 'UN      '.
                   88  WL-LIST-EU          VALUE 'EU      '.
                   88  WL-LIST-INTERNAL    VALUE 'INTERNAL'.
               10  WL-ENTRY-ID             PIC X(16).
           05  WL-ENTITY-NAME              PIC X(60).
           05  WL-ENTITY-TYPE              PIC X.
               88  WL-TYPE-INDIVIDUAL      VALUE 'I'.
               88  WL-TYPE-ORGANISATION    VALUE 'O'.
               88  WL-TYPE-VESSEL          VALUE 'V'.
           05  WL-COUNTRY                  PIC X(3).
           05  WL-DOB                      PIC 9(8).
           05  WL-LISTED-DATE              PIC 9(8).
           05  WL-DELISTED-DATE            PIC 9(8).
           05  WL-ACTIVE-FLG               PIC X.
               88  WL-ACTIVE               VALUE 'Y'.
               88  WL-DELISTED             VALUE 'N'.
           05  WL-PROGRAM-CD               PIC X(12).
      *
      *    TRAILER - BUILT BY THE WEEKLY REBUILD ONLY
           05  WL-SEARCH-NAME              PIC X(25).
           05  WL-SOUNDEX                  PIC X(4).
           05  WL-LOAD-DATE                PIC 9(6).
