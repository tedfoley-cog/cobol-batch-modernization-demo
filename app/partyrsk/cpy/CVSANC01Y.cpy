      ******************************************************************
      * CVSANC01Y - SANCTIONS / WATCHLIST RECORD (PARTYRSK INTERNAL)   *
      * DB2 PARTYDB.PARTY_SANCTION AND VSAM WATCHLST.                  *
      ******************************************************************
       01  SANCTION-RECORD.
           05  SN-KEY.
               10  SN-LIST-CD              PIC X(8).
                   88  SN-LIST-OFAC        VALUE 'OFAC    '.
                   88  SN-LIST-UN          VALUE 'UN      '.
                   88  SN-LIST-EU          VALUE 'EU      '.
                   88  SN-LIST-INTERNAL    VALUE 'INTERNAL'.
               10  SN-ENTRY-ID             PIC X(16).
           05  SN-ENTITY-NAME              PIC X(60).
           05  SN-ENTITY-TYPE              PIC X.
           05  SN-COUNTRY                  PIC X(3).
           05  SN-DOB                      PIC 9(8).
           05  SN-LISTED-DATE              PIC 9(8).
           05  SN-DELISTED-DATE            PIC 9(8).
           05  SN-ACTIVE-FLG               PIC X.
           05  SN-PROGRAM-CD               PIC X(12).
           05  SN-FILLER                   PIC X(20).
      *
      ******************************************************************
      * MATCH RESULT RETURNED BY THE SCREENING ROUTINE                 *
      ******************************************************************
       01  SANCTION-MATCH.
           05  SM-PARTY-ID                 PIC X(11).
           05  SM-SCREEN-DATE              PIC 9(8).
           05  SM-MATCH-CNT                PIC 9(2).
           05  SM-BEST-SCORE               PIC 9(3).
           05  SM-HIT-FLG                  PIC X.
               88  SM-HIT                  VALUE 'Y'.
               88  SM-NO-HIT               VALUE 'N'.
               88  SM-POSSIBLE             VALUE 'P'.
           05  SM-MATCH OCCURS 5 TIMES.
               10  SM-MT-LIST-CD           PIC X(8).
               10  SM-MT-ENTRY-ID          PIC X(16).
               10  SM-MT-SCORE             PIC 9(3).
               10  SM-MT-DISPOSITION       PIC X(4).
