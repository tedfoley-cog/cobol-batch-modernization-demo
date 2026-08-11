      ******************************************************************
      * CVERRS01Y - ERROR / ABEND HANDLING AREA                        *
      * PASSED TO CACRD91 (ONLINE) AND CBCRD91 (BATCH) ERROR ROUTINES. *
      ******************************************************************
       01  ERROR-AREA.
           05  ER-PGM-NAME                 PIC X(8).
           05  ER-PARAGRAPH                PIC X(30).
           05  ER-SEVERITY                 PIC X.
               88  ER-SEV-INFO             VALUE 'I'.
               88  ER-SEV-WARNING          VALUE 'W'.
               88  ER-SEV-ERROR            VALUE 'E'.
               88  ER-SEV-FATAL            VALUE 'F'.
           05  ER-ERROR-TYPE               PIC X(4).
               88  ER-TYPE-SQL             VALUE 'SQL '.
               88  ER-TYPE-VSAM            VALUE 'VSAM'.
               88  ER-TYPE-CICS            VALUE 'CICS'.
               88  ER-TYPE-ROUTE           VALUE 'ROUT'.
               88  ER-TYPE-DATA            VALUE 'DATA'.
               88  ER-TYPE-BUSINESS        VALUE 'BUSN'.
           05  ER-REASON-CD                PIC X(4).
           05  ER-MESSAGE                  PIC X(78).
      *
           05  ER-SQL-DETAIL.
               10  ER-SQLCODE              PIC S9(9) COMP.
               10  ER-SQLSTATE             PIC X(5).
               10  ER-SQL-TABLE            PIC X(18).
               10  ER-SQL-OPERATION        PIC X(8).
      *
           05  ER-VSAM-DETAIL.
               10  ER-FILE-NAME            PIC X(8).
               10  ER-FILE-STATUS          PIC X(2).
               10  ER-VSAM-RC              PIC 9(4).
               10  ER-VSAM-KEY             PIC X(32).
      *
           05  ER-CICS-DETAIL.
               10  ER-EIBRESP              PIC S9(8) COMP.
               10  ER-EIBRESP2             PIC S9(8) COMP.
               10  ER-EIBFN                PIC X(2).
               10  ER-TRAN-ID              PIC X(4).
               10  ER-TERM-ID              PIC X(4).
      *
           05  ER-ABEND-CODE               PIC X(4).
           05  ER-ABEND-REQUESTED          PIC X.
           05  ER-TIMESTAMP                PIC X(26).
           05  ER-FILLER                   PIC X(20).
