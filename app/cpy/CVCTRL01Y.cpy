      ******************************************************************
      * CVCTRL01Y - BATCH CYCLE CONTROL RECORD                         *
      * VSAM CYCLCTL.  ONE RECORD PER CYCLE TYPE.                      *
      * ALSO CARRIES THE CHECKPOINT/RESTART POSITION.                  *
      ******************************************************************
       01  CYCLE-CTRL-RECORD.
           05  CC-KEY.
               10  CC-CYCLE-TYPE           PIC X(8).
                   88  CC-CYCLE-NIGHTLY    VALUE 'CARDNITE'.
                   88  CC-CYCLE-BILLING    VALUE 'CARDBILL'.
                   88  CC-CYCLE-REFDATA    VALUE 'CARDREF '.
                   88  CC-CYCLE-PARTYWK    VALUE 'PARTYWK '.
               10  CC-CYCLE-DATE           PIC 9(8).
           05  CC-CYCLE-ID                 PIC X(8).
           05  CC-STATUS                   PIC X.
               88  CC-NOT-STARTED          VALUE 'N'.
               88  CC-RUNNING              VALUE 'R'.
               88  CC-COMPLETE             VALUE 'C'.
               88  CC-FAILED               VALUE 'F'.
               88  CC-RESTARTED            VALUE 'S'.
           05  CC-CURRENT-STEP             PIC X(8).
           05  CC-LAST-GOOD-STEP           PIC X(8).
           05  CC-START-TS                 PIC X(26).
           05  CC-END-TS                   PIC X(26).
      *
      *    CHECKPOINT - CBCRD04J AND CBCRD07J COMMIT EVERY
      *    CC-COMMIT-FREQ RECORDS AND REWRITE THIS RECORD
           05  CC-CHECKPOINT.
               10  CC-COMMIT-FREQ          PIC 9(6).
               10  CC-RECS-READ            PIC 9(9).
               10  CC-RECS-WRITTEN         PIC 9(9).
               10  CC-RECS-REJECTED        PIC 9(9).
               10  CC-LAST-KEY             PIC X(32).
               10  CC-RESTART-CNT          PIC 9(2).
      *
           05  CC-TOTALS.
               10  CC-TOTAL-DR-AMT         PIC S9(13)V99 COMP-3.
               10  CC-TOTAL-CR-AMT         PIC S9(13)V99 COMP-3.
               10  CC-HASH-TOTAL           PIC S9(15) COMP-3.
           05  CC-ONLINE-CLOSED-FLG        PIC X.
           05  CC-FILLER                   PIC X(30).
