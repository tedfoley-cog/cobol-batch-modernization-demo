      ******************************************************************
      * CVCONSTY - SYSTEM WIDE LITERALS AND CONSTANTS                  *
      ******************************************************************
       01  WS-CONSTANTS.
           05  WS-SYSTEM-ID                PIC X(8)  VALUE 'CARDSVC '.
           05  WS-TRAN-CARD                PIC X(4)  VALUE 'CA00'.
           05  WS-TRAN-ADMIN               PIC X(4)  VALUE 'CA99'.
      *
      *    DATE WINDOWING PIVOT FOR 6 DIGIT DATES
           05  WS-CENTURY-PIVOT            PIC 9(2)  VALUE 50.
           05  WS-CENTURY-19               PIC 9(2)  VALUE 19.
           05  WS-CENTURY-20               PIC 9(2)  VALUE 20.
      *
           05  WS-MODULE-CARDSVC           PIC X(8)  VALUE 'CARDSVC '.
           05  WS-MODULE-PARTYRSK          PIC X(8)  VALUE 'PARTYRSK'.
      *
           05  WS-DISPATCHER-ONLINE        PIC X(8)  VALUE 'CACRD90 '.
           05  WS-DISPATCHER-BATCH         PIC X(8)  VALUE 'CBCRD90 '.
           05  WS-ERROR-PGM-ONLINE         PIC X(8)  VALUE 'CACRD91 '.
           05  WS-ERROR-PGM-BATCH          PIC X(8)  VALUE 'CBCRD91 '.
      *
      *    ROUTE KEYS USED WHEN CROSSING INTO PARTYRSK
           05  WS-ROUTE-RISKSVC            PIC X(8)  VALUE 'RISKSVC '.
           05  WS-ROUTE-KYCINQ             PIC X(8)  VALUE 'KYCINQ  '.
           05  WS-ROUTE-RSKRECAL           PIC X(8)  VALUE 'RSKRECAL'.
      *
           05  WS-CURRENCY-USD             PIC X(3)  VALUE 'USD'.
           05  WS-COUNTRY-USA              PIC X(3)  VALUE 'USA'.
      *
           05  WS-MAX-AUTH-AMT             PIC S9(9)V99 COMP-3
                                                     VALUE 999999.99.
           05  WS-CASH-ADV-FEE-PCT         PIC S9(3)V9(5) COMP-3
                                                     VALUE 3.00000.
           05  WS-VELOCITY-WINDOW-MINS     PIC 9(4)  VALUE 0060.
           05  WS-VELOCITY-MAX-CNT         PIC 9(2)  VALUE 05.
      *
           05  WS-COMMIT-FREQUENCY         PIC 9(6)  VALUE 001000.
      *
           05  WS-RC-OK                    PIC 9(4)  VALUE 0000.
           05  WS-RC-WARNING               PIC 9(4)  VALUE 0004.
           05  WS-RC-ERROR                 PIC 9(4)  VALUE 0008.
           05  WS-RC-FATAL                 PIC 9(4)  VALUE 0012.
