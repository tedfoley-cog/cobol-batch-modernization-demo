      ******************************************************************
      * CACRD04 - AUTHORIZATION REQUEST ENTRY AND VALIDATION           *
      *                                                                *
      * FIRST PROGRAM OF THE AUTHORIZATION CHAIN.  RECEIVES THE        *
      * REQUEST SCREEN, EDITS EVERY FIELD AND BUILDS THE AUTH-RECORD   *
      * SKELETON, THEN PASSES CONTROL DOWN THE CHAIN.                  *
      *                                                                *
      * THE SKELETON IS PARKED IN THE TEMPORARY STORAGE QUEUE CARDAUTQ *
      * BECAUSE IT WILL NOT FIT IN THE 512 BYTE CHAIN COMMAREA.  THE   *
      * QUEUE IS DELETED BY CACRD17 AT THE END OF THE SESSION.         *
      *                                                                *
      * CALLED BY   - CACRD90 XCTL, ROUTE MENU/OPT04                   *
      * CALLS       - CACRD05  XCTL, RISK ASSESSMENT                   *
      *             - CACRD00  XCTL ON PF3                             *
      *             - CACRD91  ERROR HANDLER                           *
      * MAPSET      - CARDSET   MAP CARDAUT                            *
      * TSQ         - CARDAUTQ  AUTH RECORD SKELETON                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD04.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD04 '.
       01  WS-NEXT-PGM                 PIC X(8)  VALUE 'CACRD05 '.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CACRD00 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDSET '.
       01  WS-MAP                      PIC X(8)  VALUE 'CARDAUT '.
       01  WS-AUTH-TSQ                 PIC X(8)  VALUE 'CARDAUTQ'.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-TSQ-ITEM                 PIC S9(4) COMP VALUE 1.
       01  WS-TSQ-LENGTH               PIC S9(4) COMP VALUE 0.
      *
       01  WS-ERASE-SW                 PIC X     VALUE 'Y'.
           88  WS-ERASE                          VALUE 'Y'.
       01  WS-VALID-SW                 PIC X     VALUE 'Y'.
           88  WS-VALID                          VALUE 'Y'.
           88  WS-NOT-VALID                      VALUE 'N'.
      *
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE ZERO.
       01  WS-DATE-OUT                 PIC X(8)  VALUE SPACES.
       01  WS-DATE-YYYYMMDD            PIC 9(8)  VALUE ZERO.
       01  WS-TIME-HHMMSS              PIC 9(6)  VALUE ZERO.
       01  WS-TIMESTAMP                PIC X(26) VALUE SPACES.
      *
      *    ---------------------------------------------------------
      *    MOD 10 CHECK DIGIT WORK AREA
      *    ---------------------------------------------------------
       01  WS-MOD10-WORK.
           05  WS-M10-CARD             PIC X(16) VALUE SPACES.
           05  WS-M10-DIGITS REDEFINES WS-M10-CARD.
               10  WS-M10-DIGIT        PIC 9 OCCURS 16 TIMES.
           05  WS-M10-SUB              PIC S9(4) COMP VALUE 0.
           05  WS-M10-POS              PIC S9(4) COMP VALUE 0.
           05  WS-M10-VAL              PIC S9(4) COMP VALUE 0.
           05  WS-M10-TOTAL            PIC S9(4) COMP VALUE 0.
           05  WS-M10-REMAIN           PIC S9(4) COMP VALUE 0.
           05  WS-M10-ODD-SW           PIC X     VALUE 'Y'.
      *
      *    AMOUNT EDIT - THE SCREEN FIELD IS FREE FORMAT 13 CHARACTERS
       01  WS-AMT-IN                   PIC X(13) VALUE SPACES.
       01  WS-AMT-DIGITS               PIC 9(11) VALUE ZERO.
       01  WS-AMT-NUM REDEFINES WS-AMT-DIGITS.
           05  WS-AMT-WHOLE            PIC 9(9).
           05  WS-AMT-CENTS            PIC 9(2).
       01  WS-AMT-PACKED               PIC S9(9)V99 COMP-3 VALUE ZERO.
       01  WS-AMT-SUB                  PIC S9(4) COMP VALUE 0.
       01  WS-AMT-DEC-SW               PIC X     VALUE 'N'.
       01  WS-AMT-DEC-CNT              PIC S9(4) COMP VALUE 0.
       01  WS-EDIT-AMT                 PIC ---,---,---,--9.99.
      *
       01  WS-CHAR                     PIC X     VALUE SPACE.
      *
       01  WS-MSG-OK                   PIC X(60) VALUE
           'KEY THE AUTHORIZATION REQUEST AND PRESS ENTER'.
       01  WS-MSG-CARD-BAD             PIC X(60) VALUE
           'CARD NUMBER FAILS THE CHECK DIGIT TEST'.
       01  WS-MSG-CARD-REQD            PIC X(60) VALUE
           'CARD NUMBER IS REQUIRED - 16 DIGITS'.
       01  WS-MSG-AMT-BAD              PIC X(60) VALUE
           'AMOUNT NOT VALID - USE 999999.99 FORMAT'.
       01  WS-MSG-AMT-HIGH             PIC X(60) VALUE
           'AMOUNT EXCEEDS THE ONLINE AUTHORIZATION MAXIMUM'.
       01  WS-MSG-AMT-ZERO             PIC X(60) VALUE
           'AMOUNT MUST BE GREATER THAN ZERO'.
       01  WS-MSG-CURR-BAD             PIC X(60) VALUE
           'CURRENCY CODE IS REQUIRED - THREE CHARACTERS'.
       01  WS-MSG-MCC-BAD              PIC X(60) VALUE
           'MCC IS REQUIRED AND MUST BE FOUR DIGITS'.
       01  WS-MSG-MERCH-BAD            PIC X(60) VALUE
           'MERCHANT ID IS REQUIRED'.
       01  WS-MSG-MODE-BAD             PIC X(60) VALUE
           'ENTRY MODE MUST BE S, C, K, E OR T'.
       01  WS-MSG-TYPE-BAD             PIC X(60) VALUE
           'AUTH TYPE MUST BE P PURCHASE, C CASH OR R REFUND'.
       01  WS-MSG-ATM-BAD              PIC X(60) VALUE
           'ATM ID AND NETWORK ARE REQUIRED FOR A CASH ADVANCE'.
       01  WS-MSG-REFUND-BAD           PIC X(60) VALUE
           'ORIGINAL AUTH ID AND DATE ARE REQUIRED FOR A REFUND'.
      *
           COPY CVAUTHW1Y.
           COPY CVAUTH01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
           COPY CARDSET.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           IF EIBCALEN = ZERO
               PERFORM 9100-NO-COMMAREA
               GO TO 0000-RETURN
           END-IF
      *
           MOVE DFHCOMMAREA            TO CA-WORK-AREA
           PERFORM 0100-INIT
      *
           IF CAW-SCREEN-ID NOT = 'CARDAUT '
               PERFORM 1000-FIRST-DISPLAY
               GO TO 0000-RETURN
           END-IF
      *
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2000-PROCESS-INPUT
               WHEN DFHPF3
                   PERFORM 7000-BACK-TO-MENU
               WHEN DFHPF12
                   PERFORM 7000-BACK-TO-MENU
               WHEN DFHCLEAR
                   PERFORM 7000-BACK-TO-MENU
               WHEN OTHER
                   MOVE 'KEY NOT ACTIVE - USE ENTER, PF3 OR PF12'
                                       TO CAW-MSG
                   PERFORM 5000-SEND-SCREEN
           END-EVALUATE
           .
       0000-RETURN.
           EXEC CICS RETURN
                     TRANSID(WS-TRAN-CARD)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
           END-EXEC
           GOBACK
           .
      *
       0100-INIT.
           MOVE SPACES                 TO ERROR-AREA
           MOVE LOW-VALUES             TO CARDAUTO
           MOVE 'Y'                    TO WS-VALID-SW
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT)
                     DATESEP('/')
                     RESP(WS-RESP)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-YYYYMMDD)
                     TIME(WS-TIME-HHMMSS)
                     RESP(WS-RESP)
           END-EXEC
      *
           IF CAW-TRAIL-CNT < 8
               ADD 1                   TO CAW-TRAIL-CNT
               MOVE WS-PGM-ID          TO CAW-TRAIL-PGM(CAW-TRAIL-CNT)
               MOVE ZERO               TO CAW-TRAIL-RC(CAW-TRAIL-CNT)
           END-IF
           .
      *
      ******************************************************************
      * 1000 - PAINT THE EMPTY REQUEST SCREEN                          *
      ******************************************************************
       1000-FIRST-DISPLAY.
           MOVE 'Y'                    TO WS-ERASE-SW
           MOVE 'CARDAUT '             TO CAW-SCREEN-ID
           MOVE SPACES                 TO CAW-AUTH-REQ
           MOVE ZERO                   TO CAW-RQ-AMT
                                          CAW-RQ-MCC
                                          CAW-RQ-ORIG-DATE
           MOVE SPACES                 TO CAW-RISK
                                          CAW-DECISION
           MOVE ZERO                   TO CAW-RSK-SCORE
                                          CAW-RSK-RC
                                          CAW-RSK-EXPOSURE
                                          CAW-RSK-AVAIL
                                          CAW-FRAUD-SCORE
                                          CAW-FRAUD-SEQ
                                          CAW-LIM-AVAIL
                                          CAW-LIM-OVER-AMT
                                          CAW-VELOCITY-CNT
                                          CAW-AUTH-SEQ-NUM
                                          CAW-AUTH-DATE
                                          CAW-AUTH-TIME
      *
           MOVE WS-CURRENCY-USD        TO CAW-RQ-CURR
           MOVE 'P'                    TO CAW-RQ-AUTH-TYPE
      *
           IF CAW-CARD-NUM NOT = SPACES AND CAW-CARD-NUM NOT =
              LOW-VALUES
               MOVE CAW-CARD-NUM       TO CAW-RQ-CARD-NUM
           END-IF
      *
           MOVE WS-MSG-OK              TO CAW-MSG
           PERFORM 5000-SEND-SCREEN
           .
      *
      ******************************************************************
      * 2000 - RECEIVE AND EDIT                                        *
      ******************************************************************
       2000-PROCESS-INPUT.
           EXEC CICS RECEIVE
                     MAP(WS-MAP)
                     MAPSET(WS-MAPSET)
                     INTO(CARDAUTI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE WS-MSG-OK      TO CAW-MSG
                   PERFORM 5000-SEND-SCREEN
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE '2000-PROCESS-INPUT'
                                       TO ER-PARAGRAPH
                   PERFORM 8100-CICS-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           PERFORM 2100-EDIT-CARD
           IF WS-VALID
               PERFORM 2200-EDIT-TYPE
           END-IF
           IF WS-VALID
               PERFORM 2300-EDIT-AMOUNT
           END-IF
           IF WS-VALID
               PERFORM 2400-EDIT-MERCHANT
           END-IF
           IF WS-VALID
               PERFORM 2500-EDIT-VARIANT
           END-IF
      *
           IF WS-NOT-VALID
               PERFORM 5000-SEND-SCREEN
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 3000-BUILD-AUTH-RECORD
           PERFORM 3900-PARK-AUTH-RECORD
           PERFORM 6000-XCTL-NEXT
      *
      *    ONLY REACHED IF THE TRANSFER FAILED
           MOVE 'AUTHORIZATION CHAIN NOT AVAILABLE - CALL SUPPORT'
                                       TO CAW-MSG
           PERFORM 5000-SEND-SCREEN
           .
       2000-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2100 - CARD NUMBER.  16 DIGITS AND A VALID MOD 10 CHECK DIGIT. *
      ******************************************************************
       2100-EDIT-CARD.
           IF AUCARDL = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-CARD-REQD   TO CAW-MSG
               GO TO 2100-EXIT
           END-IF
      *
           MOVE AUCARDI                TO WS-M10-CARD
           IF WS-M10-CARD IS NOT NUMERIC
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-CARD-REQD   TO CAW-MSG
               GO TO 2100-EXIT
           END-IF
      *
           PERFORM 2150-MOD-10-CHECK
           IF WS-M10-REMAIN NOT = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-CARD-BAD    TO CAW-MSG
               GO TO 2100-EXIT
           END-IF
      *
           MOVE WS-M10-CARD            TO CAW-RQ-CARD-NUM
                                          CAW-CARD-NUM
           .
       2100-EXIT.
           EXIT
           .
      *
      *    DOUBLE EVERY SECOND DIGIT FROM THE RIGHT, CASTING OUT NINES,
      *    AND THE TOTAL MUST BE DIVISIBLE BY TEN.
       2150-MOD-10-CHECK.
           MOVE ZERO                   TO WS-M10-TOTAL
      *
           PERFORM VARYING WS-M10-SUB FROM 16 BY -1
                     UNTIL WS-M10-SUB < 1
               COMPUTE WS-M10-POS = 16 - WS-M10-SUB + 1
               MOVE WS-M10-DIGIT(WS-M10-SUB)
                                       TO WS-M10-VAL
      *
               IF FUNCTION MOD(WS-M10-POS, 2) = ZERO
                   COMPUTE WS-M10-VAL = WS-M10-VAL * 2
                   IF WS-M10-VAL > 9
                       SUBTRACT 9    FROM WS-M10-VAL
                   END-IF
               END-IF
      *
               ADD WS-M10-VAL          TO WS-M10-TOTAL
           END-PERFORM
      *
           COMPUTE WS-M10-REMAIN = FUNCTION MOD(WS-M10-TOTAL, 10)
           .
      *
      ******************************************************************
      * 2200 - AUTHORIZATION TYPE                                      *
      ******************************************************************
       2200-EDIT-TYPE.
           IF AUTYPEL = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-TYPE-BAD    TO CAW-MSG
               GO TO 2200-EXIT
           END-IF
      *
           EVALUATE AUTYPEI
               WHEN 'P'
               WHEN 'p'
                   MOVE 'P'            TO CAW-RQ-AUTH-TYPE
               WHEN 'C'
               WHEN 'c'
                   MOVE 'C'            TO CAW-RQ-AUTH-TYPE
               WHEN 'R'
               WHEN 'r'
                   MOVE 'R'            TO CAW-RQ-AUTH-TYPE
               WHEN OTHER
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE WS-MSG-TYPE-BAD
                                       TO CAW-MSG
           END-EVALUATE
           .
       2200-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2300 - AMOUNT.  THE FIELD IS KEYED FREE FORMAT SO THE DIGITS   *
      *        ARE PICKED OUT BY HAND AND SCALED TO TWO PLACES.        *
      ******************************************************************
       2300-EDIT-AMOUNT.
           MOVE AUAMTI                 TO WS-AMT-IN
           MOVE ZERO                   TO WS-AMT-DIGITS
                                          WS-AMT-DEC-CNT
           MOVE 'N'                    TO WS-AMT-DEC-SW
      *
           IF AUAMTL = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-AMT-BAD     TO CAW-MSG
               GO TO 2300-EXIT
           END-IF
      *
           PERFORM VARYING WS-AMT-SUB FROM 1 BY 1
                     UNTIL WS-AMT-SUB > 13
               MOVE WS-AMT-IN(WS-AMT-SUB:1)
                                       TO WS-CHAR
               EVALUATE TRUE
                   WHEN WS-CHAR = SPACE OR LOW-VALUE
                       CONTINUE
                   WHEN WS-CHAR = ','
                       CONTINUE
                   WHEN WS-CHAR = '.'
                       IF WS-AMT-DEC-SW = 'Y'
                           MOVE 'N'    TO WS-VALID-SW
                       END-IF
                       MOVE 'Y'        TO WS-AMT-DEC-SW
                   WHEN WS-CHAR IS NUMERIC
                       COMPUTE WS-AMT-DIGITS =
                               WS-AMT-DIGITS * 10 + FUNCTION NUMVAL
                               (WS-CHAR)
                       IF WS-AMT-DEC-SW = 'Y'
                           ADD 1       TO WS-AMT-DEC-CNT
                       END-IF
                   WHEN OTHER
                       MOVE 'N'        TO WS-VALID-SW
               END-EVALUATE
           END-PERFORM
      *
           IF WS-NOT-VALID
               MOVE WS-MSG-AMT-BAD     TO CAW-MSG
               GO TO 2300-EXIT
           END-IF
      *
      *    NO DECIMAL POINT KEYED MEANS WHOLE UNITS
           EVALUATE WS-AMT-DEC-CNT
               WHEN 0
                   COMPUTE WS-AMT-PACKED = WS-AMT-DIGITS
               WHEN 1
                   COMPUTE WS-AMT-PACKED = WS-AMT-DIGITS / 10
               WHEN 2
                   COMPUTE WS-AMT-PACKED = WS-AMT-DIGITS / 100
               WHEN OTHER
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE WS-MSG-AMT-BAD TO CAW-MSG
           END-EVALUATE
      *
           IF WS-NOT-VALID
               GO TO 2300-EXIT
           END-IF
      *
           IF WS-AMT-PACKED = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-AMT-ZERO    TO CAW-MSG
               GO TO 2300-EXIT
           END-IF
      *
           IF WS-AMT-PACKED > WS-MAX-AUTH-AMT
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-AMT-HIGH    TO CAW-MSG
               GO TO 2300-EXIT
           END-IF
      *
           MOVE WS-AMT-PACKED          TO CAW-RQ-AMT
           .
       2300-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2400 - CURRENCY, MCC, MERCHANT, COUNTRY, TERMINAL, ENTRY MODE  *
      ******************************************************************
       2400-EDIT-MERCHANT.
           IF AUCURRL = ZERO OR AUCURRI = SPACES
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-CURR-BAD    TO CAW-MSG
               GO TO 2400-EXIT
           END-IF
           MOVE AUCURRI                TO CAW-RQ-CURR
      *
           IF AUMCCL = ZERO OR AUMCCI IS NOT NUMERIC
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-MCC-BAD     TO CAW-MSG
               GO TO 2400-EXIT
           END-IF
           MOVE AUMCCI                 TO CAW-RQ-MCC
      *
      *    A REFUND CARRIES THE ORIGINAL MERCHANT INSTEAD
           IF AUMRCHL = ZERO AND CAW-RQ-AUTH-TYPE NOT = 'C'
               MOVE 'N'                TO WS-VALID-SW
               MOVE WS-MSG-MERCH-BAD   TO CAW-MSG
               GO TO 2400-EXIT
           END-IF
           MOVE AUMRCHI                TO CAW-RQ-MERCH-ID
      *
           IF AUCTRYL = ZERO
               MOVE WS-COUNTRY-USA     TO CAW-RQ-MERCH-CTRY
           ELSE
               MOVE AUCTRYI            TO CAW-RQ-MERCH-CTRY
           END-IF
      *
           MOVE AUTERMI                TO CAW-RQ-TERMINAL
           IF AUTERML = ZERO
               MOVE EIBTRMID           TO CAW-RQ-TERMINAL
           END-IF
      *
           EVALUATE AUMODEI
               WHEN 'S'
               WHEN 'C'
               WHEN 'K'
               WHEN 'E'
               WHEN 'T'
                   MOVE AUMODEI        TO CAW-RQ-ENTRY-MODE
               WHEN OTHER
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE WS-MSG-MODE-BAD
                                       TO CAW-MSG
           END-EVALUATE
           .
       2400-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 2500 - THE FIELDS THAT ONLY APPLY TO ONE VARIANT               *
      ******************************************************************
       2500-EDIT-VARIANT.
           EVALUATE CAW-RQ-AUTH-TYPE
               WHEN 'C'
                   IF AUATML = ZERO OR AUNETL = ZERO
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE WS-MSG-ATM-BAD
                                       TO CAW-MSG
                   ELSE
                       MOVE AUATMI     TO CAW-RQ-ATM-ID
                       MOVE AUNETI     TO CAW-RQ-NETWORK
                   END-IF
               WHEN 'R'
                   IF AUOAUTL = ZERO OR AUODTL = ZERO
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE WS-MSG-REFUND-BAD
                                       TO CAW-MSG
                   ELSE
                       MOVE AUOAUTI    TO CAW-RQ-ORIG-AUTH
                       IF AUODTI IS NUMERIC
                           MOVE AUODTI TO CAW-RQ-ORIG-DATE
                       ELSE
                           MOVE 'N'    TO WS-VALID-SW
                           MOVE WS-MSG-REFUND-BAD
                                       TO CAW-MSG
                       END-IF
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
      ******************************************************************
      * 3000 - BUILD THE AUTH RECORD SKELETON                          *
      *                                                                *
      * ONLY THE FIELDS OF THE VARIANT SELECTED BY AUTH-TYPE ARE SET.  *
      * THE THREE OVERLAYS ARE NOT THE SAME LENGTH, SO THE TAIL OF THE *
      * 60 BYTE DETAIL AREA STILL HOLDS WHATEVER THE PREVIOUS REQUEST  *
      * LEFT THERE - THAT IS DELIBERATE AND MATCHES THE FILE FORMAT    *
      * THE NIGHTLY CYCLE EXPECTS.  DO NOT CLEAR IT.                   *
      ******************************************************************
       3000-BUILD-AUTH-RECORD.
           MOVE CAW-RQ-CARD-NUM        TO AUTH-CARD-NUM
           MOVE WS-DATE-YYYYMMDD       TO AUTH-DATE
           MOVE ZERO                   TO AUTH-SEQ-NUM
           MOVE CAW-ACCT-ID            TO AUTH-ACCT-ID
           MOVE CAW-CUST-ID            TO AUTH-CUST-ID
           MOVE WS-TIME-HHMMSS         TO AUTH-TIME
           MOVE CAW-RQ-AUTH-TYPE       TO AUTH-TYPE
           MOVE SPACES                 TO AUTH-STATUS
                                          AUTH-RESP-CODE
                                          AUTH-REASON-CD
           MOVE ZERO                   TO AUTH-RISK-SCORE
           MOVE SPACES                 TO AUTH-RISK-BAND
           MOVE 'N'                    TO AUTH-SETTLED-FLG
                                          AUTH-POSTED-FLG
      *
           EVALUATE AUTH-TYPE
               WHEN 'P'
                   MOVE CAW-RQ-MERCH-ID
                                       TO AP-MERCHANT-ID
                   MOVE SPACES         TO AP-MERCH-NAME
                   MOVE CAW-RQ-MCC     TO AP-MCC
                   MOVE CAW-RQ-TERMINAL
                                       TO AP-TERMINAL-ID
                   MOVE CAW-RQ-AMT     TO AP-AMOUNT
                   MOVE CAW-RQ-CURR    TO AP-CURRENCY
                   MOVE CAW-RQ-ENTRY-MODE
                                       TO AP-ENTRY-MODE
               WHEN 'C'
                   MOVE CAW-RQ-ATM-ID  TO AC-ATM-ID
                   MOVE CAW-RQ-NETWORK TO AC-NETWORK
                   MOVE CAW-RQ-MERCH-ID(1:11)
                                       TO AC-ACQUIRER-ID
                   MOVE CAW-RQ-AMT     TO AC-AMOUNT
                   COMPUTE AC-FEE ROUNDED =
                           CAW-RQ-AMT * WS-CASH-ADV-FEE-PCT / 100
                   MOVE CAW-RQ-CURR    TO AC-CURRENCY
               WHEN 'R'
                   MOVE CAW-RQ-ORIG-AUTH
                                       TO AR-ORIG-AUTH-ID
                   MOVE CAW-RQ-ORIG-DATE
                                       TO AR-ORIG-DATE
                   MOVE CAW-RQ-MERCH-ID(1:15)
                                       TO AR-ORIG-MERCHANT
                   MOVE CAW-RQ-AMT     TO AR-AMOUNT
           END-EVALUATE
      *
           MOVE WS-PGM-ID              TO AUTH-ORIG-PGM
           MOVE EIBTRMID               TO AUTH-TERM-ID
           MOVE CAW-OPER-ID            TO AUTH-OPER-ID
      *
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) RESP(WS-RESP) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-YYYYMMDD)
                     TIME(WS-TIME-HHMMSS)
                     RESP(WS-RESP)
           END-EXEC
           MOVE SPACES                 TO WS-TIMESTAMP
           MOVE WS-DATE-YYYYMMDD       TO WS-TIMESTAMP(1:8)
           MOVE WS-TIME-HHMMSS         TO WS-TIMESTAMP(10:6)
           MOVE WS-TIMESTAMP           TO AUTH-TIMESTAMP
      *
           MOVE WS-DATE-YYYYMMDD       TO CAW-AUTH-DATE
           MOVE WS-TIME-HHMMSS         TO CAW-AUTH-TIME
           .
      *
      ******************************************************************
      * 3900 - PARK THE SKELETON FOR CACRD10                           *
      ******************************************************************
       3900-PARK-AUTH-RECORD.
           EXEC CICS DELETEQ TS
                     QUEUE(WS-AUTH-TSQ)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE LENGTH OF AUTH-RECORD  TO WS-TSQ-LENGTH
      *
           EXEC CICS WRITEQ TS
                     QUEUE(WS-AUTH-TSQ)
                     FROM(AUTH-RECORD)
                     LENGTH(WS-TSQ-LENGTH)
                     ITEM(WS-TSQ-ITEM)
                     MAIN
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '3900-PARK-AUTH-RECORD'
                                       TO ER-PARAGRAPH
               MOVE 'CANNOT WRITE THE AUTH WORK QUEUE'
                                       TO ER-MESSAGE
               PERFORM 8100-CICS-ERROR
           END-IF
           .
      *
      ******************************************************************
      * 5000 - SEND                                                    *
      ******************************************************************
       5000-SEND-SCREEN.
           MOVE WS-DATE-OUT            TO AUDATEO
           MOVE CAW-MSG                TO AUMSGO
           MOVE 'CARDAUT '             TO CAW-SCREEN-ID
      *
           MOVE CAW-RQ-CARD-NUM        TO AUCARDO
           MOVE CAW-RQ-AUTH-TYPE       TO AUTYPEO
           MOVE CAW-RQ-CURR            TO AUCURRO
           MOVE CAW-RQ-MERCH-ID        TO AUMRCHO
           MOVE CAW-RQ-MERCH-CTRY      TO AUCTRYO
           MOVE CAW-RQ-TERMINAL        TO AUTERMO
           MOVE CAW-RQ-ENTRY-MODE      TO AUMODEO
           MOVE CAW-RQ-ATM-ID          TO AUATMO
           MOVE CAW-RQ-NETWORK         TO AUNETO
           MOVE CAW-RQ-ORIG-AUTH       TO AUOAUTO
      *
           IF CAW-RQ-MCC > ZERO
               MOVE CAW-RQ-MCC         TO AUMCCO
           END-IF
           IF CAW-RQ-ORIG-DATE > ZERO
               MOVE CAW-RQ-ORIG-DATE   TO AUODTO
           END-IF
           IF CAW-RQ-AMT NOT = ZERO
               MOVE CAW-RQ-AMT         TO WS-EDIT-AMT
               MOVE WS-EDIT-AMT(4:13)  TO AUAMTO
           END-IF
      *
           IF WS-ERASE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDAUTO)
                         ERASE
                         CURSOR
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
               MOVE 'N'                TO WS-ERASE-SW
           ELSE
               EXEC CICS SEND
                         MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(CARDAUTO)
                         DATAONLY
                         CURSOR
                         FREEKB
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           .
      *
      ******************************************************************
      * 6000 - HAND THE REQUEST TO THE RISK STEP                       *
      ******************************************************************
       6000-XCTL-NEXT.
           MOVE SPACES                 TO CAW-SCREEN-ID
           MOVE WS-PGM-ID              TO CAW-FROM-PGM
      *
           EXEC CICS XCTL
                     PROGRAM(WS-NEXT-PGM)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           MOVE '6000-XCTL-NEXT'       TO ER-PARAGRAPH
           PERFORM 8100-CICS-ERROR
           .
      *
       7000-BACK-TO-MENU.
           MOVE SPACES                 TO CAW-SCREEN-ID
                                          CAW-MSG
      *
           EXEC CICS XCTL
                     PROGRAM(WS-MENU-PGM)
                     COMMAREA(CA-WORK-AREA)
                     LENGTH(LENGTH OF CA-WORK-AREA)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE '7000-BACK-TO-MENU'    TO ER-PARAGRAPH
           PERFORM 8100-CICS-ERROR
           .
      *
      ******************************************************************
      * 8000 - ERROR HANDLING                                          *
      ******************************************************************
       8100-CICS-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'CICS'                 TO ER-ERROR-TYPE
           MOVE 'E'                    TO ER-SEVERITY
           MOVE WS-RESP                TO ER-EIBRESP
           MOVE WS-RESP2               TO ER-EIBRESP2
           MOVE EIBFN                  TO ER-EIBFN
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE EIBTRMID               TO ER-TERM-ID
           MOVE 'N'                    TO ER-ABEND-REQUESTED
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM-ONLINE)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
       9100-NO-COMMAREA.
           EXEC CICS SEND
                     TEXT('CACRD04 MUST BE STARTED FROM THE CA00 MENU')
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
           EXEC CICS RETURN END-EXEC
           .
