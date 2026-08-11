      ******************************************************************
      * CACRD14 - ONLINE PAYMENT POSTING                               *
      *                                                                *
      * MENU OPTION 8.  POSTS A COUNTER OR TELEPHONE PAYMENT TO A CARD *
      * ACCOUNT AND REVERSES ONE POSTED EARLIER THE SAME DAY.          *
      *                                                                *
      * CALLED BY  - CACRD90 BY XCTL, ROUTE MENU / OPT08               *
      * RETURNS TO - CACRD00 BY XCTL                                   *
      * CALLS      - CACRD91 BY LINK FOR ERROR DISPLAY                 *
      * MAPSET     - CARDST2, MAP CRD14A                               *
      * TABLES     - CARDSVC.ACCOUNT      SELECT UPDATE                *
      *              CARDSVC.CARD         SELECT                       *
      *              CARDSVC.CARD_LIMIT   SELECT UPDATE                *
      *              CARDSVC.TRANSACTION  SELECT INSERT UPDATE         *
      *                                                                *
      * EVERY UPDATE MADE BY ONE PASS OF THIS PROGRAM IS COMMITTED BY  *
      * A SINGLE SYNCPOINT.  ANY FAILURE ROLLS THE WHOLE PAYMENT BACK  *
      * - A PART POSTED PAYMENT IS WORSE THAN NO PAYMENT AT ALL.       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CACRD14.
       AUTHOR.        CARD SYSTEMS.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-PGM-ID                   PIC X(8)  VALUE 'CACRD14 '.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CARDST2 '.
       01  WS-MAP-PAY                  PIC X(8)  VALUE 'CRD14A  '.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CACRD00 '.
      *
       01  WS-RESP                     PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                    PIC S9(8) COMP VALUE 0.
       01  WS-ABSTIME                  PIC S9(15) COMP-3 VALUE 0.
      *
       01  WS-SWITCHES.
           05  WS-ERROR-SW             PIC X     VALUE 'N'.
               88  WS-ERROR-FOUND                VALUE 'Y'.
           05  WS-SIGN-SW              PIC X     VALUE '+'.
               88  WS-AMOUNT-NEGATIVE            VALUE '-'.
           05  WS-CLEARED-SW           PIC X     VALUE 'N'.
               88  WS-ARREARS-CLEARED            VALUE 'Y'.
      *
       01  WS-COUNTERS.
           05  WS-IDX                  PIC S9(4) COMP VALUE 0.
           05  WS-LEG-LEN              PIC S9(4) COMP VALUE 0.
      *
      *    THE LEG TABLE IN CVTRAN01Y STARTS AT OFFSET 150 AND EACH
      *    OCCURRENCE IS 24 BYTES LONG.
       01  WS-LEG-CONSTANTS.
           05  WS-LEG-OFFSET           PIC S9(4) COMP VALUE 150.
           05  WS-LEG-SIZE             PIC S9(4) COMP VALUE 24.
      *
       01  WS-AMOUNTS.
           05  WS-PAY-AMT              PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-ARREARS-PART         PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-PRINCIPAL-PART       PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-NEW-BALANCE          PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-NEW-MIN-PAY          PIC S9(9)V99 COMP-3  VALUE 0.
           05  WS-NEW-DELQ-AMT         PIC S9(9)V99 COMP-3  VALUE 0.
           05  WS-NEW-DELQ-BUCKET      PIC S9(4) COMP       VALUE 0.
           05  WS-EDIT-AMT             PIC ZZZ,ZZZ,ZZ9.99-.
           05  WS-EDIT-BAL             PIC Z,ZZZ,ZZZ,ZZ9.99-.
      *
      ******************************************************************
      * ZONED DECIMAL SCREEN INPUT.                                    *
      *                                                                *
      * THE PAYMENT AMOUNT IS KEYED UNSIGNED.  THE REVERSAL AMOUNT HAS *
      * ALWAYS BEEN KEYED WITH THE SIGN OVERPUNCHED ON THE LAST BYTE   *
      * THE WAY THE OLD 3270 PAYMENT SLIPS WERE PUNCHED - 1234} IS     *
      * -123.40, 1234J IS -123.41 AND SO ON.  THE OVERPUNCH IS         *
      * DECODED BY HAND BELOW BECAUSE THE FIELD ARRIVES AS CHARACTERS  *
      * AND A NUMERIC TEST WOULD REJECT IT.                            *
      ******************************************************************
       01  WS-ZONED-INPUT.
           05  WS-ZI-CHARS             PIC X(13) VALUE SPACES.
       01  WS-ZONED-INPUT-R REDEFINES WS-ZONED-INPUT.
           05  WS-ZI-CHAR OCCURS 13 TIMES
                                       PIC X.
      *
       01  WS-ZONED-WORK.
           05  WS-ZW-DIGITS            PIC X(13) VALUE ZEROES.
           05  WS-ZW-LAST-BYTE         PIC X     VALUE '0'.
           05  WS-ZW-LAST-DIGIT        PIC X     VALUE '0'.
           05  WS-ZW-LEN               PIC S9(4) COMP VALUE 0.
           05  WS-ZW-NUMERIC           PIC 9(11)V99 VALUE ZERO.
           05  WS-ZW-NUM-R REDEFINES WS-ZW-NUMERIC
                                       PIC X(13).
      *
       01  WS-OVERPUNCH-TABLE.
           05  FILLER                  PIC X(30) VALUE
               '{0+A1+B2+C3+D4+E5+F6+G7+H8+I9+'.
           05  FILLER                  PIC X(30) VALUE
               '}0-J1-K2-L3-M4-N5-O6-P7-Q8-R9-'.
       01  WS-OVERPUNCH-R REDEFINES WS-OVERPUNCH-TABLE.
           05  WS-OP-ENTRY OCCURS 20 TIMES.
               10  WS-OP-CHAR          PIC X.
               10  WS-OP-DIGIT         PIC X.
               10  WS-OP-SIGN          PIC X.
      *
       01  WS-DATE-AREAS.
           05  WS-TODAY-YYYYMMDD       PIC 9(8)  VALUE ZERO.
           05  WS-TODAY-R REDEFINES WS-TODAY-YYYYMMDD.
               10  WS-TODAY-CC         PIC 9(2).
               10  WS-TODAY-YY         PIC 9(2).
               10  WS-TODAY-MM         PIC 9(2).
               10  WS-TODAY-DD         PIC 9(2).
           05  WS-DISPLAY-DATE         PIC X(10) VALUE SPACES.
           05  WS-ISO-TODAY            PIC X(10) VALUE SPACES.
           05  WS-ISO-R REDEFINES WS-ISO-TODAY.
               10  WS-ISO-YYYY         PIC X(4).
               10  WS-ISO-SEP1         PIC X.
               10  WS-ISO-MM           PIC X(2).
               10  WS-ISO-SEP2         PIC X.
               10  WS-ISO-DD           PIC X(2).
      *
       01  WS-JULIAN.
           05  WS-JUL-YY               PIC 9(2)  VALUE ZERO.
           05  WS-JUL-DDD              PIC 9(3)  VALUE ZERO.
      *
       01  WS-MESSAGES.
           05  WS-MSG-ENTER            PIC X(78) VALUE
               'KEY ACCOUNT AND PAYMENT AMOUNT THEN PRESS ENTER'.
           05  WS-MSG-ACCT-REQD        PIC X(78) VALUE
               'ACCOUNT NUMBER MUST BE ENTERED'.
           05  WS-MSG-ACCT-BAD         PIC X(78) VALUE
               'ACCOUNT NUMBER MUST BE NUMERIC'.
           05  WS-MSG-NO-ACCT          PIC X(78) VALUE
               'ACCOUNT NOT ON FILE'.
           05  WS-MSG-ACCT-SHUT        PIC X(78) VALUE
               'ACCOUNT IS CLOSED - PAYMENT MUST GO TO RECOVERIES'.
           05  WS-MSG-ACCT-WOFF        PIC X(78) VALUE
               'ACCOUNT IS WRITTEN OFF - REFER TO COLLECTIONS'.
           05  WS-MSG-FUNC-BAD         PIC X(78) VALUE
               'FUNCTION MUST BE P TO POST OR R TO REVERSE'.
           05  WS-MSG-AMT-REQD         PIC X(78) VALUE
               'PAYMENT AMOUNT MUST BE ENTERED'.
           05  WS-MSG-AMT-BAD          PIC X(78) VALUE
               'PAYMENT AMOUNT IS NOT A VALID ZONED DECIMAL FIELD'.
           05  WS-MSG-AMT-ZERO         PIC X(78) VALUE
               'PAYMENT AMOUNT MUST BE GREATER THAN ZERO'.
           05  WS-MSG-AMT-SIGN         PIC X(78) VALUE
               'A POSTED PAYMENT MUST NOT CARRY A NEGATIVE OVERPUNCH'.
           05  WS-MSG-REV-SIGN         PIC X(78) VALUE
               'REVERSAL AMOUNT MUST CARRY A NEGATIVE OVERPUNCH'.
           05  WS-MSG-METH-BAD         PIC X(78) VALUE
               'METHOD MUST BE CASH CHQU DDEB WIRE OR TFER'.
           05  WS-MSG-CARD-BAD         PIC X(78) VALUE
               'CARD IS NOT ON THAT ACCOUNT'.
           05  WS-MSG-RTID-REQD        PIC X(78) VALUE
               'TRANSACTION ID OF THE PAYMENT TO REVERSE IS REQUIRED'.
           05  WS-MSG-RTID-BAD         PIC X(78) VALUE
               'PAYMENT NOT FOUND ON THIS ACCOUNT FOR TODAY'.
           05  WS-MSG-RTID-OLD         PIC X(78) VALUE
               'ONLY PAYMENTS POSTED TODAY MAY BE REVERSED ONLINE'.
           05  WS-MSG-RTID-DONE        PIC X(78) VALUE
               'THAT PAYMENT HAS ALREADY BEEN REVERSED'.
           05  WS-MSG-REV-AMT          PIC X(78) VALUE
               'REVERSAL AMOUNT MUST EQUAL THE ORIGINAL PAYMENT'.
           05  WS-MSG-POSTED           PIC X(78) VALUE
               'PAYMENT POSTED'.
           05  WS-MSG-POSTED-CLR       PIC X(78) VALUE
               'PAYMENT POSTED - ARREARS CLEARED'.
           05  WS-MSG-REVERSED         PIC X(78) VALUE
               'PAYMENT REVERSED'.
      *
      ******************************************************************
      * PROGRAM COMMAREA                                               *
      ******************************************************************
       01  WS-COMMAREA.
           05  CA-PGM-ID               PIC X(8).
           05  CA-FROM-PGM             PIC X(8).
           05  CA-STEP                 PIC X.
           05  CA-ACCT-ID              PIC 9(11).
           05  CA-CARD-NUM             PIC X(16).
           05  CA-FUNCTION             PIC X.
               88  CA-FUNC-POST                VALUE 'P'.
               88  CA-FUNC-REVERSE             VALUE 'R'.
           05  CA-METHOD               PIC X(4).
           05  CA-PAY-AMT              PIC S9(11)V99 COMP-3.
           05  CA-POSTED-TXN-ID        PIC X(16).
           05  CA-REV-TXN-ID           PIC X(16).
           05  CA-OPER-ID              PIC X(8).
           05  CA-FILLER               PIC X(430).
      *
           COPY CVTRAN01Y.
           COPY CVERRS01Y.
           COPY CVCONSTY.
      *
      *    BMS SYMBOLIC MAPS FOR MAPSET CARDST2
           COPY CARDST2.
      *
      ******************************************************************
      * DB2 HOST VARIABLES                                             *
      ******************************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  DCL-ACCOUNT.
           05  DCL-ACCT-ID             PIC S9(11) COMP-3.
           05  DCL-CUST-ID             PIC S9(9)  COMP-3.
           05  DCL-ACCT-STATUS         PIC X(1).
           05  DCL-CURRENCY-CD         PIC X(3).
           05  DCL-CURR-BAL            PIC S9(13)V99 COMP-3.
           05  DCL-MIN-PAY-DUE         PIC S9(11)V99 COMP-3.
           05  DCL-LAST-PAY-AMT        PIC S9(11)V99 COMP-3.
           05  DCL-LAST-PAY-DATE       PIC X(10).
           05  DCL-DELQ-BUCKET         PIC S9(4) COMP.
           05  DCL-DELQ-AMT            PIC S9(11)V99 COMP-3.
      *
       01  DCL-CARD.
           05  DCL-CARD-NUM            PIC X(16).
           05  DCL-CARD-STATUS         PIC X(1).
           05  DCL-CARD-ACCT-ID        PIC S9(11) COMP-3.
      *
       01  DCL-LIMIT.
           05  DCL-LIMIT-AMT           PIC S9(13)V99 COMP-3.
           05  DCL-USED-AMT            PIC S9(13)V99 COMP-3.
           05  DCL-AVAIL-AMT           PIC S9(13)V99 COMP-3.
      *
       01  DCL-TXN.
           05  DCL-TXN-ID              PIC X(16).
           05  DCL-POST-DATE           PIC X(10).
           05  DCL-TXN-TYPE-CD         PIC X(4).
           05  DCL-TXN-AMT             PIC S9(13)V99 COMP-3.
           05  DCL-BILLING-AMT         PIC S9(13)V99 COMP-3.
           05  DCL-TXN-DESC            PIC X(40).
           05  DCL-GL-POSTED-FLG       PIC X(1).
           05  DCL-DISPUTE-FLG         PIC X(1).
           05  DCL-LEG-CNT             PIC S9(4) COMP.
           05  DCL-LEG-DATA.
               49  DCL-LEG-LEN         PIC S9(4) COMP.
               49  DCL-LEG-TEXT        PIC X(400).
      *
       01  DCL-ORIG.
           05  DCL-ORIG-TXN-ID         PIC X(16).
           05  DCL-ORIG-AMT            PIC S9(13)V99 COMP-3.
           05  DCL-ORIG-DESC           PIC X(40).
           05  DCL-ORIG-CARD           PIC X(16).
      *
       01  DCL-REV-CNT                 PIC S9(9) COMP.
      *
       01  IND-LAST-PAY-DATE           PIC S9(4) COMP.
       01  IND-ORIG-CARD               PIC S9(4) COMP.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(512).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
       0000-MAIN-LINE.
           MOVE SPACES                 TO ERROR-AREA
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE LOW-VALUES             TO CRD14AO
      *
           IF EIBCALEN = ZERO
               MOVE 'DATA'             TO ER-ERROR-TYPE
               MOVE 'CACRD14 ENTERED WITH NO COMMAREA'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               PERFORM 0900-RETURN-MENU
               GO TO 0000-EXIT
           END-IF
      *
           MOVE DFHCOMMAREA            TO WS-COMMAREA
           PERFORM 8100-GET-DATE
      *
           IF CA-PGM-ID NOT = WS-PGM-ID
               PERFORM 1000-FIRST-TIME
               GO TO 0000-EXIT
           END-IF
      *
           EVALUATE EIBAID
               WHEN DFHPF3
                   PERFORM 0900-RETURN-MENU
                   GO TO 0000-EXIT
               WHEN DFHPF12
                   PERFORM 9600-EXIT-SESSION
                   GO TO 0000-EXIT
               WHEN DFHCLEAR
                   PERFORM 1000-FIRST-TIME
                   GO TO 0000-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           PERFORM 2000-PROCESS-SCREEN
           .
       0000-EXIT.
           EXEC CICS RETURN END-EXEC
           GOBACK
           .
      *
       0900-RETURN-MENU.
           MOVE SPACES                 TO WS-COMMAREA
           MOVE WS-PGM-ID              TO CA-FROM-PGM
           EXEC CICS XCTL
                     PROGRAM(WS-MENU-PGM)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'XCTL TO MENU FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
           END-IF
           .
      *
       1000-FIRST-TIME.
           MOVE SPACES                 TO WS-COMMAREA
           MOVE WS-PGM-ID              TO CA-PGM-ID
           MOVE '1'                    TO CA-STEP
           MOVE EIBTRMID               TO CA-OPER-ID
           MOVE ZERO                   TO CA-ACCT-ID
           MOVE ZERO                   TO CA-PAY-AMT
           MOVE 'P'                    TO CA-FUNCTION
           MOVE 'P'                    TO M14FUNCO
           MOVE 'CASH'                 TO M14METHO
           MOVE WS-MSG-ENTER           TO M14MSGO
           PERFORM 8200-SEND-MAP
           .
      *
      ******************************************************************
      * 2000 - MAIN SCREEN PASS                                        *
      ******************************************************************
       2000-PROCESS-SCREEN.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE 'N'                    TO WS-CLEARED-SW
      *
           EXEC CICS RECEIVE
                     MAP(WS-MAP-PAY)
                     MAPSET(WS-MAPSET)
                     INTO(CRD14AI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE WS-MSG-ENTER   TO M14MSGO
                   PERFORM 8200-SEND-MAP
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'CICS'         TO ER-ERROR-TYPE
                   MOVE WS-RESP        TO ER-EIBRESP
                   MOVE 'RECEIVE MAP CRD14A FAILED'
                                       TO ER-MESSAGE
                   PERFORM 9000-REPORT-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE
      *
           PERFORM 2100-EDIT-HEADER
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2200-READ-ACCOUNT
           IF WS-ERROR-FOUND
               PERFORM 8200-SEND-MAP
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 2400-PAINT-POSITION
      *
           IF CA-FUNC-REVERSE
               PERFORM 5000-REVERSE-PAYMENT
           ELSE
               PERFORM 3000-POST-PAYMENT
           END-IF
      *
           PERFORM 8200-SEND-MAP
           .
       2000-EXIT.
           EXIT
           .
      *
       2100-EDIT-HEADER.
           IF M14ACCTL = ZERO OR M14ACCTI = SPACES
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ACCT-REQD   TO M14MSGO
               GO TO 2100-EXIT
           END-IF
      *
           IF M14ACCTI IS NOT NUMERIC
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-ACCT-BAD    TO M14MSGO
               GO TO 2100-EXIT
           END-IF
           MOVE M14ACCTI               TO CA-ACCT-ID
      *
           IF M14CARDL > ZERO
               MOVE M14CARDI           TO CA-CARD-NUM
           ELSE
               MOVE SPACES             TO CA-CARD-NUM
           END-IF
      *
           IF M14FUNCL = ZERO OR M14FUNCI = SPACES
               MOVE 'P'                TO CA-FUNCTION
           ELSE
               MOVE M14FUNCI           TO CA-FUNCTION
           END-IF
      *
           IF NOT CA-FUNC-POST AND NOT CA-FUNC-REVERSE
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-FUNC-BAD    TO M14MSGO
               GO TO 2100-EXIT
           END-IF
      *
           IF M14METHL > ZERO AND M14METHI NOT = SPACES
               MOVE M14METHI           TO CA-METHOD
           ELSE
               MOVE 'CASH'             TO CA-METHOD
           END-IF
      *
           EVALUATE CA-METHOD
               WHEN 'CASH'
               WHEN 'CHQU'
               WHEN 'DDEB'
               WHEN 'WIRE'
               WHEN 'TFER'
                   CONTINUE
               WHEN OTHER
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-METH-BAD
                                       TO M14MSGO
                   GO TO 2100-EXIT
           END-EVALUATE
      *
           IF M14PAMTL = ZERO OR M14PAMTI = SPACES
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-AMT-REQD    TO M14MSGO
               GO TO 2100-EXIT
           END-IF
      *
           MOVE M14PAMTI               TO WS-ZI-CHARS
           PERFORM 7000-DECODE-ZONED
           IF WS-ERROR-FOUND
               MOVE WS-MSG-AMT-BAD     TO M14MSGO
               GO TO 2100-EXIT
           END-IF
      *
           IF WS-ZW-NUMERIC = ZERO
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-AMT-ZERO    TO M14MSGO
               GO TO 2100-EXIT
           END-IF
      *
      *    THE OVERPUNCH DECIDES THE SIGN, NOT THE FUNCTION CODE.  A
      *    POSTING WITH A NEGATIVE OVERPUNCH IS A KEYING ERROR AND A
      *    REVERSAL WITHOUT ONE IS TOO.
           IF CA-FUNC-POST AND WS-AMOUNT-NEGATIVE
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-AMT-SIGN    TO M14MSGO
               GO TO 2100-EXIT
           END-IF
      *
           IF CA-FUNC-REVERSE AND NOT WS-AMOUNT-NEGATIVE
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-REV-SIGN    TO M14MSGO
               GO TO 2100-EXIT
           END-IF
      *
           MOVE WS-ZW-NUMERIC          TO WS-PAY-AMT
           IF WS-AMOUNT-NEGATIVE
               COMPUTE WS-PAY-AMT = 0 - WS-ZW-NUMERIC
           END-IF
           MOVE WS-PAY-AMT             TO CA-PAY-AMT
      *
           IF CA-FUNC-REVERSE
               IF M14RTIDL = ZERO OR M14RTIDI = SPACES
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-RTID-REQD
                                       TO M14MSGO
                   GO TO 2100-EXIT
               END-IF
               MOVE M14RTIDI           TO CA-REV-TXN-ID
           END-IF
           .
       2100-EXIT.
           EXIT
           .
      *
       2200-READ-ACCOUNT.
           MOVE CA-ACCT-ID             TO DCL-ACCT-ID
      *
           EXEC SQL
               SELECT CUST_ID
                    , ACCT_STATUS
                    , CURRENCY_CD
                    , CURR_BAL
                    , MIN_PAY_DUE
                    , LAST_PAY_AMT
                    , CHAR(LAST_PAY_DATE, ISO)
                    , DELQ_BUCKET
                    , DELQ_AMT
                 INTO :DCL-CUST-ID
                    , :DCL-ACCT-STATUS
                    , :DCL-CURRENCY-CD
                    , :DCL-CURR-BAL
                    , :DCL-MIN-PAY-DUE
                    , :DCL-LAST-PAY-AMT
                    , :DCL-LAST-PAY-DATE :IND-LAST-PAY-DATE
                    , :DCL-DELQ-BUCKET
                    , :DCL-DELQ-AMT
                 FROM CARDSVC.ACCOUNT
                WHERE ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-NO-ACCT TO M14MSGO
                   GO TO 2200-EXIT
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'ACCOUNT           '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
                   GO TO 2200-EXIT
           END-EVALUATE
      *
           EVALUATE DCL-ACCT-STATUS
               WHEN 'C'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-ACCT-SHUT
                                       TO M14MSGO
               WHEN 'W'
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-ACCT-WOFF
                                       TO M14MSGO
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           IF WS-ERROR-FOUND
               GO TO 2200-EXIT
           END-IF
      *
           IF CA-CARD-NUM NOT = SPACES
               PERFORM 2300-READ-CARD
           ELSE
               PERFORM 2350-PICK-CARD
           END-IF
           .
       2200-EXIT.
           EXIT
           .
      *
       2300-READ-CARD.
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
      *
           EXEC SQL
               SELECT CARD_STATUS
                    , ACCT_ID
                 INTO :DCL-CARD-STATUS
                    , :DCL-CARD-ACCT-ID
                 FROM CARDSVC.CARD
                WHERE CARD_NUM = :DCL-CARD-NUM
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF DCL-CARD-ACCT-ID NOT = DCL-ACCT-ID
                       MOVE 'Y'        TO WS-ERROR-SW
                       MOVE WS-MSG-CARD-BAD
                                       TO M14MSGO
                   END-IF
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE WS-MSG-CARD-BAD
                                       TO M14MSGO
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD              '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      *    NO CARD KEYED - THE PAYMENT IS BOOKED AGAINST THE OLDEST
      *    ACTIVE PLASTIC ON THE ACCOUNT.  THAT IS THE PRIMARY CARD IN
      *    EVERY CASE THE BRANCHES HAVE EVER RAISED.
       2350-PICK-CARD.
           EXEC SQL
               SELECT CARD_NUM
                 INTO :DCL-CARD-NUM
                 FROM CARDSVC.CARD
                WHERE ACCT_ID     = :DCL-ACCT-ID
                  AND CARD_STATUS IN ('A','B')
                ORDER BY ISSUE_DATE ASC
                        , CARD_NUM   ASC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   MOVE DCL-CARD-NUM   TO CA-CARD-NUM
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'NO ACTIVE CARD ON THE ACCOUNT - KEY CARD NBR'
                                       TO M14MSGO
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'CARD              '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       2400-PAINT-POSITION.
           MOVE CA-ACCT-ID             TO M14ACCTO
           MOVE CA-CARD-NUM            TO M14CARDO
           MOVE DCL-CURR-BAL           TO WS-EDIT-BAL
           MOVE WS-EDIT-BAL            TO M14CBALO
           MOVE DCL-MIN-PAY-DUE        TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M14MPAYO
           MOVE DCL-DELQ-BUCKET        TO M14DELQO
           MOVE DCL-DELQ-AMT           TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M14DAMTO
           MOVE DCL-LAST-PAY-AMT       TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M14LPAYO
      *
           IF IND-LAST-PAY-DATE < ZERO
               MOVE 'NONE'             TO M14LPDTO
           ELSE
               MOVE DCL-LAST-PAY-DATE  TO M14LPDTO
           END-IF
           .
      *
      ******************************************************************
      * 3000 - POST A PAYMENT                                          *
      *                                                                *
      * THE PAYMENT IS APPLIED TO ARREARS FIRST AND THEN TO            *
      * PRINCIPAL.  EACH PART BECOMES A LEG ON THE TRANSACTION SO THE  *
      * GL EXTRACT CAN SPLIT IT.                                       *
      ******************************************************************
       3000-POST-PAYMENT.
           PERFORM 3100-SPLIT-PAYMENT
           PERFORM 3200-BUILD-TXN-ID
           PERFORM 3300-BUILD-LEGS
      *
           PERFORM 3400-INSERT-TXN
           IF WS-ERROR-FOUND
               GO TO 3000-BACKOUT
           END-IF
      *
           PERFORM 3500-UPDATE-ACCOUNT
           IF WS-ERROR-FOUND
               GO TO 3000-BACKOUT
           END-IF
      *
           PERFORM 3600-RESTORE-LIMIT
           IF WS-ERROR-FOUND
               GO TO 3000-BACKOUT
           END-IF
      *
           EXEC CICS SYNCPOINT RESP(WS-RESP) END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SYNCPOINT FAILED POSTING PAYMENT'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               GO TO 3000-BACKOUT
           END-IF
      *
           MOVE CA-POSTED-TXN-ID       TO M14PTIDO
           MOVE WS-NEW-BALANCE         TO WS-EDIT-BAL
           MOVE WS-EDIT-BAL            TO M14NBALO
           MOVE WS-NEW-MIN-PAY         TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M14MPAYO
           MOVE WS-NEW-DELQ-BUCKET     TO M14DELQO
           MOVE WS-NEW-DELQ-AMT        TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M14DAMTO
           MOVE WS-PAY-AMT             TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT            TO M14LPAYO
           MOVE WS-ISO-TODAY           TO M14LPDTO
      *
           IF WS-ARREARS-CLEARED
               MOVE WS-MSG-POSTED-CLR  TO M14MSGO
           ELSE
               MOVE WS-MSG-POSTED      TO M14MSGO
           END-IF
           GO TO 3000-EXIT
           .
       3000-BACKOUT.
           EXEC CICS SYNCPOINT ROLLBACK RESP(WS-RESP) END-EXEC
           IF M14MSGO = SPACES OR LOW-VALUES
               MOVE 'PAYMENT NOT POSTED - ALL UPDATES BACKED OUT'
                                       TO M14MSGO
           END-IF
           .
       3000-EXIT.
           EXIT
           .
      *
       3100-SPLIT-PAYMENT.
           MOVE ZERO                   TO WS-ARREARS-PART
           MOVE ZERO                   TO WS-PRINCIPAL-PART
      *
           IF DCL-DELQ-AMT > ZERO
               IF WS-PAY-AMT < DCL-DELQ-AMT
                   MOVE WS-PAY-AMT     TO WS-ARREARS-PART
               ELSE
                   MOVE DCL-DELQ-AMT   TO WS-ARREARS-PART
               END-IF
           END-IF
      *
           COMPUTE WS-PRINCIPAL-PART = WS-PAY-AMT - WS-ARREARS-PART
           COMPUTE WS-NEW-BALANCE    = DCL-CURR-BAL - WS-PAY-AMT
           COMPUTE WS-NEW-DELQ-AMT   = DCL-DELQ-AMT - WS-ARREARS-PART
      *
           IF WS-NEW-DELQ-AMT NOT > ZERO
               MOVE ZERO               TO WS-NEW-DELQ-AMT
               MOVE ZERO               TO WS-NEW-DELQ-BUCKET
               IF DCL-DELQ-BUCKET > ZERO
                   MOVE 'Y'            TO WS-CLEARED-SW
               END-IF
           ELSE
               MOVE DCL-DELQ-BUCKET    TO WS-NEW-DELQ-BUCKET
           END-IF
      *
      *    THE MINIMUM PAYMENT IS REDUCED BY WHAT WAS PAID, NEVER
      *    RECALCULATED - THE CYCLE JOB OWNS THAT FIGURE.
           IF WS-PAY-AMT NOT < DCL-MIN-PAY-DUE
               MOVE ZERO               TO WS-NEW-MIN-PAY
           ELSE
               COMPUTE WS-NEW-MIN-PAY = DCL-MIN-PAY-DUE - WS-PAY-AMT
           END-IF
           .
      *
       3200-BUILD-TXN-ID.
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYDDD(WS-JULIAN)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE SPACES                 TO CA-POSTED-TXN-ID
           MOVE 'PY'                   TO CA-POSTED-TXN-ID(1:2)
           MOVE WS-JUL-YY              TO CA-POSTED-TXN-ID(3:2)
           MOVE WS-JUL-DDD             TO CA-POSTED-TXN-ID(5:3)
           MOVE EIBTRMID               TO CA-POSTED-TXN-ID(8:4)
           MOVE EIBTASKN               TO CA-POSTED-TXN-ID(12:5)
           .
      *
       3300-BUILD-LEGS.
           MOVE SPACES                 TO TXN-RECORD
           MOVE 1                      TO TXN-LEG-CNT
           MOVE 1                      TO TXN-LEG-SEQ(1)
           MOVE 'PRIN'                 TO TXN-LEG-TYPE(1)
           COMPUTE TXN-LEG-AMT(1) = 0 - WS-PRINCIPAL-PART
           MOVE '2101000100'           TO TXN-LEG-GL-ACCT(1)
           MOVE 'N'                    TO TXN-LEG-REVERSED(1)
      *
           IF WS-ARREARS-PART > ZERO
               MOVE 2                  TO TXN-LEG-CNT
               MOVE 2                  TO TXN-LEG-SEQ(2)
               MOVE 'INTR'             TO TXN-LEG-TYPE(2)
               COMPUTE TXN-LEG-AMT(2) = 0 - WS-ARREARS-PART
               MOVE '4201000100'       TO TXN-LEG-GL-ACCT(2)
               MOVE 'N'                TO TXN-LEG-REVERSED(2)
           END-IF
      *
           MOVE CA-OPER-ID             TO TXN-POSTED-BY
           MOVE SPACES                 TO TXN-POSTED-TS
           MOVE SPACES                 TO TXN-CYCLE-ID
           MOVE 'N'                    TO TXN-GL-POSTED-FLG
           MOVE 'N'                    TO TXN-DISPUTE-FLG
      *
           COMPUTE WS-LEG-LEN = TXN-LEG-CNT * WS-LEG-SIZE
           MOVE SPACES                 TO DCL-LEG-TEXT
           MOVE TXN-RECORD(WS-LEG-OFFSET:WS-LEG-LEN)
                                       TO DCL-LEG-TEXT(1:WS-LEG-LEN)
           MOVE WS-LEG-LEN             TO DCL-LEG-LEN
           MOVE TXN-LEG-CNT            TO DCL-LEG-CNT
           .
      *
       3400-INSERT-TXN.
           MOVE CA-POSTED-TXN-ID       TO DCL-TXN-ID
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
           COMPUTE DCL-TXN-AMT = 0 - WS-PAY-AMT
           MOVE DCL-TXN-AMT            TO DCL-BILLING-AMT
      *
           MOVE SPACES                 TO DCL-TXN-DESC
           STRING 'PAYMENT '           DELIMITED BY SIZE
                  CA-METHOD            DELIMITED BY SIZE
                  ' TERM '             DELIMITED BY SIZE
                  EIBTRMID             DELIMITED BY SIZE
             INTO DCL-TXN-DESC
           END-STRING
      *
           EXEC SQL
               INSERT INTO CARDSVC.TRANSACTION
                     (TXN_ID
                    , POST_DATE
                    , ACCT_ID
                    , CARD_NUM
                    , TXN_TYPE_CD
                    , TXN_SOURCE
                    , TXN_AMT
                    , CURRENCY_CD
                    , BILLING_AMT
                    , FX_RATE
                    , TXN_DESC
                    , TXN_LEG_CNT
                    , TXN_LEG_DATA
                    , GL_POSTED_FLG
                    , DISPUTE_FLG
                    , POSTED_BY
                    , POSTED_TS)
               VALUES (:DCL-TXN-ID
                    , CURRENT DATE
                    , :DCL-ACCT-ID
                    , :DCL-CARD-NUM
                    , 'PYMT'
                    , 'ON'
                    , :DCL-TXN-AMT
                    , :DCL-CURRENCY-CD
                    , :DCL-BILLING-AMT
                    , 1
                    , :DCL-TXN-DESC
                    , :DCL-LEG-CNT
                    , :DCL-LEG-DATA
                    , 'N'
                    , 'N'
                    , :CA-OPER-ID
                    , CURRENT TIMESTAMP)
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN -803
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'DUPLICATE PAYMENT ID - PRESS ENTER TO RETRY'
                                       TO M14MSGO
               WHEN OTHER
                   MOVE 'INSERT  '     TO ER-SQL-OPERATION
                   MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       3500-UPDATE-ACCOUNT.
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET CURR_BAL       = CURR_BAL - :WS-PAY-AMT
                    , LAST_PAY_AMT   = :WS-PAY-AMT
                    , LAST_PAY_DATE  = CURRENT DATE
                    , MIN_PAY_DUE    = :WS-NEW-MIN-PAY
                    , DELQ_AMT       = :WS-NEW-DELQ-AMT
                    , DELQ_BUCKET    = :WS-NEW-DELQ-BUCKET
                    , LAST_MAINT_PGM = 'CACRD14 '
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE ACCT_ID        = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'ACCOUNT ROW DISAPPEARED DURING POSTING'
                                       TO M14MSGO
               WHEN OTHER
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   MOVE 'ACCOUNT           '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      *    THE PAYMENT GIVES THE CUSTOMER HIS OPEN TO BUY BACK
      *    IMMEDIATELY.  ONLY THE CURRENT CREDIT LINE ROW IS TOUCHED.
       3600-RESTORE-LIMIT.
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
      *
           EXEC SQL
               UPDATE CARDSVC.CARD_LIMIT
                  SET USED_AMT      = CASE
                          WHEN USED_AMT < :WS-PAY-AMT THEN 0
                          ELSE USED_AMT - :WS-PAY-AMT
                      END
                    , AVAIL_AMT     = CASE
                          WHEN AVAIL_AMT + :WS-PAY-AMT > LIMIT_AMT
                               THEN LIMIT_AMT
                          ELSE AVAIL_AMT + :WS-PAY-AMT
                      END
                    , LAST_MAINT_TS = CURRENT TIMESTAMP
                WHERE CARD_NUM   = :DCL-CARD-NUM
                  AND LIMIT_TYPE = 'CRED'
                  AND EXP_DATE   = '9999-12-31'
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
      *            NO CURRENT LIMIT ROW.  THE PAYMENT STILL STANDS -
      *            THE NIGHTLY LIMIT REBUILD WILL PICK IT UP.
                   CONTINUE
               WHEN OTHER
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   MOVE 'CARD_LIMIT        '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
      ******************************************************************
      * 5000 - SAME DAY PAYMENT REVERSAL                               *
      ******************************************************************
       5000-REVERSE-PAYMENT.
           PERFORM 5100-READ-ORIGINAL
           IF WS-ERROR-FOUND
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5200-CHECK-NOT-REVERSED
           IF WS-ERROR-FOUND
               GO TO 5000-EXIT
           END-IF
      *
      *    WS-PAY-AMT IS NEGATIVE HERE - THE OVERPUNCH SAID SO.  THE
      *    ABSOLUTE VALUE MUST MATCH THE ORIGINAL TO THE CENT.
           IF WS-PAY-AMT + DCL-ORIG-AMT NOT = ZERO
               MOVE 'Y'                TO WS-ERROR-SW
               MOVE WS-MSG-REV-AMT     TO M14MSGO
               GO TO 5000-EXIT
           END-IF
      *
           PERFORM 5300-BUILD-REVERSAL
           PERFORM 5400-INSERT-REVERSAL
           IF WS-ERROR-FOUND
               GO TO 5000-BACKOUT
           END-IF
      *
           PERFORM 5500-FLAG-ORIGINAL
           IF WS-ERROR-FOUND
               GO TO 5000-BACKOUT
           END-IF
      *
           PERFORM 5600-UNDO-ACCOUNT
           IF WS-ERROR-FOUND
               GO TO 5000-BACKOUT
           END-IF
      *
           PERFORM 5700-UNDO-LIMIT
           IF WS-ERROR-FOUND
               GO TO 5000-BACKOUT
           END-IF
      *
           EXEC CICS SYNCPOINT RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SYNCPOINT FAILED REVERSING PAYMENT'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
               GO TO 5000-BACKOUT
           END-IF
      *
           MOVE CA-POSTED-TXN-ID       TO M14PTIDO
      *    DCL-ORIG-AMT WAS FLIPPED POSITIVE BY 5600-UNDO-ACCOUNT
           COMPUTE WS-NEW-BALANCE = DCL-CURR-BAL + DCL-ORIG-AMT
           MOVE WS-NEW-BALANCE         TO WS-EDIT-BAL
           MOVE WS-EDIT-BAL            TO M14NBALO
           MOVE WS-MSG-REVERSED        TO M14MSGO
           GO TO 5000-EXIT
           .
       5000-BACKOUT.
           EXEC CICS SYNCPOINT ROLLBACK RESP(WS-RESP) END-EXEC
           IF M14MSGO = SPACES OR LOW-VALUES
               MOVE 'REVERSAL NOT APPLIED - ALL UPDATES BACKED OUT'
                                       TO M14MSGO
           END-IF
           .
       5000-EXIT.
           EXIT
           .
      *
       5100-READ-ORIGINAL.
           MOVE CA-REV-TXN-ID          TO DCL-ORIG-TXN-ID
      *
           EXEC SQL
               SELECT TXN_AMT
                    , TXN_DESC
                    , CARD_NUM
                 INTO :DCL-ORIG-AMT
                    , :DCL-ORIG-DESC
                    , :DCL-ORIG-CARD :IND-ORIG-CARD
                 FROM CARDSVC.TRANSACTION
                WHERE TXN_ID      = :DCL-ORIG-TXN-ID
                  AND POST_DATE   = CURRENT DATE
                  AND ACCT_ID     = :DCL-ACCT-ID
                  AND TXN_TYPE_CD = 'PYMT'
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF IND-ORIG-CARD NOT < ZERO
                       MOVE DCL-ORIG-CARD
                                       TO CA-CARD-NUM
                   END-IF
               WHEN +100
      *            EITHER IT IS NOT THERE OR IT WAS POSTED ON AN
      *            EARLIER DAY.  TELL THE OPERATOR WHICH.
                   PERFORM 5150-CHECK-OLDER
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       5150-CHECK-OLDER.
           MOVE ZERO                   TO DCL-REV-CNT
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-REV-CNT
                 FROM CARDSVC.TRANSACTION
                WHERE TXN_ID  = :DCL-ORIG-TXN-ID
                  AND ACCT_ID = :DCL-ACCT-ID
           END-EXEC
      *
           MOVE 'Y'                    TO WS-ERROR-SW
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF DCL-REV-CNT > ZERO
                       MOVE WS-MSG-RTID-OLD
                                       TO M14MSGO
                   ELSE
                       MOVE WS-MSG-RTID-BAD
                                       TO M14MSGO
                   END-IF
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
           END-EVALUATE
           .
      *
       5200-CHECK-NOT-REVERSED.
           MOVE SPACES                 TO DCL-TXN-ID
           MOVE 'RV'                   TO DCL-TXN-ID(1:2)
           MOVE CA-REV-TXN-ID(3:14)    TO DCL-TXN-ID(3:14)
           MOVE ZERO                   TO DCL-REV-CNT
      *
           EXEC SQL
               SELECT COUNT(*)
                 INTO :DCL-REV-CNT
                 FROM CARDSVC.TRANSACTION
                WHERE TXN_ID    = :DCL-TXN-ID
                  AND POST_DATE = CURRENT DATE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   IF DCL-REV-CNT > ZERO
                       MOVE 'Y'        TO WS-ERROR-SW
                       MOVE WS-MSG-RTID-DONE
                                       TO M14MSGO
                   END-IF
               WHEN OTHER
                   MOVE 'SELECT  '     TO ER-SQL-OPERATION
                   MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       5300-BUILD-REVERSAL.
           MOVE SPACES                 TO CA-POSTED-TXN-ID
           MOVE 'RV'                   TO CA-POSTED-TXN-ID(1:2)
           MOVE CA-REV-TXN-ID(3:14)    TO CA-POSTED-TXN-ID(3:14)
      *
           MOVE SPACES                 TO TXN-RECORD
           MOVE 1                      TO TXN-LEG-CNT
           MOVE 1                      TO TXN-LEG-SEQ(1)
           MOVE 'PRIN'                 TO TXN-LEG-TYPE(1)
           COMPUTE TXN-LEG-AMT(1) = 0 - DCL-ORIG-AMT
           MOVE '2101000100'           TO TXN-LEG-GL-ACCT(1)
           MOVE 'Y'                    TO TXN-LEG-REVERSED(1)
      *
           MOVE CA-OPER-ID             TO TXN-POSTED-BY
           MOVE SPACES                 TO TXN-POSTED-TS
           MOVE SPACES                 TO TXN-CYCLE-ID
           MOVE 'N'                    TO TXN-GL-POSTED-FLG
           MOVE 'N'                    TO TXN-DISPUTE-FLG
      *
           COMPUTE WS-LEG-LEN = TXN-LEG-CNT * WS-LEG-SIZE
           MOVE SPACES                 TO DCL-LEG-TEXT
           MOVE TXN-RECORD(WS-LEG-OFFSET:WS-LEG-LEN)
                                       TO DCL-LEG-TEXT(1:WS-LEG-LEN)
           MOVE WS-LEG-LEN             TO DCL-LEG-LEN
           MOVE TXN-LEG-CNT            TO DCL-LEG-CNT
           .
      *
       5400-INSERT-REVERSAL.
           MOVE CA-POSTED-TXN-ID       TO DCL-TXN-ID
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
           COMPUTE DCL-TXN-AMT = 0 - DCL-ORIG-AMT
           MOVE DCL-TXN-AMT            TO DCL-BILLING-AMT
      *
           MOVE SPACES                 TO DCL-TXN-DESC
           STRING 'REVERSAL OF '       DELIMITED BY SIZE
                  CA-REV-TXN-ID        DELIMITED BY SIZE
             INTO DCL-TXN-DESC
           END-STRING
      *
           EXEC SQL
               INSERT INTO CARDSVC.TRANSACTION
                     (TXN_ID
                    , POST_DATE
                    , ACCT_ID
                    , CARD_NUM
                    , TXN_TYPE_CD
                    , TXN_SOURCE
                    , TXN_AMT
                    , CURRENCY_CD
                    , BILLING_AMT
                    , FX_RATE
                    , TXN_DESC
                    , TXN_LEG_CNT
                    , TXN_LEG_DATA
                    , GL_POSTED_FLG
                    , DISPUTE_FLG
                    , POSTED_BY
                    , POSTED_TS)
               VALUES (:DCL-TXN-ID
                    , CURRENT DATE
                    , :DCL-ACCT-ID
                    , :DCL-CARD-NUM
                    , 'PYRV'
                    , 'ON'
                    , :DCL-TXN-AMT
                    , :DCL-CURRENCY-CD
                    , :DCL-BILLING-AMT
                    , 1
                    , :DCL-TXN-DESC
                    , :DCL-LEG-CNT
                    , :DCL-LEG-DATA
                    , 'N'
                    , 'N'
                    , :CA-OPER-ID
                    , CURRENT TIMESTAMP)
           END-EXEC
      *
           IF SQLCODE NOT = 0
               MOVE 'INSERT  '         TO ER-SQL-OPERATION
               MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           .
      *
       5500-FLAG-ORIGINAL.
           EXEC SQL
               UPDATE CARDSVC.TRANSACTION
                  SET TXN_DESC = 'REVERSED SAME DAY BY CACRD14'
                WHERE TXN_ID    = :DCL-ORIG-TXN-ID
                  AND POST_DATE = CURRENT DATE
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = +100
               MOVE 'UPDATE  '         TO ER-SQL-OPERATION
               MOVE 'TRANSACTION       '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           .
      *
      *    THE REVERSAL PUTS THE BALANCE BACK BUT NOT THE ARREARS
      *    BUCKET - COLLECTIONS RE-DERIVE THAT OVERNIGHT AND HAVE
      *    ASKED US NOT TO GUESS AT IT.
       5600-UNDO-ACCOUNT.
           COMPUTE DCL-ORIG-AMT = 0 - DCL-ORIG-AMT
      *
           EXEC SQL
               UPDATE CARDSVC.ACCOUNT
                  SET CURR_BAL       = CURR_BAL + :DCL-ORIG-AMT
                    , LAST_PAY_AMT   = 0
                    , LAST_MAINT_PGM = 'CACRD14 '
                    , LAST_MAINT_TS  = CURRENT TIMESTAMP
                WHERE ACCT_ID        = :DCL-ACCT-ID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'            TO WS-ERROR-SW
                   MOVE 'ACCOUNT ROW NOT FOUND ON REVERSAL'
                                       TO M14MSGO
               WHEN OTHER
                   MOVE 'UPDATE  '     TO ER-SQL-OPERATION
                   MOVE 'ACCOUNT           '
                                       TO ER-SQL-TABLE
                   PERFORM 9100-SQL-ERROR
                   MOVE 'Y'            TO WS-ERROR-SW
           END-EVALUATE
           .
      *
       5700-UNDO-LIMIT.
           MOVE CA-CARD-NUM            TO DCL-CARD-NUM
      *
           EXEC SQL
               UPDATE CARDSVC.CARD_LIMIT
                  SET USED_AMT      = USED_AMT + :DCL-ORIG-AMT
                    , AVAIL_AMT     = CASE
                          WHEN AVAIL_AMT - :DCL-ORIG-AMT < 0 THEN 0
                          ELSE AVAIL_AMT - :DCL-ORIG-AMT
                      END
                    , LAST_MAINT_TS = CURRENT TIMESTAMP
                WHERE CARD_NUM   = :DCL-CARD-NUM
                  AND LIMIT_TYPE = 'CRED'
                  AND EXP_DATE   = '9999-12-31'
           END-EXEC
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = +100
               MOVE 'UPDATE  '         TO ER-SQL-OPERATION
               MOVE 'CARD_LIMIT        '
                                       TO ER-SQL-TABLE
               PERFORM 9100-SQL-ERROR
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           .
      *
      ******************************************************************
      * 7000 - ZONED DECIMAL WITH OVERPUNCHED SIGN                     *
      *                                                                *
      * THE FIELD IS RIGHT JUSTIFIED BY THE OPERATOR.  EVERYTHING BUT  *
      * THE LAST NON BLANK BYTE MUST BE A DIGIT.  THE LAST BYTE IS     *
      * EITHER A DIGIT OR AN OVERPUNCH CHARACTER CARRYING BOTH THE     *
      * UNITS DIGIT AND THE SIGN.                                      *
      ******************************************************************
       7000-DECODE-ZONED.
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE '+'                    TO WS-SIGN-SW
           MOVE ZEROES                 TO WS-ZW-DIGITS
           MOVE ZERO                   TO WS-ZW-LEN
      *
      *    STRIP THE BLANKS AND ANY DECIMAL POINT THE OPERATOR KEYED
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 13
               IF WS-ZI-CHAR(WS-IDX) NOT = SPACE
                  AND WS-ZI-CHAR(WS-IDX) NOT = '.'
                  AND WS-ZI-CHAR(WS-IDX) NOT = ','
                   ADD 1               TO WS-ZW-LEN
                   MOVE WS-ZI-CHAR(WS-IDX)
                                       TO WS-ZW-DIGITS(WS-ZW-LEN:1)
               END-IF
           END-PERFORM
      *
           IF WS-ZW-LEN = ZERO
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 7000-EXIT
           END-IF
      *
           MOVE WS-ZW-DIGITS(WS-ZW-LEN:1)
                                       TO WS-ZW-LAST-BYTE
           PERFORM 7100-DECODE-LAST-BYTE
           IF WS-ERROR-FOUND
               GO TO 7000-EXIT
           END-IF
           MOVE WS-ZW-LAST-DIGIT       TO WS-ZW-DIGITS(WS-ZW-LEN:1)
      *
           IF WS-ZW-DIGITS(1:WS-ZW-LEN) IS NOT NUMERIC
               MOVE 'Y'                TO WS-ERROR-SW
               GO TO 7000-EXIT
           END-IF
      *
      *    RIGHT JUSTIFY INTO THE 9(11)V99 FIELD - THE OPERATOR KEYS
      *    CENTS, NOT DOLLARS
           MOVE ZERO                   TO WS-ZW-NUMERIC
           MOVE SPACES                 TO WS-ZI-CHARS
           MOVE WS-ZW-DIGITS(1:WS-ZW-LEN)
                                       TO WS-ZI-CHARS(14 - WS-ZW-LEN:)
           INSPECT WS-ZI-CHARS REPLACING ALL ' ' BY '0'
           MOVE WS-ZI-CHARS            TO WS-ZW-NUM-R
           .
       7000-EXIT.
           EXIT
           .
      *
       7100-DECODE-LAST-BYTE.
           IF WS-ZW-LAST-BYTE IS NUMERIC
               MOVE WS-ZW-LAST-BYTE    TO WS-ZW-LAST-DIGIT
               GO TO 7100-EXIT
           END-IF
      *
           PERFORM VARYING WS-IDX FROM 1 BY 1
                     UNTIL WS-IDX > 20
               IF WS-OP-CHAR(WS-IDX) = WS-ZW-LAST-BYTE
                   MOVE WS-OP-DIGIT(WS-IDX)
                                       TO WS-ZW-LAST-DIGIT
                   MOVE WS-OP-SIGN(WS-IDX)
                                       TO WS-SIGN-SW
                   MOVE 21             TO WS-IDX
               END-IF
           END-PERFORM
      *
           IF WS-IDX = 21
               CONTINUE
           ELSE
               MOVE 'Y'                TO WS-ERROR-SW
           END-IF
           .
       7100-EXIT.
           EXIT
           .
      *
      ******************************************************************
      * 8000 - COMMON ROUTINES                                         *
      ******************************************************************
       8100-GET-DATE.
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-YYYYMMDD)
                     RESP(WS-RESP)
           END-EXEC
      *
           MOVE WS-TODAY-YYYYMMDD      TO WS-DISPLAY-DATE(1:8)
           MOVE WS-TODAY-YYYYMMDD(1:4) TO WS-ISO-YYYY
           MOVE '-'                    TO WS-ISO-SEP1
           MOVE WS-TODAY-YYYYMMDD(5:2) TO WS-ISO-MM
           MOVE '-'                    TO WS-ISO-SEP2
           MOVE WS-TODAY-YYYYMMDD(7:2) TO WS-ISO-DD
           .
      *
       8200-SEND-MAP.
           MOVE WS-DISPLAY-DATE        TO M14DATEO
      *
           EXEC CICS SEND
                     MAP(WS-MAP-PAY)
                     MAPSET(WS-MAPSET)
                     FROM(CRD14AO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
           END-EXEC
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CICS'             TO ER-ERROR-TYPE
               MOVE WS-RESP            TO ER-EIBRESP
               MOVE 'SEND MAP CRD14A FAILED'
                                       TO ER-MESSAGE
               PERFORM 9000-REPORT-ERROR
           END-IF
      *
           EXEC CICS RETURN
                     TRANSID(WS-TRAN-CARD)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
      ******************************************************************
      * 9000 - ERROR HANDLING                                          *
      ******************************************************************
       9000-REPORT-ERROR.
           MOVE WS-PGM-ID              TO ER-PGM-NAME
           MOVE 'E'                    TO ER-SEVERITY
           MOVE EIBTRNID               TO ER-TRAN-ID
           MOVE EIBTRMID               TO ER-TERM-ID
      *
           EXEC CICS LINK
                     PROGRAM(WS-ERROR-PGM-ONLINE)
                     COMMAREA(ERROR-AREA)
                     LENGTH(LENGTH OF ERROR-AREA)
                     RESP(WS-RESP)
           END-EXEC
           .
      *
       9100-SQL-ERROR.
           MOVE 'SQL '                 TO ER-ERROR-TYPE
           MOVE SQLCODE                TO ER-SQLCODE
           MOVE SQLSTATE               TO ER-SQLSTATE
           MOVE 'F'                    TO ER-SEVERITY
           MOVE 'SQL FAILURE IN PAYMENT POSTING'
                                       TO ER-MESSAGE
           PERFORM 9000-REPORT-ERROR
           .
      *
       9600-EXIT-SESSION.
           EXEC CICS SEND TEXT
                     FROM(WS-MSG-POSTED)
                     LENGTH(78)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC
      *
           EXEC CICS RETURN RESP(WS-RESP) END-EXEC
           .
