CARDCHK  TITLE 'CARDCHK - CARD NUMBER AND PACKED FIELD VALIDATION'
***********************************************************************
*                                                                     *
* MODULE   CARDCHK                                                    *
*                                                                     *
* FUNCTION LOW LEVEL VALIDATION AND CONVERSION SERVICES CALLED        *
*          STATICALLY FROM COBOL.  FOUR FUNCTIONS, SELECTED BY THE    *
*          ONE BYTE FUNCTION CODE IN THE FIRST PARAMETER.             *
*                                                                     *
*            L   MOD-10 (LUHN) CHECK DIGIT VALIDATION OVER A          *
*                SIXTEEN BYTE ZONED CARD NUMBER                       *
*            P   PACKED DECIMAL ADD WITH OVERFLOW DETECTION           *
*            B   ZONED TO BINARY AND BINARY TO ZONED CONVERSION       *
*            C   EBCDIC COLLATING SEQUENCE COMPARISON                 *
*                                                                     *
* LINKAGE  STANDARD OS LINKAGE.  R1 ADDRESSES A LIST OF SIX FULLWORD  *
*          ADDRESSES.  THE LAST OF THEM IS A CALLER OWNED WORK AREA   *
*          OF AT LEAST 256 BYTES ON A DOUBLEWORD BOUNDARY - THIS      *
*          MODULE IS REENTRANT AND HOLDS NO WRITABLE STORAGE OF ITS   *
*          OWN, SO THE SAVE AREA AND EVERY WORK FIELD COME FROM       *
*          THERE.  THE CALLER MUST NOT SHARE ONE WORK AREA BETWEEN    *
*          TWO CONCURRENT CALLS.                                      *
*                                                                     *
* RETURN   R15   0  SUCCESSFUL                                        *
*                4  WARNING - SEE THE FEEDBACK FIELD                  *
*                8  VALIDATION FAILED                                 *
*               12  UNUSABLE REQUEST - BAD FUNCTION CODE OR OVERFLOW  *
*                                                                     *
* NOTES    NOT AN LE ENABLED ROUTINE.  IT DOES NOT ESTABLISH AN       *
*          ENCLAVE, DOES NOT ISSUE ANY SVC AND DOES NOT ISSUE ANY     *
*          EXEC CICS COMMAND, SO IT IS SAFE TO CALL FROM BOTH THE     *
*          ONLINE REGION AND FROM BATCH.                              *
*                                                                     *
*          NO CONDITION IS ALLOWED TO REACH THE CALLER AS A PROGRAM   *
*          CHECK.  THE OPERANDS ARE TESTED BEFORE EVERY DECIMAL       *
*          INSTRUCTION AND EVERY RESULT IS RANGE CHECKED BEFORE IT IS *
*          STORED, SO AN 0C7 OR AN 0CA HERE MEANS THE PARAMETER LIST  *
*          ITSELF IS WRONG.                                           *
*                                                                     *
* CHANGE HISTORY                                                      *
*   1996-04-11  ORIGINAL - L FUNCTION ONLY                            *
*   1998-09-30  P FUNCTION ADDED FOR THE POSTING ACCUMULATORS         *
*   2001-02-19  MADE REENTRANT, WORK AREA MOVED TO THE PARAMETER      *
*               LIST.  CALLERS BUILT BEFORE THIS DATE PASS FIVE       *
*               PARAMETERS AND WILL 0C4 - REBUILD THEM.               *
*   2003-11-05  B FUNCTION ADDED                                      *
*   2007-06-22  C FUNCTION ADDED FOR NAME SCREENING                   *
*   2014-01-30  AMODE 31, RMODE ANY                                   *
*                                                                     *
***********************************************************************
         EJECT
***********************************************************************
*        REGISTER EQUATES                                             *
***********************************************************************
R0       EQU   0
R1       EQU   1                  PARAMETER LIST ON ENTRY
R2       EQU   2                  WORK
R3       EQU   3                  A(INPUT 1)
R4       EQU   4                  A(INPUT 2)
R5       EQU   5                  A(OUTPUT)
R6       EQU   6                  A(FEEDBACK)
R7       EQU   7                  WORK - INDEX
R8       EQU   8                  WORK - ACCUMULATOR
R9       EQU   9                  WORK AREA BASE
R10      EQU   10                 PARAMETER LIST BASE
R11      EQU   11                 PROGRAM BASE
R12      EQU   12                 WORK
R13      EQU   13                 SAVE AREA
R14      EQU   14                 RETURN ADDRESS
R15      EQU   15                 ENTRY POINT AND RETURN CODE
         SPACE 2
***********************************************************************
*        ENTRY AND PROLOGUE                                           *
***********************************************************************
CARDCHK  CSECT
CARDCHK  AMODE 31
CARDCHK  RMODE ANY
         SAVE  (14,12),,CARDCHK   SAVE THE CALLER'S REGISTERS
         LR    R11,R15            ESTABLISH ADDRESSABILITY
         USING CARDCHK,R11
         LR    R10,R1             HOLD THE PARAMETER LIST
         USING PARMLIST,R10
*
*        THE WORK AREA COMES FROM THE CALLER.  ITS FIRST 72 BYTES
*        BECOME THIS MODULE'S SAVE AREA AND ARE CHAINED BOTH WAYS SO
*        THAT A DUMP WALKS CLEANLY BACK TO THE CALLER.
*
         L     R9,PWORK           A(CALLER SUPPLIED WORK AREA)
         USING WORKAREA,R9
         ST    R13,WSAVE+4        BACKWARD CHAIN
         ST    R9,8(,R13)         FORWARD CHAIN
         LR    R13,R9             OUR SAVE AREA
         SPACE 1
         L     R2,PFUNC           A(FUNCTION CODE)
         L     R3,PIN1            A(INPUT 1)
         L     R4,PIN2            A(INPUT 2)
         L     R5,POUT            A(OUTPUT)
         L     R6,PFEED           A(FEEDBACK)
         SPACE 1
         MVC   0(4,R6),=CL4' '    CLEAR THE FEEDBACK FIELD
         SR    R15,R15            ASSUME SUCCESS
         SPACE 1
         CLI   0(R2),C'L'         LUHN CHECK DIGIT
         BE    FUNCL
         CLI   0(R2),C'P'         PACKED ADD
         BE    FUNCP
         CLI   0(R2),C'B'         ZONED AND BINARY CONVERSION
         BE    FUNCB
         CLI   0(R2),C'C'         COLLATING COMPARISON
         BE    FUNCC
         SPACE 1
*        UNRECOGNISED FUNCTION CODE.  THIS IS A PROGRAMMING ERROR IN
*        THE CALLER, NOT A DATA CONDITION - RETURN 12.
         MVC   0(4,R6),=CL4'FUNC'
         LA    R15,12
         B     EXIT
         EJECT
***********************************************************************
*        FUNCTION L - MOD-10 CHECK DIGIT VALIDATION                   *
*                                                                     *
*        INPUT 1   CL16  CARD NUMBER, ZONED, LEFT JUSTIFIED           *
*        OUTPUT    CL1   THE CHECK DIGIT THE FIRST FIFTEEN DIGITS     *
*                        REQUIRE, WHETHER OR NOT THE NUMBER PASSED    *
*        FEEDBACK  CL4   NUMR - A BYTE WAS NOT F0 THROUGH F9          *
*                        CHKD - CHECK DIGIT DOES NOT AGREE            *
*                                                                     *
*        EVERY SECOND DIGIT COUNTING FROM THE RIGHT IS DOUBLED.  ON A *
*        SIXTEEN DIGIT NUMBER THAT IS THE DIGITS AT EVEN OFFSETS.  A  *
*        DOUBLED VALUE ABOVE NINE HAS NINE SUBTRACTED, WHICH IS THE   *
*        SAME AS SUMMING ITS TWO DIGITS.  THE NUMBER IS VALID WHEN    *
*        THE TOTAL IS A MULTIPLE OF TEN.                              *
***********************************************************************
FUNCL    DS    0H
         SR    R8,R8              RUNNING TOTAL
         SR    R7,R7              OFFSET INTO THE CARD NUMBER
         SPACE 1
LUHNLOOP DS    0H
         SR    R2,R2
         IC    R2,0(R7,R3)        NEXT BYTE OF THE CARD NUMBER
         LR    R12,R2
         N     R12,=X'000000F0'   ISOLATE THE ZONE
         C     R12,=F'240'        MUST BE X'F0'
         BNE   LUHNNUMR
         N     R2,=X'0000000F'    ISOLATE THE DIGIT
         C     R2,=F'9'
         BH    LUHNNUMR           A THROUGH F IS NOT A DIGIT
         SPACE 1
         LR    R12,R7
         N     R12,=F'1'          ODD OFFSET
         LTR   R12,R12
         BNZ   LUHNADD            ODD - TAKE THE DIGIT AS IT STANDS
         SLL   R2,1               EVEN - DOUBLE IT
         C     R2,=F'9'
         BNH   LUHNADD
         S     R2,=F'9'           AND FOLD IT BACK TO ONE DIGIT
         SPACE 1
LUHNADD  DS    0H
         AR    R8,R2
         LA    R7,1(,R7)
         C     R7,=F'16'
         BL    LUHNLOOP
         EJECT
***********************************************************************
*        THE TOTAL IS COMPLETE.  DIVIDE BY TEN AND LOOK AT THE        *
*        REMAINDER.  THE DIGIT THE NUMBER OUGHT TO CARRY IS TEN LESS  *
*        THE REMAINDER OF THE TOTAL TAKEN WITHOUT THE LAST DIGIT,     *
*        WHICH IS WHAT THE SECOND DIVIDE BELOW PRODUCES.              *
***********************************************************************
         SR    R0,R0
         LR    R1,R8
         D     R0,=F'10'          R0 REMAINDER, R1 QUOTIENT
         LR    R12,R0             HOLD THE REMAINDER
         SPACE 1
*        REBUILD THE EXPECTED CHECK DIGIT FOR THE CALLER.  THE LAST
*        DIGIT WAS ADDED UNDOUBLED, SO REMOVING IT FROM THE TOTAL AND
*        TAKING THE COMPLEMENT OF THE REMAINDER GIVES IT DIRECTLY.
         SR    R2,R2
         IC    R2,15(,R3)         THE DIGIT AS PRESENTED
         N     R2,=X'0000000F'
         SR    R8,R2              TOTAL WITHOUT IT
         SR    R0,R0
         LR    R1,R8
         D     R0,=F'10'
         LA    R2,10
         SR    R2,R0              TEN LESS THE REMAINDER
         N     R2,=X'0000000F'    TEN BECOMES ZERO
         O     R2,=X'000000F0'    MAKE IT A ZONED DIGIT
         STC   R2,0(,R5)          AND HAND IT BACK
         SPACE 1
         LTR   R12,R12            WAS THE TOTAL A MULTIPLE OF TEN
         BZ    EXIT               YES - RETURN ZERO
         MVC   0(4,R6),=CL4'CHKD'
         LA    R15,8
         B     EXIT
         SPACE 1
LUHNNUMR DS    0H
         MVC   0(4,R6),=CL4'NUMR'
         MVI   0(R5),C'?'
         LA    R15,8
         B     EXIT
         EJECT
***********************************************************************
*        FUNCTION P - PACKED DECIMAL ADD                              *
*                                                                     *
*        INPUT 1   PL8   ADDEND         S9(13)V99 COMP-3              *
*        INPUT 2   PL8   ACCUMULATOR    S9(13)V99 COMP-3              *
*        OUTPUT    PL8   SUM            S9(13)V99 COMP-3              *
*        FEEDBACK  CL4   DATA - ONE OF THE OPERANDS IS NOT VALID      *
*                        PACKED DECIMAL                               *
*                        OVFL - THE SUM WILL NOT FIT IN FIFTEEN       *
*                        DIGITS                                       *
*                                                                     *
*        THE OPERANDS ARE TESTED BEFORE THE ADD.  THE POSTING FILES   *
*        CARRY THE OCCASIONAL FIELD THAT WAS NEVER INITIALISED, AND   *
*        AN 0C7 IN THE MIDDLE OF A COMMIT INTERVAL COSTS FAR MORE     *
*        THAN THE TWO INSTRUCTIONS IT TAKES TO AVOID IT.              *
*                                                                     *
*        THE ADD IS DONE IN A SIXTEEN BYTE FIELD SO THAT AN OVERFLOW  *
*        OUT OF FIFTEEN DIGITS IS VISIBLE RATHER THAN LOST.           *
***********************************************************************
FUNCP    DS    0H
         TP    0(8,R3)            IS INPUT 1 VALID PACKED
         BNZ   PACKDATA
         TP    0(8,R4)            IS INPUT 2 VALID PACKED
         BNZ   PACKDATA
         SPACE 1
         ZAP   WPACK16,0(8,R4)    ACCUMULATOR INTO THE WIDE FIELD
         AP    WPACK16,0(8,R3)    ADD THE ADDEND
         SPACE 1
*        ANYTHING IN THE TOP BYTE MEANS THE RESULT NEEDS MORE THAN
*        THE FIFTEEN DIGITS THE CALLER'S FIELD HOLDS.
         CP    WPACK16,=P'999999999999999'
         BH    PACKOVFL
         CP    WPACK16,=P'-999999999999999'
         BL    PACKOVFL
         SPACE 1
         ZAP   0(8,R5),WPACK16    HAND BACK THE SUM
         SPACE 1
*        A ZERO OR NEGATIVE RESULT IS LEGITIMATE BUT THE POSTING
*        PROGRAMS WANT TO KNOW ABOUT IT WITHOUT RE-EXAMINING THE
*        FIELD, SO IT IS REPORTED IN THE FEEDBACK.
         CP    0(8,R5),=P'0'
         BH    EXIT
         BE    PACKZERO
         MVC   0(4,R6),=CL4'NEGA'
         LA    R15,4
         B     EXIT
         SPACE 1
PACKZERO DS    0H
         MVC   0(4,R6),=CL4'ZERO'
         LA    R15,4
         B     EXIT
         SPACE 1
PACKDATA DS    0H
         MVC   0(4,R6),=CL4'DATA'
         ZAP   0(8,R5),=P'0'
         LA    R15,8
         B     EXIT
         SPACE 1
PACKOVFL DS    0H
         MVC   0(4,R6),=CL4'OVFL'
         ZAP   0(8,R5),=P'0'
         LA    R15,12
         B     EXIT
         EJECT
***********************************************************************
*        FUNCTION B - ZONED AND BINARY CONVERSION                     *
*                                                                     *
*        INPUT 1   CL15  UNSIGNED ZONED NUMERIC                       *
*        INPUT 2   XL4   NOT USED ON INPUT                            *
*        OUTPUT    XL4   THE VALUE AS A FULLWORD BINARY               *
*                  +4    CL15 THE VALUE CONVERTED BACK TO ZONED       *
*        FEEDBACK  CL4   NUMR - NOT ALL FIFTEEN BYTES ARE DIGITS      *
*                        SIZE - THE VALUE WILL NOT FIT IN A FULLWORD  *
*                                                                     *
*        USED WHERE A DISPLAY FIELD HAS TO BECOME A BINARY KEY AND    *
*        THEN BE REDISPLAYED WITHOUT THE LEADING ZEROES CHANGING.     *
*        THE ROUND TRIP IS DONE HERE, NOT IN COBOL, BECAUSE THE       *
*        CALLERS NEED BOTH FORMS AND THE COMPILER WOULD GENERATE THE  *
*        CONVERSION TWICE.                                            *
***********************************************************************
FUNCB    DS    0H
         SR    R7,R7              OFFSET
         SPACE 1
BINSCAN  DS    0H
         SR    R2,R2
         IC    R2,0(R7,R3)
         N     R2,=X'000000F0'
         C     R2,=F'240'
         BNE   BINNUMR
         SR    R2,R2
         IC    R2,0(R7,R3)
         N     R2,=X'0000000F'
         C     R2,=F'9'
         BH    BINNUMR
         LA    R7,1(,R7)
         C     R7,=F'15'
         BL    BINSCAN
         SPACE 1
         PACK  WDBL,0(15,R3)      ZONED TO PACKED
         SPACE 1
*        CVB WILL 0CA IF THE VALUE EXCEEDS A FULLWORD, SO THE RANGE
*        IS CHECKED FIRST RATHER THAN TRAPPED AFTERWARDS.
         CP    WDBL,=P'2147483647'
         BH    BINSIZE
         SPACE 1
         CVB   R8,WDBL            PACKED TO BINARY
         ST    R8,0(,R5)          FULLWORD BINARY TO THE CALLER
         SPACE 1
         CVD   R8,WDBL            AND BACK AGAIN
         UNPK  WZONE,WDBL         PACKED TO ZONED
         OI    WZONE+14,X'F0'     STRIP THE SIGN FROM THE LAST BYTE
         MVC   4(15,R5),WZONE     ZONED FORM TO THE CALLER
         B     EXIT
         SPACE 1
BINNUMR  DS    0H
         MVC   0(4,R6),=CL4'NUMR'
         XC    0(4,R5),0(R5)
         MVC   4(15,R5),=CL15'000000000000000'
         LA    R15,8
         B     EXIT
         SPACE 1
BINSIZE  DS    0H
         MVC   0(4,R6),=CL4'SIZE'
         XC    0(4,R5),0(R5)
         MVC   4(15,R5),=CL15'999999999999999'
         LA    R15,12
         B     EXIT
         EJECT
***********************************************************************
*        FUNCTION C - EBCDIC COLLATING SEQUENCE COMPARISON            *
*                                                                     *
*        INPUT 1   CL40  LEFT OPERAND                                 *
*        INPUT 2   CL40  RIGHT OPERAND                                *
*        OUTPUT    CL1   L  INPUT 1 COLLATES LOW                      *
*                        E  THE TWO ARE IDENTICAL                     *
*                        H  INPUT 1 COLLATES HIGH                     *
*                  +1    CL2 OFFSET OF THE FIRST DIFFERING BYTE,      *
*                        ZONED, 01 THROUGH 40, OR 00 IF EQUAL         *
*        FEEDBACK  CL4   PREF - THE SHORTER OF THE TWO IS A PREFIX    *
*                        OF THE LONGER ONCE TRAILING BLANKS ARE       *
*                        IGNORED                                      *
*                                                                     *
*        THE COMPARISON IS ON THE RAW EBCDIC COLLATING SEQUENCE, SO   *
*        LOWER CASE SORTS BELOW UPPER CASE AND DIGITS SORT ABOVE      *
*        BOTH.  NAME SCREENING RELIES ON THAT ORDERING BECAUSE THE    *
*        WATCH LIST ALTERNATE INDEX IS BUILT THE SAME WAY - DO NOT    *
*        FOLD CASE HERE WITHOUT REBUILDING THE INDEX.                 *
*                                                                     *
*        THE OFFSET OF THE FIRST DIFFERENCE IS WHAT MAKES THIS WORTH  *
*        AN ASSEMBLER ROUTINE.  THE SCREENING PROGRAMS USE IT AS A    *
*        CHEAP MEASURE OF HOW CLOSE A NEAR MISS IS AND TO SET THE     *
*        GENERIC KEY FOR THE NEXT BROWSE.                             *
***********************************************************************
FUNCC    DS    0H
         SR    R7,R7              OFFSET
         SPACE 1
CMPLOOP  DS    0H
         SR    R2,R2
         IC    R2,0(R7,R3)
         SR    R12,R12
         IC    R12,0(R7,R4)
         CR    R2,R12
         BNE   CMPDIFF
         LA    R7,1(,R7)
         C     R7,=F'40'
         BL    CMPLOOP
         SPACE 1
*        NO DIFFERENCE IN FORTY BYTES.
         MVI   0(R5),C'E'
         MVC   1(2,R5),=CL2'00'
         B     EXIT
         EJECT
***********************************************************************
*        A DIFFERENCE WAS FOUND AT OFFSET R7.  RECORD THE ORDERING    *
*        AND CONVERT THE OFFSET TO A TWO BYTE ZONED NUMBER.           *
***********************************************************************
CMPDIFF  DS    0H
         CR    R2,R12
         BL    CMPLOW
         MVI   0(R5),C'H'
         B     CMPOFFS
         SPACE 1
CMPLOW   DS    0H
         MVI   0(R5),C'L'
         SPACE 1
CMPOFFS  DS    0H
         LA    R8,1(,R7)          REPORT ONE BASED
         CVD   R8,WDBL
         UNPK  WZONE(3),WDBL
         OI    WZONE+2,X'F0'
         MVC   1(2,R5),WZONE+1
         SPACE 1
*        IF EITHER SIDE HAS RUN INTO TRAILING BLANKS AT THE POINT OF
*        DIFFERENCE THEN ONE NAME IS A PREFIX OF THE OTHER.  THAT IS
*        THE MOST COMMON KIND OF NEAR MISS AND IS REPORTED SEPARATELY
*        WITH A WARNING RETURN CODE.
         C     R2,=F'64'          IS THE LEFT BYTE A BLANK
         BE    CMPPREF
         C     R12,=F'64'         IS THE RIGHT BYTE A BLANK
         BE    CMPPREF
         LA    R15,4              DIFFERENT NAMES
         B     EXIT
         SPACE 1
CMPPREF  DS    0H
         MVC   0(4,R6),=CL4'PREF'
         LA    R15,4
         B     EXIT
         EJECT
***********************************************************************
*        EPILOGUE                                                     *
*                                                                     *
*        R15 CARRIES THE RETURN CODE AND MUST SURVIVE THE RESTORE,    *
*        SO RC=(15) IS CODED ON THE RETURN MACRO.                     *
***********************************************************************
EXIT     DS    0H
         LR    R2,R15             HOLD THE RETURN CODE
         L     R13,WSAVE+4        BACK TO THE CALLER'S SAVE AREA
         LR    R15,R2
         RETURN (14,12),RC=(15)
         SPACE 2
***********************************************************************
*        LITERALS                                                     *
***********************************************************************
         LTORG
         EJECT
***********************************************************************
*        PARAMETER LIST                                               *
*                                                                     *
*        THE COBOL CALL PASSES SIX OPERANDS, SO R1 ADDRESSES SIX      *
*        FULLWORDS AND THE HIGH ORDER BIT OF THE LAST IS ON.  THAT    *
*        BIT IS NOT TESTED HERE - A CALLER THAT PASSES FEWER THAN SIX *
*        OPERANDS WILL FAIL ON THE LOAD OF PWORK, WHICH IS THE        *
*        INTENDED BEHAVIOUR.                                          *
***********************************************************************
PARMLIST DSECT
PFUNC    DS    A                  A(FUNCTION CODE      CL1 )
PIN1     DS    A                  A(INPUT 1                )
PIN2     DS    A                  A(INPUT 2                )
POUT     DS    A                  A(OUTPUT                 )
PFEED    DS    A                  A(FEEDBACK           CL4 )
PWORK    DS    A                  A(WORK AREA          CL256)
         SPACE 2
***********************************************************************
*        CALLER SUPPLIED WORK AREA                                    *
*                                                                     *
*        112 BYTES ARE USED.  THE CALLER IS ASKED FOR 256 SO THAT A   *
*        LATER FUNCTION CAN TAKE MORE WITHOUT EVERY CALLER HAVING TO  *
*        BE RECOMPILED.                                               *
***********************************************************************
WORKAREA DSECT
WSAVE    DS    18F                SAVE AREA
WDBL     DS    D                  CONVERSION DOUBLEWORD
WPACK16  DS    PL16               WIDE ACCUMULATOR FOR THE ADD
WZONE    DS    CL16               ZONED WORK FIELD
WORKLEN  EQU   *-WORKAREA
         SPACE 2
         END   CARDCHK
