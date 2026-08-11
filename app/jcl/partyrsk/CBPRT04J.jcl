//CBPRT04J JOB (PRTY,RISK,0453),'RISK AUDIT PACK',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             REGION=64M,TIME=(30,00),NOTIFY=&SYSUID
//*
//********************************************************************
//* CBPRT04J - PARTYWK WEEKLY CYCLE, STEP 4 OF 4                     *
//*                                                                  *
//* PRINTS THE WEEKLY REGULATORY RISK AUDIT PACK - BAND MOVEMENT     *
//* BY MODULE, BANDED DISTRIBUTION, SANCTIONS HITS AND OVERDUE KYC.  *
//* THE PACK IS PRINTED ON THE COMPLIANCE PRINTER AND A COPY IS      *
//* KEPT ON THE ARCHIVE GENERATION FOR SEVEN YEARS.                  *
//*                                                                  *
//* MUST RUN AFTER CBPRT03J HAS COMPLETED, OTHERWISE THE BANDED      *
//* DISTRIBUTION REPORTS LAST WEEK'S SCORES.                         *
//*                                                                  *
//* SCHEDULED    SUNDAY 1100, AFTER CBPRT03J.                        *
//* CONTACT      PARTY AND RISK ON CALL, PAGER 4471.                 *
//*              COMPLIANCE REPORTING, EXTENSION 2290.               *
//*                                                                  *
//* RETURN CODES                                                     *
//*   00  PACK PRINTED                                               *
//*   04  PACK PRINTED BUT NO AUDIT ACTIVITY IN THE PERIOD - CHECK   *
//*       THAT CBPRT03J ACTUALLY RAN THIS WEEKEND                    *
//*   12  FAILED.  NOTHING IS PRINTED ON THE COMPLIANCE PRINTER      *
//*       BECAUSE A PART PRINTED PACK MUST NOT LEAVE THE FLOOR.      *
//********************************************************************
//*
//JOBLIB   DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//*
//********************************************************************
//* STEP 010 - BUILD THE PACK ONTO A TEMPORARY DATASET.  IT IS ONLY  *
//*            RELEASED TO THE PRINTER ONCE THE STEP HAS ENDED       *
//*            CLEANLY.                                              *
//********************************************************************
//RPTAUD   EXEC PGM=IKJEFT01,DYNAMNBR=25
//STEPLIB  DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSOUT   DD SYSOUT=*
//RISKRPT  DD DSN=&&AUDRPT,
//            DISP=(NEW,PASS,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=27930)
//SYSTSIN  DD *
  DSN SYSTEM(DB2P) RETRY(5)
  RUN PROGRAM(CBPRT04) PLAN(PARTYWKP) -
      LIB('PRTY.PROD.LOADLIB')
  END
/*
//*
//********************************************************************
//* STEP 020 - ARCHIVE GENERATION.  RETAINED SEVEN YEARS BY THE      *
//*            MANAGEMENT CLASS ON THE GDG BASE.                     *
//********************************************************************
//ARCHRPT  EXEC PGM=IEBGENER,COND=(4,LT,RPTAUD)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=&&AUDRPT,DISP=(OLD,PASS,DELETE)
//SYSUT2   DD DSN=PRTY.PROD.RISKAUD.PACK(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//            DCB=(MODEL.DSCB,RECFM=FBA,LRECL=133,BLKSIZE=27930)
//SYSIN    DD DUMMY
//*
//********************************************************************
//* STEP 030 - RELEASE THE PACK TO THE COMPLIANCE PRINTER.           *
//********************************************************************
//         IF (ARCHRPT.RUN AND ARCHRPT.RC = 0) THEN
//PRTAUD   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=PRTY.PROD.RISKAUD.PACK(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(A,,CMPL),COPIES=2,
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSIN    DD DUMMY
//         ENDIF
//*
//********************************************************************
//* STEP 040 - CLOSE THE PARTYWK CYCLE CONTROL RECORD OFF.  THE      *
//*            SCHEDULER READS THIS BEFORE IT RELEASES NEXT WEEK.    *
//********************************************************************
//         IF (RPTAUD.RC <= 4) THEN
//CYCLEND  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 PRINT INDATASET(PRTY.PROD.CYCLCTL) CHARACTER COUNT(20)
/*
//         ENDIF
//*
//********************************************************************
//* STEP 050 - TELL THE OPERATOR WHAT TO DO WHEN THE PACK FAILED.    *
//********************************************************************
//         IF (RPTAUD.RC > 4) THEN
//FAILMSG  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
 CBPRT04J STEP RPTAUD FAILED.  NO PACK WAS PRINTED OR ARCHIVED.
 THE JOB IS FULLY RERUNNABLE - IT ONLY READS.  RERUN ONCE.
 IF IT FAILS TWICE CALL PARTY AND RISK ON CALL, PAGER 4471, AND
 TELL COMPLIANCE REPORTING ON EXTENSION 2290 THAT THE MONDAY PACK
 WILL BE LATE.
/*
//         ENDIF
//
