//CBPRT01J JOB (PRTY,RISK,0453),'KYC REFRESH',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             REGION=0M,TIME=(20,00),NOTIFY=&SYSUID
//*
//********************************************************************
//* CBPRT01J - PARTYWK WEEKLY CYCLE, STEP 1 OF 4                     *
//*                                                                  *
//* FINDS THE PARTIES WHOSE KYC REVIEW HAS FALLEN DUE, RAISES A      *
//* PENDING REVIEW ROW ON PARTYRSK.PARTY_KYC AND PRINTS THE WORK     *
//* LIST FOR THE FINANCIAL CRIME TEAM.                               *
//*                                                                  *
//* SCHEDULED    SUNDAY 0200, PRECEDES CBPRT02J.                     *
//* CONTACT      PARTY AND RISK ON CALL, PAGER 4471.                 *
//*                                                                  *
//* RETURN CODES                                                     *
//*   00  NOTHING DUE OR ALL DUE REVIEWS RAISED                      *
//*   04  RAISED WITH WARNINGS - CHECK THE JOB LOG                   *
//*   08  DUPLICATE OR ORPHANED KYC ROWS SKIPPED - CALL PARTY AND    *
//*       RISK BEFORE RERUNNING                                      *
//*   12  FAILED, NOTHING COMMITTED PAST THE LAST CHECKPOINT.  THE   *
//*       JOB IS RERUNNABLE FROM THE TOP - ALREADY RAISED ROWS ARE   *
//*       DETECTED AND SKIPPED.                                      *
//********************************************************************
//*
//JOBLIB   DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//         DD DSN=SYS1.COB2.SIGYCOMP,DISP=SHR
//*
//********************************************************************
//* STEP 010 - DELETE THE WORK LIST FROM THE PREVIOUS WEEK.          *
//********************************************************************
//DELWORK  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE PRTY.PROD.KYCWORK.WEEKLY NONVSAM
 IF LASTCC = 8 THEN SET MAXCC = 0
/*
//*
//********************************************************************
//* STEP 020 - RAISE THE PENDING REVIEWS.                            *
//*            RUNS UNDER TSO SO THE PLAN CAN BE ALLOCATED.          *
//********************************************************************
//KYCDUE   EXEC PGM=IKJEFT01,DYNAMNBR=25,COND=(4,LT)
//STEPLIB  DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSOUT   DD SYSOUT=*
//KYCWORK  DD DSN=PRTY.PROD.KYCWORK.WEEKLY,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(25,10),RLSE),
//            DCB=(RECFM=FB,LRECL=133,BLKSIZE=27930)
//SYSTSIN  DD *
  DSN SYSTEM(DB2P) RETRY(5)
  RUN PROGRAM(CBPRT01) PLAN(PARTYWKP) -
      LIB('PRTY.PROD.LOADLIB')
  END
/*
//*
//********************************************************************
//* STEP 030 - SORT THE WORK LIST INTO REVIEW TEAM SEQUENCE.         *
//*            KYC LEVEL DESCENDING, THEN DAYS OVERDUE DESCENDING,   *
//*            SO THE ENHANCED DUE DILIGENCE CASES COME OUT FIRST.   *
//********************************************************************
//SORTWORK EXEC PGM=SORT,COND=(4,LT)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=PRTY.PROD.KYCWORK.WEEKLY,DISP=SHR
//SORTOUT  DD DSN=PRTY.PROD.KYCWORK.SORTED(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(25,10),RLSE),
//            DCB=(MODEL.DSCB,RECFM=FB,LRECL=133,BLKSIZE=27930)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//SYSIN    DD *
* KYC LEVEL AT 74, DAYS OVERDUE AT 95, PARTY ID AT 1.
  SORT FIELDS=(74,4,CH,D,95,5,CH,D,1,11,CH,A)
  OPTION EQUALS,DYNALLOC=(SYSDA,4)
/*
//*
//********************************************************************
//* STEP 040 - PRINT THE WORK LIST FOR THE REVIEW TEAM.              *
//*            ONLY WHEN STEP 020 FOUND SOMETHING TO DO.             *
//********************************************************************
//         IF (KYCDUE.RC = 0 OR KYCDUE.RC = 4) AND
//            SORTWORK.RUN THEN
//PRTWORK  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=PRTY.PROD.KYCWORK.SORTED(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(A,,KYC1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSIN    DD DUMMY
//         ENDIF
//*
//********************************************************************
//* STEP 050 - HOUSEKEEPING NOTE FOR THE OPERATOR ON A FAILURE.      *
//********************************************************************
//         IF (KYCDUE.RC > 4) THEN
//FAILMSG  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
 CBPRT01J STEP KYCDUE ENDED ABOVE RC 04.
 DO NOT RELEASE CBPRT02J.  CALL PARTY AND RISK ON CALL, PAGER 4471,
 AND QUOTE THE RETURN CODE AND THE LAST PARTY ID ON THE JOB LOG.
/*
//         ENDIF
//
