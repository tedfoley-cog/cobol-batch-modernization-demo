//CBPRT02J JOB (PRTY,RISK,0453),'SANCTIONS INGEST',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             REGION=0M,TIME=(45,00),NOTIFY=&SYSUID
//*
//********************************************************************
//* CBPRT02J - PARTYWK WEEKLY CYCLE, STEP 2 OF 4                     *
//*                                                                  *
//* TWO PIECES OF WORK IN ONE JOB BECAUSE THEY MUST NOT BE SPLIT     *
//* ACROSS THE ONLINE WINDOW.                                        *
//*                                                                  *
//*   CBPRT02A  LOADS THE VENDOR SANCTIONS FEED INTO                 *
//*             PARTYRSK.PARTY_SANCTION.  THE FEED IS FIXED LENGTH   *
//*             EBCDIC, 200 BYTES, HEADER / DETAIL / TRAILER.        *
//*   CBPRT02B  UNLOADS THE ACTIVE ROWS INTO A FLAT FILE WHICH IS    *
//*             SORTED AND REPRO'D INTO VSAM WATCHLST.  THE          *
//*             ALTERNATE INDEX IS THEN REBUILT WITH BLDINDEX.       *
//*                                                                  *
//* THE ONLINE SCREENING TRANSACTIONS READ WATCHLST THROUGH THE      *
//* PATH, SO THE FILE MUST BE CLOSED IN CICS BEFORE THIS JOB RUNS    *
//* AND OPENED AGAIN AFTERWARDS.  THE SCHEDULER ISSUES THE MODIFY    *
//* COMMANDS EITHER SIDE - SEE THE OPERATOR RUNBOOK.                 *
//*                                                                  *
//* SCHEDULED    SUNDAY 0330, AFTER CBPRT01J.                        *
//* CONTACT      PARTY AND RISK ON CALL, PAGER 4471.                 *
//*                                                                  *
//* RETURN CODES                                                     *
//*   00  FEED LOADED AND WATCHLST REBUILT                           *
//*   04  LOADED WITH REJECTS - CHECK PRTY.PROD.SANCREJ              *
//*   08  FEED REFUSED ON CONTROL TOTALS OR DELIST VOLUME.  NOTHING  *
//*       WAS COMMITTED AND WATCHLST WAS LEFT ALONE.  THE VENDOR     *
//*       MUST RESEND BEFORE THE JOB IS RERUN.                       *
//*   12  FAILED.  WATCHLST IS ONLY REBUILT WHEN THE LOAD ENDS       *
//*       BELOW 08, SO A FAILURE HERE NEVER LEAVES A HALF BUILT      *
//*       SCREENING FILE BEHIND.                                     *
//********************************************************************
//*
//JOBLIB   DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//*
//********************************************************************
//* STEP 010 - ALLOCATE THIS WEEK'S REJECT DATASET.                  *
//********************************************************************
//ALLOCREJ EXEC PGM=IEFBR14
//SANCREJ  DD DSN=PRTY.PROD.SANCREJ(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(30,15),RLSE),
//            DCB=(MODEL.DSCB,RECFM=FB,LRECL=200,BLKSIZE=27800)
//*
//********************************************************************
//* STEP 020 - LOAD THE VENDOR FEED.                                 *
//*            THE FEED GENERATION IS CREATED BY THE TRANSMISSION    *
//*            JOB PRTYFTP1 EARLIER IN THE NIGHT.                    *
//********************************************************************
//SANCLOAD EXEC PGM=IKJEFT01,DYNAMNBR=25,COND=(4,LT)
//STEPLIB  DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSOUT   DD SYSOUT=*
//SANCFEED DD DSN=PRTY.PROD.SANCFEED(0),DISP=SHR
//SANCREJ  DD DSN=PRTY.PROD.SANCREJ(+1),DISP=(MOD,CATLG,CATLG)
//SYSTSIN  DD *
  DSN SYSTEM(DB2P) RETRY(5)
  RUN PROGRAM(CBPRT02A) PLAN(PARTYWKP) -
      LIB('PRTY.PROD.LOADLIB')
  END
/*
//*
//********************************************************************
//* STEP 030 - UNLOAD THE ACTIVE ENTRIES FOR THE SCREENING FILE.     *
//*            SKIPPED IF THE LOAD REFUSED THE FEED.                 *
//********************************************************************
//         IF (SANCLOAD.RC <= 4) THEN
//WATCHEXT EXEC PGM=IKJEFT01,DYNAMNBR=25
//STEPLIB  DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSOUT   DD SYSOUT=*
//WATCHOUT DD DSN=&&WLEXTR,
//            DISP=(NEW,PASS,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(150,50),RLSE),
//            DCB=(RECFM=FB,LRECL=160,BLKSIZE=27840)
//SYSTSIN  DD *
  DSN SYSTEM(DB2P) RETRY(5)
  RUN PROGRAM(CBPRT02B) PLAN(PARTYWKP) -
      LIB('PRTY.PROD.LOADLIB')
  END
/*
//*
//********************************************************************
//* STEP 040 - SORT THE EXTRACT INTO CLUSTER KEY SEQUENCE.           *
//*            REPRO INTO A KSDS DEMANDS ASCENDING KEYS, AND A       *
//*            SORTED INPUT LOADS FAR FASTER THAN A RANDOM ONE.      *
//*            KEY IS LIST CODE 1-8 PLUS ENTRY ID 9-24.              *
//********************************************************************
//SORTWL   EXEC PGM=SORT,COND=(0,NE,WATCHEXT)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=&&WLEXTR,DISP=(OLD,DELETE,DELETE)
//SORTOUT  DD DSN=&&WLSORT,
//            DISP=(NEW,PASS,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(150,50),RLSE),
//            DCB=(RECFM=FB,LRECL=160,BLKSIZE=27840)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(60,20))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(60,20))
//SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(60,20))
//SORTWK04 DD UNIT=SYSDA,SPACE=(CYL,(60,20))
//SYSIN    DD *
  SORT FIELDS=(1,24,CH,A)
  SUM FIELDS=NONE
  OPTION EQUALS,DYNALLOC=(SYSDA,6)
/*
//*
//********************************************************************
//* STEP 050 - BACK THE CURRENT SCREENING FILE UP BEFORE IT IS       *
//*            OVERWRITTEN.  THE BACKUP HAS BEEN NEEDED TWICE SINCE  *
//*            1998 AND BOTH TIMES IT SAVED THE MONDAY MORNING.      *
//********************************************************************
//BKUPWL   EXEC PGM=IDCAMS,COND=(0,NE,SORTWL)
//SYSPRINT DD SYSOUT=*
//WLBKUP   DD DSN=PRTY.PROD.WATCHLST.BKUP(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(150,50),RLSE),
//            DCB=(MODEL.DSCB,RECFM=VB,LRECL=164,BLKSIZE=27998)
//SYSIN    DD *
 REPRO INDATASET(PRTY.PROD.WATCHLST) -
       OUTFILE(WLBKUP)
 IF LASTCC = 4 THEN SET MAXCC = 0
/*
//*
//********************************************************************
//* STEP 060 - REBUILD THE CLUSTER AND THE ALTERNATE INDEX.          *
//*                                                                  *
//* THE UPGRADE SET HAS TO COME OFF BEFORE THE REPRO, OTHERWISE      *
//* EVERY RECORD LOADED WOULD ALSO MAINTAIN THE AIX AND THE STEP     *
//* WOULD RUN FOR HOURS.  THE AIX IS DEFINED AND BUILT AFTERWARDS.   *
//********************************************************************
//BLDWL    EXEC PGM=IDCAMS,COND=(0,NE,SORTWL)
//SYSPRINT DD SYSOUT=*
//WLIN     DD DSN=&&WLSORT,DISP=(OLD,DELETE,DELETE)
//IDCUT1   DD DSN=&&AIXWK1,UNIT=SYSDA,
//            DISP=(NEW,DELETE,DELETE),
//            SPACE=(CYL,(80,20)),
//            DCB=(RECFM=VB,LRECL=4096,BLKSIZE=27998)
//IDCUT2   DD DSN=&&AIXWK2,UNIT=SYSDA,
//            DISP=(NEW,DELETE,DELETE),
//            SPACE=(CYL,(80,20)),
//            DCB=(RECFM=VB,LRECL=4096,BLKSIZE=27998)
//SYSIN    DD *
 DELETE PRTY.PROD.WATCHLST.PATH1 PATH
 IF LASTCC <= 8 THEN SET MAXCC = 0
 DELETE PRTY.PROD.WATCHLST.AIX1 ALTERNATEINDEX
 IF LASTCC <= 8 THEN SET MAXCC = 0
 REPRO INFILE(WLIN) -
       OUTDATASET(PRTY.PROD.WATCHLST) -
       REUSE
 IF LASTCC > 0 THEN CANCEL
 DEFINE AIX                              -
        (NAME(PRTY.PROD.WATCHLST.AIX1)   -
         RELATE(PRTY.PROD.WATCHLST)      -
         KEYS(60 24)                     -
         RECORDSIZE(96 4096)             -
         NONUNIQUEKEY                    -
         UPGRADE                         -
         CYLINDERS(40 10)                -
         VOLUMES(PRTY02))
 BLDINDEX INDATASET(PRTY.PROD.WATCHLST) -
          OUTDATASET(PRTY.PROD.WATCHLST.AIX1) -
          WORKFILES(IDCUT1 IDCUT2)
 IF LASTCC > 0 THEN CANCEL
 DEFINE PATH                             -
        (NAME(PRTY.PROD.WATCHLST.PATH1)  -
         PATHENTRY(PRTY.PROD.WATCHLST.AIX1))
 LISTCAT ENTRIES(PRTY.PROD.WATCHLST) ALL
/*
//*
//         ENDIF
//*
//********************************************************************
//* STEP 070 - PRINT THE REJECTS FOR THE SANCTIONS ANALYST.          *
//*            RUNS WHATEVER THE LOAD ENDED WITH - A REFUSED FEED    *
//*            IS EXACTLY WHEN THE ANALYST WANTS TO SEE THEM.        *
//********************************************************************
//PRTREJ   EXEC PGM=IEBGENER,COND=EVEN
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=PRTY.PROD.SANCREJ(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(A,,SANC),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSIN    DD *
  GENERATE MAXFLDS=2,MAXLITS=8
  RECORD FIELD=(1,1,,2),FIELD=(132,1,,2)
/*
//
