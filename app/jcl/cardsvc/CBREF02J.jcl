//CBREF02J JOB (CARD,REFR),'REBUILD MERCHRTE',CLASS=C,MSGCLASS=X,
//             REGION=0M,TIME=(30,00),NOTIFY=&SYSUID
//*
//* ------------------------------------------------------------------
//* CARDREF STEP 2 OF 5 - REBUILD VSAM MERCHRTE FROM CARDSVC.MERCHANT
//*
//* UNLOAD, SORT, DELETE, DEFINE, REPRO, VERIFY.  THE CLUSTER IS
//* READ BY THE AUTHORIZATION PATH ONLINE, SO THE REBUILD RUNS IN
//* THE SATURDAY WINDOW WITH THE CICS FILE CLOSED.  THE OPERATOR
//* CLOSES IT BEFORE SUBMITTING AND OPENS IT WHEN THE VERIFY STEP
//* HAS ENDED WITH RC 0 - NOT BEFORE.
//*
//* IF VERFMRCH ABENDS U911 THE CLUSTER IS SHORT OR EMPTY.  DO NOT
//* OPEN THE FILE TO CICS.  SEE DOCS RUNBOOK CARDBILL.
//* ------------------------------------------------------------------
//*
//JOBLIB   DD DSN=CARD.PROD.LOADLIB,DISP=SHR
//         DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//*
//* ------------------------------------------------------------------
//* STEP 1 - UNLOAD THE ACTIVE AND SUSPENDED MERCHANTS
//* ------------------------------------------------------------------
//UNLDMRCH EXEC PGM=IKJEFT01,DYNAMNBR=40
//STEPLIB  DD DSN=CARD.PROD.LOADLIB,DISP=SHR
//         DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//MERCHUNL DD DSN=&&MRCHUNL,DISP=(NEW,PASS),
//            UNIT=SYSDA,SPACE=(CYL,(120,30),RLSE),
//            DCB=(RECFM=FB,LRECL=120,BLKSIZE=27960)
//MERCHCTL DD DSN=CARD.PROD.REF.MERCHCTL,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN  PROGRAM(CBREF02) PLAN(CARDREFP) -
       LIB('CARD.PROD.LOADLIB') PARMS('UNLOAD')
  END
/*
//*
//* ------------------------------------------------------------------
//* STEP 2 - SORT INTO MERCHANT ID ORDER FOR THE REPRO INTO A KSDS
//* ------------------------------------------------------------------
//         IF (UNLDMRCH.RC = 0) THEN
//SORTMRCH EXEC PGM=SORT,REGION=64M
//SORTIN   DD DSN=&&MRCHUNL,DISP=(OLD,DELETE)
//SORTOUT  DD DSN=&&MRCHSRT,DISP=(NEW,PASS),
//            UNIT=SYSDA,SPACE=(CYL,(120,30),RLSE),
//            DCB=(RECFM=FB,LRECL=120,BLKSIZE=27960)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(60,15))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(60,15))
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=(1,15,CH,A)
  SUM FIELDS=NONE
  OPTION EQUALS,DYNALLOC=(SYSDA,4)
/*
//*
//* ------------------------------------------------------------------
//* STEP 3 - DELETE, DEFINE AND RELOAD THE CLUSTER
//* ------------------------------------------------------------------
//BLDMRCH  EXEC PGM=IDCAMS,COND=(0,NE,SORTMRCH)
//SYSPRINT DD SYSOUT=*
//UNLDIN   DD DSN=&&MRCHSRT,DISP=(OLD,PASS)
//SYSIN    DD *
 DELETE CARD.PROD.MERCHRTE CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.MERCHRTE)        -
         INDEXED                         -
         KEYS(15 0)                      -
         RECORDSIZE(120 120)             -
         CYLINDERS(60 15)                -
         SHAREOPTIONS(2 3)               -
         FREESPACE(15 10)                -
         VOLUMES(CARD01))                -
        DATA                             -
        (NAME(CARD.PROD.MERCHRTE.DATA)   -
         CISZ(4096))                     -
        INDEX                            -
        (NAME(CARD.PROD.MERCHRTE.INDEX)  -
         CISZ(2048))
 REPRO INFILE(UNLDIN)                    -
       OUTDATASET(CARD.PROD.MERCHRTE)
/*
//*
//* ------------------------------------------------------------------
//* STEP 4 - VERIFY THE REBUILT CLUSTER AGAINST DB2
//* ------------------------------------------------------------------
//VERFMRCH EXEC PGM=IKJEFT01,DYNAMNBR=40,
//            COND=(0,NE,BLDMRCH)
//STEPLIB  DD DSN=CARD.PROD.LOADLIB,DISP=SHR
//         DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//MERCHRTE DD DSN=CARD.PROD.MERCHRTE,DISP=SHR
//MERCHCTL DD DSN=CARD.PROD.REF.MERCHCTL,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN  PROGRAM(CBREF02) PLAN(CARDREFP) -
       LIB('CARD.PROD.LOADLIB') PARMS('VERIFY')
  END
/*
//         ELSE
//NOBUILD  EXEC PGM=IEFBR14
//         ENDIF
