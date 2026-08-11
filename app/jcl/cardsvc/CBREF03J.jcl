//CBREF03J JOB (CARD,REFR),'REBUILD FRAUD REF',CLASS=C,MSGCLASS=X,
//             REGION=0M,TIME=(20,00),NOTIFY=&SYSUID
//*
//* ------------------------------------------------------------------
//* CARDREF STEP 3 OF 5 - REBUILD VSAM FRAUDRUL AND RSNCODE
//*
//* FRAUDRUL COMES FROM CARDSVC.FRAUD_RULE.  RSNCODE COMES FROM THE
//* SCHEME REASON CODE REFERENCE FEED, WHICH IS NOT IN DB2 AT ALL -
//* IT HAS ARRIVED AS A FLAT FILE SINCE THE 1998 SCHEME RELEASE AND
//* NOBODY HAS EVER LOADED IT INTO A TABLE.
//*
//* CBREF03 DROPS REASON CODE RECORDS THAT FAIL VALIDATION AND ENDS
//* WITH RC 4 WHEN IT DOES.  THAT IS NOT AN ERROR - THE SCHEME SENDS
//* THE WHOLE CATALOGUE INCLUDING CODES FOR PRODUCTS THE BANK DOES
//* NOT ISSUE.  A COUNT MISMATCH ON EITHER CLUSTER IS AN ABEND.
//* ------------------------------------------------------------------
//*
//JOBLIB   DD DSN=CARD.PROD.LOADLIB,DISP=SHR
//         DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//*
//* ------------------------------------------------------------------
//* STEP 1 - UNLOAD THE FRAUD RULES AND VALIDATE THE REASON FEED
//* ------------------------------------------------------------------
//UNLDREF  EXEC PGM=IKJEFT01,DYNAMNBR=40
//STEPLIB  DD DSN=CARD.PROD.LOADLIB,DISP=SHR
//         DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//FRAUUNL  DD DSN=&&FRAUUNL,DISP=(NEW,PASS),
//            UNIT=SYSDA,SPACE=(TRK,(45,15),RLSE),
//            DCB=(RECFM=FB,LRECL=240,BLKSIZE=27840)
//RSNFEED  DD DSN=CARD.PROD.REF.RSNFEED(0),DISP=SHR
//RSNUNL   DD DSN=&&RSNUNL,DISP=(NEW,PASS),
//            UNIT=SYSDA,SPACE=(TRK,(30,10),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=27900)
//REFCTL   DD DSN=CARD.PROD.REF.REFCTL,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN  PROGRAM(CBREF03) PLAN(CARDREFP) -
       LIB('CARD.PROD.LOADLIB') PARMS('UNLOAD')
  END
/*
//*
//* ------------------------------------------------------------------
//* STEP 2 - SORT BOTH UNLOADS INTO KEY ORDER
//* ------------------------------------------------------------------
//         IF (UNLDREF.RC <= 4) THEN
//SORTFRAU EXEC PGM=SORT,REGION=64M
//SORTIN   DD DSN=&&FRAUUNL,DISP=(OLD,DELETE)
//SORTOUT  DD DSN=&&FRAUSRT,DISP=(NEW,PASS),
//            UNIT=SYSDA,SPACE=(TRK,(45,15),RLSE),
//            DCB=(RECFM=FB,LRECL=240,BLKSIZE=27840)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(15,5))
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=(1,8,CH,A)
  OPTION EQUALS
/*
//*
//SORTRSN  EXEC PGM=SORT,REGION=64M,COND=(0,NE,SORTFRAU)
//SORTIN   DD DSN=&&RSNUNL,DISP=(OLD,DELETE)
//SORTOUT  DD DSN=&&RSNSRT,DISP=(NEW,PASS),
//            UNIT=SYSDA,SPACE=(TRK,(30,10),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=27900)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(15,5))
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=(1,8,CH,A)
  OPTION EQUALS
/*
//*
//* ------------------------------------------------------------------
//* STEP 3 - REBUILD BOTH CLUSTERS
//* ------------------------------------------------------------------
//BLDREF   EXEC PGM=IDCAMS,COND=(0,NE,SORTRSN)
//SYSPRINT DD SYSOUT=*
//FRAUIN   DD DSN=&&FRAUSRT,DISP=(OLD,PASS)
//RSNIN    DD DSN=&&RSNSRT,DISP=(OLD,PASS)
//SYSIN    DD *
 DELETE CARD.PROD.FRAUDRUL CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.FRAUDRUL)        -
         INDEXED                         -
         KEYS(8 0)                       -
         RECORDSIZE(240 240)             -
         TRACKS(45 15)                   -
         SHAREOPTIONS(2 3)               -
         FREESPACE(20 10)                -
         VOLUMES(CARD01))                -
        DATA                             -
        (NAME(CARD.PROD.FRAUDRUL.DATA)   -
         CISZ(4096))                     -
        INDEX                            -
        (NAME(CARD.PROD.FRAUDRUL.INDEX)  -
         CISZ(1024))
 REPRO INFILE(FRAUIN)                    -
       OUTDATASET(CARD.PROD.FRAUDRUL)
 DELETE CARD.PROD.RSNCODE CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.RSNCODE)         -
         INDEXED                         -
         KEYS(8 0)                       -
         RECORDSIZE(100 100)             -
         TRACKS(30 10)                   -
         SHAREOPTIONS(2 3)               -
         VOLUMES(CARD01))                -
        DATA                             -
        (NAME(CARD.PROD.RSNCODE.DATA)    -
         CISZ(4096))                     -
        INDEX                            -
        (NAME(CARD.PROD.RSNCODE.INDEX)   -
         CISZ(1024))
 REPRO INFILE(RSNIN)                     -
       OUTDATASET(CARD.PROD.RSNCODE)
/*
//*
//* ------------------------------------------------------------------
//* STEP 4 - VERIFY BOTH CLUSTERS
//* ------------------------------------------------------------------
//VERFREF  EXEC PGM=IKJEFT01,DYNAMNBR=40,COND=(0,NE,BLDREF)
//STEPLIB  DD DSN=CARD.PROD.LOADLIB,DISP=SHR
//         DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//FRAUDRUL DD DSN=CARD.PROD.FRAUDRUL,DISP=SHR
//RSNCODE  DD DSN=CARD.PROD.RSNCODE,DISP=SHR
//REFCTL   DD DSN=CARD.PROD.REF.REFCTL,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN  PROGRAM(CBREF03) PLAN(CARDREFP) -
       LIB('CARD.PROD.LOADLIB') PARMS('VERIFY')
  END
/*
//         ELSE
//NOBUILD  EXEC PGM=IEFBR14
//         ENDIF
