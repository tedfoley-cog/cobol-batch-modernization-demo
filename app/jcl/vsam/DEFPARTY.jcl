//DEFPARTY JOB (PRTY,SYS),'DEFINE PARTYRSK VSAM',CLASS=A,
//             MSGCLASS=X,NOTIFY=&SYSUID
//*
//* DEFINE THE PARTYRSK VSAM CLUSTERS.
//* PARTYRSK OWNS ITS OWN HLQ AND VOLUMES.
//*
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 /* ------------------------------------------------------------- */
 /* RSKPARM - RISK MODEL PARAMETERS                         KSDS   */
 /* KEY IS MODEL ID + PARM CODE, SEE CVSCOR01Y.                    */
 /* ------------------------------------------------------------- */
 DELETE PRTY.PROD.RSKPARM CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(PRTY.PROD.RSKPARM)         -
         INDEXED                         -
         KEYS(16 0)                      -
         RECORDSIZE(120 120)             -
         TRACKS(45 15)                   -
         SHAREOPTIONS(2 3)               -
         FREESPACE(20 10)                -
         VOLUMES(PRTY01))                -
        DATA                             -
        (NAME(PRTY.PROD.RSKPARM.DATA)    -
         CISZ(2048))                     -
        INDEX                            -
        (NAME(PRTY.PROD.RSKPARM.INDEX)   -
         CISZ(1024))
 /* ------------------------------------------------------------- */
 /* WATCHLST - CONSOLIDATED SANCTIONS WATCH LIST            KSDS   */
 /* REBUILT BY CBPRT02J FROM PARTYDB.PARTY_SANCTION.               */
 /* ------------------------------------------------------------- */
 DELETE PRTY.PROD.WATCHLST CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(PRTY.PROD.WATCHLST)        -
         INDEXED                         -
         KEYS(24 0)                      -
         RECORDSIZE(160 160)             -
         CYLINDERS(120 30)               -
         SHAREOPTIONS(2 3)               -
         FREESPACE(10 5)                 -
         VOLUMES(PRTY01 PRTY02))         -
        DATA                             -
        (NAME(PRTY.PROD.WATCHLST.DATA)   -
         CISZ(4096))                     -
        INDEX                            -
        (NAME(PRTY.PROD.WATCHLST.INDEX)  -
         CISZ(2048))
 /* ------------------------------------------------------------- */
 /* ALTERNATE INDEX - WATCHLST BY ENTITY NAME                      */
 /* USED BY PRKYC02 FOR FUZZY NAME SCREENING.                      */
 /* ------------------------------------------------------------- */
 DELETE PRTY.PROD.WATCHLST.AIX1 CLUSTER PURGE
 SET MAXCC = 0
 DEFINE AIX                              -
        (NAME(PRTY.PROD.WATCHLST.AIX1)   -
         RELATE(PRTY.PROD.WATCHLST)      -
         KEYS(60 24)                     -
         RECORDSIZE(96 4096)             -
         NONUNIQUEKEY                    -
         UPGRADE                         -
         CYLINDERS(40 10)                -
         VOLUMES(PRTY02))
 DEFINE PATH                             -
        (NAME(PRTY.PROD.WATCHLST.PATH1)  -
         PATHENTRY(PRTY.PROD.WATCHLST.AIX1))
/*
//
