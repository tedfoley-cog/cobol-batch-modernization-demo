//DEFCARD  JOB (CARD,SYS),'DEFINE CARDSVC VSAM',CLASS=A,
//             MSGCLASS=X,NOTIFY=&SYSUID
//*
//* DEFINE THE CARDSVC VSAM CLUSTERS.
//* RUN ONCE AT INSTALL.  DELETE STATEMENTS TOLERATE NOT FOUND.
//*
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 /* ------------------------------------------------------------- */
 /* CARDXREF - CARD NUMBER TO ACCOUNT CROSS REFERENCE       KSDS   */
 /* ------------------------------------------------------------- */
 DELETE CARD.PROD.CARDXREF CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.CARDXREF)        -
         INDEXED                         -
         KEYS(16 0)                      -
         RECORDSIZE(128 128)             -
         CYLINDERS(150 30)               -
         SHAREOPTIONS(2 3)               -
         FREESPACE(20 10)                -
         VOLUMES(CARD01)                 -
         SPEED)                          -
        DATA                             -
        (NAME(CARD.PROD.CARDXREF.DATA)   -
         CISZ(4096))                     -
        INDEX                            -
        (NAME(CARD.PROD.CARDXREF.INDEX)  -
         CISZ(2048))
 /* ------------------------------------------------------------- */
 /* AUTHLOG - RAW AUTHORIZATION LOG, WRITTEN ONLINE         ESDS   */
 /* RECORDS ARE THE 60 BYTE VARIANT LAYOUT FROM CVAUTH01Y.         */
 /* ------------------------------------------------------------- */
 DELETE CARD.PROD.AUTHLOG CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.AUTHLOG)         -
         NONINDEXED                      -
         RECORDSIZE(200 200)             -
         CYLINDERS(900 150)              -
         SHAREOPTIONS(2 3)               -
         VOLUMES(CARD02 CARD03)          -
         SPEED)                          -
        DATA                             -
        (NAME(CARD.PROD.AUTHLOG.DATA)    -
         CISZ(8192))
 /* ------------------------------------------------------------- */
 /* MERCHRTE - MERCHANT SETTLEMENT ROUTING                  KSDS   */
 /* ------------------------------------------------------------- */
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
 /* ------------------------------------------------------------- */
 /* FRAUDRUL - FRAUD RULE PARAMETERS, LOADED FROM DB2       KSDS   */
 /* ------------------------------------------------------------- */
 DELETE CARD.PROD.FRAUDRUL CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.FRAUDRUL)        -
         INDEXED                         -
         KEYS(8 0)                       -
         RECORDSIZE(240 240)             -
         CYLINDERS(10 5)                 -
         SHAREOPTIONS(2 3)               -
         VOLUMES(CARD01))                -
        DATA                             -
        (NAME(CARD.PROD.FRAUDRUL.DATA)   -
         CISZ(4096))                     -
        INDEX                            -
        (NAME(CARD.PROD.FRAUDRUL.INDEX)  -
         CISZ(2048))
 /* ------------------------------------------------------------- */
 /* CUSTPREF - CUSTOMER SERVICING PREFERENCES               KSDS   */
 /* ------------------------------------------------------------- */
 DELETE CARD.PROD.CUSTPREF CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.CUSTPREF)        -
         INDEXED                         -
         KEYS(9 0)                       -
         RECORDSIZE(180 180)             -
         CYLINDERS(90 20)                -
         SHAREOPTIONS(2 3)               -
         FREESPACE(20 10)                -
         VOLUMES(CARD02))                -
        DATA                             -
        (NAME(CARD.PROD.CUSTPREF.DATA)   -
         CISZ(4096))                     -
        INDEX                            -
        (NAME(CARD.PROD.CUSTPREF.INDEX)  -
         CISZ(2048))
 /* ------------------------------------------------------------- */
 /* RSNCODE - REASON AND RESPONSE CODE REFERENCE            KSDS   */
 /* ------------------------------------------------------------- */
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
         CISZ(2048))                     -
        INDEX                            -
        (NAME(CARD.PROD.RSNCODE.INDEX)   -
         CISZ(1024))
 /* ------------------------------------------------------------- */
 /* CYCLCTL - BATCH CYCLE CONTROL AND RESTART               KSDS   */
 /* SHAREOPTIONS(1 3) - ONLY ONE UPDATER AT A TIME.                */
 /* ------------------------------------------------------------- */
 DELETE CARD.PROD.CYCLCTL CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.CYCLCTL)         -
         INDEXED                         -
         KEYS(16 0)                      -
         RECORDSIZE(256 256)             -
         TRACKS(15 5)                    -
         SHAREOPTIONS(1 3)               -
         VOLUMES(CARD01))                -
        DATA                             -
        (NAME(CARD.PROD.CYCLCTL.DATA)    -
         CISZ(2048))                     -
        INDEX                            -
        (NAME(CARD.PROD.CYCLCTL.INDEX)   -
         CISZ(1024))
 /* ------------------------------------------------------------- */
 /* PGMROUT - DYNAMIC ROUTE FALLBACK COPY OF PGM_ROUTE      KSDS   */
 /* REBUILT WEEKLY BY CBREF04J FROM THE DB2 TABLE.                 */
 /* ------------------------------------------------------------- */
 DELETE CARD.PROD.PGMROUT CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(CARD.PROD.PGMROUT)         -
         INDEXED                         -
         KEYS(16 0)                      -
         RECORDSIZE(97 97)               -
         TRACKS(15 5)                    -
         SHAREOPTIONS(2 3)               -
         VOLUMES(CARD01))                -
        DATA                             -
        (NAME(CARD.PROD.PGMROUT.DATA)    -
         CISZ(2048))                     -
        INDEX                            -
        (NAME(CARD.PROD.PGMROUT.INDEX)   -
         CISZ(1024))
 /* ------------------------------------------------------------- */
 /* ALTERNATE INDEX - CARDXREF BY ACCOUNT NUMBER                   */
 /* ------------------------------------------------------------- */
 DELETE CARD.PROD.CARDXREF.AIX1 CLUSTER PURGE
 SET MAXCC = 0
 DEFINE AIX                              -
        (NAME(CARD.PROD.CARDXREF.AIX1)   -
         RELATE(CARD.PROD.CARDXREF)      -
         KEYS(11 16)                     -
         RECORDSIZE(64 2048)             -
         NONUNIQUEKEY                    -
         UPGRADE                         -
         CYLINDERS(30 10)                -
         VOLUMES(CARD01))
 DEFINE PATH                             -
        (NAME(CARD.PROD.CARDXREF.PATH1)  -
         PATHENTRY(CARD.PROD.CARDXREF.AIX1))
/*
//
