//DEFPWCTL JOB (PRTY,SYS,0453),'DEFINE PARTYWK CYCLCTL',CLASS=A,
//             MSGCLASS=X,NOTIFY=&SYSUID
//*
//********************************************************************
//* DEFINE THE PARTYWK CYCLE CONTROL CLUSTER.                        *
//*                                                                  *
//* ONE RECORD PER CYCLE TYPE AND CYCLE DATE, LAYOUT CVCTRL01Y.      *
//* CBPRT03 REWRITES IT AT EVERY CHECKPOINT, WHICH IS WHY THE        *
//* SHARE OPTIONS ALLOW ONLY ONE UPDATER.                            *
//*                                                                  *
//* RUN ONCE AT INSTALL.  RERUNNING IT DELETES THE RESTART           *
//* POSITION - DO NOT RUN IT TO CLEAR A FAILED CYCLE.                *
//********************************************************************
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 /* ------------------------------------------------------------- */
 /* CYCLCTL - PARTYRSK BATCH CYCLE CONTROL AND RESTART      KSDS   */
 /* KEY IS CYCLE TYPE 8 PLUS CYCLE DATE 8.                         */
 /* ------------------------------------------------------------- */
 DELETE PRTY.PROD.CYCLCTL CLUSTER PURGE
 SET MAXCC = 0
 DEFINE CLUSTER                          -
        (NAME(PRTY.PROD.CYCLCTL)         -
         INDEXED                         -
         KEYS(16 0)                      -
         RECORDSIZE(256 256)             -
         TRACKS(15 5)                    -
         SHAREOPTIONS(1 3)               -
         FREESPACE(30 20)                -
         VOLUMES(PRTY01))                -
        DATA                             -
        (NAME(PRTY.PROD.CYCLCTL.DATA)    -
         CISZ(2048))                     -
        INDEX                            -
        (NAME(PRTY.PROD.CYCLCTL.INDEX)   -
         CISZ(1024))
/*
//
