//CBPRT03J JOB (PRTY,RISK,0453),'PORTFOLIO RESCORE',
//             CLASS=C,MSGCLASS=X,MSGLEVEL=(1,1),
//             REGION=0M,TIME=(300,00),NOTIFY=&SYSUID
//*
//********************************************************************
//* CBPRT03J - PARTYWK WEEKLY CYCLE, STEP 3 OF 4                     *
//*                                                                  *
//* DRIVES THE WHOLE ACTIVE PARTY BASE THROUGH THE RISK              *
//* RECALCULATION CHAIN.  THE LONGEST JOB IN THE CYCLE - ALLOW       *
//* FOUR TO FIVE HOURS ON A NORMAL WEEK.                             *
//*                                                                  *
//* SCHEDULED    SUNDAY 0500, AFTER CBPRT02J.                        *
//* CONTACT      PARTY AND RISK ON CALL, PAGER 4471.                 *
//*                                                                  *
//********************************************************************
//* RESTART=                                                         *
//*                                                                  *
//* THIS JOB IS CHECKPOINTED.  RESCORE COMMITS EVERY 1000 PARTIES    *
//* AND THEN REWRITES THE LAST PARTY ID INTO THE PARTYWK CYCLE       *
//* CONTROL RECORD ON PRTY.PROD.CYCLCTL.  ON A FAILURE THE           *
//* OPERATOR DOES NOT RERUN FROM THE TOP.                            *
//*                                                                  *
//* TO RESTART                                                       *
//*   1. CHECK THE JOB LOG FOR THE LINE                              *
//*        CBPRT03 RESTART WITH PARM=RESTART AFTER PARTY nnnnnnnnnnn *
//*      IF THE LINE IS NOT THERE THE STEP DIED BEFORE THE FIRST     *
//*      CHECKPOINT - RESUBMIT ON PARMS('/COLD') INSTEAD.            *
//*   2. RESUBMIT WITH                                               *
//*        RESTART=RESCORE                                           *
//*      ON THE JOB CARD AND CHANGE THE SYSTSIN RUN CARD FROM        *
//*        PARMS('/COLD')  TO  PARMS('/RESTART')                     *
//*      THE STEP THEN POSITIONS THE DRIVING CURSOR PAST THE LAST    *
//*      COMMITTED PARTY AND CARRIES ON.                             *
//*   3. DO NOT SKIP THE CHKCTL STEP - IT PROVES THE CONTROL         *
//*      RECORD IS THERE BEFORE FOUR HOURS OF WORK BEGIN.            *
//*                                                                  *
//* RERUNNING FROM THE TOP IS SAFE BUT WASTEFUL.  A PARTY SCORED     *
//* TWICE SIMPLY GETS A SECOND TIMESTAMPED SCORE ROW.                *
//*                                                                  *
//* IF THE CONTROL RECORD IS LOST, LEAVE THE RUN CARD ON             *
//* PARMS('/COLD').  THAT RESETS THE CYCLE AND STARTS AT THE FIRST   *
//* PARTY.                                                           *
//********************************************************************
//*
//JOBLIB   DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//*
//********************************************************************
//* STEP 010 - PROVE THE CYCLE CONTROL CLUSTER IS AVAILABLE.         *
//********************************************************************
//CHKCTL   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 LISTCAT ENTRIES(PRTY.PROD.CYCLCTL) ALL
/*
//*
//********************************************************************
//* STEP 020 - THE RESCORE ITSELF.                                   *
//*                                                                  *
//* THE PROGRAM PARM RIDES IN ON THE DSN RUN CARD                    *
//*   PARMS('/COLD')    START AT THE FIRST PARTY                     *
//*   PARMS('/RESTART') RESUME FROM THE CHECKPOINT                   *
//********************************************************************
//RESCORE  EXEC PGM=IKJEFT01,DYNAMNBR=40,COND=(0,NE,CHKCTL)
//STEPLIB  DD DSN=PRTY.PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS2.DB2P.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSOUT   DD SYSOUT=*
//CYCLCTL  DD DSN=PRTY.PROD.CYCLCTL,DISP=SHR
//RSKPARM  DD DSN=PRTY.PROD.RSKPARM,DISP=SHR
//WATCHNAM DD DSN=PRTY.PROD.WATCHLST.PATH1,DISP=SHR
//RSKEXCP  DD DSN=PRTY.PROD.RSKEXCP(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(80,40),RLSE),
//            DCB=(MODEL.DSCB,RECFM=FB,LRECL=133,BLKSIZE=27930)
//SYSTSIN  DD *
  DSN SYSTEM(DB2P) RETRY(5)
  RUN PROGRAM(CBPRT03) PLAN(PARTYWKP) -
      LIB('PRTY.PROD.LOADLIB') -
      PARMS('/COLD')
  END
/*
//*
//********************************************************************
//* STEP 030 - SORT THE EXCEPTIONS.  WORST RETURN CODE FIRST, THEN   *
//*            BY PARTY, SO THE REVIEW QUEUE LOADS IN PRIORITY       *
//*            ORDER.  RETURN CODE IS AT 13, PARTY ID AT 1.          *
//********************************************************************
//SORTEXCP EXEC PGM=SORT,COND=(8,LT,RESCORE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=PRTY.PROD.RSKEXCP(+1),DISP=SHR
//SORTOUT  DD DSN=PRTY.PROD.RSKEXCP.SORTED(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(80,40),RLSE),
//            DCB=(MODEL.DSCB,RECFM=FB,LRECL=133,BLKSIZE=27930)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(40,20))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(40,20))
//SYSIN    DD *
  SORT FIELDS=(13,4,CH,D,1,11,CH,A)
  OPTION EQUALS,DYNALLOC=(SYSDA,4)
/*
//*
//********************************************************************
//* STEP 040 - PRINT THE MANUAL REVIEW LIST.                         *
//********************************************************************
//         IF (SORTEXCP.RUN AND SORTEXCP.RC = 0) THEN
//PRTEXCP  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=PRTY.PROD.RSKEXCP.SORTED(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(A,,RSK1),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//SYSIN    DD DUMMY
//         ENDIF
//*
//********************************************************************
//* STEP 050 - RESTART INSTRUCTIONS ON THE OPERATOR CONSOLE LISTING  *
//*            WHEN THE RESCORE FAILED.                              *
//********************************************************************
//         IF (RESCORE.RC > 8) THEN
//FAILMSG  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
 CBPRT03J STEP RESCORE FAILED.
 DO NOT RERUN FROM THE TOP.  TAKE THE LAST PARTY ID FROM THE JOB LOG,
 RESUBMIT WITH RESTART=RESCORE AND PARMS('/RESTART') ON THE RUN CARD.
 CALL PARTY AND RISK ON CALL, PAGER 4471, IF THE SECOND ATTEMPT ALSO
 FAILS OR IF THE JOB LOG SHOWS NO CHECKPOINT LINE.
/*
//         ENDIF
//
