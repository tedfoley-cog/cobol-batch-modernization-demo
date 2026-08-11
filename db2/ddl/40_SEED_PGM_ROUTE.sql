-- ---------------------------------------------------------------------
-- PGM_ROUTE SEED - THE ACTIVE DISPATCH TABLE
--
-- EVERY DYNAMICALLY REACHED PROGRAM IN THE ESTATE IS REGISTERED HERE.
-- CACRD90 (ONLINE) AND CBCRD90 (BATCH) RESOLVE TARGETS FROM THIS TABLE
-- AT RUN TIME.  THE VSAM PGMROUT FILE IS REBUILT FROM THESE ROWS BY
-- CBREF04J AND IS USED WHEN DB2 IS UNAVAILABLE.
-- ---------------------------------------------------------------------
SET CURRENT SQLID = 'CARDADM';

-- ---------------------------------------------------------------------
-- MENU - TRANSACTION CA00 OPTION DISPATCH FROM CACRD00
-- ---------------------------------------------------------------------
INSERT INTO CARDSVC.PGM_ROUTE
  (ROUTE_TYPE, ROUTE_KEY, SEQ_NBR, PGM_NAME, CALL_TYPE, MODULE_ID,
   EFF_DATE, ACTIVE_FLG, FALLBACK_PGM, DESCRIPTION, LAST_MAINT_BY)
VALUES
  ('MENU','OPT01   ',1,'CACRD01 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','ACCOUNT INQUIRY                         ','CARDADM '),
  ('MENU','OPT02   ',1,'CACRD02 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','CARD LIST / CROSS REFERENCE             ','CARDADM '),
  ('MENU','OPT03   ',1,'CACRD03 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','CARD DETAIL AND STATUS                  ','CARDADM '),
  ('MENU','OPT04   ',1,'CACRD04 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','AUTHORIZATION REQUEST ENTRY             ','CARDADM '),
  ('MENU','OPT05   ',1,'CACRD11 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','CARD MAINTENANCE BLOCK / REISSUE        ','CARDADM '),
  ('MENU','OPT06   ',1,'CACRD12 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','CREDIT LIMIT CHANGE REQUEST             ','CARDADM '),
  ('MENU','OPT07   ',1,'CACRD13 ','X','CARDSVC ','2020-06-01','Y','CACRD91 ','DISPUTE / CHARGEBACK ENTRY              ','CARDADM '),
  ('MENU','OPT08   ',1,'CACRD14 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','ONLINE PAYMENT POSTING                  ','CARDADM '),
  ('MENU','OPT09   ',1,'CACRD15 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','CUSTOMER / KYC INQUIRY                  ','CARDADM '),
  ('MENU','OPT10   ',1,'CACRD16 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','REFERENCE / REASON CODE LOOKUP          ','CARDADM '),
  ('MENU','OPTX    ',1,'CACRD17 ','X','CARDSVC ','2019-01-01','Y','CACRD91 ','EXIT AND SESSION AUDIT                  ','CARDADM ');

-- ---------------------------------------------------------------------
-- XMOD - CROSS MODULE CROSSINGS INTO PARTYRSK
--
-- THESE THREE ROWS ARE THE ONLY PLACE THE PARTYRSK ENTRY POINTS APPEAR
-- OUTSIDE PARTYRSK ITSELF.  NO CARDSVC SOURCE MEMBER NAMES THEM.
-- ---------------------------------------------------------------------
INSERT INTO CARDSVC.PGM_ROUTE
  (ROUTE_TYPE, ROUTE_KEY, SEQ_NBR, PGM_NAME, CALL_TYPE, MODULE_ID,
   EFF_DATE, ACTIVE_FLG, FALLBACK_PGM, DESCRIPTION, LAST_MAINT_BY)
VALUES
  ('XMOD','RISKSVC ',1,'PRKYC01 ','L','PARTYRSK','2019-01-01','Y','CACRD92 ','AUTH RISK ASSESSMENT ENTRY POINT        ','PRTYADM '),
  ('XMOD','KYCINQ  ',1,'PRKYC03 ','L','PARTYRSK','2019-01-01','Y','CACRD92 ','PARTY / KYC INQUIRY ENTRY POINT         ','PRTYADM '),
  ('XMOD','RSKRECAL',1,'PRBRSK1 ','D','PARTYRSK','2019-01-01','Y','        ','BATCH EXPOSURE RECALC ENTRY POINT       ','PRTYADM ');

-- ---------------------------------------------------------------------
-- FRAU - FRAUD RULE HANDLER PIPELINE, DRIVEN BY CACRD07 IN SEQ ORDER
-- ---------------------------------------------------------------------
INSERT INTO CARDSVC.PGM_ROUTE
  (ROUTE_TYPE, ROUTE_KEY, SEQ_NBR, PGM_NAME, CALL_TYPE, MODULE_ID,
   EFF_DATE, ACTIVE_FLG, FALLBACK_PGM, DESCRIPTION, LAST_MAINT_BY)
VALUES
  ('FRAU','STANDARD',1,'CAFRD01 ','D','CARDSVC ','2019-01-01','Y','        ','VELOCITY RULES                          ','CARDADM '),
  ('FRAU','STANDARD',2,'CAFRD02 ','D','CARDSVC ','2019-01-01','Y','        ','GEOGRAPHY AND MCC RULES                 ','CARDADM '),
  ('FRAU','STANDARD',3,'CAFRD03 ','D','CARDSVC ','2021-04-01','Y','        ','AMOUNT PATTERN RULES                    ','CARDADM '),
  ('FRAU','STANDARD',4,'CAFRD04 ','D','CARDSVC ','2022-09-01','Y','        ','MERCHANT REPUTATION RULES               ','CARDADM '),
  ('FRAU','HIGHRISK',1,'CAFRD01 ','D','CARDSVC ','2019-01-01','Y','        ','VELOCITY RULES                          ','CARDADM '),
  ('FRAU','HIGHRISK',2,'CAFRD02 ','D','CARDSVC ','2019-01-01','Y','        ','GEOGRAPHY AND MCC RULES                 ','CARDADM '),
  ('FRAU','HIGHRISK',3,'CAFRD04 ','D','CARDSVC ','2022-09-01','Y','        ','MERCHANT REPUTATION RULES               ','CARDADM ');

-- ---------------------------------------------------------------------
-- FEEC - FEE TYPE HANDLERS CALLED BY CBCRD05A
-- ---------------------------------------------------------------------
INSERT INTO CARDSVC.PGM_ROUTE
  (ROUTE_TYPE, ROUTE_KEY, SEQ_NBR, PGM_NAME, CALL_TYPE, MODULE_ID,
   EFF_DATE, ACTIVE_FLG, FALLBACK_PGM, DESCRIPTION, LAST_MAINT_BY)
VALUES
  ('FEEC','ANNU    ',1,'CBFEE01 ','D','CARDSVC ','2019-01-01','Y','        ','ANNUAL FEE HANDLER                      ','CARDADM '),
  ('FEEC','LATE    ',1,'CBFEE01 ','D','CARDSVC ','2019-01-01','Y','        ','LATE PAYMENT FEE HANDLER                ','CARDADM '),
  ('FEEC','OVLM    ',1,'CBFEE02 ','D','CARDSVC ','2019-01-01','Y','        ','OVER LIMIT FEE HANDLER                  ','CARDADM '),
  ('FEEC','CASH    ',1,'CBFEE02 ','D','CARDSVC ','2019-01-01','Y','        ','CASH ADVANCE FEE HANDLER                ','CARDADM '),
  ('FEEC','FRGN    ',1,'CBFEE03 ','D','CARDSVC ','2019-01-01','Y','        ','FOREIGN TRANSACTION FEE HANDLER         ','CARDADM ');

-- ---------------------------------------------------------------------
-- STMT - STATEMENT FORMAT HANDLERS CALLED BY CBBIL04
-- ---------------------------------------------------------------------
INSERT INTO CARDSVC.PGM_ROUTE
  (ROUTE_TYPE, ROUTE_KEY, SEQ_NBR, PGM_NAME, CALL_TYPE, MODULE_ID,
   EFF_DATE, ACTIVE_FLG, FALLBACK_PGM, DESCRIPTION, LAST_MAINT_BY)
VALUES
  ('STMT','PAPR    ',1,'CBSTM01 ','D','CARDSVC ','2019-01-01','Y','        ','PAPER STATEMENT FORMATTER               ','CARDADM '),
  ('STMT','ELEC    ',1,'CBSTM02 ','D','CARDSVC ','2020-01-01','Y','CBSTM01 ','ELECTRONIC STATEMENT FORMATTER          ','CARDADM ');

-- ---------------------------------------------------------------------
-- SUPERSEDED ROWS - RETAINED WITH EXP_DATE SET.  THE DISPATCHERS
-- FILTER ON CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE.
-- ---------------------------------------------------------------------
INSERT INTO CARDSVC.PGM_ROUTE
  (ROUTE_TYPE, ROUTE_KEY, SEQ_NBR, PGM_NAME, CALL_TYPE, MODULE_ID,
   EFF_DATE, EXP_DATE, ACTIVE_FLG, FALLBACK_PGM, DESCRIPTION, LAST_MAINT_BY)
VALUES
  ('MENU','OPT07   ',1,'CACRD07X','X','CARDSVC ','2019-01-01','2020-05-31','N','CACRD91 ','OLD DISPUTE ENTRY - REPLACED BY CACRD13 ','CARDADM '),
  ('STMT','ELEC    ',1,'CBSTM01 ','D','CARDSVC ','2019-01-01','2019-12-31','N','        ','ELEC STATEMENTS PRINTED AS PAPER        ','CARDADM ');

COMMIT;
