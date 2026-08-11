-- ---------------------------------------------------------------------
-- CARDDB TABLES - CARD SERVICING MODULE (CARDSVC)
-- ---------------------------------------------------------------------
SET CURRENT SQLID = 'CARDADM';

-- ---------------------------------------------------------------------
-- ACCOUNT
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.ACCOUNT
   (ACCT_ID              DECIMAL(11,0)  NOT NULL,
    CUST_ID              DECIMAL(9,0)   NOT NULL,
    PARTY_ID             CHAR(11)       NOT NULL,
    PRODUCT_CD           CHAR(4)        NOT NULL,
    ACCT_STATUS          CHAR(1)        NOT NULL WITH DEFAULT 'O',
    OPEN_DATE            DATE           NOT NULL,
    CLOSE_DATE           DATE,
    CURRENCY_CD          CHAR(3)        NOT NULL WITH DEFAULT 'USD',
    CURR_BAL             DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    STMT_BAL             DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    PENDING_AUTH_AMT     DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    CASH_BAL             DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    MIN_PAY_DUE          DECIMAL(11,2)  NOT NULL WITH DEFAULT 0,
    LAST_PAY_AMT         DECIMAL(11,2)  NOT NULL WITH DEFAULT 0,
    LAST_PAY_DATE        DATE,
    CYCLE_DAY            SMALLINT       NOT NULL WITH DEFAULT 1,
    LAST_CYCLE_DATE      DATE,
    NEXT_CYCLE_DATE      DATE,
    PAY_DUE_DATE         DATE,
    DELQ_BUCKET          SMALLINT       NOT NULL WITH DEFAULT 0,
    DELQ_AMT             DECIMAL(11,2)  NOT NULL WITH DEFAULT 0,
    STMT_COUNT           INTEGER        NOT NULL WITH DEFAULT 0,
    BRANCH_CD            CHAR(5),
    LAST_MAINT_PGM       CHAR(8),
    LAST_MAINT_TS        TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (ACCT_ID),
    CONSTRAINT CC_ACCT_STAT CHECK (ACCT_STATUS IN ('O','C','S','W')),
    CONSTRAINT CC_ACCT_DELQ CHECK (DELQ_BUCKET BETWEEN 0 AND 6))
  IN CARDDB.TSACCT
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- CARD
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.CARD
   (CARD_NUM             CHAR(16)       NOT NULL,
    ACCT_ID              DECIMAL(11,0)  NOT NULL,
    CUST_ID              DECIMAL(9,0)   NOT NULL,
    EMBOSSED_NAME        CHAR(26)       NOT NULL,
    PRODUCT_CD           CHAR(4)        NOT NULL,
    CARD_STATUS          CHAR(1)        NOT NULL WITH DEFAULT 'N',
    EXPIRY_YYMM          CHAR(4)        NOT NULL,
    ISSUE_DATE           DATE           NOT NULL,
    ACTIVATION_DATE      DATE,
    LAST_USED_DATE       DATE,
    CVV_IND              CHAR(1)        NOT NULL WITH DEFAULT 'Y',
    PIN_TRIES            SMALLINT       NOT NULL WITH DEFAULT 0,
    REISSUE_CNT          SMALLINT       NOT NULL WITH DEFAULT 0,
    PREV_CARD_NUM        CHAR(16),
    BLOCK_REASON         CHAR(4),
    BLOCK_DATE           DATE,
    LAST_MAINT_PGM       CHAR(8),
    LAST_MAINT_TS        TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (CARD_NUM),
    CONSTRAINT CC_CARD_STAT CHECK (CARD_STATUS IN ('A','B','C','L','E','N')))
  IN CARDDB.TSCARD
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- CARD_LIMIT
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.CARD_LIMIT
   (CARD_NUM             CHAR(16)       NOT NULL,
    LIMIT_TYPE           CHAR(4)        NOT NULL,
    LIMIT_AMT            DECIMAL(13,2)  NOT NULL,
    USED_AMT             DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    AVAIL_AMT            DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    DAILY_CNT_LIMIT      SMALLINT       NOT NULL WITH DEFAULT 0,
    DAILY_CNT_USED       SMALLINT       NOT NULL WITH DEFAULT 0,
    VELOCITY_WINDOW_MIN  SMALLINT       NOT NULL WITH DEFAULT 60,
    VELOCITY_MAX_CNT     SMALLINT       NOT NULL WITH DEFAULT 5,
    APR_PCT              DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
    CASH_APR_PCT         DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
    RISK_BAND            CHAR(1),
    LAST_REVIEW_DATE     DATE,
    EFF_DATE             DATE           NOT NULL,
    EXP_DATE             DATE           NOT NULL WITH DEFAULT '9999-12-31',
    LAST_MAINT_TS        TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (CARD_NUM, LIMIT_TYPE, EFF_DATE),
    CONSTRAINT CC_LIM_TYPE CHECK (LIMIT_TYPE IN ('CRED','CASH','DAIL','FRGN')))
  IN CARDDB.TSLIMIT
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- AUTHORIZATION
-- AUTH_DETAIL IS THE 60 BYTE VARIANT AREA - SEE COPYBOOK CVAUTH01Y.
-- STORED AS CHAR SO THE HOST STRUCTURE MAPS DIRECTLY.
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.AUTHORIZATION
   (CARD_NUM             CHAR(16)       NOT NULL,
    AUTH_DATE            DATE           NOT NULL,
    AUTH_SEQ_NUM         DECIMAL(9,0)   NOT NULL,
    ACCT_ID              DECIMAL(11,0)  NOT NULL,
    CUST_ID              DECIMAL(9,0)   NOT NULL,
    AUTH_TIME            TIME           NOT NULL,
    AUTH_TYPE            CHAR(1)        NOT NULL,
    AUTH_STATUS          CHAR(1)        NOT NULL,
    RESP_CODE            CHAR(2),
    REASON_CD            CHAR(4),
    RISK_SCORE           SMALLINT,
    RISK_BAND            CHAR(1),
    SETTLED_FLG          CHAR(1)        NOT NULL WITH DEFAULT 'N',
    POSTED_FLG           CHAR(1)        NOT NULL WITH DEFAULT 'N',
    AUTH_DETAIL          CHAR(60)       NOT NULL,
    ORIG_PGM             CHAR(8),
    TERM_ID              CHAR(4),
    OPER_ID              CHAR(8),
    CREATE_TS            TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (CARD_NUM, AUTH_DATE, AUTH_SEQ_NUM),
    CONSTRAINT CC_AUTH_TYPE CHECK (AUTH_TYPE IN ('P','C','R')),
    CONSTRAINT CC_AUTH_STAT CHECK (AUTH_STATUS IN ('A','D','F','V')))
  IN CARDDB.TSAUTH
  PARTITION BY (AUTH_DATE ASC)
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- TRANSACTION
-- LEG DETAIL IS HELD IN TXN_LEG_DATA AS A VARIABLE LENGTH IMAGE OF THE
-- ODO TABLE IN CVTRAN01Y.  TXN_LEG_CNT GIVES THE OCCURRENCE COUNT.
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.TRANSACTION
   (TXN_ID               CHAR(16)       NOT NULL,
    POST_DATE            DATE           NOT NULL,
    ACCT_ID              DECIMAL(11,0)  NOT NULL,
    CARD_NUM             CHAR(16)       NOT NULL,
    AUTH_SEQ_NUM         DECIMAL(9,0),
    TXN_TYPE_CD          CHAR(4)        NOT NULL,
    TXN_SOURCE           CHAR(2)        NOT NULL,
    TXN_AMT              DECIMAL(13,2)  NOT NULL,
    CURRENCY_CD          CHAR(3)        NOT NULL,
    BILLING_AMT          DECIMAL(13,2)  NOT NULL,
    FX_RATE              DECIMAL(8,5)   NOT NULL WITH DEFAULT 1,
    MERCHANT_ID          CHAR(15),
    MCC                  CHAR(4),
    TXN_DESC             CHAR(40),
    TXN_LEG_CNT          SMALLINT       NOT NULL WITH DEFAULT 1,
    TXN_LEG_DATA         VARCHAR(400)   NOT NULL,
    CYCLE_ID             CHAR(8),
    GL_POSTED_FLG        CHAR(1)        NOT NULL WITH DEFAULT 'N',
    DISPUTE_FLG          CHAR(1)        NOT NULL WITH DEFAULT 'N',
    POSTED_BY            CHAR(8),
    POSTED_TS            TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (TXN_ID, POST_DATE),
    CONSTRAINT CC_TXN_LEGS CHECK (TXN_LEG_CNT BETWEEN 1 AND 12))
  IN CARDDB.TSTRAN
  PARTITION BY (POST_DATE ASC)
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- MERCHANT
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.MERCHANT
   (MERCHANT_ID          CHAR(15)       NOT NULL,
    MERCHANT_NAME        CHAR(40)       NOT NULL,
    MCC                  CHAR(4)        NOT NULL,
    ACQUIRER_ID          CHAR(11),
    COUNTRY_CD           CHAR(3)        NOT NULL,
    CITY                 CHAR(25),
    HIGH_RISK_FLG        CHAR(1)        NOT NULL WITH DEFAULT 'N',
    CHARGEBACK_RATE      DECIMAL(5,2)   NOT NULL WITH DEFAULT 0,
    SETTLE_ROUTE_CD      CHAR(4),
    STATUS               CHAR(1)        NOT NULL WITH DEFAULT 'A',
    ONBOARD_DATE         DATE,
    LAST_MAINT_TS        TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (MERCHANT_ID))
  IN CARDDB.TSREF
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- FEE_SCHEDULE
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.FEE_SCHEDULE
   (PRODUCT_CD           CHAR(4)        NOT NULL,
    FEE_TYPE             CHAR(4)        NOT NULL,
    EFF_DATE             DATE           NOT NULL,
    EXP_DATE             DATE           NOT NULL WITH DEFAULT '9999-12-31',
    FLAT_AMT             DECIMAL(9,2)   NOT NULL WITH DEFAULT 0,
    PCT_RATE             DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
    MIN_AMT              DECIMAL(9,2)   NOT NULL WITH DEFAULT 0,
    MAX_AMT              DECIMAL(9,2)   NOT NULL WITH DEFAULT 0,
    WAIVER_RULE_CD       CHAR(4),
    HANDLER_PGM          CHAR(8),
    CURRENCY_CD          CHAR(3)        NOT NULL WITH DEFAULT 'USD',
    DESCRIPTION          CHAR(40),
    PRIMARY KEY (PRODUCT_CD, FEE_TYPE, EFF_DATE),
    CONSTRAINT CC_FEE_TYPE CHECK (FEE_TYPE IN
       ('ANNU','LATE','OVLM','CASH','FRGN','RETN','REPL')))
  IN CARDDB.TSREF
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- FRAUD_RULE
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.FRAUD_RULE
   (RULE_ID              CHAR(8)        NOT NULL,
    RULE_CLASS           CHAR(4)        NOT NULL,
    RULE_SEQ             SMALLINT       NOT NULL,
    HANDLER_PGM          CHAR(8)        NOT NULL,
    THRESHOLD_AMT        DECIMAL(11,2),
    THRESHOLD_CNT        SMALLINT,
    THRESHOLD_PCT        DECIMAL(5,2),
    SCORE_POINTS         SMALLINT       NOT NULL WITH DEFAULT 0,
    ACTION_CD            CHAR(4)        NOT NULL,
    MCC_LIST             VARCHAR(200),
    COUNTRY_LIST         VARCHAR(120),
    EFF_DATE             DATE           NOT NULL,
    EXP_DATE             DATE           NOT NULL WITH DEFAULT '9999-12-31',
    ACTIVE_FLG           CHAR(1)        NOT NULL WITH DEFAULT 'Y',
    DESCRIPTION          CHAR(60),
    PRIMARY KEY (RULE_ID),
    CONSTRAINT CC_FRAUD_ACT CHECK (ACTION_CD IN ('SCOR','REFR','DECL','FLAG')))
  IN CARDDB.TSREF
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- REWARDS
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.REWARDS
   (ACCT_ID              DECIMAL(11,0)  NOT NULL,
    CYCLE_DATE           DATE           NOT NULL,
    PROGRAM_CD           CHAR(4)        NOT NULL,
    OPEN_POINTS          DECIMAL(11,0)  NOT NULL WITH DEFAULT 0,
    EARNED_POINTS        DECIMAL(11,0)  NOT NULL WITH DEFAULT 0,
    REDEEMED_POINTS      DECIMAL(11,0)  NOT NULL WITH DEFAULT 0,
    EXPIRED_POINTS       DECIMAL(11,0)  NOT NULL WITH DEFAULT 0,
    CLOSE_POINTS         DECIMAL(11,0)  NOT NULL WITH DEFAULT 0,
    EARN_RATE            DECIMAL(8,5)   NOT NULL WITH DEFAULT 1,
    BONUS_AMT            DECIMAL(11,2)  NOT NULL WITH DEFAULT 0,
    CALC_PGM             CHAR(8),
    CALC_TS              TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (ACCT_ID, CYCLE_DATE, PROGRAM_CD))
  IN CARDDB.TSREF
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- GL_POSTING
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.GL_POSTING
   (GL_BATCH_ID          CHAR(12)       NOT NULL,
    GL_SEQ_NUM           DECIMAL(9,0)   NOT NULL,
    POST_DATE            DATE           NOT NULL,
    VALUE_DATE           DATE           NOT NULL,
    GL_ACCOUNT           CHAR(10)       NOT NULL,
    COST_CENTRE          CHAR(6),
    DR_CR_IND            CHAR(1)        NOT NULL,
    AMOUNT               DECIMAL(15,2)  NOT NULL,
    CURRENCY_CD          CHAR(3)        NOT NULL,
    SOURCE_TXN_ID        CHAR(16),
    SOURCE_ACCT_ID       DECIMAL(11,0),
    NARRATIVE            CHAR(40),
    CYCLE_ID             CHAR(8),
    REVERSAL_FLG         CHAR(1)        NOT NULL WITH DEFAULT 'N',
    POSTED_BY            CHAR(8),
    POSTED_TS            TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (GL_BATCH_ID, GL_SEQ_NUM),
    CONSTRAINT CC_GL_DRCR CHECK (DR_CR_IND IN ('D','C')))
  IN CARDDB.TSGL
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- STATEMENT
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.STATEMENT
   (ACCT_ID              DECIMAL(11,0)  NOT NULL,
    CYCLE_DATE           DATE           NOT NULL,
    STMT_NUMBER          DECIMAL(6,0)   NOT NULL,
    CUST_ID              DECIMAL(9,0)   NOT NULL,
    FORMAT_CD            CHAR(4)        NOT NULL WITH DEFAULT 'PAPR',
    PERIOD_FROM          DATE           NOT NULL,
    PERIOD_TO            DATE           NOT NULL,
    DUE_DATE             DATE           NOT NULL,
    OPEN_BAL             DECIMAL(13,2)  NOT NULL,
    CLOSE_BAL            DECIMAL(13,2)  NOT NULL,
    PURCHASES_AMT        DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    CASH_ADV_AMT         DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    PAYMENTS_AMT         DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    FEES_AMT             DECIMAL(11,2)  NOT NULL WITH DEFAULT 0,
    INTEREST_AMT         DECIMAL(11,2)  NOT NULL WITH DEFAULT 0,
    MIN_PAY_AMT          DECIMAL(11,2)  NOT NULL WITH DEFAULT 0,
    CREDIT_LIMIT         DECIMAL(13,2)  NOT NULL,
    AVAIL_CREDIT         DECIMAL(13,2)  NOT NULL,
    APR_PCT              DECIMAL(8,5)   NOT NULL,
    REWARD_POINTS        DECIMAL(11,0)  NOT NULL WITH DEFAULT 0,
    LINE_CNT             INTEGER        NOT NULL WITH DEFAULT 0,
    PAGE_CNT             SMALLINT       NOT NULL WITH DEFAULT 1,
    GEN_PGM              CHAR(8),
    GEN_TS               TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (ACCT_ID, CYCLE_DATE))
  IN CARDDB.TSSTMT
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- DISPUTE
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.DISPUTE
   (DISPUTE_ID           CHAR(12)       NOT NULL,
    TXN_ID               CHAR(16)       NOT NULL,
    POST_DATE            DATE           NOT NULL,
    ACCT_ID              DECIMAL(11,0)  NOT NULL,
    CARD_NUM             CHAR(16)       NOT NULL,
    RAISED_DATE          DATE           NOT NULL,
    REASON_CD            CHAR(4)        NOT NULL,
    DISPUTE_AMT          DECIMAL(13,2)  NOT NULL,
    CURRENCY_CD          CHAR(3)        NOT NULL,
    STATUS               CHAR(2)        NOT NULL WITH DEFAULT 'OP',
    PROV_CREDIT_FLG      CHAR(1)        NOT NULL WITH DEFAULT 'N',
    PROV_CREDIT_AMT      DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    CHARGEBACK_REF       CHAR(16),
    REPRESENTMENT_FLG    CHAR(1)        NOT NULL WITH DEFAULT 'N',
    RESOLVED_DATE        DATE,
    RESOLUTION_CD        CHAR(4),
    RAISED_BY            CHAR(8),
    LAST_MAINT_TS        TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (DISPUTE_ID),
    CONSTRAINT CC_DISP_STAT CHECK (STATUS IN ('OP','IN','CB','RP','CL','RJ')))
  IN CARDDB.TSDISP
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- PGM_ROUTE - DYNAMIC CALL CONTROL TABLE
-- READ BY CACRD90 (ONLINE) AND CBCRD90 (BATCH).
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.PGM_ROUTE
   (ROUTE_TYPE           CHAR(4)        NOT NULL,
    ROUTE_KEY            CHAR(8)        NOT NULL,
    SEQ_NBR              SMALLINT       NOT NULL WITH DEFAULT 1,
    PGM_NAME             CHAR(8)        NOT NULL,
    CALL_TYPE            CHAR(1)        NOT NULL,
    MODULE_ID            CHAR(8)        NOT NULL,
    EFF_DATE             DATE           NOT NULL,
    EXP_DATE             DATE           NOT NULL WITH DEFAULT '9999-12-31',
    ACTIVE_FLG           CHAR(1)        NOT NULL WITH DEFAULT 'Y',
    FALLBACK_PGM         CHAR(8),
    DESCRIPTION          CHAR(40),
    LAST_MAINT_BY        CHAR(8),
    LAST_MAINT_TS        TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (ROUTE_TYPE, ROUTE_KEY, SEQ_NBR, EFF_DATE),
    CONSTRAINT CC_RT_CALL CHECK (CALL_TYPE IN ('L','X','C','D')),
    CONSTRAINT CC_RT_TYPE CHECK (ROUTE_TYPE IN
       ('MENU','FRAU','FEEC','XMOD','STMT')))
  IN CARDDB.TSCTRL
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- ROUTE_AUDIT - WHAT WAS ACTUALLY DISPATCHED
-- ---------------------------------------------------------------------
CREATE TABLE CARDSVC.ROUTE_AUDIT
   (AUDIT_TS             TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    CORREL_ID            CHAR(16)       NOT NULL,
    CALLER_PGM           CHAR(8)        NOT NULL,
    ROUTE_TYPE           CHAR(4)        NOT NULL,
    ROUTE_KEY            CHAR(8)        NOT NULL,
    SEQ_NBR              SMALLINT       NOT NULL,
    RESOLVED_PGM         CHAR(8)        NOT NULL,
    RESOLVED_MODULE      CHAR(8)        NOT NULL,
    CALL_TYPE            CHAR(1)        NOT NULL,
    USED_FALLBACK        CHAR(1)        NOT NULL WITH DEFAULT 'N',
    ROUTE_SOURCE         CHAR(1)        NOT NULL WITH DEFAULT 'D',
    RETURN_CD            SMALLINT,
    ELAPSED_MS           INTEGER,
    TRAN_ID              CHAR(4),
    JOB_NAME             CHAR(8),
    PRIMARY KEY (AUDIT_TS, CORREL_ID, CALLER_PGM, SEQ_NBR))
  IN CARDDB.TSCTRL
  CCSID EBCDIC;
