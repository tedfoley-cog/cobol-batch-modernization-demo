-- ---------------------------------------------------------------------
-- PARTYDB TABLES - PARTY / KYC / RISK MODULE (PARTYRSK)
-- ---------------------------------------------------------------------
SET CURRENT SQLID = 'PRTYADM';

-- ---------------------------------------------------------------------
-- CUSTOMER
-- ---------------------------------------------------------------------
CREATE TABLE PARTYRSK.CUSTOMER
   (CUST_ID              DECIMAL(9,0)   NOT NULL,
    PARTY_ID             CHAR(11)       NOT NULL,
    PARTY_TYPE           CHAR(1)        NOT NULL WITH DEFAULT 'I',
    TITLE                CHAR(4),
    FIRST_NAME           CHAR(20),
    MIDDLE_INIT          CHAR(1),
    LAST_NAME            CHAR(25)       NOT NULL,
    LEGAL_NAME           CHAR(60),
    DOB                  DATE,
    NATIONAL_ID          CHAR(11),
    TAX_ID               CHAR(15),
    ADDR_LINE1           CHAR(30),
    ADDR_LINE2           CHAR(30),
    CITY                 CHAR(25),
    STATE_CD             CHAR(2),
    POSTAL_CD            CHAR(10),
    COUNTRY_CD           CHAR(3)        NOT NULL,
    DOMICILE_CTRY        CHAR(3),
    CITIZENSHIP_CTRY     CHAR(3),
    PHONE_HOME           CHAR(15),
    PHONE_MOBILE         CHAR(15),
    EMAIL                CHAR(50),
    SEGMENT_CD           CHAR(4),
    PEP_FLG              CHAR(1)        NOT NULL WITH DEFAULT 'N',
    VIP_FLG              CHAR(1)        NOT NULL WITH DEFAULT 'N',
    CUST_STATUS          CHAR(1)        NOT NULL WITH DEFAULT 'A',
    ONBOARD_DATE         DATE           NOT NULL,
    LAST_MAINT_TS        TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (CUST_ID),
    CONSTRAINT CC_CUST_TYPE CHECK (PARTY_TYPE IN ('I','O','T')),
    CONSTRAINT CC_CUST_STAT CHECK (CUST_STATUS IN ('A','D','X')))
  IN PARTYDB.TSCUST
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- PARTY_KYC
-- ---------------------------------------------------------------------
CREATE TABLE PARTYRSK.PARTY_KYC
   (PARTY_ID             CHAR(11)       NOT NULL,
    KYC_SEQ              SMALLINT       NOT NULL,
    KYC_STATUS           CHAR(2)        NOT NULL,
    KYC_LEVEL            CHAR(4)        NOT NULL,
    REVIEW_TYPE          CHAR(4)        NOT NULL,
    REVIEW_DATE          DATE           NOT NULL,
    NEXT_REVIEW_DATE     DATE,
    EXPIRY_DATE          DATE,
    ID_DOC_TYPE          CHAR(4),
    ID_DOC_REF           CHAR(20),
    ID_DOC_EXPIRY        DATE,
    ADDR_VERIFIED_FLG    CHAR(1)        NOT NULL WITH DEFAULT 'N',
    SOURCE_OF_FUNDS_CD   CHAR(4),
    RISK_RATING          CHAR(1),
    ENHANCED_DD_FLG      CHAR(1)        NOT NULL WITH DEFAULT 'N',
    REVIEWED_BY          CHAR(8),
    REVIEW_NOTES         VARCHAR(200),
    LAST_MAINT_TS        TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (PARTY_ID, KYC_SEQ),
    CONSTRAINT CC_KYC_STAT CHECK (KYC_STATUS IN ('OK','PN','EX','FL')))
  IN PARTYDB.TSKYC
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- PARTY_SANCTION
-- ---------------------------------------------------------------------
CREATE TABLE PARTYRSK.PARTY_SANCTION
   (LIST_CD              CHAR(8)        NOT NULL,
    ENTRY_ID             CHAR(16)       NOT NULL,
    ENTITY_NAME          CHAR(60)       NOT NULL,
    ENTITY_TYPE          CHAR(1)        NOT NULL WITH DEFAULT 'I',
    COUNTRY_CD           CHAR(3),
    DOB                  DATE,
    PROGRAM_CD           CHAR(12),
    LISTED_DATE          DATE           NOT NULL,
    DELISTED_DATE        DATE,
    ACTIVE_FLG           CHAR(1)        NOT NULL WITH DEFAULT 'Y',
    LOAD_JOB             CHAR(8),
    LOAD_TS              TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (LIST_CD, ENTRY_ID))
  IN PARTYDB.TSSANC
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- PARTY_EXPOSURE
-- ---------------------------------------------------------------------
CREATE TABLE PARTYRSK.PARTY_EXPOSURE
   (PARTY_ID             CHAR(11)       NOT NULL,
    AS_OF_DATE           DATE           NOT NULL,
    PROD_SYSTEM          CHAR(8)        NOT NULL,
    PRODUCT_CD           CHAR(4)        NOT NULL,
    CURRENCY_CD          CHAR(3)        NOT NULL,
    ACCT_CNT             INTEGER        NOT NULL WITH DEFAULT 0,
    TOTAL_LIMIT          DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
    TOTAL_DRAWN          DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
    TOTAL_AVAILABLE      DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
    UNSECURED_AMT        DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
    SECURED_AMT          DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
    PAST_DUE_AMT         DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    WRITTEN_OFF_AMT      DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
    DELQ_BUCKET          SMALLINT       NOT NULL WITH DEFAULT 0,
    UTILISATION_PCT      DECIMAL(5,2)   NOT NULL WITH DEFAULT 0,
    STALE_FLG            CHAR(1)        NOT NULL WITH DEFAULT 'N',
    CALC_PGM             CHAR(8),
    CALC_TS              TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    PRIMARY KEY (PARTY_ID, AS_OF_DATE, PROD_SYSTEM, PRODUCT_CD))
  IN PARTYDB.TSEXPO
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- PARTY_RISK_SCORE
-- ---------------------------------------------------------------------
CREATE TABLE PARTYRSK.PARTY_RISK_SCORE
   (PARTY_ID             CHAR(11)       NOT NULL,
    SCORE_TS             TIMESTAMP      NOT NULL,
    MODEL_ID             CHAR(8)        NOT NULL,
    MODEL_VERSION        SMALLINT       NOT NULL,
    RISK_SCORE           SMALLINT       NOT NULL,
    RISK_BAND            CHAR(1)        NOT NULL,
    PD_PCT               DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
    EXPOSURE_AMT         DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
    ADVICE_CD            CHAR(4)        NOT NULL,
    REASON_CD            CHAR(4),
    KYC_STATUS           CHAR(2),
    SANCTION_FLG         CHAR(1)        NOT NULL WITH DEFAULT 'N',
    COMPONENT_DATA       VARCHAR(400),
    OVERRIDE_FLG         CHAR(1)        NOT NULL WITH DEFAULT 'N',
    OVERRIDE_BY          CHAR(8),
    OVERRIDE_REASON      CHAR(40),
    SCORED_BY            CHAR(8)        NOT NULL,
    SOURCE_CHANNEL       CHAR(1)        NOT NULL,
    PRIMARY KEY (PARTY_ID, SCORE_TS),
    CONSTRAINT CC_SCORE_BAND CHECK (RISK_BAND IN ('A','B','C','X')),
    CONSTRAINT CC_SCORE_RANGE CHECK (RISK_SCORE BETWEEN 0 AND 999),
    CONSTRAINT CC_SCORE_ADV CHECK (ADVICE_CD IN ('APPR','REFR','DECL')))
  IN PARTYDB.TSSCORE
  CCSID EBCDIC;

-- ---------------------------------------------------------------------
-- PARTY_RISK_AUDIT
-- REGULATORY AUDIT TRAIL - APPEND ONLY, NEVER UPDATED.
-- ---------------------------------------------------------------------
CREATE TABLE PARTYRSK.PARTY_RISK_AUDIT
   (AUDIT_ID             CHAR(20)       NOT NULL,
    PARTY_ID             CHAR(11)       NOT NULL,
    EVENT_TS             TIMESTAMP      NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    EVENT_TYPE           CHAR(4)        NOT NULL,
    REQUESTING_MODULE    CHAR(8)        NOT NULL,
    REQUESTING_PGM       CHAR(8)        NOT NULL,
    CORREL_ID            CHAR(16),
    CHANNEL              CHAR(1)        NOT NULL,
    OLD_SCORE            SMALLINT,
    NEW_SCORE            SMALLINT,
    OLD_BAND             CHAR(1),
    NEW_BAND             CHAR(1),
    KYC_STATUS           CHAR(2),
    SANCTION_FLG         CHAR(1),
    ADVICE_CD            CHAR(4),
    REASON_TXT           CHAR(60),
    HOP_TRACE            VARCHAR(120),
    PRIMARY KEY (AUDIT_ID),
    CONSTRAINT CC_AUD_EVENT CHECK (EVENT_TYPE IN
       ('AUTH','INQY','RCAL','WKLY','OVRD')))
  IN PARTYDB.TSRAUD
  CCSID EBCDIC;
