# CBCRD03 — sort + merchant enrichment (CBCRD03J: DFSORT step + CBCRD03, wave 3)

Target: Spring Batch job `cbcrd03` in `backend/…/jobs/cbcrd03/` (two steps mirroring the JCL).
Stream FRs owned: CARDNITE-FR-005 (merchant enrichment).

## 1. Trigger / caller contract

Runs on CBCRD02J-OK. Two steps, gated `IF STEP010.RC = 0` (`app/jcl/cardsvc/CBCRD03J.jcl:64-65`).

### 1.1 STEP010 — DFSORT utility step contract

`PGM=SORT` (`CBCRD03J.jcl:37-58`): SORTIN `AUTHCLN(0)` FB 300 → SORTOUT `CARD.PROD.AUTHSRT(+1)` FB 300.
Control cards: `SORT FIELDS` on card number / date / sequence, plus an `INCLUDE` dropping rejected records; field displacements 035/051/059/214 per the CVAXTR01Y map (`CBCRD03J.jcl:16-20`).
*Target:* a sort step (in-memory or external sort via Spring Batch) keyed (cardNumber, date, seq) with a filter on edit status ≠ 'R' — the INCLUDE is a belt-and-braces filter since CBCRD02 already routes rejects away; keep it (defensive parity).

### 1.2 STEP020 — CBCRD03 enrichment

Via CARDDB2 proc, plan CARDNITP. PARM: `PARM('20240131')` cycle date only on the DSN RUN card (`CBCRD03J.jcl:69`).
SYSIN control cards: `DEFAULT-ROUTE=DFLT`, `DEFAULT-ACQID=99999999999` — business parameters owned by Settlements (`CBCRD03J.jcl:85-90`).

## 2. Field-level inputs / outputs

- In: `AUTHSRT(0)` FB 300; VSAM KSDS `CARD.PROD.MERCHRTE` (KEYS(15 0), RECORDSIZE(120 120), `app/jcl/vsam/DEFCARD.jcl:53-60`); CYCLCTL (`CBCRD03J.jcl:72-79`; SELECTs `app/cardsvc/cbl/CBCRD03.cbl:48-62`).
- Db2: `SELECT … FROM CARDSVC.MERCHANT` (`CBCRD03.cbl:389`, header `CBCRD03.cbl:25`).
- Out: `CARD.PROD.AUTHENR(+1)` FB 300 with AX-ENRICH populated: MCC, acquirer, settlement route, merchant name, high-risk flag (`app/cardsvc/cpy/CVAXTR01Y.cpy:31-40`).

## 3. Requirements owned

**CBCRD03-FR-001 — Deterministic ordering.** Downstream posting consumes card/date/seq order; the sort establishes it (CARDNITE-FR-005 precondition).

**CBCRD03-FR-002 — Merchant enrichment with governed defaults.** Every record is enriched from MERCHANT/MERCHRTE; unresolved merchants take the Settlements-owned defaults (DEFAULT-ROUTE/DEFAULT-ACQID) and count toward RC 4.

## 4. Target mechanism

`merch_route` table (D4) replaces MERCHRTE browse; MERCHANT via repository. Defaults move from SYSIN cards to configuration owned by Settlements (parameter table per plan wave 3). Files per D5 convention.

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | all merchants resolved |
| 4 | defaults taken |
| 8 | *unreachable in source* — JCL comment only; see divergence below |
| 12 | fatal U0302–U0303 |

(`CBCRD03J.jcl:22-26`.)

**Divergence reconciliation (unreachable RC 8 — stream FR §5.3 item 15):** the JCL comment labels 8 "default card missing or MERCHRTE unusable", but the source sets only 0000/0004 (`WS-RC-WARNING` on rejects, `CBCRD03.cbl:28-30,493-495`) and routes fatal conditions through the U030x abend path (RC 12) — no 0008 is ever set. Resolution: **source governs** — missing/unusable configuration is fatal (exit 12), not RC 8; JCL comment retired.

Restart: RESTART=STEP020 safe if the sort was good (`CBCRD03J.jcl:28-31`) — target: step-level restart with the sorted file retained.

## 6. Hard-stop boundary

None.

## 7. Acceptance criteria

- Output order is strictly (cardNumber, date, seq); any record with edit status 'R' present on input is dropped by the sort filter.
- Known merchant enriches with its MCC/acquirer/route; unknown merchant gets DFLT route + default acquirer id and the job exits 4.
- Missing default configuration is fatal at startup (exit 12, U030x-equivalent) before processing records (divergence §5).
- Enrichment never mutates the 179-byte auth image.
