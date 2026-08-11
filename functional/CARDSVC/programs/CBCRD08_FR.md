# CBCRD08 — delinquency roll and aging (CBCRD08J, wave 6)

Target: Spring Batch job `cbcrd08` in `backend/…/jobs/cbcrd08/`.
Stream FRs owned: CARDNITE-FR-017 (delinquency/collections).

Note: the scheduler DESC "general ledger extract and balancing" (`sched/CARDNITE.sched:301`) is stale — CBCRD08 is the **delinquency roll / aging** (`app/jcl/cardsvc/CBCRD08J.jcl:6-11`); analysis §8 item 1.

## 1. Trigger / caller contract

Runs on CBCRD07J-OK. STEP010 CBCRD08; STEP020 IDCAMS prints the collections feed for transmission reconciliation when RC≤4 (`CBCRD08J.jcl:61-66`); the feed is transmitted by external job CBCRD88J, released on RC≤4 (`CBCRD08J.jcl:55-58`).
PARM: cycle date only (`CBCRD08J.jcl:38`).
SYSIN: `GRACE-DAYS=005` — collections policy CP-07 (`CBCRD08J.jcl:13-14,49-53`).

## 2. Field-level inputs / outputs

- Files: CYCLCTL; aging report AGERPT SYSOUT FBA 133; collections feed `CARD.PROD.COLLECT(+1)` FB **LRECL 100** (`CBCRD08J.jcl:41-48`; SELECTs `app/cardsvc/cbl/CBCRD08.cbl:56-64`).
- Db2: cursor + positioned UPDATE on `CARDSVC.ACCOUNT` (`CBCRD08.cbl:33,291,550`) — DELQ_BUCKET, DELQ_AMT.

## 3. Requirements owned

**CBCRD08-FR-001 — Bounded bucket roll.** DELQ_BUCKET rolls forward **at most one bucket per cycle**; DELQ_AMT recomputed; roll driven from payment due date + shortfall (with GRACE-DAYS), not the previous bucket alone — so a same-date re-run does not double-roll (`CBCRD08J.jcl:5-11,23-29`). Cross-ref CARDNITE-FR-017.

**CBCRD08-FR-002 — Collections feed.** Accounts at bucket ≥ 3 are written to the FB-100 collections feed (byte-compatible — external transmission contract, plan §4.3).

**CBCRD08-FR-003 — Legacy date handling.** Mixed 8-digit and legacy 6-digit dates handled with century windowing, pivot 50 (`CBCRD08.cbl:273`, `app/cpy/CVCONSTY.cpy:9-12`) — preserved in the shared date utility (D7).

## 4. Target mechanism

Chunk step with positioned updates via JPA (plan §3); GRACE-DAYS to validated configuration; commit interval `WS-COMMIT-FREQUENCY` (`CBCRD08.cbl:480`); COLLECT via `FlatFileItemWriter` byte-compatible FB 100; AGERPT layout-preserving (D6).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | nothing at charge-off |
| 4 | bucket-6 charge-off candidates present |
| 8 | no accounts or grace card rejected |
| 12 | U0803 SQL |

(`CBCRD08J.jcl:16-21`; U0802 file at `CBCRD08.cbl:337`.)

**Divergence reconciliation (FR §5.3 — CBCRD08 RC 8):** the scheduler labels RC 8 `OUTOFBAL` (out-of-balance), but out-of-balance is CBCRD07's U0704 abend, not a CBCRD08 outcome; the source's RC 8 means "no accounts selected or grace card rejected". Resolution: **source semantics govern**; RC 8 is reserved for the source meanings only, generic failures map to 12, and the `OUTOFBAL` scheduler label is retired (target design RC-mapping convention: a reserved business RC is never reused).

## 6. Hard-stop boundary

Collections transmission (CBCRD88J) is external — the byte-compatible file is the contract; nothing past the file is migrated.

## 7. Acceptance criteria

- Account one payment behind rolls exactly one bucket after grace; re-running the same cycle date rolls no further.
- Bucket ≥ 3 accounts appear on the FB-100 feed byte-exactly; bucket 6 candidates set RC 4.
- 6-digit legacy dates window correctly around pivot 50 (49→2049, 50→1950 boundary tests).
- Invalid GRACE-DAYS configuration exits 8 touching nothing.
