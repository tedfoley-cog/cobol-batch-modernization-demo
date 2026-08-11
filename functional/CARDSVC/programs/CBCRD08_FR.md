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

**CBCRD08-FR-001 — Bounded bucket roll.** Per execution, DELQ_BUCKET rolls forward **at most one bucket** and DELQ_AMT is recomputed; the roll is driven from payment due date + shortfall (with GRACE-DAYS) (`CBCRD08.cbl:495-540`; `CBCRD08J.jcl:5-11,23-29`). **The source has no per-cycle guard** — see the re-run divergence in §5. Cross-ref CARDNITE-FR-017.

**CBCRD08-FR-002 — Collections feed.** Accounts at bucket ≥ 3 are written to the FB-100 collections feed (byte-compatible — external transmission contract, plan §4.3).

**CBCRD08-FR-003 — Legacy date handling.** Mixed 8-digit and legacy 6-digit dates handled with century windowing, pivot 50 (`CBCRD08.cbl:273`, `app/cpy/CVCONSTY.cpy:9-12`) — preserved in the shared date utility (D7).

## 4. Target mechanism

Chunk step with positioned updates via JPA (plan §3); GRACE-DAYS to validated configuration; commit interval `WS-COMMIT-FREQUENCY` (`CBCRD08.cbl:480`); COLLECT via `FlatFileItemWriter` byte-compatible FB 100; AGERPT layout-preserving (D6).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | nothing at charge-off |
| 4 | bucket-6 charge-off candidates present |
| 8 | *unreachable in source* — JCL comment only; see divergences below |
| 12 | U0803 SQL |

(`CBCRD08J.jcl:16-21`; U0802 file at `CBCRD08.cbl:337`.)

**Divergence reconciliation (FR §5.3 — CBCRD08 RC 8):** the scheduler labels RC 8 `OUTOFBAL` (out-of-balance), but out-of-balance is CBCRD07's U0704 abend, not a CBCRD08 outcome; the JCL comment's RC 8 ("no accounts selected or grace card rejected") is itself **unreachable in the source** (stream FR §5.3 item 15) — the program sets only `WS-RC-WARNING` (0004) for charge-off candidates (`CBCRD08.cbl:520-537`) and ignores unrecognized control cards rather than rejecting them (`CBCRD08.cbl:378-392`). Resolution: **source semantics govern**; no RC 8 outcome exists, generic failures map to 12, and both the `OUTOFBAL` scheduler label and the JCL comment are retired.

**Divergence reconciliation (same-date re-run — same class as FR §5.3 item 9):** the JCL restart comment claims "a second run on the same cycle date does not double roll" (`CBCRD08J.jcl:23-29`), but the source records no cycle date on the account and the DELQCSR predicate (`DELQ_BUCKET > 0 OR (PAY_DUE_DATE < cycle date AND MIN_PAY_DUE > 0)`, `CBCRD08.cbl:275-303`) re-selects already-rolled accounts, whose `WHEN OTHER` arm unconditionally adds +1 (`CBCRD08.cbl:495-540`) — a same-date re-run **double-rolls** every still-short account. Resolution: legacy same-date re-run is unsafe; the **target adds an explicit per-cycle idempotency guard** (e.g. a last-roll cycle date on the account or roll ledger) as a deliberate improvement, proven by a re-run test — the §7 re-run criterion is a target rule, not legacy parity.

**Grace-days default caveat:** the source hardcodes `WS-GRACE-DAYS VALUE 003` (`CBCRD08.cbl:144`) while the operational SYSIN card supplies 005 (`CBCRD08J.jcl:13-14`), and a missing/invalid card is silently ignored — falling back to 3 would silently change bucket-1 timing. **Target:** graceDays is a required, validated parameter (no built-in default); startup fails fast when absent or non-numeric.

## 6. Hard-stop boundary

Collections transmission (CBCRD88J) is external — the byte-compatible file is the contract; nothing past the file is migrated.

## 7. Acceptance criteria

- Account one payment behind rolls exactly one bucket after grace; re-running the same cycle date rolls no further (per-cycle guard — target rule, §5; legacy double-rolls on re-run).
- Bucket ≥ 3 accounts appear on the FB-100 feed byte-exactly; bucket 6 candidates set RC 4.
- 6-digit legacy dates window correctly around pivot 50 (49→2049, 50→1950 boundary tests).
- Missing or invalid graceDays fails startup touching nothing (required parameter — target rule, §5; legacy silently falls back to the hardcoded 3).
