# CBCRD07 — general ledger feed (CBCRD07J, wave 6)

Target: Spring Batch job `cbcrd07` in `backend/…/jobs/cbcrd07/`; the aggregation+proof is one `@Transactional` service (plan §3 — app-tier, no SP).
Stream FRs owned: CARDNITE-FR-016 (GL balancing).

Note: the scheduler DESC "interest accrual on revolving balances" (`sched/CARDNITE.sched:270`) is stale — CBCRD07 is the **GL feed** (`app/jcl/cardsvc/CBCRD07J.jcl:6-13`, `app/cardsvc/cbl/CBCRD07.cbl:7`); analysis §8 item 1.

## 1. Trigger / caller contract

Runs on CBCRD06J-OK; holds `CARDDB-POSTING QUANTITY=1` (never overlaps CBCRD04J, `sched/CARDNITE.sched:280`); posts `GL-CARD-FEED-READY` for downstream GL pickup (`sched:313`).
PARM: cycle date only (`CBCRD07J.jcl:47`).
Control cards (SYSIN): Finance-owned mapping — `BATCH=`, `SUSPENSE=`, and `MAP <txn-type> <product|****> <debit-GL> <credit-GL> <cost-centre>` fixed-column cards; mapped types PURC, CASH, REFN, FEEA(GOLD/CLAS), FEEL, FEEO, FEEC, FEEF, INTR, INTC, RWDA (`CBCRD07J.jcl:15-24,53-70`).

## 2. Field-level inputs / outputs

- Files: CYCLCTL; GL report GLRPT SYSOUT FBA 133 (`CBCRD07J.jcl:50-52`; SELECTs `CBCRD07.cbl:66-70`).
- Db2: cursor over `CARDSVC.TRANSACTION`+`ACCOUNT` where `GL_POSTED_FLG='N'` (`CBCRD07.cbl:275-276`); INSERT `CARDSVC.GL_POSTING` (`CBCRD07.cbl:598`); UPDATE `TRANSACTION` posted flag (`CBCRD07.cbl:647-654`).

## 3. Requirements owned

**CBCRD07-FR-001 — Mapped double-entry feed.** Every unposted transaction produces debit/credit GL_POSTING rows per the Finance mapping; unmapped type/product combinations go to the SUSPENSE account and end RC 4 (Finance adds a card next day).

**CBCRD07-FR-002 — Debits = credits or nothing.** Total debits must equal total credits before commit; on difference the whole feed rolls back and the job abends U0704 — **no partial ledger ever** (`CBCRD07J.jcl:10-13`, `CBCRD07.cbl:737`; procedure `docs/runbook-cardnite.md:184-196`). Cross-ref CARDNITE-FR-016.

**CBCRD07-FR-003 — Once-only feed.** The `GL_POSTED_FLG='N'` predicate makes the job restartable/idempotent (`CBCRD07J.jcl:34-37`; `sched/CARDNITE.sched:293-294`).

## 4. Target mechanism

Mapping cards → `gl_mapping` table (Finance-owned, plan §3); aggregation + proof + flag update in one `@Transactional` service — rollback-on-imbalance is the Spring transaction; commit interval `WS-COMMIT-FREQUENCY` (`CBCRD07.cbl:522`) applies to the pre-proof staging reads only if the implementation keeps chunking, with the proof-and-post as the single final transaction. GLRPT layout-preserving (D6). Downstream pickup: table-is-the-interface + completion marker (plan §4.3).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | balanced |
| 4 | suspense used — Finance adds a mapping card |
| 8 | no transactions |
| 12 | U0701 card error, U0703 SQL, U0704 out of balance |

(`CBCRD07J.jcl:26-32`.)

## 6. Hard-stop boundary

GL downstream pickup is external — signalled only by `GL-CARD-FEED-READY`/completion marker; the feed itself stays in `gl_posting` (analysis §7.2).

## 7. Acceptance criteria

- Balanced seeded set: every transaction gets debit+credit rows, flags set, RC 0, GLRPT totals match.
- Unmapped combination posts to suspense, RC 4, named on GLRPT.
- Forced imbalance: zero `gl_posting` rows, zero flags updated, exit 12 with U0704 (Phase 4 failure-path E2E, FR-016).
- Re-run after success feeds nothing (flags already 'Y'), RC 8 "no transactions" semantics preserved.
- Malformed mapping configuration fails startup (U0701-equivalent), touching nothing.
