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

**CBCRD07-FR-002 — Debits = credits or nothing (target rule).** Total debits must equal total credits; on difference the job abends U0704 with **nothing left in the ledger** (`CBCRD07J.jcl:10-13`, `CBCRD07.cbl:728-738`; procedure `docs/runbook-cardnite.md:184-196`). **Divergence — legacy cannot deliver full rollback (stream FR §5.3 item 17):** the header claims "the whole unit of work is rolled back" (`CBCRD07.cbl:27-30`), but the loop commits every `WS-COMMIT-FREQUENCY` inserted rows (`CBCRD07.cbl:522-528`) after already flagging transactions `GL_POSTED_FLG='Y'` (`CBCRD07.cbl:645-654`); the proof runs only at end of run (`CBCRD07.cbl:706-740`) and the U0704 `ROLLBACK` (`CBCRD07.cbl:820`) undoes only work since the last commit — a feed larger than the commit frequency (default 1000) leaves committed `GL_POSTING` rows and flags behind, and a re-run skips them (the `GL_POSTED_FLG='N'` predicate). The target performs the proof **before** any commit (single transaction) — a deliberate correction, not legacy parity. Cross-ref CARDNITE-FR-016.

**CBCRD07-FR-003 — Once-only feed.** The `GL_POSTED_FLG='N'` predicate makes the job restartable/idempotent (`CBCRD07J.jcl:34-37`; `sched/CARDNITE.sched:293-294`).

## 4. Target mechanism

Mapping cards → `gl_mapping` table (Finance-owned, plan §3); aggregation + proof + flag update in one `@Transactional` service — rollback-on-imbalance is the Spring transaction; commit interval `WS-COMMIT-FREQUENCY` (`CBCRD07.cbl:522`) applies to the pre-proof staging reads only if the implementation keeps chunking, with the proof-and-post as the single final transaction. GLRPT layout-preserving (D6). Downstream pickup: table-is-the-interface + completion marker (plan §4.3).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | balanced |
| 4 | suspense used — Finance adds a mapping card |
| 8 | *unreachable in source* — JCL comment only; see divergence below |
| 12 | U0701 card error, U0703 SQL, U0704 out of balance |

(`CBCRD07J.jcl:26-32`.)

**Divergence reconciliation (unreachable RC 8 — stream FR §5.3 item 15):** the JCL comment labels 8 "no transactions", but the source sets only `WS-RC-WARNING` (0004, suspense used, `CBCRD07.cbl:560-566`) — no 0008 is ever set; a feed with nothing to post ends RC 0/4. Resolution: **source governs**; JCL comment retired.

## 6. Hard-stop boundary

GL downstream pickup is external — signalled only by `GL-CARD-FEED-READY`/completion marker; the feed itself stays in `gl_posting` (analysis §7.2).

## 7. Acceptance criteria

- Balanced seeded set: every transaction gets debit+credit rows, flags set, RC 0, GLRPT totals match.
- Unmapped combination posts to suspense, RC 4, named on GLRPT.
- Forced imbalance: zero `gl_posting` rows, zero flags updated, exit 12 with U0704 (Phase 4 failure-path E2E, FR-016) — **target-only guarantee**; legacy leaves committed rows/flags behind when the feed exceeds the commit frequency (§3 FR-002 divergence, stream FR §5.3 item 17).
- Re-run after success feeds nothing (flags already 'Y') and ends clean — no RC 8 exists in the source (divergence §5).
- Malformed mapping configuration fails startup (U0701-equivalent), touching nothing.
