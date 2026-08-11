# CBCRD06X — work-list sort and de-duplicate (CBCRD06J STEP020, wave 5)

Target: step of Spring Batch job `cbcrd06`.
Stream FRs owned: CARDNITE-FR-011 (work-list construction — uniqueness half).

## 1. Trigger / caller contract

Gated `IF RC <= 4` after STEP010 (`app/jcl/cardsvc/CBCRD06J.jcl:77-90`). PARM: cycle date.

## 2. Field-level inputs / outputs

- In: `PARTYWK(0)` FB 150 (`CBCRD06J.jcl:82-90`; SELECTs `app/cardsvc/cbl/CBCRD06X.cbl:44-52`).
- Out: `CARD.PROD.PARTYSRT(+1)` FB 150 — internal COBOL SORT with SORTWK DDs, then de-duplication.
- No Db2.

## 3. Requirements owned

**CBCRD06X-FR-001 — Ordered, unique dispatch list.** PARTYSRT is sorted on the PW key (party id + acct id) and de-duplicated so CBCRD06B dispatches each party/account exactly once — the risk crossing must never be driven twice for the same party in a cycle (CARDNITE-FR-011; cost/idempotency of the crossing).

## 4. Target mechanism

Sort+distinct step (in-memory or Spring Batch sort by (partyId, acctId)); duplicates collapse keeping the first record (legacy internal-SORT semantics — confirm keep-first vs merge during implementation against `CBCRD06X.cbl`'s de-dup paragraph and record the choice in the wave PR).

## 5. Error / edge behavior and RC mapping

- RC 0 normal; abends U0602 I/O, U0604 (`CBCRD06X.cbl:166,186`).
- Empty input ⇒ empty output, RC 0.

## 6. Hard-stop boundary

None.

## 7. Acceptance criteria

- Output strictly ordered by (partyId, acctId) with no duplicate keys.
- Duplicate-heavy input collapses deterministically (documented keep rule).
- Byte-compatible FB 150 CVPWRK01Y records, outcome block untouched.
