# CBCRD06X — work-list sort and per-party summarisation (CBCRD06J STEP020, wave 5)

Target: step of Spring Batch job `cbcrd06`.
Stream FRs owned: CARDNITE-FR-011 (work-list construction — party summarisation half).

## 1. Trigger / caller contract

Gated `IF RC <= 4` after STEP010 (`app/jcl/cardsvc/CBCRD06J.jcl:77-90`). PARM: cycle date.

## 2. Field-level inputs / outputs

- In: `PARTYWK(0)` FB 150 (`CBCRD06J.jcl:82-90`; SELECTs `app/cardsvc/cbl/CBCRD06X.cbl:44-52`).
- Out: `CARD.PROD.PARTYSRT(+1)` FB 150 — internal COBOL SORT with SORTWK DDs, then a per-party control break; an internal SORT rather than a DFSORT step because the band-precedence summarisation cannot be expressed in sort control cards (`CBCRD06X.cbl:15-17`).
- No Db2.

## 3. Requirements owned

**CBCRD06X-FR-001 — One summarised record per party.** PARTYSRT carries exactly one record per party: the input is sorted on (SR-PARTY-ID, SR-ACCT-ID) but the output procedure breaks on `PW-PARTY-ID` alone, **summing** `PW-POSTED-AMT`, `PW-TXN-CNT`, `PW-CURR-BAL`, `PW-CREDIT-LIMIT` and `PW-EXPOSURE-AMT` across the party's accounts into one group record (`2300-ACCUMULATE`, `CBCRD06X.cbl:178-245`) — so CBCRD06B dispatches the risk crossing once per party with the party's **total** exposure (CARDNITE-FR-011). The group record starts as a copy of the group's **first record in sort order** (`2200-START-GROUP`, `CBCRD06X.cbl:221-228`), so the lowest account number in the group is kept as the representative account — deliberately, "so CBCRD06C has something to update against" (`CBCRD06X.cbl:11-13`) — and the first record's other non-summed fields carry forward with it.

**CBCRD06X-FR-002 — Worst risk band carried forward.** The group record carries the worst risk band across the party's accounts, precedence X > C > B > A with unknown bands ranking lowest (`2500-RANK-BAND`, `CBCRD06X.cbl:265-283`); per the 2006 maintenance note, "worst band in the group carried forward instead of the first one read".

## 4. Target mechanism

Sort by (partyId, acctId), then a per-party aggregation (group-by partyId): sum the five money/count fields, carry the worst band per the X > C > B > A precedence, and keep the group's lowest account id (first record in sort order) as the representative key plus its non-summed fields — not a distinct/keep-first step for the amounts, but deterministic keep-first for the representative account.

## 5. Error / edge behavior and RC mapping

- RC 0 normal / RC 4 nothing to dispatch (`CBCRD06X.cbl:26,293-295`); abends U0602 I/O, U0604 sort failed (`CBCRD06X.cbl:28-29`).
- Empty input ⇒ empty output, RC 4 (`IF WS-OUT-CNT = ZERO ⇒ WS-RC-WARNING`, `CBCRD06X.cbl:293-295`) — STEP030 still runs (gated `RC <= 4`).

## 6. Hard-stop boundary

None.

## 7. Acceptance criteria

- Exactly one output record per partyId, in party order; the record's acctId is the party's lowest account number and its non-summed fields come from that first record (deterministic representative for CBCRD06C).
- Multi-account party: posted amount, txn count, current balance, credit limit and exposure are the sums across its accounts; risk band is the worst by X > C > B > A (unknown ranks below A).
- Empty input ⇒ RC 4 warning, empty output.
- Byte-compatible FB 150 CVPWRK01Y records, outcome block untouched.
