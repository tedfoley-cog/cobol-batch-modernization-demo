# CBCRD05A — fee assessment branch (CBCRD05AJ, wave 4)

Target: Spring Batch job `cbcrd05a` in `backend/…/jobs/cbcrd05a/` using the wave-1 router and wave-2 fee handlers.
Stream FRs owned: CARDNITE-FR-008 (fee assessment), FR-010 (branch flag).

## 1. Trigger / caller contract

Forked leg: triggers on CBCRD04J-OK, concurrent with CBCRD05BJ, no shared resource (`sched/CARDNITE.sched:155-189`). CBCRD05AJ STEP010, STEPLIB concatenates `CARD.PROD.FEELIB` (fee handlers) — a missing handler surfaces as a route failure, not a link-edit error (`app/jcl/cardsvc/CBCRD05AJ.jcl:13-19,36-48`). Built DYNAM (`app/jcl/build/BUILDALL.jcl:192`).

PARM: cycle date only (`CBCRD05AJ.jcl:47`).

## 2. Field-level inputs / outputs

- In: CYCLCTL (`CBCRD05AJ.jcl:50`).
- Out: fee audit trail `CARD.PROD.FEEAUDIT(+1)` LRECL 133 (`CBCRD05AJ.jcl:51-55`), printed when RC≤8 (`CBCRD05AJ.jcl:62-67`).
- Db2: cursors on `CARDSVC.ACCOUNT`/`TRANSACTION` and `FEE_SCHEDULE` (`app/cardsvc/cbl/CBCRD05A.cbl:243-262`), `CARD_LIMIT`+`CARD` join (`CBCRD05A.cbl:394-395`), INSERT `TRANSACTION` / UPDATE `ACCOUNT` (`CBCRD05A.cbl:601,647`) — fee writes are done by the handlers; 05A owns the unit of work.
- Dispatch: per applicable fee type fills FEE-WORK-AREA (FR-CALLER-ID 'CBCRD05A', `CBCRD05A.cbl:138,455`) and calls CBCRD90 with route FEEC (`CBCRD05A.cbl:495-508`).

## 3. Requirements owned

**CBCRD05A-FR-001 — Fee applicability scan.** Determines per account which fee types (ANNU/LATE/OVLM/CASH/FRGN) apply this cycle and dispatches each to its routed handler (CARDNITE-FR-008).

**CBCRD05A-FR-002 — Fee audit trail.** Every dispatch outcome (assessed, waived, declined) is a FEEAUDIT line.

**CBCRD05A-FR-003 — Branch completion flag.** As its last act posts `CC-FILLER(1:1)` = 'A' complete / 'F' failed plus the cycle id at `(3:8)` (`CBCRD05A.cbl:710-747`) — the join contract with CBCRD06W (CARDNITE-FR-010). Target: `cycle_control.fee_branch_status` / `branch_cycle_id` (D2).

## 4. Target mechanism

Chunk step over accounts; per fee type calls `ProgramRouter` → fee handler bean; commits every `WS-COMMIT-FREQUENCY` units (`CBCRD05A.cbl:378`). FEEAUDIT as a 133-col layout-preserving report file (D6).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | all assessed |
| 4 | skips, handler warning, or unroutable/declined fee |
| 12 | fatal — U0501/U0503/U0505 (source); no flag posted |

Abends (source, `CBCRD05A.cbl:33-35`): U0501 CYCLCTL unusable, U0503 SQL (`CBCRD05A.cbl:332`), U0505 fatal handler RC (`CBCRD05A.cbl:531-536`). **Divergence:** the JCL comment labels the fatal path U0502 (`CBCRD05AJ.jcl:21-26`) — the source raises no 0502; source codes govern, JCL label retired.

**Divergence reconciliation (FR §5.3 — unroutable fee):** the JCL claims "8 fee type not routable (flag F, join held)" (`CBCRD05AJ.jcl:21-26`), but the source handles dispatcher 'ROUTE NOT FOUND' (RC 8, `app/cardsvc/cbl/CBCRD90.cbl:115-120`) as a **declined fee — warning RC 4, flag 'A', join proceeds** (`CBCRD05A.cbl:522-530,728-732`). Resolution: **source behavior governs** (an unroutable fee type is a configuration gap, not a cycle-stopper), with a mandatory structured warning naming the unroutable fee type so operations sees the gap; the JCL comment is retired.

**Divergence reconciliation (FR §5.3 — re-run deletion):** the JCL restart comment claims cycle-date fee rows are deleted before starting (`CBCRD05AJ.jcl:28-31`) but no DELETE exists in the source. Resolution: the target implements **explicit same-cycle idempotency** — fee inserts carry the cycle date and an existence check (as CBFEE02 already does for over-limit) so a re-run never double-charges; verified by a re-run test.

## 6. Hard-stop boundary

Dispatches only FEEC routes — never XMOD. The fee handlers are in-module (wave 2).

## 7. Acceptance criteria

- Account matrix covering each fee type dispatches to the right handler with a correctly-populated work area (callerId 'CBCRD05A').
- Unroutable fee type: job exits 4, flag 'A', warning logged with the fee type; join proceeds.
- Handler RC 12 rolls back the current chunk and fails the job (flag 'F').
- Re-running the job over the same cycle assesses no duplicate fees.
- On success `fee_branch_status='A'` and `branch_cycle_id` set; on failure `'F'` — both visible to CBCRD06W.
