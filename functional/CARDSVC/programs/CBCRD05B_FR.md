# CBCRD05B — interest and rewards branch (CBCRD05BJ, wave 4)

Target: Spring Batch job `cbcrd05b` in `backend/…/jobs/cbcrd05b/`.
Stream FRs owned: CARDNITE-FR-009 (interest/rewards), FR-010 (branch flag).

## 1. Trigger / caller contract

Forked leg: triggers on CBCRD04J-OK, concurrent with CBCRD05AJ (`sched/CARDNITE.sched:192-221`). CBCRD05BJ STEP010 (`app/jcl/cardsvc/CBCRD05BJ.jcl:32-40`).

PARM: cycle date only (`CBCRD05BJ.jcl:40`).
SYSIN control cards: rounding convention `HALFUP`, agreed with Finance/Audit, change-record controlled — a business parameter on cards, not in the program (`CBCRD05BJ.jcl:14-18,53-59`).

## 2. Field-level inputs / outputs

- In: CYCLCTL (`CBCRD05BJ.jcl:43`); SYSIN cards.
- Out: interest report INTRPT SYSOUT FBA 133 (`CBCRD05BJ.jcl:44-45`).
- Db2: cursor/UPDATE `CARDSVC.ACCOUNT` (`app/cardsvc/cbl/CBCRD05B.cbl:251,723`), SELECT `CARD_LIMIT`+`CARD` (`CBCRD05B.cbl:487-488`), SELECT/INSERT `TRANSACTION` (`CBCRD05B.cbl:544,676`), INSERT/UPDATE `CARDSVC.REWARDS` (`CBCRD05B.cbl:798-852`).

## 3. Requirements owned

**CBCRD05B-FR-001 — Interest accrual.** Average-daily-balance interest at the account APR, cash APR computed separately (`app/jcl/cardsvc/CBCRD05BJ.jcl:5-12`; CARDNITE-FR-009).

**CBCRD05B-FR-002 — Configured rounding only.** Rounding follows the SYSIN convention (HALFUP standing value); an unsupported convention is a control-card error — RC 8, flag 'F' (U0506 path, `CBCRD05B.cbl:388-395`).

**CBCRD05B-FR-003 — Rewards accrual.** Rewards inserted/updated per account in the same pass (`CBCRD05B.cbl:798-852`).

**CBCRD05B-FR-004 — Branch completion flag.** Posts `CC-FILLER(2:1)` = 'B' complete / 'F' failed as its last act (`CBCRD05B.cbl:943-977`) — join contract with CBCRD06W (CARDNITE-FR-010). Target: `cycle_control.interest_branch_status` (D2).

## 4. Target mechanism

Chunk step over accounts; commits every `WS-COMMIT-FREQUENCY` (`CBCRD05B.cbl:467`); rounding convention becomes validated configuration (Finance-owned); `BigDecimal` with the convention applied exactly once per amount, mirroring the card semantics. INTRPT as a layout-preserving report file (D6).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | complete |
| 4 | accounts skipped, no APR on file |
| 8 | control card error or unsupported rounding (flag posted 'F') |
| 12 | U0512 — no flag posted |

(`CBCRD05BJ.jcl:20-25`.) Also U0506 rounding/control-card abend path (`CBCRD05B.cbl:388-395`), U0503 SQL, U0501 CYCLCTL.

**Divergence reconciliation (FR §5.3 — re-run deletion):** the JCL claims interest rows for the cycle date are deleted before accruing (`CBCRD05BJ.jcl:27-30`) but no DELETE exists in the source. Resolution: as for CBCRD05A — the target implements explicit same-cycle idempotency (cycle-dated interest/rewards transactions with an existence check), proven by a re-run test; legacy re-run behavior is recorded as unverified.

## 6. Hard-stop boundary

None — no dispatching, no external calls.

## 7. Acceptance criteria

- ADB interest at account APR and separate cash-APR accrual verified against hand-computed golden accounts.
- Account without an APR is skipped, counted, job exits 4.
- Unsupported rounding convention: nothing accrued, RC 8, `interest_branch_status='F'`, join holds.
- Re-run over the same cycle accrues no duplicate interest or rewards.
- On success `interest_branch_status='B'` visible to CBCRD06W.
