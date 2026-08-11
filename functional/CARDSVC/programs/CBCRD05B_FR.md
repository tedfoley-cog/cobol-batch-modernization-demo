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

**CBCRD05B-FR-002 — Configured rounding only.** Rounding follows the SYSIN convention (HALFUP standing value); an unsupported convention or missing cycle-date card is fatal, never a silent default (U0506, `CBCRD05B.cbl:20-23,384-397`).

**CBCRD05B-FR-003 — Rewards accrual.** Rewards inserted/updated per account in the same pass (`CBCRD05B.cbl:798-852`).

**CBCRD05B-FR-004 — Branch completion flag.** Posts `CC-FILLER(2:1)` = 'B' complete / 'F' failed as its last act (`CBCRD05B.cbl:943-977`) — join contract with CBCRD06W (CARDNITE-FR-010). Target: `cycle_control.interest_branch_status` (D2).

## 4. Target mechanism

Chunk step over accounts; commits every `WS-COMMIT-FREQUENCY` (`CBCRD05B.cbl:467`); rounding convention becomes validated configuration (Finance-owned); `BigDecimal` with the convention applied exactly once per amount, mirroring the card semantics. INTRPT as a layout-preserving report file (D6).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | complete |
| 4 | accounts skipped, no APR on file |
| 12 | fatal — U0501 CYCLCTL, U0503 SQL, U0506 control-card/rounding error (`CBCRD05B.cbl:41-44`); no flag posted |

**Divergence reconciliation (JCL-vs-source — control-card failure):** the JCL comment claims a control-card error ends "RC 8, flag 'F'" (`CBCRD05BJ.jcl:20-25`), but the source validates the convention in initialization (`1200-VALIDATE-CONVENTION`, `CBCRD05B.cbl:384-397`) and abends U0506 through `9500-FATAL-ERROR` — RC 12, rollback, **no branch flag posted** (`CBCRD05B.cbl:991-1003`); legacy CBCRD06W then waits out its poll window and fails U0611, not the immediate U0612 an 'F' flag would give. Resolution: **source behavior governs at parity** — configuration validation failure exits 12 with `interest_branch_status` untouched; the target validates configuration at job startup so this fires before any accrual, and the join step's timeout (not a fail-fast flag) covers this case. JCL comment retired. The JCL's U0512 label is likewise not raised by the source (only 0501/0503/0506, `CBCRD05B.cbl:41-44`) — source codes govern.

**Divergence reconciliation (FR §5.3 — re-run deletion):** the JCL claims interest rows for the cycle date are deleted before accruing (`CBCRD05BJ.jcl:27-30`) but no DELETE exists in the source. Resolution: as for CBCRD05A — the target implements explicit same-cycle idempotency (cycle-dated interest/rewards transactions with an existence check), proven by a re-run test; legacy re-run behavior is recorded as unverified.

## 6. Hard-stop boundary

None — no dispatching, no external calls.

## 7. Acceptance criteria

- ADB interest at account APR and separate cash-APR accrual verified against hand-computed golden accounts.
- Account without an APR is skipped, counted, job exits 4.
- Unsupported rounding convention: nothing accrued, exit 12 (U0506-equivalent), `interest_branch_status` untouched; the join step subsequently times out (U0611 path).
- Re-run over the same cycle accrues no duplicate interest or rewards.
- On success `interest_branch_status='B'` **and `branch_cycle_id`** visible to CBCRD06W — legacy 05B writes no cycle id (`CBCRD05B.cbl:947-978`); the target must populate it so the join's stale-cycle guard (CBCRD06W_FR §7) doesn't read a blank id and hold the join (recorded improvement).
