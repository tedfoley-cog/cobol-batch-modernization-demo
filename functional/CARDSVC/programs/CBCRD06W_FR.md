# CBCRD06W — branch join guard (CBCRD06J STEP005, wave 5)

Target: first step of Spring Batch job `cbcrd06` in `backend/…/jobs/cbcrd06/`.
Stream FRs owned: CARDNITE-FR-010 (branch join gate).

## 1. Trigger / caller contract

CBCRD06J triggers on the scheduler AND-join of CBCRD05AJ-OK and CBCRD05BJ-OK (`sched/CARDNITE.sched:240-242`), but STEP005 re-verifies from the cycle-control record and **does not trust the scheduler** (`app/jcl/cardsvc/CBCRD06J.jcl:10-14`). STEP010 is gated `IF STEP005.RC = 0` (`CBCRD06J.jcl:61`).

PARM: `'&CYCDATE,WAIT=NNN'` — NNN one-minute polls, standing value 030 (`CBCRD06J.jcl:27-29,50-51`); parsed at `app/cardsvc/cbl/CBCRD06W.cbl:137-141`.
*Target CLI:* `cycleDate=…, waitLimit=…`.

## 2. Field-level inputs / outputs

- In: CYCLCTL only (`CBCRD06J.jcl:55`) — reads the CARDNITE control record's branch flags: `CC-FILLER(1:1)` fee 'A'/'F', `CC-FILLER(2:1)` interest 'B'/'F' (`CBCRD06W.cbl:179-180`; overlay per CYCLCTL_contract_FR).
- Out: RC only; no files, no Db2.

## 3. Requirements owned

**CBCRD06W-FR-001 — Application-level join.** RC 0 only when both branch flags read complete (`CBCRD06W.cbl:248`); the exposure chain never runs on a scheduler race or a failed branch (CARDNITE-FR-010).

**CBCRD06W-FR-002 — Bounded wait.** When a flag is missing it polls once a minute up to the WAIT limit (legacy delay is a CPU-spin loop, `CBCRD06W.cbl:205-215`, `docs/runbook-cardnite.md:44-47`), then fails distinguishing which branch: U0610 fee not complete / U0611 interest not complete / U0612 a branch posted 'F'; U0601 = CYCLCTL unusable (`CBCRD06W.cbl:28-33,216-243`).

## 4. Target mechanism

Tasklet step polling `cycle_control.fee_branch_status` / `interest_branch_status` with a real sleep (replacing the CPU spin — behavioral equivalence, resource fix). WAIT limit from the `waitLimit` parameter; failure exit distinguishes the three legacy outcomes in the structured error record.

## 5. Error / edge behavior and RC mapping

| Outcome | Legacy | Target |
|---|---|---|
| both flags complete | RC 0 | step COMPLETED |
| fee branch not complete at limit | U0610, RC 12 | exit 12, code U0610 logged |
| interest branch not complete at limit | U0611 | exit 12, U0611 |
| a branch posted 'F' — fail immediately, no wait | U0612 (intended; see caveat below) | exit 12, U0612 |
| control record unusable | U0601 | exit 12, U0601 |

**Source caveat (failed-flag path):** the intended immediate U0612 relies on an ill-defined construct — `2000-CHECK-BRANCHES` handles the failed flag with `GO TO 2000-EXIT` **without** incrementing `WS-POLL-CNT` (`CBCRD06W.cbl:187-189`), but `2000-EXIT` is a separate paragraph outside the PERFORM range (no `THRU`, `CBCRD06W.cbl:114-116,199-201`), so behavior depends on the compiler/runtime's handling of a GO TO out of a performed paragraph: fall-through reaches `3000-TERMINATE` (U0612), while a return to the loop would spin re-reading CYCLCTL forever since the counter never advances. The legacy outcome cannot be confirmed from the repo; the U0612 abend and runbook table indicate immediate failure is the intent. **Target:** deterministic immediate exit 12 / U0612 on a failed flag — recorded as intent-preserving; if the legacy runtime in fact hangs, this is a (desirable) divergence to note at parity sign-off. Note also that under legacy neither branch ever posts 'F' (CBCRD05A's fatal path GOBACKs before the flag post, CBCRD05B abends without posting — stream FR §5.3 items 11/15), so the legacy failed-branch signal is in practice the timeout (U0610/U0611), not U0612.

**Divergence reconciliation (FR §5.3 — U0602/U0603):** the JCL comment and runbook label the join failures U0602/U0603 (`CBCRD06J.jcl:32-34`, `docs/runbook-cardnite.md:101-102`) but the source raises only U0601/U0610/U0611/U0612 (`CBCRD06W.cbl:28-33`). Resolution: **source codes govern**; runbook/JCL labels retired at cutover.

**Divergence reconciliation (FR §5.3 — WAIT):** the JCL passes `WAIT=030` (`CBCRD06J.jcl:51`) while the program header claims the standard schedule passes WAIT=000 (`CBCRD06W.cbl:22-24`). Resolution: **JCL authoritative** — the migrated standing schedule passes waitLimit=30.

## 6. Hard-stop boundary

None.

## 7. Acceptance criteria

- Both flags set ⇒ proceeds immediately.
- One flag missing, appearing during the window ⇒ proceeds on the poll that sees it.
- Flag never appearing ⇒ fails with the branch-specific code after waitLimit+1 checks/reads and waitLimit delays — legacy loops `UNTIL … WS-POLL-CNT > WS-WAIT-LIMIT` with the counter incremented after a failed check (`CBCRD06W.cbl:113-116,194-201`), so WAIT=030 means 31 reads / 30 one-minute delays; the target reproduces this window.
- Either flag 'F' ⇒ fails immediately (no polling) with U0612.
- A branch flag from a different `branch_cycle_id` is treated as missing (**stale-cycle guard — target-only improvement**: legacy CBCRD06W reads only the flag bytes and never inspects the cycle id, `CBCRD06W.cbl:179-186`; depends on both 05A and 05B populating `branch_cycle_id` in the target — see CBCRD05B_FR §7 and CYCLCTL_contract_FR §3).
