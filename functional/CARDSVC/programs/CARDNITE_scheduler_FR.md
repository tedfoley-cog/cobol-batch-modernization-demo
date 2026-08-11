# CARDNITE scheduler-table semantics (wave 6 unit)

Target: `backend/…/common/scheduling/` conventions + external scheduler seam (plan wave 6; §4.3 row 1).
Stream FRs owned: CARDNITE-FR-001 (entry), FR-002 (hold), FR-019 (completion conditions).

## 1. Trigger / contract

Scheduler table `CARDNITE` (`sched/CARDNITE.sched`):
- Ordered daily except Sunday: `CALENDAR=ALLDAY`, `NOT-CALENDAR=WEEKSUN`, order time 17:30 (`sched/CARDNITE.sched:17-27`).
- Entry gate: `CICS-CARD-CLOSED`, posted by region shutdown job CICSCARD (`sched:8-10,43`).
- Due-out: complete before online open 06:00 (`sched:397-400`).
- Completion: CBCRD10J posts `CARDNITE-COMPLETE` + `CICS-CARD-OPEN-OK` (releases CARDONLJ) and deletes all *-OK conditions (`sched:375-389`).

## 2. Requirements owned

**SCHED-FR-001 — Chain order.** CBCRD01J→02J→03J→04J → fork {05AJ ∥ 05BJ} → join 06J (AND, `sched:240-242`) → 07J→08J→09J→10J; fork legs share no resource (`sched:155-158`); CBCRD04J and CBCRD07J serialize on `CARDDB-POSTING QUANTITY=1` (`sched:130,280`).

**SCHED-FR-002 — Hold, never skip.** Any NOTOK places the cycle in `CARDNITE-HELD`; remaining jobs do not run; operators resume, never re-order (`sched:403-408`; CARDNITE-FR-002). **Exception:** a CBCRD09J failure still posts its successor so the close can run (`sched:350-357`).

**SCHED-FR-003 — OK thresholds per job.** Scheduler treats each job's RC per the estate convention (0/4 OK, 8+ NOTOK) with the job-specific meanings in each program FR doc.

## 3. Target mechanism

The enterprise scheduler seam stays external: whoever orders the jobs launches the CLI (`spring.batch.job.name=<job> cycleDate=…`) and reads exit codes (plan §4.3). Cycle-internal state (entry/complete/held) is modeled as `cycle_control` status transitions; the fork/join is protected application-side by the branch flags + CBCRD06W regardless of orchestrator. Phase 4 E2E runs the chain in scheduler order incl. the fork and the CBCRD09 exception.

## 4. Divergences owned here

- **Stale DESC lines** (`sched:230,270,301`): job descriptions contradict the source (CBCRD07 is GL feed, CBCRD08 delinquency, interest is in 05B) — retired; source/JCL authoritative (analysis §8 item 1).
- **ABEND-COND codes U4001–U4010** (`sched:55,87,147` etc.): match nothing the source raises (FR §5.3) — retired with the table; the migrated estate emits only source-derived abend codes via `ErrorReporter`.
- **FORCEOPEN operator note 4** (`sched:416-419`): resolved in `CBCRD10_FR.md` §5 (dropped; admin-CLI override instead).
- **Sunday exclusion**: `NOT-CALENDAR=WEEKSUN` — the replacement schedule must keep the six-day calendar.

## 5. Hard-stop boundary

CICSCARD, CARDONLJ, and the scheduler product itself are external; only the condition contract (consume closed, produce complete/open-OK) is in scope.

## 6. Acceptance criteria

- Phase 4 E2E executes the ten jobs in order with the fork/join and asserts: mid-chain failure stops downstream (except after CBCRD09), and the final state posts the completion equivalents (`cycle_control` complete + open flag).
- A held cycle can be resumed from the failed job without re-running completed jobs.
- The schedule definition (whatever orchestrator) encodes daily-except-Sunday and the 06:00 due-out as an alert.
