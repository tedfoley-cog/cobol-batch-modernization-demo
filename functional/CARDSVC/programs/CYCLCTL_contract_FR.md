# CYCLCTL — cycle control / checkpoint contract (shared component, wave 1)

Target component: cycle-control component in `backend/…/common/` over the `cycle_control` table (plan §5 D2, approved PostgreSQL data target).
Stream FRs carried: CARDNITE-FR-002 (hold state), FR-007 (restart/checkpoint), FR-010 (branch join flags), FR-018 (export/backup), FR-019 (online-closed flag).

## 1. Contract / callers

VSAM KSDS `CARD.PROD.CYCLCTL`, KEYS(16 0) RECORDSIZE(256 256) (`app/jcl/vsam/DEFCARD.jcl:132-141`), DD `CYCLCTL` in all 10 jobs. Record layout CYCLE-CTRL-RECORD (`app/cpy/CVCTRL01Y.cpy:6-41`):

| Field | Picture / meaning |
|---|---|
| CC-CYCLE-TYPE X(8) + CC-CYCLE-DATE X(8) | key — CARDNITE + CCYYMMDD |
| CC-STATUS X(1) | N new / R running / C complete / F failed / S suspended |
| CC-COMMIT-FREQ 9(4) | operator-tunable commit interval; 0 ⇒ program constant `WS-COMMIT-FREQUENCY`=1000 (`app/cpy/CVCONSTY.cpy:37`, `app/cardsvc/cbl/CBCRD04.cbl:334-335`) |
| CC-RECS-READ / -WRITTEN / -REJECTED 9(9) | counters |
| CC-LAST-KEY X(32) | checkpoint restart key |
| CC-RESTART-CNT 9(2) | restarts this cycle; runbook cap ≤ 3 (`docs/runbook-cardnite.md:114-136`) |
| CC-TOT-DR / CC-TOT-CR COMP-3 | control totals (`CVCTRL01Y.cpy:36-39`) |
| CC-ONLINE-CLOSED-FLG X(1) | region gate flag set by CBCRD10 (`CVCTRL01Y.cpy:40`, `CBCRD10.cbl:713-717`) |
| CC-FILLER X(30) | **undocumented overlay** (`CVCTRL01Y.cpy:41`): (1:1) fee branch 'A'/'F' (`CBCRD05A.cbl:729-733`), (2:1) interest branch 'B'/'F' (`CBCRD05B.cbl:960-962`), (3:8) cycle id (`CBCRD05A.cbl:733`); read by CBCRD06W (`CBCRD06W.cbl:179-180`) |

## 2. Requirements owned

**CYCLCTL-FR-001 — One control record per cycle is the single source of cycle state.** Opened/initialized by CBCRD01 (`CBCRD01.cbl:305-312`), read by every job, checkpointed by posting/fee/interest/risk steps, closed by CBCRD10. A job that cannot read its control record abends (U0101/U0201/U0401/U0501/U0601 per owning program docs).

**CYCLCTL-FR-002 — Checkpoint protocol.** Checkpointing programs rewrite CC-LAST-KEY + counters every CC-COMMIT-FREQ units of work in the same commit as the business writes (`CBCRD04.cbl:901-925`); restart resumes after CC-LAST-KEY (CARDNITE-FR-007).

**CYCLCTL-FR-003 — Branch join flags.** The 05A/05B fork completion protocol lives in the CC-FILLER overlay bytes; CBCRD06W trusts only these flags, not the scheduler (`app/jcl/cardsvc/CBCRD06J.jcl:10-14`).

## 3. Target mechanism (approved D2)

`cycle_control` table: PK (cycle_type, cycle_date); checkpoint block as columns; **overlay promoted to named columns** `fee_branch_status`, `interest_branch_status`, `branch_cycle_id` — a deliberate divergence from byte-compat, approved at STOP 2 (plan §5 D2). Chunk size for tunable-interval jobs is bound to `commit_freq` on the row, not hardcoded (target design "Persistence" row). Restart-count guard (≤3) enforced in the component (plan Phase 5).

## 4. Error / edge behavior

- Missing/unreadable control record is fatal to every job (per-program U0x01 codes) — the component throws to `ErrorReporter`.
- CC-COMMIT-FREQ = 0 falls back to the program constant 1000; negative/non-numeric is a data error (legacy would abend on the PIC).
- Checkpoint update and business writes must be atomic — a crash between them may not lose or double work (CARDNITE-FR-006/007).
- Status transitions: N→R→C on success; R→F on failure; S per operator suspend. Same-cycle re-open with status C is an operator error the component must reject (legacy CBCRD01 zeroes counters unconditionally — target adds an explicit guard; recorded divergence, resolve in wave 3 CBCRD01 doc).

## 5. Hard-stop boundary

None — internal state. The scheduler conditions built on top of cycle state are covered by `CARDNITE_scheduler_FR.md`.

## 6. Acceptance criteria

- A killed-and-restarted CBCRD04 run resumes after `cc_last_key` and posts each authorization exactly once (FR-006/007 E2E, plan Phase 4).
- Branch flags written by 05A/05B are visible to 06W within the same cycle row; a failed flag holds the join.
- `commit_freq` change on the row changes the observed chunk/commit size on the next run without redeploy.
- Restart count > 3 blocks automatic restart with an operator-facing message.
- Flyway migration keys `cycle_control` on (cycle_type, cycle_date) and carries the three promoted overlay columns.
