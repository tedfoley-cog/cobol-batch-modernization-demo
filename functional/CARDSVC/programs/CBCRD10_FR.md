# CBCRD10 — cycle close, control report, online-region gate (CBCRD10J, wave 6)

Target: Spring Batch job `cbcrd10` in `backend/…/jobs/cbcrd10/`.
Stream FRs owned: CARDNITE-FR-019 (close/open gate), plus the FR-001/002 completion conditions.

## 1. Trigger / caller contract

Last job; on OK posts `CARDNITE-COMPLETE` + `CICS-CARD-OPEN-OK` (releasing region start job CARDONLJ) and deletes all *-OK conditions (`sched/CARDNITE.sched:375-389`; `app/jcl/cardsvc/CBCRD10J.jcl:14-18`).
PARM: cycle date only (`CBCRD10J.jcl:40`) — parsed as `LK-PARM-DATA(1:8)` (`app/cardsvc/cbl/CBCRD10.cbl:325-331`).
STEP020 keeps the control report as a disk GDG `CARD.PROD.CTLRPT(+1)` when RC≤8 (`CBCRD10J.jcl:50-62`).

## 2. Field-level inputs / outputs

- Files: CYCLCTL; stage statistics `CARD.PROD.STGSTAT` QSAM LRECL 120 (`CBCRD10.cbl:29,60-80`) — producer external and never found in the repo (analysis §8; plan §4.3: synthesize from Spring Batch step statistics); control report CTLRPT SYSOUT FBA 133 (`CBCRD10J.jcl:45-46`).
- Db2 (read-only): `TRANSACTION` counts (`CBCRD10.cbl:527,543`), `GL_POSTING` totals (`CBCRD10.cbl:566`), `ACCOUNT` delinquency count (`CBCRD10.cbl:594`).

## 3. Requirements owned

**CBCRD10-FR-001 — Close proof.** Verifies every stage ran and the ledger is proven (counts/totals cross-checked against cycle-control totals and stage statistics) before the cycle may close (CARDNITE-FR-019).

**CBCRD10-FR-002 — Region gate.** The open/hold decision is recorded in **both** the step RC and `CC-ONLINE-CLOSED-FLG` (`CBCRD10.cbl:9,713-717`); RC table at `docs/runbook-cardnite.md:172-177`. RC 8 = a stage did not run / ledger unproven — region held (`CBCRD10J.jcl:24-26`).

**CBCRD10-FR-003 — Control report.** CTLRPT summarizes the cycle (stage counts, totals, delinquency, decision) and is retained as a generation.

## 4. Target mechanism

Tasklet/report step: repository count/total queries + `cycle_control` totals + step statistics (synthesized STGSTAT); decision → exit code via `ExitCodeGenerator` + `cycle_control.online_closed_flg`; report file per D6. Completion conditions map to `cycle_control` status transitions + CLI exit codes; the enterprise scheduler seam stays external (plan §4.3).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | clean — region may open |
| 4 | exceptions noted, region opens |
| 8 | stage missing / ledger unproven — **region held** |
| 12 | U1002 file, U1003 SQL |

(`CBCRD10J.jcl:20-27`.)

**Divergence reconciliation (FR §5.3 — FORCEOPEN):** scheduler operator note 4 documents a `FORCEOPEN` PARM (`sched/CARDNITE.sched:416-419`), but CBCRD10 parses only the 8-byte cycle date and never reads a FORCEOPEN token (`CBCRD10.cbl:325-331`; no reference anywhere in the program). Resolution: **FORCEOPEN is DROPPED** — it was never implemented, so migrating it would invent behavior. The operational need (open the region despite a held close) is met outside the job: an operator flips `cycle_control.online_closed_flg` via the Phase 5 admin CLI, leaving an audit trail. Recorded as the resolution required by the target design Scheduling seam row.

## 6. Hard-stop boundary

Region start (CARDONLJ) and the condition plumbing are external — the job's contract is its exit code + `online_closed_flg` (plan §4.3).

## 7. Acceptance criteria

- Complete, balanced cycle exits 0 and sets the open flag; a missing stage or unproven ledger exits 8 and holds.
- CTLRPT reproduces the legacy sections (stage counts, DR/CR proof, delinquency summary) per cycle date.
- No `forceOpen` job parameter exists; the admin-CLI override path is documented and audited.
- Exit code and `online_closed_flg` never disagree.
