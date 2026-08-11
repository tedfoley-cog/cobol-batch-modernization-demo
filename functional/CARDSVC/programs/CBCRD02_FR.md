# CBCRD02 — extract edit and reject split (CBCRD02J, wave 3)

Target: Spring Batch job `cbcrd02` in `backend/…/jobs/cbcrd02/`.
Stream FRs owned: CARDNITE-FR-004 (validation and reject tolerance).

## 1. Trigger / caller contract

Runs on CBCRD01J-OK. CBCRD02J STEP010 `PGM=CBCRD02` (`app/jcl/cardsvc/CBCRD02J.jcl:34`); reject listing printed when RC 4/8 (`CBCRD02J.jcl:62-67`).

PARM: `'&CYCDATE,&TOLER'` — TOLER = reject tolerance as whole percent of records read, standing value 02, duty-manager-controlled (`CBCRD02J.jcl:13-18,34-35`); parsed as `LK-PARM-DATA(1:8)` date, `(10:2)` tolerance (`app/cardsvc/cbl/CBCRD02.cbl:240-241`).
*Target CLI:* `cycleDate=…, tolerancePct=…` (target design Scheduling seam row).

## 2. Field-level inputs / outputs

- In: `CARD.PROD.AUTHEXTR(0)` FB 300 (`CBCRD02J.jcl:39`), CYCLCTL (`CBCRD02J.jcl:40`).
- Out: `CARD.PROD.AUTHCLN(+1)` and `CARD.PROD.AUTHREJ(+1)`, both FB 300 (`CBCRD02J.jcl:41-50`). Clean records carry AX-EDIT status C/W; rejects carry status R + reason (`app/cardsvc/cpy/CVAXTR01Y.cpy:23-28`) and reject reason codes R001–R010 (`app/cardsvc/cpy/CVRJCT01Y.cpy:13-22`).
- No Db2.

## 3. Requirements owned

**CBCRD02-FR-001 — Per-record edit.** Decodes the 60-byte auth-detail overlay by AUTH-TYPE and range-checks each variant (`CBCRD02J.jcl:8-11`); each record ends clean (C), warning (W) or rejected (R+reason).

**CBCRD02-FR-002 — Tolerance gate.** Rejects as a whole-percent share of records read are compared against TOLER; within tolerance the cycle continues at RC 4, over tolerance ends RC 8 and the cycle is held (`CBCRD02J.jcl:19-25`, `docs/runbook-cardnite.md:76-77`). Cross-ref CARDNITE-FR-004.

## 4. Target mechanism

Chunk step: `FlatFileItemReader` (CVAXTR01Y layout) → classifier writer to clean/reject files per D5 convention. Tolerance computed at step end from counters; validation rules are the R001–R010 catalogue (each a testable rule class).

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | no rejects |
| 4 | rejects within tolerance |
| 8 | rejects over tolerance — cycle held |
| 12 | fatal |

Abends: U0201 control unusable (`CBCRD02.cbl:29,237`), U0202 extract file failure (`CBCRD02.cbl:261`).

**Divergence reconciliation (FR §5.3 — CBCRD02 RC 4):** the scheduler comments call RC 4 `EMPTY` and post the successor with an operator notify on a zero-record outcome (`sched/CARDNITE.sched:75-79`), while the source's RC 4 means "rejects within tolerance" (`CBCRD02.cbl:25-28`). Resolution: **source semantics govern** — RC 4 = within-tolerance warning. The zero-record case cannot reach CBCRD02 in practice (CBCRD01 exits 8 on an empty extract and holds the cycle — `CBCRD01J.jcl:15-19`); the migrated job still logs an explicit `records_read=0` warning if it ever occurs. The stale scheduler note is retired with the scheduler table.

## 6. Hard-stop boundary

None.

## 7. Acceptance criteria

- Each R001–R010 rule fires on a crafted bad record and routes it to the reject file with the correct reason code; clean records pass byte-unchanged except the edit status.
- Reject share is compared as a whole percent rounded to the nearest integer (`CBCRD02.cbl:584-590`): rounded share ≤ tolerance ⇒ RC 4; rounded share > tolerance ⇒ RC 8 and downstream does not run (boundary tests must use the rounded value, not a single-record delta).
- Tolerance is a required job parameter; a non-numeric value fails startup, not mid-file.
- clean+reject record counts equal records read; counters written to `cycle_control`.
