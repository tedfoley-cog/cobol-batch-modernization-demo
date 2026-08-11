# CBCRD04 — posting engine (CBCRD04J, wave 4)

Target: Spring Batch job `cbcrd04` in `backend/…/jobs/cbcrd04/`.
Stream FRs owned: CARDNITE-FR-006 (exactly-once posting), FR-007 (restart/checkpoint).

## 1. Trigger / caller contract

Runs on CBCRD03J-OK; holds serialized resource `CARDDB-POSTING QUANTITY=1` shared with CBCRD07J (`sched/CARDNITE.sched:130,280`). CBCRD04J STEP010 via CARDDB2 proc, plan CARDNITP (`app/jcl/cardsvc/CBCRD04J.jcl:41-47`).

PARM: `CCYYMMDD[,RESTART=Y|N]` (`CBCRD04J.jcl:14-17`; contract `app/cardsvc/cbl/CBCRD04.cbl:29`).
*Target CLI:* `cycleDate=…, restart=Y|N` — safety-critical cold/warm switch (target design Scheduling seam row).

## 2. Field-level inputs / outputs

- In: `AUTHENR(0)` FB 300, CYCLCTL (`CBCRD04J.jcl:48-49`).
- Out: bypass file `CARD.PROD.POSTREJ(+1)` FB 300 on DD AUTHREJ (`CBCRD04J.jcl:50-54`); bypass listing printed when RC≤4 (`CBCRD04J.jcl:62-67`).
- Db2: INSERT `CARDSVC.TRANSACTION` incl. the variable leg array (CVTRAN01Y, `CBCRD04.cbl:194,730`; `CBCRD04J.jcl:8-9`); SELECT/UPDATE `CARDSVC.ACCOUNT` (`CBCRD04.cbl:458,816`); SELECT/UPDATE `CARDSVC.CARD_LIMIT` (`CBCRD04.cbl:38`); UPDATE `CARDSVC.AUTHORIZATION` posted flag (`CBCRD04.cbl:850`).

## 3. Requirements owned

**CBCRD04-FR-001 — Exactly-once posting.** Each enriched authorization posts exactly one transaction and updates account/limit/authorization state once, across any number of restarts (CARDNITE-FR-006).

**CBCRD04-FR-002 — Checkpointed commit protocol.** Commits every `CC-COMMIT-FREQ` postings, rewriting `CC-LAST-KEY`/counters into CYCLCTL in the same commit (`CBCRD04.cbl:19-25,414-415,901-925`). `CC-COMMIT-FREQ` operator-tunable; zero ⇒ `WS-COMMIT-FREQUENCY` 1000 (`CBCRD04.cbl:334-335`, `app/cpy/CVCONSTY.cpy:37`). Cross-ref CARDNITE-FR-007.

**CBCRD04-FR-003 — Warm restart.** RESTART=Y skips forward to `CC-LAST-KEY` before posting (`CBCRD04.cbl:342-348`); a duplicate insert inside the checkpoint window is treated as already done (`CBCRD04.cbl:779`). Restart procedure + `CC-RESTART-CNT` ≤ 3 rule at `docs/runbook-cardnite.md:114-136`; cold re-run requires backout (CBCRD99J — recreated as an admin CLI in Phase 5, plan §4.3).

## 4. Target mechanism

Chunk-oriented step; chunk size bound to `cycle_control.commit_freq` (not hardcoded — target design Persistence row); checkpoint written in the chunk transaction. Restart: Spring Batch restart metadata **plus** the `cc_last_key` contract (the operator-visible source of truth). `BigDecimal` money throughout.

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | all posted |
| 4 | bypasses written to reject |
| 8 | too many bypasses or restart key not found |
| 12 | fatal — U0401 CYCLCTL unusable, U0402 file open/I-O failure, U0403 unrecoverable SQL error, U0404 restart key not found (source `CBCRD04.cbl:44-48`; **divergence:** the JCL comment `CBCRD04J.jcl:19-23` has U0402/U0403 swapped — source codes govern) |

Scheduler: U4004/S0C7 NOTOK, usually a bad packed field (`sched/CARDNITE.sched:147-148,420-421`) — packed-decimal parse failures become typed validation errors at file read.

## 6. Hard-stop boundary

None.

## 7. Acceptance criteria

- Kill mid-run, restart with restart=Y: every record posted exactly once (Phase 4 failure-path E2E, FR-007).
- restart=Y with a `cc_last_key` absent from input exits with the U0404-equivalent error.
- Changing `commit_freq` on the control row changes commit cadence next run.
- Unpostable records go to the bypass file and RC 4; bypass share above the threshold exits 8.
- Debit/credit control totals accumulate in `cycle_control` (`cc_tot_dr/cr`) for CBCRD10's proof.
