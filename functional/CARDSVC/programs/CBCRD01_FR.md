# CBCRD01 — cycle open + authorization log extract (CBCRD01J, wave 3)

Target: Spring Batch job `cbcrd01` in `backend/…/jobs/cbcrd01/`.
Stream FRs owned: CARDNITE-FR-001 (cycle entry), FR-003 (authorization extraction).

## 1. Trigger / caller contract

First job of the chain, ordered at 17:30 gated on `CICS-CARD-CLOSED` (`sched/CARDNITE.sched:43,52-55`). CBCRD01J STEP010 `PGM=CBCRD01` (`app/jcl/cardsvc/CBCRD01J.jcl:32`); STEP020 IEBGENER prints the extract control copy when RC 0/4 (`CBCRD01J.jcl:55-60`).

PARM: `'&CYCDATE,&CYCID'` — CCYYMMDD cycle date, cycle id after the comma (`CBCRD01J.jcl:12-13,32-33`).
*Target CLI:* `spring.batch.job.name=cbcrd01 cycleDate=… cycleId=…` (target design Scheduling seam row).

## 2. Field-level inputs / outputs

- In: VSAM ESDS `CARD.PROD.AUTHLOG` (DD AUTHLOG, `CBCRD01J.jcl:37`; SELECT `app/cardsvc/cbl/CBCRD01.cbl:48`) — 200-byte auth log records; VSAM `CARD.PROD.CYCLCTL` (`CBCRD01J.jcl:38`).
- Out: `CARD.PROD.AUTHEXTR(+1)` GDG, FB 300, layout CVAXTR01Y (`CBCRD01J.jcl:39-43`): AX-HEADER (cycle date/id, extract seq, source RBA), AX-AUTH-IMAGE X(179) uninterpreted, AX-EDIT/AX-ENRICH left for downstream (`app/cardsvc/cpy/CVAXTR01Y.cpy:13-40`).
- No Db2.

## 3. Requirements owned

**CBCRD01-FR-001 — Cycle open.** Initializes the cycle: sets `CC-COMMIT-FREQ` from `WS-COMMIT-FREQUENCY`, zeroes counters and `CC-LAST-KEY` (`CBCRD01.cbl:305-312`). Cross-ref CARDNITE-FR-001.

**CBCRD01-FR-002 — Faithful extract.** Every AUTHLOG record for the cycle is copied into an AUTHEXTR record with cycle header + source RBA; the 179-byte auth image is carried uninterpreted (CARDNITE-FR-003; `CVAXTR01Y.cpy:20`).

## 4. Target mechanism

`FlatFileItemWriter` to `AUTHEXTR.{cycleDate}.{gen}` per D5 file convention; `auth_log` read via the append-only table (D3) ordered by the synthetic sequence (stands in for RBA). Cycle open through the shared cycle-control component (CYCLCTL_contract_FR §2).

## 5. Error / edge behavior and RC mapping

| RC | Meaning | Source |
|---|---|---|
| 0 | complete | `CBCRD01J.jcl:15-19` |
| 4 | records skipped | same |
| 8 | *unreachable in source* — JCL comment only (`CBCRD01J.jcl:18`); see divergence below | — |
| 12 | fatal — U0101/U0102 (source) | `CBCRD01.cbl:26-27` |

Abends (source): U0101 cycle-control record unreadable (`CBCRD01.cbl:26,204`), U0102 AUTHLOG open failed (`CBCRD01.cbl:27,235`). **Divergence:** U0103 appears only in the runbook (`docs/runbook-cardnite.md:94`) — the source raises no 0103; source codes govern, runbook label retired at cutover.
**Divergence reconciliation (unreachable RC 8 — stream FR §5.3 item 15):** the JCL comment claims "8 = no records selected, cycle held", but the source moves only `WS-RC-WARNING` (0004) on zero written records (`CBCRD01.cbl:24,449-453`) — the program never sets 0008, so an empty AUTHLOG ends RC 4 and the chain **proceeds** (scheduler NOTOK fires at 8+, `sched/CARDNITE.sched:52-55`). Resolution: **source behavior governs at parity** — empty extract exits 4 with the warning; whether the business wants an empty online day to hold the cycle is a sign-off question, not silent target behavior.

Restart: STEP010 rerunnable from scratch; never restart at STEP020 (`CBCRD01J.jcl:21-26`) — target job is idempotent per cycle (re-run overwrites the cycle's extract file).
Edge: empty AUTHLOG ⇒ RC 4 warning ('NO RECORDS SELECTED') and the chain proceeds with an empty extract (see divergence above).

## 6. Hard-stop boundary

None. Consumes the `CICS-CARD-CLOSED` condition only as an external gate (scheduler seam, plan §4.3).

## 7. Acceptance criteria

- Given N auth-log rows for the cycle, the extract has exactly N records, byte-identical auth images, sequential extract seq, correct cycle date/id header.
- Cycle-control row is (re)initialized: counters zero, `cc_last_key` cleared, `commit_freq` defaulted.
- Empty input exits 4 with the no-records warning; the chain proceeds (legacy parity — divergence §5).
- Unreadable cycle control exits 12 with the U0101-equivalent error record.
