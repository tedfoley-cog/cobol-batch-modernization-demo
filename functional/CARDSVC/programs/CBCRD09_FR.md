# CBCRD09 — backups and AUTHLOG reset (CBCRD09J: 3 IDCAMS steps + CBCRD09, wave 3)

Target: Spring Batch job `cbcrd09` in `backend/…/jobs/cbcrd09/` (four steps mirroring the JCL).
Stream FRs owned: CARDNITE-FR-018 (backup/AUTHLOG reset).

## 1. Trigger / caller contract

Runs on CBCRD08J-OK. **Scheduler exception:** a CBCRD09J failure still posts its successor so the close/report job can run (`sched/CARDNITE.sched:350-357`; CARDNITE-FR-002 exception). Step order is load-bearing (`app/jcl/cardsvc/CBCRD09J.jcl:8-16`): never reset before backups are verified-writable.
PARM (STEP040): cycle date only (`CBCRD09J.jcl:113`).

## 2. Step contracts

### STEP010 — IDCAMS EXPORT
`EXPORT CARD.PROD.CYCLCTL … TEMPORARY CIMODE` to `CARD.PROD.EXPORT.CYCLCTL(+1)` (`CBCRD09J.jcl:34-47`).

### STEP020 — IDCAMS REPRO (gated `IF STEP010.RC=0`, `CBCRD09J.jcl:50`)
REPRO three clusters to backup GDGs: CARDXREF→`CARD.PROD.BKP.CARDXREF(+1)` FB 128, CYCLCTL→`BKP.CYCLCTL(+1)` FB 256, AUTHLOG→`BKP.AUTHLOG(+1)` FB 200 (`CBCRD09J.jcl:51-78`).

### STEP030 — IDCAMS DELETE/DEFINE (gated after STEP020, `CBCRD09J.jcl:89`)
DELETE/DEFINE `CARD.PROD.AUTHLOG` — NONINDEXED (ESDS), RECORDSIZE(200 200), CISZ 8192, matching `app/jcl/vsam/DEFCARD.jcl:37-48` (`CBCRD09J.jcl:89-107`). AUTHLOG is never reused in place; the new day starts fresh.

### STEP040 — CBCRD09 verification (gated `IF STEP030.RC=0`, `CBCRD09J.jcl:111`)
Reads the three backup generations + live AUTHLOG + CYCLCTL, writes backup register BKPREG FBA 133 (`CBCRD09J.jcl:112-122`; SELECTs `app/cardsvc/cbl/CBCRD09.cbl:46-68`). No Db2.

## 3. Requirements owned

**CBCRD09-FR-001 — Verified backups before reset.** Backups are taken and provably non-empty before the auth log is reset; the reset is proven effective (log empty) afterwards (CARDNITE-FR-018).

**CBCRD09-FR-002 — Generation-size sanity.** A backup smaller than the previous generation is a warning (RC 4) for operator review.

## 4. Target mechanism (approved D3)

`auth_log` as an append-only table; preferred mapping is **day-partitioning** — backup = detach/archive partition, "delete/redefine" = new partition (alternative: truncate-after-verified-backup). CYCLCTL/CARDXREF backups become table exports (e.g. `COPY TO` files per D5 naming). Verification step re-reads the exports + the emptied live set, producing BKPREG-equivalent output.

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | verified and AUTHLOG empty |
| 4 | backup smaller than previous generation |
| 12 | U0901 empty CARDXREF backup, U0902 empty AUTHLOG backup, U0903 AUTHLOG not empty after redefine |

(`CBCRD09J.jcl:18-24`; abend sites `CBCRD09.cbl:305,344,394,441-461`.)
Restart rules split at STEP030: before it, rerun from the top; after it, never re-run the export/REPRO steps against the now-empty log (`CBCRD09J.jcl:25-30`, `docs/runbook-cardnite.md:208-219`) — the target job enforces this with step-status guards.

## 6. Hard-stop boundary

None.

## 7. Acceptance criteria

- Reset never executes when any backup step failed or produced an empty backup (U0901/U0902 paths exit 12 pre-reset).
- After reset, verification proves the live auth set empty; residual rows exit 12 (U0903).
- Smaller-than-previous backup exits 4 and the cycle continues.
- A CBCRD09 failure does not block CBCRD10 (scheduler-exception parity, FR-002/018).
- Restart after the reset step cannot re-export the emptied log.
