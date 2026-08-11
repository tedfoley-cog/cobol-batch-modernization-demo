# PARTYWK weekly cycle — operator runbook

Applies to the four PARTYRSK weekly jobs. Operator level only. Anything not
covered here goes to the on-call, do not improvise on a production weekend.

| Item | Value |
|---|---|
| Cycle name | `PARTYWK` |
| Scheduler | Sunday, released after the Saturday nightly has ended |
| DB2 subsystem | `DB2P` |
| Plan | `PARTYWKP` |
| Load library | `PRTY.PROD.LOADLIB` |
| Cycle control | `PRTY.PROD.CYCLCTL`, key `PARTYWK ` + cycle date |
| On-call | Party and Risk, pager **4471** |
| Compliance contact | Compliance Reporting, extension **2290** (office hours) |
| Escalation | Batch duty manager, pager 4400, after two failed attempts |

## Run order

```
CBPRT01J  02:00   KYC refresh
CBPRT02J  03:30   sanctions feed load + WATCHLST rebuild
CBPRT03J  05:00   full portfolio rescore        <- longest job, 4-5 hours
CBPRT04J  11:00   regulatory risk audit pack
```

Each job waits for the one above it. Do not release a job early to catch up
time — `CBPRT04J` in particular reports last week's numbers if `CBPRT03J` has
not finished.

`CBPRT02J` needs VSAM `WATCHLST` closed in CICS. The scheduler issues the
close before the job and the open after it. If the job is submitted by hand,
ask the CICS operator to close `WATCHLST` and its path first, and to open them
again when the job ends.

---

## CBPRT01J — KYC refresh

Finds parties whose KYC review date has passed, raises a pending review row
and writes the work list for the financial crime team.

| Step | What it does |
|---|---|
| `DELWORK` | deletes last week's work list |
| `KYCDUE` | `CBPRT01`, raises the pending rows |
| `SORTWORK` | sorts the work list, enhanced due diligence first |
| `PRTWORK` | prints the work list to class A, destination `KYC1` |

Return codes from `KYCDUE`:

| RC | Meaning | Action |
|---|---|---|
| 00 | all due reviews raised | none |
| 04 | raised with warnings | note it on the shift log, carry on |
| 08 | duplicate or orphaned KYC rows skipped | call on-call before releasing `CBPRT02J` |
| 12 | failed | see below |

On RC 12 the job is rerunnable from the top. Rows already raised are detected
and skipped, so a rerun does not double up the review queue. Rerun once. If it
fails a second time, call on-call.

---

## CBPRT02J — sanctions ingest and WATCHLST rebuild

Loads the vendor feed generation `PRTY.PROD.SANCFEED(0)` into DB2, then
unloads the active entries, sorts them and rebuilds VSAM `WATCHLST` and its
alternate index.

| Step | What it does |
|---|---|
| `ALLOCREJ` | allocates this week's reject generation |
| `SANCLOAD` | `CBPRT02A`, loads the feed |
| `WATCHEXT` | `CBPRT02B`, unloads the active entries |
| `SORTWL` | sorts the extract into cluster key order |
| `BKUPWL` | backs the current `WATCHLST` up to a generation |
| `BLDWL` | `REPRO` into the cluster, then `DEFINE AIX` / `BLDINDEX` / `DEFINE PATH` |
| `PRTREJ` | prints the rejects, runs even after a failure |

Return codes from `SANCLOAD`:

| RC | Meaning | Action |
|---|---|---|
| 00 | feed loaded clean | none |
| 04 | loaded with rejects | rejects print in step `PRTREJ`, give them to the sanctions analyst Monday |
| 08 | **feed refused** — control totals did not balance, or the feed asked to delist more entries than the tolerance allows | nothing was committed and `WATCHLST` was left alone. Call on-call. The vendor has to resend before the job is rerun |
| 12 | failed | rerun once, then call on-call |

Return codes from `WATCHEXT` (`CBPRT02B`):

| RC | Meaning | Action |
|---|---|---|
| 00 | extract written | none |
| 04 | written, some entries skipped as unsearchable | note on the shift log |
| 08 | extract below the minimum volume | `WATCHLST` is **not** rebuilt. The old file stays in place, which is the safe outcome. Call on-call |
| 12 | failed | call on-call |

`WATCHLST` is only rebuilt when the load ended below 08, so a failure never
leaves a half-built screening file behind. If `BLDWL` itself fails, the
cluster may be part loaded — restore it from `PRTY.PROD.WATCHLST.BKUP(0)`
before CICS is allowed to open it, and call on-call.

Do not let CICS reopen `WATCHLST` until the job has ended with RC 00 or 04.

---

## CBPRT03J — full portfolio rescore

Drives every active party through the risk recalculation chain. The longest
job in the cycle. Four to five hours on a normal week; longer if the
sanctions feed brought a large batch of new entries.

| Step | What it does |
|---|---|
| `CHKCTL` | proves the cycle control cluster is there |
| `RESCORE` | `CBPRT03`, the rescore itself |
| `SORTEXCP` | sorts the exception list, worst return code first |
| `PRTEXCP` | prints the manual review list, destination `RSK1` |
| `FAILMSG` | prints the restart instructions when the rescore failed |

Return codes from `RESCORE`:

| RC | Meaning | Action |
|---|---|---|
| 00 | whole portfolio rescored clean | none |
| 04 | rescored with warnings | note on the shift log |
| 08 | rescored, manual review cases on the exception list | normal on most weeks. The list prints in `PRTEXCP` and goes to the financial crime team |
| 12 | failed | restart from the checkpoint, see below |

### Restarting CBPRT03J

The job checkpoints. `CBPRT03` commits every 1000 parties and then writes the
last party id into the `PARTYWK` cycle control record on `PRTY.PROD.CYCLCTL`.
**Do not rerun it from the top** — that works, but it throws away hours of
completed work.

1. Find this line on the job log:

   ```
   CBPRT03  RESTART WITH PARM=RESTART AFTER PARTY nnnnnnnnnnn
   ```

   If the line is not there, the step died before the first checkpoint.
   Leave the run card on `PARMS('/COLD')` and resubmit normally.

2. Otherwise edit the job:
   - put `RESTART=RESCORE` on the job card,
   - change the `SYSTSIN` run card from `PARMS('/COLD')` to
     `PARMS('/RESTART')`.

3. Resubmit. The step positions past the last committed party and carries on.
   The totals on the job log cover the whole cycle, not just the last leg.

Rescoring a party twice is harmless — it gets a second timestamped score row
and the exposure row is replaced — so if you are unsure whether a checkpoint
was taken, restarting is always the safer choice than skipping ahead.

If the cycle control record has been lost or the LISTCAT in `CHKCTL` fails,
call on-call. Do not rerun the `DEFPWCTL` define job to fix it: that deletes
the restart position for good.

---

## CBPRT04J — regulatory risk audit pack

Prints the weekly pack for the financial crime committee: band movement by
module, banded distribution, sanctions hit summary and overdue KYC
exceptions.

| Step | What it does |
|---|---|
| `RPTAUD` | `CBPRT04`, builds the pack on a temporary dataset |
| `ARCHRPT` | copies it to the archive generation, retained seven years |
| `PRTAUD` | releases two copies to the compliance printer, destination `CMPL` |
| `CYCLEND` | prints the cycle control record for the scheduler |

Return codes from `RPTAUD`:

| RC | Meaning | Action |
|---|---|---|
| 00 | pack printed | none |
| 04 | printed but no audit activity in the period | check that `CBPRT03J` actually ran this weekend. If it did not, run it before rerunning this job |
| 12 | failed | nothing was printed or archived |

The job only reads, so it is fully rerunnable. Rerun once on RC 12. If it
fails twice, call on-call and tell Compliance Reporting on extension 2290 that
the Monday pack will be late — they have a submission deadline and would
rather know on Sunday.

---

## Abend codes

| Code | Job | Meaning |
|---|---|---|
| `U3111` | `CBPRT01J` | KYC refresh hit an unrecoverable SQL or file error |
| `U3121` | `CBPRT02J` | sanctions load hit an unrecoverable error, nothing committed |
| `U3122` | `CBPRT02J` | watch list extract hit an unrecoverable error |
| `U3131` | `CBPRT03J` | rescore failed, restartable from the last checkpoint |
| `U3141` | `CBPRT04J` | audit pack failed, nothing printed |

Every one of these prints a diagnostic line naming the program, the paragraph
and the party id or key involved. Quote those to on-call — they are what the
support team asks for first.

## Datasets

| Dataset | Used by |
|---|---|
| `PRTY.PROD.KYCWORK.WEEKLY` | `CBPRT01J` work list |
| `PRTY.PROD.KYCWORK.SORTED` | `CBPRT01J` sorted work list, GDG |
| `PRTY.PROD.SANCFEED` | vendor sanctions feed, GDG, created by `PRTYFTP1` |
| `PRTY.PROD.SANCREJ` | sanctions rejects, GDG |
| `PRTY.PROD.WATCHLST` | screening file, VSAM KSDS |
| `PRTY.PROD.WATCHLST.AIX1` | alternate index by entity name |
| `PRTY.PROD.WATCHLST.PATH1` | path the online screening reads |
| `PRTY.PROD.WATCHLST.BKUP` | pre-rebuild backup, GDG |
| `PRTY.PROD.RSKPARM` | risk model parameters, VSAM KSDS |
| `PRTY.PROD.CYCLCTL` | cycle control and restart position, VSAM KSDS |
| `PRTY.PROD.RSKEXCP` | rescore exceptions, GDG |
| `PRTY.PROD.RISKAUD.PACK` | archived audit pack, GDG, seven year retention |
