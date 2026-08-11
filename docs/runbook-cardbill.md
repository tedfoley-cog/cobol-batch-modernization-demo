# Operator runbook — CARDBILL and CARDREF

Card systems batch. This runbook is for the console operator and the
on-call batch analyst. It covers what each job does, what its return
codes mean, and how to restart it.

Contacts:

| Situation | Who |
|---|---|
| Job abends, no instruction below fits | Card systems on-call |
| Cycle does not balance (`CBBIL06J` RC 8) | Billing control, then card systems |
| VSAM rebuild count verification fails | Card systems on-call — **page, do not wait for the morning** |
| Acquirer feed missing or short | Merchant services |

Return code convention across both chains:

| RC | Meaning |
|---|---|
| 0 | Clean |
| 4 | Warning — work completed, exceptions listed on the step report |
| 8 | Business condition — work did not complete as intended |
| 12 | Fatal, normally with a user abend |

---

## Chain CARDBILL — monthly billing and statements

Runs once per cycle code, on the cycle day. Six jobs, strictly in
order. Nothing in the chain may be skipped or run out of sequence.

The cycle date, cycle day and cycle id are on the `SYSIN` cards of
every job in the chain. They are set by the scheduler from the cycle
calendar. **If you edit them by hand for a rerun, they must match the
open cycle control record** — `CBBIL01J` step `PRTCTL` prints it.

### CBBIL01J — cycle selection

Selects the accounts whose cycle day matches, snapshots them to
`CARD.PROD.BILL.BILLWRK(+1)` and opens the cycle in VSAM `CYCLCTL`.
Step `COPYWRK` takes the snapshot copy that `CBBIL06J` reconciles
against.

| RC | Action |
|---|---|
| 0 | Continue |
| 4 | Accounts were skipped — listed on `SYSOUT`. Continue, tell billing control in the morning |
| 8 | The cycle is already marked complete in `CYCLCTL`. Do not rerun. Call card systems |
| 12 | U801 — see below |

**Restart.** `CBBIL01` is restartable from the top. It finds the open
cycle control record, bumps the restart count and repositions after
the last committed account. Resubmit the whole job. Do **not** delete
the `BILLWRK` generation first — it is opened MOD and the restart
appends to it.

### CBBIL02J — statement assembly

Sorts the work file, then builds one statement record per account from
`CARDSVC.TRANSACTION`.

| RC | Action |
|---|---|
| 0 | Continue |
| 4 | One or more accounts had more transactions than the line array holds, or a bucket total disagreed with the ledger by more than `BAL-TOL`. The accounts are on `SYSOUT`. Continue |
| 8 | The cycle control record is not open, or is open for a different cycle date. Stop, call card systems |
| 12 | U802 — see below |

**Restart.** Resubmit from the top. The step deletes and recreates its
own output generation, so a partial `STMTWRK` from the failed run is
discarded.

If the job failed on `SB37` or `E37` on `STMTWRK`, increase the
secondary space on the `STMTWRK` DD before resubmitting. Do **not**
lower `LRECL` — it is 24208 because the variable record can reach
24204 bytes plus the RDW.

### CBBIL03J — minimum payment and due date

Computes the minimum payment and the payment due date, and writes them
back to `CARDSVC.ACCOUNT`.

| RC | Action |
|---|---|
| 0 | Continue |
| 4 | Some accounts took the floor amount rather than the percentage, or a due date rolled more than three days. Continue |
| 8 | The minimum payment percentage or floor card is missing or unreadable. Stop, correct the `SYSIN` and rerun |
| 12 | U803 — see below |

**Restart.** Resubmit from the top. The account update is keyed by
cycle date and is applied again with the same result, so a partial run
is safe to repeat.

The `HOLIDAY` cards are the federal calendar for the cycle year. A
missing card is not an error and will not fail the job — it means due
dates can land on a closed day. Report it, do not hold the chain.

### CBBIL04J — statement rendering

Reads the delivery preference from VSAM `CUSTPREF` and renders each
statement through the formatter the route table resolves for the
delivery key. Step `PRINT` sends the paper images to the print class.

| RC | Action |
|---|---|
| 0 | Continue |
| 4 | Some accounts fell back to paper — no `CUSTPREF` record, a consent older than the cut-over date, or the electronic formatter returned a warning. The paper print is complete and correct. Continue |
| 8 | The route for `STMT`/`PAPR` or `STMT`/`ELEC` could not be resolved. Stop. This is normally a `PGMROUT`/`PGM_ROUTE` problem — see the CARDREF section |
| 12 | U804 — see below |

**Restart.** Resubmit from the top. Both output files are recreated.

If the print step has already run and the render step is rerun, the
paper images are printed twice. Cancel the duplicate print in the
output queue.

### CBBIL05J — statement persistence and archive

Inserts the statement rows and writes the seven year archive.

| RC | Action |
|---|---|
| 0 | Continue |
| 4 | Statements already present in `CARDSVC.STATEMENT` were rewritten rather than inserted. Expected on a rerun. Continue |
| 8 | The archive dataset could not be written. The DB2 rows are committed. Stop, call card systems |
| 12 | U805 — see below |

**Restart.** Resubmit from the top. `CBBIL05` is idempotent — it
detects the existing row on `-803` and rewrites it. The archive is
allocated MOD, so a restart appends; duplicate archive images for the
same statement are tolerated by the retrieval service, which reads the
last one.

If the archive generation has already been cut and migrated and only
the DB2 side needs repeating, set `ARCHIVE N` on the `SYSIN` card.

### CBBIL06J — reconciliation and cycle close

Proves the cycle and closes the cycle control record.

| RC | Action |
|---|---|
| 0 | Cycle closed. Chain complete |
| 4 | Cycle closed with exceptions listed on `RECONRPT`. Send the report to billing control. Chain complete |
| 8 | **The cycle did not balance.** The cycle control record has been left open. Nothing downstream may run. Call billing control and card systems. Do not rerun any CARDBILL job until they have signed off |
| 12 | U806 — see below |

**Restart.** Resubmit from the top. The step is read-only until the
proof passes, so it can be run as many times as needed while the
difference is being investigated.

### CARDBILL user abends

| Code | Meaning | Action |
|---|---|---|
| U801 | `CBBIL01` — `CYCLCTL` open/read failed, or the selection cursor failed | Check the cluster is not held by another job or closed to the region, then the `SYSTSPRT` `SQLCODE`. `-911` is a timeout, resubmit |
| U802 | `CBBIL02` — statement assembly file or SQL failure | Check the `SQLCODE` and the `STMTWRK` allocation |
| U803 | `CBBIL03` — account update file or SQL failure | Check the `SQLCODE`. A `-803` here is not normal, call card systems |
| U804 | `CBBIL04` — the dispatcher returned a fatal condition, or a formatter failed | Do not resubmit until the route table has been checked |
| U805 | `CBBIL05` — statement insert or archive write failed | Check the archive space first, then the `SQLCODE` |
| U806 | `CBBIL06` — reconciliation could not read the snapshot, `CYCLCTL` or DB2 | Check `CARD.PROD.BILL.BILLSNAP` exists for the cycle |

---

## Chain CARDREF — weekly reference and VSAM rebuild

Runs Saturday night. Five jobs in order. Three of them rebuild VSAM
clusters that the online regions read.

**Before submitting `CBREF02J`, `CBREF03J` or `CBREF04J` the operator
closes the affected file to the CICS regions, and does not reopen it
until the verify step of that job has ended with RC 0.**

Files to close, by job:

| Job | Close to CICS |
|---|---|
| `CBREF02J` | `MERCHRTE` |
| `CBREF03J` | `FRAUDRUL`, `RSNCODE` |
| `CBREF04J` | `PGMROUT` |

### CBREF01J — merchant reference refresh

Sorts the acquirer feed and applies it to `CARDSVC.MERCHANT`.

| RC | Action |
|---|---|
| 0 | Continue |
| 4 | Records were rejected, inside the `REJECT-TOL` limit. The reject list is on `MERCHRPT` — it goes back to the acquirer. Continue |
| 8 | Rejects exceeded `REJECT-TOL`. The feed is probably the wrong generation or the wrong acquirer. Stop, call merchant services |
| 12 | U901/U902/U903 — see below |

**Restart.** Resubmit from the top. The apply commits every
`COMMIT-FREQ` records; records already applied are reapplied with the
same result because the feed carries the full merchant image.

Deletes are logical — the merchant status is set to `D` and the row
stays. A merchant that "disappears" from the feed is not deleted at
all. Do not clean these rows up.

### CBREF02J — rebuild MERCHRTE

Unload, sort, delete/define/repro, verify.

| RC | Action |
|---|---|
| 0 | Reopen `MERCHRTE` to the regions |
| 4 | Merchants were suppressed from the unload — listed on `SYSOUT`. The cluster is complete for everything that was unloaded. Reopen and report |
| 12 | U911 — file error or **count verification failed, see below**. U912 is an SQL failure on the unload |

### CBREF03J — rebuild FRAUDRUL and RSNCODE

| RC | Action |
|---|---|
| 0 | Reopen both files |
| 4 | Reason code records were dropped in validation. This is normal — the scheme sends the whole catalogue including codes for products the bank does not issue. Reopen both files |
| 12 | U921 — file error or count verification failed, see below. U922 is an SQL failure on the fraud rule unload |

### CBREF04J — rebuild PGMROUT

Rebuilds the dispatchers' fallback route copy from the rows that are
active and effective on the run date.

| RC | Action |
|---|---|
| 0 | Reopen `PGMROUT` |
| 4 | Route generations collapsed onto the same key, or cross-module rows were counted — both are listed on `SYSOUT`. Reopen and pass the listing to card systems |
| 12 | U931 — file error or count verification failed, see below. U932 is an SQL failure on the route unload |

An abend here leaves the estate with no fallback route copy. The
online regions keep running from the DB2 table, so this is **not** an
outage, but it must be cleared before the next DB2 maintenance window.

The run date is on the `PARMS` of both steps and is deliberately not
defaulted to the system date. A rebuild that runs after midnight must
still use the date the cycle was processed for.

### CBREF05J — reference integrity report

Read-only. Orphan checks, route table sanity, VSAM against DB2 counts.

| RC | Action |
|---|---|
| 0 | Chain complete |
| 4 | Findings inside tolerance. File the report |
| 8 | Tolerance exceeded, or a cluster count disagrees with DB2. Step `NOTIFY` has already printed the instruction. Raise a data quality ticket against card systems and attach the report. **Do not rerun the rebuild jobs to clear it** — the counts will agree on the rerun and the underlying rows will not have changed |
| 12 | U941 (file or data) or U942 (SQL) |

**Restart.** Resubmit from the top at any time. The step changes
nothing.

---

## When a VSAM rebuild count verification fails

This is U911, U921 or U931. It means the verify step counted the
records in the rebuilt cluster and got a different number from the
control record written by the unload, or from a fresh count of the
table.

By the time this happens the old cluster has already been deleted. A
short cluster is worse than no cluster — the online path would silently
decline or misroute everything that did not make it into the rebuild.

1. **Do not reopen the file to the CICS regions.** This is the whole
   point of the verification.
2. **Do not resubmit the job blind.** If the cause is still present the
   second rebuild is short as well, and the evidence from the first is
   gone.
3. Take the counts off the verify step `SYSOUT`. It prints the control
   count from the unload, the count read out of the cluster and the
   fresh count from DB2.
4. Read them like this:

   | Pattern | Usual cause |
   |---|---|
   | Cluster count < control count, IDCAMS `MAXCC` 4 or 8 | The `REPRO` hit a duplicate or out of sequence key and stopped. Look at the `BLDxxxx` step `SYSPRINT` |
   | Cluster count < control count, IDCAMS clean | The cluster ran out of space. Look for `IEC070I` on the job log and increase the `CYLINDERS`/`TRACKS` on the `DEFINE` |
   | Control count ≠ fresh DB2 count, cluster = control | Someone updated the table while the chain was running. Confirm with database services, then rerun the whole job |
   | All three counts zero | The unload found nothing. Check the `SYSIN`/`PARMS` run date before anything else |

5. Once the cause is understood, resubmit the **whole job** from the
   unload step. The rebuild is not restartable from the middle — the
   temporary unload and sort files do not survive the failure.
6. Reopen the file only after the verify step ends RC 0 or RC 4.

If the file must be back for the online day before the cause can be
found, card systems can restore the previous cluster from the nightly
backup. That is a card systems decision, not an operator one — page
them.

---

## Rerunning a whole chain

CARDBILL and CARDREF are separate chains and do not share datasets.
Either can be rerun without the other.

To rerun CARDBILL from the start for a cycle that has already been
closed, card systems must first reset the cycle control record. There
is no operator procedure for that — `CBBIL01` will end RC 8 rather
than reopen a closed cycle, and that check is deliberate.

To rerun CARDREF from the start, submit `CBREF01J` through `CBREF05J`
in order, closing and reopening the VSAM files as above.
