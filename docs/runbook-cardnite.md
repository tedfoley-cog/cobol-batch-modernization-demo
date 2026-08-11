# CARDNITE — nightly authorization posting cycle

Operator runbook. Card Services batch, application group `CARD`, scheduler table
`CARDNITE`. Contact: Card Batch Services on-call, then the duty manager.

Nothing in this document replaces the change record for a parameter change. If a
control card or `PARM` value has to be altered, the duty manager approves it and
the change is written up the same night.

## 1. Running order

| Order | Job | Program(s) | What it does |
|---|---|---|---|
| 1 | `CBCRD01J` | `CBCRD01` | Extracts the authorization log for the cycle to `CARD.PROD.AUTHEXTR(+1)` |
| 2 | `CBCRD02J` | `CBCRD02` | Edits the extract, splits clean records from rejects |
| 3 | `CBCRD03J` | `SORT`, `CBCRD03` | Sorts the clean file, enriches merchant, MCC, acquirer and settlement route |
| 4 | `CBCRD04J` | `CBCRD04` | Posting engine — inserts transactions, updates balances |
| 5a | `CBCRD05AJ` | `CBCRD05A` | Fee assessment branch |
| 5b | `CBCRD05BJ` | `CBCRD05B` | Interest and rewards branch |
| 6 | `CBCRD06J` | `CBCRD06W/A/X/B/C` | Join — exposure recalculation and risk band update |
| 7 | `CBCRD07J` | `CBCRD07` | General ledger feed, proves debits equal credits |
| 8 | `CBCRD08J` | `CBCRD08` | Delinquency roll, aging report, collections feed |
| 9 | `CBCRD09J` | `IDCAMS`, `CBCRD09` | VSAM backups, GDG roll, authorization log reset |
| 10 | `CBCRD10J` | `CBCRD10` | Cycle close and control report |

`CBCRD05AJ` and `CBCRD05BJ` are released together as soon as `CBCRD04J` ends with
`RC <= 4`. They run in parallel and in either order. Everything else is a straight
chain: a job is released only when the previous one ended `RC <= 4`, except
`CBCRD06J` which has the additional check described below.

The cycle date is carried on the job `SET CYCDATE=` statement and reaches the
programs on the `PARM`. The scheduler substitutes it from the run calendar. If a
job is submitted by hand, the cycle date on every job of the night must be the
same value — a mismatched cycle date is the most common cause of an empty run.

## 2. The fork and the join

`CBCRD05AJ` and `CBCRD05BJ` each post a completion flag into the cycle control
record `CARD.PROD.CYCLCTL` as the very last thing they do:

* fee branch flag — `A` complete, `F` failed
* interest branch flag — `B` complete, `F` failed

`CBCRD06J` step `STEP005` runs `CBCRD06W`, which reads the cycle control record for
the cycle date and checks both flags. It does not rely on the scheduler dependency.
If a flag is missing it waits and polls (one minute per poll, poll limit on the
`PARM`, standing value `030`), then abends.

**Before releasing `CBCRD06J` by hand, confirm both branches yourself:**

1. Both branch jobs are shown ended in the scheduler with `RC <= 4`.
2. `CBCRD05A` job log shows `CBCRD05A - FEE BRANCH FLAG POSTED AS A`.
3. `CBCRD05B` job log shows `CBCRD05B - INTEREST BRANCH FLAG POSTED AS B`.
   A flag posted as `F` means the branch failed and the join must not run.
4. If either message is absent, the branch did not finish its last step even if
   the job appears to have ended. Do not release the join.

Never force `CBCRD06J` past `STEP005`. The step exists because the join has been
released early in the past and accounts were recalculated against half a night of
fees. If `STEP005` is holding the cycle, the answer is to fix or re-run the branch,
not to bypass the check.

## 3. Return codes

Estate convention, used by every program in the cycle:

| RC | Meaning | Operator action |
|---|---|---|
| `0000` | Clean | Release the next job |
| `0004` | Warning or partial | Release the next job, work the printed exception listing the same morning |
| `0008` | Business decline or serious condition | Do **not** release the next job unless this runbook says the job is gated to continue. Call Card Batch Services |
| `0012` | Fatal | Job has abended. Call Card Batch Services. Do not resubmit blind |

Job specific meanings:

* `CBCRD02J` `0008` — rejects exceeded the tolerance on the `PARM` (standing value
  `02` percent). The feed is suspect. Hold the cycle.
* `CBCRD03J` `0004` — merchants were defaulted. Settlements are informed by the
  warning count in the job log.
* `CBCRD04J` `0008` — too many postings bypassed, or a restart key was not found.
* `CBCRD06J` `STEP030` `0008` — at least one party went to manual review. This is a
  business outcome. `STEP040` still runs for the accepted parties — see section 6.
* `CBCRD07J` `0004` — some groups took the suspense account. Finance must add a
  mapping card before the next cycle.
* `CBCRD08J` `0004` — accounts reached bucket 6 and are charge-off candidates.
  Collections work the feed the same morning.
* `CBCRD10J` `0008` — the region stays closed, see section 7.

## 4. User abends

| Abend | Job | Meaning | Action |
|---|---|---|---|
| `U0102` | `CBCRD01J` | Authorization log or control file failure | Call Card Batch Services |
| `U0103` | `CBCRD01J` | No cycle control record for the cycle date | Check the cycle date on the `PARM` |
| `U0202` | `CBCRD02J` | Extract file failure | Call Card Batch Services |
| `U0302/3` | `CBCRD03J` | Merchant route file or SQL failure | Call Card Batch Services |
| `U0402` | `CBCRD04J` | SQL failure in the posting engine | Section 5 |
| `U0403` | `CBCRD04J` | Cycle control could not be updated at a commit point | Section 5 |
| `U0502` | `CBCRD05AJ` | Fee branch fatal, no flag posted | Re-run the branch job |
| `U0512` | `CBCRD05BJ` | Interest branch fatal, no flag posted | Re-run the branch job |
| `U0602` | `CBCRD06J` | Branches not complete after the poll limit | Section 2 |
| `U0603` | `CBCRD06J` | A branch posted a failed flag | Re-run the failed branch first |
| `U0632` | `CBCRD06J` | The crossing returned a fatal condition | Section 6 |
| `U0701` | `CBCRD07J` | Ledger mapping control card error | Finance corrects the card, re-run |
| `U0703` | `CBCRD07J` | SQL failure | Call Card Batch Services |
| `U0704` | `CBCRD07J` | Ledger feed does not balance, feed backed out | Section 8 |
| `U0803` | `CBCRD08J` | SQL failure in the delinquency roll | Call Card Batch Services |
| `U0901` | `CBCRD09J` | Card cross reference backup is empty | Section 9 |
| `U0902` | `CBCRD09J` | Authorization log backup is empty | Section 9 |
| `U0903` | `CBCRD09J` | Redefined authorization log is not empty | Section 9 |
| `U1002` | `CBCRD10J` | File or VSAM failure at cycle close | Call Card Batch Services |
| `U1003` | `CBCRD10J` | SQL failure at cycle close | Call Card Batch Services |

## 5. Restarting `CBCRD04J` from a checkpoint

The posting engine commits every 1000 rows and writes its restart position into the
cycle control record at each commit point. It can always be resumed; it must never
be resumed after a cold re-run of the cycle.

1. Do **not** delete or recreate any data set. The work committed before the
   failure is good and must be kept.
2. List the cycle control record for the cycle date with `CBCRD98J`. Note:
   * `CC-LAST-KEY` — the last authorization key committed
   * `CC-RECS-WRITTEN` — transactions posted so far
   * `CC-RESTART-CNT` — how many restarts have already been taken
3. Check the job log of the failed run. The last message
   `CBCRD04 COMMIT AT KEY ...` must agree with `CC-LAST-KEY`. If they disagree,
   stop and call Card Batch Services.
4. Resubmit with `RESTART=STEP010` and change the `PARM` on the `SYSTSIN` `RUN`
   card to `PARM('ccyymmdd,RESTART=Y')`. The program skips forward to the restart
   key before it posts anything.
5. If `CC-RESTART-CNT` has reached 3, do not restart again. The cycle is being
   defeated by something that will not fix itself; call Card Batch Services.
6. A cold re-run of the whole cycle means running `CBCRD99J` (cycle backout) first.
   Never run `CBCRD04J` with `RESTART=N` a second time against a cycle date that
   already posted — the transactions would be duplicated.

## 6. Handling the crossing in `CBCRD06J`

`STEP030` sends each party out for an exposure recalculation and gets a return code
back per party.

* `0` — the new exposure is accepted.
* `4` — accepted with a warning, counted into the exception listing on `RISKEXC`.
* `8` — sanctions or another serious condition. The party is written to
  `CARD.PROD.PARTYRVW` and excluded from `STEP040`. The step's own return code
  becomes `8`.
* `12` — fatal, the step abends `U0632`.

**What to do when `STEP030` ends `8`:**

1. This is not a failure of the cycle. `STEP040` is gated `IF RC <= 8` and applies
   the accepted parties as normal. Let the job run to the end.
2. `STEP050` prints the manual review file. Take the listing off the spool.
3. Send the listing to the Financial Crime duty officer the same morning. It is a
   same-day obligation, not a next-working-day one.
4. Do not re-run `CBCRD06J` to try to get a clean return code, and do not re-run
   `STEP030` alone. The parties on the review file must stay excluded until the
   duty officer releases them.
5. If `STEP030` ends `12` (`U0632`), the crossing itself failed rather than a
   party. Resubmit with `RESTART=STEP030` once. If it fails again, call Card Batch
   Services — the target system may be down, in which case the cycle is held.

## 7. The online region

`CBCRD10J` decides whether the online region reopens. The decision is in two places
and they always agree:

* the step return code, which the scheduler tests before releasing `CARDONLJ`, and
* the online-closed flag on the cycle control record, which the operator can see.

| `CBCRD10J` RC | Region | Action |
|---|---|---|
| `0000` | Reopens | Release `CARDONLJ` normally |
| `0004` | Reopens | Release `CARDONLJ`, work the exception summary the same morning |
| `0008` | Held closed | Page the duty manager. Do not start the region by hand |
| `0012` | Held closed | Page the duty manager and Card Batch Services |

An `0008` from `CBCRD10J` means one of: a stage of the cycle produced no statistics
and probably did not run, transactions are still not in the ledger, or the ledger
does not balance. The control report names which. Starting the region in that state
puts customers in front of balances that have not been proven.

## 8. Ledger out of balance — `U0704`

`CBCRD07J` proves total debits equal total credits before it commits. On a
difference it rolls the whole feed back and abends. Nothing partial is left in
`CARDSVC.GL_POSTING`.

1. Take the general ledger report off the spool. The proof lines at the end show
   the debit total, the credit total and the difference.
2. If the difference is exactly the value of one group, a mapping card is wrong or
   missing. Finance corrects the card and the job is resubmitted with
   `RESTART=STEP010`.
3. Otherwise call Card Batch Services. Do not resubmit repeatedly — every attempt
   re-reads the same unposted transactions and will fail the same way.

## 9. Backups and the authorization log reset — `CBCRD09J`

The steps must run in this order and must not be reordered:

1. `STEP010` exports the cycle control cluster.
2. `STEP020` copies the cross reference, control and authorization clusters to the
   backup generations.
3. `STEP030` deletes and redefines the authorization log for the new day.
4. `STEP040` verifies all three backups and proves the redefined log is empty.

Restart rules:

* Failed in `STEP010` or `STEP020` — `RESTART=STEP010` is safe, nothing has been
  scratched yet.
* Failed in or after `STEP030` — the live authorization log has already been
  replaced. Use `RESTART=STEP040` only, and call the storage administrator before
  running any `IDCAMS` step again.
* `U0901` or `U0902` — a backup generation is empty. The backup is worthless. Call
  the storage administrator immediately and do not let the cycle continue.
* `U0903` — the redefined authorization log is not empty, which means the define
  did not take effect or the online region is already writing to it. Confirm the
  region is down, then call the storage administrator.

## 10. Morning checklist

1. Control report from `CBCRD10J` filed, with the debit and credit totals and the
   hash total.
2. Reject listing from `CBCRD02J` worked, or noted as empty.
3. Bypassed posting listing from `CBCRD04J` worked, or noted as empty.
4. Manual review listing from `CBCRD06J` sent to Financial Crime, if any.
5. General ledger report from `CBCRD07J` filed.
6. Aging report from `CBCRD08J` filed and the collections feed transmission
   confirmed.
7. Backup register from `CBCRD09J` filed.
8. Online region reopened, or the hold recorded with the duty manager's name.
