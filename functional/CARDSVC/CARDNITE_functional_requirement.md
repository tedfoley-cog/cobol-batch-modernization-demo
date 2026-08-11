# CARDNITE stream — functional requirements

Stream-level functional requirement set for the CARDNITE nightly cycle (CARDSVC module), derived from source. This is the business sign-off oracle for the stream; per-program FR documents are produced later and will link from the program index (§7).

Companion document: `functional/CARDSVC/CARDNITE_analysis.md` (estate analysis, DAG, surfaces).
Scope boundary: PARTYRSK/PRBRSK1 behavior is **out of scope**; the XMOD/RSKRECAL crossing appears only as a caller-contract requirement (CARDNITE-FR-013).

Requirement ID scheme: `CARDNITE-FR-###` (stable; per-program FRs will reference these IDs).

---

## 1. Functional requirements

Each requirement is stated business trigger → job-visible result, cited at `file:line-range`, with acceptance criteria. Mechanics, routing, formatting and unreachable behavior are **not** listed here — see §3.

### Cycle entry and control

**CARDNITE-FR-001 — Nightly cycle opens only after online close.**
When the CICS card region has closed (condition `CICS-CARD-CLOSED`), the nightly cycle starts with CBCRD01J; the cycle is ordered every day except Sunday (`CALENDAR=ALLDAY`, `NOT-CALENDAR=WEEKSUN`) at 17:30 and must complete before online open at 06:00 (`sched/CARDNITE.sched:17-27,43,397-400`).
*Acceptance criteria:*
- The first job does not start while the online region is open.
- A cycle-control record for cycle type CARDNITE and the cycle date is created/opened with counters zeroed (`app/cardsvc/cbl/CBCRD01.cbl:305-312`).

**CARDNITE-FR-002 — A failed cycle is held, never skipped or cancelled.**
When any job in the chain ends NOTOK, the cycle is placed in `CARDNITE-HELD` and stops; the remaining jobs do not run and the cycle is resumed by operators, not re-ordered (`sched/CARDNITE.sched:403-408`). Exception: a CBCRD09J (backup) failure still posts its successor so the close can run — see CARDNITE-FR-018 (`sched/CARDNITE.sched:350-357`).
*Acceptance criteria:*
- After a mid-chain failure, no downstream job runs until the failure is resolved — except a CBCRD09J (backup) failure, which still posts its successor (see CARDNITE-FR-018).
- The cycle-control record retains status and restart position (`app/cpy/CVCTRL01Y.cpy:6-34`).

### Authorization intake (CBCRD01J → CBCRD03J)

**CARDNITE-FR-003 — All of the day's authorizations are extracted for settlement.**
When the cycle opens, every record on the authorization log (VSAM ESDS AUTHLOG) is extracted to the day's extract generation (AUTHEXTR GDG, FB 300) with cycle date/id stamped on each record (`app/jcl/cardsvc/CBCRD01J.jcl:32-43`, `app/cardsvc/cbl/CBCRD01.cbl:48`, layout `app/cardsvc/cpy/CVAXTR01Y.cpy:13-20`).
*Acceptance criteria:*
- Extract record count equals AUTHLOG record count minus explicitly skipped records; skips surface as RC 4 (`CBCRD01J.jcl:15-19`).
- Zero records selected surfaces as RC 8, which the scheduler maps to NOTOK for CBCRD01J — operations are notified, `CARDNITE-HELD` is set and the cycle stops (`app/jcl/cardsvc/CBCRD01J.jcl:18`, `sched/CARDNITE.sched:47-53`).

**CARDNITE-FR-004 — Extracted authorizations are edited and rejects split out under a duty-manager tolerance.**
When the extract is processed, each record is validated (auth-type overlay decode + range checks) and written to the clean file or the reject file with a reason code R001–R010; the job ends RC 4 when rejects are within the PARM tolerance (standing 02 percent) and RC 8 — cycle held — when over it (`app/jcl/cardsvc/CBCRD02J.jcl:8-25,34-50`, `app/cardsvc/cbl/CBCRD02.cbl:240-241`, reason codes `app/cardsvc/cpy/CVRJCT01Y.cpy:13-22`, `docs/runbook-cardnite.md:76-77`).
*Acceptance criteria:*
- clean + reject counts = records read; every reject carries a reason code.
- Tolerance is taken from the PARM (`'&CYCDATE,&TOLER'`), not hard-coded.
- Over-tolerance stops the stream (cycle held).

**CARDNITE-FR-005 — Clean authorizations are sequenced and enriched with merchant/settlement routing before posting.**
When the clean file exists, it is sorted into card/date/sequence order (rejected records dropped), then each record is enriched with MCC, acquirer and settlement route from the merchant reference (VSAM MERCHRTE + `CARDSVC.MERCHANT`); unmatched merchants take the Settlements-owned defaults from control cards (`DEFAULT-ROUTE=DFLT`, `DEFAULT-ACQID=…`) and surface as RC 4 (`app/jcl/cardsvc/CBCRD03J.jcl:16-26,37-58,65-90`, `app/cardsvc/cbl/CBCRD03.cbl:389`).
*Acceptance criteria:*
- Output is in posting sequence; no rejected record reaches posting.
- Defaulted-merchant count is reported; missing default card is RC 8.

### Posting (CBCRD04J)

**CARDNITE-FR-006 — Every enriched authorization is posted exactly once to the account ledger.**
When the enriched file is processed, each authorization creates a `CARDSVC.TRANSACTION` row, updates `CARDSVC.ACCOUNT` balances and `CARDSVC.CARD_LIMIT` exposure, and marks the source `CARDSVC.AUTHORIZATION` row posted; unpostable records are bypassed to a reject file rather than failing the run (`app/jcl/cardsvc/CBCRD04J.jcl:41-54`, `app/cardsvc/cbl/CBCRD04.cbl:458,730,816,850`).
*Acceptance criteria:*
- posted + bypassed = records read; bypasses surface as RC 4, "too many" as RC 8 (`CBCRD04J.jcl:19-23`).
- A transaction is never posted twice for the same authorization (duplicate insert in the checkpoint window is treated as already done, `CBCRD04.cbl:779`).

**CARDNITE-FR-007 — Posting is restartable from the last committed checkpoint without re-posting.**
When posting fails mid-run and is restarted with `RESTART=Y`, processing resumes after the last committed key held in the cycle-control record (`CC-LAST-KEY`), committing every `CC-COMMIT-FREQ` records (operator-tunable; default 1000 when zero) (`CBCRD04.cbl:19-25,334-348,901-925`, `app/cpy/CVCONSTY.cpy:37`, `docs/runbook-cardnite.md:114-136`).
*Acceptance criteria:*
- A warm restart produces the same end-state as an uninterrupted run (no double-posting, no gaps).
- Restart with a key not on the input fails hard (U0404) rather than posting from the top.
- More than 3 restarts of one cycle requires intervention (`CC-RESTART-CNT` rule, `docs/runbook-cardnite.md:114-136`).

### Fee and interest branches (CBCRD05AJ ∥ CBCRD05BJ)

**CARDNITE-FR-008 — Cycle fees are assessed per the fee schedule, once per cycle.**
When posting completes, each account is evaluated for annual, late, over-limit, cash-advance and foreign-transaction fees against `CARDSVC.FEE_SCHEDULE`; each assessed fee inserts a `CARDSVC.TRANSACTION` and updates `CARDSVC.ACCOUNT` (`app/jcl/cardsvc/CBCRD05AJ.jcl:36-55`, `app/cardsvc/cbl/CBCRD05A.cbl:243-262,601,647`; handlers `CBFEE01/02/03`, rates and caps entirely on FEEPARM control cards, `app/cardsvc/cbl/CBFEE01.cbl:15,43`).
*Acceptance criteria:*
- Fee amounts, thresholds, caps and waiver rules come from FEEPARM cards / FEE_SCHEDULE, never from code.
- A fee type with no resolvable handler is treated as a declined fee: nothing is posted, the branch continues and ends RC 4 with flag 'A' — the join proceeds (`app/cardsvc/cbl/CBCRD90.cbl:115-120`, `CBCRD05A.cbl:522-530,728-732`). The JCL comment claiming RC 8/flag 'F' is a divergence — see §5.3 item 8.
- Re-run safety: the JCL restart comment claims the program deletes the cycle's fee rows before assessing (`CBCRD05AJ.jcl:28-31`), but no DELETE exists in CBCRD05A — see §5.3 item 9. The migration must provide same-cycle idempotency and treat the legacy re-run claim as unverified.

**CARDNITE-FR-009 — Interest accrues on average daily balance at the account's APRs, and rewards accrue, once per cycle.**
In parallel with fees, interest is accrued per account on ADB at the account APR with cash advances at the cash APR, using the Finance/Audit-agreed rounding convention from control cards (`ROUNDING=HALFUP`, change-record controlled); rewards are accrued to `CARDSVC.REWARDS` (`app/jcl/cardsvc/CBCRD05BJ.jcl:5-18,53-59`, `app/cardsvc/cbl/CBCRD05B.cbl:251,487-488,544,676,723,798-852`).
*Acceptance criteria:*
- Accounts with no APR on file are skipped and counted (RC 4), not defaulted.
- An unsupported rounding value or control-card error stops the branch (RC 8, flag posted failed) — no accrual under an unagreed convention (`CBCRD05BJ.jcl:20-25`).
- Re-run safety: the JCL restart comment claims prior interest rows are deleted first (`CBCRD05BJ.jcl:27-30`), but no DELETE exists in CBCRD05B — see §5.3 item 9. The migration must provide same-cycle idempotency; the legacy claim is unverified.

**CARDNITE-FR-010 — Exposure refresh runs only after both branches have completed successfully.**
When the fee and interest branches finish, each posts a completion flag on the cycle-control record ('A'/'B' complete, 'F' failed); the exposure job verifies both flags itself (it does not trust the scheduler), waits up to the PARM poll limit (JCL standing value WAIT=030; the program header claims the standard schedule passes WAIT=000 — the JCL is authoritative, see §5.3 item 10), and stops the cycle if a branch is incomplete or failed (`app/cardsvc/cbl/CBCRD05A.cbl:710-747`, `CBCRD05B.cbl:943-977`, `CBCRD06W.cbl:28-33,137-141,179-180,205-243`, `app/jcl/cardsvc/CBCRD06J.jcl:10-14,27-29`).
*Acceptance criteria:*
- Exposure refresh never runs on a cycle where either branch failed or did not complete.
- A late branch within the wait limit is tolerated; past the limit the cycle stops (branch-specific abend).

### Exposure and risk refresh (CBCRD06J)

**CARDNITE-FR-011 — Every party with cycle activity is selected exactly once for risk recalculation.**
When both branches are complete, a party work list is built from accounts with cycle activity (`CARDSVC.ACCOUNT` with a `CARDSVC.TRANSACTION` existence test), then sorted and de-duplicated so each party appears once (`app/jcl/cardsvc/CBCRD06J.jcl:62-90`, `app/cardsvc/cbl/CBCRD06A.cbl:150-155`, `CBCRD06X.cbl:44-52`; layout `app/cardsvc/cpy/CVPWRK01Y.cpy:7-12`).
*Acceptance criteria:*
- No duplicate party ids on the sorted work list.
- Work-list records carry party/account keys and COMP-3 balances per CVPWRK01Y (LRECL 150).

**CARDNITE-FR-012 — Each party's exposure is recalculated by the external risk service and the outcome is captured per party.** *(caller contract — see FR-013)*
For each party on the work list, a risk recalculation request is dispatched and the returned risk score, band (A/B/C/X), KYC status, sanction flag and exposure are recorded in the party outcome; outcomes route the party to the accepted file (RC 0/4) or the manual-review file (RC 8) (`app/cardsvc/cbl/CBCRD06B.cbl:6-25,300-341,394-429`).
*Acceptance criteria:*
- accepted + review = parties dispatched; RC 4 parties also appear on the exception listing.
- A fatal per-party outcome (RC 12) stops the step; partial results up to the last commit stand.

**CARDNITE-FR-013 — Risk recalculation crosses to PARTYRSK under a fixed, versioned contract.**
The crossing is: CBCRD06B → CBCRD90 (route XMOD/RSKRECAL, call type 'D') → PRBRSK1, exchanging the 512-byte CVRISK01Y area, version 0003, request type 'RCAL', channel 'B'; the version is checked on return and a down-level area is fatal (U0607) (`app/cardsvc/cbl/CBCRD06B.cbl:348-361,376-381`, `db2/ddl/40_SEED_PGM_ROUTE.sql:42`, `app/cpy/CVRISK01Y.cpy:8-14`; PRBRSK1 rejects non-RCAL / non-batch-channel requests RC 12, `app/partyrsk/cbl/PRBRSK1.cbl:239-258` — internals out of scope).
*Acceptance criteria:*
- The request area carries exactly the CVRISK01Y v0003 fields (ids, exposure capped at S9(9)V99, currency, country, request type, channel, hop trace).
- A commarea version mismatch is fatal, never silently coerced.
- The migrated caller must not depend on PARTYRSK internals — only on this contract.

**CARDNITE-FR-014 — Manual-review parties are excluded from limit updates and handed to Financial Crime.**
When a party returns RC 8 (sanctions/KYC serious condition), it is written to the review file, excluded from the accepted file, and the review listing is printed for Financial Crime the same morning; the step ends RC 8 (a business outcome) and the apply step still runs for accepted parties (`app/cardsvc/cbl/CBCRD06B.cbl:16-25`, `app/jcl/cardsvc/CBCRD06J.jcl:132-157`, `docs/runbook-cardnite.md:79-80,138-162`).
*Acceptance criteria:*
- No reviewed party's CARD_LIMIT is updated in that cycle.
- Reviewed parties stay excluded until Financial Crime releases them; re-running the dispatch step alone must not clear the RC 8 (`docs/runbook-cardnite.md:150-159`).

**CARDNITE-FR-015 — Accepted risk outcomes update card limits.**
When the accepted file exists, each accepted party's risk band and exposure are applied to `CARDSVC.CARD_LIMIT` via positioned update, and a band-movement report is produced (`app/cardsvc/cbl/CBCRD06C.cbl:222-224,442-452`, `app/jcl/cardsvc/CBCRD06J.jcl:139-149`).
*Acceptance criteria:*
- Only records with accepted per-party RC (0/4) are applied.
- Applied count equals accepted-file record count minus explicitly skipped records.

### Downstream financials (CBCRD07J, CBCRD08J)

**CARDNITE-FR-016 — The general ledger feed is complete, mapped, and provably balanced — or not sent at all.**
When posting data exists, every unposted transaction (`GL_POSTED_FLG = 'N'`) is aggregated by transaction type/product into `CARDSVC.GL_POSTING` using the Finance-owned mapping cards; total debits must equal total credits to the cent before commit — on any difference the entire feed is rolled back and the job abends (U0704), leaving no partial ledger (`app/cardsvc/cbl/CBCRD07.cbl:275-276,598,647-654,699-746`, `app/jcl/cardsvc/CBCRD07J.jcl:10-24,53-70`, `docs/runbook-cardnite.md:184-196`).
*Acceptance criteria:*
- Debits = credits, always, or nothing is written.
- Unmapped combinations post to the suspense account and surface RC 4 so Finance adds a mapping card (`CBCRD07J.jcl:26-32`).
- Re-run only picks up transactions still flagged unposted (idempotent).

**CARDNITE-FR-017 — Delinquency rolls forward at most one bucket per cycle and feeds collections.**
When the ledger feed completes, each account's delinquency bucket is rolled forward at most one bucket per cycle, driven by payment due date and shortfall (not the previous bucket alone, so a same-date re-run cannot double-roll); `DELQ_AMT` is recomputed, accounts at bucket ≥ 3 are written to the collections feed (LRECL 100), and bucket-6 charge-off candidates surface as RC 4 (`app/jcl/cardsvc/CBCRD08J.jcl:5-29,34-58`, `app/cardsvc/cbl/CBCRD08.cbl:273,291,550`, `docs/runbook-cardnite.md:85-86`).
*Acceptance criteria:*
- No account moves more than one bucket in one cycle; same-date re-run is a no-op on buckets.
- The collections feed contains exactly the bucket ≥ 3 population; the feed is not transmitted when the job fails (transmission job released only on RC ≤ 4).
- Grace days come from the SYSIN card (collections policy CP-07), not code.
- Legacy 6-digit dates are windowed with the century pivot (`app/cpy/CVCONSTY.cpy:9-12`).

### Housekeeping and close (CBCRD09J, CBCRD10J)

**CARDNITE-FR-018 — Cycle datasets are backed up, verified, and the authorization log is reset empty for the next day.**
When financials complete, CYCLCTL is exported, the CARDXREF/CYCLCTL/AUTHLOG clusters are copied to backup generations, AUTHLOG is deleted and redefined (ESDS, RECORDSIZE 200), and a verify step proves the backups are non-empty and the redefined AUTHLOG is empty — an empty backup or a non-empty log is fatal (U0901/U0902/U0903) (`app/jcl/cardsvc/CBCRD09J.jcl:8-16,34-122`, `app/cardsvc/cbl/CBCRD09.cbl:305,344,394,441-461`).
*Acceptance criteria:*
- Step order is preserved (backup strictly before delete/define); the verify step gates on it.
- A backup generation smaller than the previous one surfaces RC 4 for investigation.
- CBCRD09J failure does not hold the cycle close (`sched/CARDNITE.sched:350-357`) — but the failure is reported.

**CARDNITE-FR-019 — The cycle closes with a control report, and the online region reopens only on a provable, complete cycle.**
At end of cycle, control totals are reconciled (transaction counts, GL totals, delinquency counts vs. the cycle-control record and stage statistics), a control report is produced and kept, the cycle-control record is closed, and the region-open decision is recorded in both the step RC and the control record: RC 0/4 releases `CARDNITE-COMPLETE` + `CICS-CARD-OPEN-OK`; RC 8 (a stage missing or ledger unproven) holds the region closed (`app/jcl/cardsvc/CBCRD10J.jcl:14-27,36-62`, `app/cardsvc/cbl/CBCRD10.cbl:9,325-331,527-602,713-717`, `docs/runbook-cardnite.md:172-177`, `sched/CARDNITE.sched:375-389`).
*Acceptance criteria:*
- The region never reopens on an incomplete or out-of-balance cycle.
- The close report totals reconcile to the cycle-control counters.
- All per-job `*-OK` conditions are deleted at completion so the next day starts clean.

---

## 2. Requirement-level RC map

Estate convention (all jobs): 0000 clean / 0004 warning-partial / 0008 business decline-serious / 0012 fatal (`docs/runbook-cardnite.md:63-73`, `app/cpy/CVCONSTY.cpy:39-42`). Job-specific meanings are inside each FR's acceptance criteria; divergences are in §5.3.

---

## 3. Supporting sections (explicitly NOT functional requirements)

### 3.1 Mechanics and routing (technical, preserved by design not by FR)
- CBCRD90 dispatcher mechanics: PGM_ROUTE resolution, VSAM PGMROUT fallback, 200-entry route cache, `CANCEL` after every call, fallback program retry, ROUTE_AUDIT rows (`app/cardsvc/cbl/CBCRD90.cbl:70-149,217-261,294-375`). The *business* content is only "the configured handler for the fee type / risk route is invoked" (FR-008, FR-013).
- Branch flags carried in `CC-FILLER` bytes (undocumented overlay, `app/cpy/CVCTRL01Y.cpy:41`) — the requirement is the gate (FR-010), not the byte layout.
- CBCRD06W's one-minute CPU-spin polling loop (`CBCRD06W.cbl:205-215`) — the requirement is the bounded wait, not the spin implementation.
- DYNAM/NODYNAM build split, STEPLIB concatenation, plan/package bind structure (`app/jcl/build/BUILDALL.jcl:159-195`, `BINDCARD.jcl`).
- Commit frequency plumbing (`CC-COMMIT-FREQ` fallback to 1000) — the requirement is restartability (FR-007), the interval is tunable mechanics.
- PRBRSK1 commit-interval degradation under the cancelling driver (analysis §7.1) — cross-module mechanics, documented as a migration note only.

### 3.2 Formatting / reports
Report layouts (all SYSOUT FBA 133: reject listing, bypass listing, FEEAUDIT, INTRPT, RISKEXC, BANDRPT, GLRPT, AGERPT, BKPREG, CTLRPT) and the morning filing checklist (`docs/runbook-cardnite.md:221-232`) are surface spec (§4), not functional requirements; column-level layouts belong to the per-program FR docs.

### 3.3 Unreachable / dead-vs-source items (do not implement without a decision)
- **FORCEOPEN**: exists only in scheduler operator note 4 (`sched/CARDNITE.sched:416-419`); CBCRD10 parses only the 8-byte cycle date (`app/cardsvc/cbl/CBCRD10.cbl:325-331`). Not a requirement; the CBCRD10 per-program FR doc must resolve implemented-or-dropped.
- CBCRD06W abend labels U0602/U0603: JCL/runbook only; the program raises U0601/U0610/U0611/U0612 (`CBCRD06W.cbl:28-33`).
- Scheduler ABEND-COND codes U4001–U4010: match nothing in the source (analysis §8.2).
- **CBCRD06B U0605 (route unresolvable) is unreachable**: on a not-found route CBCRD90 sets only `LK-RETURN-CD` = 8 and exits before `3000-DISPATCH` (`app/cardsvc/cbl/CBCRD90.cbl:115-121`), while `RQ-RC` is written only inside `3000-DISPATCH` (`CBCRD90.cbl:307`) or as 0012 on table error (`CBCRD90.cbl:146-149`); CBCRD06B's check tests `RQ-RC-NOT-FOUND OR RQ-RC-TABLE-ERROR` (`CBCRD06B.cbl:367-374`), so a missing XMOD route leaves `RQ-RC` at zero and U0605 never fires. The table-unavailable case is equally dead: CBCRD90 exits with `LK-RETURN-CD` = 12 before the `ROUTE-REQUEST` copy-back (`CBCRD90.cbl:108-113,126`), so `RQ-RC-TABLE-ERROR` is never visible to the caller either (there it folds in as RC 12 → fatal, but via U0606, not U0605). The actual outcome: CBCRD06B folds the dispatcher RC into the risk area (`CBCRD06B.cbl:383-388` takes the higher of the two), so `CV-RISK-RC` becomes 8 and `2400-HANDLE-PARTY-RC` sends every party to manual review with an unpopulated risk area — review file + exception listing, step RC 8 (`CBCRD06B.cbl:394-421`). A latent defect to resolve (not replicate) in the migration.

---

## 4. Batch surface / field spec (stream level)

### 4.1 Input/output datasets
Authoritative dataset table (DSN, LRECL, producer → consumer) is analysis §4.4; VSAM clusters §4.3; Db2 read/write matrix §4.2. Stream-level FR-relevant surfaces:

| Surface | Spec | FR |
|---|---|---|
| AUTHLOG (VSAM ESDS, 200) | day's authorizations in; reset empty at close | FR-003, FR-018 |
| AUTHEXTR→AUTHCLN/AUTHREJ→AUTHSRT→AUTHENR GDGs (FB 300, CVAXTR01Y) | intake pipeline | FR-003–FR-006 |
| POSTREJ GDG (FB 300) | posting bypasses | FR-006 |
| PARTYWK/PARTYSRT/PARTYACC/PARTYRVW GDGs (FB 150, CVPWRK01Y) | risk work lists | FR-011–FR-015 |
| COLLECT GDG (FB 100) | collections feed, bucket ≥ 3 | FR-017 |
| BKP.* GDGs (128/256/200) + EXPORT.CYCLCTL | backups | FR-018 |
| CTLRPT GDG (133) + STGSTAT (120, external producer) | close report inputs/outputs | FR-019 |
| CYCLCTL (VSAM KSDS 256, CVCTRL01Y) | cycle state, checkpoints, branch flags | FR-001, FR-007, FR-010, FR-019 |
| Db2 CARDSVC: ACCOUNT, CARD, CARD_LIMIT, AUTHORIZATION, TRANSACTION, MERCHANT, FEE_SCHEDULE, REWARDS, GL_POSTING, PGM_ROUTE, ROUTE_AUDIT (`db2/ddl/10_CARDDB_TABLES.sql`) | rows written per FR as cited | FR-006–FR-017 |

### 4.2 Per-job PARM contracts

| Job | PARM | Source |
|---|---|---|
| CBCRD01J | `'&CYCDATE,&CYCID'` | `app/jcl/cardsvc/CBCRD01J.jcl:12-13,32-33` |
| CBCRD02J | `'&CYCDATE,&TOLER'` (tolerance NN%, standing 02) | `CBCRD02J.jcl:13-18,34-35` |
| CBCRD03J | cycle date only (DSN RUN PARM) | `CBCRD03J.jcl:69` |
| CBCRD04J | `CCYYMMDD[,RESTART=Y\|N]` (safety-critical cold/warm switch) | `CBCRD04J.jcl:14-17,41-47` |
| CBCRD05AJ / CBCRD05BJ | cycle date only | `CBCRD05AJ.jcl:47`, `CBCRD05BJ.jcl:40` |
| CBCRD06J STEP005 | `'&CYCDATE,WAIT=NNN'` (standing 030) | `CBCRD06J.jcl:27-29,50-51` |
| CBCRD06J STEP010–040 | cycle date only | `CBCRD06J.jcl:66,113` |
| CBCRD07J–CBCRD10J | cycle date only | `CBCRD07J.jcl:47`, `CBCRD08J.jcl:38`, `CBCRD09J.jcl:113`, `CBCRD10J.jcl:40` |

### 4.3 Business-parameter control cards (not code)
- CBCRD03: `DEFAULT-ROUTE`, `DEFAULT-ACQID` (Settlements) — `CBCRD03J.jcl:85-90`.
- CBCRD05B: `ROUNDING=HALFUP`, `CYCLE-DAYS`, `GRACE-DAYS` (Finance/Audit) — `CBCRD05BJ.jcl:53-59`.
- CBFEE01/02/03: FEEPARM rate/threshold/cap/waiver cards — `CBFEE01.cbl:15,43`.
- CBCRD07: `BATCH=`, `SUSPENSE=`, `MAP …` GL mapping cards (Finance) — `CBCRD07J.jcl:53-70`.
- CBCRD08: `GRACE-DAYS=005` (collections policy CP-07) — `CBCRD08J.jcl:13-14,49-53`.

---

## 5. Validation and error catalogue

### 5.1 Edits and rejects
- Intake edits: auth-type overlay decode + range checks; reject reasons R001–R010 (`CVRJCT01Y.cpy:13-22`) — FR-004.
- Enrichment: unmatched merchant → defaults + warning count (FR-005).
- Posting: unpostable → bypass file, not failure; excess bypasses → RC 8 (FR-006).
- Risk: per-party RC 0/4/8/12 (FR-012/014); commarea version check U0607 (FR-013).
- Interest: unsupported rounding / card error → RC 8, branch flagged failed (FR-009).
- GL: unmapped → suspense + RC 4; imbalance → U0704 full rollback (FR-016).

### 5.2 Tolerances
- Reject tolerance: PARM `&TOLER` whole percent of records read (FR-004).
- Join wait: `WAIT=NNN` minutes (FR-010).
- Backup shrinkage: RC 4 investigation flag (FR-018).
- Restart count: `CC-RESTART-CNT` ≤ 3 before escalation (FR-007).

### 5.3 RC semantics and scheduler/program divergences (source authoritative)
1. **CBCRD02 RC 4**: program/JCL mean "rejects within tolerance" (`CBCRD02J.jcl:19-25`), but the scheduler maps RC 4 to outcome `EMPTY` ("extracted zero records") (`sched/CARDNITE.sched:75-86`) — wrong label; both post the successor, but the operator notification text is misleading.
2. **CBCRD08 RC 8 "OUTOFBAL"**: the scheduler maps CBCRD08J RC 8 / abend U4010 to `OUTOFBAL` "GL OUT OF BALANCE - DO NOT SEND" (`sched/CARDNITE.sched:317-330`), but CBCRD08 RC 8 actually means "no accounts selected or grace card rejected" (`CBCRD08J.jcl:16-21`); GL out-of-balance is CBCRD07's U0704 abend, not an RC 8 on CBCRD08J. The scheduler row also carries the stale swapped DESC (analysis §8.1).
3. **CBCRD10 FORCEOPEN**: scheduler-only; not parsed by the program (§3.3).
4. **CBCRD06W U0602/U0603**: JCL/runbook labels for abends the program raises as U0610/U0611/U0612 (§3.3).
5. **CBCRD06B step gate**: program header says STEP040 gate "IF RC <= 4" (`CBCRD06B.cbl:24-25`); the JCL's authoritative gate is `IF RC <= 8` (`CBCRD06J.jcl:138`) so accepted parties proceed on a manual-review RC 8.
6. **Runbook U0632 vs source U0606** for the crossing-fatal abend (`docs/runbook-cardnite.md:103,148` vs `CBCRD06B.cbl:43`).
7. Scheduler ABEND-COND U4001–U4010 codes match nothing in the source (analysis §8.2).
8. **CBCRD05A unroutable fee type**: the JCL comment says RC 8 / flag 'F' / join held (`CBCRD05AJ.jcl:21-26`), but the source treats a dispatcher 'ROUTE NOT FOUND' (RC 8, `CBCRD90.cbl:115-120`) as a declined fee — warning only, step RC 4, flag 'A', join proceeds (`CBCRD05A.cbl:522-530,728-732`); only handler RC 12 abends (U0505, `CBCRD05A.cbl:531-536`). Consequence: RC 8 is unreachable in CBCRD05A, and a missing fee handler load module is indistinguishable from a legitimately declined fee — fees silently go unassessed while the branch posts 'A' and the join proceeds. Same latent-defect class as the U0605 item in §3.3; the migration should resolve (not replicate) it.
9. **CBCRD05A/05B re-run deletion**: the JCL restart comments claim each branch deletes its cycle-date rows before starting (`CBCRD05AJ.jcl:28-31`, `CBCRD05BJ.jcl:27-30`), but neither program contains a DELETE — legacy same-cycle re-run safety is unverified; the migration must implement idempotency explicitly (FR-008/FR-009).
10. **CBCRD06W standing WAIT**: the JCL passes `WAIT=030` (`CBCRD06J.jcl:51`); the program header claims the standard schedule passes `WAIT=000` (single check, no wait) (`CBCRD06W.cbl:22-24`) — JCL authoritative.

### 5.4 User abends by job
Source-derived abend inventory per program is in analysis §3 (U0101–U1003); the runbook table (`docs/runbook-cardnite.md:91-112`) is the operator view, with the divergences above.

---

## 6. Traceability matrix

Covering test and verification case columns to be filled by the migration waves.

| Requirement | Source cite (primary) | Covering test | Verification case |
|---|---|---|---|
| CARDNITE-FR-001 | `sched/CARDNITE.sched:17-27,43`; `CBCRD01.cbl:305-312` | _(wave)_ | region closed → cycle opens; record initialized |
| CARDNITE-FR-002 | `sched/CARDNITE.sched:403-408` | _(wave)_ | mid-chain NOTOK → downstream held |
| CARDNITE-FR-003 | `CBCRD01J.jcl:32-43`; `CBCRD01.cbl:48` | _(wave)_ | extract count reconciles; empty log → RC 8, cycle held |
| CARDNITE-FR-004 | `CBCRD02J.jcl:8-25`; `CVRJCT01Y.cpy:13-22` | _(wave)_ | tolerance boundary RC 4/8; reason codes |
| CARDNITE-FR-005 | `CBCRD03J.jcl:37-90`; `CBCRD03.cbl:389` | _(wave)_ | sequence order; defaulting; RC 4 count |
| CARDNITE-FR-006 | `CBCRD04J.jcl:41-54`; `CBCRD04.cbl:730,850` | _(wave)_ | post-once; bypass accounting |
| CARDNITE-FR-007 | `CBCRD04.cbl:334-348,901-925` | _(wave)_ | kill/restart equivalence; bad key U0404 |
| CARDNITE-FR-008 | `CBCRD05AJ.jcl:28-55`; `CBCRD05A.cbl:601,647` | _(wave)_ | fee idempotency (new — legacy unverified, §5.3 item 9); card-driven rates; unroutable fee → RC 4 declined |
| CARDNITE-FR-009 | `CBCRD05BJ.jcl:27-30,53-59`; `CBCRD05B.cbl:676-852` | _(wave)_ | ADB/APR math, HALFUP; re-run idempotency (new — legacy unverified, §5.3 item 9) |
| CARDNITE-FR-010 | `CBCRD05A.cbl:710-747`; `CBCRD05B.cbl:943-977`; `CBCRD06W.cbl:179-243` | _(wave)_ | late/failed branch gating |
| CARDNITE-FR-011 | `CBCRD06A.cbl:150-155`; `CBCRD06X.cbl:44-52` | _(wave)_ | uniqueness; population match |
| CARDNITE-FR-012 | `CBCRD06B.cbl:16-25,394-429` | _(wave)_ | outcome routing per RC |
| CARDNITE-FR-013 | `CBCRD06B.cbl:348-381`; `CVRISK01Y.cpy:8-14`; `40_SEED_PGM_ROUTE.sql:42` | _(wave)_ | contract-only stub; version mismatch fatal |
| CARDNITE-FR-014 | `CBCRD06B.cbl:16-25`; `CBCRD06J.jcl:132-157` | _(wave)_ | review exclusion persists; step-alone rerun forbidden |
| CARDNITE-FR-015 | `CBCRD06C.cbl:222-224,442-452` | _(wave)_ | accepted-only application |
| CARDNITE-FR-016 | `CBCRD07.cbl:275-276,699-746`; `CBCRD07J.jcl:53-70` | _(wave)_ | balance proof; suspense RC 4; rollback on imbalance |
| CARDNITE-FR-017 | `CBCRD08J.jcl:5-29`; `CBCRD08.cbl:273-550` | _(wave)_ | one-bucket rule; re-run no-op; feed population |
| CARDNITE-FR-018 | `CBCRD09J.jcl:8-16,34-122`; `CBCRD09.cbl:305-461` | _(wave)_ | backup-before-reset; empty-log proof |
| CARDNITE-FR-019 | `CBCRD10J.jcl:14-27`; `CBCRD10.cbl:527-717`; `sched:375-389` | _(wave)_ | region gate on RC 8; totals reconcile; condition cleanup |

---

## 7. Program index (programs → requirements they own)

| Program | FR doc | Owns / contributes to |
|---|---|---|
| CBCRD01 | [CBCRD01_FR.md](programs/CBCRD01_FR.md) | FR-001, FR-003 |
| CBCRD02 | [CBCRD02_FR.md](programs/CBCRD02_FR.md) | FR-004 |
| DFSORT step + CBCRD03 | [CBCRD03_FR.md](programs/CBCRD03_FR.md) | FR-005 |
| CBCRD04 | [CBCRD04_FR.md](programs/CBCRD04_FR.md) | FR-006, FR-007 |
| CBCRD05A (+CBCRD90, CBFEE01/02/03) | [CBCRD05A_FR.md](programs/CBCRD05A_FR.md) | FR-008, FR-010 (flag) |
| CBCRD05B | [CBCRD05B_FR.md](programs/CBCRD05B_FR.md) | FR-009, FR-010 (flag) |
| CBCRD06W | [CBCRD06W_FR.md](programs/CBCRD06W_FR.md) | FR-010 (gate) |
| CBCRD06A / CBCRD06X | [CBCRD06A_FR.md](programs/CBCRD06A_FR.md) / [CBCRD06X_FR.md](programs/CBCRD06X_FR.md) | FR-011 |
| CBCRD06B (+CBCRD90) | [CBCRD06B_FR.md](programs/CBCRD06B_FR.md) | FR-012, FR-013, FR-014 |
| CBCRD06C | [CBCRD06C_FR.md](programs/CBCRD06C_FR.md) | FR-015 |
| CBCRD07 | [CBCRD07_FR.md](programs/CBCRD07_FR.md) | FR-016 |
| CBCRD08 | [CBCRD08_FR.md](programs/CBCRD08_FR.md) | FR-017 |
| IDCAMS ×3 + CBCRD09 | [CBCRD09_FR.md](programs/CBCRD09_FR.md) | FR-018 |
| CBCRD10 | [CBCRD10_FR.md](programs/CBCRD10_FR.md) | FR-019, FR-001/002 (conditions) |
| CBCRD90 | [CBCRD90_FR.md](programs/CBCRD90_FR.md) | mechanics only (§3.1); contract carrier for FR-008/FR-013 |
| CBCRD91 (absent — analysis §8) | [CBCRD91_FR.md](programs/CBCRD91_FR.md) | fatal-path mechanics for all FRs; contract reconstructed as ErrorReporter |
| CBFEE01 / CBFEE02 / CBFEE03 | [CBFEE01_FR.md](programs/CBFEE01_FR.md) / [CBFEE02_FR.md](programs/CBFEE02_FR.md) / [CBFEE03_FR.md](programs/CBFEE03_FR.md) | FR-008 (handlers) |
| CYCLCTL cycle-control contract | [CYCLCTL_contract_FR.md](programs/CYCLCTL_contract_FR.md) | FR-002, FR-007, FR-010, FR-018, FR-019 (state carrier) |
| XMOD/RSKRECAL caller contract | [RSKRECAL_contract_FR.md](programs/RSKRECAL_contract_FR.md) | FR-012, FR-013, FR-014 (contract only) |
| Scheduler table CARDNITE | [CARDNITE_scheduler_FR.md](programs/CARDNITE_scheduler_FR.md) | FR-001, FR-002, FR-019 |

19 functional requirements total (CARDNITE-FR-001 … CARDNITE-FR-019).
