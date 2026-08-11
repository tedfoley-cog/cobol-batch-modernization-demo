# CARDNITE stream analysis

Analysis of the CARDNITE nightly cycle (CARDSVC module) for migration planning.
Analysis only — no functional requirements, no target design decisions.

* Stream: **CARDNITE** nightly authorization settlement and posting cycle. Process type: **BATCH**.
* Entry point: scheduler table `CARDNITE` (`sched/CARDNITE.sched:17-27`), ordered every day except Sunday (`CALENDAR=ALLDAY`, `NOT-CALENDAR=WEEKSUN`) at 17:30, gated on `CICS-CARD-CLOSED` (`sched/CARDNITE.sched:43`), must complete before online open at 06:00 (`sched/CARDNITE.sched:397-400`).
* Job chain: CBCRD01J → CBCRD02J → CBCRD03J → CBCRD04J → fork {CBCRD05AJ ∥ CBCRD05BJ} → join CBCRD06J → CBCRD07J → CBCRD08J → CBCRD09J → CBCRD10J.
* HARD STOP: the XMOD/RSKRECAL dispatch from CBCRD06B via CBCRD90 into PRBRSK1 (PARTYRSK module) is a boundary crossing. Its contract is documented in §7; PARTYRSK internals are out of scope.

Known source discrepancy: the scheduler `DESC` lines are stale relative to the source. `sched/CARDNITE.sched:270` calls CBCRD07J "INTEREST ACCRUAL ON REVOLVING BALANCES" and `sched/CARDNITE.sched:301` calls CBCRD08J "GENERAL LEDGER EXTRACT AND BALANCING", but the source and JCL are authoritative: CBCRD07 is the **general ledger feed** (`app/jcl/cardsvc/CBCRD07J.jcl:6-13`, `app/cardsvc/cbl/CBCRD07.cbl:7`) and CBCRD08 is the **delinquency roll / aging** (`app/jcl/cardsvc/CBCRD08J.jcl:6-11`). Interest accrual actually runs in the CBCRD05BJ branch (`app/jcl/cardsvc/CBCRD05BJ.jcl:5-12`). The runbook running-order table agrees with the source (`docs/runbook-cardnite.md:12-24`); the scheduler DESCs at `sched/CARDNITE.sched:230` (06J "exposure and delinquency refresh" — actually exposure/risk only) are similarly stale.

---

## 1. Program inventory

All batch programs run under plan `CARDNITP` when they touch Db2 (e.g. `app/jcl/cardsvc/CBCRD04J.jcl:42-44`). Compile is via `COBBATCH.proc` — NODYNAM default, `CALLOPT=DYNAM` where noted (`app/jcl/build/COBBATCH.proc:22-39`).

| Program | Path | Job / step | Compile | Role |
|---|---|---|---|---|
| CBCRD01 | `app/cardsvc/cbl/CBCRD01.cbl` | CBCRD01J STEP010 (`app/jcl/cardsvc/CBCRD01J.jcl:32`) | NODYNAM (`app/jcl/build/BUILDALL.jcl:159`) | Open cycle, AUTHLOG extract to AUTHEXTR GDG |
| CBCRD02 | `app/cardsvc/cbl/CBCRD02.cbl` | CBCRD02J STEP010 (`app/jcl/cardsvc/CBCRD02J.jcl:34`) | NODYNAM (`BUILDALL.jcl:160`) | Edit extract, split clean/reject |
| (DFSORT) | n/a | CBCRD03J STEP010 `PGM=SORT` (`app/jcl/cardsvc/CBCRD03J.jcl:37-58`) | utility | Sort clean file into card/date/seq order, drop rejects |
| CBCRD03 | `app/cardsvc/cbl/CBCRD03.cbl` | CBCRD03J STEP020 (`CBCRD03J.jcl:65-79`) | NODYNAM (`BUILDALL.jcl:161`) | Merchant/MCC/acquirer/settlement-route enrichment |
| CBCRD04 | `app/cardsvc/cbl/CBCRD04.cbl` | CBCRD04J STEP010 (`app/jcl/cardsvc/CBCRD04J.jcl:41-47`) | NODYNAM (`BUILDALL.jcl:162`) | Posting engine, checkpoint/restart |
| CBCRD05A | `app/cardsvc/cbl/CBCRD05A.cbl` | CBCRD05AJ STEP010 (`app/jcl/cardsvc/CBCRD05AJ.jcl:36-48`) | **DYNAM** (`BUILDALL.jcl:192`) | Fee assessment branch; dispatches FEEC routes via CBCRD90 |
| CBCRD05B | `app/cardsvc/cbl/CBCRD05B.cbl` | CBCRD05BJ STEP010 (`app/jcl/cardsvc/CBCRD05BJ.jcl:32-40`) | NODYNAM (`BUILDALL.jcl:163`) | Interest (ADB at account APR, cash APR separate) + rewards branch |
| CBCRD06W | `app/cardsvc/cbl/CBCRD06W.cbl` | CBCRD06J STEP005 (`app/jcl/cardsvc/CBCRD06J.jcl:50-51`) | NODYNAM (`BUILDALL.jcl:164`) | Join guard: verify both branch flags, poll/abend |
| CBCRD06A | `app/cardsvc/cbl/CBCRD06A.cbl` | CBCRD06J STEP010 (`CBCRD06J.jcl:62-66`) | NODYNAM (`BUILDALL.jcl:165`) | Assemble party work list (PARTYWK) |
| CBCRD06X | `app/cardsvc/cbl/CBCRD06X.cbl` | CBCRD06J STEP020 (`CBCRD06J.jcl:78-90`) | NODYNAM (`BUILDALL.jcl:166`) | Internal SORT + de-duplicate work list (PARTYSRT) |
| CBCRD06B | `app/cardsvc/cbl/CBCRD06B.cbl` | CBCRD06J STEP030 (`CBCRD06J.jcl:102-113`) | **DYNAM** (`BUILDALL.jcl:193`) | Dispatch exposure recalculation (XMOD/RSKRECAL crossing) |
| CBCRD06C | `app/cardsvc/cbl/CBCRD06C.cbl` | CBCRD06J STEP040 (`CBCRD06J.jcl:139-143`) | NODYNAM (`BUILDALL.jcl:167`) | Apply returned risk outcomes to CARD_LIMIT |
| CBCRD07 | `app/cardsvc/cbl/CBCRD07.cbl` | CBCRD07J STEP010 (`app/jcl/cardsvc/CBCRD07J.jcl:43-48`) | NODYNAM (`BUILDALL.jcl:168`) | GL feed, debit=credit proof, GL_POSTING insert |
| CBCRD08 | `app/cardsvc/cbl/CBCRD08.cbl` | CBCRD08J STEP010 (`app/jcl/cardsvc/CBCRD08J.jcl:34-38`) | NODYNAM (`BUILDALL.jcl:169`) | Delinquency bucket roll, aging report, collections feed |
| (IDCAMS ×3) | n/a | CBCRD09J STEP010/020/030 (`app/jcl/cardsvc/CBCRD09J.jcl:34-107`) | utility | EXPORT CYCLCTL; REPRO 3 clusters to GDG; DELETE/DEFINE AUTHLOG |
| CBCRD09 | `app/cardsvc/cbl/CBCRD09.cbl` | CBCRD09J STEP040 (`CBCRD09J.jcl:112-113`) | NODYNAM (`BUILDALL.jcl:170`) | Verify backups, prove redefined AUTHLOG empty |
| CBCRD10 | `app/cardsvc/cbl/CBCRD10.cbl` | CBCRD10J STEP010 (`app/jcl/cardsvc/CBCRD10J.jcl:36-41`) | NODYNAM (`BUILDALL.jcl:171`) | Cycle close, control report, online-region gate |
| CBCRD90 | `app/cardsvc/cbl/CBCRD90.cbl` | called by CBCRD05A/CBCRD06B (`CBCRD90.cbl:11`) | **DYNAM** (`BUILDALL.jcl:195`) | Batch dispatcher: PGM_ROUTE resolution + dynamic CALL |
| CBCRD91 | **ABSENT** — see §8 | called by every batch program (e.g. `CBCRD01.cbl:529`) | listed NODYNAM (`BUILDALL.jcl:172`) | Batch fatal error handler / abend driver |
| CBFEE01 | `app/cardsvc/cbl/CBFEE01.cbl` | reached dynamically from CBCRD05AJ (`CBFEE01.cbl:4-6`) | NODYNAM (`BUILDALL.jcl:183`) | Annual + late payment fee handler (FEEC ANNU/LATE) |
| CBFEE02 | `app/cardsvc/cbl/CBFEE02.cbl` | reached dynamically (`CBFEE02.cbl:4-5`) | NODYNAM (`BUILDALL.jcl:184`) | Over-limit + cash advance fee handler (FEEC OVLM/CASH) |
| CBFEE03 | `app/cardsvc/cbl/CBFEE03.cbl` | reached dynamically (`CBFEE03.cbl:4-5`) | NODYNAM (`BUILDALL.jcl:185`) | Foreign transaction fee handler (FEEC FRGN) |

Compile notes:
* The CBCRD90 dispatch chain callers (CBCRD05A, CBCRD06B) and CBCRD90 itself are the only CARDNITE members built `CALLOPT=DYNAM` (`app/jcl/build/BUILDALL.jcl:190-195`). A DYNAM module resolves *all* its calls dynamically, including literal ones (`app/jcl/build/COBBATCH.proc:27-39`), so CBCRD91 must be present in a STEPLIB library at run time for these members too.
* Fee handlers live in `CARD.PROD.FEELIB`, concatenated only in CBCRD05AJ's STEPLIB (`app/jcl/cardsvc/CBCRD05AJ.jcl:37-43`); a missing handler surfaces as a route failure, not a link-edit error (`CBCRD05AJ.jcl:13-19`).
* PARTYRSK load library `PRSK.PROD.LOADLIB` is concatenated only on CBCRD06J STEP030 (`app/jcl/cardsvc/CBCRD06J.jcl:98,103-109`).
* Db2 steps run through the `CARDDB2` proc (IKJEFT01/DSN RUN, `app/jcl/proc/CARDDB2.jcl`) with plan CARDNITP; packages are bound by `app/jcl/build/BINDCARD.jcl` (CBCRD91's package at `BINDCARD.jcl:182`).

---

## 2. Dependency DAG (leaf-first)

### 2.1 Static CALL graph

Every CARDNITE batch program statically calls `CBCRD91` (fatal path only), e.g. `app/cardsvc/cbl/CBCRD01.cbl:529`, `CBCRD02.cbl:658`, `CBCRD03.cbl:556`, `CBCRD04.cbl:1027`, `CBCRD05A.cbl:770`, `CBCRD05B.cbl:1000`, `CBCRD06A.cbl:405`, `CBCRD06B.cbl:615`, `CBCRD06C.cbl:594`, `CBCRD06W.cbl:262`, `CBCRD06X.cbl:318`, `CBCRD07.cbl:821`, `CBCRD08.cbl:719`, `CBCRD09.cbl:540`, `CBCRD10.cbl:804`.

Dispatcher calls (static CALL to CBCRD90, which then calls dynamically):
* `CBCRD05A.cbl:508` — `CALL 'CBCRD90' USING ROUTE-REQUEST …` for each fee type (route type FEEC).
* `CBCRD06B.cbl:359` — `CALL 'CBCRD90' USING ROUTE-REQUEST CV-RISK-AREA WS-RETURN-AREA` per party (route XMOD/RSKRECAL).

### 2.2 CBCRD90 dynamic dispatch targets (from PGM_ROUTE seed)

CBCRD90 resolves `(ROUTE_TYPE, ROUTE_KEY, SEQ_NBR)` against `CARDSVC.PGM_ROUTE` (Db2 cursor `app/cardsvc/cbl/CBCRD90.cbl:70-82`), falling back to VSAM `PGMROUT` when Db2 is unavailable (`CBCRD90.cbl:133-146,217-261`), then issues `CALL WS-PGM-NAME` and `CANCEL`s the module after each call (`CBCRD90.cbl:294-309`). On load failure it retries the row's `FALLBACK_PGM` (`CBCRD90.cbl:311-341`). Every dispatch is audited to `CARDSVC.ROUTE_AUDIT` (`CBCRD90.cbl:343-375`).

CARDNITE-relevant seed rows (`db2/ddl/40_SEED_PGM_ROUTE.sql`):

| Route | Key | Target | Call type | Module | Seed line |
|---|---|---|---|---|---|
| FEEC | ANNU | CBFEE01 | D | CARDSVC | `40_SEED_PGM_ROUTE.sql:66` |
| FEEC | LATE | CBFEE01 | D | CARDSVC | `40_SEED_PGM_ROUTE.sql:67` |
| FEEC | OVLM | CBFEE02 | D | CARDSVC | `40_SEED_PGM_ROUTE.sql:68` |
| FEEC | CASH | CBFEE02 | D | CARDSVC | `40_SEED_PGM_ROUTE.sql:69` |
| FEEC | FRGN | CBFEE03 | D | CARDSVC | `40_SEED_PGM_ROUTE.sql:70` |
| XMOD | RSKRECAL | **PRBRSK1** (PARTYRSK — boundary) | D | PARTYRSK | `40_SEED_PGM_ROUTE.sql:42` |

The XMOD rows are the *only* place PARTYRSK entry points appear outside PARTYRSK; no CARDSVC source member names them (`40_SEED_PGM_ROUTE.sql:30-35`).

### 2.3 Job-level scheduler dependencies (`sched/CARDNITE.sched`)

```
CICS-CARD-CLOSED                       (sched:43)
  └─ CBCRD01J ──ok──► CBCRD02J ──ok──► CBCRD03J ──ok──► CBCRD04J
                                                          │ (fork, sched:155-158)
                              ┌───────────────────────────┴───────────────┐
                        CBCRD05AJ (sched:160-189)                 CBCRD05BJ (sched:192-221)
                              └───────────────┬───────────────────────────┘
                                    CBCRD06J  (join: IN-COND both, AND — sched:240-242)
                                        └──► CBCRD07J ──► CBCRD08J ──► CBCRD09J ──► CBCRD10J
                                                              CBCRD10J posts CARDNITE-COMPLETE + CICS-CARD-OPEN-OK
                                                              and deletes all *-OK conditions (sched:375-389)
```

* Fork semantics: 05A and 05B both trigger on `CBCRD04J-OK` (`sched:172,204`), run concurrently, touch different tables, hold no common resource (`sched/CARDNITE.sched:155-158`); serialized resources: CBCRD04J and CBCRD07J both hold `CARDDB-POSTING QUANTITY=1` (`sched:130,280`) so posting and GL never overlap.
* Join semantics: scheduler AND-join (`sched:240-242`) **plus** an application-level check — CBCRD06J STEP005 (CBCRD06W) reads the CYCLCTL record and does not trust the scheduler (`app/jcl/cardsvc/CBCRD06J.jcl:10-14`). Branch completion flags live in the *filler* bytes of the cycle control record: fee branch posts `CC-FILLER(1:1)` = 'A' complete / 'F' failed (`app/cardsvc/cbl/CBCRD05A.cbl:729-733`), interest branch posts `CC-FILLER(2:1)` = 'B' / 'F' (`CBCRD05B.cbl:960-962`); CBCRD06W reads both (`CBCRD06W.cbl:179-180`), polls once a minute up to the `WAIT=NNN` PARM limit (standing 030) with a CPU-spin delay loop (`CBCRD06W.cbl:205-215`, `docs/runbook-cardnite.md:44-47`), then abends U0610 (fee branch not complete), U0611 (interest branch not complete) or U0612 (a branch posted a failed flag); U0601 is CYCLCTL unusable (`CBCRD06W.cbl:28-33,216-243`). The JCL comment and runbook label these outcomes U0602/U0603 instead (`app/jcl/cardsvc/CBCRD06J.jcl:32-34`, `docs/runbook-cardnite.md:101-102`) — a JCL/runbook-vs-source divergence, see §8. **Migration note:** these flags are an undocumented overlay of `CC-FILLER` (declared as plain filler at `app/cpy/CVCTRL01Y.cpy:41`); CBCRD05A also stores its cycle id at `CC-FILLER(3:8)` (`CBCRD05A.cbl:733`).
* CBCRD09J failure does not hold the cycle — the successor condition is posted either way (`sched:350-357`).
* On any NOTOK the cycle is **held, not cancelled** via `CARDNITE-HELD` (`sched:403-408`).

### 2.4 Intra-job step DAGs

* CBCRD03J: STEP010 SORT → STEP020 enrichment, gated `IF STEP010.RC = 0` (`app/jcl/cardsvc/CBCRD03J.jcl:64-65`).
* CBCRD06J: STEP005 → STEP010 (`IF STEP005.RC = 0`) → STEP020 (`IF RC <= 4`) → STEP030 (`IF RC <= 4`) → STEP040 (`IF STEP030.RC <= 8`) → STEP050 print review file (`IF STEP030.RC = 8`) (`app/jcl/cardsvc/CBCRD06J.jcl:61,77,101,138,157`).
* CBCRD09J: STEP010 EXPORT → STEP020 REPRO (`IF STEP010.RC=0`) → STEP030 DELETE/DEFINE → STEP040 verify (`IF STEP030.RC=0`) (`app/jcl/cardsvc/CBCRD09J.jcl:50,89,111`); order is load-bearing (`CBCRD09J.jcl:8-16`).

---

## 3. Surface per program

Estate RC convention for the whole cycle: 0000 clean / 0004 warning-partial / 0008 business decline-serious / 0012 fatal (`docs/runbook-cardnite.md:63-73`, constants at `app/cpy/CVCONSTY.cpy:39-42`). Job-specific meanings below and at `docs/runbook-cardnite.md:74-87`.

PARM convention: the cycle date is set by `SET CYCDATE=` and reaches every program as the first 8 bytes of the PARM (`docs/runbook-cardnite.md:31-34`). Per-job PARM contracts differ and are listed per program.

### CBCRD01 — authorization log extract (CBCRD01J)
* Inputs: VSAM `CARD.PROD.AUTHLOG` (ESDS, DD AUTHLOG, `app/jcl/cardsvc/CBCRD01J.jcl:37`; SELECT at `CBCRD01.cbl:48`), VSAM `CARD.PROD.CYCLCTL` (DD CYCLCTL, `CBCRD01J.jcl:38`).
* Outputs: `CARD.PROD.AUTHEXTR(+1)` GDG, FB LRECL 300 (`CBCRD01J.jcl:39-43`); control copy printed by STEP020 IEBGENER when RC 0/4 (`CBCRD01J.jcl:55-60`).
* PARM: `'&CYCDATE,&CYCID'` — CCYYMMDD cycle date, cycle id after the comma (`CBCRD01J.jcl:12-13,32-33`).
* Restart/checkpoint: opens the cycle — initializes `CC-COMMIT-FREQ` from `WS-COMMIT-FREQUENCY` and zeroes counters / `CC-LAST-KEY` (`CBCRD01.cbl:305-312`). RESTART=STEP010 rerunnable from scratch; never restart at STEP020 (`CBCRD01J.jcl:21-26`).
* RC: 0 complete / 4 records skipped / 8 no records selected / 12 fatal U0102-U0103 (`CBCRD01J.jcl:15-19`); scheduler treats RC≤4 OK, 8+ NOTOK, abend U4001 (`sched:52-55`). Program abends: U0101 CYCLCTL unreadable (`CBCRD01.cbl:26,204`), U0102 file failures (`CBCRD01.cbl:235`), U0103 no control record (`docs/runbook-cardnite.md:94`).

### CBCRD02 — edit and reject split (CBCRD02J)
* Inputs: `CARD.PROD.AUTHEXTR(0)` (DD AUTHEXTR, `CBCRD02J.jcl:39`), CYCLCTL (`CBCRD02J.jcl:40`).
* Outputs: `CARD.PROD.AUTHCLN(+1)` and `CARD.PROD.AUTHREJ(+1)`, both FB 300 (`app/jcl/cardsvc/CBCRD02J.jcl:41-50`); reject listing printed when RC 4/8 (`CBCRD02J.jcl:62-67`).
* PARM: `'&CYCDATE,&TOLER'` — NN = reject tolerance as whole percent of records read, standing value 02, duty-manager-controlled (`CBCRD02J.jcl:13-18,34-35`); parsed at `CBCRD02.cbl:240-241` (`LK-PARM-DATA(1:8)` date, `(10:2)` tolerance).
* Decodes the 60-byte auth-detail overlay by AUTH-TYPE and range-checks (`CBCRD02J.jcl:8-11`). Writes reject reason codes R001-R010 (`app/cardsvc/cpy/CVRJCT01Y.cpy:13-22`).
* RC: 0 no rejects / 4 within tolerance / 8 over tolerance — cycle held (`CBCRD02J.jcl:19-25`, `docs/runbook-cardnite.md:76-77`); scheduler posts the successor with an operator notify on a zero-record EMPTY outcome (`sched/CARDNITE.sched:75-79`). Abends U0201 control unusable (`CBCRD02.cbl:29,237`), U0202 extract file failure (`CBCRD02.cbl:261`).

### DFSORT step + CBCRD03 — sort and merchant enrichment (CBCRD03J)
* STEP010 SORT: SORTIN `AUTHCLN(0)` → SORTOUT `CARD.PROD.AUTHSRT(+1)` FB 300; control cards `SORT FIELDS` on card num / date / seq with an INCLUDE dropping rejected records (`app/jcl/cardsvc/CBCRD03J.jcl:37-58`; displacement table documented at `CBCRD03J.jcl:16-20`).
* STEP020 CBCRD03 (via CARDDB2 proc): inputs `AUTHSRT(0)`, VSAM `CARD.PROD.MERCHRTE` KSDS, CYCLCTL; output `CARD.PROD.AUTHENR(+1)` FB 300 (`CBCRD03J.jcl:72-79`). SELECTs at `CBCRD03.cbl:48-62`.
* Control cards (SYSIN): `DEFAULT-ROUTE=DFLT`, `DEFAULT-ACQID=99999999999` — business parameters owned by Settlements (`CBCRD03J.jcl:85-90`).
* PARM: `PARM('20240131')` cycle date only, on the DSN RUN card (`CBCRD03J.jcl:69`).
* Db2: `SELECT … FROM CARDSVC.MERCHANT` (`CBCRD03.cbl:389`, header `CBCRD03.cbl:25`).
* RC: 0 all resolved / 4 defaults taken / 8 default card missing or MERCHRTE unusable / 12 U0302-U0303 (`CBCRD03J.jcl:22-26`). RESTART=STEP020 safe if the sort was good (`CBCRD03J.jcl:28-31`).

### CBCRD04 — posting engine (CBCRD04J)
* Inputs: `AUTHENR(0)`, CYCLCTL (`app/jcl/cardsvc/CBCRD04J.jcl:48-49`).
* Outputs: bypass file `CARD.PROD.POSTREJ(+1)` FB 300 on DD AUTHREJ (`CBCRD04J.jcl:50-54`); bypass listing printed when RC≤4 (`CBCRD04J.jcl:62-67`).
* PARM: `CCYYMMDD[,RESTART=Y|N]` via the CARDDB2 proc RUN card (`CBCRD04J.jcl:41-47`; contract at `CBCRD04.cbl:29`, JCL notes `CBCRD04J.jcl:14-17`).
* Db2: INSERT `CARDSVC.TRANSACTION` (`CBCRD04.cbl:730`), SELECT/UPDATE `CARDSVC.ACCOUNT` (`CBCRD04.cbl:458,816`), SELECT/UPDATE `CARDSVC.CARD_LIMIT` (header `CBCRD04.cbl:38`), UPDATE `CARDSVC.AUTHORIZATION` posted flag (`CBCRD04.cbl:850`).
* Checkpoint/restart: commits every `CC-COMMIT-FREQ` postings, rewriting `CC-LAST-KEY`/counters into CYCLCTL at each commit (`CBCRD04.cbl:19-25,414-415,901-925`). `CC-COMMIT-FREQ` is operator-tunable on the control record; when zero the program falls back to `WS-COMMIT-FREQUENCY` = 1000 (`CBCRD04.cbl:334-335`, `app/cpy/CVCONSTY.cpy:37`). RESTART=Y skips forward to `CC-LAST-KEY` before posting (`CBCRD04.cbl:342-348`); duplicate-insert during the checkpoint window treated as already done (`CBCRD04.cbl:779`). Full restart procedure and CC-RESTART-CNT ≤ 3 rule: `docs/runbook-cardnite.md:114-136`. Cold re-run requires CBCRD99J backout first (`CBCRD04J.jcl:31-34`).
* RC: 0 all posted / 4 bypasses to reject / 8 too many bypasses or restart key not found / 12 U0402 SQL, U0403 control failure (`CBCRD04J.jcl:19-23`); U0401 CYCLCTL unusable (`CBCRD04.cbl:45`), U0404 restart key not on input (`CBCRD04.cbl:48`). Scheduler: U4004/S0C7 NOTOK, usually a bad packed field (`sched:147-148,420-421`).

### CBCRD05A — fee assessment branch (CBCRD05AJ)
* Inputs: CYCLCTL (`app/jcl/cardsvc/CBCRD05AJ.jcl:50`); output audit trail `CARD.PROD.FEEAUDIT(+1)` LRECL 133 (`CBCRD05AJ.jcl:51-55`), printed when RC≤8 (`CBCRD05AJ.jcl:62-67`).
* PARM: cycle date only (`CBCRD05AJ.jcl:47`).
* Db2: cursors on `CARDSVC.ACCOUNT`/`CARDSVC.TRANSACTION` and `CARDSVC.FEE_SCHEDULE` (`CBCRD05A.cbl:243-262`), `CARDSVC.CARD_LIMIT`+`CARDSVC.CARD` join (`CBCRD05A.cbl:394-395`), INSERT `CARDSVC.TRANSACTION` / UPDATE `CARDSVC.ACCOUNT` (`CBCRD05A.cbl:601,647`).
* Dispatch: for each applicable fee type, fills `FEE-WORK-AREA` (`FR-CALLER-ID` 'CBCRD05A', `CBCRD05A.cbl:138,455`) and calls CBCRD90 with route FEEC (`CBCRD05A.cbl:495-508`); a dispatcher 'ROUTE NOT FOUND' (RC 8, `CBCRD90.cbl:115-120`) is handled as a declined fee — warning RC 4 only (`CBCRD05A.cbl:522-530`); U0505 is raised only for a fatal handler RC (`CBCRD05A.cbl:531-536`).
* Restart: the JCL comment claims the program deletes its own fee rows for the cycle date before starting (`CBCRD05AJ.jcl:28-31`), but no DELETE exists in CBCRD05A — a JCL-vs-source divergence, see §8. Commits every `WS-COMMIT-FREQUENCY` (`CBCRD05A.cbl:378`).
* Completion flag: posts `CC-FILLER(1:1)` 'A'/'F' + cycle id at `(3:8)` as its last act (`CBCRD05A.cbl:710-747`).
* RC: 0 all assessed / 4 skips, handler warning, or unroutable/declined fee / 12 U0502 no flag posted (`CBCRD05AJ.jcl:21-26`); the JCL's "8 fee type not routable (flag F)" is a JCL-vs-source divergence — the source ends RC 4 with flag 'A' (`CBCRD05A.cbl:522-530,728-732`), see §8. U0501 CYCLCTL unusable (`CBCRD05A.cbl:33`), U0503 SQL (`CBCRD05A.cbl:332` etc.).

### CBCRD05B — interest and rewards branch (CBCRD05BJ)
* Inputs: CYCLCTL (`app/jcl/cardsvc/CBCRD05BJ.jcl:43`); report `INTRPT` SYSOUT FBA 133 (`CBCRD05BJ.jcl:44-45`).
* PARM: cycle date only (`CBCRD05BJ.jcl:40`).
* Control cards (SYSIN): rounding convention `HALFUP` agreed with Finance/Audit, change-record controlled — a business parameter on cards, not in the program (`CBCRD05BJ.jcl:14-18,53-59`).
* Db2: cursor/UPDATE `CARDSVC.ACCOUNT` (`CBCRD05B.cbl:251,723`), SELECT `CARDSVC.CARD_LIMIT`+`CARD` (`CBCRD05B.cbl:487-488`), SELECT/INSERT `CARDSVC.TRANSACTION` (`CBCRD05B.cbl:544,676`), INSERT/UPDATE `CARDSVC.REWARDS` (`CBCRD05B.cbl:798-852`).
* Restart: the JCL comment claims interest rows for the cycle date are deleted before accruing (`CBCRD05BJ.jcl:27-30`), but no DELETE exists in CBCRD05B — a JCL-vs-source divergence, see §8. Commits every `WS-COMMIT-FREQUENCY` (`CBCRD05B.cbl:467`).
* Completion flag: `CC-FILLER(2:1)` 'B'/'F' (`CBCRD05B.cbl:943-977`).
* RC: 0 complete / 4 accounts skipped, no APR on file / 8 control card error or unsupported rounding (flag posted F) / 12 U0512 no flag (`CBCRD05BJ.jcl:20-25`). U0506 rounding/control card abend path (`CBCRD05B.cbl:388-395`), U0503 SQL, U0501 CYCLCTL.

### CBCRD06W — join guard (CBCRD06J STEP005)
* Input: CYCLCTL only (`app/jcl/cardsvc/CBCRD06J.jcl:55`).
* PARM: `'&CYCDATE,WAIT=NNN'` — NNN polls, one minute each, standing value 030 (`CBCRD06J.jcl:27-29,50-51`); parsed at `CBCRD06W.cbl:137-141`.
* Behavior: reads the CARDNITE control record, checks the two branch flags, polls, and abends U0601 (control unusable) / U0610/U0611 (branch not complete at poll limit) / U0612 (branch posted failed) per §2.3 (`CBCRD06W.cbl:28-33`); the JCL/runbook's U0602/U0603 labels do not exist in the source (§8). RC 0 only when both flags complete (`CBCRD06W.cbl:248`).

### CBCRD06A — party work list build (CBCRD06J STEP010)
* Output: `CARD.PROD.PARTYWK(+1)` FB **LRECL 150** (`CBCRD06J.jcl:70-74`); record layout CVPWRK01Y.
* PARM: cycle date only (`CBCRD06J.jcl:66`).
* Db2: cursor over `CARDSVC.ACCOUNT` with a `CARDSVC.TRANSACTION` existence subquery (`CBCRD06A.cbl:150-155`), `CARD_LIMIT`+`CARD` join (`CBCRD06A.cbl:328-329`).
* RC: 0/4 warnings; abends U0601 CYCLCTL, U0602 file, U0603 SQL (`CBCRD06A.cbl:29,197-263`).

### CBCRD06X — sort/de-dup (CBCRD06J STEP020)
* Input `PARTYWK(0)`, output `CARD.PROD.PARTYSRT(+1)` FB 150 via internal SORT with SORTWK DDs (`CBCRD06J.jcl:82-90`; SELECTs `CBCRD06X.cbl:44-52`). Abends U0602 I/O, U0604 (`CBCRD06X.cbl:166,186`).

### CBCRD06B — exposure recalculation dispatch (CBCRD06J STEP030) — the crossing
* Files: PARTYSRT input LRECL 150; PARTYACC accepted output 150; PARTYRVW manual review output 150; RISKEXC exception listing FBA 133 (`CBCRD06B.cbl:30-33,62-99`; JCL `CBCRD06J.jcl:116-128`).
* PARM: cycle date only (`CBCRD06J.jcl:113`; parsed `CBCRD06B.cbl:230-236`).
* Db2: INSERT `CARDSVC.ROUTE_AUDIT` per crossing (`CBCRD06B.cbl:533-556`); commit every `WS-COMMIT-FREQUENCY` dispatches (`CBCRD06B.cbl:287-290`).
* Per-party RC handling (contract in §7): worst-seen becomes the step RC; the JCL gates STEP040 on `IF RC <= 8` (`CBCRD06J.jcl:132-138`), although the program header says STEP040 "can be gated with IF RC <= 4" (`CBCRD06B.cbl:16-25`) — a source-comment-vs-JCL divergence (the JCL's ≤8 is what lets accepted parties proceed on a manual-review RC 8), see §8.
* Abends: U0602 file, U0605 route unresolvable, U0606 crossing fatal, U0607 commarea version mismatch (`CBCRD06B.cbl:41-44,367-381`). Runbook maps the step's fatal path to U0632 (`docs/runbook-cardnite.md:103,148`) — see §8 discrepancies. Note: U0605 is unreachable — CBCRD90 exits with only `LK-RETURN-CD` = 8 on a not-found route without setting `RQ-RC` (`CBCRD90.cbl:115-121,307`), so CBCRD06B's `RQ-RC-NOT-FOUND` test (`CBCRD06B.cbl:367-374`) never fires for a missing route.
* Operator doctrine: never re-run STEP030 alone to clear an RC 8; reviewed parties stay excluded until Financial Crime releases them (`docs/runbook-cardnite.md:150-159`).

### CBCRD06C — apply outcomes (CBCRD06J STEP040)
* Input `PARTYACC(0)`; report BANDRPT SYSOUT FBA 133; CYCLCTL (`CBCRD06J.jcl:146-149`; SELECTs `CBCRD06C.cbl:47-55`).
* Db2: `ACCOUNT`+`CARD`+`CARD_LIMIT` join cursor (`CBCRD06C.cbl:222-224`), positioned UPDATE `CARDSVC.CARD_LIMIT` risk band / exposure (`CBCRD06C.cbl:442-452`). Per-party: applies only records whose `PW-RC` accepted (0/4); commit every `WS-COMMIT-FREQUENCY` (`CBCRD06C.cbl:364`). Abends U0602 file, U0608 SQL (`CBCRD06C.cbl:344,429-466`).

### CBCRD07 — general ledger feed (CBCRD07J)
* Files: CYCLCTL; GL report GLRPT SYSOUT FBA 133 (`app/jcl/cardsvc/CBCRD07J.jcl:50-52`; SELECTs `CBCRD07.cbl:66-70`).
* PARM: cycle date only (`CBCRD07J.jcl:47`).
* Control cards: Finance-owned GL/cost-centre mapping — `BATCH=`, `SUSPENSE=`, and `MAP <txn-type> <product|****> <debit-GL> <credit-GL> <cost-centre>` cards, fixed columns (`CBCRD07J.jcl:15-24,53-70`). Transaction types mapped: PURC, CASH, REFN, FEEA(GOLD/CLAS), FEEL, FEEO, FEEC, FEEF, INTR, INTC, RWDA (`CBCRD07J.jcl:58-69`).
* Db2: cursor over `CARDSVC.TRANSACTION`+`ACCOUNT` where `GL_POSTED_FLG = 'N'` (`CBCRD07.cbl:275-276`; restart property `CBCRD07J.jcl:34-37`), INSERT `CARDSVC.GL_POSTING` (`CBCRD07.cbl:598`), UPDATE `CARDSVC.TRANSACTION` posted flag (`CBCRD07.cbl:647-654`).
* Balancing: proves total debits = total credits before commit; on difference rolls the whole feed back and abends U0704 — no partial ledger (`CBCRD07J.jcl:10-13`, `CBCRD07.cbl:737`, procedure `docs/runbook-cardnite.md:184-196`).
* Commit interval `WS-COMMIT-FREQUENCY` (`CBCRD07.cbl:522`); checkpoint-restartable, accrual idempotent per scheduler note (`sched:293-294`).
* RC: 0 balanced / 4 suspense used — Finance adds a mapping card / 8 no transactions / 12 U0701 card error, U0703 SQL, U0704 out of balance (`CBCRD07J.jcl:26-32`).

### CBCRD08 — delinquency and aging (CBCRD08J)
* Files: CYCLCTL; AGERPT SYSOUT FBA 133; collections feed `CARD.PROD.COLLECT(+1)` FB **LRECL 100** (`app/jcl/cardsvc/CBCRD08J.jcl:41-48`; SELECTs `CBCRD08.cbl:56-64`). STEP020 IDCAMS print of the feed for transmission reconciliation when RC≤4 (`CBCRD08J.jcl:61-66`); feed transmitted by job CBCRD88J (`CBCRD08J.jcl:55-58`).
* PARM: cycle date only (`CBCRD08J.jcl:38`); SYSIN `GRACE-DAYS=005` collections policy CP-07 (`CBCRD08J.jcl:13-14,49-53`).
* Db2: cursor + positioned UPDATE on `CARDSVC.ACCOUNT` (`CBCRD08.cbl:33,291,550`). Rolls DELQ_BUCKET forward at most one bucket per cycle, recomputes DELQ_AMT, feeds bucket ≥ 3 to collections (`CBCRD08J.jcl:5-11`). Handles mixed 8-digit and legacy 6-digit dates with century windowing (`CBCRD08.cbl:273`, pivot at `CVCONSTY.cpy:9-12`).
* Restart: bucket roll driven from payment due date + shortfall, not previous bucket alone — same-date re-run does not double-roll (`CBCRD08J.jcl:23-29`). Commit interval `WS-COMMIT-FREQUENCY` (`CBCRD08.cbl:480`).
* RC: 0 nothing at charge-off / 4 bucket-6 charge-off candidates / 8 no accounts or grace card rejected / 12 U0803 SQL (`CBCRD08J.jcl:16-21`). U0802 file (`CBCRD08.cbl:337`).

### IDCAMS steps + CBCRD09 — backups and AUTHLOG reset (CBCRD09J)
* STEP010: `EXPORT CARD.PROD.CYCLCTL … TEMPORARY CIMODE` to `CARD.PROD.EXPORT.CYCLCTL(+1)` (`app/jcl/cardsvc/CBCRD09J.jcl:34-47`).
* STEP020: REPRO CARDXREF→`CARD.PROD.BKP.CARDXREF(+1)` FB 128, CYCLCTL→`BKP.CYCLCTL(+1)` FB 256, AUTHLOG→`BKP.AUTHLOG(+1)` FB 200 (`CBCRD09J.jcl:51-78`).
* STEP030: DELETE/DEFINE `CARD.PROD.AUTHLOG` — NONINDEXED (ESDS), RECORDSIZE(200 200), CISZ 8192; matches DEFCARD.jcl (`CBCRD09J.jcl:89-107`).
* STEP040 CBCRD09: PARM cycle date only (`CBCRD09J.jcl:113`); reads the three backup generations + live AUTHLOG + CYCLCTL, writes backup register BKPREG FBA 133 (`CBCRD09J.jcl:114-122`; SELECTs `CBCRD09.cbl:46-68`). No Db2.
* RC: 0 verified and AUTHLOG empty / 4 backup smaller than previous generation / 12 U0901 empty CARDXREF backup, U0902 empty AUTHLOG backup, U0903 AUTHLOG not empty after redefine (`CBCRD09J.jcl:18-24`; abend sites `CBCRD09.cbl:305,344,394,441-461`). Restart rules split at STEP030 (`CBCRD09J.jcl:25-30`, `docs/runbook-cardnite.md:208-219`).

### CBCRD10 — cycle close and control report (CBCRD10J)
* Files: CYCLCTL; stage statistics `CARD.PROD.STGSTAT` QSAM input LRECL 120 (`CBCRD10.cbl:29,60-80`); control report CTLRPT SYSOUT FBA 133 (`app/jcl/cardsvc/CBCRD10J.jcl:45-46`), kept as a disk GDG via STEP020 when RC≤8 (`CBCRD10J.jcl:50-62`).
* PARM: cycle date only (`CBCRD10J.jcl:40`).
* Db2 (read-only counts/totals): `CARDSVC.TRANSACTION` counts (`CBCRD10.cbl:527,543`), `CARDSVC.GL_POSTING` totals (`CBCRD10.cbl:566`), `CARDSVC.ACCOUNT` delinquency count (`CBCRD10.cbl:594`).
* Region gate: decision recorded in both the step RC and `CC-ONLINE-CLOSED-FLG` (`CBCRD10.cbl:9,713-717`); RC table at `docs/runbook-cardnite.md:172-177`. RC 8 = a stage did not run / ledger unproven — region held (`CBCRD10J.jcl:24-26`).
* RC: 0 clean / 4 exceptions / 8 region held / 12 U1002 file, U1003 SQL (`CBCRD10J.jcl:20-27`).

### CBCRD90 — batch dispatcher (shared)
* Parameters: `LK-ROUTE-REQUEST` PIC X(38), pass-through `LK-PARM-AREA` PIC X(512), `LK-RETURN-AREA` (RC + program + message) (`CBCRD90.cbl:84-95`).
* Route cache: 200 entries, loaded once per run unit, Db2 first / VSAM PGMROUT fallback (`CBCRD90.cbl:133-149`; cache layout `app/cpy/CVROUT01Y.cpy:36-51`). Route filter: `ACTIVE_FLG='Y' AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE` (`CBCRD90.cbl:79-80`).
* Return codes to caller: 8 route not found, 12 route table unavailable / target and fallback not loadable (`CBCRD90.cbl:108-121,325-337`).
* `CANCEL` after every call so a route change takes effect on re-drive (`CBCRD90.cbl:305-306`) — but see §7 on PRBRSK1's counter assumption.

### CBFEE01/02/03 — fee handlers (dispatched, route FEEC)
* Parameters: `FEE-WORK-AREA` (CVFEEW1Y, 512 bytes) + `BATCH-RETURN-AREA` (CVBRTN1Y) (`CBFEE01.cbl:177-181`, `CBFEE02.cbl:156-160`, `CBFEE03.cbl:165-169`).
* Control cards: FEEPARM DD — every rate, threshold and cap is on cards, none coded in the procedure division (`CBFEE01.cbl:15,43`; `CBFEE02.cbl:41`; `CBFEE03.cbl:49`).
* Db2: read `CARDSVC.FEE_SCHEDULE` + `CARDSVC.TRANSACTION` (CBFEE03 also `CARDSVC.MERCHANT`, `CBFEE03.cbl:402`); write INSERT `CARDSVC.TRANSACTION` + UPDATE `CARDSVC.ACCOUNT` (`CBFEE01.cbl:653,702`; `CBFEE02.cbl:531,581-597`; `CBFEE03.cbl:589,639`).
* RC 12 = SQL failure, caller must back out (`CBFEE01.cbl:34`, `CBFEE02.cbl:32`, `CBFEE03.cbl:40`). CBCRD90 cancels the handler after every call, so no state survives between fees (`CBFEE01.cbl:17`).

---

## 4. Field dictionary

### 4.1 Copybook record layouts touched by CARDNITE

| Copybook | Path | Layout | Used by |
|---|---|---|---|
| CVCTRL01Y | `app/cpy/CVCTRL01Y.cpy` | CYCLE-CTRL-RECORD, VSAM CYCLCTL, key CC-CYCLE-TYPE(8)+CC-CYCLE-DATE(8); CC-STATUS N/R/C/F/S; checkpoint block CC-COMMIT-FREQ, CC-RECS-READ/WRITTEN/REJECTED, CC-LAST-KEY X(32), CC-RESTART-CNT (`CVCTRL01Y.cpy:6-34`); totals COMP-3 DR/CR (`:36-39`); CC-ONLINE-CLOSED-FLG (`:40`); CC-FILLER X(30) (`:41`) — bytes 1/2 overlaid as branch flags, 3:8 cycle id (§2.3) | every job (CYCLCTL DD in all 10) |
| CVAXTR01Y | `app/cardsvc/cpy/CVAXTR01Y.cpy` | AUTH-EXTRACT-REC, fixed 300: AX-HEADER (cycle date/id, extract seq, source RBA `:13-17`), AX-AUTH-IMAGE X(179) uninterpreted (`:20`), AX-EDIT status C/W/R + reason (`:23-28`), AX-ENRICH MCC/acquirer/route/merchant/high-risk (`:31-40`) | CBCRD01→02→03→04 (`CVAXTR01Y.cpy:4-5`); DFSORT displacements 035/051/059/214 (`CBCRD03J.jcl:16-20`) |
| CVAUTH01Y | `app/cpy/CVAUTH01Y.cpy` | 179-byte authorization record redefined over AX-AUTH-IMAGE (`CVAXTR01Y.cpy:9-10`) | CBCRD01/02/03/04, CBFEE02 |
| CVRJCT01Y | `app/cardsvc/cpy/CVRJCT01Y.cpy` | CYCLE-REJECT-REC fixed 300; reason codes R001-R010 (`:13-22`); written by CBCRD02/03/04, read by CBCRD10 (`:4-6`) | reject/bypass files |
| CVPWRK01Y | `app/cardsvc/cpy/CVPWRK01Y.cpy` | PARTY-WORK-REC fixed **150**, key PW-PARTY-ID(11)+PW-ACCT-ID(11) (`:7,10-12`); balances COMP-3; PW-OUTCOME: PW-RC 0/4/8/12, reason, advice, sanction flag, score, dispatch TS (`:25-34`). Same layout on the work-list, accepted and manual-review files (`:4-7`) | CBCRD06A/X/B/C |
| CVROUT01Y | `app/cpy/CVROUT01Y.cpy` | ROUTE-RECORD (mirrors PGM_ROUTE + VSAM PGMROUT, `:7-34`), ROUTE-CACHE (200 entries, `:36-51`), ROUTE-REQUEST with RQ-RC 0/8/12 (`:53-64`) | CBCRD90, CBCRD05A, CBCRD06B |
| CVRISK01Y | `app/cpy/CVRISK01Y.cpy` | CV-RISK-AREA, **512 bytes**, shared CARDSVC↔PARTYRSK; version 0003 (`:13-14`); header/in/out/status/hop-trace sections (`:11-86`); coordinated rebind + version bump on change (`:8-9`) | CBCRD06B ↔ PRBRSK1 |
| CVFEEW1Y | `app/cardsvc/cpy/CVFEEW1Y.cpy` | FEE-WORK-AREA 512 bytes (dispatcher passes X(512), `:5-6`); FW-FEE-TYPE ANNU/LATE/OVLM/CASH/FRGN (`:20-25`); account snapshot, fee basis incl. FW-AUTH-IMAGE X(60), prior-cycle flags (`:29-73`) | CBCRD05A ↔ CBFEE01/02/03 |
| CVBRTN1Y | `app/cardsvc/cpy/CVBRTN1Y.cpy` | BATCH-RETURN-AREA: BR-RETURN-CD 0/4/8/12 + pgm + msg (first 70 bytes addressed by CBCRD90, `:4-6`), BR-EXTENSION fee totals/waiver/lines/hash (`:12-32`) | fee handlers, CBCRD05A |
| CVERRS01Y | `app/cpy/CVERRS01Y.cpy` | ERROR-AREA passed to CBCRD91: severity, type SQL/VSAM/CICS/ROUT/DATA/BUSN, SQL & VSAM detail blocks, ER-ABEND-CODE (`:5-45`) | all programs |
| CVCONSTY | `app/cpy/CVCONSTY.cpy` | estate constants: dispatcher/error program names (`:18-20`), module IDs (`:14-15`), route keys incl. RSKRECAL (`:24-25`), WS-COMMIT-FREQUENCY 1000 (`:37`), RC constants (`:39-42`), century pivot 50 (`:9-12`) | all programs |
| CVTRAN01Y | `app/cpy/CVTRAN01Y.cpy` | TXN-RECORD incl. variable leg array built by CBCRD04 (`CBCRD04.cbl:194`, JCL note `CBCRD04J.jcl:8-9`) | CBCRD04 |

DCLGEN-style host structures are declared inline (e.g. `DCL-ROUTE` in `CBCRD90.cbl:59-66`, `DCL-AUDIT` in `CBCRD06B.cbl:190-201`); no separate DCLGEN members exist in the repo.

### 4.2 Db2 tables per program (schema CARDSVC, `db2/ddl/10_CARDDB_TABLES.sql`)

| Table (DDL line) | Read by | Written by |
|---|---|---|
| ACCOUNT (`10_CARDDB_TABLES.sql:9`) | 05A, 05B, 06A, 06C, 07, 08, 10 | 04 (UPDATE), 05A/05B (UPDATE), 08 (UPDATE), CBFEE01/02/03 (UPDATE) |
| CARD (`:44`) | 05A, 05B, 06A, 06C (joins) | — |
| CARD_LIMIT (`:71`) | 04, 05A, 05B, 06A, 06C | 04 (UPDATE), 06C (UPDATE risk band/exposure) |
| AUTHORIZATION (`:98`) | 04 | 04 (UPDATE posted flag, `CBCRD04.cbl:850`) |
| TRANSACTION (`:130`) | 05A, 05B, 07, 10, CBFEE01/02/03 | 04, 05A, 05B, CBFEE01/02/03 (INSERT); 07 (UPDATE GL_POSTED_FLG) |
| MERCHANT (`:161`) | 03 (`CBCRD03.cbl:389`), CBFEE03 (`CBFEE03.cbl:402`) | — |
| FEE_SCHEDULE (`:181`) | 05A, CBFEE01/02/03 | — |
| REWARDS (`:227`) | 05B | 05B (INSERT/UPDATE, `CBCRD05B.cbl:798-852`) |
| GL_POSTING (`:247`) | 10 (totals) | 07 (INSERT, `CBCRD07.cbl:598`) |
| PGM_ROUTE (`:332`) | CBCRD90 (`CBCRD90.cbl:78`) | — (seeded by `40_SEED_PGM_ROUTE.sql`) |
| ROUTE_AUDIT (`:356`) | — | CBCRD90 (`CBCRD90.cbl:345`), CBCRD06B (`CBCRD06B.cbl:533`) |

Not touched by CARDNITE: STATEMENT, DISPUTE, FRAUD_RULE (CARDBILL / online estate).

### 4.3 VSAM clusters (`app/jcl/vsam/DEFCARD.jcl`)

| Cluster | Type | Keys / recordsize | CARDNITE usage |
|---|---|---|---|
| CARD.PROD.AUTHLOG | **ESDS** (NONINDEXED) | RECORDSIZE(200 200), CISZ 8192 (`DEFCARD.jcl:37-48`) | read by CBCRD01; backed up + deleted/redefined by CBCRD09J (`CBCRD09J.jcl:89-107`); never reused in place, new day starts fresh (`CBCRD09J.jcl:8-16`) |
| CARD.PROD.CYCLCTL | KSDS | KEYS(16 0), RECORDSIZE(256 256) (`DEFCARD.jcl:132-141`) | every job; checkpoint + branch flags |
| CARD.PROD.MERCHRTE | KSDS | KEYS(15 0), RECORDSIZE(120 120) (`DEFCARD.jcl:53-60`) | CBCRD03 enrichment browse |
| CARD.PROD.CARDXREF | KSDS | KEYS(16 0), RECORDSIZE(128 128) + AIX (`DEFCARD.jcl:15-29,172-181`) | backed up/verified by CBCRD09J only |
| CARD.PROD.PGMROUT | KSDS | KEYS(16 0), RECORDSIZE(97 97) (`DEFCARD.jcl:152-164`) | CBCRD90 Db2-outage fallback; rebuilt from PGM_ROUTE by CBREF04J (`40_SEED_PGM_ROUTE.sql:6-7`) |

### 4.4 Sequential datasets / GDGs

| DSN | LRECL | Producer → consumer |
|---|---|---|
| CARD.PROD.AUTHEXTR(GDG) | 300 | CBCRD01 → CBCRD02 |
| CARD.PROD.AUTHCLN(GDG) | 300 | CBCRD02 → SORT |
| CARD.PROD.AUTHREJ(GDG) | 300 | CBCRD02 → print |
| CARD.PROD.AUTHSRT(GDG) | 300 | SORT → CBCRD03 |
| CARD.PROD.AUTHENR(GDG) | 300 | CBCRD03 → CBCRD04 |
| CARD.PROD.POSTREJ(GDG) | 300 | CBCRD04 → print |
| CARD.PROD.FEEAUDIT(GDG) | 133 | CBCRD05A → print |
| CARD.PROD.PARTYWK(GDG) | 150 | CBCRD06A → CBCRD06X |
| CARD.PROD.PARTYSRT(GDG) | 150 | CBCRD06X → CBCRD06B |
| CARD.PROD.PARTYACC(GDG) | 150 | CBCRD06B → CBCRD06C |
| CARD.PROD.PARTYRVW(GDG) | 150 | CBCRD06B → print/Financial Crime |
| CARD.PROD.COLLECT(GDG) | 100 | CBCRD08 → CBCRD88J transmission |
| CARD.PROD.BKP.* (GDG) | 128/256/200 | IDCAMS → CBCRD09 verify |
| CARD.PROD.EXPORT.CYCLCTL(GDG) | export | IDCAMS EXPORT |
| CARD.PROD.CTLRPT(GDG) | 133 | CBCRD10 → disk GDG copy (`CBCRD10J.jcl:50-62`) |
| CARD.PROD.STGSTAT | 120 | external job → CBCRD10 (`CBCRD10.cbl:21,29`) |

Reports (SYSOUT FBA 133): reject listing (02), bypass listing (04), FEEAUDIT print (05A), INTRPT (05B), RISKEXC + BANDRPT + review print (06), GLRPT (07), AGERPT (08), BKPREG (09), CTLRPT (10). Morning filing checklist at `docs/runbook-cardnite.md:221-232`.

---

## 5. Wave grouping (leaf-first, by DAG depth)

Waves order migration units so nothing is migrated before its callees/contracts. Shared components and the data-access seam go first.

**Wave 1 — seam and shared infrastructure (no CARDNITE dependencies of their own)**
* CARDSVC schema data access: the 11 tables in §4.2 (`db2/ddl/10_CARDDB_TABLES.sql`) + VSAM clusters CYCLCTL/MERCHRTE/AUTHLOG/PGMROUT (`app/jcl/vsam/DEFCARD.jcl`) + the CYCLCTL cycle-control/checkpoint contract (`app/cpy/CVCTRL01Y.cpy`) including the CC-FILLER branch-flag overlay.
* CBCRD91 error handler contract (module absent — §8; contract is CVERRS01Y + abend semantics).
* CBCRD90 dispatcher + PGM_ROUTE/ROUTE_AUDIT + PGMROUT fallback (`app/cardsvc/cbl/CBCRD90.cbl`).

**Wave 2 — dispatched leaf handlers (called only through CBCRD90)**
* CBFEE01, CBFEE02, CBFEE03 (+ FEEPARM card contracts, CVFEEW1Y/CVBRTN1Y areas).
* The XMOD/RSKRECAL caller contract (CVRISK01Y v0003) — stubbed/bridged, PARTYRSK itself out of scope.

**Wave 3 — file pipeline jobs (depend on wave 1 seam only)**
* CBCRD01, CBCRD02, DFSORT step + CBCRD03 (CVAXTR01Y pipeline).
* CBCRD09 (+ its three IDCAMS steps — backup/reset doctrine).

**Wave 4 — posting core and branches (depend on waves 1-2)**
* CBCRD04 (checkpoint/restart engine).
* CBCRD05A (needs CBCRD90 + CBFEE*), CBCRD05B. Both must preserve the branch-flag posting protocol.

**Wave 5 — join and crossing (depends on waves 1-4)**
* CBCRD06W, CBCRD06A, CBCRD06X, CBCRD06B (crossing caller), CBCRD06C.

**Wave 6 — downstream financials and close (depend on posted data)**
* CBCRD07 (GL feed + Finance mapping cards), CBCRD08 (delinquency + collections feed), CBCRD10 (close/control report/region gate), plus the CARDNITE scheduler table semantics (hold-not-skip, condition deletes, DUE-OUT 06:00).

6 waves total.

---

## 6. Stored-procedure candidates

| Candidate | Source | Rationale | Caveats |
|---|---|---|---|
| Fee handler bodies (CBFEE01/02/03) | `CBFEE01.cbl:355-702` etc. | Pure Db2 read-compute-insert/update per account: read FEE_SCHEDULE/TRANSACTION, insert fee TRANSACTION, update ACCOUNT. No file I/O except FEEPARM cards (externalizable as a parameter table). Natural per-call unit of work already framed by the dispatcher | FEEPARM card semantics and BR-EXTENSION return fields must become SP parameters; waiver rules per fee type |
| GL aggregation + proof (CBCRD07 2xxx sections) | `CBCRD07.cbl:275-746` | Set-based aggregate of TRANSACTION by type/product/currency into GL_POSTING with a debits=credits proof inside one unit of work; rollback-on-imbalance maps directly to an SP transaction (U0704 semantics) | Mapping cards (Finance-owned SYSIN) must move to a mapping table; suspense-account fallback is business logic to keep |
| Delinquency bucket roll (CBCRD08) | `CBCRD08.cbl:291-563` | Cursor + positioned UPDATE on ACCOUNT only; roll rule is due-date/shortfall-driven and idempotent per cycle date | Aging report + collections feed writes are file outputs and stay outside the SP |
| Cycle-close statistics (CBCRD10 counts/totals) | `CBCRD10.cbl:527-602` | Read-only COUNT/SUM queries — trivially a set of views or one reporting SP | Low value alone; part of the control-report rewrite |
| **Not** candidates | CBCRD01/02/03/09 (file/VSAM-dominated), CBCRD04 (checkpoint restart across file + Db2 state), CBCRD05A/06B (dynamic dispatch orchestration), CBCRD06W (polling control-flow) | file I/O, cross-resource restart semantics, or dispatch orchestration make them process-tier logic | |

---

## 7. Boundary crossings — every call that leaves CARDNITE

### 7.1 XMOD/RSKRECAL → PRBRSK1 (PARTYRSK) — the HARD-STOP crossing

* Caller: CBCRD06B (CBCRD06J STEP030). Route: `RQ-ROUTE-TYPE='XMOD'`, `RQ-ROUTE-KEY='RSKRECAL'`, `RQ-SEQ-NBR=1` (`CBCRD06B.cbl:348-353`; route key constant at `CVCONSTY.cpy:25`). Resolved from `CARDSVC.PGM_ROUTE` to `PRBRSK1`, call type **'D'** (dynamic), module PARTYRSK (`db2/ddl/40_SEED_PGM_ROUTE.sql:42`); no fallback program on the row. CBCRD06B deliberately never names the target (`CBCRD06B.cbl:6-12`).
* Transport: `CALL 'CBCRD90' USING ROUTE-REQUEST, CV-RISK-AREA, WS-RETURN-AREA` (`CBCRD06B.cbl:359-361`); CBCRD90 passes CV-RISK-AREA through as PIC X(512) (`CBCRD90.cbl:86,297`). Target library `PRSK.PROD.LOADLIB` is on STEP030's STEPLIB only (`CBCRD06J.jcl:98,103-109`).
* Exchanged data — `CV-RISK-AREA` (CVRISK01Y, 512 bytes, version 0003, `app/cpy/CVRISK01Y.cpy:11-86`):
  * Request (set per party at `CBCRD06B.cbl:300-341`): version 0003, caller id/mod, correlation id `RCAL`+yymmdd+seq, channel 'B', party/cust/acct/card ids, exposure amount capped at S9(9)V99 max (`CBCRD06B.cbl:320-329`), currency USD, country USA, request type **'RCAL'**; hop trace seeded with the caller.
  * Response (consumed at `CBCRD06B.cbl:394-407`): CV-RISK-RC, score, band A/B/C/X, KYC status, sanction flag, exposure amount, advice/reason codes; the version is checked on return — down-level → U0607 (`CBCRD06B.cbl:376-381`).
  * PRBRSK1 accepts batch-only: request type 'RCAL' and channel 'B', else RC 12 (`app/partyrsk/cbl/PRBRSK1.cbl:8-11`, validation at `PRBRSK1.cbl:239-258`). It owns the Db2 unit of work for its chain (`PRBRSK1.cbl:13-17`).
* Per-party RC handling (`CBCRD06B.cbl:16-25,411-429`; runbook §6 `docs/runbook-cardnite.md:138-162`):
  * 0 — accepted, written to PARTYACC.
  * 4 — accepted with warning, also written to RISKEXC exception listing.
  * 8 — serious (sanctions/KYC), written to `CARD.PROD.PARTYRVW`, excluded from STEP040; step RC becomes 8; review listing to Financial Crime same morning.
  * 12 — fatal, step abends (runbook U0632 / source U0606 — see §8).
  * The dispatcher return area is only a transport-failure indicator; the higher of dispatcher RC and CV-RISK-RC wins (`CBCRD06B.cbl:383-388`).
* Work-list files carried across the step boundary: PARTYSRT (in), PARTYACC/PARTYRVW (out), all LRECL 150 CVPWRK01Y (`CBCRD06J.jcl:116-126`).
* **Contract risk found:** CBCRD90 issues `CANCEL` after every call (`CBCRD90.cbl:305-306`), but PRBRSK1's run-unit counters assume "the driver does not cancel us between parties" (`PRBRSK1.cbl:51-52`) — its every-N-parties commit logic therefore degrades to commit-per-party under this driver. Document for the migration; do not change behavior.

### 7.2 Other exits from the analysis boundary

* Scheduler conditions: consumes `CICS-CARD-CLOSED` (posted by region shutdown job CICSCARD, `sched:8-10`); produces `GL-CARD-FEED-READY` (`sched:313`), `CARDNITE-COMPLETE` and `CICS-CARD-OPEN-OK` which release region start job `CARDONLJ` (`sched:375-379`, `CBCRD10J.jcl:14-18`).
* Collections feed: `CARD.PROD.COLLECT(+1)` LRECL 100, transmitted by file-transfer job `CBCRD88J` released on CBCRD08 RC≤4 (`CBCRD08J.jcl:55-58`) — external system feed.
* GL: the feed itself stays in `CARDSVC.GL_POSTING`; downstream GL pickup is signalled only by `GL-CARD-FEED-READY`.
* Operational utilities referenced but outside the stream: CBCRD98/CBCRD98J (cycle-control list utility, `CBCRD04J.jcl:28-30`, `docs/runbook-cardnite.md:122`), CBCRD99J (cycle backout, `CBCRD04J.jcl:33-34`), CARDONLJ, CICSCARD — none present in the repo (§8).
* PGMROUT VSAM fallback is rebuilt by CBREF04J (CARDREF cycle — out of scope; `40_SEED_PGM_ROUTE.sql:6-7`).

---

## 8. Absent-module and discrepancy flags

Referenced but **not present** in the repo:

| Name | Referenced at | Notes |
|---|---|---|
| **CBCRD91** (batch error handler) | called by all 15+ batch programs (e.g. `CBCRD01.cbl:529`); build step `app/jcl/build/BUILDALL.jcl:172`; bind `app/jcl/build/BINDCARD.jcl:182`; named in `CVCONSTY.cpy:20` and `CVERRS01Y.cpy:3` | **No `CBCRD91.cbl` exists anywhere in the repo.** The contract is inferable from ERROR-AREA (CVERRS01Y) and call-site behavior (populate ER-*, CALL, then set RC 12 / abend `U`+ER-ABEND-CODE). Its online sibling CACRD91 exists (`app/cardsvc/cbl/CACRD91.cbl`). Migration must reconstruct the handler from its call sites |
| CBCRD98 / CBCRD98J | `CBCRD04J.jcl:28-30`, `docs/runbook-cardnite.md:122` | cycle-control list utility, no source/JCL |
| CBCRD99J | `CBCRD04J.jcl:33-34`, `docs/runbook-cardnite.md:134` | cycle backout job, no JCL |
| CBCRD88J | `CBCRD08J.jcl:55-58` | collections feed transmission job, no JCL |
| CARDONLJ | `CBCRD10J.jcl:14-16`, `docs/runbook-cardnite.md:169` | online region start job, no JCL |
| CICSCARD | `sched/CARDNITE.sched:9` | region shutdown job posting CICS-CARD-CLOSED, no JCL |
| CARD.PROD.STGSTAT builder | `CBCRD10.cbl:21-22` says STGSTAT "is built by the job" — no producing step exists in any CARDNITE JCL | stage statistics producer unidentified in repo |
| DFSORT/IDCAMS/IEBGENER, IKJEFT01 | throughout JCL | IBM utilities, expected absent |

Documentation discrepancies (source is authoritative):

1. Scheduler DESC lines for CBCRD06J/07J/08J are stale vs the COBOL/JCL (detailed at top of this document).
2. Abend numbering: the runbook and scheduler use a `U4xxx`/`U0x0y` mixed scheme (`sched:55,87,147` U4001/U4002/U4004; `docs/runbook-cardnite.md:91-112` U0102/U0202/…), while the source builds abend codes from `WS-ABEND-CODE` values like 0101/0201/0301/0401/0501/0601/0602/0605-0608/0610-0612/0701-0704/0802-0803/0901-0903/1002-1003 (§3 per program). In particular the runbook's U0632 "crossing fatal" (`docs/runbook-cardnite.md:103,148`) corresponds to U0606 in the source (`CBCRD06B.cbl:43`), and the scheduler's ABEND-COND codes (U4001-U4010) match nothing in the source. Map both schemes during migration.
3. `CBCRD10J` PARM `FORCEOPEN` documented in scheduler operator note 4 (`sched:416-419`) has no corresponding handling in the source: CBCRD10 parses only `LK-PARM-DATA(1:8)` as the cycle date and never reads a FORCEOPEN token (`CBCRD10.cbl:325-331`; no reference to FORCEOPEN anywhere in the program). The operator note is stale vs the source — the CBCRD10 functional-requirements doc must resolve whether FORCEOPEN is implemented or dropped.
4. CBCRD06W abend codes: the source raises only U0601/U0610/U0611/U0612 (`CBCRD06W.cbl:28-33`); the U0602/U0603 codes for STEP005 exist only in the JCL comment (`app/jcl/cardsvc/CBCRD06J.jcl:32-34`) and runbook (`docs/runbook-cardnite.md:101-102`) — the legacy program never issues them.
5. CBCRD06B header says STEP040 "can be gated with IF RC <= 4" (`CBCRD06B.cbl:24-25`), but the JCL gates on `IF RC <= 8` (`CBCRD06J.jcl:138`); the JCL is authoritative — RC 8 is the manual-review business outcome and STEP040 must still run for accepted parties.
6. The CBCRD05A/05B branch flags are carried in bytes officially declared as filler (`app/cpy/CVCTRL01Y.cpy:41`) — the copybook does not document the overlay; only the program comments do (`CBCRD05A.cbl:710`, `CBCRD05B.cbl:943`, `CBCRD06W.cbl:10-11`).
