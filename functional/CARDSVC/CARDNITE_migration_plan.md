# CARDNITE stream migration plan

Phased migration plan for the CARDNITE nightly cycle. Consumes:
- `functional/CARDSVC/CARDNITE_analysis.md` (estate analysis; DAG, surfaces, waves)
- `functional/CARDSVC/CARDNITE_functional_requirement.md` (CARDNITE-FR-001…019, the sign-off oracle)
- `functional/CARDSVC/CARDSVC_target_design.md` (target stack: Java 21 / Spring Boot 3 / Spring Batch 5 / PostgreSQL 16, package-per-job under `backend/`)

Repo topology: single repo. Code lands in `backend/`; per-program FR docs land in `functional/CARDSVC/programs/`.

**Decision points for the user (STOP 2):** §4 (cross-boundary resolutions — recommendations only, not decided here) and §5 (data target sign-off items).

---

## 1. Phases

### Phase 0 — minimal scaffolding
Goal: an empty but building, testable, CI-gated backend so wave PRs are code-only.
- `backend/` Spring Boot 3 + Spring Batch 5 skeleton per the target-design layout (`pom.xml`, `com.cardsvc.common/jobs/domain/repository`, no job logic).
- Docker PostgreSQL 16 profile (compose file + Spring profile) for local runs and Testcontainers for tests.
- `.github/workflows/cardsvc-ci.yml` skeleton: build + unit + integration test jobs (target design "Build / CI" row).
- CLI job-launch seam: `spring.batch.job.name=<job>` with `cycleDate` required and per-job parameters mirroring each JCL PARM (FR §4.2 table; target-design Scheduling seam row). FORCEOPEN is explicitly NOT scaffolded (scheduler-note-only; CBCRD10 FR doc resolves it).
- Exit-code seam: `ExitCodeGenerator` wiring for the estate 0/4/8/12 convention.

Exit criteria: CI green on an empty build; `java -jar … --spring.batch.job.name=noop cycleDate=…` launches and exits 0.

### Phase 1 — data mapping / persistence
Goal: the data seam of analysis wave 1, sign-off items of §5 approved first.
- Flyway migrations translated from `db2/ddl/10_CARDDB_TABLES.sql` (11 CARDNITE tables) + `40_SEED_PGM_ROUTE.sql` seed → `pgm_route`.
- VSAM→table mappings: `cycle_control` (CYCLCTL incl. checkpoint block and the branch-flag fields promoted from the `CC-FILLER` overlay — see §5), `merch_route` (MERCHRTE), `card_xref` (CARDXREF), `pgm_route` fallback unification, `auth_log` append-only (AUTHLOG ESDS).
- JPA entities + Spring Data repositories for all of the above; `BigDecimal` scales matching COMP-3 PICs.
- Seed framework: repeatable dev/test seeds (accounts, cards, limits, fee schedule, merchants, route rows) + fixture loaders for the FB-300/150/100 file layouts (CVAXTR01Y, CVPWRK01Y, collections feed).
- GDG→file/table decisions applied per §5.

Exit criteria: Flyway clean-migrate green in CI; repository round-trip tests for every table; seed data loads.

### Phase 2 — program waves 1..6 (leaf-first)
Wave order, contents and gating in §2. Each wave: per-program FR docs written first (`functional/CARDSVC/programs/`), then implementation, then FR-acceptance tests.

*(There is intentionally no Phase 3 — phase numbers follow the orchestrator's contract, which reserves 3; the sequence is 0 → 1 → 2 → 4 → 5.)*

### Phase 4 — E2E flow test + CI regression gate
- Full-cycle E2E test: seeded `auth_log` → run CBCRD01…CBCRD10 in scheduler order (incl. the 05A∥05B fork and the 06W gate) → assert the FR acceptance criteria end-state (post-once totals, balanced GL, bucket rolls, region-gate exit code, reset `auth_log`).
- Failure-path E2E: kill/restart CBCRD04 mid-run (FR-007), fail one branch and prove the 06W hold (FR-010), force a GL imbalance and prove full rollback (FR-016).
- The E2E suite becomes a required CI gate (`cardsvc-ci.yml`) — the regression oracle for all later work.

### Phase 5 — hardening + sign-off
- Traceability matrix (FR doc §6) completed: every CARDNITE-FR row has covering test + verification case; gaps are defects.
- Operational hardening: structured logging with cycle date/job name, runbook-equivalent operator messages, restart-count guard (`CC-RESTART-CNT` ≤ 3), documented cold-rerun procedure replacing CBCRD99J.
- Divergence closure: each item in FR §5.3 (CBCRD02 RC4/EMPTY, CBCRD08 RC8/OUTOFBAL, FORCEOPEN, U0602/U0603, ≤4-vs-≤8 gate, U0632/U0606) carries an explicit reconciliation decision in the owning program's FR doc.
- Business sign-off against the FR acceptance criteria (§6 gate).

---

## 2. Waves 1..6 (Phase 2 detail, from the analysis DAG)

Gating rule: **wave N is merged to main with CI green (including all prior waves' tests) before wave N+1 starts.** Shared components precede callers; nothing is migrated before its callees/contracts.

| Wave | Programs / units | Per-program FR docs consumed (`functional/CARDSVC/programs/`) | Paths touched |
|---|---|---|---|
| 1 — seam & shared infra | Data seam (Phase 1 output), `CBCRD90` → `ProgramRouter`, `CBCRD91` contract → `ErrorReporter` (resolution: §4.2), cycle-control/checkpoint component | `CBCRD90_FR.md`, `CBCRD91_FR.md` (contract-from-callers), `CYCLCTL_contract_FR.md` | `backend/src/main/java/com/cardsvc/common/`, `domain/`, `repository/`, `backend/src/main/resources/db/migration/` |
| 2 — dispatched leaf handlers | `CBFEE01`, `CBFEE02`, `CBFEE03` (FEEPARM card contracts → parameter table/config), XMOD/RSKRECAL caller-contract client seam (stub per §4.1 until decided) | `CBFEE01_FR.md`, `CBFEE02_FR.md`, `CBFEE03_FR.md`, `RSKRECAL_contract_FR.md` | `backend/…/common/fees/`, `common/risk/` (client seam), `functional/CARDSVC/programs/` |
| 3 — file pipeline jobs | `CBCRD01`, `CBCRD02`, DFSORT step + `CBCRD03`, `CBCRD09` (+3 IDCAMS steps as job steps) | `CBCRD01_FR.md`, `CBCRD02_FR.md`, `CBCRD03_FR.md`, `CBCRD09_FR.md` | `backend/…/jobs/cbcrd01..cbcrd03`, `jobs/cbcrd09` |
| 4 — posting core & branches | `CBCRD04` (checkpoint/restart engine), `CBCRD05A` (needs wave 2 fee handlers + router), `CBCRD05B`; branch-flag posting protocol preserved | `CBCRD04_FR.md`, `CBCRD05A_FR.md`, `CBCRD05B_FR.md` | `backend/…/jobs/cbcrd04, cbcrd05a, cbcrd05b` |
| 5 — join & crossing | `CBCRD06W`, `CBCRD06A`, `CBCRD06X`, `CBCRD06B` (crossing caller via wave-2 seam), `CBCRD06C` | `CBCRD06W_FR.md`, `CBCRD06A_FR.md`, `CBCRD06X_FR.md`, `CBCRD06B_FR.md`, `CBCRD06C_FR.md` | `backend/…/jobs/cbcrd06` |
| 6 — downstream financials & close | `CBCRD07` (GL feed + mapping-card table), `CBCRD08` (delinquency + collections feed), `CBCRD10` (close/region gate), scheduler-table semantics (hold-not-skip, condition cleanup, DUE-OUT) | `CBCRD07_FR.md`, `CBCRD08_FR.md`, `CBCRD10_FR.md`, `CARDNITE_scheduler_FR.md` | `backend/…/jobs/cbcrd07, cbcrd08, cbcrd10`, `common/scheduling/` |

**6 waves total** (unchanged from analysis §5). FR ownership per wave follows the FR doc's program index (§7): e.g. wave 4 must satisfy CARDNITE-FR-006…010; wave 5 FR-010…015; wave 6 FR-016…019 plus FR-001/002 conditions.

---

## 3. Stored-procedure division

**Recommendation: no stored procedures in the target.** The analysis (§6) identified four *theoretical* SP candidates (fee handler bodies, GL aggregation+proof, delinquency bucket roll, cycle-close statistics), but each maps naturally onto the confirmed target design instead:
- Fee handlers → Spring components behind `ProgramRouter` (their card-driven parameters become a table — keeping them in the app tier preserves the dispatcher/waiver logic and the BR-EXTENSION return contract).
- GL aggregation + debits=credits proof → one `@Transactional` service (rollback-on-imbalance is exactly a Spring transaction; U0704 semantics preserved as exit 12).
- Delinquency roll → chunk-oriented step with positioned updates via JPA.
- Close statistics → repository queries/views.

Putting logic in PL/pgSQL would split the business logic across tiers, bypass the JPA/entity model the design mandates, and complicate the Testcontainers-based FR test strategy. Recorded as: **no SPs, all candidates implemented app-tier**; revisit only if E2E performance data (Phase 4) forces set-based SQL for GL aggregation.

---

## 4. Cross-boundary resolutions (user decides at STOP 2 — recommendations only)

### 4.1 XMOD/RSKRECAL → PRBRSK1 (PARTYRSK) — the hard-stop crossing
Contract (analysis §7.1, FR-013): CBCRD06B → CBCRD90 route `XMOD/RSKRECAL` call type 'D' → `PRBRSK1`; 512-byte CVRISK01Y area v0003; request type `RCAL`, channel 'B'; per-party RC 0/4/8/12; version mismatch fatal.

Options:
1. **New service now** — migrate PARTYRSK risk recalculation as a new Spring service in this effort. ✗ Violates the hard stop (PARTYRSK internals out of scope); expands scope by an entire module.
2. **Already-migrated service** — call a PARTYRSK service if its own migration lands first. Clean, but CARDNITE wave 5 would gate on another stream's timeline.
3. **Legacy mainframe API/gateway** — expose PRBRSK1 via a z/OS gateway (CICS/z/OS Connect or MQ bridge) and call it from the migrated CBCRD06B. Real parity during coexistence, but needs mainframe-side work (**lead-time request: gateway definition + EBCDIC↔JSON mapping of CVRISK01Y v0003, batch channel 'B' honored; request early if chosen**).
4. **Explicit stub/deferral** — implement a `RiskRecalculationClient` interface with a deterministic stub (configurable per-party outcomes incl. RC 8 review and RC 12 fatal) and defer the real binding.

**Recommended default: option 4 (stub) with the client seam shaped so options 2/3 are drop-in.**
- Client seam: `RiskRecalculationClient.recalculate(RiskRequest) → RiskResponse` in `backend/…/common/risk/`, records mirroring CVRISK01Y v0003 field-for-field (ids, capped exposure, currency/country, request type, channel, hop trace; response score/band/KYC/sanction/advice codes). Version is an explicit field, checked on response; mismatch → fatal per FR-013.
- Error/timeout behavior: per-party timeout maps to per-party RC 12 semantics (step stops, worst-seen RC preserved); transport failure distinct from business RC exactly as the dispatcher return area vs `CV-RISK-RC` today (analysis §7.1). No retries beyond the legacy behavior (none).
- Test doubles: stub is also the Phase 4 E2E test double, scripted to exercise FR-012/013/014 paths.
- Lead time: none for the stub; if the user picks option 3 at STOP 2, the gateway request must be raised before wave 5 starts.

### 4.2 CBCRD91 (absent batch error handler)
No `CBCRD91.cbl` exists (analysis §8). Options: (1) obtain the real source from the estate owner and migrate it; (2) reconstruct from the call-site contract (CVERRS01Y ERROR-AREA + abend semantics + online sibling `CACRD91.cbl`); (3) stub to plain logging; (4) legacy gateway (nonsensical for an in-process abend driver — listed for completeness).
**Recommended default: option 2 — reconstruct as the `ErrorReporter` component** (already the target-design decision, Error handling row): severity/type taxonomy from CVERRS01Y, abend code → exit 12 + structured log. Lead-time request (non-blocking, parallel): ask the estate owner whether CBCRD91 source can be produced; if it surfaces before wave 1 closes, diff the reconstruction against it.

### 4.3 Other exits from the stream (analysis §7.2)
| Crossing | Resolution options | Recommended default |
|---|---|---|
| `CICS-CARD-CLOSED` consumption / `CARDNITE-COMPLETE`+`CICS-CARD-OPEN-OK` production (scheduler conditions, FR-001/019) | orchestrator-native conditions; DB flags in `cycle_control`; deferral to whatever scheduler replaces the table | Model as `cycle_control` status transitions + CLI exit codes; the enterprise scheduler seam stays external (whoever orders the jobs reads exit codes). No mainframe lead time |
| Collections feed → CBCRD88J transmission (FR-017) | keep producing the LRECL-100 file for the existing transmission job; API to collections; stub | Keep the file contract byte-compatible (FB 100) and let the existing transmission arrangement stand; revisit with the collections system owner later |
| GL downstream pickup (`GL-CARD-FEED-READY`, rows in `gl_posting`) | table-is-the-interface (downstream reads `gl_posting`); export file; message | Table-is-the-interface + a completion marker on `cycle_control`; no change to downstream required |
| STGSTAT producer (external, unidentified — analysis §8) | locate the producer; synthesize from Spring Batch step statistics | Synthesize from the migrated jobs' own step statistics (the data is native to Spring Batch); flag to the user that the legacy producer was never found |
| CBCRD98/CBCRD98J, CBCRD99J operator utilities (absent) | recreate as admin CLI commands; defer | Recreate minimal equivalents in Phase 5 (cycle-control list; cycle backout for cold rerun of CBCRD04) — required by the runbook procedures FR-007 depends on |
| PGMROUT VSAM fallback (rebuilt by out-of-scope CBREF04J) | drop (single `pgm_route` table has no Db2-outage fallback split); keep a file snapshot | Drop — the fallback existed only because VSAM survived Db2 outages; PostgreSQL is the single store (target design already unifies both sources). Record as an accepted behavior change |

---

## 5. Data target sign-off items (PostgreSQL 16 — user approval required)

| # | Decision | Proposed mapping | Notes |
|---|---|---|---|
| D1 | Db2 schema | `db2/ddl/10_CARDDB_TABLES.sql` → Flyway V1 migrations, types mapped (CHAR→text/char, DECIMAL→numeric with exact scale, TIMESTAMP→timestamptz?) | **Sub-decision: timestamps** — legacy is zoneless; propose `timestamp` (no tz) + UTC convention |
| D2 | CYCLCTL → `cycle_control` | KSDS key (cycle_type, cycle_date) → PK; checkpoint block → columns; **branch flags and cycle-id promoted from the undocumented `CC-FILLER` overlay to named columns** (`fee_branch_status`, `interest_branch_status`, `branch_cycle_id`) | Promoting the overlay is a deliberate divergence from byte-compat — flags become first-class (FR-010) |
| D3 | AUTHLOG (ESDS) → `auth_log` | Append-only table with synthetic `bigserial` sequence standing in for the RBA; "delete/redefine" (FR-018) becomes truncate-after-verified-backup or day-partitioned tables | Prefer day-partitioning (backup = detach partition) — cleaner audit story; needs approval |
| D4 | MERCHRTE, CARDXREF, PGMROUT → `merch_route`, `card_xref`, `pgm_route` | KSDS keys → PKs; CARDXREF AIX → secondary index; PGMROUT collapses into `pgm_route` (see §4.3) | |
| D5 | GDG work datasets (AUTHEXTR→AUTHENR FB 300, PARTYWK/SRT/ACC/RVW FB 150, POSTREJ, FEEAUDIT, COLLECT FB 100, BKP.*) | Filesystem files via `FlatFileItemReader/Writer` with a `{dataset}.{cycleDate}.{gen}` naming convention standing in for GDG generations; COLLECT stays byte-compatible FB 100 (§4.3) | Alternative: staging tables — proposed only for PARTY* lists if E2E shows file handoffs hurt; default is files |
| D6 | Reports (FBA 133 SYSOUT ×10) | Generated as files per cycle date under a reports directory, layout-preserving where operators depend on them | Column-exact layouts resolved per program FR doc |
| D7 | Character/numeric semantics | EBCDIC→UTF-8 at the edge; COMP-3 → `BigDecimal(precision, scale)` per PIC; zoned overpunch only at file-parse boundaries; century window pivot 50 for legacy 6-digit dates (`CVCONSTY.cpy:9-12`) preserved in a shared date utility | |
| D8 | `pgm_route` seed | `40_SEED_PGM_ROUTE.sql` as a repeatable Flyway seed; VSAM-vs-Db2 divergence reconciliation flagged per target design | |

---

## 6. Sign-off gate

Stream sign-off = **every CARDNITE-FR acceptance criterion demonstrably satisfied**:
1. Traceability matrix (FR doc §6) fully populated — each of FR-001…019 has at least one automated covering test named, green in CI.
2. Phase 4 E2E suite (golden path + the three failure paths) green as a required CI check.
3. All §4 boundary decisions recorded (user-decided at STOP 2) and implemented or explicitly deferred with an owner.
4. All §5 data decisions approved and reflected in the Flyway baseline.
5. All FR §5.3 scheduler/program divergences (FR doc numbering) carry a written reconciliation in the owning per-program FR doc (incl. FORCEOPEN implemented-or-dropped in `CBCRD10_FR.md`).
6. No open wave defects (drift from `CARDSVC_target_design.md` is a wave defect by definition).

---

*Plan only — no scaffolding or code is created by this step. Phase 0 begins after STOP 2 decisions.*
