# CARDSVC Target Design Reference

Confirmed at STOP 0 (session c8e42fa3). Governs all migrated CARDSVC code in this
repository. Drift from this document is a wave defect.

## Stack

| Concern | Decision |
|---|---|
| Language / runtime | Java 21 (LTS) |
| Framework | Spring Boot 3.x |
| Batch runtime | Spring Batch 5.x — one Spring Batch `Job` per scheduler job (`CBCRD01J`..`CBCRD10J`, including the forked legs `CBCRD05AJ`/`CBCRD05BJ`), one `Step` per JCL step that runs application code |
| Persistence | Spring Data JPA (Hibernate) over PostgreSQL; explicit `@Transactional` boundaries; chunk-oriented steps commit per chunk mirroring each program's own COBOL commit/checkpoint behavior, resolved per job in its FR doc — where the interval is operator-tunable (e.g. CBCRD04 reads `CC-COMMIT-FREQ` from the cycle control record, falling back to its own constant when zero, `app/cardsvc/cbl/CBCRD04.cbl:334-335`) the chunk size is bound to the `cycle_control` row, not hardcoded |
| Data target | PostgreSQL 16. Db2 DDL in `db2/ddl/` translated to Flyway migrations. VSAM clusters become tables — KSDS (CYCLCTL, CARDXREF, ...) keyed on the cluster key, and the AUTHLOG ESDS (append-only, entry-sequenced — `NONINDEXED` per `app/jcl/vsam/DEFCARD.jcl:37-47`) as an append-only table with a synthetic sequence; QSAM/GDG work datasets become filesystem files (Spring Batch `FlatFileItemReader/Writer`) or tables where the plan says so |
| DTO / mapping | Java `record` DTOs at job/step boundaries; MapStruct for entity↔DTO mapping |
| Error handling | Estate return-code convention preserved: each job exits 0 / 4 / 8 / 12 via `ExitCodeGenerator`; the batch error handler `CBCRD91` (called by every job but ABSENT from the repo — no `CBCRD91.cbl` exists) becomes a shared `ErrorReporter` component whose contract is derived from the callers' `ERROR-AREA` usage and the online `CACRD91.cbl` severity semantics; the absence is flagged as an absent-module item in the stream analysis |
| Logging | SLF4J + Logback, structured key=value messages carrying cycle date and job name |
| Testing | JUnit 5, AssertJ, Testcontainers (PostgreSQL) for integration tests; Spring Batch `JobLauncherTestUtils` for job-level tests |
| Build / CI | Maven, GitHub Actions (`cardsvc-ci.yml`): build, unit + integration tests |
| Scheduling seam | Jobs are launchable via CLI (`spring.batch.job.name=<job>`) with `cycleDate` required for every job plus per-job parameters mirroring each JCL `PARM` exactly, resolved in the job's FR doc — e.g. CBCRD01 `cycleId` (`PARM='&CYCDATE,&CYCID'`), CBCRD02 `tolerancePct` (`&TOLER`), CBCRD04 `restart=Y|N` (safety-critical cold/warm switch), CBCRD06 `waitLimit` (`WAIT=030`), CBCRD09 cycleDate only. Note: `FORCEOPEN` for CBCRD10 exists only in scheduler operator note 4 (`sched/CARDNITE.sched:416-419`) and is NOT parsed by CBCRD10 (`CBCRD10.cbl:325-331` reads only the CCYYMMDD cycle date) — the CBCRD10 FR doc must resolve whether it is implemented or dropped |

## Layout and conventions

```
backend/
  pom.xml
  src/main/java/com/cardsvc/
    common/            shared components (cycle control, routing, error reporter)
    jobs/cbcrd01..cbcrd04, cbcrd05a, cbcrd05b, cbcrd06..cbcrd10   one package per job
    domain/            JPA entities per CARDSVC table
    repository/        Spring Data repositories
  src/main/resources/db/migration/   Flyway migrations
  src/test/java/...
```

- Package-per-job, discovered by convention (`@Configuration` per job) — no shared
  hand-edited registry, so waves never conflict by construction.
- Legacy routing has two physical sources: the batch dispatcher `CBCRD90` loads
  its route cache from Db2 `CARDSVC.PGM_ROUTE` first, falling back to the
  `PGMROUT` VSAM KSDS (`CARD.PROD.PGMROUT`, `app/jcl/vsam/DEFCARD.jcl:152-165`)
  when Db2 is unavailable (`app/cardsvc/cbl/CBCRD90.cbl:133-150`); the online
  dispatcher `CACRD90` resolves from a TSQ cache first, then the Db2 table, then
  the same `PGMROUT` VSAM fallback (`app/cardsvc/cbl/CACRD90.cbl:130-155`). Both
  converge onto the single migrated
  `pgm_route` table; the migration seeds it from the Db2 seed
  (`db2/ddl/40_SEED_PGM_ROUTE.sql`) and the analysis must reconcile and flag any
  divergence between the VSAM cluster contents and the Db2 rows.
- The dispatch maps to a `ProgramRouter` component that
  resolves handler beans from the migrated `pgm_route` table by `(routeType,
  routeKey)` filtered on `ACTIVE_FLG` and the EFF_DATE/EXP_DATE window, ordered
  by `SEQ_NBR` (pipelines like FRAU return an ordered handler list), preserving
  fallback semantics via `FALLBACK_PGM`.
- RC 4 "warning/partial" legs (e.g. CBCRD06's PARTIAL refresh) map to a
  per-step `ExitStatus` contribution resolved in each job's FR doc, so a warning
  never surfaces as FAILED. The default job-level exit code is the max of its
  steps' mapped RCs, but each FR doc defines the job's RC mapping explicitly —
  where a specific RC is a distinct business outcome (e.g. CBCRD08's RC 8 =
  OUTOFBAL) that code is reserved for that outcome only, and generic step
  failures map to 12, never to a reserved business RC. Where scheduler and
  program disagree on an RC's meaning (e.g. CBCRD02 RC 4 is `EMPTY` in
  `sched/CARDNITE.sched` but "rejects within tolerance" in
  `app/cardsvc/cbl/CBCRD02.cbl:25-28`), the FR doc must reconcile the
  divergence explicitly before migration.
- Restartability: the cycle control record (`CYCLCTL`) becomes a `cycle_control`
  table; checkpointed jobs resume from `cc_last_key`, matching operator note 3 in
  `sched/CARDNITE.sched`.
- Money is `BigDecimal` with explicit scale matching the packed-decimal PIC clauses;
  no floating point in business logic.
- Labels/messages: English, preserving the estate's operator-facing message texts
  where operators depend on them (e.g. branch-flag log lines checked by the runbook).
