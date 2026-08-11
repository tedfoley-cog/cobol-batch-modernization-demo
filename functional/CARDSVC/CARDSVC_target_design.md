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
| Data target | PostgreSQL 16. Db2 DDL in `db2/ddl/` translated to Flyway migrations. VSAM clusters become tables — KSDS (CYCLCTL, CARDXREF, ...) keyed on the cluster key, and the AUTHLOG ESDS (append-only, entry-sequenced per `app/jcl/vsam/DEFCARD.jcl:32`) as an append-only table with a synthetic sequence; QSAM/GDG work datasets become filesystem files (Spring Batch `FlatFileItemReader/Writer`) or tables where the plan says so |
| DTO / mapping | Java `record` DTOs at job/step boundaries; MapStruct for entity↔DTO mapping |
| Error handling | Estate return-code convention preserved: each job exits 0 / 4 / 8 / 12 via `ExitCodeGenerator`; the batch error handler `CBCRD91` (called by every job but ABSENT from the repo — no `CBCRD91.cbl` exists) becomes a shared `ErrorReporter` component whose contract is derived from the callers' `ERROR-AREA` usage and the online `CACRD91.cbl` severity semantics; the absence is flagged as an absent-module item in the stream analysis |
| Logging | SLF4J + Logback, structured key=value messages carrying cycle date and job name |
| Testing | JUnit 5, AssertJ, Testcontainers (PostgreSQL) for integration tests; Spring Batch `JobLauncherTestUtils` for job-level tests |
| Build / CI | Maven, GitHub Actions (`cardsvc-ci.yml`): build, unit + integration tests |
| Scheduling seam | Jobs are launchable via CLI (`spring.batch.job.name=<job>`) with `cycleDate` required for every job plus per-job parameters mirroring each JCL `PARM` exactly, resolved in the job's FR doc — e.g. CBCRD01 `cycleId` (`PARM='&CYCDATE,&CYCID'`), CBCRD02 `tolerancePct` (`&TOLER`), CBCRD04 `restart=Y|N` (safety-critical cold/warm switch), CBCRD06 `waitLimit` (`WAIT=030`), CBCRD09 cycleDate only |

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
- The `PGM_ROUTE` dispatch table (CBCRD90) maps to a `ProgramRouter` component that
  resolves handler beans from the migrated `pgm_route` table by `(routeType,
  routeKey)` filtered on `ACTIVE_FLG` and the EFF_DATE/EXP_DATE window, ordered
  by `SEQ_NBR` (pipelines like FRAU return an ordered handler list), preserving
  fallback semantics via `FALLBACK_PGM`.
- RC 4 "warning/partial" legs (e.g. CBCRD06's PARTIAL refresh) map to a
  per-step `ExitStatus` contribution resolved in each job's FR doc, so a warning
  never surfaces as FAILED; the job-level exit code is the max of its steps'
  mapped RCs.
- Restartability: the cycle control record (`CYCLCTL`) becomes a `cycle_control`
  table; checkpointed jobs resume from `cc_last_key`, matching operator note 3 in
  `sched/CARDNITE.sched`.
- Money is `BigDecimal` with explicit scale matching the packed-decimal PIC clauses;
  no floating point in business logic.
- Labels/messages: English, preserving the estate's operator-facing message texts
  where operators depend on them (e.g. branch-flag log lines checked by the runbook).
