# CARDSVC Target Design Reference

Confirmed at STOP 0 (session c8e42fa3). Governs all migrated CARDSVC code in this
repository. Drift from this document is a wave defect.

## Stack

| Concern | Decision |
|---|---|
| Language / runtime | Java 21 (LTS) |
| Framework | Spring Boot 3.x |
| Batch runtime | Spring Batch 5.x — one Spring Batch `Job` per scheduler job (`CBCRD01J`..`CBCRD10J`, including the forked legs `CBCRD05AJ`/`CBCRD05BJ`), one `Step` per JCL step that runs application code |
| Persistence | Spring Data JPA (Hibernate) over PostgreSQL; explicit `@Transactional` boundaries; chunk-oriented steps commit per chunk mirroring each program's own COBOL commit/checkpoint interval, resolved per job in its FR doc (e.g. CBCRD04's `CHECKPOINTS EVERY 1000 RECORDS` → `chunk(1000)`; other jobs use their own thresholds) |
| Data target | PostgreSQL 16. Db2 DDL in `db2/ddl/` translated to Flyway migrations. VSAM clusters become tables — KSDS (CYCLCTL, CARDXREF, ...) keyed on the cluster key, and the AUTHLOG ESDS (append-only, entry-sequenced per `app/jcl/vsam/DEFCARD.jcl:32`) as an append-only table with a synthetic sequence; QSAM/GDG work datasets become filesystem files (Spring Batch `FlatFileItemReader/Writer`) or tables where the plan says so |
| DTO / mapping | Java `record` DTOs at job/step boundaries; MapStruct for entity↔DTO mapping |
| Error handling | Estate return-code convention preserved: each job exits 0 / 4 / 8 / 12 via `ExitCodeGenerator`; `CBCRD91`-style error logging becomes a shared `ErrorReporter` component |
| Logging | SLF4J + Logback, structured key=value messages carrying cycle date and job name |
| Testing | JUnit 5, AssertJ, Testcontainers (PostgreSQL) for integration tests; Spring Batch `JobLauncherTestUtils` for job-level tests |
| Build / CI | Maven, GitHub Actions (`cardsvc-ci.yml`): build, unit + integration tests |
| Scheduling seam | Jobs are launchable via CLI (`spring.batch.job.name=<job>`) with `cycleDate` as a required job parameter, mirroring the JCL `PARM` / `SET CYCDATE=` |

## Layout and conventions

```
backend/
  pom.xml
  src/main/java/com/cardsvc/
    common/            shared components (cycle control, routing, error reporter)
    jobs/cbcrd01..cbcrd10, cbcrd05a, cbcrd05b   one package per migrated job
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
- Restartability: the cycle control record (`CYCLCTL`) becomes a `cycle_control`
  table; checkpointed jobs resume from `cc_last_key`, matching operator note 3 in
  `sched/CARDNITE.sched`.
- Money is `BigDecimal` with explicit scale matching the packed-decimal PIC clauses;
  no floating point in business logic.
- Labels/messages: English, preserving the estate's operator-facing message texts
  where operators depend on them (e.g. branch-flag log lines checked by the runbook).
