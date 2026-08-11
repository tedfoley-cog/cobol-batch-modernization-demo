# CBCRD91 — batch fatal error handler (ABSENT module — contract reconstructed, wave 1)

**No `CBCRD91.cbl` exists in the repo** (analysis §8): it is called by every CARDNITE batch program (e.g. `app/cardsvc/cbl/CBCRD01.cbl:529`), built at `app/jcl/build/BUILDALL.jcl:172`, bound at `app/jcl/build/BINDCARD.jcl:182`, and named in `app/cpy/CVCONSTY.cpy:20` and `app/cpy/CVERRS01Y.cpy:3`. This document reconstructs the contract from (a) the callers' ERROR-AREA usage and (b) the online sibling `CACRD91.cbl` severity semantics, per the approved STOP 2 decision: **reconstruct as the shared `ErrorReporter` component** (plan §4.2; target design "Error handling" row). A parallel source request to the estate owner is open; if source surfaces before wave 1 closes, diff the reconstruction against it.

## 1. Trigger / caller contract

Called statically on every fatal path: `CALL 'CBCRD91' USING ERROR-AREA` (call sites in all 15+ batch programs, analysis §2.1 — `CBCRD01.cbl:529`, `CBCRD02.cbl:658`, `CBCRD03.cbl:556`, `CBCRD04.cbl:1027`, `CBCRD05A.cbl:770`, `CBCRD05B.cbl:1000`, `CBCRD06A.cbl:405`, `CBCRD06B.cbl:615`, `CBCRD06C.cbl:594`, `CBCRD06W.cbl:262`, `CBCRD06X.cbl:318`, `CBCRD07.cbl:821`, `CBCRD08.cbl:719`, `CBCRD09.cbl:540`, `CBCRD10.cbl:804`).

ERROR-AREA (`app/cpy/CVERRS01Y.cpy:5-45`), populated by the caller before the call:
- severity;
- `ER-ERROR-TYPE`: SQL / VSAM / CICS / ROUT / DATA / BUSN;
- SQL detail block (SQLCODE etc.) and VSAM detail block (file status);
- `ER-MESSAGE` operator text;
- `ER-ABEND-CODE` 4-digit numeric — caller-selected (e.g. 0605 at `CBCRD06B.cbl:372`).

Observed caller-side semantics: populate ER-*, CALL, then the run terminates with user abend `U` + ER-ABEND-CODE and RC 12 — CBCRD91 never returns control on the fatal path (every caller treats the call as terminal; per-program abend tables at analysis §3).

## 2. Requirements owned

**CBCRD91-FR-001 — Uniform fatal-error reporting.** Every fatal condition in any CARDNITE job flows through one component that records severity, type, SQL/VSAM detail and operator message, then terminates the job with the caller's abend identity.
*Cross-ref:* fatal-path mechanics for all CARDNITE-FR ids (stream FR §7 program index).

**CBCRD91-FR-002 — Abend identity is preserved for operations.** The `U0xyz` code is the operator's routing key in the runbook (`docs/runbook-cardnite.md:91-112`); the migrated equivalent must keep the code visible.

## 3. Target mechanism

`ErrorReporter` component in `backend/…/common/`:
- API mirroring ERROR-AREA: `report(ErrorReport)` where `ErrorReport` is a record carrying severity, type enum (SQL/VSAM/CICS/ROUT/DATA/BUSN), detail blocks, message, abendCode.
- Behavior: structured SLF4J log (key=value, cycle date + job name per target design "Logging" row) then throw a terminal exception carrying `abendCode`; job exits **12** via `ExitCodeGenerator`, with the abend code in the exit log line (`U0605`-style token preserved for operator grep parity).
- Severity taxonomy from CACRD91 (`app/cardsvc/cbl/CACRD91.cbl`) — CACRD91 is the online sibling and its severity handling is the best available reference; batch differences (no CICS, always terminal) noted above.

## 4. Error / RC mapping

| Legacy | Target |
|---|---|
| U+ER-ABEND-CODE user abend, RC 12 | exception → job `ExitStatus` FAILED, process exit 12, abend code logged |
| ER-ERROR-TYPE routing (SQL vs VSAM detail) | typed detail fields on the structured log record |

Divergence note: the scheduler's ABEND-COND codes U4001–U4010 match nothing the source raises (FR §5.3 item 7); the ErrorReporter emits only source-derived codes — the scheduler mapping is retired with the scheduler table (see `CARDNITE_scheduler_FR.md`).

## 5. Hard-stop boundary

None — in-process component. Option 4 (gateway) was rejected as nonsensical for an in-process abend driver (plan §4.2).

## 6. Acceptance criteria

- Every migrated job's fatal path calls `ErrorReporter` and exits 12 — no job ever exits 12 without a structured error record.
- The abend code for each legacy fatal condition (per-program tables in the owning FR docs) appears verbatim in the log output.
- SQL failures carry SQLCODE-equivalent detail (SQLSTATE/vendor code); file failures carry the file/path detail.
- `ErrorReporter` never swallows: after `report(...)` the step/job terminates.
- If CBCRD91 source is obtained, a recorded diff of reconstruction vs source exists before wave 1 sign-off.
