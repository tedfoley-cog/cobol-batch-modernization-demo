# CBCRD06B — exposure recalculation dispatch (CBCRD06J STEP030, wave 5) — the crossing caller

Target: step of Spring Batch job `cbcrd06`, calling the wave-2 `RiskRecalculationClient` seam.
Stream FRs owned: CARDNITE-FR-012 (outcome capture), FR-013 (external caller contract), FR-014 (manual review).

## 1. Trigger / caller contract

Gated `IF RC <= 4` after STEP020 (`app/jcl/cardsvc/CBCRD06J.jcl:101-113`); STEPLIB concatenates `PRSK.PROD.LOADLIB` — the only step with PARTYRSK code visible (`CBCRD06J.jcl:98,103-109`). Built DYNAM (`app/jcl/build/BUILDALL.jcl:193`).
PARM: cycle date only (`CBCRD06J.jcl:113`; parsed `app/cardsvc/cbl/CBCRD06B.cbl:230-236`).

## 2. Field-level inputs / outputs

- Files: PARTYSRT in, PARTYACC accepted out, PARTYRVW manual-review out — all FB 150 CVPWRK01Y; RISKEXC exception listing FBA 133 (`CBCRD06B.cbl:30-33,62-99`; `CBCRD06J.jcl:116-128`).
- Per party: populates CV-RISK-AREA request (see RSKRECAL_contract_FR §2), calls `CALL 'CBCRD90' USING ROUTE-REQUEST CV-RISK-AREA WS-RETURN-AREA` (`CBCRD06B.cbl:359-361`), consumes the response into PW-OUTCOME (PW-RC, reason, advice, sanction flag, score, dispatch timestamp — `app/cardsvc/cpy/CVPWRK01Y.cpy:25-34`).
- Db2: INSERT `CARDSVC.ROUTE_AUDIT` per crossing (`CBCRD06B.cbl:533-556`); commit every `WS-COMMIT-FREQUENCY` dispatches (`CBCRD06B.cbl:287-290`). Note: this insert is **in addition to** the dispatcher's own audit insert (`CBCRD90.cbl:343-375`, CBCRD90-FR-004) — legacy writes **two** `ROUTE_AUDIT` rows per crossing (one caller-side, one dispatcher-side); parity keeps both.

## 3. Requirements owned

**CBCRD06B-FR-001 — Per-party dispatch and routing of outcomes.** RC 0 → PARTYACC; RC 4 → PARTYACC + RISKEXC listing; RC 8 → PARTYRVW, excluded from STEP040, step RC 8; RC 12 → fatal stop (`CBCRD06B.cbl:16-25,411-429`; `docs/runbook-cardnite.md:138-162`). Worst-seen RC becomes the step RC. Cross-ref CARDNITE-FR-012/014.

**CBCRD06B-FR-002 — Version guard.** Response commarea version below 0003 → U0607 fatal (`CBCRD06B.cbl:376-381`); CARDNITE-FR-013.

**CBCRD06B-FR-003 — Transport vs business RC.** The dispatcher return area is only a transport indicator; the higher of dispatcher RC and CV-RISK-RC wins (`CBCRD06B.cbl:383-388`).

## 4. Target mechanism

Chunk step over PARTYSRT records; per record `RiskRecalculationClient.recalculate(...)` (stub initially, plan §4.1); classifier writer to accepted/review files + exception report; `route_audit` insert per crossing in the chunk transaction.

## 5. Error / edge behavior and RC mapping

| RC | Meaning |
|---|---|
| 0 | all accepted |
| 4 | warnings listed |
| 8 | ≥1 party diverted to manual review — STEP040 still runs for accepted parties |
| 12 | fatal (U0606 crossing fatal; U0602 file; U0607 version) |

Abend table: `CBCRD06B.cbl:41-44,367-381`. Runbook maps the fatal path to U0632 (`docs/runbook-cardnite.md:103,148`) — **reconciliation:** source U0606 governs; U0632 is a runbook alias, retired.

**Divergence reconciliation (FR §5.3 — ≤4 vs ≤8 gate):** the header says STEP040 "can be gated with IF RC <= 4" (`CBCRD06B.cbl:24-25`) but the JCL gates `IF RC <= 8` (`CBCRD06J.jcl:138`). Resolution: **JCL governs** — RC 8 is the manual-review business outcome and accepted parties must still be applied (CARDNITE-FR-014/015).

**Divergence reconciliation (unreachable U0605):** CBCRD90 returns route-not-found/table-error only in `LK-RETURN-CD` without copying `RQ-RC` back (`app/cardsvc/cbl/CBCRD90.cbl:98-100,115-126`), so CBCRD06B's `RQ-RC-NOT-FOUND`/`RQ-RC-TABLE-ERROR` tests (`CBCRD06B.cbl:367-374`) never fire; the dispatcher RC folds into `CV-RISK-RC` (`CBCRD06B.cbl:383-388`) and every party diverts to manual review with an unpopulated risk area, step RC 8 (`CBCRD06B.cbl:394-421`). Resolution: the target makes the intended behavior real — a missing/unavailable risk route is a **configuration failure that stops the step fatally (exit 12, U0605-equivalent)** rather than flooding Financial Crime with empty reviews; recorded as a deliberate correction of a latent defect (plan Phase 5 divergence closure).

Operator doctrine: never re-run STEP030 alone to clear an RC 8; reviewed parties stay excluded until Financial Crime releases them (`docs/runbook-cardnite.md:150-159`).

## 6. Hard-stop boundary

Everything past `RiskRecalculationClient` is out of scope (see RSKRECAL_contract_FR). This step never names or embeds PARTYRSK logic (`CBCRD06B.cbl:6-12` parity).

## 7. Acceptance criteria

- Stub scripted RC 0/4/8/12 per party routes records to PARTYACC/RISKEXC/PARTYRVW/fatal exactly per FR-001; step exit is worst-seen.
- Version-mismatch response fails the step fatally.
- Unresolvable risk route fails the step fatally (corrected behavior) — no review-file flood.
- Two `route_audit` rows per dispatched party — the caller-side insert plus the dispatcher's (legacy parity, §2); consolidating to one would be a divergence requiring sign-off.
- Re-driving after a mid-run failure does not re-dispatch parties already committed (checkpoint parity with `WS-COMMIT-FREQUENCY` commits).
