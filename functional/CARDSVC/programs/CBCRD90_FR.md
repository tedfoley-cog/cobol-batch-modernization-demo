# CBCRD90 — batch program dispatcher (shared component, wave 1)

Target component: `ProgramRouter` in `backend/src/main/java/com/cardsvc/common/` (target design "ProgramRouter" convention).
Stream FRs carried: mechanics for CARDNITE-FR-008 (FEEC dispatch) and CARDNITE-FR-013 (XMOD/RSKRECAL crossing).

## 1. Trigger / caller contract

Not a job — a statically-called component (`CALL 'CBCRD90'`) used by CBCRD05A per fee type (`app/cardsvc/cbl/CBCRD05A.cbl:495-508`) and CBCRD06B per party (`CBCRD06B.cbl:359-361`). Callers must be compiled DYNAM (`app/jcl/build/BUILDALL.jcl:190-195`).

Interface (`app/cardsvc/cbl/CBCRD90.cbl:84-95`):

| Parameter | Layout | Direction |
|---|---|---|
| `LK-ROUTE-REQUEST` PIC X(38) | ROUTE-REQUEST, CVROUT01Y (`app/cpy/CVROUT01Y.cpy:53-64`): RQ-ROUTE-TYPE X(4), RQ-ROUTE-KEY X(8), RQ-SEQ-NBR 9(4), RQ-RESOLVED-PGM X(8), RQ-RESOLVED-CALL X(1), RQ-RESOLVED-MOD X(8), RQ-USED-FALLBACK X(1), RQ-RC 9(4) | in/out |
| `LK-PARM-AREA` PIC X(512) | opaque pass-through to the target (FEE-WORK-AREA or CV-RISK-AREA) | in/out |
| `LK-RETURN-AREA` | LK-RETURN-CD 9(4) + program + message (`CBCRD90.cbl:88-91`) | out |

## 2. Behavior (requirements owned)

**CBCRD90-FR-001 — Route resolution from the routing table, date/active filtered.**
The dispatcher resolves `(ROUTE_TYPE, ROUTE_KEY, SEQ_NBR)` against `CARDSVC.PGM_ROUTE` with `ACTIVE_FLG='Y' AND CURRENT DATE BETWEEN EFF_DATE AND EXP_DATE` (`CBCRD90.cbl:70-82`), caching up to 200 routes per run unit (`app/cpy/CVROUT01Y.cpy:36-51`, `CBCRD90.cbl:133-149`).
*Target:* `ProgramRouter` resolves handler beans from `pgm_route` by the exact caller-supplied triple `(routeType, routeKey, seqNbr)` with the same active/date filter — the sequence number is part of the resolution key, not an ordering hint (callers pass seq 1: `CBCRD05A.cbl:500`, `CBCRD06B.cbl:352`; multi-row keys such as FRAU/HIGHRISK seq 1-3 exist in the seed, `40_SEED_PGM_ROUTE.sql:55-57`).

**CBCRD90-FR-002 — VSAM fallback on route-store outage.**
When the Db2 load fails, routes load from VSAM `CARD.PROD.PGMROUT` (`CBCRD90.cbl:133-146,217-261`).
*Target resolution (plan §4.3, accepted behavior change):* dropped — single `pgm_route` table; PostgreSQL is the only store.

**CBCRD90-FR-003 — Dynamic call with fallback target and post-call cancel.**
The resolved target is called dynamically (`CALL WS-PGM-NAME`, `CBCRD90.cbl:294-304`); on load failure the row's `FALLBACK_PGM` is retried (`CBCRD90.cbl:311-341`); after every call the module is `CANCEL`ed so a changed route takes effect on re-drive (`CBCRD90.cbl:305-306`).
*Target:* Spring bean resolution replaces load/cancel; `FALLBACK_PGM` semantics preserved via a fallback bean name on the route row. Note the PRBRSK1 counter caveat: `CANCEL` between parties degrades PRBRSK1's every-N commit to per-call (analysis §7.1) — do not "fix" without a boundary decision.

**CBCRD90-FR-004 — Every dispatch is audited.**
Each dispatch inserts `CARDSVC.ROUTE_AUDIT` with correlation id, route, target, outcome (`CBCRD90.cbl:343-375`).
*Target:* `route_audit` insert per dispatch in the same transaction scope as today (audit row survives handler failure exactly as the legacy commit scope dictates — resolve scope during wave 1 implementation with a test).

## 3. Error / RC contract to callers

| Condition | LK-RETURN-CD | Source |
|---|---|---|
| Route table unavailable (Db2 + VSAM both fail) | 12, 'ROUTE TABLE UNAVAILABLE' | `CBCRD90.cbl:108-113` |
| Route not found | 8, 'ROUTE NOT FOUND' | `CBCRD90.cbl:115-121` |
| Target and fallback not loadable | 12 | `CBCRD90.cbl:325-337` |
| Handler completed | handler's RC propagated | `CBCRD90.cbl:294-309` |

**Known latent defect (do not replicate):** on the not-found and table-error exits CBCRD90 returns before copying `ROUTE-REQUEST` back to the caller, so `RQ-RC` is never set for the caller (`CBCRD90.cbl:98-100,115-126`); CBCRD06B's `RQ-RC`-based U0605 check is unreachable (FR doc §3.3). The target `ProgramRouter` must surface route-not-found/table-error distinctly and deterministically to callers.

## 4. Hard-stop boundary

CBCRD90 is the carrier of the XMOD/RSKRECAL crossing but owns none of PRBRSK1's behavior. The only PARTYRSK knowledge permitted here is the seed row `XMOD/RSKRECAL → PRBRSK1, call type 'D', module PARTYRSK` (`db2/ddl/40_SEED_PGM_ROUTE.sql:42`). In the target, the router dispatches XMOD routes to the `RiskRecalculationClient` seam (plan §4.1) — never to PARTYRSK code.

## 5. Acceptance criteria

- Resolving each seeded FEEC key (ANNU/LATE/OVLM/CASH/FRGN → CBFEE01/02/02/02/03 handlers) returns the same target as `db2/ddl/40_SEED_PGM_ROUTE.sql:66-70`.
- Inactive or out-of-date-window routes are never resolved.
- Route-not-found is reported to the caller as a distinct outcome (RC 8 equivalent) — visibly, unlike the legacy copy-back defect.
- Unloadable target with a configured fallback dispatches the fallback and flags `usedFallback`.
- Every successful resolution produces exactly one `route_audit` row (legacy parity: `4000-AUDIT-DISPATCH` runs only after resolution, `CBCRD90.cbl:123-124`; not-found/table-error exits leave no audit row, `CBCRD90.cbl:385-395`). **Target improvement (explicit):** failed resolutions are also audited, and audit-insert failures are surfaced rather than ignored (legacy ignores the INSERT SQLCODE, `CBCRD90.cbl:373-375`).
- A route change is honored by the next dispatch (legacy CANCEL semantics).
