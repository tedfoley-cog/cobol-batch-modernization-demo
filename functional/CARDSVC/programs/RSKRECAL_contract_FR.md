# RSKRECAL — cross-module risk recalculation caller contract (wave 2 seam)

**Caller contract ONLY.** PARTYRSK/PRBRSK1 internals are past the hard stop and are not documented or migrated here. Approved STOP 2 resolution: **explicit stub behind a drop-in `RiskRecalculationClient` seam** (plan §4.1).
Stream FRs: CARDNITE-FR-012 (outcome capture), FR-013 (external caller contract), FR-014 (manual review).

## 1. Trigger / caller contract

Sole caller: CBCRD06B, once per party. Route request `RQ-ROUTE-TYPE='XMOD'`, `RQ-ROUTE-KEY='RSKRECAL'`, `RQ-SEQ-NBR=1` (`app/cardsvc/cbl/CBCRD06B.cbl:348-353`; key constant `app/cpy/CVCONSTY.cpy:25`), resolved by CBCRD90 to `PRBRSK1`, call type 'D', module PARTYRSK, **no fallback program** (`db2/ddl/40_SEED_PGM_ROUTE.sql:42`). CBCRD06B deliberately never names the target (`CBCRD06B.cbl:6-12`). Transport: `CALL 'CBCRD90' USING ROUTE-REQUEST, CV-RISK-AREA, WS-RETURN-AREA` (`CBCRD06B.cbl:359-361`); the risk area passes through as PIC X(512) (`CBCRD90.cbl:86,297`).

## 2. Exchanged data — CV-RISK-AREA (CVRISK01Y, 512 bytes, version 0003)

Layout `app/cpy/CVRISK01Y.cpy:11-86`; coordinated rebind + version bump on change (`CVRISK01Y.cpy:8-9`).

Request (set per party, `CBCRD06B.cbl:300-341`):
- version `0003`; caller id/mod; correlation id `RCAL`+yymmdd+seq;
- channel **'B'** (batch); request type **'RCAL'**;
- party/cust/acct/card ids; exposure amount capped at S9(9)V99 max (`CBCRD06B.cbl:320-329`); currency USD; country USA;
- hop trace seeded with the caller.

Response (consumed at `CBCRD06B.cbl:394-407`):
- `CV-RISK-RC` (per-party outcome), risk score, band A/B/C/X, KYC status, sanction flag, exposure amount, advice/reason codes;
- version checked on return — down-level → U0607 fatal (`CBCRD06B.cbl:376-381`).

Boundary-visible target validation (caller-side knowledge only): the target accepts batch-only — request type 'RCAL' and channel 'B', else RC 12 (`app/partyrsk/cbl/PRBRSK1.cbl:8-11,239-258`); it owns the Db2 unit of work for its own chain (`PRBRSK1.cbl:13-17`).

## 3. Per-party RC contract (`CBCRD06B.cbl:16-25,411-429`; `docs/runbook-cardnite.md:138-162`)

| CV-RISK-RC | Meaning | Caller action |
|---|---|---|
| 0 | accepted | write PARTYACC |
| 4 | accepted with warning | write PARTYACC + RISKEXC exception listing |
| 8 | serious (sanctions/KYC) | write PARTYRVW, exclude from STEP040, step RC 8, Financial Crime review same morning |
| 12 | fatal | step abends (source U0606; runbook U0632 — divergence, analysis §8) |

The dispatcher return area is a transport-failure indicator only; the higher of dispatcher RC and CV-RISK-RC wins (`CBCRD06B.cbl:383-388`).

## 4. Target mechanism (approved)

`RiskRecalculationClient.recalculate(RiskRequest) → RiskResponse` in `backend/…/common/risk/`, records mirroring CVRISK01Y v0003 field-for-field; version an explicit checked field (mismatch → fatal per FR-013). Initial binding: **deterministic stub** with configurable per-party outcomes (incl. RC 8 review and RC 12 fatal) — also the Phase 4 E2E test double. Seam shaped so an already-migrated PARTYRSK service (option 2) or a mainframe gateway (option 3) is drop-in; option 3 carries a lead-time request (gateway + EBCDIC↔JSON mapping of v0003) raised before wave 5 if chosen.

## 5. Error / timeout behavior

- Per-party timeout maps to per-party RC 12 semantics: step stops, worst-seen RC preserved (plan §4.1).
- Transport failure stays distinct from business RC, exactly as the dispatcher return area vs CV-RISK-RC today.
- No retries (legacy has none).
- Known legacy caveat carried at the boundary: CBCRD90's `CANCEL` per call degrades the target's every-N commit assumption to per-party (`CBCRD90.cbl:305-306`, `PRBRSK1.cbl:51-52`) — the client seam makes no batching promise either.

## 6. Hard-stop boundary

Everything beyond the CV-RISK-AREA exchange — scoring, KYC, sanctions logic, PARTYRSK data — is out of scope. This document must never grow PARTYRSK internals.

## 7. Acceptance criteria

- `RiskRequest`/`RiskResponse` field set is bijective with CVRISK01Y v0003 (checked by a mapping test against the copybook layout).
- Version mismatch on response fails the step fatally (exit 12).
- Stub scripts each RC 0/4/8/12 path and drives the FR-012/013/014 acceptance tests (accepted, warning listing, review diversion, fatal stop).
- Request population parity: channel 'B', type 'RCAL', exposure capped at S9(9)V99 max, correlation id format `RCAL`+yymmdd+seq.
- Swapping the stub for another `RiskRecalculationClient` implementation requires no caller change (seam test compiles against the interface only).
