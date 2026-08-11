# CBFEE02 — over-limit fee and cash advance fee handler (dispatched leaf, wave 2)

Target: Spring component behind `ProgramRouter` in `backend/…/common/fees/` (plan §3 — app-tier, no SP).
Stream FRs: CARDNITE-FR-008.

## 1. Trigger / caller contract

Reached only dynamically: routes FEEC/OVLM and FEEC/CASH (`app/cardsvc/cbl/CBFEE02.cbl:4-5`; seed `db2/ddl/40_SEED_PGM_ROUTE.sql:68-69`). Same parameter pair as CBFEE01: FEE-WORK-AREA (CVFEEW1Y) + BATCH-RETURN-AREA (CVBRTN1Y) (`CBFEE02.cbl:156-160`); FEEPARM 80-byte KEYWORD/VALUE cards (`CBFEE02.cbl:41-56`); cancelled after every call.

## 2. Field-level inputs / outputs

- Reads `CARDSVC.FEE_SCHEDULE`, `CARDSVC.TRANSACTION`, FEEPARM cards (`CBFEE02.cbl:9-10`).
- Writes INSERT `CARDSVC.TRANSACTION` (`CBFEE02.cbl:531`), UPDATE `CARDSVC.ACCOUNT` (`CBFEE02.cbl:581-597`).
- Cash-advance basis comes from `FW-AUTH-IMAGE` — the 60-byte CVAUTH01Y detail area addressed through the `AUTH-CASH-ADV` REDEFINES under `EVALUATE AUTH-TYPE` (`CBFEE02.cbl:19-27`).
- Outputs: BR-RETURN-CD, BR-FEE-TOTAL, BR-FEE-COUNT.

## 3. Requirements owned

**CBFEE02-FR-001 — Over-limit fee only with opt-in.** Without the account's opt-in the bank may not charge, whatever the excess (`CBFEE02.cbl:13-14`).

**CBFEE02-FR-002 — One over-limit fee per cycle, re-drive safe.** The TRANSACTION table is checked for an existing over-limit fee before posting, because the nightly cycle can be re-driven after an abend (`CBFEE02.cbl:15-17`).

**CBFEE02-FR-003 — Cash advance fee.** A percentage of the advance with a minimum, taken from the authorization image (`CBFEE02.cbl:19-21`).

**CBFEE02-FR-004 — Untrusted image tail.** The CASH variant is shorter than the 60-byte area; the tail bytes hold whatever the previous record left and must not be trusted — the currency is validated before use (`CBFEE02.cbl:23-27`).

## 4. Target mechanism

`OverlimitCashFeeHandler` bean for FEEC/OVLM and FEEC/CASH. The auth-image overlay becomes a typed parse of the CVAUTH01Y cash-advance variant with explicit field-boundary validation (FR-004 above becomes a parser rule, not a byte convention). Idempotency check (FR-002) preserved as a `transaction` existence query in the same chunk transaction.

## 5. Error / edge behavior and RC mapping (`CBFEE02.cbl:29-33`)

| RC | Meaning | Target |
|---|---|---|
| 00 | fee assessed | OK |
| 04 | nothing to assess | OK-no-action |
| 08 | no schedule row, not my fee type, or **unusable image** | declined-fee outcome |
| 12 | SQL failure — caller must back out | exception; caller rolls back |

## 6. Hard-stop boundary

None — in-module leaf.

## 7. Acceptance criteria

- No over-limit fee is ever assessed for a non-opted-in account regardless of excess.
- Re-running the fee step over the same cycle produces no second over-limit fee (FR-002 idempotency test).
- Cash advance fee = configured percentage with minimum applied, computed from validated image fields only; an image failing currency validation returns 08 and assesses nothing.
- SQL failure leaves no partial fee.
