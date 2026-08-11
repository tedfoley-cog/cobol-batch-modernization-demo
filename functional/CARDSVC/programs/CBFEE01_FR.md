# CBFEE01 — annual fee and late fee handler (dispatched leaf, wave 2)

Target: Spring component behind `ProgramRouter` in `backend/…/common/fees/` (plan §3 — app-tier, no SP).
Stream FRs: CARDNITE-FR-008 (fee assessment).

## 1. Trigger / caller contract

Reached only dynamically: CBCRD05A moves `FEEC` + `ANNU` or `LATE` into the route request and calls CBCRD90, which calls CBFEE01 (`app/cardsvc/cbl/CBFEE01.cbl:4-9`; seed rows `db2/ddl/40_SEED_PGM_ROUTE.sql:66-67`). Lives in `CARD.PROD.FEELIB`, on STEPLIB only in CBCRD05AJ (`app/jcl/cardsvc/CBCRD05AJ.jcl:37-43`).

Parameters (`CBFEE01.cbl:177-181`):
- `FEE-WORK-AREA` (CVFEEW1Y, 512 bytes): FW-FEE-TYPE, account snapshot, fee basis incl. FW-AUTH-IMAGE X(60), prior-cycle flags (`app/cardsvc/cpy/CVFEEW1Y.cpy:20-73`); FR-CALLER-ID = 'CBCRD05A'.
- `BATCH-RETURN-AREA` (CVBRTN1Y): BR-RETURN-CD + BR-EXTENSION fee totals/count/waiver/lines/hash (`app/cardsvc/cpy/CVBRTN1Y.cpy:4-32`).

CBCRD90 cancels the module after every call — no state survives between fees; FEEPARM cards are re-read each call (`CBFEE01.cbl:17-19`).

## 2. Field-level inputs / outputs

- Inputs: FEE-WORK-AREA request fields; FEEPARM control cards (FD 80-byte KEYWORD/VALUE records, `CBFEE01.cbl:43-57`) — **no rate, threshold or cap is coded in the procedure division**; amounts come from `CARDSVC.FEE_SCHEDULE`, everything else from cards (`CBFEE01.cbl:15-18`).
- Db2 reads: `CARDSVC.FEE_SCHEDULE`, `CARDSVC.TRANSACTION` (`CBFEE01.cbl:11`).
- Db2 writes: INSERT fee `CARDSVC.TRANSACTION` (`CBFEE01.cbl:653`), UPDATE `CARDSVC.ACCOUNT` (`CBFEE01.cbl:702`).
- Outputs: BR-RETURN-CD, BR-FEE-TOTAL, BR-FEE-COUNT and BR-EXTENSION fields.

## 3. Requirements owned

**CBFEE01-FR-001 — Annual fee.** Charged in the anniversary month of the account open date; waived in the first year; waived when the product waiver rule says so; never charged twice in twelve months (`CBFEE01.cbl:21-23`).

**CBFEE01-FR-002 — Late fee.** Charged when the minimum payment was not met by the due date; amount tiered on the balance; capped by the regulatory maximum on the card; suppressed when a late fee was already charged in the previous cycle (`CBFEE01.cbl:25-28`).

**CBFEE01-FR-003 — Fee posting.** An assessed fee is one TRANSACTION insert + one ACCOUNT update inside the caller's unit of work (caller commits/backs out — `CBFEE01.cbl:34`).

## 4. Target mechanism

`AnnualLateFeeHandler` bean registered for routes FEEC/ANNU and FEEC/LATE; FEEPARM card parameters move to the fee parameter table/config (plan wave 2); FEE_SCHEDULE stays the amount source; `BigDecimal` arithmetic per PIC scales. BR-EXTENSION becomes the handler's typed response record.

## 5. Error / edge behavior and RC mapping (`CBFEE01.cbl:30-35`)

| RC | Meaning | Target |
|---|---|---|
| 00 | fee assessed | handler OK, fee response populated |
| 04 | nothing to assess or fee waived | OK-no-action (caller counts a skip, RC 4 warning at job level) |
| 08 | no FEE_SCHEDULE row / not my fee type | declined-fee outcome (caller treats as declined, warning — see CBCRD05A_FR §5) |
| 12 | SQL failure — **caller must back out** | exception propagated; caller's transaction rolls back (U0505 path, `CBCRD05A.cbl:531-536`) |

Edge: waiver decisions must be visible in BR-EXTENSION (waiver flag) for the FEEAUDIT trail.

## 6. Hard-stop boundary

None — in-module leaf. Never calls anything (`CBFEE01.cbl:10`).

## 7. Acceptance criteria

- Anniversary-month account with first year elapsed and no annual fee in the last 12 months gets exactly one annual fee; first-year and waiver-rule accounts get none, reported as RC 04/waived.
- Missed-minimum account gets a late fee at the correct balance tier, never above the card's regulatory cap; an account late-fee-charged in the previous cycle is suppressed.
- Missing FEE_SCHEDULE row returns the declined outcome (08), assesses nothing, and writes nothing.
- SQL failure leaves no partial fee (transaction + account update atomic with the caller's chunk).
- Rates/thresholds are sourced from configuration/table only — none hardcoded (parity with `CBFEE01.cbl:15-18`).
