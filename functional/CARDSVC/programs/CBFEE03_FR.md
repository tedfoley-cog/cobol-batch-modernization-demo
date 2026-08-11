# CBFEE03 — foreign transaction fee handler (dispatched leaf, wave 2)

Target: Spring component behind `ProgramRouter` in `backend/…/common/fees/` (plan §3 — app-tier, no SP).
Stream FRs: CARDNITE-FR-008.

## 1. Trigger / caller contract

Reached only dynamically: route FEEC/FRGN (`app/cardsvc/cbl/CBFEE03.cbl:4-5`; seed `db2/ddl/40_SEED_PGM_ROUTE.sql:70`). Parameters: FEE-WORK-AREA (CVFEEW1Y) + BATCH-RETURN-AREA (CVBRTN1Y) (`CBFEE03.cbl:165-169`); FEEPARM cards (`CBFEE03.cbl:49-58`); cancelled after every call.

## 2. Field-level inputs / outputs

- Reads `CARDSVC.FEE_SCHEDULE`, `CARDSVC.TRANSACTION`, `CARDSVC.MERCHANT` (`CBFEE03.cbl:9-10`, MERCHANT at `CBFEE03.cbl:402`), FEEPARM cards.
- Writes INSERT `CARDSVC.TRANSACTION` (`CBFEE03.cbl:589`), UPDATE `CARDSVC.ACCOUNT` (`CBFEE03.cbl:639`).
- Outputs: BR-RETURN-CD, BR-FEE-TOTAL, BR-FEE-COUNT.

## 3. Requirements owned

**CBFEE03-FR-001 — Currency conversion fee.** When the transaction currency differs from the account currency, the fee is a percentage of the **converted** (billing) amount after the FX rate — never of the original-currency amount (`CBFEE03.cbl:14-19`).

**CBFEE03-FR-002 — Cross-border fee.** When the acquirer country differs from the account country even though the currency matches (domestic-currency purchase at a foreign acquirer), the cardholder is assessed at the cross-border rate from the FEEPARM card (`CBFEE03.cbl:21-25`).

**CBFEE03-FR-003 — Rounding convention (Finance-agreed, 2004).** All fee arithmetic is carried at five decimal places and rounded HALF UP to two decimals **once**, at the point the fee amount is established; intermediate results are never rounded; the converted amount is not re-derived from the rounded fee. Do not change without a regression against the settlement proof (`CBFEE03.cbl:27-35`).

## 4. Target mechanism

`ForeignFeeHandler` bean for FEEC/FRGN. FR-003 maps to `BigDecimal` with scale 5 intermediates and a single `RoundingMode.HALF_UP` `setScale(2)` at fee establishment — a dedicated unit test locks the convention. Cross-border detection keeps the merchant/acquirer-country lookup.

## 5. Error / edge behavior and RC mapping (`CBFEE03.cbl:37-41`)

| RC | Meaning | Target |
|---|---|---|
| 00 | fee assessed | OK |
| 04 | not a foreign transaction | OK-no-action |
| 08 | no schedule row, not my fee type, or unusable basis | declined-fee outcome |
| 12 | SQL failure — caller must back out | exception; caller rolls back |

## 6. Hard-stop boundary

None — in-module leaf.

## 7. Acceptance criteria

- Currency-mismatch transaction is assessed on the converted billing amount; a same-currency/foreign-acquirer transaction is assessed at the cross-border rate; a fully domestic transaction returns 04 untouched.
- A rounding regression test proves 5-decimal intermediates and a single HALF-UP round to 2 decimals (golden cases at tie values, e.g. .005).
- Missing schedule row / unusable basis returns 08 and assesses nothing; SQL failure leaves no partial fee.
