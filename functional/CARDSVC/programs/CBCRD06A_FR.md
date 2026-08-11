# CBCRD06A — party work-list build (CBCRD06J STEP010, wave 5)

Target: step of Spring Batch job `cbcrd06`.
Stream FRs owned: CARDNITE-FR-011 (party work-list construction).

## 1. Trigger / caller contract

Runs after the join guard, gated `IF STEP005.RC = 0` (`app/jcl/cardsvc/CBCRD06J.jcl:61-66`). PARM: cycle date only (`CBCRD06J.jcl:66`).

## 2. Field-level inputs / outputs

- Db2 in: cursor over `CARDSVC.ACCOUNT` with a `CARDSVC.TRANSACTION` existence subquery — only parties with cycle activity are listed (`app/cardsvc/cbl/CBCRD06A.cbl:150-155`); `CARD_LIMIT`+`CARD` join for exposure fields (`CBCRD06A.cbl:328-329`).
- File out: `CARD.PROD.PARTYWK(+1)` FB **LRECL 150** (`CBCRD06J.jcl:70-74`), layout PARTY-WORK-REC (CVPWRK01Y): key PW-PARTY-ID(11)+PW-ACCT-ID(11), COMP-3 balances, PW-OUTCOME block left for CBCRD06B (`app/cardsvc/cpy/CVPWRK01Y.cpy:7-34`).
- CYCLCTL read.

## 3. Requirements owned

**CBCRD06A-FR-001 — Complete work list.** Every party owning an account with transaction activity in the cycle appears on the work list with current balances/exposure fields populated; parties without activity are excluded (CARDNITE-FR-011).

## 4. Target mechanism

Chunk step: repository cursor (account + transaction-existence predicate) → `FlatFileItemWriter` PARTYWK per D5 (staging table alternative only if Phase 4 E2E shows file handoffs hurt — plan D5).

## 5. Error / edge behavior and RC mapping

- RC 0 normal / RC 4 warning (empty work list, `CBCRD06A.cbl:27,367-370`); abends U0601 CYCLCTL, U0602 file, U0603 SQL (`app/cardsvc/cbl/CBCRD06A.cbl:29,197-263`).
- Empty work list (no activity) ends RC 4 (`WORK LIST IS EMPTY` warning) — downstream steps still run and process zero records (STEP020 is gated `RC <= 4`).
- A party appearing under multiple accounts produces multiple records here; per-party summarisation is CBCRD06X's job, not this one.

## 6. Hard-stop boundary

None.

## 7. Acceptance criteria

- Given seeded accounts with/without cycle transactions, the work list contains exactly the active ones, with COMP-3-scale-faithful `BigDecimal` balances.
- Empty work list exits 4 with the warning message.
- Outcome block is written empty/initial.
- Record length/layout is byte-compatible CVPWRK01Y FB 150.
