# CBCRD06C — apply risk outcomes (CBCRD06J STEP040, wave 5)

Target: step of Spring Batch job `cbcrd06`.
Stream FRs owned: CARDNITE-FR-015 (accepted-risk application).

## 1. Trigger / caller contract

Gated `IF STEP030.RC <= 8` (`app/jcl/cardsvc/CBCRD06J.jcl:132-143`) — runs even when parties went to manual review, applying only the accepted ones. STEP050 prints the review file when STEP030.RC = 8 (`CBCRD06J.jcl:157`). PARM: cycle date.

## 2. Field-level inputs / outputs

- In: `PARTYACC(0)` FB 150 CVPWRK01Y; CYCLCTL (`CBCRD06J.jcl:146-149`; SELECTs `app/cardsvc/cbl/CBCRD06C.cbl:47-55`).
- Out: band report BANDRPT SYSOUT FBA 133.
- Db2: `ACCOUNT`+`CARD`+`CARD_LIMIT` join cursor (`CBCRD06C.cbl:222-224`); positioned UPDATE `CARDSVC.CARD_LIMIT` risk band / exposure (`CBCRD06C.cbl:442-452`).

## 3. Requirements owned

**CBCRD06C-FR-001 — Accepted outcomes only.** Applies only records whose `PW-RC` is accepted (0/4); review/fatal records are never applied (`CBCRD06C.cbl` per analysis §3; CARDNITE-FR-015).

**CBCRD06C-FR-002 — Limit update.** For each accepted party, `CARD_LIMIT.risk band` and exposure are updated from the returned outcome; band changes are reported on BANDRPT.

## 4. Target mechanism

Chunk step: `FlatFileItemReader` PARTYACC → JPA positioned-update equivalent on `card_limit`; commit every `WS-COMMIT-FREQUENCY` (`CBCRD06C.cbl:364`). BANDRPT layout-preserving (D6).

## 5. Error / edge behavior and RC mapping

- RC 0 applied / 4 warnings; abends U0602 file, U0608 SQL (`CBCRD06C.cbl:344,429-466`).
- A PARTYACC record with PW-RC 8/12 (should not occur) is a data error — rejected with a warning, not silently applied.
- Empty PARTYACC (all parties reviewed) is a valid RC 0 no-op.

## 6. Hard-stop boundary

None — consumes outcomes; never calls the risk seam.

## 7. Acceptance criteria

- Accepted records update band/exposure exactly once; reviewed parties' limits are untouched.
- BANDRPT lists each band change with before/after.
- Re-run after mid-run failure does not double-apply (checkpoint parity).
