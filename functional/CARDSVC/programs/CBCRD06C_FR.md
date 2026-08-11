# CBCRD06C — apply risk outcomes (CBCRD06J STEP040, wave 5)

Target: step of Spring Batch job `cbcrd06`.
Stream FRs owned: CARDNITE-FR-015 (accepted-risk application).

## 1. Trigger / caller contract

Gated `IF STEP030.RC <= 8` (`app/jcl/cardsvc/CBCRD06J.jcl:132-143`) — runs even when parties went to manual review, applying only the accepted ones. STEP050 prints the review file when STEP030.RC = 8 (`CBCRD06J.jcl:157`) — note it re-references `PARTYRVW(+1)` rather than `(0)` (`CBCRD06J.jcl:122,159`); within one job both resolve to the generation STEP030 created, so it works on z/OS, but the migrated print step must read the file the dispatch step actually produced, not mirror the relative-generation pattern. PARM: cycle date.

## 2. Field-level inputs / outputs

- In: `PARTYACC(0)` FB 150 CVPWRK01Y; CYCLCTL (`CBCRD06J.jcl:146-149`; SELECTs `app/cardsvc/cbl/CBCRD06C.cbl:47-55`).
- Out: band report BANDRPT SYSOUT FBA 133.
- Db2: `ACCOUNT`+`CARD`+`CARD_LIMIT` join cursor (`CBCRD06C.cbl:222-224`); positioned UPDATE `CARDSVC.CARD_LIMIT` setting `RISK_BAND`, `LAST_REVIEW_DATE` and `AVAIL_AMT` (`CBCRD06C.cbl:433-459`).

## 3. Requirements owned

**CBCRD06C-FR-001 — Accepted outcomes only.** Only accepted outcomes are ever applied — an **upstream invariant owned by CBCRD06B**: reviewed/fatal parties are never written to PARTYACC, and CBCRD06C itself has no `PW-RC` test — it applies every record it reads (`CBCRD06C.cbl:9-10,321-369`; CARDNITE-FR-015).

**CBCRD06C-FR-002 — Limit update with band-X suppression.** For each accepted party, the positioned update always sets `RISK_BAND` and `LAST_REVIEW_DATE`; `AVAIL_AMT` is recomputed as `LIMIT_AMT - USED_AMT` **except** when the new band is 'X', where `AVAIL_AMT` is forced to zero (suppression counted) so the online authorisation path stops approving — `LIMIT_AMT` itself is deliberately untouched (`CBCRD06C.cbl:433-459`, CRD5810 header note). Band changes are reported on BANDRPT.

## 4. Target mechanism

Chunk step: `FlatFileItemReader` PARTYACC → JPA positioned-update equivalent on `card_limit`; commit every `WS-COMMIT-FREQUENCY` (`CBCRD06C.cbl:364`). BANDRPT layout-preserving (D6).

## 5. Error / edge behavior and RC mapping

- RC 0 applied / 4 warnings; abends U0602 file, U0608 SQL (`CBCRD06C.cbl:344,429-466`).
- A PARTYACC record with PW-RC 8/12 (should not occur — upstream invariant, FR-001) is a data error — rejected with a warning, not silently applied. **Target-only defensive addition:** legacy has no such check and would apply the record.
- Empty PARTYACC (all parties reviewed) is a valid RC 0 no-op.

## 6. Hard-stop boundary

None — consumes outcomes; never calls the risk seam.

## 7. Acceptance criteria

- Accepted records update `RISK_BAND`/`LAST_REVIEW_DATE`/`AVAIL_AMT` exactly once; reviewed parties' limits are untouched.
- A party moved to band 'X' gets `AVAIL_AMT = 0` with `LIMIT_AMT` unchanged and counts as a suppression; any other band recomputes `AVAIL_AMT = LIMIT_AMT - USED_AMT`.
- BANDRPT lists each band change with before/after.
- Re-run after mid-run failure does not double-apply (checkpoint parity).
