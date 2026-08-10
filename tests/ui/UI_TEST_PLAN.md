# UI test plan

The journeys run against the generated, self-contained `site/index.html` and prove real domain
content rather than merely checking that controls exist.

1. Cross-tab navigation visits every tab and asserts `SETLUSD`, `TRDPB001`, and `TBTRDSTQ`.
2. Requirement authoring submits an invalid ID/EARS statement and asserts schema-derived errors,
   then submits a valid requirement and asserts the new row and KPI change.
3. Requirement filtering combines free text, EARS pattern, migration category, and sort order.
4. Evidence drawer opens a requirement, asserts the COBOL line range/snippet and paragraph, then
   closes without losing the table state.
5. Golden path opens a JCL step, follows the data-layer matrix to `TBTRDSTQ`, and verifies parity
   status `601` plus the packed-amount normalized row.
