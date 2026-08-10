# Implementation Plan — COBOL Batch Modernization Demo

Comprehension-first modernization of a **z/OS batch** estate: schema-constrained requirements
extraction, migration planning, and machine-checkable input/output verification. No front-end
workloads are in scope — the estate is batch only (JCL → PROC → COBOL → Db2).

## 1. What the demo proves

A migration is only trustworthy if the *understanding* that precedes it is auditable. This demo
proves that Devin can take a real, undocumented z/OS batch estate and produce **artifacts an
engineer and a business reviewer can sign off on**: a stream inventory derived from the JCL chain,
functional requirements written in a constrained syntax with line-level code evidence,
non-functional requirements derived from the operational envelope encoded in the JCL/Db2 layer,
a data-layer model (CRUD matrix, field map, decimal and restart semantics), a migration plan
sequenced into cutover waves, and a parity report that verifies migrated output against the legacy
behaviour under explicit normalization rules. Every artifact conforms to a **JSON Schema the
customer owns and edits** — change the schema, and every downstream artifact changes shape with it.

## 2. What Devin does live

Devin decomposes the trade-acceptance batch stream, extracts schema-conforming functional and
non-functional requirements with line-level evidence for every claim, generates the migration plan
and the parity harness inputs, fails and fixes its own artifacts against the validator gates, then
click-tests the resulting review UI.

## 3. The estate (source selection)

**Upstream:** [`cloudframe-samples/cloudframe-mainframe-modernization-tradingapp`](https://github.com/cloudframe-samples/cloudframe-mainframe-modernization-tradingapp)
— Apache-2.0, pinned at commit `d1450ca062445a6e70f2a2638af6ba286ca8fecc`.

Why this estate:

| Criterion | Evidence in the estate |
|---|---|
| Batch only, no front-end | 22 JCL jobs + 1 PROC (`TRDPROC`), invoked via `IKJEFT01` with a Db2 plan; no BMS maps, no CICS screens |
| Real z/OS shape | Sequence-numbered source (cols 73–80 with change levels), `EXEC SQL INCLUDE` DCLGENs, `//STEPLIB DD DSN=DSNC10.…SDSNLOAD`, control cards in a `CARD` PDS, DCB dataset attributes, and an empty `VSAMDEF` |
| Non-trivial logic chain | Order acceptance/validation/matching (`TRDPB000`, 1 376 lines) → settlement (`TRDPB001`) → securities book-keeping (`TRDPB002`) → money book-keeping (`TRDPB003`) → summary/statistics (`TRDPB004`, `TRDPB006`) → exception handling (`TRDPBEXC`); 9 order-status states with 8 distinct overdue reason codes |
| Tricky data layer | 10 Db2 tables with DDL + LOAD control cards, DCLGEN copybooks, `DECLARE … CURSOR WITH HOLD`, checkpoint frequency passed as a run parameter (`PARM('USD 0100')`), commit/restart logic, `USAGE POINTER` task lists, `COMP`/`COMP-3` money fields across 10 currencies |
| Verifiable off-mainframe | Upstream ships `db2/portable-db2-h2/Trade-db.mv.db` plus `SMALL` load datasets, so target-side runs and comparisons are executable without z/OS |
| Legally reusable | Apache-2.0; we **fetch** rather than vendor it (`tools/fetch_estate.py`), so this repo carries no third-party source |

Rejected alternatives: `aws-samples/aws-mainframe-modernization-carddemo` (excluded by request —
shallow logic chain for this audience), `IBM/Bank-of-Z` and `cicsdev/cics-banking-sample-application-cbsa`
(CICS/IMS online-first; batch is a single statement job), `navikt/DSF` (PL/I, not COBOL),
ACAS (real 100k-line GnuCOBOL accounting estate but no JCL/Db2/VSAM, so it does not look like z/OS),
`eliellmiranda/EMUNAH-BANK-LAB` (good batch day-cycle shape, but unlicensed, heavily pre-commented,
non-English identifiers).

## 4. Framework mapping (sources)

Cognition framework, *Next Generation COBOL Mainframe Modernization with Devin* (Aug 2026):
decomposition to the minimum testable block (one program), machine-checkable feedback loops at
multiple layers, and logical 1/1 parity; reverse engineering (documentation) then forward
engineering; migration steps 1–4 (module/stream analysis → functional requirements & migration
plan → phased migration → testing & deployment); three-way output comparison (semantic expectation,
COBOL execution, migrated execution) with normalization because COMP-3 and fixed-width fields do
not map 1:1 onto Java types.

Sequencing model (from the customer's incumbent SI approach): four migration categories —
Foundation, Like-to-Like, New Design, Depend-on-New — and a five-step sequencing process
(inventory & decompose → classify → sequence within category → align across categories with
cutover waves → validate & baseline). These are modelled as first-class enums in the migration-plan
schema so the plan Devin emits is directly comparable with the incumbent's baseline.

Requirements-engineering standards and research anchoring the schema design:

| Source | What we take from it |
|---|---|
| EARS — Easy Approach to Requirements Syntax (Mavin et al., RE'09; <https://alistairmavin.com/ears/>) | Constrained requirement syntax: `ubiquitous`, `state_driven` (While), `event_driven` (When), `optional_feature` (Where), `unwanted_behaviour` (If/Then), `complex`; clause decomposition into preconditions / trigger / system name / system response |
| ISO/IEC/IEEE 29148:2018 §5.2.5–5.2.6 | Nine characteristics of an individual requirement (necessary, appropriate, unambiguous, complete, singular, feasible, verifiable, correct, conforming) and set-level characteristics; verification methods (inspection, analysis, demonstration, test) |
| ISO/IEC 25010:2023 | NFR taxonomy: the nine product-quality characteristics (functional suitability, performance efficiency, compatibility, interaction capability, reliability, security, maintainability, flexibility, safety) with subcharacteristics |
| *Leveraging Generative AI for Extracting Business Requirements from Legacy COBOL and PL/I Code* (ACL 2026 Industry Track) | Schema-constrained LLM generation over a deterministically parsed IR, with **bidirectional traceability to code**; artifact set of rule catalogs, data lineage, CRUD matrices and field-level source-to-target mappings; reported 93% agreement with expert-authored rules on a 3.4M-line estate |
| COBREX (rule-based CFG extraction) and COBRAIN (LLM extraction), EASE 2025 | Determinism anchors facts, the model supplies semantics — mirrors the framework's "combining determinism with AI"; motivates confidence + provenance fields rather than unqualified prose |

## 5. Repo layout

```
README.md                              flowchart-first landing page
DEMO_NOTES.md                          presenter cheat sheet (5–8 bullets)
Makefile                               fetch / install / validate / report / test / serve
pyproject.toml                         ruff + pytest config, deps pinned
docs/IMPLEMENTATION_PLAN.md            this file
docs/FRAMEWORK_MAPPING.md              framework step → artifact → schema → gate, + customer-ask map
docs/flowchart.html                    standalone shareable demo-flow diagram
docs/flowchart.png                     full-page render of the same diagram
schemas/stream-inventory.schema.json   Step 1: JCL chain → jobs → steps → programs → datasets/Db2
schemas/requirements.schema.json       Step 2: FR (EARS) + NFR (ISO 25010) + data layer + traceability
schemas/migration-plan.schema.json     Step 2: chunks, 4 categories, waves, target design mapping
schemas/parity-report.schema.json      Steps 3–4: three-way comparison + normalization + verdicts
schemas/profiles/oem-batch.profile.json  customer-owned profile: extra required fields, id conventions
schemas/README.md                      how to edit the schema/profile and what re-validates
artifacts/reference-example/*.json     hand-verified gold artifacts for the settlement stream
artifacts/generated/.gitkeep           where the live session writes its output
tools/fetch_estate.py                  pinned clone of the upstream estate into legacy/
tools/validate.py                      schema + evidence + traceability + lint gates (CI gate)
tools/normalize.py                     COMP-3 / zoned / fixed-width / codepage / scale normalization
tools/parity.py                        three-way comparison → parity-report.json
tools/report.py                        renders the self-contained interactive review UI
tools/assets/{app.js,styles.css}        UI behaviour and styling (inlined at build time)
tests/test_*.py                        unit tests for validator, normalizer, parity, report
tests/ui/UI_TEST_PLAN.md               scenario → what it proves in the demo story
tests/ui/test_*.py                     Playwright journeys (≥5)
.agents/skills/cobol-batch-modernization/SKILL.md  drives the live session, UI click-test finale
.github/workflows/ci.yml               python gate + Playwright UI gate on every push
```

Deviation from the 10–25 file default: ~35 files. The request was for maximum depth on comprehension
and artifact quality; the extra files are schemas, validator gates and the UI test suite.

## 6. Flowchart outline

Nodes: `Trigger Prompt` → `Fetch Pinned Estate` → `Parse JCL Chain` → `Stream Inventory` →
`Extract Requirements` → `Data Layer Model` → `Validate Artifacts` (loop back to
`Extract Requirements` on gate failure) → `Migration Plan` → `SME Review Gate` →
`Migrate One Program` → `Parity Harness` → `Build Review UI` → `Devin Click-Tests UI`.
Subgraphs: *Comprehension* (parse → inventory → requirements → data layer → validate),
*Forward Engineering* (plan → migrate → parity). Legend: artifact files, schema-gated steps,
human sign-off point.

## 7. Runtime plan

Fully runnable on Linux, no mainframe and no emulator required:

```
make fetch        # pinned clone of the upstream estate into legacy/ (gitignored)
make install      # uv/pip install; playwright install chromium
make validate     # schemas self-validate; artifacts validate; evidence resolves against legacy/
make report       # writes site/index.html (self-contained)
make test         # pytest unit tests
make test-ui      # Playwright suite against the built report
make serve        # local static server for the report
```

`make validate` is the demo's feedback loop: it fails when a requirement cites a line range that
does not exist, when a cited snippet's hash does not match the estate, when an EARS pattern does not
match the statement's keywords, when an NFR names a characteristic outside ISO 25010, when a plan
chunk has no requirement behind it, or when coverage/confidence falls below the profile's gates.

## 8. Visual artifact plan

Primary visual: the flowchart (shareable HTML + PNG + native Mermaid in the README). Second:
the **self-contained interactive review UI** generated by `tools/report.py` from the artifact JSON —
this is the use case's native output (the artifacts themselves), rendered for review rather than a
dashboard. It passes the Dashboard Decision Gate on none of the three criteria as a *live* dashboard,
which is exactly why it ships as a generated static file: openable by anyone, attachable, and
diff-reviewable. Third: the parity report table (pass/fail per field with normalization applied).

## 9. UI test gauntlet plan

Surface: the generated review UI (tabs: Overview, Streams, Requirements, Data Layer, Plan, Parity,
Schema). Scenarios committed under `tests/ui/`:

1. Cross-tab navigation asserting real domain content at each stop (stream `SETLUSD`, program
   `TRDPB001`, table `TBTRDSTQ`).
2. Requirement authoring form: invalid submission asserts schema-derived validation errors
   (bad id pattern, EARS pattern/statement mismatch, missing evidence); valid submission asserts the
   new row and the updated coverage KPI.
3. Filter/sort/search combination over requirements (ISO characteristic + SME status + free-text)
   asserting the exact resulting id set and sort order.
4. Evidence drawer drill-down: open a requirement, assert the COBOL snippet, line range and
   paragraph match the estate, follow its business-rule table, close, assert side-effects.
5. Golden path: Streams → click a JCL step → drawer shows Db2 access → jump to the Data Layer CRUD
   matrix → filter to that table → open the requirement that writes it → Parity tab shows its verdict.

Triggering: a CI job on every push (build report → serve → run headless), and the fixed final step
of the live session skill, where Devin click-tests the UI in its own browser and records it.

## 10. CI plan

One workflow, two jobs. `python`: ruff, pytest, `tools/validate.py` against the fetched estate
(schema self-validation, evidence resolution, traceability, lint gates). `ui`: build the report,
install Chromium, run the full `tests/ui/` suite headlessly. Both run on every push and PR.

## 11. Risks and open items

- **Estate availability at demo time.** Mitigated by a pinned commit, an inventory check after
  fetch, and caching the clone in the Devin blueprint so the live session starts warm.
- **DCLGEN provenance.** `tools/dclgen.py` regenerates DCLGEN-shaped members from the shipped
  catalog DDL because the public estate's DCLGEN PDS has no members. Confirm generated members
  against the customer's DCLGEN PDS and production scale metadata before migration.
- **Legacy-side execution.** The upstream programs contain embedded Db2 SQL and cannot be compiled
  with GnuCOBOL as-is, so the "legacy execution" leg of the three-way comparison is fed by captured
  run output rather than a live z/OS run. The harness treats it as a supplied input and states its
  provenance in every report; the other two legs (semantic expectation, migrated execution against
  the shipped H2 database) execute locally.
- **Reference-example scope.** The committed gold artifacts cover the settlement stream only; the
  live session generates the acceptance/matching stream. If the audience wants breadth over depth,
  the same skill fans out across the remaining currencies in parallel sessions.
- **Target stack.** The plan's target design assumes Java 17 / Spring Batch idioms, matching the
  incumbent's target. If the customer's target is different, the mapping table in the migration-plan
  schema is the only thing that changes.
