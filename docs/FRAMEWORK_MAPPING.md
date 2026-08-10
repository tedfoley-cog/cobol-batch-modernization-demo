# Framework mapping

| Framework step | Artifact | Schema | Gate |
|---|---|---|---|
| Module/stream analysis | `stream-inventory.json`, generated DCLGEN members | stream inventory, requirements | schema, evidence provenance, paragraph containment |
| Functional requirements and migration planning | `requirements.json`, `migration-plan.json` | requirements, migration plan | EARS, ISO 25010, profile, dangling references |
| Phased migration | migration chunks and target design mappings | migration plan | category, dependency, sign-off, FR traceability |
| Testing and deployment | `parity-report.json`, generated review UI | parity report | three-leg normalization, parity verdict, UI journeys |
| Inventory and decompose | stream programs, jobs, steps | stream inventory | computed program/paragraph coverage |
| Classify | chunk category | migration plan | profile category enum |
| Sequence within category | `sequence_within_category` | migration plan | chunk ID and sequence |
| Align across categories | dependencies and cutover waves | migration plan | dangling references |
| Validate and baseline | all artifacts | all schemas | evidence hashes and summary table |

## Discovery ask map

| Discovery ask | Where it lives |
|---|---|
| Code comprehension | `tools/fetch_estate.py`, stream inventory, evidence drawer |
| Source-to-target mapping | requirements data layer, generated DCLGEN field map, and migration plan target design |
| FR/NFR documentation | `artifacts/reference-example/requirements.json` |
| Migration planning | `artifacts/reference-example/migration-plan.json` |
| Code migration | live Devin skill, acceptance stream sequence |
| Input/output validation | `tools/normalize.py`, `tools/parity.py`, fixtures |
| Result verification | parity report, validator, Playwright journeys |
| Backend/batch-only scope | README, fetched estate, generated evidence viewer |
