# Customer-editable schemas

The four draft 2020-12 schemas describe the comprehension and parity contract. `$defs` keep
evidence, verification, confidence, and review objects consistent across artifacts. Edit a schema
or the `profiles/oem-batch.profile.json` policy, then run `make validate`; the validator first
checks every schema against the JSON Schema 2020-12 metaschema and then validates the reference
artifacts plus profile-specific gates.

Evidence paths are relative to `legacy/`, and hashes are SHA-256 over the exact newline-preserving
physical line span. This is intentionally stricter than prose citations: changing the fetched
estate or a claimed citation makes validation fail.
