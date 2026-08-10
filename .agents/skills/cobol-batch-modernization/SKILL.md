---
name: cobol-batch-modernization
description: Drives an evidence-backed COBOL batch modernization session end to end when an engineer asks Devin to analyze, plan, migrate, and verify a z/OS batch estate.
---

# COBOL batch modernization

Use this skill for a live backend/batch modernization. Devin is the runtime; the presenter should
not substitute a terminal walkthrough for the live session.

## Ordered flow

1. Fetch the pinned estate exactly:

   ```sh
   make fetch
   ```

2. Decompose the **acceptance/matching stream** (`ACCP<CUR>` → `TRDPROC` → `TRDPB000`, the
   1,376-line program). The committed settlement stream is the shipped reference example.
3. Generate DCLGEN-shaped members from the shipped catalog DDL:

   ```sh
   make dclgen
   ```

   Treat them as generated layout evidence and retain the generated provenance marker.
4. Emit a stream inventory from the JCL chain, PROC, programs, datasets, and Db2 access.
5. Extract EARS FRs, ISO 25010 NFRs, and the data layer against the customer schemas. Cite actual
   source spans and generated layout members distinctly.
6. Run the deterministic feedback loop:

   ```sh
   make validate
   ```

   Gate failures are expected on the first pass. Fixing those failures on screen is the point:
   iterate until every gate passes.
7. Create a migration plan with foundation, like-to-like, new-design, and depend-on-new chunks,
   dependencies, waves, and sign-off.
8. Migrate one program to Java 17/Spring Batch idioms.
9. Run parity from the committed fixtures:

   ```sh
   make parity
   ```

10. Rebuild the evidence viewer:

   ```sh
   make report
   ```

11. Finale: Devin opens the generated UI in its own browser and click-tests every tab, the
    evidence drawer, the data-layer matrix, parity, and the authoring form live. This step is
    mandatory and fixed.
