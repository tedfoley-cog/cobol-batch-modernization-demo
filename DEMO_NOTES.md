# Demo notes

## Setup

* Start a Devin session with the acceptance/matching stream prompt and confirm the pinned estate is available.
* Display live views through Devin's browser/Desktop tab; the shipped settlement stream is the reference baseline.

## Demo flow

* Devin fetches the pinned estate and decomposes `ACCP<CUR>` → `TRDPROC` → `TRDPB000`.
* Devin emits schema-constrained inventory, FR/NFR, data mappings, and a migration plan, then fixes expected first-pass gate failures on screen.
* Devin migrates one program, runs the three-leg parity harness, and explains the missing-DCLGEN gap rather than inventing a copybook.
* Devin rebuilds the review UI and performs the live browser click-test finale.
