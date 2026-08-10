"""Build a self-contained evidence-review HTML artifact."""
from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from tools.validate import citations, compute_coverage

ROOT = Path(__file__).resolve().parents[1]
ARTIFACTS = ROOT / "artifacts/reference-example"

def load_json(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text())

def evidence_snippets(all_evidence: list[dict[str, Any]]) -> dict[str, list[dict[str, Any]]]:
    snippets: dict[str, list[dict[str, Any]]] = {}
    for evidence in all_evidence:
        source = ROOT / evidence["path"] if evidence["path"].startswith("artifacts/generated/") else ROOT / "legacy" / evidence["path"]
        if not source.is_file():
            continue
        lines = source.read_text(errors="replace").splitlines()
        start = max(1, evidence["line_start"] - 3)
        end = min(len(lines), evidence["line_end"] + 3)
        key = f"{evidence['path']}:{evidence['line_start']}-{evidence['line_end']}"
        snippets[key] = [
            {"number": number, "text": lines[number - 1],
             "cited": evidence["line_start"] <= number <= evidence["line_end"]}
            for number in range(start, end + 1)
        ]
    return snippets


def main() -> None:
    artifacts = {name: load_json(ARTIFACTS / filename) for name, filename in {
        "stream": "stream-inventory.json", "requirements": "requirements.json",
        "plan": "migration-plan.json", "parity": "parity-report.json"}.items()}
    schemas = {path.name.removesuffix(".schema.json"): load_json(path) for path in (ROOT / "schemas").glob("*.schema.json")}
    profile = load_json(ROOT / "schemas/profiles/oem-batch.profile.json")
    all_evidence = [item for artifact in artifacts.values() for item in citations(artifact)]
    css = (ROOT / "tools/assets/styles.css").read_text()
    js = (ROOT / "tools/assets/app.js").read_text()
    data = json.dumps({**artifacts, "schemas": schemas, "profile": profile,
                       "computed_coverage": compute_coverage(artifacts["stream"], all_evidence),
                       "source_snippets": evidence_snippets(all_evidence)})
    html = f"""<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>COBOL Batch Evidence Review</title><style>{css}</style></head><body><header><h1>COBOL Batch Modernization Evidence Review</h1><div class="subtitle">Settlement stream · schema-constrained, evidence-backed backend artifacts</div></header><nav>{''.join(f'<button class="tab" data-tab="{tab}">{tab}</button>' for tab in ('Overview','Streams','Requirements','Data Layer','Plan','Parity','Schema'))}</nav><main id="app"></main><script>window.REVIEW_DATA={data};</script><script>{js}</script></body></html>"""
    output = ROOT / "site/index.html"
    output.parent.mkdir(exist_ok=True)
    output.write_text(html)
    print(f"REPORT PASS: wrote {output} ({len(html)} bytes)")


if __name__ == "__main__":
    main()
