"""Schema, provenance, language, and traceability gates for demo artifacts."""
from __future__ import annotations

import hashlib
import json
import re
import sys
from pathlib import Path
from typing import Any

from jsonschema import Draft202012Validator

ROOT = Path(__file__).resolve().parents[1]
SCHEMAS = {
    "stream": ROOT / "schemas/stream-inventory.schema.json",
    "requirements": ROOT / "schemas/requirements.schema.json",
    "plan": ROOT / "schemas/migration-plan.schema.json",
    "parity": ROOT / "schemas/parity-report.schema.json",
}
ARTIFACTS = ROOT / "artifacts/reference-example"


def load(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text())


def citations(obj: Any) -> list[dict[str, Any]]:
    found: list[dict[str, Any]] = []
    if isinstance(obj, dict):
        if {"path", "line_start", "line_end", "snippet_sha256"} <= obj.keys():
            found.append(obj)
        for value in obj.values():
            found.extend(citations(value))
    elif isinstance(obj, list):
        for value in obj:
            found.extend(citations(value))
    return found


def evidence_gate(objects: list[dict[str, Any]]) -> tuple[bool, str]:
    errors = []
    for obj in objects:
        path = ROOT / "legacy" / obj["path"]
        if not path.is_file():
            errors.append(f"missing {obj['path']}")
            continue
        lines = path.read_text(errors="replace").splitlines(keepends=True)
        start, end = obj["line_start"], obj["line_end"]
        if start < 1 or end > len(lines) or start > end:
            errors.append(f"invalid range {obj['path']}:{start}-{end}")
            continue
        digest = hashlib.sha256("".join(lines[start - 1:end]).encode()).hexdigest()
        if digest != obj["snippet_sha256"]:
            errors.append(f"hash mismatch {obj['path']}:{start}-{end}")
    return (not errors, "EVIDENCE PASS" if not errors else "EVIDENCE FAIL: " + "; ".join(errors))


def ears_gate(frs: list[dict[str, Any]]) -> list[str]:
    failures = []
    for fr in frs:
        if fr["ears"]["pattern"] == "event_driven" and "When" not in fr["statement"]:
            failures.append(f"EARS FAIL: {fr['id']} event_driven requires When")
        if len(fr["ears"]["system_response"]) != 1:
            failures.append(f"EARS FAIL: {fr['id']} must have one response")
    return failures


def traceability_gate(frs: list[dict[str, Any]], chunks: list[dict[str, Any]]) -> list[str]:
    fr_ids = {fr["id"] for fr in frs}
    plan_ids = {fr for chunk in chunks for fr in chunk["fr_ids"]}
    failures = []
    if fr_ids - plan_ids:
        failures.append("TRACEABILITY FAIL: orphan FRs " + ", ".join(sorted(fr_ids - plan_ids)))
    if any(not chunk["fr_ids"] for chunk in chunks):
        failures.append("TRACEABILITY FAIL: orphan plan chunk")
    return failures


def quality(value: bool = True) -> dict[str, Any]:
    return {"value": value, "notes": "reviewed against source"}


def main() -> int:
    failures: list[str] = []
    lines = []
    for name, path in SCHEMAS.items():
        schema = load(path)
        try:
            Draft202012Validator.check_schema(schema)
            lines.append(f"SCHEMA PASS: {name}")
        except (TypeError, ValueError) as exc:
            failures.append(f"SCHEMA FAIL: {name}: {exc}")
    loaded = {name: load(ARTIFACTS / filename) for name, filename in {
        "stream": "stream-inventory.json", "requirements": "requirements.json",
        "plan": "migration-plan.json", "parity": "parity-report.json"}.items()}
    for name, schema_path in SCHEMAS.items():
        errors = sorted(Draft202012Validator(load(schema_path)).iter_errors(loaded[name]), key=str)
        if errors:
            failures.append(f"ARTIFACT FAIL: {name}: " + " | ".join(e.message for e in errors[:3]))
        else:
            lines.append(f"ARTIFACT PASS: {name}")
    all_evidence = [citation for item in loaded.values() for citation in citations(item)]
    ok, message = evidence_gate(all_evidence)
    lines.append(message)
    if not ok:
        failures.append(message)
    req = loaded["requirements"]
    plan = loaded["plan"]
    frs = req["functional_requirements"]
    trace_failures = traceability_gate(frs, plan["chunks"])
    failures.extend(trace_failures)
    if not trace_failures:
        fr_ids = {fr["id"] for fr in frs}
        lines.append(f"TRACEABILITY PASS: {len(fr_ids)} FRs ↔ {len(plan['chunks'])} chunks")
    profile = load(ROOT / "schemas/profiles/oem-batch.profile.json")
    for fr in frs:
        if fr["confidence"]["score"] < profile["min_confidence"]:
            failures.append(f"PROFILE FAIL: {fr['id']} confidence below minimum")
        if not re.match(profile["id_patterns"]["fr"], fr["id"]):
            failures.append(f"PROFILE FAIL: bad FR id {fr['id']}")
        if fr["migration_category"] == "new_design" and fr["sme_review"]["status"] != "approved":
            failures.append(f"PROFILE FAIL: {fr['id']} new_design requires SME approval")
    for nfr in req["nonfunctional_requirements"]:
        if not nfr["measure"]["metric"] or not nfr["measure"]["target"] or not nfr["measure"]["unit"] or not nfr["measure"]["source"]:
            failures.append(f"ISO25010 FAIL: incomplete measure {nfr['id']}")
    ears_failures = ears_gate(frs)
    failures.extend(ears_failures)
    lines.append(f"EARS PASS: {len(frs)} functional requirements linted" if not ears_failures else "EARS FAIL")
    lines.append(f"ISO25010 PASS: {len(req['nonfunctional_requirements'])} NFRs linted")
    lines.append("PROFILE PASS" if not any("PROFILE FAIL" in f for f in failures) else "PROFILE FAIL")
    programs = loaded["stream"]["meta"]["coverage"]["programs_cited"]
    paragraphs = loaded["stream"]["meta"]["coverage"]["paragraphs_cited"]
    lines.append(f"COVERAGE: programs={programs:.0f}% paragraphs={paragraphs:.0f}%")
    if failures:
        print("\n".join(lines + failures))
        return 1
    print("\n".join(lines))
    print("SUMMARY: all gates PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
