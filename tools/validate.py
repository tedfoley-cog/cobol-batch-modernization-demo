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
HEADER_RE = re.compile(r"^\d{6}\s+(\d{4}-[A-Z0-9-]+)\.")
ISO25010 = {
    "functional_suitability": {"functional_completeness", "functional_correctness", "functional_appropriateness"},
    "performance_efficiency": {"time_behavior", "resource_utilization", "capacity"},
    "compatibility": {"co_existence", "interoperability"},
    "interaction_capability": {
        "appropriateness_recognizability", "learnability", "operability",
        "user_assistance", "self_descriptiveness", "inclusivity", "user_engagement",
    },
    "reliability": {"maturity", "availability", "fault_tolerance", "recoverability", "faultlessness"},
    "security": {"confidentiality", "integrity", "non_repudiation", "accountability", "authenticity", "resistance"},
    "maintainability": {"modularity", "reusability", "analysability", "modifiability", "testability"},
    "flexibility": {"adaptability", "scalability", "installability", "replaceability"},
    "safety": {"operational_constraint", "risk_identification", "fail_safe", "hazard_warning", "safe_integration"},
}


def load(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text())


def citations(obj: Any) -> list[dict[str, Any]]:
    if isinstance(obj, dict):
        own = [obj] if {"path", "line_start", "line_end", "snippet_sha256"} <= obj.keys() else []
        return own + [item for value in obj.values() for item in citations(value)]
    if isinstance(obj, list):
        return [item for value in obj for item in citations(value)]
    return []


def paragraph_headers(path: Path) -> list[tuple[int, str]]:
    return [(i, match.group(1)) for i, line in enumerate(path.read_text(errors="replace").splitlines(), 1)
            if (match := HEADER_RE.match(line))]


def evidence_gate(objects: list[dict[str, Any]]) -> tuple[bool, str]:
    errors = []
    for obj in objects:
        if obj["path"].startswith("artifacts/generated/"):
            if obj.get("provenance") != "generated":
                errors.append(f"generated citation missing provenance {obj['path']}")
                continue
            path = ROOT / obj["path"]
        else:
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
    return not errors, "PASS" if not errors else "FAIL: " + "; ".join(errors)


def paragraph_gate(objects: list[dict[str, Any]]) -> tuple[bool, str]:
    errors = []
    for obj in objects:
        if not obj.get("paragraph") or not obj["path"].startswith("cobol/"):
            continue
        path = ROOT / "legacy" / obj["path"]
        headers = paragraph_headers(path)
        matches = [(line, name) for line, name in headers if name == obj["paragraph"]]
        if not matches:
            errors.append(f"unknown paragraph {obj['path']}:{obj['paragraph']}")
            continue
        header_line = matches[0][0]
        if not obj["line_start"] <= header_line <= obj["line_end"]:
            errors.append(f"paragraph outside span {obj['path']}:{obj['line_start']}-{obj['line_end']} {obj['paragraph']}")
        next_headers = [line for line, name in headers if line > header_line and not name.endswith("-EXIT")]
        next_line = min(next_headers) if next_headers else len(path.read_text().splitlines()) + 1
        if obj["line_end"] >= next_line and not obj.get("paragraphs"):
            errors.append(f"span crosses next paragraph {obj['path']}:{obj['line_start']}-{obj['line_end']} {obj['paragraph']}")
    return not errors, "PASS" if not errors else "FAIL: " + "; ".join(errors)


def ears_gate(frs: list[dict[str, Any]]) -> tuple[bool, str]:
    failures = []
    keywords = {"state_driven": "While", "event_driven": "When", "optional_feature": "Where"}
    for fr in frs:
        statement, pattern = fr["statement"], fr["ears"]["pattern"]
        present = {word.lower() for word in ("While", "When", "Where", "If") if re.search(rf"\b{word}\b", statement, re.I)}
        if statement.count("shall") != 1:
            failures.append(f"{fr['id']} shall count")
        if pattern == "ubiquitous" and present:
            failures.append(f"{fr['id']} ubiquitous has a trigger keyword")
        if pattern in keywords and keywords[pattern].lower() not in present:
            failures.append(f"{fr['id']} {pattern} requires {keywords[pattern]}")
        if pattern == "unwanted_behaviour" and not re.search(r"\bIf\b.*\bThen\b", statement, re.I):
            failures.append(f"{fr['id']} unwanted_behaviour requires If...Then")
        if pattern == "complex" and len(present) < 2:
            failures.append(f"{fr['id']} complex requires two trigger keywords")
        if pattern == "event_driven" and not fr["ears"]["trigger"]:
            failures.append(f"{fr['id']} event_driven requires trigger")
        if pattern == "ubiquitous" and fr["ears"]["trigger"]:
            failures.append(f"{fr['id']} ubiquitous forbids trigger")
        if pattern != "complex" and len(fr["ears"]["system_response"]) != 1:
            failures.append(f"{fr['id']} must have one response")
    return not failures, "PASS" if not failures else "FAIL: " + "; ".join(failures)


def iso_gate(nfrs: list[dict[str, Any]]) -> tuple[bool, str]:
    failures = [f"{nfr['id']} invalid pair" for nfr in nfrs
                if nfr["subcharacteristic"] not in ISO25010.get(nfr["characteristic"], set())]
    return not failures, "all pairs valid" if not failures else "FAIL: " + "; ".join(failures)


def traceability_gate(req: dict[str, Any], plan: dict[str, Any], parity: dict[str, Any],
                      programs: set[str]) -> tuple[bool, str]:
    fr_ids = {fr["id"] for fr in req["functional_requirements"]}
    failures = []
    chunk_ids = {fr for chunk in plan["chunks"] for fr in chunk["fr_ids"]}
    report_ids = set(parity["fr_ids"])
    if unknown := sorted(chunk_ids - fr_ids):
        failures.append("plan dangling FRs " + ", ".join(unknown))
    if unknown := sorted(report_ids - fr_ids):
        failures.append("parity dangling FRs " + ", ".join(unknown))
    if orphan := sorted(fr_ids - chunk_ids):
        failures.append("orphan FRs " + ", ".join(orphan))
    if any(not chunk["fr_ids"] for chunk in plan["chunks"]):
        failures.append("orphan plan chunk")
    for collection in ("business_rules", "data_elements", "error_paths"):
        if any(not set(item["fr_ids"]) <= fr_ids for item in req[collection]):
            failures.append(f"{collection} dangling FR link")
    named = {chunk.get("program") for chunk in plan["chunks"]} | {parity["subject"]["program"]}
    if unknown := sorted((named - {None}) - programs):
        failures.append("unknown programs " + ", ".join(unknown))
    return not failures, "PASS" if not failures else "FAIL: " + "; ".join(failures)


def coverage_gate(stream: dict[str, Any], all_evidence: list[dict[str, Any]], profile: dict[str, Any]) -> tuple[bool, str]:
    coverage = compute_coverage(stream, all_evidence)
    program_pct = coverage["programs_cited"]
    paragraph_pct = coverage["paragraphs_cited"]
    declared_cov = stream["meta"]["coverage"]
    thresholds = profile["coverage_thresholds"]
    ok = (program_pct >= thresholds["programs_cited"] and paragraph_pct >= thresholds["paragraphs_cited"]
          and abs(program_pct - declared_cov["programs_cited"]) < 0.01
          and abs(paragraph_pct - declared_cov["paragraphs_cited"]) < 0.01)
    return ok, f"programs={program_pct:.0f}% paragraphs={paragraph_pct:.0f}%"


def compute_coverage(stream: dict[str, Any], all_evidence: list[dict[str, Any]]) -> dict[str, float]:
    declared = {p["name"] for module in stream["modules"] for item in module["streams"] for p in item["programs"]}
    cited = {Path(item["path"]).name for item in all_evidence if item["path"].startswith("cobol/")}
    program_pct = 100 * len(cited & declared) / max(1, len(declared))
    total = cited_paragraphs = 0
    for program in declared:
        path = ROOT / "legacy" / "cobol" / program
        headers = paragraph_headers(path)
        total += len(headers)
        ranges = [(e["line_start"], e["line_end"]) for e in all_evidence if e["path"] == f"cobol/{program}"]
        cited_paragraphs += sum(1 for line, _ in headers if any(a <= line <= b for a, b in ranges))
    paragraph_pct = 100 * cited_paragraphs / max(1, total)
    return {"programs_cited": program_pct, "paragraphs_cited": paragraph_pct}


def main() -> int:
    failures = []
    rows = []

    def gate(name: str, ok: bool, detail: str) -> None:
        rows.append((name, ok, detail))
        if not ok:
            failures.append(f"{name}: {detail}")

    for name, path in SCHEMAS.items():
        try:
            Draft202012Validator.check_schema(load(path))
            gate(f"schema:{name}", True, "2020-12 metaschema")
        except (TypeError, ValueError) as exc:
            gate(f"schema:{name}", False, str(exc))
    loaded = {name: load(ARTIFACTS / filename) for name, filename in {
        "stream": "stream-inventory.json", "requirements": "requirements.json",
        "plan": "migration-plan.json", "parity": "parity-report.json"}.items()}
    for name, schema_path in SCHEMAS.items():
        errors = sorted(Draft202012Validator(load(schema_path)).iter_errors(loaded[name]), key=str)
        gate(f"artifact:{name}", not errors, "schema valid" if not errors else errors[0].message)
    all_evidence = [item for artifact in loaded.values() for item in citations(artifact)]
    ok, detail = evidence_gate(all_evidence)
    gate("evidence hashes", ok, detail)
    ok, detail = paragraph_gate(all_evidence)
    gate("paragraph containment", ok, detail)
    req, plan, parity, stream = loaded["requirements"], loaded["plan"], loaded["parity"], loaded["stream"]
    programs = {p["name"] for m in stream["modules"] for s in m["streams"] for p in s["programs"]}
    ok, detail = traceability_gate(req, plan, parity, programs)
    gate("dangling references", ok, detail)
    ok, detail = ears_gate(req["functional_requirements"])
    gate("EARS lint", ok, detail)
    ok, detail = iso_gate(req["nonfunctional_requirements"])
    gate("ISO 25010 pairs", ok, detail)
    profile = load(ROOT / "schemas/profiles/oem-batch.profile.json")
    profile_failures = []
    for fr in req["functional_requirements"]:
        if fr["confidence"]["score"] < profile["min_confidence"] or not re.match(profile["id_patterns"]["fr"], fr["id"]):
            profile_failures.append(fr["id"])
        if fr["migration_category"] in profile["require_sme_approval_for"] and fr["sme_review"]["status"] != "approved":
            profile_failures.append(f"{fr['id']} SME")
    for nfr in req["nonfunctional_requirements"]:
        if not re.match(profile["id_patterns"]["nfr"], nfr["id"]):
            profile_failures.append(nfr["id"])
        if not all(nfr["measure"].get(field) for field in ("metric", "target", "unit", "source")):
            profile_failures.append(f"{nfr['id']} measure")
    for chunk in plan["chunks"]:
        if not re.match(profile["id_patterns"]["chunk"], chunk["id"]):
            profile_failures.append(chunk["id"])
    ok, detail = coverage_gate(stream, all_evidence, profile)
    gate("computed coverage", ok, detail)
    gate("profile policy", not profile_failures,
         "all IDs, confidence, measures, and approvals valid" if not profile_failures else ", ".join(profile_failures))
    print("gate                      status  detail")
    print("------------------------  ------  ----------------------------------------")
    for name, ok, detail in rows:
        print(f"{name:<24}  {'PASS' if ok else 'FAIL':<6}  {detail}")
    print(f"\nSUMMARY: {'all gates PASS' if not failures else f'{len(failures)} gate(s) FAILED'}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
