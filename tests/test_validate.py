import json
from pathlib import Path

from tools.validate import coverage_gate, ears_gate, evidence_gate, iso_gate, paragraph_gate, traceability_gate


def test_tampered_evidence_hash_fails():
    evidence = {"path": "cobol/TRDPB001", "line_start": 1, "line_end": 1, "snippet_sha256": "0" * 64}
    ok, message = evidence_gate([evidence])
    assert not ok
    assert "hash mismatch" in message


def test_orphan_chunk_is_rejected_by_traceability_rule():
    artifact = json.loads(Path("artifacts/reference-example/migration-plan.json").read_text())
    artifact["chunks"][0]["fr_ids"] = []
    req = json.loads(Path("artifacts/reference-example/requirements.json").read_text())
    parity = json.loads(Path("artifacts/reference-example/parity-report.json").read_text())
    ok, message = traceability_gate(req, artifact, parity, {"TRDPB001", "TRDPB002", "TRDPB003", "TRDPBEXC"})
    assert not ok
    assert "orphan plan chunk" in message


def test_broken_ears_pattern_is_detected(tmp_path):
    req = json.loads(Path("artifacts/reference-example/requirements.json").read_text())
    req["functional_requirements"][0]["ears"]["pattern"] = "event_driven"
    req["functional_requirements"][0]["statement"] = "The system shall read the parameter."
    fr = req["functional_requirements"][0]
    ok, message = ears_gate([fr])
    assert not ok
    assert "FR-SETL-001 event_driven requires When" in message


def test_fabricated_paragraph_span_is_rejected():
    evidence = {"path": "cobol/TRDPB001", "line_start": 95, "line_end": 115,
                "paragraph": "1000-OPEN-SETTLEMENT-CURSOR", "snippet_sha256": "0" * 64}
    ok, message = paragraph_gate([evidence])
    assert not ok
    assert "unknown paragraph" in message


def test_dangling_fr_is_rejected():
    req = json.loads(Path("artifacts/reference-example/requirements.json").read_text())
    plan = json.loads(Path("artifacts/reference-example/migration-plan.json").read_text())
    parity = json.loads(Path("artifacts/reference-example/parity-report.json").read_text())
    plan["chunks"][0]["fr_ids"].append("FR-MISSING-999")
    ok, message = traceability_gate(req, plan, parity, {"TRDPB001", "TRDPB002", "TRDPB003", "TRDPBEXC"})
    assert not ok
    assert "plan dangling FRs" in message


def test_coverage_threshold_is_rejected():
    stream = json.loads(Path("artifacts/reference-example/stream-inventory.json").read_text())
    profile = json.loads(Path("schemas/profiles/oem-batch.profile.json").read_text())
    profile["coverage_thresholds"]["paragraphs_cited"] = 99
    ok, _ = coverage_gate(stream, [], profile)
    assert not ok


def test_invalid_iso_pair_is_rejected():
    req = json.loads(Path("artifacts/reference-example/requirements.json").read_text())
    req["nonfunctional_requirements"][0]["subcharacteristic"] = "not_real"
    ok, message = iso_gate(req["nonfunctional_requirements"])
    assert not ok
    assert "invalid pair" in message


def test_bad_chunk_id_is_rejected_by_profile():
    profile = json.loads(Path("schemas/profiles/oem-batch.profile.json").read_text())
    import re

    assert not re.match(profile["id_patterns"]["chunk"], "bad-chunk")


def test_generated_evidence_requires_provenance_marker():
    evidence = {
        "path": "artifacts/generated/dclgen/DCLTBTRDSTQ",
        "line_start": 1,
        "line_end": 1,
        "snippet_sha256": "0" * 64,
    }
    ok, message = evidence_gate([evidence])
    assert not ok
    assert "missing provenance" in message
