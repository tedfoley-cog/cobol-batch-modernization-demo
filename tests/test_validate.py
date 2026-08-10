import json
from pathlib import Path

from tools.validate import ears_gate, evidence_gate, traceability_gate


def test_tampered_evidence_hash_fails():
    evidence = {"path": "cobol/TRDPB001", "line_start": 1, "line_end": 1, "snippet_sha256": "0" * 64}
    ok, message = evidence_gate([evidence])
    assert not ok
    assert "hash mismatch" in message


def test_orphan_chunk_is_rejected_by_traceability_rule():
    artifact = json.loads(Path("artifacts/reference-example/migration-plan.json").read_text())
    artifact["chunks"][0]["fr_ids"] = []
    failures = traceability_gate(
        json.loads(Path("artifacts/reference-example/requirements.json").read_text())["functional_requirements"],
        artifact["chunks"],
    )
    assert "TRACEABILITY FAIL: orphan plan chunk" in failures


def test_broken_ears_pattern_is_detected(tmp_path):
    req = json.loads(Path("artifacts/reference-example/requirements.json").read_text())
    req["functional_requirements"][0]["ears"]["pattern"] = "event_driven"
    req["functional_requirements"][0]["statement"] = "The system shall read the parameter."
    fr = req["functional_requirements"][0]
    assert ears_gate([fr]) == ["EARS FAIL: FR-SETL-001 event_driven requires When"]
