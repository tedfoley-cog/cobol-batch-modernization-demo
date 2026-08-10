import json
from pathlib import Path

from tools.parity import build_report


def test_parity_report_passes_for_equal_legs():
    payload = {
        "subject": {"program": "TRDPB001", "stream": "SETLUSD"},
        "legs": {
            "expected_semantic": {"records": [{"status": "601"}], "provenance": {"kind": "semantic", "source": "test"}},
            "legacy_execution": {"records": [{"status": "601"}], "provenance": {"kind": "supplied", "source": "test"}},
            "target_execution": {"records": [{"status": "601"}], "provenance": {"kind": "target", "source": "test"}},
        },
        "fr_ids": ["FR-SETL-006"],
    }
    assert build_report(payload, [])[ "verdict"] == "pass"


def test_reference_report_is_json():
    data = json.loads(Path("artifacts/reference-example/parity-report.json").read_text())
    assert data["legs"]["legacy_execution"]["provenance"]["kind"] == "supplied"
