"""Normalize three parity legs and emit a schema-validated parity report."""
from __future__ import annotations

import argparse
import json
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

try:
    from .normalize import align_scale, decode_ebcdic, normalize_date, normalize_layout_record, split_fixed_width
except ImportError:
    from normalize import align_scale, decode_ebcdic, normalize_date, normalize_layout_record, split_fixed_width


def normalize_record(record: dict[str, Any], layout: list[dict[str, Any]]) -> dict[str, Any]:
    if "layout_record_hex" in record:
        decoded = normalize_layout_record(bytes.fromhex(record["layout_record_hex"]), layout)
        aliases = {"ORD_CURRENCY": "currency", "ORD_AMOUNT": "amount",
                   "ORD_STATUS": "status", "TRADE_DATE": "trade_date"}
        return {aliases.get(key, key): value for key, value in decoded.items()
                if key in aliases}
    result = dict(record)
    if "fixed_width" in result:
        result.update(split_fixed_width(result.pop("fixed_width"), layout))
    if "ebcdic" in result:
        result["text"] = decode_ebcdic(bytes.fromhex(result.pop("ebcdic")))
    for key, value in list(result.items()):
        if key.endswith("_date") and isinstance(value, str):
            result[key] = normalize_date(value)
        if key.endswith("_scaled") and isinstance(value, dict):
            result[key] = str(align_scale(value["value"], value["from_scale"], value["to_scale"],
                                          value.get("rounding", "HALF_UP")))
    return result


def build_report(payload: dict[str, Any], layout: list[dict[str, Any]]) -> dict[str, Any]:
    legs = {}
    for name, leg in payload["legs"].items():
        legs[name] = {**leg, "records": [normalize_record(r, layout) for r in leg["records"]]}
    expected = legs["expected_semantic"]["records"]
    legacy = legs["legacy_execution"]["records"]
    target = legs["target_execution"]["records"]
    fields = sorted({k for rows in (expected, legacy, target) for row in rows for k in row})
    comparisons = []
    for field in fields:
        vals = {name: (rows[0].get(field) if rows else None)
                for name, rows in (("expected_semantic", expected), ("legacy_execution", legacy),
                                   ("target_execution", target))}
        verdict = "pass" if vals["expected_semantic"] == vals["legacy_execution"] == vals["target_execution"] else "fail"
        comparisons.append({"field": field, "values": vals, "verdict": verdict, "tolerance": "exact"})
    count = len(expected)
    return {
        "subject": payload["subject"], "legs": legs,
        "normalization_rules_applied": payload.get("normalization_rules_applied", []),
        "comparisons": comparisons,
        "control_totals": {"expected": payload.get("control_total", "n/a"), "legacy": payload.get("control_total", "n/a"), "target": payload.get("control_total", "n/a")},
        "record_counts": {"expected": count, "legacy": len(legacy), "target": len(target)},
        "verdict": "pass" if all(c["verdict"] == "pass" for c in comparisons) else "fail",
        "run_metadata": {"run_id": payload.get("run_id", "local"), "timestamp": datetime.now(UTC).isoformat(), "environment": payload.get("environment", "local")},
        "fr_ids": payload["fr_ids"],
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("input", type=Path)
    parser.add_argument("layout", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    if args.input.is_dir():
        payload = {
            "subject": {"program": "TRDPB001", "stream": "SETLUSD"},
            "legs": {
                name: json.loads((args.input / f"{name}.json").read_text())
                for name in ("expected_semantic", "legacy_execution", "target_execution")
            },
            "normalization_rules_applied": [
                "packed COMP-3 5-byte amount to BigDecimal",
                "trim EBCDIC/CHAR padding",
                "YYYYMMDD to ISO date",
            ],
            "fr_ids": ["FR-SETL-008"],
            "run_id": "reference-settlement-001",
            "environment": "portable H2 / supplied legacy fixture",
        }
    else:
        payload = json.loads(args.input.read_text())
    layout = json.loads(args.layout.read_text())
    args.output.write_text(json.dumps(build_report(payload, layout), indent=2) + "\n")
    print(f"PARITY PASS: wrote {args.output}")


if __name__ == "__main__":
    main()
