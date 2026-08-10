"""Fetch the exact, non-vendored upstream estate used by the demo."""
from __future__ import annotations

import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEST = ROOT / "legacy"
URL = "https://github.com/cloudframe-samples/cloudframe-mainframe-modernization-tradingapp.git"
PIN = "d1450ca062445a6e70f2a2638af6ba286ca8fecc"
EXPECTED_MEMBERS = {
    "MSTPB001", "MSTPB002", "TRDPB000", "TRDPB001", "TRDPB002",
    "TRDPB003", "TRDPB004", "TRDPB006", "TRDPB007", "TRDPBEXC",
}


def run(*args: str) -> str:
    return subprocess.check_output(args, cwd=ROOT, text=True).strip()


def main() -> None:
    if not DEST.exists():
        subprocess.check_call(["git", "clone", "--depth", "1", URL, str(DEST)], cwd=ROOT)
    current = run("git", "-C", str(DEST), "rev-parse", "HEAD")
    if current != PIN:
        subprocess.check_call(["git", "-C", str(DEST), "fetch", "--depth", "1", "origin", PIN])
        subprocess.check_call(["git", "-C", str(DEST), "checkout", "--detach", PIN])
        current = run("git", "-C", str(DEST), "rev-parse", "HEAD")
    if current != PIN:
        raise SystemExit(f"FETCH FAIL: expected {PIN}, got {current}")
    members = {p.name for p in (DEST / "cobol").iterdir() if p.is_file()}
    if members != EXPECTED_MEMBERS:
        raise SystemExit(
            f"FETCH FAIL: expected {len(EXPECTED_MEMBERS)} COBOL members "
            f"{sorted(EXPECTED_MEMBERS)}, got {len(members)} {sorted(members)}"
        )
    required = ["jcl/SETLUSD.jcl", "proc/TRDPROC.proc", "controlcard/SETLUSD"]
    missing = [item for item in required if not (DEST / item).is_file()]
    if missing:
        raise SystemExit(f"FETCH FAIL: missing expected files: {missing}")
    print(f"FETCH PASS: {current}; COBOL members={len(members)}; required files={len(required)}")


if __name__ == "__main__":
    main()
