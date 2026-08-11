"""Reads the estate's own artifacts so the driver is not a second source of truth.

Everything here is derived from files that already exist in the repository:

  * routes       - db2/ddl/40_SEED_PGM_ROUTE.sql
  * program maps - the WS-MAP working storage literals in the COBOL
  * data         - app/data/*.csv

Nothing about the call structure is hard-coded; the driver resolves a menu
option the same way ``CACRD00`` does, by looking up ``MENU``/``OPTnn`` in the
route table.
"""

from __future__ import annotations

import csv
import re
from dataclasses import dataclass
from datetime import date
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
SEED_SQL = REPO_ROOT / "db2" / "ddl" / "40_SEED_PGM_ROUTE.sql"
BMS_DIR = REPO_ROOT / "app" / "cardsvc" / "bms"
CBL_DIR = REPO_ROOT / "app" / "cardsvc" / "cbl"
DATA_DIR = REPO_ROOT / "app" / "data"

HIGH_DATE = date(9999, 12, 31)


@dataclass(frozen=True)
class Route:
    route_type: str
    route_key: str
    seq: int
    program: str
    call_type: str
    module: str
    eff_date: date
    exp_date: date
    active: bool
    fallback: str
    description: str

    def in_effect(self, on: date) -> bool:
        return self.active and self.eff_date <= on <= self.exp_date


# The seed uses two column orders: with and without EXP_DATE.
_INSERT_RE = re.compile(
    r"INSERT\s+INTO\s+CARDSVC\.PGM_ROUTE\s*\((?P<cols>[^)]*)\)\s*VALUES\s*(?P<vals>.*?);",
    re.S | re.I,
)
_ROW_RE = re.compile(r"\((?P<row>[^()]*)\)", re.S)


def _split_row(row: str) -> list[str]:
    out: list[str] = []
    cur: list[str] = []
    quoted = False
    i = 0
    while i < len(row):
        ch = row[i]
        if quoted:
            if ch == "'":
                if i + 1 < len(row) and row[i + 1] == "'":
                    cur.append("'")
                    i += 1
                else:
                    quoted = False
            else:
                cur.append(ch)
        elif ch == "'":
            quoted = True
        elif ch == ",":
            out.append("".join(cur).strip())
            cur = []
        else:
            cur.append(ch)
        i += 1
    out.append("".join(cur).strip())
    return out


def _as_date(text: str, default: date) -> date:
    text = text.strip()
    if not text:
        return default
    return date.fromisoformat(text)


def load_routes(path: Path = SEED_SQL) -> list[Route]:
    """Parse the PGM_ROUTE seed into Route records."""
    sql = path.read_text()
    routes: list[Route] = []

    for stmt in _INSERT_RE.finditer(sql):
        cols = [c.strip().upper() for c in stmt.group("cols").split(",")]
        for m in _ROW_RE.finditer(stmt.group("vals")):
            values = _split_row(m.group("row"))
            if len(values) != len(cols):
                continue
            row = dict(zip(cols, values))
            routes.append(
                Route(
                    route_type=row["ROUTE_TYPE"].strip(),
                    route_key=row["ROUTE_KEY"].strip(),
                    seq=int(row["SEQ_NBR"]),
                    program=row["PGM_NAME"].strip(),
                    call_type=row["CALL_TYPE"].strip(),
                    module=row["MODULE_ID"].strip(),
                    eff_date=_as_date(row.get("EFF_DATE", ""), date(1900, 1, 1)),
                    exp_date=_as_date(row.get("EXP_DATE", ""), HIGH_DATE),
                    active=row.get("ACTIVE_FLG", "Y").strip().upper() == "Y",
                    fallback=row.get("FALLBACK_PGM", "").strip(),
                    description=row.get("DESCRIPTION", "").strip(),
                )
            )
    return routes


class RouteTable:
    """The dispatcher's view of PGM_ROUTE, honouring dates and the active flag."""

    def __init__(self, routes: list[Route], as_of: date | None = None):
        self.routes = routes
        self.as_of = as_of or date.today()

    def resolve(self, route_type: str, route_key: str, seq: int = 1) -> Route | None:
        for r in self.routes:
            if (
                r.route_type == route_type
                and r.route_key == route_key
                and r.seq == seq
                and r.in_effect(self.as_of)
            ):
                return r
        return None

    def pipeline(self, route_type: str, route_key: str) -> list[Route]:
        found = [
            r
            for r in self.routes
            if r.route_type == route_type
            and r.route_key == route_key
            and r.in_effect(self.as_of)
        ]
        return sorted(found, key=lambda r: r.seq)


# ---------------------------------------------------------------------------
# Program -> BMS map, read out of the COBOL working storage literals.
# ---------------------------------------------------------------------------

_MAP_LIT_RE = re.compile(
    r"^\s{6,}01\s+(?P<name>WS-MAPSET|WS-MAP[A-Z0-9-]*)\s+PIC\s+X\(8\)\s+VALUE\s+'(?P<val>[^']{1,8})'",
    re.M,
)


def load_program_maps(cbl_dir: Path = CBL_DIR) -> dict[str, dict[str, str]]:
    """For each program, the mapset it uses and the maps it sends.

    Returns ``{program: {"mapset": name, "maps": {ws_name: map_name}}}``.
    """
    out: dict[str, dict[str, str]] = {}
    for path in sorted(cbl_dir.glob("*.cbl")):
        text = path.read_text(errors="replace")
        mapset = None
        maps: dict[str, str] = {}
        for m in _MAP_LIT_RE.finditer(text):
            name, val = m.group("name"), m.group("val").strip()
            if name == "WS-MAPSET":
                mapset = val
            else:
                maps[name] = val
        if mapset and maps:
            out[path.stem] = {"mapset": mapset, "maps": maps}
    return out


# ---------------------------------------------------------------------------
# Sample data
# ---------------------------------------------------------------------------


def _read_csv(name: str) -> list[dict[str, str]]:
    path = DATA_DIR / name
    if not path.exists():
        return []
    with path.open(newline="") as fh:
        return list(csv.DictReader(fh))


class SampleData:
    """The CSV companions to the EBCDIC datasets, indexed for lookup."""

    def __init__(self) -> None:
        self.accounts = _read_csv("accounts.csv")
        self.cards = _read_csv("cards.csv")
        self.customers = _read_csv("customers.csv")
        self.limits = _read_csv("limits.csv")
        self.kyc = _read_csv("kyc.csv")
        self.merchants = _read_csv("merchants.csv")
        self.parties = _read_csv("parties.csv")
        self.sanctions = _read_csv("sanctions.csv")
        self.fraud_rules = _read_csv("fraud_rules.csv")
        self.fee_schedule = _read_csv("fee_schedule.csv")

        self._by_acct = {r["ACCT_ID"]: r for r in self.accounts}
        self._by_card = {r["CARD_NUM"]: r for r in self.cards}
        self._by_cust = {r["CUST_ID"]: r for r in self.customers}
        self._by_merch = {r["MERCHANT_ID"]: r for r in self.merchants}

        self._cards_by_acct: dict[str, list[dict[str, str]]] = {}
        for r in self.cards:
            self._cards_by_acct.setdefault(r["ACCT_ID"], []).append(r)

        self._limits_by_card: dict[str, list[dict[str, str]]] = {}
        for r in self.limits:
            self._limits_by_card.setdefault(r["CARD_NUM"], []).append(r)

        self._kyc_by_party: dict[str, list[dict[str, str]]] = {}
        for r in self.kyc:
            self._kyc_by_party.setdefault(r["PARTY_ID"], []).append(r)

    # -- lookups --------------------------------------------------------

    def account(self, acct_id: str) -> dict[str, str] | None:
        return self._by_acct.get(acct_id.strip())

    def card(self, card_num: str) -> dict[str, str] | None:
        return self._by_card.get(card_num.strip())

    def customer(self, cust_id: str) -> dict[str, str] | None:
        return self._by_cust.get(cust_id.strip())

    def merchant(self, merchant_id: str) -> dict[str, str] | None:
        return self._by_merch.get(merchant_id.strip())

    def cards_for_account(self, acct_id: str) -> list[dict[str, str]]:
        return self._cards_by_acct.get(acct_id.strip(), [])

    def limits_for_card(self, card_num: str) -> list[dict[str, str]]:
        return self._limits_by_card.get(card_num.strip(), [])

    def credit_limit(self, card_num: str) -> dict[str, str] | None:
        for r in self.limits_for_card(card_num):
            if r["LIMIT_TYPE"] == "CRED":
                return r
        rows = self.limits_for_card(card_num)
        return rows[0] if rows else None

    def latest_kyc(self, party_id: str) -> dict[str, str] | None:
        rows = self._kyc_by_party.get(party_id.strip(), [])
        if not rows:
            return None
        return max(rows, key=lambda r: int(r["KYC_SEQ"]))

    def sanctions_for_party(self, party_id: str) -> list[dict[str, str]]:
        return [r for r in self.sanctions if r.get("PARTY_ID", "") == party_id.strip()]

    def first_account(self) -> dict[str, str] | None:
        return self.accounts[0] if self.accounts else None
