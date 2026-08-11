"""Transaction CA00 screen flow.

This drives the maps and the menu routing.  It is a *harness*, not the
estate: the COBOL is not executed, and the field values shown come from the
CSV companions in app/data.  What it does reproduce faithfully is the screen
layout, the field attributes, the AID key conventions and the way an option
is turned into a program name by looking it up in PGM_ROUTE.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass
from datetime import date, datetime

from .bmsparse import parse_all
from .estate import BMS_DIR, RouteTable, SampleData, load_program_maps, load_routes
from .screen import Screen

MENU_PROGRAM = "CACRD00"
ERROR_PROGRAM = "CACRD91"

# Status and decision text the screens display alongside their codes.
CARD_STATUS_TEXT = {
    "A": "ACTIVE",
    "B": "BLOCKED",
    "C": "CLOSED",
    "L": "LOST / STOLEN",
    "E": "EXPIRED",
    "N": "NEW - NOT ACTIVATED",
}
ACCT_STATUS_TEXT = {
    "O": "OPEN",
    "C": "CLOSED",
    "S": "SUSPENDED",
    "W": "WRITTEN OFF",
}
DELQ_TEXT = {
    "0": "CURRENT",
    "1": "1-29 DAYS",
    "2": "30-59 DAYS",
    "3": "60-89 DAYS",
    "4": "90+ DAYS",
}
BLOCK_REASONS = {
    "LS": "REPORTED LOST",
    "ST": "REPORTED STOLEN",
    "FR": "SUSPECTED FRAUD",
    "CH": "CUSTOMER REQUEST",
    "DQ": "DELINQUENCY",
}
REASON_CODES = [
    ("DISP", "10.4", "OTHER FRAUD CARD ABS", "CB", "Y", "120", "VISA"),
    ("DISP", "13.1", "SERVICES NOT PROVIDED", "CB", "Y", "120", "VISA"),
    ("DISP", "13.3", "NOT AS DESCRIBED", "CB", "Y", "120", "VISA"),
    ("DISP", "13.6", "CREDIT NOT PROCESSED", "CB", "Y", "120", "VISA"),
    ("DISP", "4853", "CARDHOLDER DISPUTE", "CB", "Y", "120", "MSTR"),
    ("DISP", "4837", "NO CARDHOLDER AUTH", "CB", "N", "090", "MSTR"),
    ("AUTH", "05", "DO NOT HONOUR", "DC", "N", "000", "BOTH"),
    ("AUTH", "51", "INSUFFICIENT FUNDS", "DC", "N", "000", "BOTH"),
    ("AUTH", "54", "EXPIRED CARD", "DC", "N", "000", "BOTH"),
    ("AUTH", "61", "EXCEEDS LIMIT", "DC", "N", "000", "BOTH"),
    ("AUTH", "62", "RESTRICTED CARD", "DC", "N", "000", "BOTH"),
    ("AUTH", "65", "VELOCITY EXCEEDED", "DC", "N", "000", "BOTH"),
    ("BLOK", "LS", "REPORTED LOST", "BL", "Y", "000", "N/A"),
    ("BLOK", "ST", "REPORTED STOLEN", "BL", "Y", "000", "N/A"),
    ("BLOK", "FR", "SUSPECTED FRAUD", "BL", "Y", "030", "N/A"),
    ("FEE", "ANNU", "ANNUAL FEE", "PS", "Y", "365", "N/A"),
    ("FEE", "LATE", "LATE PAYMENT FEE", "PS", "Y", "030", "N/A"),
    ("FEE", "OVLM", "OVER LIMIT FEE", "PS", "N", "030", "N/A"),
    ("FEE", "CASH", "CASH ADVANCE FEE", "PS", "Y", "000", "N/A"),
    ("FEE", "FRGN", "FOREIGN TXN FEE", "PS", "Y", "000", "N/A"),
]

# AID keys.
ENTER, CLEAR = "ENTER", "CLEAR"
PF = {n: f"PF{n}" for n in range(1, 25)}


@dataclass
class Transition:
    """What the harness does after an AID key."""

    kind: str  # "stay" | "goto" | "exit"
    program: str | None = None
    map_name: str | None = None
    message: str = ""
    route_note: str = ""


@dataclass
class Commarea:
    """Stands in for the 512-byte CA00 commarea passed between programs."""

    acct_id: str = ""
    card_num: str = ""
    cust_id: str = ""
    party_id: str = ""
    txn_id: str = ""
    amount: str = ""
    action: str = ""
    from_program: str = ""


@dataclass
class Trace:
    """One dispatch, as ROUTE_AUDIT would record it."""

    seq: int
    from_program: str
    route_type: str
    route_key: str
    program: str
    call_type: str
    module: str
    used_fallback: bool = False

    def line(self) -> str:
        arrow = "XCTL" if self.call_type == "X" else "LINK" if self.call_type == "L" else "CALL"
        flag = " (FALLBACK)" if self.used_fallback else ""
        return (
            f"{self.seq:>3} {self.from_program:<8} {self.route_type}/{self.route_key:<8} "
            f"{arrow} {self.program:<8} [{self.module}]{flag}"
        )


def _money(value: str, width: int = 16) -> str:
    try:
        amount = float(value)
    except (TypeError, ValueError):
        return "".rjust(width)
    return f"{amount:,.2f}".rjust(width)


def _put_money(screen: Screen, name: str, value: str) -> None:
    """Right align an amount in whatever width the map gave the field."""
    width = screen.width(name)
    if width:
        screen.put(name, _money(value, width))


def _yymmdd(iso: str) -> str:
    if not iso:
        return ""
    try:
        d = date.fromisoformat(iso)
    except ValueError:
        return iso
    return d.strftime("%m/%d/%y")


def _band(score: int) -> str:
    """Risk band from a score, on the estate's A-D scale."""
    if score >= 700:
        return "A"
    if score >= 500:
        return "B"
    if score >= 300:
        return "C"
    return "D"


def _stable_int(seed: str, lo: int, hi: int) -> int:
    digest = hashlib.sha256(seed.encode()).digest()
    return lo + int.from_bytes(digest[:4], "big") % (hi - lo + 1)


class Session:
    """Terminal session state for one CA00 conversation."""

    def __init__(self, operator: str = "CSROP01", termid: str = "T001",
                 as_of: date | None = None):
        self.mapsets = parse_all(BMS_DIR)
        self.routes = RouteTable(load_routes(), as_of=as_of)
        self.program_maps = load_program_maps()
        self.data = SampleData()

        self.operator = operator
        self.termid = termid
        self.started = datetime.now()
        self.screen_count = 0
        self.trace: list[Trace] = []

        self.commarea = Commarea()
        self.program = MENU_PROGRAM
        self.map_name = "CARDMNU"
        self.message = ""
        self.page = 0
        self.ref_filter = ""
        self.finished = False
        # Output written by an AID handler, reapplied when the map is redrawn.
        self.out: dict[str, str] = {}

        self.screen = self._build()

    # -- map handling ---------------------------------------------------

    def _map(self, name: str):
        for ms in self.mapsets.values():
            if name in ms.maps:
                return ms.maps[name]
        raise KeyError(f"map {name} is not in any mapset")

    def mapset_for(self, name: str) -> str:
        for msname, ms in self.mapsets.items():
            if name in ms.maps:
                return msname
        return ""

    def _build(self) -> Screen:
        self.screen_count += 1
        screen = Screen.from_map(self._map(self.map_name))
        self._stamp(screen)
        # A program with more than one map gets a fill routine per map.
        handler = getattr(self, f"_fill_{self.map_name.lower()}", None) or getattr(
            self, f"_fill_{self.program.lower()}", None
        )
        if handler:
            handler(screen)
        for name, value in self.out.items():
            screen.put(name, value)
        for name in ("MNMSG", "AIMSG", "CLMSG", "CDMSG", "AUMSG", "ARMSG", "ERMSG",
                     "BYMSG", "M11MSG", "M11BMSG", "M12MSG", "M13MSG", "M13BMSG",
                     "M14MSG", "M15MSG", "M16MSG", "M16BMSG"):
            if self.message:
                screen.put(name, self.message)
        return screen

    def _stamp(self, screen: Screen) -> None:
        now = datetime.now()
        for name in ("MNDATE", "AIDATE", "CLDATE", "CDDATE", "AUDATE", "ARDATE",
                     "ERDATE", "M11DATE", "M12DATE", "M13DATE", "M13BDTE",
                     "M14DATE", "M15DATE", "M16DATE", "M16BDTE"):
            screen.put(name, now.strftime("%m/%d/%y"))
        for name in ("MNTIME", "ERTIME", "BYTIME", "M11TIME"):
            screen.put(name, now.strftime("%H:%M:%S"))
        screen.put("MNOPER", self.operator)
        screen.put("MNTERM", self.termid)
        screen.put("ERTRAN", "CA00")
        screen.put("ERTERM", self.termid)

    # -- dispatch -------------------------------------------------------

    def _dispatch(self, route_type: str, route_key: str) -> Transition:
        route = self.routes.resolve(route_type, route_key)
        if route is None:
            return Transition(
                "stay", message=f"NO ACTIVE ROUTE FOR {route_type}/{route_key} - CHECK PGM_ROUTE"
            )
        self.trace.append(
            Trace(
                seq=len(self.trace) + 1,
                from_program=self.program,
                route_type=route.route_type,
                route_key=route.route_key,
                program=route.program,
                call_type=route.call_type,
                module=route.module,
            )
        )
        if route.program not in self.program_maps:
            # The route resolves but the load module is not in the estate -
            # exactly what CACRD90 turns into PGMIDERR and a fallback.
            self.trace.append(
                Trace(
                    seq=len(self.trace) + 1,
                    from_program=self.program,
                    route_type=route.route_type,
                    route_key=route.route_key,
                    program=route.fallback or ERROR_PROGRAM,
                    call_type=route.call_type,
                    module="CARDSVC",
                    used_fallback=True,
                )
            )
            return Transition(
                "goto",
                program=route.fallback or ERROR_PROGRAM,
                message=f"PGMIDERR ON {route.program} - FALLBACK TAKEN",
            )
        return Transition("goto", program=route.program)

    def _first_map(self, program: str) -> str:
        entry = self.program_maps.get(program)
        if not entry:
            return "CARDERR"
        return next(iter(entry["maps"].values()))

    def goto(self, program: str, map_name: str | None = None, message: str = "") -> None:
        self.commarea.from_program = self.program
        self.program = program
        self.map_name = map_name or self._first_map(program)
        self.message = message
        self.page = 0
        self.out.clear()
        self.screen = self._build()

    # -- the AID key entry point ----------------------------------------

    def submit(self, aid: str) -> None:
        """Handle an AID key against the current screen."""
        handler = getattr(self, f"_aid_{self.map_name.lower()}", None) or getattr(
            self, f"_aid_{self.program.lower()}", None
        )
        transition = handler(aid) if handler else self._default_aid(aid)

        if transition.kind == "exit":
            self.finished = True
            return
        if transition.kind == "goto" and transition.program:
            self.goto(transition.program, transition.map_name, transition.message)
            return
        self.message = transition.message
        if transition.map_name and transition.map_name != self.map_name:
            self.map_name = transition.map_name
            self.out.clear()
            self.screen = self._build()
        else:
            rebuilt = self._build()
            # Preserve what the operator typed when staying on the same map.
            for f in self.screen.inputs:
                value = self.screen.get(f.name)
                if value:
                    rebuilt.put(f.name, value)
            self.screen = rebuilt

    def _default_aid(self, aid: str) -> Transition:
        if aid == PF[3]:
            return Transition("goto", program=MENU_PROGRAM)
        if aid == PF[12]:
            return self._dispatch("MENU", "OPTX")
        return Transition("stay", message=f"{aid} IS NOT ACTIVE ON THIS SCREEN")

    # -- CACRD00 main menu ----------------------------------------------

    def _fill_cacrd00(self, screen: Screen) -> None:
        screen.put("MNACCT", self.commarea.acct_id)
        screen.put("MNCARD", self.commarea.card_num)

    def _aid_cacrd00(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._dispatch("MENU", "OPTX")
        if aid != ENTER:
            return Transition("stay", message="INVALID KEY - ENTER, PF3 OR PF12")

        self.commarea.acct_id = self.screen.get("MNACCT")
        self.commarea.card_num = self.screen.get("MNCARD")
        option = self.screen.get("MNOPT").upper()

        if not option:
            return Transition("stay", message="ENTER AN OPTION")
        if option == "X":
            return self._dispatch("MENU", "OPTX")
        if not option.isdigit() or not 1 <= int(option) <= 10:
            return Transition("stay", message=f"INVALID OPTION {option}")

        return self._dispatch("MENU", f"OPT{int(option):02d}")

    # -- CACRD01 account inquiry -----------------------------------------

    def _fill_cacrd01(self, screen: Screen) -> None:
        screen.put("AIACCT", self.commarea.acct_id)
        acct = self.data.account(self.commarea.acct_id)
        if not acct:
            return
        screen.put("AICUST", acct["CUST_ID"])
        screen.put("AISTAT", acct["ACCT_STATUS"])
        screen.put("AISTATD", ACCT_STATUS_TEXT.get(acct["ACCT_STATUS"], ""))
        screen.put("AIPROD", acct["PRODUCT_CD"])
        screen.put("AICURR", acct["CURRENCY_CD"])
        screen.put("AIBRCH", acct["BRANCH_CD"])
        _put_money(screen, "AICBAL", acct["CURR_BAL"])
        _put_money(screen, "AISBAL", acct["STMT_BAL"])
        _put_money(screen, "AIPEND", acct["PENDING_AUTH_AMT"])
        _put_money(screen, "AICASH", acct["CASH_BAL"])
        _put_money(screen, "AIMINP", acct["MIN_PAY_DUE"])
        _put_money(screen, "AILPAY", acct["LAST_PAY_AMT"])
        screen.put("AILPDT", _yymmdd(acct["LAST_PAY_DATE"]))
        screen.put("AICYCD", acct["CYCLE_DAY"])
        screen.put("AILCYC", _yymmdd(acct["LAST_CYCLE_DATE"]))
        screen.put("AINCYC", _yymmdd(acct["NEXT_CYCLE_DATE"]))
        screen.put("AIDUE", _yymmdd(acct["PAY_DUE_DATE"]))
        screen.put("AIDELQ", acct["DELQ_BUCKET"])
        screen.put("AIDELQD", DELQ_TEXT.get(acct["DELQ_BUCKET"], ""))
        _put_money(screen, "AIDLQA", acct["DELQ_AMT"])

    def _aid_cacrd01(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        acct_id = self.screen.get("AIACCT")
        if not acct_id:
            return Transition("stay", message="ENTER AN ACCOUNT NUMBER")
        self.commarea.acct_id = acct_id
        if not self.data.account(acct_id):
            return Transition("stay", message=f"ACCOUNT {acct_id} NOT FOUND - SQLCODE +100")
        return Transition("stay")

    # -- CACRD02 card list ------------------------------------------------

    def _fill_cacrd02(self, screen: Screen) -> None:
        screen.put("CLACCT", self.commarea.acct_id)
        cards = self.data.cards_for_account(self.commarea.acct_id)
        page = cards[self.page * 10 : self.page * 10 + 10]
        screen.put("CLPAGE", str(self.page + 1))
        screen.put("CLCNT", str(len(page)))
        for i, card in enumerate(page, start=1):
            screen.put(f"CLCARD{i}", card["CARD_NUM"])
            screen.put(f"CLSTAT{i}", card["CARD_STATUS"])
            screen.put(f"CLPROD{i}", card["PRODUCT_CD"])
            screen.put(f"CLEXP{i}", card["EXPIRY_YYMM"])
            screen.put(f"CLNAME{i}", card["EMBOSSED_NAME"])

    def _aid_cacrd02(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        self.commarea.acct_id = self.screen.get("CLACCT")
        cards = self.data.cards_for_account(self.commarea.acct_id)

        if aid == PF[8]:
            if (self.page + 1) * 10 < len(cards):
                self.page += 1
                return Transition("stay")
            return Transition("stay", message="LAST PAGE")
        if aid == PF[7]:
            if self.page:
                self.page -= 1
                return Transition("stay")
            return Transition("stay", message="FIRST PAGE")

        for i in range(1, 11):
            if self.screen.get(f"CLSEL{i}").upper() == "S":
                index = self.page * 10 + i - 1
                if index < len(cards):
                    self.commarea.card_num = cards[index]["CARD_NUM"]
                    return self._dispatch("MENU", "OPT03")
        if not cards:
            return Transition("stay", message="NO CARDS FOR THIS ACCOUNT")
        return Transition("stay")

    # -- CACRD03 card detail ----------------------------------------------

    def _fill_cacrd03(self, screen: Screen) -> None:
        screen.put("CDCARD", self.commarea.card_num)
        card = self.data.card(self.commarea.card_num)
        if not card:
            return
        screen.put("CDACCT", card["ACCT_ID"])
        screen.put("CDCUST", card["CUST_ID"])
        screen.put("CDNAME", card["EMBOSSED_NAME"])
        screen.put("CDPROD", card["PRODUCT_CD"])
        screen.put("CDSTAT", card["CARD_STATUS"])
        screen.put("CDSTATD", CARD_STATUS_TEXT.get(card["CARD_STATUS"], ""))
        screen.put("CDEXP", card["EXPIRY_YYMM"])
        screen.put("CDISS", _yymmdd(card["ISSUE_DATE"]))
        screen.put("CDACT", card["ACTIVATION_YYMMDD"])
        screen.put("CDLUSE", _yymmdd(card["LAST_USED_DATE"]))
        screen.put("CDBLKR", card["BLOCK_REASON"])
        screen.put("CDBLKD", _yymmdd(card["BLOCK_DATE"]))
        screen.put("CDPIN", card["PIN_TRIES"])

        limits = self.data.limits_for_card(self.commarea.card_num)
        for i, lim in enumerate(limits[:4], start=1):
            screen.put(f"CDLTYP{i}", lim["LIMIT_TYPE"])
            _put_money(screen, f"CDLLIM{i}", lim["LIMIT_AMT"])
            _put_money(screen, f"CDLUSD{i}", lim["USED_AMT"])
            _put_money(screen, f"CDLAVL{i}", lim["AVAIL_AMT"])
        credit = self.data.credit_limit(self.commarea.card_num)
        if credit:
            screen.put("CDAPR", credit["APR_PCT"])
            screen.put("CDCAPR", credit["CASH_APR_PCT"])
            screen.put("CDBAND", credit["RISK_BAND"])

    def _aid_cacrd03(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        card_num = self.screen.get("CDCARD")
        if not card_num:
            return Transition("stay", message="ENTER A CARD NUMBER")
        self.commarea.card_num = card_num
        card = self.data.card(card_num)
        if not card:
            return Transition("stay", message=f"CARD {card_num} NOT ON CARDXREF")
        self.commarea.acct_id = card["ACCT_ID"]
        return Transition("stay")

    # -- CACRD04 authorization request ------------------------------------

    def _fill_cacrd04(self, screen: Screen) -> None:
        screen.put("AUCARD", self.commarea.card_num)
        screen.put("AUTYPE", "P")
        screen.put("AUCURR", "USD")
        screen.put("AUCTRY", "USA")

    def _aid_cacrd04(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)

        card_num = self.screen.get("AUCARD")
        amount = self.screen.get("AUAMT")
        auth_type = (self.screen.get("AUTYPE") or "P").upper()

        if not card_num:
            return Transition("stay", message="CARD NUMBER IS REQUIRED")
        card = self.data.card(card_num)
        if not card:
            return Transition("stay", message=f"CARD {card_num} NOT ON CARDXREF")
        if not amount:
            return Transition("stay", message="AMOUNT IS REQUIRED")
        try:
            float(amount)
        except ValueError:
            return Transition("stay", message="AMOUNT IS NOT NUMERIC")
        if auth_type not in ("P", "C", "R"):
            return Transition("stay", message="AUTH TYPE MUST BE P, C OR R")

        self.commarea.card_num = card_num
        self.commarea.acct_id = card["ACCT_ID"]
        self.commarea.cust_id = card["CUST_ID"]
        self.commarea.amount = amount

        # CACRD05 crosses into the other module before the chain continues.
        self._cross_into_risk("RISKSVC")
        return Transition("goto", program="CACRD10")

    def _party_for_card(self, card_num: str) -> str:
        card = self.data.card(card_num) or {}
        customer = self.data.customer(card.get("CUST_ID", "")) or {}
        return customer.get("PARTY_ID", "")

    def _risk_score(self, party_id: str) -> int:
        """PARTYRSK's score for a party, the same wherever it is asked for."""
        return _stable_int(party_id or "UNKNOWN", 120, 890)

    def _exposure(self, party_id: str) -> int:
        return _stable_int((party_id or "UNKNOWN") + "E", 1000, 90000)

    def _cross_into_risk(self, route_key: str) -> None:
        route = self.routes.resolve("XMOD", route_key)
        if route is None:
            return
        self.trace.append(
            Trace(
                seq=len(self.trace) + 1,
                from_program="CACRD05" if route_key == "RISKSVC" else self.program,
                route_type="XMOD",
                route_key=route_key,
                program=route.program,
                call_type=route.call_type,
                module=route.module,
            )
        )

    # -- CACRD10 authorization outcome -------------------------------------

    def _decide(self) -> dict[str, str]:
        """Approve or decline from the sample data, deterministically."""
        card = self.data.card(self.commarea.card_num) or {}
        acct = self.data.account(self.commarea.acct_id) or {}
        credit = self.data.credit_limit(self.commarea.card_num) or {}
        amount = float(self.commarea.amount or 0)

        available = float(credit.get("AVAIL_AMT", 0) or 0)
        score = self._risk_score(self._party_for_card(self.commarea.card_num))
        band = _band(score)
        fraud_score = _stable_int(
            f"{self.commarea.card_num}:{self.commarea.amount}F", 5, 95
        )

        status = card.get("CARD_STATUS", "A")
        if status != "A":
            return {
                "decision": "D", "text": "DECLINED", "resp": "05", "reason": "62",
                "reason_text": f"CARD STATUS {status} - {CARD_STATUS_TEXT.get(status, '')}",
                "score": score, "band": band, "fraud": fraud_score,
                "available": available,
            }
        if amount > available:
            return {
                "decision": "D", "text": "DECLINED", "resp": "05", "reason": "61",
                "reason_text": "AMOUNT EXCEEDS AVAILABLE CREDIT",
                "score": score, "band": band, "fraud": fraud_score,
                "available": available,
            }
        if fraud_score > 85:
            return {
                "decision": "R", "text": "REFERRAL", "resp": "01", "reason": "65",
                "reason_text": "FRAUD SCORE ABOVE REFERRAL THRESHOLD",
                "score": score, "band": band, "fraud": fraud_score,
                "available": available,
            }
        if acct.get("ACCT_STATUS") != "O":
            return {
                "decision": "D", "text": "DECLINED", "resp": "05", "reason": "62",
                "reason_text": "ACCOUNT NOT OPEN",
                "score": score, "band": band, "fraud": fraud_score,
                "available": available,
            }
        if band == "D":
            return {
                "decision": "R", "text": "REFERRAL", "resp": "01", "reason": "63",
                "reason_text": f"RISK BAND {band} - REFER TO PARTY AND RISK",
                "score": score, "band": band, "fraud": fraud_score,
                "available": available,
            }
        return {
            "decision": "A", "text": "APPROVED", "resp": "00", "reason": "  ",
            "reason_text": "", "score": score, "band": band, "fraud": fraud_score,
            "available": available,
        }

    def _fill_cacrd10(self, screen: Screen) -> None:
        outcome = self._decide()
        screen.put("ARCARD", self.commarea.card_num)
        screen.put("ARACCT", self.commarea.acct_id)
        _put_money(screen, "ARAMT", self.commarea.amount)
        screen.put("ARCURR", "USD")
        screen.put("ARDEC", outcome["decision"])
        screen.put("ARDECD", outcome["text"])
        screen.put("ARRESP", outcome["resp"])
        screen.put("ARRSN", str(outcome["reason"]))
        screen.put("ARRSND", outcome["reason_text"])
        screen.put("ARSEQ", str(_stable_int(self.commarea.card_num, 1, 999999)).zfill(6))
        screen.put("ARSCOR", str(outcome["score"]))
        screen.put("ARBAND", outcome["band"])
        screen.put("ARADV", "NONE" if outcome["decision"] == "A" else "ADVS")
        screen.put("ARFSCR", str(outcome["fraud"]))
        # The fraud handlers are themselves a route pipeline; the one that
        # scored highest is the rule the response reports.
        pipeline = self.routes.pipeline("FRAU", "STANDARD")
        screen.put("ARFACT", str(len(pipeline)))
        if pipeline:
            fired = pipeline[_stable_int(self.commarea.card_num + "R", 0, len(pipeline) - 1)]
            screen.put("ARFRUL", fired.program)
        screen.put("ARLTYP", "CRED")
        _put_money(screen, "ARLAVL", str(outcome["available"]))
        credit = self.data.credit_limit(self.commarea.card_num) or {}
        screen.put("ARVEL", credit.get("DAILY_CNT_USED", "0"))

    def _aid_cacrd10(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        return Transition("goto", program="CACRD04")

    # -- CACRD11 card maintenance -------------------------------------------

    def _fill_cacrd11(self, screen: Screen) -> None:
        screen.put("M11CARD", self.commarea.card_num)
        card = self.data.card(self.commarea.card_num)
        if not card:
            return
        screen.put("M11STAT", card["CARD_STATUS"])
        screen.put("M11STXT", CARD_STATUS_TEXT.get(card["CARD_STATUS"], ""))
        screen.put("M11ACCT", card["ACCT_ID"])
        screen.put("M11NAME", card["EMBOSSED_NAME"])
        screen.put("M11PROD", card["PRODUCT_CD"])
        screen.put("M11EXPY", card["EXPIRY_YYMM"])
        screen.put("M11ISSD", _yymmdd(card["ISSUE_DATE"]))
        screen.put("M11RCNT", card["REISSUE_CNT"])
        screen.put("M11PREV", card["PREV_CARD_NUM"])
        screen.put("M11BRSD", BLOCK_REASONS.get(card["BLOCK_REASON"], ""))
        screen.put("M11BDAT", _yymmdd(card["BLOCK_DATE"]))
        screen.put("M11ACTV", card["ACTIVATION_YYMMDD"])

    ALLOWED_ACTIONS = {"B": "BLOCK", "U": "UNBLOCK", "R": "REISSUE", "C": "CLOSE"}

    def _aid_cacrd11(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        card_num = self.screen.get("M11CARD")
        if not card_num:
            return Transition("stay", message="ENTER A CARD NUMBER")
        card = self.data.card(card_num)
        if not card:
            return Transition("stay", message=f"CARD {card_num} NOT ON CARDXREF")
        self.commarea.card_num = card_num

        action = self.screen.get("M11ACTN").upper()
        if not action:
            return Transition("stay")
        if action not in self.ALLOWED_ACTIONS:
            return Transition("stay", message="ACTION MUST BE B, U, R OR C")

        status = card["CARD_STATUS"]
        if action == "B" and status != "A":
            return Transition("stay", message=f"CANNOT BLOCK A CARD IN STATUS {status}")
        if action == "U" and status != "B":
            return Transition("stay", message=f"CANNOT UNBLOCK A CARD IN STATUS {status}")
        if action == "B" and not self.screen.get("M11BRSN"):
            return Transition("stay", message="BLOCK REASON IS REQUIRED")
        if action == "R" and status in ("C", "E"):
            return Transition("stay", message=f"CANNOT REISSUE A CARD IN STATUS {status}")

        self.commarea.action = action
        return Transition("stay", map_name="CRD11B", message="CONFIRM WITH Y, PF12 TO CANCEL")

    NEW_STATUS = {"B": "B", "U": "A", "R": "A", "C": "C"}

    def _fill_crd11b(self, screen: Screen) -> None:
        card = self.data.card(self.commarea.card_num) or {}
        action = self.commarea.action
        screen.put("M11BCRD", self.commarea.card_num)
        screen.put("M11BACT", self.ALLOWED_ACTIONS.get(action, ""))
        screen.put("M11BOST", card.get("CARD_STATUS", ""))
        screen.put("M11BNST", self.NEW_STATUS.get(action, ""))
        if action == "R":
            # A reissue keeps the account and issues the next number in range.
            base = self.commarea.card_num[:-4]
            tail = _stable_int(self.commarea.card_num + "N", 1000, 9999)
            screen.put("M11BNEW", f"{base}{tail}")

    def _aid_crd11b(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        confirm = self.screen.get("M11BCNF").upper()
        if confirm == "Y":
            return Transition(
                "goto",
                program="CACRD11",
                message=f"{self.ALLOWED_ACTIONS.get(self.commarea.action, '')} APPLIED",
            )
        if confirm == "N":
            return Transition("goto", program="CACRD11", message="REQUEST ABANDONED")
        return Transition("stay", message="ENTER Y TO COMMIT OR N TO ABANDON")

    # -- CACRD12 credit limit change ----------------------------------------

    def _fill_cacrd12(self, screen: Screen) -> None:
        screen.put("M12CARD", self.commarea.card_num)
        credit = self.data.credit_limit(self.commarea.card_num)
        if not credit:
            return
        screen.put("M12LTYP", credit["LIMIT_TYPE"])
        _put_money(screen, "M12CLIM", credit["LIMIT_AMT"])
        _put_money(screen, "M12USED", credit["USED_AMT"])
        _put_money(screen, "M12AVAL", credit["AVAIL_AMT"])
        screen.put("M12EFFD", _yymmdd(credit["EFF_DATE"]))
        screen.put("M12BAND", credit["RISK_BAND"])

    def _aid_cacrd12(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        card_num = self.screen.get("M12CARD")
        if not card_num:
            return Transition("stay", message="ENTER A CARD NUMBER")
        credit = self.data.credit_limit(card_num)
        if not credit:
            return Transition("stay", message=f"NO CREDIT LIMIT ROW FOR {card_num}")
        self.commarea.card_num = card_num

        if aid == PF[5]:
            return self._risk_opinion()

        new_limit = self.screen.get("M12NLIM")
        if not new_limit:
            return Transition("stay")
        try:
            requested = float(new_limit)
        except ValueError:
            return Transition("stay", message="NEW LIMIT IS NOT NUMERIC")

        current = float(credit["LIMIT_AMT"])
        if requested <= current * 1.25:
            return Transition("stay", message="APPLIED - NEW CARD_LIMIT ROW INSERTED")
        return Transition(
            "stay", message="INCREASE ABOVE TOLERANCE - PF5 FOR A RISK OPINION"
        )

    def _risk_opinion(self) -> Transition:
        card = self.data.card(self.commarea.card_num) or {}
        customer = self.data.customer(card.get("CUST_ID", "")) or {}
        party_id = customer.get("PARTY_ID", "")
        self._cross_into_risk("KYCINQ")

        kyc = self.data.latest_kyc(party_id) or {}
        score = self._risk_score(party_id)
        band = _band(score)
        self.out["M12SCOR"] = str(score)
        self.out["M12BAND"] = band
        self.out["M12KYC"] = kyc.get("KYC_STATUS", "??")
        self.out["M12EXPO"] = _money(
            str(self._exposure(party_id)), self.screen.width("M12EXPO")
        )
        if band in ("C", "D") or kyc.get("KYC_STATUS") != "OK":
            self.out["M12ADVC"] = "DECLINE"
            self.out["M12DECN"] = "REFUSED - RISK OPINION NEGATIVE"
            return Transition("stay", message="RISK OPINION NEGATIVE - CHANGE REFUSED")
        self.out["M12ADVC"] = "APPROVE"
        self.out["M12DECN"] = "MAY BE APPLIED"
        return Transition("stay", message="RISK OPINION POSITIVE - PRESS ENTER TO APPLY")

    # -- CACRD13 dispute entry ------------------------------------------------

    def _transactions(self, acct_id: str) -> list[dict[str, str]]:
        """Deterministic transactions for an account.

        There is no transaction extract in app/data, so the browse is filled
        from a stable hash of the account id and the merchant file.
        """
        if not acct_id or not self.data.merchants:
            return []
        rows = []
        for i in range(1, 17):
            seed = f"{acct_id}:{i}"
            merchant = self.data.merchants[_stable_int(seed, 0, len(self.data.merchants) - 1)]
            day = _stable_int(seed + "d", 1, 28)
            rows.append(
                {
                    "TXN_ID": f"T{_stable_int(seed + 'x', 100000, 999999)}",
                    "POST_DATE": f"2024-07-{day:02d}",
                    "AMOUNT": f"{_stable_int(seed + 'a', 500, 90000) / 100:.2f}",
                    "MERCHANT": merchant["MERCHANT_NAME"],
                    "MCC": merchant["MCC"],
                    "DISPUTED": "Y" if _stable_int(seed + "p", 1, 10) == 1 else "N",
                }
            )
        return rows

    def _fill_cacrd13(self, screen: Screen) -> None:
        screen.put("M13ACCT", self.commarea.acct_id)
        rows = self._transactions(self.commarea.acct_id)
        page = rows[self.page * 8 : self.page * 8 + 8]
        screen.put("M13PAGE", str(self.page + 1))
        for i, txn in enumerate(page, start=1):
            screen.put(f"M13TID{i}", txn["TXN_ID"])
            screen.put(f"M13DAT{i}", _yymmdd(txn["POST_DATE"]))
            _put_money(screen, f"M13AMT{i}", txn["AMOUNT"])
            screen.put(f"M13MER{i}", txn["MERCHANT"][:20])
            screen.put(f"M13MCC{i}", txn["MCC"])
            screen.put(f"M13DSP{i}", txn["DISPUTED"])

    def _aid_cacrd13(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        self.commarea.acct_id = self.screen.get("M13ACCT")
        rows = self._transactions(self.commarea.acct_id)
        if not rows:
            return Transition("stay", message="ENTER AN ACCOUNT NUMBER")

        if aid == PF[8]:
            if (self.page + 1) * 8 < len(rows):
                self.page += 1
            return Transition("stay")
        if aid == PF[7]:
            self.page = max(0, self.page - 1)
            return Transition("stay")

        for i in range(1, 9):
            if self.screen.get(f"M13SEL{i}").upper() == "S":
                index = self.page * 8 + i - 1
                if index < len(rows):
                    txn = rows[index]
                    if txn["DISPUTED"] == "Y":
                        return Transition(
                            "stay", message=f"TXN {txn['TXN_ID']} IS ALREADY IN DISPUTE"
                        )
                    self.commarea.txn_id = txn["TXN_ID"]
                    self.commarea.amount = txn["AMOUNT"]
                    return Transition("stay", map_name="CRD13B")
        return Transition("stay")

    def _fill_crd13b(self, screen: Screen) -> None:
        rows = {t["TXN_ID"]: t for t in self._transactions(self.commarea.acct_id)}
        txn = rows.get(self.commarea.txn_id, {})
        screen.put("M13BTID", self.commarea.txn_id)
        screen.put("M13BPDT", _yymmdd(txn.get("POST_DATE", "")))
        _put_money(screen, "M13BTAM", txn.get("AMOUNT", "0"))
        screen.put("M13BMER", txn.get("MERCHANT", ""))
        screen.put("M13BWIN", "120")
        screen.put("M13BPRV", "N")
        if not screen.get("M13BDAM").strip():
            screen.put("M13BDAM", txn.get("AMOUNT", ""))

    def _aid_crd13b(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        reason = self.screen.get("M13BRSN").upper().strip()
        if not reason:
            return Transition("stay", message="REASON CODE IS REQUIRED - PF1 FOR THE LIST")
        if reason not in {code for cat, code, *_ in REASON_CODES if cat == "DISP"}:
            return Transition("stay", message=f"REASON {reason} IS NOT A DISPUTE CODE")
        rows = {t["TXN_ID"]: t for t in self._transactions(self.commarea.acct_id)}
        txn = rows.get(self.commarea.txn_id, {})
        try:
            claimed = float(self.screen.get("M13BDAM") or 0)
        except ValueError:
            return Transition("stay", message="DISPUTED AMOUNT IS NOT NUMERIC")
        if claimed <= 0 or claimed > float(txn.get("AMOUNT", 0)):
            return Transition("stay", message="DISPUTED AMOUNT EXCEEDS THE TRANSACTION")
        case = f"D{_stable_int(self.commarea.txn_id + reason, 100000, 999999)}"
        self.out["M13BDID"] = case
        self.out["M13BPRV"] = "Y" if claimed <= 500 else "N"
        return Transition("stay", message=f"DISPUTE {case} RAISED - CHARGEBACK CLOCK STARTED")

    # -- CACRD14 online payment -----------------------------------------------

    def _fill_cacrd14(self, screen: Screen) -> None:
        screen.put("M14ACCT", self.commarea.acct_id)
        screen.put("M14CARD", self.commarea.card_num)
        acct = self.data.account(self.commarea.acct_id)
        if not acct:
            return
        _put_money(screen, "M14CBAL", acct["CURR_BAL"])
        _put_money(screen, "M14MPAY", acct["MIN_PAY_DUE"])
        screen.put("M14DELQ", acct["DELQ_BUCKET"])
        _put_money(screen, "M14DAMT", acct["DELQ_AMT"])
        _put_money(screen, "M14LPAY", acct["LAST_PAY_AMT"])
        screen.put("M14LPDT", _yymmdd(acct["LAST_PAY_DATE"]))

    def _aid_cacrd14(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        acct_id = self.screen.get("M14ACCT")
        acct = self.data.account(acct_id)
        if not acct:
            return Transition("stay", message=f"ACCOUNT {acct_id} NOT FOUND")
        self.commarea.acct_id = acct_id

        amount = self.screen.get("M14PAMT")
        if not amount:
            return Transition("stay")
        try:
            paid = float(amount)
        except ValueError:
            return Transition("stay", message="PAYMENT AMOUNT IS NOT NUMERIC")
        if paid <= 0:
            return Transition("stay", message="PAYMENT MUST BE GREATER THAN ZERO")

        new_balance = float(acct["CURR_BAL"]) - paid
        self.out["M14PTID"] = f"P{_stable_int(acct_id + amount, 100000, 999999)}"
        self.out["M14NBAL"] = _money(f"{new_balance:.2f}", self.screen.width("M14NBAL"))
        return Transition("stay", message="PAYMENT ACCEPTED - GL POSTING QUEUED")

    # -- CACRD15 customer / KYC inquiry ----------------------------------------

    def _fill_cacrd15(self, screen: Screen) -> None:
        screen.put("M15CUST", self.commarea.cust_id)
        screen.put("M15CARD", self.commarea.card_num)
        customer = self.data.customer(self.commarea.cust_id)
        if not customer:
            return
        name = " ".join(
            p for p in (customer["TITLE"], customer["FIRST_NAME"],
                        customer["MIDDLE_INIT"], customer["LAST_NAME"]) if p
        )
        screen.put("M15NAME", name)
        screen.put("M15PTY", customer["PARTY_ID"])
        screen.put("M15AD1", customer["ADDR_LINE1"])
        screen.put("M15CITY", customer["CITY"])
        screen.put("M15STZP", f"{customer['STATE_CD']} {customer['POSTAL_CD']}")
        screen.put("M15CTRY", customer["COUNTRY_CD"])
        screen.put("M15SEG", customer["SEGMENT_CD"])
        screen.put("M15VIP", customer["VIP_FLG"])
        screen.put("M15DOB", _yymmdd(customer["DOB"]))

        # Everything below the line comes back from the other module.
        self._cross_into_risk("KYCINQ")
        party_id = customer["PARTY_ID"]
        kyc = self.data.latest_kyc(party_id) or {}
        score = self._risk_score(party_id)
        screen.put("M15SCOR", str(score))
        screen.put("M15BAND", _band(score))
        screen.put("M15SCDT", _yymmdd(kyc.get("REVIEW_DATE", "")))
        screen.put("M15KYC", kyc.get("KYC_STATUS", ""))
        screen.put("M15KYCT", kyc.get("REVIEW_TYPE", ""))
        hits = self.data.sanctions_for_party(party_id)
        screen.put("M15SANC", "Y" if hits else "N")
        _put_money(screen, "M15EXPO", str(self._exposure(party_id)))
        screen.put("M15MODL", "RSK3")
        if customer["PEP_FLG"] == "Y":
            screen.put("M15WARN", "PEP - ENHANCED DUE DILIGENCE APPLIES")
        elif hits:
            screen.put("M15WARN", "WATCHLIST CANDIDATE - REFER TO COMPLIANCE")

    def _aid_cacrd15(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        cust_id = self.screen.get("M15CUST")
        card_num = self.screen.get("M15CARD")
        if not cust_id and card_num:
            card = self.data.card(card_num)
            if card:
                cust_id = card["CUST_ID"]
        if not cust_id:
            return Transition("stay", message="ENTER A CUSTOMER NUMBER OR A CARD NUMBER")
        if not self.data.customer(cust_id):
            return Transition("stay", message=f"CUSTOMER {cust_id} NOT FOUND")
        self.commarea.cust_id = cust_id
        return Transition("stay")

    # -- CACRD16 reference lookup ------------------------------------------------

    def _fill_cacrd16(self, screen: Screen) -> None:
        wanted = self.ref_filter
        rows = [r for r in REASON_CODES if not wanted or r[0] == wanted]
        page = rows[self.page * 10 : self.page * 10 + 10]
        screen.put("M16CAT", wanted)
        screen.put("M16PAGE", str(self.page + 1))
        suffixes = "123456789A"
        for i, row in enumerate(page):
            s = suffixes[i]
            cat, code, desc, actn, prv, days, network = row
            screen.put(f"M16CT{s}", cat)
            screen.put(f"M16CD{s}", code)
            screen.put(f"M16DS{s}", desc)
            screen.put(f"M16AC{s}", actn)
            screen.put(f"M16PV{s}", prv)
            screen.put(f"M16DY{s}", days)
            screen.put(f"M16NW{s}", network)

    def _aid_cacrd16(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        if aid == PF[5]:
            return Transition("stay", map_name="CRD16B")
        self.ref_filter = self.screen.get("M16CAT").upper()
        rows = [r for r in REASON_CODES if not self.ref_filter or r[0] == self.ref_filter]
        if not rows:
            return Transition("stay", message=f"NO CODES IN CATEGORY {self.ref_filter}")
        if aid == PF[8]:
            if (self.page + 1) * 10 < len(rows):
                self.page += 1
            return Transition("stay")
        if aid == PF[7]:
            self.page = max(0, self.page - 1)
            return Transition("stay")
        return Transition("stay")

    def _fill_crd16b(self, screen: Screen) -> None:
        merchants = self.data.merchants
        page = merchants[self.page * 8 : self.page * 8 + 8]
        screen.put("M16BPGE", str(self.page + 1))
        for i, m in enumerate(page, start=1):
            screen.put(f"M16BI{i}", m["MERCHANT_ID"])
            screen.put(f"M16BN{i}", m["MERCHANT_NAME"][:24])
            screen.put(f"M16BC{i}", m["MCC"])
            screen.put(f"M16BY{i}", m["COUNTRY_CD"])
            screen.put(f"M16BR{i}", m["CHARGEBACK_RATE"])
            screen.put(f"M16BH{i}", m["HIGH_RISK_FLG"])
            screen.put(f"M16BS{i}", m["STATUS"])

    def _aid_crd16b(self, aid: str) -> Transition:
        if aid in (PF[3], PF[12]):
            return self._default_aid(aid)
        if aid == PF[5]:
            return Transition("stay", map_name="CRD16A")
        if aid == PF[8]:
            if (self.page + 1) * 8 < len(self.data.merchants):
                self.page += 1
            return Transition("stay")
        if aid == PF[7]:
            self.page = max(0, self.page - 1)
            return Transition("stay")
        return Transition("stay")

    # -- CACRD17 exit / CACRD91 error -------------------------------------------

    def _fill_cacrd17(self, screen: Screen) -> None:
        screen.put("BYOPER", self.operator)
        screen.put("BYTERM", self.termid)
        screen.put("BYCNT", str(self.screen_count))
        screen.put("BYMSG", "SESSION ENDED - CA00 AUDIT ROW WRITTEN")

    def _aid_cacrd17(self, aid: str) -> Transition:
        return Transition("exit")

    def _fill_cacrd91(self, screen: Screen) -> None:
        screen.put("ERPGM", self.commarea.from_program or self.program)
        screen.put("ERSEV", "E")
        screen.put("ERTYPE", "PGMIDERR")
        screen.put("ERRSN", "ROUTE TARGET NOT IN THE LOAD LIBRARY")
        screen.put("ERRESP", "27")
        screen.put("ERRES2", "3")
        screen.put("ERINST", "REPORT TO THE ON CALL AND RETURN TO THE MENU WITH PF3")

    def _aid_cacrd91(self, aid: str) -> Transition:
        return Transition("goto", program=MENU_PROGRAM)
