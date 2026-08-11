"""Parser for BMS (Basic Mapping Support) mapset source.

Reads the DFHMSD/DFHMDI/DFHMDF macro assembler source in app/cardsvc/bms and
returns the mapsets, maps and fields it describes.  Only the subset of the
macro operands the estate actually uses is understood.
"""

from __future__ import annotations

from dataclasses import dataclass, field as dc_field
from pathlib import Path

# BMS source is assembler fixed format: label 1-8, operation 10-14,
# operands from 16, continuation indicator in column 72.
LABEL_COLS = slice(0, 8)
OPERAND_START = 15
CONTINUATION_COL = 71


@dataclass
class Field:
    """One DFHMDF - a single field on a map."""

    name: str | None
    row: int
    col: int
    length: int
    attrs: frozenset[str]
    initial: str = ""
    color: str | None = None
    hilight: str | None = None
    picin: str | None = None
    picout: str | None = None
    justify: str | None = None
    occurs: int = 1

    @property
    def is_input(self) -> bool:
        return "UNPROT" in self.attrs

    @property
    def is_numeric(self) -> bool:
        return "NUM" in self.attrs

    @property
    def is_dark(self) -> bool:
        return "DRK" in self.attrs

    @property
    def is_bright(self) -> bool:
        return "BRT" in self.attrs

    @property
    def has_cursor(self) -> bool:
        return "IC" in self.attrs


@dataclass
class Map:
    """One DFHMDI - a screen."""

    name: str
    rows: int = 24
    cols: int = 80
    fields: list[Field] = dc_field(default_factory=list)

    def named(self, name: str) -> Field | None:
        for f in self.fields:
            if f.name == name:
                return f
        return None

    @property
    def input_fields(self) -> list[Field]:
        return [f for f in self.fields if f.is_input]


@dataclass
class Mapset:
    """One DFHMSD - a set of maps assembled into one load module."""

    name: str
    maps: dict[str, Map] = dc_field(default_factory=dict)


class BmsSyntaxError(Exception):
    pass


def _logical_lines(text: str) -> list[tuple[str, str, str]]:
    """Join continuations into (label, operation, operands) triples.

    A non-blank character in column 72 continues the statement on the next
    line, whose operands restart in column 16.
    """
    out: list[tuple[str, str, str]] = []
    pending: list[str] = []
    label = operation = ""

    for raw in text.splitlines():
        line = raw.rstrip("\n")
        if line.startswith("*") or not line.strip():
            continue

        continues = len(line) > CONTINUATION_COL and line[CONTINUATION_COL] != " "
        # Blanks before the continuation column are part of the operand: a
        # literal may legitimately end in spaces.
        body = (
            line[OPERAND_START:CONTINUATION_COL].ljust(CONTINUATION_COL - OPERAND_START)
            if continues
            else line[OPERAND_START:].rstrip()
        )

        if pending:
            pending.append(body)
        else:
            label = line[LABEL_COLS].strip()
            rest = line[8:OPERAND_START].strip()
            operation = rest.split()[0] if rest else ""
            pending = [body]

        if not continues:
            out.append((label, operation, "".join(pending)))
            pending = []
            label = operation = ""

    if pending:
        out.append((label, operation, "".join(pending)))
    return out


def _split_operands(operands: str) -> list[str]:
    """Split on commas that are not inside parentheses or quotes."""
    parts: list[str] = []
    depth = 0
    quoted = False
    current: list[str] = []

    i = 0
    while i < len(operands):
        ch = operands[i]
        if quoted:
            current.append(ch)
            if ch == "'":
                # A doubled quote is an escaped literal quote.
                if i + 1 < len(operands) and operands[i + 1] == "'":
                    current.append("'")
                    i += 1
                else:
                    quoted = False
        elif ch == "'":
            quoted = True
            current.append(ch)
        elif ch == "(":
            depth += 1
            current.append(ch)
        elif ch == ")":
            depth -= 1
            current.append(ch)
        elif ch == "," and depth == 0:
            parts.append("".join(current).strip())
            current = []
        else:
            current.append(ch)
        i += 1

    if current:
        parts.append("".join(current).strip())
    return [p for p in parts if p]


def _parse_keywords(operands: str) -> dict[str, str]:
    kw: dict[str, str] = {}
    for part in _split_operands(operands):
        if "=" not in part:
            kw[part] = ""
            continue
        key, _, value = part.partition("=")
        kw[key.strip()] = value.strip()
    return kw


def _unquote(value: str) -> str:
    if value.startswith("'") and value.endswith("'") and len(value) >= 2:
        return value[1:-1].replace("''", "'")
    return value


def _paren_list(value: str) -> list[str]:
    if value.startswith("(") and value.endswith(")"):
        value = value[1:-1]
    return [v.strip() for v in value.split(",") if v.strip()]


COLOR_WORDS = {
    "BLUE", "RED", "PINK", "GREEN", "TURQUOISE", "YELLOW", "NEUTRAL", "WHITE",
}
HILIGHT_WORDS = {"BLINK", "REVERSE", "UNDERLINE", "OFF"}


def _parse_field(name: str, operands: str) -> Field:
    kw = _parse_keywords(operands)

    if "POS" not in kw:
        raise BmsSyntaxError(f"DFHMDF {name or '(unnamed)'} has no POS")
    pos = _paren_list(kw["POS"])
    if len(pos) == 1:
        # POS=n is a linear displacement; the estate does not use it.
        raise BmsSyntaxError(f"DFHMDF {name}: linear POS is not supported")
    row, col = int(pos[0]), int(pos[1])

    attrs = set()
    color = None
    hilight = None
    for word in _paren_list(kw.get("ATTRB", "")):
        if word in COLOR_WORDS:
            color = word
        elif word in HILIGHT_WORDS:
            hilight = word
        else:
            attrs.add(word)

    if "COLOR" in kw:
        color = kw["COLOR"].strip()
    if "HILIGHT" in kw:
        hilight = kw["HILIGHT"].strip()

    # ASKIP and PROT are both output-only; UNPROT is the only input attribute.
    if "UNPROT" not in attrs:
        attrs.add("PROT")

    return Field(
        name=name or None,
        row=row,
        col=col,
        length=int(kw.get("LENGTH", "0")),
        attrs=frozenset(attrs),
        initial=_unquote(kw.get("INITIAL", "")),
        color=color,
        hilight=hilight,
        picin=_unquote(kw["PICIN"]) if "PICIN" in kw else None,
        picout=_unquote(kw["PICOUT"]) if "PICOUT" in kw else None,
        justify=kw.get("JUSTIFY"),
        occurs=int(kw.get("OCCURS", "1")),
    )


def parse_mapset(path: str | Path) -> Mapset:
    """Parse one BMS source member."""
    text = Path(path).read_text(errors="replace")

    mapset: Mapset | None = None
    current: Map | None = None

    for label, operation, operands in _logical_lines(text):
        if operation == "DFHMSD":
            if _parse_keywords(operands).get("TYPE", "").upper() == "FINAL":
                continue
            mapset = Mapset(name=label)
        elif operation == "DFHMDI":
            if mapset is None:
                raise BmsSyntaxError(f"DFHMDI {label} before any DFHMSD")
            kw = _parse_keywords(operands)
            size = _paren_list(kw.get("SIZE", "(24,80)"))
            current = Map(name=label, rows=int(size[0]), cols=int(size[1]))
            mapset.maps[label] = current
        elif operation == "DFHMDF":
            if current is None:
                raise BmsSyntaxError(f"DFHMDF {label} before any DFHMDI")
            current.fields.append(_parse_field(label, operands))
        elif operation == "DFHMSD" or operation == "END":
            continue

    if mapset is None:
        raise BmsSyntaxError(f"{path}: no DFHMSD found")
    return mapset


def parse_all(bms_dir: str | Path) -> dict[str, Mapset]:
    """Parse every .bms member in a directory, keyed by mapset name."""
    sets: dict[str, Mapset] = {}
    for path in sorted(Path(bms_dir).glob("*.bms")):
        ms = parse_mapset(path)
        sets[ms.name] = ms
    return sets
