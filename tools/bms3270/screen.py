"""A 3270 display buffer built from a parsed BMS map.

BMS POS=(row,col) gives the position of the field *attribute byte*; the field
data starts in the following position.  That one-column shift is reproduced
here, because it is visible on a real terminal and shifts every field right
by one relative to a naive reading of the map source.
"""

from __future__ import annotations

from dataclasses import dataclass, replace

from .bmsparse import Field, Map

# Attribute byte positions display as a blank on a 3270.
ATTR_CHAR = " "


@dataclass
class Cell:
    char: str = " "
    color: str = "GREEN"
    bright: bool = False
    dark: bool = False
    reverse: bool = False
    underline: bool = False
    field: str | None = None
    editable: bool = False


@dataclass
class InputField:
    """An unprotected field, with the buffer coordinates of its data area."""

    name: str
    row: int
    col: int
    length: int
    numeric: bool
    dark: bool

    def clamp(self, value: str) -> str:
        return value[: self.length]


def _default_color(f: Field) -> str:
    if f.color:
        return f.color
    if f.is_input:
        return "GREEN"
    return "WHITE" if f.is_bright else "GREEN"


class Screen:
    """A rows x cols character buffer with per-cell attributes."""

    def __init__(self, rows: int = 24, cols: int = 80):
        self.rows = rows
        self.cols = cols
        self.cells = [[Cell() for _ in range(cols)] for _ in range(rows)]
        self.inputs: list[InputField] = []
        self.cursor: tuple[int, int] = (0, 0)

    # -- construction ---------------------------------------------------

    @classmethod
    def from_map(cls, m: Map) -> "Screen":
        s = cls(m.rows, m.cols)
        for f in m.fields:
            if f.occurs > 1:
                # A DFHMDF with OCCURS builds a table the COBOL indexes as
                # FIELDO(n).  In this estate every such table runs down the
                # screen, one occurrence per row, so occurrence n sits at
                # POS row + n - 1 in the same column.  Each occurrence is
                # given a 1-based suffix so it can be addressed individually.
                for n in range(f.occurs):
                    s._place(
                        replace(
                            f,
                            row=f.row + n,
                            name=f"{f.name}{n + 1}" if f.name else None,
                            occurs=1,
                        )
                    )
            else:
                s._place(f)
        if not s.cursor_set and s.inputs:
            first = s.inputs[0]
            s.cursor = (first.row, first.col)
        return s

    cursor_set = False

    def _place(self, f: Field) -> None:
        # Attribute byte sits at POS; data begins one column later.
        attr_r, attr_c = f.row - 1, f.col - 1
        if not (0 <= attr_r < self.rows and 0 <= attr_c < self.cols):
            return
        self.cells[attr_r][attr_c] = Cell(char=ATTR_CHAR)

        data_r, data_c = attr_r, attr_c + 1
        color = _default_color(f)
        text = f.initial.ljust(f.length)[: f.length]

        for i in range(f.length):
            c = data_c + i
            r = data_r
            # 3270 fields wrap onto the next line at the screen edge.
            while c >= self.cols:
                c -= self.cols
                r += 1
            if r >= self.rows:
                break
            self.cells[r][c] = Cell(
                char=text[i] if not f.is_dark else " ",
                color=color,
                bright=f.is_bright,
                dark=f.is_dark,
                reverse=(f.hilight == "REVERSE"),
                underline=(f.hilight == "UNDERLINE") or f.is_input,
                field=f.name,
                editable=f.is_input,
            )

        if f.is_input and f.name:
            self.inputs.append(
                InputField(
                    name=f.name,
                    row=data_r,
                    col=data_c,
                    length=f.length,
                    numeric=f.is_numeric,
                    dark=f.is_dark,
                )
            )
            if f.has_cursor:
                self.cursor = (data_r, data_c)
                self.cursor_set = True

    # -- field access ---------------------------------------------------

    def field(self, name: str) -> InputField | None:
        for f in self.inputs:
            if f.name == name:
                return f
        return None

    def width(self, name: str) -> int:
        """Declared length of a field, protected or not."""
        target = self.field(name)
        if target is not None:
            return target.length
        return sum(
            1
            for row in self.cells
            for cell in row
            if cell.field == name
        )

    def put(self, name: str, value: str) -> None:
        """Write a value into a named field, input or output."""
        target = self.field(name)
        if target is not None:
            self._write(target.row, target.col, value.ljust(target.length)[: target.length], name)
            return
        # Output-only field: locate it by the field tag stamped on its cells.
        cells = [
            (r, c)
            for r in range(self.rows)
            for c in range(self.cols)
            if self.cells[r][c].field == name
        ]
        if not cells:
            return
        cells.sort()
        text = value.ljust(len(cells))[: len(cells)]
        for (r, c), ch in zip(cells, text):
            self.cells[r][c].char = ch
            self.cells[r][c].dark = False

    def get(self, name: str) -> str:
        target = self.field(name)
        if target is None:
            return ""
        return "".join(
            self.cells[target.row][target.col + i].char for i in range(target.length)
        ).strip()

    def _write(self, row: int, col: int, text: str, name: str | None = None) -> None:
        for i, ch in enumerate(text):
            c = col + i
            if c >= self.cols:
                break
            cell = self.cells[row][c]
            cell.char = ch
            if name is not None:
                cell.field = name

    def clear_inputs(self) -> None:
        for f in self.inputs:
            self._write(f.row, f.col, " " * f.length, f.name)

    # -- rendering ------------------------------------------------------

    def to_text(self) -> str:
        """Plain text, no attributes - useful for diffing and snapshots."""
        return "\n".join(
            "".join(cell.char for cell in row).rstrip() for row in self.cells
        )

    def to_framed_text(self) -> str:
        top = "\u2554" + "\u2550" * self.cols + "\u2557"
        bottom = "\u255a" + "\u2550" * self.cols + "\u255d"
        body = [
            "\u2551" + "".join(cell.char for cell in row).ljust(self.cols) + "\u2551"
            for row in self.cells
        ]
        return "\n".join([top, *body, bottom])
