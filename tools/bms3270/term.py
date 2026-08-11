"""A 3270-style terminal for transaction CA00, driven from a shell.

Keys follow c3270 conventions: Tab / Back-Tab move between unprotected
fields, Home goes to the first one, Enter is the ENTER AID, F1-F12 are PF1
to PF12, and Esc leaves the emulator.
"""

from __future__ import annotations

import curses
from datetime import date

from .app import ENTER, Session
from .screen import Screen

COLOR_IDS = {
    "GREEN": 1,
    "TURQUOISE": 2,
    "BLUE": 3,
    "RED": 4,
    "PINK": 5,
    "YELLOW": 6,
    "WHITE": 7,
    "NEUTRAL": 7,
}
CURSES_COLORS = {
    "GREEN": curses.COLOR_GREEN,
    "TURQUOISE": curses.COLOR_CYAN,
    "BLUE": curses.COLOR_BLUE,
    "RED": curses.COLOR_RED,
    "PINK": curses.COLOR_MAGENTA,
    "YELLOW": curses.COLOR_YELLOW,
    "WHITE": curses.COLOR_WHITE,
}

FKEYS = {getattr(curses, f"KEY_F{n}"): f"PF{n}" for n in range(1, 13)}


class Terminal:
    def __init__(self, stdscr, session: Session, trace: bool = False):
        self.stdscr = stdscr
        self.session = session
        self.show_trace = trace
        self.status = "READY"
        self.field_index = 0
        self.offset = 0

    # -- geometry --------------------------------------------------------

    @property
    def screen(self) -> Screen:
        return self.session.screen

    def _sync_field(self) -> None:
        """Snap the emulator cursor onto the current input field."""
        inputs = self.screen.inputs
        if not inputs:
            self.field_index = 0
            self.offset = 0
            return
        self.field_index %= len(inputs)
        f = inputs[self.field_index]
        self.offset = max(0, min(self.offset, f.length - 1))
        self.screen.cursor = (f.row, f.col + self.offset)

    def _from_screen_cursor(self) -> None:
        """Pick the field the map asked for with IC."""
        row, col = self.screen.cursor
        for i, f in enumerate(self.screen.inputs):
            if f.row == row and f.col <= col < f.col + f.length:
                self.field_index = i
                self.offset = col - f.col
                return
        self.field_index = 0
        self.offset = 0

    # -- rendering -------------------------------------------------------

    def draw(self) -> None:
        self.stdscr.erase()
        height, width = self.stdscr.getmaxyx()
        if height < 26 or width < 84:
            self.stdscr.addstr(0, 0, "TERMINAL TOO SMALL - NEED AT LEAST 84x26")
            self.stdscr.refresh()
            return

        top = 1
        left = 1
        self.stdscr.attron(curses.color_pair(COLOR_IDS["BLUE"]))
        self.stdscr.addstr(top - 1, left - 1, "+" + "-" * 80 + "+")
        self.stdscr.addstr(top + 24, left - 1, "+" + "-" * 80 + "+")
        for r in range(24):
            self.stdscr.addstr(top + r, left - 1, "|")
            self.stdscr.addstr(top + r, left + 80, "|")
        self.stdscr.attroff(curses.color_pair(COLOR_IDS["BLUE"]))

        for r, row in enumerate(self.screen.cells):
            for c, cell in enumerate(row):
                ch = " " if cell.dark and cell.char != " " else cell.char
                attr = curses.color_pair(COLOR_IDS.get(cell.color, 1))
                if cell.bright:
                    attr |= curses.A_BOLD
                if cell.reverse:
                    attr |= curses.A_REVERSE
                if cell.underline or cell.editable:
                    attr |= curses.A_UNDERLINE
                self.stdscr.addstr(top + r, left + c, ch, attr)

        self._draw_status(top + 25, left - 1)
        if self.show_trace:
            self._draw_trace(top, left + 83)

        cur_r, cur_c = self.screen.cursor
        self.stdscr.move(top + cur_r, left + cur_c)
        self.stdscr.refresh()

    def _draw_status(self, row: int, col: int) -> None:
        s = self.session
        cur_r, cur_c = self.screen.cursor
        mapset = s.mapset_for(s.map_name)
        left = f"4 A {s.program:<8} {mapset}({s.map_name})"
        right = f"{cur_r + 1:03d}/{cur_c + 1:03d}"
        middle = self.status
        line = f"{left}  {middle}".ljust(74)[:74] + right
        self.stdscr.addstr(row, col, line[:81], curses.color_pair(COLOR_IDS["BLUE"]))

    def _draw_trace(self, top: int, col: int) -> None:
        height, width = self.stdscr.getmaxyx()
        if width < col + 40:
            return
        self.stdscr.addstr(top, col, "PGM_ROUTE DISPATCHES", curses.A_BOLD)
        for i, t in enumerate(self.session.trace[-20:]):
            if top + 2 + i >= height:
                break
            self.stdscr.addstr(top + 2 + i, col, t.line()[: width - col - 1])

    # -- editing ---------------------------------------------------------

    def _type(self, ch: str) -> None:
        inputs = self.screen.inputs
        if not inputs:
            self.status = "X PROTECTED"
            return
        f = inputs[self.field_index]
        if f.numeric and not (ch.isdigit() or ch in "+-. "):
            self.status = "X NUM"
            return
        value = self.screen.get(f.name).ljust(f.length)
        value = value[: self.offset] + ch + value[self.offset + 1 :]
        self.screen.put(f.name, value[: f.length])
        if self.offset + 1 < f.length:
            self.offset += 1
        else:
            self.field_index = (self.field_index + 1) % len(inputs)
            self.offset = 0
        self.status = "READY"

    def _backspace(self) -> None:
        inputs = self.screen.inputs
        if not inputs:
            return
        f = inputs[self.field_index]
        if self.offset == 0:
            return
        self.offset -= 1
        value = self.screen.get(f.name).ljust(f.length)
        value = value[: self.offset] + " " + value[self.offset + 1 :]
        self.screen.put(f.name, value)

    def _erase_eof(self) -> None:
        inputs = self.screen.inputs
        if not inputs:
            return
        f = inputs[self.field_index]
        value = self.screen.get(f.name).ljust(f.length)
        self.screen.put(f.name, value[: self.offset])

    def _clear(self) -> None:
        self.screen.clear_inputs()
        self.field_index = 0
        self.offset = 0

    # -- main loop --------------------------------------------------------

    def run(self) -> None:
        self._from_screen_cursor()
        while not self.session.finished:
            self._sync_field()
            self.draw()
            key = self.stdscr.getch()

            if key == 27:  # Esc
                return
            if key in FKEYS:
                self._aid(FKEYS[key])
                continue
            if key in (curses.KEY_ENTER, 10, 13):
                self._aid(ENTER)
                continue
            if key == curses.KEY_BTAB:
                self.field_index -= 1
                self.offset = 0
            elif key == 9:  # Tab
                self.field_index += 1
                self.offset = 0
            elif key == curses.KEY_HOME:
                self.field_index = 0
                self.offset = 0
            elif key == curses.KEY_LEFT:
                self.offset -= 1
                if self.offset < 0:
                    self.field_index -= 1
                    inputs = self.screen.inputs
                    self.offset = (
                        inputs[self.field_index % len(inputs)].length - 1 if inputs else 0
                    )
            elif key == curses.KEY_RIGHT:
                self.offset += 1
            elif key in (curses.KEY_DOWN,):
                self.field_index += 1
                self.offset = 0
            elif key in (curses.KEY_UP,):
                self.field_index -= 1
                self.offset = 0
            elif key in (curses.KEY_BACKSPACE, 127, 8):
                self._backspace()
            elif key == curses.KEY_DC:
                self._erase_eof()
            elif key == 12:  # Ctrl-L, the CLEAR key
                self._clear()
                self.status = "CLEARED"
            elif key == 20:  # Ctrl-T
                self.show_trace = not self.show_trace
            elif 32 <= key < 127:
                self._type(chr(key))

        self.draw()
        self.stdscr.addstr(0, 0, " SESSION ENDED - PRESS ANY KEY ", curses.A_REVERSE)
        self.stdscr.getch()

    def _aid(self, aid: str) -> None:
        self.session.submit(aid)
        self.status = self.session.message or f"{aid}"
        self.field_index = 0
        self.offset = 0
        self._from_screen_cursor()


def _init_colors() -> None:
    curses.start_color()
    curses.use_default_colors()
    for name, ident in COLOR_IDS.items():
        curses.init_pair(ident, CURSES_COLORS.get(name, curses.COLOR_GREEN), -1)


def run(operator: str = "CSROP01", termid: str = "T001", trace: bool = False,
        as_of: date | None = None) -> None:
    session = Session(operator=operator, termid=termid, as_of=as_of)

    def _main(stdscr):
        curses.curs_set(1)
        _init_colors()
        stdscr.keypad(True)
        Terminal(stdscr, session, trace=trace).run()

    curses.wrapper(_main)
