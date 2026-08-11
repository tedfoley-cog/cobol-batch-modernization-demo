# bms3270 - a terminal for transaction CA00

Renders the CARDSVC BMS maps as 24x80 green screens and lets you drive them
from a shell.

```
python3 -m tools.bms3270                # interactive terminal
python3 -m tools.bms3270 run --trace    # with the dispatch log beside the screen
python3 -m tools.bms3270 maps           # list the mapsets and maps
python3 -m tools.bms3270 show CARDMNU   # render one map and exit
python3 -m tools.bms3270 routes         # PGM_ROUTE as the dispatcher resolves it
python3 -m tools.bms3270 play tools/bms3270/demo.keys
```

Python 3.10 or later, standard library only. The window needs at least 84x26,
or 130x26 with `--trace`.

## Keys

| Key | Meaning |
| --- | --- |
| Tab / Shift-Tab | next / previous unprotected field |
| Home | first unprotected field |
| Arrows | move within and between fields |
| Backspace | erase the character to the left |
| Delete | erase to the end of the field |
| Enter | the ENTER AID |
| F1-F12 | PF1 to PF12 |
| Ctrl-L | CLEAR |
| Ctrl-T | toggle the dispatch log |
| Esc | leave the emulator |

`PF3` returns to the menu and `PF12` ends the session everywhere. Numeric
fields reject non-numeric input the way a 3270 does, and the status line
shows the current program, mapset, map and cursor position.

## What is and is not real

Real, because it is read out of the repository rather than restated here:

* every screen layout, field position, length, colour and attribute comes
  from `app/cardsvc/bms/*.bms`;
* the menu option to program mapping, and the two cross-module crossings,
  are resolved from `db2/ddl/40_SEED_PGM_ROUTE.sql` at run time, honouring
  `ACTIVE_FLG` and the effective dates - `--as-of` moves the clock, which
  changes which programs the menu reaches;
* which map each program sends is read from the `WS-MAP...` literals in
  `app/cardsvc/cbl/*.cbl`;
* account, card, limit, customer, KYC and merchant values come from
  `app/data/*.csv`.

Not real, and deliberately so:

* **the COBOL is not executed.** There is no CICS, no DB2, no VSAM. The
  screen flow, the validation messages and the authorization decision are
  written in `app.py`, and they are a plausible reading of the maps, not the
  behaviour of the programs.
* transactions for the dispute browse are generated from a hash of the
  account number, because the estate ships no transaction extract;
* the reason code reference is a table in `app.py`, because `RSNCODE` is
  defined as a VSAM cluster but carries no data;
* a `DFHMDF` with `OCCURS` is laid out down the screen, one occurrence per
  row, which is how `CACRD02` and `CACRD03` index those tables. Occurrence
  *n* is addressable as `FIELDn`, so `CLCARD1` through `CLCARD10`.

## Replay scripts

`play` takes a keystroke script. `@FIELD=value` sets a named field, a bare
word is an AID key, `#` starts a comment. Every resulting screen is printed,
followed by the dispatches the session made. `demo.keys` walks all seventeen
maps.
