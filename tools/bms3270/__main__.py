"""Command line entry point.

    python3 -m tools.bms3270              interactive terminal
    python3 -m tools.bms3270 maps         list the mapsets and maps
    python3 -m tools.bms3270 show CARDMNU render one map and exit
    python3 -m tools.bms3270 play FILE    replay a keystroke script
    python3 -m tools.bms3270 routes       show the PGM_ROUTE table as resolved
"""

from __future__ import annotations

import argparse
import sys
from datetime import date

from .app import Session
from .bmsparse import parse_all
from .estate import BMS_DIR, RouteTable, load_routes
from .screen import Screen


def cmd_maps(args: argparse.Namespace) -> int:
    for name, mapset in parse_all(BMS_DIR).items():
        print(f"{name}")
        for map_name, m in mapset.maps.items():
            inputs = len([f for f in m.fields if f.is_input])
            print(f"  {map_name:<8} {m.rows}x{m.cols}  {len(m.fields):>3} fields  "
                  f"{inputs:>2} unprotected")
    return 0


def cmd_show(args: argparse.Namespace) -> int:
    mapsets = parse_all(BMS_DIR)
    for mapset in mapsets.values():
        if args.map in mapset.maps:
            print(Screen.from_map(mapset.maps[args.map]).to_framed_text())
            return 0
    print(f"no such map: {args.map}", file=sys.stderr)
    return 1


def cmd_routes(args: argparse.Namespace) -> int:
    table = RouteTable(load_routes(), as_of=date.fromisoformat(args.as_of) if args.as_of else None)
    print(f"as of {table.as_of}\n")
    print(f"{'TYPE':<5} {'KEY':<9} {'SEQ':>3} {'PROGRAM':<8} {'CT':<2} {'MODULE':<8} ACTIVE")
    for r in table.routes:
        live = "Y" if r.in_effect(table.as_of) else "n"
        print(f"{r.route_type:<5} {r.route_key:<9} {r.seq:>3} {r.program:<8} "
              f"{r.call_type:<2} {r.module:<8} {live}")
    return 0


def cmd_play(args: argparse.Namespace) -> int:
    """Replay a keystroke script, printing every screen it produces.

    Each line of the script is either literal text to type into the field
    under the cursor, ``@FIELD=value`` to set a named field, or an AID key
    such as ``ENTER``, ``PF3`` or ``PF12``.  Blank lines and ``#`` comments
    are ignored.
    """
    session = Session(
        operator=args.operator,
        termid=args.termid,
        as_of=date.fromisoformat(args.as_of) if args.as_of else None,
    )
    script = sys.stdin.read() if args.script == "-" else open(args.script).read()

    def emit(label: str) -> None:
        print(f"\n--- {label}  {session.program} / {session.map_name} "
              f"{'-' * max(0, 40 - len(label))}")
        print(session.screen.to_framed_text())

    emit("initial")
    for raw in script.splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if line.startswith("@"):
            name, _, value = line[1:].partition("=")
            session.screen.put(name.strip(), value)
            continue
        session.submit(line.upper())
        emit(line.upper())
        if session.finished:
            break

    if session.trace:
        print("\nPGM_ROUTE dispatches")
        for t in session.trace:
            print("  " + t.line())
    return 0


def cmd_run(args: argparse.Namespace) -> int:
    from .term import run

    run(
        operator=args.operator,
        termid=args.termid,
        trace=args.trace,
        as_of=date.fromisoformat(args.as_of) if args.as_of else None,
    )
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="tools.bms3270", description=__doc__)
    parser.add_argument("--operator", default="CSROP01")
    parser.add_argument("--termid", default="T001")
    parser.add_argument(
        "--as-of",
        help="resolve PGM_ROUTE effective dates as of YYYY-MM-DD instead of today",
    )
    parser.set_defaults(func=cmd_run, trace=False)

    sub = parser.add_subparsers()

    p_run = sub.add_parser("run", help="interactive terminal (default)")
    p_run.add_argument("--trace", action="store_true", help="show dispatches beside the screen")
    p_run.set_defaults(func=cmd_run)

    sub.add_parser("maps", help="list mapsets and maps").set_defaults(func=cmd_maps)

    p_show = sub.add_parser("show", help="render one map")
    p_show.add_argument("map")
    p_show.set_defaults(func=cmd_show)

    sub.add_parser(
        "routes", help="show PGM_ROUTE as the dispatcher sees it"
    ).set_defaults(func=cmd_routes)

    p_play = sub.add_parser("play", help="replay a keystroke script")
    p_play.add_argument("script", help="script file, or - for stdin")
    p_play.set_defaults(func=cmd_play)

    args = parser.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
