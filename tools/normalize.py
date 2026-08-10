"""Deterministic normalization primitives for mainframe parity comparisons."""
from __future__ import annotations

from decimal import ROUND_DOWN, ROUND_HALF_UP, Decimal
from typing import Any


def unpack_comp3(data: bytes, digits: int, scale: int = 0) -> Decimal:
    """Decode packed decimal bytes (last nibble is C/F positive, D negative)."""
    nibbles = "".join(f"{byte:02x}" for byte in data)
    sign_nibble, body = nibbles[-1], nibbles[:-1]
    sign = -1 if sign_nibble.lower() == "d" else 1
    value = int(body)
    return sign * Decimal(value).scaleb(-scale)


def decode_zoned(data: bytes, scale: int = 0) -> Decimal:
    """Decode EBCDIC/zoned digits with trailing sign overpunch."""
    text = data.decode("cp037")
    last = text[-1]
    signs = {"{": 1, "A": 1, "B": 1, "C": 1, "D": 1, "E": 1, "F": 1, "}": -1, "J": -1, "K": -1, "L": -1, "M": -1, "N": -1, "O": -1, "P": -1}
    if last in signs:
        digit = "0" if last in "{}" else str((ord(last) - ord("A")) % 10)
        text = text[:-1] + digit
        return signs[last] * Decimal(text).scaleb(-scale)
    return Decimal(text).scaleb(-scale)


def split_fixed_width(record: str | bytes, layout: list[dict[str, Any]]) -> dict[str, str]:
    """Split a fixed-width record using an explicit, customer-owned layout spec."""
    if isinstance(record, bytes):
        record = record.decode("cp037")
    return {field["name"]: record[field["start"] - 1: field["end"]] for field in layout}


def decode_ebcdic(data: bytes) -> str:
    return data.decode("cp037")


def align_scale(value: Decimal | int | str, from_scale: int, to_scale: int,
                rounding: str = "HALF_UP") -> Decimal:
    target = Decimal(value)
    if isinstance(value, int):
        target = target.scaleb(-from_scale)
    quant = Decimal(1).scaleb(-to_scale)
    mode = ROUND_HALF_UP if rounding == "HALF_UP" else ROUND_DOWN
    return target.quantize(quant, rounding=mode)


def normalize_date(value: str) -> str:
    """Normalize YYYYMMDD, YYMMDD, and YYYY-MM-DD to ISO YYYY-MM-DD."""
    digits = value.replace("-", "")
    if len(digits) == 6:
        year = int(digits[:2])
        year += 2000 if year < 50 else 1900
        digits = f"{year:04d}{digits[2:]}"
    if len(digits) != 8 or not digits.isdigit():
        raise ValueError(f"unsupported date format: {value}")
    return f"{digits[:4]}-{digits[4:6]}-{digits[6:]}"
