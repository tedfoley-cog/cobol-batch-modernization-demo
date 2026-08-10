from decimal import Decimal

from tools.normalize import (
    align_scale,
    decode_ebcdic,
    decode_zoned,
    normalize_date,
    split_fixed_width,
    unpack_comp3,
)


def test_comp3_positive_and_negative():
    assert unpack_comp3(bytes.fromhex("12345c"), 5, 2) == Decimal("123.45")
    assert unpack_comp3(bytes.fromhex("12345d"), 5, 2) == Decimal("-123.45")


def test_comp3_rejects_invalid_digits_and_sign():
    import pytest

    with pytest.raises(ValueError):
        unpack_comp3(bytes.fromhex("12345c"), 6, 2)
    with pytest.raises(ValueError):
        unpack_comp3(bytes.fromhex("12345e"), 5, 2)
    with pytest.raises(ValueError):
        unpack_comp3(bytes.fromhex("12a45c"), 5, 2)


def test_zoned_overpunch_and_ebcdic():
    assert decode_zoned("123{".encode("cp037")) == Decimal(1230)
    assert decode_zoned("123}".encode("cp037")) == Decimal(-1230)
    assert decode_ebcdic("HELLO".encode("cp037")) == "HELLO"


def test_fixed_width_scale_and_date():
    assert split_fixed_width("ABC123", [{"name": "a", "start": 1, "end": 3}, {"name": "b", "start": 4, "end": 6}]) == {"a": "ABC", "b": "123"}
    assert align_scale("12.345", 3, 2) == Decimal("12.35")
    assert align_scale("12.345", 3, 2, "truncate") == Decimal("12.34")
    assert normalize_date("240101") == "2024-01-01"
