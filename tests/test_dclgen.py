import pytest

from tools.dclgen import Column, cobol_items


def test_decimal_mapping():
    assert cobol_items(Column("AMOUNT", "DECIMAL(9,2)", False)) == [
        "10 AMOUNT PIC S9(7)V9(2) COMP-3."
    ]
    assert cobol_items(Column("COUNT", "DECIMAL(5,0)", False)) == [
        "10 COUNT PIC S9(5) COMP-3."
    ]


def test_nullable_indicator_and_character_mapping():
    assert cobol_items(Column("TRADE_DATA", "CHAR(88)", True)) == [
        "10 TRADE-DATA PIC X(88).",
        "10 TRADE-DATA-NULL PIC S9(4) COMP.",
    ]


def test_integer_identity_mapping():
    assert cobol_items(Column("STQ_ID", "INT", False)) == [
        "10 STQ-ID PIC S9(9) COMP."
    ]


def test_unmapped_type_is_rejected():
    with pytest.raises(ValueError, match="unsupported SQL type"):
        cobol_items(Column("PAYLOAD", "XML", False))
