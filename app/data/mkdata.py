#!/usr/bin/env python3
"""Generate the sample datasets in this directory.

Two kinds of output:

  *.ebc / *.vb   EBCDIC (cp037) mainframe files.  Packed decimal fields are
                 really packed, signed zoned fields really carry an overpunch
                 in their last byte, and the variable length files carry a
                 real RDW.  Converting one of these with a plain ASCII
                 translate corrupts the numerics - that is the point of them.

  *.csv          ASCII companions for the DB2 seed tables.  Same data, so a
                 row in accounts.csv can be reconciled against the matching
                 record in acctmast.ebc.

Everything is synthetic.  Card numbers use the standard test BINs and carry
valid mod-10 check digits, so they are usable as test data and are not
issuable.  Values are fixed, not random - rerunning this script reproduces
the files byte for byte.

    python3 mkdata.py            regenerate every file in this directory

The script writes data.  It does not read the COBOL source and knows nothing
about the programs; the record layouts below were transcribed by hand from
the copybooks named against each one.
"""

import csv
import os
from decimal import Decimal

HERE = os.path.dirname(os.path.abspath(__file__))
EBCDIC = "cp037"


# ----------------------------------------------------------------------
# field encoders
# ----------------------------------------------------------------------

def alnum(value, length):
    """PIC X(n) - blank padded, truncated on the right."""
    text = "" if value is None else str(value)
    return text[:length].ljust(length).encode(EBCDIC)


def zoned(value, length):
    """PIC 9(n) - unsigned zoned decimal, zero filled on the left."""
    text = str(int(value)).rjust(length, "0")
    return text[-length:].encode(EBCDIC)


def zoned_signed(value, digits, scale=0):
    """PIC S9(n)V9(m) DISPLAY - sign carried as an overpunch on the last byte.

    Positive values take a C zone, negative a D zone, which is what the
    compiler generates for SIGN TRAILING INCLUDED.  A file holding these
    cannot be read as text: the last byte of a positive 1234 is X'C4', which
    is the letter D.
    """
    scaled = int((Decimal(str(value)) * (10 ** scale)).to_integral_value())
    negative = scaled < 0
    text = str(abs(scaled)).rjust(digits, "0")[-digits:]
    head = text[:-1].encode(EBCDIC)
    zone = 0xD0 if negative else 0xC0
    tail = bytes([zone | int(text[-1])])
    return head + tail


def comp3(value, digits, scale=0):
    """COMP-3 - packed decimal, sign nibble in the low half of the last byte.

    Length is (digits + 1) / 2 rounded up, which is what the compiler
    allocates for a signed field.
    """
    scaled = int((Decimal(str(value)) * (10 ** scale)).to_integral_value())
    negative = scaled < 0
    text = str(abs(scaled)).rjust(digits, "0")[-digits:]
    if len(text) % 2 == 0:
        text = "0" + text
    out = bytearray()
    for i in range(0, len(text) - 1, 2):
        out.append((int(text[i]) << 4) | int(text[i + 1]))
    out.append((int(text[-1]) << 4) | (0x0D if negative else 0x0C))
    return bytes(out)


def comp3_len(digits):
    return (digits + 2) // 2


def rdw(record):
    """Prefix a record with its RECFM=V descriptor word."""
    length = len(record) + 4
    return bytes([length >> 8, length & 0xFF, 0, 0]) + record


def luhn_check_digit(fifteen):
    total = 0
    for offset, char in enumerate(fifteen + "0"):
        digit = int(char)
        if offset % 2 == 0:
            digit *= 2
            if digit > 9:
                digit -= 9
        total += digit
    return (10 - total % 10) % 10


def card_number(bin_prefix, serial):
    """Sixteen digits on a test BIN with a valid check digit."""
    body = (bin_prefix + str(serial).rjust(15 - len(bin_prefix), "0"))[:15]
    return body + str(luhn_check_digit(body))


# ----------------------------------------------------------------------
# reference data
#
# Deliberate awkward cases, all of them things the estate really contains:
#
#   PTY00000004  one party holding four customer numbers
#   CUST 400000107 / card 4111110000000109
#                the embossed name and the customer record disagree on the
#                spelling of the surname - MCALLISTER against MACALLISTER
#   MRC000000000019
#                a merchant with no settlement route code, so nothing
#                resolves it in the routing file
#   card 4000000000000208
#                activation date 981105, a six digit date that windows to
#                1998 against the pivot of 50 in CVCONSTY
#   TXN0000000000031
#                twelve legs, the maximum the ODO allows
#   OFAC / SDN0000000000412
#                MERIWEATHER, JONATHON against customer MERRIWEATHER,
#                JONATHAN - a near miss the screening has to grade
# ----------------------------------------------------------------------

PARTIES = [
    # party_id, type, legal name, short name, dob, national id, tax id,
    # domicile, residence, citizenship, pep, status, onboard
    ("PTY00000001", "I", "HOLLOWAY, MARGARET ANNE", "HOLLOWAY MA", 19610318,
     "NID00061318", "TAX000000061318", "USA", "USA", "USA", "N", "A", 19980412),
    ("PTY00000002", "I", "OKONKWO, DANIEL CHUKA", "OKONKWO DC", 19750902,
     "NID00075902", "TAX000000075902", "USA", "USA", "NGA", "N", "A", 20030117),
    ("PTY00000003", "I", "VANDERBECK, PIETER JAN", "VANDERBECK PJ", 19680724,
     "NID00068724", "TAX000000068724", "NLD", "USA", "NLD", "N", "A", 20051108),
    ("PTY00000004", "I", "MERRIWEATHER, JONATHAN ROSS", "MERRIWEATHER JR",
     19570211, "NID00057211", "TAX000000057211", "USA", "USA", "USA",
     "N", "A", 19961002),
    ("PTY00000005", "I", "SZABO, ILONA KATALIN", "SZABO IK", 19820516,
     "NID00082516", "TAX000000082516", "HUN", "USA", "HUN", "N", "A", 20110623),
    ("PTY00000006", "O", "CASCADE RIDGE OUTFITTERS INCORPORATED",
     "CASCADE RIDGE", 19910305, "NID00091305", "TAX000000091305",
     "USA", "USA", "USA", "N", "A", 20010919),
    ("PTY00000007", "I", "ABIODUN, FOLASADE TEMILADE", "ABIODUN FT", 19880130,
     "NID00088130", "TAX000000088130", "USA", "USA", "NGA", "Y", "A", 20140205),
    ("PTY00000008", "I", "MACALLISTER, ROBERT DEAN", "MACALLISTER RD",
     19640827, "NID00064827", "TAX000000064827", "USA", "USA", "USA",
     "N", "A", 19990715),
    ("PTY00000009", "I", "PETROVA, YELENA SERGEEVNA", "PETROVA YS", 19790404,
     "NID00079404", "TAX000000079404", "USA", "USA", "RUS", "N", "A", 20080311),
    ("PTY00000010", "T", "THE LINDQVIST FAMILY TRUST 1994", "LINDQVIST TRUST",
     19940601, "NID00094601", "TAX000000094601", "USA", "USA", "USA",
     "N", "A", 19940601),
    ("PTY00000011", "I", "NAKAMURA, HIROSHI", "NAKAMURA H", 19710919,
     "NID00071919", "TAX000000071919", "JPN", "USA", "JPN", "N", "D", 20020426),
    ("PTY00000012", "I", "DELACROIX-BENNETT, SIMONE", "DELACROIX-BENNETT S",
     19860712, "NID00086712", "TAX000000086712", "FRA", "USA", "FRA",
     "N", "A", 20160830),
]

# cust_id, party_id, title, first, mi, last, dob, addr1, city, state, postal,
# country, segment, pep, vip, status, onboard
CUSTOMERS = [
    (400000101, "PTY00000001", "MRS ", "MARGARET", "A", "HOLLOWAY", 19610318,
     "1184 WILLOW BEND ROAD", "PORTLAND", "OR", "97218", "USA", "RETL",
     "N", "N", "A", 19980412),
    (400000102, "PTY00000002", "MR  ", "DANIEL", "C", "OKONKWO", 19750902,
     "77 EASTGATE TERRACE APT 4B", "NEWARK", "NJ", "07104", "USA", "RETL",
     "N", "N", "A", 20030117),
    (400000103, "PTY00000003", "MR  ", "PIETER", "J", "VANDERBECK", 19680724,
     "3300 HARBOUR POINT DRIVE", "SEATTLE", "WA", "98109", "USA", "AFFL",
     "N", "Y", "A", 20051108),
    (400000104, "PTY00000004", "MR  ", "JONATHAN", "R", "MERRIWEATHER",
     19570211, "58 CHESTNUT HILL LANE", "HARTFORD", "CT", "06105", "USA",
     "AFFL", "N", "Y", "A", 19961002),
    (400000105, "PTY00000004", "MR  ", "JONATHAN", "R", "MERRIWEATHER",
     19570211, "58 CHESTNUT HILL LANE", "HARTFORD", "CT", "06105", "USA",
     "RETL", "N", "N", "A", 20040518),
    (400000106, "PTY00000004", "MR  ", "JON", "R", "MERRIWEATHER", 19570211,
     "PO BOX 2214", "HARTFORD", "CT", "06104", "USA", "RETL",
     "N", "N", "D", 20090722),
    (400000107, "PTY00000008", "MR  ", "ROBERT", "D", "MACALLISTER", 19640827,
     "912 SOUTH FERNWOOD AVENUE", "DENVER", "CO", "80209", "USA", "RETL",
     "N", "N", "A", 19990715),
    (400000108, "PTY00000005", "MS  ", "ILONA", "K", "SZABO", 19820516,
     "440 RIVERWALK COURT UNIT 12", "CHICAGO", "IL", "60654", "USA", "RETL",
     "N", "N", "A", 20110623),
    (400000109, "PTY00000006", "    ", "", " ", "CASCADE RIDGE OUTFITTERS",
     19910305, "2 INDUSTRIAL PARKWAY", "BEND", "OR", "97701", "USA", "COMM",
     "N", "N", "A", 20010919),
    (400000110, "PTY00000007", "MS  ", "FOLASADE", "T", "ABIODUN", 19880130,
     "1901 CONGRESS AVENUE SUITE 700", "AUSTIN", "TX", "78701", "USA", "AFFL",
     "Y", "Y", "A", 20140205),
    (400000111, "PTY00000009", "MS  ", "YELENA", "S", "PETROVA", 19790404,
     "265 BRIGHTON BEACH AVENUE", "BROOKLYN", "NY", "11235", "USA", "RETL",
     "N", "N", "A", 20080311),
    (400000112, "PTY00000010", "    ", "", " ", "LINDQVIST FAMILY TRUST",
     19940601, "ONE COMMERCE SQUARE FLOOR 22", "MEMPHIS", "TN", "38103",
     "USA", "TRST", "N", "Y", "A", 19940601),
    (400000113, "PTY00000011", "MR  ", "HIROSHI", " ", "NAKAMURA", 19710919,
     "1450 POST STREET APT 903", "SAN FRANCISCO", "CA", "94109", "USA",
     "RETL", "N", "N", "D", 20020426),
    (400000114, "PTY00000012", "MS  ", "SIMONE", " ", "DELACROIX-BENNETT",
     19860712, "77 BEACON STREET", "BOSTON", "MA", "02108", "USA", "AFFL",
     "N", "N", "A", 20160830),
    (400000115, "PTY00000001", "MRS ", "MARGARET", "A", "HOLLOWAY", 19610318,
     "1184 WILLOW BEND ROAD", "PORTLAND", "OR", "97218", "USA", "COMM",
     "N", "N", "A", 20120904),
    (400000116, "PTY00000002", "MR  ", "DANIEL", "C", "OKONKWO", 19750902,
     "77 EASTGATE TERRACE APT 4B", "NEWARK", "NJ", "07104", "USA", "RETL",
     "N", "N", "A", 20170213),
    (400000117, "PTY00000003", "MR  ", "PIETER", "J", "VANDERBECK", 19680724,
     "3300 HARBOUR POINT DRIVE", "SEATTLE", "WA", "98109", "USA", "AFFL",
     "N", "Y", "A", 20130607),
    (400000118, "PTY00000006", "    ", "", " ", "CASCADE RIDGE OUTFITTERS",
     19910305, "2 INDUSTRIAL PARKWAY", "BEND", "OR", "97701", "USA", "COMM",
     "N", "N", "A", 20150401),
    (400000119, "PTY00000004", "MR  ", "JONATHAN", "R", "MERRIWEATHER",
     19570211, "58 CHESTNUT HILL LANE", "HARTFORD", "CT", "06105", "USA",
     "COMM", "N", "N", "A", 20180226),
]

# acct_id, cust_id, party_id, product, status, open, close, currency,
# curr_bal, stmt_bal, pending, cash_bal, min_pay, last_pay, last_pay_date,
# cycle_day, last_cycle, next_cycle, pay_due, delq_bucket, delq_amt,
# stmt_count, branch
ACCOUNTS = [
    (40000000101, 400000101, "PTY00000001", "GOLD", "O", 19980420, 0, "USD",
     "2418.77", "2290.14", "185.00", "0.00", "68.00", "300.00", 20240705,
     3, 20240603, 20240703, 20240728, 0, "0.00", 297, "PDX01"),
    (40000000102, 400000102, "PTY00000002", "CLAS", "O", 20030201, 0, "USD",
     "874.20", "802.55", "0.00", "150.00", "35.00", "80.00", 20240710,
     11, 20240611, 20240711, 20240805, 0, "0.00", 251, "NWK04"),
    (40000000103, 400000103, "PTY00000003", "PLAT", "O", 20051201, 0, "USD",
     "11842.63", "11204.09", "620.45", "0.00", "355.00", "1200.00", 20240702,
     18, 20240618, 20240718, 20240812, 0, "0.00", 223, "SEA02"),
    (40000000104, 400000104, "PTY00000004", "PLAT", "O", 19961015, 0, "USD",
     "6390.11", "6102.88", "0.00", "0.00", "190.00", "500.00", 20240628,
     25, 20240625, 20240725, 20240819, 0, "0.00", 333, "HFD01"),
    (40000000105, 400000105, "PTY00000004", "CLAS", "O", 20040601, 0, "USD",
     "1204.90", "1150.00", "45.00", "0.00", "40.00", "40.00", 20240612,
     3, 20240603, 20240703, 20240728, 1, "40.00", 240, "HFD01"),
    (40000000106, 400000106, "PTY00000004", "CLAS", "C", 20090801, 20190314,
     "USD", "0.00", "0.00", "0.00", "0.00", "0.00", "212.44", 20190301,
     11, 20190211, 20190311, 20190405, 0, "0.00", 114, "HFD01"),
    (40000000107, 400000107, "PTY00000008", "GOLD", "O", 19990801, 0, "USD",
     "3877.05", "3702.61", "0.00", "400.00", "115.00", "250.00", 20240709,
     18, 20240618, 20240718, 20240812, 0, "0.00", 299, "DEN03"),
    (40000000108, 400000108, "PTY00000005", "CLAS", "O", 20110701, 0, "USD",
     "412.38", "398.10", "22.50", "0.00", "25.00", "25.00", 20240705,
     25, 20240625, 20240725, 20240819, 0, "0.00", 156, "CHI07"),
    (40000000109, 400000109, "PTY00000006", "BUSN", "O", 20011001, 0, "USD",
     "24906.44", "23811.70", "1450.00", "0.00", "740.00", "2500.00", 20240701,
     3, 20240603, 20240703, 20240728, 0, "0.00", 273, "BND01"),
    (40000000110, 400000110, "PTY00000007", "PLAT", "O", 20140301, 0, "USD",
     "9105.22", "8720.00", "310.00", "0.00", "265.00", "900.00", 20240708,
     11, 20240611, 20240711, 20240805, 0, "0.00", 124, "AUS02"),
    (40000000111, 400000111, "PTY00000009", "CLAS", "O", 20080401, 0, "USD",
     "1993.47", "1902.30", "0.00", "75.00", "60.00", "60.00", 20240520,
     18, 20240618, 20240718, 20240812, 2, "120.00", 194, "BKN05"),
    (40000000112, 400000112, "PTY00000010", "PLAT", "O", 19940715, 0, "USD",
     "18220.09", "17800.44", "0.00", "0.00", "540.00", "1800.00", 20240703,
     25, 20240625, 20240725, 20240819, 0, "0.00", 359, "MEM01"),
    (40000000113, 400000113, "PTY00000011", "CLAS", "S", 20020501, 0, "USD",
     "645.90", "645.90", "0.00", "0.00", "35.00", "0.00", 20231114,
     3, 20240603, 20240703, 20240728, 4, "645.90", 265, "SFO04"),
    (40000000114, 400000114, "PTY00000012", "GOLD", "O", 20160901, 0, "USD",
     "5012.66", "4880.20", "260.00", "0.00", "150.00", "600.00", 20240706,
     11, 20240611, 20240711, 20240805, 0, "0.00", 94, "BOS01"),
    (40000000115, 400000115, "PTY00000001", "BUSN", "O", 20121001, 0, "USD",
     "7440.18", "7190.55", "0.00", "0.00", "225.00", "800.00", 20240704,
     18, 20240618, 20240718, 20240812, 0, "0.00", 141, "PDX01"),
    (40000000116, 400000116, "PTY00000002", "CLAS", "O", 20170301, 0, "USD",
     "288.14", "288.14", "0.00", "0.00", "25.00", "25.00", 20240702,
     25, 20240625, 20240725, 20240819, 0, "0.00", 88, "NWK04"),
    (40000000117, 400000117, "PTY00000003", "GOLD", "O", 20130701, 0, "USD",
     "3320.75", "3180.00", "95.00", "0.00", "100.00", "350.00", 20240707,
     3, 20240603, 20240703, 20240728, 0, "0.00", 132, "SEA02"),
    (40000000118, 400000118, "PTY00000006", "BUSN", "W", 20150501, 0, "USD",
     "15904.22", "15904.22", "0.00", "0.00", "0.00", "0.00", 20220916,
     11, 20240611, 20240711, 20240805, 6, "15904.22", 105, "BND01"),
]

# card_num, acct_id, cust_id, embossed, product, status, expiry, issue,
# activation (6 digit), last_used, cvv, pin_tries, reissue, prev, block, blkdt
CARDS = [
    (card_number("400000", 101), 40000000101, 400000101,
     "MARGARET A HOLLOWAY", "GOLD", "A", "2703", 20230318, 230402, 20240711,
     "Y", 0, 3, card_number("400000", 1), "    ", 0),
    (card_number("400000", 102), 40000000101, 400000101,
     "MARGARET A HOLLOWAY", "GOLD", "E", "2403", 20200318, 200405, 20230228,
     "Y", 0, 2, "", "    ", 0),
    (card_number("411111", 103), 40000000102, 400000102,
     "DANIEL C OKONKWO", "CLAS", "A", "2611", 20221101, 221114, 20240709,
     "Y", 0, 4, card_number("411111", 3), "    ", 0),
    (card_number("411111", 104), 40000000103, 400000103,
     "PIETER J VANDERBECK", "PLAT", "A", "2806", 20240601, 240610, 20240712,
     "Y", 0, 6, card_number("411111", 4), "    ", 0),
    (card_number("510510", 105), 40000000103, 400000103,
     "P J VANDERBECK", "PLAT", "B", "2606", 20220601, 220614, 20240301,
     "Y", 2, 1, "", "FRAU", 20240305),
    (card_number("510510", 106), 40000000104, 400000104,
     "JONATHAN R MERRIWEATHER", "PLAT", "A", "2712", 20231201, 231218,
     20240710, "Y", 0, 9, card_number("510510", 6), "    ", 0),
    # embossed name and the customer record disagree on the surname spelling
    (card_number("411111", 107), 40000000107, 400000107,
     "ROBERT D MCALLISTER", "GOLD", "A", "2609", 20220901, 220919, 20240708,
     "Y", 0, 5, card_number("411111", 7), "    ", 0),
    # six digit activation date that windows back to 1998
    (card_number("400000", 208), 40000000104, 400000104,
     "J R MERRIWEATHER", "PLAT", "C", "0011", 19981101, 981105, 20001204,
     "N", 0, 0, "", "CLSD", 20001231),
    (card_number("555555", 109), 40000000105, 400000105,
     "JONATHAN R MERRIWEATHER", "CLAS", "A", "2604", 20220401, 220417,
     20240612, "Y", 1, 3, "", "    ", 0),
    (card_number("555555", 110), 40000000108, 400000108,
     "ILONA K SZABO", "CLAS", "A", "2708", 20230801, 230812, 20240705,
     "Y", 0, 2, "", "    ", 0),
    (card_number("601100", 111), 40000000109, 400000109,
     "CASCADE RIDGE OUTFITTERS", "BUSN", "A", "2705", 20230501, 230522,
     20240711, "Y", 0, 4, "", "    ", 0),
    (card_number("601100", 112), 40000000109, 400000109,
     "T R HALVERSON", "BUSN", "A", "2705", 20230501, 230522, 20240628,
     "Y", 0, 4, "", "    ", 0),
    (card_number("400000", 113), 40000000110, 400000110,
     "FOLASADE T ABIODUN", "PLAT", "A", "2802", 20240201, 240214, 20240712,
     "Y", 0, 3, card_number("400000", 13), "    ", 0),
    (card_number("411111", 114), 40000000111, 400000111,
     "YELENA S PETROVA", "CLAS", "A", "2510", 20211001, 211018, 20240519,
     "Y", 0, 2, "", "    ", 0),
    (card_number("510510", 115), 40000000112, 400000112,
     "LINDQVIST FAMILY TRUST", "PLAT", "A", "2807", 20240701, 240709, 0,
     "Y", 0, 7, card_number("510510", 15), "    ", 0),
    (card_number("510510", 116), 40000000113, 400000113,
     "HIROSHI NAKAMURA", "L", "L", "2603", 20220301, 220314, 20231110,
     "Y", 3, 2, "", "LOST", 20231112),
    (card_number("555555", 117), 40000000114, 400000114,
     "SIMONE DELACROIX-BENNETT", "GOLD", "A", "2609", 20220901, 220914,
     20240706, "Y", 0, 2, "", "    ", 0),
    (card_number("601100", 118), 40000000115, 400000115,
     "MARGARET A HOLLOWAY", "BUSN", "A", "2604", 20220401, 220419, 20240704,
     "Y", 0, 1, "", "    ", 0),
    (card_number("400000", 119), 40000000116, 400000116,
     "DANIEL C OKONKWO", "CLAS", "N", "2812", 20240601, 0, 0,
     "Y", 0, 0, "", "    ", 0),
    (card_number("411111", 120), 40000000117, 400000117,
     "PIETER J VANDERBECK", "GOLD", "A", "2611", 20221101, 221121, 20240707,
     "Y", 0, 2, "", "    ", 0),
    (card_number("510510", 121), 40000000118, 400000118,
     "CASCADE RIDGE OUTFITTERS", "BUSN", "C", "2405", 20200501, 200518,
     20220914, "Y", 0, 1, "", "WOFF", 20220930),
    (card_number("555555", 122), 40000000101, 400000101,
     "M A HOLLOWAY", "GOLD", "A", "2703", 20230318, 230402, 20240610,
     "Y", 0, 0, "", "    ", 0),
]

# card_num index shortcut
CARD_NUMS = [row[0] for row in CARDS]

# merchant_id, name, mcc, acquirer, country, city, high risk, chargeback pct,
# settle route, status, onboard
MERCHANTS = [
    ("MRC000000000001", "NORTHLAKE GROCERY COOPERATIVE", "5411",
     "ACQ00000001", "USA", "PORTLAND", "N", "0.04", "DOM1", "A", 20040112),
    ("MRC000000000002", "HARBOUR POINT FUEL AND SERVICE", "5541",
     "ACQ00000001", "USA", "SEATTLE", "N", "0.11", "DOM1", "A", 20060830),
    ("MRC000000000003", "CHESTNUT HILL PHARMACY", "5912",
     "ACQ00000002", "USA", "HARTFORD", "N", "0.02", "DOM1", "A", 20011105),
    ("MRC000000000004", "TRANSATLANTIC AIRWAYS", "4511",
     "ACQ00000003", "GBR", "LONDON", "N", "0.38", "INTL", "A", 19990624),
    ("MRC000000000005", "BEND OUTDOOR SUPPLY WAREHOUSE", "5941",
     "ACQ00000001", "USA", "BEND", "N", "0.07", "DOM1", "A", 20030517),
    ("MRC000000000006", "RIVERWALK BISTRO", "5812",
     "ACQ00000002", "USA", "CHICAGO", "N", "0.19", "DOM2", "A", 20120209),
    ("MRC000000000007", "CONGRESS AVENUE PARKING AUTHORITY", "7523",
     "ACQ00000002", "USA", "AUSTIN", "N", "0.01", "DOM2", "A", 20150714),
    ("MRC000000000008", "BRIGHTON BEACH ELECTRONICS", "5732",
     "ACQ00000004", "USA", "BROOKLYN", "Y", "2.44", "DOM2", "A", 20180322),
    ("MRC000000000009", "COMMERCE SQUARE OFFICE SUPPLY", "5943",
     "ACQ00000002", "USA", "MEMPHIS", "N", "0.06", "DOM1", "A", 20070911),
    ("MRC000000000010", "POST STREET LAUNDRY", "7211",
     "ACQ00000001", "USA", "SAN FRANCISCO", "N", "0.00", "DOM2", "A",
     20091028),
    ("MRC000000000011", "BEACON HILL BOOKSELLERS", "5942",
     "ACQ00000002", "USA", "BOSTON", "N", "0.03", "DOM1", "A", 20050406),
    ("MRC000000000012", "WILLOW BEND VETERINARY CLINIC", "0742",
     "ACQ00000001", "USA", "PORTLAND", "N", "0.02", "DOM1", "A", 20101201),
    ("MRC000000000013", "EASTGATE TELECOM SERVICES", "4814",
     "ACQ00000004", "USA", "NEWARK", "N", "0.52", "DOM2", "A", 20161107),
    ("MRC000000000014", "SOUTH FERNWOOD HARDWARE", "5251",
     "ACQ00000001", "USA", "DENVER", "N", "0.05", "DOM1", "A", 20020318),
    ("MRC000000000015", "AMSTERDAM CANAL HOTELS BV", "7011",
     "ACQ00000005", "NLD", "AMSTERDAM", "N", "0.44", "INTL", "A", 20081015),
    ("MRC000000000016", "TOKYO EXPRESS RAIL PASS", "4112",
     "ACQ00000005", "JPN", "TOKYO", "N", "0.09", "INTL", "A", 20111219),
    ("MRC000000000017", "GLOBAL DIGITAL GOODS EXCHANGE", "5816",
     "ACQ00000004", "MLT", "VALLETTA", "Y", "4.87", "INTL", "S", 20200604),
    ("MRC000000000018", "CASCADE RIDGE FLEET FUEL", "5542",
     "ACQ00000001", "USA", "BEND", "N", "0.08", "DOM1", "A", 20130225),
    # no settlement route code - nothing in the routing file resolves this one
    ("MRC000000000019", "PIONEER SQUARE POPUP MARKET", "5999",
     "ACQ00000001", "USA", "PORTLAND", "N", "0.00", "", "A", 20230815),
    ("MRC000000000020", "HUNGARIAN CULTURAL FOUNDATION", "8398",
     "ACQ00000005", "HUN", "BUDAPEST", "N", "0.00", "INTL", "A", 20190503),
]

# product, fee type, eff, exp, flat, pct, min, max, waiver, handler, ccy, desc
FEE_SCHEDULE = [
    ("CLAS", "ANNU", 20180101, 99991231, "0.00", "0.00000", "0.00", "0.00",
     "ALL ", "CBFEE01", "USD", "NO ANNUAL FEE - CLASSIC"),
    ("GOLD", "ANNU", 20180101, 20221231, "75.00", "0.00000", "0.00", "0.00",
     "SPND", "CBFEE01", "USD", "ANNUAL FEE - GOLD"),
    ("GOLD", "ANNU", 20230101, 99991231, "95.00", "0.00000", "0.00", "0.00",
     "SPND", "CBFEE01", "USD", "ANNUAL FEE - GOLD"),
    ("PLAT", "ANNU", 20230101, 99991231, "395.00", "0.00000", "0.00", "0.00",
     "VIP ", "CBFEE01", "USD", "ANNUAL FEE - PLATINUM"),
    ("BUSN", "ANNU", 20230101, 99991231, "150.00", "0.00000", "0.00", "0.00",
     "    ", "CBFEE01", "USD", "ANNUAL FEE - BUSINESS"),
    ("CLAS", "LATE", 20210401, 99991231, "39.00", "0.00000", "0.00", "39.00",
     "FRST", "CBFEE02", "USD", "LATE PAYMENT FEE"),
    ("GOLD", "LATE", 20210401, 99991231, "39.00", "0.00000", "0.00", "39.00",
     "FRST", "CBFEE02", "USD", "LATE PAYMENT FEE"),
    ("PLAT", "LATE", 20210401, 99991231, "29.00", "0.00000", "0.00", "29.00",
     "FRST", "CBFEE02", "USD", "LATE PAYMENT FEE"),
    ("BUSN", "LATE", 20210401, 99991231, "49.00", "0.00000", "0.00", "49.00",
     "    ", "CBFEE02", "USD", "LATE PAYMENT FEE"),
    ("CLAS", "OVLM", 20210401, 99991231, "35.00", "0.00000", "0.00", "35.00",
     "OPTI", "CBFEE02", "USD", "OVER LIMIT FEE"),
    ("GOLD", "OVLM", 20210401, 99991231, "35.00", "0.00000", "0.00", "35.00",
     "OPTI", "CBFEE02", "USD", "OVER LIMIT FEE"),
    ("CLAS", "CASH", 20200101, 99991231, "10.00", "3.00000", "10.00",
     "0.00", "    ", "CBFEE03", "USD", "CASH ADVANCE FEE"),
    ("GOLD", "CASH", 20200101, 99991231, "10.00", "3.00000", "10.00",
     "0.00", "    ", "CBFEE03", "USD", "CASH ADVANCE FEE"),
    ("PLAT", "CASH", 20200101, 99991231, "5.00", "2.50000", "5.00",
     "0.00", "    ", "CBFEE03", "USD", "CASH ADVANCE FEE"),
    ("BUSN", "CASH", 20200101, 99991231, "10.00", "3.50000", "10.00",
     "0.00", "    ", "CBFEE03", "USD", "CASH ADVANCE FEE"),
    ("CLAS", "FRGN", 20190601, 99991231, "0.00", "2.75000", "0.00", "0.00",
     "    ", "CBFEE03", "USD", "FOREIGN TRANSACTION FEE"),
    ("GOLD", "FRGN", 20190601, 99991231, "0.00", "2.00000", "0.00", "0.00",
     "    ", "CBFEE03", "USD", "FOREIGN TRANSACTION FEE"),
    ("PLAT", "FRGN", 20190601, 99991231, "0.00", "0.00000", "0.00", "0.00",
     "    ", "CBFEE03", "USD", "NO FOREIGN TRANSACTION FEE"),
    ("CLAS", "RETN", 20210401, 99991231, "29.00", "0.00000", "0.00", "29.00",
     "FRST", "CBFEE02", "USD", "RETURNED PAYMENT FEE"),
    ("GOLD", "REPL", 20150101, 99991231, "15.00", "0.00000", "0.00", "15.00",
     "LOST", "CBFEE02", "USD", "CARD REPLACEMENT FEE"),
]

# rule_id, class, seq, handler, thr_amt, thr_cnt, thr_pct, points, action,
# mcc list, country list, eff, exp, active, description
FRAUD_RULES = [
    ("FR000001", "STAN", 1, "CAFRD01", "0.00", 6, "0.00", 25, "SCOR",
     "", "", 20190101, 99991231, "Y",
     "VELOCITY - MORE THAN SIX AUTHS IN THE WINDOW"),
    ("FR000002", "STAN", 1, "CAFRD01", "0.00", 12, "0.00", 60, "REFR",
     "", "", 20190101, 99991231, "Y",
     "VELOCITY - MORE THAN TWELVE AUTHS IN THE WINDOW"),
    ("FR000003", "STAN", 2, "CAFRD02", "0.00", 0, "0.00", 40, "SCOR",
     "", "RUS,PRK,IRN,SYR", 20190101, 99991231, "Y",
     "GEOGRAPHY - RESTRICTED COUNTRY"),
    ("FR000004", "STAN", 2, "CAFRD02", "0.00", 3, "0.00", 55, "REFR",
     "", "", 20200301, 99991231, "Y",
     "GEOGRAPHY - THREE COUNTRIES IN TWENTY FOUR HOURS"),
    ("FR000005", "STAN", 3, "CAFRD03", "2500.00", 0, "0.00", 30, "SCOR",
     "", "", 20190101, 99991231, "Y",
     "AMOUNT - SINGLE AUTH ABOVE 2500"),
    ("FR000006", "STAN", 3, "CAFRD03", "0.00", 0, "80.00", 45, "SCOR",
     "", "", 20190101, 99991231, "Y",
     "AMOUNT - ABOVE EIGHTY PERCENT OF AVAILABLE"),
    ("FR000007", "STAN", 4, "CAFRD04", "0.00", 0, "2.00", 35, "SCOR",
     "", "", 20190101, 99991231, "Y",
     "MERCHANT - CHARGEBACK RATE ABOVE TWO PERCENT"),
    ("FR000008", "STAN", 4, "CAFRD04", "0.00", 0, "0.00", 70, "REFR",
     "5816,7995,6051", "", 20210901, 99991231, "Y",
     "MERCHANT - HIGH RISK MCC"),
    ("FR000009", "HIGH", 1, "CAFRD01", "0.00", 3, "0.00", 60, "REFR",
     "", "", 20190101, 99991231, "Y",
     "HIGH RISK BAND - VELOCITY ABOVE THREE"),
    ("FR000010", "HIGH", 2, "CAFRD03", "500.00", 0, "0.00", 65, "REFR",
     "", "", 20190101, 99991231, "Y",
     "HIGH RISK BAND - AUTH ABOVE 500"),
    ("FR000011", "HIGH", 3, "CAFRD04", "0.00", 0, "0.00", 99, "DECL",
     "5816,7995", "", 20210901, 99991231, "Y",
     "HIGH RISK BAND AND HIGH RISK MCC - DECLINE"),
    ("FR000012", "STAN", 5, "CAFRD03", "0.00", 0, "0.00", 20, "FLAG",
     "", "", 20170101, 20201231, "N",
     "WITHDRAWN 2020 - ROUND AMOUNT PATTERN"),
]

# party_id, seq, status, level, review type, review date, next review,
# expiry, doc type, doc ref, doc expiry, addr verified, source of funds,
# rating, edd, reviewer
KYC = [
    ("PTY00000001", 1, "OK", "STD ", "PERI", 20230412, 20260412, 20260412,
     "PASS", "P4471902", 20291130, "Y", "SALW", "L", "N", "PRTYOP01"),
    ("PTY00000002", 1, "OK", "STD ", "PERI", 20220117, 20250117, 20250117,
     "DLIC", "NJ88213004", 20270902, "Y", "SALW", "L", "N", "PRTYOP01"),
    ("PTY00000003", 1, "OK", "ENH ", "PERI", 20231108, 20251108, 20251108,
     "PASS", "NL6620114", 20301231, "Y", "INVT", "M", "Y", "PRTYOP03"),
    ("PTY00000004", 1, "OK", "STD ", "PERI", 20221002, 20251002, 20251002,
     "PASS", "P2210447", 20280211, "Y", "SALW", "L", "N", "PRTYOP02"),
    ("PTY00000004", 2, "PN", "ENH ", "EVNT", 20240614, 20240814, 0,
     "    ", "", 0, "Y", "SALW", "M", "Y", "PRTYOP03"),
    ("PTY00000005", 1, "OK", "STD ", "PERI", 20230623, 20260623, 20260623,
     "PASS", "HU4408122", 20281115, "Y", "SALW", "L", "N", "PRTYOP01"),
    ("PTY00000006", 1, "OK", "ENH ", "PERI", 20220919, 20250919, 20250919,
     "INCO", "OR-C-441907", 99991231, "Y", "BUSN", "M", "Y", "PRTYOP03"),
    ("PTY00000007", 1, "OK", "ENH ", "PERI", 20240205, 20250205, 20250205,
     "PASS", "P9917204", 20310130, "Y", "SALW", "H", "Y", "PRTYOP04"),
    ("PTY00000008", 1, "OK", "STD ", "PERI", 20220715, 20250715, 20250715,
     "DLIC", "CO40128877", 20260827, "Y", "SALW", "L", "N", "PRTYOP01"),
    ("PTY00000009", 1, "EX", "ENH ", "PERI", 20210311, 20240311, 20240311,
     "PASS", "RU7712043", 20250404, "N", "SALW", "M", "Y", "PRTYOP03"),
    ("PTY00000010", 1, "OK", "ENH ", "PERI", 20230601, 20260601, 20260601,
     "TRDD", "TN-TR-940601", 99991231, "Y", "INHT", "M", "Y", "PRTYOP03"),
    ("PTY00000011", 1, "FL", "ENH ", "EVNT", 20231120, 20240220, 0,
     "PASS", "JP5504118", 20260919, "N", "    ", "H", "Y", "PRTYOP04"),
    ("PTY00000012", 1, "OK", "STD ", "PERI", 20230830, 20260830, 20260830,
     "PASS", "FR8807122", 20300712, "Y", "SALW", "L", "N", "PRTYOP02"),
    ("PTY00000006", 2, "OK", "ENH ", "EVNT", 20240301, 20250301, 20250301,
     "INCO", "OR-C-441907", 99991231, "Y", "BUSN", "M", "Y", "PRTYOP03"),
]

# list_cd, entry_id, entity name, type, country, dob, program, listed,
# delisted, active, load job
SANCTIONS = [
    ("OFAC    ", "SDN0000000000101", "PETROV, SERGEI ALEKSANDROVICH", "I",
     "RUS", 19660214, "UKRAINE-EO14024", 20220401, 0, "Y", "CBPRT01J"),
    ("OFAC    ", "SDN0000000000147", "NORTHERN STAR SHIPPING LLC", "O",
     "RUS", 0, "UKRAINE-EO14024", 20220401, 0, "Y", "CBPRT01J"),
    # near miss against customer 400000104 MERRIWEATHER, JONATHAN ROSS
    ("OFAC    ", "SDN0000000000412", "MERIWEATHER, JONATHON", "I",
     "USA", 19570211, "SDNTK", 20190722, 0, "Y", "CBPRT01J"),
    ("OFAC    ", "SDN0000000000488", "AL-MASRI, TAREQ IBRAHIM", "I",
     "SYR", 19801103, "SDGT", 20170310, 0, "Y", "CBPRT01J"),
    ("OFAC    ", "SDN0000000000512", "HANSEN, ERIK JOHANN", "I",
     "DNK", 19721219, "SDNTK", 20150608, 20210930, "N", "CBPRT01J"),
    ("UN      ", "UNL0000000000034", "KHALID TRADING ESTABLISHMENT", "O",
     "IRN", 0, "IRAN-1737", 20120115, 0, "Y", "CBPRT01J"),
    ("UN      ", "UNL0000000000091", "OKONKWO, DANIELLA CHIOMA", "I",
     "NGA", 19910408, "TALIBAN-1988", 20200217, 0, "Y", "CBPRT01J"),
    ("UN      ", "UNL0000000000122", "PYONGYANG METAL EXPORT COMPANY", "O",
     "PRK", 0, "DPRK-1718", 20160930, 0, "Y", "CBPRT01J"),
    ("EU      ", "EUL0000000000205", "VOLKOV, DMITRI NIKOLAEVICH", "I",
     "RUS", 19690817, "EU-269-2014", 20220315, 0, "Y", "CBPRT01J"),
    ("EU      ", "EUL0000000000241", "SZABO, ILDIKO", "I",
     "HUN", 19820516, "EU-2580-2001", 20180912, 0, "Y", "CBPRT01J"),
    ("EU      ", "EUL0000000000260", "BALTIC MARINE LOGISTICS OU", "O",
     "EST", 0, "EU-833-2014", 20230224, 0, "Y", "CBPRT01J"),
    ("INTERNAL", "INT0000000000012", "NAKAMURA, HIROSHI", "I",
     "JPN", 19710919, "INTERNAL-FRAUD", 20231115, 0, "Y", "PRKYC04"),
    ("INTERNAL", "INT0000000000019", "GLOBAL DIGITAL GOODS EXCHANGE", "O",
     "MLT", 0, "INTERNAL-MERCH", 20200711, 0, "Y", "PRKYC04"),
    ("INTERNAL", "INT0000000000023", "ABIODUN, FOLASHADE", "I",
     "NGA", 19880130, "INTERNAL-PEP", 20140205, 0, "Y", "PRKYC04"),
    ("OFAC    ", "SDN0000000000633", "DELACROIX, SIMON", "I",
     "FRA", 19860712, "SDNTK", 20211104, 0, "Y", "CBPRT01J"),
    ("OFAC    ", "SDN0000000000701", "VANDERBEEK, PETER J", "I",
     "NLD", 19680724, "CYBER2", 20200528, 0, "Y", "CBPRT01J"),
    ("UN      ", "UNL0000000000188", "HOLLOWAY MINING VENTURES LTD", "O",
     "ZWE", 0, "UN-1521", 20110603, 20190418, "N", "CBPRT01J"),
    ("EU      ", "EUL0000000000299", "PETROVA, YELENA", "I",
     "RUS", 19790404, "EU-269-2014", 20240119, 0, "Y", "CBPRT01J"),
]

# card_num index, limit type, limit, used, avail, daily cnt limit, daily used,
# velocity window, velocity max, apr, cash apr, band, last review, eff, exp
LIMITS = [
    (0, "CRED", "15000.00", "2418.77", "12581.23", 20, 3, 60, 5,
     "18.99000", "24.99000", "A", 20240301, 20230318, 99991231),
    (0, "CASH", "3000.00", "0.00", "3000.00", 3, 0, 1440, 2,
     "24.99000", "24.99000", "A", 20240301, 20230318, 99991231),
    (0, "DAIL", "5000.00", "412.90", "4587.10", 20, 3, 1440, 20,
     "0.00000", "0.00000", "A", 20240301, 20230318, 99991231),
    (2, "CRED", "4500.00", "874.20", "3625.80", 12, 1, 60, 5,
     "22.99000", "27.99000", "B", 20240115, 20221101, 99991231),
    (2, "CASH", "900.00", "150.00", "750.00", 2, 1, 1440, 2,
     "27.99000", "27.99000", "B", 20240115, 20221101, 99991231),
    (3, "CRED", "40000.00", "11842.63", "28157.37", 30, 4, 60, 8,
     "15.49000", "21.99000", "A", 20240401, 20240601, 99991231),
    (3, "FRGN", "20000.00", "3204.11", "16795.89", 30, 2, 60, 8,
     "15.49000", "21.99000", "A", 20240401, 20240601, 99991231),
    (5, "CRED", "35000.00", "6390.11", "28609.89", 30, 2, 60, 8,
     "15.49000", "21.99000", "A", 20240201, 20231201, 99991231),
    (6, "CRED", "12000.00", "3877.05", "8122.95", 20, 2, 60, 5,
     "19.99000", "25.99000", "B", 20240301, 20220901, 99991231),
    (6, "CASH", "2400.00", "400.00", "2000.00", 3, 1, 1440, 2,
     "25.99000", "25.99000", "B", 20240301, 20220901, 99991231),
    (8, "CRED", "3500.00", "1204.90", "2295.10", 12, 2, 60, 5,
     "24.99000", "27.99000", "C", 20240501, 20220401, 99991231),
    (9, "CRED", "2500.00", "412.38", "2087.62", 10, 1, 60, 4,
     "22.99000", "27.99000", "B", 20240601, 20230801, 99991231),
    (10, "CRED", "75000.00", "24906.44", "50093.56", 50, 8, 60, 12,
     "16.99000", "22.99000", "A", 20240401, 20230501, 99991231),
    (12, "CRED", "50000.00", "9105.22", "40894.78", 40, 5, 60, 10,
     "15.49000", "21.99000", "A", 20240301, 20240201, 99991231),
    (13, "CRED", "3000.00", "1993.47", "1006.53", 10, 2, 60, 4,
     "26.99000", "29.99000", "C", 20240201, 20211001, 99991231),
    (14, "CRED", "60000.00", "18220.09", "41779.91", 40, 3, 60, 10,
     "14.99000", "20.99000", "A", 20240501, 20240701, 99991231),
    (16, "CRED", "20000.00", "5012.66", "14987.34", 25, 4, 60, 6,
     "18.99000", "24.99000", "A", 20240401, 20220901, 99991231),
    (17, "CRED", "25000.00", "7440.18", "17559.82", 30, 3, 60, 8,
     "17.99000", "23.99000", "A", 20240301, 20220401, 99991231),
    (19, "CRED", "18000.00", "3320.75", "14679.25", 25, 2, 60, 6,
     "18.99000", "24.99000", "A", 20240501, 20221101, 99991231),
    (0, "CRED", "10000.00", "1980.00", "8020.00", 20, 3, 60, 5,
     "18.99000", "24.99000", "B", 20220301, 20200318, 20230317),
]


# ----------------------------------------------------------------------
# EBCDIC file - card master, layout CVCARD01Y
# ----------------------------------------------------------------------

def write_cardmast(path):
    out = bytearray()
    for (num, acct, cust, embossed, product, status, expiry, issue,
         activation, last_used, cvv, pin_tries, reissue, prev, block,
         block_date) in CARDS:
        rec = bytearray()
        rec += alnum(num, 16)
        rec += zoned(acct, 11)
        rec += zoned(cust, 9)
        rec += alnum(embossed, 26)
        rec += alnum(product, 4)
        rec += alnum(status, 1)
        rec += zoned(expiry, 4)
        rec += zoned(issue, 8)
        rec += zoned(last_used, 8)
        rec += zoned(activation, 6)
        rec += alnum(cvv, 1)
        rec += zoned(pin_tries, 1)
        rec += zoned(reissue, 2)
        rec += alnum(prev, 16)
        rec += alnum(block, 4)
        rec += zoned(block_date, 8)
        rec += alnum("CBREF02", 8)
        rec += alnum("2024-07-12-03.14.22.481900", 26)
        rec += alnum("", 20)
        out += rec
    _write(path, bytes(out), len(rec))
    return len(rec)


# ----------------------------------------------------------------------
# EBCDIC file - account master, layout CVACCT01Y
# ----------------------------------------------------------------------

def write_acctmast(path):
    out = bytearray()
    for (acct, cust, party, product, status, open_dt, close_dt, ccy,
         curr_bal, stmt_bal, pending, cash_bal, min_pay, last_pay,
         last_pay_date, cycle_day, last_cycle, next_cycle, pay_due,
         delq_bucket, delq_amt, stmt_count, branch) in ACCOUNTS:
        rec = bytearray()
        rec += zoned(acct, 11)
        rec += zoned(cust, 9)
        rec += alnum(party, 11)
        rec += alnum(product, 4)
        rec += alnum(status, 1)
        rec += zoned(open_dt, 8)
        rec += zoned(close_dt, 8)
        rec += alnum(ccy, 3)
        rec += comp3(curr_bal, 13, 2)
        rec += comp3(stmt_bal, 13, 2)
        rec += comp3(pending, 13, 2)
        rec += comp3(cash_bal, 13, 2)
        rec += comp3(min_pay, 11, 2)
        rec += comp3(last_pay, 11, 2)
        rec += zoned(cycle_day, 2)
        rec += zoned(last_cycle, 8)
        rec += zoned(next_cycle, 8)
        rec += zoned(pay_due, 8)
        rec += zoned(last_pay_date, 8)
        rec += zoned(delq_bucket, 1)
        rec += comp3(delq_amt, 11, 2)
        rec += zoned(stmt_count, 4)
        rec += alnum(branch, 5)
        rec += alnum("", 24)
        out += rec
    _write(path, bytes(out), len(rec))
    return len(rec)


# ----------------------------------------------------------------------
# EBCDIC file - authorization extract, layout CVAUTH01Y
#
# The detail area is 60 bytes and the three variants are 60, 36 and 39 bytes
# long.  The short ones are written into a buffer that is NOT cleared between
# records, so their tail carries whatever the previous record left there.
# That is exactly what the real extract does - the program moves the variant
# into the 60 byte area and writes, and the area is WORKING-STORAGE.
# ----------------------------------------------------------------------

AUTHS = [
    # seq, card index, type, status, resp, reason, score, band, settled,
    # posted, time, detail tuple
    (1, 0, "P", "A", "00", "    ", 120, "A", "Y", "Y", 91432,
     ("MRC000000000001", "NORTHLAKE GROCERY COOP", 5411, "TRM04412",
      "84.19", "USD", "C")),
    (2, 0, "P", "A", "00", "    ", 95, "A", "Y", "Y", 133007,
     ("MRC000000000012", "WILLOW BEND VET CLINIC", 742, "TRM00981",
      "212.50", "USD", "C")),
    (3, 2, "C", "A", "00", "    ", 305, "B", "Y", "Y", 201155,
     ("ATM04417", "STAR", "ACQ00000001", "150.00", "10.00", "USD")),
    (4, 3, "P", "A", "00", "    ", 140, "A", "N", "N", 84422,
     ("MRC000000000002", "HARBOUR POINT FUEL", 5541, "TRM10022",
      "68.44", "USD", "M")),
    (5, 3, "P", "D", "05", "LIMT", 480, "C", "N", "N", 84931,
     ("MRC000000000015", "AMSTERDAM CANAL HOTELS", 7011, "TRM77301",
      "3204.11", "EUR", "K")),
    (6, 6, "R", "A", "00", "    ", 60, "A", "Y", "Y", 154210,
     ("AUTH00000412", 240611, "MRC000000000014", "-45.90")),
    (7, 6, "P", "A", "00", "    ", 110, "A", "Y", "Y", 174855,
     ("MRC000000000014", "SOUTH FERNWOOD HARDWARE", 5251, "TRM33110",
      "119.87", "USD", "C")),
    (8, 8, "P", "F", "01", "VELO", 620, "C", "N", "N", 220401,
     ("MRC000000000008", "BRIGHTON BEACH ELECTRON", 5732, "TRM88014",
      "1899.00", "USD", "K")),
    (9, 9, "C", "D", "51", "FUND", 410, "B", "N", "N", 74510,
     ("ATM10088", "PLUS", "ACQ00000002", "400.00", "12.00", "USD")),
    (10, 10, "P", "A", "00", "    ", 85, "A", "Y", "Y", 112233,
     ("MRC000000000005", "BEND OUTDOOR SUPPLY", 5941, "TRM55201",
      "1450.00", "USD", "C")),
    (11, 10, "R", "A", "00", "    ", 50, "A", "Y", "N", 160044,
     ("AUTH00000988", 240628, "MRC000000000005", "-320.00")),
    (12, 12, "P", "A", "00", "    ", 210, "B", "N", "N", 191500,
     ("MRC000000000007", "CONGRESS AVE PARKING", 7523, "TRM12007",
      "18.00", "USD", "C")),
    (13, 12, "P", "F", "01", "GEOG", 555, "C", "N", "N", 34122,
     ("MRC000000000016", "TOKYO EXPRESS RAIL PASS", 4112, "TRM90441",
      "310.00", "JPY", "K")),
    (14, 13, "P", "D", "14", "STAT", 700, "X", "N", "N", 203344,
     ("MRC000000000017", "GLOBAL DIGITAL GOODS", 5816, "TRM99001",
      "249.99", "USD", "K")),
    (15, 16, "P", "A", "00", "    ", 130, "A", "Y", "Y", 130909,
     ("MRC000000000011", "BEACON HILL BOOKSELLERS", 5942, "TRM21044",
      "76.40", "USD", "C")),
    (16, 17, "C", "A", "00", "    ", 240, "B", "Y", "Y", 95511,
     ("ATM22190", "CIRR", "ACQ00000001", "500.00", "17.50", "USD")),
    (17, 19, "P", "A", "00", "    ", 100, "A", "Y", "Y", 145522,
     ("MRC000000000019", "PIONEER SQUARE POPUP", 5999, "TRM44012",
      "95.00", "USD", "C")),
    (18, 5, "P", "A", "00", "    ", 155, "A", "N", "N", 181212,
     ("MRC000000000003", "CHESTNUT HILL PHARMACY", 5912, "TRM66330",
      "42.18", "USD", "C")),
    (19, 5, "R", "A", "00", "    ", 45, "A", "N", "N", 182002,
     ("AUTH00001104", 240705, "MRC000000000003", "-42.18")),
    (20, 4, "P", "V", "00", "REVR", 190, "A", "N", "N", 101010,
     ("MRC000000000006", "RIVERWALK BISTRO", 5812, "TRM70115",
      "88.25", "USD", "C")),
]


def _auth_detail(auth_type, detail):
    if auth_type == "P":
        merchant, name, mcc, terminal, amount, currency, entry = detail
        body = (alnum(merchant, 15) + alnum(name, 22) + zoned(mcc, 4)
                + alnum(terminal, 8) + comp3(amount, 11, 2)
                + alnum(currency, 3) + alnum(entry, 1) + alnum("", 1))
    elif auth_type == "C":
        atm, network, acquirer, amount, fee, currency = detail
        body = (alnum(atm, 8) + alnum(network, 4) + alnum(acquirer, 11)
                + comp3(amount, 11, 2) + comp3(fee, 7, 2)
                + alnum(currency, 3))
    else:
        orig_auth, orig_date, orig_merchant, amount = detail
        body = (alnum(orig_auth, 12) + zoned(orig_date, 6)
                + alnum(orig_merchant, 15) + comp3(amount, 11, 2))
    return body


def write_authextr(path):
    out = bytearray()
    # the 60 byte detail area, carried between records and never cleared
    detail_area = bytearray(alnum("", 60))
    for (seq, card_ix, auth_type, status, resp, reason, score, band,
         settled, posted, time_hhmmss, detail) in AUTHS:
        card = CARDS[card_ix]
        body = _auth_detail(auth_type, detail)
        detail_area[0:len(body)] = body
        rec = bytearray()
        rec += alnum(card[0], 16)
        rec += zoned(20240712, 8)
        rec += zoned(seq, 9)
        rec += zoned(card[1], 11)
        rec += zoned(card[2], 9)
        rec += zoned(time_hhmmss, 6)
        rec += alnum(auth_type, 1)
        rec += alnum(status, 1)
        rec += alnum(resp, 2)
        rec += alnum(reason, 4)
        rec += zoned(score, 3)
        rec += alnum(band, 1)
        rec += alnum(settled, 1)
        rec += alnum(posted, 1)
        rec += bytes(detail_area)
        rec += alnum("CACRD08", 8)
        rec += alnum("T%03d" % (seq % 1000), 4)
        rec += alnum("CSROP%03d" % (seq % 100), 8)
        rec += alnum("2024-07-12-%02d.%02d.%02d.%06d"
                     % (time_hhmmss // 10000, (time_hhmmss // 100) % 100,
                        time_hhmmss % 100, seq * 1117), 26)
        out += rec
    _write(path, bytes(out), len(rec))
    return len(rec)


# ----------------------------------------------------------------------
# EBCDIC file - posted transactions, layout CVTRAN01Y
#
# RECFM=VB.  The leg table is an OCCURS DEPENDING ON and the trailer sits
# after it, so no two records with different leg counts have their trailer at
# the same offset.  Leg counts here run from one to twelve, twelve being the
# maximum the copybook allows.
# ----------------------------------------------------------------------

LEG_TYPES = ["PRIN", "FEE ", "INTR", "RWRD", "FXAD"]

TXNS = [
    # txn seq, card index, type, source, amount, ccy, billing, fx, merchant,
    # mcc, description, leg count
    (1, 0, "PURC", "IC", "84.19", "USD", "84.19", "1.00000",
     "MRC000000000001", 5411, "NORTHLAKE GROCERY COOPERATIVE", 1),
    (2, 0, "PURC", "IC", "212.50", "USD", "212.50", "1.00000",
     "MRC000000000012", 742, "WILLOW BEND VETERINARY CLINIC", 2),
    (3, 2, "CASH", "IC", "150.00", "USD", "150.00", "1.00000",
     "", 6011, "ATM CASH ADVANCE STAR NETWORK", 3),
    (4, 3, "PURC", "IC", "68.44", "USD", "68.44", "1.00000",
     "MRC000000000002", 5541, "HARBOUR POINT FUEL AND SERVICE", 1),
    (5, 3, "PURC", "IC", "2940.00", "EUR", "3204.11", "1.08983",
     "MRC000000000015", 7011, "AMSTERDAM CANAL HOTELS BV", 4),
    (6, 6, "RFND", "IC", "-45.90", "USD", "-45.90", "1.00000",
     "MRC000000000014", 5251, "REFUND SOUTH FERNWOOD HARDWARE", 1),
    (7, 6, "PURC", "IC", "119.87", "USD", "119.87", "1.00000",
     "MRC000000000014", 5251, "SOUTH FERNWOOD HARDWARE", 2),
    (8, 8, "PURC", "IC", "1899.00", "USD", "1899.00", "1.00000",
     "MRC000000000008", 5732, "BRIGHTON BEACH ELECTRONICS", 5),
    (9, 9, "PYMT", "BT", "-25.00", "USD", "-25.00", "1.00000",
     "", 0, "PAYMENT THANK YOU - ACH", 1),
    (10, 10, "PURC", "IC", "1450.00", "USD", "1450.00", "1.00000",
     "MRC000000000005", 5941, "BEND OUTDOOR SUPPLY WAREHOUSE", 3),
    (11, 10, "RFND", "IC", "-320.00", "USD", "-320.00", "1.00000",
     "MRC000000000005", 5941, "REFUND BEND OUTDOOR SUPPLY", 2),
    (12, 12, "PURC", "IC", "18.00", "USD", "18.00", "1.00000",
     "MRC000000000007", 7523, "CONGRESS AVENUE PARKING", 1),
    (13, 12, "PURC", "IC", "48200.00", "JPY", "310.00", "0.00643",
     "MRC000000000016", 4112, "TOKYO EXPRESS RAIL PASS", 4),
    (14, 13, "FEE ", "BT", "35.00", "USD", "35.00", "1.00000",
     "", 0, "LATE PAYMENT FEE CYCLE 202406", 1),
    (15, 16, "PURC", "IC", "76.40", "USD", "76.40", "1.00000",
     "MRC000000000011", 5942, "BEACON HILL BOOKSELLERS", 2),
    (16, 17, "CASH", "IC", "500.00", "USD", "500.00", "1.00000",
     "", 6011, "ATM CASH ADVANCE CIRRUS", 3),
    (17, 19, "PURC", "IC", "95.00", "USD", "95.00", "1.00000",
     "MRC000000000019", 5999, "PIONEER SQUARE POPUP MARKET", 1),
    (18, 5, "PURC", "IC", "42.18", "USD", "42.18", "1.00000",
     "MRC000000000003", 5912, "CHESTNUT HILL PHARMACY", 2),
    (19, 5, "PURC", "IC", "88.25", "USD", "88.25", "1.00000",
     "MRC000000000006", 5812, "RIVERWALK BISTRO", 1),
    (20, 4, "ADJT", "BT", "-12.44", "USD", "-12.44", "1.00000",
     "", 0, "GOODWILL ADJUSTMENT CASE 44120", 2),
    (21, 0, "PURC", "IC", "1204.00", "USD", "1204.00", "1.00000",
     "MRC000000000009", 5943, "COMMERCE SQUARE OFFICE SUPPLY", 6),
    (22, 14, "PURC", "IC", "8420.00", "USD", "8420.00", "1.00000",
     "MRC000000000004", 4511, "TRANSATLANTIC AIRWAYS", 8),
    (23, 12, "PURC", "IC", "249.99", "USD", "249.99", "1.00000",
     "MRC000000000017", 5816, "GLOBAL DIGITAL GOODS EXCHANGE", 2),
    (24, 17, "PURC", "IC", "640.10", "USD", "640.10", "1.00000",
     "MRC000000000018", 5542, "CASCADE RIDGE FLEET FUEL", 3),
    (25, 19, "PURC", "IC", "58.00", "USD", "58.00", "1.00000",
     "MRC000000000010", 7211, "POST STREET LAUNDRY", 1),
    (26, 0, "PYMT", "ON", "-300.00", "USD", "-300.00", "1.00000",
     "", 0, "PAYMENT THANK YOU - ONLINE", 1),
    (27, 6, "PYMT", "ON", "-250.00", "USD", "-250.00", "1.00000",
     "", 0, "PAYMENT THANK YOU - ONLINE", 1),
    (28, 10, "FEE ", "BT", "150.00", "USD", "150.00", "1.00000",
     "", 0, "ANNUAL FEE - BUSINESS 202406", 2),
    (29, 3, "INTR", "BT", "182.41", "USD", "182.41", "1.00000",
     "", 0, "PURCHASE INTEREST CHARGE", 3),
    (30, 13, "PURC", "IC", "112.00", "USD", "112.00", "1.00000",
     "MRC000000000013", 4814, "EASTGATE TELECOM SERVICES", 2),
    # the maximum leg count the ODO allows
    (31, 10, "PURC", "IC", "9840.55", "USD", "9840.55", "1.00000",
     "MRC000000000004", 4511, "TRANSATLANTIC AIRWAYS GROUP BKG", 12),
]


def _txn_legs(count, gross):
    """Split the amount over the legs, principal first, remainder on the last."""
    legs = []
    running = Decimal("0.00")
    for seq in range(1, count + 1):
        leg_type = "PRIN" if seq == 1 else LEG_TYPES[(seq - 1) % len(LEG_TYPES)]
        if seq == count:
            amount = Decimal(gross) - running
        elif seq == 1:
            amount = (Decimal(gross) * Decimal("0.80")).quantize(
                Decimal("0.01"))
        else:
            amount = (Decimal(gross) * Decimal("0.02")).quantize(
                Decimal("0.01"))
        running += amount
        gl = "GL%08d" % (4100 + seq)
        legs.append((seq, leg_type, amount, gl, "N" if seq != count else "N"))
    return legs


def write_txnpost(path):
    out = bytearray()
    lengths = set()
    for (seq, card_ix, type_cd, source, amount, ccy, billing, fx, merchant,
         mcc, description, leg_count) in TXNS:
        card = CARDS[card_ix]
        rec = bytearray()
        rec += alnum("TXN%013d" % seq, 16)
        rec += zoned(20240712, 8)
        rec += zoned(card[1], 11)
        rec += alnum(card[0], 16)
        rec += zoned(seq, 9)
        rec += alnum(type_cd, 4)
        rec += alnum(source, 2)
        rec += comp3(amount, 13, 2)
        rec += alnum(ccy, 3)
        rec += comp3(billing, 13, 2)
        rec += comp3(fx, 8, 5)
        rec += alnum(merchant, 15)
        rec += zoned(mcc, 4)
        rec += alnum(description, 40)
        rec += zoned(leg_count, 2)
        for leg_seq, leg_type, leg_amt, gl, reversed_flg in _txn_legs(
                leg_count, billing):
            rec += zoned(leg_seq, 2)
            rec += alnum(leg_type, 4)
            rec += comp3(leg_amt, 13, 2)
            rec += alnum(gl, 10)
            rec += alnum(reversed_flg, 1)
        rec += alnum("CBCRD04", 8)
        rec += alnum("2024-07-12-02.41.09.223104", 26)
        rec += alnum("CY202407", 8)
        rec += alnum("Y", 1)
        rec += alnum("N", 1)
        rec += alnum("", 16)
        lengths.add(len(rec))
        out += rdw(bytes(rec))
    _write(path, bytes(out), "V %d-%d" % (min(lengths) + 4, max(lengths) + 4))
    return min(lengths), max(lengths)


# ----------------------------------------------------------------------
# EBCDIC file - statement archive, layout CVSTMT01Y, RECFM=VB
# ----------------------------------------------------------------------

STMT_LINE_TEXT = [
    ("NORTHLAKE GROCERY COOPERATIVE", "84.19", "D"),
    ("WILLOW BEND VETERINARY CLINIC", "212.50", "D"),
    ("HARBOUR POINT FUEL AND SERVICE", "68.44", "D"),
    ("PAYMENT THANK YOU - ONLINE", "300.00", "C"),
    ("SOUTH FERNWOOD HARDWARE", "119.87", "D"),
    ("REFUND SOUTH FERNWOOD HARDWARE", "45.90", "C"),
    ("BEND OUTDOOR SUPPLY WAREHOUSE", "1450.00", "D"),
    ("CONGRESS AVENUE PARKING", "18.00", "D"),
    ("BEACON HILL BOOKSELLERS", "76.40", "D"),
    ("ATM CASH ADVANCE CIRRUS", "500.00", "D"),
    ("CASH ADVANCE FEE", "17.50", "D"),
    ("CHESTNUT HILL PHARMACY", "42.18", "D"),
    ("RIVERWALK BISTRO", "88.25", "D"),
    ("COMMERCE SQUARE OFFICE SUPPLY", "1204.00", "D"),
    ("TRANSATLANTIC AIRWAYS", "8420.00", "D"),
    ("POST STREET LAUNDRY", "58.00", "D"),
    ("EASTGATE TELECOM SERVICES", "112.00", "D"),
    ("PURCHASE INTEREST CHARGE", "182.41", "D"),
]

# account index, cycle date, stmt number, format, from, to, due, line count
STMTS = [
    (0, 20240703, 297, "PAPR", 20240604, 20240703, 20240728, 14),
    (1, 20240711, 251, "ELEC", 20240612, 20240711, 20240805, 6),
    (2, 20240718, 223, "PAPR", 20240619, 20240718, 20240812, 31),
    (3, 20240725, 333, "ELEC", 20240626, 20240725, 20240819, 9),
    (4, 20240703, 240, "PAPR", 20240604, 20240703, 20240728, 3),
    (6, 20240718, 299, "ELEC", 20240619, 20240718, 20240812, 22),
    (7, 20240725, 156, "ELEC", 20240626, 20240725, 20240819, 4),
    (8, 20240703, 273, "PAPR", 20240604, 20240703, 20240728, 118),
    (9, 20240711, 124, "ELEC", 20240612, 20240711, 20240805, 17),
    (10, 20240718, 194, "PAPR", 20240619, 20240718, 20240812, 8),
    (11, 20240725, 359, "PAPR", 20240626, 20240725, 20240819, 47),
    (13, 20240711, 94, "ELEC", 20240612, 20240711, 20240805, 12),
    (14, 20240718, 141, "ELEC", 20240619, 20240718, 20240812, 26),
    (15, 20240725, 88, "ELEC", 20240626, 20240725, 20240819, 1),
    (16, 20240703, 132, "PAPR", 20240604, 20240703, 20240728, 300),
]


def write_stmtarch(path):
    out = bytearray()
    lengths = set()
    for (acct_ix, cycle_date, number, fmt, period_from, period_to, due,
         line_count) in STMTS:
        acct = ACCOUNTS[acct_ix]
        rec = bytearray()
        rec += zoned(acct[0], 11)
        rec += zoned(cycle_date, 8)
        rec += zoned(number, 6)
        rec += zoned(acct[1], 9)
        rec += alnum(fmt, 4)
        rec += zoned(period_from, 8)
        rec += zoned(period_to, 8)
        rec += zoned(due, 8)
        rec += comp3(acct[9], 13, 2)      # open balance   (prior stmt bal)
        rec += comp3(acct[8], 13, 2)      # closing balance
        rec += comp3("1840.55", 13, 2)    # purchases
        rec += comp3(acct[11], 13, 2)     # cash advances
        rec += comp3(acct[13], 13, 2)     # payments
        rec += comp3("35.00", 11, 2)      # fees
        rec += comp3("182.41", 11, 2)     # interest
        rec += comp3(acct[12], 11, 2)     # minimum payment
        rec += comp3("15000.00", 13, 2)   # credit limit
        rec += comp3("12581.23", 13, 2)   # available credit
        rec += comp3("18.99000", 8, 5)    # apr
        rec += zoned(4210 + number, 9)    # reward points
        rec += zoned(line_count, 4)
        for line in range(line_count):
            desc, amount, dr_cr = STMT_LINE_TEXT[line % len(STMT_LINE_TEXT)]
            day = 1 + (line % 28)
            rec += zoned(period_from + day, 8)
            rec += zoned(period_from + day + 2, 8)
            rec += alnum(desc, 40)
            rec += alnum("REF%013d" % (number * 1000 + line), 16)
            rec += comp3(amount if dr_cr == "D" else "-" + amount, 13, 2)
            rec += alnum(dr_cr, 1)
        rec += alnum("CBBIL04", 8)
        rec += alnum("2024-07-12-04.55.31.907712", 26)
        rec += zoned(1 + line_count // 40, 3)
        rec += alnum("", 20)
        lengths.add(len(rec))
        out += rdw(bytes(rec))
    _write(path, bytes(out), "V %d-%d" % (min(lengths) + 4, max(lengths) + 4))
    return min(lengths), max(lengths)


# ----------------------------------------------------------------------
# EBCDIC file - general ledger posting interface, CARDSVC.GL_POSTING
#
# The GL takes a flat interface file, not a DB2 unload, and it has taken the
# amount as signed zoned decimal since long before the card system existed.
# The sign is an overpunch on the last byte: 1234.56 ends X'C6', which reads
# as the letter F, and -1234.56 ends X'D6', which reads as O.  A transfer
# that translates this file as text destroys every credit in it.
# ----------------------------------------------------------------------

GL_POSTINGS = [
    # seq, gl account, cost centre, dr/cr, amount, source txn seq,
    # account index, narrative
    (1, "GL00004101", "CC1100", "D", "84.19", 1, 0, "PURCHASE POSTING"),
    (2, "GL00004101", "CC1100", "D", "212.50", 2, 0, "PURCHASE POSTING"),
    (3, "GL00004205", "CC1100", "D", "150.00", 3, 1, "CASH ADVANCE POSTING"),
    (4, "GL00004410", "CC1300", "C", "-10.00", 3, 1, "CASH ADVANCE FEE"),
    (5, "GL00004101", "CC1100", "D", "68.44", 4, 2, "PURCHASE POSTING"),
    (6, "GL00004101", "CC1200", "D", "3204.11", 5, 2, "PURCHASE POSTING FX"),
    (7, "GL00004420", "CC1300", "C", "-64.08", 5, 2, "FOREIGN TXN FEE"),
    (8, "GL00004102", "CC1100", "C", "-45.90", 6, 6, "REFUND POSTING"),
    (9, "GL00004101", "CC1100", "D", "119.87", 7, 6, "PURCHASE POSTING"),
    (10, "GL00004101", "CC1100", "D", "1899.00", 8, 8, "PURCHASE POSTING"),
    (11, "GL00005110", "CC2100", "C", "-25.00", 9, 9, "PAYMENT RECEIVED"),
    (12, "GL00004101", "CC1100", "D", "1450.00", 10, 10, "PURCHASE POSTING"),
    (13, "GL00004102", "CC1100", "C", "-320.00", 11, 10, "REFUND POSTING"),
    (14, "GL00004430", "CC1300", "C", "-35.00", 14, 13, "LATE PAYMENT FEE"),
    (15, "GL00004205", "CC1100", "D", "500.00", 16, 14, "CASH ADVANCE POSTING"),
    (16, "GL00004410", "CC1300", "C", "-17.50", 16, 14, "CASH ADVANCE FEE"),
    (17, "GL00004101", "CC1100", "D", "95.00", 17, 16, "PURCHASE POSTING"),
    (18, "GL00005110", "CC2100", "C", "-300.00", 26, 0, "PAYMENT RECEIVED"),
    (19, "GL00005110", "CC2100", "C", "-250.00", 27, 6, "PAYMENT RECEIVED"),
    (20, "GL00004440", "CC1300", "C", "-150.00", 28, 8, "ANNUAL FEE"),
    (21, "GL00004510", "CC1400", "C", "-182.41", 29, 2, "PURCHASE INTEREST"),
    (22, "GL00004101", "CC1100", "D", "9840.55", 31, 8, "PURCHASE POSTING"),
    (23, "GL00009900", "CC9000", "D", "12.44", 20, 3, "GOODWILL ADJUSTMENT"),
    (24, "GL00004101", "CC1100", "D", "8420.00", 22, 11, "PURCHASE POSTING"),
]


def write_glpost(path):
    out = bytearray()
    for (seq, gl_account, cost_centre, dr_cr, amount, txn_seq, acct_ix,
         narrative) in GL_POSTINGS:
        acct = ACCOUNTS[acct_ix]
        rec = bytearray()
        rec += alnum("GLB202407120", 12)
        rec += zoned(seq, 9)
        rec += zoned(20240712, 8)
        rec += zoned(20240712, 8)
        rec += alnum(gl_account, 10)
        rec += alnum(cost_centre, 6)
        rec += alnum(dr_cr, 1)
        rec += zoned_signed(amount, 15, 2)
        rec += alnum("USD", 3)
        rec += alnum("TXN%013d" % txn_seq, 16)
        rec += zoned(acct[0], 11)
        rec += alnum(narrative, 40)
        rec += alnum("CY202407", 8)
        rec += alnum("N", 1)
        rec += alnum("CBCRD08", 8)
        rec += alnum("2024-07-12-03.58.44.120033", 26)
        out += rec
    _write(path, bytes(out), len(rec))
    return len(rec)


# ----------------------------------------------------------------------
# CSV companions
# ----------------------------------------------------------------------

def write_csv(name, header, rows):
    path = os.path.join(HERE, name)
    with open(path, "w", newline="") as handle:
        writer = csv.writer(handle, lineterminator="\n")
        writer.writerow(header)
        for row in rows:
            writer.writerow(row)
    print("%-16s %5d rows" % (name, len(rows)))


def _date(value):
    """CCYYMMDD integer to the ISO form DB2 LOAD expects, blank for zero."""
    if not value:
        return ""
    text = str(value)
    return "%s-%s-%s" % (text[0:4], text[4:6], text[6:8])


def write_all_csv():
    write_csv("parties.csv",
              ["PARTY_ID", "PARTY_TYPE", "LEGAL_NAME", "SHORT_NAME",
               "DOB_INCORP", "NATIONAL_ID", "TAX_ID", "DOMICILE_CTRY",
               "RESIDENCE_CTRY", "CITIZENSHIP", "PEP_FLG", "STATUS",
               "ONBOARD_DATE", "LINK_CNT"],
              [(p[0], p[1], p[2], p[3], _date(p[4]), p[5], p[6], p[7], p[8],
                p[9], p[10], p[11], _date(p[12]),
                sum(1 for c in CUSTOMERS if c[1] == p[0]))
               for p in PARTIES])

    write_csv("customers.csv",
              ["CUST_ID", "PARTY_ID", "PARTY_TYPE", "TITLE", "FIRST_NAME",
               "MIDDLE_INIT", "LAST_NAME", "DOB", "ADDR_LINE1", "CITY",
               "STATE_CD", "POSTAL_CD", "COUNTRY_CD", "SEGMENT_CD",
               "PEP_FLG", "VIP_FLG", "CUST_STATUS", "ONBOARD_DATE"],
              [(c[0], c[1],
                next(p[1] for p in PARTIES if p[0] == c[1]),
                c[2].strip(), c[3], c[4], c[5], _date(c[6]), c[7], c[8],
                c[9], c[10], c[11], c[12], c[13], c[14], c[15], _date(c[16]))
               for c in CUSTOMERS])

    write_csv("accounts.csv",
              ["ACCT_ID", "CUST_ID", "PARTY_ID", "PRODUCT_CD", "ACCT_STATUS",
               "OPEN_DATE", "CLOSE_DATE", "CURRENCY_CD", "CURR_BAL",
               "STMT_BAL", "PENDING_AUTH_AMT", "CASH_BAL", "MIN_PAY_DUE",
               "LAST_PAY_AMT", "LAST_PAY_DATE", "CYCLE_DAY",
               "LAST_CYCLE_DATE", "NEXT_CYCLE_DATE", "PAY_DUE_DATE",
               "DELQ_BUCKET", "DELQ_AMT", "STMT_COUNT", "BRANCH_CD"],
              [(a[0], a[1], a[2], a[3], a[4], _date(a[5]), _date(a[6]), a[7],
                a[8], a[9], a[10], a[11], a[12], a[13], _date(a[14]), a[15],
                _date(a[16]), _date(a[17]), _date(a[18]), a[19], a[20],
                a[21], a[22]) for a in ACCOUNTS])

    write_csv("cards.csv",
              ["CARD_NUM", "ACCT_ID", "CUST_ID", "EMBOSSED_NAME",
               "PRODUCT_CD", "CARD_STATUS", "EXPIRY_YYMM", "ISSUE_DATE",
               "ACTIVATION_YYMMDD", "LAST_USED_DATE", "CVV_IND", "PIN_TRIES",
               "REISSUE_CNT", "PREV_CARD_NUM", "BLOCK_REASON", "BLOCK_DATE"],
              [(c[0], c[1], c[2], c[3], c[4], c[5], c[6], _date(c[7]),
                str(c[8]).rjust(6, "0") if c[8] else "", _date(c[9]), c[10],
                c[11], c[12], c[13], c[14].strip(), _date(c[15]))
               for c in CARDS])

    write_csv("limits.csv",
              ["CARD_NUM", "LIMIT_TYPE", "LIMIT_AMT", "USED_AMT",
               "AVAIL_AMT", "DAILY_CNT_LIMIT", "DAILY_CNT_USED",
               "VELOCITY_WINDOW_MIN", "VELOCITY_MAX_CNT", "APR_PCT",
               "CASH_APR_PCT", "RISK_BAND", "LAST_REVIEW_DATE", "EFF_DATE",
               "EXP_DATE"],
              [(CARD_NUMS[l[0]], l[1], l[2], l[3], l[4], l[5], l[6], l[7],
                l[8], l[9], l[10], l[11], _date(l[12]), _date(l[13]),
                _date(l[14])) for l in LIMITS])

    write_csv("merchants.csv",
              ["MERCHANT_ID", "MERCHANT_NAME", "MCC", "ACQUIRER_ID",
               "COUNTRY_CD", "CITY", "HIGH_RISK_FLG", "CHARGEBACK_RATE",
               "SETTLE_ROUTE_CD", "STATUS", "ONBOARD_DATE"],
              [(m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9],
                _date(m[10])) for m in MERCHANTS])

    write_csv("fee_schedule.csv",
              ["PRODUCT_CD", "FEE_TYPE", "EFF_DATE", "EXP_DATE", "FLAT_AMT",
               "PCT_RATE", "MIN_AMT", "MAX_AMT", "WAIVER_RULE_CD",
               "HANDLER_PGM", "CURRENCY_CD", "DESCRIPTION"],
              [(f[0], f[1], _date(f[2]), _date(f[3]), f[4], f[5], f[6], f[7],
                f[8].strip(), f[9], f[10], f[11]) for f in FEE_SCHEDULE])

    write_csv("fraud_rules.csv",
              ["RULE_ID", "RULE_CLASS", "RULE_SEQ", "HANDLER_PGM",
               "THRESHOLD_AMT", "THRESHOLD_CNT", "THRESHOLD_PCT",
               "SCORE_POINTS", "ACTION_CD", "MCC_LIST", "COUNTRY_LIST",
               "EFF_DATE", "EXP_DATE", "ACTIVE_FLG", "DESCRIPTION"],
              [(r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9],
                r[10], _date(r[11]), _date(r[12]), r[13], r[14])
               for r in FRAUD_RULES])

    write_csv("kyc.csv",
              ["PARTY_ID", "KYC_SEQ", "KYC_STATUS", "KYC_LEVEL",
               "REVIEW_TYPE", "REVIEW_DATE", "NEXT_REVIEW_DATE",
               "EXPIRY_DATE", "ID_DOC_TYPE", "ID_DOC_REF", "ID_DOC_EXPIRY",
               "ADDR_VERIFIED_FLG", "SOURCE_OF_FUNDS_CD", "RISK_RATING",
               "ENHANCED_DD_FLG", "REVIEWED_BY"],
              [(k[0], k[1], k[2], k[3].strip(), k[4], _date(k[5]),
                _date(k[6]), _date(k[7]), k[8].strip(), k[9], _date(k[10]),
                k[11], k[12].strip(), k[13], k[14], k[15]) for k in KYC])

    write_csv("sanctions.csv",
              ["LIST_CD", "ENTRY_ID", "ENTITY_NAME", "ENTITY_TYPE",
               "COUNTRY_CD", "DOB", "PROGRAM_CD", "LISTED_DATE",
               "DELISTED_DATE", "ACTIVE_FLG", "LOAD_JOB"],
              [(s[0].strip(), s[1], s[2], s[3], s[4], _date(s[5]), s[6],
                _date(s[7]), _date(s[8]), s[9], s[10]) for s in SANCTIONS])


# ----------------------------------------------------------------------

def _write(name, data, lrecl):
    path = os.path.join(HERE, name)
    with open(path, "wb") as handle:
        handle.write(data)
    print("%-16s %6d bytes   lrecl %s" % (name, len(data), lrecl))


def main():
    write_cardmast("cardmast.ebc")
    write_acctmast("acctmast.ebc")
    write_authextr("authextr.ebc")
    write_txnpost("txnpost.vb")
    write_stmtarch("stmtarch.vb")
    write_glpost("glpost.ebc")
    write_all_csv()


if __name__ == "__main__":
    main()
