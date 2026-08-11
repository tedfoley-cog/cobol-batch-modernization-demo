# Card Services Mainframe Estate

Production source for the card issuing platform.

Two application modules share the estate:

| Module     | Owner              | Databases | Online | Batch                          |
|------------|--------------------|-----------|--------|--------------------------------|
| `CARDSVC`  | Card Systems       | `CARDDB`  | `CA00` | `CARDNITE`, `CARDBILL`, `CARDREF` |
| `PARTYRSK` | Party & Risk       | `PARTYDB` | `CA00` (linked) | `PARTYWK`             |

## Layout

```
app/cpy/              copybooks shared across the estate
app/cardsvc/cbl/      CARDSVC COBOL sources
app/cardsvc/bms/      BMS map sources
app/partyrsk/cbl/     PARTYRSK COBOL sources
app/partyrsk/cpy/     PARTYRSK private copybooks
app/jcl/              production JCL, PROCs and IDCAMS members
app/data/             sample datasets (EBCDIC, cp037)
db2/ddl/              DDL, seed and grant members
sched/                scheduler definitions
tools/bms3270/        offline terminal for the CA00 maps (not shipped to z/OS)
```

## Viewing the CA00 screens off the mainframe

`tools/bms3270` renders the BMS maps as 24x80 screens in an ordinary shell and
lets you page through the transaction:

```
python3 -m tools.bms3270
```

It reads the map sources, the `PGM_ROUTE` seed and the sample datasets, but it
does not run the COBOL - see `tools/bms3270/README.md`.

## Install order

1. `db2/ddl/00_CARDDB.sql`, `db2/ddl/01_PARTYDB.sql` — storage groups, databases, tablespaces
2. `db2/ddl/10_CARDDB_TABLES.sql`, `db2/ddl/11_PARTYDB_TABLES.sql`
3. `db2/ddl/20_INDEXES.sql`
4. `db2/ddl/30_FOREIGN_KEYS.sql`
5. `db2/ddl/40_SEED_PGM_ROUTE.sql` — dispatch control table, required before the online region starts
6. `db2/ddl/50_GRANTS.sql`
7. `app/jcl/vsam/DEFCARD.jcl`, `app/jcl/vsam/DEFPARTY.jcl`

## Compile options

`CARDSVC` and `PARTYRSK` are compiled `NODYNAM` except the dispatchers `CACRD90`
and `CBCRD90` and anything they reach, which require `DYNAM`. Both modules bind
into their own collection (`CARDCOLL`, `PRTYCOLL`).

## Contacts

Card Systems on-call for `CARDSVC` and the nightly cycle. Party & Risk on-call
for `PARTYRSK`, `PARTYWK` and the sanctions feed.
