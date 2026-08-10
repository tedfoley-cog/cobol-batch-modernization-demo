PYTHON ?= $(if $(wildcard .venv/bin/python),.venv/bin/python,python3)

.PHONY: fetch install validate report parity test test-ui serve

fetch:
	$(PYTHON) tools/fetch_estate.py

install:
	$(PYTHON) -m pip install -e ".[dev]"

validate: fetch
	$(PYTHON) tools/validate.py

report:
	$(PYTHON) tools/report.py

parity:
	$(PYTHON) tools/parity.py harness/fixtures artifacts/reference-example/layout-spec.json artifacts/reference-example/parity-report.json

test:
	$(PYTHON) -m pytest

test-ui:
	$(PYTHON) -m pytest tests/ui -m ui

serve:
	$(PYTHON) -m http.server 8000 --directory site
