PYTHON ?= $(if $(wildcard .venv/bin/python),.venv/bin/python,python3)

.PHONY: fetch install dclgen validate report parity test test-ui serve

fetch:
	$(PYTHON) tools/fetch_estate.py

dclgen: fetch
	$(PYTHON) tools/dclgen.py

install:
	$(PYTHON) -m pip install -e ".[dev]"

validate: dclgen
	$(PYTHON) tools/validate.py

report: dclgen
	$(PYTHON) tools/report.py

parity: dclgen
	$(PYTHON) tools/parity.py harness/fixtures artifacts/reference-example/layout-spec.json artifacts/reference-example/parity-report.json

test:
	$(PYTHON) -m pytest

test-ui:
	$(PYTHON) -m pytest tests/ui -m ui

serve:
	$(PYTHON) -m http.server 8000 --directory site
