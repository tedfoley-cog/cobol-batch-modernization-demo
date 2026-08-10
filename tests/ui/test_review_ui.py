import re

import pytest
from playwright.sync_api import Page, expect


pytestmark = pytest.mark.ui


def test_cross_tab_navigation(page: Page, site_url: str):
    page.goto(site_url)
    for tab, text in [("Streams", "SETLUSD"), ("Requirements", "TRDPB001"),
                      ("Data Layer", "TBTRDSTQ"), ("Plan", "CH-SETL-01"),
                      ("Parity", "TRDPB001"), ("Schema", "Rendered schemas")]:
        page.get_by_role("button", name=tab).click()
        expect(page.get_by_text(re.compile(text, re.I)).first).to_be_visible()


def test_requirement_authoring_validation_and_append(page: Page, site_url: str):
    page.goto(site_url)
    page.get_by_role("button", name="Schema").click()
    page.locator("input[name=id]").fill("bad")
    page.locator("textarea[name=statement]").fill("The system shall do something.")
    page.get_by_role("button", name="Validate and append").click()
    expect(page.locator("#form-msg")).to_contain_text("profile.id_patterns.fr")
    page.locator("input[name=id]").fill("FR-SETL-099")
    page.locator("textarea[name=statement]").fill("When a test starts, the system shall pass.")
    page.get_by_role("button", name="Validate and append").click()
    expect(page.locator("#form-msg")).to_contain_text("appended")


def test_filter_sort_requirements(page: Page, site_url: str):
    page.goto(site_url)
    page.get_by_role("button", name="Requirements").click()
    page.locator("#pattern").select_option("unwanted_behaviour")
    expect(page.locator("#req-table")).to_contain_text("FR-SETL-009")
    expect(page.locator("#req-table")).not_to_contain_text("FR-SETL-001")


def test_evidence_drawer(page: Page, site_url: str):
    page.goto(site_url)
    page.get_by_role("button", name="Requirements").click()
    page.get_by_role("cell", name="FR-SETL-008", exact=True).click()
    expect(page.locator(".drawer")).to_contain_text("COBOL evidence")
    expect(page.locator(".snippet")).to_contain_text("2000-ACCOUNTING")
    page.get_by_role("button", name="Close").click()
    expect(page.locator(".drawer")).to_have_count(0)


def test_golden_path_and_packed_amount(page: Page, site_url: str):
    page.goto(site_url)
    page.get_by_role("button", name="Streams").click()
    page.locator("[data-step=STEP01]").click()
    expect(page.locator(".drawer")).to_contain_text("CLFRPLAN")
    page.get_by_role("button", name="Close").click()
    page.get_by_role("button", name="Data Layer").click()
    expect(page.locator("body")).to_contain_text("TBTRDSTQ")
    expect(page.locator("body")).to_contain_text("generated from DDL")
    page.get_by_role("button", name="Parity").click()
    expect(page.locator("body")).to_contain_text("123.45")
    expect(page.locator("body")).to_contain_text("601")
