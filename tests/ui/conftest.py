import pytest


@pytest.fixture
def site_url(base_url):
    return base_url or "file://" + __import__("pathlib").Path("site/index.html").resolve().as_posix()
