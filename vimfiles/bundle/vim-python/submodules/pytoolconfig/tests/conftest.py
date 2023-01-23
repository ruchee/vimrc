from __future__ import annotations

from pathlib import Path

from pytest import fixture


@fixture
def cwd():
    return Path(__file__).parent / "configfiles"
