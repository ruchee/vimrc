from __future__ import annotations

import sys

from pytoolconfig.sources import IniConfig, PyProject, SetupConfig


def test_base_pyproject(cwd):
    pyproject = PyProject(cwd, "pytoolconfig", [])
    assert pyproject.parse()["formatter"] == "black"
    universal = pyproject.universalconfig()
    assert universal.min_py_version == (3, 7)
    assert universal.max_py_version == (sys.version_info.major, sys.version_info.minor)
    assert universal.formatter == "black"
    assert "sphinx" in [dep.name for dep in universal.optional_dependencies["doc"]]


def test_empty_pyproject(tmp_path):
    pyproject_toml = tmp_path / "pyproject.toml"
    with pyproject_toml.open(mode="w") as f:
        f.write("[spam]")
    pyproject = PyProject(tmp_path, "pytoolconfig", [])
    pyproject.parse()
    pyproject.universalconfig()

    with pyproject_toml.open(mode="w") as f:
        pass
    pyproject.parse()


def test_base_ini(cwd):
    config = IniConfig(cwd, "test_config.ini", "pytoolconfig").parse()
    assert config["formatter"] == "yapf"


def test_setup_cfg(cwd):
    setup_cfg = SetupConfig(cwd, "pytoolconfig")
    assert setup_cfg.parse()
