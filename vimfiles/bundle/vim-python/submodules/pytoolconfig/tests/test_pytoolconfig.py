import os
import sys
from argparse import ArgumentParser
from dataclasses import dataclass, fields
from typing import Tuple

import pytest

from pytoolconfig import PyToolConfig, UniversalKey, field
from pytoolconfig.sources import IniConfig
from pytoolconfig.universal_config import UniversalConfig


@dataclass
class SimpleModel:
    formatter: str = "NOT THIS"


@dataclass
class EmptyModel:
    pass


@dataclass
class SubTool:
    foo: str = field(description="foobar", default="lo")


@dataclass
class NestedModel:
    subtool: SubTool = field(default_factory=lambda: SubTool())
    foo_other: str = field(description="w", default="no", command_line=("--foo", "-f"))

    target: Tuple[int, int] = field(
        description="Minimum python version",
        default=(3, 1),
        universal_config=UniversalKey.min_py_version,
    )


def test_simple(cwd):
    config = PyToolConfig("pytoolconfig", cwd, SimpleModel)
    result = config.parse()
    assert result.formatter == "black"


def test_invalid_key(cwd):
    config = PyToolConfig("pytoolconfig", cwd, EmptyModel)
    result = config.parse()
    with pytest.raises(AttributeError):
        assert result.formatter


def test_nested(cwd):
    config = PyToolConfig(
        "bogus",
        cwd,
        NestedModel,
        custom_sources=[IniConfig(cwd, "test_config.ini", "bogus")],
    )
    result = config.parse()
    assert result.subtool.foo == "barr"
    config = PyToolConfig(
        "bogus",
        cwd,
        NestedModel,
    )
    result = config.parse()
    # Default argument
    assert result.subtool.foo == "lo"
    assert result.target == (3, 7)


def test_cli(cwd):
    config = PyToolConfig("bogus", cwd, NestedModel, arg_parser=ArgumentParser())
    result = config.parse()
    assert result.subtool.foo == "lo"
    result = config.parse(["--foo", "bar"])
    assert result.foo_other == "bar"


def test_global(cwd):
    if sys.platform != "linux":
        pytest.skip()
    os.environ["XDG_CONFIG_HOME"] = cwd.as_posix()
    config = PyToolConfig("bogus", cwd, NestedModel, global_config=True)
    result = config.parse()
    assert result.subtool.foo == "ajf"


def test_fall_through(cwd):
    config = PyToolConfig(
        "fall_through",
        cwd,
        NestedModel,
        custom_sources=[IniConfig(cwd, "test_config.ini", "bogus")],
        fall_through=True,
    )
    result = config.parse()
    assert result.subtool.foo == "barr"
    assert result.foo_other == "ba"


def test_universal_key():
    assert [field.name for field in fields(UniversalConfig)] == [
        name for name in UniversalKey.__members__
    ]
