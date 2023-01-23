from pytoolconfig.utils import find_config_file, parse_dependencies


def test_find_pyproject(cwd):
    result = find_config_file(cwd.parent, "pyproject.toml", [".git"])
    assert result
    assert result == cwd.parent.parent / "pyproject.toml"


def test_parse_deps():
    deps = [
        'tomli>=2.0; python_version < "3.11"',
        "packaging>=21.3",
        'typing-extensions; python_version < "3.9"',
    ]
    assert [dep.name for dep in parse_dependencies(deps)] == [
        "tomli",
        "packaging",
        "typing-extensions",
    ]
