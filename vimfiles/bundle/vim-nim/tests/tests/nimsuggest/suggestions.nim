import
    strutils,
    sequtils,
    util

static:
    test "outline", "tests/tmodule.nim", CompletionV2, 1, 1:
        assert results.len == 10
        assert results[1].location == "tmodule.Vec"
        assert results[1].module == "tmodule"
        assert results[1].lname == "x"

    # v1 is broken
    test "use", "tests/tmodule.nim", CompletionV1, 14, 8:
        assert results.len == 3 # Wrong (only finds results under search)
    test "use", "tests/tmodule.nim", CompletionV2, 14, 8:
        assert results.len == 9
    test "use", "tests/tmodule.nim", CompletionV1, 9, 22:
        assert results.len == 1 # Wrong
    test("use", "tests/tmodule.nim", CompletionV2, 9, 22):
        assert results.len == 9

    # v2 is broken
    test("def", "tests/tmodule.nim", CompletionV1, 21, 8):
        assert results.len == 1
        assert results[0].doc == "\"\""
    test("def", "tests/tmodule.nim", CompletionV2, 21, 8):
        assert results.len == 0 # Wrong (should find one)

    test("sug", "tests/tmodule.nim", CompletionV1, 22, 3):
        assert results.len == 4
    test("sug", "tests/tmodule.nim", CompletionV2, 22, 3):
        assert results.len == 4

    # con returns the function def??
    test("con", "tests/tmodule.nim", CompletionV1, 18, 7):
        assert results.len == 1
    test("con", "tests/tmodule.nim", CompletionV2, 18, 7):
        assert results.len == 1
