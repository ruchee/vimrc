import
    sequtils,
    strutils


proc last[T](a: seq[T]): T = a[a.len - 1]

proc slice[T](a: seq[T], hi: static[int]): auto = slice(a, 0, hi)

proc slice[T](a: seq[T], lo, hi: static[int]): seq[T] =
    result = @[]
    let realHi = if hi < 0: a.len + hi else: hi
    for idx in lo..min(realHi, a.len - 1):
        result.add(a[idx])


type
    CompletionV1* = object
        values: array[10, string]
    CompletionV2* = object
        values: array[10, string]
    Completion* = CompletionV1 | CompletionV2


proc len*(c: Completion): auto = c.values.len
proc `$`*(c: seq[Completion]): string =
    result = ""
    for r in c:
        result &= $r & "\n"
        

# Create accessors for array-types
template access(name: expr, idx: static[int]): expr =
    proc name*(c: Completion): string = c.values[idx]

access(ctype, 0)
access(kind, 1)
access(symbol, 2)
access(signature, 3)
access(file, 4)
access(line, 5)
access(col, 6)
access(doc, 7)
access(random, 8)
access(`$`, 9)
access(str, 9)


proc lname*(c: Completion): string =
    c.symbol.split(".").last

proc location*(c: Completion): string =
    c.symbol.split(".").slice(0, -2).join(".")

proc module*(c: Completion): string =
    c.symbol.split(".").slice(0, -3).join(".")


proc cmd*(CT: typedesc, source: string, ctype: string, line, col: int): string {.compileTime.} =
    when CT is CompletionV1:
        let paramsV2 = ""
    else:
        let paramsV2 = " --v2 "

    "echo \"" &
        ctype &
        " " &
        source &
        ":" & $line & ":" & $col & "\"" &
        " | nimsuggest --stdin " & paramsV2 & source & "\n"


proc getResults*[T: Completion](f: string, ctype: string, line, col: int = 1): seq[T] {.compileTime.} =
    result = @[]
    let command = cmd(T, f, ctype, line, col)
    let lines = command.staticExec.split("\n")

    for line in lines[4..lines.len - 2]:
        var completion: T
        let raw = line.split("\t")
        for idx in 0..completion.len - 1:
            if idx < raw.len:
                completion.values[9] = line
                completion.values[idx] = raw[idx]
        result.add(completion)
            

template test*(name, file: static[string], t: typedesc, line, col: int, body: expr): expr =
    block:
        let results {.inject.} = getResults[t](file, name, line, col)
        body
