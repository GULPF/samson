import std / [os, strutils, terminal, json, options, tables]
import .. / src / samson,
       .. / src / samson / private / [parser, jtrees]

var allOk = true

proc `$`(t: JTree, idx: JNodeIdx): string =
  let n = t.nodes[idx]
  case n.kind
  of nkArray:
    result.add "["
    for idx, itemIdx in n.items:
      if idx > 0:
        result.add ", "
      result.add `$`(t, itemIdx)
    result.add "]"
  of nkObject:
    result.add "{"
    for key, valIdx in n.kvpairs:
      result.add toJson5(key)
      result.add ": "
      result.add `$`(t, valIdx)
      result.add ", "
    if n.kvpairs.len > 0:
      result.setLen(result.len - 2)
    result.add "}"
  of nkNumber:
    result = toJson5(n.numVal)
  of nkInt64:
    result = toJson5(n.int64Val)
  of nkString:
    result = toJson5(n.strVal)
  of nkBool:
    result = toJson5(n.boolVal)
  of nkNull:
    result = toJson5(none(bool))
  of nkEmpty:
    discard

proc `$`(t: JTree): string =
  `$`(t, 0)

block:
  styledEcho fgBlue, "Running official tests\n"
  var pass = 0
  var fail = 0

  for file in walkDirRec("./tests/json5-official-tests"):
    if ".git" in file:
      continue
    if file.endsWith(".json") or file.endsWith(".json5"):
      let content = readFile(file)
      try:
        discard parseJson5(content)
        pass.inc
      except Exception:
        fail.inc
        styledEcho fgRed, file
    elif file.endsWith(".txt") or file.endsWith(".js"):
      let content = readFile(file)
      try:
        discard parseJson5(content)
        fail.inc
        styledEcho fgRed, file, fgDefault, " (expected to fail but succeeded)"
      except Exception:
        pass.inc

  allOk = allOk and fail == 0
  echo "PASS: " & $pass & " / " & $(pass + fail)

echo ""

block:
  type
    ErrorCase = object
      error: string
      input: string

  styledEcho fgBlue, "Running error message tests\n"

  var pass = 0
  var fail = 0

  let content = readFile("./tests/json5-error-messages.json")
  let suite = parseJson(content).to(OrderedTable[string, ErrorCase])

  for k, v in suite:
    try:
      discard parseJson5(v.input)
      styledEcho fgRed, k, fgDefault, " (parsing succeeded)"
      fail.inc
    except Exception as e:
      if e.msg != v.error:
        styledEcho fgRed, k
        echo "Output differs"
        echo "- - -"
        echo v.error
        echo ""
        echo e.msg
        echo "- - -"
        fail.inc
      else:
        pass.inc
  
  allOk = allOk and fail == 0  
  echo "PASS: " & $pass & " / " & $(pass + fail)

echo ""

block:
  type
    TestCase = object
      output: Option[string]
      input: string

  styledEcho fgBlue, "Running inofficial tests\n"

  var pass = 0
  var fail = 0

  let content = readFile("./tests/json5-inofficial-tests.json")
  let suite = parseJson(content).to(OrderedTable[string, TestCase])

  for k, v in suite:
    var excep: ref Exception
    let (err, tree) =
      try:
        let tree = parseJson5(v.input)
        (false, tree)
      except Exception as e:
        excep = e
        (true, JTree())

    let treeStr =
      try:
        if not err:
          $tree
        else:
          ""
      except:
        echo "Failed to stringify."
        ""

    if v.output.isNone and not err:
      styledEcho fgRed, k, fgDefault, " (expected to fail but succeeded)"
      fail.inc
    elif v.output.isSome and v.output.get != treeStr:
      # if err:
      #   raise excep
      # else:
      styledEcho fgRed, k
      echo "Output differs"
      echo "- - -"
      echo v.output.get
      echo ""
      echo treeStr
      echo "- - -"
      fail.inc
    else:
      pass.inc

  allOk = allOk and fail == 0
  echo "PASS: " & $pass & " / " & $(pass + fail)

echo ""

styledEcho fgBlue, "Running API tests"

include apitests

if not allOk:
  styledEcho fgRed, "= = = = = = =\nTests failed\n= = = = = = ="