##[
  See also
  ========
  * `pragmas module <pragmas.html>`_ 
  * `errors module <errors.html>`_ (exported by this module)
]##

import std / [unicode, strutils, tables, math, options, macros, times, sets]
import samson / private / [jtrees, parser, lexer, xunicode, xtypetraits],
  samson / [pragmas, errors],
  samson / experimental / [eithers, jsonvalues]

export errors

type
  PlainObject = (object) and
    (not (Option|Either|Table|OrderedTable|HashSet|OrderedSet|DateTime|Time))

  SupportedIntegerTypes = int8|int16|int32|int|int64|uint8|uint16|uint32

const DefaultDateTimeFormat = "uuuu-MM-dd'T'HH:mm:ss'.'fffffffffzzz"

template len(s: set): int = card(s)

template resolveFieldName(fieldName: string, fieldSym: untyped): string =
  when hasCustomPragma(fieldSym, jsonFieldName):
    getCustomPragmaVal(fieldSym, jsonFieldName)
  else:
    fieldName

template shouldExcludeField(fieldSym: untyped): bool =
  hasCustomPragma(fieldSym, jsonExclude)

template resolveDateTimeFormat(fieldSym: untyped): string =
  if hasCustomPragma(fieldSym, jsonDateTimeFormat):
    getCustomPragmaVal(fieldSym, jsonDateTimeFormat)
  else:
    DefaultDateTimeFormat

proc schemaError(expected: typedesc, actual: string) {.noReturn.} =
  raise newException(JsonSchemaError, "Failed to deserialize to type. " &
    "Expected " & $expected & " but found " & actual)

# Used for better error messages
proc `$`(t: JTree, idx: JNodeIdx): string

# Nim->JSON5

proc addJson5String(result: var string, value: string) =
  result.add '\"'
  var pos = 0
  while pos < value.len:
    case value[pos]
    of '"':
      result.add "\\\""
      pos.inc
    of '\\':
      result.add "\\\\"
      pos.inc
    of '\x08':
      result.add "\\b"
      pos.inc
    of '\x0C':
      result.add "\\f"
      pos.inc
    of '\x0A':
      result.add "\\n"
      pos.inc
    of '\x0D':
      result.add "\\r"
      pos.inc
    of '\x09':
      result.add "\\t"
      pos.inc
    of {'\32'..'\126'} - {'\\', '\"'}:
      result.add value[pos]
      pos.inc
    of '\128'..high(char):
      let (rune, len) = value.runeAndLenAt(pos, allowSurrogate = true)
      # LS and PS are actually valid here, but they are not valid here in JS
      # so it's generally recommended to escape them.
      # Surrogates are invalid in UTF-8 strings, so it could be argued that
      # they should be replaced with 0xFFFD. However, the JSON5 spec _does_
      # allow lone surrogates and we do support parsing them, so we
      # need to support generating them as well.
      if rune == LS or rune == PS or rune in SurrogateRange:
        result.add "\\u"
        result.add toHex(rune.int32, 4).toLowerAscii
      else:
        result.add $rune
      pos.inc len
    else:
      let (rune, len) = value.runeAndLenAt(pos)
      result.add "\\u"
      result.add toHex(rune.int32, 4).toLowerAscii
      pos.inc len
  result.add '\"'

proc addJson5DateTime(result: var string, value: DateTime|Time,
    f = DefaultDateTimeFormat) =
  result.addJson5String value.utc.format(f)

proc addJson5[T](result: var string, value: T) =
  when T is Option:
    if value.isSome:
      result.addJson5(value.get)
    else:
      result.add "null"
  elif T is JsonValue:
    case value.kind
    of jsonNull:
      result.add "null"
    of jsonInteger:
      result.add $value.getInt64
    of jsonFloat:
      result.addJson5 value.getFloat
    of jsonString:
      result.addJson5 value.getString
    of jsonArray:
      result.addJson5 value.getSeq
    of jsonObject:
      result.addJson5 value.getTable
  elif T is Either:
    type A = T.generic(0)
    type B = T.generic(1)
    if value.isType(A):
      result.addJson5(value.get(A))
    else:
      result.addJson5(value.get(B))
  elif T is bool:
    result.add $value
  elif T is SupportedIntegerTypes:
    result.add $value
  elif T is SomeFloat:
    case classify(value)
    of fcInf:
      result.add "Infinity"
    of fcNegInf:
      result.add "-Infinity"
    of fcNan:
      result.add "NaN"
    of fcNegZero:
      result.add "-0.0"
    else:
      result.add $value
  elif T is enum:
    result.add $value.int
  elif T is Table|OrderedTable:
    result.add "{"
    for k, v in value:
      result.addJson5 k
      result.add ": "
      result.addJson5(v)
      result.add ", "
    if value.len > 0:
      result.setLen(result.len - 2)
    result.add "}"
  elif T is seq|array|set|HashSet|OrderedSet:
    result.add "["
    for v in value:
      result.addJson5(v)
      result.add ", "
    if value.len > 0:
      result.setLen(result.len - 2)
    result.add "]"
  elif T is DateTime|Time:
    result.addJson5DateTime(value)
  elif T is string:
    result.addJson5String(value)
  elif T is char:
    result.addJson5String($value)
  elif T is PlainObject:
    result.add "{"
    for fieldName, fieldSym in value.fieldPairs:
      if not shouldExcludeField(fieldSym):
        let resolvedFieldName = resolveFieldName(fieldName, fieldSym)
        result.addJson5 resolvedFieldName
        result.add ": "
        when type(fieldSym) is DateTime|Time:
          const f = resolveDateTimeFormat(fieldSym)
          result.addJson5DateTime fieldSym, f
        elif type(fieldSym) is enum and
            hasCustomPragma(fieldSym, jsonStringEnum):
          result.addJson5String $fieldSym
        else:
          result.addJson5 fieldSym
        result.add ", "
    result.setLen(result.len - 2)
    result.add "}"
  elif T is tuple:
    result.add "["
    for val in value.fields:
      result.addJson5 val
      result.add ", "
    result.setLen(result.len - 2)
    result.add "]"
  else:
    {.error: "Unsupported type: " & $T.}

# This needs to be implemented as an overload due to limitations in Nim
proc addJson5(result: var string, value: type(nil)) =
  result.add "null"

# JSON5->Nim

proc dateTimeFromJson5(tree: JTree, idx: JnodeIdx,
    T: typedesc[DateTime|Time], f: string = DefaultDateTimeFormat): T =
  if tree.nodes[idx].kind != nkString:
    schemaError(T, `$`(tree, idx))
  else:
    when T is DateTime:
      result = parse(tree.nodes[idx].strVal, f)
    else:
      result = parseTime(tree.nodes[idx].strVal, f, local())

proc fromJson5Impl(tree: JTree, idx: JNodeIdx, T: typedesc): T =
  template error {.used.} =
    schemaError(T, `$`(tree, idx))

  when T is DateTime|Time:
    result = dateTimeFromJson5(tree, idx, T)
  elif T is Table|OrderedTable:
    when T.generic(0) isnot string:
      {.error: "Tables must be indexed by strings " &
        "for compatibility with samson".}
    if tree.nodes[idx].kind != nkObject:
      error()
    else:
      let size = tables.rightSize(tree.nodes[idx].kvpairs.len)
      when T is Table:
        result = initTable[string, T.generic(1)](size)
      else:
        result = initOrderedTable[string, T.generic(1)](size)
      for k, v in tree.nodes[idx].kvpairs:
        result[k] = fromJson5Impl(tree, v, T.generic(1))
  elif T is HashSet|OrderedSet:
    if tree.nodes[idx].kind != nkArray:
      error()
    else:
      let size = sets.rightSize(tree.nodes[idx].items.len)
      when T is HashSet:
        result = initHashSet[T.generic(0)](size)
      else:
        result = initOrderedSet[T.generic(0)](size)
      for idx in tree.nodes[idx].items:
        let value = fromJson5Impl(tree, idx, T.generic(0))
        if value in result:
          error()
        result.incl value
  elif T is seq:
    if tree.nodes[idx].kind != nkArray:
      error()
    else:
      result = newSeq[T.generic(0)](tree.nodes[idx].items.len)
      for idx, itemIdx in tree.nodes[idx].items:
        result[idx] = fromJson5Impl(tree, itemIdx, T.generic(0))
  elif T is array:
    if tree.nodes[idx].kind != nkArray or
        len(result) != tree.nodes[idx].items.len:
      error()
    else:
      for idx, itemIdx in tree.nodes[idx].items:
        result[idx] = fromJson5Impl(tree, itemIdx, T.generic(1))
  elif T is set:
    if tree.nodes[idx].kind != nkArray:
      error()
    else:
      for itemIdx in tree.nodes[idx].items:
        let v = fromJson5Impl(tree, itemIdx, T.generic(0))
        if v in result:
          error()
        result.incl v
  elif T is string:
    if tree.nodes[idx].kind != nkString:
      error()
    else:
      shallowCopy(result, tree.nodes[idx].strVal)
  elif T is char:
    if tree.nodes[idx].kind != nkString or
        tree.nodes[idx].strVal.len != 1:
      error()
    else:
      result = tree.nodes[idx].strVal[0]
  elif T is range:
    type UnderlyingType = rangeUnderlyingType(T)
    when UnderlyingType is Ordinal:
      type StorageType = int64
    else:
      type StorageType = float64
    let ord = fromJson5Impl(tree, idx, StorageType)
    if ord < T.low.StorageType or T.high.StorageType < ord:
      error()
    result = ord.T
  elif T is SupportedIntegerTypes:
    if tree.nodes[idx].kind != nkInt64:
      error()
    else:
      let ord = tree.nodes[idx].int64Val
      if ord < T.low.int64 or T.high.int64 < ord:
        error()
      result = ord.T
  elif T is SomeFloat:
    case tree.nodes[idx].kind
    of nkInt64:
      result = tree.nodes[idx].int64Val.T
    of nkNumber:
      result = tree.nodes[idx].numVal.T
    else:
      error()
  elif T is bool:
    if tree.nodes[idx].kind != nkBool:
      error()
    else:
      result = tree.nodes[idx].boolVal
  elif T is Option:
    if tree.nodes[idx].kind != nkNull:
      result = some(fromJson5Impl(tree, idx, T.generic(0)))
  elif T is Either:
    try:
      either[T.generic(0), T.generic(1)](fromJson5Impl(tree, idx, T.generic(0)))
    except:
      either[T.generic(0), T.generic(1)](fromJson5Impl(tree, idx, T.generic(1)))
  elif T is enum:
    if tree.nodes[idx].kind != nkInt64:
      error()
    else:
      let ord = tree.nodes[idx].int64Val
      if ord < T.low.int64 or T.high.int64 < ord:
        error()
      result = ord.T
  elif T is PlainObject:
    if tree.nodes[idx].kind != nkObject:
      error()
    else:
      var nIncludedFields = 0
      for fieldName, fieldSym in result.fieldPairs:
        if not shouldExcludeField(fieldSym):
          nIncludedFields.inc
          let resolvedFieldName = resolveFieldName(fieldName, fieldSym)
          if resolvedFieldName notin tree.nodes[idx].kvpairs:
            if type(fieldSym) is Option:
              # This is semantically wrong but it makes the check
              # down below work with missing optional fields
              nIncludedFields.dec
            else:
              error()
          else:
            let fieldIdx = tree.nodes[idx].kvpairs[resolvedFieldName]
            when type(fieldSym) is DateTime|Time:
              const f = resolveDateTimeFormat(fieldSym)
              fieldSym = dateTimeFromJson5(tree, fieldIdx, type(fieldSym), f)
            elif type(fieldSym) is enum and
                hasCustomPragma(fieldSym, jsonStringEnum):
              let strVal = fromJson5Impl(tree, fieldIdx, string)
              try:
                fieldSym = parseEnum[type(fieldSym)](strVal)
              except ValueError:
                error()
            else:
              fieldSym = fromJson5Impl(tree, fieldIdx, type(fieldSym))
      # TODO: This check should be optional since it breaks
      #       forward compatibility in the JSON schema
      if nIncludedFields != tree.nodes[idx].kvpairs.len:
        error()
  elif T is tuple:
    let expectedLen = tupleLen(T)
    if tree.nodes[idx].kind != nkArray:
      error()
    elif tree.nodes[idx].items.len != expectedLen:
      error()
    else:
      var tupleFieldIdx = 0
      for fieldName, fieldSym in result.fieldPairs:
        let jnodeIdx = tree.nodes[idx].items[tupleFieldIdx]
        fieldSym = fromJson5Impl(tree, jnodeIdx, type(fieldSym))
        tupleFieldIdx.inc
  else:
    {.error: "Unsupported type: " & $T.}

proc toJson5*[T](value: T): string =
  ## Serialize `value` to it's JSON5 representation. This representation
  ## will be valid JSON as well, with a single exception: the special float
  ## values `NaN`, `+Infinity`, and `-Infinity` are all supported.
  runnableExamples:
    type Obj = object
      field1: int
      field2: string
    let obj = Obj(field1: 1234, field2: "foobar")
    doAssert toJson5(obj) == """{"field1": 1234, "field2": "foobar"}"""
    ## 'NaN' is not valid in JSON, but in JSON5 it is!
    doAssert toJson5(NaN) == "NaN"
  result.addJson5(value)

proc fromJson5*(input: string, T: typedesc): T =
  ## Deserialize the JSON5 string `input` into a value of type `T`.
  runnableExamples:
    type Obj = object
      field1: int
      field2: string
    let input = """{"field1": 1234, "field2": "foobar"}"""
    doAssert fromJson5(input, Obj) == Obj(field1: 1234, field2: "foobar")
  let tree = parseJson5(input)
  result = fromJson5Impl(tree, 0, T)

# buildJson

proc transformToJson(stmts, resultNode, x: NimNode) =
  let addJ5Sym = bindSym"addJson5"
  case x.kind
  of nnkBracket:
    stmts.add newCall(bindSym"add", resultNode, newLit"[")
    for n in x:
      transformToJson(stmts, resultNode, n)
      stmts.add newCall(bindSym"add", resultNode, newLit", ")
    if x.len > 0:
      stmts.del(stmts.len - 1)
    stmts.add newCall(bindSym"add", resultNode, newLit"]")
  of nnkCurly:
    doAssert x.len == 0
    stmts.add newCall(bindSym"add", resultNode, newLit"{}")    
  of nnkTableConstr:
    stmts.add newCall(bindSym"add", resultNode, newLit"{")
    for n in x:
      expectKind(n[0], nnkStrLit)
      transformToJson(stmts, resultNode, n[0])      
      stmts.add newCall(bindSym"add", resultNode, newLit": ")      
      transformToJson(stmts, resultNode, n[1])
      stmts.add newCall(bindSym"add", resultNode, newLit", ")
    if x.len > 0:
      stmts.del(stmts.len - 1)
    stmts.add newCall(bindSym"add", resultNode, newLit"}")
  else:
    stmts.add newCall(addJ5Sym, resultNode, x)

# Will be exported in the future.
macro buildJson(x: untyped): string =
  ## The `buildJsonValue` macro can be used to construct
  ## JSON using a syntax similiar to JSON itself.
  let resultSym = genSym(nskVar)
  result = newBlockStmt(newStmtList(newVarStmt(resultSym, newLit(""))))
  transformToJson(result[1], resultSym, x)
  result[1].add resultSym

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