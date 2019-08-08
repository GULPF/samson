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

  FromJsonOpts* = object
    allowComments*: bool
    allowTrailingCommas*: bool
    allowSpecialFloats*: bool
  
  ToJsonOpts* = object
    allowSpecialFloats*: bool  
    indent*: Natural ## \
      ## The indention size to use for the output. When set to 0,
      ## compact JSON without any newlines or whitespace will be generated.

  ToJson5Opts* = object
    indent*: Natural ## \
      ## The indention size to use for the output. When set to 0,
      ## compact JSON without any newlines or whitespace will be generated.

const DefaultDateTimeFormat = "uuuu-MM-dd'T'HH:mm:ss'.'fffffffffzzz"

const DefaultFromJsonOpts = FromJsonOpts(
  allowComments: true,
  allowTrailingCommas: true,
  allowSpecialFloats: true
)

const StrictFromJsonOpts = FromJsonOpts(
  allowComments: false,
  allowTrailingCommas: false,
  allowSpecialFloats: false
)

const DefaultToJsonOpts = ToJsonOpts(
  allowSpecialFloats: true,
  indent: 0
)

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

proc addJsonString(result: var string, value: string) =
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
    # TODO: This escape doesn't exist in JSON
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

proc addJsonDateTime(result: var string, value: DateTime|Time,
    f = DefaultDateTimeFormat) =
  result.addJsonString value.utc.format(f)

proc addJson[T](result: var string, value: T, opts: ToJsonOpts, currIndent: int) =
  template indent(len: int): string {.used.} = ' '.repeat(len)
  template pretty: bool {.used.} = opts.indent > 0

  when T is Option:
    if value.isSome:
      result.addJson(value.get, opts, currIndent)
    else:
      result.add "null"
  elif T is JsonValue:
    case value.kind
    of jsonNull:
      result.add "null"
    of jsonInteger:
      result.add $value.getInt64
    of jsonFloat:
      result.addJson(value.getFloat, opts, currIndent)
    of jsonString:
      result.addJson(value.getString, opts, currIndent)
    of jsonArray:
      result.addJson(value.getSeq, opts, currIndent)
    of jsonObject:
      result.addJson(value.getTable, opts, currIndent)
  elif T is Either:
    type A = T.generic(0)
    type B = T.generic(1)
    if value.isType(A):
      result.addJson(value.get(A), opts, currIndent)
    else:
      result.addJson(value.get(B), opts, currIndent)
  elif T is bool:
    result.add $value
  elif T is SupportedIntegerTypes:
    result.add $value
  elif T is SomeFloat:
    let floatClass = classify(value)
    if not opts.allowSpecialFloats and floatClass in {fcInf, fcNegInf, fcNaN}:
      raise newException(JsonGenerateError, "Found illegal float value: " & $value)
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
    if pretty:
      result.add "\n"
    for k, v in value:
      if pretty:
        result.add indent(currIndent + opts.indent)
      result.addJson(k, opts, currIndent + opts.indent)
      result.add ": "
      result.addJson(v, opts, currIndent + opts.indent)
      result.add ", "
    if value.len > 0:
      result.setLen(result.len - 2)
    if pretty:
      result.add "\n" & indent(currIndent)
    result.add "}"
  elif T is seq|array|set|HashSet|OrderedSet:
    result.add "["
    for v in value:
      result.addJson(v, opts, currIndent)
      result.add ", "
    if value.len > 0:
      result.setLen(result.len - 2)
    result.add "]"
  elif T is DateTime|Time:
    result.addJsonDateTime(value)
  elif T is string:
    result.addJsonString(value)
  elif T is char:
    result.addJsonString($value)
  elif T is PlainObject:
    var hasFields = false
    result.add "{"
    if pretty:
      result.add "\n"
    for fieldName, fieldSym in value.fieldPairs:
      if not shouldExcludeField(fieldSym):
        hasFields = true
        let resolvedFieldName = resolveFieldName(fieldName, fieldSym)
        if pretty:
          result.add indent(currIndent + opts.indent)
        result.addJson(resolvedFieldName, opts, currIndent)
        result.add ": "
        when type(fieldSym) is DateTime|Time:
          const f = resolveDateTimeFormat(fieldSym)
          result.addJsonDateTime fieldSym, f
        elif type(fieldSym) is enum and
            hasCustomPragma(fieldSym, jsonStringEnum):
          result.addJsonString $fieldSym
        else:
          result.addJson(fieldSym, opts, currIndent + opts.indent)
        if pretty:
          result.add ",\n"
        else:
          result.add ", "
    if hasFields:
      if pretty:
        discard
        result.delete(result.high - 1, result.high - 1)
      else:
        result.setLen(result.len - 2)
    if pretty:
      result.add indent(currIndent)
    result.add "}"
  elif T is tuple:
    result.add "["
    for val in value.fields:
      result.addJson(val, opts, currIndent)
      result.add ", "
    result.setLen(result.len - 2)
    result.add "]"
  else:
    {.error: "Unsupported type: " & $T.}

# This needs to be implemented as an overload due to limitations in Nim
proc addJson(result: var string, value: type(nil), opts: ToJsonOpts, currIndent: var int) =
  result.add "null"

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
  result.addJson(value, ToJsonOpts(allowSpecialFloats: true, indent: 0), 0)

proc toJson5*[T](value: T, opts: ToJson5Opts): string =
  let opts = ToJsonOpts(allowSpecialFloats: true, indent: opts.indent)
  result.addJson(value, opts, 0)

proc toJson*[T](value: T): string =
  var currIndent: int  
  result.addJson(value, ToJsonOpts(allowSpecialFloats: true, indent: 0), currIndent)

proc toJson*[T](value: T, opts: ToJsonOpts): string =
  var currIndent: int  
  result.addJson(value, opts, currIndent)

# JSON5->Nim

proc dateTimeFromJson(tree: JTree, idx: JnodeIdx,
    T: typedesc[DateTime|Time], f: string = DefaultDateTimeFormat): T =
  if tree.nodes[idx].kind != nkString:
    schemaError(T, `$`(tree, idx))
  else:
    when T is DateTime:
      result = parse(tree.nodes[idx].strVal, f)
    else:
      result = parseTime(tree.nodes[idx].strVal, f, local())

proc fromJsonImpl(tree: JTree, idx: JNodeIdx, T: typedesc): T =
  template error {.used.} =
    schemaError(T, `$`(tree, idx))

  when T is DateTime|Time:
    result = dateTimeFromJson(tree, idx, T)
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
        result[k] = fromJsonImpl(tree, v, T.generic(1))
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
        let value = fromJsonImpl(tree, idx, T.generic(0))
        if value in result:
          error()
        result.incl value
  elif T is seq:
    if tree.nodes[idx].kind != nkArray:
      error()
    else:
      result = newSeq[T.generic(0)](tree.nodes[idx].items.len)
      for idx, itemIdx in tree.nodes[idx].items:
        result[idx] = fromJsonImpl(tree, itemIdx, T.generic(0))
  elif T is array:
    if tree.nodes[idx].kind != nkArray or
        len(result) != tree.nodes[idx].items.len:
      error()
    else:
      for idx, itemIdx in tree.nodes[idx].items:
        result[idx] = fromJsonImpl(tree, itemIdx, T.generic(1))
  elif T is set:
    if tree.nodes[idx].kind != nkArray:
      error()
    else:
      for itemIdx in tree.nodes[idx].items:
        let v = fromJsonImpl(tree, itemIdx, T.generic(0))
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
    let ord = fromJsonImpl(tree, idx, StorageType)
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
      result = some(fromJsonImpl(tree, idx, T.generic(0)))
  elif T is Either:
    try:
      either[T.generic(0), T.generic(1)](fromJsonImpl(tree, idx, T.generic(0)))
    except:
      either[T.generic(0), T.generic(1)](fromJsonImpl(tree, idx, T.generic(1)))
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
              fieldSym = dateTimeFromJson(tree, fieldIdx, type(fieldSym), f)
            elif type(fieldSym) is enum and
                hasCustomPragma(fieldSym, jsonStringEnum):
              let strVal = fromJsonImpl(tree, fieldIdx, string)
              try:
                fieldSym = parseEnum[type(fieldSym)](strVal)
              except ValueError:
                error()
            else:
              fieldSym = fromJsonImpl(tree, fieldIdx, type(fieldSym))
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
        fieldSym = fromJsonImpl(tree, jnodeIdx, type(fieldSym))
        tupleFieldIdx.inc
  else:
    {.error: "Unsupported type: " & $T.}

proc fromJson5*(input: string, T: typedesc): T =
  ## Deserialize the JSON5 string `input` into a value of type `T`.
  runnableExamples:
    type Obj = object
      field1: int
      field2: string
    let input = """{"field1": 1234, "field2": "foobar"}"""
    doAssert fromJson5(input, Obj) == Obj(field1: 1234, field2: "foobar")
  let tree = parseJson5(input)
  result = fromJsonImpl(tree, 0, T)

proc fromJson*(input: string, T: typedesc):T =
  ## Deserialize the JSON string `input` into a value of type `T`.  
  let tree = parseJson(input,
    DefaultFromJsonOpts.allowComments,
    DefaultFromJsonOpts.allowSpecialFloats,
    DefaultFromJsonOpts.allowTrailingCommas)
  result = fromJsonImpl(tree, 0, T)

proc fromJson*(input: string, T: typedesc, opts: FromJsonOpts):T =
  ## Deserialize the JSON string `input` into a value of type `T`.
  let tree = parseJson(input, opts.allowComments, opts.allowSpecialFloats, opts.allowTrailingCommas)
  result = fromJsonImpl(tree, 0, T)

# buildJson

proc transformToJson(stmts, resultNode, x: NimNode) =
  let addJ5Sym = bindSym"addJson"
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