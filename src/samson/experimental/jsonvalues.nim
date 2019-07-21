import std / [tables, macros]

#[
    The idea is to support something like this for dynamic stuff:

    let str = toJson5(buildJsonValue({
        ...jv,
        newfield: value,
        newfield2: [...jv2, val2]
    }))

    Note here that `...` will be treated as a magic identifier in the macro.
    IMO that's fine for this case, since it's unlikely to clash with anything.
]#

type
    JsonValueKind* = enum
        jsonInteger
        jsonFloat
        jsonString
        jsonArray
        jsonObject
        jsonNull

    JsonValue* = object
        case kind: JsonValueKind
        of jsonInteger:
            intVal: int64
        of jsonFloat:
            floatVal: float
        of jsonString:
            strVal: string
        of jsonArray:
            arrVal: seq[JsonValue]
        of jsonObject:
            objVal: OrderedTable[string, JsonValue]
        of jsonNull:
            discard

proc isSafeInteger(f: float): bool =
    f.int64.float == f

proc initJsonValue*(intVal: int64): JsonValue =
    JsonValue(kind: jsonInteger, intVal: intVal)

proc initJsonValue*(floatVal: float): JsonValue =
    if isSafeInteger(floatVal):
        JsonValue(kind: jsonInteger, intVal: floatVal.int64)
    else:
        JsonValue(kind: jsonFloat, floatVal: floatVal)

proc initJsonValue*(strVal: string): JsonValue =
    JsonValue(kind: jsonString, strVal: strVal)

proc initJsonValue*(arrVal: openArray[JsonValue]): JsonValue =
    JsonValue(kind: jsonArray, arrVal: @arrVal)

proc initJsonValue*(objVal: OrderedTable[string, JsonValue]): JsonValue =
    JsonValue(kind: jsonObject, objVal: objVal)

proc initJsonValue*(nilVal: type(nil)): JsonValue =
    JsonValue(kind: jsonNull)

proc kind*(jv: JsonValue): JsonValueKind =
    jv.kind

proc getInt*(jv: JsonValue): int =
    jv.intVal.int

proc getInt64*(jv: JsonValue): int64 =
    jv.intVal

proc getFloat*(jv: JsonValue): float =
    if jv.kind == jsonInteger:
        jv.intVal.float
    else:
        jv.floatVal

proc getString*(jv: JsonValue): string =
    jv.strVal

proc getSeq*(jv: JsonValue): seq[JsonValue] =
    jv.arrVal

proc getTable*(jv: JsonValue): OrderedTable[string, JsonValue] =
    jv.objVal

proc `[]`*(jv: JsonValue, idx: int): JsonValue =
    doAssert jv.kind == jsonArray
    jv.arrVal[idx]

proc `[]`*(jv: JsonValue, field: string): JsonValue =
    doAssert jv.kind == jsonObject
    jv.objVal[field]

proc len*(jv: JsonValue): int =
    doAssert jv.kind in {jsonArray, jsonObject}
    if jv.kind == jsonArray:
        result = jv.arrVal.len
    elif jv.kind == jsonObject:
        result = jv.objVal.len

proc `==`*(a, b: JsonValue): bool =
    doAssert a.kind == b.kind
    case a.kind
    of jsonInteger:
        a.intVal == b.intval
    of jsonString:
        a.strVal == b.strVal
    of jsonFloat:
        a.floatVal == b.floatVal
    of jsonArray:
        a.arrVal == b.arrVal
    of jsonObject:
        a.objVal == b.objVal
    of jsonNull:
        true

let emptyObject = JsonValue(kind: jsonObject)

proc transformNode(x: NimNode): NimNode =
    let ijv = bindSym"initJsonValue"
    case x.kind
    of nnkBracket:
        let arrNode = newNimNode(nnkBracket)
        for n in x:
            arrNode.add transformNode(n)
        result = newCall(ijv, arrNode)
    of nnkCurly:
        doAssert x.len == 0
        result = bindSym"emptyObject"
    of nnkTableConstr:
        let objNode = newNimNode(nnkTableConstr)
        let pairs = newNimNode(nnkExprColonExpr)
        objNode.add(pairs)
        let n = x[0]
        for i in countup(0, len(n)-1, 2):
            expectKind(n[i], nnkStrLit)
            pairs.add(n[i])
            pairs.add(transformNode(n[i+1]))
        result = newCall(ijv, newCall(bindSym"toOrderedTable", objNode))
    else:
        result = newCall(ijv, x)
    # echo x.treeRepr    
    # echo result.repr
    # echo "- - -"

macro buildJsonValue*(x: untyped): JsonValue =
    ## The `buildJsonValue` macro can be used to construct
    ## JsonValues using a syntax similiar to JSON itself.
    result = transformNode(x)

# echo buildJsonValue({"field": [1, "two"]})