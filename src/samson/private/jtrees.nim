import std / tables

type
  JNodeIdx* = int

  JTree* = object
    nodes*: seq[JNode]

  JNodeKind* = enum
    nkEmpty
    nkObject
    nkArray
    nkString
    nkNumber
    nkInt64
    nkBool
    nkNull

  JNode* = object
    case kind*: JNodeKind
    of nkObject:
      kvpairs*: OrderedTable[string, JNodeIdx]
    of nkArray:
      items*: seq[JNodeIdx]
    of nkString:
      strVal*: string
    of nkNumber:
      numVal*: float
    of nkInt64:
      int64Val*: int64
    of nkBool:
      boolVal*: bool
    else:
      discard