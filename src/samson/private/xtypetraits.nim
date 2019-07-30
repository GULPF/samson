import std / macros

macro generic*(T: typedesc, i: static[int]): typedesc =
  ## Return the generic argument at position `i` in the generic type T.
  doAssert i >= 0
  var impl = getTypeImpl(T)
  expectKind(impl, nnkBracketExpr)
  impl = impl[1]
  while true:
    case impl.kind
      of nnkSym:
        impl = impl.getImpl
        continue
      of nnkTypeDef:
        impl = impl[2]
        continue
      of nnkBracketExpr:
        impl=impl[1 + i]
        break
      else:
        error "internal error: impl.kind: " & $impl.kind
  impl

macro tupleLen*(typ: typedesc[tuple]): int =
  ## Returns the number of elements in a tuple type.
  let impl = getType(typ)
  result = newIntlitNode(impl[1].len - 1)

macro rangeUnderlyingType*(typ: typedesc[range]): typedesc =
  ## Returns the underlying type of a range.
  runnableExamples:
    doAssert rangeUnderlyingType(range[1'i8..2'i8]) is int8
    doAssert rangeUnderlyingType(range[1.0..2.0]) is float
  result = getType(getType(typ)[1][1])
