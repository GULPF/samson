type
  Either*[A; B: not A] = object
    case isA: bool
    of true:
      a: A
    of false:
      b: B

proc isType*[A, B](either: Either[A, B], T: typedesc[A]): bool =
  either.isA

proc isType*[A, B](either: Either[A, B], T: typedesc[B]): bool =
  not either.isA

proc get*[A, B](either: Either[A, B], T: typedesc[A]): A =
  either.a

proc get*[A, B](either: Either[A, B], T: typedesc[B]): B =
  either.b

proc either*[A, B](val: A): Either[A, B] =
  Either[A, B](isA: true, a: val)

proc either*[A, B](val: B): Either[A, B] =
  Either[A, B](isA: false, b: val)

proc `==`*[A, B](a, b: Either[A, B]): bool =
  if a.isType(A) != b.isType(A):
    return false
  return (a.isType(A) and a.a == b.a) or (a.isType(B) and a.b == b.b)

proc `$`*[A, B](either: Either[A, B]): string =
  result = "Either[" & $A & ", " & $B & "]("
  if either.isType(A):
    result.addQuoted(either.a)
  else:
    result.addQuoted(either.b)
  result.add ")"

converter swapGenerics*[A, B](either: Either[A, B]): Either[B, A] =
  ## Either[A, B] and Either[B, A] are conceptually the same thing,
  ## and hence they are implicitly convertible to each other.
  if either.isA:
    Either[B, A](isA: false, b: either.a)
  else:
    Either[B, A](isA: true, a: either.b)
