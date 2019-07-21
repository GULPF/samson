template jsonFieldName*(fieldName: string) {.pragma.}
  ## Allows specifying a field name for serialization.
  ## This is required when the field name is not a valid Nim identifier.
  ##
  ## .. code-block:: nim
  ##  type Obj = object
  ##    field {.jsonFieldName: "newFieldName".}: int
  ##  doAssert toJson5(Obj(field: 1)) == """{"newFieldName": 1}"""

template jsonExclude*() {.pragma.}
  ## Indicates that a field should be excluded from serialization.
  ##
  ## .. code-block:: nim
  ##  type Obj = object
  ##    field1: int
  ##    field2 {.jsonExclude.}: int
  ##  doAssert toJson5(Obj(field1: 1, field2: 2)) == """{"field1": 1}"""

template jsonDateTimeFormat*(format: string) {.pragma.}
  ## Indicates that a `times.DateTime` or `times.Time` field should
  ## use some specific format for serialization.
  ##
  ## .. code-block:: nim
  ##  import std/times
  ##  type Obj = object
  ##    field {.jsonDateTimeFormat: "yyyy-MM-dd".}: DateTime
  ##  let dt = initDateTime(01, mJan, 2010, 00, 00, 00, utc())
  ##  doAssert toJson5(Obj(field: dt)) == """{"field": "2010-01-01"}"""

template jsonStringEnum*() {.pragma.}
  ## Indicates that an `enum` field should be serialized as a string instead
  ## of as an integer.
  ##
  ## .. code-block:: nim
  ##  type
  ##    Enum = enum
  ##      first, second
  ##    Obj = object
  ##      field1: Enum
  ##      field2 {.jsonStringEnum.}: Enum
  ##  let obj = Obj(field1: first, field2: first)
  ##  doAssert toJson5(obj) == """{"field1": 0, "field2": "first"}"""