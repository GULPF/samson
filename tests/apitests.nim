import std / [unittest, options, tables, times, math, sets]
from std / strutils import strip
import
  .. / src / samson,
  .. / src / samson / pragmas,
  .. / src / samson / experimental / [eithers, jsonvalues]

template reject(body: untyped) =
  check not compiles(body)

suite "fromJson5":
  test "string":
    check fromJson5("\"foo\"", string) == "foo"

  test "char":
    check fromJson5("'a'", char) == 'a'
    expect(JsonSchemaError): discard fromJson5("'ab'", char)

  test "bool":
    check fromJson5("true", bool) == true
    check fromJson5("false", bool) == false

  test "Option[T]":
    check fromJson5("null", Option[bool]) == none(bool)
    check fromJson5("true", Option[bool]) == some(true)

    type ObjWithOptField = object
      field: Option[int]
    check fromJson5("{}", ObjWithOptField) == ObjWithOptField()
    check fromJson5("{field: 1}", ObjWithOptField) ==
      ObjWithOptField(field: some(1))
    check fromJson5("{field: null}", ObjWithOptField) == ObjWithOptField()

  test "Either[A, B]":
    check fromJson5("1", Either[int, string]) == either[int, string](1)
    check fromJson5("'AB'", Either[int, string]) == either[int, string]("AB")
    # "1.0" represents a value which can be represented by `int`, so
    # either will use int in this case.
    check fromJson5("1.0", Either[int, float]) == either[int, float](1.int)
    # But not here.
    check fromJson5("1.5", Either[int, float]) == either[int, float](1.5)

  test "Table[string, T] | OrderedTable[string, T]":
    let t = fromJson5("{a: 1, b: 2}", Table[string, int])
    check t == {"a": 1, "b": 2}.toTable

    let ot = fromJson5("{a: 1, b: 2}", OrderedTable[string, int])
    check ot == {"a": 1, "b": 2}.toOrderedTable

  test "HashSet[T] | OrderedSet[T]":
    let s = fromJson5("[1, 2]", HashSet[int])
    check s == [1, 2].toHashSet
    let os = fromJson5("[1, 2]", OrderedSet[int])
    check os == [1, 2].toOrderedSet
    expect(JsonSchemaError): discard fromJson5("[1, 2, 2]", HashSet[int])

  test "SomeIntenger":
    check fromJson5("1", int8) == 1'i8
    check fromJson5("1.0", int8) == 1'i8
    check fromJson5("1", int16) == 1'i16
    check fromJson5("1", int32) == 1'i32
    check fromJson5("1", int64) == 1'i64
    check fromJson5("1", int) == 1
    check fromJson5("1", uint8) == 1'u8
    check fromJson5("1", uint16) == 1'u16
    check fromJson5("1", uint32) == 1'u32

    # test range checks

    expect(JsonSchemaError): discard fromJson5("500", int8)
    expect(JsonSchemaError): discard fromJson5("-1", uint8)

  test "SomeFloat":
    check fromJson5("1", float) == 1.0
    check fromJson5("1.1", float) == 1.1
    check fromJson5("1", float32) == 1.0'f32
    check fromJson5("1.1", float32) == 1.1'f32

    check fromJson5("Infinity", float) == Inf
    check fromJson5("-Infinity", float) == -Inf
    check fromJson5("NaN", float) != fromJson5("NaN", float)
    # Currently fails, but not a big deal
    # check fromJson5("-1.0", float).classify == fcNegZero

  test "range":
    type IntRange = range[0..4]
    check fromJson5("1", IntRange) == 1.IntRange
    expect(JsonSchemaError): discard fromJson5("-1", IntRange)
    expect(JsonSchemaError): discard fromJson5("5", IntRange)
    type FloatRange = range[1.1..2.2]
    check fromJson5("2", FloatRange) == 2.FloatRange
    check fromJson5("1.5", FloatRange) == (1.5).FloatRange
    expect(JsonSchemaError): discard fromJson5("1", FloatRange)
    expect(JsonSchemaError): discard fromJson5("2.5", FloatRange)

    when false:
      type EnumForRange = enum efrA, efrB, efrC
      type EnumRange = range[efrA..efrB]
      check fromJson5("0", EnumRange) == efrA
      expect(JsonSchemaError): discard fromJson5("2", EnumRange)

  test "object":
    type PlainObject = object
      a: int
      b: bool
      c: seq[int]
    check fromJson5("{a: 0, b: true, c: [1]}", PlainObject) ==
      PlainObject(a: 0, b: true, c: @[1])

  test "object pragmas":
    type Enum = enum first, second
    type TaggedObjectFrom = object
      foo {.jsonFieldName("bar").}: int
      skip {.jsonExclude.}: int
      dt {.jsonDateTimeFormat("YYYY-MM-ddzzz").}: Time
      strenum {.jsonStringEnum.}: Enum
      baz: bool
    let json5str = "{bar: 1, dt: \"1970-01-01Z\", baz: true, strenum: 'first'}"
    check fromJson5(json5str, TaggedObjectFrom) ==
      TaggedObjectFrom(foo: 1, baz: true, dt: fromUnix(0), strenum: first)

  test "seq":
    check fromJson5("[1, 2, 3]", seq[int]) == @[1, 2, 3]

  test "set":
    check fromJson5("[1, 2]", set[int8]) == {1'i8, 2'i8}
    expect(JsonSchemaError): discard fromJson5("[1, 1]", set[int8])

  test "array":
    check fromJson5("[1, 2, 3]", array[3, int]) == [1, 2, 3]
    expect(JsonSchemaError): discard fromJson5("[1, 2, 3]", array[2, int])
    expect(JsonSchemaError): discard fromJson5("[1, 2, 3]", array[4, int])

  test "enum":
    type E = enum
      first = 5,
      second = 6,
      third = 8
    check fromJson5("5", E) == first
    expect(JsonSchemaError): discard fromJson5("4", E)
    # TODO: implement checking for holes
    # expect(JsonSchemaError): discard fromJson5("7", E)

  test "DateTime | Time":
    check fromJson5("'1970-01-01T00:00:00.000000000Z'", Time) == fromUnix(0)
    check fromJson5("'1970-01-01T00:00:00.000000000Z'", DateTime) ==
      fromUnix(0).local

  test "tuple":
    check fromJson5("[1, 'foo']", (int, string)) == (1, "foo")

  test "field mismatch":
    type MissingFieldObj = object
      a: int
      b: int
    expect(JsonSchemaError): discard fromJson5("{a: 1}", MissingFieldObj)
    expect(JsonSchemaError): discard fromJson5("{a: 1, b: 2, c: 3}", MissingFieldObj)

  test "wrong array len":
    expect(JsonSchemaError): discard fromJson5("[1, 2, 3]", (int, int))
    expect(JsonSchemaError): discard fromJson5("[1]", (int, int))

suite "toJson5":

  test "JsonValue":
    check toJson5(initJsonValue(nil)) == "null"
    check toJson5(initJsonValue(1)) == "1"
    check toJson5(initJsonValue("foo")) == "\"foo\""

  test "string":
    check toJson5("foo") == "\"foo\""

  test "char":
    check toJson5('a') == "\"a\""

  test "bool":
    check toJson5(true) == "true"
    check toJson5(false) == "false"

  test "Option[T]":
    check toJson5(some(true)) == "true"
    check toJson5(none(bool)) == "null"

  test "Either[A, B]":
    check toJson5(either[string, int](1)) == "1"
    check toJson5(either[string, int]("1")) == "\"1\""

  test "Table[string, T] | OrderedTable[string, T]":
    check toJson5({"a": 1, "b": 2}.toTable) == "{\"a\": 1, \"b\": 2}"
    check toJson5({"a": 1, "b": 2}.toOrderedTable) == "{\"a\": 1, \"b\": 2}"
    check toJson5({"a": 1}.toTable) == "{\"a\": 1}"

  test "HashSet[T] | OrderedSet[T]":
    # Order not guaranteed for HashSet
    check toJson5([1, 2].toHashSet) == "[1, 2]" or toJson5([1, 2].toHashSet) == "[2, 1]"
    check toJson5([1, 2, 3].toOrderedSet) == "[1, 2, 3]"

  test "SomeInteger":
    check toJson5(1) == "1"
    check toJson5(1'i8) == "1"
    check toJson5(1'i16) == "1"
    check toJson5(1'i32) == "1"
    check toJson5(1'i64) == "1"
    check toJson5(1'u8) == "1"
    check toJson5(1'u16) == "1"
    check toJson5(1'u32) == "1"

  test "SomeFloat":
    check toJson5(1.1) == "1.1"
    check toJson5(1'f32) == "1.0"
    check toJson5(Inf) == "Infinity"
    check toJson5(-Inf) == "-Infinity"
    check toJson5(-0.0) == "-0.0"
    check toJson5(NaN) == "NaN"

  test "range":
    type IntRange = range[0..1]
    check toJson5(0.IntRange) == "0"
    type FloatRange = range[0.0..1.0]
    check toJson5(0.0.FloatRange) == "0.0"
    type EnumForRange = enum efrA, efrB, efrC
    type EnumRange = range[efrA..efrB]
    check toJson5(efrA.EnumRange) == "0"

  test "object":
    type PlainObject = object
      a: int
      b: bool
    check toJson5(PlainObject(a: 1, b: true)) == "{\"a\": 1, \"b\": true}"

  test "object pragmas":
    type
      Enum = enum
        first, second
      TaggedObjectTo = object
        foo {.jsonFieldName("bar").}: int
        skip {.jsonExclude.}: int
        dt {.jsonDateTimeFormat("YYYY").}: Time
        strenum {.jsonStringEnum.}: Enum
        baz: bool
    let obj = TaggedObjectTo(foo: 1, baz: true, dt: fromUnix(0),
      strenum: first)
    check toJson5(obj) ==
      """{"bar": 1, "dt": "1970", "strenum": "first", "baz": true}"""

  test "seq":
    check toJson5(@[1, 2, 3]) == "[1, 2, 3]"
    check toJson5(@[1]) == "[1]"

  test "set":
    check toJson5({1, 2}) == "[1, 2]"
    check toJson5({1}) == "[1]"

  test "array":
    check toJson5([1, 2, 3]) == "[1, 2, 3]"

  test "enum":
    type E = enum
      first = 5,
      second = 6,
      third = 7
    check toJson5(first) == "5"

  test "DateTime | Time":
    check toJson5(fromUnix(0)) == "\"1970-01-01T00:00:00.000000000Z\""
    check toJson5(fromUnix(0).utc) == "\"1970-01-01T00:00:00.000000000Z\""

  test "tuple":
    let tup = (first: 1, second: "foo")
    check toJson5(tup) == """[1, "foo"]"""
    let tupAnon = (1, "foo")
    check toJson5(tupAnon) == """[1, "foo"]"""

suite "Either[A, B]":

  test "==":
    check either[int, string](1) == either[string, int](1)

  test "isType":
    check either[int, string](1).isType(int)
    check either[int, string]("foo").isType(string)

  test "get":
    check either[int, string](1).get(int) == 1
    check either[int, string]("foo").get(string) == "foo"

suite "JsonValue":

  test "basic":
    doAssert initJsonValue(1'i64).getInt == 1
    # The motivation for the implicit i64->f64 conversion
    # here is that JSON itself doesn't make a distinction
    # between floats and integers. A dynamically language
    # might encode the value `1` as either `1.0` or `1`,
    # and both variations needs to be deserializable to int/float.
    doAssert initJsonValue(1'i64).getFloat == 1.0
    # And for the same reason, below is intended behavior
    doAssert initJsonValue(1.0).kind == jsonInteger
    doAssert initJsonValue(1.0) == initJsonValue(1'i64)

  when false:
    test "buildJsonValue":
      check buildJsonValue(1) == initJsonValue(1)
      check buildJsonValue("abc") == initJsonValue("abc")
      check buildJsonValue([1, "foo"]) == initJsonValue([
        initJsonValue(1), initJsonValue("foo")
      ])
      check buildJsonValue({}) == initJsonValue(initOrderedTable[string, JsonValue]())
      check buildJsonValue([1, nil]) == initJsonvalue([
        initJsonValue(1), initJsonValue(nil)
      ])
      check buildJsonValue({"foo": 1}) == initJsonvalue({
        "foo": initJsonValue(1)
      }.toOrderedTable)
      check buildJsonValue({"foo": [1, "two"]}) == initJsonvalue({
        "foo": initJsonValue([
          initJsonValue(1), initJsonValue("two")
        ])
      }.toOrderedTable)

suite "misc":

  test "errors":
    reject fromJson5("null", Table[int, int])

suite "Unicode":
  test "Invalid UTF-8":
    let badString = "'\xC0\xC0\xC0'"
    check fromJson5(badString, string) == "\uFFFD\uFFFD\uFFFD"

  test "Invalid UTF-8 (overlong encoding)":
    let badString = "'\xE0\x80\x80'"
    check fromJson5(badString, string) == "\uFFFD\uFFFD\uFFFD"

  test "Invalid UTF-8 (non-greedy replacement)":
    let badString = "'\xE0a'"
    check fromJson5(badString, string) == "\uFFFDa"

  test "Invalid UTF-8 (lonely surrogate)":
    let badString = "'\uDEAD'"
    check fromJson5(badString, string) == "\uFFFD\uFFFD\uFFFD"

  test "Invalid Unicode escape (lonely surrogate)":
    let badString = "'\\uDEAD'"
    # NOTE: lonely surrogates are allowed by JSON5.
    # The result here is not a valid Unicode sequence, but that's ok.
    check fromJson5(badString, string) == "\uDEAD"
    # The result of `toJson5` should never be invalid UTF-8, so here
    # it should be escaped.
    check toJson5("\uDEAD") == "\"\\udead\""

  test "Valid surrogate":
    check fromJson5("'\\uD83D\\uDE00'", string) == "\u{1F600}"

  test "Complete example":

    type
      Config = object
        dict: OrderedTable[string, Either[string, ComplexConfig]]

      ComplexConfig = object
        field1: int
        field2 {.jsonFieldName("field3").}: int

    const jsonStr = """
      {
        dict: {
          "first": "val1",
          "second": "val2",
          "third": { field1: 1, field3: 2 }
        }
      }
    """

    discard fromJson5(jsonStr, Config)

const jsonStr = "[1, 2, 3]"
const output = fromJson5(jsonStr, seq[int])
doAssert output == @[1, 2, 3]