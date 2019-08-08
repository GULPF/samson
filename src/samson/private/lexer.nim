import std / [strutils, macros]
import xunicode, .. / errors

type
  TokenKind* = enum
    tkBracketL
    tkBracketR
    tkBraceL
    tkBraceR
    tkColon
    tkComma
    tkString
    tkNumber
    tkInt64
    tkLineComment # Unused
    tkBlockComment # Unused
    tkIdent
    tkEoi

  Token* = object
    case kind*: TokenKind
    of tkString:
      strVal*: string
    of tkNumber:
      numVal*: float
    of tkInt64:
      int64Val*: int64
    of tkIdent:
      ident*: string
    else:
      discard
    pos*: Natural

  BaseLexer* = object of RootObj
    input*: string
    pos*: Natural

  Json5Lexer* = object of BaseLexer

  JsonLexer* = object of BaseLexer
    allowComments*: bool
    allowSpecialFloats*: bool
  
  # Workaround for https://github.com/nim-lang/Nim/issues/11861
  Lexer* = Json5Lexer | JsonLexer

# https://spec.json5.org/#white-space
const Json5AsciiWhiteSpace = {
  0x0009.char, 0x000A.char, 0x000B.char, 0x000C.char,
  0x000D.char, 0x0020.char
}
const Json5UnicodeWhiteSpace = [
  0x2028.Rune, 0x2029.Rune, 0x1680.Rune, 0x2000.Rune,
  0x2001.Rune, 0x2002.Rune, 0x2003.Rune, 0x2004.Rune,
  0x2005.Rune, 0x2006.Rune, 0x2007.Rune, 0x2008.Rune,
  0x2009.Rune, 0x200A.Rune, 0x202F.Rune, 0x205F.Rune,
  0x3000.Rune, 0x00A0.Rune, 0xFEFF.Rune
]

# https://tools.ietf.org/html/rfc8259#section-2
const JsonWhitespace = [
  0x0020.char, 0x0009.char, 0x000A.char, 0x000D.char
]

const LF* = 0x000A.char # Line feed
const CR* = 0x000D.char # Carriage return
const LS* = 0x2028.Rune # Line separator
const PS* = 0x2029.Rune # Paragraph separator
const ZWNJ = 0x200C.Rune # Zero width non-joiner
const ZWJ  = 0x200D.Rune # Zero width jointer

proc initJson5Lexer*(input: string): Json5Lexer =
  result.input = input

proc initJsonLexer*(input: string,
    allowComments, allowSpecialFloats: bool): JsonLexer =
  JsonLexer(input: input, allowComments: allowComments,
    allowSpecialFloats: allowSpecialFloats)

template head(l: Lexer): char =
  l.input[l.pos]

template uHead(l: Lexer): (Rune, int) =
  l.input.runeAndLenAt(l.pos)

template eoi(l: Lexer): bool =
  l.pos > l.input.high

proc linecolstr(str: string, pos: Natural): string =
  var line = 1
  var col = 1

  var stop = pos
  var pos = 0

  while pos < stop:
    case str[pos]
    of CR:
      line.inc
      col = 1
      if pos + 1 < stop and str[pos + 1] == LF:
        pos.inc
      pos.inc
    of LF:
      line.inc
      col = 1
      pos.inc
    of '\128'..'\xFF':
      let (rune, len) = str.runeAndLenAt(pos)
      if rune in [LS, PS]:
        line.inc
        col = 1
        pos.inc len
      else:
        col.inc
        pos.inc len
    else:
      col.inc
      pos.inc

  return "[Ln " & $line & ", Col " & $col & "]"

proc isIdentStart(r: Rune): bool =
  return isLetter(r)

proc isIdentCont(r: Rune): bool =
  return isLetter(r) or isCombiningMark(r) or isDigit(r) or
    isConnectorPunctuation(r) or r == ZWNJ or r == ZWJ

proc error*(l: Lexer, pos: int, msg: string) {.noreturn.} =
  raise newException(JsonParseError, linecolstr(l.input, pos) & " " & msg)

proc extractInteger(l: var Lexer, appendTo: var string) =
  while not l.eoi and l.head in Digits:
    appendTo.add l.head
    l.pos.inc

proc extractInvalidNumber(str: string, start: int): string =
  var idx = start
  while idx < str.len and str[idx] in Digits + {'.', '+', '-', 'e', 'E', 'x'}:
    result.add(str[idx])
    idx.inc

proc extractInvalidUnicodeEscape(str: string, start: int): string =
  return str.substr(start, start + 5)

proc `$`*(tok: Token): string =
  # NOTE: This wont give the original string for some edge cases.
  #       Maybe the token should also save a "len" field so that
  #       the original string can be extracted.
  case tok.kind
  of tkBracketL: "["
  of tkBracketR: "]"
  of tkBraceL: "{"
  of tkBraceR: "}"
  of tkColon: ":"
  of tkComma: ","
  of tkString: '"' & tok.strVal & '"'
  of tkNumber: $tok.numVal
  of tkInt64: $tok.int64Val
  of tkLineComment, tkBlockComment, tkEoi: raiseAssert("Unexpected token")
  of tkIdent: tok.ident

macro startsWith(l: var Lexer, str: static[string]): bool =
  ## Checks start of string without slicing to avoid allocations
  result = newLit(true)
  for it in countdown(str.high, 0):
    let c = newLit(str[it])
    let cond = quote do: `l`.input[`l`.pos + `it`] == `c` 
    result = newTree(nnkInfix, bindSym"and", cond, result)
  let identLen = newLit(str.len)
  let firstCond = quote do: `l`.pos >= `l`.input.high - `identLen`
  result = newTree(nnkInfix, bindSym"and", firstCond, result)

# Common

proc parseComment(l: var Lexer, allowJson5Newlines: bool) =
  doAssert l.head == '/'
  l.pos.inc
  case l.head
  of '/':
    l.pos.inc
    while true:
      if l.eoi or l.head in {LF, CR} or
          (allowJson5Newlines and l.head >= 128.char and
            l.uHead[0] in [LS, PS]):
        return
      l.pos.inc
  of '*':
    let startPos = l.pos - 1
    while true:
      # Need two chars to end a block comment
      if l.pos > l.input.high - 1:
        l.error(startPos, "Unclosed block comment")
      elif l.head == '*' and l.input[l.pos + 1] == '/':
        l.pos.inc 2
        return
      l.pos.inc
  else:
    l.error(l.pos - 1, "Unexpected character: /")

proc parseUnicodeHex(l: var Lexer, ndigits: int): Rune =
  let start = l.pos - 2
  if l.pos + ndigits > l.input.high:
    l.error(start, "Invalid unicode escape: " &
      l.input.extractInvalidUnicodeEscape(start))
  var hexStr = ""
  for i in 0 ..< ndigits:
    if l.eoi or l.input[l.pos] notin HexDigits:
      l.error(start, "Invalid unicode escape: " &
        l.input.extractInvalidUnicodeEscape(start))
    hexStr.add l.input[l.pos]
    l.pos.inc
  result = parseHexInt(hexStr).Rune

proc parseUEscape(l: var Lexer, appendTo: var string) =
  doAssert l.head == '\\' and l.input[l.pos + 1] == 'u'
  l.pos.inc 2
  var rune: Rune
  let high = l.parseUnicodeHex(ndigits = 4)
  if high in SurrogateRange and
      l.pos + 2 < l.input.len and
      l.input[l.pos] == '\\' and l.input[l.pos + 1] == 'u':
    l.pos.inc 2
    let low = l.parseUnicodeHex(ndigits = 4)
    if low in SurrogateRange:
      rune = decodeSurrogates(high, low)
    else:
      # Backoff
      l.pos.dec 6
      rune = high
  else:
    rune = high
  appendTo.add rune.toUtf8

# JSON5 Lexer

proc skipWhitespace(l: var Json5Lexer) =
  while not l.eoi:
    if l.head in Json5AsciiWhiteSpace:
      l.pos.inc
    elif l.head >= 128.char:
      let (rune, len) = l.uHead
      if rune in Json5UnicodeWhiteSpace:
        l.pos.inc len
      else:
        break
    else:
      break

proc parseNumber(l: var Json5Lexer): Token =
  var num: string
  var isFloat = false
  var start = l.pos
  var sgn = 1

  template error() =
    l.error(start, "Invalid number: " & extractInvalidNumber(l.input, start))

  # Sign
  if l.head == '+':
    l.pos.inc
  elif l.head == '-':
    sgn = -1
    l.pos.inc

  # Infinity & NaN
  if l.startsWith("NaN"):
    l.pos.inc 3
    return Token(kind: tkNumber, numVal: NaN)

  if l.startsWith("Infinity"):
    l.pos.inc 8
    return Token(kind: tkNumber, numVal: sgn.float * Inf)

  # Integer part
  if l.head == '0':
    num.add '0'
    l.pos.inc
    if l.eoi:
      return Token(kind: tkInt64, int64Val: 0)
    if l.head in Digits:
      error()
    if l.head in {'x', 'X'}:
      num.add 'x'
      l.pos.inc
      while not l.eoi and l.head in HexDigits:
        num.add l.head
        l.pos.inc
      return Token(kind: tkInt64, int64Val: sgn * parseHexInt(num))
  elif l.head in {'1' .. '9'}:
    l.extractInteger(appendTo = num)

  # Float part
  if not l.eoi and l.head == '.':
    isFloat = true
    num.add '.'
    l.pos.inc
    # NOTE: Trailing '.' is allowed, even with an exponent part
    #       However, a number beginning with a '.' must have at least one
    #       decimal (even if it has an exponent part)
    if num == "." and l.head notin Digits:
      error()
    l.extractInteger(appendTo = num)

  # Exponent part
  if not l.eoi and l.head in {'e', 'E'}:
    isFloat = true
    num.add 'e'
    l.pos.inc
    if l.eoi:
      error()
    if l.head in {'+', '-'}:
      num.add l.head
      l.pos.inc
    if l.eoi or l.head notin Digits:
      error()
    l.extractInteger(appendTo = num)

  result =
    if isFloat:
      let f = parseFloat(num)
      if f <= float(high(int64)) and f >= float(low(int64)) and
          f == float(int64(f)):
        Token(kind: tkInt64, int64Val: sgn * int64(f))
      else:
        Token(kind: tkNumber, numVal: sgn.float * f)
    else:
      Token(kind: tkInt64, int64Val: sgn * parseBiggestInt(num))

proc parseString(l: var Json5Lexer): Token =
  var tok = Token(kind: tkString)
  let terminator = l.head
  let startOfString = l.pos
  l.pos.inc
  while true:
    if l.pos > l.input.high:
      l.error(startOfString, "Unclosed string")
    elif l.head == terminator:
      l.pos.inc
      break
    elif l.head == '\\':
      l.pos.inc
      case l.head
      of 'x':
        l.pos.inc
        tok.strVal.add l.parseUnicodeHex(ndigits = 2).toUtf8
      of 'u':
        l.pos.dec
        l.parseUEscape(appendTo = tok.strVal)
      of '"', '\'', '\\':
        tok.strVal.add l.head
        l.pos.inc
      of 'b':
        tok.strVal.add '\x08'
        l.pos.inc
      of 'f':
        tok.strVal.add '\x0C'
        l.pos.inc
      of 'n':
        tok.strVal.add '\x0A'
        l.pos.inc
      of 'r':
        tok.strVal.add '\x0D'
        l.pos.inc
      of 't':
        tok.strVal.add '\x09'
        l.pos.inc
      of 'v':
        tok.strVal.add '\x0B'
        l.pos.inc
      of CR:
        l.pos.inc
        if not l.eoi and l.head == LF:
          l.pos.inc
      of LF:
        l.pos.inc
      of '0':
        if l.pos + 1 < l.input.len and l.input[l.pos + 1] in Digits:
          l.error(l.pos + 1, "Unexpected character: " & l.input[l.pos + 1])
        tok.strVal.add '\x00'
        l.pos.inc
      of '1'..'9':
        l.error(l.pos - 1, "Invalid escape: \\" & l.head)
      else:
        let (rune, len) = l.input.runeAndLenAt(l.pos)
        if rune notin [LS, PS]:
          tok.strVal.add rune.toUtf8
        l.pos.inc len
    elif l.head <= 127.char:
      if l.head in {LF, CR}:
        l.error(startOfString, "Unclosed string")
      tok.strVal.add l.head
      l.pos.inc
    else:
      let (rune, len) = l.input.runeAndLenAt(l.pos)
      # if rune in [LS, PS]:
      #   l.error("Unexpected end of string")
      # else:
      tok.strVal.add rune.toUtf8
      l.pos.inc len
  return tok

proc parseIdentifier(l: var Json5Lexer): Token =
  var ident = ""
  case l.head
  of { 'a' .. 'z', 'A' .. 'Z', '_', '$' }:
    ident.add l.head
    l.pos.inc
  of '\\':
    l.pos.inc
    if l.eoi or l.head != 'u':
      l.error(l.pos - 1, "Unexpected character: \\")
    l.pos.inc
    let runeStart = l.pos
    let rune = l.parseUnicodeHex(ndigits = 4)
    if not isIdentStart(rune):
      # TODO: Print rune as a unicode escape sequence instead
      l.error(runeStart, "Invalid character at start of identifier: " & $rune)
    ident.add $rune
  else:
    let (rune, len) = l.uHead
    if not isIdentStart(rune):
      # TODO: Print rune as a unicode escape sequence instead
      l.error(l.pos, "Invalid character at start of identifier: " & $rune)
    l.pos.inc len
    ident.add $rune

  while not l.eoi:
    case l.head
    of { 'a' .. 'z', 'A' .. 'Z', '_', '$' }:
      ident.add l.head
      l.pos.inc
    of '\\':
      let startOfEscape = l.pos
      l.pos.inc
      if l.eoi or l.head != 'u':
        l.error(l.pos - 1, "Unexpected character: \\")
      l.pos.inc
      var hexStr = ""
      for i in 0 .. 3:
        if l.eoi or l.input[l.pos] notin HexDigits:
          l.error(startOfEscape, "Invalid unicode escape sequence")
        hexStr.add l.input[l.pos]
        l.pos.inc
      let rune = parseHexInt(hexStr).Rune
      if not isIdentCont(rune):
        l.error(startOfEscape, "Unexpected character: " & "\\u" & hexStr)
      ident.add $rune
    else:
      let (rune, len) = l.uHead
      if not isIdentCont(rune):
        break
      l.pos.inc len
      ident.add $rune
  return Token(kind: tkIdent, ident: ident)

proc next*(l: var Json5Lexer): Token =
  template singleCharToken(k: TokenKind) =
    l.pos.inc
    return Token(kind: k, pos: tokenStartPos)

  l.skipWhitespace()
  let tokenStartPos = l.pos

  if l.eoi:
    l.pos = l.input.high + 1
    return Token(kind: tkEoi, pos: tokenStartPos)

  case l.head
  of '[': singleCharToken(tkBracketL)
  of ']': singleCharToken(tkBracketR)
  of '{': singleCharToken(tkBraceL)
  of '}': singleCharToken(tkBraceR)
  of ',': singleCharToken(tkComma)
  of ':': singleCharToken(tkColon)

  of '/':
    l.parseComment(allowJson5Newlines = true)
    result = l.next()

  of '"', '\'':
    result = l.parseString()
    result.pos = tokenStartPos

  of Digits, '.', '+', '-':
    result = l.parseNumber()
    result.pos = tokenStartPos

  else:
    result = l.parseIdentifier()
    result.pos = tokenStartPos

# JSON Lexer

proc skipWhitespace(l: var JsonLexer) =
  while not l.eoi:
    if l.head in JsonWhiteSpace:
      l.pos.inc
    else:
      break

proc parseNumber(l: var JsonLexer): Token =
  var num: string
  var isFloat = false
  var startOfNumber = l.pos
  var sgn = 1

  template error() =
    l.error(startOfNumber, "Invalid number: " & extractInvalidNumber(l.input, startOfNumber))

  # Sign
  if l.head == '-':
    sgn = -1
    l.pos.inc
  
  # NOTE: Signed NaN is not allowed, even with `allowSpecialFloats`
  if l.allowSpecialFloats and l.startsWith("Infinity"):
    l.pos.inc 8
    return Token(kind: tkNumber, numVal: sgn.float * Inf)

  # Integer part
  if l.head == '0':
    num.add '0'
    l.pos.inc
    if l.eoi:
      return Token(kind: tkInt64, int64Val: 0)
    if l.head in Digits:
      error()
  elif l.head in {'1' .. '9'}:
    l.extractInteger(appendTo = num)

  # Float part
  if not l.eoi and l.head == '.':
    isFloat = true
    num.add '.'
    l.pos.inc
    # NOTE: Unlike JSON5, JSON never allows a trailing '.'
    if l.head notin Digits:
      error()
    l.extractInteger(appendTo = num)

  # Exponent part
  if not l.eoi and l.head in {'e', 'E'}:
    isFloat = true
    num.add 'e'
    l.pos.inc
    if l.eoi:
      error()
    if l.head in {'+', '-'}:
      num.add l.head
      l.pos.inc
    if l.eoi or l.head notin Digits:
      error()
    l.extractInteger(appendTo = num)

  result =
    if isFloat:
      let f = parseFloat(num)
      if f <= float(high(int64)) and f >= float(low(int64)) and
          f == float(int64(f)):
        Token(kind: tkInt64, int64Val: sgn * int64(f))
      else:
        Token(kind: tkNumber, numVal: sgn.float * f)
    else:
      Token(kind: tkInt64, int64Val: sgn * parseBiggestInt(num))

proc parseString(l: var JsonLexer): Token =
  doAssert l.head == '"'
  var tok = Token(kind: tkString)
  let startOfString = l.pos
  l.pos.inc
  while true:
    if l.pos > l.input.high:
      l.error(startOfString, "Unclosed string")
    elif l.head == '"':
      l.pos.inc
      break
    elif l.head == '\\':
      l.pos.inc
      case l.head
      of 'u':
        l.pos.dec
        l.parseUEscape(appendTo = tok.strVal)
      of '"', '\\':
        tok.strVal.add l.head
        l.pos.inc
      of 'b':
        tok.strVal.add '\x08'
        l.pos.inc
      of 'f':
        tok.strVal.add '\x0C'
        l.pos.inc
      of 'n':
        tok.strVal.add '\x0A'
        l.pos.inc
      of 'r':
        tok.strVal.add '\x0D'
        l.pos.inc
      of 't':
        tok.strVal.add '\x09'
        l.pos.inc
      else:
        l.error(l.pos - 1, "Invalid escape: \\" & l.head)
    elif l.head <= 127.char:
      if l.head in {LF, CR}:
        l.error(startOfString, "Unclosed string")
      tok.strVal.add l.head
      l.pos.inc
    else:
      let (rune, len) = l.input.runeAndLenAt(l.pos)
      tok.strVal.add rune.toUtf8
      l.pos.inc len
  return tok

proc parseIdentifier(l: var JsonLexer): Token =
  # In strict JSON, `true`, `false`, and `null` are the only idents.
  if l.startsWith("true"):
    l.pos.inc 4
    return Token(kind: tkIdent, ident: "true")
  elif l.startsWith("false"):
    l.pos.inc 4
    return Token(kind: tkIdent, ident: "false")
  elif l.startsWith("null"):
    l.pos.inc 4
    return Token(kind: tkIdent, ident: "null")
  elif l.allowSpecialFloats:
    if l.startsWith("NaN"):
      l.pos.inc 3
      return Token(kind: tkNumber, numVal: NaN)
    elif l.startsWith("Infinity"):
      return Token(kind: tkNumber, numVal: Inf)
    else:
      l.error(l.pos, "Unexpected character: " & l.head)
  else:
    l.error(l.pos, "Unexpected character: " & l.head)

proc next*(l: var JsonLexer): Token =
  template singleCharToken(k: TokenKind) =
    l.pos.inc
    return Token(kind: k, pos: tokenStartPos)

  l.skipWhitespace()
  let tokenStartPos = l.pos

  if l.eoi:
    l.pos = l.input.high + 1
    return Token(kind: tkEoi, pos: tokenStartPos)

  case l.head
  of '[': singleCharToken(tkBracketL)
  of ']': singleCharToken(tkBracketR)
  of '{': singleCharToken(tkBraceL)
  of '}': singleCharToken(tkBraceR)
  of ',': singleCharToken(tkComma)
  of ':': singleCharToken(tkColon)

  of '/':
    if not l.allowComments:
      l.error(l.pos, "Comments are not allowed in strict JSON")
    l.parseComment(allowJson5Newlines = false)

  of '"':
    result = l.parseString()
    result.pos = tokenStartPos

  of Digits, '-':
    result = l.parseNumber()
    result.pos = tokenStartPos

  else:
    result = l.parseIdentifier()
    result.pos = tokenStartPos
