import std / unicode
import generated / unicode_data

export unicode

template ones(n: untyped): untyped = ((1 shl n)-1)

const InvalidChars = {'\xC0', '\xC1', '\xF5' .. '\xFF'}
const SurrogateRange* = 0xD800.Rune .. 0xDFFF.Rune
const ReplacementRune* = 0xFFFD.Rune

proc contains*(slice: Slice[Rune], r: Rune): bool =
  slice.a <=% r and r <=% slice.b

proc isOverlongEncoded(r: Rune, len: range[1..4]): bool =
  # See https://en.wikipedia.org/wiki/UTF-8#Description
  case len
  of 1:
    r <% 0x0000.Rune or 0x007F.Rune <% r
  of 2:
    r <% 0x0080.Rune or 0x07FF.Rune <% r
  of 3:
    r <% 0x0800.Rune or 0xFFFF.Rune <% r
  of 4:
    r <% 0x10000.Rune or 0x10FFFF.Rune <% r

proc runeAndLenAt*(s: string, i: Natural, allowSurrogate = false): (Rune, int) =
  ## Retrieve the rune starting at the byte inedx `i` as well as the
  ## number of bytes taken up by the rune.
  ##
  ## If there's no valid byte starting at `s[i]`,
  ## returns `(ReplacementRune, 1)`.
  ##
  ## If `allowSurrogate` is true, surrogate code points are treated as valid
  ## UTF-8 (even though they are not).
  var checkForOverlong = true
  result =
    if ord(s[i]) <=% 127:
      (Rune(s[i]), 1)
    elif s[i] in InvalidChars or ord(s[i]) shr 6 == 0b10:
      (ReplacementRune, 1)
    elif ord(s[i]) shr 5 == 0b110 and i <= s.len - 2 and
        ord(s[i+1]) shr 6 == 0b10:
      (Rune(
        (ord(s[i]) and (ones(5))) shl 6 or
        (ord(s[i+1]) and ones(6))), 2)
    elif ord(s[i]) shr 4 == 0b1110 and i <= s.len - 3 and
        ord(s[i+1]) shr 6 == 0b10 and
        ord(s[i+2]) shr 6 == 0b10:
      (Rune(
        (ord(s[i]) and ones(4)) shl 12 or
        (ord(s[i+1]) and ones(6)) shl 6 or
        (ord(s[i+2]) and ones(6))), 3)
    elif ord(s[i]) shr 3 == 0b11110 and i <= s.len - 4 and
        ord(s[i+1]) shr 6 == 0b10 and
        ord(s[i+2]) shr 6 == 0b10 and
        ord(s[i+3]) shr 6 == 0b10:
      (Rune(
        (ord(s[i]) and ones(3)) shl 18 or
        (ord(s[i+1]) and ones(6)) shl 12 or
        (ord(s[i+2]) and ones(6)) shl 6 or
        (ord(s[i+3]) and ones(6))), 4)
    else:
      checkForOverlong = false
      (ReplacementRune, 1)

  if checkForOverlong:
    if isOverlongEncoded(result[0], result[1]):
      result = (ReplacementRune, 1)

  if not allowSurrogate and result[0] in SurrogateRange:
    result = (ReplacementRune, 1)

proc decodeSurrogates*(high, low: Rune): Rune =
  ## Convert a UTF-16 surrogate pair to the corresponding Rune.
  assert (high in SurrogateRange) and (low in SurrogateRange)
  let (high, low) = (high.int32, low.int32)
  let a = (high - 0xD800) * 0x400
  let b = low - 0xDC00
  result = (0x10000 + (a or b)).Rune

template isXImpl(name: untyped, r: Rune): bool =
  var isX = false
  block outer:
    for i in countup(`name Ranges`.low, `name Ranges`.high, 2):
      if `name Ranges`[i] <=% r and r <=% `name Ranges`[i + 1]:
        isX = true
        break outer
    isX = r in `name Singles`
  isX

proc isLetter*(r: Rune): bool = isXImpl(Letter, r)
proc isCombiningMark*(r: Rune): bool = isXImpl(CombiningMark, r)
proc isDigit*(r: Rune): bool = isXImpl(Digit, r)
proc isConnectorPunctuation*(r: Rune): bool = isXImpl(ConnectorPunctuation, r)
