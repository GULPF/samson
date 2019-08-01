import std / tables
import jtrees, lexer

type
  Parser[T: Lexer] = object
    l: T
    tok: Token
    allowTrailingComma: bool
    allowIdentifierObjectKey: bool

template next(p: var Parser) =
  p.tok = p.l.next()

proc parseValue(p: var Parser, jtree: var JTree): int =
  template jnode: var JNode = jtree.nodes[result]
  template reserveNode(jn: JNode) =
    jtree.nodes.add(jn)
    result = jtree.nodes.high

  case p.tok.kind

  of tkBracketL:
    p.next()
    reserveNode(JNode(kind: nkArray))
    # Empty array
    if p.tok.kind == tkBracketR:
      p.next()
    else:
      while true:
        let nodeIdx = p.parseValue(jtree)
        jnode.items.add(nodeIdx)
        if p.tok.kind == tkComma:
          let commaPos = p.tok.pos
          p.next()
          if p.tok.kind == tkBracketR:
            if p.allowTrailingComma:
              p.next()
              break
            else:
              p.l.error(commaPos, "Trailing comma")
        elif p.tok.kind == tkBracketR:
          p.next()
          break
        else:
          p.l.error(p.tok.pos, "Expected ',' or ']' but found: " & $p.tok)

  of tkBraceL:
    p.next()
    reserveNode(JNode(kind: nkObject))
    jnode.kvpairs = initOrderedTable[string, JNodeIdx](4)
    # Empty object
    if p.tok.kind == tkBraceR:
      p.next()
    else:
      while true:
        var key: string
        case p.tok.kind
        of tkString: shallowCopy(key, p.tok.strVal)
        of tkIdent:
          if p.allowIdentifierObjectKey:
            shallowCopy(key, p.tok.ident)
          else:
            p.l.error(p.tok.pos, "Expected object key but found: " & $p.tok)          
        else:
          p.l.error(p.tok.pos, "Expected object key but found: " & $p.tok)
        p.next()

        if p.tok.kind != tkColon:
          p.l.error(p.tok.pos, "Expected ':' but found: " & $p.tok)
        p.next()

        let nodeIdx = p.parseValue(jtree)
        jnode.kvpairs[key] = nodeIdx

        if p.tok.kind == tkComma:
          let commaPos = p.tok.pos
          p.next()
          if p.tok.kind == tkBraceR:
            if p.allowTrailingComma:
              p.next()
              break
            else:
              p.l.error(commaPos, "Trailing comma")
        elif p.tok.kind == tkBraceR:
          p.next()
          break
        else:
          p.l.error(p.tok.pos, "Expected ',' or '}' but found: " & $p.tok)

  of tkNumber:
    reserveNode(JNode(kind: nkNumber, numVal: p.tok.numVal))
    p.next()

  of tkInt64:
    reserveNode(JNode(kind: nkInt64, int64Val: p.tok.int64Val))
    p.next()

  of tkString:
    reserveNode(JNode(kind: nkString))
    shallowCopy(jnode.strVal, p.tok.strVal)
    p.next()

  of tkIdent:
    case p.tok.ident
    of "true":
      reserveNode(JNode(kind: nkBool, boolVal: true))
    of "false":
      reserveNode(JNode(kind: nkBool, boolVal: false))
    of "NaN":
      reserveNode(JNode(kind: nkNumber, numVal: NaN))
    of "Infinity":
      reserveNode(JNode(kind: nkNumber, numVal: Inf))
    of "null":
      reserveNode(JNode(kind: nkNull))
    else:
      p.l.error(p.tok.pos, "Expected value but found identifier: " & p.tok.ident)
    p.next()

  else:
    p.l.error(p.tok.pos, "Expected value but found: " & $p.tok)

template parseImpl(p: var Parser) =
  result = JTree(nodes: @[])
  p.next()
  if p.tok.kind == tkEoi:
    p.l.error(0, "An empty string is not valid JSON5")
  discard parseValue(p, result)

  if p.tok.kind != tkEoi:
    p.l.error(p.tok.pos, "Expected end of input but found: " & $p.tok)

proc parseJson5*(input: string): JTree =
  var p = Parser[Json5Lexer](
    l: initJson5Lexer(input),
    allowTrailingComma: true,
    allowIdentifierObjectKey: true
  )
  parseImpl(p)

proc parseJson*(input: string,
    allowComments: bool,
    allowSpecialFloats: bool,
    allowTrailingComma: bool): JTree =
  let l = initJsonLexer(input,
    allowComments, allowSpecialFloats)
  var p = Parser[JsonLexer](
    l: l,
    allowTrailingComma: allowTrailingComma,
    allowIdentifierObjectKey: false
  )
  parseImpl(p)