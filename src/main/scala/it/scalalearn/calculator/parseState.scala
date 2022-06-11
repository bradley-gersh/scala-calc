package it.scalalearn.calculator

sealed abstract class ParseState(tokens: List[Token], parens: List[Token])

case class SimpleParseState(tokens: List[Token], parens: List[Token]) extends ParseState(tokens, parens)

case class NestedParseState(tokens: List[Token], tree: ParseNode, parens: List[Token]) extends ParseState(tokens, parens)

