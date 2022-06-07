package it.scalalearn.calculator

import scala.annotation.tailrec

object Parser {

  @tailrec
  def parseExpression(tokens: List[Token], rootIn: ParseNode, parenLevel: List[Token]): (List[Token], ParseNode, List[Token]) =
    if (tokens.isEmpty) (List(), rootIn, parenLevel)
    else {
      val t = tokens.head
      if (t.tokenType == TokenType.RPAREN) {
        if (parenLevel.isEmpty) throw new ParserException(s"unmatched ) character(s)")
        else (tokens.tail, rootIn, parenLevel.tail)
      } else {
        val (remainingTokens, newExpr, newParenLevel) = parseTerm(tokens, EmptyNode(), parenLevel)
        parseExpression(remainingTokens, newExpr, newParenLevel)
      }
    }

  @tailrec
  def parseTerm(tokens: List[Token], leftRootIn: ParseNode, parenLevel: List[Token]): (List[Token], ParseNode, List[Token]) = {
    val (tokensAfterLeft, leftRoot, newParenLevelLeft) =
      if (leftRootIn.isEmpty) parseFactor(tokens, leftRootIn, parenLevel)
      else (tokens, leftRootIn, parenLevel)

    if (tokensAfterLeft.isEmpty) (tokensAfterLeft, leftRoot, newParenLevelLeft)
    else {
      val t = tokensAfterLeft.head
      if (t.tokenType == TokenType.PLUS || t.tokenType == TokenType.DASH) {
        val (tokensAfterRight, rightRoot, newParenLevelRight) =
          parseFactor(tokensAfterLeft.tail, EmptyNode(), newParenLevelLeft)
        parseTerm(tokensAfterRight, TermNode(t, leftRoot, rightRoot), newParenLevelRight)
      } else (tokensAfterLeft, leftRoot, newParenLevelLeft)
    }
  }

  @tailrec
  def parseFactor(tokens: List[Token], leftRootIn: ParseNode, parenLevel: List[Token]): (List[Token], ParseNode, List[Token]) = {
    val (tokensAfterLeft, leftRoot, newParenLevelLeft) =
      if (leftRootIn.isEmpty) parseSign(tokens, parenLevel)
      else (tokens, leftRootIn, parenLevel)

    if (tokensAfterLeft.isEmpty) (tokensAfterLeft, leftRoot, newParenLevelLeft)
    else {
      val t = tokensAfterLeft.head
      if (t.tokenType == TokenType.STAR || t.tokenType == TokenType.SLASH) {
        val (tokensAfterRight, rightRoot, newParenLevelRight) =
          parseSign(tokensAfterLeft.tail, newParenLevelLeft)
        parseFactor(tokensAfterRight, FactorNode(t, leftRoot, rightRoot), newParenLevelRight)
      } else (tokensAfterLeft, leftRoot, newParenLevelLeft)
    }
  }

  def parseSign(tokens: List[Token], parenLevel: List[Token]): (List[Token], ParseNode, List[Token]) = {
    val t = tokens.head
    if (t.tokenType == TokenType.DASH) {
      val (remainingTokens, number, newParenLevel) = parseNumber(tokens.tail, parenLevel)
      (remainingTokens, SignNode(t, number), newParenLevel)
    }
    else parseNumber(tokens, parenLevel)
  }

  def parseNumber(tokens: List[Token], parenLevel: List[Token]): (List[Token], ParseNode, List[Token]) = {
    val t = tokens.head
    if (t.tokenType == TokenType.NUMBER) (tokens.tail, NumberNode(t.string.toDouble), parenLevel)
    else if (t.tokenType == TokenType.LPAREN) {
      val (remainingTokens: List[Token], expr: ParseNode, newParenLevel) =
        parseExpression(tokens.tail, EmptyNode(), t +: parenLevel)
      (remainingTokens, expr, newParenLevel)
    }
    else if (t.tokenType == TokenType.DASH) throw new ParserException("`--` sequence not permitted")
    else throw new ParserException(s"parse error at token $t in number term")
  }

  /**
   * Parses tokens into tree and evaluates computation
   *
   * @param  tokens a list of Tokens lexed from user input
   * @return        the parse tree determined by the tokens
   */
  def apply(tokens: List[Token]): ParseNode = {
    val (leftoverTokens, tree, leftoverParens) = parseExpression(tokens, EmptyNode(), List[Token]())
    if (leftoverTokens.nonEmpty) throw new ParserException(s"unparsed tokens: $leftoverTokens")
    if (leftoverParens.nonEmpty) throw new ParserException(s"unmatched ( character(s)3 *: depth ${leftoverParens.length}")
    printer(tree)
    tree
  }

  /**
   * Prints a syntax tree in S-expressions.
   *
   * @param tree Root node of tree to be printed.
   */
  def printer(tree: ParseNode): Unit = {
    println(tree.printMe())
  }

}

class ParserException(private val message: String) extends RuntimeException(message) {
  override def getMessage: String = s"[error] $message"
}