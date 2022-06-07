package it.scalalearn.calculator

import scala.annotation.tailrec

object Parser {

  def parseExpression(tokens: List[Token], rootIn: ParseNode): (List[Token], ParseNode) =
    if (tokens.isEmpty) (List(), rootIn)
    else parseTerm(tokens, EmptyNode())

  @tailrec
  def parseTerm(tokens: List[Token], leftRootIn: ParseNode): (List[Token], ParseNode) = {
    val (tokensAfterLeft, leftRoot) =
      if (leftRootIn.isEmpty) parseFactor(tokens, leftRootIn)
      else (tokens, leftRootIn)

    if (tokensAfterLeft.isEmpty) (tokensAfterLeft, leftRoot)
    else {
      val t = tokensAfterLeft.head
      if (t.tokenType == TokenType.PLUS || t.tokenType == TokenType.DASH) {
        val (tokensAfterRight, rightRoot) = parseFactor(tokensAfterLeft.tail, EmptyNode())
        parseTerm(tokensAfterRight, TermNode(t, leftRoot, rightRoot))
      } else throw new ParserException(s"parse error at token $t in addition term")
    }
  }

  @tailrec
  def parseFactor(tokens: List[Token], leftRootIn: ParseNode): (List[Token], ParseNode) = {
    val (tokensAfterLeft, leftRoot) =
      if (leftRootIn.isEmpty) parseSign(tokens)
      else (tokens, leftRootIn)

    if (tokensAfterLeft.isEmpty) (tokensAfterLeft, leftRoot)
    else {
      val t = tokensAfterLeft.head
      if (t.tokenType == TokenType.STAR || t.tokenType == TokenType.SLASH) {
        val (tokensAfterRight, rightRoot) = parseSign(tokensAfterLeft.tail)
        parseFactor(tokensAfterRight, FactorNode(t, leftRoot, rightRoot))
      } else (tokensAfterLeft, leftRoot)
    }
  }

  def parseSign(tokens: List[Token]): (List[Token], ParseNode) = {
    val t = tokens.head
    if (t.tokenType == TokenType.DASH) {
      val (remainingTokens, number) = parseNumber(tokens.tail)
      (remainingTokens, SignNode(t, number))
    }
    else parseNumber(tokens)
  }

  def parseNumber(tokens: List[Token]): (List[Token], ParseNode) = {
    val t = tokens.head
    println(s"processing token ${t.string}")
    if (t.tokenType == TokenType.NUMBER) (tokens.tail, NumberNode(t.string.toDouble))
    else if (t.tokenType == TokenType.DASH) throw new ParserException("`--` sequence not permitted")
    else throw new ParserException(s"parse error at token $t")
  }

  /**
   * Parses tokens into tree and evaluates computation
   *
   * @param  tokens a list of Tokens lexed from user input
   * @return        the parse tree determined by the tokens
   */
  def apply(tokens: List[Token]): ParseNode = {
    val (leftoverTokens, tree) = parseExpression(tokens, EmptyNode())
    if (leftoverTokens.nonEmpty) throw new ParserException(s"unparsed tokens: $leftoverTokens")
    printer(tree)
    tree
  }

  /**
   * Prints a syntax tree in S-expressions.
   * @param tree Root node of tree to be printed.
   */
  def printer(tree: ParseNode): Unit = {
    println(tree.printMe())
  }

}

class ParserException(private val message: String) extends RuntimeException(message) {
  override def getMessage: String = s"[error] $message"
}