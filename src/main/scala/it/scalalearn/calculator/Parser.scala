package it.scalalearn.calculator

object Parser {

  def parseExpression(tokens: List[Token]): (List[Token], ParseNode) =
    parseTerm(tokens)

  def parseTerm(tokens: List[Token]): (List[Token], ParseNode) = {
    val (remainingTokens1, expr1) = parseFactor(tokens)
    if (remainingTokens1.isEmpty) { // may need to make a while loop for multiple operators w/o parens
      (remainingTokens1, expr1)
    } else {
      val t = remainingTokens1.head
      if (t.tokenType == TokenType.PLUS || t.tokenType == TokenType.DASH) {
        val (remainingTokens2, expr2) = parseFactor(remainingTokens1.tail)
        (remainingTokens2, Term(t, expr1, expr2))
      } else (remainingTokens1, expr1)
    }
  }

  def parseFactor(tokens: List[Token]): (List[Token], ParseNode) = {
    val (remainingTokens1, expr1) = parseSign(tokens)
    if (remainingTokens1.isEmpty) (remainingTokens1, expr1)
    else {
      val t = remainingTokens1.head
      if (t.tokenType == TokenType.STAR || t.tokenType == TokenType.SLASH) {
        val (remainingTokens2, expr2) = parseSign(remainingTokens1.tail)
        (remainingTokens2, Factor(t, expr1, expr2))
      } else (remainingTokens1, expr1)
    }
  }

  def parseSign(tokens: List[Token]): (List[Token], ParseNode) = {
    val t = tokens.head
    if (t.tokenType == TokenType.DASH) {
      val (remainingTokens, number) = parseNumber(tokens.tail)
      (remainingTokens, Sign(t, number))
    }
    else parseNumber(tokens)
  }

  def parseNumber(tokens: List[Token]): (List[Token], ParseNode) = {
    val t = tokens.head
    println(s"processing token ${t.string}")
    if (t.tokenType == TokenType.NUMBER) (tokens.tail, Number(t.string.toDouble))
    else throw new RuntimeException(s"[error] parse error at token $t")
  }

  /**
   * Parses tokens into tree and evaluates computation
   *
   * @param  tokens a list of Tokens lexed from user input
   * @return        the parse tree determined by the tokens
   */
  def apply(tokens: List[Token]): ParseNode = {
    val (leftoverTokens, tree) = parseExpression(tokens)
    printer(tree)
    tree
  }

  def printer(tree: ParseNode): Unit = {
    println(tree.printMe())
  }

}
