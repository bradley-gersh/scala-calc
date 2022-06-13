package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

/**
 * Parser singleton to transform tokens into a syntax tree
 */
object Parser {

  /**
   * Public access to the Parser object
   *
   * @param  tokens a list of Tokens lexed from user input
   * @return        Try object wrapping the parse tree determined by the tokens
   */
  def apply(tokens: List[Token]): Try[ParseNode] = {
    Try(parseRoot(tokens))
  }

  /**
   * Begin and terminate parsing by recursive descent
   *
   * @param  tokens          a list of lexed Tokens
   * @return                 the parse tree determined by the tokens
   * @throws ParserException if there are leftover unparsed tokens
   * @throws ParserException if there are leftover unmatched parentheses
   */
  private def parseRoot(tokens: List[Token]): ParseNode =
    if (tokens.nonEmpty) {
      parseExpression(NestedParseState(tokens, EmptyNode, List[Token]())) match {
        case NestedParseState(excessTokens, _, _) if excessTokens.nonEmpty =>
          if (excessTokens.contains(RPAREN)) throw new ParserException("unmatched `)`")
          else throw new ParserException(
            s"unparsed tokens: ${excessTokens.foldLeft(mutable.StringBuilder())((acc, token) => acc.append(token.string))}")
        case NestedParseState(_, _, excessParens) if excessParens.nonEmpty =>
          throw new ParserException(s"unmatched `(`: depth ${excessParens.length}")
        case NestedParseState(_, tree, _) => tree
      }
    } else EmptyNode

  /**
   * Parses an expression, either wrapped in parentheses or in outermost scope
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. leftRootIn: if this expression is the second argument in an infix (binary) operator,
   *                                          represents node at the root of the parse tree of the first argument
   *                                          (not used here but passed through to the binary-operator nodes)
   *                           3. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 a parse state containing:
   *                           1. tokens: a list of tokens remaining after parsing this expression
   *                           2. tree:   the root node of the parse tree generated by this expression
   *                           3. parens: a list of unclosed parentheses after parsing this expression
   * @throws ParserException if an unmatched right parenthesis is detected
   */
  @tailrec
  private def parseExpression(parseState: NestedParseState): NestedParseState =
    val NestedParseState(tokens, leftRootIn, parens) = parseState

    tokens match {
      case Nil => NestedParseState(List(), leftRootIn, parens)
      case RPAREN :: rest =>
        if (parens.isEmpty) throw new ParserException(s"unmatched `)`")
        else NestedParseState(rest, leftRootIn, parens.tail)
      case _ =>
        val NestedParseState(remainingTokens, newExpr, newParens) =
          parseTerm(NestedParseState(tokens, EmptyNode, parens))
        if (newParens.nonEmpty) parseExpression(NestedParseState(remainingTokens, newExpr, newParens))
        else NestedParseState(remainingTokens, newExpr, newParens)
    }

  /**
   * Parses a term (sum or difference) expression, including left associativity
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. leftRootIn: node at the root of the parse tree of the first argument to this function
   *                           3. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 a parse state containing:
   *                           1. tokens:     a list of tokens remaining after parsing this expression
   *                           2. tree:       the root node of the parse tree generated by this expression
   *                           3. parens:     a list of unclosed parentheses after parsing this expression
   */
  @tailrec
  private def parseTerm(parseState: NestedParseState): NestedParseState = {
    val NestedParseState(tokens, leftRootIn, parens) = parseState

    // Process left expression
    val NestedParseState(tokensAfterLeft, leftRoot, newParensLeft) =
      if (leftRootIn.isEmpty) parseFactor(NestedParseState(tokens, leftRootIn, parens))
      else NestedParseState(tokens, leftRootIn, parens)

    // Process right expression
    tokensAfterLeft match {
      case Nil => NestedParseState(tokensAfterLeft, leftRoot, newParensLeft)
      case (first @ (PLUS | DASH)) :: rest =>
        val NestedParseState(tokensAfterRight, rightRoot, newParensRight) =
          parseFactor(NestedParseState(rest, EmptyNode, newParensLeft))
        parseTerm(NestedParseState(tokensAfterRight, TermNode(first, leftRoot, rightRoot), newParensRight))
      case _ => NestedParseState(tokensAfterLeft, leftRoot, newParensLeft)
    }
  }

  /**
   * Parses a factor (product or quotient) expression, including left associativity
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. leftRootIn: node at the root of the parse tree of the first argument to this function
   *                           3. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 a parse state containing:
   *                           1. tokens:     a list of tokens remaining after parsing this expression
   *                           2. tree:       the root node of the parse tree generated by this expression
   *                           3. parens:     a list of unclosed parentheses after parsing this expression
   */
  @tailrec
  private def parseFactor(parseState: NestedParseState): NestedParseState = {
    val NestedParseState(tokens, leftRootIn, parens) = parseState

    // Process left expression
    val NestedParseState(tokensAfterLeft, leftRoot, newParensLeft) =
      if (leftRootIn.isEmpty) parseSign(SimpleParseState(tokens, parens))
      else NestedParseState(tokens, leftRootIn, parens)

    // Process right expression
    tokensAfterLeft match {
      case Nil => NestedParseState(tokensAfterLeft, leftRoot, newParensLeft)
      case (first @ (STAR | SLASH)) :: rest =>
        val NestedParseState(tokensAfterRight, rightRoot, newParensRight) =
          parseSign(SimpleParseState(rest, newParensLeft))
        parseFactor(NestedParseState(tokensAfterRight, FactorNode(first, leftRoot, rightRoot), newParensRight))
      case _ => NestedParseState(tokensAfterLeft, leftRoot, newParensLeft)
    }
  }

  /**
   * Parses unary (sign) expression, including right associativity
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 a parse state containing:
   *                           1. tokens:     a list of tokens remaining after parsing this expression
   *                           2. tree:       the root node of the parse tree generated by this expression
   *                           3. parens:     a list of unclosed parentheses after parsing this expression
   */
  private def parseSign(parseState: SimpleParseState): NestedParseState = {
    val SimpleParseState(tokens, parens) = parseState

    tokens match {
      case Nil => throw new ParserException("expression terminated where a value was expected")
      case DASH :: rest =>
        val NestedParseState(newTokens, number, newParens) = parseSign(SimpleParseState(rest, parens))
        NestedParseState(newTokens, SignNode(DASH, number), newParens)
      case _ => parseNumber(SimpleParseState(tokens, parens))
    }
  }

  /**
   * Parses a number token or expression into either a numeric terminal node or a nested parse tree
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 a parse state containing:
   *                           1. tokens:     a list of tokens remaining after parsing this expression
   *                           2. tree:       the root node of the parse tree generated by this expression
   *                           3. parens:     a list of unclosed parentheses after parsing this expression
   * @throws ParserException if an infinite value (infinity or divide by zero) is obtained
   * @throws ParserException if an unexpected token is found where a number or parenthesized expression should be
   */
  private def parseNumber(parseState: SimpleParseState): NestedParseState = {
    val SimpleParseState(tokens, parens) = parseState

    tokens match {
      case Nil => throw new ParserException("expression terminated where a value was expected")
      case NUMBER(string) :: rest =>
        val value = string.toDouble
        if (value.isInfinite) throw new ParserException("infinite value obtained")
        else NestedParseState(rest, NumberNode(string.toDouble), parens)
      case LPAREN :: rest => parseExpression(NestedParseState(rest, EmptyNode, LPAREN +: parens))
      case _ => throw new ParserException(s"found `${tokens.head.string}` where a value was expected")
    }
  }
}

