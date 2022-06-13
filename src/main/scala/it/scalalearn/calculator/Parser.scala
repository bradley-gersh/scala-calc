package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Parser singleton to transform tokens into a syntax tree
 */
object Parser {

  /**
   * Public access to the Parser object
   *
   * @param  tokens a list of Tokens lexed from user input
   * @return        Either an error message or the parse tree determined by the tokens
   */
  def apply(tokens: List[Token]): Either[String, ParseNode] = parseRoot(tokens)

  /**
   * Begin and terminate parsing by recursive descent
   *
   * @param  tokens          a list of lexed Tokens
   * @return                 Either an error message or the parse tree determined by the tokens
   */
  private def parseRoot(tokens: List[Token]): Either[String, ParseNode] =
    if (tokens.nonEmpty) {
      parseExpression(NestedParseState(tokens, EmptyNode, List[Token]())) match {
        case Left(error) => Left(error)

        case Right(NestedParseState(excessTokens, _, _)) if excessTokens.nonEmpty =>
          if (excessTokens.contains(RPAREN)) Left("unmatched `)`")
          else Left(s"unparsed tokens: ${excessTokens.foldLeft(mutable.StringBuilder())((acc, token) => acc.append(token.string))}")

        case Right(NestedParseState(_, _, excessParens)) if excessParens.nonEmpty => Left(s"unmatched `(`: depth ${excessParens.length}")

        case Right(NestedParseState(_, tree, _)) => Right(tree)
      }
    } else Right(EmptyNode)

  /**
   * Parses an expression, either wrapped in parentheses or in outermost scope
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. leftRootIn: if this expression is the second argument in an infix (binary) operator,
   *                                          represents node at the root of the parse tree of the first argument
   *                                          (not used here but passed through to the binary-operator nodes)
   *                           3. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 Either an error message or a parse state containing:
   *                           1. tokens: a list of tokens remaining after parsing this expression
   *                           2. tree:   the root node of the parse tree generated by this expression
   *                           3. parens: a list of unclosed parentheses after parsing this expression
   */
  @tailrec
  private def parseExpression(parseState: NestedParseState): Either[String, NestedParseState] =
    val NestedParseState(tokens, leftRootIn, parens) = parseState

    tokens match {
      case Nil => Right(NestedParseState(List(), leftRootIn, parens))

      case RPAREN :: rest =>
        if (parens.isEmpty) Left(s"unmatched `)`")
        else Right(NestedParseState(rest, leftRootIn, parens.tail))

      case _ =>
        parseTerm(NestedParseState(tokens, EmptyNode, parens)) match {
          case Left(error) => Left(error)
          case Right(NestedParseState(remainingTokens, newExpr, newParens)) =>
            if (newParens.nonEmpty) parseExpression(NestedParseState(remainingTokens, newExpr, newParens))
            else Right(NestedParseState(remainingTokens, newExpr, newParens))
        }
    }

  /**
   * Parses a term (sum or difference) expression, including left associativity
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. leftRootIn: node at the root of the parse tree of the first argument to this function
   *                           3. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 Either an error message or a parse state containing:
   *                           1. tokens:     a list of tokens remaining after parsing this expression
   *                           2. tree:       the root node of the parse tree generated by this expression
   *                           3. parens:     a list of unclosed parentheses after parsing this expression
   */
  @tailrec
  private def parseTerm(parseState: NestedParseState): Either[String, NestedParseState] = {
    val NestedParseState(tokens, leftRootIn, parens) = parseState

    // Process left expression
    val NestedParseState(tokensAfterLeft, leftRoot, newParensLeft) =
      if (leftRootIn.isEmpty) parseFactor(NestedParseState(tokens, leftRootIn, parens))
      else NestedParseState(tokens, leftRootIn, parens)

    // Process right expression
    tokensAfterLeft match {
      case Nil => Right(NestedParseState(tokensAfterLeft, leftRoot, newParensLeft))
      case (first @ (PLUS | DASH)) :: rest =>
        parseFactor(NestedParseState(rest, EmptyNode, newParensLeft)) match {
          case Left(error) => Left(error)
          case Right(NestedParseState(tokensAfterRight, rightRoot, newParensRight)) =>
            parseTerm(NestedParseState(tokensAfterRight, TermNode(first, leftRoot, rightRoot), newParensRight))
        }
      case _ => Right(NestedParseState(tokensAfterLeft, leftRoot, newParensLeft))
    }
  }

  /**
   * Parses a factor (product or quotient) expression, including left associativity
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. leftRootIn: node at the root of the parse tree of the first argument to this function
   *                           3. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 Either an error message or a parse state containing:
   *                           1. tokens:     a list of tokens remaining after parsing this expression
   *                           2. tree:       the root node of the parse tree generated by this expression
   *                           3. parens:     a list of unclosed parentheses after parsing this expression
   */
  @tailrec
  private def parseFactor(parseState: NestedParseState): Either[String, NestedParseState] = {
    val NestedParseState(tokens, leftRootIn, parens) = parseState

    // Process left expression
    val NestedParseState(tokensAfterLeft, leftRoot, newParensLeft) =
      if (leftRootIn.isEmpty) parseSign(SimpleParseState(tokens, parens))
      else Right(NestedParseState(tokens, leftRootIn, parens))

    // Process right expression
    tokensAfterLeft match {
      case Nil => Right(NestedParseState(tokensAfterLeft, leftRoot, newParensLeft))

      case (first @ (STAR | SLASH)) :: rest => parseSign(SimpleParseState(rest, newParensLeft)) match {
        case Left(error) => Left(error)
        case Right(NestedParseState(tokensAfterRight, rightRoot, newParensRight)) =>
          parseFactor(NestedParseState(tokensAfterRight, FactorNode(first, leftRoot, rightRoot), newParensRight))
      }

      case _ => Right(NestedParseState(tokensAfterLeft, leftRoot, newParensLeft))
    }
  }

  /**
   * Parses unary (sign) expression, including right associativity
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 Either an error message or a parse state containing:
   *                           1. tokens:     a list of tokens remaining after parsing this expression
   *                           2. tree:       the root node of the parse tree generated by this expression
   *                           3. parens:     a list of unclosed parentheses after parsing this expression
   */
  private def parseSign(parseState: SimpleParseState): Either[String, NestedParseState] = {
    val SimpleParseState(tokens, parens) = parseState

    tokens match {
      case Nil => Left("expression terminated where a value was expected")

      case DASH :: rest => parseSign(SimpleParseState(rest, parens)) match {
        case Left(error) => Left(error)
        case Right(NestedParseState(newTokens, number, newParens)) =>
          Right(NestedParseState(newTokens, SignNode(DASH, number), newParens))
      }

      case _ => parseNumber(SimpleParseState(tokens, parens))
    }
  }

  /**
   * Parses a number token or expression into either a numeric terminal node or a nested parse tree
   *
   * @param  parseState      wrapper for current parse state, containing:
   *                           1. tokens:     list of tokens remaining to be parsed in this expression
   *                           2. parens:     a list of as-yet unclosed parentheses at this level
   * @return                 Either an error message or a parse state containing:
   *                           1. tokens:     a list of tokens remaining after parsing this expression
   *                           2. tree:       the root node of the parse tree generated by this expression
   *                           3. parens:     a list of unclosed parentheses after parsing this expression
   */
  private def parseNumber(parseState: SimpleParseState): Either[String, NestedParseState] = {
    val SimpleParseState(tokens, parens) = parseState

    tokens match {
      case Nil => Left("expression terminated where a value was expected")

      case NUMBER(string) :: rest =>
        if (string.toDouble.isInfinite) Left("infinite value obtained")
        else Right(NestedParseState(rest, NumberNode(string.toDouble), parens))

      case LPAREN :: rest => parseExpression(NestedParseState(rest, EmptyNode, LPAREN +: parens))

      case _ => Left(s"found `${tokens.head.string}` where a value was expected")
    }
  }
}

