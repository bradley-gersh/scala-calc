package it.scalalearn.calculator

import scala.util.Try

/**
 * Evaluates a given parse tree for the return value
 */
object Evaluator extends Function[ParseNode, Try[Double]] {
  /**
   * Public access to Evaluator function
   *
   * @param  tree  root node of the parse tree
   * @return       Try of the numerical value associated with evaluating the parse tree
   */
  def apply(tree: ParseNode): Try[Double] = {
    val value = Try(evaluate(tree))

    if (value.isSuccess) {
      if (value.get.isInfinite) Try(throw new EvaluatorException("infinite value obtained"))
      else if (value.get == -0.0) Try(0.0) // the JVM allows -0.0, but we will not
      else value
    } else value
  }

  private val evaluate: ParseNode => Double = {
    case EmptyNode() => throw new ParserException("incomplete input; missing a sub-expression")
    case NumberNode(value) =>
      // a NaN value should not be found if the lexer works, so this will not be handled as a routine ParserException
      if (value.isNaN) throw new IllegalArgumentException("NaN value evaluated")
      else if (value.isInfinite) throw new ParserException("infinite value evaluated")
      else value
    case SignNode(sign, expr) =>
      sign.tokenType match {
        case TokenType.PLUS => evaluate(expr)
        case TokenType.DASH => -evaluate(expr)
        case _ => throw new ParserException(s"invalid unary operator `${sign.string}`")
      }
    case FactorNode(op, expr1, expr2) =>
      if (op.tokenType == TokenType.STAR) evaluate(expr1) * evaluate(expr2)
      else if (op.tokenType == TokenType.SLASH) {
        val numerator = evaluate(expr1)
        val denominator = evaluate(expr2)

        if (denominator == 0.0) {
          if (numerator == 0.0) throw new ParserException("indeterminate form 0/0 obtained")
          else throw new ParserException("division by zero")
        } else numerator / denominator
      }
      else throw new ParserException(s"improper operation ${op.string} where multiplication or division was expected")
    case TermNode(op, expr1, expr2) =>
      if (op.tokenType == TokenType.PLUS) evaluate(expr1) + evaluate(expr2)
      else if (op.tokenType == TokenType.DASH) evaluate(expr1) - evaluate(expr2)
      else throw new ParserException(s"improper operation ${op.string} where addition or subtraction was expected")
  }
}