package it.scalalearn.calculator

import scala.util.Try

/**
 * Evaluates a given parse tree for the return value
 */
object Evaluator extends Function[ParseNode, Try[Double]] {

  /**
   * Public access to the Evaluator function
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

  /**
   * Recursively evaluates the numerical value of a parse tree
   *
   * @return                           computed value of the parse tree
   * @throws  IllegalArgumentException if a NaN value is obtained
   * @throws  ParserException          if an infinite value is obtained
   * @throws  ParserException          if the parse tree has an empty node
   * @throws  ParserException          in case of division by 0
   * @throws  ParserException          if an operator node contains an invalid operator
   *
   */
  def evaluate: ParseNode => Double = {
    case EmptyNode() => throw new ParserException("incomplete input; missing a sub-expression")

    case NumberNode(value) =>
      // a NaN value should not be found if the lexer works, so this will not be handled as a routine ParserException
      if (value.isNaN) throw new IllegalArgumentException("NaN value evaluated")
      else if (value.isInfinite) throw new ParserException("infinite value evaluated")
      else value

    case SignNode(PLUS, expr) => evaluate(expr)
    case SignNode(DASH, expr) => -evaluate(expr)
    case SignNode(op, _) => throw new ParserException(s"invalid unary operator `${op.string}`")

    case FactorNode(STAR, expr1, expr2) => evaluate(expr1) * evaluate(expr2)
    case FactorNode(SLASH, expr1, expr2) =>
      val numerator = evaluate(expr1)
      val denominator = evaluate(expr2)

      if (denominator == 0.0) {
        if (numerator == 0.0) throw new ParserException("indeterminate form 0/0 obtained")
        else throw new ParserException("division by zero")
      } else numerator / denominator
    case FactorNode(op, _, _) =>
      throw new ParserException(s"improper operation ${op.string} where multiplication or division was expected")

    case TermNode(PLUS, expr1, expr2) => evaluate(expr1) + evaluate(expr2)
    case TermNode(DASH, expr1, expr2) => evaluate(expr1) - evaluate(expr2)
    case TermNode(op, _, _) =>
      throw new ParserException(s"improper operation ${op.string} where addition or subtraction was expected")
  }
}