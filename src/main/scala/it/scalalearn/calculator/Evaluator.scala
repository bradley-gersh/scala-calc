package it.scalalearn.calculator

/**
 * Evaluates a given parse tree for the return value
 */
object Evaluator {

  /**
   * Evaluates the numerical value of a parse tree
   *
   * @param  tree  root node of the parse tree
   * @return       Either an error message or the numerical value of the parse tree
   */
  def eval(tree: ParseNode): Either[String, Double] = {
    evaluate(tree) match {
      case Left(error) => Left(error)
      case Right(value) if value.isInfinite => Left("infinite value obtained")
      case Right(value) if value == -0.0 => Right(0.0)
      case Right(value) => Right(value)
    }
  }

  /**
   * Recursively evaluates the numerical value of a parse tree
   *
   * @return  Either an error message or the computed value of the parse tree (possibly infinite)
   */
  def evaluate: ParseNode => Either[String, Double] = {
    case EmptyNode => Left("incomplete input; missing a sub-expression")

    case NumberNode(value) =>
      // a NaN value should not be found if the lexer works, so this will not be handled as a routine ParserException
      if (value.isNaN) Left("NaN value evaluated")
      else if (value.isInfinite) Left("infinite value evaluated")
      else Right(value)

    case SignNode(PLUS, expr) => evaluate(expr)
    case SignNode(DASH, expr) => evaluate(expr).map(-_)
    case SignNode(op, _) => Left(s"invalid unary operator `${op.string}`")

    case FactorNode(STAR, expr1, expr2) => evaluate(expr1).flatMap(val1 => evaluate(expr2).map(val2 => val1 * val2))
    case FactorNode(SLASH, expr1, expr2) =>
      val numerator = evaluate(expr1)
      val denominator = evaluate(expr2)
      (numerator, denominator) match {
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
        case (Right(0.0), Right(0.0)) => Left("indeterminate form 0/0 obtained")
        case (_, Right(0.0)) => Left("division by 0")
        case (Right(num), Right(denom)) => Right(num / denom)
      }

    case FactorNode(op, _, _) => Left(s"improper operation ${op.string} where multiplication or division was expected")

    case TermNode(PLUS, expr1, expr2) => evaluate(expr1).flatMap(val1 => evaluate(expr2).map(val2 => val1 + val2))
    case TermNode(DASH, expr1, expr2) => evaluate(expr1).flatMap(val1 => evaluate(expr2).map(val2 => val1 - val2))
    case TermNode(op, _, _) => Left(s"improper operation ${op.string} where addition or subtraction was expected")
  }
}