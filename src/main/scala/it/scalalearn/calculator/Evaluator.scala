package it.scalalearn.calculator

/**
 * Evaluates a given parse tree for the return value
 */
object Evaluator {

  /**
   * Evaluates the numerical value of a parse tree
   *
   * @param  tree  root node of the parse tree
   * @return       Either an error or the numerical value of the parse tree
   */
  def eval(tree: ParseNode): Either[CalculatorException, Double] = {
    evaluate(tree) match {
      case Left(error) => Left(error)
      case Right(value) if value.isInfinite => Left(InfiniteValueException("infinite value obtained"))
      case Right(value) if value == -0.0 => Right(0.0)
      case Right(value) => Right(value)
    }
  }

  /**
   * Recursively evaluates the numerical value of a parse tree
   *
   * @return  Either an error or the computed value of the parse tree (possibly infinite)
   */
  def evaluate: ParseNode => Either[CalculatorException, Double] = {
    case EmptyNode => Left(EmptyNodeException("incomplete input; missing a sub-expression"))

    case NumberNode(value) =>
      // a NaN value should not be found if the lexer works, so this will not be handled as a routine ParserException
      if (value.isNaN) Left(NaNValueException("NaN value evaluated"))
      else if (value.isInfinite) Left(InfiniteValueException("infinite value evaluated"))
      else Right(value)

    case SignNode(PLUS, expr) => evaluate(expr)
    case SignNode(DASH, expr) => evaluate(expr).map(-_)
    case SignNode(op, _) => Left(InvalidOperatorException(s"invalid unary operator `${op.string}`"))

    case FactorNode(STAR, expr1, expr2) => evaluate(expr1).flatMap(val1 => evaluate(expr2).map(val2 => val1 * val2))
    case FactorNode(SLASH, expr1, expr2) =>
      for {
        numerator <- evaluate(expr1)
        denominator <- evaluate(expr2)
        tuple <- validateDivision(numerator, denominator)
      } yield tuple._1 / tuple._2 // destructuring not allowed on tuples in for comprehensions

    case FactorNode(op, _, _) => Left(InvalidOperatorException(s"improper operation ${op.string} where multiplication or division was expected"))

    case TermNode(PLUS, expr1, expr2) => evaluate(expr1).flatMap(val1 => evaluate(expr2).map(val2 => val1 + val2))
    case TermNode(DASH, expr1, expr2) => evaluate(expr1).flatMap(val1 => evaluate(expr2).map(val2 => val1 - val2))
    case TermNode(op, _, _) => Left(InvalidOperatorException(s"improper operation ${op.string} where addition or subtraction was expected"))
  }

  /**
   * Validates numerator and denominator for division
   */
  private def validateDivision(numerator: Double, denominator: Double): Either[CalculatorException, (Double, Double)] = {
    if (numerator == 0 && denominator == 0) Left(DivisionByZeroException("indeterminate form 0/0 obtained"))
    else if (denominator == 0) Left(DivisionByZeroException("division by 0"))
    else Right(numerator, denominator)
  }
}