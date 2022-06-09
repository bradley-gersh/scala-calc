package it.scalalearn.calculator

import scala.util.Try

object Evaluator {
  /**
   * Evaluates a given parse tree for the return value
   *
   * @param  tree  root node of the parse tree
   * @return       Try of the numerical value associated with evaluating the parse tree
   */
  def apply(tree: ParseNode): Try[Double] = {
    val value = Try(tree.eval())
    if (value.isSuccess) {
      if (value.get.isInfinite) Try(throw new EvaluatorException("infinite value obtained"))
      else if (value.get == -0.0) Try(0.0) // the JVM allows -0.0, but we will not
      else value
    } else value
  }
}

class EvaluatorException(private val message: String) extends CalculatorException(message)
