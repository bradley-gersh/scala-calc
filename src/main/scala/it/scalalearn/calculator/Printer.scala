package it.scalalearn.calculator

import scala.util.Try

/**
 * Flattens the parse tree to a string for printing
 */
object Printer extends Function[ParseNode, Try[String]] {
  /**
   * Public access to Printer function
   *
   * @param  tree  root node of the parse tree
   * @return       string representation of the parse tree
   */
  def apply(tree: ParseNode): Try[String] = Try(printNodes(tree))

  private val printNodes: ParseNode => String = {
    case EmptyNode() => "()"
    case NumberNode(value) => value.toString
    case f: FuncNode => f.exprs.map(expr => printNodes(expr))
                               .mkString(s"(${f.op.string} ", " ", ")")
  }
}
