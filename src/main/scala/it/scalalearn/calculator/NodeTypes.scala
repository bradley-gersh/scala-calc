package it.scalalearn.calculator

/**
 * eBNF
 * expression: term
 * term: factor (+- factor)*
 * factor: sign (/* sign)*
 * sign: number | -number
 * number: NUMBER | (expression)
   */*/

trait ParseNode() {
   def isEmpty = false
   def printMe(): String
}

case class EmptyNode() extends ParseNode {
   override def isEmpty = true
   override def printMe() = ""
}

class FuncNode(func: Token, exprs: ParseNode*) extends ParseNode() {
   override def printMe(): String = s"(${func.string} " +
     exprs.map(expr => expr.printMe()).mkString(" ") + ")"
}

case class ExpressionNode(expr: ParseNode) extends ParseNode() {
   override def printMe(): String = expr.printMe()
}

case class TermNode(op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2)

case class FactorNode(op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2)

case class SignNode(sign: Token, expr: ParseNode) extends FuncNode(sign, expr)

case class NumberNode(value: Double) extends ParseNode() {
   override def printMe(): String = value.toString
}