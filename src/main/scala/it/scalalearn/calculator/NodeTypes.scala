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
   def printMe(): String
}

class Func(func: Token, exprs: ParseNode*) extends ParseNode() {
   override def printMe(): String = s"(${func.string} " +
     exprs.map(expr => expr.printMe()).mkString(" ") + ")"
}

case class Expression(expr: ParseNode) extends ParseNode() {
   override def printMe(): String = expr.printMe()
}

case class Term(op: Token, expr1: ParseNode, expr2: ParseNode) extends Func(op, expr1, expr2)

case class Factor(op: Token, expr1: ParseNode, expr2: ParseNode) extends Func(op, expr1, expr2)

case class Sign(sign: Token, expr: ParseNode) extends Func(sign, expr)

case class Number(value: Double) extends ParseNode() {
   override def printMe(): String = value.toString
}