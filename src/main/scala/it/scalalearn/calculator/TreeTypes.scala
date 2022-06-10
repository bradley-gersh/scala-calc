package it.scalalearn.calculator

import scala.util.{Failure, Success, Try}

sealed trait ParseNode() {
   def isEmpty = false
   override def toString: String
}

case class NumberNode(value: Double) extends ParseNode {
  override def toString: String = value.toString
}

case class EmptyNode() extends ParseNode {
   override def isEmpty = true
   override def toString = "[empty]"
}

// Expressions and values
case class ExpressionNode(expr: ParseNode) extends ParseNode {
   override def toString: String = expr.toString
}

// Functions and operators
sealed trait FuncNode(func: Token, exprs: ParseNode*) extends ParseNode {
  override def toString: String = s"(${func.string} " +
    exprs.map(expr => expr.toString).mkString(" ") + ")"
}

case class TermNode(op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2)

case class FactorNode(op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2)

case class SignNode(sign: Token, expr: ParseNode) extends FuncNode(sign, expr)
