package it.scalalearn.calculator

import scala.util.{Failure, Success, Try}

sealed trait ParseNode {
   def isEmpty = false
}

class FuncNode(val op: Token, val exprs: ParseNode*) extends ParseNode

case object EmptyNode extends ParseNode {
   override def isEmpty = true
}

case class NumberNode(value: Double) extends ParseNode

case class TermNode(override val op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2)

case class FactorNode(override val op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2)

case class SignNode(override val op: Token, expr: ParseNode) extends FuncNode(op, expr)
