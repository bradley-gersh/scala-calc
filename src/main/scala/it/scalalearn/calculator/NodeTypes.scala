package it.scalalearn.calculator

trait ParseNode() {
   def isEmpty = false
   override def toString(): String
   def eval(): Double
}

case class EmptyNode() extends ParseNode {
   override def isEmpty = true
   override def toString() = "[empty]"
   override def eval() = throw new NoSuchElementException("cannot evaluate empty node")
}

case class ExpressionNode(expr: ParseNode) extends ParseNode() {
   override def toString(): String = expr.toString()
   override def eval(): Double = expr.eval()
}

class FuncNode(func: Token, exprs: ParseNode*) extends ParseNode() {
   override def toString(): String = s"(${func.string} " +
     exprs.map(expr => expr.toString()).mkString(" ") + ")"

   override def eval() = Double.NaN // reserved for future use, e.g. trigonometric functions
}

case class TermNode(op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2) {
   override def eval() =
      if (op.tokenType == TokenType.PLUS) expr1.eval() + expr2.eval()
      else if (op.tokenType == TokenType.DASH) expr1.eval() - expr2.eval()
      else throw new IllegalArgumentException("improper operation in term node")
}

case class FactorNode(op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2) {
   override def eval() =
      if (op.tokenType == TokenType.STAR) expr1.eval() * expr2.eval()
      else if (op.tokenType == TokenType.SLASH) expr1.eval() / expr2.eval()
      else throw new IllegalArgumentException("improper operation in term node")
}

case class SignNode(sign: Token, expr: ParseNode) extends FuncNode(sign, expr) {
   override def eval() = sign.tokenType match {
      case TokenType.PLUS => expr.eval()
      case TokenType.DASH => -expr.eval()
   }
}

case class NumberNode(value: Double) extends ParseNode() {
   override def toString(): String = value.toString
   override def eval(): Double = value
}