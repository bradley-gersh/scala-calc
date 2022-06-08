package it.scalalearn.calculator

import scala.util.Try

object Evaluator {
   /**
    * Evaluates a given parse tree for the return value
    *
    * @param  tree  root node of the parse tree
    * @return       Try of the numerical value associated with evaluating the parse tree
    */
   def apply(tree: ParseNode): Try[Double] = Try(tree.eval())
}

trait ParseNode() {
   def isEmpty = false
   override def toString(): String
   def eval(): Double
}

case class EmptyNode() extends ParseNode {
   override def isEmpty = true
   override def toString() = "[empty]"
   override def eval() = throw new NoSuchElementException("cannot evaluate an empty expression")
}

// Expressions and values
case class ExpressionNode(expr: ParseNode) extends ParseNode() {
   override def toString(): String = expr.toString()
   override def eval(): Double = expr.eval()
}

case class NumberNode(value: Double) extends ParseNode() {
   override def toString(): String = value.toString
   override def eval(): Double = value
}

// Functions and operators
trait FuncNode(func: Token, exprs: ParseNode*) extends ParseNode {
   override def toString(): String = s"(${func.string} " +
     exprs.map(expr => expr.toString()).mkString(" ") + ")"
}

case class TermNode(op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2) {
   override def eval() =
      if (op.tokenType == TokenType.PLUS) expr1.eval() + expr2.eval()
      else if (op.tokenType == TokenType.DASH) expr1.eval() - expr2.eval()
      else throw new ParserException(s"improper operation ${op.string} where addition or subtraction was expected")
}

case class FactorNode(op: Token, expr1: ParseNode, expr2: ParseNode) extends FuncNode(op, expr1, expr2) {
   override def eval() =
      if (op.tokenType == TokenType.STAR) expr1.eval() * expr2.eval()
      else if (op.tokenType == TokenType.SLASH) {
         val numerator = expr1.eval()
         val denominator = expr2.eval()

         if (denominator == 0.0) {
            if (numerator == 0.0) throw new ParserException("indeterminate form 0/0 obtained")
            else throw new ParserException("division by zero")
         } else numerator / denominator
      }
      else throw new ParserException(s"improper operation ${op.string} where multiplication or division was expected")
}

case class SignNode(sign: Token, expr: ParseNode) extends FuncNode(sign, expr) {
   override def eval() = sign.tokenType match {
      case TokenType.PLUS => expr.eval()
      case TokenType.DASH => -expr.eval()
      case _  => throw new ParserException(s"invalid unary operator `${sign.string}`")
   }
}