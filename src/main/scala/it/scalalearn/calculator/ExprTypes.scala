package it.scalalearn.calculator

enum Token {
  case DIGIT, LPAREN, RPAREN, DOT, PLUS, DASH, STAR, SLASH
}

class Number(val value: Double)

class Expression(expr: Expression) {
  def apply(expr: Expression): Double = ???
}

class Product(expr1: Expression, op: Token, expr2: Expression) {
}

class Sum(expr1: Expression, op: Token, expr2: Expression) {
}

