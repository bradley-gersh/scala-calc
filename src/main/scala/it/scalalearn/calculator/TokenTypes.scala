package it.scalalearn.calculator

/**
 * Valid tokens accepted by the calculator
 */
enum TokenType {
  case DIGIT, LPAREN, RPAREN, DOT, PLUS, DASH, STAR, SLASH, OTHER
}

case class Token(val tokenType: TokenType, val string: String)

