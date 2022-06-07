package it.scalalearn.calculator

/**
 * Valid tokens accepted by the calculator
 */
enum TokenType {
  case NUMBER, LPAREN, RPAREN, DOT, PLUS, DASH, STAR, SLASH, WS, OTHER
}

case class Token(tokenType: TokenType, string: String)

