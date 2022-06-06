package it.scalalearn.calculator

import scala.collection.mutable.ArrayBuffer

object Reader {
  val digits = """\d+""".r.unanchored

  /**
   * Lexes a line of input for tokens.
   */
  def apply(input: String): Array[Token] = {
    // is there a functional way to do the lexer?
    val tokens = input.foldLeft(ArrayBuffer[Token]())(
      (tokenList, char) => tokenList.append(char match {
        case digits() => Token(TokenType.DIGIT, char.toString)
        case '('=> Token(TokenType.LPAREN, char.toString)
        case ')'=> Token(TokenType.RPAREN, char.toString)
        case '+'=> Token(TokenType.PLUS, char.toString)
        case '-'=> Token(TokenType.DASH, char.toString)
        case '*'=> Token(TokenType.STAR, char.toString)
        case '/'=> Token(TokenType.SLASH, char.toString)
        case '.'=> Token(TokenType.DOT, char.toString)
        case _ => Token(TokenType.OTHER, char.toString)
      })
    )
    tokens.toArray

  }


}
