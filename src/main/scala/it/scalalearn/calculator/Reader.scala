package it.scalalearn.calculator

object Reader {
  /**
   * Lexes a line of input for tokens.
   */
  def lexer(input: String): Array[Token] = ???

  /**
   * Parses tokens into class instances and evaluates computation
   */
  def parser(tokens: Array[Token]): Double = ???
}
