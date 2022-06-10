package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Lexer singleton to scan calculator input into tokens.
 */
object Lexer {

  /**
   * Public access to the Lexer object
   *
   * @param  input  string to be lexed
   * @return        Try object wrapping the list of tokens found in the input
   */
  def apply(input: String): Try[List[Token]] = {
    Try(read(input.toList, List[Token]()))
  }

  private final val SEPARATOR = '.' // May be ',' depending on locale
  private def isDigit(c: Char): Boolean = (c >= '0') && (c <= '9')
  private def isWS(c: Char): Boolean = (c == ' ') || (c == '\t')

  /**
   * Processes a line of input for tokens.
   *
   * @param  input  list of the characters yet to be processed
   * @param  tokens list of the tokens processed so far
   * @return       a list of processed tokens
   */
  @tailrec
  private def read(input: List[Char], tokens: List[Token]): List[Token] = {
    if (input.isEmpty) tokens.reverse // Finished processing
    else {
      val c = input.head
      if (isDigit(c) || c == SEPARATOR) { // Numeric tokens
        val (newTail, numberToken) = readNumberToken(input.tail, List(c), c == SEPARATOR)
        read(newTail, numberToken +: tokens)
      } else if (isWS(c)) { // Whitespace
        read(input.tail, tokens)
      } else { // One-character tokens
        val newToken: Token = c match {
          case '(' => LPAREN
          case ')' => RPAREN
          case '+' => PLUS
          case '-' => DASH
          case '*' => STAR
          case '/' => SLASH
          case _ => throw new UnknownTokenException(c.toString)
        }
        read(input.tail, newToken +: tokens)
      }
    }
  }

  /**
   * Processes multiple digits and possibly a decimal point into a single number token.
   *
   * @param  input          list of the characters yet to be processed
   * @param  currToken      list of the characters to be wrapped in the current token
   * @param  fractionalPart currently lexing the fractional part of a decimal number
   * @return                a 2-tuple containing both the remaining characters to be processed and the
   *                        newly generated Number token.
   */
  @tailrec
  private def readNumberToken(input: List[Char], currToken: List[Char], fractionalPart: Boolean): (List[Char], Token) = {
    def flushNumber() = {
      val numberString = currToken.reverse.mkString
      if (numberString == SEPARATOR.toString) throw new LexerException("isolated . not permitted")
      else (input, NUMBER(numberString))
    }

    if (input.isEmpty) flushNumber()
    else {
      val c = input.head
      if (isDigit(c)) readNumberToken(input.tail, c +: currToken, fractionalPart)
      else if (c == SEPARATOR) {
        if (!fractionalPart) readNumberToken(input.tail, c +: currToken, true)
        else throw new LexerException(s"only one '$SEPARATOR' character permitted per number")
      } else flushNumber()
    }
  }
}
