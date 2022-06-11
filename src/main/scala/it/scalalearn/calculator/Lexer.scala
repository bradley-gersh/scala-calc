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

  /**
   * Fragments (useful patterns that are not themselves tokens)
   */
  private final val SEPARATOR = '.' // May be ',' depending on locale
  private final val DIGITS = """\d+""".r.unanchored
  private final val WS = """\s+""".r.unanchored

  /**
   * Processes a line of input for tokens.
   *
   * @param  input  list of the characters yet to be processed
   * @param  tokens list of the tokens processed so far
   * @return        a list of processed tokens
   */
  @tailrec
  private def read(input: List[Char], tokens: List[Token]): List[Token] = {
    input match {
      // Finished processing
      case Nil => tokens.reverse

      // Ignore whitespace
      case WS() :: rest => read(rest, tokens)

      // Number literals
      case (first @ (DIGITS() | SEPARATOR)) +: rest =>
        val (newRest, numberToken) = readNumberToken(rest, List(first), first == SEPARATOR)
        read(newRest, numberToken +: tokens)

      // One-character patterns
      case first +: rest =>
        val newToken = first match {
          case '(' => LPAREN
          case ')' => RPAREN
          case '+' => PLUS
          case '-' => DASH
          case '*' => STAR
          case '/' => SLASH
          case _ => throw new UnknownTokenException(first.toString)
        }
        read(rest, newToken +: tokens)
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

    input match {
      case (first @ DIGITS()) :: rest => readNumberToken(rest, first :: currToken, fractionalPart)
      case (first @ SEPARATOR) +: rest =>
        if (!fractionalPart) readNumberToken(rest, first +: currToken, true)
        else throw new LexerException(s"only one '$SEPARATOR' character permitted per number")
      case _ => flushNumber()
    }
  }
}
