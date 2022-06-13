package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Lexer singleton to scan calculator input into tokens.
 */
object Lexer {

  /**
   * Public access to the Lexer object
   *
   * @param  input  string to be lexed
   * @return        Either an error message or the list of tokens found in the input
   */
  def apply(input: String): Either[String, List[Token]] = {
    read(input.toList, List[Token]())
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
   * @return        Either an error message or a list of processed tokens
   */
  @tailrec
  private def read(input: List[Char], tokens: List[Token]): Either[String, List[Token]] = {
    input match {
      // Finished processing
      case Nil => Right(tokens.reverse)

      // Ignore whitespace
      case WS() :: rest => read(rest, tokens)

      // Number literals
      case (first @ (DIGITS() | SEPARATOR)) +: rest =>
        readNumberToken(rest, List(first), first == SEPARATOR) match {
          case Left(error) => Left(error)
          case Right(newRest, numberToken) => read(newRest, numberToken +: tokens)
        }

      // One-character patterns
      case first +: rest => first match {
        case '(' => read(rest, LPAREN +: tokens)
        case ')' => read(rest, RPAREN +: tokens)
        case '+' => read(rest, PLUS +: tokens)
        case '-' => read(rest, DASH +: tokens)
        case '*' => read(rest, STAR +: tokens)
        case '/' => read(rest, SLASH +: tokens)
        case unknown => Left(s"unrecognized character: $unknown")
      }
    }
  }

  /**
   * Processes multiple digits and possibly a decimal point into a single number token.
   *
   * @param  input          list of the characters yet to be processed
   * @param  currToken      list of the characters to be wrapped in the current token
   * @param  fractionalPart currently lexing the fractional part of a decimal number
   * @return                Either an error message or a 2-tuple containing both the remaining
   *                        characters to be processed and the newly generated Number token.
   */
  @tailrec
  private def readNumberToken(input: List[Char], currToken: List[Char], fractionalPart: Boolean): Either[String, (List[Char], Token)] = {
    def flushNumber() = {
      val numberString = currToken.reverse.mkString
      if (numberString == SEPARATOR.toString) Left("isolated . not permitted")
      else Right(input, NUMBER(numberString))
    }

    input match {
      case (first @ DIGITS()) :: rest => readNumberToken(rest, first :: currToken, fractionalPart)
      case (first @ SEPARATOR) +: rest =>
        if (!fractionalPart) readNumberToken(rest, first +: currToken, true)
        else Left(s"only one '$SEPARATOR' character permitted per number")
      case _ => flushNumber()
    }
  }
}
