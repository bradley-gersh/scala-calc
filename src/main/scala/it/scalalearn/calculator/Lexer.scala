package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Lexer singleton to scan calculator input into tokens.
 */
object Lexer {

  /**
   * Fragments (useful patterns that are not themselves tokens)
   */
  private final val SEPARATOR = '.' // May be ',' depending on locale
  private final val DIGITS = """\d+""".r.unanchored
  private final val WS = """\s+""".r.unanchored

  /**
   * Processes a line of input for tokens.
   *
   * @param  input  string to be lexed
   * @return        Either an error or the list of tokens found in the input
   */
  def read(input: String): Either[CalculatorException, List[Token]] = read(input.toList, List[Token]())

  /**
   * Processes a line of input for tokens.
   *
   * @param  input  list of the characters yet to be processed
   * @param  tokens list of the tokens processed so far
   * @return        Either an error or a list of processed tokens
   */
  @tailrec
  private def read(input: List[Char], tokens: List[Token]): Either[CalculatorException, List[Token]] = {
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
      case first +: rest => readSingleCharToken(first) match {
        case UNKNOWN(unknownToken) => Left(UnknownTokenException(s"unrecognized character $unknownToken"))
        case newToken => read(rest, newToken +: tokens)
      }

      // Error
      case unknown => Left(UnknownTokenException(s"unable to read this string: $unknown"))
    }
  }

  /**
   * Produce a single-character token
   *
   * @param char  a (single) character to be interpreted as a token
   * @return      a Token object matched to the input
   */
  private def readSingleCharToken(char: Char): Token = char match {
    case '(' => LPAREN
    case ')' => RPAREN
    case '+' => PLUS
    case '-' => DASH
    case '*' => STAR
    case '/' => SLASH
    case _ => UNKNOWN(char.toString)
  }

  /**
   * Processes multiple digits and possibly a decimal point into a single number token.
   *
   * @param  input          list of the characters yet to be processed
   * @param  currToken      list of the characters to be wrapped in the current token
   * @param  fractionalPart currently lexing the fractional part of a decimal number
   * @return                Either an error or a 2-tuple containing both the remaining
   *                        characters to be processed and the newly generated Number token.
   */
  @tailrec
  private def readNumberToken(input: List[Char], currToken: List[Char], fractionalPart: Boolean): Either[CalculatorException, (List[Char], Token)] = {
    def flushNumber() = {
      val numberString = currToken.reverse.mkString
      if (numberString == SEPARATOR.toString) Left(NumberFormattingException("isolated . not permitted"))
      else Right(input, NUMBER(numberString))
    }

    input match {
      case (first @ DIGITS()) :: rest => readNumberToken(rest, first :: currToken, fractionalPart)
      case (first @ SEPARATOR) +: rest =>
        if (!fractionalPart) readNumberToken(rest, first +: currToken, true)
        else Left(NumberFormattingException(s"only one '$SEPARATOR' character permitted per number"))
      case _ => flushNumber()
    }
  }
}
