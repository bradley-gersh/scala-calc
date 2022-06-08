package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Reader object for lexing calculator input into tokens.
 */
object Reader {
  /**
   * Public access to Reader object
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
        val (newTail, numberToken) = readNumberToken(input.tail, List(c))
        read(newTail, numberToken +: tokens)
      } else if (isWS(c)) { // Whitespace
        read(input.tail, tokens)
      } else { // One-character tokens
        val newToken = c match {
          case '(' => Token(TokenType.LPAREN, c.toString)
          case ')' => Token(TokenType.RPAREN, c.toString)
          case '+' => Token(TokenType.PLUS, c.toString)
          case '-' => Token(TokenType.DASH, c.toString)
          case '*' => Token(TokenType.STAR, c.toString)
          case '/' => Token(TokenType.SLASH, c.toString)
          case _ => throw new UnknownTokenException(c.toString)
        }
        read(input.tail, newToken +: tokens)
      }
    }
  }

  /**
   * Processes multiple digits and possibly a decimal point into a single number token.
   *
   * @param  input     list of the characters yet to be processed
   * @param  currToken list of the characters to be wrapped in the current token
   * @return          a 2-tuple containing both the remaining characters to be processed and the
   *                  newly generated Number token.
   */
  @tailrec
  private def readNumberToken(input: List[Char], currToken: List[Char], integerPart: Boolean = true): (List[Char], Token) = {
    if (input.isEmpty) {
      (input, Token(TokenType.NUMBER, currToken.reverse.mkString))
    } else {
      val c = input.head
      if (isDigit(c)) readNumberToken(input.tail, c +: currToken, integerPart)
      else if (c == SEPARATOR) {
        if (integerPart) readNumberToken(input.tail, c +: currToken, false)
        else throw new LexerException(s"Only one '$SEPARATOR' character permitted per number")
      }
      else (input, Token(TokenType.NUMBER, currToken.reverse.mkString))
    }
  }
}

class LexerException(private val message: String) extends RuntimeException(message)

class UnknownTokenException(private val message: String) extends LexerException(message) {
  override def getMessage: String = s"Unrecognized character: $message"
}
