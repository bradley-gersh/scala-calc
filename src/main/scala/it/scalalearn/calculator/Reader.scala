package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object Reader {
  enum ReadState { case DEFAULT, IN_NUMBER }

  def isDigit(c: Char): Boolean = (c >= '0') && (c <= '9')
  def isWS(c: Char): Boolean = (c == ' ') || (c == '\t')

  /**
   * Process a line of input for tokens.
   * @param input The remaining characters to be processed.
   * @param tokens The tokens processed so far.
   * @return A list of processed tokens.
   */
  @tailrec
  def read(input: List[Char], tokens: List[Token]): List[Token] = {
    if (input.isEmpty) tokens.reverse // Finished processing
    else {
      val c = input.head
      if (isDigit(c) || c == '.') { // Numeric tokens
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
   * Process a number token.
   * @param input The remaining characters to be processed.
   * @param currToken A list of the characters to be wrapped in the current token.
   * @return A 2-tuple containing the remaining characters to be processed and the
   *         newly generated Number token.
   */
  @tailrec
  def readNumberToken(input: List[Char], currToken: List[Char], integerPart: Boolean = true): (List[Char], Token) = {
    if (input.isEmpty) {
      (input, Token(TokenType.NUMBER, currToken.reverse.mkString))
    } else {
      val c = input.head
      if (isDigit(c)) readNumberToken(input.tail, c +: currToken, integerPart)
      else if (c == '.') {
        if (integerPart) readNumberToken(input.tail, c +: currToken, false)
        else throw new LexerException("Only one '.' character permitted per number")
      }
      else (input, Token(TokenType.NUMBER, currToken.reverse.mkString))
    }
  }

  def apply(input: String): Try[List[Token]] = {
    Try(read(input.toList, List[Token]()))
  }
}

class LexerException(private val message: String) extends Exception(message) {
  override def getMessage: String = s"[error] $message"
}

class UnknownTokenException(private val message: String) extends LexerException(message) {
  override def getMessage: String = s"[error] Unrecognized character: $message"
}
