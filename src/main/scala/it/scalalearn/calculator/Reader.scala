package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Reader {
  enum ReadState { case DEFAULT, IN_NUMBER }

  def isDigit(c: Char): Boolean = (c >= '0') && (c <= '9')
  def isWS(c: Char): Boolean = (c == ' ') || (c == '\t')

  /**
   * Process a line of input for tokens.
   */
  @tailrec
  def read(input: List[Char], currState: ReadState, currToken: List[Char], tokens: List[Token]): List[Token] = {
    if (currState == ReadState.IN_NUMBER) {
      if (input.isEmpty) {
        (Token(TokenType.NUMBER, currToken.reverse.mkString) +: tokens).reverse
      } else {
        val c = input.head
        if (isDigit(c)) read(input.tail, ReadState.IN_NUMBER, c +: currToken, tokens)
        else read(input, ReadState.DEFAULT, List[Char](), Token(TokenType.NUMBER, currToken.reverse.mkString) +: tokens)
      }
    } else { // ReadState.DEFAULT
      if (input.isEmpty) {
        tokens.reverse
      } else {
        val c = input.head
        if (isDigit(c)) {
          read(input.tail, ReadState.IN_NUMBER, List(c), tokens)
        } else if (isWS(c)) {
          read(input.tail, currState, currToken, tokens)
        } else {
          val newToken = c match {
            case '(' => Token(TokenType.LPAREN, c.toString)
            case ')' => Token(TokenType.RPAREN, c.toString)
            case '+' => Token(TokenType.PLUS, c.toString)
            case '-' => Token(TokenType.DASH, c.toString)
            case '*' => Token(TokenType.STAR, c.toString)
            case '/' => Token(TokenType.SLASH, c.toString)
            case '.' => Token(TokenType.DOT, c.toString)
            case _ => throw new BadTokenException(c.toString)
          }
          read(input.tail, currState, List[Char](), newToken +: tokens)
        }
      }
    }
  }

  def apply(input: String): Array[Token] = {
    val tokens = try {
      read(input.toList, ReadState.DEFAULT, List[Char](), List[Token]())
    } catch {
      case e: BadTokenException =>
        println(e.getMessage)
        List[Token]()
    }
    tokens.toArray
  }
}

class BadTokenException(private val message: String) extends Exception(message) {
  override def getMessage: String = s"Unrecognized character: $message"
}
