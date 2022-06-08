package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite
import scala.util.{Failure, Success}

class ReaderTest extends AnyFunSuite {
  test("Reader reads in a nonnegative integer string and produces a single Number token") {
    assert(Reader("5") === Success(List(Token(TokenType.NUMBER, "5"))))
    assert(Reader("33") === Success(List(Token(TokenType.NUMBER, "33"))))
    assert(Reader("91023457689") === Success(List(Token(TokenType.NUMBER, "91023457689"))))
  }

  test("Reader reads in a positive decimal string and produces a Number token") {
    assert(Reader("4.3") === Success(List(Token(TokenType.NUMBER, "4.3"))))
    assert(Reader("4.") === Success(List(Token(TokenType.NUMBER, "4."))))
    assert(Reader(".3") === Success(List(Token(TokenType.NUMBER, ".3"))))
  }

  test("Reader reads in various operators and produces the equivalent tokens") {
    assert(Reader("+-*/") === Success(List(
      Token(TokenType.PLUS, "+"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.SLASH, "/"),
    )))
  }

  test("Reader skips whitespace") {
    assert(Reader("5   5") === Success(List(
      Token(TokenType.NUMBER, "5"),
      Token(TokenType.NUMBER, "5"),
    )))
  }

  test("Reader reads in a string of mixed characters and produces the appropriate tokens") {
    assert(Reader("4.2-+)((  3*  7/.1 9. 00") === Success(List(
      Token(TokenType.NUMBER, "4.2"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "3"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "7"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, ".1"),
      Token(TokenType.NUMBER, "9."),
      Token(TokenType.NUMBER, "00"),
    )))
  }

  test("Reader should throw an exception if it receives an unrecognized character.") {
    assert(Reader("&").isFailure)
  }

  test("Reader should throw an exception if two decimals appear in one number.") {
    assert(Reader("4.6.4").isFailure)
  }

  test("Reader should throw an exception if it receives an isolated decimal") {
    assert(Reader(".").isFailure)
    assert(Reader("4 . 2").isFailure)
  }
}
