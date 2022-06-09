package it.scalalearn.calculator

import scala.util.{Failure, Success}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues.convertTryToSuccessOrFailure

class LexerTest extends AnyFunSuite {
  test("Lexer reads in a nonnegative integer string and produces a single Number token") {
    assert(Lexer("5") === Success(List(Token(TokenType.NUMBER, "5"))))
    assert(Lexer("33") === Success(List(Token(TokenType.NUMBER, "33"))))
    assert(Lexer("91023457689") === Success(List(Token(TokenType.NUMBER, "91023457689"))))
  }

  test("Lexer reads in a positive decimal string and produces a Number token") {
    assert(Lexer("4.3") === Success(List(Token(TokenType.NUMBER, "4.3"))))
    assert(Lexer("4.") === Success(List(Token(TokenType.NUMBER, "4."))))
    assert(Lexer(".3") === Success(List(Token(TokenType.NUMBER, ".3"))))
  }

  test("Lexer reads in various operators and produces the equivalent tokens") {
    assert(Lexer("+-*/") === Success(List(
      Token(TokenType.PLUS, "+"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.SLASH, "/"),
    )))
  }

  test("Lexer skips whitespace") {
    assert(Lexer("5   5") === Success(List(
      Token(TokenType.NUMBER, "5"),
      Token(TokenType.NUMBER, "5"),
    )))
  }

  test("Lexer reads in a string of mixed characters and produces the appropriate tokens") {
    assert(Lexer("4.2-+)((  3*  7/.1 9. 00") === Success(List(
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

  test("Lexer should fail if it receives an unrecognized character.") {
    assert(convertTryToSuccessOrFailure(Lexer("&")).failure.exception.getMessage contains "unrecognized character")
  }

  test("Lexer should fail if two decimals appear in one number.") {
    assert(convertTryToSuccessOrFailure(Lexer("4.6.2")).failure.exception.getMessage contains "only one")
  }

  test("Lexer should fail if it receives an isolated decimal") {
    assert(convertTryToSuccessOrFailure(Lexer(".")).failure.exception.getMessage contains "isolated")
    assert(convertTryToSuccessOrFailure(Lexer("4 . 2")).failure.exception.getMessage contains "isolated")
  }
}
