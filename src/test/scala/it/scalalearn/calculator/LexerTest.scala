package it.scalalearn.calculator

import scala.util.{Failure, Success}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues.convertTryToSuccessOrFailure

class LexerTest extends AnyFunSuite {
  test("Lexer reads in a nonnegative integer string and produces a single Number token") {
    assert(Lexer.read("5") === Success(List(NUMBER("5"))))
    assert(Lexer.read("33") === Success(List(NUMBER("33"))))
    assert(Lexer.read("91023457689") === Success(List(NUMBER("91023457689"))))
  }

  test("Lexer reads in a positive decimal string and produces a Number token") {
    assert(Lexer.read("4.3") === Success(List(NUMBER("4.3"))))
    assert(Lexer.read("4.") === Success(List(NUMBER("4."))))
    assert(Lexer.read(".3") === Success(List(NUMBER(".3"))))
  }

  test("Lexer reads in various operators and produces the equivalent tokens") {
    assert(Lexer.read("+-*/") === Success(List(PLUS, DASH, STAR, SLASH)))
  }

  test("Lexer skips whitespace") {
    assert(Lexer.read("5   5") === Success(List(NUMBER("5"), NUMBER("5"))))
  }

  test("Lexer reads in a string of mixed characters and produces the appropriate tokens") {
    assert(Lexer.read("4.2-+)((  3*  7/.1 9. 00") === Success(List(
      NUMBER("4.2"),
      DASH,
      PLUS,
      RPAREN,
      LPAREN,
      LPAREN,
      NUMBER("3"),
      STAR,
      NUMBER("7"),
      SLASH,
      NUMBER(".1"),
      NUMBER("9."),
      NUMBER("00"),
    )))
  }

  test("Lexer should fail if it receives an unrecognized character.") {
    assert(convertTryToSuccessOrFailure(Lexer.read("&")).failure.exception.getMessage contains "unrecognized character")
  }

  test("Lexer should fail if two decimals appear in one number.") {
    assert(convertTryToSuccessOrFailure(Lexer.read("4.6.2")).failure.exception.getMessage contains "only one")
  }

  test("Lexer should fail if it receives an isolated decimal") {
    assert(convertTryToSuccessOrFailure(Lexer.read(".")).failure.exception.getMessage contains "isolated")
    assert(convertTryToSuccessOrFailure(Lexer.read("4 . 2")).failure.exception.getMessage contains "isolated")
  }
}
