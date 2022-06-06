package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite

class ReaderTest extends AnyFunSuite {
  test("Reader.lexer reads in a nonnegative integer string and produces a single Number token") {
    assert(Reader("5") === List(Token(TokenType.NUMBER, "5")))
    assert(Reader("33") === List(Token(TokenType.NUMBER, "33")))
    assert(Reader("91023457689") === List(Token(TokenType.NUMBER, "91023457689")))
  }

  test("Reader.lexer reads in a positive decimal string and produces a Number token") {

  }

  test("Reader.lexer reads in various operators and produces the equivalent tokens") {
    assert(Reader("+-*/") === List(
      Token(TokenType.PLUS, "+"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.SLASH, "/"),
    ))
  }

  test("Reader.lexer reads in a string of mixed characters and produces the appropriate tokens") {

  }

  test("Reader.lexer prints an error message if it receives an unrecognized character.") {

  }
}
