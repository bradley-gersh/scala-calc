package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite

class ReaderTest extends AnyFunSuite {
  test("Reader.lexer reads in a nonnegative integer string and produces a Number token") {
    assert(Reader("5") === Array(Token(TokenType.DIGIT, "5")))
    assert(Reader("33") === Array(Token(TokenType.DIGIT, "3"), Token(TokenType.DIGIT, "3")))
    assert(Reader("91023457689") === Array(
      Token(TokenType.DIGIT, "9"),
      Token(TokenType.DIGIT, "1"),
      Token(TokenType.DIGIT, "0"),
      Token(TokenType.DIGIT, "2"),
      Token(TokenType.DIGIT, "3"),
      Token(TokenType.DIGIT, "4"),
      Token(TokenType.DIGIT, "5"),
      Token(TokenType.DIGIT, "7"),
      Token(TokenType.DIGIT, "6"),
      Token(TokenType.DIGIT, "8"),
      Token(TokenType.DIGIT, "9"),
    ))
  }

  test("Reader.lexer reads in a positive decimal string and produces a Number token") {

  }

  test("Reader.lexer reads in various operators and produces the equivalent tokens") {

  }

  test("Reader.lexer reads in a string of mixed characters and produces the appropriate tokens") {

  }
}
