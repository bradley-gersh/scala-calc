package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite

class ReaderTest extends AnyFunSuite {
  test("Reader.lexer reads in a nonnegative integer string and produces a Number token") {
    assert(Reader("5") === Array(Token(TokenType.NUMBER, "5")))
    assert(Reader("33") === Array(Token(TokenType.NUMBER, "33")))
    assert(Reader("91023457689") === Array(Token(TokenType.NUMBER, "91023457689")))
  }

  test("Reader.lexer reads in a positive decimal string and produces a Number token") {

  }

  test("Reader.lexer reads in various operators and produces the equivalent tokens") {

  }

  test("Reader.lexer reads in a string of mixed characters and produces the appropriate tokens") {

  }
}
