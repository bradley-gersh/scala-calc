package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {
  test("Calculator.repl should close when an empty line is entered") {
  }

  /* The follow are integration tests. For proper unit testing, the individual .eval methods on each
   * NodeType should be verified.
   */

  test("Calculator.evaluate should echo back a single number") {

  }

  test("Calculator.evaluate should evaluate simple sums and differences") {

  }

  test("Calculator.evaluate should evaluate simple products and quotients") {

  }

  test("Calculator.evaluate should handle parentheses") {

  }

  test("Calculator.evaluate should correctly handle order of operations") {

  }

  test("Calculator.evaluate should gracefully recover from division by 0") {

  }

  test("Calculator.evaluate should fail in case of overflow") {
    assert(Calculator.evaluate("9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999",
      false).isFailure)
  }

  test("Calculator.evaluate should fail in case of division by zero") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "0"),
    )).map(_.toString).isFailure)

    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.RPAREN, ")"),
    )).map(_.toString).isFailure)

    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.NUMBER, "1.9999999999999999999999999999"),
      Token(TokenType.RPAREN, ")"),
    )).map(_.toString).isFailure)

    assert(Parser(List(
      Token(TokenType.NUMBER, "0.0"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "0.0"),
    )).map(_.toString).isFailure)
  }

  test("Calculator.evaluate should fail if parentheses are empty") {
    assert(Parser(List(
      Token(TokenType.LPAREN, "("),
      Token(TokenType.RPAREN, ")"),
    )).map(_.toString).isFailure)

    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.RPAREN, ")"),
    )).map(_.toString).isFailure)
  }

  test("Calculator.evaluate should fail if parentheses are unmatched (left or right)") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "2"),
    )).map(_.toString).isFailure)

    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.RPAREN, ")"),
    )).map(_.toString).isFailure)
  }

  test("Calculator.evaluate should fail if a binary operator is lacking two arguments") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
    )).map(_.toString).isFailure)

    assert(Parser(List(
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "1"),
    )).map(_.toString).isFailure)

    assert(Parser(List(
      Token(TokenType.NUMBER, "5"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.RPAREN, ")"),
    )).map(_.toString).isFailure)
  }

  test("Calculator.evaluate should fail if it receives adjacent numbers without an operator or parentheses") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "5"),
      Token(TokenType.NUMBER, "1"),
    )).map(_.toString).isFailure)
  }

}