package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class ParserTest extends AnyFunSuite {

  // Test proper inputs

  test("Parser should handle empty input") {
    assert(Parser(List[Token]()).map(_.toString) === Success("[empty]"))
  }

  test("Parser should parse non-negative numbers") {
    assert(Parser(List(Token(TokenType.NUMBER, "3"))).map(_.toString) === Success("3.0"))
    assert(Parser(List(Token(TokenType.NUMBER, "30"))).map(_.toString) === Success("30.0"))
    assert(Parser(List(Token(TokenType.NUMBER, "3.0"))).map(_.toString) === Success("3.0"))
    assert(Parser(List(Token(TokenType.NUMBER, "3."))).map(_.toString) === Success("3.0"))
    assert(Parser(List(Token(TokenType.NUMBER, "0.3"))).map(_.toString) === Success("0.3"))
    assert(Parser(List(Token(TokenType.NUMBER, ".3"))).map(_.toString) === Success("0.3"))
    assert(Parser(List(Token(TokenType.NUMBER, "03"))).map(_.toString) === Success("3.0"))
  }

  test("Parser should parse negative numbers (unary -)") {
    assert(Parser(List(Token(TokenType.DASH, "-"), Token(TokenType.NUMBER, "3"))).map(_.toString) === Success("(- 3.0)"))
    assert(Parser(List(Token(TokenType.DASH, "-"), Token(TokenType.NUMBER, ".3"))).map(_.toString) === Success("(- 0.3)"))
  }

  test("Parser should parse two-term addition and subtraction") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "3"))).map(_.toString) === Success("(+ 4.0 3.0)"))

    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.NUMBER, "3"))).map(_.toString) === Success("(- 4.0 3.0)"))
  }

  test("Parser should parse two-factor multiplication and division") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "3"))).map(_.toString) === Success("(* 4.0 3.0)"))

    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "3"))).map(_.toString) === Success("(/ 4.0 3.0)"))
  }

  test("Parser should parse multi-term addition and subtraction") {

    // 4 + 0.5 - 6 - 2 + 4
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.PLUS, "-"),
      Token(TokenType.NUMBER, "6"),
      Token(TokenType.PLUS, "-"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "4"))).map(_.toString) === Success("(+ (- (- (+ 4.0 0.5) 6.0) 2.0) 4.0)"))
  }

  test("Parser should parse multi-factor multiplication and division") {

    // 4 * 0.5 / 6 * 2 / 4
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "6"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "4"))).map(_.toString) === Success("(/ (* (/ (* 4.0 0.5) 6.0) 2.0) 4.0)"))
  }

  test("Parser should handle order of operations in expressions without parentheses") {

    // 4 * 0.5 + 6
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "6"))).map(_.toString) === Success("(+ (* 4.0 0.5) 6.0)"))

    // 4 + 0.5 * 6
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "6"))).map(_.toString) === Success("(+ 4.0 (* 0.5 6.0))"))

    // 4 + 0.5 * 6 - 2 / 4 * 9
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "6"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "9"),
    )).map(_.toString) === Success("(- (+ 4.0 (* 0.5 6.0)) (* (/ 2.0 4.0) 9.0))"))
  }

  test("Parser should prioritize parentheses over other operators") {

    // 4 * (0.5 + 6)
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "6"),
      Token(TokenType.RPAREN, ")"))).map(_.toString) === Success("(* 4.0 (+ 0.5 6.0))"))

    // (4 + 0.5) * 6
    assert(Parser(List(
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "6"))).map(_.toString) === Success("(* (+ 4.0 0.5) 6.0)"))

  }

  test("Parser should correctly handle nested parentheses") {

    // 4 * (0.5 + ((6 - 2 * 2) / ((4 * 4) - 0.5) + 9))
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "6"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "9"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.RPAREN, ")"),
    )).map(_.toString) === Success("(* 4.0 (+ 0.5 (+ (/ (- 6.0 (* 2.0 2.0)) (- (* 4.0 4.0) 0.5)) 9.0)))"))
  }

  test("Parser should fail if it receives adjacent numbers without an operator or parentheses") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "5"),
      Token(TokenType.NUMBER, "1"),
    )).isFailure)
  }

  test("Parser should fail if it receives unmatched closing parentheses") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.RPAREN, ")"),
    )).map(_.toString).isFailure)
  }

  test("Parser should fail if it has leftover unclosed parentheses") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "2"),
    )).map(_.toString).isFailure)
  }

  test("Parser should fail if an infix binary operation is lacking two arguments") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/")
    )).isFailure)

    assert(Parser(List(
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "1")
    )).isFailure)

    assert(Parser(List(
      Token(TokenType.NUMBER, "5"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.RPAREN, ")")
    )).isFailure)
  }
}