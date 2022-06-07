package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class ParserTest extends AnyFunSuite {
  test("Parser handles empty input") {
    assert(Parser(List[Token]()).printMe() == "")
  }

  test("Parser parses non-negative numbers") {
    assert(Parser(List(Token(TokenType.NUMBER, "3"))).printMe() == "3.0")
    assert(Parser(List(Token(TokenType.NUMBER, "30"))).printMe() == "30.0")
    assert(Parser(List(Token(TokenType.NUMBER, "3.0"))).printMe() == "3.0")
    assert(Parser(List(Token(TokenType.NUMBER, "3."))).printMe() == "3.0")
    assert(Parser(List(Token(TokenType.NUMBER, "0.3"))).printMe() == "0.3")
    assert(Parser(List(Token(TokenType.NUMBER, ".3"))).printMe() == "0.3")
    assert(Parser(List(Token(TokenType.NUMBER, "03"))).printMe() == "3.0")
  }

  test("Parser parses negative numbers (unary -)") {
    assert(Parser(List(Token(TokenType.DASH, "-"), Token(TokenType.NUMBER, "3"))).printMe() == "(- 3.0)")
    assert(Parser(List(Token(TokenType.DASH, "-"), Token(TokenType.NUMBER, ".3"))).printMe() == "(- 0.3)")
  }

  test("Parser parses two-term addition and subtraction") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "3"))).printMe() == "(+ 4.0 3.0)")

    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.NUMBER, "3"))).printMe() == "(- 4.0 3.0)")
  }

  test("Parser parses two-factor multiplication and division") {
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "3"))).printMe() == "(* 4.0 3.0)")

    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "3"))).printMe() == "(/ 4.0 3.0)")
  }

  test("Parser parses multi-term addition and subtraction") {

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
      Token(TokenType.NUMBER, "4"))).printMe() == "(+ (- (- (+ 4.0 0.5) 6.0) 2.0) 4.0)")
  }

  test("Parser parses multi-factor multiplication and division") {

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
      Token(TokenType.NUMBER, "4"))).printMe() == "(/ (* (/ (* 4.0 0.5) 6.0) 2.0) 4.0)")
  }

  test("Parser correctly handles order of operations in expressions without parentheses") {

    // 4 * 0.5 + 6
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "6"))).printMe() == "(+ (* 4.0 0.5) 6.0)")

    // 4 + 0.5 * 6
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "6"))).printMe() == "(+ 4.0 (* 0.5 6.0))")

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
    )).printMe() == "(- (+ 4.0 (* 0.5 6.0)) (* (/ 2.0 4.0) 9.0))")
  }

  test("Parser prioritizes parentheses over other operators") {

    // 4 * (0.5 + 6)
    assert(Parser(List(
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "6"),
      Token(TokenType.RPAREN, ")"))).printMe() == "(* 4.0 (+ 0.5 6.0))")

    // (4 + 0.5) * 6
    assert(Parser(List(
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.NUMBER, "0.5"),
      Token(TokenType.RPAREN, ")"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.NUMBER, "6"))).printMe() == "(* (+ 4.0 0.5) 6.0)")

  }

  test("Parser correctly handles nested parentheses") {

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
    )).printMe() == "(* 4.0 (+ 0.5 (+ (/ (- 6.0 (* 2.0 2.0)) (- (* 4.0 4.0) 0.5)) 9.0)))")
  }

  ignore("Parser throws an error if it detects the `--` sequence") {
    assert(Parser(List(
      Token(TokenType.DASH, "-"),
      Token(TokenType.DASH, "-"),
      Token(TokenType.NUMBER, "4"),
      Token(TokenType.NUMBER, "6"))).printMe() == "(* (+ 4.0 0.5) 6.0)")
  }

  ignore("Parser throws an error if parentheses are empty") {

  }

  ignore("Parser throws an error if parentheses are unmatched (left or right)") {

  }

  ignore("parser throws an error if a binary operator is lacking two arguments") {
  // including in parentheses, like `5 + (3 *)`
  }
}