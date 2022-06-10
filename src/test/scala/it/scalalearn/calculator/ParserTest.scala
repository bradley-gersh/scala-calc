package it.scalalearn.calculator

import scala.util.{Failure, Success}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues.convertTryToSuccessOrFailure

// NOTE: For brevity, I have used the Printer function to verify that the
// proper trees were produced. For better isolation, need to create separate trees.
class ParserTest extends AnyFunSuite {

  // Test proper inputs

  test("Parser should handle empty input") {
    assert(Parser(List[Token]()) === Success(EmptyNode()))
  }

  test("Parser should parse non-negative numbers") {
    assert(Parser(List(Token(TokenType.NUMBER, "3"))) === Success(NumberNode(3)))
    assert(Parser(List(Token(TokenType.NUMBER, "30"))) === Success(NumberNode(30)))
    assert(Parser(List(Token(TokenType.NUMBER, "3.0"))) === Success(NumberNode(3.0)))
    assert(Parser(List(Token(TokenType.NUMBER, "3."))) === Success(NumberNode(3.0)))
    assert(Parser(List(Token(TokenType.NUMBER, "0.3"))) === Success(NumberNode(0.3)))
    assert(Parser(List(Token(TokenType.NUMBER, ".3"))) === Success(NumberNode(0.3)))
    assert(Parser(List(Token(TokenType.NUMBER, "03"))) === Success(NumberNode(3)))
  }

  test("Parser should parse negative numbers (unary -)") {
    assert(Parser(List(Token(TokenType.DASH, "-"), Token(TokenType.NUMBER, "3")))
      === Success(SignNode(Token(TokenType.DASH, "-"), NumberNode(3))))
    assert(Parser(List(Token(TokenType.DASH, "-"), Token(TokenType.NUMBER, ".3")))
      === Success(SignNode(Token(TokenType.DASH, "-"), NumberNode(0.3))))
  }

  test("Parser should parse two-term addition and subtraction") {
    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.NUMBER, "3"))) ===
      Success(TermNode(
        Token(TokenType.PLUS, "+"),
        NumberNode(4),
        NumberNode(3))))

    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.DASH, "-"),
        Token(TokenType.NUMBER, "3"))) ===
      Success(TermNode(
        Token(TokenType.DASH, "-"),
        NumberNode(4),
        NumberNode(3))))
  }

  test("Parser should parse two-factor multiplication and division") {
    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.STAR, "*"),
        Token(TokenType.NUMBER, "3"))) ===
      Success(FactorNode(
        Token(TokenType.STAR, "*"),
        NumberNode(4),
        NumberNode(3))))

    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.SLASH, "/"),
        Token(TokenType.NUMBER, "3"))) ===
      Success(FactorNode(
        Token(TokenType.SLASH, "/"),
        NumberNode(4),
        NumberNode(3))))
  }

  test("Parser should parse multi-term addition and subtraction") {

    // 4 + 0.5 - 6 - 2 + 4
    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.NUMBER, "0.5"),
        Token(TokenType.DASH, "-"),
        Token(TokenType.NUMBER, "6"),
        Token(TokenType.DASH, "-"),
        Token(TokenType.NUMBER, "2"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.NUMBER, "4"))) ===
      Success(
        TermNode(
          Token(TokenType.PLUS, "+"),
          TermNode(
            Token(TokenType.DASH, "-"),
            TermNode(
              Token(TokenType.DASH, "-"),
              TermNode(
                Token(TokenType.PLUS, "+"),
                NumberNode(4),
                NumberNode(0.5)),
              NumberNode(6)),
            NumberNode(2)),
          NumberNode(4))))
  }

  test("Parser should parse multi-factor multiplication and division") {

    // 4 * 0.5 / 6 * 2 / 4
    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.STAR, "*"),
        Token(TokenType.NUMBER, "0.5"),
        Token(TokenType.SLASH, "/"),
        Token(TokenType.NUMBER, "6"),
        Token(TokenType.STAR, "*"),
        Token(TokenType.NUMBER, "2"),
        Token(TokenType.SLASH, "/"),
        Token(TokenType.NUMBER, "4"))) ===
      Success(
        FactorNode(
          Token(TokenType.SLASH, "/"),
          FactorNode(
            Token(TokenType.STAR, "*"),
            FactorNode(
              Token(TokenType.SLASH, "/"),
              FactorNode(
                Token(TokenType.STAR, "*"),
                NumberNode(4),
                NumberNode(0.5)),
              NumberNode(6)),
            NumberNode(2)),
          NumberNode(4))))
  }

  test("Parser should handle order of operations in expressions without parentheses") {

    // 4 * 0.5 + 6
    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.STAR, "*"),
        Token(TokenType.NUMBER, "0.5"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.NUMBER, "6"))) ===
      Success(TermNode(
        Token(TokenType.PLUS, "+"),
        FactorNode(
          Token(TokenType.STAR, "*"),
          NumberNode(4),
          NumberNode(0.5)),
        NumberNode(6))))

    // 4 + 0.5 * 6
    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.NUMBER, "0.5"),
        Token(TokenType.STAR, "*"),
        Token(TokenType.NUMBER, "6"))) ===
      Success(TermNode(
        Token(TokenType.PLUS, "+"),
        NumberNode(4),
        FactorNode(
          Token(TokenType.STAR, "*"),
          NumberNode(0.5),
          NumberNode(6)))))

    // 4 + 0.5 * 6 - 2 / 4 * 9
    assert(
      Parser(List(
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
        Token(TokenType.NUMBER, "9"))) ===
      Success(TermNode(
        Token(TokenType.DASH, "-"),
        TermNode(
          Token(TokenType.PLUS, "+"),
          NumberNode(4.0),
          FactorNode(
            Token(TokenType.STAR, "*"),
            NumberNode(0.5),
            NumberNode(6))),
        FactorNode(
          Token(TokenType.STAR, "*"),
          FactorNode(
            Token(TokenType.SLASH, "/"),
            NumberNode(2.0),
            NumberNode(4.0)),
          NumberNode(9)))))
  }

  test("Parser should prioritize parentheses over other operators") {

    // 4 * (0.5 + 6)
    assert(
      Parser(List(
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.STAR, "*"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.NUMBER, "0.5"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.NUMBER, "6"),
        Token(TokenType.RPAREN, ")"))) ===
      Success(FactorNode(
        Token(TokenType.STAR, "*"),
        NumberNode(4),
        TermNode(
          Token(TokenType.PLUS, "+"),
          NumberNode(0.5),
          NumberNode(6)))))

    // (4 + 0.5) * 6
    assert(
      Parser(List(
        Token(TokenType.LPAREN, "("),
        Token(TokenType.NUMBER, "4"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.NUMBER, "0.5"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.STAR, "*"),
        Token(TokenType.NUMBER, "6"))) ===
      Success(FactorNode(
        Token(TokenType.STAR, "*"),
        TermNode(
          Token(TokenType.PLUS, "+"),
          NumberNode(4),
          NumberNode(0.5)),
        NumberNode(6))))

  }

  test("Parser should correctly handle nested parentheses") {

    // (2 + (3 * (2 - 5) + (2)) / -7) + (5 - 3)
    assert(
      Parser(List(
        Token(TokenType.LPAREN, "("),
        Token(TokenType.NUMBER, "2"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.NUMBER, "3"),
        Token(TokenType.STAR, "*"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.NUMBER, "2"),
        Token(TokenType.DASH, "-"),
        Token(TokenType.NUMBER, "5"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.NUMBER, "2"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.SLASH, "/"),
        Token(TokenType.DASH, "-"),
        Token(TokenType.NUMBER, "7"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.NUMBER, "5"),
        Token(TokenType.DASH, "-"),
        Token(TokenType.NUMBER, "3"),
        Token(TokenType.RPAREN, ")"))) ===
      Success(TermNode(
        Token(TokenType.PLUS, "+"),
        TermNode(
          Token(TokenType.PLUS, "+"),
          NumberNode(2),
          FactorNode(
            Token(TokenType.SLASH, "/"),
            TermNode(
              Token(TokenType.PLUS, "+"),
              FactorNode(
                Token(TokenType.STAR, "*"),
                NumberNode(3),
                TermNode(
                  Token(TokenType.DASH, "-"),
                  NumberNode(2),
                  NumberNode(5))),
              NumberNode(2)),
            SignNode(
              Token(TokenType.DASH, "-"),
              NumberNode(7)))),
        TermNode(
          Token(TokenType.DASH, "-"),
          NumberNode(5),
          NumberNode(3)))))

    // 4 * (0.5 + ((6 - 2 * 2) / ((4 * 4) - 0.5) + 9))
    assert(
      Parser(List(
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
        Token(TokenType.RPAREN, ")"))) ===
      Success(FactorNode(
        Token(TokenType.STAR, "*"),
        NumberNode(4),
        TermNode(
          Token(TokenType.PLUS, "+"),
          NumberNode(0.5),
          TermNode(
            Token(TokenType.PLUS, "+"),
            FactorNode(
              Token(TokenType.SLASH, "/"),
              TermNode(
                Token(TokenType.DASH, "-"),
                NumberNode(6),
                FactorNode(
                  Token(TokenType.STAR, "*"),
                  NumberNode(2),
                  NumberNode(2))),
              TermNode(
                Token(TokenType.DASH, "-"),
                FactorNode(
                  Token(TokenType.STAR, "*"),
                  NumberNode(4),
                  NumberNode(4)),
                NumberNode(0.5))),
            NumberNode(9))))))
  }

  test("Parser should fail if it receives adjacent numbers without an operator or parentheses") {
    assert(convertTryToSuccessOrFailure(Parser(List(
      Token(TokenType.NUMBER, "5"),
      Token(TokenType.NUMBER, "1")))
    ).failure.exception.getMessage contains "unparsed tokens")
  }

  test("Parser should fail if it receives unmatched closing parentheses") {
    assert(convertTryToSuccessOrFailure(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.RPAREN, ")")))
    ).failure.exception.getMessage contains "unmatched `)`")
  }

  test("Parser should fail if it has leftover unclosed parentheses") {
    assert(convertTryToSuccessOrFailure(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "2")))
    ).failure.exception.getMessage contains "unmatched `(`")
  }

  test("Parser should fail if an infix binary operation is lacking two arguments") {
    assert(convertTryToSuccessOrFailure(Parser(List(
      Token(TokenType.NUMBER, "1"),
      Token(TokenType.SLASH, "/")))
    ).failure.exception.getMessage contains "a value was expected")

    assert(convertTryToSuccessOrFailure(Parser(List(
      Token(TokenType.SLASH, "/"),
      Token(TokenType.NUMBER, "1")))
    ).failure.exception.getMessage contains "a value was expected")

    assert(convertTryToSuccessOrFailure(Parser(List(
      Token(TokenType.NUMBER, "5"),
      Token(TokenType.PLUS, "+"),
      Token(TokenType.LPAREN, "("),
      Token(TokenType.NUMBER, "2"),
      Token(TokenType.STAR, "*"),
      Token(TokenType.RPAREN, ")")))
    ).failure.exception.getMessage contains "a value was expected")
  }
}