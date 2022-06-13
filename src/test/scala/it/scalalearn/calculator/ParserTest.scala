package it.scalalearn.calculator

import scala.util.{Failure, Success}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues.convertTryToSuccessOrFailure

// NOTE: For brevity, I have used the Printer function to verify that the
// proper trees were produced. For better isolation, need to create separate trees.
class ParserTest extends AnyFunSuite {

  // Test proper inputs

  test("Parser should handle empty input") {
    assert(Parser(List[Token]()) === Success(EmptyNode))
  }

  test("Parser should parse non-negative numbers") {
    assert(Parser(List(NUMBER("3"))) === Success(NumberNode(3)))
    assert(Parser(List(NUMBER("30"))) === Success(NumberNode(30)))
    assert(Parser(List(NUMBER("3.0"))) === Success(NumberNode(3.0)))
    assert(Parser(List(NUMBER("3."))) === Success(NumberNode(3.0)))
    assert(Parser(List(NUMBER("0.3"))) === Success(NumberNode(0.3)))
    assert(Parser(List(NUMBER(".3"))) === Success(NumberNode(0.3)))
    assert(Parser(List(NUMBER("03"))) === Success(NumberNode(3)))
  }

  test("Parser should parse negative numbers (unary -)") {
    assert(Parser(List(DASH, NUMBER("3")))
      === Success(SignNode(DASH, NumberNode(3))))
    assert(Parser(List(DASH, NUMBER(".3")))
      === Success(SignNode(DASH, NumberNode(0.3))))
  }

  test("Parser should parse two-term addition and subtraction") {
    assert(
      Parser(List(
        NUMBER("4"),
        PLUS,
        NUMBER("3"))) ===
      Success(TermNode(
        PLUS,
        NumberNode(4),
        NumberNode(3))))

    assert(
      Parser(List(
        NUMBER("4"),
        DASH,
        NUMBER("3"))) ===
      Success(TermNode(
        DASH,
        NumberNode(4),
        NumberNode(3))))
  }

  test("Parser should parse two-factor multiplication and division") {
    assert(
      Parser(List(
        NUMBER("4"),
        STAR,
        NUMBER("3"))) ===
      Success(FactorNode(
        STAR,
        NumberNode(4),
        NumberNode(3))))

    assert(
      Parser(List(
        NUMBER("4"),
        SLASH,
        NUMBER("3"))) ===
      Success(FactorNode(
        SLASH,
        NumberNode(4),
        NumberNode(3))))
  }

  test("Parser should parse multi-term addition and subtraction") {

    // 4 + 0.5 - 6 - 2 + 4
    assert(
      Parser(List(
        NUMBER("4"),
        PLUS,
        NUMBER("0.5"),
        DASH,
        NUMBER("6"),
        DASH,
        NUMBER("2"),
        PLUS,
        NUMBER("4"))) ===
      Success(
        TermNode(
          PLUS,
          TermNode(
            DASH,
            TermNode(
              DASH,
              TermNode(
                PLUS,
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
        NUMBER("4"),
        STAR,
        NUMBER("0.5"),
        SLASH,
        NUMBER("6"),
        STAR,
        NUMBER("2"),
        SLASH,
        NUMBER("4"))) ===
      Success(
        FactorNode(
          SLASH,
          FactorNode(
            STAR,
            FactorNode(
              SLASH,
              FactorNode(
                STAR,
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
        NUMBER("4"),
        STAR,
        NUMBER("0.5"),
        PLUS,
        NUMBER("6"))) ===
      Success(TermNode(
        PLUS,
        FactorNode(
          STAR,
          NumberNode(4),
          NumberNode(0.5)),
        NumberNode(6))))

    // 4 + 0.5 * 6
    assert(
      Parser(List(
        NUMBER("4"),
        PLUS,
        NUMBER("0.5"),
        STAR,
        NUMBER("6"))) ===
      Success(TermNode(
        PLUS,
        NumberNode(4),
        FactorNode(
          STAR,
          NumberNode(0.5),
          NumberNode(6)))))

    // 4 + 0.5 * 6 - 2 / 4 * 9
    assert(
      Parser(List(
        NUMBER("4"),
        PLUS,
        NUMBER("0.5"),
        STAR,
        NUMBER("6"),
        DASH,
        NUMBER("2"),
        SLASH,
        NUMBER("4"),
        STAR,
        NUMBER("9"))) ===
      Success(TermNode(
        DASH,
        TermNode(
          PLUS,
          NumberNode(4.0),
          FactorNode(
            STAR,
            NumberNode(0.5),
            NumberNode(6))),
        FactorNode(
          STAR,
          FactorNode(
            SLASH,
            NumberNode(2.0),
            NumberNode(4.0)),
          NumberNode(9)))))
  }

  test("Parser should prioritize parentheses over other operators") {

    // 4 * (0.5 + 6)
    assert(
      Parser(List(
        NUMBER("4"),
        STAR,
        LPAREN,
        NUMBER("0.5"),
        PLUS,
        NUMBER("6"),
        RPAREN)) ===
      Success(FactorNode(
        STAR,
        NumberNode(4),
        TermNode(
          PLUS,
          NumberNode(0.5),
          NumberNode(6)))))

    // (4 + 0.5) * 6
    assert(
      Parser(List(
        LPAREN,
        NUMBER("4"),
        PLUS,
        NUMBER("0.5"),
        RPAREN,
        STAR,
        NUMBER("6"))) ===
      Success(FactorNode(
        STAR,
        TermNode(
          PLUS,
          NumberNode(4),
          NumberNode(0.5)),
        NumberNode(6))))

  }

  test("Parser should correctly handle nested parentheses") {

    // (2 + (3 * (2 - 5) + (2)) / -7) + (5 - 3)
    assert(
      Parser(List(
        LPAREN,
        NUMBER("2"),
        PLUS,
        LPAREN,
        NUMBER("3"),
        STAR,
        LPAREN,
        NUMBER("2"),
        DASH,
        NUMBER("5"),
        RPAREN,
        PLUS,
        LPAREN,
        NUMBER("2"),
        RPAREN,
        RPAREN,
        SLASH,
        DASH,
        NUMBER("7"),
        RPAREN,
        PLUS,
        LPAREN,
        NUMBER("5"),
        DASH,
        NUMBER("3"),
        RPAREN)) ===
      Success(TermNode(
        PLUS,
        TermNode(
          PLUS,
          NumberNode(2),
          FactorNode(
            SLASH,
            TermNode(
              PLUS,
              FactorNode(
                STAR,
                NumberNode(3),
                TermNode(
                  DASH,
                  NumberNode(2),
                  NumberNode(5))),
              NumberNode(2)),
            SignNode(
              DASH,
              NumberNode(7)))),
        TermNode(
          DASH,
          NumberNode(5),
          NumberNode(3)))))

    // 4 * (0.5 + ((6 - 2 * 2) / ((4 * 4) - 0.5) + 9))
    assert(
      Parser(List(
        NUMBER("4"),
        STAR,
        LPAREN,
        NUMBER("0.5"),
        PLUS,
        LPAREN,
        LPAREN,
        NUMBER("6"),
        DASH,
        NUMBER("2"),
        STAR,
        NUMBER("2"),
        RPAREN,
        SLASH,
        LPAREN,
        LPAREN,
        NUMBER("4"),
        STAR,
        NUMBER("4"),
        RPAREN,
        DASH,
        NUMBER("0.5"),
        RPAREN,
        PLUS,
        NUMBER("9"),
        RPAREN,
        RPAREN)) ===
      Success(FactorNode(
        STAR,
        NumberNode(4),
        TermNode(
          PLUS,
          NumberNode(0.5),
          TermNode(
            PLUS,
            FactorNode(
              SLASH,
              TermNode(
                DASH,
                NumberNode(6),
                FactorNode(
                  STAR,
                  NumberNode(2),
                  NumberNode(2))),
              TermNode(
                DASH,
                FactorNode(
                  STAR,
                  NumberNode(4),
                  NumberNode(4)),
                NumberNode(0.5))),
            NumberNode(9))))))
  }

  test("Parser should fail if it receives adjacent numbers without an operator or parentheses") {
    assert(convertTryToSuccessOrFailure(Parser(List(
      NUMBER("5"),
      NUMBER("1")))
    ).failure.exception.getMessage contains "unparsed tokens")
  }

  test("Parser should fail if it receives unmatched closing parentheses") {
    assert(convertTryToSuccessOrFailure(Parser(List(
      NUMBER("1"),
      SLASH,
      NUMBER("2"),
      RPAREN))
    ).failure.exception.getMessage contains "unmatched `)`")
  }

  test("Parser should fail if it has leftover unclosed parentheses") {
    assert(convertTryToSuccessOrFailure(Parser(List(
      NUMBER("1"),
      SLASH,
      LPAREN,
      NUMBER("2")))
    ).failure.exception.getMessage contains "unmatched `(`")
  }

  test("Parser should fail if an infix binary operation is lacking two arguments") {
    assert(convertTryToSuccessOrFailure(Parser(List(
      NUMBER("1"),
      SLASH))
    ).failure.exception.getMessage contains "a value was expected")

    assert(convertTryToSuccessOrFailure(Parser(List(
      SLASH,
      NUMBER("1")))
    ).failure.exception.getMessage contains "a value was expected")

    assert(convertTryToSuccessOrFailure(Parser(List(
      NUMBER("5"),
      PLUS,
      LPAREN,
      NUMBER("2"),
      STAR,
      RPAREN))
    ).failure.exception.getMessage contains "a value was expected")
  }
}