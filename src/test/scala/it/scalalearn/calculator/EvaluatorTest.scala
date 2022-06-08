package it.scalalearn.calculator

import scala.util.Success

import org.scalatest.funsuite.AnyFunSuite

class EvaluatorTest extends AnyFunSuite {
  test("Evaluator should fail if attempting to evaluate an empty parse tree") {
    val testEmpty = EmptyNode()
    assert(Evaluator(testEmpty).isFailure)
  }

  test("Evaluator should evaluate a single number node") {
    val testSingleNumber = NumberNode(4.5)
    assert(Evaluator(testSingleNumber).getOrElse(Double.NaN) == 4.5)
  }

  test("Evaluator should reject a number node with NaN") {
    val testNaN = NumberNode(Double.NaN)
    assert(Evaluator(testNaN).isFailure)
  }

  test("Evaluator should reject an infinite number node") {
    val testPositiveInfinity = NumberNode(Double.PositiveInfinity)
    assert(Evaluator(testPositiveInfinity).isFailure)

    val testNegativeInfinity = NumberNode(Double.NegativeInfinity)
    assert(Evaluator(testNegativeInfinity).isFailure)
  }

  test("Evaluator should fail in case of overflow") {
    val testOverflow = TermNode(
      Token(TokenType.STAR, "*"),
      NumberNode(99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999),
      NumberNode(99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999)
    )
    assert(Evaluator(testOverflow).isFailure)
  }

  test("Evaluator should evaluate simple sums and differences") {
    val testSum = TermNode(
      Token(TokenType.PLUS, "+"),
      NumberNode(3.2),
      NumberNode(-0.1))
    assert(Evaluator(testSum) === Success(3.1))

    val testDifference = TermNode(
      Token(TokenType.DASH, "-"),
      NumberNode(3.0),
      NumberNode(-0.5))
    assert(Evaluator(testDifference) === Success(3.5))
  }

  test("Evaluator should evaluate simple products and quotients") {
    val testProduct = FactorNode(
      Token(TokenType.STAR, "*"),
      NumberNode(3.2),
      NumberNode(-0.5))
    assert(Evaluator(testProduct) === Success(-1.6))

    val testQuotient = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(3.2),
      NumberNode(-0.1))
    assert(Evaluator(testQuotient) === Success(-32.0))
  }

  test("Evaluator should handle nested operations") {
    val testNested = FactorNode(
      Token(TokenType.STAR, "*"),
      TermNode(
        Token(TokenType.PLUS, "+"),
        NumberNode(5),
        NumberNode(2.2)
      ),
      NumberNode(4))
    assert(Evaluator(testNested) === Success(28.8))
  }

  test("Evaluator should fail to evaluate division by 0") {
    val testDivZeroLiteral = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(2.0),
      NumberNode(0)
    )
    assert(Evaluator(testDivZeroLiteral).isFailure)

    val testDivZeroSubexpression = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(2.0),
      TermNode(
        Token(TokenType.DASH, "-"),
        NumberNode(5.0),
        NumberNode(5.0)
      ))
    assert(Evaluator(testDivZeroSubexpression).isFailure)

    val testDivZeroSubexpressionRoundoff = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(2.0),
      TermNode(
        Token(TokenType.DASH, "-"),
        NumberNode(5.0),
        NumberNode(4.9999999999999999999999)
      ))
    assert(Evaluator(testDivZeroSubexpressionRoundoff).isFailure)

    val testZeroOverZero = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(0),
      NumberNode(0)
    )
    assert(Evaluator(testZeroOverZero).isFailure)
  }
}
