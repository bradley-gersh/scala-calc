package it.scalalearn.calculator

import scala.util.Success

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues.convertTryToSuccessOrFailure

class EvaluatorTest extends AnyFunSuite {
  test("Evaluator should fail if attempting to evaluate an empty parse tree") {
    val testEmpty = EmptyNode()
    assert(convertTryToSuccessOrFailure(Evaluator(testEmpty)).failure.exception.getMessage contains "incomplete input")
  }

  test("Evaluator should evaluate a single number node") {
    val testSingleNumber = NumberNode(4.5)
    assert(Evaluator(testSingleNumber).getOrElse(Double.NaN) == 4.5)
  }

  test("Evaluator should reject a number node with NaN") {
    val testNaN = NumberNode(Double.NaN)
    assert(convertTryToSuccessOrFailure(
      Evaluator(testNaN)).failure.exception.getMessage contains "NaN")
  }

  test("Evaluator should reject an infinite number node") {
    val testPositiveInfinity = NumberNode(Double.PositiveInfinity)
    assert(convertTryToSuccessOrFailure(
      Evaluator(testPositiveInfinity)).failure.exception.getMessage contains "infinite")

    val testNegativeInfinity = NumberNode(Double.NegativeInfinity)
    assert(convertTryToSuccessOrFailure(
      Evaluator(testNegativeInfinity)).failure.exception.getMessage contains "infinite")
  }

  test("Evaluator should fail if an infinite value is computed") {
    val testOverflow = FactorNode(
      Token(TokenType.STAR, "*"),
      NumberNode(99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999),
      NumberNode(99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999)
    )
    assert(convertTryToSuccessOrFailure(
      Evaluator(testOverflow)).failure.exception.getMessage contains "infinite")
  }

  test("Evaluator should evaluate simple sums and differences") {
    val testSum = TermNode(
      Token(TokenType.PLUS, "+"),
      NumberNode(3.1),
      NumberNode(0.1))
    assert(Evaluator(testSum) === Success(3.2))

    val testDifference = TermNode(
      Token(TokenType.DASH, "-"),
      NumberNode(3.0),
      NumberNode(0.5))
    assert(Evaluator(testDifference) === Success(2.5))
  }

  test("Evaluator should evaluate simple products and quotients") {
    val testProduct = FactorNode(
      Token(TokenType.STAR, "*"),
      NumberNode(3.2),
      NumberNode(0.5))
    assert(Evaluator(testProduct) === Success(1.6))

    val testQuotient = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(3.2),
      NumberNode(0.1))
    assert(Evaluator(testQuotient) === Success(32.0))
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
    assert(convertTryToSuccessOrFailure(
      Evaluator(testDivZeroLiteral)).failure.exception.getMessage contains "division by zero")

    val testDivZeroSubexpression = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(2.0),
      TermNode(
        Token(TokenType.DASH, "-"),
        NumberNode(5.0),
        NumberNode(5.0)
      ))
    assert(convertTryToSuccessOrFailure(
      Evaluator(testDivZeroSubexpression)).failure.exception.getMessage contains "division by zero")

    val testDivZeroSubexpressionRoundoff = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(2.0),
      TermNode(
        Token(TokenType.DASH, "-"),
        NumberNode(5.0),
        NumberNode(4.9999999999999999999999)
      ))
    assert(convertTryToSuccessOrFailure(
      Evaluator(testDivZeroSubexpressionRoundoff)).failure.exception.getMessage contains "division by zero")

    val testZeroOverZero = FactorNode(
      Token(TokenType.SLASH, "/"),
      NumberNode(0),
      NumberNode(0)
    )
    assert(convertTryToSuccessOrFailure(
      Evaluator(testZeroOverZero)).failure.exception.getMessage contains "0/0")
  }
}
