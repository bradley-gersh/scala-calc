package it.scalalearn.calculator

import scala.util.Success

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues.convertTryToSuccessOrFailure

class EvaluatorTest extends AnyFunSuite {
  test("Evaluator should fail if attempting to evaluate an empty parse tree") {
    val testEmpty = EmptyNode
    assert(convertTryToSuccessOrFailure(Evaluator.eval(testEmpty)).failure.exception.getMessage contains "incomplete input")
  }

  test("Evaluator should evaluate a single number node") {
    val testSingleNumber = NumberNode(4.5)
    assert(Evaluator.eval(testSingleNumber).getOrElse(Double.NaN) == 4.5)
  }

  test("Evaluator should reject a number node with NaN") {
    val testNaN = NumberNode(Double.NaN)
    assert(convertTryToSuccessOrFailure(
      Evaluator.eval(testNaN)).failure.exception.getMessage contains "NaN")
  }

  test("Evaluator should reject an infinite number node") {
    val testPositiveInfinity = NumberNode(Double.PositiveInfinity)
    assert(convertTryToSuccessOrFailure(
      Evaluator.eval(testPositiveInfinity)).failure.exception.getMessage contains "infinite")

    val testNegativeInfinity = NumberNode(Double.NegativeInfinity)
    assert(convertTryToSuccessOrFailure(
      Evaluator.eval(testNegativeInfinity)).failure.exception.getMessage contains "infinite")
  }

  test("Evaluator should fail if an infinite value is computed") {
    val testOverflow = FactorNode(
      STAR,
      NumberNode(99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999),
      NumberNode(99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999)
    )
    assert(convertTryToSuccessOrFailure(
      Evaluator.eval(testOverflow)).failure.exception.getMessage contains "infinite")
  }

  test("Evaluator should evaluate simple sums and differences") {
    val testSum = TermNode(
      PLUS,
      NumberNode(3.1),
      NumberNode(0.1))
    assert(Evaluator.eval(testSum) === Success(3.2))

    val testDifference = TermNode(
      DASH,
      NumberNode(3.0),
      NumberNode(0.5))
    assert(Evaluator.eval(testDifference) === Success(2.5))
  }

  test("Evaluator should evaluate simple products and quotients") {
    val testProduct = FactorNode(
      STAR,
      NumberNode(3.2),
      NumberNode(0.5))
    assert(Evaluator.eval(testProduct) === Success(1.6))

    val testQuotient = FactorNode(
      SLASH,
      NumberNode(3.2),
      NumberNode(0.1))
    assert(Evaluator.eval(testQuotient) === Success(32.0))
  }

  test("Evaluator should handle nested operations") {
    val testNested = FactorNode(
      STAR,
      TermNode(
        PLUS,
        NumberNode(5),
        NumberNode(2.2)
      ),
      NumberNode(4))
    assert(Evaluator.eval(testNested) === Success(28.8))
  }

  test("Evaluator should fail to evaluate division by 0") {
    val testDivZeroLiteral = FactorNode(
      SLASH,
      NumberNode(2.0),
      NumberNode(0)
    )
    assert(convertTryToSuccessOrFailure(
      Evaluator.eval(testDivZeroLiteral)).failure.exception.getMessage contains "division by zero")

    val testDivZeroSubexpression = FactorNode(
      SLASH,
      NumberNode(2.0),
      TermNode(
        DASH,
        NumberNode(5.0),
        NumberNode(5.0)
      ))
    assert(convertTryToSuccessOrFailure(
      Evaluator.eval(testDivZeroSubexpression)).failure.exception.getMessage contains "division by zero")

    val testDivZeroSubexpressionRoundoff = FactorNode(
      SLASH,
      NumberNode(2.0),
      TermNode(
        DASH,
        NumberNode(5.0),
        NumberNode(4.9999999999999999999999)
      ))
    assert(convertTryToSuccessOrFailure(
      Evaluator.eval(testDivZeroSubexpressionRoundoff)).failure.exception.getMessage contains "division by zero")

    val testZeroOverZero = FactorNode(
      SLASH,
      NumberNode(0),
      NumberNode(0)
    )
    assert(convertTryToSuccessOrFailure(
      Evaluator.eval(testZeroOverZero)).failure.exception.getMessage contains "0/0")
  }
}
