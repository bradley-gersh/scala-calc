package it.scalalearn.calculator

import scala.util.Right

import org.scalatest.funsuite.AnyFunSuite

class EvaluatorTest extends AnyFunSuite {
  test("Evaluator should fail if attempting to evaluate an empty parse tree") {
    val testEmpty = EmptyNode
    assert(Evaluator.eval(testEmpty).left.filterToOption(_ contains "incomplete input").nonEmpty)
  }

  test("Evaluator should evaluate a single number node") {
    val testSingleNumber = NumberNode(4.5)
    assert(Evaluator.eval(testSingleNumber).getOrElse(Double.NaN) == 4.5)
  }

  test("Evaluator should reject a number node with NaN") {
    val testNaN = NumberNode(Double.NaN)
    assert(Evaluator.eval(testNaN).left.filterToOption(_ contains "NaN").nonEmpty)
  }

  test("Evaluator should reject an infinite number node") {
    val testPositiveInfinity = NumberNode(Double.PositiveInfinity)
    assert(Evaluator.eval(testPositiveInfinity).left.filterToOption(_ contains "infinite").nonEmpty)

    val testNegativeInfinity = NumberNode(Double.NegativeInfinity)
    assert(Evaluator.eval(testNegativeInfinity).left.filterToOption(_ contains "infinite").nonEmpty)
  }

  test("Evaluator should fail if an infinite value is computed") {
    val testOverflow = FactorNode(
      STAR,
      NumberNode(99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999),
      NumberNode(99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999)
    )
    assert(Evaluator.eval(testOverflow).left.filterToOption(_ contains "infinite").nonEmpty)
  }

  test("Evaluator should evaluate simple sums and differences") {
    val testSum = TermNode(
      PLUS,
      NumberNode(3.1),
      NumberNode(0.1))
    assert(Evaluator.eval(testSum) === Right(3.2))

    val testDifference = TermNode(
      DASH,
      NumberNode(3.0),
      NumberNode(0.5))
    assert(Evaluator.eval(testDifference) === Right(2.5))
  }

  test("Evaluator should evaluate simple products and quotients") {
    val testProduct = FactorNode(
      STAR,
      NumberNode(3.2),
      NumberNode(0.5))
    assert(Evaluator.eval(testProduct) === Right(1.6))

    val testQuotient = FactorNode(
      SLASH,
      NumberNode(3.2),
      NumberNode(0.1))
    assert(Evaluator.eval(testQuotient) === Right(32.0))
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
    assert(Evaluator.eval(testNested) === Right(28.8))
  }

  test("Evaluator should fail to evaluate division by 0") {
    val testDivZeroLiteral = FactorNode(
      SLASH,
      NumberNode(2.0),
      NumberNode(0)
    )
    assert(Evaluator.eval(testDivZeroLiteral).left.filterToOption(_ contains "division by 0").nonEmpty)

    val testDivZeroSubexpression = FactorNode(
      SLASH,
      NumberNode(2.0),
      TermNode(
        DASH,
        NumberNode(5.0),
        NumberNode(5.0)
      ))
    assert(Evaluator.eval(testDivZeroSubexpression).left.filterToOption(_ contains "division by 0").nonEmpty)

    val testDivZeroSubexpressionRoundoff = FactorNode(
      SLASH,
      NumberNode(2.0),
      TermNode(
        DASH,
        NumberNode(5.0),
        NumberNode(4.9999999999999999999999)
      ))
    assert(Evaluator.eval(testDivZeroSubexpressionRoundoff).left.filterToOption(_ contains "division by 0").nonEmpty)

    val testZeroOverZero = FactorNode(
      SLASH,
      NumberNode(0),
      NumberNode(0)
    )
    assert(Evaluator.eval(testZeroOverZero).left.filterToOption(_ contains "0/0").nonEmpty)
  }
}
