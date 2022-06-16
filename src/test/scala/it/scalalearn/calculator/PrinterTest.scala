package it.scalalearn.calculator

import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Success

class PrinterTest extends AnyFunSuite {
  test("Printer should return () for an empty parse tree") {
    val testEmpty = EmptyNode
    assert(Printer(testEmpty) === Success("()"))
  }

  test("Printer should print a single number node") {
    val testSingleNumber = NumberNode(4.5)
    assert(Printer(testSingleNumber) === Success("4.5"))
  }

  test("Printer should print `Infinity` for an infinite number node") {
    val testPositiveInfinity = NumberNode(Double.PositiveInfinity)
    assert(Printer(testPositiveInfinity) === Success("Infinity"))

    val testNegativeInfinity = NumberNode(Double.NegativeInfinity)
    assert(Printer(testNegativeInfinity) === Success("-Infinity"))
  }

  test("Printer should print unary operations") {
    val testNeg = SignNode(
      DASH,
      NumberNode(3.2))
    assert(Printer(testNeg) === Success("(- 3.2)"))
  }

  test("Printer should print binary operations") {
    val testSum = TermNode(
      PLUS,
      NumberNode(3.1),
      NumberNode(0.1))
    assert(Printer(testSum) === Success("(+ 3.1 0.1)"))

    val testDifference = TermNode(
      DASH,
      NumberNode(3.0),
      NumberNode(0.5))
    assert(Printer(testDifference) === Success("(- 3.0 0.5)"))

    val testProduct = FactorNode(
      STAR,
      NumberNode(3.2),
      NumberNode(0.5))
    assert(Printer(testProduct) === Success("(* 3.2 0.5)"))

    val testQuotient = FactorNode(
      SLASH,
      NumberNode(3.2),
      NumberNode(0.1))
    assert(Printer(testQuotient) === Success("(/ 3.2 0.1)"))
  }

  test("Printer should handle nested operations") {
    val testSimpleNested = FactorNode(
      STAR,
      TermNode(
        PLUS,
        NumberNode(5),
        NumberNode(2.2)
      ),
      NumberNode(4))
    assert(Printer(testSimpleNested) === Success("(* (+ 5.0 2.2) 4.0)"))

    val testComplexNested = TermNode(
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
        NumberNode(3)))

    assert(Printer(testComplexNested) === Success("(+ (+ 2.0 (/ (+ (* 3.0 (- 2.0 5.0)) 2.0) (- 7.0))) (- 5.0 3.0))"))
  }
}
