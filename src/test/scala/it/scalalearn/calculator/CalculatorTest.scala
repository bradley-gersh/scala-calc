package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {
  test("Calculator.expr should parse double-precision floats") {
    assert(Calculator.expr("4.2") === 4.2)
    assert(Calculator.expr("-3.1") === -3.1)
    assert(Calculator.expr("0") === 0.0)
  }

  test("Calculator.expr should parse integers into double-precision floats") {
    assert(Calculator.expr("3") === 3.0)
    assert(Calculator.expr("-3") === -3.0)
  }

  test("Calculator.expr should process simple addition and subtraction") {
    assert(Calculator.expr("2 + 4") === 6.0)
    assert(Calculator.expr("2 - 4") === -2.0)
  }
}
