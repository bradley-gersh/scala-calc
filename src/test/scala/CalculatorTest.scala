import org.scalatest.funsuite.AnyFunSuite

import it.scalalearn.calculator.Calculator

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

}
