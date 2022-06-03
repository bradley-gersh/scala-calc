import org.scalatest.funsuite.AnyFunSuite

import it.scalalearn.calculator.Calculator

class CalculatorTest extends AnyFunSuite {
  test("Calculator.expr") {
    assert(Calculator.expr(3) === 27.0)
  }
}
