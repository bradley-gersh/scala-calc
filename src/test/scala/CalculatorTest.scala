import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {
  test("Calculator.expr") {
    assert(Calculator.expr(3) === 27.0)
  }
}
