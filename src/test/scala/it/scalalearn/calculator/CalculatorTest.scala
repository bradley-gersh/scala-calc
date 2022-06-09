package it.scalalearn.calculator

import java.io.{ByteArrayOutputStream, StringReader}

import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {
  test("Calculator.repl should close when an empty line is entered") {
    val mockOut = new ByteArrayOutputStream()
    Console.withOut(mockOut) {
      Console.withIn(new StringReader("")) {
        Calculator.repl()
      }
    }
    assert(mockOut.toString.split("\\s").contains("goodbye"))
  }

  ignore("Calculator.evaluate should echo back a single number") {
  }

  ignore("Calculator.evaluate should evaluate simple sums and differences") {

  }

  ignore("Calculator.evaluate should evaluate simple products and quotients") {
  }

  ignore("Calculator.evaluate should handle parentheses") {

  }

  ignore("Calculator.evaluate should correctly handle order of operations") {

  }

  ignore("Calculator.evaluate should gracefully recover from division by 0") {

  }

  ignore("Calculator.evaluate should fail in case of overflow") {
  }

  ignore("Calculator.evaluate should fail in case of division by zero") {
  }

  ignore("Calculator.evaluate should fail if parentheses are empty") {
  }

  ignore("Calculator.evaluate should fail if parentheses are unmatched (left or right)") {
  }

  ignore("Calculator.evaluate should fail if a binary operator is lacking two arguments") {
  }

  ignore("Calculator.evaluate should fail if it receives adjacent numbers without an operator or parentheses") {
  }

}