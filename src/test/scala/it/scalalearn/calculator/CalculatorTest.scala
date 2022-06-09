package it.scalalearn.calculator

import java.io.{ByteArrayOutputStream, StringReader}
import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {
  private def nextRandomDouble(): Double = Random.nextDouble() * Random.nextInt(100)

  test("Calculator.repl should close when an empty line is entered") {
    val mockOut = new ByteArrayOutputStream()
    Console.withOut(mockOut) {
      Console.withIn(new StringReader("")) {
        Calculator.repl()
      }
    }
    assert(mockOut.toString.split("\\s").contains("goodbye"))
  }

  test("Calculator.processInput should echo back a single number") {
    assert(Calculator.processInput("0") == "= 0.0\n")
    assert(Calculator.processInput("1.2") == "= 1.2\n")
    assert(Calculator.processInput("-1.2") == "= -1.2\n")

    for (_ <- 1 to 1000) {
      val randomDouble = nextRandomDouble()
      assert(Calculator.processInput(randomDouble.toString) === s"= $randomDouble\n")
    }
  }

  test("Calculator.processInput should evaluate simple sums and differences") {
    assert(Calculator.processInput("4 + 3") == "= 7.0\n")
    assert(Calculator.processInput("2 - 5") == "= -3.0\n")
    assert(Calculator.processInput("46.2 + 2") == "= 48.2\n")

    for (_ <- 1 to 1000) {
      val randomDouble1 = nextRandomDouble()
      val randomDouble2 = nextRandomDouble()
      val plus = Random.nextBoolean()
      val randomResult =
        if (plus) randomDouble1 + randomDouble2
        else randomDouble1 - randomDouble2
      val input = s"$randomDouble1 ${if (plus) "+" else "-"} $randomDouble2"
      assert(Calculator.processInput(input) === s"= $randomResult\n")
    }
  }

  test("Calculator.processInput should evaluate simple products and quotients") {
    assert(Calculator.processInput("4 * 3") == "= 12.0\n")
    assert(Calculator.processInput("2 / 5") == "= 0.4\n")
    assert(Calculator.processInput("46.2 / 2") == "= 23.1\n")

    for (_ <- 1 to 1000) {
      val randomDouble1 = nextRandomDouble()
      val randomDouble2 = nextRandomDouble()
      if (randomDouble2 != 0.0) {
        val times = Random.nextBoolean()
        val randomResult =
          if (times) randomDouble1 * randomDouble2
          else randomDouble1 / randomDouble2
        val input = s"$randomDouble1 ${if (times) "*" else "/"} $randomDouble2"
        assert(Calculator.processInput(input) === s"= $randomResult\n")
      } else assert(true) // don't test division by zero here
    }
  }

  ignore("Calculator.processInput should handle parentheses") {
  }

  ignore("Calculator.processInput should correctly handle order of operations") {
  }

  ignore("Calculator.processInput should fail in case of overflow") {
  }

  ignore("Calculator.processInput should fail in case of division by zero") {
  }

  ignore("Calculator.processInput should fail if parentheses are empty") {
  }

  ignore("Calculator.processInput should fail if parentheses are unmatched (left or right)") {
  }

  ignore("Calculator.processInput should fail if a binary operator is lacking two arguments") {
  }

  ignore("Calculator.processInput should fail if it receives adjacent numbers without an operator or parentheses") {
  }

}