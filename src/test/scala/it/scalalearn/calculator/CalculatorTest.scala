package it.scalalearn.calculator

import java.io.{ByteArrayOutputStream, StringReader}
import scala.util.Random

class CalculatorTest extends BaseTest {
  private def nextRandomDouble(): Double = Random.nextDouble() * Random.nextInt(100) + 0.01
  private def wrap(answer: Double): String = s"= $answer\n"

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
    assert(Calculator.processInput("0") === wrap(0.0))
    assert(Calculator.processInput("1.2") === wrap(1.2))
    assert(Calculator.processInput("-1.2") === wrap(-1.2))

    for (_ <- 1 to 1000) {
      val randomDouble = nextRandomDouble()
      assert(Calculator.processInput(randomDouble.toString) === s"= $randomDouble\n")
    }
  }

  test("Calculator.processInput should not treat -0 as a negative number") {
    // The JVM considers 0.0 and -0.0 to be distinct Doubles, which we forbid here
    assert(Calculator.processInput("-0") === wrap(0.0))
  }

  test("Calculator.processInput should handle multiple unary negative signs") {
    assert(Calculator.processInput("-2") === wrap(-2.0))
    assert(Calculator.processInput("--2") === wrap(2.0))
    assert(Calculator.processInput("---2") === wrap(-2.0))
    assert(Calculator.processInput("-------------2") === wrap(-2.0))
  }

  test("Calculator.processInput should evaluate simple sums and differences") {
    assert(Calculator.processInput("4 + 3") === wrap(7.0))
    assert(Calculator.processInput("2 - 5") === wrap(-3.0))
    assert(Calculator.processInput("46.2 + 2") === wrap(48.2))

    for (_ <- 1 to 1000) {
      val randomDouble1 = nextRandomDouble()
      val randomDouble2 = nextRandomDouble()
      val plus = Random.nextBoolean()
      val randomResult =
        if (plus) randomDouble1 + randomDouble2
        else randomDouble1 - randomDouble2
      val input = s"$randomDouble1 ${if (plus) "+" else "-"} $randomDouble2"
      if (randomResult.toString.contains('E')) assert(true) // calculator doesn't allow scientific input
      else assert(Calculator.processInput(input) === wrap(randomResult))
    }
  }

  test("Calculator.processInput should evaluate simple products and quotients") {
    assert(Calculator.processInput("4 * 3") === wrap(12.0))
    assert(Calculator.processInput("2 / 5") === wrap(0.4))
    assert(Calculator.processInput("46.2 / 2") === wrap(23.1))

    for (_ <- 1 to 1000) {
      val randomDouble1 = nextRandomDouble()
      val randomDouble2 = nextRandomDouble()
      if (randomDouble2 != 0.0) {
        val times = Random.nextBoolean()
        val randomResult =
          if (times) randomDouble1 * randomDouble2 else randomDouble1 / randomDouble2
        if (randomResult.toString.contains('E')) assert(true) // calculator doesn't allow scientific input
        val input = s"$randomDouble1 ${if (times) "*" else "/"} $randomDouble2"
        assert(Calculator.processInput(input) === wrap(randomResult))
      } else assert(true) // don't test division by zero here
    }
  }

  test("Calculator.processInput should handle unary negative after an operator") {
    assert(Calculator.processInput("3 + -7") === wrap(-4.0))
    assert(Calculator.processInput("3 - -7") === wrap(10.0))
    assert(Calculator.processInput("3 * -7") === wrap(-21.0))
    assert(Calculator.processInput("3 / -6") === wrap(-0.5))
  }

  test("Calculator.processInput should correctly evaluate associative operations") {
    assert(Calculator.processInput("3 + 3 + 3 + 3 + 3") === wrap(15.0))
    assert(Calculator.processInput("3 - 3 - 3 - 3 - 3") === wrap(-9.0))
    assert(Calculator.processInput("3 - 3 + 3 - 3 + 3") === wrap(3.0))
    assert(Calculator.processInput("3 * 3 * 3 * 3 * 3") === wrap(243.0))
    assert(Calculator.processInput("2 / 2 / 2 / 2 / 2") === wrap(0.125))
    assert(Calculator.processInput("2 * 2 / 2 * 2 / 2") === wrap(2.0))
  }


  test("Calculator.processInput should correctly handle order of operations without parentheses") {
    assert(Calculator.processInput("3 * 2 + 4") === wrap(10.0))
    assert(Calculator.processInput("3 + 2 * 4") === wrap(11.0))
    assert(Calculator.processInput("3 - 2 / 4") === wrap(2.5))
    assert(Calculator.processInput("3 / 2 - 4") === wrap(-2.5))
  }

  test("Calculator.processInput should handle unnested parentheses") {
    assert(Calculator.processInput("(3)") === wrap(3.0))
    assert(Calculator.processInput("(3 - 20.5)") === wrap(-17.5))
    assert(Calculator.processInput("-3 * (4 + 3)") === wrap(-21.0))
    assert(Calculator.processInput("(2 - 3) / (1 + 1)") === wrap(-0.5))
  }

  test("Calculator.processInput should handle nested parentheses") {
    assert(Calculator.processInput("(2 + (3 * (2 - 5) + (2)) / -7) + (5 - 3)") === wrap(5.0))
  }

  test("Calculator.processInput should handle unary negative before an expression") {
    assert(Calculator.processInput("-2") === wrap(-2.0))
    assert(Calculator.processInput("-(3 + 6)") === wrap(-9.0))
    assert(Calculator.processInput("-(3 * -6)") === wrap(18.0))
  }

  test("Calculator.processInput should ignore space characters on one line") {
    assert(Calculator.processInput("                 ") contains "incomplete input")
    assert(Calculator.processInput("           2      ") === wrap(2.0))
    assert(Calculator.processInput("((3) - (4 / (3 + 1))) * (2 + 6)") === wrap(16.0))
    assert(Calculator.processInput("((3)-(4/(3+1)))*(2+6)") === wrap(16.0))
    assert(Calculator.processInput("(          (    3)-(   4/(3+ 1))  )   *(2+  6   )") === wrap(16.0))

    // currently, we are allowing for whitespace after the unary negative, so "4+- 5" evaluates to "-1.0"
    // consider forbidding in future by scanning unary negative with numbers at the lexer level
  }

  test("Calculator.processInput should print the syntax tree when input begins with `?`") {
    assert(Calculator.processInput("?3 + 3") === "--> parse tree: (+ 3.0 3.0)\n" + wrap(6.0))
    assert(Calculator.processInput("? 3 + 3") === "--> parse tree: (+ 3.0 3.0)\n" + wrap(6.0))
  }

  test("Calculator.processInput should accept decimal points that are empty on one side") {
    assert(Calculator.processInput(".2") === wrap(0.2))
    assert(Calculator.processInput("2.") === wrap(2.0))
    assert(Calculator.processInput(".2 + 2.") === wrap(2.2))
  }

  test("Calculator.processInput should fail if a decimal point is empty on both sides") {
    assert(Calculator.processInput(".") contains "isolated")
    assert(Calculator.processInput("3 . 2") contains "isolated")
  }

  test("Calculator.processInput should fail if a number contains multiple decimal points") {
    assert(Calculator.processInput("3.2.1") contains "only one")
    assert(Calculator.processInput("..2") contains "only one")
    assert(Calculator.processInput("2..") contains "only one")
  }

  test("Calculator.processInput should fail in case of overflow") {
    val enormous = "99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"
    assert(Calculator.processInput(s"$enormous * $enormous") contains "infinite value")
  }

  test("Calculator.processInput should fail in case of division by zero") {
    assert(Calculator.processInput("4 / 0") contains "division by 0")
    assert(Calculator.processInput("4 / (3 - 3)") contains "division by 0")
    assert(Calculator.processInput("4 / (3 - 2.9999999999999999999999999999999999)") contains "division by 0")
  }

  test("Calculator.processInput should give a distinctive error for 0/0") {
    assert(Calculator.processInput("0 / 0") contains "0/0")
    assert(Calculator.processInput("0 / (3 - 2.9999999999999999999999999999999999)") contains "0/0")
  }

  test("Calculator.processInput should fail if parentheses are empty") {
    assert(Calculator.processInput("()") contains "incomplete")
    assert(Calculator.processInput("3 + ()") contains "incomplete")
  }

  test("Calculator.processInput should fail if parentheses are unmatched (left or right)") {
    assert(Calculator.processInput("(3 + 3") contains "unmatched `(`")
    assert(Calculator.processInput("(3 + (2 * 6)") contains "unmatched `(`")
    assert(Calculator.processInput("3 + 3)") contains "unmatched `)`")
    assert(Calculator.processInput("3 + (2 * 6))") contains "unmatched `)`")
  }

  test("Calculator.processInput should fail if a binary operator is lacking two arguments") {
    assert(Calculator.processInput("3 +") contains "value was expected")
    assert(Calculator.processInput("(3 +") contains "value was expected")
    assert(Calculator.processInput("(3 +)") contains "value was expected")
    assert(Calculator.processInput("* 3") contains "value was expected")
    assert(Calculator.processInput("(* 3") contains "value was expected")
    assert(Calculator.processInput("(* 3)") contains "value was expected")
  }

  test("Calculator.processInput should fail if it receives adjacent numbers without an operator or parentheses") {
    assert(Calculator.processInput("3 2") contains "unparsed tokens")
  }

}