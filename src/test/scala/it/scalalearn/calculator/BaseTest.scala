package it.scalalearn.calculator

import org.scalatest.funsuite.AnyFunSuite

class BaseTest extends AnyFunSuite {
  def isError(result: Either[CalculatorException, Any], refMessage: String): Boolean =
    result.left.filterToOption(_.message contains refMessage).nonEmpty
}
