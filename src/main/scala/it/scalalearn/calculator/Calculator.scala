package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Calculator {
  /**
   * Entry point: Print banner and run REPL
   */
  def main(args: Array[String]): Unit = {
    repl()
  }

  /**
   * The REPL user interface
   */
  @tailrec
  def repl(): Unit = {
    print("> ")
    val input = readLine()
    if (input.length > 0) {
      println(evaluate(input))
      repl()
    } else {
      println("goodbye")
    }
  }

  /**
   * Evaluate an arithmetic expression using the lexer and parser
   */
  def evaluate(input: String): String = {
    val tokens = Reader(input)
    tokens.mkString("\n")
//    val output = Parser(tokens)
//    output.toString
  }
}
