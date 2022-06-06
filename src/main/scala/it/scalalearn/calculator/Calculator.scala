package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Calculator {
  /**
   * Entry point: Print banner and run REPL
   */
  def main(args: Array[String]): Unit = {
    println("ScalaCalc v0.1\n\nEnter an empty line or ctrl-D to quit (ctrl-Z + enter on Windows).")
    repl()
  }

  /**
   * The REPL interface
   */
  @tailrec
  def repl(): Unit = {
    print("> ")
    val input = Option(readLine())
    if (input.getOrElse("").length > 0) {
      println(evaluate(input.getOrElse("")))
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
    tokens.fold[String](err => err.getMessage, success => success.mkString("\n"))
//    val output = Parser(tokens)
//    output.toString
  }
}
