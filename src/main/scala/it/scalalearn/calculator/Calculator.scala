package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine

/**
 * Application for evaluating the numerical value of arithmetic expressions
 */
object Calculator {

  /**
   * Entry point: Print banner and run REPL
   */
  def main(args: Array[String]): Unit = {
    println("""
      | ScalaCalc v0.1
      |
      | Enter an empty line or ctrl-D to quit (ctrl-Z + enter on Windows).
      |
      | Begin input with a single ? character to view the parse tree for the expression.
      |""".stripMargin)
    repl()
  }

  /**
   * REPL interface
   */
  @tailrec
  def repl(): Unit = {
    val input = Option(readLine("> "))
    if (input.getOrElse("").nonEmpty) {
      println(processInput(input.getOrElse("")))
      repl()
    } else {
      println("goodbye")
    }
  }

  /**
   * Processes and evaluates input from user
   *
   * @param  input  raw user input for parsing
   * @return        result from parsing and evaluating input
   */
  def processInput(input: String): String =
    val (expressionInput, viewTree) =
      if (input(0) == '?') (input.tail, true)
      else (input, false)

    interpret(expressionInput) match {
      case Left(error) => s"[error]: $error\n"
      case Right((value, tree)) => (
        (if (viewTree) s"--> parse tree: ${Printer(tree).getOrElse("[error printing parse tree]")}\n" else "")
          + s"= $value\n")
    }

  /**
   * Evaluates an arithmetic expression using the lexer and parser
   *
   * @param  input    a preprocessed string of user input to be evaluated
   * @return          Try of numerical value from evaluating input and the parse tree for possible printing
   */
  def interpret(input: String): Either[String, (Double, ParseNode)] = {
    for {
      tokens <- Lexer(input)
      tree <- Parser(tokens)
      value <- Evaluator(tree)
    } yield (value, tree)
  }
}