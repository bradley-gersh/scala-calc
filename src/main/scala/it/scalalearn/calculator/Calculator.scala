package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}

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
  def repl(): Unit =
    Option(readLine("> ")) match {
      case Some(input) if input.nonEmpty => println(processInput(input))
        repl()
      case _ => println("goodbye")
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

    val evalOutput = interpret(expressionInput)

    evalOutput match {
      case Success((value, tree)) => (
        (if (viewTree) s"--> parse tree: ${Printer(tree).getOrElse("[error printing parse tree]")}\n" else "")
          + s"= $value\n")
      case Failure(exception: CalculatorException) => s"Syntax error: ${exception.getMessage}\n" // improper user input
      case Failure(exception) => s"[error] $exception\n ${exception.printStackTrace()}\n" // software bug
    }

  /**
   * Evaluates an arithmetic expression using the lexer and parser
   *
   * @param  input    a preprocessed string of user input to be evaluated
   * @return          Try of numerical value from evaluating input and the parse tree for possible printing
   */
  def interpret(input: String): Try[(Double, ParseNode)] = {
    for {
      tokens <- Lexer(input)
      tree <- Parser(tokens)
      value <- Evaluator(tree)
    } yield (value, tree)
  }
}