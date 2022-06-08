package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}

object Calculator {
  /**
   * Entry point: Print banner and run REPL
   */
  def main(args: Array[String]): Unit = {
    println("ScalaCalc v0.1\n\n"
      + "Enter an empty line or ctrl-D to quit (ctrl-Z + enter on Windows).\n\n"
      + "Begin input with a single ? character to view the parse tree for the expression.\n")
    repl()
  }

  /**
   * REPL interface
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
   * Evaluates an arithmetic expression using the lexer and parser
   *
   * @param  input a string of user input to be evaluated
   * @return       string of numerical value or error message from evaluating input
   */
  def evaluate(input: String): String = {
    val (trimmedInput, viewTree) =
      if (input(0) == '?') (input.tail, true)
      else (input, false)

    val value = for {
      tokens <- Reader(trimmedInput)
      tree <- Parser(tokens)
    } yield (tree.eval(), tree)
    value match {
      case Success((value, tree)) => (if (viewTree) s"--> parse tree: $tree\n" else "") + s"= $value\n"
      case Failure(exception) => s"[error] ${exception.getClass.getSimpleName}: ${exception.getMessage}\n"
    }
  }
}
