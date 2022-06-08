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
      val (trimmedInput, viewTree) = checkForShowTree(input.getOrElse(""))
      printResult(evaluate(trimmedInput, viewTree))
      repl()
    } else {
      println("goodbye")
    }
  }

  /**
   * Evaluates an arithmetic expression using the lexer and parser
   *
   * @param  input    a preprocessed string of user input to be evaluated
   * @return          Try of numerical value from evaluating input, the tree for possible printing, and a Boolean
   *                  representing whether the user has requested to see the parse tree
   */
  def evaluate(input: String, showTree: Boolean): Try[(Double, ParseNode, Boolean)] = {
    for {
      tokens <- Reader(input)
      tree <- Parser(tokens)
      value <- Evaluator(tree)
    } yield (value, tree, showTree)
  }

  /**
   * Checks whether user has requested to show the parse tree
   *
   * @param  input  raw user input for parsing
   * @return        a 2-tuple containing the trimmed input and Boolean indicating whether or not to show the parse tree
   */
  private def checkForShowTree(input: String): (String, Boolean) =
      if (input(0) == '?') (input.tail, true)
      else (input, false)

  /**
   * Displays the result of evaluating the parse tree of the computation.
   *
   * @param  evalOutput  direct output from the `evaluate` function: a Try containing the result of a computation,
   *                     the root node of the parse tree, and a Boolean of whether to display the parse tree
   */
  private def printResult(evalOutput: Try[(Double, ParseNode, Boolean)]): Unit = {
    println(evalOutput match {
      case Success((value, tree, showTree)) => (if (showTree) s"--> parse tree: $tree\n" else "") + s"= $value\n"
      case Failure(exception) => s"[error] ${exception.getClass.getSimpleName}: ${exception.getMessage}\n"
    })
  }
}
