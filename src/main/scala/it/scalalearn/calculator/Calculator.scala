package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Calculator {
  def expr(input: String): Double = ???

  @tailrec
  def repl(): Unit = {
    print("> ")
    val mytext: String = readLine()
    if (mytext.length > 0) {
      // eval goes here
      println(s"You entered $mytext")
      repl()
    }
  }
}
