package it.scalalearn.calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Calculator {
  def expr(input: String): Double = {
    input.toDouble
  }

  @tailrec
  def readIn(): Unit = {
    print("Enter something: ")
    val mytext: String = readLine()
    if (mytext.length > 0) {
      println(s"You entered $mytext")
      readIn()
    }
  }

}
