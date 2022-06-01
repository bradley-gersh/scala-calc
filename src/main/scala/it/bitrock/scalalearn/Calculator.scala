package it.bitrock.scalalearn

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Calculator {
  @tailrec
  def readIn(): Unit = {
    print("Enter something: ")
    val mytext: String = readLine()
    if (mytext.length > 0) {
      println(s"You entered $mytext")
      readIn()
    }
  }

  println("in the calculator")
  readIn()
}
