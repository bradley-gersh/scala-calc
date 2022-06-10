package it.scalalearn.calculator

/**
 * Valid tokens accepted by the calculator
 */
sealed abstract class Token(val string: String)

case class LPAREN() extends Token(string ="(")
case class RPAREN() extends Token(string = ")")
case class DOT() extends Token(string = ".")
case class PLUS() extends Token(string = "+")
case class DASH() extends Token(string = "-")
case class STAR() extends Token(string = "*")
case class SLASH() extends Token(string = "/")
case class WS() extends Token(string = " ")
case class NUMBER(override val string: String) extends Token(string)
case class OTHER(override val string: String) extends Token(string)
