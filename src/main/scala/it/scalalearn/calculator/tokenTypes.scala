package it.scalalearn.calculator

/**
 * Valid tokens accepted by the calculator
 */
sealed abstract trait Token(val string: String)

case object LPAREN extends Token(string ="(")
case object RPAREN extends Token(string = ")")
case object DOT extends Token(string = ".")
case object PLUS extends Token(string = "+")
case object DASH extends Token(string = "-")
case object STAR extends Token(string = "*")
case object SLASH extends Token(string = "/")
case class NUMBER(override val string: String) extends Token(string)