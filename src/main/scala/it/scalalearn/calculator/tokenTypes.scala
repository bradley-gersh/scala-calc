package it.scalalearn.calculator

/**
 * Valid tokens accepted by the calculator
 */
sealed trait Token {
  def string: String
}

case object LPAREN extends Token {
  override val string = "("
}

case object RPAREN extends Token {
  override val string = ")"
}

case object DOT extends Token {
  override val string = "."
}

case object PLUS extends Token {
  override val string = "+"
}

case object DASH extends Token {
  override val string = "-"
}

case object STAR extends Token {
  override val string = "*"
}

case object SLASH extends Token {
  override val string = "/"
}

case class NUMBER(override val string: String) extends Token

case class UNKNOWN(override val string: String) extends Token