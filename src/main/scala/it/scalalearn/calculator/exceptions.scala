package it.scalalearn.calculator

class CalculatorException(private val message: String) extends RuntimeException(message)

class EvaluatorException(private val message: String) extends CalculatorException(message)

class ParserException(private val message: String) extends CalculatorException(message)

class LexerException(private val message: String) extends CalculatorException(message)

class UnknownTokenException(private val message: String) extends LexerException(message) {
  override def getMessage: String = s"unrecognized character: $message"
}