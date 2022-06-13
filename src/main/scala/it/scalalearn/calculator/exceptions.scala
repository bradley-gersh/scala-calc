package it.scalalearn.calculator

class CalculatorException(private val message: String) extends RuntimeException(message)

class EvaluatorException(private val message: String) extends CalculatorException(message)

class ParserException(private val message: String) extends CalculatorException(message)

class InfiniteValueException(private val message: String) extends ParserException(message)

class EmptyNodeException(private val message: String) extends ParserException(message)

class DivisionByZeroException(private val message: String) extends ParserException(message)

class InvalidOperatorException(private val message: String) extends ParserException(message)

class UnparsedTokensException(private val message: String) extends ParserException(message)

class UnmatchedParenthesesException(private val message: String) extends ParserException(message)

class LexerException(private val message: String) extends CalculatorException(message)

class UnknownTokenException(private val message: String) extends LexerException(message) {
  override def getMessage: String = s"unrecognized character: $message"
}