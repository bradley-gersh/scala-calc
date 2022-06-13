package it.scalalearn.calculator

sealed trait CalculatorException(val message: String)

class LexerException(message: String) extends CalculatorException(message)
class ParserException(message: String) extends CalculatorException(message)
class EvaluatorException(message: String) extends CalculatorException(message)

case class NumberFormattingException(override val message: String) extends LexerException(message)
case class UnknownTokenException(override val message: String) extends LexerException(message)

case class InvalidOperatorException(override val message: String) extends ParserException(message)
case class UnmatchedParenthesesException(override val message: String) extends ParserException(message)
case class UnparsedTokensException(override val message: String) extends ParserException(message)
case class EmptyNodeException(override val message: String) extends ParserException(message)

case class DivisionByZeroException(override val message: String) extends EvaluatorException(message)
case class InfiniteValueException(override val message: String) extends EvaluatorException(message)
case class NaNValueException(override val message: String) extends EvaluatorException(message)
