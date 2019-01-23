package net.cucumbersome.pascallInterpreter

sealed trait Token{
  def isMathematicalOperator: Boolean = {
    this match {
      case Token.Plus | Token.Minus => true
      case _ => false
    }
  }
}

object Token{
  case class IntNumber(value: Int) extends Token
  case object Plus extends Token
  case object Eof extends Token
  case object Minus extends Token

}

