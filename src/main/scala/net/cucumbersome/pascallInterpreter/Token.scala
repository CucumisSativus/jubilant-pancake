package net.cucumbersome.pascallInterpreter

sealed trait MatematicalOperator

sealed trait Token{
  def isMathematicalOperator: Boolean = {
    this.isInstanceOf[MatematicalOperator]
  }
}


object Token{
  case class IntNumber(value: Int) extends Token
  case object Plus extends Token with MatematicalOperator
  case object Eof extends Token
  case object Minus extends Token with MatematicalOperator

}

