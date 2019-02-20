package net.cucumbersome.pascallInterpreter

import cats.instances.either._
import net.cucumbersome.pascallInterpreter.Interpreter.{InterpreterError, TokenizerOutput}
class TokenizerTest extends org.specs2.mutable.Specification {
  type InterpreterMonad[T] = Either[InterpreterError, T]

  "Tokenizer" >> {
    "getNextToken" >> {
      "Reading eof" >> {
        run(List.empty) should_=== Right(TokenizerOutput(List.empty, Token.Eof))
      }
      "reding number" >> {
        run(List('1')) should_=== Right(TokenizerOutput(List.empty, Token.IntNumber(1)))
        run(List('2')) should_=== Right(TokenizerOutput(List.empty, Token.IntNumber(2)))
        run(List('3')) should_=== Right(TokenizerOutput(List.empty, Token.IntNumber(3)))
        run(List('4')) should_=== Right(TokenizerOutput(List.empty, Token.IntNumber(4)))
        run(List('1', '4')) should_=== Right(TokenizerOutput(List.empty, Token.IntNumber(14)))
        run(List(' ', '1', '4')) should_=== Right(TokenizerOutput(List.empty, Token.IntNumber(14)))

      }
      "reading +" >> {
        run(List('+')) should_=== Right(TokenizerOutput(List.empty, Token.Plus))
      }
      "reading -" >> {
        run(List('-')) should_=== Right(TokenizerOutput(List.empty, Token.Minus))
      }
    }
  }

  def run(input: List[Char]): InterpreterMonad[TokenizerOutput] = {
    Interpreter.getNextToken[InterpreterMonad](input)
  }
}
