package net.cucumbersome.pascallInterpreter

import cats.data.StateT
import cats.mtl.instances.state._
import cats.instances.either._
import net.cucumbersome.pascallInterpreter.Interpreter.{InterpreterError, InterpreterState}
class InterpreterTest extends org.specs2.mutable.Specification {

  type CanFailMonad[T] = Either[InterpreterError, T]
  type InterpreterMonad[T] = StateT[CanFailMonad, InterpreterState, T]


  "Interpreter" >>{
    "should format simple math expressions" >> {
      "one digit + other digit" >> {
        run("1+3", Token.IntNumber(4))
      }
    }
  }

  def run(input: String, expectedOutput: Token) = {
    val a = for{
      token <- Interpreter.evaluate[InterpreterMonad](input)
    } yield {
      token should_=== expectedOutput
    }
    a.run(InterpreterState(List.empty, None)).getOrElse(sys.error("failed"))._2
  }
}
