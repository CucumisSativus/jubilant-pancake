package net.cucumbersome.pascallInterpreter

import cats.data.StateT
import cats.mtl.instances.state._
import cats.instances.either._
import net.cucumbersome.pascallInterpreter.Interpreter.{InterpreterError, InterpreterState}
import org.specs2.matcher.MatchResult
class InterpreterTest extends org.specs2.mutable.Specification {

  type CanFailMonad[T] = Either[InterpreterError, T]
  type InterpreterMonad[T] = StateT[CanFailMonad, InterpreterState, T]


  "Interpreter" >>{
    "should format simple math expressions" >> {
      "one digit + other digit" >> {
        run("1+3", Token.IntNumber(4))
      }

      "one digit - other digit" >> {
        run("3-1", Token.IntNumber(2))
      }

    }
  }

  def run(input: String, expectedOutput: Token): MatchResult[Token] = {
    val a = for{
      token <- Interpreter.evaluate[InterpreterMonad](input)
    } yield {
      token should_=== expectedOutput
    }
    val res: CanFailMonad[(InterpreterState, MatchResult[Token])] = a.run(InterpreterState(List.empty, None))
    res match {
      case Right((_, matchResult)) => matchResult
      case Left(err)  => sys.error(s"Interpreter failed with error $err")
    }
  }
}
