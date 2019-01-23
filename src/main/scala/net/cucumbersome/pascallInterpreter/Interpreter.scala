package net.cucumbersome.pascallInterpreter
import cats.{Monad, MonadError}
import cats.implicits._
import cats.mtl.MonadState
object Interpreter {

  sealed trait InterpreterError
  case object ParsingError                 extends InterpreterError
  case class UnexpectedToken(token: Token) extends InterpreterError

  case class InterpreterState(input: List[Char], currentToken: Option[Token])

  type WithState[F[_]] = MonadState[F, InterpreterState]
  type CanFail[F[_]]   = MonadError[F, InterpreterError]
  case class TokenizerOutput(rest: List[Char], token: Token)
  def getNextToken[F[_]](input: List[Char])(implicit F: CanFail[F]): F[TokenizerOutput] =
    input match {
      case Nil => F.pure(TokenizerOutput(List.empty[Char], Token.Eof))
      case num :: tail if num.isDigit =>
        F.pure(TokenizerOutput(tail, Token.IntNumber(num.toInt - 48)))
      case '+' :: tail => F.pure(TokenizerOutput(tail, Token.Plus))
      case _           => F.raiseError(ParsingError)
    }

  def eat[F[_]: Monad](condition: Token => Boolean)(implicit F: CanFail[F],
                                                    S: WithState[F]): F[Token] = {
    def possibleToken(state: InterpreterState): F[TokenizerOutput] =
      getNextToken[F](state.input).flatMap {
        case TokenizerOutput(remainingInput, token) =>
          if (condition(token)) F.pure(TokenizerOutput(remainingInput, token))
          else F.raiseError(UnexpectedToken(token))
      }

    for {
      state <- S.get
      res   <- possibleToken(state)
      _     <- S.modify(_.copy(input = res.rest))
    } yield res.token

  }

  def evaluate[F[_]: CanFail](input: String)(implicit S: WithState[F]): F[Token] = {
    for{
      _ <- S.set(InterpreterState(input.toSeq.toList, None))
      firstDigit <- eat[F](_.isInstanceOf[Token.IntNumber])
      _ <- eat[F](_.isInstanceOf[Token.Plus.type])
      secondDigit <- eat[F](_.isInstanceOf[Token.IntNumber])
    } yield {
      (firstDigit, secondDigit) match {
        case (Token.IntNumber(value), Token.IntNumber(value2)) => Token.IntNumber(value + value2)
        case _ => Token.Eof
      }
    }
  }
}
