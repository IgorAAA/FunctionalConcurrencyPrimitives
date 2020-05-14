package mvars

import cats.effect.concurrent.MVar
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Sync}
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.implicits._

import scala.language.higherKinds

sealed trait LogCommandF[F[_]]
final case class MessageF[F[_]](value: String) extends LogCommandF[F]
final case class StopF[F[_]](m: MVar[F, Unit]) extends LogCommandF[F]

final case class LoggerF[F[_]: ConcurrentEffect: ContextShift](
  private val value: MVar[F, LogCommandF[F]]
)

object LoggerF {
  def initLogger[F[_]: ConcurrentEffect: ContextShift]: F[LoggerF[F]] =
    for {
      m <- MVar.empty[F, LogCommandF[F]]
      log = LoggerF[F](m)
      _ <- logger(log).start
    } yield log

  private def logger[F[_]: Sync: ContextShift](l: LoggerF[F]): F[Unit] =
    for {
      cmd <- l.value.take
      _ <- cmd match {
        case MessageF(v) => Sync[F].delay(println(v)) *> logger(l)
        case StopF(s)    => Sync[F].delay(println("Logger: stop")) *> s.put(())
      }
    } yield ()

  def logMessage[F[_]: Sync](logger: LoggerF[F], msg: String): F[Unit] = {
    logger.value.put(MessageF(msg))
  }

  def logStop[F[_]: ConcurrentEffect: ContextShift](
    logger: LoggerF[F]
  ): F[Unit] =
    for {
      s <- MVar.empty[F, Unit]
      _ <- logger.value.put(StopF(s))
      _ <- s.take
    } yield ()
}

object LoggingExample extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      l <- LoggerF.initLogger[IO]
      _ <- LoggerF.logMessage(l, "Hello!")
      _ <- LoggerF.logMessage(l, "Bye!")
      _ <- LoggerF.logStop(l)
      _ <- LoggerF.logMessage(l, "We never come here")
    } yield ExitCode.Success
}
