package mvars

import cats.effect.concurrent.MVar
import cats.effect.{Concurrent, ContextShift, ExitCode, IO, IOApp}
import cats.syntax.apply._

sealed trait LogCommand
final case class Message(value: String) extends LogCommand
final case class Stop(m: MVar[IO, Unit]) extends LogCommand

final case class Logger(private val value: MVar[IO, LogCommand])
object Logger {
  def initLogger(implicit cs: ContextShift[IO], c: Concurrent[IO]): IO[Logger] =
    for {
      m <- MVar.empty[IO, LogCommand]
      log = Logger(m)
      _ <- logger(log).start
    } yield log

  private def logger(l: Logger): IO[Unit] =
    for {
      cmd <- l.value.take
      _ <- cmd match {
        case Message(v) => IO(println(v)) *> logger(l)
        case Stop(s)    => IO(println("Logger: stop")) *> s.put(())
      }
    } yield ()

  def logMessage(logger: Logger, msg: String): IO[Unit] =
    logger.value.put(Message(msg))

  def logStop(logger: Logger)(implicit cs: ContextShift[IO]): IO[Unit] =
    for {
      s <- MVar.empty[IO, Unit]
      _ <- logger.value.put(Stop(s))
      _ <- s.take
    } yield ()
}

object LoggingExampleIO extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      l <- Logger.initLogger
      _ <- Logger.logMessage(l, "Hello!")
      _ <- Logger.logMessage(l, "Bye!")
      _ <- Logger.logStop(l)
      _ <- Logger.logMessage(l, "Never come here")
    } yield ExitCode.Success
}
