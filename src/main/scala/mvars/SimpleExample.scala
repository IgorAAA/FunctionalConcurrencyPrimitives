package mvars

import cats.effect.concurrent.MVar
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration._

class SimpleExample extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      m <- MVar.empty[IO, Char]
      _ <- IO(println("Start"))
      _ <- (timer.sleep(3 seconds) *> m.put('x')).start
      r <- m.take
      _ <- IO(println(r))
    } yield ExitCode.Success
  }
}
