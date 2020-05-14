package mvars.http

import cats.effect.concurrent.MVar
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IO, IOApp}
import cats.effect.syntax.concurrent._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.language.higherKinds

final case class Async[F[_], A](m: MVar[F, A])

object Async {
  def async[F[_]: ConcurrentEffect: ContextShift, A](
    action: F[A]
  ): F[Async[F, A]] =
    for {
      m <- MVar.empty[F, A]
      _ <- (for {
        a <- action
        _ <- m.put(a)
      } yield ()).start
    } yield Async(m)

  def waitA[F[_], A]: Async[F, A] => F[A] = _.m.take
}

object GetUrl2 extends IOApp {
  import Async._

  val httpClient = new HttpClient[IO]

  override def run(args: List[String]): IO[ExitCode] =
    for {
      a1 <- async[IO, String](
        httpClient.getUrl("https://en.wikipedia.org/wiki/Shove")
      )
      a2 <- async[IO, String](
        httpClient.getUrl("https://en.wikipedia.org/wiki/Spade")
      )
      r1 <- waitA(a1)
      r2 <- waitA(a2)
      _ <- IO(println(r1.length))
      _ <- IO(println(r2.length))
    } yield ExitCode.Success
}
