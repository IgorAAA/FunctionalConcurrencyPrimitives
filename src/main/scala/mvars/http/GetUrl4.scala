package mvars.http

import cats.ApplicativeError
import cats.effect.concurrent.MVar
import cats.effect.syntax.concurrent._
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.language.higherKinds

final case class AsyncE[F[_], A](m: MVar[F, Either[Throwable, A]])

object AsyncE {
  def async[F[_]: ConcurrentEffect: ContextShift, A](
    action: F[A]
  )(implicit AE: ApplicativeError[F, Throwable]): F[AsyncE[F, A]] =
    for {
      m <- MVar.empty[F, Either[Throwable, A]]
      _ <- (for {
        a <- AE.attempt(action)
        _ <- m.put(a)
      } yield ()).start
    } yield AsyncE(m)

  private def waitCatch[F[_], A]: AsyncE[F, A] => F[Either[Throwable, A]] =
    _.m.take

  def waitA[F[_]: Sync, A](
    a: AsyncE[F, A]
  )(implicit AE: ApplicativeError[F, Throwable]): F[A] = waitCatch(a).flatMap {
    case Left(err)  => AE.raiseError[A](err)
    case Right(res) => Sync[F].delay(res)
  }
}

object GetUrl4 extends IOApp {
  import AsyncE._

  val httpClient = new HttpClient[IO]

  override def run(args: List[String]): IO[ExitCode] =
    for {
      a1 <- async[IO, String](
        httpClient.getUrl("https://en.wikipedia.org/wiki/Shove")
      )
      a2 <- async[IO, String](
        httpClient.getUrl("https://en.wikipedia.org/wiki/333fas")
      )
      r1 <- waitA(a1).handleErrorWith(_ => IO.pure(""))
      r2 <- waitA(a2).handleErrorWith(_ => IO.pure(""))
      _ <- IO(println(r1.length))
      _ <- IO(println(r2.length))
    } yield ExitCode.Success
}
