package mvars.http

import cats.effect.{Clock, Sync, Timer}
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent.duration._
import scala.language.higherKinds

class TimeIt[F[_]: Sync: Clock, A] {
  def timeIt: F[A] => F[(A, Long)] =
    action =>
      for {
        start <- Clock[F].monotonic(MILLISECONDS)
        a <- action
        end <- Clock[F].monotonic(MILLISECONDS)
      } yield (a, end - start)
}
