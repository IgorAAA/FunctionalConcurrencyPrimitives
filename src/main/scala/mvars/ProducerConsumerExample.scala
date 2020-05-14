package mvars

import cats.Monoid
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.effect.concurrent.MVar
import cats.derived.semi
import cats.instances.int._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.parallel._

class Channel[F[_]: Sync, A: Monoid] {
  def producer(ch: MVar[F, Option[A]])(input: List[A]): F[Unit] = input match {
    case Nil => ch.put(None)
    case (x :: xs) =>
      Sync[F].delay(println(s"trying to put $x")) >> ch
        .put(Some(x)) >> producer(ch)(xs)
  }

  def consumer(ch: MVar[F, Option[A]]): F[A] = {
    def consume(ch: MVar[F, Option[A]], sum: A): F[A] =
      ch.take.flatMap {
        case Some(x) => consume(ch, Monoid[A].combine(x, sum))
        case None    => Sync[F].pure(sum)
      }
    consume(ch, Monoid[A].empty)
  }
}

object ProducerConsumerExample extends IOApp {
  val N = 1000

  case class Item(value: Int)

  implicit val m: Monoid[Item] = semi.monoid

  val channel = new Channel[IO, Item]

  val program: IO[Unit] =
    for {
      ch <- MVar[IO].empty[Option[Item]]
      pr = channel.producer(ch)((0 to N).toList.map(Item))
      cons = channel.consumer(ch)
      sum <- (pr, cons).parMapN((_, sum) => sum)
      _ <- IO(println(sum))
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)
}
