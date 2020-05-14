package mvars.http

import cats.effect.concurrent.MVar
import cats.effect.{ExitCode, IO, IOApp}

object GetUrl1 extends IOApp {
  val client = new HttpClient[IO]

  override def run(args: List[String]): IO[ExitCode] =
    for {
      m1 <- MVar.empty[IO, String]
      m2 <- MVar.empty[IO, String]
      _ <- (for {
        r <- client.getUrl("https://en.wikipedia.org/wiki/Shove")
        _ <- m1.put(r)
      } yield ()).start
      _ <- (for {
        r <- client.getUrl("https://en.wikipedia.org/wiki/Spade")
        _ <- m2.put(r)
      } yield ()).start
      r1 <- m1.take
      _ <- IO(println(r1.length))
      r2 <- m2.take
      _ <- IO(println(r2.length))
    } yield ExitCode.Success
}
