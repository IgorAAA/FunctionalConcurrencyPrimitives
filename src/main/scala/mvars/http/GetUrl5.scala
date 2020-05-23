package mvars.http

import cats.Applicative
import cats.effect.concurrent.MVar
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object GetUrl5 extends IOApp {

  val httpClient = new HttpClient[IO]
  val timeIt = new TimeIt[IO, String]

  val sites = List(
    "http://www.google.com",
    "http://www.bing.com",
    "https://en.wikipedia.org/wiki/Shove",
    "https://en.wikipedia.org/wiki/Spade"
  )

  def download(m: MVar[IO, (String, String)])(url: String): IO[Unit] =
    for {
      r <- httpClient.getUrl(url)
      _ <- m.put(url, r)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for {
      m <- MVar.empty[IO, (String, String)]
      _ <- sites.traverse(download(m)(_)).start
      r <- m.take
      (url, page) = r
      _ <- IO(println(s"$url was first ${page.length}"))
      _ <- Applicative[IO].replicateA(sites.size - 1, m.take)
    } yield ExitCode.Success
}
