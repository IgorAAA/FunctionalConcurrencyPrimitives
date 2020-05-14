package mvars.http

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object GetUrl3 extends IOApp {
  import Async._

  val httpClient = new HttpClient[IO]
  val timeIt = new TimeIt[IO, String]

  val sites = List(
    "http://www.google.com",
    "http://www.bing.com",
    "https://en.wikipedia.org/wiki/Shove",
    "https://en.wikipedia.org/wiki/Spade"
  )

  def timeDownload(url: String): IO[Unit] =
    for {
      res <- timeIt.timeIt(httpClient.getUrl(url))
      (page, t) = res
      _ <- IO(
        println(s"Downloaded: $url with length ${page.length} in $t millis")
      )
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for {
      as <- sites.traverse(site => async(timeDownload(site)))
      _ <- as.traverse(waitA(_))
    } yield ExitCode.Success
}
