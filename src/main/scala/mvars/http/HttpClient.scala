package mvars.http

import cats.effect.ConcurrentEffect
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext.global
import scala.language.higherKinds

class HttpClient[F[_]: ConcurrentEffect] {
  val clientBuilder: BlazeClientBuilder[F] = BlazeClientBuilder[F](global)

  def getUrl(url: String): F[String] =
    clientBuilder.resource.use { client =>
      client.expect[String](url)
    }
}
