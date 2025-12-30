package pekko.http.benchmark

import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.http.scaladsl.util.FastFuture

import scala.concurrent.Future

trait Bootstrap {
  def run(): Unit
}

trait BenchmarkBootstrap extends Bootstrap { _: Infrastructure with RequestMapping =>
  override def run(): Unit =
    Http().newServerAt(
      appConfig.getString("pekko.http.benchmark.host"),
      appConfig.getInt("pekko.http.benchmark.port"))
      .adaptSettings(settings => settings.mapHttp2Settings(_.withMaxConcurrentStreams(16)))
      .bind(handler)

  val plainTextResponse = FastFuture.successful(HttpResponse(entity = HttpEntity("Hello, World!")))
  lazy val mainHandler: HttpRequest => Future[HttpResponse] = asRoute
  lazy val handler: HttpRequest => Future[HttpResponse] = {
    case HttpRequest(HttpMethods.GET, Uri.Path("/plaintext"), _, _, _) => plainTextResponse
    case x => mainHandler(x)
  }
}