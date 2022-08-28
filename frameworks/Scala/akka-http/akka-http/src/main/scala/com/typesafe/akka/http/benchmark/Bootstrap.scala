package com.typesafe.akka.http.benchmark

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.util.FastFuture

import scala.concurrent.Future

trait Bootstrap {
  def run(): Unit
}

trait BenchmarkBootstrap extends Bootstrap { _: Infrastructure with RequestMapping =>
  override def run(): Unit =
    Http().newServerAt(
      appConfig.getString("akka.http.benchmark.host"),
      appConfig.getInt("akka.http.benchmark.port"))
      .adaptSettings(settings => settings.mapHttp2Settings(_.withMaxConcurrentStreams(16)))
      .bind(handler)

  val plainTextResponse = FastFuture.successful(HttpResponse(entity = HttpEntity("Hello, World!")))
  lazy val mainHandler: HttpRequest => Future[HttpResponse] = asRoute
  lazy val handler: HttpRequest => Future[HttpResponse] = {
    case HttpRequest(HttpMethods.GET, Uri.Path("/plaintext"), _, _, _) => plainTextResponse
    case x => mainHandler(x)
  }
}