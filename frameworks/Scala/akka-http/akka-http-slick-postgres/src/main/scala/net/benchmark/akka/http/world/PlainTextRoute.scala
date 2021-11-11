package net.benchmark.akka.http.world
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaType}
import akka.http.scaladsl.server.Directives.{complete, path}
import akka.http.scaladsl.server.Route

object PlainTextRoute {

  private val pt = HttpResponse(
    entity = HttpEntity(MediaType.customWithFixedCharset("text", "plain", `UTF-8`), "Hello, World!"))

  def route: Route =
    path("plaintext") {
      complete(pt)
    }

}
