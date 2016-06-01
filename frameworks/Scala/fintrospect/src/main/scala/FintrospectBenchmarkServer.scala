import java.time.ZonedDateTime._
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME

import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status._
import com.twitter.finagle.http.path.Root
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.finagle.{Http, Service}
import com.twitter.util.Await
import io.circe.generic.auto._
import io.fintrospect.formats.ResponseBuilder
import io.fintrospect.formats.json.Circe.JsonFormat._
import io.fintrospect.formats.json.Circe.ResponseBuilder.implicits._
import io.fintrospect.{ModuleSpec, RouteSpec}

object FintrospectBenchmarkServer extends App {

  private def addRequiredHeaders(r: ResponseBuilder[_]) = r.withHeaders("Server" -> "Example", "Date" -> RFC_1123_DATE_TIME.format(now()))

  val plainTextHelloWorld = {
    import io.fintrospect.formats.PlainText.ResponseBuilder.implicits._
    Service.mk { r: Request => addRequiredHeaders(Ok("Hello, World!")) }
  }

  case class Message(message: String)

  val jsonHelloWorld = Service.mk { r: Request => addRequiredHeaders(Ok(encode(Message("Hello, World!")))) }

  val module = ModuleSpec(Root)
    .withRoute(RouteSpec().at(Get) / "plaintext" bindTo plainTextHelloWorld)
    .withRoute(RouteSpec().at(Get) / "json" bindTo jsonHelloWorld)

  Await.ready(
    Http.server
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withTracer(NullTracer)
      .serve(":9000", module.toService)
  )
}
