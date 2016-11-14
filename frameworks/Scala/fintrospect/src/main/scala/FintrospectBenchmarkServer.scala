import java.time.ZonedDateTime._
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME

import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status._
import com.twitter.finagle.http.path.Root
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.finagle.{Http, Service}
import com.twitter.io.Buf
import com.twitter.util.{Await, NullMonitor}
import io.fintrospect.formats.Json4sJackson.JsonFormat._
import io.fintrospect.{ModuleSpec, RouteSpec}

object FintrospectBenchmarkServer extends App {

  val preAllocatedHelloWorldText = Buf.Utf8("Hello, World!")

  val plainTextHelloWorld = {
    import io.fintrospect.formats.PlainText.ResponseBuilder.implicits._
    Service.mk { r: Request =>
      Ok(preAllocatedHelloWorldText)
        .withHeaders("Server" -> "Example", "Date" -> RFC_1123_DATE_TIME.format(now()))
    }
  }

  val jsonHelloWorld = {
    import io.fintrospect.formats.Json4sJackson.ResponseBuilder.implicits._
    Service.mk { r: Request => Ok(obj("message" -> string("Hello, World!")))
      .withHeaders("Server" -> "Example", "Date" -> RFC_1123_DATE_TIME.format(now()))
    }
  }

  val module = ModuleSpec(Root)
    .withRoute(RouteSpec().at(Get) / "plaintext" bindTo plainTextHelloWorld)
    .withRoute(RouteSpec().at(Get) / "json" bindTo jsonHelloWorld)

  Await.ready(
    Http.server
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withTracer(NullTracer)
      .withMonitor(NullMonitor)
      .serve(":9000", module.toService)
  )
}
