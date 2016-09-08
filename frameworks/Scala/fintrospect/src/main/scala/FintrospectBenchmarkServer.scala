import java.time.ZonedDateTime._
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME

import com.twitter.finagle.http.path.Root
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.finagle.{Filter, Http}
import com.twitter.util.{Await, NullMonitor}
import io.fintrospect.ModuleSpec
import io.fintrospect.renderers.simplejson.SimpleJson

object FintrospectBenchmarkServer extends App {

  val addServerAndDate = Filter.mk[Request, Response, Request, Response] { (req, svc) =>
    svc(req).map(resp => {
      resp.headerMap("Server") = "Example"
      resp.headerMap("Date") = RFC_1123_DATE_TIME.format(now())
      resp
    })
  }

  val module = ModuleSpec(Root, SimpleJson(), addServerAndDate)
    .withRoute(JsonHelloWorld.route)
    .withRoute(PlainTextHelloWorld.route)
    .withRoute(Fortunes.route)

  Await.ready(
    Http.server
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withTracer(NullTracer)
      .withMonitor(NullMonitor)
      .serve(":9000", module.toService)
  )
}
