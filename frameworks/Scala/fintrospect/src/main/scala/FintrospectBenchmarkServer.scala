import java.util.TimeZone.getTimeZone

import com.twitter.finagle.http.path.Root
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.finagle.{Filter, Http}
import com.twitter.util.{Await, NullMonitor}
import io.fintrospect.ModuleSpec
import io.fintrospect.renderers.simplejson.SimpleJson
import org.apache.commons.lang.time.FastDateFormat.getInstance

object FintrospectBenchmarkServer extends App {

  private val dateFormat = getInstance("EEE, dd MMM yyyy HH:mm:ss 'GMT'", getTimeZone("GMT"))

  val addServerAndDate = Filter.mk[Request, Response, Request, Response] { (req, svc) =>
    svc(req).map(resp => {
      resp.headerMap("Server") = "Example"
      resp.headerMap("Date") = dateFormat.format(System.currentTimeMillis())
      resp
    })
  }

  val database = Database()

  val module = ModuleSpec(Root, SimpleJson(), addServerAndDate)
    .withRoute(JsonRoute())
    .withRoute(PlainTextRoute())
    .withRoute(FortunesRoute(database))
    .withRoutes(DatabaseRoutes(database))

  Await.ready(
    Http.server
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withTracer(NullTracer)
      .withMonitor(NullMonitor)
      .serve(":9000", module.toService)
  )
}
