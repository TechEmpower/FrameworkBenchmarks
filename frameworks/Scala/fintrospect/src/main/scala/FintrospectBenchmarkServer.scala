import java.util.TimeZone.getTimeZone

import com.twitter.finagle.http.path.Root
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Filter, Http}
import com.twitter.util.Await
import io.fintrospect.RouteModule
import io.fintrospect.filters.ResponseFilters
import org.apache.commons.lang3.time.FastDateFormat.getInstance

object FintrospectBenchmarkServer extends App {

  val dateFormat = getInstance("EEE, d MMM yyyy HH:mm:ss 'GMT'", getTimeZone("GMT"))

  val addServerAndDate = Filter.mk[Request, Response, Request, Response] { (req, svc) =>
    svc(req).map(resp => {
      resp.headerMap("Server") = "Example"
      resp.headerMap("Date") = dateFormat.format(System.currentTimeMillis())
      resp
    })
  }

  val database = Database()

  val module = RouteModule(Root)
    .withRoute(JsonRoute())
    .withRoute(PlainTextRoute())
    .withRoute(FortunesRoute(database))
    .withRoutes(DatabaseRoutes(database))

  Await.ready(
    Http.server
      .withCompressionLevel(0)
      .serve(":9000", ResponseFilters.CatchAll().andThen(addServerAndDate).andThen(module.toService))
  )
}
