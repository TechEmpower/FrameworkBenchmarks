import com.twitter.finagle.client.DefaultPool.Param
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.mysql.{IntValue, QueryRequest, Result, ResultSet, StringValue}
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.finagle.{Mysql, Service}
import com.twitter.util.Duration.fromSeconds
import com.twitter.util.NullMonitor
import io.fintrospect.RouteSpec
import io.fintrospect.formats.Html
import io.fintrospect.templating.MustacheTemplates.CachingClasspath
import io.fintrospect.templating.{RenderView, View}

case class Fortune(id: Int, message: String)

case class FortunesList(items: Seq[Fortune]) extends View

object Fortunes {
  private val toFortunes: PartialFunction[Result, Seq[Fortune]] = {
    case rs: ResultSet => rs.rows
      .map(row =>
        Fortune(
          row("id").map {
            case IntValue(l) => l
            case _ => 0
          }.get,
          row("message").map {
            case StringValue(l) => l
            case _ => "unknown"
          }.get)
      )
    case _ => Seq.empty
  }

  val databaseService = Mysql.client
    .withCredentials("benchmarkdbuser", "benchmarkdbpass")
    .withDatabase("hello_world")
    .configured(Param(low = 0, high = 10, idleTime = fromSeconds(5 * 60), bufferSize = 0, maxWaiters = Int.MaxValue))
    .withStatsReceiver(NullStatsReceiver)
    .withMonitor(NullMonitor)
    .withTracer(NullTracer)
    .withMaxConcurrentPrepareStatements(256)
    .newClient("localhost:3306")

  val statement = QueryRequest("SELECT * FROM Fortune")

  val service = new RenderView(Html.ResponseBuilder, CachingClasspath()).andThen(
    Service.mk {
      r: Request =>
        databaseService()
          .flatMap {
            service => service(statement).map(toFortunes) ensure {
              service.close()
            }
          }.map(f => {
          val sortedFortunes = (Fortune(-1, "Additional fortune added at request time.") +: f).sortBy(_.message)
          FortunesList(sortedFortunes)
        })
    })

  val route = RouteSpec().at(Get) / "fortunes" bindTo Fortunes.service
}
