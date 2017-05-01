import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.mysql._
import io.fintrospect.RouteSpec
import io.fintrospect.formats.Html
import io.fintrospect.templating.MustacheTemplates.CachingClasspath
import io.fintrospect.templating.{RenderView, View}

case class Fortune(id: Int, message: String)

case class FortunesList(items: Seq[Fortune]) extends View

object FortunesRoute {

  private val toFortunes: PartialFunction[Result, Seq[Fortune]] = {
    case rs: ResultSet => rs.rows
      .map(row => {
        val IntValue(id) = row("id").get
        val StringValue(message) = row("message").get
        Fortune(id, message)
      })
    case _ => Seq.empty
  }

  def apply(database: Client) = {

    val statement = database.prepare("SELECT * FROM fortune")

    val service = new RenderView(Html.ResponseBuilder, CachingClasspath()).andThen(
      Service.mk {
        _: Request =>
          statement().map(toFortunes).map(f => {
            val sortedFortunes = (Fortune(0, "Additional fortune added at request time.") +: f).sortBy(_.message)
            FortunesList(sortedFortunes)
          })
      })

    RouteSpec().at(Get) / "fortunes" bindTo service
  }
}
