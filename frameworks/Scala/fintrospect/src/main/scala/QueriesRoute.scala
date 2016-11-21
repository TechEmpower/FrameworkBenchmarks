import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status.{NotFound, Ok}
import com.twitter.finagle.mysql.Parameter.wrap
import com.twitter.finagle.mysql.{Client, IntValue, Result, ResultSet}
import io.fintrospect.RouteSpec
import io.fintrospect.formats.Json4sJackson.JsonFormat.{number, obj}
import io.fintrospect.formats.Json4sJackson.ResponseBuilder.implicits._
import org.json4s.JValue

import scala.util.Random

object QueriesRoute {

  private val toJson: PartialFunction[Result, Option[JValue]] = {
    case rs: ResultSet => rs.rows.headOption
      .map(row => {
        val IntValue(id) = row("id").get
        val IntValue(random) = row("randomNumber").get
        obj("id" -> number(id), "randomNumber" -> number(random))
      })
    case _ => None
  }

  private def generateRandomId = Random.nextInt(9999) + 1

  def apply(database: Client) = {
    val statement = database.prepare("SELECT id, randomNumber FROM world WHERE id = ?")

    val service = Service.mk {
      r: Request => statement(generateRandomId)
        .map(toJson)
        .map(_.map(Ok(_)).getOrElse(NotFound()).build())
    }

    RouteSpec().at(Get) / "db" bindTo service
  }
}
