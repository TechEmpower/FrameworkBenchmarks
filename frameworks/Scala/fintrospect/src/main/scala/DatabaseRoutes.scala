import com.fasterxml.jackson.databind.JsonNode
import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.mysql.Parameter.wrap
import com.twitter.finagle.mysql.{Client, IntValue, Result, ResultSet}
import com.twitter.util.Future.collect
import io.fintrospect.formats.Jackson.JsonFormat.{array, number, obj}
import io.fintrospect.formats.Jackson.ResponseBuilder._
import io.fintrospect.parameters.{ParameterSpec, Query, StringValidations}
import io.fintrospect.{RouteSpec, ServerRoutes}

import scala.language.reflectiveCalls
import scala.util.{Random, Try}

object DatabaseRoutes {

  private val toJson: PartialFunction[Result, Option[JsonNode]] = {
    case rs: ResultSet => rs.rows.headOption
      .map(row => {
        val IntValue(id) = row("id").get
        val IntValue(random) = row("randomNumber").get
        obj("id" -> number(id), "randomNumber" -> number(random))
      })
    case _ => None
  }

  private def generateRandomNumber = Random.nextInt(9999) + 1

  def apply(database: Client) = {
    val getStatement = database.prepare("SELECT id, randomNumber FROM world WHERE id = ?")
    val updateStatement = database.prepare("UPDATE world SET randomNumber = ? WHERE id = ?")

    val queryRoute = RouteSpec().at(Get) / "db" bindTo Service.mk {
      _: Request => getStatement(generateRandomNumber)
        .map(toJson)
        .map(_.map(Ok(_)).getOrElse(NotFound("")).build())
    }

    val numberOfQueries = Query.optional(ParameterSpec.string(StringValidations.EmptyIsValid).map {
      i => Try(i.toInt).getOrElse(1).max(1).min(500)
    }, "queries")

    val multipleRoute = RouteSpec()
      .taking(numberOfQueries)
      .at(Get) / "queries" bindTo Service.mk {
      r: Request => {
        collect(1.to((numberOfQueries <-- r).getOrElse(1))
          .map(i => getStatement(generateRandomNumber).map(toJson)))
          .map(f => f.flatMap(_.toSeq))
          .flatMap(c => Ok(array(c)))
      }
    }

    val updateRoute = RouteSpec()
      .taking(numberOfQueries)
      .at(Get) / "updates" bindTo Service.mk {
      r: Request => {
        collect(1.to((numberOfQueries <-- r).getOrElse(1))
          .map(i => {
            val id = generateRandomNumber
            updateStatement(generateRandomNumber, id)
              .flatMap(_ => getStatement(id))
              .map(toJson)
          }))
          .map(f => f.flatMap(_.toSeq))
          .flatMap(c => Ok(array(c)))
      }
    }

    new ServerRoutes[Request, Response] {
      add(queryRoute)
      add(multipleRoute)
      add(updateRoute)
    }
  }
}
