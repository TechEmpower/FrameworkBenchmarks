package pekko.http.benchmark.handlers

import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import pekko.http.benchmark.Infrastructure
import pekko.http.benchmark.datastore.DataStore
import pekko.http.benchmark.util.RandomGenerator

import scala.concurrent.Future
import scala.util.Try

trait QueriesHandler {
  _: Infrastructure with DataStore with RandomGenerator =>
  import com.github.pjfanning.pekkohttpjsoniterscala.JsoniterScalaSupport._

  def queriesEndpoint: Route =
    get {
      path("queries") {
        parameter("queries".?) { numQueries =>
          // The queries parameter must be bounded to between 1 and 500. If the parameter is missing,
          // is not an integer, or is an integer less than 1, the value should be interpreted as 1;
          // if greater than 500, the value should be interpreted as 500.
          val realNumQueries = Try(numQueries.getOrElse("1").toInt).getOrElse(1).min(500).max(1)
          complete {
            Future.traverse(Seq.fill(realNumQueries)(nextRandomIntBetween1And10000))(requireWorldById)
          }
        }
      }
    }
}