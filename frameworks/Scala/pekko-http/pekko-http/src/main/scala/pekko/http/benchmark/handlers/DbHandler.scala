package pekko.http.benchmark.handlers

import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import pekko.http.benchmark.Infrastructure
import pekko.http.benchmark.datastore.DataStore
import pekko.http.benchmark.util.RandomGenerator

trait DbHandler { _: Infrastructure with DataStore with RandomGenerator =>
  import com.github.pjfanning.pekkohttpjsoniterscala.JsoniterScalaSupport._

  def dbEndpoint: Route =
    get {
      path("db") {
        complete(requireWorldById(nextRandomIntBetween1And10000))
      }
    }
}