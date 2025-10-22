package pekko.http.benchmark.handlers

import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import pekko.http.benchmark.Infrastructure
import pekko.http.benchmark.datastore.DataStore
import pekko.http.benchmark.entity.World
import pekko.http.benchmark.util.RandomGenerator

import scala.concurrent.Future
import scala.util.Try

trait UpdatesHandler { _: Infrastructure with DataStore with RandomGenerator =>
  import com.github.pjfanning.pekkohttpjsoniterscala.JsoniterScalaSupport._

  def updatesEndpoint: Route =
    get {
      path("updates") {
        parameter("queries".?) { numQueries =>
          val realNumQueries = Try(numQueries.getOrElse("1").toInt).getOrElse(1).min(500).max(1)

          def mutateOne(id: Int): Future[World] =
            for {
              world <- requireWorldById(id)
              newWorld = world.copy(randomNumber = nextRandomIntBetween1And10000)
              _ <- updateWorld(newWorld) // ignore `wasUpdated`
            } yield newWorld

          complete {
            Future.traverse(Seq.fill(realNumQueries)(nextRandomIntBetween1And10000))(mutateOne)
          }
        }
      }
    }
}
