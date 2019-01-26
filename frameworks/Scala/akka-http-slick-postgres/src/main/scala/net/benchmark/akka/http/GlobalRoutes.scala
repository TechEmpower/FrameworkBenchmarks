package net.benchmark.akka.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import net.benchmark.akka.http.db.DatabaseRepositoryLoader
import net.benchmark.akka.http.world.{FortuneRoutes, WorldRoutes}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext

object GlobalRoutes {
  final val REALM = "Authorization"

  val log: Logger = LoggerFactory.getLogger(getClass)

  def baseRoutes(dbLoader: DatabaseRepositoryLoader)(implicit ec: ExecutionContext): Route =
    handleRejections(RejectionHandler.default) {
      val eh: ExceptionHandler = ExceptionHandler {
        case ex @ (_: Exception) =>
          log.error("Error occured.", ex)
          complete(StatusCodes.InternalServerError)
      }

      val worldRoutes = new WorldRoutes(dbLoader.loadWorldRepository(), ec)
      val fortuneRoutes = new FortuneRoutes(dbLoader.loadFortuneRepository(), ec)

      handleExceptions(eh) {
        pathPrefix("") {
          worldRoutes.worldRoutes() ~
          fortuneRoutes.fortuneRoutes()
        }
      }
    }

  def routes(dbLoader: DatabaseRepositoryLoader)(implicit ec: ExecutionContext): Route = {
    baseRoutes(dbLoader)
  }

}
