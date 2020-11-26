package net.benchmark.akka.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import net.benchmark.akka.http.db.DatabaseRepositoryLoader
import net.benchmark.akka.http.fortune.FortuneRoute
import net.benchmark.akka.http.world.WorldRoutes
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContextExecutor

object ApiRoutes {

  private val log: Logger = LoggerFactory.getLogger(getClass)

  def routes(dbLoader: DatabaseRepositoryLoader, sd: ExecutionContextExecutor)(implicit system: ActorSystem): Route =
    handleRejections(RejectionHandler.default) {
      val eh: ExceptionHandler = ExceptionHandler {
        case ex @ (_: Exception) =>
          log.error("Error occured.", ex)
          complete(StatusCodes.InternalServerError)
      }

      val worldRoutes = new WorldRoutes(dbLoader.loadWorldRepository(), sd)
      val fortuneRoutes = new FortuneRoute(dbLoader.loadFortuneRepository(), sd)

      handleExceptions(eh) {
        get {
          worldRoutes.routes() ~
          fortuneRoutes.route()
        }
      }
    }
}
