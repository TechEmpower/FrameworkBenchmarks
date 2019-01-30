package net.benchmark.akka.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import net.benchmark.akka.http.db.DatabaseRepositoryLoader
import net.benchmark.akka.http.fortune.FortuneRoutes
import net.benchmark.akka.http.world.WorldRoutes
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContextExecutor

object GlobalRoutes {
  final val REALM = "Authorization"

  val log: Logger = LoggerFactory.getLogger(getClass)

  def routes(dbLoader: DatabaseRepositoryLoader,
             qd: ExecutionContextExecutor,
             ud: ExecutionContextExecutor,
             dd: ExecutionContextExecutor,
             fd: ExecutionContextExecutor): Route =
    handleRejections(RejectionHandler.default) {
      val eh: ExceptionHandler = ExceptionHandler {
        case ex @ (_: Exception) =>
          log.error("Error occured.", ex)
          complete(StatusCodes.InternalServerError)
      }

      val worldRoutes = new WorldRoutes(dbLoader.loadWorldRepository(), qd, ud, dd)
      val fortuneRoutes = new FortuneRoutes(dbLoader.loadFortuneRepository(), fd)

      handleExceptions(eh) {
        get {
          worldRoutes.routes() ~
          fortuneRoutes.routes()
        }
      }
    }
}
