package net.benchmark.akka.http

import akka.actor._
import akka.http.scaladsl.Http
import akka.pattern.pipe
import com.typesafe.config.Config
import net.benchmark.akka.http.ApiSupervisor.ApiMessages
import net.benchmark.akka.http.db.DatabaseRepositoryLoader
import net.benchmark.akka.http.util.SameThreadDirectExecutor

import scala.util.Failure

class ApiSupervisor(dbLoader: DatabaseRepositoryLoader) extends Actor with ActorLogging {

  private val config: Config = context.system.settings.config
  private val port: Int = config.getInt("akka-http-slick-postgres.api.port")
  private val address: String = config.getString("akka-http-slick-postgres.api.address")

  implicit val system: ActorSystem = context.system

  private val sd = SameThreadDirectExecutor.executionContext()

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  override def receive: Receive = {
    case ApiMessages.Shutdown =>
      log.info("Received Shutdown command from {}.", sender().path)
      val _ = context.system.terminate()
      context.stop(self)

    case ApiMessages.StartApi =>
      log.info("Received StartApi command from {}.", sender().path)

      import context.dispatcher

      val _ = Http(system)
        .newServerAt(address, port)
        .bind(ApiRoutes.routes(dbLoader, sd))
        .pipeTo(self)

      context.become(running(sender()))

    case Terminated(ref) =>
      log.debug("Received Terminated message from {}.", ref.path)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def running(supervisor: ActorRef): Receive = {
    case Http.ServerBinding(socketAddress) =>
      log.info("Listening on {}.", socketAddress)
      supervisor ! ApiMessages.ApiStarted

    case Failure(cause) =>
      log.error(cause, "Can't bind to {}:{}!", address, port)
      context.stop(self)

    case ApiMessages.Shutdown =>
      log.info("Received Shutdown message")
      context.stop(self)
  }

}

object ApiSupervisor {

  final val Name: String = "ApiSupervisorActor"

  def props(dbLoader: DatabaseRepositoryLoader): Props =
    Props(new ApiSupervisor(dbLoader))

  /**
    * A sealed trait for the messages of the actor.
    */
  sealed trait ApiMessages

  /**
    * A companion object for the trait to keep the namespace clean.
    */
  object ApiMessages {

    /**
      * Indicates that the API has been started successfully.
      */
    case object ApiStarted extends ApiMessages

    /**
      * Tell the actor to start the Api.
      */
    case object StartApi extends ApiMessages

    /**
      * Tell the actor to shutdown the actor system.
      */
    case object Shutdown extends ApiMessages

  }
}
