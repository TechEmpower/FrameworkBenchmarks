package net.benchmark.akka.http.util
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.{ActorAttributes, ActorMaterializerSettings, Attributes, Supervision}
import org.slf4j.{Logger, LoggerFactory}

object Deciders {

  def stoppingMat(name: String)(implicit system: ActorSystem): ActorMaterializerSettings = {
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(Deciders.StoppingDecider.stoppingDecider(s"materializer_$name"))
  }

  def stopping(name: String): Attributes = {
    ActorAttributes.supervisionStrategy(Deciders.StoppingDecider.stoppingDecider(s"stage_$name"))
  }

  def restartingMat(name: String)(implicit system: ActorSystem): ActorMaterializerSettings = {
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(Deciders.RestartingDecider.restartingDecider(s"materializer_$name"))
  }

  def restarting(name: String): Attributes = {
    ActorAttributes.supervisionStrategy(Deciders.RestartingDecider.restartingDecider(s"stage_$name"))
  }

  def resumingMat(name: String)(implicit system: ActorSystem): ActorMaterializerSettings = {
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(Deciders.ResumingDecider.resumingDecider(s"materializer_$name"))
  }

  def resuming(name: String): Attributes = {
    ActorAttributes.supervisionStrategy(Deciders.ResumingDecider.resumingDecider(s"stage_$name"))
  }

  val logLevels: Attributes = Attributes.logLevels(
    onElement = Logging.DebugLevel,
    onFinish = Logging.InfoLevel,
    onFailure = Logging.WarningLevel
  )

  private object StoppingDecider {

    private val log: Logger =
      LoggerFactory.getLogger(StoppingDecider.getClass.getName)

    def stoppingDecider(name: String): Function[Throwable, Supervision.Directive] = {
      val loggingStoppingDecider: Supervision.Decider = {
        case t: Throwable =>
          log.error(name + " - Throwable during streaming. Stopping.", t)
          Supervision.Stop
        case _ =>
          log.error(name + " - Unexpected case!! Stopping.")
          Supervision.Stop
      }
      loggingStoppingDecider
    }
  }

  private object RestartingDecider {

    private val log: Logger =
      LoggerFactory.getLogger(RestartingDecider.getClass.getName)

    def restartingDecider(name: String): Function[Throwable, Supervision.Directive] = {
      val loggingRestartingDecider: Supervision.Decider = {
        case t: Throwable =>
          log.error(name + " - Throwable during streaming. Restarting.", t)
          Supervision.Restart
        case _ =>
          log.error(name + " - Unexpected case!! Stopping.")
          Supervision.Stop
      }
      loggingRestartingDecider
    }
  }

  private object ResumingDecider {

    private val log: Logger =
      LoggerFactory.getLogger(ResumingDecider.getClass.getName)

    def resumingDecider(name: String): Function[Throwable, Supervision.Directive] = {
      val loggingResumingDecider: Supervision.Decider = {
        case t: Throwable =>
          log.error(name + " - Throwable during streaming. Resuming.", t)
          Supervision.Resume
        case _ =>
          log.error(name + " - Unexpected case!! Stopping.")
          Supervision.Stop
      }
      loggingResumingDecider
    }
  }

}
