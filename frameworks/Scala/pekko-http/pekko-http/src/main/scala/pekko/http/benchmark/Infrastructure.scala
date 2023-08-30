package pekko.http.benchmark

import org.apache.pekko.actor.ActorSystem
import com.typesafe.config.Config

import scala.concurrent.ExecutionContext

trait Infrastructure {
  implicit def system: ActorSystem

  implicit def executionContext: ExecutionContext

  def appConfig: Config
}
