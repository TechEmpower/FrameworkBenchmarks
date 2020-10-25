package startup

import play.api._
import play.api.inject._

class StartupModule extends Module {
  def bindings(environment: Environment, configuration: Configuration) = Seq(
    bind[Startup].toSelf.eagerly()
  )
}