import play.api.{controllers => _, _}
import router.Routes
import routing.Router
import ApplicationLoader.Context

import com.softwaremill.macwire._
import play.modules.reactivemongo.ReactiveMongoApiFromContext
import startup.Startup

class AppLoader extends ApplicationLoader {
  def load(context: Context) = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment, context.initialConfiguration, Map.empty)
    }
    new Startup(context.initialConfiguration.underlying)
    new AppComponents(context).application
  }
}

class AppComponents(context: Context)
  extends ReactiveMongoApiFromContext(context) {

  override lazy val router: Router = new Routes(
    httpErrorHandler,
    wire[controllers.Application]
  )

  lazy val httpFilters = Seq.empty
}
