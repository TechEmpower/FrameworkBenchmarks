import controllers.AssetsComponents
import play.api.{controllers => _, _}
import router.Routes
import routing.Router
import ApplicationLoader.Context

import com.softwaremill.macwire._
import play.modules.reactivemongo.ReactiveMongoApiFromContext

class AppLoader extends ApplicationLoader {
  def load(context: Context) = new AppComponents(context).application
}

class AppComponents(context: Context)
  extends ReactiveMongoApiFromContext(context)
  with AssetsComponents {

  override lazy val router: Router = new Routes(
    httpErrorHandler,
    wire[controllers.Application],
    wire[controllers.Assets]
  )

  lazy val httpFilters = Seq.empty
}
