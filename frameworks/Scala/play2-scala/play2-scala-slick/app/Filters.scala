import javax.inject.{Singleton, Inject}

import play.api.http.HttpFilters
import play.api.mvc.{RequestHeader, EssentialAction, EssentialFilter}
import play.mvc.Http
import scala.concurrent.ExecutionContext

@Singleton()
class Filters @Inject() (headerFilter: HeaderFilter) extends HttpFilters {
  override def filters: Seq[EssentialFilter] = Seq(
    headerFilter)
}

@Singleton
class HeaderFilter @Inject() (implicit ec: ExecutionContext) extends EssentialFilter {
  def apply(next: EssentialAction) = new EssentialAction {
    def apply(request: RequestHeader) = {
      next(request).map(result =>
        result.withHeaders(Http.HeaderNames.SERVER -> "EXAMPLE")  // Only here to address the WARN about this header.
      )
    }
  }
}
