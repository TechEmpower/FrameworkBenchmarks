import javax.inject.{Singleton, Inject}

import play.api.http.HttpFilters
import play.api.mvc.{RequestHeader, EssentialAction, EssentialFilter}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.mvc.Http

@Singleton()
class Filters @Inject() (headerFilter: HeaderFilter) extends HttpFilters {
  override def filters: Seq[EssentialFilter] = Seq(
    headerFilter)
}

@Singleton
class HeaderFilter extends EssentialFilter {
  def apply(next: EssentialAction) = new EssentialAction {
    def apply(request: RequestHeader) = {
      next(request).map(result =>
        result.withHeaders(Http.HeaderNames.SERVER -> "EXAMPLE")  // Only here to address the WARN about this header.
      )
    }
  }
}