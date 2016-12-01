import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status.Ok
import io.fintrospect.RouteSpec
import io.fintrospect.RouteSpec.RequestValidation.none
import io.fintrospect.formats.Json4sJackson.JsonFormat.{obj, string}
import io.fintrospect.formats.Json4sJackson.ResponseBuilder.implicits._

object JsonRoute {

  private val service = Service.mk { r: Request => Ok(obj("message" -> string("Hello, World!"))) }

  def apply() = RouteSpec(validation = none).at(Get) / "json" bindTo service
}
