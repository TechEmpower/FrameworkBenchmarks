import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import io.fintrospect.RouteSpec
import io.fintrospect.formats.Jackson.JsonFormat.{encodeToBuf, obj, string}
import io.fintrospect.formats.Jackson.ResponseBuilder._

object JsonRoute {

  private val service = Service.mk { _: Request => Ok(encodeToBuf(obj("message" -> string("Hello, World!")))) }

  def apply() = RouteSpec().at(Get) / "json" bindTo service
}
