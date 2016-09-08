import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status.Ok
import io.fintrospect.RouteSpec
import io.fintrospect.formats.json.Circe.JsonFormat.{obj, string}
import io.fintrospect.formats.json.Circe.ResponseBuilder.implicits._

object JsonHelloWorld {

  private val service = Service.mk { r: Request => Ok(obj("message" -> string("Hello, World!"))) }

  val route = RouteSpec().at(Get) / "json" bindTo JsonHelloWorld.service
}
