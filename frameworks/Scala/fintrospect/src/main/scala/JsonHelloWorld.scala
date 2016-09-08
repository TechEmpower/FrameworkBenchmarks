import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status.Ok
import io.fintrospect.RouteSpec
import io.fintrospect.formats.json.Circe.JsonFormat.{obj, string}

object JsonHelloWorld {

  import io.fintrospect.formats.json.Circe.ResponseBuilder.implicits._

  private val service = Service.mk { r: Request => Ok(obj("message" -> string("Hello, World!"))) }

  val route = RouteSpec().at(Get) / "json" bindTo JsonHelloWorld.service
}
