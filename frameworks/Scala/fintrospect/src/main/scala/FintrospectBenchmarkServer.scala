import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status._
import com.twitter.finagle.http.path.Root
import com.twitter.finagle.{Http, Service}
import com.twitter.util.Await
import io.fintrospect.formats.json.Circe.JsonFormat._
import io.fintrospect.{ModuleSpec, RouteSpec}

object FintrospectBenchmarkServer extends App {

  def plainTextHelloWorld() = {
    import io.fintrospect.formats.PlainText.ResponseBuilder._
    Service.mk { r: Request => Ok("Hello, World!") }
  }

  def jsonHelloWorld() = {
    import io.fintrospect.formats.json.Circe.ResponseBuilder._
    Service.mk { r: Request => Ok(obj("message" -> string("Hello, World!"))) }
  }

  val module = ModuleSpec(Root)
    .withRoute(RouteSpec().at(Get) / "plaintext" bindTo plainTextHelloWorld)
    .withRoute(RouteSpec().at(Get) / "json" bindTo jsonHelloWorld)

  Await.ready(
    Http.serve(":9000", module.toService)
  )
}
