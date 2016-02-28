import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status._
import com.twitter.finagle.http.path.Root
import com.twitter.finagle.{Http, Service}
import com.twitter.util.Await
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.fintrospect.formats.json.Circe.ResponseBuilder._
import io.fintrospect.formats.json.Circe.JsonFormat._
import io.fintrospect.{ModuleSpec, RouteSpec}

object FintrospectBenchmarkServer extends App {

  def plainTextHelloWorld() = {
    import io.fintrospect.formats.PlainText.ResponseBuilder._
    Service.mk { r: Request => Ok("Hello, World!") }
  }

  def jsonHelloWorld() = {
    case class Message(message: String)

    Service.mk { r: Request => Ok(encode(Message("Hello, World!"))) }
  }

  val module = ModuleSpec(Root)
    .withRoute(RouteSpec().at(Get) / "plaintext" bindTo plainTextHelloWorld)
    .withRoute(RouteSpec().at(Get) / "json" bindTo jsonHelloWorld)

  Await.ready(
    Http.serve(":9000", module.toService)
  )
}
