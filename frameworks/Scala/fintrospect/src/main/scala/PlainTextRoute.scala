import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status.Ok
import com.twitter.io.Buf
import io.fintrospect.RouteSpec
import io.fintrospect.RouteSpec.RequestValidation.none
import io.fintrospect.formats.PlainText.ResponseBuilder.implicits._

object PlainTextRoute {

  private val preallocatedMsgForPlainText = Buf.Utf8("Hello, World!")

  private val service = Service.mk { r: Request => Ok(preallocatedMsgForPlainText) }

  def apply() = RouteSpec(validation = none).at(Get) / "plaintext" bindTo service
}
